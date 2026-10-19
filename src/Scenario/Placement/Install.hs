{-# LANGUAGE Strict #-}
-- | Committing structure pieces and placed locations onto a live page
--   (#2712, SCN-12 of epic #2698). It runs ON THE WORLD THREAD, the only
--   writer of the tile store, the page's edit log and its generation
--   parameters, so a check and the write that depends on it can never be
--   separated by another writer; every other thread asks through
--   'requestPlacementInstall', which carries the page incarnation it
--   resolved and is answered 'PlacementNotAdmitted' (nothing changed) if
--   the page was replaced or destroyed since, and 'PlacementNotArena'
--   (nothing changed) for a generated world, whose chunks are not rebuilt
--   from the arena base.
--
--   __Replacement, not accumulation.__ A materialization establishes the
--   page's physical baseline, so each family a request names REPLACES what
--   the page holds: every structure piece ('WeSetStructure' /
--   'WeClearStructure') in the durable log is dropped and the new pieces
--   are appended after the rest of it, and the placed-location table is
--   rebuilt from nothing (the allocator is kept, so an id is never
--   reused). A second materialization or a recapture therefore
--   accumulates nothing, and a removed wall stays removed because the
--   durable log, not a builder, is what rebuilds a chunk.
--
--   __The pieces are committed, not queued.__ Each resident chunk's
--   structure overlay is rebuilt from the new log through the ordinary
--   edit replay ('World.Edit.Apply.applyEdit') and swapped in by
--   'World.Flora.Designation.replaceChunkForgettingFlora', the owner that
--   forgets the tags, regrowth timers and designations of whatever the
--   swap removes; a chunk that is not resident gets only the log, rebuilt
--   by its own admission. The simulation is not re-seeded: structure
--   pieces carry no fluid, and nothing here advances it. Staged
--   placements the Lua builders left in the page's write-ahead cache are
--   dropped (a piece the log does not hold must not read as real), and
--   construction designations are revalidated against the new geometry.
--
--   __Palette ids are interned here, once the page is known to admit the
--   request__ ('Structure.Palette.internPath' on the live palette): the
--   palette is a persistent component whose unreferenced entries are never
--   pruned, so a path interned for a request that then changed nothing
--   would ride into every later save. The runtime handle of each path is
--   resolved by the existing Lua resolve tick, never here.
--
--   __Locations__ are committed in one transition on the page's generation
--   parameters: 'Scenario.Placement.Location.applyLocationInstalls' on the
--   table, and the anchor chunk's geometry marker
--   ('World.Generate.Types.wgpLocationStamped') for each new instance, so
--   the every-load stamp dispatch treats the geometry as complete.
module Scenario.Placement.Install
    ( requestPlacementInstall
    , handleWorldInstallScenarioPlacementCommand
    , installPlacement
    , PlacementRefs(..)
    , replaceStructureEdits
    ) where

import UPrelude
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.RenderHandoff (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Capability.ContentRegistries (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.State (EngineEnv)
import Engine.Core.Log (LoggerState)
import qualified Data.Vector.Unboxed as VU
import Location.Instance (LocationInstance(..), LocationInstanceId, LocationInstances(..))
import Scenario.Placement.Location
import Scenario.Placement.Structure
import Scenario.Placement.Types
import Scenario.Types
    ( DiagnosticEffect(..), DiagnosticReason(..), ScenarioDiagnostic(..), ScenarioId
    , StructurePiece(..) )
import Scenario.World.Build (groupEditsByChunk)
import Scenario.World.Capture (rebuiltChunk)
import Scenario.World.Install (isStructureEdit)
import Location.Types (LocationRegistry)
import Data.IORef (IORef)
import Structure.ArtCatalog (StructureArtCatalog)
import Structure.Palette (TexPalette, internPath)
import Structure.Types (emptyChunkStructures, clearStagedAll)
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Residency (ChunkGeneration)
import World.Chunk.Types (LoadedChunk(..), columnIndex)
import World.Command.Types (WorldCommand(..))
import World.Construct.Revalidate (ConstructScope(..), revalidateConstructDesignations)
import World.Edit.Apply (applyEdit)
import World.Edit.Types (WorldEdit(..), WorldEdits)
import World.Flora.Designation (replaceChunkForgettingFlora)
import World.Generate.Coordinates (globalToChunk)
import World.Generate.Types (WorldGenParams(..), isArenaParams)
import World.Page.Types (WorldPageId)
import World.State.Types
import World.Tile.Types (WorldTileData(..), lookupChunk)

-- | Ask the world thread to commit @request@ on @page@ and wait for the
--   answer. Must not be called ON the world thread, which would wait for
--   itself.
requestPlacementInstall ∷ WorldSimCapability → WorldPageId → ChunkGeneration
                        → ScenarioPlacementRequest → IO PlacementInstallOutcome
requestPlacementInstall wsc page epoch request = do
    reply ← newEmptyMVar
    Q.writeQueue (wsWorldQueue wsc)
        (WorldInstallScenarioPlacement page epoch request (PlacementInstallReply reply))
    takeMVar reply

-- | The world-thread handler: check the incarnation and the page kind,
--   commit, answer.
handleWorldInstallScenarioPlacementCommand
    ∷ EngineEnv → LoggerState → WorldPageId → ChunkGeneration
    → ScenarioPlacementRequest → PlacementInstallReply → IO ()
handleWorldInstallScenarioPlacementCommand env logger page epoch request (PlacementInstallReply reply) = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    outcome ← case lookup page (wmWorlds mgr) of
        Nothing → pure PlacementNotAdmitted
        Just ws → do
            now ← pageIncarnation ws
            mParams ← readIORef (wsGenParamsRef ws)
            if now ≢ epoch
                then pure PlacementNotAdmitted
                else if not (maybe False isArenaParams mParams)
                    then pure PlacementNotArena
                    else PlacementInstalled ⊚ installPlacement refs env logger page ws request
    putMVar reply outcome
  where
    handoff = toRenderHandoffCapability env
    refs = PlacementRefs
        { prPalette = rhTexPaletteRef handoff
        , prArt = rhStructureArtCatalogRef handoff
        , prLocations = crLocationDefsRef (toContentRegistriesCapability env) }

-- | The live registries a commit reads and the one it interns into,
--   handed in by the handler like "Scenario.Units.Install"'s refs: the
--   palette is written only through the structure owners' interning, never
--   by naming the field.
data PlacementRefs = PlacementRefs
    { prPalette   ∷ IORef TexPalette
    , prArt       ∷ IORef StructureArtCatalog
    , prLocations ∷ IORef LocationRegistry
    }

-- | The page's edit log with every structure entry replaced by @new@:
--   the non-structure entries first, in their original order, then the new
--   pieces; a chunk left with nothing has no key.
replaceStructureEdits ∷ WorldEdits → WorldEdits → WorldEdits
replaceStructureEdits old new =
    let retained = HM.map (filter (not ∘ isStructureEdit)) old
    in HM.filter (not ∘ null) (HM.unionWith (⧺) retained new)

-- | Commit. World thread only.
installPlacement ∷ PlacementRefs → EngineEnv → LoggerState → WorldPageId → WorldState
                 → ScenarioPlacementRequest → IO PlacementInstallResult
installPlacement refs env logger page ws request = do
    (structRefs, structDiags) ← case sprStructures request of
        Nothing → pure ([], [])
        Just entries → installStructures refs env logger page ws entries
    (locRefs, locDiags) ← case sprLocations request of
        Nothing → pure ([], [])
        Just installs → installLocations refs ws installs
    pure PlacementInstallResult
        { pirStructures = structRefs
        , pirLocations = locRefs
        , pirDiagnostics = structDiags ⧺ locDiags }

installStructures ∷ PlacementRefs → EngineEnv → LoggerState → WorldPageId → WorldState
                  → [(Text, StructurePiece)]
                  → IO ([(ScenarioId, (Int, Int, Word8))], [ScenarioDiagnostic])
installStructures refs env logger _page ws entries = do
    mParams ← readIORef (wsGenParamsRef ws)
    let bounds = mParams ⌦ wgpArenaBounds
        seed = maybe 0 wgpSeed mParams
    oldEdits ← readIORef (wsEditsRef ws)
    td ← readIORef (wsTilesRef ws)
    art ← readIORef (prArt refs)
    -- the column surface under each entry, from the page's terrain: the
    -- resident chunk where there is one, else the chunk the durable log
    -- rebuilds
    let chunkAt c = fromMaybe (rebuiltChunk bounds seed oldEdits c) (lookupChunk c td)
        needed = HS.toList (HS.fromList
            [ fst (globalToChunk (spX sp) (spY sp)) | (_, sp) ← entries ])
        chunks = Map.fromList [ (c, chunkAt c) | c ← needed ]
        surface (x, y) = case Map.lookup (fst (globalToChunk x y)) chunks of
            Just lc → surfaceZAt lc x y
            Nothing → surfaceZAt (chunkAt (fst (globalToChunk x y))) x y
        built = buildStructures StructureBuildEnv
                    { sbeBounds = bounds, sbeArt = art, sbeSurfaceZ = surface } entries
    -- intern each path ONCE, in the order the pieces come, and only now
    -- that the page is known to admit the request
    ids ← atomicModifyIORef' (prPalette refs) $ \pal0 →
        let step (pal, m) path = case Map.lookup path m of
                Just _ → (pal, m)
                Nothing → let (i, pal') = internPath path pal in (pal', Map.insert path i m)
            (palN, mapN) = foldl' step (pal0, Map.empty)
                [ p | bp ← sbPieces built, p ← [bpTexture bp, bpFacemap bp] ]
        in (palN, mapN)
    let edit bp = WeSetStructure (bpX bp) (bpY bp) (bpSlotTag bp)
                      (ids Map.! bpTexture bp) (ids Map.! bpFacemap bp) (bpZ bp)
        newStructureEdits = groupEditsByChunk (map edit (sbPieces built))
        newEdits = replaceStructureEdits oldEdits newStructureEdits
    -- the durable state first, so a chunk rebuilt below replays the new log
    writeIORef (wsEditsRef ws) newEdits
    -- every resident chunk's structure overlay is rebuilt from the new log
    -- through the ordinary replay and swapped in through the owner that
    -- forgets what a swap removes
    forM_ (HM.elems (wtdChunks td)) $ \old → do
        let c = lcCoord old
            mine = filter isStructureEdit (HM.lookupDefault [] c newEdits)
            fresh = foldl' (flip applyEdit) old { lcStructures = emptyChunkStructures } mine
        replaceChunkForgettingFlora ws old fresh
    -- a write-ahead entry the Lua builders staged that the log does not hold
    -- must not read as real
    atomicModifyIORef' (wsStructureStageRef ws) (\st → (clearStagedAll st, ()))
    _ ← revalidateConstructDesignations env logger ws ConstructWholePage
    bumpQuadCacheGen ws
    writeIORef (wsZoomQuadCacheRef ws) Nothing
    writeIORef (wsBgQuadCacheRef ws) Nothing
    pure ( [ (bpId bp, pieceKey bp) | bp ← sbPieces built ], sbDiagnostics built )
  where
    surfaceZAt lc x y =
        let (_, (lx, ly)) = globalToChunk x y
        in lcTerrainSurfaceMap lc VU.! columnIndex lx ly

-- | Commit the location table and the anchor chunks' geometry markers.
installLocations ∷ PlacementRefs → WorldState → [LocationInstall]
                 → IO ([(ScenarioId, LocationInstanceId)], [ScenarioDiagnostic])
installLocations refs ws installs = do
    registry ← readIORef (prLocations refs)
    result ← atomicModifyIORef' (wsGenParamsRef ws) $ \mp → case mp of
        Nothing → (mp, ([], [ScenarioDiagnostic "locations"
                                (InvalidValue "a page with generation parameters") EntryRejected ]))
        Just p →
            let lenv = LocationBuildEnv
                    { lbeDefs = registry, lbeBounds = wgpArenaBounds p
                    , lbeSeed = wgpSeed p, lbeNamer = Nothing }
                (lis', refs, ds) = applyLocationInstalls lenv installs (wgpLocationInstances p)
                stamped = HS.fromList [ liChunk i | i ← HM.elems (lisById lis') ]
            in ( Just p { wgpLocationInstances = lis', wgpLocationStamped = stamped }
               , (refs, ds) )
    bumpQuadCacheGen ws
    writeIORef (wsZoomQuadCacheRef ws) Nothing
    writeIORef (wsBgQuadCacheRef ws) Nothing
    pure result
