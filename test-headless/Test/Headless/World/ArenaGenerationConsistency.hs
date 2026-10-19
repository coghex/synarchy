{-# LANGUAGE OverloadedStrings #-}
-- | "arena generation consistency" (#2707, SCN-07 of epic #2698).
--
--   An arena's base is reconstructed, never stored, so the SAME recorded
--   seed and canonical chunk coordinate must give the SAME complete chunk
--   through every path that can produce one. This group compares the real
--   production paths against each other and against an independently
--   written description of the base — a helper compared with itself
--   would pass with the loaders still wired to the wrong generator.
--
--   The paths, each driven for real:
--
--   * startup — @WorldInitArena@ (the 5×5 batch wrapper);
--   * camera — the camera loader (which also regenerates evicted chunks);
--   * queued demand — the same enqueue @world.loadChunksInRegion@ makes,
--     drained by the explicit\/init queue drain (used to hand an arena's
--     parameters to the geological generator);
--   * eviction and readmission, with and without edits;
--   * staged restore — 'World.Load.Stage.stageSession' on an in-process
--     arena snapshot (the headless harness cannot run @engine.saveWorld@;
--     the disk round trip is @tools/multiworld_save_probe.py --arena@).
--
--   The independent description ('baseProperties') states the approved
--   foundation in the issue's own terms: sixteen cells per column, twelve
--   granite under four loam, the top at sea level, vegetation on the top
--   cell only and within the four grass variants, every default map
--   empty. Coordinates include the centre, the footprint's corners, a
--   chunk just outside it, negative coordinates, and a far coordinate
--   whose canonical form would be wrongly folded if the arena's sentinel
--   world size were ever used as a wrap width.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "arena generation consistency"'@.
module Test.Headless.World.ArenaGenerationConsistency (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import GHC.Clock (getMonotonicTimeNSec)
import Data.List (nub, sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Core.Log (LoggerState, LogConfig(..), LogBackend(..), initLogger, defaultLogConfig)
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Item.Knowledge (emptyPortableKnowledge)
import Sim.Command.Types (SimCommand(..))
import Structure.Palette (emptyTexPalette)
import Structure.Types (emptyChunkStructures)
import Test.Headless.Harness
    ( queueChunks, sendWorldCommand, waitForChunksAt, waitForWorldInit )
import World.Chunk.Residency (canonicalChunkCoord)
import World.Edit.Types (WorldEdit(..), WorldEdits, appendEdit, emptyWorldEdits)
import World.Generate.Arena
    ( arenaFootprint, generateArenaChunk, generateArenaChunks )
import World.Generate.Coordinates (globalToChunk)
import World.Grid (gridToWorld)
import World.Load.Publish (publishStagedSession)
import World.Load.Stage (renderStageError, stageSession)
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Material (MaterialId(..), matGranite, matLoam)
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Snapshot (LiveCameraSnapshot(..), PageSnapshot(..), SessionSnapshot(..))
import World.Save.Envelope (decodeSessionEnvelope)
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import World.Types
import World.Vegetation (vegMediumGrass)
import qualified Data.Text as T

-- * The independent description of the base

-- | What EVERY unedited arena column, and every default map, must be —
--   written from the issue's words, not from the generator.
baseProperties ∷ ChunkCoord → LoadedChunk → [String]
baseProperties coord lc = concat
    [ [ "coordinate " ⧺ show (lcCoord lc) | lcCoord lc ≢ coord ]
    , [ "columns " ⧺ show (V.length (lcTiles lc)) | V.length (lcTiles lc) ≢ 256 ]
    , concat [ columnProblems i col | (i, col) ← zip [0 ∷ Int ..] (V.toList (lcTiles lc)) ]
    , [ "surface map" | lcSurfaceMap lc ≢ VU.replicate 256 seaLevel ]
    , [ "terrain surface map" | lcTerrainSurfaceMap lc ≢ VU.replicate 256 seaLevel ]
    , [ "fluid map" | any isJust (V.toList (lcFluidMap lc)) ∨ V.length (lcFluidMap lc) ≢ 256 ]
    , [ "ice map" | lcIceMap lc ≢ emptyIceMap ]
    , [ "flora" | lcFlora lc ≢ emptyFloraChunkData ]
    , [ "side deco" | lcSideDeco lc ≢ VU.replicate 256 0 ]
    , [ "water table" | lcWaterTableMap lc ≢ VU.replicate 256 (seaLevel - 2) ]
    , [ "magma" | isJust (lcMagma lc) ]
    , [ "structures" | lcStructures lc ≢ emptyChunkStructures ]
    ]
  where
    MaterialId granite = matGranite
    MaterialId loam = matLoam
    columnProblems i col = concat
        [ [ "start z @" ⧺ show i | ctStartZ col ≢ seaLevel - 15 ]
        , [ "mats @" ⧺ show i
          | VU.toList (ctMats col) ≢ replicate 12 granite ⧺ replicate 4 loam ]
        , [ "slopes @" ⧺ show i | VU.toList (ctSlopes col) ≢ replicate 16 0 ]
        , [ "veg @" ⧺ show i
          | let v = VU.toList (ctVeg col)
          , length v ≢ 16 ∨ take 15 v ≢ replicate 15 0
            ∨ last v `notElem` [vegMediumGrass .. vegMediumGrass + 3] ]
        ]

-- | The coordinates every path is compared at.
probeCoords ∷ [ChunkCoord]
probeCoords =
    [ ChunkCoord 0 0                       -- centre
    , ChunkCoord 2 (-2), ChunkCoord (-2) 2 -- footprint corners
    , ChunkCoord 3 0                       -- just outside the footprint
    , ChunkCoord (-4) 5                    -- outside, negative x
    , ChunkCoord (-3) (-6)                 -- outside, both negative
    ]

-- | A far, NON-WRAPPING coordinate: its u (cx − cy) is 120000, past the
--   arena sentinel's 50000, so a wrap through the sentinel would fold it
--   onto a different chunk; its v (cx + cy) is 0, inside the camera
--   loader's bound.
farCoord ∷ ChunkCoord
farCoord = ChunkCoord 60000 (-60000)

-- | Point the camera at the middle of a chunk. The camera position is in
--   WORLD (screen) space, so the tile is converted with the camera's own
--   facing — the same frame the camera loader reads it back in.
goToChunk ∷ EngineEnv → ChunkCoord → IO ()
goToChunk env (ChunkCoord cx cy) = do
    facing ← camFacing ⊚ readIORef (cameraRef env)
    let (wx, wy) = gridToWorld facing (cx * 16 + 8) (cy * 16 + 8)
    atomicModifyIORef' (cameraRef env) $ \cam →
        (cam { camPosition = (wx, wy), camZoom = 0.5 }, ())

-- * Harness

-- | A fresh arena page that is VISIBLE (the camera loader serves visible
--   pages), torn down afterwards so examples stay independent.
withArena ∷ EngineEnv
          → (WorldPageId → WorldState → WorldGenParams → IO α) → IO α
withArena env act = do
    -- A page id is a reusable NAME: a unique one per example keeps an
    -- earlier example's teardown from being mistaken for this page's.
    n ← getMonotonicTimeNSec
    let pid = WorldPageId (T.pack ("arena_consistency_" <> show n))
    sendWorldCommand env (WorldInitArena pid Nothing)
    ws ← waitForWorldInit env pid 60
    sendWorldCommand env (WorldInitArenaDone pid)
    visible ← waitUntil 30 $ do
        mgr ← readIORef (worldManagerRef env)
        pure (pid `elem` wmVisible mgr)
    unless visible $ expectationFailure "the arena page never became visible"
    mParams ← readIORef (wsGenParamsRef ws)
    params ← maybe (expectationFailure "arena recorded no params" ≫ error "unreachable")
                   pure mParams
    _ ← Q.flushQueue (simQueue env)
    goToChunk env (ChunkCoord 0 0)
    act pid ws params `finally` do
        sendWorldCommand env (WorldDestroy pid)
        goToChunk env (ChunkCoord 0 0)

residentChunk ∷ WorldState → ChunkCoord → IO (Maybe LoadedChunk)
residentChunk ws coord = HM.lookup coord ∘ wtdChunks ⊚ readIORef (wsTilesRef ws)

-- | Lower the page's residency budget to one camera window (5×5 chunks),
--   so moving the camera away evicts everything outside the new window.
--   A fixture lever on the page's own tile map; it changes no production
--   budget, selection rule or reservation.
tightBudget ∷ WorldState → IO ()
tightBudget ws = atomicModifyIORef' (wsTilesRef ws) $ \td → (td { wtdMaxChunks = 25 }, ())

-- | Wait (bounded) until the condition holds.
waitUntil ∷ Int → IO Bool → IO Bool
waitUntil secs cond = go (secs * 20)
  where go n | n ≤ 0 = pure False
             | otherwise = cond ≫= \ok → if ok then pure True
                                         else threadDelay 50000 ≫ go (n - 1 ∷ Int)

-- | Wait for a chunk and return it.
awaitChunk ∷ WorldState → ChunkCoord → IO LoadedChunk
awaitChunk ws coord = do
    ok ← waitForChunksAt ws coord 60
    unless ok $ expectationFailure ("chunk never became resident: " ⧺ show coord)
    residentChunk ws coord ≫= maybe (expectationFailure "chunk vanished" ≫ error "unreachable") pure

-- | Wait until the page's explicit\\/init queue is empty and quiet.
awaitDrained ∷ WorldState → IO ()
awaitDrained ws = do
    ok ← waitUntil 30 (null ⊚ readIORef (wsInitQueueRef ws))
    ok `shouldBe` True
    threadDelay 300000

-- | Every 'SimChunkLoaded' coordinate for the page since the last flush.
simLoaded ∷ EngineEnv → WorldPageId → IO [ChunkCoord]
simLoaded env pid = do
    cmds ← Q.flushQueue (simQueue env)
    pure [ c | SimChunkLoaded p _ _ c _ _ ← cmds, p ≡ pid ]

-- * The group

spec ∷ SpecWith EngineEnv
spec = do
    pureSpec
    describe "through the production paths" $ do
        startupSpec
        cameraSpec
        queueSpec
        overlapSpec
        evictionSpec
        restoreSpec
        legacySpec

-- | The authority itself: the independent description, order
--   independence, and what a seed is allowed to vary.
pureSpec ∷ SpecWith EngineEnv
pureSpec = describe "the arena authority" $ do

    it "gives the approved base at every probe coordinate, the far one included" $ \_ →
        forM_ (probeCoords ⧺ [farCoord]) $ \c →
            baseProperties c (generateArenaChunk 0 c) `shouldBe` []

    it "is independent of batch size, request order and repetition" $ \_ → do
        let single = [ (c, generateArenaChunk 0 c) | c ← probeCoords ]
            reversed = reverse [ (c, generateArenaChunk 0 c) | c ← reverse probeCoords ]
            batch = HM.fromList [ (lcCoord c, c) | c ← generateArenaChunks 0 ]
        map snd single `shouldBe` map snd reversed
        forM_ arenaFootprint $ \c →
            HM.lookup c batch `shouldBe` Just (generateArenaChunk 0 c)
        generateArenaChunk 0 (ChunkCoord 3 0) `shouldBe` generateArenaChunk 0 (ChunkCoord 3 0)

    it "gives neighbouring chunks their own vegetation, not one shared vector" $ \_ → do
        let vegs = [ map (VU.last ∘ ctVeg) (V.toList (lcTiles (generateArenaChunk 0 c)))
                   | c ← arenaFootprint ]
        length (nub vegs) `shouldBe` length arenaFootprint

    it "lets a seed vary the vegetation variants and nothing else" $ \_ → do
        let a = generateArenaChunk 0 (ChunkCoord 3 0)
            b = generateArenaChunk 20260827 (ChunkCoord 3 0)
            tops lc = map (VU.last ∘ ctVeg) (V.toList (lcTiles lc))
            stripVeg lc = lc { lcTiles = V.map (\col → col { ctVeg = VU.empty }) (lcTiles lc) }
        tops a `shouldNotBe` tops b
        stripVeg a `shouldBe` stripVeg b

    it "is recognised as an arena only by the canonical seed-zero rule" $ \_ → do
        let arena = defaultWorldGenParams { wgpSeed = 0, wgpWorldSize = 100000 }
        isArenaParams arena `shouldBe` True
        isArenaParams arena { wgpSeed = 1 } `shouldBe` False
        canonicalChunkCoord arena farCoord `shouldBe` farCoord

-- | Startup: the 5×5 batch wrapper, as the init command publishes it.
startupSpec ∷ SpecWith EngineEnv
startupSpec = describe "startup" $
    it "publishes exactly the footprint, each chunk the approved base and the authority's output" $ \env → do
        withArena env $ \_ ws params → do
            tiles ← wtdChunks ⊚ readIORef (wsTilesRef ws)
            sort (HM.keys tiles) `shouldBe` sort arenaFootprint
            forM_ (HM.toList tiles) $ \(c, lc) → do
                baseProperties c lc `shouldBe` []
                lc `shouldBe` generateArenaChunk (wgpSeed params) c

-- | Camera expansion: the camera loader supplies chunks outside the footprint.
cameraSpec ∷ SpecWith EngineEnv
cameraSpec = describe "camera expansion" $
    it "supplies the approved base outside the footprint, at negative and far coordinates" $ \env → do
        withArena env $ \_ ws params →
            forM_ (drop 3 probeCoords ⧺ [farCoord]) $ \c → do
                goToChunk env c
                lc ← awaitChunk ws c
                baseProperties c lc `shouldBe` []
                lc `shouldBe` generateArenaChunk (wgpSeed params) c

-- | Queued demand: what @world.loadChunksInRegion@ does, beyond the footprint.
queueSpec ∷ SpecWith EngineEnv
queueSpec = describe "queued demand" $
    it "builds chunks outside the footprint from the same authority instead of the geological generator" $ \env → do
        withArena env $ \pid ws params → do
            let wanted = drop 3 probeCoords ⧺ [farCoord]
            queueChunks pid ws wanted
            forM_ wanted $ \c → do
                lc ← awaitChunk ws c
                baseProperties c lc `shouldBe` []
                lc `shouldBe` generateArenaChunk (wgpSeed params) c

-- | Overlapping and repeated demand.
overlapSpec ∷ SpecWith EngineEnv
overlapSpec = describe "overlapping and repeated demand" $ do

    it "admits a chunk once and reports it only when its terrain is available" $ \env → do
        withArena env $ \pid ws params → do
            let target = ChunkCoord 4 3
                around = [ ChunkCoord (4 + dx) (3 + dy) | dx ← [-1 .. 1], dy ← [-1 .. 1] ]
            -- the queue and the camera ask for the same chunks together
            queueChunks pid ws around
            goToChunk env target
            queueChunks pid ws around
            forM_ around $ \c → awaitChunk ws c ≫= \lc →
                lc `shouldBe` generateArenaChunk (wgpSeed params) c
            awaitDrained ws
            loaded ← simLoaded env pid
            -- the footprint was seeded by init BEFORE the flush above;
            -- everything since is the nine chunks, each exactly once
            sort (filter (`elem` around) loaded) `shouldBe` sort around
            length loaded `shouldBe` length (nub loaded)

    it "does not regenerate a resident chunk on a repeated request" $ \env → do
        withArena env $ \pid ws _ → do
            let c = ChunkCoord 3 0
            goToChunk env c
            lc0 ← awaitChunk ws c
            -- a recognisable mark that a regeneration would erase
            let marked = lc0 { lcSideDeco = VU.replicate 256 7 }
            atomicModifyIORef' (wsTilesRef ws) $ \td →
                (td { wtdChunks = HM.insert c marked (wtdChunks td) }, ())
            queueChunks pid ws [c]
            queueChunks pid ws [c]
            awaitDrained ws
            residentChunk ws c `shouldReturn` Just marked

-- | Eviction and readmission, edited and unedited.
evictionSpec ∷ SpecWith EngineEnv
evictionSpec = describe "eviction and readmission" $ do

    it "regenerates an evicted chunk identically" $ \env → do
        withArena env $ \_ ws params → do
            let near = ChunkCoord 3 0
                away = ChunkCoord 30 30
            tightBudget ws
            goToChunk env near
            before ← awaitChunk ws near
            goToChunk env away
            _ ← awaitChunk ws away
            gone ← waitUntil 30 (isNothing ⊚ residentChunk ws near)
            gone `shouldBe` True
            goToChunk env near
            after ← awaitChunk ws near
            after `shouldBe` before
            after `shouldBe` generateArenaChunk (wgpSeed params) near

    it "replays an edit on an expanded chunk after eviction and changes nothing else" $ \env → do
        withArena env $ \pid ws params → do
            let near = ChunkCoord 3 0
                away = ChunkCoord 30 30
                (gx, gy) = (56, 8)       -- inside chunk (3, 0)
                MaterialId graniteId = matGranite
                (_, (lx, ly)) = globalToChunk gx gy
                idx = ly * 16 + lx
            tightBudget ws
            goToChunk env near
            _ ← awaitChunk ws near
            sendWorldCommand env (WorldAddTile pid gx gy matGranite)
            edited ← waitUntil 30 $
                (maybe False (\lc → lcSurfaceMap lc VU.! idx ≡ seaLevel + 1)) ⊚ residentChunk ws near
            edited `shouldBe` True
            goToChunk env away
            _ ← awaitChunk ws away
            gone ← waitUntil 30 (isNothing ⊚ residentChunk ws near)
            gone `shouldBe` True
            goToChunk env near
            back ← awaitChunk ws near
            let base = generateArenaChunk (wgpSeed params) near
            -- the edit survived the eviction ...
            lcSurfaceMap back VU.! idx `shouldBe` seaLevel + 1
            VU.last (ctMats (lcTiles back V.! idx)) `shouldBe` graniteId
            -- ... and every other column is the untouched base
            [ i | i ← [0 .. 255], i ≢ idx, lcTiles back V.! i ≢ lcTiles base V.! i ]
                `shouldBe` []

-- | Staged restore, and load followed by explicit expansion.
restoreSpec ∷ SpecWith EngineEnv
restoreSpec = describe "staged restore" $ do

    it "rebuilds the footprint identically to startup and keeps edits outside it pending" $ \env → do
        withArena env $ \_ startupWs params → do
            startupTiles ← wtdChunks ⊚ readIORef (wsTilesRef startupWs)
            staged ← stageArena env params outsideEdit
            tiles ← wtdChunks ⊚ readIORef (wsTilesRef (stagedWorldState staged))
            sort (HM.keys tiles) `shouldBe` sort arenaFootprint
            forM_ (HM.toList tiles) $ \(c, lc) → do
                baseProperties c lc `shouldBe` []
                Just lc `shouldBe` HM.lookup c startupTiles
            -- the edit outside the footprint is carried by the overlay,
            -- not applied to a chunk that is not resident
            edits ← readIORef (wsEditsRef (stagedWorldState staged))
            HM.member (fst (globalToChunk 56 8)) edits `shouldBe` True

    it "applies a footprint edit on the rebuilt base" $ \env → do
        withArena env $ \_ _ params → do
            staged ← stageArena env params (appendEdit (ChunkCoord 0 0)
                                                (WeAddTile 6 6 matGranite) emptyWorldEdits)
            tiles ← wtdChunks ⊚ readIORef (wsTilesRef (stagedWorldState staged))
            let lc = tiles HM.! ChunkCoord 0 0
                (_, (lx, ly)) = globalToChunk 6 6
                idx = ly * 16 + lx
            lcSurfaceMap lc VU.! idx `shouldBe` seaLevel + 1
            [ i | i ← [0 .. 255], i ≢ idx
                , lcTiles lc V.! i ≢ lcTiles (generateArenaChunk 0 (ChunkCoord 0 0)) V.! i ]
                `shouldBe` []

    it "after a load, explicit expansion beyond the footprint succeeds and replays the saved edit" $ \env → do
        -- a published load leaves the engine PAUSED; later examples need
        -- the world thread to keep ticking
        (`finally` writeIORef (enginePausedRef env) False) $
          withArena env $ \_ _ params → do
            staged ← stageArena env params outsideEdit
            logger ← quietLog
            publishStagedSession env logger 707 (stagedSession staged)
            -- the loaded session has its page; ask for the chunk the edit lives in
            let pid = WorldPageId stagedName
                outside = fst (globalToChunk 56 8)
                (_, (lx, ly)) = globalToChunk 56 8
                idx = ly * 16 + lx
                ws = stagedWorldState staged
            queueChunks pid ws [outside, ChunkCoord (-5) 7]
            lc ← awaitChunk ws outside
            lcSurfaceMap lc VU.! idx `shouldBe` seaLevel + 1
            other ← awaitChunk ws (ChunkCoord (-5) 7)
            other `shouldBe` generateArenaChunk (wgpSeed params) (ChunkCoord (-5) 7)

-- | A real LEGACY arena save: captured on the build that existed before
--   the arena generation was unified (see the manifest provenance of
--   @z14-arena-legacy-shallow@ for the exact commit and commands). Its
--   chunks outside the footprint, and its footprint after a camera
--   round trip, were one loam cell per column; the save keeps none of
--   that, only the page's parameters and its edit overlay (an edit at
--   (6, 6) inside the footprint and one at (56, 8) on the expanded chunk
--   (3, 0)). Loading must reconstruct everything on the uniform
--   foundation and replay both edits (owner decision D-56), without
--   touching the file.
legacySpec ∷ SpecWith EngineEnv
legacySpec = describe "a legacy arena save" $ do

    it "adopts the sixteen-cell foundation for every chunk and replays its edits on top" $ \env → do
        bytes ← BS.readFile legacyFixture
        snap ← decodeLegacy bytes
        let pid = legacyPage
            edit6 = globalToChunk 6 6
            edit56 = globalToChunk 56 8
        -- the fixture really does carry both edits, one outside the footprint
        let edits = maybe emptyWorldEdits pgsEdits (HM.lookup pid (snapPages snap))
        HM.member (fst edit6) edits `shouldBe` True
        HM.member (fst edit56) edits `shouldBe` True
        fst edit56 `elem` arenaFootprint `shouldBe` False
        staged ← stageSnapshot env snap pid
        tiles ← wtdChunks ⊚ readIORef (wsTilesRef (stagedWorldState staged))
        sort (HM.keys tiles) `shouldBe` sort arenaFootprint
        forM_ (HM.toList tiles) $ \(c, lc) → do
            let base = generateArenaChunk 0 c
                (_, (lx, ly)) = edit6
                idx = ly * 16 + lx
            if c ≡ fst edit6
                then do
                    -- the edit is replayed on the sixteen-cell base ...
                    lcSurfaceMap lc VU.! idx `shouldBe` seaLevel + 1
                    V.length (lcTiles lc) `shouldBe` 256
                    -- ... and nothing else of the chunk changed
                    [ i | i ← [0 .. 255], i ≢ idx, lcTiles lc V.! i ≢ lcTiles base V.! i ]
                        `shouldBe` []
                else do
                    baseProperties c lc `shouldBe` []
                    lc `shouldBe` base
        -- the chunk the old build expanded to is reconstructed on demand,
        -- with its edit replayed on the same foundation
        logger ← quietLog
        (`finally` writeIORef (enginePausedRef env) False) $ do
            publishStagedSession env logger 7071 (stagedSession staged)
            let ws = stagedWorldState staged
                (_, (lx, ly)) = edit56
                idx = ly * 16 + lx
            queueChunks pid ws [fst edit56]
            lc ← awaitChunk ws (fst edit56)
            lcSurfaceMap lc VU.! idx `shouldBe` seaLevel + 1
            [ i | i ← [0 .. 255], i ≢ idx
                , lcTiles lc V.! i ≢ lcTiles (generateArenaChunk 0 (fst edit56)) V.! i ]
                `shouldBe` []
            -- every unedited column has the sixteen cells, not the legacy
            -- single one; the edited column is those sixteen plus its edit
            [ VU.length (ctMats col) | col ← V.toList (lcTiles lc) ]
                `shouldBe` [ if i ≡ idx then 17 else 16 | i ← [0 .. 255 ∷ Int] ]
        -- loading read the file; it never rewrote it
        BS.readFile legacyFixture `shouldReturn` bytes

legacyFixture ∷ FilePath
legacyFixture = "test-headless/data/save-compat/z14-arena-legacy-shallow.bin"

legacyPage ∷ WorldPageId
legacyPage = WorldPageId "legacy_arena"

decodeLegacy ∷ BS.ByteString → IO SessionSnapshot
decodeLegacy bytes =
    let names = HS.fromList ["unit_ai", "building_spawn"]
    in case decodeSessionEnvelope names names bytes of
        Left err → expectationFailure (T.unpack err) ≫ error "unreachable"
        Right (_, snap, _, _) → pure snap

-- * Staging helpers

stagedName ∷ T.Text
stagedName = "arena_consistency_staged"

-- | A saved edit on a chunk OUTSIDE the startup footprint.
outsideEdit ∷ WorldEdits
outsideEdit = appendEdit (fst (globalToChunk 56 8)) (WeAddTile 56 8 matGranite) emptyWorldEdits

-- | What staging hands back, with the whole session for publishing.
data StagedArena = StagedArena
    { saPage ∷ StagedPage, saSession ∷ StagedSession }

stagedWorldState ∷ StagedArena → WorldState
stagedWorldState = spWorldState ∘ saPage

stagedSession ∷ StagedArena → StagedSession
stagedSession = saSession

quietLog ∷ IO LoggerState
quietLog = initLogger defaultLogConfig { lcBackend = LogToCallback (\_ → pure ()) }

-- | Stage one in-process arena snapshot through the production staging
--   entry point.
stageArena ∷ EngineEnv → WorldGenParams → WorldEdits → IO StagedArena
stageArena env params edits = do
    logger ← quietLog
    let pid = WorldPageId stagedName
        page = (blankPageSnapshot pid params { wgpSeed = 0 }) { pgsEdits = edits }
        snap = SessionSnapshot
            { snapGameTime = 0, snapTexPalette = emptyTexPalette
            , snapNextItemId = 1, snapNextBuildingId = 1, snapNextUnitId = 1
            , snapActivePage = pid, snapVisiblePages = [pid]
            , snapLiveCamera = LiveCameraSnapshot (Just pid) 0 0 1 FaceSouth
            , snapPages = HM.singleton pid page
            , snapItemTags = mempty, snapItemRefs = mempty
            , snapPortableKnowledge = emptyPortableKnowledge }
        saveData = snapshotToSaveData
            (SaveRequestMeta "arena_consistency" "2026-10-18T00:00:00.000000Z" False) snap
    stageSaveData env logger pid saveData

-- | Stage a decoded snapshot (a real save's content) through the
--   production staging entry point.
stageSnapshot ∷ EngineEnv → SessionSnapshot → WorldPageId → IO StagedArena
stageSnapshot env snap pid = do
    logger ← quietLog
    stageSaveData env logger pid
        (snapshotToSaveData
            (SaveRequestMeta "arena_consistency" "2026-10-18T00:00:00.000000Z" False) snap)

stageSaveData ∷ EngineEnv → LoggerState → WorldPageId → SaveData → IO StagedArena
stageSaveData env logger pid saveData = do
    matReg ← readIORef (materialRegistryRef env)
    staged ← stageSession env logger saveData matReg ≫= \r → case r of
        Left e → expectationFailure (T.unpack (renderStageError e)) ≫ error "unreachable"
        Right s → pure s
    case filter ((≡ pid) ∘ spPageId) (ssPages staged) of
        (p : _) → pure (StagedArena p staged)
        [] → expectationFailure "the staged arena page is missing" ≫ error "unreachable"
