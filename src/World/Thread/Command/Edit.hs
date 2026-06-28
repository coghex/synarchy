{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Command.Edit
    ( handleWorldAddTileCommand
    , handleWorldDeleteTileCommand
    , handleWorldSetFluidTileCommand
    , handleWorldSetSlopeCommand
    , handleWorldSetCellCommand
    , handleWorldSetStructureCommand
    , handleWorldClearStructureCommand
    , handleWorldClearAllStructuresCommand
    , handleWorldDigTileCommand
    ) where

import UPrelude
import Data.Word (Word8)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..), freshItemInstanceId)
import Sim.Command.Types (SimCommand(..))
import Unit.Command.Types (UnitCommand(..))
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import World.Types
import World.Chunk.Types (LoadedChunk(..), ColumnTiles(..), columnIndex)
import World.Fluid.Types (FluidType(..))
import World.Tile.Types (lookupChunk, insertChunk)
import World.Generate.Coordinates (globalToChunk)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Edit.Apply (applyEdit)
import Structure.Types (emptyChunkStructures)
import World.Material (MaterialProps(..), getMaterialProps
                      , materialIdByName)
import World.Material.Id (MaterialId(..))
import World.Mine.Apply (applyDigSlopeToChunk)
import World.Mine.Types (MineDesignation(..), drainCorners, cornersDone)
import World.Gem (gemChanceAt)
import World.Spoil.Logic (spoilTileOk, spoilStartVertex)
import Item.Ground (GroundItem(..), GroundItems(..), spawnGroundItem)
import Item.Roll (rollItemSpec, rollItemWeight)
import Item.Types (ItemDef(..), ItemInstance(..), lookupItemDef)
import System.Random (randomR)
import World.Spoil.Types (SpoilPile(..), spoilCapacity, depositSpoil
                         , candidateVertices, promotableTiles
                         , debitPromotedTile, tileCornerVertices)
import World.Thread.Helpers (unWorldPageId)

-- | After a live terrain/fluid edit lands in the chunk, re-seed that
--   chunk's sim state from the now-authoritative tiles AND activate it so
--   the new fluid actually flows. Without the sync the sim runs against
--   pre-edit fluid/terrain and writes its stale result back over the edit;
--   without the activation (the old SimChunkLoaded path) the edited chunk
--   kept the new snapshot but sat frozen because the volume sim only
--   advances active chunks (#60).
syncEditToSim ∷ EngineEnv → WorldPageId → LoadedChunk → IO ()
syncEditToSim env pageId lc =
    Q.writeQueue (simQueue env) $
        SimChunkEdited pageId (lcCoord lc)
            (lcFluidMap lc) (lcTerrainSurfaceMap lc)

-- | Dig the top of the column at (gx, gy) down by 1 Z.
--   Records the edit in the world's edit log so it survives chunk
--   eviction + save/load. The in-memory chunk is mutated via the
--   same `applyEdit` function that replays the log on chunk reload —
--   single source of truth.
handleWorldDeleteTileCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldDeleteTileCommand env logger pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for delete tile: " <> unWorldPageId pageId
        Just ws → do
            let (coord, (lx, ly)) = globalToChunk gx gy
                idx = columnIndex lx ly
                edit = WeDeleteTile gx gy
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing →
                    logWarn logger CatWorld $
                        "Chunk not loaded for delete tile at "
                          <> T.pack (show gx) <> "," <> T.pack (show gy)
                Just lc → do
                    -- Pre-check the column bounds so we can warn on
                    -- invalid edits before appending. applyEdit is
                    -- silent on out-of-bounds (it has to be — replay
                    -- has no logger), but live edits should fail loud.
                    let oldTopZ = lcSurfaceMap lc VU.! idx
                        col     = lcTiles lc V.! idx
                        colLen  = VU.length (ctMats col)
                        i       = oldTopZ - ctStartZ col
                    if i < 0 ∨ i ≥ colLen
                      then logWarn logger CatWorld $
                             "Delete tile out of column range at "
                               <> T.pack (show gx) <> "," <> T.pack (show gy)
                               <> " topZ=" <> T.pack (show oldTopZ)
                               <> " startZ=" <> T.pack (show (ctStartZ col))
                               <> " len=" <> T.pack (show colLen)
                      else do
                        let lc' = applyEdit edit lc
                        atomicModifyIORef' (wsTilesRef ws) $ \w →
                            (insertChunk lc' w, ())
                        atomicModifyIORef' (wsEditsRef ws) $ \es →
                            (appendEdit coord edit es, ())
                        -- Keep the sim's chunk in step with the new terrain.
                        syncEditToSim env pageId lc'
                        -- Invalidate all three render caches so the next
                        -- tick rebuilds quads from the modified chunk.
                        bumpQuadCacheGen ws
                        writeIORef (wsZoomQuadCacheRef ws) Nothing
                        writeIORef (wsBgQuadCacheRef ws)   Nothing
                        -- Re-snap any idle unit standing on this tile to
                        -- the new surface (otherwise it floats mid-air
                        -- over the hole — stationary units never
                        -- re-ground on their own).
                        Q.writeQueue (unitQueue env) (UnitReGround gx gy)
                        logDebug logger CatWorld $
                            "Deleted tile at " <> T.pack (show gx) <> ","
                              <> T.pack (show gy) <> " z=" <> T.pack (show oldTopZ)

-- | Raise the column at (gx, gy) one z of the given material.
--   Records the edit in the log (same WeAddTile path spoil promotion
--   uses) so it survives chunk eviction + save/load. Debug terrain
--   placement is the primary caller (world.addTile from the debug
--   overlay's Terrain section).
handleWorldAddTileCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → MaterialId → IO ()
handleWorldAddTileCommand env logger pageId gx gy mat = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for add tile: " <> unWorldPageId pageId
        Just ws → do
            let (coord, (lx, ly)) = globalToChunk gx gy
                idx  = columnIndex lx ly
                edit = WeAddTile gx gy mat
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing →
                    logWarn logger CatWorld $
                        "Chunk not loaded for add tile at "
                          <> T.pack (show gx) <> "," <> T.pack (show gy)
                Just lc → do
                    -- Loud bounds pre-check, mirroring delete: replay
                    -- is silent on out-of-range, live edits warn.
                    -- i == colLen is fine — applyEdit grows the
                    -- column by one cell (no headroom is the normal
                    -- allocation).
                    let oldTopZ = lcTerrainSurfaceMap lc VU.! idx
                        col     = lcTiles lc V.! idx
                        colLen  = VU.length (ctMats col)
                        i       = oldTopZ + 1 - ctStartZ col
                    if i < 0 ∨ i > colLen
                      then logWarn logger CatWorld $
                             "Add tile out of column range at "
                               <> T.pack (show gx) <> "," <> T.pack (show gy)
                               <> " topZ=" <> T.pack (show oldTopZ)
                      else do
                        let lc' = applyEdit edit lc
                        atomicModifyIORef' (wsTilesRef ws) $ \w →
                            (insertChunk lc' w, ())
                        atomicModifyIORef' (wsEditsRef ws) $ \es →
                            (appendEdit coord edit es, ())
                        syncEditToSim env pageId lc'
                        bumpQuadCacheGen ws
                        writeIORef (wsZoomQuadCacheRef ws) Nothing
                        writeIORef (wsBgQuadCacheRef ws)   Nothing
                        -- Units standing on the tile ride up.
                        Q.writeQueue (unitQueue env) (UnitReGround gx gy)
                        logDebug logger CatWorld $
                            "Added tile at " <> T.pack (show gx) <> ","
                              <> T.pack (show gy)
                              <> " mat=" <> T.pack (show mat)

-- | Place a structure piece (floor/wall/post/ceiling) at (gx,gy,slot-tag) via
--   the WeSetStructure edit path: live-apply to the loaded chunk's structure
--   overlay AND append to the per-chunk edit log, so it persists + replays on
--   eviction. Palette ids (texture/facemap) are resolved Lua-side; the cap
--   variant is already baked into facePaletteId (the BUILDER chose it). No
--   terrain is touched — but it shares the ordered log with terrain edits, so
--   a dig recorded before this lands before it on replay.
handleWorldSetStructureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Word8 → Int → Int → Int → IO ()
handleWorldSetStructureCommand env logger pageId gx gy slotTag texId faceId z = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for set structure: " <> unWorldPageId pageId
        Just ws → do
            let (coord, _) = globalToChunk gx gy
                edit = WeSetStructure gx gy slotTag texId faceId z
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing →
                    logWarn logger CatWorld $
                        "Chunk not loaded for set structure at "
                          <> T.pack (show gx) <> "," <> T.pack (show gy)
                Just lc → do
                    let lc' = applyEdit edit lc
                    atomicModifyIORef' (wsTilesRef ws) $ \w →
                        (insertChunk lc' w, ())
                    atomicModifyIORef' (wsEditsRef ws) $ \es →
                        (appendEdit coord edit es, ())

-- | Remove the structure piece at (gx,gy,slot-tag) via WeClearStructure.
--   Unlike the SET path, the clear is recorded in the per-chunk edit log
--   ALWAYS — even when the chunk isn't loaded. The piece being cleared may
--   live only in the persisted edits of an UNLOADED/evicted chunk (its
--   WeSetStructure), so without recording the clear it would replay back on
--   reload / after save/load. The live lcStructures overlay is additionally
--   updated when the chunk happens to be loaded. (Replaying a clear with no
--   matching set is a harmless no-op — a HM.delete on an absent key.)
handleWorldClearStructureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Word8 → IO ()
handleWorldClearStructureCommand env logger pageId gx gy slotTag = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for clear structure: " <> unWorldPageId pageId
        Just ws → do
            let (coord, _) = globalToChunk gx gy
                edit = WeClearStructure gx gy slotTag
            atomicModifyIORef' (wsEditsRef ws) $ \es →
                (appendEdit coord edit es, ())
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing → pure ()
                Just lc → do
                    let lc' = applyEdit edit lc
                    atomicModifyIORef' (wsTilesRef ws) $ \w →
                        (insertChunk lc' w, ())

-- | Remove EVERY structure piece in the world. Clears the live per-chunk
--   'lcStructures' overlay on all loaded chunks AND strips the structure
--   edits (WeSetStructure / WeClearStructure) from the per-chunk log so they
--   do not replay on eviction/reload. This is the authoritative "wipe all":
--   it touches the same overlay + edit-log that rendering and persistence
--   read, so a cleared world stays cleared after a chunk evicts or a
--   save/load round-trip. (No quad-cache bust: the structure pass renders
--   from 'lcStructures' live every frame, never from the cached terrain quads.)
handleWorldClearAllStructuresCommand ∷ EngineEnv → LoggerState → WorldPageId
    → IO ()
handleWorldClearAllStructuresCommand env logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for clear all structures: " <> unWorldPageId pageId
        Just ws → do
            atomicModifyIORef' (wsTilesRef ws) $ \w →
                ( w { wtdChunks = HM.map clearChunkStructures (wtdChunks w) }
                , () )
            atomicModifyIORef' (wsEditsRef ws) $ \es →
                (HM.map (filter (not . isStructureEdit)) es, ())
  where
    clearChunkStructures lc = lc { lcStructures = emptyChunkStructures }
    isStructureEdit (WeSetStructure {})   = True
    isStructureEdit (WeClearStructure {}) = True
    isStructureEdit _                     = False

-- | Set the walkable-ramp slope bitmask of an existing tile at (gx,gy,z).
--   Routes through the edit log (WeSetSlope) like every other edit, so a
--   ramp authored by a test harness survives chunk eviction + save/load.
--   No generator emits this — addTile only ever produces flat tops
--   (slope = 0), so authoring a walkable ramp needs an explicit set.
handleWorldSetSlopeCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Int → Word8 → IO ()
handleWorldSetSlopeCommand env logger pageId gx gy z bits = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for set slope: " <> unWorldPageId pageId
        Just ws → do
            let (coord, (lx, ly)) = globalToChunk gx gy
                idx  = columnIndex lx ly
                edit = WeSetSlope gx gy z bits
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing →
                    logWarn logger CatWorld $
                        "Chunk not loaded for set slope at "
                          <> T.pack (show gx) <> "," <> T.pack (show gy)
                Just lc → do
                    let col = lcTiles lc V.! idx
                        i   = z - ctStartZ col
                    if i < 0 ∨ i ≥ VU.length (ctSlopes col)
                      then logWarn logger CatWorld $
                             "Set slope z out of column range at "
                               <> T.pack (show gx) <> "," <> T.pack (show gy)
                               <> " z=" <> T.pack (show z)
                      else do
                        let lc' = applyEdit edit lc
                        atomicModifyIORef' (wsTilesRef ws) $ \w →
                            (insertChunk lc' w, ())
                        atomicModifyIORef' (wsEditsRef ws) $ \es →
                            (appendEdit coord edit es, ())
                        -- No syncEditToSim here: a slope bitmask is pure
                        -- walkability — it changes neither the surface
                        -- elevation nor fluid the sim reads, so there is no
                        -- stale sim state to refresh (#60), and re-seeding
                        -- would needlessly reset an in-flight fluid sim.
                        bumpQuadCacheGen ws
                        writeIORef (wsZoomQuadCacheRef ws) Nothing
                        writeIORef (wsBgQuadCacheRef ws)   Nothing
                        logDebug logger CatWorld $
                            "Set slope at " <> T.pack (show gx) <> ","
                              <> T.pack (show gy) <> " z=" <> T.pack (show z)
                              <> " bits=" <> T.pack (show bits)

-- | Set a single 3D cell at (gx, gy, z) to a material (id 0 = air) via
--   the WeSetCell edit path — the locations primitive for carving
--   interior air, walls, ceilings, and staircases. Routes through the
--   edit log so the geometry survives chunk eviction + save/load, exactly
--   like every other edit. applyEdit grows the column upward to reach z;
--   a z below the column floor is silently dropped there (replay is
--   silent on out-of-range), so this handler warns loudly on it.
handleWorldSetCellCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Int → MaterialId → IO ()
handleWorldSetCellCommand env logger pageId gx gy z mat = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for set cell: " <> unWorldPageId pageId
        Just ws → do
            let (coord, (lx, ly)) = globalToChunk gx gy
                idx  = columnIndex lx ly
                edit = WeSetCell gx gy z mat
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing →
                    logWarn logger CatWorld $
                        "Chunk not loaded for set cell at "
                          <> T.pack (show gx) <> "," <> T.pack (show gy)
                Just lc → do
                    let col = lcTiles lc V.! idx
                        i   = z - ctStartZ col
                    if i < 0
                      then logWarn logger CatWorld $
                             "Set cell z below column floor at "
                               <> T.pack (show gx) <> "," <> T.pack (show gy)
                               <> " z=" <> T.pack (show z)
                               <> " startZ=" <> T.pack (show (ctStartZ col))
                      else do
                        let lc' = applyEdit edit lc
                        atomicModifyIORef' (wsTilesRef ws) $ \w →
                            (insertChunk lc' w, ())
                        atomicModifyIORef' (wsEditsRef ws) $ \es →
                            (appendEdit coord edit es, ())
                        syncEditToSim env pageId lc'
                        bumpQuadCacheGen ws
                        writeIORef (wsZoomQuadCacheRef ws) Nothing
                        writeIORef (wsBgQuadCacheRef ws)   Nothing
                        -- A surface-changing cell write (e.g. a staircase
                        -- mouth) leaves units floating; re-ground them.
                        Q.writeQueue (unitQueue env) (UnitReGround gx gy)
                        logDebug logger CatWorld $
                            "Set cell at " <> T.pack (show gx) <> ","
                              <> T.pack (show gy) <> " z=" <> T.pack (show z)
                              <> " mat=" <> T.pack (show mat)

-- | Apply dig progress to the designated tile at (gx, gy).
--
--   The digger's position picks the drain order (digger-side corners
--   first — 'drainCorners'); the partial state writes its slope-mask
--   override into the loaded chunk ('applyDigSlopeToChunk') so the
--   tile renders progressively excavated. When every corner reaches
--   zero, the tile drops one z through the regular delete-tile path
--   (edit log + replay + save survival all included) and the
--   designation is removed.
--
--   Spoil: when the dug material declares dig_spoil, the excavated
--   volume × dig_bulking is routed into the vertex piles around the
--   dig (World.Spoil). If the surrounding piles can't absorb the
--   tick's spoil, the dig REFUSES (no drain — material never
--   vanishes; the AI sees the blocked flag via getDigInfoAt). Tiles
--   whose four corners complete a full pile level are promoted to
--   real terrain through the WeAddTile edit path.
--
--   No-ops when the tile isn't designated (e.g. two diggers raced and
--   one finished it) or its chunk isn't loaded.
handleWorldDigTileCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Float → Float → Float → Float → Float → IO ()
handleWorldDigTileCommand env logger pageId gx gy ux uy amount skill percep = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for dig tile: " <> unWorldPageId pageId
        Just ws → do
            desigs ← readIORef (wsMineDesignationsRef ws)
            case HM.lookup (gx, gy) desigs of
                Nothing → pure ()
                Just md → do
                    td0 ← readIORef (wsTilesRef ws)
                    registry ← readIORef (materialRegistryRef env)
                    piles ← readIORef (wsSpoilRef ws)
                    let oldCorners = mdCorners md
                        sumC (a, b, c, d) = a + b + c + d
                        -- Properties of the dug material (the
                        -- column's cell at the designation z).
                        mDigProps = do
                            lc ← lookupChunk digCoord td0
                            let col  = lcTiles lc V.! digIdx
                                relZ = mdZ md - ctStartZ col
                            matId ← if relZ ≥ 0
                                       ∧ relZ < VU.length (ctMats col)
                                    then Just (ctMats col VU.! relZ)
                                    else Nothing
                            pure (getMaterialProps registry
                                      (MaterialId matId))
                        mSpoil = do
                            props ← mDigProps
                            spoilName ← mpDigSpoil props
                            spoilId ← materialIdByName registry spoilName
                            pure (spoilId, mpDigBulking props)
                        mChunkItem = mDigProps >>= mpDigChunk
                        tileOk = spoilTileOk td0 desigs (mdZ md)
                        startV = spoilStartVertex (ux, uy) (gx, gy)
                        -- Refusal gate: this tick's worst-case spoil
                        -- must fit before anything drains.
                        plannedSpoil = case mSpoil of
                            Nothing → 0
                            Just (_, bulking) →
                                min amount (sumC oldCorners) * bulking
                        capacity = case mSpoil of
                            Nothing → 0
                            Just (spoilId, _) →
                                spoilCapacity tileOk spoilId startV piles
                        blocked = plannedSpoil > 0 ∧ capacity < plannedSpoil
                    if blocked
                      then logDebug logger CatWorld $
                             "Dig blocked (no spoil room) at "
                               <> T.pack (show gx) <> "," <> T.pack (show gy)
                      else do
                        let corners' = drainCorners (ux, uy) (gx, gy)
                                                    amount oldCorners
                            drained  = sumC oldCorners - sumC corners'
                        -- Route the spoil before the tile mutates so
                        -- the legality predicate sees the pre-dig
                        -- world (the dig tile is excluded by its own
                        -- designation either way).
                        case mSpoil of
                            Nothing → pure ()
                            Just (spoilId, bulking) | drained > 0 → do
                                let (piles', leftover) = depositSpoil
                                        tileOk spoilId startV
                                        (drained * bulking) piles
                                when (leftover > 0.001) $
                                    logWarn logger CatWorld $
                                        "Spoil leftover "
                                          <> T.pack (show leftover)
                                          <> " despite capacity check at "
                                          <> T.pack (show gx) <> ","
                                          <> T.pack (show gy)
                                writeIORef (wsSpoilRef ws) piles'
                                -- Promote any tile whose corners
                                -- completed a full level.
                                promoteFullSpoilTiles env logger pageId
                                    ws startV
                            _ → pure ()
                        -- Chunk-yield accumulator: deterministic, per
                        -- tile, scaled by the CURRENT digger's mining
                        -- skill each tick (0.5 + skill/100 chunks per
                        -- full tile = 4 corner-units). Whole chunks
                        -- spawn as ground items at the dig site; the
                        -- fractional remainder rides on the
                        -- designation (and dies with it — one tile
                        -- only provides what was extracted from it).
                        chunkRemainder ← case mChunkItem of
                            Nothing → pure (mdChunkProgress md)
                            Just chunkDef | drained > 0 → do
                                let rate = (0.5 + skill / 100) / 4
                                    p    = mdChunkProgress md
                                         + drained * rate
                                    n    = floor p ∷ Int
                                when (n > 0) $
                                    spawnYieldItems env logger ws
                                        chunkDef (gx, gy) n
                                pure (p - fromIntegral n)
                            _ → pure (mdChunkProgress md)
                        if cornersDone corners'
                          then do
                            -- Gem roll, once per COMPLETED tile: the
                            -- seeded region field says which gem (if
                            -- any) this area hosts and how rich it
                            -- runs; the finishing digger's PERCEPTION
                            -- scales the find chance (spotting the
                            -- glint — deliberately not mining skill).
                            when (maybe False mpDigGems mDigProps) $ do
                                paramsM ← readIORef (wsGenParamsRef ws)
                                let seed = maybe 0 (fromIntegral ∘ wgpSeed)
                                                 paramsM
                                case gemChanceAt seed (gx, gy) percep of
                                    Nothing → pure ()
                                    Just (gemDef, chance) → do
                                        roll ← atomicModifyIORef'
                                            (statRNGRef env) $ \g →
                                            let (v, g') = randomR
                                                    (0, 1 ∷ Float) g
                                            in (g', v)
                                        when (roll < chance) $
                                            spawnYieldItems env logger ws
                                                gemDef (gx, gy) 1
                            atomicModifyIORef' (wsMineDesignationsRef ws) $ \m →
                                (HM.delete (gx, gy) m, ())
                            handleWorldDeleteTileCommand env logger pageId gx gy
                          else do
                            let md' = md { mdCorners = corners'
                                         , mdChunkProgress = chunkRemainder }
                            atomicModifyIORef' (wsMineDesignationsRef ws) $ \m →
                                (HM.insert (gx, gy) md' m, ())
                            td ← readIORef (wsTilesRef ws)
                            case lookupChunk digCoord td of
                                Nothing → pure ()
                                Just lc → do
                                    let lc' = applyDigSlopeToChunk (gx, gy) md' lc
                                    atomicModifyIORef' (wsTilesRef ws) $ \w →
                                        (insertChunk lc' w, ())
                                    bumpQuadCacheGen ws
                                    writeIORef (wsZoomQuadCacheRef ws) Nothing
                                    writeIORef (wsBgQuadCacheRef ws)   Nothing
  where
    (digCoord, (digLx, digLy)) = globalToChunk gx gy
    digIdx = columnIndex digLx digLy

-- | Spawn @n@ yield items (chunks, gems) as ground items scattered
--   on the dig tile. Each gets a random sub-tile position, retried a
--   few times to keep ≥ 0.15 tiles from existing ground items so
--   finds lay out as a scatter instead of a stack. Quality/condition
--   roll from the item def's spec like any other instance.
spawnYieldItems ∷ EngineEnv → LoggerState → WorldState → Text
                → (Int, Int) → Int → IO ()
spawnYieldItems env logger ws defName (gx, gy) n = do
    itemMgr ← readIORef (itemManagerRef env)
    case lookupItemDef defName itemMgr of
        Nothing →
            logWarn logger CatWorld $
                "Dig yield: unknown item def '" <> defName
                  <> "' — dropping " <> T.pack (show n)
        Just iDef → forM_ [1 .. n] $ \_ → do
            qual ← rollItemSpec (idQualitySpec iDef)   (statRNGRef env)
            cond ← rollItemSpec (idConditionSpec iDef) (statRNGRef env)
            wght ← rollItemWeight iDef (statRNGRef env)
            iid ← freshItemInstanceId env
            let inst = ItemInstance
                    { iiDefName     = defName
                    , iiCurrentFill = 0
                    , iiQuality     = qual
                    , iiCondition   = cond
                    , iiWeight      = wght
                    , iiSharpness   = 100.0
                    , iiContents    = []
                    , iiInstanceId  = iid
                    }
            gis ← readIORef (wsGroundItemsRef ws)
            (px, py) ← pickScatterPos env gis
            _ ← atomicModifyIORef' (wsGroundItemsRef ws) $
                    spawnGroundItem inst px py
            pure ()
  where
    -- Up to 6 candidate offsets inside the tile; first one clear of
    -- existing items wins, last candidate is the fallback.
    pickScatterPos env' gis = go (6 ∷ Int)
      where
        clearOf (px, py) = all (\gi →
            let dx = giX gi - px
                dy = giY gi - py
            in dx * dx + dy * dy ≥ 0.15 * 0.15)
            (HM.elems (gisItems gis))
        go k = do
            ox ← atomicModifyIORef' (statRNGRef env') $ \g →
                let (v, g') = randomR (0.15, 0.85 ∷ Float) g in (g', v)
            oy ← atomicModifyIORef' (statRNGRef env') $ \g →
                let (v, g') = randomR (0.15, 0.85 ∷ Float) g in (g', v)
            let pos = (fromIntegral gx + ox, fromIntegral gy + oy)
            if k ≤ 1 ∨ clearOf pos
              then pure pos
              else go (k - 1)

-- | Compact every spoil tile around @startV@ whose four corners hold
--   a full level: raise the terrain one z via the WeAddTile edit
--   (live mutation + log append, same single-source applyEdit as
--   delete) and debit the contributing piles. Loops because a debit
--   never re-fills a corner — one pass per promoted tile is enough,
--   but promoting one tile can't complete another, so a single sweep
--   over the candidate set suffices.
promoteFullSpoilTiles ∷ EngineEnv → LoggerState → WorldPageId
    → WorldState → (Int, Int) → IO ()
promoteFullSpoilTiles env logger pageId ws startV = do
    piles ← readIORef (wsSpoilRef ws)
    registry ← readIORef (materialRegistryRef env)
    let ready = promotableTiles piles (candidateVertices startV)
    forM_ ready $ \tile@(tx, ty) → do
        ps ← readIORef (wsSpoilRef ws)
        -- Material of the promoted cell = the pile material at the
        -- tile's first corner. All four corners are guaranteed to share
        -- one material: slotUsable refuses to fill a tile's corner with
        -- a material that differs from spoil already on the tile.
        let mMat = listToMaybe
                [ spMat p
                | (v, _) ← tileCornerVertices tile
                , Just p ← [HM.lookup v ps] ]
        case mMat of
            Nothing → pure ()
            Just mat → do
                let (coord, _) = globalToChunk tx ty
                    edit = WeAddTile tx ty mat
                td ← readIORef (wsTilesRef ws)
                case lookupChunk coord td of
                    Nothing → pure ()
                    Just lc → do
                        let lc' = applyEdit edit lc
                        atomicModifyIORef' (wsTilesRef ws) $ \w →
                            (insertChunk lc' w, ())
                        atomicModifyIORef' (wsEditsRef ws) $ \es →
                            (appendEdit coord edit es, ())
                        atomicModifyIORef' (wsSpoilRef ws) $ \sp →
                            (debitPromotedTile tile sp, ())
                        bumpQuadCacheGen ws
                        writeIORef (wsZoomQuadCacheRef ws) Nothing
                        writeIORef (wsBgQuadCacheRef ws)   Nothing
                        -- Anything standing on the tile rides up.
                        Q.writeQueue (unitQueue env) (UnitReGround tx ty)
                        logDebug logger CatWorld $
                            "Spoil promoted to terrain at "
                              <> T.pack (show tx) <> "," <> T.pack (show ty)

-- | Place one tile of fluid on top of the column at (gx, gy). Records
--   the edit in the world's log; in-memory mutation uses the same
--   `applyEdit` helper.
handleWorldSetFluidTileCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → FluidType → IO ()
handleWorldSetFluidTileCommand env logger pageId gx gy fluidType = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing →
            logWarn logger CatWorld $
                "World not found for set fluid: " <> unWorldPageId pageId
        Just ws → do
            let (coord, _) = globalToChunk gx gy
                edit = WeSetFluidTile gx gy fluidType
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing →
                    logWarn logger CatWorld $
                        "Chunk not loaded for set fluid at "
                          <> T.pack (show gx) <> "," <> T.pack (show gy)
                Just lc → do
                    let lc' = applyEdit edit lc
                    atomicModifyIORef' (wsTilesRef ws) $ \w →
                        (insertChunk lc' w, ())
                    atomicModifyIORef' (wsEditsRef ws) $ \es →
                        (appendEdit coord edit es, ())
                    -- Re-seed the sim with the placed fluid so it flows /
                    -- settles instead of being overwritten by stale sim
                    -- output (#60).
                    syncEditToSim env pageId lc'
                    bumpQuadCacheGen ws
                    writeIORef (wsZoomQuadCacheRef ws) Nothing
                    writeIORef (wsBgQuadCacheRef ws)   Nothing
                    logDebug logger CatWorld $
                        "Placed fluid " <> T.pack (show fluidType)
                          <> " at " <> T.pack (show gx) <> ","
                          <> T.pack (show gy)
