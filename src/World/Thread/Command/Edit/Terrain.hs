{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Terrain-column edit handlers (add/delete a Z-level, walkable-ramp
--   slope, arbitrary single-cell writes). Split out of
--   "World.Thread.Command.Edit" (issue #563).
module World.Thread.Command.Edit.Terrain
    ( handleWorldAddTileCommand
    , handleWorldDeleteTileCommand
    , handleWorldSetSlopeCommand
    , handleWorldSetCellCommand
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Unit.Command.Types (UnitCommand(..))
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import World.Types
import World.Generate.Coordinates (globalToChunk)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Edit.Apply (applyEdit)
import World.Material.Id (MaterialId(..))
import World.Thread.Command.Edit.Sync (syncEditToSim)
import World.Thread.Helpers (unWorldPageId)

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
