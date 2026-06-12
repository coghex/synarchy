{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Command.Edit
    ( handleWorldDeleteTileCommand
    , handleWorldSetFluidTileCommand
    , handleWorldDigTileCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import qualified Engine.Core.Queue as Q
import Engine.Core.State (EngineEnv(..))
import Unit.Command.Types (UnitCommand(..))
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import World.Types
import World.Chunk.Types (LoadedChunk(..), ColumnTiles(..), columnIndex)
import World.Fluid.Types (FluidType(..))
import World.Tile.Types (lookupChunk, insertChunk)
import World.Generate.Coordinates (globalToChunk)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Edit.Apply (applyEdit)
import World.Mine.Apply (applyDigSlopeToChunk)
import World.Mine.Types (MineDesignation(..), drainCorners, cornersDone)
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
                        -- Invalidate all three render caches so the next
                        -- tick rebuilds quads from the modified chunk.
                        writeIORef (wsQuadCacheRef ws)     Nothing
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
--   No-ops when the tile isn't designated (e.g. two diggers raced and
--   one finished it) or its chunk isn't loaded.
handleWorldDigTileCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Float → Float → Float → IO ()
handleWorldDigTileCommand env logger pageId gx gy ux uy amount = do
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
                    let corners' = drainCorners (ux, uy) (gx, gy)
                                                amount (mdCorners md)
                    if cornersDone corners'
                      then do
                        atomicModifyIORef' (wsMineDesignationsRef ws) $ \m →
                            (HM.delete (gx, gy) m, ())
                        handleWorldDeleteTileCommand env logger pageId gx gy
                      else do
                        let md' = md { mdCorners = corners' }
                        atomicModifyIORef' (wsMineDesignationsRef ws) $ \m →
                            (HM.insert (gx, gy) md' m, ())
                        td ← readIORef (wsTilesRef ws)
                        let (coord, _) = globalToChunk gx gy
                        case lookupChunk coord td of
                            Nothing → pure ()
                            Just lc → do
                                let lc' = applyDigSlopeToChunk (gx, gy) md' lc
                                atomicModifyIORef' (wsTilesRef ws) $ \w →
                                    (insertChunk lc' w, ())
                                writeIORef (wsQuadCacheRef ws)     Nothing
                                writeIORef (wsZoomQuadCacheRef ws) Nothing
                                writeIORef (wsBgQuadCacheRef ws)   Nothing

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
                    writeIORef (wsQuadCacheRef ws)     Nothing
                    writeIORef (wsZoomQuadCacheRef ws) Nothing
                    writeIORef (wsBgQuadCacheRef ws)   Nothing
                    logDebug logger CatWorld $
                        "Placed fluid " <> T.pack (show fluidType)
                          <> " at " <> T.pack (show gx) <> ","
                          <> T.pack (show gy)
