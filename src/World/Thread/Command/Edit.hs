{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Command.Edit
    ( handleWorldDeleteTileCommand
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, logWarn, LogCategory(..), LoggerState)
import World.Types
import World.Chunk.Types (LoadedChunk(..), ColumnTiles(..), columnIndex)
import World.Tile.Types (lookupChunk, insertChunk)
import World.Generate.Coordinates (globalToChunk)
import World.Thread.Helpers (unWorldPageId)

-- | Dig the top of the column at (gx, gy) down by 1 Z.
--   Lowers both surface maps by 1 and zeros the material at the old top
--   in the column's tile storage so the renderer skips it on the next tick.
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
            td ← readIORef (wsTilesRef ws)
            case lookupChunk coord td of
                Nothing →
                    logWarn logger CatWorld $
                        "Chunk not loaded for delete tile at "
                          <> T.pack (show gx) <> "," <> T.pack (show gy)
                Just lc → do
                    let oldTopZ = lcSurfaceMap lc VU.! idx
                        col     = lcTiles lc V.! idx
                        colLen  = VU.length (ctMats col)
                        i       = oldTopZ - ctStartZ col
                        canEdit = i ≥ 0 ∧ i < colLen
                    if not canEdit
                      then logWarn logger CatWorld $
                             "Delete tile out of column range at "
                               <> T.pack (show gx) <> "," <> T.pack (show gy)
                               <> " topZ=" <> T.pack (show oldTopZ)
                               <> " startZ=" <> T.pack (show (ctStartZ col))
                               <> " len=" <> T.pack (show colLen)
                      else do
                        let mats'   = ctMats col   VU.// [(i, 0)]
                            slopes' = ctSlopes col VU.// [(i, 0)]
                            veg'    = ctVeg col    VU.// [(i, 0)]
                            col'    = col { ctMats = mats'
                                          , ctSlopes = slopes'
                                          , ctVeg = veg'
                                          }
                            tiles'  = lcTiles lc V.// [(idx, col')]
                            surf'   = lcSurfaceMap lc        VU.// [(idx, oldTopZ - 1)]
                            tsurf'  = lcTerrainSurfaceMap lc VU.// [(idx, oldTopZ - 1)]
                            lc'     = lc { lcTiles             = tiles'
                                         , lcSurfaceMap        = surf'
                                         , lcTerrainSurfaceMap = tsurf'
                                         , lcModified          = True
                                         }
                        atomicModifyIORef' (wsTilesRef ws) $ \w →
                            (insertChunk lc' w, ())
                        -- Invalidate all three render caches so the next
                        -- tick rebuilds quads from the modified chunk.
                        writeIORef (wsQuadCacheRef ws)     Nothing
                        writeIORef (wsZoomQuadCacheRef ws) Nothing
                        writeIORef (wsBgQuadCacheRef ws)   Nothing
                        logDebug logger CatWorld $
                            "Deleted tile at " <> T.pack (show gx) <> ","
                              <> T.pack (show gy) <> " z=" <> T.pack (show oldTopZ)
