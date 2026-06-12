module World.Thread.Command.Cursor
    ( handleWorldSetZoomCursorHoverCommand
    , handleWorldSetZoomCursorSelectCommand
    , handleWorldSetZoomCursorDeselectCommand
    , handleWorldSetZoomCursorSelectTextureCommand
    , handleWorldSetZoomCursorHoverTextureCommand
    , handleWorldSetWorldCursorHoverCommand
    , handleWorldSetWorldCursorSelectCommand
    , handleWorldSetWorldCursorDeselectCommand
    , handleWorldSetWorldCursorSelectTextureCommand
    , handleWorldSetWorldCursorHoverTextureCommand
    , handleWorldSetWorldCursorSelectBgTextureCommand
    , handleWorldSetWorldCursorHoverBgTextureCommand
    , handleWorldSelectTileByCoordCommand
    , handleWorldSetMineAnchorCommand
    , handleWorldClearMineAnchorCommand
    , handleWorldDesignateMineCommand
    , handleWorldSetMineDesignateTextureCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Engine.Asset.Handle (TextureHandle)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logInfo, logDebug, logError, logWarn
                       , LogCategory(..), LoggerState)
import Engine.Graphics.Camera (Camera2D(..))
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Chunk.Types (LoadedChunk(..), ColumnTiles(..), columnIndex)
import World.Tile.Types (lookupChunk)
import World.Constants (seaLevel)
import World.Generate (generateChunk, globalToChunk)
import World.Mine.Types (designationFromSlope)
import World.Generate.Constants (chunkLoadRadius)
import World.Generate.Timeline (applyTimelineFast)
import World.Geology (buildTimeline)
import World.Geology.Log (formatTimeline, formatPlatesSummary)
import World.Fluids (computeOceanMap, isOceanChunk)
import World.Plate (generatePlates, elevationAtGlobal)
import World.Preview (buildPreviewImage, PreviewImage(..))
import World.Render (surfaceHeadroom)
import World.ZoomMap (buildZoomCache)
import World.Weather (initEarlyClimate, formatWeather, defaultClimateParams)
import World.Thread.Helpers (sendGenLog, unWorldPageId)
import World.Thread.ChunkLoading (maxChunksPerTick)

handleWorldSetZoomCursorHoverCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetZoomCursorHoverCommand env logger pageId x y = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { zoomCursorPos = Just (x, y) }, ())
        Nothing → 
            logWarn logger CatWorld $ 
                "World not found for cursor hover update: " <> unWorldPageId pageId
handleWorldSetZoomCursorSelectCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldSetZoomCursorSelectCommand env logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { zoomSelectNow = True }, ())
        Nothing → pure ()
handleWorldSetZoomCursorDeselectCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldSetZoomCursorDeselectCommand env logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { zoomSelectedPos = Nothing, zoomSelectNow = False }, ())
        Nothing → pure ()
handleWorldSetZoomCursorSelectTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetZoomCursorSelectTextureCommand env logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { zoomCursorTexture = Just tid }, ())
        Nothing → 
            logWarn logger CatWorld $ 
                "World not found for zoom cursor texture update: "
                    <> unWorldPageId pageId
handleWorldSetZoomCursorHoverTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetZoomCursorHoverTextureCommand env logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { zoomHoverTexture = Just tid }, ())
        Nothing → 
            logWarn logger CatWorld $ 
                "World not found for zoom cursor hover texture update: "
                    <> unWorldPageId pageId
handleWorldSetWorldCursorHoverCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetWorldCursorHoverCommand env logger pageId x y = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldCursorPos = Just (x, y) }, ())
        Nothing → 
            logWarn logger CatWorld $ 
                "World not found for cursor hover update: " <> unWorldPageId pageId
handleWorldSetWorldCursorSelectCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldSetWorldCursorSelectCommand env logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { worldSelectNow = True }, ())
        Nothing → pure ()
handleWorldSetWorldCursorDeselectCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldSetWorldCursorDeselectCommand env logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { worldSelectedTile = Nothing, worldSelectNow = False }, ())
        Nothing → pure ()
handleWorldSetWorldCursorSelectTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetWorldCursorSelectTextureCommand env logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldCursorTexture = Just tid }, ())
        Nothing → 
            logWarn logger CatWorld $ 
                "World not found for cursor texture update: "
                    <> unWorldPageId pageId
handleWorldSetWorldCursorHoverTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetWorldCursorHoverTextureCommand env logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldHoverTexture = Just tid }, ())
        Nothing → 
            logWarn logger CatWorld $ 
                "World not found for cursor hover texture update: "
                    <> unWorldPageId pageId
handleWorldSetWorldCursorSelectBgTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetWorldCursorSelectBgTextureCommand env logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldCursorBgTexture = Just tid }, ())
        Nothing → 
            logWarn logger CatWorld $ 
                "World not found for cursor texture update: "
                    <> unWorldPageId pageId
handleWorldSetWorldCursorHoverBgTextureCommand ∷ EngineEnv → LoggerState → WorldPageId
    → TextureHandle → IO ()
handleWorldSetWorldCursorHoverBgTextureCommand env logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
              (cs { worldHoverBgTexture = Just tid }, ())
        Nothing →
            logWarn logger CatWorld $
                "World not found for cursor hover texture update: "
                    <> unWorldPageId pageId

-- * Mine designation tool

handleWorldSetMineAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetMineAnchorCommand env _logger pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { mineAnchor = Just (gx, gy) }, ())
        Nothing → pure ()

handleWorldClearMineAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId → IO ()
handleWorldClearMineAnchorCommand env _logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { mineAnchor = Nothing }, ())
        Nothing → pure ()

-- | Cap on the designation rectangle's side length. Guards against a
--   misclick across the map turning into a 100k-tile designation.
maxDesignateSide ∷ Int
maxDesignateSide = 128

-- | Commit a designation rectangle. DF-style: designations are
--   PER-Z-LEVEL — (gx1, gy1) is the anchor corner, and only tiles
--   whose surface z equals the anchor tile's surface z are taken, so
--   a sweep across a slope marks just the anchor's level. Entries
--   store that z (markers render from it, no per-frame column reads).
--   Unloaded-chunk tiles are skipped — designate what you can see.
--   Also clears the anchor so the Lua side can't desync from a
--   dropped clear command.
handleWorldDesignateMineCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Int → Int → IO ()
handleWorldDesignateMineCommand env logger pageId gx1 gy1 gx2 gy2 = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            let surfaceZAt gx gy = do
                    let (coord, (lx, ly)) = globalToChunk gx gy
                    lc ← lookupChunk coord tileData
                    pure (lcSurfaceMap lc VU.! columnIndex lx ly)
                -- Gen-time slope at the tile's surface: tiles that are
                -- already sloped start with the lowered corners pre-dug
                -- ('designationFromSlope'), so the designation's volume
                -- matches the material that's actually there.
                slopeAt gx gy z =
                    let (coord, (lx, ly)) = globalToChunk gx gy
                    in case lookupChunk coord tileData of
                        Nothing → 0
                        Just lc →
                            let col = lcTiles lc V.! columnIndex lx ly
                                i = z - ctStartZ col
                            in if i ≥ 0 ∧ i < VU.length (ctSlopes col)
                               then ctSlopes col VU.! i
                               else 0
                xLo = min gx1 gx2
                yLo = min gy1 gy2
                xHi = min (max gx1 gx2) (xLo + maxDesignateSide - 1)
                yHi = min (max gy1 gy2) (yLo + maxDesignateSide - 1)
                entries = case surfaceZAt gx1 gy1 of
                    Nothing → []   -- anchor chunk unloaded: nothing
                    Just anchorZ →
                        [ ((gx, gy), designationFromSlope z (slopeAt gx gy z))
                        | gx ← [xLo .. xHi]
                        , gy ← [yLo .. yHi]
                        , Just z ← [surfaceZAt gx gy]
                        , z ≡ anchorZ
                        ]
            atomicModifyIORef' (wsMineDesignationsRef worldState) $ \m →
                (foldl' (\acc (k, v) → HM.insert k v acc) m entries, ())
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { mineAnchor = Nothing }, ())
            logDebug logger CatWorld $
                "Mine designation: +" <> T.pack (show (length entries))
                <> " tiles (" <> T.pack (show xLo) <> ","
                <> T.pack (show yLo) <> ")–(" <> T.pack (show xHi)
                <> "," <> T.pack (show yHi) <> ")"

handleWorldSetMineDesignateTextureCommand ∷ EngineEnv → LoggerState
    → WorldPageId → TextureHandle → IO ()
handleWorldSetMineDesignateTextureCommand env _logger pageId tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { mineDesignTexture = Just tid }, ())
        Nothing → pure ()

-- | Directly select the column at (gx, gy) on the given world, using
--   the loaded chunk's surface z. Used by the context-menu "Info" path
--   so a tile can be selected without going through the hover-then-
--   select cursor flow (which races with the per-tick mouse-hover
--   updates from hud.update). No-op if the chunk isn't loaded.
handleWorldSelectTileByCoordCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSelectTileByCoordCommand env _logger pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            let (chunkCoord, (lx, ly)) = globalToChunk gx gy
            case lookupChunk chunkCoord tileData of
                Nothing → pure ()
                Just lc → do
                    let z = lcSurfaceMap lc VU.! columnIndex lx ly
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                        (cs { worldSelectedTile = Just (gx, gy, z) }, ())
