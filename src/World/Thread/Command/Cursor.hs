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
    , handleWorldSetConstructAnchorCommand
    , handleWorldClearConstructAnchorCommand
    , handleWorldDesignateConstructCommand
    , handleWorldCancelConstructCommand
    , handleWorldSetConstructStatusCommand
    , handleWorldSetConstructDesignateTextureCommand
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
import World.Construct.Types ( ConstructTarget(..), ConstructStatus(..)
                             , ConstructDesignation(..)
                             , newConstructDesignation
                             , constructTargetCategory )
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
            -- Only ARM the selection here. The chunk is resolved from the
            -- cursor hover at render time (makeCursorQuad), which is also
            -- where the opposing tile selection is cleared — doing the
            -- clear here instead would blank the cursor for the frames
            -- before the commit lands (issue #135).
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
            -- Only ARM the selection here. The tile is resolved from the
            -- cursor hover at render time (renderWorldCursorQuads), which
            -- is also where the opposing chunk selection is cleared —
            -- doing the clear here instead would blank the cursor for the
            -- frames before the commit lands (issue #135).
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

-- * Construction designation tool (#95)
--
--   Mirrors the mine designation tool: an anchor→rectangle commit that
--   stores per-tile designations (build target + status + progress) in
--   wsConstructDesignationsRef. The build AI (#96) is the consumer.

handleWorldSetConstructAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetConstructAnchorCommand env _logger pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructAnchor = Just (gx, gy) }, ())
        Nothing → pure ()

handleWorldClearConstructAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → IO ()
handleWorldClearConstructAnchorCommand env _logger pageId = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructAnchor = Nothing }, ())
        Nothing → pure ()

-- | Commit a construction designation. Per-z-level like mining: only
--   tiles at the anchor's surface z are taken. STRUCTURE targets fill the
--   whole rectangle (paint a floor / wall run); BUILDING targets mark
--   only the anchor tile (one footprint, not a grid of buildings).
--   Unloaded-chunk tiles are skipped. Clears the anchor afterwards.
handleWorldDesignateConstructCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Int → Int → ConstructTarget → IO ()
handleWorldDesignateConstructCommand env logger pageId gx1 gy1 gx2 gy2 tgt = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            let surfaceZAt gx gy = do
                    let (coord, (lx, ly)) = globalToChunk gx gy
                    lc ← lookupChunk coord tileData
                    pure (lcSurfaceMap lc VU.! columnIndex lx ly)
                xLo = min gx1 gx2
                yLo = min gy1 gy2
                xHi = min (max gx1 gx2) (xLo + maxDesignateSide - 1)
                yHi = min (max gy1 gy2) (yLo + maxDesignateSide - 1)
                entries = case surfaceZAt gx1 gy1 of
                    Nothing → []   -- anchor chunk unloaded: nothing
                    Just anchorZ → case tgt of
                        -- A building is a single footprint: only the
                        -- anchor tile, at its own surface z.
                        CtBuilding _ →
                            [ ((gx1, gy1), newConstructDesignation anchorZ tgt) ]
                        -- Structure pieces tile the rectangle, per-z-level.
                        CtStructure _ →
                            [ ((gx, gy), newConstructDesignation z tgt)
                            | gx ← [xLo .. xHi]
                            , gy ← [yLo .. yHi]
                            , Just z ← [surfaceZAt gx gy]
                            , z ≡ anchorZ
                            ]
            atomicModifyIORef' (wsConstructDesignationsRef worldState) $ \m →
                (foldl' (\acc (k, v) → HM.insert k v acc) m entries, ())
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructAnchor = Nothing }, ())
            logDebug logger CatWorld $
                "Construct designation: +" <> T.pack (show (length entries))
                <> " tiles (" <> constructTargetCategory tgt <> ")"

handleWorldCancelConstructCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldCancelConstructCommand env _logger pageId gx gy = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsConstructDesignationsRef worldState) $ \m →
                (HM.delete (gx, gy) m, ())
        Nothing → pure ()

-- | Build AI hook (#96): set a designation's status. Complete removes it.
handleWorldSetConstructStatusCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → ConstructStatus → IO ()
handleWorldSetConstructStatusCommand env _logger pageId gx gy st = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsConstructDesignationsRef worldState) $ \m →
                case st of
                    CsComplete → (HM.delete (gx, gy) m, ())
                    _          → (HM.adjust (\cd → cd { cdStatus = st })
                                           (gx, gy) m, ())
        Nothing → pure ()

handleWorldSetConstructDesignateTextureCommand ∷ EngineEnv → LoggerState
    → WorldPageId → Text → TextureHandle → IO ()
handleWorldSetConstructDesignateTextureCommand env _logger pageId cat tid = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                case cat of
                    "building" → (cs { constructBuildingTexture = Just tid }, ())
                    _          → (cs { constructStructTexture = Just tid }, ())
        Nothing → pure ()

-- | Directly select the column at (gx, gy) on the given world. The
--   @Maybe Int@ picks the z: @Just z@ selects that exact tile (the
--   live-picked z from a left-click, so clicking below the surface
--   selects the clicked tile rather than the column top — issue #367);
--   @Nothing@ falls back to the loaded chunk's surface z (the
--   context-menu "Info" path, which has no live pick). Used so a tile
--   can be selected without going through the hover-then-select cursor
--   flow (which races with the per-tick mouse-hover updates from
--   hud.update). No-op if the chunk isn't loaded.
handleWorldSelectTileByCoordCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Maybe Int → IO ()
handleWorldSelectTileByCoordCommand env _logger pageId gx gy mz = do
    mgr ← readIORef (worldManagerRef env)
    case lookup pageId (wmWorlds mgr) of
        Nothing → pure ()
        Just worldState → do
            tileData ← readIORef (wsTilesRef worldState)
            let (chunkCoord, (lx, ly)) = globalToChunk gx gy
            case lookupChunk chunkCoord tileData of
                Nothing → pure ()
                Just lc → do
                    -- Use the live-picked z when supplied; otherwise
                    -- default to the column surface.
                    let z = fromMaybe (lcSurfaceMap lc VU.! columnIndex lx ly) mz
                    -- This path resolves the tile immediately (no hover
                    -- round-trip), so the set and the opposing-chunk clear
                    -- happen in the SAME write — no blank window. A new
                    -- tile selection drops any chunk selection (issue #135).
                    atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                        (cs { worldSelectedTile = Just (gx, gy, z)
                            , zoomSelectedPos   = Nothing }, ())
