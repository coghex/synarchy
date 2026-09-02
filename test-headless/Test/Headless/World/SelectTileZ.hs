-- | Regression for #367: a tile selection honours an explicit z.
--
--   The Info-tool left-click resolves the tile under the cursor at the
--   active z-slice ('world.pickTile', which now returns that z) and
--   passes it to 'world.selectTile'. Below the surface that z is NOT the
--   column top, so dropping it — the old behaviour — silently selected
--   the surface tile instead of the clicked one (the reported bug).
--
--   'WorldSelectTileByCoord' now carries a @Maybe Int@: @Just z@ pins the
--   clicked tile; @Nothing@ falls back to the surface z, a latent API
--   affordance no current UI path exercises (both the left-click and
--   the right-click → Info context-menu path resolve a live pick and
--   always forward its z — see the "UI wiring" block below, #802). This
--   first example drives the real command handler against the shared
--   42/64/3 world and asserts both arms. It only touches that world's
--   cursor selection (not its tiles/edits), so sharing the read-only
--   world is safe.
--
--   #802: the handler-level example above proves the Haskell side
--   honours an explicit z once it receives one, but it says nothing
--   about whether the Lua UI callers still SEND one — either
--   scripts/hud.lua's Info-tool left-click or
--   scripts/init_context_menu.lua's right-click → Info callback could
--   regress to a 3-argument @world.selectTile@ call (silently dropping
--   z) and this handler test would stay green. The "UI wiring" block
--   drives the REAL Lua modules (via 'executeDebugLua', the same
--   loadstring+pcall primitive the TCP debug console uses) against a
--   bare Lua backend, spies on @world.pickTile@/@world.selectTile@ to
--   inject and capture a sentinel z distinct from any real coordinate,
--   and asserts each path forwards it end to end.
--
--   #1702: 'handleWorldSelectTileByCoordCommand' is also the tile-side
--   half of #813's "a direct selection wins outright" rule, and it used
--   to preserve both deferred arms instead of clearing them. The
--   "stale deferred arms" example below arms BOTH
--   'zoomSelectNow' and 'worldSelectNow', commits a direct tile
--   selection, and then runs the two REAL render-time commit paths
--   ('renderWorldCursorQuads' and 'makeCursorQuad'). Two control
--   passes run first with the arms deliberately left set, proving the
--   hover inputs the example feeds those paths really do resolve (a
--   'Nothing' hover would take each path's no-op branch and the
--   regression would pass vacuously).
module Test.Headless.World.SelectTileZ (spec) where

import UPrelude
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Test.Hspec
import Data.IORef (readIORef, newIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..), defaultCamera)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import World.Tile.Types (WorldTileData(..))
import World.Chunk.Types (LoadedChunk(..), columnIndex)
import World.Generate (viewDepth)
import World.Generate.Coordinates (chunkToGlobal)
import World.Generate.Types (WorldGenParams(..))
import World.Grid (gridToWorld)
import World.Cursor.Types (CursorState(..), emptyCursorState)
import World.Render.CursorQuads (renderWorldCursorQuads)
import World.Render.HitTest (pickWorldTile)
import World.Render.ViewBounds (computeViewBounds)
import World.Render.Zoom.Cursor (makeCursorQuad, pixelToChunkOrigin)
import World.State.Types (WorldState(..))
import World.Page.Types (WorldPageId(..))
import World.Thread.Command.Cursor (handleWorldSelectTileByCoordCommand)
import Test.Headless.Harness (sharedWorld, getWorldTileData, getWorldGenParams)

spec ∷ SpecWith EngineEnv
spec = do
    describe "tile selection z (#367)" $ do
        it "Just z pins the clicked tile; Nothing falls back to surface z" $ \env → do
            ws     ← sharedWorld env 42 64 3
            -- The handler ignores the logger, but takes a real one.
            logger ← readIORef (loggerRef env)
            -- Reuse the shared world's page id (mirrors Harness.sharedWorld).
            let pid = WorldPageId "shared_42_64_3"
            tiles  ← getWorldTileData ws
            case HM.toList (wtdChunks tiles) of
                [] → expectationFailure "shared world has no loaded chunks"
                ((coord, lc) : _) → do
                    let (gx, gy) = chunkToGlobal coord 0 0
                        surfZ    = lcSurfaceMap lc VU.! columnIndex 0 0
                        belowZ   = surfZ - 2   -- a distinct tile below the top

                    -- Nothing → the handler's own surface-z fallback.
                    handleWorldSelectTileByCoordCommand (toWorldSimCapability env) logger pid gx gy Nothing
                    sel0 ← worldSelectedTile <$> readIORef (wsCursorRef ws)
                    sel0 `shouldBe` Just (gx, gy, surfZ)

                    -- Just z → the clicked tile, even below the surface. The
                    -- old code snapped this to surfZ — that is the #367 bug.
                    handleWorldSelectTileByCoordCommand (toWorldSimCapability env) logger pid gx gy (Just belowZ)
                    sel1 ← worldSelectedTile <$> readIORef (wsCursorRef ws)
                    sel1 `shouldBe` Just (gx, gy, belowZ)

        it "a fresh direct tile selection wins outright over stale deferred arms on EITHER side (#1702)" $ \env → do
            ws     ← sharedWorld env 42 64 3
            logger ← readIORef (loggerRef env)
            let pid = WorldPageId "shared_42_64_3"
            tiles   ← getWorldTileData ws
            mParams ← getWorldGenParams ws
            case (HM.toList (wtdChunks tiles), mParams) of
                ([], _)      → expectationFailure "shared world has no loaded chunks"
                (_, Nothing) → expectationFailure "shared world has no gen params"
                (chunks, Just params) → do
                    let facing       = FaceSouth
                        zoom         = 2.0 ∷ Float
                        worldSize    = wgpWorldSize params
                        (fbW, fbH)   = (800, 600)
                        (winW, winH) = (800, 600)
                        effectiveDepth =
                            min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
                        pixX = winW `div` 2
                        pixY = winH `div` 2
                        -- Same center-pixel aiming trick as
                        -- Test.Headless.World.SelectChunk's shared spec:
                        -- at the screen's dead center the view offset is
                        -- zero, so parking the camera on a tile's own
                        -- world-space position guarantees that pixel
                        -- lands on it. Search EVERY loaded chunk (not an
                        -- arbitrary "first" from the unordered HashMap —
                        -- steep/edge terrain may yield no clean hit).
                        tryChunk (coord, lc) =
                            let (tgx, tgy)   = chunkToGlobal coord 0 0
                                surfZ        = lcSurfaceMap lc VU.! columnIndex 0 0
                                (camX, camY) = gridToWorld facing tgx tgy
                                tryZSlice zSlice =
                                    let cam = defaultCamera
                                            { camPosition = (camX, camY), camZoom = zoom
                                            , camFacing = facing, camZSlice = zSlice }
                                        vb = computeViewBounds cam fbW fbH effectiveDepth
                                    in pickWorldTile facing zoom zSlice camX camY fbW fbH
                                           winW winH worldSize effectiveDepth vb tiles pixX pixY
                            in [ (tgx, tgy, surfZ, camX, camY, zSlice, hit)
                               | zSlice ← [surfZ - 40 .. surfZ + 40]
                               , Just hit ← [tryZSlice zSlice]
                               ]
                        hits = concatMap tryChunk chunks
                    case hits of
                        [] → expectationFailure
                            "could not find any loaded chunk where the center \
                            \pixel hits a real tile"
                        ((tgx, tgy, surfZ, camX, camY, zSlice, (hgx, hgy, _, _, _)) : _) → do
                            writeIORef (cameraRef env) $ defaultCamera
                                { camPosition = (camX, camY), camZoom = zoom
                                , camFacing = facing, camZSlice = zSlice }
                            writeIORef (windowSizeRef env) (winW, winH)
                            writeIORef (framebufferSizeRef env) (fbW, fbH)

                            -- The directly selected tile must differ from
                            -- the tile the world hover resolves to, or
                            -- "was not retargeted" would be unfalsifiable.
                            -- Both candidates sit inside the same loaded
                            -- chunk (chunkSize is 16), so the handler's
                            -- unloaded-chunk no-op can't swallow either.
                            let directTile@(dgx, dgy)
                                    | (hgx, hgy) ≢ (tgx, tgy) = (tgx, tgy)
                                    | otherwise               = (tgx + 1, tgy + 1)
                                directZ = surfZ - 2
                            directTile `shouldNotBe` (hgx, hgy)

                            -- The zoom hover must resolve to a real chunk
                            -- too, or makeCursorQuad would take its
                            -- off-map no-op branch instead of the commit
                            -- branch this example is about.
                            pixelToChunkOrigin facing
                                    defaultCamera { camPosition = (camX, camY)
                                                  , camZoom = zoom, camFacing = facing
                                                  , camZSlice = zSlice }
                                    winW winH fbW fbH worldSize pixX pixY
                                `shouldNotBe` Nothing

                            let armedCursor = emptyCursorState
                                    { zoomSelectNow     = True
                                    , worldSelectNow    = True
                                    , worldCursorPos    = Just (pixX, pixY)
                                    , zoomCursorPos     = Just (pixX, pixY)
                                    , worldSelectedTile = Just (dgx, dgy, directZ)
                                    }

                            -- Control 1: with worldSelectNow left ARMED,
                            -- the tile render pass really does retarget
                            -- the selection to the hover result.
                            writeIORef (wsCursorRef ws) armedCursor
                            _ ← renderWorldCursorQuads env pid ws 1.0
                            ctrlTile ← readIORef (wsCursorRef ws)
                            fmap (\(gx', gy', _) → (gx', gy')) (worldSelectedTile ctrlTile)
                                `shouldBe` Just (hgx, hgy)

                            -- Control 2: with zoomSelectNow left ARMED,
                            -- the zoom render pass really does commit a
                            -- chunk and erase the tile selection.
                            writeIORef (wsCursorRef ws) armedCursor
                            _ ← makeCursorQuad facing
                                    defaultCamera { camPosition = (camX, camY)
                                                  , camZoom = zoom, camFacing = facing
                                                  , camZSlice = zSlice }
                                    winW winH fbW fbH worldSize (wsCursorRef ws)
                                    lookupSlot (-1)
                            ctrlZoom ← readIORef (wsCursorRef ws)
                            zoomSelectedPos ctrlZoom `shouldNotBe` Nothing
                            worldSelectedTile ctrlZoom `shouldBe` Nothing

                            -- The real case: both arms pending, plus a
                            -- stale chunk selection the #135 opposing
                            -- clear must drop.
                            writeIORef (wsCursorRef ws) $ armedCursor
                                { worldSelectedTile = Nothing
                                , zoomSelectedPos   = Just (tgx, tgy)
                                }
                            handleWorldSelectTileByCoordCommand (toWorldSimCapability env)
                                logger pid dgx dgy (Just directZ)
                            afterSelect ← readIORef (wsCursorRef ws)
                            worldSelectedTile afterSelect `shouldBe` Just (dgx, dgy, directZ)
                            zoomSelectedPos afterSelect `shouldBe` Nothing
                            zoomSelectNow afterSelect `shouldBe` False
                            worldSelectNow afterSelect `shouldBe` False

                            -- A later tile render pass must NOT resolve
                            -- the now-cleared arm and retarget the fresh
                            -- selection to the hover tile.
                            _ ← renderWorldCursorQuads env pid ws 1.0
                            afterTileRender ← readIORef (wsCursorRef ws)
                            worldSelectedTile afterTileRender `shouldBe` Just (dgx, dgy, directZ)

                            -- Nor may a later zoom render pass commit a
                            -- chunk and wipe the fresh tile selection.
                            _ ← makeCursorQuad facing
                                    defaultCamera { camPosition = (camX, camY)
                                                  , camZoom = zoom, camFacing = facing
                                                  , camZSlice = zSlice }
                                    winW winH fbW fbH worldSize (wsCursorRef ws)
                                    lookupSlot (-1)
                            afterZoomRender ← readIORef (wsCursorRef ws)
                            worldSelectedTile afterZoomRender `shouldBe` Just (dgx, dgy, directZ)
                            zoomSelectedPos afterZoomRender `shouldBe` Nothing

    describe "UI wiring forwards the picked z (#802)" $ do
        it "Info-tool left-click forwards world.pickTile's z to world.selectTile (scripts/hud.lua)" $ \env → do
            ls ← newBareLuaBackend env
            setup ← evalDebug ls leftClickWiringSetupLua
            setup `shouldNotSatisfy` isLuaError

            call ← evalDebug ls "require('scripts.hud').onMouseDown(1, 10, 20)"
            call `shouldNotSatisfy` isLuaError

            evalDebug ls "_G.__capturedZ" ≫= (`shouldBe` "4242")

        it "right-click context-menu Info callback forwards world.pickTile's z to world.selectTile (scripts/init_context_menu.lua)" $ \env → do
            ls ← newBareLuaBackend env
            setup ← evalDebug ls rightClickWiringSetupLua
            setup `shouldNotSatisfy` isLuaError

            tryMenu ← evalDebug ls "require('scripts.init_context_menu').tryTileMenu(10, 20)"
            tryMenu `shouldNotSatisfy` isLuaError

            invokeInfo ← evalDebug ls "_G.__infoCallback()"
            invokeInfo `shouldNotSatisfy` isLuaError

            evalDebug ls "_G.__capturedZ" ≫= (`shouldBe` "4242")

-- | Stable-handle → slot resolution for the cursor quad emitters; the
--   #1702 example only inspects cursor STATE, never the quads.
lookupSlot ∷ TextureHandle → Int
lookupSlot _ = 0

-- * Real-Lua-backend helper (mirrors Test.Headless.UI.InputOwnership's
--   newBareLuaBackend — a real Lua backend with the full Lua API
--   registered and nothing preloaded, so scripts/hud.lua and
--   scripts/init_context_menu.lua are pulled in the same way any real
--   caller's @require@ would).

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run one command through the exact loadstring+pcall primitive the
--   real TCP debug console itself uses.
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | Reach hud.lua's info-select branch (scripts/hud.lua, inside
--   onMouseDown's @currentView == "zoomed_in"@/@button_num == 1@ arm)
--   and spy on the two calls that carry z across the #367 boundary.
--   @unit@/@building@/@item@ are cleared so their real (nil-selection)
--   guards can't interfere; @world.getToolMode@ is stubbed because the
--   real one reads a live world's tool-mode ref, and this test never
--   calls @world.init@.
leftClickWiringSetupLua ∷ Text
leftClickWiringSetupLua = T.concat
    [ "local hud = require('scripts.hud'); "
    , "local uiManager = require('scripts.ui_manager'); "
    , "uiManager.currentMenu = 'world_view'; "
    , "require('scripts.pause_menu').visible = false; "
    , "hud.visible = true; "
    , "hud.currentView = 'zoomed_in'; "
    , "hud.worldId = 'test_page'; "
    , "unit = nil; "
    , "building = nil; "
    , "item = nil; "
    , "world.getToolMode = function() return 'info' end; "
    , "world.pickTile = function(cx, cy) return 11, 22, 4242 end; "
    , "_G.__capturedZ = nil; "
    , "world.selectTile = function(pageId, gx, gy, z) _G.__capturedZ = z end"
    ]

-- | Reach init_context_menu.lua's tryTileMenu and capture its "Info"
--   menu item's callback (the closure that actually calls
--   world.selectTile) by spying on contextMenu.show, which the real
--   function passes the menu item list to instead of calling
--   world.selectTile directly.
rightClickWiringSetupLua ∷ Text
rightClickWiringSetupLua = T.concat
    [ "local hud = require('scripts.hud'); "
    , "hud.worldId = 'test_page'; "
    , "hud.toolToggleId = nil; "
    , "world.pickTile = function(x, y) return 33, 44, 4242 end; "
    , "local contextMenu = require('scripts.ui.context_menu'); "
    , "_G.__infoCallback = nil; "
    , "contextMenu.show = function(menuItems, mx, my) "
    , "  for _, menuItem in ipairs(menuItems) do "
    , "    if menuItem.label == 'Info' then _G.__infoCallback = menuItem.callback end "
    , "  end "
    , "end; "
    , "_G.__capturedZ = nil; "
    , "world.selectTile = function(pageId, gx, gy, z) _G.__capturedZ = z end"
    ]
