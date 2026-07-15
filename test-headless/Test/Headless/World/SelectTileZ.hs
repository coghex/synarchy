{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
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
module Test.Headless.World.SelectTileZ (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef, newIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import World.Tile.Types (WorldTileData(..))
import World.Chunk.Types (LoadedChunk(..), columnIndex)
import World.Generate.Coordinates (chunkToGlobal)
import World.Cursor.Types (CursorState(..))
import World.State.Types (WorldState(..))
import World.Page.Types (WorldPageId(..))
import World.Thread.Command.Cursor (handleWorldSelectTileByCoordCommand)
import Test.Headless.Harness (sharedWorld, getWorldTileData)

spec ∷ SpecWith EngineEnv
spec = do
    describe "tile selection z (#367)" $
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
                    handleWorldSelectTileByCoordCommand env logger pid gx gy Nothing
                    sel0 ← worldSelectedTile <$> readIORef (wsCursorRef ws)
                    sel0 `shouldBe` Just (gx, gy, surfZ)

                    -- Just z → the clicked tile, even below the surface. The
                    -- old code snapped this to surfZ — that is the #367 bug.
                    handleWorldSelectTileByCoordCommand env logger pid gx gy (Just belowZ)
                    sel1 ← worldSelectedTile <$> readIORef (wsCursorRef ws)
                    sel1 `shouldBe` Just (gx, gy, belowZ)

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
