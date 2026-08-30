-- | #1931: the gameplay-input gate on @hud.update@'s per-tick world
--   cursor hover push.
--
--   @hud.update@ ran every UI tick gated on @hud.visible@ alone and
--   submitted @world.setZoomCursorHover@ \/ @world.setWorldCursorHover@
--   from the live pointer. That visibility gate (#153) covers a menu
--   opened over a HIDDEN gameplay view, but the pause menu
--   (@scripts\/pause_menu.lua@) and keep-world Settings
--   (@scripts\/ui_manager_menu.lua@) deliberately leave the HUD up, and
--   @uiManager.update@ keeps ticking regardless of @currentMenu@ — so
--   the world's hover, and any armed designation tool's anchor→hover
--   preview, tracked the pointer across those overlays. The click path
--   in the same module already carried the correction (#154); this is
--   the same @uiManager.isGameplayInputActive()@ gate on the hover half.
--
--   Everything here drives the PRODUCTION @hud.update@ entry point
--   through the real @scripts\/hud.lua@, @scripts\/ui_manager.lua@,
--   @scripts\/pause_menu.lua@ and @scripts\/ui\/view_teardown.lua@ via the
--   same @loadstring@+@pcall@ primitive the TCP debug console uses. Only
--   the collaborators that would need a generated world or a rendered
--   frame are stubbed: the designation tools, the camera's zoom
--   readback, the mouse position, and the two @world.*@ hover verbs
--   themselves — those last being the very calls under test, so "was it
--   submitted, and with which coordinates" is answered by observing the
--   boundary rather than by re-deriving engine cursor state (which this
--   issue explicitly does not touch). Shape precedent:
--   'Test.Headless.UI.ZoomBandInputGate'.
module Test.Headless.UI.HudHoverGate (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, writeIORef)
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import UI.Manager
import UI.Types
import Unit.Types (emptyUnitManager)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), emptyWorldManager, emptyWorldState )

spec ∷ Spec
spec = around withHoverEngine $ do

    -- §1: gameplay input inactive ⇒ no hover is submitted, in either
    -- band, for either of the two overlays that keep the HUD visible.
    describe "an inactive-gameplay-input tick submits no world cursor \
             \hover (§1)" $ do

        it "the pause menu open over a zoomed_in HUD submits no \
           \setWorldCursorHover" $ \env → do
            ls ← hoverBackend env
            setBand ls "zoomed_in"
            openPauseMenu ls
            gameplayInputActive ls `shouldReturn` "false"

            tick ls

            hoverCounts ls `shouldReturn` ("0", "0")

        it "the pause menu open over a zoomed_out HUD submits no \
           \setZoomCursorHover" $ \env → do
            ls ← hoverBackend env
            setBand ls "zoomed_out"
            openPauseMenu ls

            tick ls

            hoverCounts ls `shouldReturn` ("0", "0")

        it "keep-world Settings — which leaves world and HUD visible by \
           \design — submits no hover in either band" $ \env → do
            ls ← hoverBackend env
            openKeepWorldSettings ls
            gameplayInputActive ls `shouldReturn` "false"

            setBand ls "zoomed_in"
            tick ls
            setBand ls "zoomed_out"
            tick ls

            hoverCounts ls `shouldReturn` ("0", "0")

        it "a visible exclusive modal page over an unpaused gameplay \
           \view submits no hover either" $ \env → do
            ls ← hoverBackend env
            setBand ls "zoomed_in"
            showModal env
            gameplayInputActive ls `shouldReturn` "false"

            tick ls

            hoverCounts ls `shouldReturn` ("0", "0")

        it "a hidden HUD still submits nothing, so the pre-existing \
           \visibility gate is not weakened (#153)" $ \env → do
            ls ← hoverBackend env
            setBand ls "zoomed_in"
            evalOk ls "require('scripts.hud').visible = false; return true"
                `shouldReturn` "true"

            tick ls

            hoverCounts ls `shouldReturn` ("0", "0")

    -- §2: ordinary gameplay is unchanged, in BOTH bands, and the
    -- coordinates submitted are the live pointer's.
    describe "an active-gameplay-input tick still submits the band's \
             \hover from the current pointer (§2)" $ do

        it "zoomed_in submits setWorldCursorHover with the current mouse \
           \position, and no zoom-map hover" $ \env → do
            ls ← hoverBackend env
            setBand ls "zoomed_in"
            setMouse ls 111 222
            gameplayInputActive ls `shouldReturn` "true"

            tick ls

            hoverCounts ls `shouldReturn` ("0", "1")
            lastWorldHover ls "hud_hover_gate_page:111,222"

        it "zoomed_out submits setZoomCursorHover with the current mouse \
           \position, and no world hover" $ \env → do
            ls ← hoverBackend env
            setBand ls "zoomed_out"
            setMouse ls 33 44

            tick ls

            hoverCounts ls `shouldReturn` ("1", "0")
            lastZoomHover ls "hud_hover_gate_page:33,44"

        it "the `none` fade band submits neither, as before" $ \env → do
            ls ← hoverBackend env
            setBand ls "none"

            tick ls

            hoverCounts ls `shouldReturn` ("0", "0")

    -- §3: only the hover SUBMISSION is suppressed. The band reconcile
    -- that shares the tick keeps running, so momentum that coasts the
    -- camera across a band while an overlay is up still swaps pages and
    -- runs the zoomBand teardown sweep.
    describe "view reconciliation still runs on an inactive visible-HUD \
             \tick (§3)" $ do

        it "a band crossing behind the pause menu still updates \
           \hud.currentView and runs the zoomBand teardown" $ \env → do
            ls ← hoverBackend env
            setBand ls "zoomed_in"
            openPauseMenu ls
            -- Camera momentum carries past the band with no further
            -- wheel event; only the per-tick driver can notice.
            setZoom ls "3.0"

            tick ls

            currentView ls "zoomed_out"
            evalOk ls "return probe.toolCancels > 0" `shouldReturn` "true"
            hoverCounts ls `shouldReturn` ("0", "0")

        it "the same crossing behind keep-world Settings reconciles too" $
          \env → do
            ls ← hoverBackend env
            setBand ls "zoomed_out"
            openKeepWorldSettings ls
            setZoom ls "0.5"

            tick ls

            currentView ls "zoomed_in"
            hoverCounts ls `shouldReturn` ("0", "0")

    -- §4: returning to gameplay resumes from the LIVE pointer, with no
    -- stale coordinate replayed from the suppressed ticks.
    describe "returning to gameplay resumes hover on the next tick (§4)" $ do

        it "a pointer that moved while the pause menu was open resumes \
           \at its NEW position, never the pre-menu one" $ \env → do
            ls ← hoverBackend env
            setBand ls "zoomed_in"
            setMouse ls 10 20
            tick ls
            lastWorldHover ls "hud_hover_gate_page:10,20"

            openPauseMenu ls
            setMouse ls 700 500
            tick ls
            tick ls
            -- Nothing new was submitted while the menu was up.
            hoverCounts ls `shouldReturn` ("0", "1")

            closePauseMenu ls
            setMouse ls 640 360
            tick ls

            hoverCounts ls `shouldReturn` ("0", "2")
            lastWorldHover ls "hud_hover_gate_page:640,360"

        it "a modal dismissed with the band changed underneath it \
           \resumes in the NEW band" $ \env → do
            ls ← hoverBackend env
            setBand ls "zoomed_in"
            showModal env
            setZoom ls "3.0"
            tick ls
            currentView ls "zoomed_out"

            hideModal env
            setMouse ls 5 6
            tick ls

            hoverCounts ls `shouldReturn` ("1", "0")
            lastZoomHover ls "hud_hover_gate_page:5,6"

-- * Fixture

-- | Engine init itself materializes @config\/*.local.yaml@ (#1357 \/
--   'Test.Headless.Harness.Isolation'), and this suite loads the real
--   @scripts\/ui_manager.lua@ tree, so the scratch resource root goes
--   AROUND the engine rather than inside it.
withHoverEngine ∷ (EngineEnv → IO α) → IO α
withHoverEngine action =
    withIsolatedResourceRoot (withHeadlessEngineNoWorld action)

hoverPage ∷ WorldPageId
hoverPage = WorldPageId "hud_hover_gate_page"

-- | One visible in-memory page, so the @item.deselect@ \/
--   @building.deselect@ entries the real @zoomBand@ teardown sweep runs
--   are the production verbs against a real manager. No generation
--   parameters, so no world worker is needed.
installHoverWorld ∷ EngineEnv → IO ()
installHoverWorld env = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(hoverPage, ws)], wmVisible = [hoverPage] }
    writeIORef (unitManagerRef env) emptyUnitManager
    writeIORef (uiManagerRef env) emptyUIPageManager
    writeIORef (windowSizeRef env) (1280, 720)
    writeIORef (framebufferSizeRef env) (1280, 720)

-- | A real Lua backend with the full API registered, the page fixture
--   installed, and the narrowest possible set of stubs.
--
--   Stubbed: the five designation tools (their real @cancel@ \/
--   @hidePicker@ need a generated world, and their call is how §3
--   observes the teardown sweep), the camera zoom readback (so a band
--   crossing is deterministic rather than dependent on a real camera),
--   @engine.getMousePosition@, and the two hover verbs under test. NOT
--   stubbed: @scripts\/hud.lua@, @scripts\/ui_manager.lua@,
--   @scripts\/pause_menu.lua@ and @scripts\/ui\/view_teardown.lua@.
hoverBackend ∷ EngineEnv → IO LuaBackendState
hoverBackend env = do
    installHoverWorld env
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    setup ← evalDebug ls
        "_G.probe = { zoomHovers = 0, worldHovers = 0, toolCancels = 0, \
        \             lastZoom = '', lastWorld = '', \
        \             zoom = 0.5, mouse = { 0, 0 } }; \
        \local function makeTool() return { \
        \    handleMouseDown = function() return false end, \
        \    cancel = function() \
        \        probe.toolCancels = probe.toolCancels + 1 end } end; \
        \local buildTool = makeTool(); \
        \buildTool.hidePicker = function() end; \
        \buildTool.exitPlacement = function() end; \
        \package.loaded['scripts.build_tool'] = buildTool; \
        \package.loaded['scripts.mine_tool']  = makeTool(); \
        \package.loaded['scripts.chop_tool']  = makeTool(); \
        \package.loaded['scripts.till_tool']  = makeTool(); \
        \package.loaded['scripts.plant_tool'] = makeTool(); \
        \world.setZoomCursorHover = function(id, x, y) \
        \    probe.zoomHovers = probe.zoomHovers + 1; \
        \    probe.lastZoom = tostring(id) .. ':' .. tostring(x) \
        \                     .. ',' .. tostring(y) end; \
        \world.setWorldCursorHover = function(id, x, y) \
        \    probe.worldHovers = probe.worldHovers + 1; \
        \    probe.lastWorld = tostring(id) .. ':' .. tostring(x) \
        \                      .. ',' .. tostring(y) end; \
        \engine.getMousePosition = function() \
        \    return probe.mouse[1], probe.mouse[2] end; \
        \camera.getZoom = function() return probe.zoom end; \
        \camera.getZoomFadeStart = function() return 1.0 end; \
        \camera.getZoomFadeEnd = function() return 2.0 end; \
        \require('scripts.ui_manager').currentMenu = 'world_view'; \
        \require('scripts.pause_menu').visible = false; \
        \local h = require('scripts.hud'); \
        \h.visible = true; h.worldId = 'hud_hover_gate_page'; \
        \h.currentView = 'zoomed_in'; \
        \return true"
    when (setup ≢ "true") $
        error ("hud hover gate fixture setup failed: " ⧺ T.unpack setup)
    pure ls

-- * Driving the production entry point

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

-- | Evaluate and fail the example on a Lua error rather than letting a
--   stringified error silently compare unequal to an expected value.
evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls src = do
    got ← evalDebug ls src
    when ("error:" `T.isPrefixOf` got ∨ "syntax error:" `T.isPrefixOf` got) $
        expectationFailure ("Lua error from " ⧺ show src ⧺ ": " ⧺ T.unpack got)
    pure got

-- | The production per-tick entry point, exactly as
--   @scripts\/ui_manager_boot.lua@'s @uiManager.update@ calls it.
tick ∷ LuaBackendState → IO ()
tick ls =
    evalOk ls "require('scripts.hud').update(0.1); return true"
        `shouldReturn` "true"

-- | Put the HUD in a band AND put the stubbed camera zoom in agreement
--   with it, so @hud.reconcileView@ is a genuine no-op until a test
--   moves the zoom itself ('setZoom').
setBand ∷ LuaBackendState → Text → IO ()
setBand ls band = do
    let z = case band of
                "zoomed_out" → "3.0"
                "zoomed_in"  → "0.5"
                _            → "1.5"
    evalOk ls ("probe.zoom = " <> z
               <> "; require('scripts.hud').currentView = '" <> band
               <> "'; return require('scripts.hud').currentView == '"
               <> band <> "'") `shouldReturn` "true"

-- | Move the camera's zoom WITHOUT touching @hud.currentView@, the way
--   coasting momentum crosses a band between ticks.
setZoom ∷ LuaBackendState → Text → IO ()
setZoom ls z =
    evalOk ls ("probe.zoom = " <> z <> "; return true") `shouldReturn` "true"

setMouse ∷ LuaBackendState → Int → Int → IO ()
setMouse ls x y =
    evalOk ls ("probe.mouse = { " <> tshow x <> ", " <> tshow y
               <> " }; return true") `shouldReturn` "true"

-- | The real pause overlay's own flag — the one
--   @uiManager.isGameplayView@ consults — rather than a stand-in
--   predicate.
openPauseMenu ∷ LuaBackendState → IO ()
openPauseMenu ls =
    evalOk ls "require('scripts.pause_menu').visible = true; return true"
        `shouldReturn` "true"

closePauseMenu ∷ LuaBackendState → IO ()
closePauseMenu ls =
    evalOk ls "require('scripts.pause_menu').visible = false; return true"
        `shouldReturn` "true"

-- | Game→Settings with @keepWorld@: the world and HUD stay visible and
--   @hud.hide()@ is deliberately skipped, so only @currentMenu@ moves.
openKeepWorldSettings ∷ LuaBackendState → IO ()
openKeepWorldSettings ls =
    evalOk ls "require('scripts.ui_manager').currentMenu = 'settings'; \
              \return require('scripts.hud').visible == true"
        `shouldReturn` "true"

-- * Assertions

gameplayInputActive ∷ LuaBackendState → IO Text
gameplayInputActive ls =
    evalOk ls "return require('scripts.ui_manager').isGameplayInputActive()"

-- | Compared INSIDE Lua: 'executeDebugLua' renders a returned string
--   quoted, so a Haskell-side text comparison would be against
--   @\"zoomed_out\"@ rather than @zoomed_out@.
currentView ∷ LuaBackendState → Text → IO ()
currentView ls expected =
    evalOk ls ("return require('scripts.hud').currentView == '"
               <> expected <> "'") `shouldReturn` "true"

-- | (zoom-map hover submissions, world hover submissions).
hoverCounts ∷ LuaBackendState → IO (Text, Text)
hoverCounts ls = do
    z ← evalOk ls "return probe.zoomHovers"
    w ← evalOk ls "return probe.worldHovers"
    pure (z, w)

-- | @\<page id\>:\<x\>,\<y\>@ of the last submission on each verb, so
--   the PAGE and COORDINATES are checked and not merely the call count.
--   Compared in Lua for the same quoting reason as 'currentView'.
lastZoomHover, lastWorldHover ∷ LuaBackendState → Text → IO ()
lastZoomHover  = lastHover "lastZoom"
lastWorldHover = lastHover "lastWorld"

lastHover ∷ Text → LuaBackendState → Text → IO ()
lastHover field ls expected =
    evalOk ls ("return probe." <> field <> " == '" <> expected <> "'")
        `shouldReturn` "true"

-- | A visible modal-exclusive page, so @UI.isInputBlocked()@ — and
--   therefore @uiManager.isGameplayInputActive()@ — goes false with the
--   view unpaused and @currentMenu@ still a gameplay view.
showModal ∷ EngineEnv → IO ()
showModal env = do
    let (h, m1) = createPage "hud_hover_gate_modal" LayerModal emptyUIPageManager
    writeIORef (uiManagerRef env) (showPage h m1)

hideModal ∷ EngineEnv → IO ()
hideModal env = writeIORef (uiManagerRef env) emptyUIPageManager
