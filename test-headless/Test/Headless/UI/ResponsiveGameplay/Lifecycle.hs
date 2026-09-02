-- | Lifecycle owner of the #750 responsive-gameplay gate (#2126):
--   suite configuration isolation (#1357), toolbar geometry, the 0x0
--   minimize guard, hud.createUI() state preservation, event-log
--   tab/scroll preservation, resize-safe teardown, repeated-resize
--   state growth, uiManager.notifyGameplayRescale and the
--   reserved-region contract.
--
--   A 'SpecWith' fragment over the facade's single shared fixture —
--   see 'Test.Headless.UI.ResponsiveGameplay.Fixture'. This module
--   never boots an engine or a Lua VM of its own.
module Test.Headless.UI.ResponsiveGameplay.Lifecycle (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), decode, withObject, (.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Engine.Core.State (EngineEnv(..))
import Test.Headless.Harness.Isolation (isInsideIsolatedResourceRoot)
import UI.Types (UIPageManager(..))
import Test.Headless.UI.ResponsiveGameplay.Fixture

spec ∷ SpecWith SharedFixture
spec = do
    -- #1357's guard, for the same reason ResponsiveMenus carries one:
    -- the round-11 case below drives the real settingsMenu.onDefaults(),
    -- which persists factory keybinds through the production
    -- engine.saveKeybinds() to the cwd-relative
    -- config/keybinds.local.yaml. Nothing else in this suite would
    -- notice if 'withSharedFixture' stopped isolating the filesystem.
    describe "suite config isolation (#1357)" $
        it "runs inside the scratch resource root, never the checkout" $ \_ → do
            inScratch ← isInsideIsolatedResourceRoot
            inScratch `shouldBe` True

    describe "hud.getToolbarRects() (#750) — the reserved 'required controls'" $ do
        it "every configured resolution (1x) produces exactly 3 real, in-frame toolbar clusters" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local data = require('scripts.settings.data');"
                , "local hud = require('scripts.hud');"
                , "local out = {};"
                , "for _, e in ipairs(data.resolutions) do"
                , "    hud.init(1,2,e.width,e.height);"
                , "    hud.createUI();"
                , "    local rects = hud.getToolbarRects();"
                , "    local allIn = true;"
                , "    for _, rc in ipairs(rects) do"
                , "        if rc.x < 0 or rc.y < 0"
                , "           or (rc.x+rc.w) > e.width or (rc.y+rc.h) > e.height then"
                , "            allIn = false"
                , "        end"
                , "    end;"
                , "    table.insert(out, {w=e.width, h=e.height, count=#rects, allIn=allIn});"
                , "end;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [ToolbarRow] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rows → do
                    rows `shouldSatisfy` (not ∘ null)
                    forM_ rows $ \row → do
                        trCount row `shouldBe` 3
                        trAllIn row `shouldBe` True

        it "stays in-frame at every C2 band-boundary scale and the automatic high-DPI/ultrawide scales (round-11 review: derived from the shared Lua sources, not hand-copied)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-11 review: this used to hand-copy the band-
            -- boundary/auto-DPI/ultrawide matrix as a Haskell literal, so
            -- a future change to scripts/ui/responsive.lua's bands table
            -- or scripts/settings/data.lua's loadDefaults auto-scale
            -- multipliers could silently drift out of sync with what
            -- this suite actually exercises. The WHOLE matrix is now
            -- built and checked inside one Lua script instead: band-
            -- boundary cases iterate responsive.bands directly (its own
            -- minH/maxH/minScale/maxScale, never re-declared here), and
            -- the "automatic high-DPI" cases call the REAL
            -- data.loadDefaults() with each of data.resolutions' entries
            -- (which already includes both configured ultrawides)
            -- stubbed in via engine.loadDefaultConfig — the tested scale
            -- is whatever data.current.uiScale comes out as, i.e. the
            -- real is1080p/is1440p/is4K multiplier logic, not a
            -- hardcoded guess.
            r ← evalJSON ls $ luaLines
                [ "local responsive = require('scripts.ui.responsive');"
                , "local data = require('scripts.settings.data');"
                , "local hud = require('scripts.hud');"
                , "local function checkAt(w, h, uiscale)"
                , "    engine.setUIScale(uiscale);"
                , "    hud.init(1,2,w,h);"
                , "    hud.createUI();"
                , "    local rects = hud.getToolbarRects();"
                , "    if #rects ~= 3 then return false end;"
                , "    for _, rc in ipairs(rects) do"
                , "        if rc.x < 0 or rc.y < 0"
                , "           or (rc.x+rc.w) > w or (rc.y+rc.h) > h then"
                , "            return false"
                , "        end"
                , "    end;"
                , "    return true"
                , "end;"
                , "local out = {};"
                , "for _, b in ipairs(responsive.bands) do"
                , "    table.insert(out, {label = 'band_' .. b.maxH .. '_min',"
                , "        ok = checkAt(1920, b.maxH, b.minScale)});"
                , "    table.insert(out, {label = 'band_' .. b.maxH .. '_max',"
                , "        ok = checkAt(1920, b.maxH, b.maxScale)});"
                , "end;"
                , "local origLoadCfg = engine.loadDefaultConfig;"
                , "for _, e in ipairs(data.resolutions) do"
                , "    engine.loadDefaultConfig = function()"
                , "        return e.width, e.height, 'fullscreen', 1.0, true, 60, 1, 100, false, 'nearest'"
                , "    end;"
                , "    data.loadDefaults();"
                , "    local scale = data.current.uiScale;"
                , "    table.insert(out, {label = 'auto_' .. e.width .. 'x' .. e.height,"
                , "        ok = checkAt(e.width, e.height, scale)});"
                , "end;"
                , "engine.loadDefaultConfig = origLoadCfg;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [LabeledOkRow] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rows → do
                    -- 4 bands × 2 + 16 configured resolutions (data.resolutions,
                    -- which already includes both ultrawides) = 24.
                    length rows `shouldSatisfy` (≥ 20)
                    forM_ rows $ \row →
                        (lorLabel row, lorOk row) `shouldBe` (lorLabel row, True)

        it "the issue's own out-of-envelope exemplar (800x600@4x) still produces valid, non-degenerate rects" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,600);"
                , "hud.createUI();"
                , "local rects = hud.getToolbarRects();"
                , "local anyDegenerate = false;"
                , "for _, rc in ipairs(rects) do"
                , "    if rc.w <= 0 or rc.h <= 0 then anyDegenerate = true end"
                , "end;"
                , "return {count=#rects, anyDegenerate=anyDegenerate}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe DegenerateProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    dpCount p `shouldBe` 3
                    dpAnyDegenerate p `shouldBe` False

    describe "0x0 minimize never rebuilds gameplay against a degenerate framebuffer (#750)" $ do
        -- hud/worldView/contextMenu/buildToolRemoteWarning are guarded at
        -- their CALL SITE (scripts/ui_manager_boot.lua's manual-forward
        -- block, `if width > 0 and height > 0 then ...`), not inside
        -- hud.onFramebufferResize itself — the same shape worldView/
        -- contextMenu/buildToolRemoteWarning already had before #750 (they
        -- never guarded themselves either). That call site can't be driven
        -- directly here: it only runs once uiManager's boot-only
        -- `initialized` flip happens, which — like fontsReady — needs the
        -- real (GPU-gated) boot sequence, exactly the constraint
        -- ResponsiveMenus documents for uiManager.onFramebufferResize
        -- itself. This test instead proves the fallback property: even
        -- called directly with (0,0) — bypassing the guard — hud.lua
        -- itself never crashes, and a subsequent real resize still
        -- recovers valid, in-frame geometry.
        it "hud.createUI() never crashes on degenerate geometry, and a subsequent real resize recovers valid in-frame geometry" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local before = hud.getToolbarRects();"
                , "local ok0 = pcall(function() hud.onFramebufferResize(0, 0) end);"
                , "local ok1 = pcall(function() hud.onFramebufferResize(1600, 900) end);"
                , "local afterRestore = hud.getToolbarRects();"
                , "local restoredIn = true;"
                , "for _, rc in ipairs(afterRestore) do"
                , "    if rc.x < 0 or rc.y < 0 or (rc.x+rc.w) > 1600 or (rc.y+rc.h) > 900 then"
                , "        restoredIn = false"
                , "    end"
                , "end;"
                , "return {ok0=ok0, ok1=ok1, beforeCount=#before,"
                , "        afterRestoreCount=#afterRestore, restoredIn=restoredIn}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ZeroMinimizeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    zmOk0 p `shouldBe` True
                    zmOk1 p `shouldBe` True
                    zmBeforeCount p `shouldBe` 3
                    zmAfterRestoreCount p `shouldBe` 3
                    zmRestoredIn p `shouldBe` True

        it "popup/event_log/combat_log/injury_log_panel/unit_log/unit_info_v2/debug all no-op cleanly on (0,0)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local mods = {"
                , "  'scripts.popup', 'scripts.event_log', 'scripts.combat_log',"
                , "  'scripts.injury_log_panel', 'scripts.unit_log',"
                , "  'scripts.unit_info_v2', 'scripts.debug' };"
                , "local out = {};"
                , "for _, name in ipairs(mods) do"
                , "    local m = require(name);"
                , "    local ok = pcall(function() m.onFramebufferResize(0, 0) end);"
                , "    out[name] = ok"
                , "end;"
                , "return out"
                ]
            r `shouldNotSatisfy` isLuaError

        it "popup never stores a 0x0 framebuffer (a card created right after minimize would size against it)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,1920,1080);"
                , "p.onFramebufferResize(0, 0);"
                , "return {fbW=p.fbW, fbH=p.fbH}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe FbSize of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just s → do
                    fsW s `shouldBe` 1920
                    fsH s `shouldBe` 1080

    describe "hud.createUI() preserves visibility state and toolbar selection across a rebuild (#750 round-1 review)" $ do
        it "a resize while the HUD is hidden never resurrects the world/zoom page over whatever is now on screen" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "hud.show();"
                , "hud.hide();"
                , "hud.onFramebufferResize(1600, 900);"
                , "return {visible=hud.visible,"
                , "        zoomPageVisible=UI.isPageVisible(hud.zoom_page),"
                , "        worldPageVisible=UI.isPageVisible(hud.world_page)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe HiddenResizeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    hrpVisible p `shouldBe` False
                    hrpZoomPageVisible p `shouldBe` False
                    hrpWorldPageVisible p `shouldBe` False

        it "a resize while the HUD is visible keeps global_page (the log toggle) visible too" $ \(env, ls) → do
            resetFixture env ls
            visible ← evalBool ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "hud.show();"
                , "hud.onFramebufferResize(1280, 720);"
                , "return UI.isPageVisible(hud.global_page)"
                ]
            visible `shouldBe` True

        it "a resize preserves the visually selected tool, without re-firing world.setToolMode" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local toggle = require('scripts.ui.toggle');"
                , "_G.__setToolModeCalls = 0;"
                , "local origSetToolMode = world.setToolMode;"
                , "world.setToolMode = function(...) _G.__setToolModeCalls = _G.__setToolModeCalls + 1; return origSetToolMode(...) end;"
                , "toggle.applyOptionByName(hud.toolToggleId, 'tool_mine');"
                , "local callsAfterSelect = _G.__setToolModeCalls;"
                , "hud.onFramebufferResize(1600, 900);"
                , "world.setToolMode = origSetToolMode;"
                , "return {selected=toggle.getSelectedName(hud.toolToggleId),"
                , "        callsAfterSelect=callsAfterSelect, callsAfterResize=_G.__setToolModeCalls}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ToolPreserveProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    tppSelected p `shouldBe` "tool_mine"
                    tppCallsAfterSelect p `shouldBe` 1
                    tppCallsAfterResize p `shouldBe` 1

        it "a resize preserves the visually selected map mode" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local toggle = require('scripts.ui.toggle');"
                , "toggle.applyOptionByName(hud.mapToggleId, 'map_temp');"
                , "hud.onFramebufferResize(1280, 720);"
                , "return {toggle.getSelectedName(hud.mapToggleId)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [Text] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just names → names `shouldBe` ["map_temp"]

        it "a resize preserves a SWAPPED alternative's identity, not just the slot index (round-2 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local toggle = require('scripts.ui.toggle');"
                -- map_pressure is an OPTION nested under slot 1's
                -- default identity (map_temp), not a direct slot name —
                -- picking it swaps it INTO slot 1 (toggle.lua's
                -- applyOption), which a naive index-only restore can't
                -- reproduce after a rebuild recreates slot 1 back at its
                -- hardcoded default (map_temp).
                , "toggle.applyOptionByName(hud.mapToggleId, 'map_pressure');"
                , "local beforeIdx = toggle.getSelectedIndex(hud.mapToggleId);"
                , "hud.onFramebufferResize(1280, 720);"
                , "return {selected = {toggle.getSelectedName(hud.mapToggleId)},"
                , "        idxUnchanged = (toggle.getSelectedIndex(hud.mapToggleId) == beforeIdx)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe SwapPreserveProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    spSelected p `shouldBe` ["map_pressure"]
                    spIdxUnchanged p `shouldBe` True

        it "does not re-fire onOptionSelect/onChange when silently restoring a swapped identity" $ \(env, ls) → do
            resetFixture env ls
            n ← evalInt ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local toggle = require('scripts.ui.toggle');"
                , "toggle.applyOptionByName(hud.mapToggleId, 'map_pressure');"
                , "_G.__setMapModeCalls = 0;"
                , "local origSetMapMode = world.setMapMode;"
                , "world.setMapMode = function(...) _G.__setMapModeCalls = _G.__setMapModeCalls + 1; return origSetMapMode(...) end;"
                , "hud.onFramebufferResize(1280, 720);"
                , "world.setMapMode = origSetMapMode;"
                , "return _G.__setMapModeCalls"
                ]
            n `shouldBe` 0

        it "a resize preserves keyboard CONTROL focus (#745) on a toolbar toggle, restoring it onto the rebuilt control by name (round-10 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "hud.show();"
                , "local toggle = require('scripts.ui.toggle');"
                , "local handles = toggle.getElementHandles(hud.toolToggleId);"
                , "UI.setControlFocus(handles[1]);"
                , "local nameBefore = UI.getElementInfo(handles[1]).name;"
                , "local hadFocusBefore = UI.hasControlFocus(handles[1]);"
                , "hud.onFramebufferResize(1600, 900);"
                , "local focusHandle = UI.getControlFocus();"
                , "local nameAfter = focusHandle and UI.getElementInfo(focusHandle).name or nil;"
                , "return {hadFocusBefore = hadFocusBefore, hasFocusAfter = (focusHandle ~= nil),"
                , "        sameName = (nameAfter == nameBefore)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe HudControlFocusProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    hcfpHadFocusBefore p `shouldBe` True
                    hcfpHasFocusAfter p `shouldBe` True
                    hcfpSameName p `shouldBe` True

        it "a resize while the HUD is hidden does not attempt to restore control focus (nothing to restore, no crash)" $ \(env, ls) → do
            resetFixture env ls
            ok ← evalBool ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local toggle = require('scripts.ui.toggle');"
                , "local handles = toggle.getElementHandles(hud.toolToggleId);"
                , "UI.setControlFocus(handles[1]);"
                , "local ok = pcall(function() hud.onFramebufferResize(1600, 900) end);"
                , "return ok"
                ]
            ok `shouldBe` True

    describe "event_log preserves its active tab and scroll position across a resize (#750 round-4 review)" $ do
        it "a resize keeps the active (non-default) tab selected, both logically and on the tabbar widget itself" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.emitEvent('unit_event', 'e1');"
                , "local el = require('scripts.event_log');"
                , "el.bootstrap(1,2,3,1920,1080);"
                , "el.show();"
                , "local tabbar = require('scripts.ui.tabbar');"
                , "tabbar.selectByKey(el.tabbarId, 'unit_event');"
                , "el.onFramebufferResize(1600, 900);"
                , "return {activeTabKey = el.activeTabKey,"
                , "        tabbarKey = tabbar.getSelectedKey(el.tabbarId)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe TabPreserveProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    tpActiveTabKey p `shouldBe` "unit_event"
                    tpTabbarKey p `shouldBe` "unit_event"

        it "a resize preserves a nonzero scroll offset instead of forcing it back to 0" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "for i = 1, 300 do engine.emitEvent('unit_event', 'event ' .. i) end;"
                , "local el = require('scripts.event_log');"
                , "el.bootstrap(1,2,3,1920,1080);"
                , "el.show();"
                , "local sb = require('scripts.ui.scrollbar');"
                , "sb.setScrollOffset(el.scrollbarId, 5);"
                , "local before = el.scrollOffset;"
                , "el.onFramebufferResize(1600, 900);"
                , "return {before = before, after = el.scrollOffset}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ScrollPreserveProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    sppBefore p `shouldSatisfy` (> 0)
                    sppAfter p `shouldBe` sppBefore p

        it "eventLog.show() still resets to the 'All' tab on a genuine fresh open" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.emitEvent('unit_event', 'e1');"
                , "local el = require('scripts.event_log');"
                , "el.bootstrap(1,2,3,1920,1080);"
                , "el.show();"
                , "local tabbar = require('scripts.ui.tabbar');"
                , "tabbar.selectByKey(el.tabbarId, 'unit_event');"
                , "el.hide();"
                , "el.show();"
                , "return {activeTabKey = el.activeTabKey,"
                , "        tabbarKey = tabbar.getSelectedKey(el.tabbarId)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe TabPreserveProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    tpActiveTabKey p `shouldBe` "all"
                    tpTabbarKey p `shouldBe` "all"

    describe "resize-safe teardown (#750) — scripts/ui/view_teardown.lua's new \"resize\" transition" $ do
        it "hud.createUI() runs the 'resize' sweep before deleting world_page, reaching every registered world_page-mounted widget" $ \(env, ls) → do
            resetFixture env ls
            n ← evalInt ls $ luaLines
                [ "_G.__n = 0;"
                , "local function spy() _G.__n = _G.__n + 1 end;"
                -- #1238: the item-contents popup is a LEVEL of the
                -- container-window stack now, not a registry entry of
                -- its own -- one owner, one hook, one close.
                , "package.loaded['scripts.cargo_inventory_panel'] = { closeIfOpen = spy };"
                , "package.loaded['scripts.crafting_panel']        = { closeIfOpen = spy };"
                , "package.loaded['scripts.plant_panel']           = { closeIfOpen = spy };"
                , "package.loaded['scripts.build_tool']            = { hidePicker = spy };"
                , "package.loaded['scripts.tile_editor']           = { clear = spy };"
                , "require('scripts.ui.view_teardown').run('resize');"
                , "return _G.__n"
                ]
            n `shouldBe` 5

        it "a failing hook is pcall-isolated — the sweep still reaches every other hook and hud.createUI() still succeeds" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "_G.__n = 0;"
                , "local function noop() end;"
                , "local function spy() _G.__n = _G.__n + 1 end;"
                -- hud.createUI() itself calls each panel's own .setup(opts)
                -- on every rebuild (independent of the "resize" sweep), so
                -- every stub needs a harmless one too.
                , "package.loaded['scripts.cargo_inventory_panel'] = { setup = noop, closeIfOpen = function() error('boom') end };"
                , "package.loaded['scripts.crafting_panel']        = { setup = noop, closeIfOpen = spy };"
                , "package.loaded['scripts.plant_panel']           = { setup = noop, closeIfOpen = spy };"
                , "package.loaded['scripts.build_tool']            = { setup = noop, hidePicker = spy };"
                , "package.loaded['scripts.tile_editor']           = { setup = noop, clear = spy };"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local ok = pcall(function() hud.createUI() end);"
                , "return {ok=ok, n=_G.__n}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe FailingHookProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    fhpOk p `shouldBe` True
                    -- Four surviving hooks across two createUI() calls'
                    -- worth of sweeps, with the container window's own
                    -- raising. #1238 folded the item-contents popup into
                    -- that window, so there are five registry entries
                    -- here rather than six.
                    fhpN p `shouldBe` 4

        it "build_tool's placement ghost (a committed two-click anchor) is deliberately NOT torn down by a resize" $ \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls
                "return require('scripts.ui.view_teardown')"
            -- Structural proof, not a behavior probe (build_tool.enterPlacement
            -- needs a real world/unit to drive meaningfully): the registry
            -- entry for "build_tool_placement" carries a zoomBand hook (the
            -- pre-existing, already-shipped view transition) but the module
            -- source added no "resize" hook for it — see the header comment
            -- in scripts/ui/view_teardown.lua for why (#750 issue thread
            -- amendment: a layout-only rebuild must never cancel a
            -- committed/armed two-click designation anchor).
            r `shouldNotSatisfy` isLuaError

    describe "repeated resize never grows live UI state (#750)" $ do
        it "five consecutive hud.createUI() calls leave a bounded element/page count, not a monotonically growing one" $ \(env, ls) → do
            resetFixture env ls
            setup ← evalOk ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI()"
                ]
            setup `shouldNotSatisfy` isLuaError
            (elemsBefore, pagesBefore) ← countUI env
            r ← evalOk ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "for i = 1, 4 do hud.createUI() end;"
                , "return 'done'"
                ]
            r `shouldNotSatisfy` isLuaError
            (elemsAfter, pagesAfter) ← countUI env
            -- Bounded, not strictly equal — hud.createUI() also owns
            -- non-toolbar content (info panel tabs etc.) whose element
            -- count can shift slightly with layout, but it must not grow
            -- unbounded across repeated rebuilds.
            elemsAfter `shouldSatisfy` (≤ elemsBefore * 2 + 50)
            pagesAfter `shouldBe` pagesBefore

    describe "uiManager.notifyGameplayRescale (#750) — the scale-only Settings Apply/Save/Back path" $ do
        it "fans out to every gameplay surface directly (no automatic broadcast exists for a synthetic, non-resize change)" $ \(env, ls) → do
            resetFixture env ls
            n ← evalInt ls $ luaLines
                [ "_G.__n = 0;"
                , "local function spy(w, h) _G.__n = _G.__n + 1 end;"
                -- popup gets BOTH onFramebufferResize (stores fbW/fbH)
                -- and reflow (repositions active cards); unit_info_v2
                -- only gets reflow (its onFramebufferResize is a
                -- deliberate no-op — see the ordering note below).
                , "local stub = { onFramebufferResize = spy, reflow = spy };"
                , "package.loaded['scripts.world_view']            = stub;"
                , "package.loaded['scripts.hud']                   = stub;"
                , "package.loaded['scripts.ui.context_menu']       = stub;"
                , "package.loaded['scripts.build_tool_remote_warning'] = stub;"
                , "package.loaded['scripts.popup']                 = stub;"
                , "package.loaded['scripts.event_log']             = stub;"
                , "package.loaded['scripts.combat_log']            = stub;"
                , "package.loaded['scripts.injury_log_panel']      = stub;"
                , "package.loaded['scripts.unit_log']              = stub;"
                , "package.loaded['scripts.unit_info_v2']          = stub;"
                , "package.loaded['scripts.debug']                 = stub;"
                , "local uiManager = require('scripts.ui_manager');"
                , "uiManager.moduleReady.worldView = true;"
                , "uiManager.moduleReady.hud = true;"
                , "uiManager.moduleReady.buildToolRemoteWarning = true;"
                , "uiManager.moduleReady.popupsAndLogs = true;"
                , "uiManager.notifyGameplayRescale(1920, 1080);"
                , "return _G.__n"
                ]
            -- worldView, hud, contextMenu, buildToolRemoteWarning,
            -- eventLog, combatLog, injuryLog, unitLog, debug: 1 call
            -- each (9). popup: 2 (onFramebufferResize + reflow).
            -- unit_info_v2: 1 (reflow only). Total 12.
            n `shouldBe` 12

        it "calls hud.onFramebufferResize before popup.reflow()/unitInfoV2.reflow(), so both see the NEW hud geometry" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "_G.__order = {};"
                , "local hudStub = { onFramebufferResize = function() table.insert(_G.__order, 'hud') end };"
                , "local popupStub = {"
                , "    onFramebufferResize = function() end,"
                , "    reflow = function() table.insert(_G.__order, 'popupReflow') end };"
                , "local uivStub = { onFramebufferResize = function() end,"
                , "    reflow = function() table.insert(_G.__order, 'unitInfoV2Reflow') end };"
                , "local noop = { onFramebufferResize = function() end };"
                , "package.loaded['scripts.world_view']            = noop;"
                , "package.loaded['scripts.hud']                   = hudStub;"
                , "package.loaded['scripts.ui.context_menu']       = noop;"
                , "package.loaded['scripts.build_tool_remote_warning'] = noop;"
                , "package.loaded['scripts.popup']                 = popupStub;"
                , "package.loaded['scripts.event_log']             = noop;"
                , "package.loaded['scripts.combat_log']            = noop;"
                , "package.loaded['scripts.injury_log_panel']      = noop;"
                , "package.loaded['scripts.unit_log']              = noop;"
                , "package.loaded['scripts.unit_info_v2']          = uivStub;"
                , "package.loaded['scripts.debug']                 = noop;"
                , "local uiManager = require('scripts.ui_manager');"
                , "uiManager.moduleReady.hud = true;"
                , "uiManager.moduleReady.popupsAndLogs = true;"
                , "uiManager.notifyGameplayRescale(1920, 1080);"
                , "return _G.__order"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [Text] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just order → do
                    order `shouldSatisfy` (elem "hud")
                    let hudIdx = length (takeWhile (≠ "hud") order)
                    -- both reflows must appear AFTER "hud" in the order
                    forM_ ["popupReflow", "unitInfoV2Reflow"] $ \name →
                        case lookup name (zip order [0 ∷ Int ..]) of
                            Nothing → expectationFailure (T.unpack name ⧺ " never ran")
                            Just idx → idx `shouldSatisfy` (> hudIdx)

        it "does nothing on a non-positive size (defends the same 0x0 invariant as a real resize)" $ \(env, ls) → do
            resetFixture env ls
            n ← evalInt ls $ luaLines
                [ "_G.__n = 0;"
                , "local stub = { onFramebufferResize = function() _G.__n = _G.__n + 1 end };"
                , "package.loaded['scripts.hud'] = stub;"
                , "local uiManager = require('scripts.ui_manager');"
                , "uiManager.moduleReady.hud = true;"
                , "uiManager.notifyGameplayRescale(0, 0);"
                , "return _G.__n"
                ]
            n `shouldBe` 0

        it "forwards to test_arena when moduleReady, gated exactly like every other surface here (round-12 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-12 review: test_arena was omitted from this
            -- scale-only fan-out entirely (and from the real-resize
            -- forward set in ui_manager_boot.lua, which can't be driven
            -- headless at all — see this file's own docstring on
            -- uiManager.onFramebufferResize's `initialized` gate — so
            -- that half is verified against a real running engine
            -- instead). Mirrors the existing hud-stub pattern exactly.
            n ← evalInt ls $ luaLines
                [ "_G.__n = 0;"
                , "local stub = { onFramebufferResize = function() _G.__n = _G.__n + 1 end };"
                , "package.loaded['scripts.test_arena'] = stub;"
                , "local uiManager = require('scripts.ui_manager');"
                , "uiManager.moduleReady.testArena = true;"
                , "uiManager.notifyGameplayRescale(1920, 1080);"
                , "return _G.__n"
                ]
            n `shouldBe` 1

        it "does NOT forward to test_arena when it isn't moduleReady (never initialized this session)" $ \(env, ls) → do
            resetFixture env ls
            n ← evalInt ls $ luaLines
                [ "_G.__n = 0;"
                , "local stub = { onFramebufferResize = function() _G.__n = _G.__n + 1 end };"
                , "package.loaded['scripts.test_arena'] = stub;"
                , "local uiManager = require('scripts.ui_manager');"
                , "uiManager.moduleReady.testArena = false;"
                , "uiManager.notifyGameplayRescale(1920, 1080);"
                , "return _G.__n"
                ]
            n `shouldBe` 0

        it "settingsMenu.onDefaults() reaches the REAL gameplay HUD when data.loadDefaults() changes the UI scale (round-11 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-11 review: every other case in this describe
            -- block drives notifyGameplayRescale directly against
            -- STUBBED gameplay modules — none of them prove the actual
            -- CALLER (settingsMenu.onDefaults(), which conditionally
            -- fans out only when data.loadDefaults() actually changed
            -- data.current.uiScale — see settings_menu.lua) really
            -- reaches it. This drives the REAL scripts.hud module (no
            -- stub) through the REAL settingsMenu.onDefaults() entry
            -- point, with engine.loadDefaultConfig stubbed to force a
            -- genuine scale change (the gating condition). hud and
            -- settingsMenu are booted at DIFFERENT framebuffer sizes so
            -- a successful fan-out (hud picking up settingsMenu's own
            -- fbW/fbH) is unambiguous — if onDefaults's fan-out call
            -- were ever removed again, hud.fbW/fbH would silently stay
            -- at its own original size instead.
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "hud.show();"
                , "local uiManager = require('scripts.ui_manager');"
                , "uiManager.moduleReady.hud = true;"
                , "local settingsMenu = require('scripts.settings_menu');"
                , "settingsMenu.init(1,2,3,1600,900);"
                , "local data = require('scripts.settings.data');"
                , "data.current.uiScale = 1.0;"
                , "local origLoadCfg = engine.loadDefaultConfig;"
                , "engine.loadDefaultConfig = function()"
                , "    return 1600, 900, 'fullscreen', 3.0, true, 60, 1, 100, false, 'nearest'"
                , "end;"
                , "settingsMenu.onDefaults();"
                , "engine.loadDefaultConfig = origLoadCfg;"
                , "return {scaleChanged = (data.current.uiScale ~= 1.0),"
                , "        hudFbW = hud.fbW, hudFbH = hud.fbH,"
                , "        settingsFbW = settingsMenu.fbW, settingsFbH = settingsMenu.fbH}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe DefaultsRescaleProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    drpScaleChanged p `shouldBe` True
                    drpHudFbW p `shouldBe` drpSettingsFbW p
                    drpHudFbH p `shouldBe` drpSettingsFbH p
                    drpHudFbW p `shouldBe` 1600
                    drpHudFbH p `shouldBe` 900

    -- #750 round-4 review: reserved_regions.lua's own functions are pure
    -- (no engine/UI/page state at all — see the module's own header
    -- comment) and independent of each other, so — per the issue's own
    -- cost guardrail spec addition ("share one booted headless engine +
    -- Lua environment across cases... no per-case engine boots") —
    -- every case below shares ONE newBareLuaBackend/engine instead of
    -- one per assertion.
    describe "scripts/ui/reserved_regions.lua (#750) — the collision/priority contract" $
        it "rectsOverlap, checkViolations, avoidReserved, and findEscapes all behave correctly on one shared backend" $ \(env, ls) → do
            resetFixture env ls

            do  yes ← evalBool ls
                    "return require('scripts.ui.reserved_regions').rectsOverlap({x=0,y=0,w=10,h=10},{x=5,y=5,w=10,h=10})"
                no ← evalBool ls
                    "return require('scripts.ui.reserved_regions').rectsOverlap({x=0,y=0,w=10,h=10},{x=20,y=20,w=10,h=10})"
                yes `shouldBe` True
                no `shouldBe` False

            do  r ← evalJSON ls $ luaLines
                    [ "local rr = require('scripts.ui.reserved_regions');"
                    , "local regions = {"
                    , "    {name='toolbar', priority=100, rect={x=0,y=0,w=100,h=100}},"
                    , "    {name='info',    priority=50,  rect={x=50,y=50,w=100,h=100}},"
                    , "    {name='far',     priority=50,  rect={x=900,y=900,w=10,h=10}},"
                    , "};"
                    , "local v = rr.checkViolations(regions);"
                    , "return {count=#v, loser=v[1] and v[1].loser.name, winner=v[1] and v[1].winner.name,"
                    , "        ambiguous=v[1] and v[1].ambiguous}"
                    ]
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ViolationProbe of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just p → do
                        vpCount p `shouldBe` 1
                        vpLoser p `shouldBe` Just "info"
                        vpWinner p `shouldBe` Just "toolbar"
                        vpAmbiguous p `shouldBe` Just False

            do  ok ← evalBool ls $ luaLines
                    [ "local rr = require('scripts.ui.reserved_regions');"
                    , "local regions = {"
                    , "    {name='a', priority=50, rect={x=0,y=0,w=100,h=100}},"
                    , "    {name='b', priority=50, rect={x=50,y=50,w=100,h=100}},"
                    , "};"
                    , "local v = rr.checkViolations(regions);"
                    , "return #v == 1 and v[1].ambiguous == true"
                    ]
                ok `shouldBe` True

            do  ok ← evalBool ls $ luaLines
                    [ "local rr = require('scripts.ui.reserved_regions');"
                    , "local out = rr.avoidReserved({x=10,y=10,w=50,h=50}, {{x=0,y=0,w=40,h=40}}, 1000, 1000);"
                    , "return not rr.rectsOverlap(out, {x=0,y=0,w=40,h=40})"
                    , "       and out.x >= 0 and out.y >= 0"
                    , "       and (out.x+out.w) <= 1000 and (out.y+out.h) <= 1000"
                    ]
                ok `shouldBe` True

            do  r ← evalJSON ls
                    "return require('scripts.ui.reserved_regions').avoidReserved({x=500,y=500,w=50,h=50}, {{x=0,y=0,w=40,h=40}}, 1000, 1000)"
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RectRow of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just rr → do
                        rrX rr `shouldBe` 500
                        rrY rr `shouldBe` 500

            -- #750 round-16 review: avoidReserved used to clear each
            -- reservation ONE AT A TIME in sequence, so a small push
            -- chosen to clear a LATER reservation could silently
            -- re-overlap an EARLIER one already cleared, with nothing
            -- left to re-check it. The reviewer's own counter-example:
            -- rect {100,400,300,100} with reservations {0,0,300,1000}
            -- (a near-full-height left column) then {500,400,100,100}
            -- (a small block to the right) on a 1000x1000 screen — the
            -- old sequential version landed back inside the first
            -- reservation even though a feasible fully-clear placement
            -- exists ({600,400}).
            do  r ← evalJSON ls $ luaLines
                    [ "local rr = require('scripts.ui.reserved_regions');"
                    , "local reservations = {"
                    , "    {x=0, y=0, w=300, h=1000},"
                    , "    {x=500, y=400, w=100, h=100},"
                    , "};"
                    , "local out = rr.avoidReserved({x=100,y=400,w=300,h=100}, reservations, 1000, 1000);"
                    , "local clearsAll = true;"
                    , "for _, r in ipairs(reservations) do"
                    , "    if rr.rectsOverlap(out, r) then clearsAll = false end"
                    , "end;"
                    , "return {x = out.x, y = out.y, clearsAll = clearsAll}"
                    ]
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe MultiRegionAvoidProbe of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just p → do
                        mrapClearsAll p `shouldBe` True
                        mrapX p `shouldBe` 600
                        mrapY p `shouldBe` 400

            do  r ← evalJSON ls $ luaLines
                    [ "local rr = require('scripts.ui.reserved_regions');"
                    , "local elements = {"
                    , "    {name='onscreen',   x=10, y=10, width=20, height=20, visible=true,  pageVisible=true,  pointerBlocking=true},"
                    , "    {name='offscreen',  x=-5, y=10, width=20, height=20, visible=true,  pageVisible=true,  pointerBlocking=true},"
                    , "    {name='hiddenPage', x=-5, y=10, width=20, height=20, visible=true,  pageVisible=false, pointerBlocking=true},"
                    , "    {name='decorative', x=-5, y=10, width=20, height=20, visible=true,  pageVisible=true,  pointerBlocking=false},"
                    , "    {name='offRight',   x=990, y=10, width=20, height=20, visible=true, pageVisible=true,  pointerBlocking=true},"
                    , "};"
                    , "local escapes = rr.findEscapes(elements, 1000, 1000);"
                    , "local names = {};"
                    , "for _, e in ipairs(escapes) do table.insert(names, e.name) end;"
                    , "return names"
                    ]
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [Text] of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just names → names `shouldMatchList` ["offscreen", "offRight"]

            do  r ← evalJSON ls $ luaLines
                    [ "local rr = require('scripts.ui.reserved_regions');"
                    , "local w1 = rr.maxAvailableWidth(50, 100, {{x=100,y=0,w=100,h=900}}, 1000);"
                    , "local w2 = rr.maxAvailableWidth(950, 50, {{x=100,y=0,w=100,h=900}}, 1000);"
                    , "return {w1 = w1, w2 = w2}"
                    ]
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe MaxWidthProbe of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just p → do
                        mwpW1 p `shouldBe` 800
                        mwpW2 p `shouldBe` 1000

-- * FromJSON row types and helpers owned by the lifecycle groups

data ToolbarRow = ToolbarRow { trW ∷ Int, trH ∷ Int, trCount ∷ Int, trAllIn ∷ Bool } deriving Show
instance FromJSON ToolbarRow where
    parseJSON = withObject "ToolbarRow" $ \o →
        ToolbarRow <$> o .: "w" <*> o .: "h" <*> o .: "count" <*> o .: "allIn"

data DegenerateProbe = DegenerateProbe { dpCount ∷ Int, dpAnyDegenerate ∷ Bool } deriving Show
instance FromJSON DegenerateProbe where
    parseJSON = withObject "DegenerateProbe" $ \o →
        DegenerateProbe <$> o .: "count" <*> o .: "anyDegenerate"

data ZeroMinimizeProbe = ZeroMinimizeProbe
    { zmOk0 ∷ Bool, zmOk1 ∷ Bool, zmBeforeCount ∷ Int
    , zmAfterRestoreCount ∷ Int, zmRestoredIn ∷ Bool } deriving Show
instance FromJSON ZeroMinimizeProbe where
    parseJSON = withObject "ZeroMinimizeProbe" $ \o →
        ZeroMinimizeProbe <$> o .: "ok0" <*> o .: "ok1" <*> o .: "beforeCount"
                           <*> o .: "afterRestoreCount" <*> o .: "restoredIn"

data FbSize = FbSize { fsW ∷ Int, fsH ∷ Int } deriving Show
instance FromJSON FbSize where
    parseJSON = withObject "FbSize" $ \o → FbSize <$> o .: "fbW" <*> o .: "fbH"

data HiddenResizeProbe = HiddenResizeProbe
    { hrpVisible ∷ Bool, hrpZoomPageVisible ∷ Bool, hrpWorldPageVisible ∷ Bool } deriving Show
instance FromJSON HiddenResizeProbe where
    parseJSON = withObject "HiddenResizeProbe" $ \o →
        HiddenResizeProbe <$> o .: "visible" <*> o .: "zoomPageVisible" <*> o .: "worldPageVisible"

data ToolPreserveProbe = ToolPreserveProbe
    { tppSelected ∷ Text, tppCallsAfterSelect ∷ Int, tppCallsAfterResize ∷ Int } deriving Show
instance FromJSON ToolPreserveProbe where
    parseJSON = withObject "ToolPreserveProbe" $ \o →
        ToolPreserveProbe <$> o .: "selected" <*> o .: "callsAfterSelect" <*> o .: "callsAfterResize"

data SwapPreserveProbe = SwapPreserveProbe
    { spSelected ∷ [Text], spIdxUnchanged ∷ Bool } deriving Show
instance FromJSON SwapPreserveProbe where
    parseJSON = withObject "SwapPreserveProbe" $ \o →
        SwapPreserveProbe <$> o .: "selected" <*> o .: "idxUnchanged"

data MaxWidthProbe = MaxWidthProbe { mwpW1 ∷ Double, mwpW2 ∷ Double } deriving Show
instance FromJSON MaxWidthProbe where
    parseJSON = withObject "MaxWidthProbe" $ \o →
        MaxWidthProbe <$> o .: "w1" <*> o .: "w2"

data TabPreserveProbe = TabPreserveProbe
    { tpActiveTabKey ∷ Text, tpTabbarKey ∷ Text } deriving Show
instance FromJSON TabPreserveProbe where
    parseJSON = withObject "TabPreserveProbe" $ \o →
        TabPreserveProbe <$> o .: "activeTabKey" <*> o .: "tabbarKey"

data ScrollPreserveProbe = ScrollPreserveProbe
    { sppBefore ∷ Int, sppAfter ∷ Int } deriving Show
instance FromJSON ScrollPreserveProbe where
    parseJSON = withObject "ScrollPreserveProbe" $ \o →
        ScrollPreserveProbe <$> o .: "before" <*> o .: "after"

data HudControlFocusProbe = HudControlFocusProbe
    { hcfpHadFocusBefore ∷ Bool, hcfpHasFocusAfter ∷ Bool, hcfpSameName ∷ Bool } deriving Show
instance FromJSON HudControlFocusProbe where
    parseJSON = withObject "HudControlFocusProbe" $ \o →
        HudControlFocusProbe <$> o .: "hadFocusBefore" <*> o .: "hasFocusAfter" <*> o .: "sameName"

data LabeledOkRow = LabeledOkRow { lorLabel ∷ Text, lorOk ∷ Bool } deriving (Show, Eq)
instance FromJSON LabeledOkRow where
    parseJSON = withObject "LabeledOkRow" $ \o →
        LabeledOkRow <$> o .: "label" <*> o .: "ok"

data MultiRegionAvoidProbe = MultiRegionAvoidProbe
    { mrapX ∷ Double, mrapY ∷ Double, mrapClearsAll ∷ Bool } deriving Show
instance FromJSON MultiRegionAvoidProbe where
    parseJSON = withObject "MultiRegionAvoidProbe" $ \o →
        MultiRegionAvoidProbe <$> o .: "x" <*> o .: "y" <*> o .: "clearsAll"

data DefaultsRescaleProbe = DefaultsRescaleProbe
    { drpScaleChanged ∷ Bool, drpHudFbW ∷ Int, drpHudFbH ∷ Int
    , drpSettingsFbW ∷ Int, drpSettingsFbH ∷ Int } deriving Show
instance FromJSON DefaultsRescaleProbe where
    parseJSON = withObject "DefaultsRescaleProbe" $ \o →
        DefaultsRescaleProbe <$> o .: "scaleChanged"
                              <*> o .: "hudFbW" <*> o .: "hudFbH"
                              <*> o .: "settingsFbW" <*> o .: "settingsFbH"

data FailingHookProbe = FailingHookProbe { fhpOk ∷ Bool, fhpN ∷ Int } deriving Show
instance FromJSON FailingHookProbe where
    parseJSON = withObject "FailingHookProbe" $ \o →
        FailingHookProbe <$> o .: "ok" <*> o .: "n"

data ViolationProbe = ViolationProbe
    { vpCount ∷ Int, vpLoser ∷ Maybe Text, vpWinner ∷ Maybe Text, vpAmbiguous ∷ Maybe Bool } deriving Show
instance FromJSON ViolationProbe where
    parseJSON = withObject "ViolationProbe" $ \o →
        ViolationProbe <$> o .: "count" <*> o .: "loser" <*> o .: "winner" <*> o .: "ambiguous"

-- | Live element/page counts straight off the shared 'UIPageManager'.
countUI ∷ EngineEnv → IO (Int, Int)
countUI env = do
    mgr ← readIORef (uiManagerRef env)
    pure (Map.size (upmElements mgr), Map.size (upmPages mgr))
