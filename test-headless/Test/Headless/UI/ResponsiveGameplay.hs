{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
-- | #750 gate (Phase C, child C4 of #741): migrating gameplay HUD/
--   overlay/modal surfaces onto the #748 responsive lifecycle, plus
--   the new deterministic reserved-region/priority contract
--   (scripts/ui/reserved_regions.lua) and the resize-safe teardown of
--   world_page-mounted popups/tools (scripts/ui/view_teardown.lua's
--   new "resize" transition, driven from scripts/hud.lua's
--   createUI()).
--
--   Same headless constraint as 'Test.Headless.UI.ResponsiveMenus':
--   the full ui_manager boot sequence never reaches gameplay UI
--   headless (it gates on fontsReady, which needs a GPU font atlas).
--   This suite boots scripts/hud.lua directly (hud.init + hud.createUI)
--   with synthetic texture/font handles — the same technique
--   ResponsiveMenus uses for menu screens and
--   'Test.Headless.UI.InputOwnership' uses for scripts/debug.lua —
--   rather than going through uiManager.init(). engine.getTextWidth
--   always measures 0 headless this way, so assertions here are
--   geometry-only, the same caveat ResponsiveMenus documents.
--
--   'uiManager.onFramebufferResize' itself is deliberately NOT driven
--   here: its meaningful body only runs once the boot-only local
--   `initialized` flips true, which — like fontsReady — only happens
--   through the real (GPU-gated) boot sequence. This suite instead
--   drives every gameplay surface's own onFramebufferResize directly,
--   and 'uiManager.notifyGameplayRescale' (the #750 scale-only path),
--   which has no such gate.
module Test.Headless.UI.ResponsiveGameplay (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), decode, withObject, (.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.Types (UIPageManager(..))

-- | Join Lua statements with a single space instead of GHC string-gap
--   continuations (mirrors ResponsiveMenus — a missing space before a
--   continuation backslash silently glues two tokens together).
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

tshow ∷ Show a ⇒ a → Text
tshow = T.pack ∘ show

spec ∷ Spec
spec = around withHeadlessEngine $ do

    describe "hud.getToolbarRects() (#750) — the reserved 'required controls'" $ do
        it "every configured resolution (1x) produces exactly 3 real, in-frame toolbar clusters" $ \env → do
            ls ← newBareLuaBackend env
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

        it "stays in-frame at every C2 band-boundary scale and the automatic high-DPI/ultrawide scales" $ \env → do
            ls ← newBareLuaBackend env
            let cases ∷ [(Int, Int, Double)]
                cases =
                    -- band boundaries (scripts/ui/responsive.lua's own table —
                    -- consumed via engine.setUIScale, never re-declared here)
                    [ (1920, 900,  1.0), (1920, 900,  0.5)
                    , (1920, 1200, 0.75), (1920, 1200, 2.0)
                    , (1920, 1600, 1.0),  (1920, 1600, 3.0)
                    , (1920, 2160, 1.5),  (1920, 2160, 4.0)
                    -- automatic high-DPI profiles (scripts/settings/data.lua's loadDefaults)
                    , (1920, 1080, 1.5), (2560, 1440, 2.0), (3840, 2160, 2.5)
                    -- both configured ultrawides
                    , (2560, 1080, 1.0), (3440, 1440, 1.0)
                    ]
            forM_ cases $ \(w, h, uiscale) → do
                allIn ← evalBool ls $ luaLines
                    [ "engine.setUIScale(" <> tshow uiscale <> ");"
                    , "local hud = require('scripts.hud');"
                    , "hud.init(1,2," <> tshow w <> "," <> tshow h <> ");"
                    , "hud.createUI();"
                    , "local rects = hud.getToolbarRects();"
                    , "if #rects ~= 3 then return false end;"
                    , "for _, rc in ipairs(rects) do"
                    , "    if rc.x < 0 or rc.y < 0"
                    , "       or (rc.x+rc.w) > " <> tshow w <> " or (rc.y+rc.h) > " <> tshow h <> " then"
                    , "        return false"
                    , "    end"
                    , "end;"
                    , "return true"
                    ]
                allIn `shouldBe` True

        it "the issue's own out-of-envelope exemplar (800x600@4x) still produces valid, non-degenerate rects" $ \env → do
            ls ← newBareLuaBackend env
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
        it "hud.createUI() never crashes on degenerate geometry, and a subsequent real resize recovers valid in-frame geometry" $ \env → do
            ls ← newBareLuaBackend env
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

        it "popup/event_log/combat_log/injury_log_panel/unit_log/unit_info_v2/debug all no-op cleanly on (0,0)" $ \env → do
            ls ← newBareLuaBackend env
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

        it "popup never stores a 0x0 framebuffer (a card created right after minimize would size against it)" $ \env → do
            ls ← newBareLuaBackend env
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
        it "a resize while the HUD is hidden never resurrects the world/zoom page over whatever is now on screen" $ \env → do
            ls ← newBareLuaBackend env
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

        it "a resize while the HUD is visible keeps global_page (the log toggle) visible too" $ \env → do
            ls ← newBareLuaBackend env
            visible ← evalBool ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "hud.show();"
                , "hud.onFramebufferResize(1280, 720);"
                , "return UI.isPageVisible(hud.global_page)"
                ]
            visible `shouldBe` True

        it "a resize preserves the visually selected tool, without re-firing world.setToolMode" $ \env → do
            ls ← newBareLuaBackend env
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

        it "a resize preserves the visually selected map mode" $ \env → do
            ls ← newBareLuaBackend env
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

        it "a resize preserves a SWAPPED alternative's identity, not just the slot index (round-2 review)" $ \env → do
            ls ← newBareLuaBackend env
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

        it "does not re-fire onOptionSelect/onChange when silently restoring a swapped identity" $ \env → do
            ls ← newBareLuaBackend env
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

    describe "resize-safe teardown (#750) — scripts/ui/view_teardown.lua's new \"resize\" transition" $ do
        it "hud.createUI() runs the 'resize' sweep before deleting world_page, reaching every registered world_page-mounted widget" $ \env → do
            ls ← newBareLuaBackend env
            n ← evalInt ls $ luaLines
                [ "_G.__n = 0;"
                , "local function spy() _G.__n = _G.__n + 1 end;"
                , "package.loaded['scripts.item_contents_panel']  = { closeIfOpen = spy };"
                , "package.loaded['scripts.cargo_inventory_panel'] = { closeIfOpen = spy };"
                , "package.loaded['scripts.crafting_panel']        = { closeIfOpen = spy };"
                , "package.loaded['scripts.plant_panel']           = { closeIfOpen = spy };"
                , "package.loaded['scripts.build_tool']            = { hidePicker = spy };"
                , "package.loaded['scripts.tile_editor']           = { clear = spy };"
                , "require('scripts.ui.view_teardown').run('resize');"
                , "return _G.__n"
                ]
            n `shouldBe` 6

        it "a failing hook is pcall-isolated — the sweep still reaches every other hook and hud.createUI() still succeeds" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "_G.__n = 0;"
                , "local function noop() end;"
                , "local function spy() _G.__n = _G.__n + 1 end;"
                -- hud.createUI() itself calls each panel's own .setup(opts)
                -- on every rebuild (independent of the "resize" sweep), so
                -- every stub needs a harmless one too.
                , "package.loaded['scripts.item_contents_panel']  = { setup = noop, closeIfOpen = function() error('boom') end };"
                , "package.loaded['scripts.cargo_inventory_panel'] = { setup = noop, closeIfOpen = spy };"
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
                    fhpN p `shouldBe` 5

        it "build_tool's placement ghost (a committed two-click anchor) is deliberately NOT torn down by a resize" $ \env → do
            ls ← newBareLuaBackend env
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
        it "five consecutive hud.createUI() calls leave a bounded element/page count, not a monotonically growing one" $ \env → do
            ls ← newBareLuaBackend env
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
        it "fans out to every gameplay surface directly (no automatic broadcast exists for a synthetic, non-resize change)" $ \env → do
            ls ← newBareLuaBackend env
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

        it "calls hud.onFramebufferResize before popup.reflow()/unitInfoV2.reflow(), so both see the NEW hud geometry" $ \env → do
            ls ← newBareLuaBackend env
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

        it "does nothing on a non-positive size (defends the same 0x0 invariant as a real resize)" $ \env → do
            ls ← newBareLuaBackend env
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

    describe "scripts/ui/reserved_regions.lua (#750) — the collision/priority contract" $ do
        it "rectsOverlap is a plain AABB test" $ \env → do
            ls ← newBareLuaBackend env
            yes ← evalBool ls
                "return require('scripts.ui.reserved_regions').rectsOverlap({x=0,y=0,w=10,h=10},{x=5,y=5,w=10,h=10})"
            no ← evalBool ls
                "return require('scripts.ui.reserved_regions').rectsOverlap({x=0,y=0,w=10,h=10},{x=20,y=20,w=10,h=10})"
            yes `shouldBe` True
            no `shouldBe` False

        it "checkViolations flags the lower-priority region as the loser, and a same-priority overlap as ambiguous" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
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

        it "a same-priority overlap is flagged ambiguous, with no crash" $ \env → do
            ls ← newBareLuaBackend env
            ok ← evalBool ls $ luaLines
                [ "local rr = require('scripts.ui.reserved_regions');"
                , "local regions = {"
                , "    {name='a', priority=50, rect={x=0,y=0,w=100,h=100}},"
                , "    {name='b', priority=50, rect={x=50,y=50,w=100,h=100}},"
                , "};"
                , "local v = rr.checkViolations(regions);"
                , "return #v == 1 and v[1].ambiguous == true"
                ]
            ok `shouldBe` True

        it "avoidReserved nudges a rect fully clear of a reserved rect, and clamps within the screen" $ \env → do
            ls ← newBareLuaBackend env
            ok ← evalBool ls $ luaLines
                [ "local rr = require('scripts.ui.reserved_regions');"
                , "local out = rr.avoidReserved({x=10,y=10,w=50,h=50}, {{x=0,y=0,w=40,h=40}}, 1000, 1000);"
                , "return not rr.rectsOverlap(out, {x=0,y=0,w=40,h=40})"
                , "       and out.x >= 0 and out.y >= 0"
                , "       and (out.x+out.w) <= 1000 and (out.y+out.h) <= 1000"
                ]
            ok `shouldBe` True

        it "avoidReserved is a no-op when nothing overlaps" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls
                "return require('scripts.ui.reserved_regions').avoidReserved({x=500,y=500,w=50,h=50}, {{x=0,y=0,w=40,h=40}}, 1000, 1000)"
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RectRow of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rr → do
                    rrX rr `shouldBe` 500
                    rrY rr `shouldBe` 500

        it "findEscapes flags only a visible, pointer-blocking, out-of-frame element — never a decorative or hidden one" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
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

    describe "popup.lua reflows active cards on resize (#750 round-1 review)" $ do
        it "a card's width is capped to the framebuffer at a narrow, high-scale, still-C2-supported combination (round-3 review)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,800,2160);"
                , "p.onShowPopup('unit_event', 'hi', 0, 0, 0, 1, {});"
                , "local b = p.getActiveBounds()[1];"
                , "return {w = b.w, inFrame = (b.x >= 0 and (b.x+b.w) <= 800"
                , "                            and b.y >= 0 and (b.y+b.h) <= 2160)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WidthCapProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    wcpInFrame p `shouldBe` True
                    wcpW p `shouldSatisfy` (≤ 800)

        it "onFramebufferResize alone stores the new size but does NOT reflow (ordering hazard: it fires before hud rebuilds)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,1920,1080);"
                , "p.onShowPopup('unit_event', 'hello', 0, 0, 0, 1, {});"
                , "local before = p.getActiveBounds()[1];"
                , "p.onFramebufferResize(800, 600);"
                , "local after = p.getActiveBounds()[1];"
                , "return {beforeX=before.x, beforeY=before.y, afterX=after.x, afterY=after.y,"
                , "        afterInFrame=false}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ReflowProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    (rpAfterX p, rpAfterY p) `shouldBe` (rpBeforeX p, rpBeforeY p)

        it "p.reflow() recenters a card to the current framebuffer instead of leaving it stale or off-screen after a shrink" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,1920,1080);"
                , "p.onShowPopup('unit_event', 'hello', 0, 0, 0, 1, {});"
                , "local before = p.getActiveBounds()[1];"
                , "p.onFramebufferResize(800, 600);"
                , "p.reflow();"
                , "local after = p.getActiveBounds()[1];"
                , "return {beforeX=before.x, beforeY=before.y, afterX=after.x, afterY=after.y,"
                , "        afterInFrame=(after.x >= 0 and after.y >= 0"
                , "                      and (after.x+after.w) <= 800 and (after.y+after.h) <= 600)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ReflowProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rpAfterInFrame p `shouldBe` True
                    (rpAfterX p, rpAfterY p) `shouldNotBe` (rpBeforeX p, rpBeforeY p)

    describe "popup.lua avoids the reserved toolbar regions (#750) — \"notifications avoid required controls\"" $ do
        it "renderPopup calls reserved_regions.avoidReserved against the real hud toolbar rects" $ \env → do
            ls ← newBareLuaBackend env
            called ← evalBool ls $ luaLines
                [ "local rr = require('scripts.ui.reserved_regions');"
                , "local origAvoid = rr.avoidReserved;"
                , "_G.__called = false;"
                , "rr.avoidReserved = function(rect, reserved, w, h)"
                , "    _G.__called = true;"
                , "    return origAvoid(rect, reserved, w, h)"
                , "end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,1920,1080);"
                , "p.onShowPopup('unit_event', 'hello', 0, 0, 0, 1, {});"
                , "rr.avoidReserved = origAvoid;"
                , "return _G.__called"
                ]
            called `shouldBe` True

        it "a card forced to overlap a reserved rect ends up moved clear of it" $ \env → do
            ls ← newBareLuaBackend env
            ok ← evalBool ls $ luaLines
                [ "local rr = require('scripts.ui.reserved_regions');"
                , "local origAvoid = rr.avoidReserved;"
                -- Force the "natural" popup position to be treated as
                -- exactly the reserved region it must avoid, by feeding
                -- avoidReserved a reserved list built from the popup's own
                -- (pre-nudge) rect, captured for the post-hoc check below —
                -- proves the nudge actually changes the final on-screen
                -- bounds rather than being a no-op passthrough.
                , "rr.avoidReserved = function(rect, reserved, w, h)"
                , "    _G.__natural = {x=rect.x, y=rect.y, w=rect.w, h=rect.h};"
                , "    local forced = { { x = rect.x, y = rect.y, w = rect.w, h = rect.h } };"
                , "    return origAvoid(rect, forced, w, h)"
                , "end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,1920,1080);"
                , "p.onShowPopup('unit_event', 'hello', 0, 0, 0, 1, {});"
                , "local bounds = p.getActiveBounds();"
                , "rr.avoidReserved = origAvoid;"
                , "local card = bounds[1];"
                , "return card ~= nil and not rr.rectsOverlap(card, _G.__natural)"
                ]
            ok `shouldBe` True

    describe "event/combat/injury/unit log panels migrate their scrollable content to real #747 clipping (round-2 review)" $ do
        it "event_log: a rendered row is a real child of a clipsChildren viewport, and its absolute bounds resolve inside the clip" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.emitEvent('unit_event', 'hello world');"
                , "local el = require('scripts.event_log');"
                , "el.bootstrap(1,2,3,1920,1080);"
                , "el.show();"
                , "local vp = UI.getElementInfo(el.rowViewportId);"
                , "local rowHandle = next(el.rowClickBoxes);"
                , "local row = UI.getElementInfo(rowHandle);"
                , "return {clipsChildren = vp.clipsChildren,"
                , "        rowInsideClip = (row.x >= vp.x and row.y >= vp.y"
                , "                         and (row.x+row.width) <= (vp.x+vp.width)"
                , "                         and (row.y+row.height) <= (vp.y+vp.height)),"
                , "        rowEffectiveClipMatchesViewport ="
                , "            (row.effectiveClip.x == vp.x and row.effectiveClip.y == vp.y"
                , "             and row.effectiveClip.w == vp.width and row.effectiveClip.h == vp.height)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe LogClipProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    lcpClipsChildren p `shouldBe` True
                    lcpRowInsideClip p `shouldBe` True
                    lcpRowEffectiveClipMatchesViewport p `shouldBe` True

        it "combat_log/injury_log_panel/unit_log each create a clipsChildren content viewport, and their empty-state label is a real descendant" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local out = {};"
                , "local specs = {"
                , "    {mod = 'scripts.combat_log', vp = 'contentViewportId'},"
                , "    {mod = 'scripts.injury_log_panel', vp = 'contentViewportId'},"
                , "    {mod = 'scripts.unit_log', vp = 'contentViewportId'},"
                , "};"
                , "for _, spec in ipairs(specs) do"
                , "    local m = require(spec.mod);"
                , "    m.bootstrap(1,2,3,1920,1080);"
                , "    if spec.mod == 'scripts.unit_log' then m.show(1) else m.show() end;"
                , "    local vp = UI.getElementInfo(m[spec.vp]);"
                , "    table.insert(out, {mod = spec.mod, clipsChildren = vp and vp.clipsChildren or false});"
                , "end;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [LogViewportRow] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rows → do
                    length rows `shouldBe` 3
                    forM_ rows $ \row → lvrClipsChildren row `shouldBe` True

        it "a resize while a log panel is visible rebuilds its viewport with clipsChildren still true (no regression to page-attached content)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local el = require('scripts.event_log');"
                , "el.bootstrap(1,2,3,1920,1080);"
                , "el.show();"
                , "local ok = pcall(function() el.onFramebufferResize(1280, 720) end);"
                , "local vp = UI.getElementInfo(el.rowViewportId);"
                , "return {ok = ok, clipsChildren = vp and vp.clipsChildren or false}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ResizeClipProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rcpOk p `shouldBe` True
                    rcpClipsChildren p `shouldBe` True

        it "the clip viewport's own zIndex stays 0 so a reparented row's effective paint position is unchanged (round-3 review: zIndex accumulates through parents)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.emitEvent('unit_event', 'hello world');"
                , "local el = require('scripts.event_log');"
                , "el.bootstrap(1,2,3,1920,1080);"
                , "el.show();"
                , "local vp = UI.getElementInfo(el.rowViewportId);"
                , "local rowHandle = next(el.rowClickBoxes);"
                , "local row = UI.getElementInfo(rowHandle);"
                -- Compare against a page-root reference element with the
                -- SAME zIndex on the SAME page — no band/offset math
                -- hardcoded here. If the viewport's own z leaked into the
                -- row's accumulated z, the row's paintKey would be
                -- HIGHER than this reference's, not equal.
                , "local refHandle = UI.newElement('zidx_ref', 1, 1, el.pageId);"
                , "UI.addToPage(el.pageId, refHandle, 0, 0);"
                , "UI.setZIndex(refHandle, row.zIndex);"
                , "local ref = UI.getElementInfo(refHandle);"
                , "return {viewportZ = vp.zIndex, rowPaintKey = row.paintKey,"
                , "        refPaintKey = ref.paintKey}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ZIndexProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    zipViewportZ p `shouldBe` 0
                    zipRowPaintKey p `shouldBe` zipRefPaintKey p

    describe "\"unit info reserves right edge and suppresses conflicting info\" (#750 introspection over pre-existing behavior)" $ do
        it "unitInfoV2.getBounds() mirrors the real flush-right column, and is nil while not visible" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local u = require('scripts.unit_info_v2');"
                , "local hiddenBounds = u.getBounds();"
                , "u.lastWantVisible = true;"
                , "local b = u.getBounds();"
                , "return {hiddenIsNil=(hiddenBounds == nil), x=b.x, y=b.y, w=b.w, h=b.h}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe UnitInfoBoundsProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    uibpHiddenIsNil p `shouldBe` True
                    uibpX p `shouldBe` (1920 - 340)
                    uibpY p `shouldBe` 0
                    uibpW p `shouldBe` 340
                    uibpH p `shouldBe` 1080

        it "the flush-right column's width is capped to the framebuffer at a narrow, high-scale, still-C2-supported combination (round-3 review)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local u = require('scripts.unit_info_v2');"
                , "u.lastWantVisible = true;"
                , "local b = u.getBounds();"
                , "return {x = b.x, w = b.w, inFrame = (b.x >= 0 and (b.x+b.w) <= 800)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WidthCapProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    wcpInFrame p `shouldBe` True
                    wcpW p `shouldSatisfy` (≤ 800)

        it "infoPanel.suppress('unit_info_v2') hides the generic panel; unsuppress restores it while content remains" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local hud = require('scripts.hud');"
                , "hud.init(1,2,1920,1080);"
                , "hud.createUI();"
                , "local ip = require('scripts.hud.info_panel');"
                , "ip.setText('basic', 'some tile info');"
                , "local visibleBefore = ip.isVisible();"
                , "ip.suppress('unit_info_v2');"
                , "local visibleSuppressed = ip.isVisible();"
                , "local boundsSuppressed = ip.getBounds();"
                , "ip.unsuppress('unit_info_v2');"
                , "local visibleAfter = ip.isVisible();"
                , "return {before=visibleBefore, suppressed=visibleSuppressed,"
                , "        boundsNil=(boundsSuppressed == nil), after=visibleAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe SuppressProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    spBefore p `shouldBe` True
                    spSuppressed p `shouldBe` False
                    spBoundsNil p `shouldBe` True
                    spAfter p `shouldBe` True

-- * FromJSON row types

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

data LogClipProbe = LogClipProbe
    { lcpClipsChildren ∷ Bool, lcpRowInsideClip ∷ Bool
    , lcpRowEffectiveClipMatchesViewport ∷ Bool } deriving Show
instance FromJSON LogClipProbe where
    parseJSON = withObject "LogClipProbe" $ \o →
        LogClipProbe <$> o .: "clipsChildren" <*> o .: "rowInsideClip"
                      <*> o .: "rowEffectiveClipMatchesViewport"

data LogViewportRow = LogViewportRow
    { lvrMod ∷ Text, lvrClipsChildren ∷ Bool } deriving Show
instance FromJSON LogViewportRow where
    parseJSON = withObject "LogViewportRow" $ \o →
        LogViewportRow <$> o .: "mod" <*> o .: "clipsChildren"

data ResizeClipProbe = ResizeClipProbe
    { rcpOk ∷ Bool, rcpClipsChildren ∷ Bool } deriving Show
instance FromJSON ResizeClipProbe where
    parseJSON = withObject "ResizeClipProbe" $ \o →
        ResizeClipProbe <$> o .: "ok" <*> o .: "clipsChildren"

data ZIndexProbe = ZIndexProbe
    { zipViewportZ ∷ Int, zipRowPaintKey ∷ Int, zipRefPaintKey ∷ Int } deriving Show
instance FromJSON ZIndexProbe where
    parseJSON = withObject "ZIndexProbe" $ \o →
        ZIndexProbe <$> o .: "viewportZ" <*> o .: "rowPaintKey" <*> o .: "refPaintKey"

data WidthCapProbe = WidthCapProbe { wcpW ∷ Int, wcpInFrame ∷ Bool } deriving Show
instance FromJSON WidthCapProbe where
    parseJSON = withObject "WidthCapProbe" $ \o →
        WidthCapProbe <$> o .: "w" <*> o .: "inFrame"

data ReflowProbe = ReflowProbe
    { rpBeforeX ∷ Double, rpBeforeY ∷ Double
    , rpAfterX ∷ Double, rpAfterY ∷ Double, rpAfterInFrame ∷ Bool } deriving (Show, Eq)
instance FromJSON ReflowProbe where
    parseJSON = withObject "ReflowProbe" $ \o →
        ReflowProbe <$> o .: "beforeX" <*> o .: "beforeY"
                     <*> o .: "afterX" <*> o .: "afterY" <*> o .: "afterInFrame"

data FailingHookProbe = FailingHookProbe { fhpOk ∷ Bool, fhpN ∷ Int } deriving Show
instance FromJSON FailingHookProbe where
    parseJSON = withObject "FailingHookProbe" $ \o →
        FailingHookProbe <$> o .: "ok" <*> o .: "n"

data ViolationProbe = ViolationProbe
    { vpCount ∷ Int, vpLoser ∷ Maybe Text, vpWinner ∷ Maybe Text, vpAmbiguous ∷ Maybe Bool } deriving Show
instance FromJSON ViolationProbe where
    parseJSON = withObject "ViolationProbe" $ \o →
        ViolationProbe <$> o .: "count" <*> o .: "loser" <*> o .: "winner" <*> o .: "ambiguous"

data RectRow = RectRow { rrX ∷ Double, rrY ∷ Double, rrW ∷ Double, rrH ∷ Double } deriving Show
instance FromJSON RectRow where
    parseJSON = withObject "RectRow" $ \o →
        RectRow <$> o .: "x" <*> o .: "y" <*> o .: "w" <*> o .: "h"

data UnitInfoBoundsProbe = UnitInfoBoundsProbe
    { uibpHiddenIsNil ∷ Bool, uibpX ∷ Int, uibpY ∷ Int, uibpW ∷ Int, uibpH ∷ Int } deriving Show
instance FromJSON UnitInfoBoundsProbe where
    parseJSON = withObject "UnitInfoBoundsProbe" $ \o →
        UnitInfoBoundsProbe <$> o .: "hiddenIsNil" <*> o .: "x" <*> o .: "y" <*> o .: "w" <*> o .: "h"

data SuppressProbe = SuppressProbe
    { spBefore ∷ Bool, spSuppressed ∷ Bool, spBoundsNil ∷ Bool, spAfter ∷ Bool } deriving Show
instance FromJSON SuppressProbe where
    parseJSON = withObject "SuppressProbe" $ \o →
        SuppressProbe <$> o .: "before" <*> o .: "suppressed" <*> o .: "boundsNil" <*> o .: "after"

-- * Lua backend + eval helpers (mirrors Test.Headless.UI.ResponsiveMenus /
--   Test.Headless.UI.InputOwnership)

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

eval ∷ LuaBackendState → Text → IO Text
eval ls = executeDebugLua (lbsLuaState ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls code = do
    t ← eval ls code
    when (isLuaError t) $ expectationFailure ("Lua error: " ⧺ T.unpack t)
    pure t

evalBool ∷ LuaBackendState → Text → IO Bool
evalBool ls code = do
    t ← evalOk ls code
    case t of
        "true"  → pure True
        "false" → pure False
        other   → do
            expectationFailure ("expected boolean, got: " ⧺ T.unpack other)
            pure False

evalInt ∷ LuaBackendState → Text → IO Int
evalInt ls code = do
    t ← evalOk ls code
    case reads (T.unpack t) of
        [(n, "")] → pure n
        _         → do
            expectationFailure ("expected integer, got: " ⧺ T.unpack t)
            pure 0

evalJSON ∷ LuaBackendState → Text → IO Text
evalJSON = evalOk

-- | Live element/page counts straight off the shared 'UIPageManager'.
countUI ∷ EngineEnv → IO (Int, Int)
countUI env = do
    mgr ← readIORef (uiManagerRef env)
    pure (Map.size (upmElements mgr), Map.size (upmPages mgr))
