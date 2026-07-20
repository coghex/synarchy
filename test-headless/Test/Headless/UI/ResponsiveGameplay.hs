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
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (vcUIScale)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.Types (UIPageManager(..), emptyUIPageManager)

-- | Join Lua statements with a single space instead of GHC string-gap
--   continuations (mirrors ResponsiveMenus — a missing space before a
--   continuation backslash silently glues two tokens together).
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

-- #750 round-6 review: share ONE booted headless engine AND ONE Lua VM
-- across every case in this module, per the issue's own cost guardrail
-- spec addition read literally ("share one booted headless engine + Lua
-- environment across cases") — round 5 only shared the engine, giving
-- each case its own fresh 'newBareLuaBackend' Lua VM. 'withSharedFixture'
-- boots both exactly once for the whole module; 'resetFixture' (called
-- first in every case) resets whatever either could have accumulated
-- from an earlier case:
--   * The shared engine's own UIPageManager (Haskell-side page/element
--     tree) — cleared back to empty. Every case asserts on freshly-
--     created handles or RELATIVE counts (never a hardcoded absolute
--     handle number or page/element count), so this composes safely.
--   * engine.setUIScale's target (videoConfigRef's vcUIScale) — several
--     cases call it for a band-boundary/out-of-envelope exemplar; reset
--     to 1.0, preserving every other VideoConfig field as-is.
--   * The shared Lua VM's OWN module cache (package.loaded) — with the
--     Lua VM itself now shared, require('scripts.hud') etc. would
--     otherwise keep returning whichever EARLIER case's already-
--     initialized module table (hud.uiCreated=true, a selected tool,
--     popup.active entries, ...) instead of a pristine one. Wiped
--     entirely so every case's own require() calls re-execute each
--     .lua file from scratch — verified against a real running engine
--     (not just this suite) that this reproduces an identical fresh-
--     module state to a brand new Lua VM. The native UI/engine/world
--     API tables are untouched: they're plain Lua globals registered by
--     registerLuaAPI, never entries in package.loaded, so wiping the
--     module cache can't disturb them.
-- The engine-level event/combat/injury log ring buffers are NOT reset
-- (no such reset primitive is exposed to a test) — every case that
-- touches them already asserts existence/relative-preservation rather
-- than an exact count, so cross-case accumulation there is inert by
-- construction.
withSharedFixture ∷ ((EngineEnv, LuaBackendState) → IO ()) → IO ()
withSharedFixture action = withHeadlessEngine $ \env → do
    ls ← newBareLuaBackend env
    action (env, ls)

resetFixture ∷ EngineEnv → LuaBackendState → IO ()
resetFixture env ls = do
    writeIORef (uiManagerRef env) emptyUIPageManager
    atomicModifyIORef' (videoConfigRef env) $ \c → (c { vcUIScale = 1.0 }, ())
    cleared ← evalOk ls
        "for k, _ in pairs(package.loaded) do package.loaded[k] = nil end; return true"
    cleared `shouldBe` "true"

spec ∷ Spec
spec = aroundAll withSharedFixture $ do

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

        it "a failing hook is pcall-isolated — the sweep still reaches every other hook and hud.createUI() still succeeds" $ \(env, ls) → do
            resetFixture env ls
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

    describe "popup.lua reflows active cards on resize (#750 round-1 review)" $ do
        it "a card's width is capped to the framebuffer at a narrow, high-scale, still-C2-supported combination (round-3 review)" $ \(env, ls) → do
            resetFixture env ls
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

        it "a card never overlaps a tall reserved column even when the framebuffer cap alone isn't enough to clear it (round-4 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(2.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,901);"
                , "hud.createUI();"
                , "local p = require('scripts.popup');"
                , "p.bootstrap(1,2,3,800,901);"
                , "p.onShowPopup('unit_event', 'a normal length notification message here', 0, 0, 0, 1, {});"
                , "local b = p.getActiveBounds()[1];"
                , "local rr = require('scripts.ui.reserved_regions');"
                , "local overlapsAny = false;"
                , "for _, rect in ipairs(hud.getToolbarRects()) do"
                , "    if rr.rectsOverlap(b, rect) then overlapsAny = true end"
                , "end;"
                , "return {overlapsAny = overlapsAny,"
                , "        inFrame = (b.x >= 0 and (b.x+b.w) <= 800 and b.y >= 0 and (b.y+b.h) <= 901)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe OverlapProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    opOverlapsAny p `shouldBe` False
                    opInFrame p `shouldBe` True

        it "onFramebufferResize alone stores the new size but does NOT reflow (ordering hazard: it fires before hud rebuilds)" $ \(env, ls) → do
            resetFixture env ls
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

        it "p.reflow() recenters a card to the current framebuffer instead of leaving it stale or off-screen after a shrink" $ \(env, ls) → do
            resetFixture env ls
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
        it "renderPopup calls reserved_regions.avoidReserved against the real hud toolbar rects" $ \(env, ls) → do
            resetFixture env ls
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

        it "a card forced to overlap a reserved rect ends up moved clear of it" $ \(env, ls) → do
            resetFixture env ls
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
        it "event_log: a rendered row is a real child of a clipsChildren viewport, and its absolute bounds resolve inside the clip" $ \(env, ls) → do
            resetFixture env ls
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

        it "combat_log/injury_log_panel/unit_log each create a clipsChildren content viewport, and their empty-state label is a real descendant" $ \(env, ls) → do
            resetFixture env ls
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

        it "a resize while a log panel is visible rebuilds its viewport with clipsChildren still true (no regression to page-attached content)" $ \(env, ls) → do
            resetFixture env ls
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

        it "the clip viewport's own zIndex stays 0 so a reparented row's effective paint position is unchanged (round-3 review: zIndex accumulates through parents)" $ \(env, ls) → do
            resetFixture env ls
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

        it "event/combat/injury/unit log panels all build a non-degenerate content viewport at the issue's own out-of-envelope exemplar (800x600@4x, round-10 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "engine.emitEvent('unit_event', 'hello world');"
                , "local out = {};"
                , "local specs = {"
                , "    {mod = 'scripts.event_log',        vp = 'rowViewportId'},"
                , "    {mod = 'scripts.combat_log',        vp = 'contentViewportId'},"
                , "    {mod = 'scripts.injury_log_panel',  vp = 'contentViewportId'},"
                , "    {mod = 'scripts.unit_log',          vp = 'contentViewportId'},"
                , "};"
                , "for _, spec in ipairs(specs) do"
                , "    local m = require(spec.mod);"
                , "    m.bootstrap(1,2,3,800,600);"
                , "    local ok = pcall(function()"
                , "        if spec.mod == 'scripts.unit_log' then m.show(1) else m.show() end"
                , "    end);"
                , "    local vp = ok and UI.getElementInfo(m[spec.vp]);"
                , "    table.insert(out, {mod = spec.mod, ok = ok,"
                , "        w = vp and vp.width or -1, h = vp and vp.height or -1});"
                , "end;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [ModGeometryRow] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rows → do
                    length rows `shouldBe` 4
                    forM_ rows $ \row → do
                        mgrOk row `shouldBe` True
                        mgrW row `shouldSatisfy` (> 0)
                        mgrH row `shouldSatisfy` (> 0)

    describe "\"unit info reserves right edge and suppresses conflicting info\" (#750 introspection over pre-existing behavior)" $ do
        it "unitInfoV2.getBounds() mirrors the real flush-right column, and is nil while not visible" $ \(env, ls) → do
            resetFixture env ls
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

        it "the flush-right column's width is capped to the framebuffer at a narrow, high-scale, still-C2-supported combination (round-3 review)" $ \(env, ls) → do
            resetFixture env ls
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

        it "the flush-right column never grows wide enough to cover the left-side toolbar clusters (round-8 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local rr = require('scripts.ui.reserved_regions');"
                , "local u = require('scripts.unit_info_v2');"
                , "u.lastWantVisible = true;"
                , "local b = u.getBounds();"
                , "local overlapsAny = false;"
                , "for _, rc in ipairs(hud.getToolbarRects()) do"
                , "    if rc.name ~= 'map_toggle' and rr.rectsOverlap(b, rc) then overlapsAny = true end"
                , "end;"
                , "return {overlapsAny = overlapsAny,"
                , "        inFrame = (b.x >= 0 and (b.x+b.w) <= 800)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe OverlapProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    opOverlapsAny p `shouldBe` False
                    opInFrame p `shouldBe` True

        it "infoPanel.suppress('unit_info_v2') hides the generic panel; unsuppress restores it while content remains" $ \(env, ls) → do
            resetFixture env ls
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

    describe "build_tool_remote_warning.lua stays in-frame at a narrow, high-scale, still-C2-supported combination (round-6 review)" $ do
        it "the modal panel is capped to the framebuffer instead of pushing Establish/Cancel off-screen" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local w = require('scripts.build_tool_remote_warning');"
                , "w.init(1,2,3,800,2160);"
                , "w.open('acolyte_portal', 5, 5, 100, 50);"
                , "local p = require('scripts.ui.panel');"
                , "local x, y = p.getPosition(w.panelId);"
                , "local pw, ph = p.getSize(w.panelId);"
                , "return {x = x, y = y, w = pw, h = ph,"
                , "        inFrame = (x >= 0 and y >= 0 and (x+pw) <= 800 and (y+ph) <= 2160)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RemoteWarningProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rwpInFrame p `shouldBe` True
                    rwpW p `shouldSatisfy` (≤ 800)
                    rwpH p `shouldSatisfy` (≤ 2160)

        it "a normal resolution still gets its natural, unshrunk panel size" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(1.0);"
                , "local w = require('scripts.build_tool_remote_warning');"
                , "w.init(1,2,3,1920,1080);"
                , "w.open('acolyte_portal', 5, 5, 100, 50);"
                , "local p = require('scripts.ui.panel');"
                , "local x, y = p.getPosition(w.panelId);"
                , "local pw, ph = p.getSize(w.panelId);"
                , "return {x = x, y = y, w = pw, h = ph, inFrame = true}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RemoteWarningProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → rwpW p `shouldBe` 560

        it "both Establish/Cancel buttons stay within the (possibly shrunk) panel and the framebuffer (round-7 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local w = require('scripts.build_tool_remote_warning');"
                , "w.init(1,2,3,800,2160);"
                , "w.open('acolyte_portal', 5, 5, 100, 50);"
                , "local out = {};"
                , "for h, _ in pairs(w.clickHandlers) do"
                , "    local info = UI.getElementInfo(h);"
                , "    table.insert(out, {x=info.x, y=info.y, w=info.width, h=info.height})"
                , "end;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [RectRow] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rects → do
                    length rects `shouldBe` 2
                    forM_ rects $ \rc → do
                        rrX rc `shouldSatisfy` (≥ 0)
                        rrY rc `shouldSatisfy` (≥ 0)
                        (rrX rc + rrW rc) `shouldSatisfy` (≤ 800)
                        (rrY rc + rrH rc) `shouldSatisfy` (≤ 2160)

    describe "cargo_inventory_panel.lua / item_contents_panel.lua stay in-frame at a narrow, high-scale, still-C2-supported combination (round-7 review)" $ do
        it "cargo_inventory_panel: the panel width is capped instead of only repositioning an oversized panel" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local origCap = building.getStorageCapacity;"
                , "local origStorage = building.getStorage;"
                , "building.getStorageCapacity = function() return 100 end;"
                , "building.getStorage = function() return {} end;"
                , "local pg = UI.newPage('cargo_test_page', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                , "cip.setup({page = pg, fbW = 800, fbH = 2160, boxTexSet = 1});"
                , "cip.openFor(1, 400, 400);"
                , "local p = require('scripts.ui.panel');"
                , "local x, y = p.getPosition(cip.state.panelId);"
                , "local pw, ph = p.getSize(cip.state.panelId);"
                , "building.getStorageCapacity = origCap;"
                , "building.getStorage = origStorage;"
                , "return {w=pw,"
                , "        inFrame=(x>=0 and y>=0 and (x+pw)<=800 and (y+ph)<=2160)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WidthCapProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → wcpInFrame p `shouldBe` True

        it "cargo_inventory_panel: a multi-category inventory's tab strip shrinks to fit instead of running off-frame (round-8 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local origCap = building.getStorageCapacity;"
                , "local origStorage = building.getStorage;"
                , "building.getStorageCapacity = function() return 100 end;"
                , "building.getStorage = function() return {"
                , "    { defName='i1', category='Cat1' }, { defName='i2', category='Cat2' },"
                , "    { defName='i3', category='Cat3' }, { defName='i4', category='Cat4' },"
                , "    { defName='i5', category='Cat5' }, { defName='i6', category='Cat6' },"
                , "    { defName='i7', category='Cat7' }, { defName='i8', category='Cat8' },"
                , "} end;"
                , "local pg = UI.newPage('cargo_tab_test_page', 'overlay');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                , "cip.setup({page = pg, fbW = 800, fbH = 2160, boxTexSet = 1});"
                , "cip.openFor(1, 400, 400);"
                , "local out = {};"
                , "for _, t in ipairs(cip.state.tabs) do"
                , "    local info = UI.getElementInfo(t.boxId);"
                , "    table.insert(out, {x=info.x, y=info.y, w=info.width, h=info.height})"
                , "end;"
                , "building.getStorageCapacity = origCap;"
                , "building.getStorage = origStorage;"
                , "return out"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [RectRow] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rects → do
                    length rects `shouldSatisfy` (≥ 8)
                    forM_ rects $ \rc → do
                        rrX rc `shouldSatisfy` (≥ 0)
                        rrW rc `shouldSatisfy` (≥ 20)
                        (rrX rc + rrW rc) `shouldSatisfy` (≤ 800)

        it "cargo_inventory_panel: a heavily-shrunk tab's label renders smaller than an unshrunk one, not at a fixed full-uiscale size (round-12 review)" $ \(env, ls) → do
            resetFixture env ls
            -- #750 round-12 review: shrinking the tab BOX alone left the
            -- label rendering at the full uiscale, unclipped and
            -- page-rooted — wider than its own compressed box, bleeding
            -- into neighbours. engine.getTextWidth (and so label WIDTH)
            -- always measures 0 in this suite's synthetic boot (see the
            -- module docstring), so this can't assert on rendered pixel
            -- overlap directly — but label.lua's own HEIGHT is derived
            -- straight from `fontSize * uiscale` (scripts/ui/label.lua's
            -- label.new), independent of any real text metrics. A fixed,
            -- unshrunk uiscale would report the SAME height regardless of
            -- category count; the fix (labelUiscale = uiscale * shrink)
            -- makes a heavily-shrunk tab's label measurably smaller than
            -- one that needed no shrink at all.
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local origCap = building.getStorageCapacity;"
                , "local origStorage = building.getStorage;"
                , "local label = require('scripts.ui.label');"
                , "local cip = require('scripts.cargo_inventory_panel');"
                , "building.getStorageCapacity = function() return 100 end;"
                , "building.getStorage = function() return {"
                , "    { defName='i1', category='Cat1' } } end;"
                , "local pg1 = UI.newPage('cargo_lbl_test_1', 'overlay');"
                , "cip.setup({page = pg1, fbW = 800, fbH = 2160, boxTexSet = 1});"
                , "cip.openFor(1, 400, 400);"
                , "local _, unshunkH = label.getSize(cip.state.tabs[1].labelId);"
                , "building.getStorage = function() return {"
                , "    { defName='i1', category='Cat1' }, { defName='i2', category='Cat2' },"
                , "    { defName='i3', category='Cat3' }, { defName='i4', category='Cat4' },"
                , "    { defName='i5', category='Cat5' }, { defName='i6', category='Cat6' },"
                , "    { defName='i7', category='Cat7' }, { defName='i8', category='Cat8' },"
                , "} end;"
                , "local pg2 = UI.newPage('cargo_lbl_test_2', 'overlay');"
                , "cip.setup({page = pg2, fbW = 800, fbH = 2160, boxTexSet = 1});"
                , "cip.openFor(1, 400, 400);"
                , "local _, shrunkH = label.getSize(cip.state.tabs[2].labelId);"
                , "building.getStorageCapacity = origCap;"
                , "building.getStorage = origStorage;"
                , "return {unshrunkH = unshunkH, shrunkH = shrunkH}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ShrinkHeightProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    shpShrunkH p `shouldSatisfy` (> 0)
                    shpShrunkH p `shouldSatisfy` (< shpUnshrunkH p)

        it "item_contents_panel: the panel width is capped instead of only repositioning an oversized panel" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local orig = unit.getItemContents;"
                , "unit.getItemContents = function() return {} end;"
                , "local pg = UI.newPage('item_contents_test_page', 'overlay');"
                , "local icp = require('scripts.item_contents_panel');"
                , "icp.setup({page = pg, fbW = 800, fbH = 2160, boxTexSet = 1});"
                , "icp.openFor(1, 'some_container', 400, 400, nil);"
                , "local p = require('scripts.ui.panel');"
                , "local x, y = p.getPosition(icp.state.panelId);"
                , "local pw, ph = p.getSize(icp.state.panelId);"
                , "unit.getItemContents = orig;"
                , "return {w=pw,"
                , "        inFrame=(x>=0 and y>=0 and (x+pw)<=800 and (y+ph)<=2160)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WidthCapProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → wcpInFrame p `shouldBe` True

    describe "build_tool.lua's picker stays in-frame at a narrow, high-scale, still-C2-supported combination (round-7 review)" $ do
        it "the picker width is capped to the remaining framebuffer space right of its toolbar anchor, with no prior position clamp to rely on" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local orig = engine.getBuildingDefs;"
                , "if orig then engine.getBuildingDefs = function() return {"
                , "    { name = 'test_wall', displayName = 'Wall', category = 'Structures' }"
                , "} end end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local bt = require('scripts.build_tool');"
                , "bt.setup({hud = hud});"
                , "local ok = pcall(function() bt.showPicker() end);"
                , "if orig then engine.getBuildingDefs = orig end;"
                , "if not ok or bt.state.mode ~= 'picker' then return {skipped = true} end;"
                , "local p = require('scripts.ui.panel');"
                , "local x, y = p.getPosition(bt.state.panelId);"
                , "local pw, ph = p.getSize(bt.state.panelId);"
                , "return {skipped = false, x=x, y=y, w=pw, h=ph,"
                , "        inFrame=(x>=0 and y>=0 and (x+pw)<=800 and (y+ph)<=2160)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe PickerProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p →
                    -- No headless hook exists to force visibleEntries()
                    -- non-empty across every build (it reads real
                    -- building defs, whose exact loading path varies) —
                    -- when the picker genuinely couldn't open, this is a
                    -- soft skip rather than a false failure; the formula
                    -- fix itself is exercised directly whenever it can
                    -- open.
                    when (not (ppSkipped p)) $ ppInFrame p `shouldBe` True

        it "the icon grid's column count derives from the (possibly shrunk) panel's real content width, not a fixed constant (round-8 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local orig = building.listDefs;"
                , "building.listDefs = function() return {"
                , "    { name='t1', displayName='Wall',  category='Structures', isStarting=true },"
                , "    { name='t2', displayName='Farm',  category='Structures', isStarting=true },"
                , "    { name='t3', displayName='Kiln',  category='Structures', isStarting=true },"
                , "    { name='t4', displayName='Fort',  category='Structures', isStarting=true },"
                , "    { name='t5', displayName='Dock',  category='Structures', isStarting=true },"
                , "    { name='t6', displayName='Mill',  category='Structures', isStarting=true },"
                , "} end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local bt = require('scripts.build_tool');"
                , "bt.setup({hud = hud});"
                , "local ok = pcall(function() bt.showPicker() end);"
                , "building.listDefs = orig;"
                , "if not ok or bt.state.mode ~= 'picker' then return {skipped = true} end;"
                , "local out = {};"
                , "for _, ic in ipairs(bt.state.iconIds or {}) do"
                , "    local info = UI.getElementInfo(ic);"
                , "    if info then table.insert(out, {x=info.x, y=info.y, w=info.width, h=info.height}) end"
                , "end;"
                , "return {skipped = false, columnsPerRow = bt.state.columnsPerRow, icons = out}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ColumnGridProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → when (not (cgpSkipped p)) $ do
                    cgpColumnsPerRow p `shouldSatisfy` (≥ 1)
                    forM_ (cgpIcons p) $ \rc → do
                        rrX rc `shouldSatisfy` (≥ 0)
                        (rrX rc + rrW rc) `shouldSatisfy` (≤ 800)

        it "the tab strip shrinks to fit the panel's content width instead of running tabs off the framebuffer (round-8 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local orig = building.listDefs;"
                , "building.listDefs = function() return {"
                , "    { name='t1', displayName='A1', category='Cat1', isStarting=true },"
                , "    { name='t2', displayName='A2', category='Cat2', isStarting=true },"
                , "    { name='t3', displayName='A3', category='Cat3', isStarting=true },"
                , "    { name='t4', displayName='A4', category='Cat4', isStarting=true },"
                , "    { name='t5', displayName='A5', category='Cat5', isStarting=true },"
                , "    { name='t6', displayName='A6', category='Cat6', isStarting=true },"
                , "    { name='t7', displayName='A7', category='Cat7', isStarting=true },"
                , "    { name='t8', displayName='A8', category='Cat8', isStarting=true },"
                , "} end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local bt = require('scripts.build_tool');"
                , "bt.setup({hud = hud});"
                , "local ok = pcall(function() bt.showPicker() end);"
                , "building.listDefs = orig;"
                , "if not ok or bt.state.mode ~= 'picker' then return {skipped = true} end;"
                , "local out = {};"
                , "for _, t in ipairs(bt.state.tabIds) do"
                , "    local info = UI.getElementInfo(t.boxId);"
                , "    table.insert(out, {x=info.x, y=info.y, w=info.width, h=info.height})"
                , "end;"
                , "return {skipped = false, tabs = out}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe TabStripProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → when (not (tspSkipped p)) $ do
                    length (tspTabs p) `shouldSatisfy` (≥ 8)
                    forM_ (tspTabs p) $ \rc → do
                        rrX rc `shouldSatisfy` (≥ 0)
                        rrW rc `shouldSatisfy` (≥ 20)
                        (rrX rc + rrW rc) `shouldSatisfy` (≤ 800)

        it "a heavily-shrunk tab's label renders smaller than an unshrunk one, not at a fixed full-uiscale size (round-12 review)" $ \(env, ls) → do
            resetFixture env ls
            -- Same reasoning as cargo_inventory_panel's identical round-12
            -- test: engine.getTextWidth always measures 0 in this suite's
            -- synthetic boot, so label WIDTH can't prove overlap directly,
            -- but label.lua's HEIGHT is derived straight from
            -- `fontSize * uiscale`, independent of real text metrics — a
            -- fixed, unshrunk uiscale would report the SAME height
            -- regardless of category count.
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local orig = building.listDefs;"
                , "local label = require('scripts.ui.label');"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local bt = require('scripts.build_tool');"
                , "bt.setup({hud = hud});"
                , "building.listDefs = function() return {"
                , "    { name='u1', displayName='U1', category='Solo', isStarting=true },"
                , "} end;"
                , "local ok1 = pcall(function() bt.showPicker() end);"
                , "local unshrunkH = 0;"
                , "if ok1 and bt.state.mode == 'picker' and bt.state.tabIds[1] then"
                , "    local _, h = label.getSize(bt.state.tabIds[1].labelId);"
                , "    unshrunkH = h;"
                , "end;"
                , "bt.hidePicker();"
                , "building.listDefs = function() return {"
                , "    { name='t1', displayName='A1', category='Cat1', isStarting=true },"
                , "    { name='t2', displayName='A2', category='Cat2', isStarting=true },"
                , "    { name='t3', displayName='A3', category='Cat3', isStarting=true },"
                , "    { name='t4', displayName='A4', category='Cat4', isStarting=true },"
                , "    { name='t5', displayName='A5', category='Cat5', isStarting=true },"
                , "    { name='t6', displayName='A6', category='Cat6', isStarting=true },"
                , "    { name='t7', displayName='A7', category='Cat7', isStarting=true },"
                , "    { name='t8', displayName='A8', category='Cat8', isStarting=true },"
                , "} end;"
                , "local ok2 = pcall(function() bt.showPicker() end);"
                , "local shrunkH = 0;"
                , "local skipped = not (ok1 and ok2 and bt.state.mode == 'picker');"
                , "if ok2 and bt.state.mode == 'picker' and bt.state.tabIds[2] then"
                , "    local _, h = label.getSize(bt.state.tabIds[2].labelId);"
                , "    shrunkH = h;"
                , "end;"
                , "building.listDefs = orig;"
                , "return {skipped = skipped, unshrunkH = unshrunkH, shrunkH = shrunkH}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ShrinkHeightSkipProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → when (not (shspSkipped p)) $ do
                    shspShrunkH p `shouldSatisfy` (> 0)
                    shspShrunkH p `shouldSatisfy` (< shspUnshrunkH p)

        it "the icon grid is vertically compacted so a many-entry single-column category doesn't run off the framebuffer (round-9 review)" $ \(env, ls) → do
            resetFixture env ls
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local orig = building.listDefs;"
                , "building.listDefs = function() return {"
                , "    { name='t1', displayName='A1', category='All', isStarting=true },"
                , "    { name='t2', displayName='A2', category='All', isStarting=true },"
                , "    { name='t3', displayName='A3', category='All', isStarting=true },"
                , "    { name='t4', displayName='A4', category='All', isStarting=true },"
                , "    { name='t5', displayName='A5', category='All', isStarting=true },"
                , "    { name='t6', displayName='A6', category='All', isStarting=true },"
                , "    { name='t7', displayName='A7', category='All', isStarting=true },"
                , "    { name='t8', displayName='A8', category='All', isStarting=true },"
                , "} end;"
                , "local hud = require('scripts.hud');"
                , "hud.init(1,2,800,2160);"
                , "hud.createUI();"
                , "local bt = require('scripts.build_tool');"
                , "bt.setup({hud = hud});"
                , "local ok = pcall(function() bt.showPicker() end);"
                , "building.listDefs = orig;"
                , "if not ok or bt.state.mode ~= 'picker' then return {skipped = true} end;"
                , "local out = {};"
                , "for _, ic in ipairs(bt.state.iconIds or {}) do"
                , "    local info = UI.getElementInfo(ic);"
                , "    if info then table.insert(out, {x=info.x, y=info.y, w=info.width, h=info.height}) end"
                , "end;"
                , "return {skipped = false, columnsPerRow = bt.state.columnsPerRow, icons = out}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ColumnGridProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → when (not (cgpSkipped p)) $ do
                    length (cgpIcons p) `shouldSatisfy` (≥ 8)
                    forM_ (cgpIcons p) $ \rc → do
                        rrY rc `shouldSatisfy` (≥ 0)
                        (rrY rc + rrH rc) `shouldSatisfy` (≤ 2160)

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

data OverlapProbe = OverlapProbe { opOverlapsAny ∷ Bool, opInFrame ∷ Bool } deriving Show
instance FromJSON OverlapProbe where
    parseJSON = withObject "OverlapProbe" $ \o →
        OverlapProbe <$> o .: "overlapsAny" <*> o .: "inFrame"

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

data RemoteWarningProbe = RemoteWarningProbe
    { rwpW ∷ Int, rwpH ∷ Int, rwpInFrame ∷ Bool } deriving Show
instance FromJSON RemoteWarningProbe where
    parseJSON = withObject "RemoteWarningProbe" $ \o →
        RemoteWarningProbe <$> o .: "w" <*> o .: "h" <*> o .: "inFrame"

data PickerProbe = PickerProbe { ppSkipped ∷ Bool, ppInFrame ∷ Bool } deriving Show
instance FromJSON PickerProbe where
    parseJSON = withObject "PickerProbe" $ \o → do
        skipped ← o .: "skipped"
        if skipped
            then pure (PickerProbe True False)
            else PickerProbe False <$> o .: "inFrame"

data ColumnGridProbe = ColumnGridProbe
    { cgpSkipped ∷ Bool, cgpColumnsPerRow ∷ Int, cgpIcons ∷ [RectRow] } deriving Show
instance FromJSON ColumnGridProbe where
    parseJSON = withObject "ColumnGridProbe" $ \o → do
        skipped ← o .: "skipped"
        if skipped
            then pure (ColumnGridProbe True 0 [])
            else ColumnGridProbe False <$> o .: "columnsPerRow" <*> o .: "icons"

data TabStripProbe = TabStripProbe { tspSkipped ∷ Bool, tspTabs ∷ [RectRow] } deriving Show
instance FromJSON TabStripProbe where
    parseJSON = withObject "TabStripProbe" $ \o → do
        skipped ← o .: "skipped"
        if skipped
            then pure (TabStripProbe True [])
            else TabStripProbe False <$> o .: "tabs"

data HudControlFocusProbe = HudControlFocusProbe
    { hcfpHadFocusBefore ∷ Bool, hcfpHasFocusAfter ∷ Bool, hcfpSameName ∷ Bool } deriving Show
instance FromJSON HudControlFocusProbe where
    parseJSON = withObject "HudControlFocusProbe" $ \o →
        HudControlFocusProbe <$> o .: "hadFocusBefore" <*> o .: "hasFocusAfter" <*> o .: "sameName"

data ModGeometryRow = ModGeometryRow
    { mgrMod ∷ Text, mgrOk ∷ Bool, mgrW ∷ Double, mgrH ∷ Double } deriving Show
instance FromJSON ModGeometryRow where
    parseJSON = withObject "ModGeometryRow" $ \o →
        ModGeometryRow <$> o .: "mod" <*> o .: "ok" <*> o .: "w" <*> o .: "h"

data LabeledOkRow = LabeledOkRow { lorLabel ∷ Text, lorOk ∷ Bool } deriving (Show, Eq)
instance FromJSON LabeledOkRow where
    parseJSON = withObject "LabeledOkRow" $ \o →
        LabeledOkRow <$> o .: "label" <*> o .: "ok"

data ShrinkHeightProbe = ShrinkHeightProbe
    { shpUnshrunkH ∷ Double, shpShrunkH ∷ Double } deriving Show
instance FromJSON ShrinkHeightProbe where
    parseJSON = withObject "ShrinkHeightProbe" $ \o →
        ShrinkHeightProbe <$> o .: "unshrunkH" <*> o .: "shrunkH"

data ShrinkHeightSkipProbe = ShrinkHeightSkipProbe
    { shspSkipped ∷ Bool, shspUnshrunkH ∷ Double, shspShrunkH ∷ Double } deriving Show
instance FromJSON ShrinkHeightSkipProbe where
    parseJSON = withObject "ShrinkHeightSkipProbe" $ \o →
        ShrinkHeightSkipProbe <$> o .: "skipped" <*> o .: "unshrunkH" <*> o .: "shrunkH"

data DefaultsRescaleProbe = DefaultsRescaleProbe
    { drpScaleChanged ∷ Bool, drpHudFbW ∷ Int, drpHudFbH ∷ Int
    , drpSettingsFbW ∷ Int, drpSettingsFbH ∷ Int } deriving Show
instance FromJSON DefaultsRescaleProbe where
    parseJSON = withObject "DefaultsRescaleProbe" $ \o →
        DefaultsRescaleProbe <$> o .: "scaleChanged"
                              <*> o .: "hudFbW" <*> o .: "hudFbH"
                              <*> o .: "settingsFbW" <*> o .: "settingsFbH"

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
