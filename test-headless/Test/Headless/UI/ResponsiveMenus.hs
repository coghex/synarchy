{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
-- | #748 gate (Phase C, child C2 of #741): the supported responsive
--   envelope (scripts/ui/responsive.lua's band table), the shared
--   framebuffer/UI-scale notification contract it provides, and the
--   concrete state-preservation fixes it drives across the six
--   in-scope menu screens (main, pause, settings, create-world, save
--   browser, loading).
--
--   The full ui_manager boot sequence never reaches menu construction
--   headless — it gates on fontsReady, which only flips once the
--   graphics asset pipeline finishes loading a font, and that pipeline
--   never runs without a GPU (see CLAUDE.md's headless notes and
--   'Engine.Scripting.Lua.Message''s @whenGraphical@ gate on font
--   loading). So this suite boots each screen module directly on a bare
--   Lua backend with synthetic texture/font handles — the same
--   technique 'Test.Headless.UI.InputOwnership' uses for
--   scripts/debug.lua — rather than going through uiManager.init().
--   'engine.getTextWidth' always measures 0 headless this way (no font
--   atlas ever loads without a GPU), so this suite's in-frame assertions
--   are geometry-only (panel bounds vs. framebuffer size) rather than
--   relying on differential text-width behavior.
module Test.Headless.UI.ResponsiveMenus (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), decode, withObject, (.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.Types (UIPageManager(..), emptyUIPageManager)

-- | Join Lua statements/fragments with a single space — every multi-line
--   snippet in this file is built this way instead of GHC string-gap
--   continuations, which are easy to get subtly wrong (a missing space
--   before a line-continuing backslash silently glues two tokens
--   together) in a file whose whole point is long inline Lua text.
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

spec ∷ Spec
spec = around withHeadlessEngine $ do
    describe "envelope classification (scripts/ui/responsive.lua)" $ do
        it "every configured resolution is fully supported at 1x, except 3840x2160 (whose 1601-2160 band requires 1.5x-4x)" $ \env → do
            ls ← newBareLuaBackend env
            out ← evalJSON ls $ luaLines
                [ "local data = require('scripts.settings.data');"
                , "local r = require('scripts.ui.responsive');"
                , "local res = {};"
                , "for _, e in ipairs(data.resolutions) do"
                , "    local c = r.classify(e.width, e.height, 1.0);"
                , "    table.insert(res, {w=e.width, h=e.height, supported=c.supported});"
                , "end;"
                , "return res"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 out)) ∷ Maybe [ResSupport] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack out)
                Just rows → do
                    rows `shouldSatisfy` (not ∘ null)
                    forM_ rows $ \row →
                        rsSupported row `shouldBe` not (rsW row ≡ 3840 ∧ rsH row ≡ 2160)

        it "800x600 (the formal minimum) is fully supported at 1x" $ \env → do
            ls ← newBareLuaBackend env
            supported ← evalBool ls
                "return require('scripts.ui.responsive').classify(800, 600, 1.0).supported"
            supported `shouldBe` True

        it "below the formal minimum (640x480) is never fully supported, but never crashes to classify" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local c = require('scripts.ui.responsive').classify(640, 480, 1.0);"
                , "return {supported=c.supported, reason=c.reason}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe Classification of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just c → do
                    clsSupported c `shouldBe` False
                    clsReason c `shouldBe` Just "below_minimum"

        it "the issue's own out-of-envelope exemplar (800x600@4x) is not fully supported" $ \env → do
            ls ← newBareLuaBackend env
            supported ← evalBool ls
                "return require('scripts.ui.responsive').classify(800, 600, 4.0).supported"
            supported `shouldBe` False

        it "every band boundary is inclusive on both ends, and one scale unit past either end is not supported" $ \env → do
            ls ← newBareLuaBackend env
            let boundaries =
                    -- (height, minScale, maxScale)
                    [ (900,  0.5,  1.0)
                    , (901,  0.75, 2.0)
                    , (1200, 0.75, 2.0)
                    , (1201, 1.0,  3.0)
                    , (1600, 1.0,  3.0)
                    , (1601, 1.5,  4.0)
                    , (2160, 1.5,  4.0)
                    ]
            forM_ boundaries $ \(h, lo, hi) → do
                atLo ← evalBool ls (classifyCall 1920 h lo)
                atHi ← evalBool ls (classifyCall 1920 h hi)
                belowLo ← evalBool ls (classifyCall 1920 h (lo - 0.01))
                aboveHi ← evalBool ls (classifyCall 1920 h (hi + 0.01))
                atLo `shouldBe` True
                atHi `shouldBe` True
                belowLo `shouldBe` False
                aboveHi `shouldBe` False

        it "automatic 1080p/1440p/4K scales (x1.5/x2.0/x2.5, scripts/settings/data.lua's loadDefaults) all land in-band" $ \env → do
            ls ← newBareLuaBackend env
            r1080 ← evalBool ls (classifyCall 1920 1080 1.5)
            r1440 ← evalBool ls (classifyCall 2560 1440 2.0)
            r4k   ← evalBool ls (classifyCall 3840 2160 2.5)
            r1080 `shouldBe` True
            r1440 `shouldBe` True
            r4k   `shouldBe` True

        it "both configured ultrawide resolutions are supported at 1x" $ \env → do
            ls ← newBareLuaBackend env
            uw1 ← evalBool ls (classifyCall 2560 1080 1.0)
            uw2 ← evalBool ls (classifyCall 3440 1440 1.0)
            uw1 `shouldBe` True
            uw2 `shouldBe` True

    describe "shared notification contract (scripts/ui/responsive.lua)" $ do
        it "fans a resize out to every registered screen" $ \env → do
            ls ← newBareLuaBackend env
            n ← evalInt ls $ luaLines
                [ "local r = require('scripts.ui.responsive');"
                , "_G.__n = 0;"
                , "r.register('__fakeA', {onFramebufferResize=function(w,h) _G.__n = _G.__n + 1 end});"
                , "r.register('__fakeB', {onFramebufferResize=function(w,h) _G.__n = _G.__n + 1 end});"
                , "r.notifyResize(1280, 720);"
                , "return _G.__n"
                ]
            n `shouldBe` 2

        it "0x0 (minimize) is never forwarded to a registered screen" $ \env → do
            ls ← newBareLuaBackend env
            n ← evalInt ls $ luaLines
                [ "local r = require('scripts.ui.responsive');"
                , "_G.__n = 0;"
                , "r.register('__fake', {onFramebufferResize=function(w,h) _G.__n = _G.__n + 1 end});"
                , "r.notifyResize(1280, 720);"
                , "r.notifyResize(0, 0);"
                , "return _G.__n"
                ]
            n `shouldBe` 1

        it "restoring to a real size after 0x0 forwards again, and pendingRestore reflects both states" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local r = require('scripts.ui.responsive');"
                , "_G.__n = 0;"
                , "r.register('__fake', {onFramebufferResize=function(w,h) _G.__n = _G.__n + 1 end});"
                , "r.notifyResize(1280, 720);"
                , "r.notifyResize(0, 0);"
                , "local pendingWhileMinimized = r.getGeometry('__fake').pendingRestore;"
                , "r.notifyResize(1600, 900);"
                , "local pendingAfterRestore = r.getGeometry('__fake').pendingRestore;"
                , "return {calls=_G.__n, pendingWhileMinimized=pendingWhileMinimized, pendingAfterRestore=pendingAfterRestore}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RestoreProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rpCalls p `shouldBe` 2
                    rpPendingWhileMinimized p `shouldBe` True
                    rpPendingAfterRestore p `shouldBe` False

    describe "screen geometry stays in-frame across the supported envelope" $ do
        forM_ [ (800, 600, 1.0), (1280, 720, 1.0), (1920, 1080, 1.5)
              , (2560, 1440, 2.0), (3840, 2160, 2.5), (3440, 1440, 1.0)
              ] $ \(w, h, uiscale) →
            it ("main/settings/create-world panels stay within "
                ⧺ show w ⧺ "x" ⧺ show h ⧺ "@" ⧺ show uiscale) $ \env → do
                ls ← newBareLuaBackend env
                _ ← eval ls (setScaleCall uiscale)
                forM_ [ bootMain w h, bootSettings w h, bootCreateWorld w h ] $ \bootCode → do
                    inFrame ← evalBool ls (luaLines [bootCode, "return " <> panelInFrameExpr "m" w h])
                    inFrame `shouldBe` True

        -- #748 round 8: every SETTINGS-configurable resolution
        -- (scripts/settings/data.lua's data.resolutions, not just the
        -- hand-picked sample above) must produce a reachable, in-frame
        -- layout at 1x — the scale every one of them is fully
        -- supported at (per the envelope-classification describe
        -- above), so this is real layout/reachability coverage, not
        -- just the classifier's own say-so. 3840x2160 is checked too,
        -- best-effort, at 2.5x (its own auto-detected default scale,
        -- since 1x falls outside its 1.5x-4x band).
        it "every configured resolution (data.resolutions) at 1x — main/settings/create-world panels stay reachable and in-frame" $ \env → do
            ls ← newBareLuaBackend env
            resJson ← evalJSON ls
                "local res = {}; for _, e in ipairs(require('scripts.settings.data').resolutions) do table.insert(res, {w=e.width, h=e.height}) end; return res"
            case decode (BL.fromStrict (TE.encodeUtf8 resJson)) ∷ Maybe [ResDims] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack resJson)
                Just rows → do
                    rows `shouldSatisfy` (not ∘ null)
                    forM_ rows $ \row → do
                        let w = rdW row
                            h = rdH row
                            uiscale = if w ≡ 3840 ∧ h ≡ 2160 then 2.5 else 1.0
                        ls2 ← newBareLuaBackend env
                        _ ← eval ls2 (setScaleCall uiscale)
                        forM_ [ bootMain w h, bootSettings w h, bootCreateWorld w h ] $ \bootCode → do
                            inFrame ← evalBool ls2 (luaLines [bootCode, "return " <> panelInFrameExpr "m" w h])
                            inFrame `shouldBe` True

    -- pause menu and save browser build differently (no single `.init`
    -- → auto-created page), so they're checked separately rather than
    -- folded into the loop's uniform bootCode shape above.
    describe "pause menu and save browser stay in-frame" $
        forM_ [ (800, 600 ∷ Int), (1920, 1080), (3840, 2160) ] $ \(w, h) →
            it ("at " ⧺ show w ⧺ "x" ⧺ show h) $ \env → do
                ls ← newBareLuaBackend env
                pauseOk ← evalBool ls (luaLines [bootPause w h, "return " <> panelInFrameExpr "m" w h])
                pauseOk `shouldBe` True

                ls2 ← newBareLuaBackend env
                saveOk ← evalBool ls2 (luaLines [bootSaveBrowser w h, "return " <> panelInFrameExpr "m" w h])
                saveOk `shouldBe` True

    describe "loading screen stays in-frame" $ do
        forM_ [ (800, 600 ∷ Int), (1920, 1080), (3840, 2160) ] $ \(w, h) →
            it ("at " ⧺ show w ⧺ "x" ⧺ show h) $ \env → do
                ls ← newBareLuaBackend env
                ok ← evalBool ls $ luaLines
                    [ bootLoading w h
                    , "local b = require('scripts.ui.bar');"
                    , "local info = UI.getElementInfo(b.getElementHandle(m.barId));"
                    , "return info.x >= 0 and info.y >= 0"
                        <> " and (info.x + info.width) <= " <> tshow w
                        <> " and (info.y + info.height) <= " <> tshow h
                    ]
                ok `shouldBe` True

        it "stays in-frame at a narrow, high-scale supported combination (800x2160@4x — bar width alone used to exceed the framebuffer)" $ \env → do
            ls ← newBareLuaBackend env
            ok ← evalBool ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , bootLoading 800 2160
                , "local b = require('scripts.ui.bar');"
                , "local info = UI.getElementInfo(b.getElementHandle(m.barId));"
                , "return info.x >= 0 and info.y >= 0"
                    <> " and (info.x + info.width) <= 800"
                    <> " and (info.y + info.height) <= 2160"
                ]
            ok `shouldBe` True

        it "re-shows itself after a resize while genuinely visible (its own createUI() always starts a fresh page hidden)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootLoading 1280 720
                , "local visibleBefore = UI.isPageVisible(m.page);"
                , "m.onFramebufferResize(1600, 900);"
                , "local visibleAfter = UI.isPageVisible(m.page);"
                , "return {visibleBefore=visibleBefore, visibleAfter=visibleAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe VisibilityProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    vpVisibleBefore p `shouldBe` True
                    vpVisibleAfter p `shouldBe` True

    describe "0x0 minimize never builds invalid UI, and restore rebuilds in-frame" $
        it "settings menu keeps its last valid geometry through 0x0, then rebuilds cleanly on restore" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local r = require('scripts.ui.responsive');"
                , "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "r.register('settingsMenu', m);"
                , "local p = require('scripts.ui.panel');"
                , "local x0,y0 = p.getPosition(m.panelId);"
                , "local w0,h0 = p.getSize(m.panelId);"
                , "r.notifyResize(0, 0);"
                , "local x1,y1 = p.getPosition(m.panelId);"
                , "local w1,h1 = p.getSize(m.panelId);"
                , "local unchanged = (x0==x1 and y0==y1 and w0==w1 and h0==h1);"
                , "r.notifyResize(1600, 900);"
                , "local x2,y2 = p.getPosition(m.panelId);"
                , "local w2,h2 = p.getSize(m.panelId);"
                , "local restored = (x2>=0 and y2>=0 and (x2+w2)<=1600 and (y2+h2)<=900);"
                , "return {unchanged=unchanged, restored=restored}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe MinimizeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    mpUnchanged p `shouldBe` True
                    mpRestored p `shouldBe` True

    describe "state preservation across a mere resize rebuild" $ do
        it "settings menu never discards an unapplied (submitted) frame-limit edit on a resize, in the pending table AND the rebuilt widget's own displayed value" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "local gt = require('scripts.settings.graphics_tab');"
                , "local textbox = require('scripts.ui.textbox');"
                , "gt.onTextBoxSubmit('framelimit_input', '111');"
                , "local pendingBefore = require('scripts.settings.data').pending.frameLimit;"
                , "local widgetBefore = textbox.getNumericValue(gt.frameLimitTextBoxId);"
                , "m.onFramebufferResize(1600, 900);"
                , "local pendingAfter = require('scripts.settings.data').pending.frameLimit;"
                , "local widgetAfter = textbox.getNumericValue(gt.frameLimitTextBoxId);"
                , "return {pendingBefore=pendingBefore, widgetBefore=widgetBefore,"
                    <> " pendingAfter=pendingAfter, widgetAfter=widgetAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe FrameLimitProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    flPendingBefore p `shouldBe` 111
                    flWidgetBefore p `shouldBe` 111
                    flPendingAfter p `shouldBe` 111
                    flWidgetAfter p `shouldBe` 111

        it "settings menu's active tab and clamped scroll offset survive a resize" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,620);"
                , "local ts = m.tabScroll['graphics'];"
                , "local canScroll = ts.totalRows > ts.maxVisibleRows;"
                , "if canScroll then m.onTabScroll('graphics', 1) end;"
                , "local before = ts.scrollOffset;"
                , "m.onFramebufferResize(1280, 600);"
                , "local afterTs = m.tabScroll['graphics'];"
                , "return {canScroll=canScroll, activeTabOk=(m.activeTab=='graphics'),"
                    <> " scrollOk=(not canScroll or afterTs.scrollOffset == before)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ScrollProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    -- Asserted (not just OR'd into scrollOk) so a future
                    -- change shrinking the graphics tab's row count can't
                    -- silently turn this into a vacuous pass.
                    spCanScroll p `shouldBe` True
                    spActiveTabOk p `shouldBe` True
                    spScrollOk p `shouldBe` True

        it "create-world menu's log scroll offset survives a resize (clamped, not reset to 0)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.create_world_menu');"
                , "m.init(1,2,3,1280,720);"
                , "for i=1,60 do m.onWorldGenLog('line ' .. i) end;"
                , "m.onLogScroll(3);"
                , "local before = m.logScrollOffset;"
                , "m.onFramebufferResize(1280, 700);"
                , "return {before=before, after=m.logScrollOffset}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe BeforeAfter of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    baBefore p `shouldBe` 3
                    baAfter p `shouldBe` 3

        it "save browser's selection survives a resize without re-firing onSelect" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.save_browser');"
                , "local list = require('scripts.ui.list');"
                , "_G.__selectCount = 0;"
                , "local saves = {{name='alpha',timestamp='t1'},{name='beta',timestamp='t2'},{name='gamma',timestamp='t3'}};"
                , "m.init(1,2,3,1280,720);"
                , "m.show(saves, function(v) _G.__selectCount = _G.__selectCount + 1 end, function() end);"
                , "list.selectItem(m.listId, 2);"
                , "m.onFramebufferResize(1400, 800);"
                , "return {value=list.getSelectedValue(m.listId), count=_G.__selectCount}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe SelectProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    selValue p `shouldBe` "beta"
                    selCount p `shouldBe` 1

        it "settings menu preserves an in-progress (unsubmitted) textbox edit, its cursor, and its keyboard focus across a resize" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "local gt = require('scripts.settings.graphics_tab');"
                , "local textbox = require('scripts.ui.textbox');"
                -- setText mirrors what live keystrokes do to the raw
                -- text input BEFORE Enter/blur ever fires the real
                -- onTextBoxSubmit — this value never reaches `pending`.
                , "textbox.setText(gt.frameLimitTextBoxId, '9');"
                , "textbox.focus(gt.frameLimitTextBoxId);"
                , "textbox.setCursor(gt.frameLimitTextBoxId, 1);"
                , "m.onFramebufferResize(1600, 900);"
                , "local newId = gt.frameLimitTextBoxId;"
                , "return {text=textbox.getText(newId), cursor=textbox.getCursor(newId),"
                    <> " focused=textbox.isFocused(newId)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe TextboxStateProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    tspText p `shouldBe` "9"
                    tspCursor p `shouldBe` 1
                    tspFocused p `shouldBe` True

        it "create-world menu preserves an in-progress (unsubmitted) textbox edit across a resize, even though it never syncs to `pending` until Generate" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootCreateWorld 1280 720 <> ";"
                , "local at = require('scripts.create_world.advanced_tab');"
                , "local textbox = require('scripts.ui.textbox');"
                , "textbox.setText(at.plateCountTextBoxId, '7');"
                , "m.onFramebufferResize(1600, 900);"
                , "local newId = at.plateCountTextBoxId;"
                , "return {text=textbox.getText(newId), pendingUnchanged=(m.pending.plateCount == '10')}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe PlateCountProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    pcText p `shouldBe` "7"
                    -- The in-progress edit survives WITHOUT prematurely
                    -- writing to `pending` (which only Generate does) —
                    -- proving the fix restores the WIDGET, not by
                    -- routing through pending like graphics_tab's
                    -- submitted-value case.
                    pcPendingUnchanged p `shouldBe` True

    describe "repeated resize never grows live UI state" $
        it "5 alternating-size resizes on the settings menu leave a stable element/page count" $ \env → do
            ls ← newBareLuaBackend env
            writeIORef (uiManagerRef env) emptyUIPageManager
            _ ← eval ls $ luaLines
                [ "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "m.onFramebufferResize(1300, 730);"
                , "m.onFramebufferResize(1280, 720)"
                ]
            (elems2, pages2) ← countUI env
            _ ← eval ls $ luaLines
                [ "local m = require('scripts.settings_menu');"
                , "m.onFramebufferResize(1400, 800);"
                , "m.onFramebufferResize(1280, 720);"
                , "m.onFramebufferResize(1600, 900);"
                , "m.onFramebufferResize(1280, 720)"
                ]
            (elems3, pages3) ← countUI env
            elems3 `shouldBe` elems2
            pages3 `shouldBe` pages2

    describe "outside-envelope combinations remain allowed best-effort" $
        it "800x600@4x (the issue's own exemplar) still builds a valid settings screen with Back/Apply/Save reachable" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,800,600);"
                , "local p = require('scripts.ui.panel');"
                , "local w,h = p.getSize(m.panelId);"
                , "return {hasBack=(m.backButtonId ~= nil), hasApply=(m.applyButtonId ~= nil),"
                    <> " hasSave=(m.saveButtonId ~= nil), validDims=(w > 0 and h > 0)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe OutsideEnvelopeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    oepHasBack p `shouldBe` True
                    oepHasApply p `shouldBe` True
                    oepHasSave p `shouldBe` True
                    oepValidDims p `shouldBe` True

    describe "fixed action bars avoid overlap and stay in-frame at the formal minimum (800x600@1x)" $ do
        it "create-world's bottom button bar never overlaps in the idle or done set" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.create_world_menu');"
                , "m.init(1,2,3,800,600);"
                , "local button = require('scripts.ui.button');"
                , "local function box(id)"
                , "    local info = UI.getElementInfo(button.getElementHandle(id));"
                , "    return info.x, info.y, info.width, info.height"
                , "end;"
                , "local function overlap(x1,w1,x2,w2) return x1 < x2+w2 and x2 < x1+w1 end;"
                , "local bx,by,bw,bh = box(m.backButtonId);"
                , "local dx,dy,dw,dh = box(m.defaultsButtonId);"
                , "local gx,gy,gw,gh = box(m.generateButtonId);"
                , "local idleOverlap = overlap(bx,bw,dx,dw) or overlap(bx,bw,gx,gw) or overlap(dx,dw,gx,gw);"
                , "local idleInFrame = bx >= 0 and dx >= 0 and gx >= 0"
                    <> " and (bx+bw) <= 800 and (dx+dw) <= 800 and (gx+gw) <= 800;"
                , "m.buildButtonsDone();"
                , "local bx2,by2,bw2,bh2 = box(m.backButtonId);"
                , "local dx2,dy2,dw2,dh2 = box(m.defaultsButtonId);"
                , "local rx2,ry2,rw2,rh2 = box(m.regenerateButtonId);"
                , "local cx2,cy2,cw2,ch2 = box(m.continueButtonId);"
                , "local doneOverlap = overlap(bx2,bw2,dx2,dw2) or overlap(bx2,bw2,rx2,rw2)"
                    <> " or overlap(bx2,bw2,cx2,cw2) or overlap(dx2,dw2,rx2,rw2)"
                    <> " or overlap(dx2,dw2,cx2,cw2) or overlap(rx2,rw2,cx2,cw2);"
                , "local doneInFrame = bx2 >= 0 and dx2 >= 0 and rx2 >= 0 and cx2 >= 0"
                    <> " and (bx2+bw2) <= 800 and (dx2+dw2) <= 800"
                    <> " and (rx2+rw2) <= 800 and (cx2+cw2) <= 800;"
                , "return {idleOverlap=idleOverlap, idleInFrame=idleInFrame,"
                    <> " doneOverlap=doneOverlap, doneInFrame=doneInFrame}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ButtonBarProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    bbIdleOverlap p `shouldBe` False
                    bbIdleInFrame p `shouldBe` True
                    bbDoneOverlap p `shouldBe` False
                    bbDoneInFrame p `shouldBe` True

        it "save browser's Back button stays reachable within the framebuffer with a long (12-entry) save list" $ \env → do
            ls ← newBareLuaBackend env
            let saveList = T.intercalate ","
                    [ "{name='s" <> tshow (i ∷ Int) <> "',timestamp='t'}" | i ← [1 .. 12] ]
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.save_browser');"
                , "m.init(1,2,3,800,600);"
                , "m.show({" <> saveList <> "}, function() end, function() end);"
                , "local function findByName(name)"
                , "    for _, e in ipairs(UI.getVisibleElements()) do"
                , "        if e.name == name then return e.handle end"
                , "    end"
                , "    return nil"
                , "end;"
                , "local h = findByName('save_browser_back_box');"
                , "local info = UI.getElementInfo(h);"
                , "return {y = info.y, bottom = info.y + info.height}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe BackButtonProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    bbpY p `shouldSatisfy` (>= 0)
                    bbpBottom p `shouldSatisfy` (<= 600)

        it "save browser's Back button stays reachable even at 800x600@4x (outside-envelope best-effort, where fixed chrome alone used to exceed the size cap)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local m = require('scripts.save_browser');"
                , "m.init(1,2,3,800,600);"
                , "m.show({{name='only',timestamp='t'}}, function() end, function() end);"
                , "local function findByName(name)"
                , "    for _, e in ipairs(UI.getVisibleElements()) do"
                , "        if e.name == name then return e.handle end"
                , "    end"
                , "    return nil"
                , "end;"
                , "local h = findByName('save_browser_back_box');"
                , "local info = UI.getElementInfo(h);"
                , "return {y = info.y, bottom = info.y + info.height}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe BackButtonProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    bbpY p `shouldSatisfy` (>= 0)
                    bbpBottom p `shouldSatisfy` (<= 600)

        it "settings menu's four bottom-action buttons (Back/Defaults/Apply/Save) never overlap and stay in-frame" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,800,600);"
                , "local button = require('scripts.ui.button');"
                , "local function box(id)"
                , "    local info = UI.getElementInfo(button.getElementHandle(id));"
                , "    return info.x, info.y, info.width, info.height"
                , "end;"
                , "local function overlap(x1,w1,x2,w2) return x1 < x2+w2 and x2 < x1+w1 end;"
                , "local bx,by,bw,bh = box(m.backButtonId);"
                , "local dx,dy,dw,dh = box(m.defaultsButtonId);"
                , "local ax,ay,aw,ah = box(m.applyButtonId);"
                , "local sx,sy,sw,sh = box(m.saveButtonId);"
                , "local anyOverlap = overlap(bx,bw,dx,dw) or overlap(bx,bw,ax,aw) or overlap(bx,bw,sx,sw)"
                    <> " or overlap(dx,dw,ax,aw) or overlap(dx,dw,sx,sw) or overlap(ax,aw,sx,sw);"
                , "local allInFrame = bx >= 0 and dx >= 0 and ax >= 0 and sx >= 0"
                    <> " and (bx+bw) <= 800 and (dx+dw) <= 800 and (ax+aw) <= 800 and (sx+sw) <= 800;"
                , "return {anyOverlap=anyOverlap, allInFrame=allInFrame}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe OverlapFrameProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    ofpAnyOverlap p `shouldBe` False
                    ofpAllInFrame p `shouldBe` True

        -- #748 round 6: shrinking only the button BOX left the label
        -- rendering at the unshrunk base font size — the label's own
        -- centering math (labelX = (btnWidth - labelWidth) / 2) goes
        -- negative once labelWidth exceeds the shrunk box, meaning the
        -- text starts to the LEFT of the box's own left edge. Stub
        -- engine.getTextWidth to realistic (nonzero) per-character
        -- metrics so this actually exercises that overflow.
        it "settings menu's bottom-action button labels stay within their own (shrunk) box at 800x2160@4x" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "engine.getTextWidth = function(font, text, size) return #text * size * 0.6 end;"
                , "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,800,2160);"
                , "UI.showPage(m.page);"
                , "local button = require('scripts.ui.button');"
                , "local boxInfo = UI.getElementInfo(button.getElementHandle(m.backButtonId));"
                , "local labelX = nil;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "  if e.name == 'back_btn_label' then labelX = e.x end"
                , "end;"
                , "return {boxX = boxInfo.x, labelX = labelX}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ButtonLabelFitProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → blfpLabelX p `shouldSatisfy` (>= blfpBoxX p)

        it "create-world's World Name control stays in-frame at the formal minimum, not off-screen to the left" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootCreateWorld 800 600 <> ";"
                , "local st = require('scripts.create_world.settings_tab');"
                , "local randbox = require('scripts.ui.randbox');"
                , "local info = UI.getElementInfo(randbox.getElementHandle(st.nameRandBoxId));"
                , "return {x = info.x, rightEdge = info.x + info.width}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WorldNameProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    wnpX p `shouldSatisfy` (>= 0)
                    wnpRightEdge p `shouldSatisfy` (<= 800)

    describe "main/pause menu compact fallback keeps the panel + title in-frame at the maximum supported scale" $ do
        forM_ [ ("main", "scripts.main_menu"), ("pause", "scripts.pause_menu") ] $ \(menuName, modulePath) →
            it (menuName ⧺ " menu at 3840x2160@4 with its maximum item count") $ \env → do
                ls ← newBareLuaBackend env
                r ← evalJSON ls $ luaLines
                    -- main_menu.buildMenuItems() overwrites mainMenu.saves
                    -- from engine.listSaves() itself, so a fake save list
                    -- has to go through that, not a direct field poke —
                    -- two fake saves gives main_menu its maximum 5 items
                    -- (Continue+Load Game+Create World+Settings+Quit).
                    ([ "engine.setUIScale(4.0);"
                     , "engine.listSaves = function() return {{name='a',timestamp='t'},{name='b',timestamp='t'}} end;"
                     , "local m = require('" <> modulePath <> "');"
                     , "m.init(1,2,3,4,3840,2160);"
                     ]
                     ⧺ (if modulePath ≡ "scripts.pause_menu"
                           then [ "m.show({showSave=true});" ]  -- pause_menu's own max (4 items)
                           else [])
                     ⧺
                    [ "local p = require('scripts.ui.panel');"
                    , "local px, py = p.getPosition(m.panelId);"
                    , "local pw, ph = p.getSize(m.panelId);"
                    , "local panelInFrame = px >= 0 and py >= 0"
                        <> " and (px+pw) <= 3840 and (py+ph) <= 2160;"
                    , "local titleInfo = UI.getElementInfo("
                        <> "require('scripts.ui.label').getElementHandle(m.titleLabelId));"
                    , "return {panelInFrame = panelInFrame, titleY = titleInfo.y}"
                    ])
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe CompactFallbackProbe of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just p → do
                        cfPanelInFrame p `shouldBe` True
                        cfTitleY p `shouldSatisfy` (>= 0)

        forM_ [ ("main", "scripts.main_menu"), ("pause", "scripts.pause_menu") ] $ \(menuName, modulePath) →
            it (menuName ⧺ " menu at 800x2160@4 (narrow width, not just short height — fixed button/menu padding alone used to overflow horizontally)") $ \env → do
                ls ← newBareLuaBackend env
                r ← evalJSON ls $ luaLines
                    ([ "engine.setUIScale(4.0);"
                     , "local m = require('" <> modulePath <> "');"
                     , "m.init(1,2,3,4,800,2160);"
                     ]
                     ⧺ (if modulePath ≡ "scripts.pause_menu"
                           then [ "m.show({showSave=false});" ]
                           else [])
                     ⧺
                    [ "local p = require('scripts.ui.panel');"
                    , "local px, py = p.getPosition(m.panelId);"
                    , "local pw, ph = p.getSize(m.panelId);"
                    , "local panelInFrame = px >= 0 and py >= 0"
                        <> " and (px+pw) <= 800 and (py+ph) <= 2160;"
                    , "local titleInfo = UI.getElementInfo("
                        <> "require('scripts.ui.label').getElementHandle(m.titleLabelId));"
                    , "return {panelInFrame = panelInFrame, titleY = titleInfo.y}"
                    ])
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe CompactFallbackProbe of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just p → do
                        cfPanelInFrame p `shouldBe` True
                        cfTitleY p `shouldSatisfy` (>= 0)

    describe "save browser stays in-frame at a narrow, high-scale supported combination" $
        it "800x2160@4x (panel width is a fixed 0.6 fraction of the framebuffer that doesn't scale with uiscale, while its side padding does — bounds.width used to go to zero)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "local m = require('scripts.save_browser');"
                , "m.init(1,2,3,800,2160);"
                , "m.show({{name='only',timestamp='t'}}, function() end, function() end);"
                , "local p = require('scripts.ui.panel');"
                , "local px, py = p.getPosition(m.panelId);"
                , "local pw, ph = p.getSize(m.panelId);"
                , "return {panelInFrame = (px >= 0 and py >= 0"
                    <> " and (px+pw) <= 800 and (py+ph) <= 2160), validWidth = (pw > 0)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe SaveBrowserExtremeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    sbepPanelInFrame p `shouldBe` True
                    sbepValidWidth p `shouldBe` True

    describe "settings menu's tab content stays in-frame at a narrow, high-scale supported combination" $ do
        it "800x2160@4x (the frame-limit textbox's unshrunk base width used to be positioned off the left edge)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , bootSettings 800 2160
                , "local gt = require('scripts.settings.graphics_tab');"
                , "local textbox = require('scripts.ui.textbox');"
                , "local info = UI.getElementInfo(textbox.getElementHandle(gt.frameLimitTextBoxId));"
                , "return {x = info.x, rightEdge = info.x + info.width}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WorldNameProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    wnpX p `shouldSatisfy` (>= 0)
                    wnpRightEdge p `shouldSatisfy` (<= 800)

        -- #748 round 5: a dropdown's width is driven by its OPTION TEXT
        -- metrics (dropdown.measureOptions), not a plain baseSizes field
        -- — stub engine.getTextWidth to realistic (nonzero) per-
        -- character metrics so this actually exercises the text-driven
        -- half of the fit (the floor-driven half alone was already
        -- enough to overflow, but a real font's measured widths must
        -- fit too).
        it "800x2160@4x keeps the Resolution dropdown (+ its arrow) in-frame under realistic, nonzero text metrics" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "engine.getTextWidth = function(font, text, size) return #text * size * 0.6 end;"
                , bootSettings 800 2160
                , "local gt = require('scripts.settings.graphics_tab');"
                , "local dropdown = require('scripts.ui.dropdown');"
                , "local dispInfo = UI.getElementInfo(dropdown.getElementHandle(gt.resolutionDropdownId));"
                , "local arrowInfo = UI.getElementInfo(dropdown.getArrowHandle(gt.resolutionDropdownId));"
                , "return {x = dispInfo.x, rightEdge = arrowInfo.x + arrowInfo.width}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WorldNameProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    wnpX p `shouldSatisfy` (>= 0)
                    wnpRightEdge p `shouldSatisfy` (<= 800)

        -- #748 round 5: the tab bar's own FRAME is sized to bounds.width,
        -- but each tab's clickable box is laid out at a width driven by
        -- its OWN label text + padding, left-to-right with no fit/clip
        -- of its own — stub engine.getTextWidth so this exercises real
        -- (nonzero) label metrics rather than the headless-default zero.
        it "800x2160@4x keeps every tab bar button in-frame under realistic, nonzero text metrics" $ \env → do
            ls ← newBareLuaBackend env
            inFrame ← evalBool ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "engine.getTextWidth = function(font, text, size) return #text * size * 0.6 end;"
                , bootSettings 800 2160
                , "UI.showPage(m.page);"
                , "local allInFrame = true;"
                , "local sawAny = false;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "  if e.name and e.name:match('^settings_tabs_tab_%d+$') then"
                , "    sawAny = true;"
                , "    if e.x < 0 or (e.x + e.width) > 800 then allInFrame = false end"
                , "  end"
                , "end;"
                , "return allInFrame and sawAny"
                ]
            inFrame `shouldBe` True

    describe "create-world's compact fallback keeps tab content in-frame at a narrow, high-scale supported combination" $ do
        it "800x2160@4x (fixed paddings alone used to drive contentW negative)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , bootCreateWorld 800 2160
                , "local st = require('scripts.create_world.settings_tab');"
                , "local randbox = require('scripts.ui.randbox');"
                , "local p = require('scripts.ui.panel');"
                , "local px, py = p.getPosition(m.panelId);"
                , "local pw, ph = p.getSize(m.panelId);"
                , "local panelInFrame = px >= 0 and py >= 0"
                    <> " and (px+pw) <= 800 and (py+ph) <= 2160;"
                , "local info = UI.getElementInfo(randbox.getElementHandle(st.nameRandBoxId));"
                , "return {panelInFrame = panelInFrame, nameX = info.x,"
                    <> " nameRightEdge = info.x + info.width}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe CreateWorldExtremeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    cwepPanelInFrame p `shouldBe` True
                    cwepNameX p `shouldSatisfy` (>= 0)
                    cwepNameRightEdge p `shouldSatisfy` (<= 800)

        it "800x2160@4x keeps the tab BAR itself in-frame (tab label text width alone used to overflow the panel independent of the content-width fix)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , bootCreateWorld 800 2160
                , "local tabbar = require('scripts.ui.tabbar');"
                , "local tbx, tby, tbw, tbh = tabbar.getFrameBounds(m.tabBarId);"
                , "local p = require('scripts.ui.panel');"
                , "local px, py = p.getPosition(m.panelId);"
                , "local pw, ph = p.getSize(m.panelId);"
                , "return {panelInFrame = (tbx >= 0 and (tbx+tbw) <= 800 and tbw > 0),"
                    <> " nameX = px, nameRightEdge = px + pw}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe CreateWorldExtremeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    cwepPanelInFrame p `shouldBe` True
                    cwepNameRightEdge p `shouldSatisfy` (<= 800)

    describe "create-world's tab content scrolls when it overflows the tab frame (#748 round 5)" $ do
        it "the General tab (5 rows) needs a scrollbar at the formal 800x600@1x minimum" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootCreateWorld 800 600
                , "local ts = m.tabScroll['settings'];"
                , "return {totalRows = ts.totalRows, maxVisibleRows = ts.maxVisibleRows,"
                    <> " hasScrollbar = (ts.scrollbarId ~= nil)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe TabScrollProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    tscpTotalRows p `shouldSatisfy` (> tscpMaxVisibleRows p)
                    tscpHasScrollbar p `shouldBe` True

        it "a row past the visible frame is clipped out, then reachable after scrolling" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootCreateWorld 800 600
                , "UI.showPage(m.page);"
                , "local gt = require('scripts.create_world.general_tab');"
                , "local textbox = require('scripts.ui.textbox');"
                , "local handle = textbox.getElementHandle(gt.daysPerMonthId);"
                , "local before = UI.getElementInfo(handle);"
                , "local clippedBefore = before.y < before.effectiveClip.y"
                    <> " or before.y >= (before.effectiveClip.y + before.effectiveClip.h);"
                , "m.onTabScroll('settings', m.tabScroll['settings'].totalRows"
                    <> " - m.tabScroll['settings'].maxVisibleRows);"
                , "local after = UI.getElementInfo(handle);"
                , "local visibleAfter = after.y >= after.effectiveClip.y"
                    <> " and after.y < (after.effectiveClip.y + after.effectiveClip.h);"
                , "return {clippedBefore = clippedBefore, visibleAfter = visibleAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ScrollRevealProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    srpClippedBefore p `shouldBe` True
                    srpVisibleAfter p `shouldBe` True

        it "switching tabs and back preserves each tab's own scroll offset" $ \env → do
            ls ← newBareLuaBackend env
            offsetsMatch ← evalBool ls $ luaLines
                [ bootCreateWorld 800 600
                , "m.onTabScroll('settings', 2);"
                , "m.showTab('advanced');"
                , "m.showTab('settings');"
                , "return m.tabScroll['settings'].scrollOffset == 2"
                ]
            offsetsMatch `shouldBe` True

    describe "settings menu's input (keybind) tab stays in-frame at a narrow, high-scale supported combination (#748 round 6)" $
        it "800x2160@4x (fixed key/plus button widths alone used to exceed the content column before any key count was considered)" $ \env → do
            ls ← newBareLuaBackend env
            inFrame ← evalBool ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , bootSettings 800 2160
                , "m.showTab('input');"
                , "local it = require('scripts.settings.input_tab');"
                , "local button = require('scripts.ui.button');"
                , "local allInFrame = true;"
                , "local sawAny = false;"
                , "for _, bid in ipairs(it.widgets.buttonIds) do"
                , "  sawAny = true;"
                , "  local info = UI.getElementInfo(button.getElementHandle(bid));"
                , "  if info.x < 0 or (info.x + info.width) > 800 then allInFrame = false end"
                , "end;"
                , "return allInFrame and sawAny"
                ]
            inFrame `shouldBe` True

    describe "settings menu's notifications tab stays in-frame at a narrow, high-scale supported combination (#748 round 6)" $ do
        it "800x2160@4x under realistic, nonzero text metrics: checkboxes stay nonzero-sized and in-frame (the 3-column grid's own floored geometry alone used to exceed the content width)" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "engine.getTextWidth = function(font, text, size) return #text * size * 0.6 end;"
                , bootSettings 800 2160
                , "m.showTab('notifications');"
                , "local nt = require('scripts.settings.notifications_tab');"
                , "local checkbox = require('scripts.ui.checkbox');"
                , "local firstCat = nil;"
                , "for k, _ in pairs(nt.checkboxes) do firstCat = k break end;"
                , "local cw, ch = checkbox.getSize(nt.checkboxes[firstCat].pause);"
                , "local info = UI.getElementInfo(checkbox.getElementHandle(nt.checkboxes[firstCat].pause));"
                , "return {checkboxSize = cw, x = info.x, rightEdge = info.x + info.width}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe NotifCheckboxProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    ncpCheckboxSize p `shouldSatisfy` (> 0)
                    ncpX p `shouldSatisfy` (>= 0)
                    ncpRightEdge p `shouldSatisfy` (<= 800)

        it "800x2160@4x: the header measurement fix keeps 'Event Log'/'Pause' headers from under-measuring at uiscale != 1" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "engine.getTextWidth = function(font, text, size) return #text * size * 0.6 end;"
                , bootSettings 800 2160
                , "UI.showPage(m.page);"
                , "m.showTab('notifications');"
                , "local logX, pauseX = nil, nil;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "  if e.name == 'notif_header_log_text' then logX = e.x end;"
                , "  if e.name == 'notif_header_pause_text' then pauseX = e.x end;"
                , "end;"
                , "return {x = logX, rightEdge = pauseX}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WorldNameProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    wnpX p `shouldSatisfy` (>= 0)
                    wnpRightEdge p `shouldSatisfy` (<= 800)

    describe "create-world menu preserves an in-progress randbox (World Name/Seed) edit across a resize (#748 round 6)" $
        it "text, cursor, and focus all survive a rebuild" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootCreateWorld 1280 720 <> ";"
                , "local st = require('scripts.create_world.settings_tab');"
                , "local randbox = require('scripts.ui.randbox');"
                , "randbox.setValue(st.nameRandBoxId, 'MyWorld');"
                , "randbox.focus(st.nameRandBoxId);"
                , "randbox.setCursor(st.nameRandBoxId, 3);"
                , "m.onFramebufferResize(1600, 900);"
                , "local newId = st.nameRandBoxId;"
                , "return {text = randbox.getValue(newId), cursor = randbox.getCursor(newId),"
                    <> " focused = randbox.isFocused(newId)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe TextboxStateProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    tspText p `shouldBe` "MyWorld"
                    tspCursor p `shouldBe` 3
                    tspFocused p `shouldBe` True

    describe "shell debug console adopts the shared resize/scale contract (#748 round 6)" $ do
        it "a UI-scale Apply (no framebuffer size change) updates an already-visible shell immediately, not just on next show()" $ \env → do
            ls ← newBareLuaBackend env
            noFurtherChangeNeeded ← evalBool ls $ luaLines
                [ "local shell = require('scripts.shell');"
                , "shell.init(0);"
                , "shell.show();"
                -- #748 round 7: shell is deliberately NOT registered
                -- through responsive.register/notifyResize (the engine
                -- already broadcasts a REAL framebuffer resize straight
                -- to shell.lua directly — routing it through the
                -- shared fan-out too would rebuild an already-open
                -- shell TWICE per real resize). settingsMenu.onApply/
                -- onSave call shell.onFramebufferResize directly
                -- instead, exactly the case exercised here. (Scale
                -- starts at whatever m.init() below captures as
                -- data.current.uiScale — NOT pre-set here, or Apply
                -- would see no change at all.)
                , "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "local graphicsTab = require('scripts.settings.graphics_tab');"
                , "local textbox = require('scripts.ui.textbox');"
                , "local data = require('scripts.settings.data');"
                , "local target = (data.current.uiScale >= 3.0) and 1.0 or (data.current.uiScale + 1.0);"
                , "textbox.setText(graphicsTab.uiScaleTextBoxId, tostring(target));"
                , "m.onApply();"
                -- If onApply's direct shell.onFramebufferResize call
                -- already ran rescale() internally, this second, direct
                -- rescale() call finds nothing left to change (newScale
                -- == cached uiscale) and returns false. Before the fix,
                -- the cache would still read 1.0 here, so this would
                -- return true.
                , "local changedAgain = shell.rescale();"
                , "return not changedAgain"
                ]
            noFurtherChangeNeeded `shouldBe` True

        it "settingsMenu.onApply calls shell's resize handler exactly once (not double-routed through the shared fan-out)" $ \env → do
            ls ← newBareLuaBackend env
            calls ← evalInt ls $ luaLines
                [ "local shell = require('scripts.shell');"
                , "local calls = 0;"
                , "local realHandler = shell.onFramebufferResize;"
                , "shell.onFramebufferResize = function(w, h) calls = calls + 1; return realHandler(w, h) end;"
                , "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "local graphicsTab = require('scripts.settings.graphics_tab');"
                , "local textbox = require('scripts.ui.textbox');"
                , "local data = require('scripts.settings.data');"
                , "local target = (data.current.uiScale >= 3.0) and 1.0 or (data.current.uiScale + 1.0);"
                , "textbox.setText(graphicsTab.uiScaleTextBoxId, tostring(target));"
                , "m.onApply();"
                , "return calls"
                ]
            calls `shouldBe` 1

    describe "row labels never overlap their same-row control at a narrow, high-scale supported combination (#748 round 7)" $ do
        it "graphics_tab.lua's Resolution row: label ends before the (also-reserved) dropdown begins at 800x2160@4x" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "engine.getTextWidth = function(font, text, size) return #text * size * 0.6 end;"
                , bootSettings 800 2160
                , "UI.showPage(m.page);"
                , "local gt = require('scripts.settings.graphics_tab');"
                , "local dropdown = require('scripts.ui.dropdown');"
                , "local dropdownInfo = UI.getElementInfo(dropdown.getElementHandle(gt.resolutionDropdownId));"
                , "local labelX = nil;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "  if e.name == 'resolution_label_text' then labelX = e.x end;"
                , "end;"
                , "return {x = labelX, rightEdge = dropdownInfo.x}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WorldNameProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → wnpRightEdge p `shouldSatisfy` (>= wnpX p + 10)

        it "create-world's World Name row: label ends before the (also-reserved) randbox begins at the formal 800x600@1x minimum" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.getTextWidth = function(font, text, size) return #text * size * 0.6 end;"
                , bootCreateWorld 800 600 <> ";"
                , "UI.showPage(m.page);"
                , "local st = require('scripts.create_world.settings_tab');"
                , "local randbox = require('scripts.ui.randbox');"
                , "local randboxInfo = UI.getElementInfo(randbox.getElementHandle(st.nameRandBoxId));"
                , "local labelX = nil;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "  if e.name == 'world_name_label_text' then labelX = e.x end;"
                , "end;"
                , "return {x = labelX, rightEdge = randboxInfo.x}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WorldNameProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → wnpRightEdge p `shouldSatisfy` (>= wnpX p + 10)

        -- #748 round 9: round 7's sweep covered graphics_tab.lua and
        -- create-world's tabs but missed the Input and Notifications
        -- tabs — their labels ALSO reserved a column (input_tab.lua's
        -- labelColW, notifications_tab.lua's per-header measurement)
        -- but still rendered at the tab's full uiscale.
        it "input_tab.lua's action row: label ends before the (also-fitted) key/plus buttons begin at 800x2160@4x" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "engine.getTextWidth = function(font, text, size) return #text * size * 0.6 end;"
                , bootSettings 800 2160
                , "UI.showPage(m.page);"
                , "m.showTab('input');"
                , "local it = require('scripts.settings.input_tab');"
                , "local button = require('scripts.ui.button');"
                , "local firstBtnInfo = UI.getElementInfo(button.getElementHandle(it.widgets.buttonIds[1]));"
                , "local labelX = nil;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "  if e.name and e.name:match('^keybind_lbl_.*_text$') then labelX = e.x end;"
                , "end;"
                , "return {x = labelX, rightEdge = firstBtnInfo.x}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WorldNameProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → wnpRightEdge p `shouldSatisfy` (>= wnpX p + 5)

        it "notifications_tab.lua's Category column: row labels end before the Log column's checkboxes begin at 800x2160@4x" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "engine.getTextWidth = function(font, text, size) return #text * size * 0.6 end;"
                , bootSettings 800 2160
                , "UI.showPage(m.page);"
                , "m.showTab('notifications');"
                , "local labelX, logX = nil, nil;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "  if e.name and e.name:match('^notif_row_.*_text$') then labelX = e.x end;"
                , "  if e.name and e.name:match('_log_sprite$') then logX = e.x end;"
                , "end;"
                , "return {x = labelX, rightEdge = logX}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe WorldNameProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → wnpRightEdge p `shouldSatisfy` (>= wnpX p + 5)

        it "notifications_tab.lua's checkboxes stay nonzero-sized even when header text dominates the grid's fit at 800x2160@4x" $ \env → do
            ls ← newBareLuaBackend env
            size ← evalInt ls $ luaLines
                [ "engine.setUIScale(4.0);"
                , "engine.getTextWidth = function(font, text, size) return #text * size * 0.6 end;"
                , bootSettings 800 2160
                , "m.showTab('notifications');"
                , "local nt = require('scripts.settings.notifications_tab');"
                , "local checkbox = require('scripts.ui.checkbox');"
                , "local firstCat = nil;"
                , "for k, _ in pairs(nt.checkboxes) do firstCat = k break end;"
                , "local w, _ = checkbox.getSize(nt.checkboxes[firstCat].log);"
                , "return w"
                ]
            size `shouldSatisfy` (> 0)

    describe "editable dropdowns preserve an in-progress (unsubmitted) filter edit across a resize (#748 round 7)" $ do
        it "settings menu's Resolution dropdown" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "local gt = require('scripts.settings.graphics_tab');"
                , "local dropdown = require('scripts.ui.dropdown');"
                , "dropdown.setRawText(gt.resolutionDropdownId, '19');"
                , "dropdown.focus(gt.resolutionDropdownId);"
                , "dropdown.setCursor(gt.resolutionDropdownId, 2);"
                , "m.onFramebufferResize(1600, 900);"
                , "local newId = require('scripts.settings.graphics_tab').resolutionDropdownId;"
                , "return {text = dropdown.getRawText(newId), cursor = dropdown.getCursor(newId),"
                    <> " focused = dropdown.isFocused(newId)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe TextboxStateProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    tspText p `shouldBe` "19"
                    tspCursor p `shouldBe` 2
                    tspFocused p `shouldBe` True

        it "create-world's World Size dropdown" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootCreateWorld 1280 720 <> ";"
                , "local st = require('scripts.create_world.settings_tab');"
                , "local dropdown = require('scripts.ui.dropdown');"
                , "dropdown.setRawText(st.sizeDropdownId, '25');"
                , "dropdown.focus(st.sizeDropdownId);"
                , "dropdown.setCursor(st.sizeDropdownId, 1);"
                , "m.onFramebufferResize(1600, 900);"
                , "local newId = require('scripts.create_world.settings_tab').sizeDropdownId;"
                , "return {text = dropdown.getRawText(newId), cursor = dropdown.getCursor(newId),"
                    <> " focused = dropdown.isFocused(newId)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe TextboxStateProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    tspText p `shouldBe` "25"
                    tspCursor p `shouldBe` 1
                    tspFocused p `shouldBe` True

    describe "keyboard control focus (#745) survives a resize rebuild" $ do
        it "settings menu restores focus onto the rebuilt control with the same name" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                -- createUI() never shows its own page (some callers,
                -- e.g. init(), deliberately want it built-but-hidden);
                -- showing it here simulates the screen genuinely being
                -- the one on-screen, which is what onFramebufferResize's
                -- wasVisible guard checks for before restoring focus.
                , "UI.showPage(m.page);"
                , "local button = require('scripts.ui.button');"
                , "UI.setControlFocus(button.getElementHandle(m.backButtonId));"
                , "local hadFocusBefore = UI.hasControlFocus(button.getElementHandle(m.backButtonId));"
                , "m.onFramebufferResize(1600, 900);"
                , "local hasFocusAfter = UI.hasControlFocus(button.getElementHandle(m.backButtonId));"
                , "return {hadFocusBefore=hadFocusBefore, hasFocusAfter=hasFocusAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ControlFocusProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    cfpHadFocusBefore p `shouldBe` True
                    cfpHasFocusAfter p `shouldBe` True

        it "create-world menu restores focus onto the rebuilt control with the same name" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootCreateWorld 1280 720 <> ";"
                , "UI.showPage(m.page);"
                , "local button = require('scripts.ui.button');"
                , "UI.setControlFocus(button.getElementHandle(m.backButtonId));"
                , "local hadFocusBefore = UI.hasControlFocus(button.getElementHandle(m.backButtonId));"
                , "m.onFramebufferResize(1600, 900);"
                , "local hasFocusAfter = UI.hasControlFocus(button.getElementHandle(m.backButtonId));"
                , "return {hadFocusBefore=hadFocusBefore, hasFocusAfter=hasFocusAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ControlFocusProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    cfpHadFocusBefore p `shouldBe` True
                    cfpHasFocusAfter p `shouldBe` True

        -- #748 round 5: main_menu/pause_menu/save_browser previously
        -- destroyed+recreated their clickable controls on resize with no
        -- focus snapshot/restore at all (settings_menu/create_world_menu
        -- were the only two screens covered above).
        it "main menu restores focus onto the rebuilt control with the same name" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootMain 1280 720 <> ";"
                , "UI.showPage(m.page);"
                , "UI.setControlFocus(m.ownedBoxes[1]);"
                , "local hadFocusBefore = UI.hasControlFocus(m.ownedBoxes[1]);"
                , "m.onFramebufferResize(1600, 900);"
                , "local hasFocusAfter = UI.hasControlFocus(m.ownedBoxes[1]);"
                , "return {hadFocusBefore=hadFocusBefore, hasFocusAfter=hasFocusAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ControlFocusProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    cfpHadFocusBefore p `shouldBe` True
                    cfpHasFocusAfter p `shouldBe` True

        it "pause menu restores focus onto the rebuilt control with the same name" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootPause 1280 720 <> ";"
                , "UI.setControlFocus(m.ownedBoxes[1]);"
                , "local hadFocusBefore = UI.hasControlFocus(m.ownedBoxes[1]);"
                , "m.onFramebufferResize(1600, 900);"
                , "local hasFocusAfter = UI.hasControlFocus(m.ownedBoxes[1]);"
                , "return {hadFocusBefore=hadFocusBefore, hasFocusAfter=hasFocusAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ControlFocusProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    cfpHadFocusBefore p `shouldBe` True
                    cfpHasFocusAfter p `shouldBe` True

        it "save browser restores focus onto the rebuilt Back button" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootSaveBrowser 1280 720 <> ";"
                , "local button = require('scripts.ui.button');"
                , "local backHandle = button.getElementHandle(m.ownedButtons[1]);"
                , "UI.setControlFocus(backHandle);"
                , "local hadFocusBefore = UI.hasControlFocus(backHandle);"
                , "m.onFramebufferResize(1600, 900);"
                , "local backHandleAfter = button.getElementHandle(m.ownedButtons[1]);"
                , "local hasFocusAfter = UI.hasControlFocus(backHandleAfter);"
                , "return {hadFocusBefore=hadFocusBefore, hasFocusAfter=hasFocusAfter}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ControlFocusProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    cfpHadFocusBefore p `shouldBe` True
                    cfpHasFocusAfter p `shouldBe` True

-- * Boot helpers (synthetic texture/font handles — nothing renders
--   headless, so their numeric values are never inspected)

bootMain, bootSettings, bootCreateWorld, bootPause, bootSaveBrowser ∷ Int → Int → Text
bootMain w h = "local m = require('scripts.main_menu'); m.init(1,2,3,4," <> tshow w <> "," <> tshow h <> ")"
bootSettings w h = "local m = require('scripts.settings_menu'); m.init(1,2,3," <> tshow w <> "," <> tshow h <> ")"
-- randbox/textbox need their own .init() (shared box textures, module-
-- level) that only ui_manager_boot.lua's real boot sequence normally
-- calls — create_world_menu.lua itself never does, since production
-- always reaches it through that boot. Skipped here (as this whole
-- suite skips uiManager.init()), a widget's underlying box element
-- silently gets a nil texture handle and UI.newBox returns no handle
-- at all, rather than erroring.
bootCreateWorld w h = "require('scripts.ui.randbox').init(); require('scripts.ui.textbox').init(); require('scripts.ui.dropdown').init(); local m = require('scripts.create_world_menu'); m.init(1,2,3," <> tshow w <> "," <> tshow h <> ")"
bootPause w h = "local m = require('scripts.pause_menu'); m.init(1,2,3,4," <> tshow w <> "," <> tshow h <> "); m.show({showSave=false})"
bootSaveBrowser w h = luaLines
    [ "local m = require('scripts.save_browser');"
    , "m.init(1,2,3," <> tshow w <> "," <> tshow h <> ");"
    , "m.show({{name='only',timestamp='t'}}, function() end, function() end)"
    ]

bootLoading ∷ Int → Int → Text
bootLoading w h = luaLines
    [ "local m = require('scripts.loading_screen');"
    , "m.init(1,2," <> tshow w <> "," <> tshow h <> ");"
    , "m.show({mode='worldgen', statusText='Loading...', fbW=" <> tshow w <> ", fbH=" <> tshow h <> "})"
    ]

-- | Bounds check for a screen's own tracked `panelId` against the
--   framebuffer size just booted with — @mVar@ names the already-
--   `require`d local the matching @boot*@ snippet bound (both are
--   joined into one Lua chunk via 'luaLines', so it stays in scope).
panelInFrameExpr ∷ Text → Int → Int → Text
panelInFrameExpr mVar w h = luaLines
    [ "(function()"
    , "local p = require('scripts.ui.panel');"
    , "local x, y = p.getPosition(" <> mVar <> ".panelId);"
    , "local pw, ph = p.getSize(" <> mVar <> ".panelId);"
    , "return x >= 0 and y >= 0 and (x + pw) <= " <> tshow w <> " and (y + ph) <= " <> tshow h
    , "end)()"
    ]

classifyCall ∷ Int → Int → Double → Text
classifyCall w h s = "return " <> classifyExpr w h s

classifyExpr ∷ Int → Int → Double → Text
classifyExpr w h s =
    "require('scripts.ui.responsive').classify(" <> tshow w <> ", " <> tshow h <> ", " <> tshow s <> ").supported"

setScaleCall ∷ Double → Text
setScaleCall s = "engine.setUIScale(" <> tshow s <> ")"

tshow ∷ Show a ⇒ a → Text
tshow = T.pack ∘ show

-- * JSON decode targets

data ResSupport = ResSupport { rsW ∷ Int, rsH ∷ Int, rsSupported ∷ Bool } deriving Show
instance FromJSON ResSupport where
    parseJSON = withObject "ResSupport" $ \o →
        ResSupport <$> o .: "w" <*> o .: "h" <*> o .: "supported"

data ResDims = ResDims { rdW ∷ Int, rdH ∷ Int } deriving Show
instance FromJSON ResDims where
    parseJSON = withObject "ResDims" $ \o → ResDims <$> o .: "w" <*> o .: "h"

data Classification = Classification { clsSupported ∷ Bool, clsReason ∷ Maybe Text } deriving Show
instance FromJSON Classification where
    parseJSON = withObject "Classification" $ \o →
        Classification <$> o .: "supported" <*> o .: "reason"

data RestoreProbe = RestoreProbe
    { rpCalls ∷ Int, rpPendingWhileMinimized ∷ Bool, rpPendingAfterRestore ∷ Bool }    deriving Show
instance FromJSON RestoreProbe where
    parseJSON = withObject "RestoreProbe" $ \o → RestoreProbe
        <$> o .: "calls" <*> o .: "pendingWhileMinimized" <*> o .: "pendingAfterRestore"

data MinimizeProbe = MinimizeProbe { mpUnchanged ∷ Bool, mpRestored ∷ Bool } deriving Show
instance FromJSON MinimizeProbe where
    parseJSON = withObject "MinimizeProbe" $ \o →
        MinimizeProbe <$> o .: "unchanged" <*> o .: "restored"

data FrameLimitProbe = FrameLimitProbe
    { flPendingBefore ∷ Int, flWidgetBefore ∷ Int
    , flPendingAfter ∷ Int, flWidgetAfter ∷ Int
    } deriving Show
instance FromJSON FrameLimitProbe where
    parseJSON = withObject "FrameLimitProbe" $ \o → FrameLimitProbe
        <$> o .: "pendingBefore" <*> o .: "widgetBefore"
        <*> o .: "pendingAfter" <*> o .: "widgetAfter"

data ScrollProbe = ScrollProbe { spCanScroll ∷ Bool, spActiveTabOk ∷ Bool, spScrollOk ∷ Bool } deriving Show
instance FromJSON ScrollProbe where
    parseJSON = withObject "ScrollProbe" $ \o → ScrollProbe
        <$> o .: "canScroll" <*> o .: "activeTabOk" <*> o .: "scrollOk"

data BeforeAfter = BeforeAfter { baBefore ∷ Int, baAfter ∷ Int } deriving Show
instance FromJSON BeforeAfter where
    parseJSON = withObject "BeforeAfter" $ \o →
        BeforeAfter <$> o .: "before" <*> o .: "after"

data SelectProbe = SelectProbe { selValue ∷ Text, selCount ∷ Int } deriving Show
instance FromJSON SelectProbe where
    parseJSON = withObject "SelectProbe" $ \o →
        SelectProbe <$> o .: "value" <*> o .: "count"

data OutsideEnvelopeProbe = OutsideEnvelopeProbe
    { oepHasBack ∷ Bool, oepHasApply ∷ Bool, oepHasSave ∷ Bool, oepValidDims ∷ Bool }    deriving Show
instance FromJSON OutsideEnvelopeProbe where
    parseJSON = withObject "OutsideEnvelopeProbe" $ \o → OutsideEnvelopeProbe
        <$> o .: "hasBack" <*> o .: "hasApply" <*> o .: "hasSave" <*> o .: "validDims"

data ButtonBarProbe = ButtonBarProbe
    { bbIdleOverlap ∷ Bool, bbIdleInFrame ∷ Bool
    , bbDoneOverlap ∷ Bool, bbDoneInFrame ∷ Bool
    } deriving Show
instance FromJSON ButtonBarProbe where
    parseJSON = withObject "ButtonBarProbe" $ \o → ButtonBarProbe
        <$> o .: "idleOverlap" <*> o .: "idleInFrame"
        <*> o .: "doneOverlap" <*> o .: "doneInFrame"

data BackButtonProbe = BackButtonProbe { bbpY ∷ Double, bbpBottom ∷ Double } deriving Show
instance FromJSON BackButtonProbe where
    parseJSON = withObject "BackButtonProbe" $ \o →
        BackButtonProbe <$> o .: "y" <*> o .: "bottom"

data CompactFallbackProbe = CompactFallbackProbe
    { cfPanelInFrame ∷ Bool, cfTitleY ∷ Double } deriving Show
instance FromJSON CompactFallbackProbe where
    parseJSON = withObject "CompactFallbackProbe" $ \o →
        CompactFallbackProbe <$> o .: "panelInFrame" <*> o .: "titleY"

data SaveBrowserExtremeProbe = SaveBrowserExtremeProbe
    { sbepPanelInFrame ∷ Bool, sbepValidWidth ∷ Bool } deriving Show
instance FromJSON SaveBrowserExtremeProbe where
    parseJSON = withObject "SaveBrowserExtremeProbe" $ \o → SaveBrowserExtremeProbe
        <$> o .: "panelInFrame" <*> o .: "validWidth"

data TabScrollProbe = TabScrollProbe
    { tscpTotalRows ∷ Int, tscpMaxVisibleRows ∷ Int, tscpHasScrollbar ∷ Bool } deriving Show
instance FromJSON TabScrollProbe where
    parseJSON = withObject "TabScrollProbe" $ \o → TabScrollProbe
        <$> o .: "totalRows" <*> o .: "maxVisibleRows" <*> o .: "hasScrollbar"

data ScrollRevealProbe = ScrollRevealProbe
    { srpClippedBefore ∷ Bool, srpVisibleAfter ∷ Bool } deriving Show
instance FromJSON ScrollRevealProbe where
    parseJSON = withObject "ScrollRevealProbe" $ \o → ScrollRevealProbe
        <$> o .: "clippedBefore" <*> o .: "visibleAfter"

data ButtonLabelFitProbe = ButtonLabelFitProbe
    { blfpBoxX ∷ Double, blfpLabelX ∷ Double } deriving Show
instance FromJSON ButtonLabelFitProbe where
    parseJSON = withObject "ButtonLabelFitProbe" $ \o → ButtonLabelFitProbe
        <$> o .: "boxX" <*> o .: "labelX"

data NotifCheckboxProbe = NotifCheckboxProbe
    { ncpCheckboxSize ∷ Double, ncpX ∷ Double, ncpRightEdge ∷ Double } deriving Show
instance FromJSON NotifCheckboxProbe where
    parseJSON = withObject "NotifCheckboxProbe" $ \o → NotifCheckboxProbe
        <$> o .: "checkboxSize" <*> o .: "x" <*> o .: "rightEdge"

data TextboxStateProbe = TextboxStateProbe
    { tspText ∷ Text, tspCursor ∷ Int, tspFocused ∷ Bool } deriving Show
instance FromJSON TextboxStateProbe where
    parseJSON = withObject "TextboxStateProbe" $ \o → TextboxStateProbe
        <$> o .: "text" <*> o .: "cursor" <*> o .: "focused"

data PlateCountProbe = PlateCountProbe
    { pcText ∷ Text, pcPendingUnchanged ∷ Bool } deriving Show
instance FromJSON PlateCountProbe where
    parseJSON = withObject "PlateCountProbe" $ \o →
        PlateCountProbe <$> o .: "text" <*> o .: "pendingUnchanged"

data OverlapFrameProbe = OverlapFrameProbe
    { ofpAnyOverlap ∷ Bool, ofpAllInFrame ∷ Bool } deriving Show
instance FromJSON OverlapFrameProbe where
    parseJSON = withObject "OverlapFrameProbe" $ \o →
        OverlapFrameProbe <$> o .: "anyOverlap" <*> o .: "allInFrame"

data WorldNameProbe = WorldNameProbe
    { wnpX ∷ Double, wnpRightEdge ∷ Double } deriving Show
instance FromJSON WorldNameProbe where
    parseJSON = withObject "WorldNameProbe" $ \o →
        WorldNameProbe <$> o .: "x" <*> o .: "rightEdge"

data VisibilityProbe = VisibilityProbe
    { vpVisibleBefore ∷ Bool, vpVisibleAfter ∷ Bool } deriving Show
instance FromJSON VisibilityProbe where
    parseJSON = withObject "VisibilityProbe" $ \o →
        VisibilityProbe <$> o .: "visibleBefore" <*> o .: "visibleAfter"

data CreateWorldExtremeProbe = CreateWorldExtremeProbe
    { cwepPanelInFrame ∷ Bool, cwepNameX ∷ Double, cwepNameRightEdge ∷ Double
    } deriving Show
instance FromJSON CreateWorldExtremeProbe where
    parseJSON = withObject "CreateWorldExtremeProbe" $ \o → CreateWorldExtremeProbe
        <$> o .: "panelInFrame" <*> o .: "nameX" <*> o .: "nameRightEdge"

data ControlFocusProbe = ControlFocusProbe
    { cfpHadFocusBefore ∷ Bool, cfpHasFocusAfter ∷ Bool } deriving Show
instance FromJSON ControlFocusProbe where
    parseJSON = withObject "ControlFocusProbe" $ \o →
        ControlFocusProbe <$> o .: "hadFocusBefore" <*> o .: "hasFocusAfter"

-- * Lua backend + eval helpers (mirrors Test.Headless.UI.InputOwnership)

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

-- | Live element/page counts straight off the shared 'UIPageManager' —
--   the growth check doesn't need any new Lua introspection surface,
--   just what 'Engine.Scripting.Lua.API.UI' already mutates in place.
countUI ∷ EngineEnv → IO (Int, Int)
countUI env = do
    mgr ← readIORef (uiManagerRef env)
    pure (Map.size (upmElements mgr), Map.size (upmPages mgr))
