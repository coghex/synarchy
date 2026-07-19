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

    describe "screen geometry stays in-frame across the supported envelope" $
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

    describe "loading screen stays in-frame" $
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

    describe "main/pause menu compact fallback keeps the panel + title in-frame at the maximum supported scale" $
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

-- * Boot helpers (synthetic texture/font handles — nothing renders
--   headless, so their numeric values are never inspected)

bootMain, bootSettings, bootCreateWorld, bootPause, bootSaveBrowser ∷ Int → Int → Text
bootMain w h = "local m = require('scripts.main_menu'); m.init(1,2,3,4," <> tshow w <> "," <> tshow h <> ")"
bootSettings w h = "local m = require('scripts.settings_menu'); m.init(1,2,3," <> tshow w <> "," <> tshow h <> ")"
bootCreateWorld w h = "local m = require('scripts.create_world_menu'); m.init(1,2,3," <> tshow w <> "," <> tshow h <> ")"
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
