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
import Data.Aeson (FromJSON(..), decode, withObject, (.:), (.:?))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (VideoConfig(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..), ScriptValue(..))
import Engine.Scripting.Lua.Util (broadcastToModules)
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Harness.Isolation
  (isInsideIsolatedResourceRoot, withIsolatedResourceRoot)
import UI.Types (UIPageManager(..), emptyUIPageManager)

-- | Join Lua statements/fragments with a single space — every multi-line
--   snippet in this file is built this way instead of GHC string-gap
--   continuations, which are easy to get subtly wrong (a missing space
--   before a line-continuing backslash silently glues two tokens
--   together) in a file whose whole point is long inline Lua text.
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

-- | The canonical UI scale every example in this suite starts from.
--   Matches the tracked @config/video_default.yaml@'s @ui_scale@, so a
--   machine with no local overlay sees no behavioral change at all.
menusBaselineUIScale ∷ Float
menusBaselineUIScale = 1.0

-- | Pin the engine's in-memory UI scale to 'menusBaselineUIScale',
--   preserving every other 'VideoConfig' field exactly as engine
--   initialization resolved it (#1266).
--
--   This is an in-memory mutation ONLY — the same narrow one
--   @engine.setUIScale@ performs ('Engine.Scripting.Lua.API.Config'),
--   whose persistence is a separate @saveVideoConfig@ call this suite
--   never makes.
--
--   That fact alone never made the whole SUITE non-writing, and #1357
--   found the gap: three examples (two here, one in
--   'Test.Headless.UI.ResponsiveGameplay') drive the real
--   @settingsMenu.onDefaults()@, whose keybind reset is write-through
--   by contract — it called the production @engine.saveKeybinds()@ and
--   silently replaced the developer's @config/keybinds.local.yaml@.
--   What guarantees #1266's \"tests never modify, truncate or
--   regenerate @config/*.local.yaml@\" is now the filesystem boundary
--   in 'withMenusEngine' below, not this function.
normalizeUIScale ∷ EngineEnv → IO ()
normalizeUIScale env =
    atomicModifyIORef' (videoConfigRef env) $ \c →
        (c { vcUIScale = menusBaselineUIScale }, ())

-- | Every example here runs against its own freshly booted headless
--   engine, and 'Engine.Core.Init' populates that engine's
--   'videoConfigRef' from the developer's @config/video.local.yaml@
--   when one exists, falling back to @config/video_default.yaml@
--   otherwise (#638/#786's local-overlay contract, which is correct
--   and out of scope here). Without this wrapper an example's
--   effective UI scale is therefore whatever the developer last saved
--   from the Settings menu, and cases whose geometry assertions were
--   written against an implicit 1x flip verdict on a machine carrying
--   @ui_scale: 1.5@ — two of them did (#1266).
--
--   So establish the canonical baseline BEFORE the example body runs,
--   which is before any of its Lua modules or menu geometry
--   initialize. Cases that intentionally exercise a different scale
--   are unaffected: they already state it themselves with an explicit
--   @engine.setUIScale(...)@ as their first Lua statement, which
--   overrides this baseline exactly as it overrode the inherited
--   value before.
--
--   'Test.Headless.UI.ResponsiveGameplay.resetFixture' does the same
--   normalization for the same reason; it just folds it into a
--   shared-fixture reset rather than a per-example wrapper, because
--   that suite shares one engine across its cases.
--
--   The wrapper ALSO establishes #1357's filesystem boundary, and does
--   so OUTSIDE 'withHeadlessEngine': engine initialization is itself a
--   config writer (see 'Test.Headless.Harness.Isolation'), so isolating
--   only after the engine came up would already be too late.
withMenusEngine ∷ (EngineEnv → IO α) → IO α
withMenusEngine action = withIsolatedResourceRoot $
    withHeadlessEngine $ \env → do
        normalizeUIScale env
        action env

spec ∷ Spec
spec = around withMenusEngine $ do
    describe "suite config isolation (#1357)" $ do
        -- The guard that keeps 'withMenusEngine's filesystem boundary
        -- from being quietly unwired: two cases below drive the real
        -- settingsMenu.onDefaults(), whose write-through keybind reset
        -- persists through the production engine.saveKeybinds() to the
        -- cwd-relative config/keybinds.local.yaml. Every assertion in
        -- this suite passed while that overwrote the developer's file,
        -- so nothing else here would notice its return.
        it "runs inside the scratch resource root, never the checkout" $ \_ → do
            inScratch ← isInsideIsolatedResourceRoot
            inScratch `shouldBe` True

    describe "suite UI-scale baseline (#1266)" $ do
        it "pins the effective scale to 1x whatever the local video config resolved to, preserving every other video setting" $ \env → do
            -- Simulate an arbitrary developer overlay landing in the
            -- engine's live config, then re-run the wrapper's own
            -- normalization: the scale must come back to the baseline
            -- and nothing else may move. Deterministic on a machine
            -- with no config/video.local.yaml (CI) as well as one
            -- with any in-envelope ui_scale saved.
            atomicModifyIORef' (videoConfigRef env) $ \c →
                (c { vcUIScale = 1.5 }, ())
            skewed ← readIORef (videoConfigRef env)
            normalizeUIScale env
            pinned ← readIORef (videoConfigRef env)
            vcUIScale pinned `shouldBe` menusBaselineUIScale
            pinned `shouldBe` skewed { vcUIScale = menusBaselineUIScale }

        it "is what an example without its own engine.setUIScale actually observes" $ \env → do
            ls ← newBareLuaBackend env
            -- The value scripts/ui/scale.lua reads for all menu
            -- geometry, through the same engine.getUIScale every screen
            -- module uses.
            atBaseline ← evalBool ls "return engine.getUIScale() == 1.0"
            atBaseline `shouldBe` True

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

        -- #748 round 8 (extended round 12): every SETTINGS-configurable
        -- resolution (scripts/settings/data.lua's data.resolutions, not
        -- just a hand-picked sample) must produce a reachable, in-frame
        -- layout at 1x for EVERY C2 screen this issue scopes — the
        -- scale every one of them is fully supported at (per the
        -- envelope-classification describe above), so this is real
        -- layout/reachability coverage, not just the classifier's own
        -- say-so. 3840x2160 is checked too, best-effort, at 2.5x (its
        -- own auto-detected default scale, since 1x falls outside its
        -- 1.5x-4x band). Round 12 extended this from main/settings/
        -- create-world to ALSO cover pause/save-browser (their own
        -- fixed panelId) and loading (its own fixed progress bar),
        -- which the original round-8 pass left at just a 3-sample check
        -- (800x600/1920x1080/3840x2160) below the full matrix.
        it "every configured resolution (data.resolutions) at 1x — every C2 screen's fixed action stays reachable and in-frame" $ \env → do
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

                        -- Pause menu, save browser, and loading each
                        -- build their own fresh page (no shared
                        -- `.init` boot shape), so each gets its own
                        -- backend instance per the existing per-screen
                        -- tests' convention.
                        lsPause ← newBareLuaBackend env
                        _ ← eval lsPause (setScaleCall uiscale)
                        pauseOk ← evalBool lsPause (luaLines [bootPause w h, "return " <> panelInFrameExpr "m" w h])
                        pauseOk `shouldBe` True

                        lsSave ← newBareLuaBackend env
                        _ ← eval lsSave (setScaleCall uiscale)
                        saveOk ← evalBool lsSave (luaLines [bootSaveBrowser w h, "return " <> panelInFrameExpr "m" w h])
                        saveOk `shouldBe` True

                        lsLoad ← newBareLuaBackend env
                        _ ← eval lsLoad (setScaleCall uiscale)
                        loadOk ← evalBool lsLoad (luaLines [bootLoading w h, "return " <> barInFrameExpr "m" w h])
                        loadOk `shouldBe` True

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
                ok ← evalBool ls (luaLines [bootLoading w h, "return " <> barInFrameExpr "m" w h])
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
                -- #748 round 13: the fixed 480px (0.8*600) panel height
                -- alone can be smaller than the SUM of scaled chrome
                -- (title, tab row, bottom button row, gaps) subtracted
                -- from it — driving the tab bar's own frameHeight
                -- negative, which tabbar.new passed straight to
                -- UI.newBox as a real (invalid) box height.
                , "local tabbar = require('scripts.ui.tabbar');"
                , "local tbx, tby, tbw, tbh = tabbar.getFrameBounds(m.tabBarId);"
                , "return {hasBack=(m.backButtonId ~= nil), hasApply=(m.applyButtonId ~= nil),"
                    <> " hasSave=(m.saveButtonId ~= nil), validDims=(w > 0 and h > 0),"
                    <> " tabFrameValid=(tbw > 0 and tbh > 0)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe OutsideEnvelopeProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    oepHasBack p `shouldBe` True
                    oepHasApply p `shouldBe` True
                    oepHasSave p `shouldBe` True
                    oepValidDims p `shouldBe` True
                    oepTabFrameValid p `shouldBe` True

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

        -- #748 round 10: shrinking only the button BOX left the label
        -- rendering at the unshrunk base font size — the label's own
        -- centering math (labelX = (btnWidth - labelWidth) / 2) goes
        -- negative once labelWidth exceeds the shrunk box, meaning the
        -- text starts to the LEFT of the box's own left edge. Mirrors
        -- settings_menu's identical round-6 fix/test.
        it "create-world's Done-set button labels (Regenerate/Continue) stay within their own (shrunk) box at the formal 800x600@1x minimum" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "engine.getTextWidth = function(font, text, size) return #text * size * 0.6 end;"
                , "local m = require('scripts.create_world_menu');"
                , "m.init(1,2,3,800,600);"
                , "UI.showPage(m.page);"
                , "m.buildButtonsDone();"
                , "local button = require('scripts.ui.button');"
                , "local boxInfo = UI.getElementInfo(button.getElementHandle(m.regenerateButtonId));"
                , "local labelX = nil;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "  if e.name == 'regenerate_btn_label' then labelX = e.x end;"
                , "end;"
                , "return {boxX = boxInfo.x, labelX = labelX}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ButtonLabelFitProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → blfpLabelX p `shouldSatisfy` (>= blfpBoxX p)

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

    -- #1107 (epic #708 Phase 2): the save browser and the loading flow
    -- became the first consumers of the persisted world identity
    -- engine.listSaves() has exposed since #707. Everything here is
    -- about the identity staying DISTINCT from the save slot — three
    -- separate facts, three separate elements, one of which (the slot
    -- name) is still the only thing a selection dispatches.
    describe "world identity in the save browser (#1107)" $ do
        it "renders slot, world name and gloss as three separate fields, and degrades by absence — never by echoing the slot name" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ nonZeroMetrics 0.2
                , listInit
                , "local m = require('scripts.save_browser');"
                , "m.init(1,2,3,1280,720);"
                , "m.show({"
                , "  {name='before_the_raid',timestamp='2026-08-08T04:13:12.407920Z',"
                    <> "autosave=false,worldName='Karadun',worldGloss='Ashen Land'},"
                -- A custom player-entered name (#708 principle 7) has no
                -- language provenance, so listSaves omits worldGloss alone.
                , "  {name='custom_slot',timestamp='2026-08-08T04:13:19.199691Z',"
                    <> "autosave=false,worldName='Bobtown'},"
                -- A pre-identity save: BOTH fields omitted.
                , "  {name='ancient_slot',timestamp='2026-08-08T04:13:50.615025Z',autosave=false}"
                , "}, function() end, function() end);"
                , dumpRowsExpr
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe [SaveRowProbe] of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just rows → do
                    length rows `shouldBe` 3
                    case rows of
                        [named, custom, unnamed] → do
                            srLabel named `shouldSatisfy` T.isInfixOf "before_the_raid"
                            -- Distinct, not merged: the identity is
                            -- nowhere in the slot line's own text.
                            srLabel named `shouldSatisfy` (not ∘ T.isInfixOf "Karadun")
                            srWorldName named `shouldBe` "Karadun"
                            srGloss named `shouldBe` "\"Ashen Land\""

                            srWorldName custom `shouldBe` "Bobtown"
                            srGloss custom `shouldBe` ""

                            srLabel unnamed `shouldSatisfy` T.isInfixOf "ancient_slot"
                            srWorldName unnamed `shouldBe` ""
                            srGloss unnamed `shouldBe` ""

                            -- Requirement 4, and the reason the identity
                            -- went on its OWN line rather than into
                            -- columns beside the slot: the slot line
                            -- still gets the row's WHOLE usable width
                            -- (the identity line spans exactly the same
                            -- span, ±rounding), so it renders the full
                            -- slot name AND its timestamp untruncated —
                            -- taking a share for the identity is what
                            -- would have pushed the timestamp out.
                            forM_ rows $ \row → do
                                srSlotWidth row
                                    `shouldSatisfy` (>= srIdentityWidth row - 2)
                                srPrimary row `shouldBe` srLabel row
                            srLabel named
                                `shouldSatisfy` T.isInfixOf "2026-08-08T04:13:12.407920Z"
                        _ → expectationFailure "expected exactly three rows"

        it "keeps the durable [Autosave] and [Recovered] tags and the timestamp, and still dispatches the SLOT name on selection" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ listInit
                , "local m = require('scripts.save_browser');"
                , "local list = require('scripts.ui.list');"
                , "_G.__picked = '';"
                , "m.init(1,2,3,1280,720);"
                , "m.show({"
                , "  {name='autosave-1',timestamp='t1',autosave=true,"
                    <> "worldName='Karadun',worldGloss='Ashen Land'},"
                , "  {name='rescued',timestamp='t2',autosave=false,recovered=true}"
                , "}, function(v) _G.__picked = v end, function() end);"
                , "list.selectItem(m.listId, 1);"
                , "local d = list.dump();"
                , "return {row1=d[1].label, row2=d[2].label,"
                    <> " picked=_G.__picked, value=list.getSelectedValue(m.listId)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe SaveTagProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    stpRow1 p `shouldSatisfy` T.isInfixOf "[Autosave]"
                    stpRow1 p `shouldSatisfy` T.isInfixOf "t1"
                    stpRow2 p `shouldSatisfy` T.isInfixOf "[Recovered]"
                    stpRow2 p `shouldSatisfy` T.isInfixOf "t2"
                    -- The world identity is display-only: the load key
                    -- stays the save slot.
                    stpPicked p `shouldBe` "autosave-1"
                    stpValue p `shouldBe` "autosave-1"

        it "truncates an over-long multi-byte world name to its own column, leaving valid UTF-8" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ nonZeroMetrics 0.6
                , listInit
                , "local m = require('scripts.save_browser');"
                , "m.init(1,2,3,1280,720);"
                , "local long = '" <> longUnicodeName <> "';"
                , "m.show({{name='s1',timestamp='t1',autosave=false,"
                    <> "worldName=long,worldGloss='Ashen Land'}},"
                    <> " function() end, function() end);"
                , "local col = require('scripts.ui.list').dump()[1].columns.worldName;"
                , "return {rendered=col.text, valid=(utf8.len(col.text) ~= nil),"
                    <> " shorter=(#col.text < #long), fits=(col.textWidth <= col.width)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe TruncationProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    tpShorter p `shouldBe` True
                    -- The whole point of #1107 requirement 6: a
                    -- byte-offset cut inside a multi-byte sequence
                    -- would leave a dangling lead byte here.
                    tpValid p `shouldBe` True
                    tpFits p `shouldBe` True
                    tpRendered p `shouldSatisfy` T.isSuffixOf ".."

        it "the shared truncation helper never produces invalid UTF-8, at any width" $ \env → do
            ls ← newBareLuaBackend env
            bad ← evalInt ls $ luaLines
                [ nonZeroMetrics 0.6
                , "local tw = require('scripts.ui.text_wrap');"
                , "local s = '" <> longUnicodeName <> "';"
                , "local bad = 0;"
                , "for px = 1, 400 do"
                , "  local r = tw.truncateToWidth(s, 1, 24, px);"
                , "  if utf8.len(r) == nil then bad = bad + 1 end"
                , "end;"
                , "return bad"
                ]
            bad `shouldBe` 0

        forM_ [ (800 ∷ Int, 600 ∷ Int, 0.5 ∷ Double), (800, 2160, 4.0) ] $ \(w, h, sc) →
            it ("every row field stays inside its own column and inside the framebuffer at "
                    ⧺ show w ⧺ "x" ⧺ show h ⧺ "@" ⧺ show sc ⧺ "x") $ \env → do
                ls ← newBareLuaBackend env
                r ← evalJSON ls $ luaLines
                    [ setScaleCall sc <> ";"
                    , nonZeroMetrics 0.6
                    , listInit
                    , "local m = require('scripts.save_browser');"
                    , "m.init(1,2,3," <> tshow w <> "," <> tshow h <> ");"
                    , "m.show({{name='before_the_raid',timestamp='2026-08-07 12:00:00',"
                        <> "autosave=true,recovered=true,worldName='"
                        <> longUnicodeName <> "',worldGloss='Ashen Land of the Long Winter'}},"
                        <> " function() end, function() end);"
                    , "local d = require('scripts.ui.list').dump()[1];"
                    , "local fields = {d.primary, d.columns.worldName, d.columns.gloss};"
                    , "local inFrame, fits = true, true;"
                    , "for _, f in ipairs(fields) do"
                    , "  if f.x < 0 or (f.x + f.width) > " <> tshow w <> " then inFrame = false end;"
                    , "  if f.textWidth > f.width then fits = false end"
                    , "end;"
                    , "return {allInFrame=inFrame, allFit=fits}"
                    ]
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RowFitProbe of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just p → do
                        rfpAllInFrame p `shouldBe` True
                        rfpAllFit p `shouldBe` True

    describe "world identity in the loading flow (#1107)" $ do
        it "the main menu resolves the listing's identity from the slot name it was handed, and passes nil when there is none" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.main_menu');"
                , "engine.loadSave = function() return true end;"
                , "m.setShowMenuCallback(function(name, p) _G.__p = p end);"
                -- Both load entry points (Continue, and the browser's
                -- onSelect) carry only the slot name; the identity has
                -- to come back off the listing.
                , "m.saves = {{name='before_the_raid',timestamp='t1',"
                    <> "worldName='Karadun',worldGloss='Ashen Land'},"
                    <> " {name='ancient_slot',timestamp='t2'}};"
                , "m.loadAndShowSave('before_the_raid');"
                , "local named = _G.__p;"
                , "m.loadAndShowSave('ancient_slot');"
                , "local unnamed = _G.__p;"
                , "return {name=named.worldName, gloss=named.worldGloss,"
                    <> " statusBefore=named.statusText,"
                    <> " statusAfter=(unnamed.worldName == nil"
                    <> " and unnamed.worldGloss == nil) and 'absent' or 'leaked'}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe LoadingIdentityProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    lipName p `shouldBe` Just "Karadun"
                    lipGloss p `shouldBe` Just "Ashen Land"
                    -- The slot-derived status text is a SEPARATE field,
                    -- and it is what still names the slot.
                    lipStatusBefore p `shouldBe` Just "Loading before_the_raid..."
                    lipStatusAfter p `shouldBe` Just "absent"

        it "keeps the world's name and gloss on screen across load-phase updates that rewrite the status line" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.loading_screen');"
                , labelTextHelper
                , "m.init(1,2,1280,720);"
                , "m.show({mode='load', statusText='Loading before_the_raid...',"
                    <> " worldName='Karadun', worldGloss='Ashen Land',"
                    <> " fbW=1280, fbH=720});"
                , "local statusBefore = txt(m.statusLabelId);"
                , "engine.getLoadStatus = function() return {phase='LoadComponentsDecoded'} end;"
                , "m.update(0.1);"
                , "engine.getLoadStatus = function() return {phase='LoadStaged'} end;"
                , "m.update(0.1);"
                , "return {name=txt(m.worldNameLabelId), gloss=txt(m.glossLabelId),"
                    <> " statusBefore=statusBefore, statusAfter=txt(m.statusLabelId)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe LoadingIdentityProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    lipName p `shouldBe` Just "Karadun"
                    lipGloss p `shouldBe` Just "\"Ashen Land\""
                    -- The status line genuinely churned underneath it —
                    -- otherwise this would prove nothing about
                    -- independence.
                    lipStatusAfter p `shouldNotBe` lipStatusBefore p
                    lipStatusAfter p `shouldBe` Just "Rebuilding world..."

        it "shows no world-name field at all for a save with no identity, rather than echoing the slot name" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.loading_screen');"
                , labelTextHelper
                , "m.init(1,2,1280,720);"
                , "m.show({mode='load', statusText='Loading ancient_slot...',"
                    <> " fbW=1280, fbH=720});"
                , "local named = false;"
                , "for _, e in ipairs(UI.getVisibleElements()) do"
                , "  if e.name == 'loading_world_name' or e.name == 'loading_world_gloss'"
                , "  then named = true end"
                , "end;"
                , "return {hasName=(m.worldNameLabelId ~= nil),"
                    <> " hasGloss=(m.glossLabelId ~= nil), anyElement=named,"
                    <> " status=txt(m.statusLabelId)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe LoadingAbsentProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    lapHasName p `shouldBe` False
                    lapHasGloss p `shouldBe` False
                    lapAnyElement p `shouldBe` False
                    lapStatus p `shouldBe` Just "Loading ancient_slot..."

        it "a named world's identity does not leak into the next transaction that has none" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local m = require('scripts.loading_screen');"
                , labelTextHelper
                , "m.init(1,2,1280,720);"
                , "m.show({mode='load', statusText='a', worldName='Karadun',"
                    <> " worldGloss='Ashen Land', fbW=1280, fbH=720});"
                , "local first = txt(m.worldNameLabelId);"
                , "m.show({mode='load', statusText='b', fbW=1280, fbH=720});"
                , "return {name=first, gloss=nil, statusBefore=nil,"
                    <> " statusAfter=(m.worldNameLabelId == nil) and 'cleared' or 'leaked'}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe LoadingIdentityProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    lipName p `shouldBe` Just "Karadun"
                    lipStatusAfter p `shouldBe` Just "cleared"

        forM_ [ (800 ∷ Int, 600 ∷ Int, 0.5 ∷ Double), (800, 2160, 4.0) ] $ \(w, h, sc) →
            it ("the world name and gloss stay in-frame at " ⧺ show w ⧺ "x" ⧺ show h
                    ⧺ "@" ⧺ show sc ⧺ "x") $ \env → do
                ls ← newBareLuaBackend env
                r ← evalJSON ls $ luaLines
                    [ setScaleCall sc <> ";"
                    , nonZeroMetrics 0.6
                    , "local m = require('scripts.loading_screen');"
                    , "local label = require('scripts.ui.label');"
                    , "m.init(1,2," <> tshow w <> "," <> tshow h <> ");"
                    , "m.show({mode='load', statusText='Loading before_the_raid...',"
                        <> " worldName='" <> longUnicodeName <> "',"
                        <> " worldGloss='Ashen Land of the Long Winter',"
                        <> " fbW=" <> tshow w <> ", fbH=" <> tshow h <> "});"
                    , "local ok = true;"
                    , "for _, id in ipairs({m.worldNameLabelId, m.glossLabelId}) do"
                    , "  local lw = label.getSize(id);"
                    , "  local info = UI.getElementInfo(label.getElementHandle(id));"
                    , "  if info.x < 0 or info.y < 0 or (info.x + lw) > " <> tshow w
                        <> " then ok = false end"
                    , "end;"
                    , "return {allInFrame=ok, allFit=true}"
                    ]
                case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RowFitProbe of
                    Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                    Just p → rfpAllInFrame p `shouldBe` True

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

        -- #748 round 11: onDefaults/onBack can ALSO change the live
        -- UI scale (data.loadDefaults' auto 4K/1440p/1080p detection;
        -- data.revert reverting an applied-but-unsaved change back to
        -- the on-disk config) — previously only onApply/onSave fanned
        -- that out. Stub data.loadDefaults/revert themselves (rather
        -- than depending on real engine default-config/auto-detection
        -- specifics) to deterministically force a scale change, then
        -- verify the SAME fan-out (including the direct shell call)
        -- now fires.
        it "settingsMenu.onDefaults fans out a real scale change to shell" $ \env → do
            ls ← newBareLuaBackend env
            calls ← evalInt ls $ luaLines
                [ "local shell = require('scripts.shell');"
                , "local calls = 0;"
                , "local realHandler = shell.onFramebufferResize;"
                , "shell.onFramebufferResize = function(w, h) calls = calls + 1; return realHandler(w, h) end;"
                , "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "local data = require('scripts.settings.data');"
                , "data.loadDefaults = function() data.current.uiScale = data.current.uiScale + 1.0 end;"
                , "m.onDefaults();"
                , "return calls"
                ]
            calls `shouldSatisfy` (> 0)

        it "settingsMenu.onBack fans out a real scale change (from data.revert) to shell" $ \env → do
            ls ← newBareLuaBackend env
            calls ← evalInt ls $ luaLines
                [ "local shell = require('scripts.shell');"
                , "local calls = 0;"
                , "local realHandler = shell.onFramebufferResize;"
                , "shell.onFramebufferResize = function(w, h) calls = calls + 1; return realHandler(w, h) end;"
                , "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "local data = require('scripts.settings.data');"
                , "data.revert = function() data.current.uiScale = data.current.uiScale + 1.0 end;"
                , "m.onBack();"
                , "return calls"
                ]
            calls `shouldSatisfy` (> 0)

        it "settingsMenu.onDefaults does NOT fan out when the scale is unchanged" $ \env → do
            ls ← newBareLuaBackend env
            calls ← evalInt ls $ luaLines
                [ "local shell = require('scripts.shell');"
                , "local calls = 0;"
                , "local realHandler = shell.onFramebufferResize;"
                , "shell.onFramebufferResize = function(w, h) calls = calls + 1; return realHandler(w, h) end;"
                , "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "local data = require('scripts.settings.data');"
                , "data.loadDefaults = function() end;"  -- leaves data.current.uiScale untouched
                , "m.onDefaults();"
                , "return calls"
                ]
            calls `shouldBe` 0

        -- #748 round 10: shell receives LuaFramebufferResize straight
        -- from the engine (never through responsive.notifyResize,
        -- deliberately, to avoid double-routing a real resize — see
        -- round 7's comment) — meaning it never got notifyResize's own
        -- 0x0-minimize guard either. A minimize used to destroy+rebuild
        -- an already-visible shell against a degenerate 0x0
        -- framebuffer.
        it "a 0x0 minimize never rebuilds an already-visible shell against degenerate geometry, and a real resize afterward rebuilds normally" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "local shell = require('scripts.shell');"
                , "shell.init(0);"
                , "shell.show();"
                , "local rebuilds = 0;"
                , "local realRebuildBox = shell.rebuildBox;"
                , "shell.rebuildBox = function(...) rebuilds = rebuilds + 1; return realRebuildBox(...) end;"
                , "shell.onFramebufferResize(0, 0);"
                , "local rebuildsAfterMinimize = rebuilds;"
                , "shell.onFramebufferResize(1600, 900);"
                , "local rebuildsAfterRestore = rebuilds;"
                , "return {afterMinimize = rebuildsAfterMinimize, afterRestore = rebuildsAfterRestore}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RebuildCountsProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    rcpAfterMinimize p `shouldBe` 0
                    rcpAfterRestore p `shouldSatisfy` (> 0)

    -- #1325: every case in the block ABOVE requires scripts.shell
    -- before booting settings, which populates package.loaded first and
    -- makes both sides share one instance — the REVERSE of production,
    -- and exactly why they all passed against the split-identity bug.
    -- Production loads the shell through engine.loadScript
    -- (scripts/init_loader.lua), whose loadModuleRef runs dofile and
    -- deliberately leaves package.loaded alone; scripts/settings_menu.lua
    -- then `require`s "scripts.shell" much later. Every case here
    -- therefore starts with package.loaded["scripts.shell"] ABSENT and
    -- loads the shell the dofile way FIRST. Reverting shell.lua's
    -- self-registration fails this block.
    describe "shell debug console has one module identity per Lua state (#1325)" $ do
        it "a dofile-path load is the table a later require resolves to, without re-executing the file" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "if package.loaded['scripts.shell'] ~= nil then"
                , "  return {preloaded = true, sameTable = false, notReexecuted = false} end;"
                -- loadModuleRef IS Lua.dofileTrace
                -- (src/Engine/Scripting/Lua/Script.hs), so a bare dofile
                -- reproduces the production load exactly.
                , "local dofiled = dofile('scripts/shell.lua');"
                -- A second execution of the body would rebind every
                -- shell.* function on whatever table it resolved,
                -- wiping this sentinel — so the sentinel surviving is
                -- the observable proof the file ran only once, and is
                -- independent of the table-identity check beside it.
                , "local sentinel = function() return 'sentinel' end;"
                , "dofiled.rescale = sentinel;"
                , "local required = require('scripts.shell');"
                , "return {preloaded = false, sameTable = (required == dofiled),"
                    <> " notReexecuted = (required.rescale == sentinel)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ShellIdentityProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    sipPreloaded p `shouldBe` False
                    sipSameTable p `shouldBe` True
                    sipNotReexecuted p `shouldBe` True

        -- Requirements 2 and 4 together, through the REAL routes both
        -- sides use: engine.loadScript registers the module ref that
        -- Engine.Scripting.Lua.Util.broadcastToModules delivers
        -- LuaFramebufferResize to (Thread/Dispatch.hs's
        -- @LuaFramebufferResize w h@ arm is exactly this call), while
        -- settings_menu.lua reaches its shell through `require`. The
        -- counter is installed on the REQUIRED table, so it only ever
        -- increments if that is the same table the broadcast reaches.
        it "a real framebuffer broadcast reaches the table settings_menu holds, exactly once, with the shell still outside the responsive registry" $ \env → do
            ls ← newBareLuaBackend env
            _ ← evalOk ls $ luaLines
                [ "assert(package.loaded['scripts.shell'] == nil,"
                    <> " 'fixture must start with scripts.shell unloaded');"
                , "assert(engine.loadScript('scripts/shell.lua', 0.5) ~= nil,"
                    <> " 'production loadScript path failed');"
                -- ui_manager -> ui_manager_boot -> settings_menu are
                -- pass-through require hops whose only role is reaching
                -- settings_menu; what this fixture must preserve is the
                -- ORDER (loadScript first, require second), and
                -- settings_menu is the module that actually holds the
                -- shell reference (scripts/settings_menu.lua's own
                -- require of "scripts.shell" runs on this line).
                , "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "_G.__shell_resize_calls = 0;"
                , "local s = require('scripts.shell');"
                , "local realHandler = s.onFramebufferResize;"
                , "s.onFramebufferResize = function(w, h)"
                    <> " _G.__shell_resize_calls = _G.__shell_resize_calls + 1;"
                    <> " return realHandler(w, h) end;"
                , "return 'ok'"
                ]
            broadcastToModules ls "onFramebufferResize"
                [ScriptNumber 1600, ScriptNumber 900]
            r ← evalJSON ls $ luaLines
                [ "return {calls = _G.__shell_resize_calls,"
                    <> " registered = require('scripts.ui.responsive').isRegistered('shell')}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ShellBroadcastProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    sbpCalls p `shouldBe` 1
                    -- #748 round 7's deliberate rule: routing the shell
                    -- through responsive.notifyResize as well would
                    -- double-fire the handler above on every real resize.
                    sbpRegistered p `shouldBe` False

        -- Requirement 3. The observable proof that Settings reached the
        -- LIVE console's own upvalues (rather than merely a nominally
        -- shared table) is shell.rescale() finding nothing left to do:
        -- it compares engine.getUIScale() against the cached `uiscale`
        -- upvalue the resize handler is supposed to have just updated.
        -- Before the fix the settings fan-out drove a second table
        -- whose shellvisible is permanently false, so this table's
        -- cache still held the pre-Apply scale and rescale() returned
        -- true.
        it "a scale-only settings Apply rebuilds the live dofile-loaded shell immediately" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ "assert(package.loaded['scripts.shell'] == nil,"
                    <> " 'fixture must start with scripts.shell unloaded');"
                , "local live = dofile('scripts/shell.lua');"
                , "live.init(0);"
                , "live.show();"
                , "local rebuilds = 0;"
                , "local realRebuildBox = live.rebuildBox;"
                , "live.rebuildBox = function(...) rebuilds = rebuilds + 1; return realRebuildBox(...) end;"
                -- Scale starts at whatever m.init() captures as
                -- data.current.uiScale — deliberately NOT pre-set, or
                -- Apply would see no change at all.
                , "local m = require('scripts.settings_menu');"
                , "m.init(1,2,3,1280,720);"
                , "local graphicsTab = require('scripts.settings.graphics_tab');"
                , "local textbox = require('scripts.ui.textbox');"
                , "local data = require('scripts.settings.data');"
                , "local target = (data.current.uiScale >= 3.0) and 1.0 or (data.current.uiScale + 1.0);"
                , "textbox.setText(graphicsTab.uiScaleTextBoxId, tostring(target));"
                , "m.onApply();"
                , "return {rebuilds = rebuilds, noFurtherChange = not live.rescale()}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe ShellLiveApplyProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    slapRebuilds p `shouldSatisfy` (> 0)
                    slapNoFurtherChange p `shouldBe` True

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

-- | Loading screen has no panelId — its fixed action is the progress
--   bar itself (m.barId via scripts/ui/bar.lua).
barInFrameExpr ∷ Text → Int → Int → Text
barInFrameExpr mVar w h = luaLines
    [ "(function()"
    , "local b = require('scripts.ui.bar');"
    , "local info = UI.getElementInfo(b.getElementHandle(" <> mVar <> ".barId));"
    , "return info.x >= 0 and info.y >= 0"
        <> " and (info.x + info.width) <= " <> tshow w
        <> " and (info.y + info.height) <= " <> tshow h
    , "end)()"
    ]

-- | This suite's Lua backend has no font atlas, so the real
--   @engine.getTextWidth@ measures 0 and no width-driven layout rule
--   ever fires. #1107's row columns and truncation are entirely
--   width-driven, so every test that exercises them replaces it with
--   proportional-per-character metrics first (@pxPerUnit@ scales the
--   result so a case can choose whether its sample text overflows).
nonZeroMetrics ∷ Double → Text
nonZeroMetrics pxPerUnit =
    "engine.getTextWidth = function(font, text, size) return #text * size * "
        <> tshow pxPerUnit <> " end;"

-- | scripts/ui/list.lua's per-row sprites (hit box, highlight) are
--   created against the ONE chrome texture list.init() loads, and
--   list.dump() reports a row only when its hit box resolves to a real
--   element. Production always reaches the list through
--   ui_manager_boot.lua's widget-init pass; this suite skips that boot
--   (as bootCreateWorld does for randbox/textbox), so any test reading
--   rendered rows has to run it itself.
listInit ∷ Text
listInit = "require('scripts.ui.list').init();"

-- | Multi-byte throughout (Latin Extended + Icelandic + Old English
--   letters), and long enough to overflow any column this suite
--   allocates — so a byte-offset cut lands inside a UTF-8 sequence
--   unless the truncation snaps back to a character boundary.
longUnicodeName ∷ Text
longUnicodeName = "Ǫrmstunga Þórsmörk Ǣthelwine Karádún Ǽsclinga Hræfnsholt"

-- | Every visible row of the ONE list this suite ever builds, as the
--   three facts #1107 requires stay distinct.
dumpRowsExpr ∷ Text
dumpRowsExpr = luaLines
    [ "local out = {};"
    , "for _, e in ipairs(require('scripts.ui.list').dump()) do"
    , "  out[#out+1] = {label=e.label, primary=e.primary.text,"
    , "                 slotWidth=e.primary.width,"
    , "                 identityWidth=(e.columns.worldName.width"
    , "                                + e.columns.gloss.width),"
    , "                 worldName=e.columns.worldName.text,"
    , "                 gloss=e.columns.gloss.text}"
    , "end;"
    , "return out"
    ]

-- | @txt(labelId)@ — the text a label widget is currently RENDERING
--   (read back off the element, not the widget's own bookkeeping), or
--   nil when the caller never created that label at all.
labelTextHelper ∷ Text
labelTextHelper = luaLines
    [ "local __label = require('scripts.ui.label');"
    , "local function txt(id)"
    , "  if not id then return nil end;"
    , "  return UI.getElementInfo(__label.getElementHandle(id)).text"
    , "end;"
    ]

classifyCall ∷ Int → Int → Double → Text
classifyCall w h s = "return " <> classifyExpr w h s

classifyExpr ∷ Int → Int → Double → Text
classifyExpr w h s =
    "require('scripts.ui.responsive').classify(" <> tshow w <> ", " <> tshow h <> ", " <> tshow s <> ").supported"

setScaleCall ∷ Double → Text
setScaleCall s = "engine.setUIScale(" <> tshow s <> ")"

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
    { oepHasBack ∷ Bool, oepHasApply ∷ Bool, oepHasSave ∷ Bool, oepValidDims ∷ Bool
    , oepTabFrameValid ∷ Bool
    } deriving Show
instance FromJSON OutsideEnvelopeProbe where
    parseJSON = withObject "OutsideEnvelopeProbe" $ \o → OutsideEnvelopeProbe
        <$> o .: "hasBack" <*> o .: "hasApply" <*> o .: "hasSave" <*> o .: "validDims"
        <*> o .: "tabFrameValid"

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

data RebuildCountsProbe = RebuildCountsProbe
    { rcpAfterMinimize ∷ Int, rcpAfterRestore ∷ Int } deriving Show
instance FromJSON RebuildCountsProbe where
    parseJSON = withObject "RebuildCountsProbe" $ \o → RebuildCountsProbe
        <$> o .: "afterMinimize" <*> o .: "afterRestore"

-- | #1325: scripts/shell.lua's single-module-identity probes.
data ShellIdentityProbe = ShellIdentityProbe
    { sipPreloaded ∷ Bool, sipSameTable ∷ Bool, sipNotReexecuted ∷ Bool }
    deriving Show
instance FromJSON ShellIdentityProbe where
    parseJSON = withObject "ShellIdentityProbe" $ \o → ShellIdentityProbe
        <$> o .: "preloaded" <*> o .: "sameTable" <*> o .: "notReexecuted"

data ShellBroadcastProbe = ShellBroadcastProbe
    { sbpCalls ∷ Int, sbpRegistered ∷ Bool } deriving Show
instance FromJSON ShellBroadcastProbe where
    parseJSON = withObject "ShellBroadcastProbe" $ \o → ShellBroadcastProbe
        <$> o .: "calls" <*> o .: "registered"

data ShellLiveApplyProbe = ShellLiveApplyProbe
    { slapRebuilds ∷ Int, slapNoFurtherChange ∷ Bool } deriving Show
instance FromJSON ShellLiveApplyProbe where
    parseJSON = withObject "ShellLiveApplyProbe" $ \o → ShellLiveApplyProbe
        <$> o .: "rebuilds" <*> o .: "noFurtherChange"

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

-- #1107 decode targets

data SaveRowProbe = SaveRowProbe
    { srLabel ∷ Text, srPrimary ∷ Text, srWorldName ∷ Text, srGloss ∷ Text
    , srSlotWidth ∷ Double, srIdentityWidth ∷ Double
    } deriving Show
instance FromJSON SaveRowProbe where
    parseJSON = withObject "SaveRowProbe" $ \o → SaveRowProbe
        <$> o .: "label" <*> o .: "primary"
        <*> o .: "worldName" <*> o .: "gloss"
        <*> o .: "slotWidth" <*> o .: "identityWidth"

data SaveTagProbe = SaveTagProbe
    { stpRow1 ∷ Text, stpRow2 ∷ Text, stpPicked ∷ Text, stpValue ∷ Text } deriving Show
instance FromJSON SaveTagProbe where
    parseJSON = withObject "SaveTagProbe" $ \o → SaveTagProbe
        <$> o .: "row1" <*> o .: "row2" <*> o .: "picked" <*> o .: "value"

data TruncationProbe = TruncationProbe
    { tpRendered ∷ Text, tpValid ∷ Bool, tpShorter ∷ Bool, tpFits ∷ Bool } deriving Show
instance FromJSON TruncationProbe where
    parseJSON = withObject "TruncationProbe" $ \o → TruncationProbe
        <$> o .: "rendered" <*> o .: "valid" <*> o .: "shorter" <*> o .: "fits"

data RowFitProbe = RowFitProbe { rfpAllInFrame ∷ Bool, rfpAllFit ∷ Bool } deriving Show
instance FromJSON RowFitProbe where
    parseJSON = withObject "RowFitProbe" $ \o →
        RowFitProbe <$> o .: "allInFrame" <*> o .: "allFit"

data LoadingIdentityProbe = LoadingIdentityProbe
    { lipName ∷ Maybe Text, lipGloss ∷ Maybe Text
    , lipStatusBefore ∷ Maybe Text, lipStatusAfter ∷ Maybe Text
    } deriving (Show, Eq)
instance FromJSON LoadingIdentityProbe where
    parseJSON = withObject "LoadingIdentityProbe" $ \o → LoadingIdentityProbe
        <$> o .:? "name" <*> o .:? "gloss"
        <*> o .:? "statusBefore" <*> o .:? "statusAfter"

data LoadingAbsentProbe = LoadingAbsentProbe
    { lapHasName ∷ Bool, lapHasGloss ∷ Bool, lapAnyElement ∷ Bool
    , lapStatus ∷ Maybe Text
    } deriving Show
instance FromJSON LoadingAbsentProbe where
    parseJSON = withObject "LoadingAbsentProbe" $ \o → LoadingAbsentProbe
        <$> o .: "hasName" <*> o .: "hasGloss" <*> o .: "anyElement"
        <*> o .:? "status"

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
