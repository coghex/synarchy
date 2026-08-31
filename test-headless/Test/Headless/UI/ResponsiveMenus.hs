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
import Engine.Scripting.Lua.API.Shell (setupShellSandbox)
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

    -- #1959: the console sized itself from its scaled `middleWidth`
    -- constant and never consulted the framebuffer width, so at 1x its
    -- right edge landed at 40 + 64 + 1200 + 64 = 1368px and every band's
    -- maximum scale multiplied that -- the box ran off the side of every
    -- display the envelope above declares supported. The shell queried
    -- engine.getFramebufferSize() in five places and used only fbHeight.
    --
    -- Everything here is a HORIZONTAL contract. Vertical fit stays out of
    -- scope: calculateBoxHeight returns before its fbHeight clamp when the
    -- history is empty, so an out-of-envelope combination can still place
    -- the box off the top -- no case below asserts a y bound.
    describe "the debug console fits the framebuffer width (#1959)" $ do
        it "keeps the whole box in frame at every supported (width, height, scale) the envelope declares" $ \env → do
            ls ← newBareLuaBackend env
            -- Requirement 5: the combinations come FROM
            -- scripts/ui/responsive.lua (its MIN_WIDTH and its band
            -- table), never restated as literals here, so widening a band
            -- or raising a maximum scale widens this gate with it.
            envelope ← decodeProbe "envelope" =≪ evalJSON ls envelopeExpr
            let widths = [epMinWidth envelope, 1366, 1920, 3840]
                combos = [ (w, h, sc)
                         | b  ← epBands envelope
                         , w  ← widths
                         , h  ← [brMinH b, brMaxH b]
                         , sc ← [brMinScale b, brMaxScale b]
                         ]
            -- Non-vacuity: a band table that failed to decode into rows
            -- would otherwise pass this example by asserting nothing.
            length combos `shouldSatisfy` (≥ 16)
            _ ← evalOk ls shellBootExpr
            failures ← fmap concat $ mapM (checkFit env ls) combos
            failures `shouldBe` []

        it "keeps the preferred floor(1200 * scale) center wherever the box fits, and narrows only below that boundary" $ \env → do
            ls ← newBareLuaBackend env
            _ ← evalOk ls shellBootExpr
            -- Requirement 4, as the issue review corrected it: the
            -- preferred center is floor(1200 * uiscale), not a literal
            -- 1200 device pixels. Two scales, so a fit rule that happened
            -- to hold at 1x alone cannot pass.
            forM_ [(720 ∷ Int, 1.0 ∷ Double), (1080, 2.0)] $ \(h, sc) → do
                -- Far wider than any preferred center: nothing narrows.
                setFramebuffer env (4096, h)
                wide ← decodeProbe "wide" =≪ evalJSON ls (shellFitExpr 4096 h sc)
                sbpReportedWidth wide `shouldBe` preferredCenter sc
                sbpCenterWidth wide `shouldBe` preferredCenter sc
                -- The rendered box's own right edge at that width IS the
                -- boundary framebuffer width (marginLeft + 2 * tileSize +
                -- preferredCenter) -- read off the sprites rather than
                -- restated from the shell's private constants.
                let boundary = round (sbpMaxX wide) ∷ Int
                setFramebuffer env (boundary, h)
                atFit ← decodeProbe "boundary" =≪ evalJSON ls (shellFitExpr boundary h sc)
                sbpReportedWidth atFit `shouldBe` preferredCenter sc
                sbpMaxX atFit `shouldBe` fromIntegral boundary
                -- One pixel narrower, and the center gives up exactly one
                -- pixel -- it never collapses to some other fallback.
                setFramebuffer env (boundary - 1, h)
                below ← decodeProbe "below boundary" =≪ evalJSON ls (shellFitExpr (boundary - 1) h sc)
                sbpReportedWidth below `shouldBe` preferredCenter sc - 1
                sbpCenterWidth below `shouldBe` preferredCenter sc - 1
                sbpMaxX below `shouldBe` fromIntegral (boundary - 1)

        it "degrades safely below the formal minimum: no crash, no non-positive sprite width or text budget" $ \env → do
            ls ← newBareLuaBackend env
            _ ← evalOk ls shellBootExpr
            -- Requirement 3. evalJSON fails the example on any Lua error,
            -- so reaching an assertion at all is the no-crash half.
            --
            -- (a) Below MIN_WIDTH, but still wide enough for the whole box.
            setFramebuffer env (320, 600)
            narrow ← decodeProbe "narrow" =≪ evalJSON ls (shellFitExpr 320 600 1.0)
            sbpSupported narrow `shouldBe` False
            sbpCount narrow `shouldBe` 9
            forM_ (positiveWidths narrow) (`shouldSatisfy` (> 0))
            -- (b) Degenerate: at 4x the two edge tiles alone are 512px, so
            -- NO center width brings a 100px framebuffer's box in frame.
            -- Best-effort here means positive geometry, not containment,
            -- which is why this case asserts sizes and budgets only.
            setFramebuffer env (100, 600)
            tiny ← decodeProbe "tiny" =≪ evalJSON ls (shellFitExpr 100 600 4.0)
            sbpSupported tiny `shouldBe` False
            sbpCount tiny `shouldBe` 9
            forM_ (positiveWidths tiny) (`shouldSatisfy` (> 0))

        it "measures the input line, the ghost hint and history wrapping against the fitted width" $ \env → do
            ls ← newBareLuaBackend env
            -- Requirement 2. The bare backend's real engine.getTextWidth
            -- measures 0 headless, so every width-driven rule would fire
            -- vacuously; charge a fixed width per byte instead, which also
            -- makes a rendered line's own length its width.
            _ ← evalOk ls (fixedCharMetrics contentPx <> " return true")
            setFramebuffer env (1024, 768)
            _ ← evalOk ls shellBootExpr
            p ← decodeProbe "content" =≪ evalJSON ls (shellContentExpr contentPx 1024 768 1.0)
            -- The case is only meaningful where the preferred width does
            -- NOT fit, so pin that first.
            scpCenter p `shouldSatisfy` (< preferredCenter 1.0)
            scpBoxRight p `shouldSatisfy` (≤ 1024)
            -- Every budget lives inside the center the box was drawn at.
            scpInputWidth p `shouldSatisfy` (≤ scpCenter p)
            scpHistoryWidth p `shouldSatisfy` (≤ scpCenter p)
            scpResultWidth p `shouldSatisfy` (≤ scpHistoryWidth p)
            -- ...and the text really wrapped against them rather than
            -- overrunning: multiple lines, none wider than its own budget
            -- and none reaching past the box's right edge.
            scpCmdLines p `shouldSatisfy` (> 1)
            scpResLines p `shouldSatisfy` (> 1)
            scpAllFit p `shouldBe` True
            scpVisibleWidth p `shouldSatisfy` (≤ scpInputWidth p)
            -- The completion hint trails the input inside the same box.
            scpGhostSeen p `shouldBe` True
            scpGhostFits p `shouldBe` True

        it "preserves input, cursor, scroll, focus and history across a width rebuild while reflowing both" $ \env → do
            ls ← newBareLuaBackend env
            _ ← evalOk ls (fixedCharMetrics contentPx <> " return true")
            setFramebuffer env (1920, 1080)
            _ ← evalOk ls shellBootExpr
            before ← decodeProbe "before" =≪ evalJSON ls (shellSeedExpr contentPx)
            -- Non-vacuity: the seed really did scroll the input line and
            -- render wrapped history at the wide size.
            sspScroll before `shouldSatisfy` (> 0)
            sspLineCount before `shouldSatisfy` (> 1)
            sspHistoryJoined before `shouldSatisfy` (not ∘ T.null)
            setFramebuffer env (1024, 768)
            after ← decodeProbe "after" =≪ evalJSON ls (shellResizeExpr contentPx 1024 768)
            -- Requirement 6: nothing the rebuild does not own may move.
            sspInput after `shouldBe` sspInput before
            sspCursor after `shouldBe` sspCursor before
            sspFocus after `shouldBe` sspFocus before
            sspFocus after `shouldBe` Just (sspShellFocusId after)
            sspHistoryJoined after `shouldBe` sspHistoryJoined before
            -- The scroll offset is the one retained value that legitimately
            -- MOVES here, and only forward: it is the window onto the
            -- buffer, and a field that just got narrower cannot keep the
            -- cursor on screen without advancing it. It stays a valid
            -- window (never past the cursor) and never rewinds.
            sspScroll after `shouldSatisfy` (≥ sspScroll before)
            sspScroll after `shouldSatisfy` (≤ sspCursor after)
            -- ...while both displays reflow to the narrower fitted width.
            sspContentWidth after `shouldSatisfy` (< sspContentWidth before)
            sspLineCount after `shouldSatisfy` (> sspLineCount before)
            sspAllFit after `shouldBe` True
            sspVisibleChars after `shouldSatisfy` (< sspVisibleChars before)
            sspVisibleWidth after `shouldSatisfy` (≤ sspInputWidth after)
            sspBoxRight after `shouldSatisfy` (≤ 1024)
            -- The RENDERED input line, not just the value it should show:
            -- rebuildBox recreates shell_buffer from the raw buffer and
            -- parks shell_cursor at the line's start, so a resize that
            -- stops there draws the whole unscrolled input past the fitted
            -- width with the cursor in the wrong place.
            sspBufferText after `shouldBe` sspVisibleText after
            sspBufferWidth after `shouldSatisfy` (≤ sspInputWidth after)
            (sspBufferX after + sspBufferWidth after) `shouldSatisfy` (≤ sspBoxRight after)
            sspCursorX after `shouldSatisfy` (> sspBufferX after)
            sspCursorX after `shouldSatisfy` (≤ sspBoxRight after)

        it "keeps the prompt, input, cursor and ghost inside the fitted center at every band's maximum scale" $ \env → do
            ls ← newBareLuaBackend env
            -- The nine box sprites fitting is not the whole of requirement
            -- 2: the input ROW has to live inside them too. Charge 64px per
            -- byte, so the two-glyph prompt measures 128px whatever the
            -- scaled font size is — which is what the narrowest supported
            -- combination (MIN_WIDTH at the top band's 4x, a 128px fitted
            -- center) really does to shell.ttf, and what a flat inset from
            -- the center could not survive.
            _ ← evalOk ls (fixedCharMetrics promptPx <> " return true")
            envelope ← decodeProbe "envelope" =≪ evalJSON ls envelopeExpr
            let combos = [ (epMinWidth envelope, brMinH b, brMaxScale b)
                         | b ← epBands envelope ]
            length combos `shouldSatisfy` (≥ 4)
            _ ← evalOk ls shellBootExpr
            rows ← mapM (inputRowAt env ls) combos
            concatMap fst rows `shouldBe` []
            -- Both ends of the degradation ladder actually run: the widest
            -- band keeps its prompt, the narrowest has to drop it.
            map snd rows `shouldSatisfy` or
            map snd rows `shouldSatisfy` (not ∘ and)

        it "rebuilds at the new scale when the scale changed while the console was hidden" $ \env → do
            ls ← newBareLuaBackend env
            setFramebuffer env (800, 1601)
            _ ← evalOk ls shellBootExpr
            big ← decodeProbe "at 4x" =≪ evalJSON ls (shellFitExpr 800 1601 4.0)
            sbpCornerWidth big `shouldBe` 256      -- floor(64 * 4)
            -- Close the console, then apply a Settings scale change: the
            -- engine still delivers the resize, and shell.rescale() still
            -- runs, but there is no open box to rebuild behind it. The
            -- retained elements are the ones the next open would reuse.
            _ ← evalOk ls "require('scripts.shell').hide(); return true"
            setFramebuffer env (1280, 720)
            _ ← evalOk ls $ luaLines
                [ "engine.setUIScale(1.0);"
                , "require('scripts.shell').onFramebufferResize(1280, 720);"
                , "return true"
                ]
            after ← decodeProbe "after reopen" =≪ evalJSON ls (shellReopenExpr 1280 720)
            sbpSupported after `shouldBe` True
            sbpCount after `shouldBe` 9
            -- The corner sprites are the tell: rebuildBox's existing-element
            -- branch repositions from the new tileSize but never resizes
            -- them, so a reused 4x corner leaves the box hanging past the
            -- framebuffer at 1x.
            sbpCornerWidth after `shouldBe` 64
            sbpMinX after `shouldSatisfy` (≥ 0)
            sbpMaxX after `shouldSatisfy` (≤ 1280)
            sbpMinWidth after `shouldSatisfy` (> 0)

        it "redraws the completion ghost a width rebuild destroyed" $ \env → do
            ls ← newBareLuaBackend env
            _ ← evalOk ls (fixedCharMetrics contentPx <> " return true")
            setFramebuffer env (1920, 1080)
            _ ← evalOk ls shellBootExpr
            -- destroyAllElements deletes shell_ghost and clears the handle,
            -- and nothing in the box/history rebuild puts it back -- so a
            -- resize used to drop a live completion hint until the next
            -- keystroke happened to recompute it.
            before ← decodeProbe "ghost before" =≪ evalJSON ls (shellGhostSeedExpr contentPx)
            sspGhostSeen before `shouldBe` True
            sspGhostText before `shouldSatisfy` (not ∘ T.null)
            setFramebuffer env (1024, 768)
            after ← decodeProbe "ghost after" =≪ evalJSON ls (shellResizeExpr contentPx 1024 768)
            sspGhostSeen after `shouldBe` True
            sspGhostText after `shouldBe` sspGhostText before
            sspGhostRight after `shouldSatisfy` (≤ sspBoxRight after)

        it "leaves the rebuilt console on the pass-through debug layer" $ \env → do
            ls ← newBareLuaBackend env
            _ ← evalOk ls shellBootExpr
            setFramebuffer env (1024, 768)
            p ← decodeProbe "layer" =≪ evalJSON ls (shellFitExpr 1024 768 1.0)
            -- Requirement 6's pass-through half, against the engine's own
            -- paint key rather than the page name: LayerDebug's band is
            -- 200000 (UI.Types.uiLayerBand), and a LayerDebug page defaults
            -- non-exclusive, so a width rebuild must leave no modal
            -- boundary behind (#742).
            sbpMinPaintKey p `shouldSatisfy` (≥ 200000)
            sbpInputBlocked p `shouldBe` False
            sbpAllInScope p `shouldBe` True

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

    -- #1671 renamed this group from "... survives a resize rebuild":
    -- resize is no longer the only rebuild covered here — Defaults and
    -- a mid-generation preview arrival destroy+recreate an already
    -- visible page just as completely, and the contract
    -- (docs/engine_contracts.md §Responsive UI lifecycle) admits no
    -- rebuild-kind exemption. The "keyboard control focus" match token
    -- is unchanged.
    describe "keyboard control focus (#745) survives a rebuild" $ do
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

        -- #1671: three rebuilds of an ALREADY-VISIBLE page skipped the
        -- by-name snapshot/restore the resize cases above establish --
        -- Settings' Defaults, Create World's Defaults, and Create
        -- World's mid-generation preview arrival. Each drives the
        -- production handler (the two Defaults cases through the real
        -- button activation callback, so the click path itself is what
        -- rebuilds), keeps the pre-rebuild handle to prove the
        -- replacement is genuinely a different element, and counts the
        -- restored control's own callback to prove restoration never
        -- re-fires it (requirement 5).
        it "settings menu Defaults restores focus onto the rebuilt Defaults button, running the action once" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootSettings 1280 720 <> ";"
                , "UI.showPage(m.page);"
                , "local button = require('scripts.ui.button');"
                , "local before = button.getElementHandle(m.defaultsButtonId);"
                , "UI.setControlFocus(before);"
                , "local hadFocusBefore = UI.hasControlFocus(before);"
                , "local runs = 0;"
                , "local realOnDefaults = m.onDefaults;"
                , "m.onDefaults = function(...) runs = runs + 1; return realOnDefaults(...) end;"
                -- The button's own onClick closure indexes
                -- settingsMenu.onDefaults at CALL time, so the counter
                -- above sees the real activation path, not a stub.
                , "button.handleClickByElement(before);"
                , "m.onDefaults = realOnDefaults;"
                , "local after = button.getElementHandle(m.defaultsButtonId);"
                , focusReportExpr "before" "after" "runs"
                ]
            expectRestoredFocus r defaultsBoxName (Just 1)

        it "create-world menu Defaults restores focus onto the rebuilt Defaults button, running the action once" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootCreateWorld 1280 720 <> ";"
                , "UI.showPage(m.page);"
                , "local button = require('scripts.ui.button');"
                , "local before = button.getElementHandle(m.defaultsButtonId);"
                , "UI.setControlFocus(before);"
                , "local hadFocusBefore = UI.hasControlFocus(before);"
                , "local runs = 0;"
                , "local realOnDefaults = m.onDefaults;"
                , "m.onDefaults = function(...) runs = runs + 1; return realOnDefaults(...) end;"
                , "button.handleClickByElement(before);"
                , "m.onDefaults = realOnDefaults;"
                , "local after = button.getElementHandle(m.defaultsButtonId);"
                , focusReportExpr "before" "after" "runs"
                ]
            expectRestoredFocus r defaultsBoxName (Just 1)

        it "create-world menu preview arrival restores focus onto the rebuilt control, firing no callback" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootCreateWorld 1280 720 <> ";"
                , "UI.showPage(m.page);"
                , "local button = require('scripts.ui.button');"
                , "local before = button.getElementHandle(m.backButtonId);"
                , "UI.setControlFocus(before);"
                , "local hadFocusBefore = UI.hasControlFocus(before);"
                -- Replaced outright (not wrapped): the restored Back
                -- button's callback must not run at all here, and
                -- letting the real one through would tear the screen
                -- down.
                , "local runs = 0;"
                , "m.onBack = function() runs = runs + 1 end;"
                , "m.onWorldPreviewReady(99);"
                , "local after = button.getElementHandle(m.backButtonId);"
                , focusReportExpr "before" "after" "runs"
                ]
            expectRestoredFocus r "back_btn_box" (Just 0)

        -- Requirement 7: an unfocused rebuild must stay unfocused --
        -- restoring by name off a stale or arbitrary handle would show
        -- up here rather than in the positive cases above.
        it "settings menu Defaults focuses nothing when nothing held control focus" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootSettings 1280 720 <> ";"
                , "UI.showPage(m.page);"
                , "local focusBeforeNil = (UI.getControlFocus() == nil);"
                , "m.onDefaults();"
                , nilFocusReportExpr
                ]
            expectNoRestoredFocus r

        it "create-world menu Defaults focuses nothing when nothing held control focus" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootCreateWorld 1280 720 <> ";"
                , "UI.showPage(m.page);"
                , "local focusBeforeNil = (UI.getControlFocus() == nil);"
                , "m.onDefaults();"
                , nilFocusReportExpr
                ]
            expectNoRestoredFocus r

        it "create-world menu preview arrival focuses nothing when nothing held control focus" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootCreateWorld 1280 720 <> ";"
                , "UI.showPage(m.page);"
                , "local focusBeforeNil = (UI.getControlFocus() == nil);"
                , "m.onWorldPreviewReady(99);"
                , nilFocusReportExpr
                ]
            expectNoRestoredFocus r

        -- Requirement 4 for the Create World side: preserveState=false
        -- scopes to in-progress EDIT values, and adding the focus
        -- restore above must not start preserving them. Plate Count is
        -- the sharpest probe -- it only syncs into `pending` at Generate
        -- time, so its raw box text is exactly the kind of unsubmitted
        -- edit the resize rebuild DOES keep (see the textbox cases
        -- earlier in this file) and Defaults must not.
        it "create-world menu Defaults still discards the pending value and the raw unsubmitted textbox edit" $ \env → do
            ls ← newBareLuaBackend env
            r ← evalJSON ls $ luaLines
                [ bootCreateWorld 1280 720 <> ";"
                , "UI.showPage(m.page);"
                , "local textbox = require('scripts.ui.textbox');"
                , "local advanced = require('scripts.create_world.advanced_tab');"
                , "m.pending.plateCount = '7';"
                , "textbox.setText(advanced.plateCountTextBoxId, '77');"
                , "local pendingBefore = m.pending.plateCount;"
                , "local rawBefore = textbox.getText(advanced.plateCountTextBoxId);"
                , "m.onDefaults();"
                , "return {pendingBefore=pendingBefore, pendingAfter=m.pending.plateCount,"
                    <> " rawBefore=rawBefore,"
                    <> " rawAfter=textbox.getText(advanced.plateCountTextBoxId)}"
                ]
            case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe DefaultsResetProbe of
                Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
                Just p → do
                    drpPendingBefore p `shouldBe` "7"
                    drpRawBefore p `shouldBe` "77"
                    drpPendingAfter p `shouldBe` "10"
                    drpRawAfter p `shouldBe` "10"

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

-- | The element name a Defaults button's control focus actually
--   carries: 'scripts/ui/button.lua' builds its clickable box as
--   @\<name\>_box@, and that box is both what holds keyboard control
--   focus and what @responsive.restoreControlFocusName@ matches on. The
--   two Defaults buttons declare @name = "defaults_btn"@
--   ('scripts/settings_menu.lua', 'scripts/create_world/bottom_buttons.lua').
defaultsBoxName ∷ Text
defaultsBoxName = "defaults_btn_box"

-- | #1671 rebuild-focus report: the pre-rebuild handle @beforeVar@, the
--   same-named replacement handle @afterVar@ read back after the
--   rebuild, and @runsVar@ -- how many times the focused control's own
--   callback fired. Reporting the CURRENTLY focused element's name
--   (rather than only a boolean) is what makes "restored by name onto
--   the replacement" observable instead of inferred.
focusReportExpr ∷ Text → Text → Text → Text
focusReportExpr beforeVar afterVar runsVar = luaLines
    [ "local focused = UI.getControlFocus();"
    , "local focusedInfo = focused and UI.getElementInfo(focused);"
    , "return {hadFocusBefore=hadFocusBefore,"
        <> " handleChanged=(" <> afterVar <> " ~= " <> beforeVar <> "),"
        <> " hasFocusAfter=UI.hasControlFocus(" <> afterVar <> "),"
        <> " focusedName=(focusedInfo and focusedInfo.name or ''),"
        <> " actionRuns=" <> runsVar <> "}"
    ]

-- | #1671 requirement 7's report: control focus was nil before the
--   rebuild and must still be nil after it.
nilFocusReportExpr ∷ Text
nilFocusReportExpr =
    "return {focusBeforeNil=focusBeforeNil, focusAfterNil=(UI.getControlFocus() == nil)}"

-- | Assert a #1671 positive case: focus was held before, the rebuild
--   really did replace the element, and control focus now sits on the
--   replacement carrying the expected name -- having fired the
--   control's own callback exactly @runs@ times.
expectRestoredFocus ∷ Text → Text → Maybe Int → Expectation
expectRestoredFocus r expectedName runs =
    case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe RebuildFocusProbe of
        Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
        Just p → do
            rfpHadFocusBefore p `shouldBe` True
            rfpHandleChanged p `shouldBe` True
            rfpHasFocusAfter p `shouldBe` True
            rfpFocusedName p `shouldBe` expectedName
            maybe (pure ()) (rfpActionRuns p `shouldBe`) runs

-- | Assert a #1671 requirement-7 case: nothing focused before, nothing
--   focused after.
expectNoRestoredFocus ∷ Text → Expectation
expectNoRestoredFocus r =
    case decode (BL.fromStrict (TE.encodeUtf8 r)) ∷ Maybe NilFocusProbe of
        Nothing → expectationFailure ("failed to decode: " ⧺ T.unpack r)
        Just p → do
            nfpFocusBeforeNil p `shouldBe` True
            nfpFocusAfterNil p `shouldBe` True

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

-- #1671 decode targets

data RebuildFocusProbe = RebuildFocusProbe
    { rfpHadFocusBefore ∷ Bool, rfpHandleChanged ∷ Bool
    , rfpHasFocusAfter ∷ Bool, rfpFocusedName ∷ Text
    , rfpActionRuns ∷ Int
    } deriving Show
instance FromJSON RebuildFocusProbe where
    parseJSON = withObject "RebuildFocusProbe" $ \o → RebuildFocusProbe
        <$> o .: "hadFocusBefore" <*> o .: "handleChanged"
        <*> o .: "hasFocusAfter" <*> o .: "focusedName"
        <*> o .: "actionRuns"

data NilFocusProbe = NilFocusProbe
    { nfpFocusBeforeNil ∷ Bool, nfpFocusAfterNil ∷ Bool } deriving Show
instance FromJSON NilFocusProbe where
    parseJSON = withObject "NilFocusProbe" $ \o →
        NilFocusProbe <$> o .: "focusBeforeNil" <*> o .: "focusAfterNil"

data DefaultsResetProbe = DefaultsResetProbe
    { drpPendingBefore ∷ Text, drpPendingAfter ∷ Text
    , drpRawBefore ∷ Text, drpRawAfter ∷ Text } deriving Show
instance FromJSON DefaultsResetProbe where
    parseJSON = withObject "DefaultsResetProbe" $ \o → DefaultsResetProbe
        <$> o .: "pendingBefore" <*> o .: "pendingAfter"
        <*> o .: "rawBefore" <*> o .: "rawAfter"

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

-- * #1959: debug-console horizontal fit

-- | scripts/ui/responsive.lua's own band table, read out of the module
--   instead of restated here — requirement 5's "derive the combinations
--   from responsive.bands" is what keeps this gate in step with the
--   envelope it is testing.
data BandRow = BandRow
    { brMinH ∷ Int, brMaxH ∷ Int, brMinScale ∷ Double, brMaxScale ∷ Double }
    deriving Show
instance FromJSON BandRow where
    parseJSON = withObject "BandRow" $ \o → BandRow
        <$> o .: "minH" <*> o .: "maxH" <*> o .: "minScale" <*> o .: "maxScale"

data EnvelopeProbe = EnvelopeProbe { epMinWidth ∷ Int, epBands ∷ [BandRow] }
    deriving Show
instance FromJSON EnvelopeProbe where
    parseJSON = withObject "EnvelopeProbe" $ \o →
        EnvelopeProbe <$> o .: "minWidth" <*> o .: "bands"

-- | One observation of the RENDERED console: the nine @shell_*@ box
--   sprites as the engine's own element tree reports them, beside the
--   four widths the shell derives from the same framebuffer. Keeping both
--   in one probe is what lets a case assert they agree.
data ShellBoxProbe = ShellBoxProbe
    { sbpSupported ∷ Bool
    , sbpCount ∷ Int
    , sbpMinX ∷ Double, sbpMaxX ∷ Double
    , sbpMinWidth ∷ Double, sbpCenterWidth ∷ Double, sbpCornerWidth ∷ Double
    , sbpReportedWidth ∷ Double, sbpInputWidth ∷ Double
    , sbpHistoryWidth ∷ Double, sbpResultWidth ∷ Double
    , sbpMinPaintKey ∷ Int, sbpAllInScope ∷ Bool, sbpInputBlocked ∷ Bool
    } deriving Show
instance FromJSON ShellBoxProbe where
    parseJSON = withObject "ShellBoxProbe" $ \o → ShellBoxProbe
        <$> o .: "supported" <*> o .: "count"
        <*> o .: "minX" <*> o .: "maxX"
        <*> o .: "minWidth" <*> o .: "centerWidth" <*> o .: "cornerWidth"
        <*> o .: "reportedWidth" <*> o .: "inputWidth"
        <*> o .: "historyWidth" <*> o .: "resultWidth"
        <*> o .: "minPaintKey" <*> o .: "allInScope" <*> o .: "inputBlocked"

-- | What the console actually DREW into its fitted center, measured with
--   'fixedCharMetrics' so a line's own byte length is its width.
data ShellContentProbe = ShellContentProbe
    { scpCenter ∷ Double, scpHistoryWidth ∷ Double, scpResultWidth ∷ Double
    , scpInputWidth ∷ Double, scpCmdLines ∷ Int, scpResLines ∷ Int
    , scpAllFit ∷ Bool, scpVisibleWidth ∷ Double, scpBoxRight ∷ Double
    , scpGhostSeen ∷ Bool, scpGhostFits ∷ Bool
    } deriving Show
instance FromJSON ShellContentProbe where
    parseJSON = withObject "ShellContentProbe" $ \o → ShellContentProbe
        <$> o .: "center" <*> o .: "historyWidth" <*> o .: "resultWidth"
        <*> o .: "inputWidth" <*> o .: "cmdLines" <*> o .: "resLines"
        <*> o .: "allFit" <*> o .: "visibleWidth" <*> o .: "boxRight"
        <*> o .: "ghostSeen" <*> o .: "ghostFits"

-- | The state a width rebuild must carry across (raw buffer, cursor,
--   scroll, focus, rendered history) beside the display facts that must
--   reflow to the new width.
data ShellStateProbe = ShellStateProbe
    { sspInput ∷ Text, sspCursor ∷ Int, sspScroll ∷ Int
    , sspFocus ∷ Maybe Int, sspShellFocusId ∷ Int
    , sspHistoryJoined ∷ Text, sspLineCount ∷ Int, sspAllFit ∷ Bool
    , sspContentWidth ∷ Double, sspInputWidth ∷ Double
    , sspVisibleChars ∷ Int, sspVisibleText ∷ Text
    , sspVisibleWidth ∷ Double, sspBoxRight ∷ Double
    , sspBufferText ∷ Text, sspBufferX ∷ Double, sspBufferWidth ∷ Double
    , sspCursorX ∷ Double
    , sspGhostSeen ∷ Bool, sspGhostText ∷ Text, sspGhostRight ∷ Double
    } deriving Show
instance FromJSON ShellStateProbe where
    parseJSON = withObject "ShellStateProbe" $ \o → ShellStateProbe
        <$> o .: "input" <*> o .: "cursor" <*> o .: "scroll"
        <*> o .:? "focus" <*> o .: "shellFocusId"
        <*> o .: "historyJoined" <*> o .: "lineCount" <*> o .: "allFit"
        <*> o .: "contentWidth" <*> o .: "inputWidth"
        <*> o .: "visibleChars" <*> o .: "visibleText"
        <*> o .: "visibleWidth" <*> o .: "boxRight"
        <*> o .: "bufferText" <*> o .: "bufferX" <*> o .: "bufferWidth"
        <*> o .: "cursorX"
        <*> o .: "ghostSeen" <*> o .: "ghostText" <*> o .: "ghostRight"

-- | Decode one JSON probe result, failing the example with the raw text
--   when it does not parse.
decodeProbe ∷ FromJSON α ⇒ String → Text → IO α
decodeProbe what r =
    maybe (fail ("failed to decode " ⧺ what ⧺ ": " ⧺ T.unpack r)) pure
          (decode (BL.fromStrict (TE.encodeUtf8 r)))

-- | The framebuffer size @engine.getFramebufferSize@ reports, which is
--   what the shell's geometry reads. Written on the engine's own IORef
--   rather than stubbed in Lua, so these cases exercise the production
--   query rather than a fixture standing in for it.
setFramebuffer ∷ EngineEnv → (Int, Int) → IO ()
setFramebuffer env = writeIORef (framebufferSizeRef env)

-- | Requirement 4's preferred center width, as the issue review corrected
--   it: @floor(1200 * uiscale)@ — the base constant scaled, never a
--   literal 1200 device pixels.
preferredCenter ∷ Double → Double
preferredCenter sc = fromIntegral (floor (1200 * sc) ∷ Int)

-- | Fixed pixels per BYTE, independent of the font size the shell scaled
--   to — so a case can compare a rendered line against its budget from
--   the line's own length. The bare backend's real @engine.getTextWidth@
--   measures 0 headless (no font atlas without a GPU), which would make
--   every width-driven rule pass vacuously.
fixedCharMetrics ∷ Int → Text
fixedCharMetrics px =
    "engine.getTextWidth = function(_, text, _) return #text * "
        <> tshow px <> " end;"

-- | The per-byte width 'fixedCharMetrics' charges in the two content
--   cases. Small enough that a few hundred characters overflow every
--   fitted center they are measured against.
contentPx ∷ Int
contentPx = 8

-- | Read the supported envelope out of scripts/ui/responsive.lua.
envelopeExpr ∷ Text
envelopeExpr = luaLines
    [ "local r = require('scripts.ui.responsive');"
    , "local bands = {};"
    , "for _, b in ipairs(r.bands) do"
    , "  bands[#bands+1] = {minH=b.minH, maxH=b.maxH,"
    , "                     minScale=b.minScale, maxScale=b.maxScale};"
    , "end;"
    , "return {minWidth=r.MIN_WIDTH, bands=bands}"
    ]

-- | Bring the real shell up, visible, on a bare backend.
shellBootExpr ∷ Text
shellBootExpr = luaLines
    [ "local shell = require('scripts.shell');"
    , "shell.init(0);"
    , "shell.show();"
    , "return true"
    ]

-- | The nine box sprites plus the shell's own reported widths, as one
--   table. An empty scan reports -1 rather than nil, so a filter that
--   silently matched nothing fails the bounds checks instead of the
--   decode.
shellBoxProbe ∷ Text
shellBoxProbe = luaLines
    [ "(function()"
    , "local shell = require('scripts.shell');"
    , "local box = {shell_nw=true, shell_n=true, shell_ne=true,"
    , "             shell_w=true,  shell_c=true, shell_e=true,"
    , "             shell_sw=true, shell_s=true, shell_se=true};"
    , "local count, minX, maxX, minW, minKey = 0, nil, nil, nil, nil;"
    , "local centerW, cornerW, inScope = nil, nil, true;"
    , "for _, e in ipairs(UI.getVisibleElements()) do"
    , "  if e.page == 'shell' and box[e.name] then"
    , "    count = count + 1;"
    , "    if minX == nil or e.x < minX then minX = e.x end;"
    , "    if maxX == nil or (e.x + e.width) > maxX then maxX = e.x + e.width end;"
    , "    if minW == nil or e.width < minW then minW = e.width end;"
    , "    if minKey == nil or e.paintKey < minKey then minKey = e.paintKey end;"
    , "    if not e.inScope then inScope = false end;"
    , "    if e.name == 'shell_c' then centerW = e.width end;"
    , "    if e.name == 'shell_nw' then cornerW = e.width end;"
    , "  end;"
    , "end;"
    , "return {count=count, minX=minX or -1, maxX=maxX or -1,"
    , "        minWidth=minW or -1, centerWidth=centerW or -1,"
    , "        cornerWidth=cornerW or -1,"
    , "        minPaintKey=minKey or -1, allInScope=inScope,"
    , "        reportedWidth=shell.getContentWidth(),"
    , "        inputWidth=shell.getMaxInputWidth(),"
    , "        historyWidth=shell.getHistoryTextWidth(),"
    , "        resultWidth=shell.getResultTextWidth(),"
    , "        inputBlocked=UI.isInputBlocked()}"
    , "end)()"
    ]

-- | Apply one (width, height, scale) through the SAME entry point the
--   engine uses — @shell.onFramebufferResize@, after the framebuffer ref
--   has been written — then report the rendered box and how the envelope
--   classifies the combination.
shellFitExpr ∷ Int → Int → Double → Text
shellFitExpr w h sc = luaLines
    [ "engine.setUIScale(" <> tshow sc <> ");"
    , "require('scripts.shell').onFramebufferResize("
        <> tshow w <> ", " <> tshow h <> ");"
    , "local p = " <> shellBoxProbe <> ";"
    , "p.supported = require('scripts.ui.responsive').classify("
        <> tshow w <> ", " <> tshow h <> ", " <> tshow sc <> ").supported;"
    , "return p"
    ]

-- | One supported combination: rebuild against that framebuffer, then
--   report one message per violated requirement-1 fact (none when the box
--   fits).
checkFit ∷ EngineEnv → LuaBackendState → (Int, Int, Double) → IO [String]
checkFit env ls (w, h, sc) = do
    setFramebuffer env (w, h)
    p ← decodeProbe "shell box" =≪ evalJSON ls (shellFitExpr w h sc)
    let at = show w ⧺ "x" ⧺ show h ⧺ " @" ⧺ show sc ⧺ "x"
    pure $ concat
        [ [ at ⧺ ": the envelope does not classify this as supported"
          | not (sbpSupported p) ]
        , [ at ⧺ ": expected 9 box sprites, found " ⧺ show (sbpCount p)
          | sbpCount p ≢ 9 ]
        , [ at ⧺ ": left edge " ⧺ show (sbpMinX p) ⧺ " is before 0"
          | sbpMinX p < 0 ]
        , [ at ⧺ ": right edge " ⧺ show (sbpMaxX p) ⧺ " is past " ⧺ show w
          | sbpMaxX p > fromIntegral w ]
        , [ at ⧺ ": non-positive sprite width " ⧺ show (sbpMinWidth p)
          | sbpMinWidth p ≤ 0 ]
        , [ at ⧺ ": center " ⧺ show (sbpCenterWidth p)
              ⧺ " is neither the preferred " ⧺ show (preferredCenter sc)
              ⧺ " nor the full fitted width"
          | sbpCenterWidth p ≢ preferredCenter sc
            ∧ sbpMaxX p ≢ fromIntegral w ]
        , [ at ⧺ ": the shell reports a " ⧺ show (sbpReportedWidth p)
              ⧺ " center but drew a " ⧺ show (sbpCenterWidth p) ⧺ " one"
          | sbpReportedWidth p ≢ sbpCenterWidth p ]
        ]

-- | The horizontal quantities requirement 3 forbids from ever going
--   non-positive, however far out of envelope the framebuffer is.
positiveWidths ∷ ShellBoxProbe → [Double]
positiveWidths p =
    [ sbpMinWidth p, sbpCenterWidth p, sbpInputWidth p
    , sbpHistoryWidth p, sbpResultWidth p ]

-- | Fill the console with history that must wrap and an input line that
--   must scroll, then report whether anything it renders overruns the
--   fitted center — history lines, the visible input, and the completion
--   ghost that trails it.
shellContentExpr ∷ Int → Int → Int → Double → Text
shellContentExpr px w h sc = luaLines
    [ "engine.setUIScale(" <> tshow sc <> ");"
    , "local shell = require('scripts.shell');"
    , "shell.onFramebufferResize(" <> tshow w <> ", " <> tshow h <> ");"
    , "local fid = shell.getFocusId();"
    , "shell.addHistory(string.rep('c', 300), string.rep('r', 300), false);"
    , "for _ = 1, 200 do shell.onCharInput(fid, 'i') end;"
    , "local hw, rw = shell.getHistoryTextWidth(), shell.getResultTextWidth();"
    , "local right = 0;"
    , "for _, e in ipairs(UI.getVisibleElements()) do"
    , "  if e.page == 'shell' and e.name == 'shell_se' then"
    , "    right = e.x + e.width;"
    , "  end;"
    , "end;"
    , "local cmdLines, resLines, allFit = 0, 0, true;"
    , "for _, e in ipairs(UI.getVisibleElements()) do"
    , "  if e.page == 'shell' and e.text then"
    , "    local width = #e.text * " <> tshow px <> ";"
    , "    if e.name:match('^shell_cmd_%d+_%d+$') then"
    , "      cmdLines = cmdLines + 1;"
    , "      if width > hw or (e.x + width) > right then allFit = false end;"
    , "    elseif e.name:match('^shell_result_%d+_%d+$') then"
    , "      resLines = resLines + 1;"
    , "      if width > rw or (e.x + width) > right then allFit = false end;"
    , "    end;"
    , "  end;"
    , "end;"
    , "local visibleWidth = #shell.getVisibleInput() * " <> tshow px <> ";"
    -- The ghost hint rides the same fitted budget: clear the line, type a
    -- prefix exactly one SANDBOX name answers, and see where the hint lands.
    -- shellSandbox, not _G: that is the environment the console completes
    -- from and executes in (#1958).
    , "shell.onInterrupt(fid);"
    , "shellSandbox.zzShellFitCompletionTarget = 1;"
    , "for _, c in ipairs({'z','z','S','h','e','l','l'}) do"
    , "  shell.onCharInput(fid, c);"
    , "end;"
    , "local ghostSeen, ghostFits = false, true;"
    , "for _, e in ipairs(UI.getVisibleElements()) do"
    , "  if e.page == 'shell' and e.name == 'shell_ghost'"
    , "     and e.visible and e.text then"
    , "    ghostSeen = true;"
    , "    if (e.x + #e.text * " <> tshow px <> ") > right then"
    , "      ghostFits = false;"
    , "    end;"
    , "  end;"
    , "end;"
    , "return {center=shell.getContentWidth(), historyWidth=hw,"
    , "        resultWidth=rw, inputWidth=shell.getMaxInputWidth(),"
    , "        cmdLines=cmdLines, resLines=resLines, allFit=allFit,"
    , "        visibleWidth=visibleWidth, boxRight=right,"
    , "        ghostSeen=ghostSeen, ghostFits=ghostFits}"
    ]

-- | The raw editing state, the focus, and the rendered history joined
--   back into one string. @textWrap.byCharacter@'s lines always
--   concatenate back to their input, so this join is invariant across a
--   rewrap — which is exactly what makes it a preservation assertion
--   rather than a restatement of the current wrapping.
shellStateProbe ∷ Int → Text
shellStateProbe px = luaLines
    [ "(function()"
    , "local shell = require('scripts.shell');"
    , "local text, cursor, scroll = shell.getInputState();"
    , "local hw, rw = shell.getHistoryTextWidth(), shell.getResultTextWidth();"
    , "local right = 0;"
    , "for _, e in ipairs(UI.getVisibleElements()) do"
    , "  if e.page == 'shell' and e.name == 'shell_se' then"
    , "    right = e.x + e.width;"
    , "  end;"
    , "end;"
    , "local parts, allFit = {}, true;"
    , "for _, e in ipairs(UI.getVisibleElements()) do"
    , "  if e.page == 'shell' and e.text then"
    , "    local isCmd = true;"
    , "    local i, j = e.name:match('^shell_cmd_(%d+)_(%d+)$');"
    , "    if not i then"
    , "      isCmd = false;"
    , "      i, j = e.name:match('^shell_result_(%d+)_(%d+)$');"
    , "    end;"
    , "    if i then"
    , "      local width = #e.text * " <> tshow px <> ";"
    , "      local budget = isCmd and hw or rw;"
    , "      if width > budget or (e.x + width) > right then allFit = false end;"
    , "      parts[#parts+1] = {i=tonumber(i), j=tonumber(j),"
    , "                         cmd=isCmd, t=e.text};"
    , "    end;"
    , "  end;"
    , "end;"
    , "table.sort(parts, function(a, b)"
    , "  if a.cmd ~= b.cmd then return a.cmd end;"
    , "  if a.i ~= b.i then return a.i < b.i end;"
    , "  return a.j < b.j;"
    , "end);"
    , "local joined = '';"
    , "for _, q in ipairs(parts) do joined = joined .. q.t end;"
    , "local visible = shell.getVisibleInput();"
    -- The three elements the resize path recreates from scratch: what the
    -- console actually DRAWS for the input line, its cursor and its
    -- completion hint. Reading getVisibleInput() alone would report the
    -- value the display is supposed to show rather than the one it does.
    , "local bufferText, bufferX = nil, -1;"
    , "local cursorX, ghostText, ghostX = -1, nil, -1;"
    , "for _, e in ipairs(UI.getVisibleElements()) do"
    , "  if e.page == 'shell' then"
    , "    if e.name == 'shell_buffer' then"
    , "      bufferText = e.text or ''; bufferX = e.x;"
    , "    elseif e.name == 'shell_cursor' then"
    , "      cursorX = e.x;"
    , "    elseif e.name == 'shell_ghost' and e.visible then"
    , "      ghostText = e.text or ''; ghostX = e.x;"
    , "    end;"
    , "  end;"
    , "end;"
    , "return {input=text, cursor=cursor, scroll=scroll,"
    , "        focus=engine.getFocusId(), shellFocusId=shell.getFocusId(),"
    , "        historyJoined=joined, lineCount=#parts, allFit=allFit,"
    , "        contentWidth=shell.getContentWidth(),"
    , "        inputWidth=shell.getMaxInputWidth(),"
    , "        visibleChars=utf8.len(visible), visibleText=visible,"
    , "        visibleWidth=#visible * " <> tshow px <> ", boxRight=right,"
    , "        bufferText=bufferText or '', bufferX=bufferX,"
    , "        bufferWidth=#(bufferText or '') * " <> tshow px <> ","
    , "        cursorX=cursorX, ghostSeen=(ghostText ~= nil),"
    , "        ghostText=ghostText or '',"
    , "        ghostRight=(ghostText and (ghostX + #ghostText * "
        <> tshow px <> ") or -1)}"
    , "end)()"
    ]

-- | Seed the console with wrapping history and a scrolled input line at
--   whatever framebuffer is current, then report its state.
--
--   The 600-character entry is sized against BOTH framebuffers the
--   preservation case uses: it wraps to five lines each at the preferred
--   1200px center and six at the fitted 856px one (so the reflow is
--   visible in the line count), and twelve lines still clear the shorter
--   framebuffer's own height budget, so no line is dropped on either side
--   and the joined history stays comparable.
shellSeedExpr ∷ Int → Text
shellSeedExpr px = luaLines
    [ "local shell = require('scripts.shell');"
    , "local fid = shell.getFocusId();"
    , "shell.addHistory(string.rep('c', 600), string.rep('r', 600), false);"
    , "for _ = 1, 300 do shell.onCharInput(fid, 'i') end;"
    , "return " <> shellStateProbe px
    ]

-- | Reopen the console and report the box it rebuilt, classified at
--   whatever UI scale is now live.
shellReopenExpr ∷ Int → Int → Text
shellReopenExpr w h = luaLines
    [ "require('scripts.shell').show();"
    , "local p = " <> shellBoxProbe <> ";"
    , "p.supported = require('scripts.ui.responsive').classify("
        <> tshow w <> ", " <> tshow h <> ", engine.getUIScale()).supported;"
    , "return p"
    ]

-- | Per-byte width for the input-row case. Large enough that the prompt
--   alone is as wide as the narrowest supported band's fitted center,
--   which is the combination that exposed a budget measured from the
--   center rather than from where the input actually starts.
promptPx ∷ Int
promptPx = 64

-- | Where the prompt, input line, cursor and completion ghost landed
--   relative to the fitted center's own interior — the two edge tiles'
--   inner faces, read off @shell_nw@ and @shell_ne@ rather than
--   recomputed.
data InputRowProbe = InputRowProbe
    { irpSupported ∷ Bool
    , irpInteriorLeft ∷ Double, irpInteriorRight ∷ Double
    , irpInputWidth ∷ Double
    , irpPromptVisible ∷ Bool, irpPromptX ∷ Double, irpPromptWidth ∷ Double
    , irpBufferX ∷ Double, irpBufferWidth ∷ Double
    , irpCursorCenter ∷ Double
    , irpGhostVisible ∷ Bool, irpGhostX ∷ Double, irpGhostRight ∷ Double
    } deriving Show
instance FromJSON InputRowProbe where
    parseJSON = withObject "InputRowProbe" $ \o → InputRowProbe
        <$> o .: "supported"
        <*> o .: "interiorLeft" <*> o .: "interiorRight"
        <*> o .: "inputWidth"
        <*> o .: "promptVisible" <*> o .: "promptX" <*> o .: "promptWidth"
        <*> o .: "bufferX" <*> o .: "bufferWidth"
        <*> o .: "cursorCenter"
        <*> o .: "ghostVisible" <*> o .: "ghostX" <*> o .: "ghostRight"

-- | Rebuild at one supported combination with a short completable input,
--   then report every violated in-bounds fact for the input row, plus
--   whether the prompt survived at that width.
inputRowAt ∷ EngineEnv → LuaBackendState → (Int, Int, Double) → IO ([String], Bool)
inputRowAt env ls (w, h, sc) = do
    setFramebuffer env (w, h)
    p ← decodeProbe "input row" =≪ evalJSON ls (shellInputRowExpr promptPx w h sc)
    let at = show w ⧺ "x" ⧺ show h ⧺ " @" ⧺ show sc ⧺ "x"
        left = irpInteriorLeft p
        right = irpInteriorRight p
        inside what x wide =
            [ at ⧺ ": " ⧺ what ⧺ " spans " ⧺ show x ⧺ ".." ⧺ show (x + wide)
                ⧺ ", outside the fitted center " ⧺ show left ⧺ ".." ⧺ show right
            | x < left ∨ (x + wide) > right ]
    pure ( concat
             [ [ at ⧺ ": the envelope does not classify this as supported"
               | not (irpSupported p) ]
             , [ at ⧺ ": non-positive input budget " ⧺ show (irpInputWidth p)
               | irpInputWidth p ≤ 0 ]
             , [ at ⧺ ": nothing rendered on the input line"
               | irpBufferWidth p ≤ 0 ]
             , if irpPromptVisible p
                 then inside "the prompt" (irpPromptX p) (irpPromptWidth p)
                 else []
             , inside "the input line" (irpBufferX p) (irpBufferWidth p)
             -- The caret glyph is drawn CENTRED on the insertion point, so
             -- half of it legitimately overhangs at either end of the
             -- field; the point itself is what must stay in the center.
             , inside "the cursor" (irpCursorCenter p) 0
             , if irpGhostVisible p
                 then inside "the completion ghost" (irpGhostX p)
                          (irpGhostRight p - irpGhostX p)
                 else []
             ]
         , irpPromptVisible p )

-- | Rebuild at (w, h, scale), type a two-character prefix exactly one
--   global answers, and report the input row's geometry.
shellInputRowExpr ∷ Int → Int → Int → Double → Text
shellInputRowExpr px w h sc = luaLines
    [ "engine.setUIScale(" <> tshow sc <> ");"
    , "local shell = require('scripts.shell');"
    , "shell.onFramebufferResize(" <> tshow w <> ", " <> tshow h <> ");"
    , "local fid = shell.getFocusId();"
    , "shell.onInterrupt(fid);"
    , "_G.zzq = 1;"
    , "shell.onCharInput(fid, 'z'); shell.onCharInput(fid, 'z');"
    , "local left, right = -1, -1;"
    , "local promptVisible, promptX, promptText = false, -1, '';"
    , "local bufferX, bufferText = -1, '';"
    , "local cursorX, cursorText = -1, '';"
    , "local ghostVisible, ghostX, ghostText = false, -1, '';"
    , "for _, e in ipairs(UI.getVisibleElements()) do"
    , "  if e.page == 'shell' then"
    , "    if e.name == 'shell_nw' then left = e.x + e.width;"
    , "    elseif e.name == 'shell_ne' then right = e.x;"
    , "    elseif e.name == 'shell_prompt' then"
    , "      promptVisible = e.visible; promptX = e.x; promptText = e.text or '';"
    , "    elseif e.name == 'shell_buffer' then"
    , "      bufferX = e.x; bufferText = e.text or '';"
    , "    elseif e.name == 'shell_cursor' then"
    , "      cursorX = e.x; cursorText = e.text or '';"
    , "    elseif e.name == 'shell_ghost' then"
    , "      ghostVisible = e.visible; ghostX = e.x; ghostText = e.text or '';"
    , "    end;"
    , "  end;"
    , "end;"
    , "return {supported=require('scripts.ui.responsive').classify("
        <> tshow w <> ", " <> tshow h <> ", " <> tshow sc <> ").supported,"
    , "        interiorLeft=left, interiorRight=right,"
    , "        inputWidth=shell.getMaxInputWidth(),"
    , "        promptVisible=promptVisible, promptX=promptX,"
    , "        promptWidth=#promptText * " <> tshow px <> ","
    , "        bufferX=bufferX, bufferWidth=#bufferText * " <> tshow px <> ","
    , "        cursorCenter=cursorX + (#cursorText * " <> tshow px <> ") / 2,"
    , "        ghostVisible=ghostVisible, ghostX=ghostX,"
    , "        ghostRight=ghostX + #ghostText * " <> tshow px <> "}"
    ]

-- | Type a short prefix exactly one SANDBOX name answers, so the completion
--   ghost is live before a resize. The name goes in @shellSandbox@ and not
--   @_G@ because that is the environment the console both completes from and
--   executes in (#1958). Deliberately SHORT: a scrolled input
--   already fills the whole field, so no ghost can fit beside it, which is
--   why the ghost case cannot share the preservation case's seed.
shellGhostSeedExpr ∷ Int → Text
shellGhostSeedExpr px = luaLines
    [ "local shell = require('scripts.shell');"
    , "local fid = shell.getFocusId();"
    , "shellSandbox.zzShellGhostCompletionTarget = 1;"
    , "for _, c in ipairs({'z','z','S','h','e','l','l'}) do"
    , "  shell.onCharInput(fid, c);"
    , "end;"
    , "return " <> shellStateProbe px
    ]

-- | Deliver a framebuffer resize the way the engine does, then report the
--   same state again.
shellResizeExpr ∷ Int → Int → Int → Text
shellResizeExpr px w h = luaLines
    [ "require('scripts.shell').onFramebufferResize("
        <> tshow w <> ", " <> tshow h <> ");"
    , "return " <> shellStateProbe px
    ]

-- * Lua backend + eval helpers (mirrors Test.Headless.UI.InputOwnership)

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    -- Production order (Engine.Scripting.Lua.Thread.luaStartup): register
    -- the API, then build the console sandbox. Since #1958 that sandbox is
    -- also where scripts/shell.lua looks for completion candidates, so the
    -- two shell cases below that need a live ghost have nothing to complete
    -- against without it.
    setupShellSandbox (lbsLuaState ls)
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
