-- | #1959/#1325/#748 gate: the shell debug console's responsive
--   lifecycle — the half of the @UI.ResponsiveMenus@ hierarchy that
--   drives scripts/shell.lua rather than one of the six menu screens
--   'Test.Headless.UI.ResponsiveMenus' scopes itself to.
--
--   Three behavior groups live here:
--
--     * the shared resize/scale contract Settings fans out to the
--       console (#748 rounds 6, 7, 10 and 11);
--     * the console's horizontal fit against the framebuffer width,
--       and the input/cursor/ghost/history/focus state a width rebuild
--       must carry across (#1959);
--     * the console's single module identity per Lua state (#1325).
--
--   This module is NOT registered with Hspec on its own. 'spec' is a
--   'SpecWith' 'EngineEnv' that 'Test.Headless.UI.ResponsiveMenus'
--   composes into its own @around withMenusEngine@ block, so every case
--   below keeps exactly the fixture it had before the split: one fresh
--   headless engine per example, the isolated resource root, the
--   canonical 1x initial UI scale, and production-order Lua API plus
--   shell-sandbox initialization. The suite-level guards on that
--   fixture (config isolation — #1357, UI-scale baseline — #1266) sit in
--   the façade and therefore still cover these cases too.
--
--   #2027's per-action fan-out matrix lives in the first group here,
--   not in the menus owner: it drives the same Settings actions that
--   group already covered, and one of the three routes it counts is the
--   direct shell.onFramebufferResize call this module owns.
module Test.Headless.UI.ResponsiveMenus.DebugConsole (spec) where

import UPrelude
import Test.Hspec
import Data.Aeson (FromJSON(..), decode, withObject, (.:), (.:?))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.IORef (writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState, ScriptValue(..))
import Engine.Scripting.Lua.Util (broadcastToModules)
import Test.Headless.UI.ResponsiveMenus.Fixture
  ( luaLines, newBareLuaBackend, evalOk, evalBool, evalJSON
  , decodeProbe )

-- | The debug-console groups, composed by
--   'Test.Headless.UI.ResponsiveMenus' under its @around withMenusEngine@.
spec ∷ SpecWith EngineEnv
spec = do
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

        -- #2027: the four Settings actions that can change the live UI
        -- scale are four INDEPENDENT production branches -- onDefaults
        -- (scripts/settings_menu.lua:163, :186-192), onApply
        -- (:978-1003), onSave (:1010-1015) and onBack (:1026-1032) --
        -- each reaching the SAME three top-level routes:
        -- responsive.notifyResize, shell.onFramebufferResize and
        -- uiManager.notifyGameplayRescale. The round-7 and round-11
        -- cases this replaces watched only shell, exactly once for
        -- Apply but at "any positive count" for Defaults and Back, and
        -- never drove onSave at all -- so removing Save's fan-out, or
        -- delivering Defaults' or Back's twice, left CI green.
        --
        -- Every pair below therefore drives ONE production entry point
        -- and counts all three routes exactly: 1 each on a real scale
        -- change, 0 each when the scale does not move.
        --
        -- What this deliberately does NOT do is re-assert what
        -- uiManager.notifyGameplayRescale then reaches. Its recipient
        -- count, ordering, readiness gating and non-positive-size guard
        -- stay in 'Test.Headless.UI.ResponsiveGameplay.Lifecycle',
        -- which also keeps the one case driving the REAL (unstubbed)
        -- gameplay HUD through settingsMenu.onDefaults. This asserts
        -- ENTRY into that route and nothing past it.
        forM_ settingsScaleActions $ \act → do
            it (T.unpack (saName act) ⧺ " reaches responsive.notifyResize, shell.onFramebufferResize and uiManager.notifyGameplayRescale exactly once on a real scale change (" ⧺ T.unpack (saHow act) ⧺ ")") $ \env → do
                ls ← newBareLuaBackend env
                p  ← decodeProbe "scale fan-out"
                        =≪ evalJSON ls (scaleFanOutExpr act True)
                -- The scale genuinely moved, so a zero count below is a
                -- MISSING fan-out rather than an action that correctly
                -- declined to fan anything out.
                fopScaleAfter p `shouldSatisfy` (≢ fopScaleBefore p)
                actionReallyRan p
                fopResponsive p `shouldBe` 1
                fopShell p `shouldBe` 1
                fopGameplay p `shouldBe` 1

            it (T.unpack (saName act) ⧺ " reaches none of the three routes when the scale does not move") $ \env → do
                ls ← newBareLuaBackend env
                p  ← decodeProbe "scale fan-out"
                        =≪ evalJSON ls (scaleFanOutExpr act False)
                fopScaleAfter p `shouldBe` fopScaleBefore p
                -- Without this, three zeroes would ALSO be reported by
                -- an action that threw before its fan-out branch, or by
                -- a setup that never reached the action at all.
                actionReallyRan p
                fopResponsive p `shouldBe` 0
                fopShell p `shouldBe` 0
                fopGameplay p `shouldBe` 0

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
    -- display scripts/ui/responsive.lua's envelope declares supported
    -- ('Test.Headless.UI.ResponsiveMenus' owns that classification; the
    -- combinations below are read out of the same module). The shell queried
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

-- * JSON decode targets

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

-- * #2027: the Settings scale-change fan-out matrix

-- | What one Settings action reached, and the proof that the action
--   itself ran. @responsive@ / @shell@ / @gameplay@ are the three
--   top-level routes settings_menu.lua fans a scale change out to;
--   the scale pair says whether the setup really moved the UI scale;
--   the witness triple is requirement 3's positive witness (see
--   'actionReallyRan').
data ScaleFanOutProbe = ScaleFanOutProbe
    { fopResponsive      ∷ Int
    , fopShell           ∷ Int
    , fopGameplay        ∷ Int
    , fopScaleBefore     ∷ Double
    , fopScaleAfter      ∷ Double
    , fopWitnessBefore   ∷ Double
    , fopWitnessExpected ∷ Double
    , fopWitness         ∷ Double
    } deriving Show

instance FromJSON ScaleFanOutProbe where
    parseJSON = withObject "ScaleFanOutProbe" $ \o → ScaleFanOutProbe
        <$> o .: "responsive" <*> o .: "shell" <*> o .: "gameplay"
        <*> o .: "scaleBefore" <*> o .: "scaleAfter"
        <*> o .: "witnessBefore" <*> o .: "witnessExpected"
        <*> o .: "witness"

-- | Requirement 3's positive witness, asserted by BOTH halves of every
--   action's pair: a non-scale field the action commits unconditionally
--   moved from a value the setup deliberately made wrong to the value
--   the action owes it. An action that threw before its fan-out branch,
--   or a setup that never reached the action at all, would otherwise
--   satisfy the three zero counts of the unchanged-scale case exactly
--   as a correct no-op does.
actionReallyRan ∷ ScaleFanOutProbe → Expectation
actionReallyRan p = do
    -- Non-vacuity: the expected value has to be one the setup did not
    -- already leave in place, or "it arrived" proves nothing.
    fopWitnessExpected p `shouldSatisfy` (≢ fopWitnessBefore p)
    fopWitness p `shouldBe` fopWitnessExpected p

-- | One Settings action that can change the live UI scale, as the
--   #2027 matrix drives it.
--
--   Each action keeps PRODUCTION's own scale-change decision inside the
--   test — settings_menu.lua's before/after comparison for Defaults and
--   Back, data.apply's @scaleChanged@ result for Apply and Save — so a
--   setup establishes the inputs that decision reads and never replaces
--   the decision itself.
data ScaleAction = ScaleAction
    { saName  ∷ Text
      -- ^ The production entry point, as the case names state it.
    , saHow   ∷ Text
      -- ^ How the change is established, for the case name.
    , saSetup ∷ Bool → Text
      -- ^ Setup Lua, run after 'scaleFanOutBoot' and BEFORE the spies.
      --   'True' must establish a real scale change and 'False' an
      --   unchanged scale; both must leave @scaleBefore@,
      --   @witnessBefore@ and @witnessExpected@ bound as locals for
      --   'scaleFanOutProbe' to read.
    , saDrive ∷ Text
      -- ^ The action call, plus any teardown of what the setup stubbed.
    }

-- | The four actions. Apply and Save share one setup because
--   @data.save@ delegates to @data.apply@ (scripts/settings/data.lua:621)
--   and both derive @scaleChanged@ from the same Graphics-tab widget
--   value (:532-541).
settingsScaleActions ∷ [ScaleAction]
settingsScaleActions =
    [ widgetScaleAction "settingsMenu.onApply" "m.onApply();"
    , widgetScaleAction "settingsMenu.onSave"  "m.onSave();"
    , defaultsScaleAction
    , backScaleAction
    ]

-- | One case's complete Lua chunk. Every action is assembled from the
--   same five pieces in the same order, so no action can quietly
--   observe or assert less than the others.
scaleFanOutExpr ∷ ScaleAction → Bool → Text
scaleFanOutExpr act changed = luaLines
    [ scaleFanOutBoot
    , saSetup act changed
    , scaleFanOutSpies
    , saDrive act
    , scaleFanOutProbe
    ]

-- | The shared boot: the real settings screen on this example's own
--   fresh bare Lua state, at a framebuffer size no case changes (only
--   the SCALE moves here — that is the whole point of the #750/#748
--   scale-only path).
scaleFanOutBoot ∷ Text
scaleFanOutBoot = luaLines
    [ "local m = require('scripts.settings_menu');"
    , "m.init(1,2,3,1280,720);"
    , "local data = require('scripts.settings.data');"
    ]

-- | Count each of the three top-level routes.
--
--   The technique is forced by how settings_menu.lua captures its
--   dependencies: @responsive@ (scripts/settings_menu.lua:6), @data@
--   (:16) and @shell@ (:21) are module-level upvalues bound at load
--   time, so a spy must replace the FIELD on the already-required
--   module table. Swapping @package.loaded['scripts.ui.responsive']@ or
--   @['scripts.shell']@ after settings_menu has loaded would observe
--   nothing at all — failing the exactly-once cases confusingly and
--   passing the zero-call cases vacuously. @ui_manager@ is the
--   exception: settings_menu require()s it lazily at each call site
--   (:191, :1002, :1013, :1031), so the same field replacement reaches
--   it — and it is required only AFTER settings_menu is loaded, keeping
--   the load cycle that laziness exists to avoid (scripts.ui_manager →
--   ui_manager_boot → settings_menu) out of the fixture too.
--
--   responsive and shell call THROUGH to the real handler, so a
--   duplicated dispatch is counted against the same live rebuild the
--   #748 finding was about; notifyGameplayRescale is a pure counter,
--   because its recipient matrix belongs to
--   'Test.Headless.UI.ResponsiveGameplay.Lifecycle' (requirement 4).
scaleFanOutSpies ∷ Text
scaleFanOutSpies = luaLines
    [ "local responsive = require('scripts.ui.responsive');"
    , "local shell = require('scripts.shell');"
    , "local uiManager = require('scripts.ui_manager');"
    , "_G.__fanResponsive = 0; _G.__fanShell = 0; _G.__fanGameplay = 0;"
    , "local realNotifyResize = responsive.notifyResize;"
    , "responsive.notifyResize = function(w, h)"
    , "    _G.__fanResponsive = _G.__fanResponsive + 1;"
    , "    return realNotifyResize(w, h) end;"
    , "local realShellResize = shell.onFramebufferResize;"
    , "shell.onFramebufferResize = function(w, h)"
    , "    _G.__fanShell = _G.__fanShell + 1;"
    , "    return realShellResize(w, h) end;"
    , "uiManager.notifyGameplayRescale = function(w, h)"
    , "    _G.__fanGameplay = _G.__fanGameplay + 1 end;"
    ]

-- | The shared result. @scaleBefore@, @witnessBefore@ and
--   @witnessExpected@ are the locals each 'saSetup' bound;
--   @data.current.frameLimit@ is the witness field every action commits
--   unconditionally, independent of the UI scale.
scaleFanOutProbe ∷ Text
scaleFanOutProbe = luaLines
    [ "return {responsive = _G.__fanResponsive,"
    , "        shell = _G.__fanShell,"
    , "        gameplay = _G.__fanGameplay,"
    , "        scaleBefore = scaleBefore,"
    , "        scaleAfter = data.current.uiScale,"
    , "        witnessBefore = witnessBefore,"
    , "        witnessExpected = witnessExpected,"
    , "        witness = data.current.frameLimit}"
    ]

-- | Apply and Save: the change comes from the Graphics tab's uiScale
--   textbox, which is exactly where production reads it
--   (scripts/settings/graphics_tab.lua:790-792 → data.apply's
--   @widgetValues.uiScale@), so data.apply's own range check and
--   @data.current.uiScale ~= newScale@ comparison still decide whether
--   anything changed.
--
--   The witness is the frame-limit textbox, a NON-scale widget value
--   the same data.apply pass commits unconditionally
--   (scripts/settings/data.lua:564-573) — so both halves of the pair
--   prove the action reached data.apply at all.
widgetScaleAction ∷ Text → Text → ScaleAction
widgetScaleAction name call = ScaleAction
    { saName = name
    , saHow  = "the Graphics tab's uiScale textbox"
    , saSetup = \changed → luaLines
        [ "local graphicsTab = require('scripts.settings.graphics_tab');"
        , "local textbox = require('scripts.ui.textbox');"
        , "local scaleBefore = data.current.uiScale;"
        , if changed
            then "local scaleTarget = (scaleBefore >= 3.0) and 1.0 or (scaleBefore + 1.0);"
            else "local scaleTarget = scaleBefore;"
        , "textbox.setText(graphicsTab.uiScaleTextBoxId, tostring(scaleTarget));"
        , "local witnessBefore = data.current.frameLimit;"
        , "local witnessExpected = (witnessBefore == 120) and 90 or 120;"
        , "textbox.setText(graphicsTab.frameLimitTextBoxId, tostring(witnessExpected));"
        ]
    , saDrive = call
    }

-- | Defaults: the change comes from the factory config
--   @data.loadDefaults@ reads, so that module's own HiDPI
--   auto-adjustment, clamp and assignment all still run
--   (scripts/settings/data.lua:308-360) — only the engine call feeding
--   it is stubbed, the same way
--   'Test.Headless.UI.ResponsiveGameplay.Lifecycle' already stubs it.
--
--   1600x900 sits below that function's 1080p auto-scale threshold
--   (:319-332), so the returned uiScale reaches data.current
--   unmultiplied and the change is exactly the one this setup declares.
--   The witness is the same factory call's frame limit, which
--   data.loadDefaults assigns unconditionally (:346).
defaultsScaleAction ∷ ScaleAction
defaultsScaleAction = ScaleAction
    { saName = "settingsMenu.onDefaults"
    , saHow  = "the factory uiScale engine.loadDefaultConfig reports"
    , saSetup = \changed → luaLines
        [ "local scaleBefore = data.current.uiScale;"
        , if changed
            then "local scaleTarget = (scaleBefore >= 3.0) and 1.0 or (scaleBefore + 1.0);"
            else "local scaleTarget = scaleBefore;"
        , "local witnessBefore = data.current.frameLimit;"
        , "local witnessExpected = (witnessBefore == 120) and 90 or 120;"
        , "_G.__realLoadDefaultConfig = engine.loadDefaultConfig;"
        , "engine.loadDefaultConfig = function()"
        , "    return 1600, 900, 'fullscreen', scaleTarget, true,"
        , "           witnessExpected, 1, 100, false, 'nearest' end;"
        ]
    , saDrive = luaLines
        [ "m.onDefaults();"
        , "engine.loadDefaultConfig = _G.__realLoadDefaultConfig;"
        ]
    }

-- | Back: the REAL @data.revert@ runs (SettingsRevert.hs already proves
--   it does so headless), reverting an applied-but-unsaved scale to the
--   baseline @data.savedVideo@ that opening Settings captured
--   (scripts/settings/data.lua:465, :658). Nothing about revert is
--   stubbed; the setup only moves data.current away from that baseline,
--   which is the state a user reaches by clicking Apply and then Back.
--
--   The witness is the frame limit, one of the snapshot fields revert
--   restores unconditionally (:703-714), so it proves the real revert
--   ran whether or not the scale moved with it.
backScaleAction ∷ ScaleAction
backScaleAction = ScaleAction
    { saName = "settingsMenu.onBack"
    , saHow  = "an applied-but-unsaved scale the real data.revert undoes"
    , saSetup = \changed → luaLines
        [ "local savedScale = data.savedVideo.uiScale;"
        , if changed
            then "data.current.uiScale = (savedScale >= 3.0) and 1.0 or (savedScale + 1.0);"
            else "data.current.uiScale = savedScale;"
        , "local scaleBefore = data.current.uiScale;"
        , "local witnessExpected = data.savedVideo.frameLimit;"
        , "local witnessBefore = (witnessExpected == 120) and 90 or 120;"
        , "data.current.frameLimit = witnessBefore;"
        ]
    , saDrive = "m.onBack();"
    }
