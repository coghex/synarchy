-- | #2628 gate: @scripts/ui/list.lua@'s @setItems@ must leave the
--   attached scrollbar agreeing with the rows it just replaced — same
--   offset, same content range, and controls visible again when the
--   replacement needs scrolling and the list itself is shown.
--
--   Every example here drives the REAL @scripts.ui.list@ and
--   @scripts.ui.scrollbar@ modules through the real Lua @UI@ API
--   (the technique 'Test.Headless.UI.Clipping' established for this
--   widget), never a stub: the defect lived entirely in how the two
--   production modules talk to each other, so a fixture that reimplements
--   either side would gate nothing.
module Test.Headless.UI.ListScrollSync (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Data.IORef (atomicModifyIORef', newIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (VideoConfig(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)

-- * Harness

-- | The same 1x baseline 'Test.Headless.UI.Clipping' pins (#1747): the
--   real list and scrollbar modules resolve @params.uiscale or
--   scale.get()@ and derive every dimension from it, so a developer's
--   persisted @ui_scale@ would otherwise decide this suite's widget
--   geometry. Nothing here asserts a pixel, but the scrollbar's track
--   height (and therefore whether it is created at all) is scale-derived,
--   so the baseline keeps the fixture's shape fixed.
listScrollBaselineUIScale ∷ Float
listScrollBaselineUIScale = 1.0

normalizeUIScale ∷ EngineEnv → IO ()
normalizeUIScale env =
    atomicModifyIORef' (videoConfigRef env) $ \c →
        (c { vcUIScale = listScrollBaselineUIScale }, ())

-- | An isolated engine at the canonical scale. Isolation is established
--   OUTSIDE 'withHeadlessEngine' because engine initialization is itself
--   a config writer (see 'Test.Headless.Harness.Isolation').
withListScrollEngine ∷ (EngineEnv → IO α) → IO α
withListScrollEngine action = withIsolatedResourceRoot $
    withHeadlessEngine $ \env → do
        normalizeUIScale env
        action env

-- | A bare Lua backend with the real API registered, no script boot.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run one command through the exact loadstring+pcall primitive the
--   real TCP debug console uses.
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- * Fixture

-- | A shown page carrying one REAL list: 20 items, @maxVisible = 4@, so
--   it is built scrolling and owns a real scrollbar from creation (#2628
--   is scoped to synchronizing an already-created scrollbar; a list built
--   without one is separate work).
--
--   Resolves into globals: the list id, the scrollbar id (via the
--   scrollbar module's own 'findByElementHandle' on its up-button, so
--   the test reads the production module's registry rather than
--   re-deriving an id), the first row's text element, and all SIX
--   scrollbar element handles by name — @scrollbar.getElementHandles@
--   exposes only four of the six @scrollbar.setVisible@ toggles, and a
--   visibility regression that spared the two track caps would slip past
--   a four-handle check.
--
--   @font = 0@ is the unset FontHandle sentinel: text elements still
--   create fine headless, they simply would not render.
listScrollSetupLua ∷ Text
listScrollSetupLua = T.concat
    [ "local page = UI.newPage('lsl_page', 'hud'); "
    , "UI.showPage(page); "
    , "local listMod = require('scripts.ui.list'); "
    , "listMod.init(); "
    , "_G.__lsItems = {}; "
    , "for i = 1, 20 do _G.__lsItems[i] = { text = 'item' .. i, value = i } end; "
    , "_G.__lsShort = { _G.__lsItems[1] }; "
    , "_G.__lsExact = {}; "
    , "for i = 1, 4 do _G.__lsExact[i] = _G.__lsItems[i] end; "
    , "_G.__lsListId = listMod.new({ name = 'lsl', x = 100, y = 100, "
    , "width = 200, itemHeight = 20, maxVisible = 4, items = _G.__lsItems, "
    , "page = page, font = 0 }); "
    , "_G.__lsSbNames = { 'lsl_scrollbar_up', 'lsl_scrollbar_down', "
    , "'lsl_scrollbar_tab', 'lsl_scrollbar_track_mid', "
    , "'lsl_scrollbar_track_top', 'lsl_scrollbar_track_bottom' }; "
    , "_G.__lsSbHandles = {}; "
    , "for _, e in ipairs(UI.getVisibleElements()) do "
    , "  if e.name == 'lsl_txt_1' then _G.__lsTxt1 = e.handle end; "
    , "  for i, n in ipairs(_G.__lsSbNames) do "
    , "    if e.name == n then _G.__lsSbHandles[i] = e.handle end; "
    , "  end; "
    , "end; "
    , "_G.__lsSbId = require('scripts.ui.scrollbar')"
    , ".findByElementHandle(_G.__lsSbHandles[1]); "
    -- The list's row offset beside the scrollbar's own stored offset.
    -- Two values, so a desync reads as the pair it is.
    , "_G.__lsOffsets = function() "
    , "  return require('scripts.ui.list').getScrollOffset(_G.__lsListId), "
    , "         require('scripts.ui.scrollbar').getScrollOffset(_G.__lsSbId) "
    , "end; "
    -- How many of the six scrollbar elements are effectively visible,
    -- beside how many exist: "6\t6" or "0\t6", never a bare count that
    -- a lost handle could quietly shrink.
    , "_G.__lsSbVisible = function() "
    , "  local shown = 0; "
    , "  for _, h in ipairs(_G.__lsSbHandles) do "
    , "    local info = UI.getElementInfo(h); "
    , "    if info and info.visible then shown = shown + 1 end; "
    , "  end; "
    , "  return shown, #_G.__lsSbHandles "
    , "end; "
    -- What row 1 actually DISPLAYS, read back off the real text element
    -- rather than recomputed from the offset the assertion is testing.
    , "_G.__lsRow1 = function() "
    , "  local info = UI.getElementInfo(_G.__lsTxt1); "
    , "  return info and info.text or '<none>' "
    , "end"
    ]

-- * Spec

spec ∷ Spec
spec = around withListScrollEngine $ do

    -- Requirement 1. Before the fix scrollbar.setContentSize only
    -- CLAMPED the stored offset (10 stays in range for 20 items) and
    -- never fired onScroll, so the rows snapped to the top while the
    -- scrollbar still held 10 — and the next step asked for 11.
    describe "a replacement on a scrolled list" $
        it "leaves both offsets at zero, and one step from there advances exactly one row" $ \env → do
            ls ← newBareLuaBackend env
            setup ← evalDebug ls listScrollSetupLua
            setup `shouldNotSatisfy` isLuaError

            _ ← evalDebug ls "require('scripts.ui.list').setScrollOffset(_G.__lsListId, 10)"
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "10\t10")
            evalDebug ls "return _G.__lsRow1()" ≫= (`shouldBe` "\"item11\"")

            _ ← evalDebug ls "require('scripts.ui.list').setItems(_G.__lsListId, _G.__lsItems)"
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "0\t0")
            evalDebug ls "return _G.__lsRow1()" ≫= (`shouldBe` "\"item1\"")

            -- One real scroll step, through the scrollbar's own
            -- down-button path: the rows must move by exactly one from
            -- what is DISPLAYED, which is the symptom the stale offset
            -- produced (a jump straight to 11).
            _ ← evalDebug ls "require('scripts.ui.scrollbar').scrollDown(_G.__lsSbId)"
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "1\t1")
            evalDebug ls "return _G.__lsRow1()" ≫= (`shouldBe` "\"item2\"")

    -- Requirement 2: the plant panel's filter-then-clear. Before the
    -- fix the growth branch called setContentSize alone, so nothing
    -- ever re-showed the controls setItems had hidden.
    describe "a scrollable → short → scrollable replacement cycle" $
        it "hides the scrollbar for the short set and shows every control again when it grows back" $ \env → do
            ls ← newBareLuaBackend env
            setup ← evalDebug ls listScrollSetupLua
            setup `shouldNotSatisfy` isLuaError

            evalDebug ls "return _G.__lsSbVisible()" ≫= (`shouldBe` "6\t6")

            _ ← evalDebug ls "require('scripts.ui.list').setItems(_G.__lsListId, _G.__lsShort)"
            evalDebug ls "return _G.__lsSbVisible()" ≫= (`shouldBe` "0\t6")
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "0\t0")

            _ ← evalDebug ls "require('scripts.ui.list').setItems(_G.__lsListId, _G.__lsItems)"
            evalDebug ls "return _G.__lsSbVisible()" ≫= (`shouldBe` "6\t6")
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "0\t0")
            evalDebug ls "return _G.__lsRow1()" ≫= (`shouldBe` "\"item1\"")

            -- And the restored scrollbar actually drives the rows.
            _ ← evalDebug ls "require('scripts.ui.scrollbar').scrollDown(_G.__lsSbId)"
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "1\t1")
            evalDebug ls "return _G.__lsRow1()" ≫= (`shouldBe` "\"item2\"")

    -- Requirement 1, the half a visibility check cannot see: a short or
    -- empty replacement must re-RANGE the scrollbar too. Before the fix
    -- setContentSize was skipped entirely for a non-scrolling set, so
    -- the old 20-item range stayed reachable through list.setScrollOffset
    -- (which routes into scrollbar.setScrollOffset and clamps against
    -- totalItems - visibleItems), scrolling the rows off a 1-item list.
    describe "a short or empty replacement" $
        it "re-ranges the scrollbar, so a later setScrollOffset cannot reach the old range" $ \env → do
            ls ← newBareLuaBackend env
            setup ← evalDebug ls listScrollSetupLua
            setup `shouldNotSatisfy` isLuaError

            _ ← evalDebug ls "require('scripts.ui.list').setItems(_G.__lsListId, _G.__lsShort)"
            _ ← evalDebug ls "require('scripts.ui.list').setScrollOffset(_G.__lsListId, 12)"
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "0\t0")
            evalDebug ls "return _G.__lsRow1()" ≫= (`shouldBe` "\"item1\"")

            _ ← evalDebug ls "require('scripts.ui.list').setItems(_G.__lsListId, {})"
            _ ← evalDebug ls "require('scripts.ui.list').setScrollOffset(_G.__lsListId, 9)"
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "0\t0")
            evalDebug ls "return _G.__lsSbVisible()" ≫= (`shouldBe` "0\t6")
            evalDebug ls "return _G.__lsRow1()" ≫= (`shouldBe` "\"\"")

    -- Requirement 3: list.setVisible stays the sole owner of whether a
    -- list is shown. "Needs scrolling" alone would un-hide the chrome of
    -- a list its caller had hidden, which is the regression the fix's
    -- visibility tracking exists to prevent.
    describe "replacements on a hidden list" $
        it "never show the scrollbar, and showing the list then shows it only when scrolling is needed" $ \env → do
            ls ← newBareLuaBackend env
            setup ← evalDebug ls listScrollSetupLua
            setup `shouldNotSatisfy` isLuaError

            _ ← evalDebug ls "require('scripts.ui.list').setVisible(_G.__lsListId, false)"
            evalDebug ls "return _G.__lsSbVisible()" ≫= (`shouldBe` "0\t6")

            -- Empty, exactly maxVisible, and scrollable — the third is
            -- the one that would wrongly reappear.
            _ ← evalDebug ls "require('scripts.ui.list').setItems(_G.__lsListId, {})"
            evalDebug ls "return _G.__lsSbVisible()" ≫= (`shouldBe` "0\t6")
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "0\t0")

            _ ← evalDebug ls "require('scripts.ui.list').setItems(_G.__lsListId, _G.__lsExact)"
            evalDebug ls "return _G.__lsSbVisible()" ≫= (`shouldBe` "0\t6")
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "0\t0")

            _ ← evalDebug ls "require('scripts.ui.list').setItems(_G.__lsListId, _G.__lsItems)"
            evalDebug ls "return _G.__lsSbVisible()" ≫= (`shouldBe` "0\t6")
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "0\t0")

            -- Shown: the scrollbar comes back because the current set
            -- needs it, at the offset the rows are actually displaying,
            -- and one step advances by exactly one.
            _ ← evalDebug ls "require('scripts.ui.list').setVisible(_G.__lsListId, true)"
            evalDebug ls "return _G.__lsSbVisible()" ≫= (`shouldBe` "6\t6")
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "0\t0")
            evalDebug ls "return _G.__lsRow1()" ≫= (`shouldBe` "\"item1\"")
            _ ← evalDebug ls "require('scripts.ui.scrollbar').scrollDown(_G.__lsSbId)"
            evalDebug ls "return _G.__lsOffsets()" ≫= (`shouldBe` "1\t1")
            evalDebug ls "return _G.__lsRow1()" ≫= (`shouldBe` "\"item2\"")

            -- And a now-VISIBLE list keeps following its item count.
            _ ← evalDebug ls "require('scripts.ui.list').setItems(_G.__lsListId, _G.__lsExact)"
            evalDebug ls "return _G.__lsSbVisible()" ≫= (`shouldBe` "0\t6")
            _ ← evalDebug ls "require('scripts.ui.list').setItems(_G.__lsListId, _G.__lsItems)"
            evalDebug ls "return _G.__lsSbVisible()" ≫= (`shouldBe` "6\t6")
