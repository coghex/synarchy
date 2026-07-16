{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | #747 gate: the shared viewport-aware floating-placement contract
--   (Phase C child C1 of #741) — "UI.PopupPlacement" is pure (no
--   Vulkan/window/Lua engine), so placement is fully Hspec-testable in
--   isolation, mirroring "UI.InputOwnership"/"UI.Clipping". Tooltip
--   placement deliberately stays on its own separate implementation
--   ("UI.Tooltip.Layout"/"UI.Tooltip.Render", untouched by this
--   change) — its own @Test.Headless.UI.Tooltip@ suite passing
--   unmodified is the regression proof for "existing tooltip
--   placement may remain separate but cannot regress".
module Test.Headless.UI.PopupPlacement (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Data.IORef (newIORef, writeIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.PopupPlacement

-- | A 1280x720 framebuffer — a typical desktop resolution, used as the
--   default budget for tests that aren't specifically exercising edge/
--   corner clamping.
fb ∷ (Float, Float)
fb = (1280, 720)

mkReq ∷ (Float, Float) → (Float, Float) → (Float, Float) → PopupDirection → (Float, Float) → PlacementRequest
mkReq (ax, ay) (aw, ah) (cw, ch) dir (fbW, fbH) = PlacementRequest
    { prAnchorX = ax, prAnchorY = ay, prAnchorW = aw, prAnchorH = ah
    , prContentW = cw, prContentH = ch, prPreferred = dir
    , prFramebufferW = fbW, prFramebufferH = fbH
    }

spec ∷ Spec
spec = do
    describe "placePopup — preferred direction / opposite-direction fallback" $ do
        it "PopupBelow: opens directly below the anchor when there's room" $
            let req = mkReq (100, 100) (50, 20) (80, 60) PopupBelow fb
                p = placePopup req
            in (plX p, plY p, plFlipped p) `shouldBe` (100, 120, False)

        it "PopupBelow: flips above when there's no room below" $
            let req = mkReq (100, 690) (50, 20) (80, 60) PopupBelow fb
                p = placePopup req
            in do
                plFlipped p `shouldBe` True
                plY p `shouldBe` 630  -- anchorY(690) - contentH(60)

        it "PopupAbove: opens directly above when there's room" $
            let req = mkReq (100, 200) (50, 20) (80, 60) PopupAbove fb
                p = placePopup req
            in (plY p, plFlipped p) `shouldBe` (140, False)

        it "PopupAbove: flips below when there's no room above" $
            let req = mkReq (100, 10) (50, 20) (80, 60) PopupAbove fb
                p = placePopup req
            in do
                plFlipped p `shouldBe` True
                plY p `shouldBe` 30  -- anchorY(10) + anchorH(20)

        it "PopupRight: opens directly right when there's room (submenu preference)" $
            let req = mkReq (100, 100) (50, 0) (80, 60) PopupRight fb
                p = placePopup req
            in (plX p, plY p, plFlipped p) `shouldBe` (150, 100, False)

        it "PopupRight: flips left when there's no room on the right (submenu flip)" $
            let req = mkReq (1200, 100) (50, 0) (80, 60) PopupRight fb
                p = placePopup req
            in do
                plFlipped p `shouldBe` True
                plX p `shouldBe` 1120  -- anchorX(1200) - contentW(80)

        it "PopupLeft: opens directly left when there's room" $
            let req = mkReq (500, 100) (50, 0) (80, 60) PopupLeft fb
                p = placePopup req
            in (plX p, plFlipped p) `shouldBe` (420, False)

        it "PopupLeft: flips right when there's no room on the left" $
            let req = mkReq (30, 100) (50, 0) (80, 60) PopupLeft fb
                p = placePopup req
            in do
                plFlipped p `shouldBe` True
                plX p `shouldBe` 80  -- anchorX(30) + anchorW(50)

        it "PopupAnchored: places exactly at the anchor point, never flips" $
            let req = mkReq (300, 300) (0, 0) (80, 60) PopupAnchored fb
                p = placePopup req
            in (plX p, plY p, plFlipped p) `shouldBe` (300, 300, False)

    describe "placePopup — submenu vertical clamp (never flipped on the non-preferred axis)" $
        it "a horizontal-direction popup's Y is clamped, not flipped, even when it would overflow" $
            let req = mkReq (100, 690) (50, 0) (80, 60) PopupRight fb
                p = placePopup req
            in do
                plY p `shouldBe` 660  -- clamped to fbH(720) - contentH(60), NOT flipped
                plFlipped p `shouldBe` False  -- flip tracks the RIGHT/LEFT decision only

    describe "placePopup — edges and corners" $ do
        it "clamps a below-preferred popup horizontally at the left edge" $
            let req = mkReq (-30, 100) (50, 20) (80, 60) PopupBelow fb
                p = placePopup req
            in plX p `shouldBe` 0

        it "clamps a below-preferred popup horizontally at the right edge" $
            let req = mkReq (1250, 100) (50, 20) (80, 60) PopupBelow fb
                p = placePopup req
            in plX p `shouldBe` 1200  -- fbW(1280) - contentW(80)

        it "clamps fully into the top-left corner when the anchor sits off both edges" $
            let req = mkReq (-40, -40) (50, 20) (80, 60) PopupAnchored fb
                p = placePopup req
            in (plX p, plY p) `shouldBe` (0, 0)

        it "clamps fully into the bottom-right corner when the anchor sits off both edges" $
            let req = mkReq (1300, 750) (50, 20) (80, 60) PopupAnchored fb
                p = placePopup req
            in (plX p, plY p) `shouldBe` (1280 - 80, 720 - 60)

        it "degenerate case: content larger than the framebuffer still clamps to the edge, not negative" $
            let req = mkReq (10, 10) (0, 0) (2000, 60) PopupAnchored fb
                p = placePopup req
            in plX p `shouldBe` 0

    describe "placePopup — full interactive size includes the scrollbar strip" $
        it "a wider contentW (display + scrollbar) changes the clamp outcome vs. the display alone" $
            let displayOnly = mkReq (1220, 100) (50, 20) (60, 200) PopupBelow fb
                withScrollbar = mkReq (1220, 100) (50, 20) (84, 200) PopupBelow fb  -- +24px scrollbar
                pDisplay = placePopup displayOnly
                pScroll  = placePopup withScrollbar
            in do
                plX pDisplay `shouldBe` 1220          -- fits fully on screen unclamped
                plX pScroll `shouldBe` (1280 - 84)     -- now clamped to keep the scrollbar reachable

    describe "fitVisibleRows — oversized dropdown row reduction" $ do
        it "keeps the preferred count when it fits" $
            fitVisibleRows 8 36 400 `shouldBe` 8

        it "reduces to however many rows fit when the preferred count doesn't" $
            fitVisibleRows 8 36 100 `shouldBe` 2  -- floor(100/36) = 2

        it "never returns fewer than 1 row even in a degenerate (near-zero) space" $
            fitVisibleRows 8 36 5 `shouldBe` 1

        it "returns 0 for a non-positive preferred count (nothing to show)" $
            fitVisibleRows 0 36 400 `shouldBe` 0

    describe "placePopup — resize recomputes cleanly (pure function, nothing cached)" $
        it "the same anchor/content placed against a smaller framebuffer reflows correctly" $
            let wide = mkReq (700, 100) (50, 20) (80, 60) PopupBelow (1280, 720)
                narrow = mkReq (700, 100) (50, 20) (80, 60) PopupBelow (720, 720)
                pWide = placePopup wide
                pNarrow = placePopup narrow
            in do
                plX pWide `shouldBe` 700       -- fits at 1280 wide
                plX pNarrow `shouldBe` (720 - 80) -- clamped once the framebuffer shrinks

    describe "placePopup — final placement is what both paint and hit-testing would use" $
        it "the resolved rect is always fully within the framebuffer when content fits at all" $
            let req = mkReq (50, 50) (20, 20) (100, 50) PopupBelow fb
                p = placePopup req
            in do
                plX p `shouldSatisfy` (≥ 0)
                plY p `shouldSatisfy` (≥ 0)
                (plX p + 100) `shouldSatisfy` (≤ 1280)
                (plY p + 50) `shouldSatisfy` (≤ 720)

    describe "placePopup — scale-invariant geometry (UI scale is applied by callers in pixels)" $
        it "uniformly scaling every input by the same factor scales the result identically" $
            let base = mkReq (100, 100) (50, 20) (80, 60) PopupBelow (1280, 720)
                scaled = mkReq (200, 200) (100, 40) (160, 120) PopupBelow (2560, 1440)
                pBase = placePopup base
                pScaled = placePopup scaled
            in do
                plX pScaled `shouldBe` plX pBase * 2
                plY pScaled `shouldBe` plY pBase * 2
                plFlipped pScaled `shouldBe` plFlipped pBase

    -- #747 review round 1 (concern 2): a real, driven-through-Lua
    -- regression proving an OPEN scripts/ui/context_menu.lua menu
    -- (root + a reopened submenu) rebuilds — new backdrop size, fresh
    -- placement — against a genuine framebuffer resize, rather than
    -- keeping stale geometry and an old-size backdrop.
    around withHeadlessEngine $
        describe "scripts/ui/context_menu.lua resize regression (#747)" $
            it "an open menu with an open submenu rebuilds against the new framebuffer size on resize" $ \env → do
                writeIORef (framebufferSizeRef env) (1280, 720)
                ls ← newBareLuaBackend env
                setup ← evalDebug ls contextMenuSetupLua
                setup `shouldNotSatisfy` isLuaError

                evalDebug ls "return UI.getElementInfo(_G.__cmBackdropId).width" ≫= (`shouldBe` "1280.0")
                evalDebug ls "return UI.getElementInfo(_G.__cmBackdropId).height" ≫= (`shouldBe` "720.0")

                -- Hover the row with a submenu open it (the real
                -- openSubMenu path — cm.onHoverEnter is the public
                -- entry point, openSubMenu itself is a local).
                openSub ← evalDebug ls
                    "require('scripts.ui.context_menu').onHoverEnter(_G.__cmRow1Id)"
                openSub `shouldNotSatisfy` isLuaError
                hasSubBefore ← evalDebug ls
                    "for _, e in ipairs(UI.getVisibleElements()) do \
                    \  if e.name == 'context_menu_sub_box' then return true end \
                    \end; return false"
                hasSubBefore `shouldBe` "true"

                -- Simulate a real resize: the input thread updates
                -- framebufferSizeRef BEFORE broadcasting to Lua
                -- (Engine.Input.Thread.Dispatch), so the test does the
                -- same ordering.
                writeIORef (framebufferSizeRef env) (400, 300)
                resizeResult ← evalDebug ls
                    "require('scripts.ui.context_menu').onFramebufferResize(400, 300)"
                resizeResult `shouldNotSatisfy` isLuaError

                -- The backdrop was fully torn down and rebuilt at the
                -- new size (the OLD backdrop handle is gone).
                evalDebug ls "return UI.getElementInfo(_G.__cmBackdropId) == nil" ≫= (`shouldBe` "true")
                newBackdropSize ← evalDebug ls
                    "local w, h; \
                    \for _, e in ipairs(UI.getVisibleElements()) do \
                    \  if e.name == 'context_menu_backdrop' then w, h = e.width, e.height end \
                    \end; return w, h"
                newBackdropSize `shouldBe` "400.0\t300.0"

                -- The submenu reopened automatically after the rebuild.
                hasSubAfter ← evalDebug ls
                    "for _, e in ipairs(UI.getVisibleElements()) do \
                    \  if e.name == 'context_menu_sub_box' then return true end \
                    \end; return false"
                hasSubAfter `shouldBe` "true"

                -- The root panel is still on-screen (never stale/out
                -- of the new, smaller framebuffer).
                rootOnScreen ← evalDebug ls
                    "local ok = true; \
                    \for _, e in ipairs(UI.getVisibleElements()) do \
                    \  if e.name == 'context_menu_root_box' then \
                    \    ok = e.x >= 0 and e.y >= 0 and e.x + e.width <= 400 and e.y + e.height <= 300 \
                    \  end \
                    \end; return ok"
                rootOnScreen `shouldBe` "true"

    -- #747 review round 1 (concern 3): a real, driven-through-Lua
    -- widget test for scripts/ui/dropdown.lua — the pure placePopup/
    -- fitVisibleRows math being correct doesn't prove dropdown.lua's
    -- CALL-SITE WIRING (which numbers it feeds in, whether it stores
    -- and reuses the result for its own click-outside bounds) is
    -- correct. Exercises oversized-row reduction, scrolling, final hit
    -- bounds matching the rendered position, and re-placement across a
    -- resize (close + reopen — dropdown.lua has no live-while-open
    -- resize hook, unlike context_menu.lua's singleton popup; every
    -- screen that owns a dropdown already tears down and rebuilds its
    -- whole UI on resize, e.g. settingsMenu.onFramebufferResize, so an
    -- open option list never survives a live resize in production).
    around withHeadlessEngine $
        describe "scripts/ui/dropdown.lua placement adoption (#747)" $
            it "reduces rows to fit, scrolls, matches its own hit bounds, and re-places on resize" $ \env → do
                writeIORef (framebufferSizeRef env) (800, 200)
                ls ← newBareLuaBackend env
                setup ← evalDebug ls dropdownSetupLua
                setup `shouldNotSatisfy` isLuaError

                -- 20 options, maxVisibleOptions=8, but only ~100px is
                -- available (100px above the display box beats 60px
                -- below) at optionHeight=36 -> floor(100/36) = 2 rows.
                evalDebug ls "require('scripts.ui.dropdown').openList(_G.__ddId); return 'ok'" ≫= (`shouldBe` "\"ok\"")
                rowsPresent ← evalDebug ls
                    "local n = 0; \
                    \for _, e in ipairs(UI.getVisibleElements()) do \
                    \  if e.name:match('^test_dd_opt_hit_') then n = n + 1 end \
                    \end; return n"
                rowsPresent `shouldBe` "2"

                -- Flipped ABOVE the display box (100+40=140 down to
                -- 200 has no room for 2*36=72px) and NOT flipped
                -- horizontally, at the exact placePopup-resolved rect.
                listInfo ← evalDebug ls
                    "local i = UI.getElementInfo(_G.__findByName('test_dd_list')); \
                    \return i.x, i.y, i.width, i.height"
                listInfo `shouldBe` "50.0\t28.0\t100.0\t72.0"

                -- Scrolling: row 1's TEXT label shows option 1 before,
                -- option 2 after.
                evalDebug ls "return UI.getElementInfo(_G.__findByName('test_dd_opt_1')).text"
                    ≫= (`shouldBe` "\"Option 1\"")
                scrollResult ← evalDebug ls
                    "require('scripts.ui.dropdown').onScrollChanged(_G.__ddId, 1); return 'ok'"
                scrollResult `shouldBe` "\"ok\""
                evalDebug ls "return UI.getElementInfo(_G.__findByName('test_dd_opt_1')).text"
                    ≫= (`shouldBe` "\"Option 2\"")

                -- Final hit bounds: a point at the HAND-COMPUTED centre
                -- of row 1's clickable hit-sprite (derived independently
                -- from the placePopup rect above: listX=50, listY=28,
                -- row 1 spans y∈[28,64), x∈[50,150)) resolves to that
                -- same row's hit handle — proving the resolved
                -- placement, not just an internal field, is what a real
                -- click would land on.
                hitMatches ← evalDebug ls
                    "return UI.findElementAt(100, 46) == _G.__findByName('test_dd_opt_hit_1')"
                hitMatches `shouldBe` "true"

                -- Resize: close, grow the framebuffer so all 8 rows now
                -- fit below, reopen — re-placement re-derives from the
                -- NEW framebuffer rather than reusing stale geometry.
                _ ← evalDebug ls "require('scripts.ui.dropdown').closeList(_G.__ddId)"
                writeIORef (framebufferSizeRef env) (800, 720)
                evalDebug ls "require('scripts.ui.dropdown').openList(_G.__ddId); return 'ok'" ≫= (`shouldBe` "\"ok\"")

                rowsAfterResize ← evalDebug ls
                    "local n = 0; \
                    \for _, e in ipairs(UI.getVisibleElements()) do \
                    \  if e.name:match('^test_dd_opt_hit_') then n = n + 1 end \
                    \end; return n"
                rowsAfterResize `shouldBe` "8"

                listInfoAfter ← evalDebug ls
                    "local i = UI.getElementInfo(_G.__findByName('test_dd_list')); \
                    \return i.x, i.y, i.width, i.height"
                listInfoAfter `shouldBe` "50.0\t140.0\t100.0\t288.0"

-- * Lua-facing helpers (mirrors Test.Headless.UI.Clipping/Slider)

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | Stubs scripts.hud with just the font field context_menu.lua reads
--   (package.loaded lookup, not a require — no need to load the real,
--   much heavier hud module), shows a 2-item root menu with one
--   submenu at (100,100), and resolves the backdrop + first root row
--   (the one with a submenu) by name into globals.
contextMenuSetupLua ∷ Text
contextMenuSetupLua = T.concat
    [ "package.loaded['scripts.hud'] = { menuFont = 1 }; "
    , "local cm = require('scripts.ui.context_menu'); "
    , "cm.show({ "
    , "  { label = 'Equip', submenu = { { label = 'Right hand', callback = function() end } } }, "
    , "  { label = 'Drop', callback = function() end }, "
    , "}, 100, 100); "
    , "for _, e in ipairs(UI.getVisibleElements()) do "
    , "  if e.name == 'context_menu_backdrop' then _G.__cmBackdropId = e.handle end; "
    , "  if e.name == 'context_menu_root_row_1' then _G.__cmRow1Id = e.handle end; "
    , "end"
    ]

-- | Defines a reusable _G.__findByName(pattern) helper (element
--   handles change across dropdown.lua's closeList/openList, so tests
--   must re-resolve by name rather than caching a stale handle), then
--   creates a 20-option dropdown at (50,100) with uiscale pinned to 1
--   (so every pixel dimension in the test is exact, not dependent on
--   engine.getUIScale()'s default) and font=0 (the "unset" sentinel —
--   text still creates fine headless with no font loaded).
dropdownSetupLua ∷ Text
dropdownSetupLua = T.concat
    [ "function _G.__findByName(name) "
    , "  for _, e in ipairs(UI.getVisibleElements()) do "
    , "    if e.name == name then return e.handle end "
    , "  end "
    , "  return nil "
    , "end; "
    , "local dd = require('scripts.ui.dropdown'); "
    , "dd.init(); "
    , "local page = UI.newPage('test_dropdown_page', 'hud'); "
    , "UI.showPage(page); "
    , "local options = {}; "
    , "for i = 1, 20 do options[i] = { text = 'Option ' .. i, value = i } end; "
    , "_G.__ddId = dd.new({ name = 'test_dd', x = 50, y = 100, page = page, "
    , "font = 0, uiscale = 1, options = options, maxVisibleOptions = 8 })"
    ]
