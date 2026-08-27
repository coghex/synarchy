-- | #783 gate: the F3 click-correlation contract. Two halves:
--
--   1. A pure proof that 'UI.Manager.Query.elementPaintKey' —
--      'ui.dumpWidgets's @paintKey@ field — agrees with
--      'UI.Manager.Query.topHitBy', the engine's own topmost-hit
--      selection, for overlapping elements within one layer, across
--      layers, and through a nested ancestor chain. paintKey alone is
--      NOT a total order (ordinary siblings sharing a band/zIndex
--      tie — the common case, since most elements never set an
--      explicit zIndex), so a further test proves
--      'UI.Manager.Query.elementPaintOrder' — @paintOrder@ — resolves
--      that exact tie the same way topHitBy's own fold does
--      (later-painted wins, round-1 review). This is the "shared
--      ordering contract" the issue calls for: an offline consumer
--      ranking dump records by @(paintKey, paintOrder)@ picks the
--      exact same control a real click would, ties included.
--   2. Real-Lua coverage (mirrors 'Test.Headless.UI.Slider') that
--      'scripts/ui/registry.lua's ui.dumpWidgets() actually carries
--      that contract into the dump: control records (button) are
--      marked control=true, passive records (panel/label) are marked
--      control=false, a shown-but-disabled control stays control=true,
--      and overlapping controls on different pages/layers carry
--      paintKeys that rank the same way topHitBy would.
--
--   #1750 extends half 2 with the ROUTING facts the offline join needs
--   to reproduce 'UI.InputOwnership.routePointer' for a left click:
--   that a HUD control under an EMPTY exclusive modal is reported out
--   of pointer scope, that scope/blocking survive the widget-module
--   @known@ dedup path, that a callback-less raw pointer blocker is
--   admitted (as @control=false@ occlusion evidence) while a purely
--   visual element still is not, and that the four routing shapes
--   requirement 5's precedence turns on carry distinct state.
module Test.Headless.UI.ClickCorrelation (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef)
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.Manager
import UI.Manager.Query (topHitBy)
import UI.Types

-- * Pure fixtures (mirrors Test.Headless.UI.ElementInputPolicy)

page ∷ Text → UILayer → UIPageManager → (PageHandle, UIPageManager)
page name layer mgr =
    let (h, m1) = createPage name layer mgr
    in (h, showPage h m1)

boxAt ∷ Text → (Float, Float) → (Float, Float) → PageHandle
     → UIPageManager → (ElementHandle, UIPageManager)
boxAt name (x, y) (w, h) pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
    in (eh, m2)

childBoxOf ∷ ElementHandle → Text → (Float, Float) → (Float, Float) → PageHandle
          → UIPageManager → (ElementHandle, UIPageManager)
childBoxOf parentH name (relX, relY) (w, h) pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addChildElement parentH eh relX relY m1
    in (eh, m2)

pt ∷ (Float, Float)
pt = (50, 50)

-- | Same hit-eligibility 'findElementAt' uses: any sized element.
sized ∷ UIElement → Bool
sized el = let (w, h) = ueSize el in w > 0 ∧ h > 0

spec ∷ Spec
spec = do
    describe "elementPaintKey agrees with topHitBy — shared ordering contract (#783)" $ do
        it "within one layer, the higher-zIndex sibling wins both topHitBy and paintKey" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (loH, m2) = boxAt "lo" pt (100, 100) hudH m1
                (hiH, m3) = boxAt "hi" pt (100, 100) hudH m2
                m4 = setElementZIndex hiH 10 m3
            in do
                topHitBy (const True) sized pt m4 `shouldBe` Just hiH
                fromMaybe 0 (elementPaintKey hiH m4)
                    `shouldSatisfy` (> fromMaybe 0 (elementPaintKey loH m4))

        it "a higher UILayer band wins regardless of equal zIndex (Modal over HUD)" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (hudEl, m2) = boxAt "hudEl" pt (100, 100) hudH m1
                (modalH, m3) = page "modal" LayerModal m2
                (modalEl, m4) = boxAt "modalEl" pt (100, 100) modalH m3
            in do
                topHitBy (const True) sized pt m4 `shouldBe` Just modalEl
                fromMaybe 0 (elementPaintKey modalEl m4)
                    `shouldSatisfy` (> fromMaybe 0 (elementPaintKey hudEl m4))

        it "accumulates zIndex through a nested ancestor chain" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (parentH, m2) = boxAt "parent" (0, 0) (200, 200) hudH m1
                m3 = setElementZIndex parentH 3 m2
                (childH, m4) = childBoxOf parentH "child" (0, 0) (100, 100) hudH m3
                m5 = setElementZIndex childH 7 m4
            in elementPaintKey childH m5 `shouldBe` Just (uiLayerBand LayerHUD 0 + 3 + 7)

        it "for a mixed set of overlapping elements across pages, topHitBy's pick is exactly the paintKey argmax" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (a, m2) = boxAt "a" pt (100, 100) hudH m1
                m3 = setElementZIndex a 50 m2
                (b, m4) = boxAt "b" pt (100, 100) hudH m3
                (menuH, m5) = page "menu" LayerMenu m4
                (c, m6) = boxAt "c" pt (100, 100) menuH m5
                keys = [fromMaybe 0 (elementPaintKey h m6) | h ← [a, b, c]]
                maxKey = maximum keys
            in case topHitBy (const True) sized pt m6 of
                Just w  → fromMaybe 0 (elementPaintKey w m6) `shouldBe` maxKey
                Nothing → expectationFailure "expected a hit"

        it "paintKey alone ties for ordinary same-band, same-zIndex siblings; paintOrder breaks it exactly like topHitBy (later wins)" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (firstH, m2) = boxAt "first" pt (100, 100) hudH m1
                (secondH, m3) = boxAt "second" pt (100, 100) hudH m2
            in do
                -- Neither sets an explicit zIndex — the common case —
                -- so their paintKey is identical: elementPaintOrder is
                -- the only thing that can distinguish them.
                elementPaintKey firstH m3 `shouldBe` elementPaintKey secondH m3
                topHitBy (const True) sized pt m3 `shouldBe` Just secondH
                fromMaybe 0 (elementPaintOrder secondH m3)
                    `shouldSatisfy` (> fromMaybe 0 (elementPaintOrder firstH m3))

    around withHeadlessEngine $
        describe "ui.dumpWidgets control/paintKey contract (real Lua, #783)" $ do
            it "marks button records control=true and label/panel records control=false" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls dumpFixtureLua
                setup `shouldNotSatisfy` isLuaError
                out ← evalDebug ls
                    "local ui=require('scripts.ui.registry'); local widgets=ui.dumpWidgets(); \
                    \local btnControl,panelControl,labelControl=nil,nil,nil; \
                    \for _,w in ipairs(widgets) do \
                    \  if w.id=='button:'..tostring(_G.__btnId) then btnControl=w.control end; \
                    \  if w.id=='panel:'..tostring(_G.__panelId) then panelControl=w.control end; \
                    \  if w.id=='label:'..tostring(_G.__labelId) then labelControl=w.control end; \
                    \end; \
                    \return btnControl == true and panelControl == false and labelControl == false"
                out `shouldBe` "true"

            it "a shown-but-disabled control stays control=true and reports enabled=false" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls dumpFixtureLua
                setup `shouldNotSatisfy` isLuaError
                disableCall ← evalDebug ls
                    "local button=require('scripts.ui.button'); \
                    \UI.setClickable(button.getElementHandle(_G.__btnId), false)"
                disableCall `shouldNotSatisfy` isLuaError
                out ← evalDebug ls
                    "local ui=require('scripts.ui.registry'); local widgets=ui.dumpWidgets(); \
                    \local found=nil; \
                    \for _,w in ipairs(widgets) do \
                    \  if w.id=='button:'..tostring(_G.__btnId) then found=w end \
                    \end; \
                    \return found.control == true and found.enabled == false"
                out `shouldBe` "true"

            it "ranks overlapping controls on different pages by paintKey, matching the engine's topmost pick" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls
                    "local hud = UI.newPage('t_hud', 'hud'); UI.showPage(hud); \
                    \local modal = UI.newPage('t_modal', 'modal'); UI.showPage(modal); \
                    \local button = require('scripts.ui.button'); button.init(); \
                    \_G.__hudBtn = button.new({name='hud_btn', page=hud, textureSet=1, x=0, y=0, width=50, height=50}); \
                    \_G.__modalBtn = button.new({name='modal_btn', page=modal, textureSet=1, x=0, y=0, width=50, height=50})"
                setup `shouldNotSatisfy` isLuaError
                out ← evalDebug ls
                    "local ui=require('scripts.ui.registry'); local widgets=ui.dumpWidgets(); \
                    \local hudKey,modalKey=nil,nil; \
                    \for _,w in ipairs(widgets) do \
                    \  if w.id=='button:'..tostring(_G.__hudBtn) then hudKey=w.paintKey end; \
                    \  if w.id=='button:'..tostring(_G.__modalBtn) then modalKey=w.paintKey end; \
                    \end; \
                    \return modalKey ~= nil and hudKey ~= nil and modalKey > hudKey"
                out `shouldBe` "true"

            it "two same-page, same-zIndex buttons tie on paintKey; paintOrder ranks the later-created one higher" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls
                    "local page = UI.newPage('t_tie', 'hud'); UI.showPage(page); \
                    \local button = require('scripts.ui.button'); button.init(); \
                    \_G.__firstBtn = button.new({name='first_btn', page=page, textureSet=1, x=0, y=0, width=50, height=50}); \
                    \_G.__secondBtn = button.new({name='second_btn', page=page, textureSet=1, x=0, y=0, width=50, height=50})"
                setup `shouldNotSatisfy` isLuaError
                out ← evalDebug ls
                    "local ui=require('scripts.ui.registry'); local widgets=ui.dumpWidgets(); \
                    \local firstKey,firstOrder,secondKey,secondOrder=nil,nil,nil,nil; \
                    \for _,w in ipairs(widgets) do \
                    \  if w.id=='button:'..tostring(_G.__firstBtn) then firstKey=w.paintKey; firstOrder=w.paintOrder end; \
                    \  if w.id=='button:'..tostring(_G.__secondBtn) then secondKey=w.paintKey; secondOrder=w.paintOrder end; \
                    \end; \
                    \return firstKey == secondKey and secondOrder ~= nil and firstOrder ~= nil and secondOrder > firstOrder"
                out `shouldBe` "true"

            -- #1750: the routing facts the offline click join needs to
            -- reproduce routePointer's LEFT-click outcome.

            it "records a HUD control below an exclusive modal as out of scope, even when the modal has no element at all" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls
                    "local hud = UI.newPage('t_scope_hud', 'hud'); UI.showPage(hud); \
                    \local button = require('scripts.ui.button'); button.init(); \
                    \_G.__scopeBtn = button.new({name='under_modal_btn', page=hud, textureSet=1, x=0, y=0, width=50, height=50}); \
                    \_G.__scopeModal = UI.newPage('t_scope_modal', 'modal')"
                setup `shouldNotSatisfy` isLuaError
                -- Before the modal is shown the button is in scope.
                before' ← evalDebug ls (scopeOfLua "_G.__scopeBtn")
                before' `shouldBe` "true"
                -- Showing the EMPTY modal establishes the boundary: the
                -- HUD button below it is out of pointer scope even
                -- though the modal has nothing at the click point. This
                -- is the empty-modal case the offline join used to
                -- mis-correlate to the unreachable control.
                shown ← evalDebug ls "UI.showPage(_G.__scopeModal)"
                shown `shouldNotSatisfy` isLuaError
                after' ← evalDebug ls (scopeOfLua "_G.__scopeBtn")
                after' `shouldBe` "false"

            it "carries pointerBlocking and scope facts through the widget-module (known) path" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls dumpFixtureLua
                setup `shouldNotSatisfy` isLuaError
                out ← evalDebug ls
                    "local ui=require('scripts.ui.registry'); local widgets=ui.dumpWidgets(); \
                    \local btn=nil; \
                    \for _,w in ipairs(widgets) do \
                    \  if w.id=='button:'..tostring(_G.__btnId) then btn=w end \
                    \end; \
                    \return btn ~= nil and btn.inScope == true and btn.pointerBlocking == true \
                    \  and btn.leftClickTarget == true and btn.leftClickAffordance == true"
                out `shouldBe` "true"

            it "keeps a callback-less raw pointer blocker in the dump, as control=false occlusion evidence" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls rawFixtureLua
                setup `shouldNotSatisfy` isLuaError
                out ← evalDebug ls
                    "local ui=require('scripts.ui.registry'); local widgets=ui.dumpWidgets(); \
                    \local blocker=nil; \
                    \for _,w in ipairs(widgets) do \
                    \  if w.handle == _G.__rawBlocker then blocker=w end \
                    \end; \
                    \return blocker ~= nil and blocker.control == false \
                    \  and blocker.type == 'blocker' \
                    \  and blocker.pointerBlocking == true and blocker.inScope == true \
                    \  and blocker.leftClickTarget == false and blocker.leftClickAffordance == false"
                out `shouldBe` "true"

            it "does not start admitting purely visual, non-blocking raw elements" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls rawFixtureLua
                setup `shouldNotSatisfy` isLuaError
                out ← evalDebug ls
                    "local ui=require('scripts.ui.registry'); local widgets=ui.dumpWidgets(); \
                    \local decor=false; \
                    \for _,w in ipairs(widgets) do \
                    \  if w.handle == _G.__rawDecor then decor=true end \
                    \end; \
                    \return decor == false"
                out `shouldBe` "true"

            it "keeps a raw left control admitted as before, with control=true and the routing facts" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls rawFixtureLua
                setup `shouldNotSatisfy` isLuaError
                out ← evalDebug ls
                    "local ui=require('scripts.ui.registry'); local widgets=ui.dumpWidgets(); \
                    \local ctrl=nil; \
                    \for _,w in ipairs(widgets) do \
                    \  if w.handle == _G.__rawCtrl then ctrl=w end \
                    \end; \
                    \return ctrl ~= nil and ctrl.control == true and ctrl.type == 'button' \
                    \  and ctrl.inScope == true \
                    \  and ctrl.pointerBlocking == true and ctrl.leftClickTarget == true \
                    \  and ctrl.leftClickAffordance == true"
                out `shouldBe` "true"

            it "gives active, passive, disabled and explicitly-blocking-disabled records distinct routing state" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls rawFixtureLua
                setup `shouldNotSatisfy` isLuaError
                -- The four cases requirement 5's precedence turns on,
                -- as (pointerBlocking, leftClickTarget,
                -- leftClickAffordance) triples read off ONE dump:
                --   active                 -> true , true , true
                --   passive blocker        -> true , false, false
                --   disabled, non-blocking -> false, false, true
                --   disabled, opted-in     -> true , false, true
                out ← evalDebug ls
                    "local ui=require('scripts.ui.registry'); local widgets=ui.dumpWidgets(); \
                    \local by={}; \
                    \for _,w in ipairs(widgets) do if w.handle then by[w.handle]=w end end; \
                    \local function trip(h) local w=by[h]; if not w then return 'missing' end; \
                    \  return tostring(w.pointerBlocking)..','..tostring(w.leftClickTarget)..','..tostring(w.leftClickAffordance) end; \
                    \return trip(_G.__rawCtrl)..'|'..trip(_G.__rawBlocker)..'|'..trip(_G.__rawDisabled)..'|'..trip(_G.__rawDisabledBlocking)"
                -- a string reply comes back JSON-quoted, unlike the
                -- bare booleans the checks above return
                out `shouldBe`
                    "\"true,true,true|true,false,false|false,false,true|true,false,true\""

            it "reports a right-click-only control as pointer-blocking but never a left target" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls rawFixtureLua
                setup `shouldNotSatisfy` isLuaError
                -- `interactive` conflates the two callbacks, which is
                -- exactly why the join needs leftClickTarget: this
                -- element blocks a LEFT click (RouteBlocked) without
                -- ever activating on one.
                out ← evalDebug ls
                    "local info = UI.getElementInfo(_G.__rawRightOnly); \
                    \return info.interactive == true and info.pointerBlocking == true \
                    \  and info.leftClickTarget == false and info.leftClickAffordance == false"
                out `shouldBe` "true"

-- | One page with a real button, panel, and label — the three module
--   kinds whose dump() records this issue distinguishes. Literal `1`
--   integer handles stand in for textureSet/font (UI.newBox/UI.newText
--   accept any integer; headless has no render pass to validate
--   against a real registered texture/font, mirroring
--   Test.Headless.UI.UnicodeTextEditing's `font=1` fixtures).
dumpFixtureLua ∷ Text
dumpFixtureLua = T.concat
    [ "local page = UI.newPage('test_click_correlation_page', 'hud'); UI.showPage(page); "
    , "local button = require('scripts.ui.button'); button.init(); "
    , "_G.__btnId = button.new({name='ctrl_btn', page=page, textureSet=1, x=0, y=0, width=50, height=50}); "
    , "local panel = require('scripts.ui.panel'); "
    , "_G.__panelId = panel.new({name='ctx_panel', page=page, textureSet=1, x=200, y=0, width=50, height=50}); "
    , "local label = require('scripts.ui.label'); "
    , "_G.__labelId = label.new({name='ctx_label', page=page, font=1, text='hi', x=200, y=100})"
    ]

-- | #1750: five RAW engine elements (no widget module) on one visible
--   HUD page, covering every distinct routing shape requirement 5's
--   precedence turns on. Raw rather than widget-module records
--   deliberately: this is the second 'ui.dumpWidgets' pass, the one
--   whose admission filter #1750 widens, and a widget module could
--   never produce a callback-less blocker.
--
--     * __rawCtrl            — clickable + onClick: an ACTIVE left
--                              target, and pointer-blocking by the
--                              pre-existing implicit rule.
--     * __rawBlocker         — no callback at all, explicit
--                              'UI.setPointerBlocking': the
--                              callback-less occlusion surface a left
--                              click stops dead at. Previously omitted
--                              from the dump entirely.
--     * __rawDisabled        — onClick registered but
--                              @setClickable(false)@: NOT pointer-
--                              blocking (the implicit rule needs
--                              clickable), so it is #783's lone
--                              disabled affordance.
--     * __rawDisabledBlocking— the same, plus the explicit blocking
--                              opt-in: it consumes the click AND is
--                              still the affordance that explains it.
--     * __rawDecor           — a plain box, no callback, no opt-in:
--                              the purely visual element the widened
--                              filter must still leave out.
--     * __rawRightOnly       — clickable + onRightClick only: reads as
--                              @interactive@ and blocks a left click
--                              without ever being a left target.
--
--   Box texture/tileSize/overflow are literal integers for the same
--   reason 'dumpFixtureLua' uses @textureSet=1@: headless has no
--   render pass to validate a handle against.
rawFixtureLua ∷ Text
rawFixtureLua = T.concat
    [ "local page = UI.newPage('t_raw_routing', 'hud'); UI.showPage(page); "
    , "local function box(name) "
    , "  local h = UI.newBox(name, 50, 50, 1, 4, 1.0, 1.0, 1.0, 1.0, 0, page); "
    , "  UI.addToPage(page, h, 0, 0); return h end; "
    , "_G.__rawCtrl = box('raw_ctrl'); "
    , "UI.setClickable(_G.__rawCtrl, true); UI.setOnClick(_G.__rawCtrl, 'noop'); "
    , "_G.__rawBlocker = box('raw_blocker'); "
    , "UI.setPointerBlocking(_G.__rawBlocker, true); "
    , "_G.__rawDisabled = box('raw_disabled'); "
    , "UI.setOnClick(_G.__rawDisabled, 'noop'); UI.setClickable(_G.__rawDisabled, false); "
    , "_G.__rawDisabledBlocking = box('raw_disabled_blocking'); "
    , "UI.setOnClick(_G.__rawDisabledBlocking, 'noop'); "
    , "UI.setClickable(_G.__rawDisabledBlocking, false); "
    , "UI.setPointerBlocking(_G.__rawDisabledBlocking, true); "
    , "_G.__rawDecor = box('raw_decor'); "
    , "_G.__rawRightOnly = box('raw_right_only'); "
    , "UI.setClickable(_G.__rawRightOnly, true); "
    , "UI.setOnRightClick(_G.__rawRightOnly, 'noop')"
    ]

-- | @UI.getElementInfo(<handle>).inScope@ as a @"true"@/@"false"@
--   console reply, for the empty-modal scope case.
scopeOfLua ∷ Text → Text
scopeOfLua handleExpr = T.concat
    [ "local info = UI.getElementInfo(require('scripts.ui.button')"
    , ".getElementHandle(", handleExpr, ")); return info.inScope == true" ]

-- * Real-Lua-backend helper (mirrors Test.Headless.UI.Slider)

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
