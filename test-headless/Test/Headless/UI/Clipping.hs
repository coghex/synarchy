{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | #747 gate: opt-in rectangular descendant clipping (Phase C child
--   C1 of #741). Most of this suite is pure "UI.Clipping"/
--   "UI.Manager.Query" coverage — the acceptance explicitly wants the
--   clip decision testable with no Vulkan, window, or Lua engine,
--   mirroring @Test.Headless.UI.ElementInputPolicy@. A final block
--   proves the new opt-in is configurable/queryable through the real
--   Lua @UI@ API.
module Test.Headless.UI.Clipping (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Data.IORef (newIORef)
import Test.Headless.Harness (withHeadlessEngine)
import UI.Clipping
import UI.InputOwnership (routePointer, routeScroll, PointerKind(..), InputRoute(..))
import UI.Manager
import UI.Types

-- * Pure fixtures (mirrors Test.Headless.UI.ElementInputPolicy)

page ∷ Text → UILayer → UIPageManager → (PageHandle, UIPageManager)
page name layer mgr =
    let (h, m1) = createPage name layer mgr
    in (h, showPage h m1)

-- | A plain container at absolute (x,y)/(w,h), optionally opting into
--   clipping its descendants.
containerAt ∷ Text → (Float, Float) → (Float, Float) → Bool → PageHandle
           → UIPageManager → (ElementHandle, UIPageManager)
containerAt name (x, y) (w, h) clips pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementClipChildren eh clips m2
    in (eh, m3)

-- | A passive child at a RELATIVE offset from its parent.
childAt ∷ ElementHandle → Text → (Float, Float) → (Float, Float) → PageHandle
       → UIPageManager → (ElementHandle, UIPageManager)
childAt parentH name (relX, relY) (w, h) pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addChildElement parentH eh relX relY m1
    in (eh, m2)

-- | A clickable hit surface parented under an existing element
--   (mirrors the real widget shape: a passive box + a clickable
--   hit-sprite child).
hitChildAt ∷ ElementHandle → Text → (Float, Float) → (Float, Float) → Text → PageHandle
          → UIPageManager → (ElementHandle, UIPageManager)
hitChildAt parentH name (relX, relY) (w, h) cb pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addChildElement parentH eh relX relY m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnClick eh cb m3
    in (eh, m4)

-- | A clickable hit surface mounted directly on the page (no parent) —
--   the "floating root-mounted content" shape (dropdown option lists,
--   context menus).
rootHitAt ∷ Text → (Float, Float) → (Float, Float) → Text → PageHandle
         → UIPageManager → (ElementHandle, UIPageManager)
rootHitAt name (x, y) (w, h) cb pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnClick eh cb m3
    in (eh, m4)

spec ∷ Spec
spec = do
    describe "effectiveClip — opt-in compatibility (#747)" $ do
        it "an element with no clipping ancestor is unclipped" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) False hudH m1
                (childH, m3) = childAt containerH "child" (0, 0) (50, 50) hudH m2
            in effectiveClip childH m3 `shouldBe` Nothing

        it "a container with ueClipChildren left False (the default) never restricts its children" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (10, 10) (20, 20) False hudH m1
                (_childH, m3) = hitChildAt containerH "row" (100, 100) (50, 20) "rowClick" hudH m2
            in routePointer PointerLeftClick (125, 110) m3 `shouldSatisfy` (≢ RouteMiss)

    describe "effectiveClip — full / partial / nested / empty intersections" $ do
        it "a child fully inside a clipping ancestor hits normally" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (childH, m3) = hitChildAt containerH "row" (10, 10) (50, 20) "rowClick" hudH m2
            in routePointer PointerLeftClick (30, 20) m3 `shouldBe` RouteElement childH "rowClick"

        it "a child straddling the clip boundary hits only within the visible slice" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                -- row spans absolute x in [60,120), clip only allows [0,100)
                (_childH, m3) = hitChildAt containerH "row" (60, 10) (60, 20) "rowClick" hudH m2
            in do
                routePointer PointerLeftClick (80, 20) m3 `shouldSatisfy` (≢ RouteMiss)
                routePointer PointerLeftClick (110, 20) m3 `shouldBe` RouteMiss

        it "nested clips intersect — a leaf is bounded by the tighter of two clipping ancestors" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (outerH, m2) = containerAt "outer" (0, 0) (200, 200) True hudH m1
                (innerH, m3) = childAt outerH "inner" (50, 50) (60, 60) hudH m2  -- abs [50,110)x[50,110)
                m4 = setElementClipChildren innerH True m3
                -- leaf abs spans [50,150)x[50,150) but inner only clips to [50,110)
                (_leafH, m5) = hitChildAt innerH "leaf" (0, 0) (100, 100) "leafClick" hudH m4
            in do
                routePointer PointerLeftClick (70, 70) m5 `shouldSatisfy` (≢ RouteMiss)
                routePointer PointerLeftClick (130, 70) m5 `shouldBe` RouteMiss
                routePointer PointerLeftClick (70, 130) m5 `shouldBe` RouteMiss

        it "an empty (non-overlapping) intersection hits nothing" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (outerH, m2) = containerAt "outer" (0, 0) (100, 100) True hudH m1
                -- inner sits entirely outside outer's own bounds
                (innerH, m3) = childAt outerH "inner" (200, 200) (50, 50) hudH m2
                m4 = setElementClipChildren innerH True m3
                (_leafH, m5) = hitChildAt innerH "leaf" (0, 0) (50, 50) "leafClick" hudH m4
            in routePointer PointerLeftClick (220, 220) m5 `shouldBe` RouteMiss

    describe "effectiveClip — movement/resize take effect immediately (no caching)" $ do
        it "moving the clipping ancestor immediately changes what's clipped" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (_childH, m3) = hitChildAt containerH "row" (0, 0) (100, 20) "rowClick" hudH m2
                m4 = setElementPosition containerH 500 500 m3
            in do
                routePointer PointerLeftClick (10, 10) m4 `shouldBe` RouteMiss
                routePointer PointerLeftClick (510, 510) m4 `shouldSatisfy` (≢ RouteMiss)

        it "resizing the clipping ancestor immediately changes the clip bounds" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (_childH, m3) = hitChildAt containerH "row" (0, 0) (100, 20) "rowClick" hudH m2
                m4 = setElementSize containerH 40 40 m3
            in do
                routePointer PointerLeftClick (30, 10) m4 `shouldSatisfy` (≢ RouteMiss)
                routePointer PointerLeftClick (80, 10) m4 `shouldBe` RouteMiss

    describe "clipQuadUV — render-path geometry shared by boxes/sprites/text" $ do
        it "passes a quad through unchanged with no clip in effect" $
            clipQuadUV Nothing (10, 10, 50, 50) (0, 0, 1, 1) `shouldBe` Just ((10, 10, 50, 50), (0, 0, 1, 1))

        it "returns Nothing for a quad fully outside the clip (nothing to draw)" $
            clipQuadUV (Just (0, 0, 100, 100)) (200, 200, 50, 50) (0, 0, 1, 1) `shouldBe` Nothing

        it "clips a partially-overlapping quad and adjusts its UV rect proportionally" $
            -- clip = [0,100)x[0,100); quad = [50,150)x[0,50) -> visible [50,100)x[0,50)
            case clipQuadUV (Just (0, 0, 100, 100)) (50, 0, 100, 50) (0, 0, 1, 1) of
                Just ((cx, cy, cw, ch), (u0, v0, u1, v1)) → do
                    (cx, cy, cw, ch) `shouldBe` (50, 0, 50, 50)
                    (u0, v0, u1, v1) `shouldBe` (0, 0, 0.5, 1)
                Nothing → expectationFailure "expected a clipped quad, got Nothing"

        it "an ancestor clip fully containing the quad leaves it unchanged" $
            clipQuadUV (Just (0, 0, 200, 200)) (10, 10, 50, 50) (0, 0, 1, 1)
                `shouldBe` Just ((10, 10, 50, 50), (0, 0, 1, 1))

    describe "hover clipping (findElementAt — backs tooltip hover detection)" $ do
        it "does not return a row clipped out of view" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (_rowH, m3) = childAt containerH "row" (150, 10) (50, 20) hudH m2
            in findElementAt (170, 20) m3 `shouldBe` Nothing

        it "still returns a row that IS within the clip" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (rowH, m3) = childAt containerH "row" (10, 10) (50, 20) hudH m2
            in findElementAt (30, 20) m3 `shouldBe` Just rowH

    describe "wheel/scroll clipping (routeScroll)" $
        it "a scroll-capturing element outside the clip does not capture wheel input" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (eh, m3) = childAt containerH "panelBg" (150, 10) (50, 20) hudH m2
                m4 = setElementCapturesScroll eh True m3
            in routeScroll (170, 20) m4 `shouldBe` Nothing

    describe "floating root-mounted content escapes trigger ancestry" $
        it "a root-mounted popup positioned outside its trigger's tiny clip still hits normally" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (_triggerH, m2) = containerAt "trigger" (0, 0) (50, 20) True hudH m1
                (popupH, m3) = rootHitAt "popup" (0, 20) (50, 80) "popupClick" hudH m2
            in routePointer PointerLeftClick (25, 60) m3 `shouldBe` RouteElement popupH "popupClick"

    describe "clip does not disturb z-order resolution" $
        it "among two overlapping clipped-in elements, the higher zIndex still wins" $
            let (hudH, m1) = page "hud" LayerHUD emptyUIPageManager
                (containerH, m2) = containerAt "box" (0, 0) (100, 100) True hudH m1
                (_loH, m3) = hitChildAt containerH "lo" (0, 0) (100, 100) "loClick" hudH m2
                (hiH, m4) = hitChildAt containerH "hi" (0, 0) (100, 100) "hiClick" hudH m3
                m5 = setElementZIndex hiH 10 m4
            in routePointer PointerLeftClick (50, 50) m5 `shouldBe` RouteElement hiH "hiClick"

    -- #747 Lua-facing coverage: the new opt-in and its effective-clip
    -- query can be configured/queried through the real UI API without
    -- any graphical engine (mirrors ElementInputPolicy.hs's pattern).
    around withHeadlessEngine $
        describe "Lua-facing UI API for clipping (#747)" $ do
            it "UI.setClipChildren / UI.isClipChildren round-trip through the real Lua UI API" $ \env → do
                ls ← newBareLuaBackend env
                before' ← evalDebug ls
                    "local pg = UI.newPage('clip_t1', 'hud'); \
                    \local el = UI.newElement('clip_e1', 10, 10, pg); \
                    \UI.addToPage(pg, el, 0, 0); \
                    \_G.__clipEl1 = el; \
                    \return UI.isClipChildren(el)"
                before' `shouldBe` "false"
                after' ← evalDebug ls
                    "UI.setClipChildren(_G.__clipEl1, true); return UI.isClipChildren(_G.__clipEl1)"
                after' `shouldBe` "true"

            it "UI.getEffectiveClip is nil when unclipped and reports the intersecting rect once a parent opts in" $ \env → do
                ls ← newBareLuaBackend env
                nilBefore ← evalDebug ls
                    "local pg = UI.newPage('clip_t2', 'hud'); \
                    \local parent = UI.newElement('clip_p2', 100, 100, pg); \
                    \UI.addToPage(pg, parent, 10, 10); \
                    \local child = UI.newElement('clip_c2', 20, 20, pg); \
                    \UI.addChild(parent, child, 5, 5); \
                    \_G.__clipParent2 = parent; _G.__clipChild2 = child; \
                    \return UI.getEffectiveClip(child) == nil"
                nilBefore `shouldBe` "true"
                clipAfter ← evalDebug ls
                    "UI.setClipChildren(_G.__clipParent2, true); \
                    \local c = UI.getEffectiveClip(_G.__clipChild2); \
                    \return c.x, c.y, c.w, c.h"
                clipAfter `shouldBe` "10.0\t10.0\t100.0\t100.0"

            it "a clipped-out element cannot be hit through UI.findElementAt" $ \env → do
                ls ← newBareLuaBackend env
                _ ← evalDebug ls
                    "local pg = UI.newPage('clip_t3', 'hud'); \
                    \local parent = UI.newElement('clip_p3', 100, 100, pg); \
                    \UI.addToPage(pg, parent, 0, 0); \
                    \UI.setClipChildren(parent, true); \
                    \local row = UI.newElement('clip_r3', 50, 20, pg); \
                    \UI.addChild(parent, row, 150, 10)"
                out ← evalDebug ls "return UI.findElementAt(170, 20)"
                out `shouldBe` "null"

    -- #747 adoption: the REAL scripts/ui/list.lua module ("Reusable
    -- list" — also reused by save_browser.lua/plant_panel.lua) now
    -- parents every visible slot under an opt-in clipping viewport
    -- (see list.lua's list.new). Drives the production module through
    -- the real Lua UI API (mirrors Test.Headless.UI.Slider's technique
    -- for scripts/ui/slider.lua) and proves the migrated region both
    -- exposes the correct effective clip AND that a row clipped out of
    -- view (simulated here by shrinking the viewport below its full
    -- slot stack, the exact "resize edge case" #747 is a safety net
    -- for) cannot be hit — while a still-visible row keeps working.
    around withHeadlessEngine $
        describe "scripts/ui/list.lua clip adoption (#747)" $
            it "exposes the list's effective clip and a row clipped out of view cannot be hit" $ \env → do
                ls ← newBareLuaBackend env
                setup ← evalDebug ls listSetupLua
                setup `shouldNotSatisfy` isLuaError

                -- The viewport itself is a real opt-in clip container.
                evalDebug ls "return UI.isClipChildren(_G.__listViewportId)" ≫= (`shouldBe` "true")

                -- Each slot's effective clip is exactly the list's own
                -- bounds (x=100,y=100,width=200,height=3*20=60).
                clipBefore ← evalDebug ls
                    "local c = UI.getEffectiveClip(_G.__listHit1Id); return c.x, c.y, c.w, c.h"
                clipBefore `shouldBe` "100.0\t100.0\t200.0\t60.0"

                -- All three rows are hittable at their own centres.
                evalDebug ls "return UI.findElementAt(150, 110) == _G.__listHit1Id" ≫= (`shouldBe` "true")
                evalDebug ls "return UI.findElementAt(150, 130) == _G.__listHit2Id" ≫= (`shouldBe` "true")
                evalDebug ls "return UI.findElementAt(150, 150) == _G.__listHit3Id" ≫= (`shouldBe` "true")

                -- Shrink the viewport below its full slot stack (e.g. a
                -- caller resizing the list smaller without re-issuing
                -- list.setItems) — rows 2 and 3 fall outside the clip.
                _ ← evalDebug ls "UI.setSize(_G.__listViewportId, 200, 20)"

                clipAfter ← evalDebug ls
                    "local c = UI.getEffectiveClip(_G.__listHit2Id); return c.x, c.y, c.w, c.h"
                clipAfter `shouldBe` "100.0\t100.0\t200.0\t20.0"

                -- Row 1 (still within the shrunk clip) keeps hitting;
                -- rows 2 and 3 (now clipped out) cannot be hit at all.
                evalDebug ls "return UI.findElementAt(150, 110) == _G.__listHit1Id" ≫= (`shouldBe` "true")
                evalDebug ls "return UI.findElementAt(150, 130)" ≫= (`shouldBe` "null")
                evalDebug ls "return UI.findElementAt(150, 150)" ≫= (`shouldBe` "null")

-- * Wire-integration helpers (mirrors Test.Headless.UI.ElementInputPolicy)

-- | A real Lua backend with the FULL Lua API registered and nothing
--   else loaded — callers drive it with 'evalDebug' snippets calling
--   the real @UI.*@ functions, exactly as any script would.
newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run one command through the exact loadstring+pcall primitive the
--   real TCP debug console itself uses ('executeDebugLua').
evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | Creates a shown page, requires the real scripts/ui/list.lua
--   module, and builds a 5-item / 3-visible list at (100,100), then
--   resolves the viewport + first three slot hit-boxes by name into
--   globals the assertions above read (mirrors
--   Test.Headless.UI.Slider's sliderSetupLua — 'UI.getVisibleElements'
--   doesn't assume anything about handle numbering). font=0 is the
--   "unset" FontHandle sentinel; text still creates fine headless with
--   no font loaded, it just wouldn't render (no GPU here anyway).
listSetupLua ∷ Text
listSetupLua = T.concat
    [ "local page = UI.newPage('test_list_page', 'hud'); "
    , "UI.showPage(page); "
    , "local listMod = require('scripts.ui.list'); "
    , "listMod.init(); "
    , "local items = {}; "
    , "for i = 1, 5 do items[i] = { text = 'item' .. i, value = i } end; "
    , "_G.__listId = listMod.new({ name = 'test_list', x = 100, y = 100, "
    , "width = 200, itemHeight = 20, maxVisible = 3, items = items, "
    , "page = page, font = 0 }); "
    , "for _, e in ipairs(UI.getVisibleElements()) do "
    , "  if e.name == 'test_list_viewport' then _G.__listViewportId = e.handle end; "
    , "  if e.name == 'test_list_hit_1' then _G.__listHit1Id = e.handle end; "
    , "  if e.name == 'test_list_hit_2' then _G.__listHit2Id = e.handle end; "
    , "  if e.name == 'test_list_hit_3' then _G.__listHit3Id = e.handle end; "
    , "end"
    ]
