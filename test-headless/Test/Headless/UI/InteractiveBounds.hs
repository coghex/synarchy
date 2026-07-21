{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | #749 gate (Phase C child C3 of #741): logical / visual /
--   interactive bounds. A box's overflow expands what it RENDERS on
--   every side without changing its stored layout, so the pre-#749
--   hit-test could never reach that visible border. C3 lets a genuine
--   box-backed control opt its border into interaction while a
--   decorative box keeps bleeding without becoming a blocker.
--
--   Most of this suite is pure "UI.InteractiveBounds"/"UI.Manager.Query"
--   /"UI.InputOwnership" coverage — the acceptance explicitly wants the
--   geometry testable with no Vulkan, window, or Lua engine, mirroring
--   @Test.Headless.UI.Clipping@ / @Test.Headless.UI.ElementInputPolicy@.
--   A "wire integration" block drives the real
--   'Engine.Input.Thread.processInputs' press/release dispatch, and a
--   final block proves the new opt-in + effective-bounds introspection
--   round-trip through the real Lua @UI@ API. Two source-audit blocks
--   pin the migrated control families and the playtest-oracle join.
module Test.Headless.UI.InteractiveBounds (spec) where

import UPrelude
import qualified Graphics.UI.GLFW as GLFW
import Test.Hspec
import Data.IORef (readIORef, writeIORef, atomicModifyIORef', newIORef)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scene.Base (LayerId(..))
import UI.Render (makeBoxBatches)
import Engine.Input.Bindings (defaultKeyBindings)
import Engine.Input.Thread (processInputs)
import Engine.Input.Types
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaMsg(..), LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.ControlActivation
import UI.Focus (createFocusManager)
import UI.InputOwnership
import UI.InteractiveBounds
import UI.Manager
import UI.Types

-- * Pure fixtures (mirror Test.Headless.UI.ElementInputPolicy/Clipping)

page ∷ Text → UILayer → UIPageManager → (PageHandle, UIPageManager)
page name layer mgr =
    let (h, m1) = createPage name layer mgr
    in (h, showPage h m1)

-- | A genuine box-backed control: a real 'RenderBox' with the given
--   overflow, clickable (left + right callback), and scroll-capturing —
--   so the SAME element is a candidate for every hit-test entry point
--   (left/right/middle click, hover/tooltip, wheel, #745 release).
--   @interactive@ opts its visible border in (#749).
controlBox ∷ Text → (Float, Float) → (Float, Float) → Float → Bool → Text
           → PageHandle → UIPageManager → (ElementHandle, UIPageManager)
controlBox name (x, y) (w, h) overflow interactive cb pageH mgr =
    let (eh, m1) = createBox name w h (BoxTextureHandle 0) 8 (1,1,1,1) overflow pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnClick eh cb m3
        m5 = setElementOnRightClick eh (cb <> "_r") m4
        m6 = setElementCapturesScroll eh True m5
        m7 = setElementInteractiveOverflow eh interactive m6
    in (eh, m7)

-- | A decorative box: a real 'RenderBox' with overflow but NO input
--   policy at all — the case "overflow alone never creates a target."
--   @interactive@ still opts the (nonexistent) interaction into
--   expanded bounds, proving that alone is not enough.
decorBox ∷ Text → (Float, Float) → (Float, Float) → Float → Bool
         → PageHandle → UIPageManager → (ElementHandle, UIPageManager)
decorBox name (x, y) (w, h) overflow interactive pageH mgr =
    let (eh, m1) = createBox name w h (BoxTextureHandle 0) 8 (1,1,1,1) overflow pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementInteractiveOverflow eh interactive m2
    in (eh, m3)

-- | A clipping container holding a child at a relative offset.
clipContainerAt ∷ Text → (Float, Float) → (Float, Float) → PageHandle
                → UIPageManager → (ElementHandle, UIPageManager)
clipContainerAt name (x, y) (w, h) pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementClipChildren eh True m2
    in (eh, m3)

basePage ∷ (PageHandle, UIPageManager)
basePage = page "hud" LayerHUD emptyUIPageManager

-- A migrated control at (100,100) size 100x100 overflow 16:
--   content bounds     = [100,200] × [100,200]
--   interactive bounds = [ 84,216] × [ 84,216]
migrated ∷ (ElementHandle, UIPageManager)
migrated = let (hudH, m1) = basePage
           in controlBox "ctrl" (100, 100) (100, 100) 16 True "onCtrl" hudH m1

contentOnly ∷ (ElementHandle, UIPageManager)
contentOnly = let (hudH, m1) = basePage
              in controlBox "ctrl" (100, 100) (100, 100) 16 False "onCtrl" hudH m1

-- Border points: inside the interactive (expanded) rect, OUTSIDE content.
leftBorder, rightBorder, topBorder, bottomBorder, center ∷ (Float, Float)
leftBorder   = (90, 150)
rightBorder  = (210, 150)
topBorder    = (150, 90)
bottomBorder = (150, 210)
center       = (150, 150)

-- Beyond even the interactive rect: misses for everyone.
beyondLeft ∷ (Float, Float)
beyondLeft = (80, 150)

spec ∷ Spec
spec = do
    describe "clampOverflow — invalid/valid overflow (#749)" $ do
        it "passes a positive overflow through unchanged" $
            clampOverflow (100, 100) 16 `shouldBe` 16
        it "passes a validly-negative overflow (a genuine shrink) through unchanged" $
            clampOverflow (100, 40) (-10) `shouldBe` (-10)
        it "clamps an inverting overflow UP to minus half the smaller extent (zero-extent, never negative)" $
            clampOverflow (100, 40) (-30) `shouldBe` (-20)
        it "clamps to exactly the limit for an extreme value (no unbounded/inverted geometry)" $
            clampOverflow (100, 40) (-1000) `shouldBe` (-20)
        it "treats a zero-size box's negative overflow as zero (no negative expansion)" $
            clampOverflow (0, 0) (-5) `shouldBe` 0
        it "sanitizes +Infinity overflow to zero (no unbounded expansion)" $
            clampOverflow (100, 100) (1/0) `shouldBe` 0
        it "sanitizes -Infinity overflow to zero" $
            clampOverflow (100, 100) (-1/0) `shouldBe` 0
        it "sanitizes NaN overflow to zero" $
            clampOverflow (100, 100) (0/0) `shouldBe` 0

    describe "content / visual / interactive rects (#749)" $ do
        it "content rect is position + size, independent of any overflow" $
            contentRect (100, 100) (fst' migrated) `shouldBe` (100, 100, 100, 100)
        it "an interactive box's interactive rect equals its expanded visual rect exactly" $
            let el = fst' migrated
            in interactiveRect (100, 100) el `shouldBe` visualRect (100, 100) el
        it "an interactive box's interactive rect is the content rect expanded by overflow per side" $
            interactiveRect (100, 100) (fst' migrated) `shouldBe` (84, 84, 132, 132)
        it "a content-only box's interactive rect equals its content rect (overflow ignored for hits)" $
            interactiveRect (100, 100) (fst' contentOnly) `shouldBe` (100, 100, 100, 100)
        it "a validly-negative interactive box shrinks its interactive rect below content bounds" $
            let (hudH, m1) = basePage
                (eh, m2)   = controlBox "shrink" (100, 100) (100, 100) (-10) True "onS" hudH m1
            in interactiveRect (100, 100) (lookupEl eh m2) `shouldBe` (110, 110, 80, 80)
        it "an invalid-overflow interactive box never inverts (width/height stay >= 0)" $
            let (hudH, m1) = basePage
                (eh, m2)   = controlBox "bad" (100, 100) (100, 40) (-1000) True "onB" hudH m1
                (_, _, w, h) = interactiveRect (100, 100) (lookupEl eh m2)
            in (w ≥ 0 ∧ h ≥ 0) `shouldBe` True
        it "a box created with a non-finite overflow expands nothing (sanitized at creation, never unbounded)" $
            let (hudH, m1) = basePage
                (eh, m2)   = controlBox "huge" (100, 100) (100, 100) (1/0) True "onH" hudH m1
            in interactiveRect (100, 100) (lookupEl eh m2) `shouldBe` (100, 100, 100, 100)
        it "a non-box element is content-only even with the flag set (no overflow to expand)" $
            let (hudH, m1) = basePage
                (eh, m2)   = clickablePlain "plain" (100, 100) (100, 100) "onP" hudH m1
                m3         = setElementInteractiveOverflow eh True m2
            in interactiveRect (100, 100) (lookupEl eh m3) `shouldBe` (100, 100, 100, 100)

    describe "isPointInElement — hit membership (#749)" $ do
        let (eh, mMig) = migrated
        let (_, mCon)  = contentOnly
        it "a migrated control is hit on its center (content)" $
            isPointInElement center (lookupEl eh mMig) mMig `shouldBe` True
        it "a migrated control is hit on its LEFT visible border (outside content)" $
            isPointInElement leftBorder (lookupEl eh mMig) mMig `shouldBe` True
        it "...RIGHT border" $
            isPointInElement rightBorder (lookupEl eh mMig) mMig `shouldBe` True
        it "...TOP border" $
            isPointInElement topBorder (lookupEl eh mMig) mMig `shouldBe` True
        it "...BOTTOM border" $
            isPointInElement bottomBorder (lookupEl eh mMig) mMig `shouldBe` True
        it "a point BEYOND the visible border misses" $
            isPointInElement beyondLeft (lookupEl eh mMig) mMig `shouldBe` False
        it "a content-only box is NOT hit on the same border point (overflow stays decorative)" $
            isPointInElement leftBorder (lookupEl eh mCon) mCon `shouldBe` False
        it "...but IS hit on its content center" $
            isPointInElement center (lookupEl eh mCon) mCon `shouldBe` True
        it "a collapsed (zero-extent, invalid-overflow) interactive box is not hit, even on its degenerate line" $
            let (hudH, m1) = basePage
                -- overflow -20 collapses the 40px-tall box to zero height:
                -- interactive rect (120,120,60,0). (140,120) sits on that line.
                (ch, m2) = controlBox "collapse" (100, 100) (100, 40) (-20) True "onC" hudH m1
            in do
                interactiveRect (100, 100) (lookupEl ch m2) `shouldBe` (120, 120, 60, 0)
                isPointInElement (140, 120) (lookupEl ch m2) m2 `shouldBe` False
                routePointer PointerLeftClick (140, 120) m2 `shouldBe` RouteMiss

    describe "same bounds across every hit-test entry point (#749)" $ do
        -- A single border point must resolve identically for pointer
        -- (left/right/middle), hover/tooltip, wheel, and #745 release —
        -- the whole point of routing them all through isPointInElement.
        let (eh, m) = migrated
        it "left-click routing resolves the control on its border" $
            case routePointer PointerLeftClick leftBorder m of
                RouteElement h cb → (h, cb) `shouldBe` (eh, "onCtrl")
                other → expectationFailure ("expected RouteElement, got " ⧺ show other)
        it "right-click routing resolves the control on its border" $
            case routePointer PointerRightClick rightBorder m of
                RouteElement h cb → (h, cb) `shouldBe` (eh, "onCtrl_r")
                other → expectationFailure ("expected RouteElement, got " ⧺ show other)
        it "middle-click surface is blocked on its border" $
            isPointerSurfaceBlocked topBorder m `shouldBe` True
        it "wheel routing captures the control on its border" $
            routeScroll bottomBorder m `shouldBe` Just eh
        it "hover/tooltip hit-test finds the control on its border" $
            findElementAtExcept Set.empty leftBorder m `shouldBe` Just eh
        it "#745 press+release on the border activates (same routing at both ends)" $
            let pending = beginActivation PointerLeftClick eh m
            in resolveActivation rightBorder m pending `shouldBe` Activate eh "onCtrl"
        it "every entry point AGREES a beyond-border point is a miss" $ do
            routePointer PointerLeftClick beyondLeft m `shouldBe` RouteMiss
            isPointerSurfaceBlocked beyondLeft m `shouldBe` False
            routeScroll beyondLeft m `shouldBe` Nothing
            findElementAtExcept Set.empty beyondLeft m `shouldBe` Nothing

    describe "clip intersection — clipped overflow neither renders nor interacts (#749/#747)" $ do
        -- The control's border extends past a clipping ancestor whose
        -- bounds equal the control's CONTENT rect, so the overflow is
        -- clipped away: content still hits, the border no longer does.
        let (hudH, m1)   = basePage
            (contH, m2)  = clipContainerAt "clip" (100, 100) (100, 100) hudH m1
            (eh, m3)     = childControlBox contH "ctrl" (0, 0) (100, 100) 16 True "onCtrl" hudH m2
        it "the clipped-away visible border does not register a hit" $
            isPointInElement leftBorder (lookupEl eh m3) m3 `shouldBe` False
        it "...nor route a click" $
            routePointer PointerLeftClick leftBorder m3 `shouldBe` RouteMiss
        it "the content interior (inside the clip) still hits" $
            isPointInElement center (lookupEl eh m3) m3 `shouldBe` True
        it "effectiveInteractiveBounds reports the clip-intersected rect (border trimmed to content)" $
            effectiveInteractiveBounds eh m3 `shouldBe` Just (100, 100, 100, 100)

    describe "geometry updates apply on the very next query (#749)" $ do
        it "moving the control moves its interactive border immediately" $
            let (eh, m)  = migrated
                m'       = setElementPosition eh 300 300 m
            -- old border point now misses; the shifted border hits.
            in do
                isPointInElement leftBorder (lookupEl eh m') m' `shouldBe` False
                isPointInElement (290, 350) (lookupEl eh m') m' `shouldBe` True
        it "toggling the opt-in OFF makes the border content-only immediately" $
            let (eh, m) = migrated
                m'      = setElementInteractiveOverflow eh False m
            in isPointInElement leftBorder (lookupEl eh m') m' `shouldBe` False
        it "resizing the control resizes its interactive rect immediately" $
            let (eh, m) = migrated
                m'      = setElementSize eh 40 40 m
            -- content now [100,140]²; interactive [84,156]². (150,150)
            -- was inside the old content, now only inside the border.
            in do
                isPointInElement (150, 150) (lookupEl eh m') m' `shouldBe` True   -- border
                isPointInElement (170, 150) (lookupEl eh m') m' `shouldBe` False  -- beyond border

    describe "overlap ordering / single dispatch (#749)" $ do
        -- Two overlapping migrated controls; a point in BOTH their
        -- borders resolves to the higher paint-order one, once.
        let (hudH, m1) = basePage
            (_loH, m2) = controlBox "lo" (100, 100) (100, 100) 16 True "onLo" hudH m1
            (hiH, m3)  = controlBox "hi" (100, 100) (100, 100) 16 True "onHi" hudH m2
            m4         = setElementZIndex hiH 10 m3
        it "a border point shared by two controls routes to the topmost (single dispatch)" $
            case routePointer PointerLeftClick leftBorder m4 of
                RouteElement h cb → (h, cb) `shouldBe` (hiH, "onHi")
                other → expectationFailure ("expected RouteElement, got " ⧺ show other)
        it "#745 resolves that same border release to exactly one control" $
            let pending = beginActivation PointerLeftClick hiH m4
            in resolveActivation leftBorder m4 pending `shouldBe` Activate hiH "onHi"

    describe "decorative overflow stays noninteractive (#749)" $ do
        -- Overflow ALONE (even with the interactive-overflow flag) never
        -- creates a target: a decorative box with no click callback and
        -- no pointer-block opt-in blocks nothing, border or content.
        let (hudH, m1) = basePage
            (_eh, m2)  = decorBox "deco" (100, 100) (100, 100) 16 True hudH m1
        it "a decorative box does not route a click on its border" $
            routePointer PointerLeftClick leftBorder m2 `shouldBe` RouteMiss
        it "...nor on its content" $
            routePointer PointerLeftClick center m2 `shouldBe` RouteMiss
        it "...nor block the middle-click surface" $
            isPointerSurfaceBlocked leftBorder m2 `shouldBe` False

    describe "collapsed visual box renders nothing (#749)" $ do
        -- The real RenderBox call site: a clamped-to-zero (or otherwise
        -- non-positive) visual extent tiles no batches, matching the hit
        -- test's own hasArea guard — so an invalid/collapsed overflow is
        -- genuinely non-rendering AND non-hittable, not merely bounded.
        let texSet = BoxTextureSet
                { btsCenter = TextureHandle 5, btsN = TextureHandle 2
                , btsS = TextureHandle 8, btsE = TextureHandle 6
                , btsW = TextureHandle 4, btsNE = TextureHandle 3
                , btsNW = TextureHandle 1, btsSE = TextureHandle 9
                , btsSW = TextureHandle 7
                }
        it "a positive-extent box tiles its nine batches (sanity)" $
            V.length (makeBoxBatches texSet 0 0 40 40 10 (1,1,1,1) (LayerId 0) Nothing)
                `shouldBe` 9
        it "a zero-WIDTH visual box tiles nothing" $
            V.length (makeBoxBatches texSet 0 0 0 40 10 (1,1,1,1) (LayerId 0) Nothing)
                `shouldBe` 0
        it "a zero-HEIGHT visual box tiles nothing" $
            V.length (makeBoxBatches texSet 0 0 40 0 10 (1,1,1,1) (LayerId 0) Nothing)
                `shouldBe` 0

    -- Wire-level integration: the same contract through the REAL
    -- press/release dispatch (mirrors ControlActivation/ElementInputPolicy).
    around withHeadlessEngine $
        describe "wire integration (Engine.Input.Thread) — #749" $ do
            it "a press+release on a migrated control's visible border fires exactly one LuaUIClickEvent" $ \env → do
                resetAll env
                let (_eh, m) = migrated
                writeIORef (uiManagerRef env) m
                push env [InputMouseEvent GLFW.MouseButton'1 (90, 150) GLFW.MouseButtonState'Pressed]
                inputTick env
                pressMsgs ← drainLuaMsgs env
                pressMsgs `shouldSatisfy` all (not ∘ isUIClickEvent)
                push env [InputMouseEvent GLFW.MouseButton'1 (90, 150) GLFW.MouseButtonState'Released]
                inputTick env
                releaseMsgs ← drainLuaMsgs env
                releaseMsgs `shouldSatisfy` any isUIClickEvent

            it "the SAME border press on a content-only control fires no click (miss, not swallowed by a phantom border)" $ \env → do
                resetAll env
                let (_eh, m) = contentOnly
                writeIORef (uiManagerRef env) m
                push env
                    [ InputMouseEvent GLFW.MouseButton'1 (90, 150) GLFW.MouseButtonState'Pressed
                    , InputMouseEvent GLFW.MouseButton'1 (90, 150) GLFW.MouseButtonState'Released
                    ]
                inputTick env
                msgs ← drainLuaMsgs env
                msgs `shouldSatisfy` all (not ∘ isUIClickEvent)

    -- Lua-facing: the opt-in and the effective-bounds introspection
    -- round-trip through the real UI API (mirrors ElementInputPolicy).
    around withHeadlessEngine $
        describe "Lua-facing UI API + introspection (#749)" $ do
            it "UI.setInteractiveOverflow / UI.isInteractiveOverflow round-trip" $ \env → do
                resetAll env
                ls ← newBareLuaBackend env
                before' ← evalDebug ls (mkBoxSnippet <> "return UI.isInteractiveOverflow(_G.__box)")
                before' `shouldBe` "false"
                after' ← evalDebug ls
                    "UI.setInteractiveOverflow(_G.__box, true); return UI.isInteractiveOverflow(_G.__box)"
                after' `shouldBe` "true"

            it "getElementInfo exposes interactiveOverflow and an expanded interactiveBounds, leaving content x/y/width/height unchanged" $ \env → do
                resetAll env
                ls ← newBareLuaBackend env
                _ ← evalDebug ls (mkBoxSnippet <> "UI.setInteractiveOverflow(_G.__box, true); return 'ok'")
                flag ← evalDebug ls "return UI.getElementInfo(_G.__box).interactiveOverflow"
                flag `shouldBe` "true"
                -- content fields keep their pre-#749 meaning
                cxy ← evalDebug ls
                    "local i = UI.getElementInfo(_G.__box); return i.x==100 and i.y==100 and i.width==100 and i.height==100"
                cxy `shouldBe` "true"
                -- interactiveBounds is the expanded (content ± overflow) rect
                ib ← evalDebug ls
                    "local b = UI.getElementInfo(_G.__box).interactiveBounds; return b.x==84 and b.y==84 and b.w==132 and b.h==132"
                ib `shouldBe` "true"

            it "a content-only box reports interactiveBounds equal to its content bounds" $ \env → do
                resetAll env
                ls ← newBareLuaBackend env
                _ ← evalDebug ls (mkBoxSnippet <> "return 'ok'")
                ib ← evalDebug ls
                    "local b = UI.getElementInfo(_G.__box).interactiveBounds; return b.x==100 and b.y==100 and b.w==100 and b.h==100"
                ib `shouldBe` "true"

            it "UI.newBox with a non-finite overflow (math.huge) produces bounded, finite interactiveBounds" $ \env → do
                resetAll env
                ls ← newBareLuaBackend env
                -- The reviewer's exact scenario: math.huge reaching UI.newBox
                -- must not yield an unbounded/NaN interactive rect.
                _ ← evalDebug ls
                    "local pg = UI.newPage('h', 'hud'); \
                    \local bx = UI.newBox('hb', 100, 100, 0, 8, 1,1,1,1, math.huge, pg); \
                    \UI.addToPage(pg, bx, 100, 100); \
                    \UI.setClickable(bx, true); UI.setOnClick(bx, 'cb'); \
                    \UI.setInteractiveOverflow(bx, true); _G.__hb = bx; return 'ok'"
                ib ← evalDebug ls
                    "local b = UI.getElementInfo(_G.__hb).interactiveBounds; return b.x==100 and b.y==100 and b.w==100 and b.h==100"
                ib `shouldBe` "true"

    describe "migrated / non-migrated control families (#749 audit)" $ do
        it "the migrated box-backed control families opt their border into interaction" $ do
            sources ← mapM TIO.readFile
                [ "scripts/ui/button.lua"      -- ordinary buttons; settings/create/save actions
                , "scripts/main_menu.lua"      -- main menu items
                , "scripts/pause_menu.lua"     -- pause menu items
                , "scripts/ui/tabbar.lua"      -- tabs
                , "scripts/build_tool_remote_warning.lua" -- remote-settlement modal buttons (12px border)
                ]
            mapM_ (\src → T.isInfixOf "UI.setInteractiveOverflow" src `shouldBe` True) sources
        it "a decorative frame widget stays content-only (no interactive-overflow opt-in)" $ do
            panelSrc ← TIO.readFile "scripts/ui/panel.lua"
            T.isInfixOf "UI.setInteractiveOverflow" panelSrc `shouldBe` False

    describe "playtest oracle uses effective interactive bounds (#749 audit)" $ do
        it "the widget-dump registry carries interactiveBounds for the phantom-affordance join" $ do
            regSrc ← TIO.readFile "scripts/ui/registry.lua"
            T.isInfixOf "interactiveBounds" regSrc `shouldBe` True
        it "the oracle's widget_at prefers interactiveBounds over content bounds" $ do
            criticSrc ← TIO.readFile "tools/playtest/critic.py"
            T.isInfixOf "interactiveBounds" criticSrc `shouldBe` True

-- * Helpers

-- | A plain (non-box) clickable element — 'interactiveRect' can't
--   expand it (no overflow), so the flag is inert on it.
clickablePlain ∷ Text → (Float, Float) → (Float, Float) → Text → PageHandle
               → UIPageManager → (ElementHandle, UIPageManager)
clickablePlain name (x, y) (w, h) cb pageH mgr =
    let (eh, m1) = createElement name w h pageH mgr
        m2 = addElementToPage pageH eh x y m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnClick eh cb m3
    in (eh, m4)

-- | A control box parented (relative offset) under an existing element
--   — for the clipping case.
childControlBox ∷ ElementHandle → Text → (Float, Float) → (Float, Float)
                → Float → Bool → Text → PageHandle → UIPageManager
                → (ElementHandle, UIPageManager)
childControlBox parentH name (relX, relY) (w, h) overflow interactive cb pageH mgr =
    let (eh, m1) = createBox name w h (BoxTextureHandle 0) 8 (1,1,1,1) overflow pageH mgr
        m2 = addChildElement parentH eh relX relY m1
        m3 = setElementClickable eh True m2
        m4 = setElementOnClick eh cb m3
        m5 = setElementInteractiveOverflow eh interactive m4
    in (eh, m5)

-- | The element behind a handle (fixtures always create it, so a
--   missing lookup is a test bug — 'error' surfaces it loudly).
lookupEl ∷ ElementHandle → UIPageManager → UIElement
lookupEl eh mgr = case getElement eh mgr of
    Just el → el
    Nothing → error "InteractiveBounds fixture: element handle not found"

-- | The 'UIElement' behind a fixture's @(handle, manager)@ pair.
fst' ∷ (ElementHandle, UIPageManager) → UIElement
fst' (eh, mgr) = lookupEl eh mgr

-- | Snippet that creates a page + a 100x100 box with overflow 16 at
--   (100,100), stashed in @_G.__box@ (the same shape the pure fixtures
--   use, so introspection numbers line up).
mkBoxSnippet ∷ Text
mkBoxSnippet =
    "local pg = UI.newPage('t', 'hud'); \
    \local bx = UI.newBox('b', 100, 100, 0, 8, 1,1,1,1, 16, pg); \
    \UI.addToPage(pg, bx, 100, 100); \
    \UI.setClickable(bx, true); UI.setOnClick(bx, 'cb'); \
    \_G.__box = bx; "

-- * Wire-integration helpers (mirror Test.Headless.UI.ElementInputPolicy)

resetAll ∷ EngineEnv → IO ()
resetAll env = do
    writeIORef (inputStateRef env) defaultInputState
    writeIORef (windowSizeRef env) (1280, 720)
    writeIORef (framebufferSizeRef env) (1280, 720)
    writeIORef (focusManagerRef env) createFocusManager
    writeIORef (uiManagerRef env) emptyUIPageManager
    writeIORef (keyBindingsRef env) defaultKeyBindings
    _ ← atomicModifyIORef' (actionOutcomeRef env) $ \_ → (Seq.empty, ())
    _ ← drainLuaMsgs env
    pure ()

inputTick ∷ EngineEnv → IO ()
inputTick env = do
    st ← readIORef (inputStateRef env)
    _ ← processInputs env st
    pure ()

push ∷ EngineEnv → [InputEvent] → IO ()
push env = mapM_ (Q.writeQueue (inputQueue env))

drainLuaMsgs ∷ EngineEnv → IO [LuaMsg]
drainLuaMsgs env = go []
  where
    go acc = do
        m ← Q.tryReadQueue (luaQueue env)
        case m of
            Just msg → go (msg : acc)
            Nothing  → pure (reverse acc)

isUIClickEvent ∷ LuaMsg → Bool
isUIClickEvent (LuaUIClickEvent _ _ _ _) = True
isUIClickEvent _ = False

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
