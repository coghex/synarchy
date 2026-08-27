-- | #1694 gate: every attached UI element has exactly ONE structural
--   owner. Pure 'UI.Manager' coverage — no Vulkan, no window, no Lua
--   engine: the whole invariant lives in
--   'UI.Manager.Hierarchy.addElementToPage'/'addChildElement' and is
--   observable through the manager alone.
module Test.Headless.UI.HierarchyOwnership (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as Map
import Data.List (sort)
import UI.ControlActivation
import UI.InputOwnership (PointerKind(..))
import UI.Manager
import UI.Types

-- * Fixtures

visiblePage ∷ Text → UIPageManager → (PageHandle, UIPageManager)
visiblePage name mgr =
    let (h, m1) = createPage name LayerHUD mgr
    in (h, showPage h m1)

elemOn ∷ Text → PageHandle → UIPageManager → (ElementHandle, UIPageManager)
elemOn name pageH mgr = createElement name 100 100 pageH mgr

rootOn ∷ Text → PageHandle → UIPageManager → (ElementHandle, UIPageManager)
rootOn name pageH mgr =
    let (eh, m1) = elemOn name pageH mgr
    in (eh, addElementToPage pageH eh 0 0 m1)

childOf ∷ Text → ElementHandle → PageHandle → UIPageManager
        → (ElementHandle, UIPageManager)
childOf name parentH pageH mgr =
    let (eh, m1) = elemOn name pageH mgr
    in (eh, addChildElement parentH eh 0 0 m1)

clickableRoot ∷ Text → PageHandle → UIPageManager → (ElementHandle, UIPageManager)
clickableRoot name pageH mgr =
    let (eh, m1) = rootOn name pageH mgr
        m2 = setElementClickable eh True m1
    in (eh, setElementOnClick eh "cb" m2)

-- | The complete structural state an attachment may touch: every
--   page's root list, and every element's owner/route/position fields.
--   Requirement 1's "leaves the complete original hierarchy unchanged"
--   is asserted against this.
hierarchy ∷ UIPageManager
          → ( [(PageHandle, [ElementHandle])]
            , [(ElementHandle, (PageHandle, Maybe ElementHandle, [ElementHandle], Int, (Float, Float)))]
            )
hierarchy mgr =
    ( [ (ph, upRootElements p) | (ph, p) ← Map.toList (upmPages mgr) ]
    , [ (eh, ( uePage el, ueParent el, ueChildren el
             , ueRouteEpoch el, uePosition el ))
      | (eh, el) ← Map.toList (upmElements mgr) ]
    )

roots ∷ PageHandle → UIPageManager → [ElementHandle]
roots ph mgr = maybe [] upRootElements (getPage ph mgr)

kids ∷ ElementHandle → UIPageManager → [ElementHandle]
kids eh mgr = maybe [] ueChildren (Map.lookup eh (upmElements mgr))

pageOf ∷ ElementHandle → UIPageManager → Maybe PageHandle
pageOf eh mgr = uePage ⊚ Map.lookup eh (upmElements mgr)

parentOf ∷ ElementHandle → UIPageManager → Maybe ElementHandle
parentOf eh mgr = Map.lookup eh (upmElements mgr) ⌦ ueParent

epochOf ∷ ElementHandle → UIPageManager → Maybe Int
epochOf eh mgr = ueRouteEpoch ⊚ Map.lookup eh (upmElements mgr)

alive ∷ ElementHandle → UIPageManager → Bool
alive eh mgr = Map.member eh (upmElements mgr)

hasNoDuplicates ∷ Ord α ⇒ [α] → Bool
hasNoDuplicates xs = go (sort xs)
  where
    go (a:b:rest) = a ≢ b ∧ go (b:rest)
    go _          = True

isCancel ∷ ActivationOutcome → Bool
isCancel (Cancel _) = True
isCancel _          = False

-- | Two visible pages plus a spare page-root element on the first.
twoPages ∷ (PageHandle, PageHandle, UIPageManager)
twoPages =
    let (p1, m1) = visiblePage "page1" emptyUIPageManager
        (p2, m2) = visiblePage "page2" m1
    in (p1, p2, m2)

spec ∷ Spec
spec = do
    describe "duplicate same-owner attachment (requirement 3)" $ do
        it "re-attaching to the same page leaves exactly one root reference" $ do
            let (p1, _, m0) = twoPages
                (eh, m1) = rootOn "e" p1 m0
                m2 = addElementToPage p1 eh 7 9 m1
            roots p1 m2 `shouldBe` [eh]
            uePosition ⊚ Map.lookup eh (upmElements m2) `shouldBe` Just (7, 9)

        it "re-attaching to the same parent leaves exactly one child reference" $ do
            let (p1, _, m0) = twoPages
                (par, m1) = rootOn "parent" p1 m0
                (ch, m2)  = childOf "child" par p1 m1
                m3 = addChildElement par ch 3 4 m2
            kids par m3 `shouldBe` [ch]
            roots p1 m3 `shouldBe` [par]

    describe "relocation in all four directions (requirement 2)" $ do
        it "page root to page root" $ do
            let (p1, p2, m0) = twoPages
                (eh, m1) = rootOn "e" p1 m0
                m2 = addElementToPage p2 eh 0 0 m1
            roots p1 m2 `shouldBe` []
            roots p2 m2 `shouldBe` [eh]
            pageOf eh m2 `shouldBe` Just p2
            parentOf eh m2 `shouldBe` Nothing

        it "page root to parent" $ do
            let (p1, p2, m0) = twoPages
                (eh, m1)  = rootOn "e" p1 m0
                (par, m2) = rootOn "parent" p2 m1
                m3 = addChildElement par eh 0 0 m2
            roots p1 m3 `shouldBe` []
            roots p2 m3 `shouldBe` [par]
            kids par m3 `shouldBe` [eh]
            parentOf eh m3 `shouldBe` Just par
            pageOf eh m3 `shouldBe` Just p2

        it "parent to parent" $ do
            let (p1, _, m0) = twoPages
                (a, m1)  = rootOn "a" p1 m0
                (b, m2)  = rootOn "b" p1 m1
                (ch, m3) = childOf "child" a p1 m2
                m4 = addChildElement b ch 0 0 m3
            kids a m4 `shouldBe` []
            kids b m4 `shouldBe` [ch]
            parentOf ch m4 `shouldBe` Just b

        it "parent to page root" $ do
            let (p1, p2, m0) = twoPages
                (a, m1)  = rootOn "a" p1 m0
                (ch, m2) = childOf "child" a p1 m1
                m3 = addElementToPage p2 ch 0 0 m2
            kids a m3 `shouldBe` []
            roots p2 m3 `shouldBe` [ch]
            parentOf ch m3 `shouldBe` Nothing
            pageOf ch m3 `shouldBe` Just p2

        it "detaches a reference the recorded fields do not name" $ do
            -- removeFromPage on a NESTED child clears ueParent while
            -- leaving the handle in the parent's ueChildren, so the
            -- recorded owner and the real one disagree. A detach that
            -- trusted the fields would leave the stale reference.
            let (p1, p2, m0) = twoPages
                (a, m1)  = rootOn "a" p1 m0
                (ch, m2) = childOf "child" a p1 m1
                m3 = removeFromPage p1 ch m2
                m4 = addElementToPage p2 ch 0 0 m3
            kids a m2 `shouldBe` [ch]
            kids a m3 `shouldBe` [ch]
            kids a m4 `shouldBe` []
            roots p2 m4 `shouldBe` [ch]

    describe "atomic rejection (requirements 1 and 10)" $ do
        it "an unknown destination page changes nothing" $ do
            let (p1, _, m0) = twoPages
                (eh, m1) = rootOn "e" p1 m0
                m2 = addElementToPage (PageHandle 999) eh 5 5 m1
            hierarchy m2 `shouldBe` hierarchy m1

        it "an unknown element handle changes nothing" $ do
            let (p1, p2, m0) = twoPages
                (_, m1) = rootOn "e" p1 m0
                m2 = addElementToPage p2 (ElementHandle 999) 5 5 m1
            hierarchy m2 `shouldBe` hierarchy m1

        it "an unknown parent handle changes nothing" $ do
            let (p1, _, m0) = twoPages
                (eh, m1) = rootOn "e" p1 m0
                m2 = addChildElement (ElementHandle 999) eh 5 5 m1
            hierarchy m2 `shouldBe` hierarchy m1

        it "an unknown child handle changes nothing" $ do
            let (p1, _, m0) = twoPages
                (par, m1) = rootOn "parent" p1 m0
                m2 = addChildElement par (ElementHandle 999) 5 5 m1
            hierarchy m2 `shouldBe` hierarchy m1

        it "an ancestor cycle is refused without detaching the child" $ do
            let (p1, _, m0) = twoPages
                (a, m1)  = rootOn "a" p1 m0
                (b, m2)  = childOf "b" a p1 m1
                (c, m3)  = childOf "c" b p1 m2
                -- a is an ancestor of c: attaching a under c cycles.
                m4 = addChildElement c a 0 0 m3
            hierarchy m4 `shouldBe` hierarchy m3
            roots p1 m4 `shouldBe` [a]
            kids b m4 `shouldBe` [c]

        it "a STRUCTURAL descendant is refused as a parent even with a cleared ueParent" $ do
            -- removeFromPage on a nested child clears its ueParent
            -- while leaving it in the parent's ueChildren, so the
            -- recorded ancestor chain no longer names the real one.
            -- Attaching A under its own structural descendant B would
            -- close an A→B→A ueChildren loop that every downward walk
            -- (render, getPageElements, hitsAtPointBy,
            -- paintTraversalOrder, deleteElementTree) follows forever.
            let (p1, _, m0) = twoPages
                (a, m1) = rootOn "a" p1 m0
                (b, m2) = childOf "b" a p1 m1
                m3 = removeFromPage p1 b m2
                m4 = addChildElement b a 0 0 m3
            parentOf b m3 `shouldBe` Nothing
            kids a m3 `shouldBe` [b]
            hierarchy m4 `shouldBe` hierarchy m3
            roots p1 m4 `shouldBe` [a]
            kids b m4 `shouldBe` []
            -- The downward traversals still terminate and stay
            -- exactly-once.
            map ueHandle (getPageElements p1 m4) `shouldBe` [a, b]
            paintTraversalOrder m4 `shouldBe` [a, b]

        it "a deeper structural descendant is refused as a parent too" $ do
            let (p1, _, m0) = twoPages
                (a, m1) = rootOn "a" p1 m0
                (b, m2) = childOf "b" a p1 m1
                (c, m3) = childOf "c" b p1 m2
                m4 = removeFromPage p1 c m3
                m5 = addChildElement c a 0 0 m4
            hierarchy m5 `shouldBe` hierarchy m4
            kids c m5 `shouldBe` []
            map ueHandle (getPageElements p1 m5) `shouldBe` [a, b, c]

        it "child-as-its-own-parent is refused without detaching it" $ do
            let (p1, _, m0) = twoPages
                (a, m1)  = rootOn "a" p1 m0
                (ch, m2) = childOf "child" a p1 m1
                m3 = addChildElement ch ch 0 0 m2
            hierarchy m3 `shouldBe` hierarchy m2
            kids a m3 `shouldBe` [ch]

    describe "recursive page propagation (requirement 4)" $ do
        it "addElementToPage reassigns the whole moved subtree" $ do
            let (p1, p2, m0) = twoPages
                (r, m1)  = rootOn "root" p1 m0
                (c, m2)  = childOf "child" r p1 m1
                (g, m3)  = childOf "grandchild" c p1 m2
                m4 = addElementToPage p2 r 0 0 m3
            map (`pageOf` m4) [r, c, g] `shouldBe` map Just [p2, p2, p2]

        it "addChildElement reassigns the whole moved subtree" $ do
            let (p1, p2, m0) = twoPages
                (r, m1)   = rootOn "root" p1 m0
                (c, m2)   = childOf "child" r p1 m1
                (g, m3)   = childOf "grandchild" c p1 m2
                (par, m4) = rootOn "host" p2 m3
                m5 = addChildElement par r 0 0 m4
            map (`pageOf` m5) [r, c, g] `shouldBe` map Just [p2, p2, p2]

        it "hiding the OLD page does not clear focus held by a moved descendant" $ do
            let (p1, p2, m0) = twoPages
                (r, m1) = rootOn "root" p1 m0
                (c, m2) = childOf "child" r p1 m1
                m3 = addElementToPage p2 r 0 0 m2
                m4 = setControlFocus c (setElementFocus c m3)
                m5 = hidePage p1 m4
            upmGlobalFocus m5 `shouldBe` Just c
            upmControlFocus m5 `shouldBe` Just c

    describe "deleting the old page (requirement 5)" $
        it "leaves the moved root and its descendants alive" $ do
            let (p1, p2, m0) = twoPages
                (r, m1) = rootOn "root" p1 m0
                (c, m2) = childOf "child" r p1 m1
                (g, m3) = childOf "grandchild" c p1 m2
                (s, m4) = rootOn "stays" p1 m3
                m5 = addElementToPage p2 r 0 0 m4
                m6 = deletePage p1 m5
            map (`alive` m6) [r, c, g] `shouldBe` [True, True, True]
            alive s m6 `shouldBe` False
            roots p2 m6 `shouldBe` [r]

    describe "activation epochs (requirements 7 and 8)" $ do
        it "attaching a fresh element is epoch-neutral" $ do
            let (p1, _, m0) = twoPages
                (eh, m1) = elemOn "e" p1 m0
                m2 = addElementToPage p1 eh 0 0 m1
            epochOf eh m2 `shouldBe` Just 0
            upmPageEpoch m2 `shouldBe` upmPageEpoch m1

        it "re-attaching a DETACHED element is epoch-neutral on the attach side" $ do
            let (p1, _, m0) = twoPages
                (eh, m1) = rootOn "e" p1 m0
                m2 = removeFromPage p1 eh m1     -- the detach itself bumps
                m3 = addElementToPage p1 eh 0 0 m2
            epochOf eh m3 `shouldBe` epochOf eh m2

        it "re-attaching to the SAME owner is epoch-neutral" $ do
            let (p1, _, m0) = twoPages
                (eh, m1)  = rootOn "e" p1 m0
                (par, m2) = rootOn "parent" p1 m1
                (ch, m3)  = childOf "child" par p1 m2
                m4 = addElementToPage p1 eh 1 1 m3
                m5 = addChildElement par ch 1 1 m4
            epochOf eh m5 `shouldBe` Just 0
            epochOf ch m5 `shouldBe` Just 0
            upmPageEpoch m5 `shouldBe` upmPageEpoch m3

        it "an actual owner change bumps the relocated ROOT exactly once" $ do
            let (p1, p2, m0) = twoPages
                (r, m1) = rootOn "root" p1 m0
                (c, m2) = childOf "child" r p1 m1
                m3 = addElementToPage p2 r 0 0 m2
            epochOf r m3 `shouldBe` Just 1
            epochOf c m3 `shouldBe` Just 0
            upmPageEpoch m3 `shouldBe` upmPageEpoch m2

        it "cancels a press interrupted by a page-root to page-root move" $ do
            let (p1, p2, m0) = twoPages
                (eh, m1) = clickableRoot "btn" p1 m0
                pending  = beginActivation PointerLeftClick eh m1
                m2 = addElementToPage p2 eh 0 0 m1
            -- Release routing still resolves to the same element at
            -- the same point; only the epoch bump can cancel it.
            resolveActivation (50, 50) m1 pending `shouldBe` Activate eh "cb"
            resolveActivation (50, 50) m2 pending `shouldSatisfy` isCancel

        it "cancels a press when an ANCESTOR of the pressed element relocates" $ do
            let (p1, p2, m0) = twoPages
                (r, m1)  = rootOn "root" p1 m0
                (ch, m2) = childOf "btn" r p1 m1
                m3 = setElementOnClick ch "cb" (setElementClickable ch True m2)
                pending = beginActivation PointerLeftClick ch m3
                m4 = addElementToPage p2 r 0 0 m3
            resolveActivation (50, 50) m3 pending `shouldBe` Activate ch "cb"
            resolveActivation (50, 50) m4 pending `shouldSatisfy` isCancel

        it "fresh focus-ring churn on the pressed element does NOT cancel" $ do
            let (p1, _, m0) = twoPages
                (eh, m1) = clickableRoot "btn" p1 m0
                pending  = beginActivation PointerLeftClick eh m1
                (ring, m2) = createElement "ring" 4 4 p1 m1
                m3 = addChildElement eh ring 0 0 m2
            resolveActivation (50, 50) m3 pending `shouldBe` Activate eh "cb"

    describe "focus retention across relocation (requirement 9)" $ do
        it "keeps text and control focus on a relocated subtree ROOT" $ do
            let (p1, p2, m0) = twoPages
                (r, m1) = rootOn "root" p1 m0
                m2 = setControlFocus r (setElementFocus r m1)
                m3 = addElementToPage p2 r 0 0 m2
            upmGlobalFocus m3 `shouldBe` Just r
            upmControlFocus m3 `shouldBe` Just r

        it "keeps text and control focus on a relocated DESCENDANT" $ do
            let (p1, p2, m0) = twoPages
                (r, m1) = rootOn "root" p1 m0
                (c, m2) = childOf "child" r p1 m1
                m3 = setControlFocus c (setElementFocus c m2)
                (par, m4) = rootOn "host" p2 m3
                m5 = addChildElement par r 0 0 m4
            upmGlobalFocus m5 `shouldBe` Just c
            upmControlFocus m5 `shouldBe` Just c

    describe "exactly-once traversal (requirement 6)" $ do
        it "getPageElements, hitsAtPointBy and paintTraversalOrder each visit a live element once" $ do
            let (p1, p2, m0) = twoPages
                (r, m1)  = rootOn "root" p1 m0
                (c, m2)  = childOf "child" r p1 m1
                (g, m3)  = childOf "grandchild" c p1 m2
                (s, m4)  = rootOn "sibling" p1 m3
                -- Every shape the invariant has to survive: a repeated
                -- same-owner attach, a cross-page move, and a
                -- parent-to-page-root move.
                m5 = addElementToPage p1 s 0 0 m4
                m6 = addChildElement c g 0 0 m5
                m7 = addElementToPage p2 r 0 0 m6
                m8 = addElementToPage p1 g 0 0 m7
                pageHandles = map ueHandle (getPageElements p1 m8)
                              ⧺ map ueHandle (getPageElements p2 m8)
                hits = map fst (hitsAtPointBy (const True) (const True) (10, 10) m8)
                paint = paintTraversalOrder m8
            sort pageHandles `shouldBe` sort [r, c, s, g]
            hasNoDuplicates pageHandles `shouldBe` True
            hasNoDuplicates hits `shouldBe` True
            hasNoDuplicates paint `shouldBe` True
            sort paint `shouldBe` sort [r, c, s, g]
            hits `shouldSatisfy` not ∘ null
