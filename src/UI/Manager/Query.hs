{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Manager.Query
  ( getElementAbsolutePosition
  , isEffectivelyVisible
  , elementText
  , getPageElements
  , getElementChildren
  , isPointInElement
  , findClickableElementAt
  , findClickableAncestor
  , findElementAt
  , findElementAtExcept
  , findRightClickableElementAt
  , topHitBy
  , hitsAtPointBy
  , elementBlocksPointer
  , elementCapturesScroll
  , isElementPointerBlocking
  , isElementScrollCapturing
  , isElementDragActivation
  , elementPaintKey
  , elementPaintOrder
  , paintTraversalOrder
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.List (sortOn, elemIndex)
import UI.Types
import UI.Manager.Page (getVisiblePages)

-- * Queries

getElementAbsolutePosition ∷ ElementHandle → UIPageManager → Maybe (Float, Float)
getElementAbsolutePosition handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → Nothing
        Just elem → Just $ computeAbsolutePos (0 ∷ Int) elem
  where
    -- Depth cap: belt-and-suspenders against parent cycles (which
    -- addChildElement refuses to create) — this runs on the render
    -- and input threads, where an unbounded walk would freeze the
    -- engine.
    computeAbsolutePos depth element =
        let (ex, ey) = uePosition element
            (px, py) = case ueParent element of
                _ | depth ≥ 64 → (0, 0)
                Nothing → (0, 0)
                Just parentHandle →
                    case Map.lookup parentHandle (upmElements mgr) of
                        Nothing → (0, 0)
                        Just parent → computeAbsolutePos (depth + 1) parent
        in (px + ex, py + ey)

-- | True iff this element AND every ancestor up to the page root has
--   'ueVisible' set — i.e. it's actually rendered right now, matching
--   'hitsAtPointBy' below (an invisible element prunes its whole
--   subtree from both rendering and hit-testing, so a visible child of
--   a hidden parent is still not "on screen"). A bare 'ueVisible'
--   check on the element alone misses that case.
isEffectivelyVisible ∷ ElementHandle → UIPageManager → Bool
isEffectivelyVisible handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → False
        Just elem → computeVisible (0 ∷ Int) elem
  where
    computeVisible depth element =
        ueVisible element ∧
        case ueParent element of
            _ | depth ≥ 64 → True
            Nothing → True
            Just parentHandle →
                case Map.lookup parentHandle (upmElements mgr) of
                    Nothing → True
                    Just parent → computeVisible (depth + 1) parent

-- | Best-effort visible text for an element: its own text if it's a
--   text element, otherwise the first direct child that is (the
--   box-plus-centered-label-child shape used throughout the UI kit —
--   see e.g. button.lua, main_menu.lua's raw menu-item boxes,
--   tabbar.lua's tabs). Deliberately shallow (direct children only) so
--   it can't wander into an unrelated grandchild's caption.
elementText ∷ UIElement → UIPageManager → Maybe Text
elementText element mgr = case ueRenderData element of
    RenderText style → Just (utsText style)
    _ → firstChildText (ueChildren element)
  where
    firstChildText [] = Nothing
    firstChildText (h:hs) = case Map.lookup h (upmElements mgr) of
        Just child → case ueRenderData child of
            RenderText style → Just (utsText style)
            _ → firstChildText hs
        Nothing → firstChildText hs

getPageElements ∷ PageHandle → UIPageManager → [UIElement]
getPageElements pageHandle mgr =
    case Map.lookup pageHandle (upmPages mgr) of
        Nothing → []
        Just page → concatMap collectElements (upRootElements page)
  where
    collectElements handle =
        case Map.lookup handle (upmElements mgr) of
            Nothing → []
            Just elem → elem : concatMap collectElements (ueChildren elem)

getElementChildren ∷ ElementHandle → UIPageManager → [UIElement]
getElementChildren handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → []
        Just elem → mapMaybe (`Map.lookup` upmElements mgr) (ueChildren elem)

-- * Click Detection

-- | Check if a point is inside an element's bounds (in screen coordinates)
isPointInElement ∷ (Float, Float) → UIElement → UIPageManager → Bool
isPointInElement (px, py) element mgr =
    if not (ueVisible element) then False
    else case getElementAbsolutePosition (ueHandle element) mgr of
        Nothing → False
        Just (ex, ey) →
            let (w, h) = ueSize element
            in px ≥ ex ∧ px ≤ (ex + w) ∧
               py ≥ ey ∧ py ≤ (ey + h)

-- | Walk every visible element in paint order, yielding the elements
--   that contain the point together with their paint key (page band +
--   accumulated element zIndex — exactly the key 'UI.Render' draws
--   with, see 'uiLayerBand'). All hit-test queries below share this
--   walk, so the element you SEE on top is the element the cursor
--   interacts with. Deliberately does NOT clip children to parent
--   bounds — the renderer doesn't either (dropdown option lists
--   extend past their display box). An invisible element prunes its
--   whole subtree, matching the renderer.
--
--   @pageOk@ is a plain filter here, not a modal-boundary decision —
--   callers that need the #742 modal-input-exclusive boundary (a miss
--   on the boundary page must not fall through to a lower one) go
--   through 'UI.InputOwnership.routePointer', which computes a scoped
--   @pageOk@ from 'UI.InputOwnership.pagesInScope' and passes it in
--   here/'topHitBy' unchanged.
hitsAtPointBy ∷ (UIPage → Bool) → (UIElement → Bool) → (Float, Float)
              → UIPageManager → [(ElementHandle, Int)]
hitsAtPointBy pageOk elemOk pos mgr =
    concatMap perPage (filter pageOk (getVisiblePages mgr))
  where
    perPage page =
        let band = uiLayerBand (upLayer page) (upZIndex page)
        in concatMap (go band) (upRootElements page)
    go base h = case Map.lookup h (upmElements mgr) of
        Nothing → []
        Just el
            | not (ueVisible el) → []
            | otherwise →
                let key  = base + ueZIndex el
                    self = if elemOk el ∧ isPointInElement pos el mgr
                           then [(h, key)]
                           else []
                    kids = sortOn (childZIndex mgr) (ueChildren el)
                in self ⧺ concatMap (go key) kids

childZIndex ∷ UIPageManager → ElementHandle → Int
childZIndex mgr h = maybe 0 ueZIndex (Map.lookup h (upmElements mgr))

-- | The topmost hit: highest paint key wins; at equal keys the
--   later-painted element wins (a later sibling paints over an
--   earlier one), which the fold's @≥@-replacement encodes since
--   'hitsAtPointBy' yields hits in paint order.
topHitBy ∷ (UIPage → Bool) → (UIElement → Bool) → (Float, Float)
         → UIPageManager → Maybe ElementHandle
topHitBy pageOk elemOk pos mgr =
    fst ⊚ foldl' step Nothing (hitsAtPointBy pageOk elemOk pos mgr)
  where
    step acc (h, k) = case acc of
        Just (_, k') | k' > k → acc
        _                     → Just (h, k)

findClickableElementAt ∷ (Float, Float) → UIPageManager → Maybe (ElementHandle, Text)
findClickableElementAt pos mgr = do
    h  ← topHitBy (const True) clickOk pos mgr
    el ← Map.lookup h (upmElements mgr)
    cb ← ueOnClick el
    pure (h, cb)
  where
    -- Elements WITHOUT a click callback don't block clicks: the click
    -- falls through to the topmost element that has one.
    clickOk el = ueClickable el ∧ isJust (ueOnClick el)

-- | Find the nearest ancestor (or self) that has an onClick callback
findClickableAncestor ∷ ElementHandle → UIPageManager → Maybe (ElementHandle, Text)
findClickableAncestor handle mgr = go handle
  where
    go h = case Map.lookup h (upmElements mgr) of
        Nothing → Nothing
        Just elem → case ueOnClick elem of
            Just cb → Just (h, cb)
            Nothing → case ueParent elem of
                Just parentHandle → go parentHandle
                Nothing → Nothing

-- | Find the topmost visible element at a point (paint order: page
--   band + accumulated zIndex, later-painted wins ties).
findElementAt ∷ (Float, Float) → UIPageManager → Maybe ElementHandle
findElementAt = findElementAtExcept Set.empty

-- | Like 'findElementAt', but skips elements that belong to any page
--   in the given ignore set. Used by the tooltip subsystem to avoid
--   hit-testing its own transient page (which would otherwise hijack
--   the hover and create flicker).
findElementAtExcept ∷ Set.Set PageHandle → (Float, Float) → UIPageManager
                    → Maybe ElementHandle
findElementAtExcept ignored pos mgr =
    topHitBy pageOk sized pos mgr
  where
    pageOk p = upHandle p `Set.notMember` ignored
    -- Zero-size elements (pure containers) are not hover targets.
    sized el = let (w, h) = ueSize el in w > 0 ∧ h > 0

-- | Like findClickableElementAt but looks at ueOnRightClick instead.
findRightClickableElementAt ∷ (Float, Float) → UIPageManager → Maybe (ElementHandle, Text)
findRightClickableElementAt pos mgr = do
    h  ← topHitBy (const True) clickOk pos mgr
    el ← Map.lookup h (upmElements mgr)
    cb ← ueOnRightClick el
    pure (h, cb)
  where
    clickOk el = ueClickable el ∧ isJust (ueOnRightClick el)

-- | #743: the EFFECTIVE pointer-blocking predicate — true when this
--   element consumes left/right/middle pointer input, whether or not
--   any callback fires. ORs the explicit 'ueBlocksPointer' opt-in with
--   the pre-existing rule that a clickable control with a registered
--   left- OR right-click callback blocks by default (so plain
--   'UI.setClickable' + 'UI.setOnClick'/'UI.setOnRightClick' keeps
--   blocking exactly as before #743 with 'ueBlocksPointer' left at its
--   default 'False'). A control with only a right-click callback
--   still blocks a LEFT click over it (consumed, no fake callback) —
--   blocking applies per-ELEMENT, not per-button, unless the element
--   opts out entirely (no flag, no callback).
elementBlocksPointer ∷ UIElement → Bool
elementBlocksPointer el = ueBlocksPointer el
    ∨ (ueClickable el ∧ (isJust (ueOnClick el) ∨ isJust (ueOnRightClick el)))

-- | #743: the scroll-capture predicate — purely the explicit
--   'ueCapturesScroll' opt-in. Unlike pointer-blocking, nothing about
--   a registered click callback implies scroll capture: the two
--   policies are independent, per the #743 contract.
elementCapturesScroll ∷ UIElement → Bool
elementCapturesScroll = ueCapturesScroll

-- | The paint/hit-test ordering key for one element — its page's
--   'uiLayerBand' plus the accumulated zIndex of every ancestor up to
--   the page root, i.e. exactly the @key@ 'hitsAtPointBy'/'topHitBy'
--   compare when picking a topmost hit (computed here bottom-up from a
--   single handle instead of top-down while walking the tree; the sum
--   is the same either way). 'Nothing' for a detached (never added to
--   a page) or deleted element/page, mirroring
--   'getElementAbsolutePosition'.
--
--   NOT a total order on its own: ordinary siblings sharing a band and
--   zIndex (the common case — most elements never set an explicit
--   zIndex) tie. 'topHitBy' breaks that tie by picking the
--   LATER-painted element ('hitsAtPointBy''s fold — see its haddock);
--   'elementPaintOrder' is that same tiebreak exposed standalone, for
--   an offline caller that needs router-parity even among tied keys.
--   Exposed to Lua as @paintKey@
--   ('Engine.Scripting.Lua.API.UI.Property.pushElementInfoTable') so an
--   offline oracle (@ui.dumpWidgets@, #783) can rank overlapping
--   controls exactly like a real click would resolve them, without
--   re-deriving the page/element tree itself — paired with
--   @paintOrder@ ('elementPaintOrder') to resolve ties the same way.
elementPaintKey ∷ ElementHandle → UIPageManager → Maybe Int
elementPaintKey handle mgr = do
    el ← Map.lookup handle (upmElements mgr)
    page ← Map.lookup (uePage el) (upmPages mgr)
    pure (uiLayerBand (upLayer page) (upZIndex page) + accumulatedZIndex (0 ∷ Int) el)
  where
    accumulatedZIndex depth element =
        ueZIndex element +
        case ueParent element of
            _ | depth ≥ 64 → 0
            Nothing → 0
            Just parentHandle → case Map.lookup parentHandle (upmElements mgr) of
                Nothing → 0
                Just parent → accumulatedZIndex (depth + 1) parent

-- | This element's position within the FULL paint/hit-test traversal
--   'hitsAtPointBy' walks — page order ('getVisiblePages'), then each
--   page's root elements in list order, then children sorted by
--   zIndex, recursively — independent of any query point (the point
--   only ever gates SELF-inclusion in 'hitsAtPointBy', never the
--   recursion/traversal order itself). A strictly later position means
--   'topHitBy' would treat it as the later-painted element at an equal
--   'elementPaintKey', so @(paintKey, paintOrder)@ compared
--   lexicographically (both descending) reproduces 'topHitBy''s exact
--   selection rule — the missing piece 'elementPaintKey' alone can't
--   provide when ordinary same-band, same-zIndex siblings tie.
--   'Nothing' for a hidden element or one pruned by a hidden ancestor
--   (mirrors 'hitsAtPointBy''s own subtree pruning — such an element
--   could never be a real hit anyway).
elementPaintOrder ∷ ElementHandle → UIPageManager → Maybe Int
elementPaintOrder handle mgr = elemIndex handle (paintTraversalOrder mgr)

-- | Every visible element, in exactly the order 'hitsAtPointBy' visits
--   them (see 'elementPaintOrder').
paintTraversalOrder ∷ UIPageManager → [ElementHandle]
paintTraversalOrder mgr = concatMap perPage (getVisiblePages mgr)
  where
    perPage page = concatMap go (upRootElements page)
    go h = case Map.lookup h (upmElements mgr) of
        Nothing → []
        Just el
            | not (ueVisible el) → []
            | otherwise →
                let kids = sortOn (childZIndex mgr) (ueChildren el)
                in h : concatMap go kids

-- | Handle-based lookup of 'elementBlocksPointer' — an unknown/deleted
-- handle never blocks.
isElementPointerBlocking ∷ ElementHandle → UIPageManager → Bool
isElementPointerBlocking h mgr =
    maybe False elementBlocksPointer (Map.lookup h (upmElements mgr))

-- | Handle-based lookup of 'elementCapturesScroll' — an unknown/deleted
-- handle never captures.
isElementScrollCapturing ∷ ElementHandle → UIPageManager → Bool
isElementScrollCapturing h mgr =
    maybe False elementCapturesScroll (Map.lookup h (upmElements mgr))

-- | #745: handle-based lookup of 'ueDragActivation' — an unknown/
--   deleted handle is never drag-activation (it degrades to the
--   ordinary discrete/deferred contract, which simply never activates
--   since the handle can't be found again at release either).
isElementDragActivation ∷ ElementHandle → UIPageManager → Bool
isElementDragActivation h mgr =
    maybe False ueDragActivation (Map.lookup h (upmElements mgr))
