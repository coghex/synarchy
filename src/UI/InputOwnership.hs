{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Page-level pointer ownership (#742): the modal-boundary contract.
--
--   A visible 'upInputExclusive' page establishes a boundary — pointer
--   input that misses every owned control on or above that page
--   cannot reach whatever paints below it (a lower menu/overlay/HUD
--   control, or the game world). Pages that aren't exclusive (HUD,
--   overlay, ordinary menus, debug/shell, and any page explicitly
--   reclassified pass-through, e.g. @scripts/popup.lua@'s notification
--   cards) are invisible to the boundary: a miss on one just continues
--   the search downward, exactly like pre-#742 behaviour with no
--   modal visible at all.
--
--   This module is pure — no Vulkan, window, or Lua engine involved —
--   so the routing decision itself is Hspec-testable in isolation; see
--   @Test.Headless.UI.InputOwnership@.
--
--   Debug/shell pass-through: 'LayerDebug' always paints (and is thus
--   always in scope for) above 'LayerModal' (see the 'UI.Types.UILayer'
--   Ord), so an owned control on the shell or a debug-layer page keeps
--   receiving input above any modal without this module treating debug
--   as a special case. @scripts/debug.lua@'s F8 overlay and
--   @scripts/debug_anim_panel.lua@ both render on a real @"debug"@
--   page (#742 review round 2 — they used to sit on @"overlay"@, below
--   'LayerModal''s band, which let their raw parallel rects claim a
--   screen position a modal was actually painted over) but their own
--   click detection runs a parallel Lua-side hit-test outside
--   'UI.Manager' entirely (@tryClaimClick@) — this module doesn't (and
--   can't) see those rects, but it doesn't need to: 'RouteMiss' is
--   still forwarded to Lua exactly as a pre-#742 miss was, so those
--   parallel paths keep getting first refusal on it regardless of any
--   modal boundary computed here. Their VALIDITY gate is decoupled from
--   modal-blocking entirely on the Lua side (@scripts/ui_manager.lua@'s
--   @isGameplayView@ vs @isGameplayInputActive@ — see
--   @scripts/debug.lua@'s @inGameplayView@), which this module has no
--   part in.
--
--   'isPointerSurfaceBlocked' extends the boundary to middle-click
--   (camera drag), which has no owned handler and no page concept of
--   its own in 'Engine.Input.Thread' — pre-#742 it swallowed on ANY
--   visible sized element; #743 narrowed that to
--   'UI.Manager.Query.elementBlocksPointer' (see below), and it
--   additionally swallows whenever a modal boundary exists at all, so
--   a gap in the modal's own layout can't leak a middle-click through
--   to panning behind it.
--
--   #743 (element-level input policy): page-level ownership above
--   decides WHICH PAGES are in scope for pointer/wheel routing; within
--   that scope, 'routePointer'/'routeScroll' further restrict the
--   search to elements that actually opt into pointer-blocking or
--   scroll-capture ('UI.Manager.Query.elementBlocksPointer' /
--   'elementCapturesScroll') rather than every clickable-with-a-
--   callback element as before. A pointer-blocking element with no
--   callback relevant to the gesture still consumes it ('RouteBlocked')
--   — no fake Lua callback fires, but the press/miss can't fall
--   through to a lower element, a lower page, or gameplay either.
--
--   'isPageInScope' is for the OTHER kind of raw Lua handler: one that
--   iterates every live widget instance regardless of page, entirely
--   outside 'routePointer'/'UI.Manager.Query' hit-testing (dropdown/
--   randbox "click outside" close-or-submit, @scripts/ui/dropdown.lua@,
--   @scripts/ui/randbox.lua@). Those still need every miss forwarded
--   (same reason as debug/shell above), but must filter out instances
--   belonging to an out-of-scope page themselves.
module UI.InputOwnership
  ( PointerKind(..)
  , InputRoute(..)
  , inputBoundaryPage
  , pagesInScope
  , isPageInScope
  , isGameplayBlocked
  , routePointer
  , routeScroll
  , isPointerSurfaceBlocked
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import UI.Types
import UI.Manager.Page (getVisiblePages)
import UI.Manager.Query (topHitBy, elementBlocksPointer, elementCapturesScroll)

-- | Which click gesture is being routed — decides which callback
--   field on an element counts as "owned" for this event. Wheel/scroll
--   is NOT a 'PointerKind' (#743): it no longer shares the click
--   callback machinery at all — see 'routeScroll'.
data PointerKind
  = PointerLeftClick
  | PointerRightClick
  deriving (Eq, Show)

-- | Outcome of routing one pointer event through the page stack.
data InputRoute
  = RouteElement ElementHandle Text
    -- ^ An owned control captured the pointer — dispatch its callback.
  | RouteConsumedNoHandler ElementHandle Text
    -- ^ Right-click landed on an ordinary left-clickable control with
    --   no right-click handler of its own: consumed (focus clears)
    --   but no right-click callback fires. Carries the control's OWN
    --   left-click callback name (its identity, for F4 outcome
    --   bookkeeping), not the fired callback — mirrors the pre-#742
    --   fallback ("no right-click handler under the cursor, but SOME
    --   clickable control is"), now scoped by the same modal boundary
    --   as everything else.
  | RouteBlocked ElementHandle
    -- ^ #743: a pointer-blocking element ('UI.Manager.Query.
    --   elementBlocksPointer') sits under the cursor but has no
    --   callback relevant to THIS gesture at all (a left-click over a
    --   right-click-only control, or any click over an element that
    --   opted into 'ueBlocksPointer' with no callback whatsoever, e.g.
    --   a scroll-capturing log panel background). Consumed — no fake
    --   Lua callback fires, and unlike 'RouteMiss' this can never fall
    --   through to a lower element, a lower page, or gameplay.
  | RouteMiss
    -- ^ No owned control captured the pointer — either nothing is
    --   there, or a modal boundary stopped the search before it could
    --   reach a lower page. The caller still forwards a miss to Lua
    --   unconditionally (debug/shell's parallel hit-test paths get
    --   first refusal there regardless of any boundary); ordinary
    --   gameplay handlers additionally consult 'isGameplayBlocked'
    --   before acting on it, so a miss that was actually stopped at a
    --   modal boundary still can't mutate the world or scroll the
    --   camera.
  deriving (Eq, Show)

-- | The topmost visible input-exclusive page — the modal boundary
--   pointer input cannot cross. 'getVisiblePages' paints bottom to
--   top, so the boundary is the LAST exclusive page in that order
--   (when two modals are visible, the more recently shown one — the
--   higher 'PageHandle' — paints on top and owns the boundary).
inputBoundaryPage ∷ UIPageManager → Maybe UIPage
inputBoundaryPage mgr = case filter upInputExclusive (getVisiblePages mgr) of
    [] → Nothing
    xs → Just (last xs)

-- | Visible pages at or above the modal boundary, in the same
--   (bottom-to-top) paint order as 'getVisiblePages'. No boundary ⇒
--   every visible page is in scope — an unrestricted global search,
--   identical to pre-#742 behaviour.
pagesInScope ∷ UIPageManager → [UIPage]
pagesInScope mgr = case inputBoundaryPage mgr of
    Nothing → getVisiblePages mgr
    Just boundary →
        dropWhile ((≢ upHandle boundary) ∘ upHandle) (getVisiblePages mgr)

-- | True when the given page is at or above the modal boundary (or
--   there is no boundary at all) — still eligible to react to pointer
--   input. #742 review round 1: a handful of raw Lua handlers
--   (dropdown/randbox "click outside closes/submits me") iterate every
--   LIVE instance regardless of which page it's on, entirely outside
--   'routePointer'/'UI.Manager.Query' hit-testing — this lets them
--   filter out instances belonging to a page the modal boundary has
--   excluded, so a click the boundary already consumed can't still
--   close a widget on a lower page while leaving same-page widgets
--   (e.g. a dropdown on the modal itself, closed by clicking elsewhere
--   on that same page) working exactly as before.
isPageInScope ∷ PageHandle → UIPageManager → Bool
isPageInScope h mgr = Set.member h (Set.fromList (map upHandle (pagesInScope mgr)))

-- | True while any visible page establishes an input-exclusive
--   boundary. Lua's gameplay-input gate
--   (@scripts/ui_manager.lua@'s @isGameplayInputActive@) folds this in
--   so keyboard shortcuts and the click/scroll gameplay fallback both
--   go inert behind a modal — mirroring how a control on/above the
--   boundary already wins the pointer routing below.
isGameplayBlocked ∷ UIPageManager → Bool
isGameplayBlocked = isJust ∘ inputBoundaryPage

-- | Pages in scope as a fast membership set — shared by 'routePointer',
--   'routeScroll', and the middle-click surface check below.
scopedPageOk ∷ UIPageManager → UIPage → Bool
scopedPageOk mgr =
    let inScope = Set.fromList (map upHandle (pagesInScope mgr))
    in \page → upHandle page `Set.member` inScope

-- | Route one pointer event through the page stack. Restricts the
--   underlying element search ('UI.Manager.Query.topHitBy') to pages
--   at or above the modal boundary (if any) — a lower page's owned
--   control, HUD button included, is invisible to the search once a
--   boundary exists, so empty modal space blocks it exactly like a
--   real control would; with no boundary the scope is every visible
--   page, so behaviour is unchanged from before #742.
--
--   #743: the element search is scoped to 'elementBlocksPointer'
--   (not just clickable-with-a-callback), so a topmost pointer-
--   blocking element with no callback still consumes the gesture
--   ('RouteBlocked') instead of falling through to a lower element,
--   callback-bearing or not. Right-click additionally falls back to
--   an ordinary left-clickable control with no right-click handler of
--   its own (pre-#742/#743 parity — 'RouteConsumedNoHandler', carrying
--   that control's left-click callback identity for F4 bookkeeping).
--
--   Firing EITHER callback still requires 'ueClickable' — pointer-
--   blocking and click-firing are independent policies, but a control
--   temporarily disabled via @UI.setClickable(el, false)@ (callback
--   left registered for later re-enabling) must not have that stale
--   callback revived just because it (or an explicit 'ueBlocksPointer'
--   opt-in) also makes it pointer-blocking; it degrades to
--   'RouteBlocked' like any other callback-less blocking element.
routePointer ∷ PointerKind → (Float, Float) → UIPageManager → InputRoute
routePointer kind pos mgr =
    case topHitBy (scopedPageOk mgr) elementBlocksPointer pos mgr of
        Nothing → RouteMiss
        Just h  → case Map.lookup h (upmElements mgr) of
            Nothing → RouteMiss
            Just el → case activeCallback kind el of
                Just cb → RouteElement h cb
                Nothing → case kind of
                    PointerRightClick → case activeCallback PointerLeftClick el of
                        Just leftCb → RouteConsumedNoHandler h leftCb
                        Nothing     → RouteBlocked h
                    PointerLeftClick  → RouteBlocked h
  where
    -- The active (fireable) callback for a gesture: 'Nothing' whenever
    -- the element isn't 'ueClickable', regardless of which callback
    -- field is populated — mirrors the pre-#743 'clickOk' gate exactly.
    activeCallback k el
        | not (ueClickable el) = Nothing
        | otherwise = case k of
            PointerLeftClick  → ueOnClick el
            PointerRightClick → ueOnRightClick el

-- | #743: route one wheel/scroll event through the page stack,
--   independent of the click callback machinery 'routePointer' uses.
--   Selects the visually topmost 'elementCapturesScroll' element in
--   scope (same modal-boundary restriction as 'routePointer' —
--   scrolling can never cross it either), reusing the same paint-order
--   'topHitBy' walk rendering/hit-testing already share: a
--   scroll-capturing container still wins over its own passive child
--   visuals (they're simply not candidates, so the walk falls through
--   to the nearest ancestor/sibling that IS), and a higher capturing
--   surface wins over an overlapping lower one. 'Nothing' means no
--   scroll-capturing surface is in scope at this point — the caller
--   falls back to gameplay (camera zoom).
routeScroll ∷ (Float, Float) → UIPageManager → Maybe ElementHandle
routeScroll pos mgr = topHitBy (scopedPageOk mgr) elementCapturesScroll pos mgr

-- | #742 review round 1: the middle-click "UI surface blocks" check
--   ('Engine.Input.Thread' — middle-click has no owned handler of its
--   own and exists purely to pan the camera). Pre-#742 this swallowed
--   on ANY visible sized element; #743 narrows the surface check to
--   'elementBlocksPointer' (unscoped — a modal boundary, if any, is
--   already folded in via 'isGameplayBlocked' below, so a purely
--   visual, pass-through element no longer blocks the camera drag on
--   its own, per #743's "a middle-click over a purely visual,
--   pass-through element must remain eligible to reach gameplay"
--   requirement — menu/panel/HUD backgrounds that should still swallow
--   middle-click opt in via 'ueBlocksPointer' or a real callback).
--   Folds in the modal boundary: once one exists, the WHOLE screen is
--   blocked for this purpose, not just the boundary page's own
--   elements — otherwise a gap in the modal's own layout (no element
--   at the exact point) would leak the middle-click through to
--   camera-drag panning behind the modal. When there's no boundary
--   this reduces to exactly the unscoped pointer-blocking surface
--   check, so behaviour outside a modal now depends only on which
--   elements opted into blocking.
isPointerSurfaceBlocked ∷ (Float, Float) → UIPageManager → Bool
isPointerSurfaceBlocked pos mgr =
    isGameplayBlocked mgr ∨ isJust (topHitBy (const True) elementBlocksPointer pos mgr)
