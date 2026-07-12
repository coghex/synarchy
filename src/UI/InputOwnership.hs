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
--   as a special case. The F8 overlay's own click detection additionally
--   runs a parallel Lua-side hit-test outside 'UI.Manager' entirely
--   (@scripts/debug.lua@'s @tryClaimClick@, and @scripts/
--   debug_anim_panel.lua@'s equivalent) — this module doesn't (and
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
--   visible sized element; here it additionally swallows whenever a
--   modal boundary exists at all, so a gap in the modal's own layout
--   can't leak a middle-click through to panning behind it.
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
  , isPointerSurfaceBlocked
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import UI.Types
import UI.Manager.Page (getVisiblePages)
import UI.Manager.Query (topHitBy, findElementAt)

-- | Which pointer gesture is being routed — decides which callback
--   field on an element counts as "owned" for this event. Wheel reuses
--   'ueOnClick', matching the pre-#742 convention
--   ('Engine.Input.Thread' already targeted @findClickableElementAt@
--   for scroll misses).
data PointerKind
  = PointerLeftClick
  | PointerRightClick
  | PointerWheel
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

-- | Route one pointer event through the page stack. Restricts the
--   underlying element search ('UI.Manager.Query.topHitBy') to pages
--   at or above the modal boundary (if any) — a lower page's owned
--   control, HUD button included, is invisible to the search once a
--   boundary exists, so empty modal space blocks it exactly like a
--   real control would; with no boundary the scope is every visible
--   page, so behaviour is unchanged from before #742. The callback
--   field checked depends on 'PointerKind'; right-click additionally
--   falls back to an ordinary left-clickable control with no handler
--   of its own (pre-#742 parity), still scoped to the same boundary.
routePointer ∷ PointerKind → (Float, Float) → UIPageManager → InputRoute
routePointer kind pos mgr = case hitBy primaryCallback of
    Just (h, cb) → RouteElement h cb
    Nothing → case kind of
        PointerRightClick → case hitBy ueOnClick of
            Just (h, leftCb) → RouteConsumedNoHandler h leftCb
            Nothing           → RouteMiss
        _ → RouteMiss
  where
    inScope = Set.fromList (map upHandle (pagesInScope mgr))
    pageOk page = upHandle page `Set.member` inScope

    primaryCallback = case kind of
        PointerLeftClick  → ueOnClick
        PointerRightClick → ueOnRightClick
        PointerWheel      → ueOnClick

    hitBy callbackOf = do
        h  ← topHitBy pageOk (clickOk callbackOf) pos mgr
        el ← Map.lookup h (upmElements mgr)
        cb ← callbackOf el
        pure (h, cb)

    clickOk callbackOf el = ueClickable el ∧ isJust (callbackOf el)

-- | #742 review round 1: the middle-click "any visible UI surface
--   blocks" check ('Engine.Input.Thread' — middle-click has no owned
--   handler of its own and exists purely to pan the camera, so ANY
--   sized element under the point already swallows it, pre-#742).
--   Folds in the modal boundary: once one exists, the WHOLE screen is
--   blocked for this purpose, not just the boundary page's own
--   elements — otherwise a gap in the modal's own layout (no element
--   at the exact point) would leak the middle-click through to
--   camera-drag panning behind the modal. When there's no boundary
--   this reduces to exactly the original (unscoped) surface check, so
--   behaviour is unchanged from before #742 in the common case.
isPointerSurfaceBlocked ∷ (Float, Float) → UIPageManager → Bool
isPointerSurfaceBlocked pos mgr =
    isGameplayBlocked mgr ∨ isJust (findElementAt pos mgr)
