{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Discrete-control pointer activation (#745, Phase B child B1 of
--   #741): the release-fires-not-press contract for ordinary UI
--   controls (buttons, checkboxes, toggles, tabs, list rows, dropdown
--   controls/options, scroll arrows, hand-built menu controls), built
--   on Phase A's #742 (modal ownership) and #743 (element-level
--   pointer policy).
--
--   Before #745 every discrete control fired its callback the instant
--   'UI.InputOwnership.routePointer' resolved a press to it
--   ('Engine.Input.Thread.Mouse' queued @LuaUIClickEvent@ immediately).
--   That made press-drag-away-to-cancel impossible and left the
--   release with nothing to decide. #745 splits the gesture in two:
--   a press against a discrete (non-drag) control now only records a
--   'PendingActivation' — no callback fires yet — and 'resolveActivation'
--   decides at the MATCHING release whether it still activates.
--
--   The decision re-runs the exact SAME routing primitive the press
--   used ('UI.InputOwnership.routePointer') against the manager state
--   AT RELEASE TIME, and only activates when it resolves to the same
--   element. This one re-check is what carries the cancellation cases
--   that are still live at release: hidden ('ueVisible' pruning in
--   'UI.Manager.Query.topHitBy'), deleted (the handle no longer
--   resolves), disabled (@UI.setClickable(el,false)@ drops the active
--   callback → 'RouteBlocked'), detached (removed from every page's
--   tree → not hit), a modal now covering the point (the modal's own
--   element wins the hit test instead), and a plain drag-outside-and-
--   release-outside (the point simply misses). Two SEPARATE,
--   independent checks (#745 review round 12) catch an interruption
--   when it's REVERTED before release, since by then routing looks
--   identical to press time again:
--
--     * 'paPageEpoch'/'UI.Types.upmPageEpoch' — bumped by
--       'UI.Manager.Page.hidePage'/'showPage' for ANY page, so the
--       pressed control's OWN page hiding/showing, or a SEPARATE
--       modal/menu page appearing then disappearing over the point,
--       both cancel even when reverted by release. Deliberately
--       GLOBAL: page-level visibility is route-affecting everywhere,
--       not just for controls the page owns.
--     * 'paChain'/'UI.Types.ueRouteEpoch' — a snapshot of the pressed
--       element's own epoch AND every ANCESTOR's epoch (walking
--       'UI.Types.ueParent' pointers), taken at press time and
--       re-derived at release. Hiding/disabling/detaching the pressed
--       element OR any real ancestor changes one of those epochs and
--       so cancels, even when reverted — but an UNRELATED element's
--       own mutation (a decorative hover-highlight sibling, say) is
--       invisible to this chain and never poisons the activation.
--       Deliberately scoped to the ONE chain, not global: a round
--       10/11 attempt at a single manager-wide epoch covering every
--       element mutation broke real production hover-highlight code
--       (@scripts/ui/toggle.lua@'s @onHoverEnter@/@onHoverLeave@,
--       @scripts/ui/list.lua@'s @setHoveredSlot@), which legitimately
--       toggles a sibling/child decoration's visibility during an
--       ordinary press-drag-out-return-inside gesture — none of which
--       is an ancestor of the control being hovered.
--
--   Nothing about this module is Vulkan/window/Lua-dependent, so the
--   decision itself is fully Hspec-testable in isolation — see
--   @Test.Headless.UI.ControlActivation@.
--
--   Drag-activation controls (slider knobs, scrollbar thumbs) never
--   enter this flow at all — see 'UI.Types.ueDragActivation' — they
--   keep firing immediately on press exactly as before #745, since a
--   drag gesture already starts from that press.
module UI.ControlActivation
  ( PendingActivation(..)
  , ActivationOutcome(..)
  , activationOutcomeName
  , beginActivation
  , resolveActivation
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import UI.Types
import UI.InputOwnership (PointerKind(..), InputRoute(..), routePointer)

-- | Captured at press time for a discrete control whose route resolved
--   to 'UI.InputOwnership.RouteElement' and whose hit element is NOT
--   drag-activation ('UI.Types.ueDragActivation'). Carries just enough
--   to re-run the same routing decision at release — the press-time
--   callback name is deliberately NOT stored, since 'resolveActivation'
--   always fires whichever callback the FRESH release-time routing
--   resolves (a mid-press @UI.setOnClick@ reassignment fires the
--   current callback, not a stale one).
--   #745 review round 12: 'paPageEpoch'/'paChain' additionally capture
--   'UI.Types.upmPageEpoch' and the pressed element's ancestor-chain
--   epochs at press time — see 'resolveActivation'.
data PendingActivation = PendingActivation
  { paElement   ∷ ElementHandle
  , paKind      ∷ PointerKind
  , paPageEpoch ∷ Int
  , paChain     ∷ [(ElementHandle, Int)]
  } deriving (Eq, Show)

-- | The release's verdict. 'Activate' carries the (possibly refreshed)
--   callback name to fire; 'Cancel' carries a short, stable reason for
--   diagnostics (F4's @aoReason@ — see 'Engine.Input.Thread.Mouse').
data ActivationOutcome
  = Activate ElementHandle Text
  | Cancel Text
  deriving (Eq, Show)

-- | F4's @aoOutcome@ vocabulary is a closed, existing set
--   ("accepted" | "rejected" | "partial" | "noop" | "deadclick",
--   'Engine.ActionOutcome.ActionOutcome') — a canceled activation is a
--   real, recognized gesture that was explicitly refused, i.e.
--   "rejected", not a novel outcome string.
activationOutcomeName ∷ ActivationOutcome → Text
activationOutcomeName (Activate _ _) = "accepted"
activationOutcomeName (Cancel _)     = "rejected"

-- | Walk from 'h' up through 'UI.Types.ueParent' pointers, collecting
--   each ancestor's (including 'h' itself, first) handle and
--   'UI.Types.ueRouteEpoch' — see 'resolveActivation'. Stops at the
--   root (no parent) or a dangling/deleted handle (an empty tail, same
--   as 'UI.Manager.Hierarchy.addChildElement's own cycle guard depth
--   cap — this manager's parent chains are always kept acyclic and
--   shallow, so the cap is only a defensive backstop here).
ancestorChain ∷ ElementHandle → UIPageManager → [(ElementHandle, Int)]
ancestorChain h0 mgr = go (64 ∷ Int) h0
  where
    go depth h
        | depth ≤ 0 = []
        | otherwise = case Map.lookup h (upmElements mgr) of
            Nothing → []
            Just el → (h, ueRouteEpoch el) : case ueParent el of
                Just p  → go (depth - 1) p
                Nothing → []

-- | Begin tracking a press against a discrete control. Callers gate on
--   'UI.Types.ueDragActivation' before reaching this — a drag-
--   activation control's press fires immediately and never produces a
--   'PendingActivation' at all. Captures 'UI.Types.upmPageEpoch' and
--   the pressed element's 'ancestorChain' (#745 review round 12) so
--   'resolveActivation' can detect an intervening route-affecting
--   mutation to the element, one of its ancestors, or any page, even
--   if it's reverted before release.
beginActivation ∷ PointerKind → ElementHandle → UIPageManager → PendingActivation
beginActivation kind h mgr =
    PendingActivation h kind (upmPageEpoch mgr) (ancestorChain h mgr)

-- | Resolve a pending activation against the CURRENT manager state at
--   the matching release. Dragging outside and releasing outside
--   cancels; returning inside before release restores activation,
--   because only the FINAL release POSITION is ever consulted for
--   that check — intermediate movement has no bearing on the
--   decision. A route-affecting STATE change to the pressed element,
--   one of its ancestors, or any page is different: #745 review round
--   12 — the issue's "returning inside before release may restore
--   pending activation" carve-out is scoped to position only, so
--   either check below cancels permanently, even if fully reverted by
--   release time (hide→show, disable→enable, detach→re-add, page
--   hide→show all leave routing looking exactly as it did at press,
--   but must still cancel). An UNRELATED element's own mutation
--   (never an ancestor of 'paElement', never a page-level change)
--   affects neither check and so never cancels — see the module
--   header for why that distinction matters. Each pending activation
--   is consumed by exactly one call to this function (the caller
--   removes it from its pending map either way), so a release
--   activates at most one control and callback once.
resolveActivation ∷ (Float, Float) → UIPageManager → PendingActivation → ActivationOutcome
resolveActivation releasePos mgr (PendingActivation h kind pageEpoch chain)
    | upmPageEpoch mgr ≢ pageEpoch =
        Cancel "a page appeared or disappeared during the press"
    | ancestorChain h mgr ≢ chain =
        Cancel "control was invalidated during the press"
    | otherwise =
        case routePointer kind releasePos mgr of
            RouteElement h' cb | h' ≡ h → Activate h' cb
            RouteElement _ _  → Cancel "released over a different control"
            RouteConsumedNoHandler h' _ | h' ≡ h → Cancel "no handler for this gesture"
            RouteBlocked h' | h' ≡ h → Cancel "control no longer input-eligible"
            _ → Cancel "released outside the control"
