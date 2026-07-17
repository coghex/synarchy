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
--   release-outside (the point simply misses). A SECOND, independent
--   check ('paEpoch'/'UI.Types.upmRouteEpoch', #745 review round 10)
--   catches an interruption when it's REVERTED before release — hidden
--   then re-shown, disabled then re-enabled, detached then re-attached,
--   a SEPARATE modal/menu page appearing then disappearing over the
--   point, or an ANCESTOR of the pressed element hidden then re-shown
--   — all of which the live re-check alone would miss, since by
--   release time routing looks identical to press time again. The
--   epoch is deliberately GLOBAL (bumped by any route-affecting
--   mutation anywhere in the manager) rather than scoped to the
--   pressed element or its own page — a narrower, per-element/per-page
--   attempt (review round 9) still missed the separate-modal and
--   ancestor cases, since neither mutation ever touches the pressed
--   element or its page directly. Nothing about this module is Vulkan/
--   window/Lua-dependent, so the decision itself is fully
--   Hspec-testable in isolation — see @Test.Headless.UI.
--   ControlActivation@.
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
--   #745 review round 10: 'paEpoch' additionally captures
--   'UI.Types.upmRouteEpoch' at press time — see 'resolveActivation'.
data PendingActivation = PendingActivation
  { paElement ∷ ElementHandle
  , paKind    ∷ PointerKind
  , paEpoch   ∷ Int
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

-- | Begin tracking a press against a discrete control. Callers gate on
--   'UI.Types.ueDragActivation' before reaching this — a drag-
--   activation control's press fires immediately and never produces a
--   'PendingActivation' at all. Captures the manager's current
--   'UI.Types.upmRouteEpoch' (#745 review round 10) so
--   'resolveActivation' can detect an intervening route-affecting
--   mutation anywhere, even if it's reverted before release.
beginActivation ∷ PointerKind → ElementHandle → UIPageManager → PendingActivation
beginActivation kind h mgr = PendingActivation h kind (upmRouteEpoch mgr)

-- | Resolve a pending activation against the CURRENT manager state at
--   the matching release. Dragging outside and releasing outside
--   cancels; returning inside before release restores activation,
--   because only the FINAL release POSITION is ever consulted for
--   that check — intermediate movement has no bearing on the
--   decision. A route-affecting STATE change ANYWHERE in the manager
--   (the pressed element hidden/disabled/detached, its page hidden/
--   shown, a separate modal/menu page appearing, an ancestor hidden)
--   is different: #745 review round 10 — the issue's "returning
--   inside before release may restore pending activation" carve-out
--   is scoped to position only, so any such interruption cancels
--   permanently via the 'paEpoch'/'UI.Types.upmRouteEpoch' check
--   below, even if fully reverted by release time (every one of those
--   mutations can leave routing looking exactly as it did at press,
--   but must still cancel). Each pending activation is consumed by
--   exactly one call to this function (the caller removes it from its
--   pending map either way), so a release activates at most one
--   control and callback once.
resolveActivation ∷ (Float, Float) → UIPageManager → PendingActivation → ActivationOutcome
resolveActivation releasePos mgr (PendingActivation h kind epoch)
    | upmRouteEpoch mgr ≢ epoch =
        Cancel "control was invalidated during the press"
    | otherwise =
        case routePointer kind releasePos mgr of
            RouteElement h' cb | h' ≡ h → Activate h' cb
            RouteElement _ _  → Cancel "released over a different control"
            RouteConsumedNoHandler h' _ | h' ≡ h → Cancel "no handler for this gesture"
            RouteBlocked h' | h' ≡ h → Cancel "control no longer input-eligible"
            _ → Cancel "released outside the control"
