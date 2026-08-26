{-# LANGUAGE Strict #-}
-- | F4 (#730/#1676): the press-time CAPTURE and release-time
--   RESOLUTION of a deferred mouse gesture's one action-outcome
--   record — split out of 'Engine.Input.Thread.Mouse' to keep that
--   module under its line budget, exactly as
--   'Engine.Input.Thread.Mouse.Activation' already is.
--
--   The two halves belong together because they are the two ends of
--   one coordinate contract (#1676): 'capturePendingUIClick' records
--   the press's framebuffer position from the geometry live at PRESS
--   dispatch, and 'resolveDeferredUIClick' spends that stored value
--   whenever the gesture resolves to the PRESS location. Only the
--   above-threshold branch — whose location is the RELEASE point —
--   converts with release-time geometry.
module Engine.Input.Thread.Mouse.Deferred
  ( uiDragThresholdPx
  , windowToFbOrRaw
  , capturePendingUIClick
  , resolveDeferredUIClick
  ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv, actionOutcomeRef)
import Engine.Core.Capability.WorldSim (WorldSimCapability(..), toWorldSimCapability)
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)
import Engine.Input.Inject (windowToFb)
import Engine.Input.Types (PendingUIClick(..))
import UI.ControlActivation (ActivationOutcome(..), activationOutcomeName)

-- | F4 (#730): window-pixel movement between a deferred press and its
--   release, at or past which the whole gesture reads as a drag
--   rather than a plain click — matches
--   @scripts/unit_drag_select.lua@'s own @DRAG_THRESHOLD@ for the
--   game-world case (both operate in the same window-coordinate
--   space, and #1676 deliberately keeps that comparison there).
uiDragThresholdPx ∷ Double
uiDragThresholdPx = 4

-- | 'Engine.Input.Inject.windowToFb' with the F4 recorders' documented
--   fallback: a degenerate viewport yields the raw window coordinate
--   rather than propagating a NaN/Infinity into a recorded outcome.
--   Shared by the press-time capture and the release-time conversion
--   so the two can never disagree about what "degenerate" means.
windowToFbOrRaw ∷ (Int, Int) → (Int, Int) → (Double, Double) → (Double, Double)
windowToFbOrRaw win fb wp = fromMaybe wp (windowToFb win fb wp)

-- | Build the deferred record for a press at window position @wp@,
--   capturing its framebuffer position from the PRESS-time geometry
--   (#1676). Pure: the caller already holds the geometry it read for
--   this event's dispatch.
capturePendingUIClick ∷ Text → Text → (Int, Int) → (Int, Int) → (Double, Double)
                      → PendingUIClick
capturePendingUIClick kind callback win fb wp = PendingUIClick
    { pucKind = kind, pucCallback = callback
    , pucPressX = fst wp, pucPressY = snd wp
    , pucPressFbX = fst fbP, pucPressFbY = snd fbP
    }
  where fbP = windowToFbOrRaw win fb wp

-- | Record the ONE outcome a deferred gesture resolves to, now that
--   the whole gesture is known — the press's original kind if the
--   release landed within 'uiDragThresholdPx' of it (a plain click, or
--   a below-threshold H1 @drag@), else @"input.drag"@.
--
--   The threshold compare stays in WINDOW pixels; only the chosen
--   LOCATION is framebuffer-space (#774). Which framebuffer position
--   that is depends on which end of the gesture was selected (#1676):
--   a drag reports its RELEASE point, converted with the release-time
--   geometry passed in here, while a click reports the press's OWN
--   press-time capture — never a reconversion of 'pucPressX'/'pucPressY'
--   under a ratio that may have moved during the hold.
--
--   @mActivation@ is #745's discrete-control decision: present only
--   for a deferred discrete control, whose F4 outcome then truthfully
--   reflects whether it actually activated. Every other route
--   (drag-activation control, camera-drag) keeps recording
--   @"accepted"@ exactly as before #745.
resolveDeferredUIClick ∷ EngineEnv → (Double, Double) → (Int, Int) → (Int, Int)
                       → Maybe ActivationOutcome → PendingUIClick → IO ()
resolveDeferredUIClick env (x, y) win fb mActivation pc = do
    gt ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    let dx = x - pucPressX pc
        dy = y - pucPressY pc
        movedPx = sqrt (dx * dx + dy * dy)
        (kind, whereX, whereY)
            | movedPx ≥ uiDragThresholdPx =
                let (rx, ry) = windowToFbOrRaw win fb (x, y)
                in ("input.drag", rx, ry)
            | otherwise = (pucKind pc, pucPressFbX pc, pucPressFbY pc)
        outcomeName = maybe "accepted" activationOutcomeName mActivation
        cancelReason = case mActivation of
            Just (Cancel reason) → Just reason
            _                    → Nothing
    pushActionOutcome (actionOutcomeRef env) ActionOutcome
        { aoTs = gt, aoKind = kind, aoOutcome = outcomeName
        , aoWhereX = Just whereX, aoWhereY = Just whereY
        , aoTarget = Nothing
        , aoRequested = Nothing, aoApplied = Nothing, aoDropped = Nothing
        , aoReason = cancelReason, aoHandler = Just (pucCallback pc)
        }
