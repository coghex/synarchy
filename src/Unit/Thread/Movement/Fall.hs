{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Fall physics: Z-interpolation during a Standing→Falling transition
--   (gravity-accelerated descent, or — when usJumpApex is set — a
--   leap's up-then-down arc via "Unit.Thread.Movement.Leap"), fall
--   duration from drop height, and initiating a fall off a cliff.
module Unit.Thread.Movement.Fall
    ( fallDuration
    , tickFallZ
    , startFall
    ) where

import UPrelude
import Unit.Sim.Types
import Unit.Thread.Movement.Types (fallGravityZ, vectorToDirection)
import Unit.Thread.Movement.Leap (jumpFlightTime)

-- | Mirror of tickClimbZ for falling: usRealZ lerps DOWN from the
--   top of the cliff to the landing tile's Z while the unit is in
--   TransitioningTo Falling. usGridZ stays at the top until the
--   landing snap so game-logic tile lookups treat the unit as still
--   on the upper tile mid-fall.
tickFallZ ∷ Double → UnitSimState → UnitSimState
tickFallZ now us =
    case (usState us, usFallFromTile us, usFallToTile us,
          usTransitionUntil us) of
        (TransitioningTo Falling, Just (fx, fy, fromZ), Just (tx, ty, toZ), Just untilT) →
            let remain = max 0 (untilT - now)
                fromZF = fromIntegral fromZ ∷ Float
                toZF   = fromIntegral toZ   ∷ Float
            in case usJumpApex us of
                Just apex →
                    -- LEAP: a gravity arc (rise then fall) plus horizontal
                    -- interpolation across the gap. realZ peaks `apex` above
                    -- the chord midpoint (4·p·(1−p) is 1 at p=0.5), so it
                    -- launches up, sails over, and comes down onto the target.
                    let dur  = max 0.001 (jumpFlightTime apex)
                        p    = realToFrac (clamp01 (1 - realToFrac remain / dur)) ∷ Float
                        arcZ = fromZF + (toZF - fromZF) * p + apex * 4 * p * (1 - p)
                    in us { usRealX = fx + p * (tx - fx)
                          , usRealY = fy + p * (ty - fy)
                          , usRealZ = if remain ≤ 0 then toZF else arcZ }
                Nothing →
                    -- FALL: parabolic, gravity-accelerated descent (realZ
                    -- falls as ½·g·t² → progress² in normalised time), xy
                    -- pinned at the launch and snapped at landing.
                    let dur      = max 0.001
                                       (untilT - fallStartFromUntil untilT fromZ toZ)
                        progress = clamp01 (1 - realToFrac remain / dur)
                        accel    = realToFrac progress * realToFrac progress ∷ Float
                        lerp     = fromZF + accel * (toZF - fromZF)
                    in us { usRealZ = if remain ≤ 0 then toZF else lerp }
        _ → us
  where
    clamp01 ∷ Double → Double
    clamp01 x = max 0 (min 1 x)

fallStartFromUntil ∷ Double → Int → Int → Double
fallStartFromUntil untilT fromZ toZ =
    untilT - fallDuration (abs (toZ - fromZ))

-- | Time (s) to free-fall `dropZ` z-levels from rest under gravity:
--   dropZ = ½·g·t² ⇒ t = √(2·dropZ/g). Replaces the old constant-rate
--   dropZ/fallSpeedZ — short falls start gently, tall falls build real
--   speed, and the descent matches the energy the injury model uses.
fallDuration ∷ Int → Double
fallDuration dropZ =
    realToFrac (sqrt (2 * fromIntegral (max 0 dropZ) / fallGravityZ))

-- | Initiate a fall sequence. Stops xy movement, faces the cliff
--   the unit's about to drop off, and starts a Standing→Falling
--   transition whose duration scales with the drop distance.
--   handleTransitionExpiry stamps the drop magnitude onto
--   usPendingFallDrop, which tickAllMovement feeds to the
--   `Unit.Fall` physics injury model to pick the landing outcome.
startFall
    ∷ Double              -- now
    → UnitSimState
    → ((Int, Int), Int)   -- dst tile (gx, gy) + dst Z (bottom)
    → Int                 -- src Z (top of the fall)
    → (Float, Float)      -- step direction (for facing)
    → UnitSimState
startFall now us ((dgx, dgy), dstZ) srcZ (nx, ny) =
    let dz       = srcZ - dstZ                 -- positive: drop magnitude
        duration = fallDuration dz
        srcX     = usRealX us
        srcY     = usRealY us
        -- Land on the destination tile center. Falls don't have the
        -- "land on the edge" semantic that climbs do — by the time
        -- you've hit the ground, you've cleared the cliff face.
        dstX     = fromIntegral dgx + 0.5
        dstY     = fromIntegral dgy + 0.5
    in us { usState            = TransitioningTo Falling
          , usTransitionUntil  = Just (now + duration)
          , usTransitionStride = 1
          , usFacing           = vectorToDirection nx ny
          , usFallFromTile     = Just (srcX, srcY, srcZ)
          , usFallToTile       = Just (dstX, dstY, dstZ)
          , usJumpApex         = Nothing   -- a fall, not a leap
          -- usPostTransition stays empty here; the landing outcome
          -- (set in handleTransitionExpiry from the drop magnitude)
          -- decides whether to chain into Collapsed/Standing/Dead.
          , usPostTransition   = []
          -- Clear any movement target the fall interrupted. The AI
          -- can re-issue after recovery. Without this, a unit that
          -- survives a fall would immediately try to walk back to
          -- its old goal mid-collapse anim.
          , usTarget           = Nothing
          , usLocalPath        = []
          }
