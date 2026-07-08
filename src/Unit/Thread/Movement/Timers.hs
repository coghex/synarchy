{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Timed pose/activity expirations: the Drinking/Eating/Picking/get-up
--   auto-return-to-Idle checks, and the generic TransitioningTo commit
--   (incl. the climb/fall/leap landing routing and post-transition
--   chain handoff).
module Unit.Thread.Movement.Timers
    ( handleGetUp
    , handleDrinkExpiry
    , handleEatExpiry
    , handlePickupExpiry
    , handleTransitionExpiry
    , chainStepDuration
    ) where

import UPrelude
import Unit.Sim.Types

-- | Fall knockdown recovery. A non-lethal fall lands the unit in the
--   Collapsed pose with a self-timed `usGetUpAt`. Once the down/landing
--   transition has finished (usState back to Idle) and the timer is
--   reached, stand the unit up and clear the timer (one-shot).
--
--   This is INTENTIONALLY independent of the resource revive gate in
--   unit_resources.lua: a fall knockdown recovers on its own clock, not
--   when the unit happens to be ≥50% blood/stamina/hydration. If the
--   unit is also genuinely exhausted/thirsty/bleeding, the Lua survival
--   tick or the combat wound tick will re-collapse it next tick — but as
--   a SURVIVAL collapse (no getup timer), correctly resource-gated and
--   now legible in the status panel.
handleGetUp ∷ Double → UnitSimState → UnitSimState
handleGetUp now us = case (usPose us, usGetUpAt us, usState us) of
    (Collapsed, Just t, Idle) | now ≥ t →
        us { usPose = Standing, usState = Idle, usGetUpAt = Nothing }
    _ → us

-- | When a Drinking timer expires, snap back to Idle.
handleDrinkExpiry ∷ Double → UnitSimState → UnitSimState
handleDrinkExpiry now us = case usDrinkUntil us of
    Just t | usState us ≡ Drinking ∧ now ≥ t →
        us { usState = Idle, usDrinkUntil = Nothing }
    _ → us

-- | When an Eating timer expires, snap back to Idle.
handleEatExpiry ∷ Double → UnitSimState → UnitSimState
handleEatExpiry now us = case usEatUntil us of
    Just t | usState us ≡ Eating ∧ now ≥ t →
        us { usState = Idle, usEatUntil = Nothing }
    _ → us

-- | When a Picking timer expires, snap back to Idle.
handlePickupExpiry ∷ Double → UnitSimState → UnitSimState
handlePickupExpiry now us = case usPickupUntil us of
    Just t | usState us ≡ Picking ∧ now ≥ t →
        us { usState = Idle, usPickupUntil = Nothing }
    _ → us

-- | When a pose transition timer expires, commit the target pose.
--
--   If the unit has a chained transition queued in usPostTransition,
--   start the next one immediately — used by the climb sequence to
--   roll Climbing → Crawling → Standing in one shot without the AI
--   having to orchestrate it. AI-side logic remains responsible for
--   one-off chains (e.g. source-drinking unit transitions to
--   Crouching, drinks, then issues TransitionTo Standing).
--
--   Note: this is a pure state transition — UnitTransitionTo is what
--   reads the next anim's duration. We can't read anim data from a
--   pure tick. Solution: leave usTransitionUntil empty for the
--   chained step and let the AI / a follow-up command set it. For
--   the climb chain specifically, we set the duration here based on
--   the climb anim asset names: the durations are short enough
--   (~0.5–1 s for pull-up / stand-up) that we can pick a conservative
--   default and have UnitTransitionTo overwrite if called again.
handleTransitionExpiry ∷ Double → UnitSimState → UnitSimState
handleTransitionExpiry now us = case (usState us, usTransitionUntil us) of
    (TransitioningTo target, Just t) | now ≥ t →
        let -- Climb chain handoff:
            --
            --   * End of Climbing transition (target == Climbing):
            --     snap xy to the cliff edge (usClimbToTile's xy)
            --     and Z to the top. Tiny snap since edgeX/edgeY are
            --     within ~0.05 tiles of the climb's frozen base xy.
            --     Pullup + standup then play at this fixed anchor.
            --
            --   * End of Crawling transition (post-pullup): nothing
            --     to snap — xy is already at the cliff edge from
            --     the previous step. Clear climb fields here so the
            --     standup phase doesn't keep climbToTile around.
            wasClimbing = usPose us ≡ Climbing
            snapped = case (target, usClimbToTile us, usFallToTile us) of
                (Climbing, Just (_, _, tz), _) →
                    -- Wall-climb phase done. The unit is hanging at the
                    -- climb-top over the BASE xy; the pullup (Crawling)
                    -- phase slides it up + forward onto the ledge
                    -- (tickPullup). So DON'T snap xy/realZ to the ledge
                    -- here — leave them where the climb left them and
                    -- only commit the logical tile-z to the top. Stamp
                    -- the dz climbed onto the XP field tickAllMovement
                    -- drains into the "climbing" skill.
                    let dz = case usClimbFromTile us of
                            Just (_, _, fromZ) →
                                fromIntegral (abs (tz - fromZ)) ∷ Float
                            Nothing → 0
                    in us { usGridZ = tz
                          , usPendingClimbXP = usPendingClimbXP us + dz
                          }
                (Falling, _, Just (tx, ty, tz)) →
                    us { usRealX = tx, usRealY = ty
                       , usGridZ = tz, usRealZ = fromIntegral tz }
                _ → us
            -- Clear climb fields after the pullup (Crawling
            -- transition while wasClimbing). The Climbing transition
            -- end keeps them set in case any future tick needs them.
            clearClimb = case target of
                Crawling | wasClimbing → True
                _                      → False
            -- Fall landing. With the physics injury model, EVERY fall (a
            -- drop ≥ fallTriggerDz — a 1-z step never enters the fall
            -- transition) becomes a knockdown: play the Collapsed landing
            -- anim, then sit knocked down until usGetUpAt. The drop
            -- magnitude is stamped onto usPendingFallDrop for
            -- tickAllMovement, which runs Unit.Fall to turn it into a SET
            -- of fracture/concussion wounds and sizes the getup stun from
            -- the worst of them. Death is NOT decided here — a tall fall
            -- kills by inflicting a lethal (vital severity ≥1) injury that
            -- the Lua injury tick acts on (death emerges from injuries).
            -- A LEAP (usJumpApex set) lands on its feet: it shares the
            -- Falling transition + snap, but skips the fall's knockdown +
            -- injury and stands the unit straight back up.
            isLeap        = isJust (usJumpApex us)
            fellAndLanded = target ≡ Falling ∧ not isLeap
            leapLanded    = target ≡ Falling ∧ isLeap
            fallDrop = case (usFallFromTile us, usFallToTile us) of
                (Just (_, _, fz), Just (_, _, tz)) → fz - tz
                _                                  → 0
            (chainAfter, clearFall, pendingDrop)
              | fellAndLanded = ([Collapsed], True, Just fallDrop)
              -- A leap touches down then recovers to standing: hold the
              -- landing frame and chain Falling→Standing, which plays the
              -- `falling-to-standing` (landing) anim — same shape as a
              -- fall's Falling→Collapsed, but no knockdown/injury.
              | leapLanded    = ([Standing], True, Nothing)
              | otherwise     = ( usPostTransition snapped, False
                                , usPendingFallDrop snapped )
            landedPose
              | fellAndLanded = Falling   -- held on the collapse landing frame
              | leapLanded    = Falling   -- held on the landing frame; chain stands it up
              | otherwise     = target
            usPosed = snapped { usPose            = landedPose
                              , usState           = Idle
                              , usTransitionUntil = Nothing
                              , usClimbFromTile   = if clearClimb
                                                    then Nothing
                                                    else usClimbFromTile snapped
                              , usClimbToTile     = if clearClimb
                                                    then Nothing
                                                    else usClimbToTile snapped
                              , usClimbStartTime  = if clearClimb
                                                    then Nothing
                                                    else usClimbStartTime snapped
                              , usFallFromTile    = if clearFall
                                                    then Nothing
                                                    else usFallFromTile snapped
                              , usFallToTile      = if clearFall
                                                    then Nothing
                                                    else usFallToTile snapped
                              , usPostTransition  = chainAfter
                              -- Drop magnitude for tickAllMovement's injury
                              -- pass; usGetUpAt is set there once the worst
                              -- injury (and thus the stun) is known.
                              , usPendingFallDrop = pendingDrop
                              -- Leap is over once it lands (cleared whether
                              -- it was a leap or a fall).
                              , usJumpApex        = Nothing
                              }
        in case usPostTransition usPosed of
            (next : rest) →
                -- Start chained transition with a placeholder duration.
                -- A future enhancement is to look up anim duration via
                -- a snapshot — for the climb's pullup + stand-up steps,
                -- 0.8s each is a reasonable default until that lands.
                usPosed
                    { usState            = TransitioningTo next
                    , usTransitionUntil  = Just (now + chainStepDuration)
                    , usPostTransition   = rest
                    }
            [] → usPosed
    _ → us

-- | Default duration for a chained pose transition fired from
--   handleTransitionExpiry. The pure tick path doesn't have anim
--   data on hand, so we pick a value that matches the typical
--   pullup / stand-up pace. Acolyte's climb_pullup and the reversed
--   standing_to_crawling are both ~0.8 s, so 0.8 s is on the nose.
chainStepDuration ∷ Double
chainStepDuration = 0.8
