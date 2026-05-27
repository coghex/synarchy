{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Command.Types
    ( UnitCommand(..)
    ) where

import UPrelude
import Unit.Types (UnitId(..))
import Unit.Sim.Types (Pose(..))

data UnitCommand
    = UnitSpawn !UnitId !Text !Float !Float !Int !Text
        -- ^ pre-allocated ID, defName, gridX, gridY, gridZ, factionId.
        --   factionId is the spawn-time-only faction tag (no def-level
        --   default); "player" for player-controlled units, "wildlife"
        --   for everything else. Used by the combat layer for
        --   hostile/friendly checks.
    | UnitDestroy !UnitId
    | UnitTeleport !UnitId !Float !Float !(Maybe Int)
        -- ^ unitId, gridX, gridY, optional gridZ (Nothing = surface lookup)
    | UnitMoveTo !UnitId !Float !Float !Float
        -- ^ unitId, targetX, targetY, speed (tiles per second)
    | UnitStop !UnitId
    | UnitCollapse !UnitId
        -- ^ Snap pose to Collapsed (no fall animation yet — deferred).
    | UnitRevive !UnitId
        -- ^ No-op unless the unit is in Collapsed pose. Snaps pose to
        --   Standing. Will eventually chain reverse transitions
        --   Collapsed → Crawling → Crouching → Standing once those
        --   assets exist.
    | UnitKill !UnitId
        -- ^ Permanent. Snaps pose to Dead, clears all in-flight state
        --   (target, path, timers). Issued by Lua when a survival
        --   resource crosses its death threshold (hydration < 5 %) or
        --   when stamina drains to zero. Dead units ignore all
        --   subsequent commands and never revive.
    | UnitDrink !UnitId
        -- ^ no-op unless the unit is Idle. Plays the drinking anim
        --   (currently keyed on the standing-drink state), blocks
        --   movement, and auto-transitions back to Idle. Stat/inventory
        --   effects are applied Lua-side BEFORE issuing this command.
    | UnitEat !UnitId
        -- ^ Same shape as UnitDrink, for the eating animation. Keyed
        --   on the <pose>-eat state. Nutrition + inventory mutation
        --   are applied Lua-side before issuing this command; the
        --   engine only handles state + anim duration.
    | UnitPickup !UnitId
        -- ^ Same shape as UnitDrink, for the canteen-refill pickup
        --   animation. Engine handles state + anim only; the fill
        --   effect is applied Lua-side at action start.
    | UnitTransitionTo !UnitId !Pose !Int
        -- ^ Initiate a pose transition. The Int is the frame stride
        --   (1 = normal, 2 = every-other-frame, etc.) — used when
        --   chaining multi-pose descents so the player doesn't wait
        --   through every frame of every transition. Duration scales
        --   inversely with stride.
        --
        --   Resolves the state key <currentPose>-to-<targetPose>
        --   against state_animations to pick the anim; missing assets
        --   yield a 0-duration transition that completes on the next
        --   tick. While transitioning, movement orders are ignored.
    deriving (Show)
