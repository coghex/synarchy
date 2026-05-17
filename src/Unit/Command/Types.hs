{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Command.Types
    ( UnitCommand(..)
    ) where

import UPrelude
import Unit.Types (UnitId(..))
import Unit.Sim.Types (Pose(..))

data UnitCommand
    = UnitSpawn !UnitId !Text !Float !Float !Int
        -- ^ pre-allocated ID, defName, gridX, gridY, gridZ
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
    | UnitDrink !UnitId
        -- ^ no-op unless the unit is Idle. Plays the drinking anim
        --   (currently keyed on the standing-drink state), blocks
        --   movement, and auto-transitions back to Idle. Stat/inventory
        --   effects are applied Lua-side BEFORE issuing this command.
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
