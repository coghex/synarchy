{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Sim.Types
    ( UnitSimState(..)
    , MoveTarget(..)
    , UnitActivity(..)
    , Direction(..)          -- re-exported from Unit.Direction
    , UnitThreadState(..)
    , emptyUnitThreadState
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Unit.Types (UnitId(..))
import Unit.Direction (Direction(..))

data UnitSimState = UnitSimState
    { usRealX     ∷ !Float
    , usRealY     ∷ !Float
    , usGridZ     ∷ !Int
    , usTarget    ∷ !(Maybe MoveTarget)
    , usState     ∷ !UnitActivity
    , usFacing    ∷ !Direction
    -- | Ordered waypoints (continuous tile-center coords) from a
    --   local A* replan. Empty = greedy heading toward usTarget.
    --   Each waypoint pops off the front as the unit arrives.
    , usLocalPath ∷ ![(Float, Float)]
    -- | Game-time when a Reviving state should auto-transition back
    --   to Idle. Nothing in all other states. Set by the UnitRevive
    --   handler from the def's reviving-anim duration.
    , usReviveUntil ∷ !(Maybe Double)
    -- | Game-time when a Drinking state should auto-transition back
    --   to Idle. Nothing in all other states. Set by UnitDrink from
    --   the def's drinking-anim duration. Effects (hydration +,
    --   canteen fill -) are applied Lua-side at start; this clock
    --   only gates how long the animation plays + movement is blocked.
    , usDrinkUntil  ∷ !(Maybe Double)
    -- | Game-time when a Picking state should auto-transition back to
    --   Idle. Same shape as usDrinkUntil. Set by UnitPickup from the
    --   def's pickup anim duration. Used for canteen refilling.
    , usPickupUntil ∷ !(Maybe Double)
    -- | Source-drinking sequence: three timers driving the chained
    --   transitions BowingDown → Crouching → StandingUp → Idle.
    --   - usBowingUntil: when BowingDown expires (anim length).
    --   - usCrouchingUntil: when Crouching expires (fixed duration,
    --     during which the unit_resources hydration regen ticks).
    --   - usStandingUntil: when StandingUp expires (anim length).
    , usBowingUntil    ∷ !(Maybe Double)
    , usCrouchingUntil ∷ !(Maybe Double)
    , usStandingUntil  ∷ !(Maybe Double)
    } deriving (Show, Eq)

data MoveTarget = MoveTarget
    { mtTargetX ∷ !Float
    , mtTargetY ∷ !Float
    , mtSpeed   ∷ !Float
    } deriving (Show, Eq)

data UnitActivity = Idle | Walking | Collapsed | Reviving | Drinking | Picking
                  | BowingDown | Crouching | StandingUp
    deriving (Show, Eq)

data UnitThreadState = UnitThreadState
    { utsSimStates ∷ !(HM.HashMap UnitId UnitSimState)
    } deriving (Show, Eq)

emptyUnitThreadState ∷ UnitThreadState
emptyUnitThreadState = UnitThreadState
    { utsSimStates = HM.empty
    }
