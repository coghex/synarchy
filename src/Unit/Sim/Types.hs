{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Sim.Types
    ( UnitSimState(..)
    , MoveTarget(..)
    , UnitActivity(..)
    , Pose(..)
    , poseDepth
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
    , usPose      ∷ !Pose
    , usState     ∷ !UnitActivity
    , usFacing    ∷ !Direction
    -- | Ordered waypoints (continuous tile-center coords) from a
    --   local A* replan. Empty = greedy heading toward usTarget.
    --   Each waypoint pops off the front as the unit arrives.
    , usLocalPath ∷ ![(Float, Float)]
    -- | Game-time when a Drinking state should auto-transition back
    --   to Idle. Nothing in all other states.
    , usDrinkUntil  ∷ !(Maybe Double)
    -- | Game-time when an Eating state should auto-transition back to
    --   Idle. Nothing in all other states.
    , usEatUntil    ∷ !(Maybe Double)
    -- | Game-time when a Picking state should auto-transition back to
    --   Idle. Used for canteen refilling.
    , usPickupUntil ∷ !(Maybe Double)
    -- | Game-time when a TransitioningTo state should commit the pose
    --   change and return to Idle. Set by UnitTransitionTo from the
    --   resolved transition anim's duration; missing/T-pose anim → 0,
    --   so the transition completes on the next tick.
    , usTransitionUntil ∷ !(Maybe Double)
    -- | Frame stride for the active transition anim. 1 = normal speed.
    --   N>1 = play every Nth frame (faster, fewer visible frames). Set
    --   by UnitTransitionTo. Reset implicitly when the transition
    --   completes (the next state isn't a transition).
    , usTransitionStride ∷ !Int
    } deriving (Show, Eq)

data MoveTarget = MoveTarget
    { mtTargetX ∷ !Float
    , mtTargetY ∷ !Float
    , mtSpeed   ∷ !Float
    } deriving (Show, Eq)

-- | What pose the unit is currently *in*. Orthogonal to UnitActivity.
--   Transitions between poses are driven by `TransitioningTo`. `Dead`
--   is terminal — entered only via `UnitKill`, never via a transition.
data Pose = Standing | Crouching | Crawling | Collapsed | Dead
    deriving (Show, Eq)

-- | Depth ordering used to derive reverse playback for shared
--   transition assets. Going from a lower-depth pose to a higher-depth
--   pose is "forward" (plays the asset normally); the reverse direction
--   plays the same asset flipped via uiAnimReverse.
poseDepth ∷ Pose → Int
poseDepth Standing  = 0
poseDepth Crouching = 1
poseDepth Crawling  = 2
poseDepth Collapsed = 3
poseDepth Dead      = 4

data UnitActivity = Idle | Walking | Drinking | Eating | Picking | TransitioningTo !Pose
    deriving (Show, Eq)

data UnitThreadState = UnitThreadState
    { utsSimStates ∷ !(HM.HashMap UnitId UnitSimState)
    } deriving (Show, Eq)

emptyUnitThreadState ∷ UnitThreadState
emptyUnitThreadState = UnitThreadState
    { utsSimStates = HM.empty
    }
