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
    { usRealX   ∷ !Float
    , usRealY   ∷ !Float
    , usGridZ   ∷ !Int
    , usTarget  ∷ !(Maybe MoveTarget)
    , usState   ∷ !UnitActivity
    , usFacing  ∷ !Direction
    } deriving (Show, Eq)

data MoveTarget = MoveTarget
    { mtTargetX ∷ !Float
    , mtTargetY ∷ !Float
    , mtSpeed   ∷ !Float
    } deriving (Show, Eq)

data UnitActivity = Idle | Walking
    deriving (Show, Eq)

data UnitThreadState = UnitThreadState
    { utsSimStates ∷ !(HM.HashMap UnitId UnitSimState)
    } deriving (Show, Eq)

emptyUnitThreadState ∷ UnitThreadState
emptyUnitThreadState = UnitThreadState
    { utsSimStates = HM.empty
    }
