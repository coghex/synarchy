{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Sim.Types
    ( UnitSimState(..)
    , MoveTarget(..)
    , UnitActivity(..)
    , Direction(..)
    , UnitThreadState(..)
    , emptyUnitThreadState
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Unit.Types (UnitId(..))

data UnitSimState = UnitSimState
    { usRealX   ∷ !Float          -- true position X
    , usRealY   ∷ !Float          -- true position Y
    , usGridZ   ∷ !Int            -- current Z (from surface lookup)
    , usTarget  ∷ !(Maybe MoveTarget)
    , usState   ∷ !UnitActivity
    , usFacing  ∷ !Direction
    } deriving (Show, Eq)

data MoveTarget = MoveTarget
    { mtTargetX ∷ !Float
    , mtTargetY ∷ !Float
    , mtSpeed   ∷ !Float          -- tiles per second
    } deriving (Show, Eq)

data UnitActivity = Idle | Walking
    deriving (Show, Eq)

data Direction = DirN | DirNE | DirE | DirSE | DirS | DirSW | DirW | DirNW
    deriving (Show, Eq)

data UnitThreadState = UnitThreadState
    { utsSimStates ∷ !(HM.HashMap UnitId UnitSimState)
    } deriving (Show, Eq)

emptyUnitThreadState ∷ UnitThreadState
emptyUnitThreadState = UnitThreadState
    { utsSimStates = HM.empty
    }
