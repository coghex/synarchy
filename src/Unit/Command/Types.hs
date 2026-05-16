{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Command.Types
    ( UnitCommand(..)
    ) where

import UPrelude
import Unit.Types (UnitId(..))

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
        -- ^ play the collapse anim and hold; unit cannot be moved
        --   until further state changes are added
    | UnitRevive !UnitId
        -- ^ no-op unless the unit is Collapsed. Plays the reviving
        --   state anim (typically the collapse anim in reverse) and
        --   auto-transitions back to Idle when it finishes.
    deriving (Show)
