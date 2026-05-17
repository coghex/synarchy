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
    | UnitDrink !UnitId
        -- ^ no-op unless the unit is Idle. Plays the drinking anim,
        --   blocks movement, and auto-transitions back to Idle after
        --   the anim's duration. Stat/inventory effects are applied
        --   Lua-side BEFORE issuing this command (see scripts/unit_ai
        --   drink action) — the command is purely state + anim.
    | UnitPickup !UnitId
        -- ^ Same shape as UnitDrink, for the canteen-refill "picking
        --   up" animation. Engine handles only state + anim; effect
        --   (canteen fill) is applied Lua-side at action start.
    | UnitBowDown !UnitId
        -- ^ Begin the source-drinking sequence: BowingDown →
        --   Crouching → StandingUp → Idle. Engine pre-computes all
        --   three timers from the def's bow_down anim length and a
        --   fixed crouch duration. During Crouching, the Lua-side
        --   unit_resources script regens hydration at a high rate.
    deriving (Show)
