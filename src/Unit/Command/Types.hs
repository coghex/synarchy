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
    | UnitJump !UnitId !Int !Int
        -- ^ unitId, target tile (gx, gy). Launches a leap — a gravity arc
        --   to the target tile at the same z — if the gap is within the
        --   unit's jump reach (jumping skill + agility/strength) and it's
        --   standing. Lands standing. See Unit.Thread.Movement.startJump.
    | UnitStop !UnitId
    | UnitCollapse !UnitId
        -- ^ Snap pose to Collapsed (no fall animation yet — deferred).
    | UnitCrawl !UnitId
        -- ^ Snap pose to Crawling — a conscious unit that can no longer
        --   walk (legs broken / a leg severed) drops to a crawl instead of
        --   collapsing. Unlike Collapsed, a Crawling unit can still be
        --   commanded to move (it crawls slowly toward the goal). Preserves
        --   any in-flight move target so a unit maimed mid-stride keeps
        --   crawling where it was headed. UnitRevive stands it back up.
    | UnitRevive !UnitId
        -- ^ No-op unless the unit is Collapsed or Crawling. Snaps pose to
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
    | UnitReGround !Int !Int
        -- ^ Terrain under tile (gx, gy) changed (delete-tile edit /
        --   dig completion): re-snap the z of any IDLE unit standing
        --   on that tile to the new surface. Moving units re-ground
        --   themselves on every tile crossing; stationary ones would
        --   otherwise keep a stale z and float mid-air over the hole.
    deriving (Show)
