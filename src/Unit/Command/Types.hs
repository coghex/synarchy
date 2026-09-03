{-# LANGUAGE Strict #-}
module Unit.Command.Types
    ( UnitCommand(..)
    , motionCoordinateInDomain
    , motionSpeedInDomain
    ) where

import UPrelude
import Unit.Types (UnitId(..))
import Unit.Faction (Faction(..))
import Unit.Sim.Types (Pose(..))
import Unit.Pathing.Hazard (MoveHazardPolicy(..))
import World.Page.Types (WorldPageId(..))

data UnitCommand
    = UnitSpawn !UnitId !Text !Float !Float !Int !Faction !WorldPageId
        -- ^ pre-allocated ID, defName, gridX, gridY, gridZ, faction,
        --   owning world page (stamped from the active world at spawn so
        --   the unit is world-scoped, #78).
        --   The faction is spawn-time-only (no def-level default) and is
        --   already TYPED here: @unit.spawn@ parses the caller's tag at
        --   ingress (#912), so an unrecognized tag is reported once at
        --   the boundary rather than travelling as a string nobody
        --   validates. Ownership/alliance/attack questions are answered
        --   by "Unit.Faction", never by comparing two of these.
    | UnitDestroy !UnitId
    | UnitTeleport !UnitId !Float !Float !(Maybe Int)
        -- ^ unitId, gridX, gridY, optional gridZ (Nothing = surface lookup)
    | UnitMoveTo !UnitId !Float !Float !Float !MoveHazardPolicy
        -- ^ unitId, targetX, targetY, speed (tiles per second), and the
        --   route's hazard policy (#1217). The policy is EXPLICIT per
        --   request and defaults to 'FallPermitted' at the scripting
        --   boundary, so every pre-existing caller keeps today's
        --   behavior; ambient wander asks for 'FallProhibited'. A new
        --   request always REPLACES the previous one's policy along with
        --   its destination — see 'UnitSetMoveSpeed', which does not.
    | UnitSetMoveSpeed !UnitId !Float
        -- ^ unitId, speed (tiles per second). Retargets the speed of an
        --   ALREADY in-flight move without touching its destination,
        --   local path, or hazard policy — a no-op if the unit has no
        --   active move target.
        --   Lets a caller (#999's stamina-adaptive pacing) adjust the
        --   commanded pace every tick without the path-reset cost a
        --   repeated UnitMoveTo would incur.
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
    | UnitReGround !WorldPageId !Int !Int
        -- ^ Terrain under tile (gx, gy) OF THE NAMED PAGE changed
        --   (delete-tile edit / dig completion): re-snap the z of any
        --   IDLE unit of that page standing on that tile to the new
        --   surface. Moving units re-ground themselves on every tile
        --   crossing; stationary ones would otherwise keep a stale z and
        --   float mid-air over the hole. The page is carried because
        --   tiles are page-local (#1593): every emit site already
        --   operates on one known 'World.State.Types.WorldState', and
        --   without it a coordinate-matched unit on ANOTHER page got
        --   snapped to this page's surface.
    | UnitClearAll
        -- ^ Drop every unit instance + selection + sim state. Enqueued by
        --   world.destroyAll (Exit to Menu) so the clear is ordered AFTER
        --   any in-flight UnitSpawns already on this queue — clearing the
        --   manager from the world thread instead would race those spawns,
        --   which would re-insert orphans right after teardown (#58).
    deriving (Show)

-- | The domain a motion command's COORDINATE must already be in: a
--   finite 'Float' (#2290).
--
--   It lives beside the constructors it constrains, rather than at
--   either boundary, because BOTH boundaries have to agree on it: the
--   scripting verbs in "Engine.Scripting.Lua.API.Units.MotionArgs"
--   refuse an out-of-domain argument before enqueueing anything, and
--   the handlers in "Unit.Thread.Command.Lifecycle" \/
--   "Unit.Thread.Command.Motion" drop an out-of-domain command
--   defensively if one reaches them anyway. Two independently written
--   predicates could disagree about which values the pair lets
--   through; one shared predicate cannot.
--
--   Why 'Float' and not something wider: the command field IS a
--   'Float', so a caller narrows before asking. That is deliberate —
--   an ordinary finite @1.0e300@ 'Double' is 'Infinity' once narrowed,
--   and the narrowed value is the only one the simulation ever sees.
--
--   What an out-of-domain coordinate does if it gets through: 'floor'
--   does not throw in GHC and 'World.Generate.globalToChunk' maps any
--   'Int' somewhere, so nothing crashes. A NaN target instead makes
--   every step NaN, the arrival test never becomes true, and the unit
--   is stuck "moving" at a position no tile lookup can map — a state
--   that is then persisted verbatim.
motionCoordinateInDomain ∷ Float → Bool
motionCoordinateInDomain v = not (isNaN v ∨ isInfinite v)

-- | The domain a motion command's SPEED must already be in: finite AND
--   non-negative (#2290). Shared by the same two boundaries as
--   'motionCoordinateInDomain', for the same reason.
--
--   Zero is IN domain — a legitimately stalled unit (an exhausted or
--   fully encumbered one whose band multipliers collapse its gait, or
--   a @unit.getMaxSpeed@ that found no def) commands zero, and
--   refusing it would turn a survivable state into a refused order.
--   Negative is not: 'Unit.Thread.Movement.PathAdvance' steps along the
--   goal direction scaled by speed, so a negative one walks the unit
--   AWAY from its target for as long as the order stands.
motionSpeedInDomain ∷ Float → Bool
motionSpeedInDomain v = motionCoordinateInDomain v ∧ v ≥ 0
