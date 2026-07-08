{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
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
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Unit.Types (UnitId(..))
import Unit.Direction (Direction(..))

data UnitSimState = UnitSimState
    { usRealX     ∷ !Float
    , usRealY     ∷ !Float
    , usGridZ     ∷ !Int
    -- | Continuous vertical position, used by the renderer for
    --   smooth motion during climbs (and any future feature that
    --   needs sub-tile Z). Game logic — tile cost lookups, slope
    --   queries, range checks — still uses the integer usGridZ.
    --   Outside of an active climb, usRealZ == fromIntegral usGridZ.
    , usRealZ     ∷ !Float
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
    -- | Queue of pose transitions to fire after the current one
    --   completes. Used for compound sequences like climb-completion
    --   (Climbing → Crawling → Standing). handleTransitionExpiry
    --   pops the head and starts a new TransitioningTo for it.
    --   Empty list = no chain pending.
    , usPostTransition ∷ ![Pose]
    -- | Origin position the climb started from. Kept set for the
    --   entire Climbing→Crawling→Standing chain so the pullup
    --   transition (Climbing→Crawling) can interpolate xy smoothly
    --   from this base to usClimbToTile's center. Without it, the
    --   unit would jump to the top tile center the moment the
    --   Climbing transition ends — visible teleport before pullup.
    , usClimbFromTile ∷ !(Maybe (Float, Float, Int))
    -- | Destination tile center + top Z. usClimbFromTile lerps to
    --   this during the pullup. Cleared when the Crawling transition
    --   completes (pullup done, unit landed on top).
    , usClimbToTile ∷ !(Maybe (Float, Float, Int))
    -- | Game-time when the Climbing transition started. Used by
    --   tickClimbZ to compute progress through a per-unit-speed
    --   climb without baking the speed constant into the
    --   interpolation math.
    , usClimbStartTime ∷ !(Maybe Double)
    -- | If set, the game-time at which a slip is scheduled to occur
    --   during the active climb. tickAllMovement watches this and
    --   converts the Climbing transition into a Falling transition
    --   from the unit's current usRealZ when now ≥ slipAt. Rolled
    --   at climb-start; only set when the slip roll actually failed.
    , usClimbSlipAt ∷ !(Maybe Double)
    -- | Origin (top) tile of a fall, set when a descent step is
    --   converted into a Falling transition. The unit's xy stays
    --   pinned at this position for the duration of the fall; only
    --   usRealZ interpolates downward.
    , usFallFromTile ∷ !(Maybe (Float, Float, Int))
    -- | Destination (bottom) tile of a fall. At end of the fall
    --   transition, xy snaps to this position and Z lands at this
    --   tile's terrain z. handleTransitionExpiry then derives the
    --   drop magnitude (fromZ → toZ), stamps it onto usPendingFallDrop,
    --   and tickAllMovement runs the Unit.Fall injury model to route
    --   the outcome (walk-off, collapse, or kill).
    , usFallToTile ∷ !(Maybe (Float, Float, Int))
    -- | Number of z-levels just climbed, set when a Climbing
    --   transition completes. Drained by tickAllMovement after
    --   the per-unit tick — it converts this into a skill bump on
    --   "climbing" (with diminishing returns) and clears the field
    --   back to 0.
    , usPendingClimbXP ∷ !Float
    -- | Game-time at which a fall KNOCKDOWN auto-stands the unit, set on
    --   a non-lethal fall landing. The movement tick stands the unit
    --   (Collapsed → Standing) once now ≥ this and then clears it
    --   (one-shot). This is deliberately INDEPENDENT of the resource
    --   revive gate in unit_resources.lua: getting the wind knocked out
    --   of you shouldn't require being fed/hydrated to stand back up. A
    --   survival (exhaustion/thirst) collapse has no getup timer and stays
    --   resource-gated. Nothing outside an active knockdown.
    , usGetUpAt ∷ !(Maybe Double)
    -- | Drop magnitude (z-levels) of a fall the unit just landed from,
    --   set by the pure landing routing. Drained by tickAllMovement
    --   (which has body-part + substance data + the unit manager), which
    --   runs the `Unit.Fall` physics model to turn it into a SET of
    --   fracture/concussion wounds, sizes the knockdown stun from the
    --   worst injury, then clears this. Nothing when no fall is pending.
    , usPendingFallDrop ∷ !(Maybe Int)
    -- | Active LEAP marker + arc apex (z above the launch). When set, the
    --   shared Standing→Falling airborne transition arcs up-then-down and
    --   interpolates xy across the gap (a jump), and lands the unit
    --   STANDING instead of collapsing like a fall. Nothing = an ordinary
    --   fall (descend only, xy pinned). Set by startJump, cleared on
    --   landing in handleTransitionExpiry.
    , usJumpApex ∷ !(Maybe Float)
    -- | Signed grade of the ground the unit is currently walking (#375):
    --   positive = heading uphill (1.0 = straight up a ramp's fall line),
    --   negative = downhill, 0 = flat or not moving. Written by the
    --   movement tick every step (and reset when the unit isn't
    --   stepping); read by the Lua stamina drain via `unit.getInfo`'s
    --   `moveGrade` so sustained uphill travel taxes endurance. Purely
    --   transient — re-derived every tick — but it rides through saves
    --   with the rest of the record (harmless; stale for at most one
    --   tick after load).
    , usMoveGrade ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

data MoveTarget = MoveTarget
    { mtTargetX ∷ !Float
    , mtTargetY ∷ !Float
    , mtSpeed   ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

-- | What pose the unit is currently *in*. Orthogonal to UnitActivity.
--   Transitions between poses are driven by `TransitioningTo`. `Dead`
--   is terminal — entered only via `UnitKill`, never via a transition.
--
--   APPEND-ONLY. `Generic Serialize` is positional by constructor tag,
--   so inserting or reordering constructors silently shifts every saved
--   unit's `usPose`. If the pose set legitimately needs to change, bump
--   `currentSaveVersion` in `World.Save.Types`.
data Pose = Standing | Crouching | Crawling | Collapsed | Dead | Climbing | Falling | Sleeping
    deriving (Show, Eq, Generic, Serialize)

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
-- Climbing slots between Standing and Crouching for transition-reverse
-- detection: Standing→Climbing plays forward, Climbing→Crawling plays
-- forward, so the chain ascent stays "downward" in pose space the same
-- way standing → crawling → collapsed does.
poseDepth Climbing  = 1
-- Falling sits at depth 2 — Standing → Falling is forward (the unit
-- is going from upright to in-air-going-down). Falling → Collapsed
-- is forward (further descent into the ground).
poseDepth Falling   = 2
-- Sleeping only ever transitions to/from Crawling (lying down onto the
-- ground to sleep): depth 3 keeps Crawling→Sleeping forward and
-- Sleeping→Crawling (waking) reverse, matching the Collapsed convention.
poseDepth Sleeping  = 3

-- | The current activity the unit is doing. Drives anim selection and
--   movement gating. APPEND-ONLY for the same reason as `Pose` and
--   `Direction` — `Generic Serialize` is positional. New activities go
--   at the end; replacements bump `currentSaveVersion`.
data UnitActivity = Idle | Walking | Drinking | Eating | Picking | TransitioningTo !Pose | Running
    deriving (Show, Eq, Generic, Serialize)

data UnitThreadState = UnitThreadState
    { utsSimStates ∷ !(HM.HashMap UnitId UnitSimState)
    } deriving (Show, Eq)

emptyUnitThreadState ∷ UnitThreadState
emptyUnitThreadState = UnitThreadState
    { utsSimStates = HM.empty
    }
