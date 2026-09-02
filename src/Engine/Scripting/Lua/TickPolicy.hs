-- | The tick-interval policy (#1695): one statement of what a Lua
--   script's update interval may be, shared by the two entry points
--   that set one — @engine.loadScript@ and @engine.setTickInterval@,
--   both in "Engine.Scripting.Lua.API.Core" — and by the scheduler in
--   "Engine.Scripting.Lua.Thread" that has to honour it.
--
--   Before this module both entry points wrote whatever Lua number they
--   were handed straight into 'scriptTickRate' and 'scriptNextTick' and
--   reported success, while the scheduler assumed a usable finite
--   interval. Zero and negative rates left a script permanently overdue,
--   running its @update@ at the 1 ms sleep floor (~1 kHz); @NaN@ left it
--   never due while pinning the loop at that same floor; @+Infinity@
--   overflowed the microsecond conversion to a zero timeout, spinning
--   the loop with no sleep at all.
--
--   __The policy__, enforced identically at both entry points:
--
--   * @0@ is ACCEPTED and means EVENT-ONLY. The script's @update@ is
--     never called on a timer; broadcasts, messages and direct calls
--     reach it exactly as they always did (dispatch walks every loaded
--     script and never consults the interval). An event-only script is
--     excluded from wake-time and due-update selection for the same
--     reason a paused one already is — including it would pin the sleep
--     at the floor and busy-spin the loop.
--   * A finite interval of at least 'minTickInterval' is ACCEPTED and
--     schedules exactly as it always has, including the ~60 Hz
--     'maxSleepMicros' cap and the @dt@ passed to @update@.
--   * Everything else is REFUSED at the API boundary: negative, @NaN@,
--     either infinity, and any positive value below 'minTickInterval'.
--     Nothing is clamped — a refused call leaves the script's existing
--     interval untouched and reports the refusal through the logger,
--     naming the offending value, rather than raising.
--
--   'minTickInterval' is 1 ms because the scheduler floors its sleep at
--   'minSleepSeconds'. A shorter interval could not be honoured, so it
--   is refused rather than silently mis-scheduled. An accepted script
--   sitting exactly at that minimum legitimately runs at the floor;
--   what the policy rules out is a script that is never due, or one
--   whose interval is meaningless, holding the loop there.
module Engine.Scripting.Lua.TickPolicy
  ( -- * Policy constants
    minTickInterval
  , minSleepSeconds
  , maxSleepMicros
  , idleWaitSeconds
    -- * Classification
  , TickInterval(..)
  , TickIntervalRefusal(..)
  , classifyTickInterval
  , tickIntervalSeconds
  , describeTickRefusal
  , isEventOnlyInterval
    -- * Scheduling
  , scriptIsTimed
  , scriptIsDue
  , nextTimerWake
  , schedulerSleepMicros
  , advanceTick
  ) where

import UPrelude
import Engine.Scripting.Lua.Types (LuaScript(..))

-- | The smallest positive interval the scheduler can honour, in
--   seconds. Equal to 'minSleepSeconds' by construction: an interval
--   below the sleep floor cannot be scheduled accurately, so the policy
--   refuses it instead.
minTickInterval ∷ Double
minTickInterval = 0.001

-- | The scheduler's sleep floor, in seconds. Kept here rather than
--   inline in "Engine.Scripting.Lua.Thread" so 'minTickInterval' and
--   the floor cannot drift apart.
minSleepSeconds ∷ Double
minSleepSeconds = 0.001

-- | The scheduler's sleep cap, in microseconds (~60 Hz). Every timeout
--   'schedulerSleepMicros' produces lies in @[1, maxSleepMicros]@.
maxSleepMicros ∷ Int
maxSleepMicros = 16666

-- | How far ahead the loop looks when nothing is on a timer at all.
--   The cap above bounds the resulting wait, so this only has to be
--   larger than the cap; it is the idle budget, not the 1 ms floor.
idleWaitSeconds ∷ Double
idleWaitSeconds = 1.0

-- | An interval that passed the policy. Constructing one is the only
--   way a value reaches 'scriptTickRate', so the stored rate is always
--   either exactly @0@ or a finite value at or above 'minTickInterval'.
data TickInterval
  = TickEventOnly       -- ^ @0@: never ticks on a timer, still receives events.
  | TickEvery !Double   -- ^ Finite, @>= 'minTickInterval'@.
  deriving (Eq, Show)

-- | Why an interval was refused. Each maps to one diagnostic in
--   'describeTickRefusal'.
data TickIntervalRefusal
  = RefusedNegative     -- ^ Below zero: the script would be permanently overdue.
  | RefusedNaN          -- ^ @NaN@: never due, and poisons the wake-time minimum.
  | RefusedInfinite     -- ^ Either infinity: never due, and overflows the timeout.
  | RefusedTooSmall     -- ^ Positive but below 'minTickInterval'.
  deriving (Eq, Show)

-- | The whole policy, in one total function. Note that @-0.0@ compares
--   equal to @0@ and so is accepted as event-only; 'tickIntervalSeconds'
--   normalises it back to a positive zero before it is stored.
classifyTickInterval ∷ Double → Either TickIntervalRefusal TickInterval
classifyTickInterval seconds
  | isNaN seconds             = Left RefusedNaN
  | isInfinite seconds        = Left RefusedInfinite
  | seconds ≡ 0               = Right TickEventOnly
  | seconds < 0               = Left RefusedNegative
  | seconds < minTickInterval = Left RefusedTooSmall
  | otherwise                 = Right (TickEvery seconds)

-- | The value an accepted interval stores in 'scriptTickRate'.
tickIntervalSeconds ∷ TickInterval → Double
tickIntervalSeconds TickEventOnly   = 0
tickIntervalSeconds (TickEvery s)   = s

-- | The refusal diagnostic. Always names the offending value, because
--   the caller's own log line is the only signal a refusal produces —
--   neither entry point raises, and neither changes its Lua return
--   convention.
describeTickRefusal ∷ TickIntervalRefusal → Double → Text
describeTickRefusal refusal seconds = reason <> " " <> policy
  where
    policy = "The interval must be 0 (event-only) or a finite value of at \
             \least " <> tshow minTickInterval <> " s; the existing interval \
             \is left unchanged."
    reason = case refusal of
      RefusedNaN → prefix
          <> " is not a number; a script with a NaN interval never \
             \becomes due and pins the scheduler at its sleep floor."
      RefusedInfinite → prefix
          <> " is infinite; a script with an infinite interval never \
             \becomes due and overflows the scheduler's timeout."
      RefusedNegative → prefix
          <> " is negative; a script with a negative interval is \
             \permanently overdue and runs at the sleep floor."
      RefusedTooSmall → prefix
          <> " is below the " <> tshow minTickInterval
          <> " s minimum the scheduler can honour."
    prefix = "tick interval " <> tshow seconds

-- | Is this stored rate the event-only sentinel? Every rate reaching a
--   script goes through 'classifyTickInterval' first, so an exact zero
--   here means event-only and nothing else.
isEventOnlyInterval ∷ Double → Bool
isEventOnlyInterval rate = rate ≡ 0

-- | Is this script driven by the tick timer at all? Paused scripts never
--   advance their 'scriptNextTick', and event-only scripts never tick;
--   including either in wake-time selection would pin the sleep at the
--   floor and busy-spin the loop at ~1 kHz.
scriptIsTimed ∷ LuaScript → Bool
scriptIsTimed s = not (scriptPaused s) ∧ not (isEventOnlyInterval (scriptTickRate s))

-- | Should this script's @update@ run now? The same timed predicate the
--   wake-time uses, so a script can never be selected as due while being
--   excluded from the sleep computation that was meant to wake it.
scriptIsDue ∷ Double → LuaScript → Bool
scriptIsDue now s = scriptIsTimed s ∧ now ≥ scriptNextTick s

-- | Reschedule a script whose @update@ has just run, given the
--   scheduler time @now@ it was found due at. Only ever applied to a
--   script 'scriptIsDue' selected, so the rate here is always an
--   accepted, finite, non-zero interval and @now@ is at or past the old
--   deadline.
--
--   The deadline rule (#2204), in interval multiples and independent of
--   "Engine.Core.Clock"'s elapsed cap:
--
--   * lateness (@now - oldDeadline@) BELOW one complete interval keeps
--     #1695's cadence — the next deadline is @oldDeadline + interval@,
--     so ordinary scheduler jitter neither drifts the phase nor stacks
--     up;
--   * lateness of one complete interval OR MORE drops the missed
--     executions — the next deadline is @now + interval@. The old rule
--     added the interval to the old deadline unconditionally, which
--     after a stall (host sleep, a long queue drain) left the script
--     still due on the very next pass and replayed every missed
--     interval as a burst.
--
--   So a script whose clock jumps across several intervals runs ONCE,
--   ends up with a deadline strictly later than the jumped clock, and is
--   not due again when the pass is repeated at that same clock value.
advanceTick ∷ Double → LuaScript → LuaScript
advanceTick now s
  | now - oldDeadline < interval = s { scriptNextTick = oldDeadline + interval }
  | otherwise                    = s { scriptNextTick = now + interval }
  where
    oldDeadline = scriptNextTick s
    interval    = scriptTickRate s

-- | The earliest wake time among the scripts actually on a timer, or
--   'Nothing' when none is — which is the ordinary idle case, not an
--   error.
nextTimerWake ∷ [LuaScript] → Maybe Double
nextTimerWake scripts = case map scriptNextTick (filter scriptIsTimed scripts) of
    [] → Nothing
    ts → Just (minimum ts)

-- | The queue-read timeout for one scheduler iteration, in microseconds.
--
--   The bound is applied in 'Double' BEFORE the 'floor'. The old
--   @min 16666 (floor (sleeptime * 1000000))@ floored first, so a large
--   but perfectly finite interval (@1e308@ seconds multiplies to
--   @Infinity@) produced an arbitrary 'Int' — zero here, which made
--   @registerDelay@ fire immediately and spun the loop with no sleep at
--   all. Bounding first makes the result provably lie in
--   @[1, 'maxSleepMicros']@ for every 'Double' input, so no accepted
--   interval can yield a zero or negative timeout.
schedulerSleepMicros ∷ Double → [LuaScript] → Int
schedulerSleepMicros currentSecs scripts = clampMicros (seconds * 1000000)
  where
    wake    = fromMaybe (currentSecs + idleWaitSeconds) (nextTimerWake scripts)
    seconds = max minSleepSeconds (wake - currentSecs)

    -- NaN cannot reach here from an accepted interval, but 'floor' of
    -- one is an arbitrary 'Int', so it is mapped to the cap rather than
    -- left to escape the bound.
    clampMicros ∷ Double → Int
    clampMicros micros
      | isNaN micros                          = maxSleepMicros
      | micros ≤ 1                            = 1
      | micros ≥ fromIntegral maxSleepMicros  = maxSleepMicros
      | otherwise                             = floor micros
