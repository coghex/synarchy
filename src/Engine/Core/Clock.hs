-- | The engine's elapsed-time boundary (#2204): ONE monotonic source and
--   ONE sanitiser shared by every interval consumer that turns "seconds
--   since my last sample" into simulation or pacing — the render loop's
--   frame timing ("Engine.Loop.Timing"), the world tick ("World.Thread"),
--   the unit tick ("Unit.Thread") and the Lua scheduler
--   ("Engine.Scripting.Lua.Util".@nowSeconds@).
--
--   Before this module those four read the wall clock
--   (@getPOSIXTime@ / @getCurrentTime@), so a host sleep, an NTP
--   correction or a manual clock change turned into an arbitrarily
--   large — or negative — elapsed value: epoch-sized frame deltas
--   integrated into the camera, a world calendar stepping backwards
--   past a forward-only date rollover, unit game-time and movement
--   jumping by the length of the sleep, and every missed Lua interval
--   replayed as a burst.
--
--   __The contract__ (gate: hspec @--match "monotonic elapsed-time
--   contract"@; prose: @docs/engine_contracts.md@ §Monotonic elapsed
--   time):
--
--   * The source is 'GHC.Clock.getMonotonicTime': never steps, never
--     runs backwards. Deliberate WALL-clock consumers — @engine.realTime()@,
--     log timestamps, save-metadata timestamps and the seed mixed into
--     "World.Page.GeneratedId" — are not elapsed-time consumers and keep
--     their own sources.
--   * Every raw difference passes through 'sanitiseElapsed' before it is
--     consumed: a negative, @NaN@ or infinite value becomes @0@, a value
--     above 'maxElapsedStep' becomes exactly 'maxElapsedStep', and
--     anything in @[0, maxElapsedStep]@ passes unchanged.
--   * The excess above the cap is DROPPED, never carried: a consumer
--     that keeps its previous raw sample replaces it with the current
--     raw sample after EVERY measurement ('sampleElapsed' does exactly
--     that), so a one-hour host sleep costs the simulation one capped
--     step and nothing accumulates as debt. Catching up elapsed
--     simulation after a long interruption is deliberately out of
--     scope.
module Engine.Core.Clock
  ( maxElapsedStep
  , sanitiseElapsed
  , monotonicSeconds
  , sampleElapsed
  ) where

import UPrelude
import Data.IORef (IORef, readIORef, writeIORef)
import GHC.Clock (getMonotonicTime)

-- | The largest elapsed step, in seconds, any interval consumer may
--   take from a single raw sample: exactly a quarter of a second. Above
--   this the difference is not a frame or a tick, it is an interruption
--   (host sleep, debugger stop, clock correction), and the policy is to
--   drop the excess rather than integrate it. Exactly representable in
--   both 'Double' and 'Float', so a consumer that narrows the sanitised
--   value (the world tick does) still sees exactly this bound.
maxElapsedStep ∷ Double
maxElapsedStep = 0.25

-- | The sanitiser. Total over every 'Double':
--
--   > sanitiseElapsed (-1)      == 0
--   > sanitiseElapsed (0/0)     == 0     -- NaN
--   > sanitiseElapsed (1/0)     == 0     -- +Infinity
--   > sanitiseElapsed (-1/0)    == 0     -- -Infinity
--   > sanitiseElapsed 0.1       == 0.1
--   > sanitiseElapsed 0.25      == 0.25  -- exactly at the cap
--   > sanitiseElapsed 3600      == 0.25  -- over the cap
--
--   @NaN@ is tested by name first: every ordering comparison against it
--   is False, so the guards below would otherwise let it through as an
--   "in-range" value.
sanitiseElapsed ∷ Double → Double
sanitiseElapsed dt
  | isNaN dt ∨ isInfinite dt ∨ dt < 0 = 0
  | dt > maxElapsedStep               = maxElapsedStep
  | otherwise                         = dt

-- | The monotonic clock, in seconds. The origin is arbitrary (process
--   start on most platforms); only DIFFERENCES between two samples mean
--   anything, which is the whole reason this is the elapsed-time source
--   and the wall clock is not.
monotonicSeconds ∷ IO Double
monotonicSeconds = getMonotonicTime

-- | One measurement against a retained raw sample: read the clock,
--   return the SANITISED seconds since the stored sample, and store the
--   new RAW sample — unconditionally, including after a negative,
--   invalid or over-cap difference. That unconditional replacement is
--   what makes the drop policy hold: the next measurement starts from
--   this sample, so excess above the cap never survives into it.
--
--   The clock is a parameter rather than 'monotonicSeconds' baked in so
--   the headless gate can script a jump; production callers pass
--   'monotonicSeconds'.
sampleElapsed ∷ IO Double → IORef Double → IO Double
sampleElapsed clock lastRef = do
    now  ← clock
    prev ← readIORef lastRef
    writeIORef lastRef now
    pure (sanitiseElapsed (now - prev))
