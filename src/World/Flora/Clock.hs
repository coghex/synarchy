{-# LANGUAGE Strict #-}
-- | The flora growth clock: the one read that turns a page's live
--   'World.Time.Types.WorldDate' into the @(dayOfYear, absoluteDay)@
--   pair every growth-derived decision is evaluated at.
--
--   It lives HERE, below both the Lua forage API and the world thread,
--   because #2212 made those two agree: the tagged-eligibility
--   predicate ('World.Flora.Growth.floraHarvestAdmits') is shared
--   between the Chop designation commit and the harvest verbs, and a
--   shared predicate fed from two independently-derived clocks is only
--   half a shared rule. A designation taken on one day-of-year and a
--   fell refused on another is exactly the selection/execution
--   disagreement the shared predicate exists to make impossible.
--
--   The screen-space half of Chop ('World.Flora.HitTest') does not call
--   this: it snapshots the calendar with the rest of its frame and
--   derives the day-of-year with 'World.Flora.Growth.floraDayOfYear',
--   which is the same value by construction.
module World.Flora.Clock
    ( growthClock
    ) where

import UPrelude
import Data.IORef (readIORef)
import World.Generate.Types (WorldGenParams(..))
import World.State.Types (WorldState(..))
import World.Time.Types
    (defaultCalendarConfig, worldAbsoluteDay, worldDateToDayOfYear)

-- | @(dayOfYear, absoluteDay)@ for the page's current date, under the
--   page's own calendar (falling back to the default calendar for a
--   page with no generation params, as every other calendar reader
--   does). The two are read from ONE 'wsDateRef' sample, so a midnight
--   rollover between them cannot pair a new day-of-year with the old
--   absolute day.
growthClock ∷ WorldState → IO (Int, Int)
growthClock ws = do
    paramsM ← readIORef (wsGenParamsRef ws)
    date ← readIORef (wsDateRef ws)
    let calendar = maybe defaultCalendarConfig wgpCalender paramsM
    pure ( worldDateToDayOfYear calendar date
         , worldAbsoluteDay calendar date )
