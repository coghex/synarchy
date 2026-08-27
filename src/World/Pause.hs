{-# LANGUAGE Strict #-}
-- | The ONE writer of the (pause flag, paused page's clock) pair
--   (#1599).
--
--   @scripts\/pause.lua@ has always promised that a chosen fast-forward
--   \"survives a pause cycle and resumes at the speed the player
--   chose\". It could only keep that promise for a pause it imposed
--   itself: every engine-side writer of 'wsEnginePausedRef' — a
--   @pause: true@ notification category
--   ('Engine.PlayerEvent.Emit'), @engine.saveWorld@'s acceptance
--   ('Engine.Scripting.Lua.API.Save.acceptSaveRequest'), the world
--   thread's save re-assertion, a load publish — runs no Lua at all, so
--   the snapshot the Lua resume branch wrote back was whatever the last
--   Lua-imposed pause had left there (1.0 in an ordinary session).
--
--   Moving the pairing here removes the split rather than papering over
--   it: whoever imposes the pause captures the speed, so whoever lifts
--   it can give it back.
--
--   __An epoch records no OWNER, so the sources are counted
--   instead.__ Because a second pause landing on an already-paused
--   session is a complete no-op, nothing downstream could tell that a
--   notification still wanted the game paused when an autosave's
--   restore came to close the epoch — so the restore erased it, leaving
--   the popup that announced the pause standing over a running game
--   (#1730). 'imposePause' therefore bumps
--   'Engine.Core.State.enginePauseGenRef' on every call, no-op
--   included, while the two pauses a save imposes on its OWN behalf
--   ('reassertSavePause' and @acceptSaveRequest@'s 'imposePauseHeld')
--   deliberately do not — a save may not count itself as a reason to
--   decline its own restore. The player's pause is not counted here
--   either; @playerIntentGenRef@ already records it, and keeping the
--   two apart is what lets a declined restore name the real reason.
--
--   __A pause is an EPOCH, not a flag.__ The capture happens on the
--   unpaused→paused TRANSITION and only there. A second pause landing
--   while the session is already paused — the world thread re-asserting
--   a save's pause, a second notification, a save taken from an already
--   paused game — finds the clock already zeroed and must not replace
--   the captured speed with that zero, so 'imposePause' does nothing at
--   all in that case. That is what makes the epoch's speed survive an
--   arbitrary number of overlapping pause sources.
--
--   __The epoch belongs to a PAGE.__ It is stored in that page's own
--   'wsResumeScaleRef', so a resume restores the page whose clock was
--   actually zeroed rather than whichever page happens to be active
--   later, and a page that is gone by then takes its epoch with it
--   instead of donating its speed to a stranger.
--
--   __Opening and closing an epoch are LINEARIZABLE against each
--   other.__ An epoch is three writes (the flag, the page's captured
--   speed, the page's clock), so protecting only the flag is not enough:
--   a notification opening one on the world thread could publish the
--   flag, have the Lua thread observe it, close the epoch it cannot yet
--   see and clear the flag, and then finish capturing — leaving an
--   UNPAUSED session at scale 0 with a resume slot nobody will ever
--   consume. Every transition therefore runs under
--   'Engine.Core.Capability.WorldSim.withPlayerIntentHeld', the mutex
--   the player's own pause and time-scale transitions already take
--   (holding it, never bumping it: an engine-imposed pause is not player
--   intent, #913). Callers that ALREADY hold that lock —
--   @engine.setPaused@, @engine.saveWorld@'s acceptance, an autosave's
--   conditional restore — must use the @…Held@ variants instead, since
--   the lock is a plain 'Control.Concurrent.MVar.MVar' and re-entering
--   it deadlocks.
module World.Pause
    ( imposePause
    , imposePauseHeld
    , reassertSavePause
    , beginPauseEpoch
    , releasePause
    , releasePauseHeld
    , setPauseResumeScale
    ) where

import UPrelude
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.Capability.WorldSim
    ( WorldSimCapability(wsEnginePausedRef, wsEnginePauseGenRef
                        , wsWorldManagerRef)
    , withPlayerIntentHeld )
import World.Types
    ( WorldManager(wmWorlds), WorldState(wsTimeScaleRef, wsResumeScaleRef)
    , visiblePageState )

-- | Pause the session, starting a pause epoch if one is not already
--   running.
--
--   On the unpaused→paused transition the visible page's chosen speed
--   is captured into its 'wsResumeScaleRef' and its clock zeroed, as
--   one step. When the session was ALREADY paused this is a complete
--   no-op — see the module haddock: the running epoch's captured speed
--   is the player's real one and the clock it would re-capture is
--   already zero.
--
--   Every engine-side pause writer goes through here, through
--   'imposePauseHeld', or — for a save re-asserting its own pause —
--   through 'reassertSavePause'.
--
--   This one also COUNTS the assertion (#1730): the caller is a source
--   independent of any save in flight, so an autosave that finds the
--   count moved since its acceptance leaves the pause alone rather than
--   erasing someone else's.
imposePause ∷ WorldSimCapability → IO ()
imposePause wsc = withEpochLock wsc $ do
    countEnginePauseAssertion wsc
    imposePauseHeld wsc

-- | The pause a SAVE re-asserts on its own behalf, once its queued
--   @WorldSave@ reaches the world thread
--   ('World.Thread.Command.Save.WriteWorld.handleWorldSaveCommand').
--
--   Identical to 'imposePause' in what it does to the epoch — almost
--   always nothing, since the Lua acceptance already opened it — and
--   deliberately different in what it RECORDS: it does not count as an
--   independent pause assertion (#1730). A save asserting its own pause
--   a second time must never be mistaken for someone else wanting the
--   game paused, or every autosave would decline to restore the state
--   it just took.
reassertSavePause ∷ WorldSimCapability → IO ()
reassertSavePause wsc = withEpochLock wsc (imposePauseHeld wsc)

-- | Record that an engine pause source INDEPENDENT of any running save
--   has asserted a pause (#1730).
--
--   Counted on every call, including the overwhelmingly common one that
--   finds the session already paused and changes nothing: that no-op is
--   precisely the case the counter exists to make visible, because the
--   epoch itself records no owner. Over-counting is the safe direction —
--   it can only make an autosave decline to resume a game somebody else
--   wanted paused — and under-counting is not.
--
--   Bare 'atomicModifyIORef'' rather than a lock of its own: every
--   caller reaches this through 'withEpochLock', and the two sites that
--   snapshot and compare the counter take that same mutex.
countEnginePauseAssertion ∷ WorldSimCapability → IO ()
countEnginePauseAssertion wsc =
    atomicModifyIORef' (wsEnginePauseGenRef wsc) (\g → (g + 1, ()))

-- | 'imposePause' for a caller already holding the player-intent lock.
--
--   The epoch half ALONE: it does not count an engine pause assertion
--   (#1730), because its two callers are the two that must not — a
--   save's own acceptance
--   ('Engine.Scripting.Lua.API.Save.acceptSaveRequest') and the
--   player's @engine.setPaused@, whose intent @playerIntentGenRef@
--   already records.
imposePauseHeld ∷ WorldSimCapability → IO ()
imposePauseHeld wsc = do
    wasPaused ← atomicModifyIORef' (wsEnginePausedRef wsc) (\p → (True, p))
    unless wasPaused $ captureVisibleClock wsc

-- | Start a FRESH pause epoch, whatever the flag already said.
--
--   For 'World.Load.Publish' alone: a load transaction pauses the
--   OUTGOING session at acceptance, so by the time the replacement
--   session is live the flag is already set and 'imposePause' would
--   preserve an epoch belonging to pages that no longer exist. The
--   published session gets its own epoch instead — its active page's
--   clock zeroed and the default speed every loaded page comes up at
--   captured — which is the load policy @scripts\/pause.lua@'s
--   @onSaveLoaded@ states: a load resumes at 1.0, never at some
--   pre-save speed.
--
--   Uncounted (#1730), like 'imposePauseHeld': a load and a save are
--   mutually exclusive for the load's whole duration
--   ('Engine.Scripting.Lua.API.Save.loadSaveFn' rejects one outright
--   while the other runs), so there is no autosave window for this to
--   land in — and the session it opens the epoch over is not the one
--   any earlier request was taken from.
beginPauseEpoch ∷ WorldSimCapability → IO ()
beginPauseEpoch wsc = withEpochLock wsc $ do
    writeIORef (wsEnginePausedRef wsc) True
    captureVisibleClock wsc

-- | Hold the mutex every epoch transition shares — see the module
--   haddock for why the three writes have to be one critical section.
--   The generation is deliberately NOT advanced: these are the engine's
--   own writes, and #913 reserves a bump for the player's.
withEpochLock ∷ WorldSimCapability → IO α → IO α
withEpochLock wsc act = withPlayerIntentHeld wsc (const act)

-- | Capture the visible page's live scale as this epoch's resume scale
--   and zero its clock, in that order — the clock must never be read as
--   zeroed before the value it was zeroed from has been recorded.
captureVisibleClock ∷ WorldSimCapability → IO ()
captureVisibleClock wsc = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    forM_ (visiblePageState mgr) $ \ws → do
        scale ← readIORef (wsTimeScaleRef ws)
        writeIORef (wsResumeScaleRef ws) (Just scale)
        writeIORef (wsTimeScaleRef ws) 0

-- | End the pause epoch: give every page carrying a resume scale its
--   speed back, then clear the flag.
--
--   Scales first, flag last, so the pair is never momentarily readable
--   as \"running at scale 0\". Only pages whose clock this epoch
--   actually took hold of carry a value, so this cannot write a scale
--   onto a bystander page; running it on an already-unpaused session
--   finds nothing to restore and is a no-op.
releasePause ∷ WorldSimCapability → IO ()
releasePause wsc = withEpochLock wsc (releasePauseHeld wsc)

-- | 'releasePause' for a caller already holding the player-intent lock.
releasePauseHeld ∷ WorldSimCapability → IO ()
releasePauseHeld wsc = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    forM_ (wmWorlds mgr) $ \(_, ws) → do
        mScale ← takeResumeScale (wsResumeScaleRef ws)
        forM_ mScale $ writeIORef (wsTimeScaleRef ws)
    writeIORef (wsEnginePausedRef wsc) False
  where
    takeResumeScale ref = atomicModifyIORef' ref (\m → (Nothing, m))

-- | Record the speed a page should come back at when the current epoch
--   ends, without touching its live clock.
--
--   Two callers, both of which know a resume scale the capture could
--   not have seen: @world.setTimeScale@ landing while the session is
--   paused (the player's choice would otherwise be dropped, since a
--   running scale must never be stored on a paused page), and a
--   successful autosave handing back the pre-request speed its own
--   'World.Save.Types.AutosaveRequest' recorded.
--
--   Takes no lock of its own: both callers already hold the epoch mutex,
--   because in both cases this write and the flag read (or the release)
--   beside it have to be one step.
setPauseResumeScale ∷ WorldState → Float → IO ()
setPauseResumeScale ws scale = writeIORef (wsResumeScaleRef ws) (Just scale)
