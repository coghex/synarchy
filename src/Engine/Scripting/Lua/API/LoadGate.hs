-- | Test-only debug verbs for the load-staging gate (issue #1181),
--   registered on the @debug@ table alongside @debug.captureScreenshot@
--   and @debug.drainActionOutcomes@ — deliberately NOT on a
--   player-facing namespace, following @engine.debugThrow@'s precedent
--   for a test-only binding that ships in engine code.
--
--   These exist so an automated test can hold a whole-session load
--   transaction in flight for as long as its checks need, instead of
--   racing them against however fast this machine regenerates the saved
--   world. @tools/transactional_load_probe.py@ is the caller; see
--   'Engine.Load.Status.StageGate' for what the gate guarantees
--   (inert when unarmed, bounded, self-releasing, one-shot).
--
--   Each verb reaches the gate through the existing
--   @save-load-coordination@ capability record rather than unrestricted
--   'Engine.Core.State' access — the gate lives inside the opaque
--   'Engine.Load.Status.LoadStatusRef' that record already projects, so
--   it needed no new 'Engine.Core.State.EngineEnv' field.
module Engine.Scripting.Lua.API.LoadGate
  ( armLoadStageGateFn
  , releaseLoadStageGateFn
  , getLoadStageGateFn
  ) where

import UPrelude
import Engine.Core.Capability.SaveLoad
    (SaveLoadCapability(..), toSaveLoadCapability)
import Engine.Core.State (EngineEnv)
import Engine.Load.Status
    (StageGate(..), armStageGate, releaseStageGate, readStageGate)
import qualified HsLua as Lua

-- | @debug.armLoadStageGate([holdSeconds]) → true@
--
--   Arm the gate so the NEXT load transaction parks before staging.
--   @holdSeconds@ is advisory: the engine clamps it into
--   @(0, 'Engine.Load.Status.maxStageGateHold']@ and falls back to
--   'Engine.Load.Status.defaultStageGateHold' when it is absent,
--   non-positive, or non-finite, so no arming mistake can wedge the
--   world thread. Always returns @true@ — arming cannot fail, and a
--   caller that needs to know whether a hold actually happened reads
--   @debug.getLoadStageGate()@.
armLoadStageGateFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
armLoadStageGateFn env = do
    secondsArg ← Lua.tonumber 1
    let seconds = maybe 0 (realToFrac ∷ Lua.Number → Double) secondsArg
    Lua.liftIO $ armStageGate (slLoadStatusRef (toSaveLoadCapability env)) seconds
    Lua.pushboolean True
    pure 1

-- | @debug.releaseLoadStageGate() → true@
--
--   Release a parked transaction (it resumes into staging on its next
--   poll) and disarm the gate. Safe to call unconditionally from a
--   test's cleanup path whether or not a hold is in progress, which is
--   what lets a probe put it in a @finally@.
releaseLoadStageGateFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
releaseLoadStageGateFn env = do
    Lua.liftIO $ releaseStageGate (slLoadStatusRef (toSaveLoadCapability env))
    Lua.pushboolean True
    pure 1

-- | @debug.getLoadStageGate() → {armed, holdSeconds, expired[, heldRequestId]}@
--
--   The gate's live state. @heldRequestId@ is present only while a
--   transaction is actually parked, and is the positive observation a
--   test needs: it names WHICH load is being held, so a check cannot
--   mistake a different (or already finished) transaction for the one
--   it armed the gate for. @expired@ reports that the most recent hold
--   ended on its own bound rather than on a release.
getLoadStageGateFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getLoadStageGateFn env = do
    gate ← Lua.liftIO $ readStageGate (slLoadStatusRef (toSaveLoadCapability env))
    Lua.newtable
    Lua.pushboolean (sgArmed gate)
    Lua.setfield (-2) "armed"
    Lua.pushnumber (Lua.Number (realToFrac (sgHoldSeconds gate)))
    Lua.setfield (-2) "holdSeconds"
    Lua.pushboolean (sgExpired gate)
    Lua.setfield (-2) "expired"
    forM_ (sgHeldRequest gate) $ \rid → do
        Lua.pushinteger (fromIntegral rid)
        Lua.setfield (-2) "heldRequestId"
    pure 1
