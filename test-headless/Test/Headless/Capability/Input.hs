-- | Projection-aliasing coverage for the @input-lua-transport@
--   capability records (issue #892, E4 of the @EngineEnv@ capability
--   split #537) — the direct counterpart of
--   "Test.Headless.Capability.Render" \/
--   "Test.Headless.Capability.WorldSim", for the same reason, and
--   covering BOTH interfaces: the full @LuaThread@ record
--   'Engine.Core.Capability.Input.InputCapability' and its strictly
--   narrower worker-safe companion
--   'Engine.Core.Capability.InputView.InputViewCapability'.
--
--   Both projections are documented as returning the __identical live
--   containers__ 'EngineEnv' already carries, never a copy and never a
--   snapshot; the migration re-projects inline at nearly every field
--   access, so a projection that minted anything fresh per call would
--   break cross-thread visibility everywhere at once.
--
--   This capability has a failure mode the earlier two did not: it
--   carries TWO same-typed @TVar Int@ barrier fields —
--   @inputBarrierNextRef@ (the Lua-side token ALLOCATOR) and
--   @inputBarrierRef@ (the input thread's processed WATERMARK). A
--   transposed projection would typecheck, pass the SS6 ratchet, and
--   look right in a diff, while making every
--   'Engine.Input.Inject.waitForBarrier' observe the allocator it just
--   bumped and return success instantly — every synthetic-injection
--   ack (#644/#727) would go green without the input thread having
--   processed anything. Each field is therefore pinned against its own
--   named counterpart, and the two barriers are additionally asserted
--   to be genuinely different containers.
--
--   Per SS6.3 test fixtures are outside the full-access ratchet, so
--   this module imports @EngineEnv(..)@ directly — that is the point:
--   it compares each capability's view against the unrestricted one.
module Test.Headless.Capability.Input (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.Input
  (InputCapability(..), toInputCapability)
import Engine.Core.Capability.InputView
  (InputViewCapability(..), toInputViewCapability)

-- | Assert two live containers are the SAME one. Neither 'IORef',
--   'Control.Concurrent.STM.TVar.TVar' nor 'Engine.Core.Queue.Queue'
--   has a 'Show' instance, so this is an 'Eq'-only assertion carrying
--   its own failure message rather than 'shouldBe'\'s rendered diff.
sameContainer ∷ Eq α ⇒ α → α → Expectation
sameContainer projected live
  | projected == live = pure ()
  | otherwise = expectationFailure
      "projected field is NOT the live EngineEnv container -- the \
      \projection copied, swapped, or reconstructed it instead of \
      \aliasing it (see Engine.Core.Capability.Input's convention)"

-- | Assert two live containers are DIFFERENT ones.
distinctContainer ∷ Eq α ⇒ α → α → Expectation
distinctContainer a b
  | a /= b = pure ()
  | otherwise = expectationFailure
      "two fields that must name different live containers resolved to \
      \the SAME one -- the projection transposed or duplicated a binding"

spec ∷ SpecWith EngineEnv
spec = do
  describe "toInputCapability (all eight input-lua-transport fields)" $ do
    let aliases name project field =
          it (name <> " aliases the live EngineEnv container") $ \env →
            sameContainer (project (toInputCapability env)) (field env)

    aliases "icInputQueue"          icInputQueue          inputQueue
    aliases "icInputBarrierNextRef" icInputBarrierNextRef inputBarrierNextRef
    aliases "icInputBarrierRef"     icInputBarrierRef     inputBarrierRef
    aliases "icInputStateRef"       icInputStateRef       inputStateRef
    aliases "icKeyBindingsRef"      icKeyBindingsRef      keyBindingsRef
    aliases "icCurrentKeyDownRef"   icCurrentKeyDownRef   currentKeyDownRef
    aliases "icLuaToEngineQueue"    icLuaToEngineQueue    luaToEngineQueue
    aliases "icLuaQueue"            icLuaQueue            luaQueue

    it "keeps the two same-typed barrier TVars on their own containers" $ \env → do
      -- The record's one genuinely dangerous pair: both are `TVar Int`,
      -- so a transposed binding compiles. Pin each to its own named
      -- counterpart AND assert they are two different containers -- an
      -- allocator that IS the watermark would satisfy the per-field
      -- checks only if EngineEnv itself had merged them.
      let cap = toInputCapability env
      sameContainer (icInputBarrierNextRef cap) (inputBarrierNextRef env)
      sameContainer (icInputBarrierRef cap) (inputBarrierRef env)
      distinctContainer (icInputBarrierNextRef cap) (icInputBarrierRef cap)

    it "keeps the two same-shaped transport queues distinct" $ \env → do
      -- luaQueue (engine→Lua) and luaToEngineQueue (Lua→engine) are
      -- both `Q.Queue`s of different payload types, but they are read
      -- through the same projection in the same expression at the
      -- migration's Lua-side call sites; state the risk explicitly.
      let cap = toInputCapability env
      sameContainer (icLuaQueue cap) (luaQueue env)
      sameContainer (icLuaToEngineQueue cap) (luaToEngineQueue env)

  describe "toInputViewCapability (the worker-safe five)" $ do
    let aliases name project field =
          it (name <> " aliases the live EngineEnv container") $ \env →
            sameContainer (project (toInputViewCapability env)) (field env)

    aliases "ivInputQueue"      ivInputQueue      inputQueue
    aliases "ivInputBarrierRef" ivInputBarrierRef inputBarrierRef
    aliases "ivInputStateRef"   ivInputStateRef   inputStateRef
    aliases "ivKeyBindingsRef"  ivKeyBindingsRef  keyBindingsRef
    aliases "ivLuaQueue"        ivLuaQueue        luaQueue

    it "binds its barrier field to the WATERMARK, not the allocator" $ \env → do
      -- The single most consequential binding in the whole migration:
      -- the worker-safe view exists precisely so the input thread can
      -- publish the processed watermark without being able to reach
      -- the Lua-private allocator. If `ivInputBarrierRef` resolved to
      -- inputBarrierNextRef, Engine.Input.Thread.Dispatch's
      -- `modifyTVar' ... (max tok)` would bump the ALLOCATOR and no
      -- injection ack would ever settle.
      let view = toInputViewCapability env
      sameContainer (ivInputBarrierRef view) (inputBarrierRef env)
      distinctContainer (ivInputBarrierRef view) (inputBarrierNextRef env)

    it "shares every container with the full record, never a copy" $ \env → do
      -- The two records are INDEPENDENT projections of EngineEnv (the
      -- view is never derived from the full record), so nothing in the
      -- types forces their overlapping five fields to agree. They must
      -- still name the same live containers -- otherwise the input
      -- thread and the Lua thread would be talking past each other on
      -- the same logically-shared queue.
      let cap  = toInputCapability env
          view = toInputViewCapability env
      sameContainer (ivInputQueue view) (icInputQueue cap)
      sameContainer (ivInputBarrierRef view) (icInputBarrierRef cap)
      sameContainer (ivInputStateRef view) (icInputStateRef cap)
      sameContainer (ivKeyBindingsRef view) (icKeyBindingsRef cap)
      sameContainer (ivLuaQueue view) (icLuaQueue cap)
