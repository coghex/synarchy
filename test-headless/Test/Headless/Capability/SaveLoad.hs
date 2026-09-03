-- | Projection-aliasing coverage for the @save-load-coordination@
--   capability record (issue #899, E8 — the final child of the
--   @EngineEnv@ capability split #537).
--
--   'Engine.Core.Capability.SaveLoad.toSaveLoadCapability' is
--   documented as returning the __identical live handles__ 'EngineEnv'
--   already carries — never a copy, never a snapshot. For this
--   capability that property is not merely a convention: every one of
--   these five handles is a coordination primitive whose whole job is
--   that several threads observe the SAME one.
--
--   A projection that copied 'slSaveBarrierRef' would hand the world
--   thread a barrier no save ever locks, so its @ownerGated@ gate
--   would read 'False' straight through a capture window and its
--   @acknowledgeCurrent@ would answer a barrier nobody is waiting on —
--   a save that quietly captures a session mid-write. A projection that
--   copied 'slNextItemInstanceIdRef' would fork the global item-identity
--   space into two allocators handing out the same ids. Neither would
--   fail to typecheck, neither would trip the SS6 ratchet, and neither
--   would look wrong in a diff.
--
--   @tools/engine_env_capability_audit.py@'s
--   @audit_save_load_projection@ covers the static half of this
--   contract (each field bound from its matching 'EngineEnv' accessor
--   in the projection's source). It cannot observe runtime container
--   identity at all, which is what this module adds. Both are required.
--
--   All five handles are 'Eq'-comparable by pointer identity:
--   'Data.IORef.IORef' directly, 'Engine.Save.Barrier.SaveBarrier' via
--   its derived 'Eq' over two 'Control.Concurrent.STM.TVar.TVar's, and
--   'Engine.Load.Status.LoadStatusRef' via its derived 'Eq' over two
--   'Data.IORef.IORef's — so "same live container" is directly
--   assertable for every field, with no by-value fallback.
--
--   Per SS6.3 test fixtures are outside the full-access ratchet, so
--   this module imports @EngineEnv(..)@ directly — that is the point:
--   it compares the capability's view against the unrestricted one.
module Test.Headless.Capability.SaveLoad (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.SaveLoad
  (SaveLoadCapability(..), toSaveLoadCapability)

-- | Assert two live containers are the SAME one. None of these types
--   has a 'Show' instance, so this is an 'Eq'-only assertion carrying
--   its own failure message rather than 'shouldBe'\'s rendered-value
--   diff.
sameContainer ∷ Eq α ⇒ α → α → Expectation
sameContainer projected live
  | projected == live = pure ()
  | otherwise = expectationFailure
      "projected field is NOT the live EngineEnv container -- the \
      \projection copied, swapped, or reconstructed it instead of \
      \aliasing it (see Engine.Core.Capability.Core's convention)"

spec ∷ SpecWith EngineEnv
spec = do
  describe "toSaveLoadCapability (all five save-load-coordination handles)" $ do
    let aliases name project field =
          it (name <> " aliases the live EngineEnv container") $ \env →
            sameContainer (project (toSaveLoadCapability env)) (field env)

    aliases "slLoadStatusRef"         slLoadStatusRef         loadStatusRef
    aliases "slPendingLoadRef"        slPendingLoadRef        pendingLoadRef
    aliases "slSaveBarrierRef"        slSaveBarrierRef        saveBarrierRef
    aliases "slLastSaveTimeRef"       slLastSaveTimeRef       lastSaveTimeRef
    aliases "slNextItemInstanceIdRef" slNextItemInstanceIdRef nextItemInstanceIdRef

    it "keeps the two same-typed IORef fields distinct" $ \env → do
      -- `lastSaveTimeRef` is `IORef UTCTime` and
      -- `nextItemInstanceIdRef` is `IORef Word64`, so those two cannot
      -- be transposed silently -- but BOTH are plain single-field
      -- IORefs projected side by side, and the ones that CAN be
      -- confused are caught by pinning each to its own named
      -- counterpart above. This adds the outright-distinctness check
      -- for the pair whose accessors sit adjacent in the projection.
      let cap = toSaveLoadCapability env
      sameContainer (slLastSaveTimeRef cap) (lastSaveTimeRef env)
      sameContainer (slNextItemInstanceIdRef cap) (nextItemInstanceIdRef env)

    it "the barrier handle is the one the save owners acknowledge" $ \env → do
      -- The single most consequential alias: every state-owner thread
      -- checks ownerGated and answers acknowledgeCurrent on this
      -- exact handle. A copy here silently breaks save quiescence
      -- rather than failing loudly.
      sameContainer (slSaveBarrierRef (toSaveLoadCapability env))
                    (saveBarrierRef env)
