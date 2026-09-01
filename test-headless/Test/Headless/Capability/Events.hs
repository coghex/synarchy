-- | Projection-aliasing coverage for the @ui-hud-events@
--   event\/notification\/popup capability record (issue #898, E7b of
--   the @EngineEnv@ capability split #537).
--
--   'Engine.Core.Capability.Events.toEventsCapability' is documented
--   as returning the __identical live containers__ 'EngineEnv' already
--   carries — never a copy, never a snapshot. That property is what
--   makes the record safe to re-project inline at every call site (E7b
--   does exactly that), and it is precisely what a refactor can
--   silently break: a projection that copied a container with
--   @newTVarIO =<< readTVarIO@ would still typecheck, still pass the
--   SS6 ratchet, and still look right in a diff — while quietly
--   detaching the event log the world thread pushes to from the one
--   @engine.getEventLog()@ reads.
--
--   Transposing 'ecEventStoreRef' and 'ecPopupQueueRef' used to be the
--   other silent break: both were @TVar (Seq PlayerEvent)@, so the
--   compiler could not object. #1714 gave the log ring its own
--   @TVar EventStore@ (rows plus the mutation-sequence counter), which
--   makes that swap a TYPE ERROR rather than a test-caught mistake —
--   the check below is kept as the standing proof that the two stay
--   distinct containers, not merely distinct names.
--
--   Three of the four fields are 'Control.Concurrent.STM.TVar.TVar' or
--   'Data.IORef.IORef' (pointer 'Eq'), so "same live container" is
--   directly assertable. The fourth, 'ecNotificationOrder', is a plain
--   immutable @[Text]@ with no identity to compare, so it is asserted
--   by value instead.
--
--   Per SS6.3 test fixtures are outside the full-access ratchet, so
--   this module imports @EngineEnv(..)@ directly — that is the point:
--   it compares the capability's view against the unrestricted one.
module Test.Headless.Capability.Events (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.Events (EventsCapability(..), toEventsCapability)

-- | Assert two live containers are the SAME one. Neither 'TVar' nor
--   'IORef' has a 'Show' instance, so this is an 'Eq'-only assertion
--   carrying its own failure message rather than 'shouldBe'\'s
--   rendered-value diff.
sameContainer ∷ Eq α ⇒ α → α → Expectation
sameContainer projected live
  | projected == live = pure ()
  | otherwise = expectationFailure
      "projected field is NOT the live EngineEnv container -- the \
      \projection copied, swapped, or reconstructed it instead of \
      \aliasing it (see Engine.Core.Capability.Core's convention)"

spec ∷ SpecWith EngineEnv
spec = do
  describe "toEventsCapability (all four ui-hud-events event/notification/popup fields)" $ do
    let aliases name project field =
          it (name <> " aliases the live EngineEnv container") $ \env →
            sameContainer (project (toEventsCapability env)) (field env)

    aliases "ecEventStoreRef"      ecEventStoreRef      eventStoreRef
    aliases "ecNotificationCfgRef" ecNotificationCfgRef notificationCfgRef
    aliases "ecPopupQueueRef"      ecPopupQueueRef      popupQueueRef

    it "ecNotificationOrder carries the live EngineEnv value" $ \env → do
      -- Not a ref: a plain immutable boot value (registry order of the
      -- category ids), so identity is meaningless and equality is the
      -- whole contract. Asserted non-empty too, since the fixture boots
      -- the real notification registry -- an empty list on both sides
      -- would make the equality vacuous.
      ecNotificationOrder (toEventsCapability env) `shouldBe` notificationOrder env
      ecNotificationOrder (toEventsCapability env) `shouldNotBe` []

    it "keeps the two event TVars pinned to their own counterparts" $ \env → do
      -- `eventStoreRef` (the log ring, read back by
      -- engine.getEventLog()) and `popupQueueRef` (popup-enabled
      -- events, write-only today) were both `TVar (Seq PlayerEvent)`
      -- until #1714, so transposing them in the projection typechecked
      -- silently. Each is still pinned to its own named counterpart
      -- here; what changed is that the log ring is now
      -- `TVar EventStore`, so the swap no longer compiles at all.
      let cap = toEventsCapability env
      sameContainer (ecEventStoreRef cap) (eventStoreRef env)
      sameContainer (ecPopupQueueRef cap) (popupQueueRef env)
