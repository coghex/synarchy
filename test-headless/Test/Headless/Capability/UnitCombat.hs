-- | Projection-aliasing coverage for the @units-buildings-combat@
--   units-and-combat capability record (issue #895, E6a of the
--   @EngineEnv@ capability split #537) — the direct counterpart of
--   "Test.Headless.Capability.WorldSim" and
--   "Test.Headless.Capability.Input", for the same reason.
--
--   'Engine.Core.Capability.UnitCombat.toUnitCombatCapability' is
--   documented as returning the __identical live containers__
--   'EngineEnv' already carries, never a copy and never a snapshot.
--   That property is what makes the record safe to project as often as
--   a consumer likes — and after this migration a great many consumers
--   do exactly that, re-projecting inline at every field access. A
--   projection that bound 'ucUnitQueue' to @combatQueue env@, or copied
--   a container with @newIORef =\<\< readIORef@, would still typecheck,
--   still pass the SS6 ratchet, and still look right in a diff — while
--   silently routing unit kills into the combat thread's own inbox, or
--   giving the unit thread a private roster the combat thread's wound
--   application never reaches.
--
--   Every field of the record is either an 'Data.IORef.IORef' (pointer
--   'Eq') or an 'Engine.Core.Queue.Queue' (which derives 'Eq' through
--   its 'TQueue'), so "same live container" is directly assertable:
--   compare the projected field against the same 'EngineEnv' field. A
--   wrong-container binding fails the corresponding example —
--   including a SWAP between two same-typed fields, since each is
--   checked against its own named counterpart rather than merely "some
--   field of the env".
--
--   Per SS6.3 test fixtures are outside the full-access ratchet, so
--   this module imports @EngineEnv(..)@ directly — that is the point:
--   it compares the capability's view against the unrestricted one.
module Test.Headless.Capability.UnitCombat (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.UnitCombat
  (UnitCombatCapability(..), toUnitCombatCapability)

-- | Assert two live containers are the SAME one. Neither 'IORef' nor
--   'Engine.Core.Queue.Queue' has a 'Show' instance, so this is an
--   'Eq'-only assertion carrying its own failure message rather than
--   'shouldBe'\'s rendered-value diff.
sameContainer ∷ Eq α ⇒ α → α → Expectation
sameContainer projected live
  | projected == live = pure ()
  | otherwise = expectationFailure
      "projected field is NOT the live EngineEnv container -- the \
      \projection copied, swapped, or reconstructed it instead of \
      \aliasing it (see Engine.Core.Capability.UnitCombat's convention)"

-- | Assert two live containers are DIFFERENT ones — the shape a
--   transposed projection produces when both sides of the swap are
--   checked against each other rather than against the env.
distinctContainer ∷ Eq α ⇒ String → α → α → Expectation
distinctContainer name a b
  | a == b = expectationFailure
      (name <> ": these two record fields resolve to the SAME live \
       \container -- the projection collapsed two distinct EngineEnv \
       \fields onto one")
  | otherwise = pure ()

spec ∷ SpecWith EngineEnv
spec = do
  describe "toUnitCombatCapability (all ten unit/combat fields)" $ do
    let aliases name project field =
          it (name <> " aliases the live EngineEnv container") $ \env →
            sameContainer (project (toUnitCombatCapability env)) (field env)

    aliases "ucUnitManagerRef"   ucUnitManagerRef   unitManagerRef
    aliases "ucUnitQueue"        ucUnitQueue        unitQueue
    aliases "ucUtsRef"           ucUtsRef           utsRef
    aliases "ucStatRNGRef"       ucStatRNGRef       statRNGRef
    aliases "ucCombatQueue"      ucCombatQueue      combatQueue
    aliases "ucCombatEventsRef"  ucCombatEventsRef  combatEventsRef
    aliases "ucInjuryEventsRef"  ucInjuryEventsRef  injuryEventsRef
    aliases "ucThoughtEventsRef" ucThoughtEventsRef thoughtEventsRef
    aliases "ucActionOutcomeRef" ucActionOutcomeRef actionOutcomeRef
    aliases "ucPathingConfigRef" ucPathingConfigRef pathingConfigRef

    it "keeps the three CombatEvent streams on their own refs" $ \env → do
      -- combatEventsRef / injuryEventsRef / thoughtEventsRef all have
      -- the IDENTICAL type `IORef (Seq CombatEvent)`, and all three are
      -- drained by their own Lua-side log panel. A transposed
      -- projection between any two would compile everywhere, pass the
      -- SS6 ratchet, and quietly deliver falls into the combat log (or
      -- thoughts into the injury log) instead of failing loudly. The
      -- per-field examples above already pin each to its own named
      -- counterpart; these assertions state the risk explicitly and
      -- also prove the three really are three different containers.
      let cap = toUnitCombatCapability env
      sameContainer (ucCombatEventsRef cap)  (combatEventsRef env)
      sameContainer (ucInjuryEventsRef cap)  (injuryEventsRef env)
      sameContainer (ucThoughtEventsRef cap) (thoughtEventsRef env)
      distinctContainer "combat vs injury"
        (ucCombatEventsRef cap) (ucInjuryEventsRef cap)
      distinctContainer "combat vs thought"
        (ucCombatEventsRef cap) (ucThoughtEventsRef cap)
      distinctContainer "injury vs thought"
        (ucInjuryEventsRef cap) (ucThoughtEventsRef cap)

    it "keeps the unit and combat command queues distinct" $ \env → do
      -- unitQueue and combatQueue are different element types, but they
      -- are the producer/consumer pair whose ordering fixes the
      -- shutdown sequence (the combat thread produces onto unitQueue,
      -- so it stops first). Both are read through the same projection
      -- in the same modules, so pin each and assert they are two
      -- containers.
      let cap = toUnitCombatCapability env
      sameContainer (ucUnitQueue cap)   (unitQueue env)
      sameContainer (ucCombatQueue cap) (combatQueue env)

    it "keeps the roster and the sim-side unit state on their own refs" $ \env → do
      -- unitManagerRef/utsRef are the pair the unit thread reads and
      -- writes together every tick, and the pair a load publish swaps
      -- together -- Unit.Thread's own comments call out that mixing a
      -- stale one with a freshly-swapped one corrupts a reused unit id.
      -- A swap here would be exactly that bug, silently.
      let cap = toUnitCombatCapability env
      sameContainer (ucUnitManagerRef cap) (unitManagerRef env)
      sameContainer (ucUtsRef cap)         (utsRef env)
