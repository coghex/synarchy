-- | Projection-aliasing coverage for the @units-buildings-combat@
--   building capability record (issue #896, E6b of the @EngineEnv@
--   capability split #537) — the direct counterpart of
--   "Test.Headless.Capability.UnitCombat", for the same reason.
--
--   'Engine.Core.Capability.Building.toBuildingCapability' is
--   documented as returning the __identical live containers__
--   'EngineEnv' already carries, never a copy and never a snapshot.
--   That property is what makes the record safe to project as often as
--   a consumer likes — and after this migration most consumers do
--   exactly that, re-projecting inline at every field access. A
--   projection that copied a container with @newIORef =\<\< readIORef@
--   would still typecheck, still pass the SS6 ratchet, and still look
--   right in a diff — while giving the Lua thread a private building
--   roster the unit thread's queue drain never writes into, so a
--   spawned building would simply never appear.
--
--   Every field of the record is either an 'Data.IORef.IORef' (pointer
--   'Eq') or an 'Engine.Core.Queue.Queue' (which derives 'Eq' through
--   its 'TQueue'), so "same live container" is directly assertable:
--   compare the projected field against the same 'EngineEnv' field,
--   each against its own named counterpart rather than merely "some
--   field of the env".
--
--   Per SS6.3 test fixtures are outside the full-access ratchet, so
--   this module imports @EngineEnv(..)@ directly — that is the point:
--   it compares the capability's view against the unrestricted one.
module Test.Headless.Capability.Building (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.Building
  (BuildingCapability(..), toBuildingCapability)

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
      \aliasing it (see Engine.Core.Capability.Building's convention)"

spec ∷ SpecWith EngineEnv
spec = do
  describe "toBuildingCapability (all three building fields)" $ do
    let aliases name project field =
          it (name <> " aliases the live EngineEnv container") $ \env →
            sameContainer (project (toBuildingCapability env)) (field env)

    aliases "bcBuildingManagerRef" bcBuildingManagerRef buildingManagerRef
    aliases "bcBuildingQueue"      bcBuildingQueue      buildingQueue
    aliases "bcBuildingGhostRef"   bcBuildingGhostRef   buildingGhostRef

-- (No transposition example, unlike
-- "Test.Headless.Capability.UnitCombat"'s three identically-typed
-- `IORef (Seq CombatEvent)` streams: all three fields here have
-- distinct types — `IORef BuildingManager`, `Queue BuildingCommand`,
-- `IORef (Maybe BuildingGhost)` — so a swapped binding cannot
-- typecheck in the first place. Copying is the only failure mode left,
-- and the examples above are exactly what catch it.)
