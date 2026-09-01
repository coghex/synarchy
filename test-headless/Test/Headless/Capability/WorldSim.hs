-- | Projection-aliasing coverage for the @world-sim-render-handoff@
--   world\/sim capability record (issue #893, E5a of the @EngineEnv@
--   capability split #537) — the direct counterpart of
--   "Test.Headless.Capability.Render", for the same reason.
--
--   'Engine.Core.Capability.WorldSim.toWorldSimCapability' is documented
--   as returning the __identical live containers__ 'EngineEnv' already
--   carries, never a copy and never a snapshot. That property is what
--   makes the record safe to project as often as a consumer likes — and
--   after this migration a great many consumers do exactly that, several
--   of them re-projecting inline at every field access. A projection
--   that bound 'wsWorldQueue' to @simQueue env@, or copied a container
--   with @newIORef =\<\< readIORef@, would still typecheck, still pass
--   the SS6 ratchet, and still look right in a diff — while silently
--   routing world commands into the sim thread's queue.
--
--   Every field of the record is either an 'Data.IORef.IORef' (pointer
--   'Eq') or an 'Engine.Core.Queue.Queue' (which derives 'Eq' through
--   its 'TQueue'), so "same live container" is directly assertable:
--   compare the projected field against the same 'EngineEnv' field. A
--   wrong-container binding fails the corresponding example — including
--   a SWAP between the two same-typed queues or between two same-typed
--   refs, since each field is checked against its own named counterpart
--   rather than merely "some field of the env".
--
--   Per SS6.3 test fixtures are outside the full-access ratchet, so this
--   module imports @EngineEnv(..)@ directly — that is the point: it
--   compares the capability's view against the unrestricted one.
module Test.Headless.Capability.WorldSim (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.WorldSim
  (WorldSimCapability(..), toWorldSimCapability)

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
      \aliasing it (see Engine.Core.Capability.WorldSim's convention)"

spec ∷ SpecWith EngineEnv
spec = do
  describe "toWorldSimCapability (nine of the eleven world/sim fields)" $ do
    -- The record carries eleven fields; `wsPlayerIntentGenRef` and
    -- `wsEnginePauseGenRef` were added later and have no alias
    -- assertion here yet. Adding them is its own change; this
    -- heading states what is actually covered.
    let aliases name project field =
          it (name <> " aliases the live EngineEnv container") $ \env →
            sameContainer (project (toWorldSimCapability env)) (field env)

    aliases "wsWorldManagerRef"     wsWorldManagerRef     worldManagerRef
    aliases "wsWorldQueue"          wsWorldQueue          worldQueue
    aliases "wsSunAngleRef"         wsSunAngleRef         sunAngleRef
    aliases "wsFloraCatalogRef"     wsFloraCatalogRef     floraCatalogRef
    aliases "wsMaterialRegistryRef" wsMaterialRegistryRef materialRegistryRef
    aliases "wsWorldGenConfigRef"   wsWorldGenConfigRef   worldGenConfigRef
    aliases "wsGameTimeRef"         wsGameTimeRef         gameTimeRef
    aliases "wsEnginePausedRef"     wsEnginePausedRef     enginePausedRef
    aliases "wsSimQueue"            wsSimQueue            simQueue

    it "keeps the two command queues distinct" $ \env → do
      -- worldQueue and simQueue are the record's only same-shaped pair
      -- of STM queues in the sense that a transposed projection would
      -- still compile at every world-thread producer that writes both
      -- (World.Thread.Command.UI and .Edit.Sync each touch one). The
      -- per-field examples above already pin each to its own named
      -- counterpart; this states the risk explicitly and asserts they
      -- really are two different containers.
      let cap = toWorldSimCapability env
      sameContainer (wsWorldQueue cap) (worldQueue env)
      sameContainer (wsSimQueue cap) (simQueue env)

    it "keeps the pause flag and the game clock on their own refs" $ \env → do
      -- enginePausedRef/gameTimeRef are the pair the unit thread reads
      -- together every tick ("advance the clock only while unpaused"),
      -- so a swap here would be a live simulation bug, not a type
      -- error -- they have different element types, but both are read
      -- through the same projection in the same expression.
      let cap = toWorldSimCapability env
      sameContainer (wsEnginePausedRef cap) (enginePausedRef env)
      sameContainer (wsGameTimeRef cap) (gameTimeRef env)
