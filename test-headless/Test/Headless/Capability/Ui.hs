-- | Projection-aliasing coverage for the @ui-hud-events@ UI\/focus\/HUD
--   capability record (issue #897, E7a of the @EngineEnv@ capability
--   split #537).
--
--   'Engine.Core.Capability.Ui.toUiCapability' is documented as
--   returning the __identical live containers__ 'EngineEnv' already
--   carries — never a copy, never a snapshot. That property is what
--   makes the record safe to re-project inline at every call site (E7a
--   does exactly that), and it is precisely what a refactor can
--   silently break: a projection that bound 'uicFocusManagerRef' to
--   @uiManagerRef env@, or copied a container with
--   @newIORef =<< readIORef@, would still typecheck, still pass the SS6
--   ratchet, and still look right in a diff — while quietly splitting
--   the UI tree the input thread mutates from the one Lua reads.
--
--   Every field of this record is an 'Data.IORef.IORef' (pointer
--   'Eq'), so "same live container" is directly assertable: compare the
--   projected field against the same 'EngineEnv' field. Each field is
--   checked against its OWN named counterpart, so a swap between two
--   fields survives neither.
--
--   Per SS6.3 test fixtures are outside the full-access ratchet, so
--   this module imports @EngineEnv(..)@ directly — that is the point:
--   it compares the capability's view against the unrestricted one.
module Test.Headless.Capability.Ui (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.Ui (UiCapability(..), toUiCapability)

-- | Assert two live containers are the SAME one. 'IORef' has no 'Show'
--   instance, so this is an 'Eq'-only assertion carrying its own
--   failure message rather than 'shouldBe'\'s rendered-value diff.
sameContainer ∷ Eq α ⇒ α → α → Expectation
sameContainer projected live
  | projected == live = pure ()
  | otherwise = expectationFailure
      "projected field is NOT the live EngineEnv container -- the \
      \projection copied, swapped, or reconstructed it instead of \
      \aliasing it (see Engine.Core.Capability.Core's convention)"

spec ∷ SpecWith EngineEnv
spec = do
  describe "toUiCapability (all four ui-hud-events UI/focus/HUD fields)" $ do
    let aliases name project field =
          it (name <> " aliases the live EngineEnv container") $ \env →
            sameContainer (project (toUiCapability env)) (field env)

    aliases "uicUiManagerRef"     uicUiManagerRef     uiManagerRef
    aliases "uicFocusManagerRef"  uicFocusManagerRef  focusManagerRef
    aliases "uicHudActivePageRef" uicHudActivePageRef hudActivePageRef
    aliases "uicTextBuffersRef"   uicTextBuffersRef   textBuffersRef

    it "keeps the two focus-carrying refs distinct" $ \env → do
      -- `uiManagerRef` carries TEXT focus + CONTROL focus (#745) while
      -- `focusManagerRef` carries the Lua-facing focus-target registry
      -- — two different notions of "focus" over two different
      -- containers, mutated by the same input-thread dispatch within a
      -- single key event. Transposing them would still typecheck at
      -- neither call site (the payload types differ), but this states
      -- the risk explicitly and pins each to its own counterpart.
      let cap = toUiCapability env
      sameContainer (uicUiManagerRef cap) (uiManagerRef env)
      sameContainer (uicFocusManagerRef cap) (focusManagerRef env)
