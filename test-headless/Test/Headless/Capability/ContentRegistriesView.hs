-- | 'Engine.Core.ReadOnlyRef.ReadOnlyRef' liveness plus
--   projection-aliasing coverage for the reader-facing
--   @content-registries@ view (issue #1896, CMA-2 of the capability
--   mutation-authority epic #1890) — the same contract every other
--   @Test.Headless.Capability.*@ module pins, extended by the one
--   property the wrapper adds.
--
--   __The wrapper must alias, never snapshot.__ 'ReadOnlyRef' exists to
--   deny WRITES, not to freeze a value. A construction that copied
--   (@newIORef =\<\< readIORef@) would still typecheck, still deny
--   writes, and still look right in a diff — while handing every reader
--   a private registry the boot-time @X.loadYaml@ populators never write
--   into, so a content catalogue would simply appear empty. The first
--   group below is that property, proved the only way it can be: mutate
--   through the raw handle, observe through the wrapper.
--
--   __The view and the raw writer record share one container.__ The
--   four registries this pilot selects are reached two ways —
--   "Engine.Core.Capability.ContentRegistries" by their one legitimate
--   writer each, "Engine.Core.Capability.ContentRegistriesView" by every
--   reader. Those must be the SAME live handle or the boundary would
--   have split the state instead of the authority, so the last group
--   asserts the two records against each other, not merely each against
--   'EngineEnv'.
--
--   Per SS6.3 test fixtures are outside the full-access ratchet, so this
--   module imports @EngineEnv(..)@ directly — that is the point: it
--   compares the capability's view against the unrestricted one.
module Test.Headless.Capability.ContentRegistriesView (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (bracket_)
import Data.IORef (newIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.ReadOnlyRef (toReadOnlyRef, readReadOnlyRef)
import Item.Types (emptyItemManager)
import Engine.Core.Capability.ContentRegistries
  (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.ContentRegistriesView
  (ContentRegistriesViewCapability(..), toContentRegistriesViewCapability)

-- | Assert two live containers are the SAME one. Neither 'ReadOnlyRef'
--   nor 'Data.IORef.IORef' has a 'Show' instance, so this is an
--   'Eq'-only assertion carrying its own failure message rather than
--   'shouldBe'\'s rendered-value diff.
sameContainer ∷ Eq α ⇒ α → α → Expectation
sameContainer projected live
  | projected == live = pure ()
  | otherwise = expectationFailure
      "projected field is NOT the live EngineEnv container -- the \
      \projection copied, swapped, or reconstructed it instead of \
      \aliasing it (see Engine.Core.Capability.ContentRegistriesView's \
      \convention)"

spec ∷ SpecWith EngineEnv
spec = do
  describe "ReadOnlyRef aliases a live IORef" $ do
    it "observes a write made through the raw handle" $ \_ → do
      raw ← newIORef (1 ∷ Int)
      let ro = toReadOnlyRef raw
      before' ← readReadOnlyRef ro
      before' `shouldBe` 1
      writeIORef raw 2
      after' ← readReadOnlyRef ro
      after' `shouldBe` 2

    it "keeps observing later writes (it is not a one-shot snapshot)" $ \_ → do
      raw ← newIORef (0 ∷ Int)
      let ro = toReadOnlyRef raw
      observed ← mapM (\n → atomicModifyIORef' raw (const (n, ()))
                              >> readReadOnlyRef ro) [1 .. 4 ∷ Int]
      observed `shouldBe` [1, 2, 3, 4]

  describe "toContentRegistriesViewCapability (five projected fields)" $ do
    let wrapped name project field =
          it (name <> " aliases the live EngineEnv container") $ \env →
            sameContainer (project (toContentRegistriesViewCapability env))
                          (toReadOnlyRef (field env))

    wrapped "crvItemManagerRef"
            crvItemManagerRef           itemManagerRef
    wrapped "crvEquipmentClassManagerRef"
            crvEquipmentClassManagerRef equipmentClassManagerRef
    wrapped "crvSubstanceManagerRef"
            crvSubstanceManagerRef      substanceManagerRef
    wrapped "crvRecipeManagerRef"
            crvRecipeManagerRef         recipeManagerRef

    -- The one deliberately UNWRAPPED field: infection is outside this
    -- pilot's structural boundary and rides here only so
    -- Engine.Scripting.Lua.API.Units.Combat needs no raw record.
    it "crvInfectionManagerRef aliases the live EngineEnv container" $ \env →
      sameContainer (crvInfectionManagerRef (toContentRegistriesViewCapability env))
                    (infectionManagerRef env)

  describe "the reader view and the raw writer record share one container" $ do
    let shared name viewField rawField =
          it (name <> " is the same handle on both records") $ \env →
            sameContainer (viewField (toContentRegistriesViewCapability env))
                          (toReadOnlyRef
                             (rawField (toContentRegistriesCapability env)))

    shared "the item registry"
           crvItemManagerRef           crItemManagerRef
    shared "the equipment-class registry"
           crvEquipmentClassManagerRef crEquipmentClassManagerRef
    shared "the substance registry"
           crvSubstanceManagerRef      crSubstanceManagerRef
    shared "the recipe registry"
           crvRecipeManagerRef         crRecipeManagerRef

    it "tracks writes made through the raw writer record, both ways" $ \env → do
      -- The whole point of the split: the four `X.loadYaml` populators
      -- keep the raw record and their insert/replace semantics, and
      -- every reader sees the result immediately through the wrapper.
      -- Two DISTINCT writes are observed here, so this cannot pass by
      -- the view having captured one value at projection time.
      -- The env is shared with every other spec, so the registry is
      -- restored under `bracket_` (the same shape
      -- Test.Headless.WorldGen uses on `crLocationDefsRef`).
      let raw     = crItemManagerRef (toContentRegistriesCapability env)
          roItems = crvItemManagerRef (toContentRegistriesViewCapability env)
      original ← readReadOnlyRef roItems
      bracket_ (writeIORef raw emptyItemManager) (writeIORef raw original) $ do
        observed ← readReadOnlyRef roItems
        observed `shouldBe` emptyItemManager
      restored ← readReadOnlyRef roItems
      restored `shouldBe` original
