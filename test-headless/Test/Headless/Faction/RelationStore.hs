-- | The "Faction relation store" gate (#2548, FTS-6B of the faction-tag
--   arc #2496): the persisted owner of every live directed relation
--   cause. Composed from four owners; the live Lua half needs its own
--   engine and so is registered separately by @test-headless/Spec.hs@
--   ("Test.Headless.Faction.RelationStoreLive").
--
--   * "Test.Headless.Faction.RelationStore.Model" — identity, merge and
--     removal against exact expected overlays;
--   * "Test.Headless.Faction.RelationStore.Persistence" — the
--     @"relation-causes"@ component through real envelope bytes;
--   * "Test.Headless.Faction.RelationStore.Lifecycle" — the real save,
--     load, publish and teardown boundaries on a private engine.
--
--   Run the whole gate: @cabal test synarchy-test-headless
--   --test-options='--match "Faction relation store"'@.
module Test.Headless.Faction.RelationStore (spec) where

import UPrelude
import Test.Hspec
import qualified Test.Headless.Faction.RelationStore.Lifecycle as Lifecycle
import qualified Test.Headless.Faction.RelationStore.Model as Model
import qualified Test.Headless.Faction.RelationStore.Persistence as Persistence

spec ∷ Spec
spec = describe "Faction relation store" $ do
    Model.spec
    Persistence.spec
    describe "lifecycle" Lifecycle.spec
