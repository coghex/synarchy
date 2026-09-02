-- | The "save migrations" gate (issue #766, save-overhaul C4): the
--   stable aggregate every documented @--match@ command and
--   @tools/save_compat_audit.py@'s real-codec validation address, and
--   the ONLY module @test-headless/Spec.hs@ registers for it. Pure —
--   no engine, no IO beyond the families' read-only access to the
--   tracked compatibility fixtures.
--
--   The contract bodies live with their owning family (#2094):
--
--   * "Test.Headless.World.Save.Compat.Baselines" — manifest-declared
--     fixture decoding and canonical summaries, and the per-baseline
--     container-knowledge, river-name/etymology and movement-hazard
--     contracts;
--
--   * "Test.Headless.World.Save.Compat.Historical" — the frozen B1/v90
--     session contract and the historical world-page shapes (language
--     provenance, stored identities, locations, rivers, encounters);
--
--   * "Test.Headless.World.Save.Compat.Legacy" — unknown optional-data
--     rejection, overwrite protection, exact B1/B2 shape recognition,
--     the B2 fallback migration, and classified error phases.
--
--   Fixture truth shared by more than one family — the real, tracked
--   B1 envelope bytes and their decode helpers — has its one
--   definition in "Test.Headless.World.Save.Compat.B1Fixture".
--
--   Composition is at describe-group granularity, and the order below
--   is the order these eight groups have always run in. The families
--   INTERLEAVE — one baseline group, the two historical groups, three
--   more baseline groups, then the two legacy groups — so composing
--   one family after another would reorder the printed tree and move
--   the deterministic first failure; sequencing the groups here keeps
--   every item path byte-identical to the single-module suite this
--   replaced. No describe level sits between "save migrations" and
--   these eight, for the same reason.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "save migrations"'@.
module Test.Headless.World.Save.Compat (spec) where

import Test.Hspec
import qualified Test.Headless.World.Save.Compat.Baselines as Baselines
import qualified Test.Headless.World.Save.Compat.Historical as Historical
import qualified Test.Headless.World.Save.Compat.Legacy as Legacy

spec ∷ Spec
spec = do
    Baselines.manifestFixturesSpec
    Historical.frozenV90Spec
    Historical.languageProvenanceSpec
    Baselines.containerKnowledgeSpec
    Baselines.riverNamesSpec
    Baselines.movementHazardSpec
    Legacy.unknownOptionalDataSpec
    Legacy.b2FallbackSpec
