{-# LANGUAGE Strict #-}
-- | Pure tests for the compiled unit-animation atlas runtime (#1259,
--   TEX-3), composed from three owners:
--
--     * "Test.Headless.Unit.Atlas.Index" — the generated index document
--       and its schema: what a well-formed one decodes to, and every
--       malformed, foreign or truncated one it must reject.
--     * "Test.Headless.Unit.Atlas.Freshness" — the compiled ARTIFACT:
--       digest parity with @tools\/pack_atlas.py@, decoded-image
--       validation, YAML selection, source-art freshness, and the real
--       temporary-filesystem loading boundary.
--     * "Test.Headless.Unit.Atlas.Consumers" — the runtime geometry
--       that READS a frame: the render quad, the hit rect, texel
--       equivalence, the extrusion ring, clipping and mirroring,
--       sampler and cache policy, and UI publication.
--
--   Everything here is pure and boots no engine. The live unit
--   registration boundary is "Test.Headless.Unit.Atlas.Loader", which
--   "Spec" runs separately under its own engine fixture.
module Test.Headless.Unit.Atlas (spec) where

import Test.Hspec
import qualified Test.Headless.Unit.Atlas.Consumers as Consumers
import qualified Test.Headless.Unit.Atlas.Freshness as Freshness
import qualified Test.Headless.Unit.Atlas.Index as Index

-- | The owners sequenced directly, with no wrapping 'describe': every
--   hspec path stays exactly what it was before the split.
spec ∷ Spec
spec = do
    Index.spec
    Freshness.spec
    Consumers.spec
