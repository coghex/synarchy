-- | The dump's named chunk region (#1081).
--
--   @--region@'s four coordinates used to be a bare four-'Int' tuple,
--   so @(cx1, cy1, cx2, cy2)@ and @(cx1, cx2, cy1, cy2)@ were the same
--   type: the corner convention lived only in the destructuring
--   patterns at the two sites that read it, and a swap between them
--   compiled and dumped a different region under the original labels.
--   'ChunkRegion' moves that convention into the type; these cases pin
--   the half a type cannot state — WHICH coordinate each flag position
--   lands in, and what the region then enumerates.
--
--   Enumeration is checked here rather than at the two call sites
--   because both of them (queueing the chunks, then encoding them) walk
--   'chunkRegionCoords' and must agree tile for tile: the JSON is
--   emitted for whatever the queue loaded.
--
--   Behavior is deliberately unchanged. In particular a malformed
--   @--region@ still silently becomes 'defaultChunkRegion' — that is
--   @docs\/code_health_findings.md@ CH-67, sequenced after this type
--   and out of scope for #1081, so it is asserted here as the CURRENT
--   contract rather than left unstated.
module Test.Headless.App.ChunkRegion (spec) where

import UPrelude
import Test.Hspec
import App.Cli
  ( ChunkRegion(..), defaultChunkRegion, parseRegion, chunkRegionCoords )

-- | A region's four coordinates in flag order, so a swapped pair is a
--   failing list rather than a passing one under different names.
corners ∷ ChunkRegion → [Int]
corners r = [crX1 r, crY1 r, crX2 r, crY2 r]

spec ∷ Spec
spec = describe "App.Cli chunk region (#1081)" $ do

  describe "parseRegion" $ do
    it "lands each --region coordinate in its own named field, in flag \
       \order -- four DISTINCT values, so exchanging any two fails" $
      corners (parseRegion ["--region", "1,2,3,4"]) `shouldBe` [1, 2, 3, 4]

    it "keeps the historical default when --region is absent" $ do
      parseRegion [] `shouldBe` defaultChunkRegion
      parseRegion ["--dump", "--seed", "42"] `shouldBe` defaultChunkRegion
      corners defaultChunkRegion `shouldBe` [-8, -8, 8, 8]

    it "still substitutes that default for a malformed value -- CH-67, \
       \deliberately untouched here" $ do
      parseRegion ["--region", "bogus"] `shouldBe` defaultChunkRegion
      parseRegion ["--region", "1,2,3"] `shouldBe` defaultChunkRegion
      parseRegion ["--region", "1,2,3,4,5"] `shouldBe` defaultChunkRegion
      parseRegion ["--region", "1,2,3,x"] `shouldBe` defaultChunkRegion
      parseRegion ["--region"] `shouldBe` defaultChunkRegion

    it "parses negative coordinates, which is the ordinary case" $
      corners (parseRegion ["--dump", "--region", "-4,-3,4,3"])
        `shouldBe` [-4, -3, 4, 3]

    it "does not require --region to be first" $
      corners (parseRegion ["--dump", "--worldSize", "32"
                           , "--region", "0,1,2,3"])
        `shouldBe` [0, 1, 2, 3]

  describe "chunkRegionCoords" $ do
    it "enumerates x outer and y inner, both ranges inclusive -- the \
       \order the dump's tile records are emitted in" $
      chunkRegionCoords (ChunkRegion { crX1 = 0, crY1 = 0
                                     , crX2 = 1, crY2 = 2 })
        `shouldBe` [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)]

    it "covers one chunk when both corners coincide" $
      chunkRegionCoords (ChunkRegion { crX1 = 3, crY1 = -5
                                     , crX2 = 3, crY2 = -5 })
        `shouldBe` [(3, -5)]

    it "leaves a reversed corner pair EMPTY rather than sorting it: the \
       \ranges are directed, which is what a reversed --region has \
       \always dumped" $ do
      chunkRegionCoords (ChunkRegion { crX1 = 2, crY1 = 0
                                     , crX2 = 1, crY2 = 3 })
        `shouldBe` []
      chunkRegionCoords (ChunkRegion { crX1 = 0, crY1 = 3
                                     , crX2 = 3, crY2 = 0 })
        `shouldBe` []

    it "covers the default region's full 17x17 chunk square" $
      length (chunkRegionCoords defaultChunkRegion) `shouldBe` 17 * 17
