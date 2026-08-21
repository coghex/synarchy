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
--   #1481 then closed @docs\/code_health_findings.md@ CH-67 here, so
--   these cases now pin the OPPOSITE of what they originally asserted:
--   'parseRegion' was the last typed-value parser under #1191's
--   contract still answering "absent" and "present, nonsense" with the
--   same 'defaultChunkRegion', and a typo in a long dump produced a
--   full, valid, WRONG region at exit 0. All four outcomes are checked
--   below — absent, valid, malformed, missing operand — plus the
--   first-occurrence precedence that keeps a malformed value from being
--   skipped past in search of a later well-formed one. That those
--   errors reach stderr with exit 1 before anything boots, and that the
--   mode-compatibility rejection still outranks them, is
--   @tools\/preview_cli_probe.py@'s job.
module Test.Headless.App.ChunkRegion (spec) where

import UPrelude
import Test.Hspec
import Data.List (isInfixOf)
import App.Cli
  ( ChunkRegion(..), defaultChunkRegion, parseRegion, chunkRegionCoords
  , CliError(..), cliErrorMessage )

-- | A region's four coordinates in flag order, so a swapped pair is a
--   failing list rather than a passing one under different names.
corners ∷ ChunkRegion → [Int]
corners r = [crX1 r, crY1 r, crX2 r, crY2 r]

-- | The four coordinates of a region a parse was expected to ACCEPT.
--   A 'Left' or a 'Right' 'Nothing' fails as a distinguishable list
--   rather than as a pattern-match error.
parsedCorners ∷ [String] → Either String [Int]
parsedCorners args = case parseRegion args of
    Right (Just r) → Right (corners r)
    Right Nothing  → Left "absent"
    Left err       → Left ("rejected: " ⧺ show err)

spec ∷ Spec
spec = describe "App.Cli chunk region (#1081, #1481)" $ do

  describe "parseRegion" $ do
    it "lands each --region coordinate in its own named field, in flag \
       \order -- four DISTINCT values, so exchanging any two fails" $
      parsedCorners ["--region", "1,2,3,4"] `shouldBe` Right [1, 2, 3, 4]

    it "answers absence with Right Nothing, so the CALLER applies the \
       \historical default (#1481)" $ do
      parseRegion [] `shouldBe` Right Nothing
      parseRegion ["--dump", "--seed", "42"] `shouldBe` Right Nothing
      corners defaultChunkRegion `shouldBe` [-8, -8, 8, 8]

    it "REJECTS every malformed shape that used to become the default \
       \silently -- CH-67, closed by #1481" $ do
      parseRegion ["--region", "bogus"]
        `shouldBe` Left (BadRegionValue "bogus")
      parseRegion ["--region", "1,2,3"]
        `shouldBe` Left (BadRegionValue "1,2,3")
      parseRegion ["--region", "1,2,3,4,5"]
        `shouldBe` Left (BadRegionValue "1,2,3,4,5")
      parseRegion ["--region", "1,2,3,x"]
        `shouldBe` Left (BadRegionValue "1,2,3,x")

    it "reports a bare trailing --region as a flag missing its value, \
       \NOT as absence" $
      parseRegion ["--dump", "--region"]
        `shouldBe` Left (MissingFlagValue "--region")

    it "names --region and the offending token exactly as typed" $ do
      let msg = cliErrorMessage (BadRegionValue "1,2,3,x")
      msg `shouldSatisfy` isInfixOf "--region"
      msg `shouldSatisfy` isInfixOf "1,2,3,x"
      cliErrorMessage (MissingFlagValue "--region")
        `shouldSatisfy` isInfixOf "--region"

    it "lets the FIRST occurrence decide, so a malformed one is an \
       \error rather than something to skip past" $ do
      parseRegion ["--region", "bogus", "--region", "1,2,3,4"]
        `shouldBe` Left (BadRegionValue "bogus")
      parsedCorners ["--region", "1,2,3,4", "--region", "5,6,7,8"]
        `shouldBe` Right [1, 2, 3, 4]

    it "parses negative coordinates, which is the ordinary case" $
      parsedCorners ["--dump", "--region", "-4,-3,4,3"]
        `shouldBe` Right [-4, -3, 4, 3]

    it "accepts a reversed corner pair: only the SHAPE is validated, \
       \and a directed region covering nothing is well-formed" $
      parsedCorners ["--region", "2,3,0,1"] `shouldBe` Right [2, 3, 0, 1]

    it "does not require --region to be first" $
      parsedCorners ["--dump", "--worldSize", "32"
                    , "--region", "0,1,2,3"]
        `shouldBe` Right [0, 1, 2, 3]

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
