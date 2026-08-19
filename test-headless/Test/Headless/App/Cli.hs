-- | @App.Cli@'s absence-vs-malformed-presence contract (#1191).
--
--   Every value parser in that module used to answer \"flag absent\" and
--   \"flag present, value nonsense\" with the SAME 'Nothing', so
--   @Main@ turned a typo into the documented default:
--   @--seed not-a-number@ generated seed 42 and exited 0 with a full,
--   valid, wrong dump, and @--dump=bogus_layer_typo@ exited 0 having
--   silently emitted no layer at all. These are the pure half of the
--   fix — the four outcomes each parser must now distinguish (absent,
--   valid, malformed, missing operand) without any process at all.
--   That the errors reach stderr with a non-zero exit, ahead of every
--   mode-specific early exit, is @tools\/preview_cli_probe.py@'s job.
--
--   @--region@ is deliberately absent from this module:
--   @docs\/code_health_findings.md@ CH-67 tracks 'parseRegion'\'s
--   identical silent default, sequenced after #1081's named-region
--   type, and #1191 leaves its behavior untouched. That named type
--   landed and has its own cases in "Test.Headless.App.ChunkRegion";
--   the silent default it still applies is asserted there.
module Test.Headless.App.Cli (spec) where

import UPrelude
import Test.Hspec
import Data.List (isInfixOf)
import App.Cli
  ( DumpLayers(..), defaultLayers, dumpLayerNames
  , CliError(..), cliErrorMessage
  , parseDump, parseArg, parseSize, lookupFlagValue )

-- | The layers a selection turned on, as a comparable tuple —
--   'DumpLayers' has no 'Eq' instance and gaining one for a test would
--   be the test dictating the type.
layerFlags ∷ DumpLayers → [Bool]
layerFlags l = [dlTerrain l, dlMaterial l, dlFluid l, dlIce l, dlOre l, dlSlope l]

-- | 'parseDump''s whole answer, made comparable: the error, or the
--   layer flags it selected ('Nothing' when @--dump@ was absent).
dumpFlags ∷ [String] → Either CliError (Maybe [Bool])
dumpFlags args = (layerFlags ⊚) ⊚ parseDump args

-- | 'parseArg' at the type every caller in @Main@ uses it at.
intArg ∷ String → [String] → Either CliError (Maybe Int)
intArg = parseArg

spec ∷ Spec
spec = describe "App.Cli value validation (#1191)" $ do

  describe "parseArg" $ do
    it "reports absence as Right Nothing — the caller's default still \
       \applies when the flag was never typed" $ do
      intArg "--seed" [] `shouldBe` Right Nothing
      intArg "--seed" ["--headless", "--port", "9008"] `shouldBe` Right Nothing

    it "parses a valid value, including a negative one" $ do
      intArg "--seed" ["--seed", "42"] `shouldBe` Right (Just 42)
      intArg "--port" ["--headless", "--port", "-1"] `shouldBe` Right (Just (-1))

    it "rejects a malformed value instead of falling through to the \
       \default — the #1191 bug in one line" $
      intArg "--seed" ["--dump", "--seed", "not-a-number"]
        `shouldBe` Left (BadNumericValue "--seed" "not-a-number")

    it "rejects a partially numeric value: 'reads' must consume the \
       \WHOLE token" $ do
      intArg "--worldSize" ["--worldSize", "16x16"]
        `shouldBe` Left (BadNumericValue "--worldSize" "16x16")
      intArg "--worldSize" ["--worldSize", "16 "]
        `shouldBe` Left (BadNumericValue "--worldSize" "16 ")

    it "treats a trailing flag with no operand as present-but-invalid, \
       \not as absence" $
      intArg "--port" ["--headless", "--port"]
        `shouldBe` Left (MissingFlagValue "--port")

    it "does not confuse a trailing OTHER flag with a missing operand" $
      intArg "--port" ["--headless"] `shouldBe` Right Nothing

    it "lets the FIRST occurrence decide: a malformed one is an error, \
       \never something to skip past in search of a later good one" $
      intArg "--seed" ["--seed", "oops", "--seed", "42"]
        `shouldBe` Left (BadNumericValue "--seed" "oops")

    it "finds the flag at any position" $
      intArg "--plates" ["--dump", "--worldSize", "16", "--plates", "5"]
        `shouldBe` Right (Just 5)

  describe "lookupFlagValue" $
    it "is the one lookup all of the above share, with the same three \
       \answers" $ do
      lookupFlagValue "--size" [] `shouldBe` Right Nothing
      lookupFlagValue "--size" ["--size", "1280x720"]
        `shouldBe` Right (Just "1280x720")
      lookupFlagValue "--size" ["--offscreen", "--size"]
        `shouldBe` Left (MissingFlagValue "--size")

  describe "parseSize" $ do
    it "reports absence as Right Nothing — offscreen still falls back \
       \to the video-config resolution" $
      parseSize ["--offscreen", "--port", "9008"] `shouldBe` Right Nothing

    it "parses a valid WxH, case-insensitively" $ do
      parseSize ["--size", "1280x720"] `shouldBe` Right (Just (1280, 720))
      parseSize ["--size", "1280X720"] `shouldBe` Right (Just (1280, 720))

    it "rejects a malformed value rather than silently rendering at the \
       \local config's resolution" $ do
      parseSize ["--size", "not-a-size"]
        `shouldBe` Left (BadSizeValue "not-a-size")
      parseSize ["--size", "1280"] `shouldBe` Left (BadSizeValue "1280")
      parseSize ["--size", "1280x720x2"]
        `shouldBe` Left (BadSizeValue "1280x720x2")

    it "keeps positivity a --size-specific rule, and rejects rather \
       \than defaults on it" $ do
      parseSize ["--size", "0x100"] `shouldBe` Left (BadSizeValue "0x100")
      parseSize ["--size", "100x0"] `shouldBe` Left (BadSizeValue "100x0")
      parseSize ["--size", "-4x100"] `shouldBe` Left (BadSizeValue "-4x100")

    it "treats a trailing --size with no operand as present-but-invalid" $
      parseSize ["--offscreen", "--size"]
        `shouldBe` Left (MissingFlagValue "--size")

  describe "parseDump" $ do
    it "reports absence as Right Nothing" $
      dumpFlags ["--headless", "--port", "9008"] `shouldBe` Right Nothing

    it "keeps a bare --dump on the documented default five layers, \
       \slope still opt-in (the worldgen baselines drive this exact \
       \invocation)" $
      dumpFlags ["--dump"] `shouldBe` Right (Just (layerFlags defaultLayers))

    it "accepts a selection, case-insensitively, with the elevation \
       \alias still meaning terrain" $ do
      dumpFlags ["--dump=terrain,ice"]
        `shouldBe` Right (Just [True, False, False, True, False, False])
      dumpFlags ["--dump=ELEVATION,Slope"]
        `shouldBe` Right (Just [True, False, False, False, False, True])

    it "rejects an empty selection instead of emitting tile records \
       \carrying nothing but coordinates" $
      dumpFlags ["--dump="] `shouldBe` Left EmptyDumpSelection

    it "rejects an unknown layer token, naming it as the user typed it" $
      dumpFlags ["--dump=bogus_layer_typo"]
        `shouldBe` Left (UnknownDumpLayer "bogus_layer_typo")

    it "identifies an empty segment inside a non-empty selection AS \
       \empty, by position — not as an unknown layer named \"\"" $ do
      dumpFlags ["--dump=terrain,"] `shouldBe` Left (EmptyDumpLayerName 2)
      dumpFlags ["--dump=terrain,,fluid"] `shouldBe` Left (EmptyDumpLayerName 2)
      dumpFlags ["--dump=,terrain"] `shouldBe` Left (EmptyDumpLayerName 1)

    it "names the FIRST offending token when a selection has several" $
      dumpFlags ["--dump=nope,alsonope"]
        `shouldBe` Left (UnknownDumpLayer "nope")

    it "accepts every name it advertises in dumpLayerNames" $
      map (\n → isRight (dumpFlags ["--dump=" ⧺ n])) dumpLayerNames
        `shouldSatisfy` and

  describe "cliErrorMessage" $ do
    it "names the flag and the offending token verbatim, so the user \
       \can find it in their own command line" $ do
      cliErrorMessage (BadNumericValue "--seed" "not-a-number")
        `shouldSatisfy` isInfixOf "--seed"
      cliErrorMessage (BadNumericValue "--seed" "not-a-number")
        `shouldSatisfy` isInfixOf "not-a-number"
      cliErrorMessage (MissingFlagValue "--port")
        `shouldSatisfy` isInfixOf "--port"

    it "says 'empty' for an empty segment rather than quoting an empty \
       \token" $
      cliErrorMessage (EmptyDumpLayerName 2) `shouldSatisfy` isInfixOf "empty"

    it "lists the accepted layer names on every dump-selection error, \
       \so the fix is in the message" $
      map (\e → all (`isInfixOf` cliErrorMessage e) dumpLayerNames)
          [EmptyDumpSelection, EmptyDumpLayerName 1, UnknownDumpLayer "x"]
        `shouldSatisfy` and

    it "is a single line on every constructor" $
      map (length ∘ lines ∘ cliErrorMessage)
          [ MissingFlagValue "--port"
          , BadNumericValue "--seed" "x"
          , BadSizeValue "x"
          , EmptyDumpSelection
          , EmptyDumpLayerName 1
          , UnknownDumpLayer "x" ]
        `shouldSatisfy` all (≡ 1)

isRight ∷ Either a b → Bool
isRight = either (const False) (const True)
