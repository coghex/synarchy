-- | #2286: what 'loadWorldGenConfig' says about the file it was pointed
--   at, and what it returns.
--
--   The loader used to treat a malformed file exactly like an absent
--   one — the complete defaults, the decoder error discarded — so a
--   single typo in @config/world_gen_default.yaml@ reverted every
--   authored plate, calendar, erosion, volcanism and ore lever with
--   nothing in the log. The fallback is unchanged and deliberately
--   whole-document; only the silence is gone.
--
--   Filesystem and logger only: no engine boots here and no world is
--   generated. The loader is one 'doesFileExist' and one
--   'Yaml.decodeFileEither', so the shared-world fixture in
--   @test-headless\/Spec.hs@ would cost a generation and prove nothing
--   extra (@src\/World\/CLAUDE.md@).
module Test.Headless.WorldGen.ConfigLoad (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import System.FilePath ((</>))
import Engine.Core.Log
  ( LogBackend(..), LogCategory(..), LogConfig(..), LogEntry(..)
  , LogLevel(..), LoggerState, defaultLogConfig, initLogger )
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)
import World.Generate.Config
  ( WorldGenConfig(..), WorldGenConfigRaw, defaultWorldGenConfig
  , loadWorldGenConfig, minimumWorldSize )

-- | A logger whose entries are captured in emission order. Everything
--   the loader emits is visible, not just the warnings, so "no warning"
--   can be asserted as "nothing at all".
capturingLogger ∷ IO (LoggerState, IO [LogEntry])
capturingLogger = do
    ref ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback
            (\e → atomicModifyIORef' ref (\es → (e : es, ()))) }
    pure (logger, reverse ⊚ readIORef ref)

-- | Run the loader against @path@ and hand back both halves of its
--   observable behaviour.
loadFrom ∷ FilePath → IO (WorldGenConfig, [LogEntry])
loadFrom path = do
    (logger, drain) ← capturingLogger
    cfg ← loadWorldGenConfig logger path
    entries ← drain
    pure (cfg, entries)

-- | The @(level, category)@ of every entry, in order. One warning is
--   @[(LevelWarn, CatInit)]@ and silence is @[]@, so both requirements
--   are the same assertion.
shapeOf ∷ [LogEntry] → [(LogLevel, LogCategory)]
shapeOf = map (\e → (leLevel e, leCategory e))

-- | What the decoder itself says about this file, obtained
--   independently of the loader. Asserting that the warning CONTAINS
--   this is what separates passing the real diagnostic through from
--   emitting a canned "config was bad" line.
--
--   Decoded as 'WorldGenConfigRaw' because that is the document shape
--   the loader itself decodes (#2288): a float leaf decodes there
--   preserving its source spelling, so a non-finite scalar is a
--   field-local domain rejection rather than the structural failure
--   this helper is about. Decoding to anything else here would be
--   comparing against a decoder the loader does not run.
decoderErrorFor ∷ FilePath → IO Text
decoderErrorFor path = do
    result ← Yaml.decodeFileEither path
    case result ∷ Either Yaml.ParseException WorldGenConfigRaw of
        Left err → pure (tshow err)
        Right _  → do
            expectationFailure ("expected " ⧺ path ⧺ " not to decode")
            pure ""

shouldContainText ∷ Text → Text → Expectation
shouldContainText haystack needle =
    (needle `T.isInfixOf` haystack, T.unpack haystack)
      `shouldBe` (True, T.unpack haystack)

inTemp ∷ (FilePath → IO α) → IO α
inTemp = withExclusiveTempDirectory "world-gen-config"

-- | A document carrying one setting that WOULD have been honoured
--   (@world_size@) and one that cannot decode (@plate_count@ is a
--   string where an Int is required). Requirement 4 is about this
--   combination: the good setting must be discarded with the bad one.
malformedDoc ∷ Text
malformedDoc = T.unlines
    [ "world_gen:"
    , "  world_size: 256"
    , "  plate_count: \"ten\""
    ]

-- | A document that decodes, whose values are all distinguishable from
--   the compiled-in defaults: two that normalization must clamp and one
--   it must carry through untouched.
validDoc ∷ Text
validDoc = T.unlines
    [ "world_gen:"
    , "  world_size: 1"
    , "  plate_count: 0"
    , "  erosion_intensity: 0.25"
    ]

spec ∷ Spec
spec = do
    describe "a file that does not exist" $
        it "returns the complete defaults and logs nothing" $ inTemp $ \dir → do
            (cfg, entries) ← loadFrom (dir </> "absent.yaml")
            cfg `shouldBe` defaultWorldGenConfig
            shapeOf entries `shouldBe` []

    describe "a file that exists but does not decode" $ do
        it "warns once at LevelWarn in CatInit, naming the file and the \
           \decoder's own error" $ inTemp $ \dir → do
            let path = dir </> "world_gen.yaml"
            writeFile path (T.unpack malformedDoc)
            expected ← decoderErrorFor path
            -- Precondition: a canned message could satisfy a containment
            -- check against an empty needle.
            T.null expected `shouldBe` False
            (_, entries) ← loadFrom path
            shapeOf entries `shouldBe` [(LevelWarn, CatInit)]
            case entries of
                [entry] → do
                    leMessage entry `shouldContainText` T.pack path
                    leMessage entry `shouldContainText` expected
                _ → expectationFailure "expected exactly one log entry"

        it "falls back whole-document, discarding the settings that did \
           \decode" $ inTemp $ \dir → do
            let path = dir </> "world_gen.yaml"
            writeFile path (T.unpack malformedDoc)
            -- Precondition: 256 is not what the defaults already say, so
            -- equality below is a discard rather than a coincidence.
            wgcWorldSize defaultWorldGenConfig `shouldNotBe` 256
            (cfg, _) ← loadFrom path
            cfg `shouldBe` defaultWorldGenConfig

    describe "a file that decodes" $
        it "normalizes it and logs nothing" $ inTemp $ \dir → do
            let path = dir </> "world_gen.yaml"
            writeFile path (T.unpack validDoc)
            -- Preconditions: every value below differs from the default
            -- it replaces, so none of the assertions can pass on a
            -- silent fallback to 'defaultWorldGenConfig'.
            wgcWorldSize defaultWorldGenConfig `shouldNotBe` minimumWorldSize
            wgcPlateCount defaultWorldGenConfig `shouldNotBe` 1
            wgcErosionIntensity defaultWorldGenConfig `shouldNotBe` 0.25
            (cfg, entries) ← loadFrom path
            -- Clamped: below one full region, and below one plate.
            wgcWorldSize cfg `shouldBe` minimumWorldSize
            wgcPlateCount cfg `shouldBe` 1
            -- Carried through: normalization touches those two fields
            -- and nothing else.
            wgcErosionIntensity cfg `shouldBe` 0.25
            shapeOf entries `shouldBe` []
