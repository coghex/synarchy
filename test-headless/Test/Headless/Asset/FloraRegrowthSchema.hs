-- | The finite, strictly-positive domain of a flora @harvestable:@
--   block's @regrowth_time@ (#1711).
--
--   @regrowth_time@ is the only thing standing between a harvested wild
--   plant and being harvestable again, and nothing used to check its
--   domain. 'Engine.Scripting.Lua.API.Forage.Harvest' gates a harvest
--   on the live timer being @≤ 0@ and then reinserts the authored value
--   unchanged, so a zero or negative one is already expired the instant
--   it is written: the next @world.harvestFlora@ on that tile spawns
--   the full yield again, with no tick in between. The regrowth tick
--   does not close it either — 'World.Flora.Harvest.tickFloraHarvests'
--   DROPS an entry that is already @≤ 0@, and no entry is the
--   harvestable state.
--
--   The fix is at the AUTHORING boundary and nowhere else (requirement
--   7): this spec therefore gates the DECODER, and there is deliberately
--   no clamp in any consumer to test. Three halves:
--
--     * __rejection__ — zero, negative, and the @1.0e+100@ that is a
--       perfectly ordinary 'Scientific' but becomes @Infinity@ in the
--       engine's 32-bit 'Float'. Each is asserted through the real
--       'loadFloraYaml' on a real file, because whole-FILE rejection
--       (the established 'Engine.Asset.YamlList' contract) is half of
--       what is under test: the loader must hand back @[]@ AND warn.
--     * __the diagnostic__ — the warning names the FILE (from
--       'loadYamlList'), the SPECIES, the KEY, and the offending value
--       or the finite/positive reason. A message missing any of those
--       leaves an author unable to find what to fix.
--     * __the shipped corpus__ — every @data/flora/*.yaml@ still loads,
--       with its species count and its authored durations unchanged.
--
--   The other side of requirement 6 — that a POSITIVE duration still
--   blocks an immediate second harvest and reopens only once the timer
--   is ticked away — is a production-path assertion over live world
--   state, and belongs to @tools/flora_growth_probe.py@.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Asset.FloraRegrowthSchema"'@.
module Test.Headless.Asset.FloraRegrowthSchema (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (sort)
import qualified Data.Text as T
import System.FilePath ((</>))
import Engine.Asset.YamlFlora
    (FloraYamlDef(..), FloraYamlHarvest(..), loadFloraYaml)
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState )
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

-- * Fixtures
--
--   Raw source text rather than constructed values, because half of
--   what is under test is how the YAML scalar resolver and the 'Float'
--   narrowing interact: @1.0e+100@ resolving to an ordinary
--   'Scientific' that overflows to @Infinity@, and @.inf@ resolving to
--   a STRING, are both facts about the source text and invisible to a
--   fixture built from Haskell values.

-- | A minimally valid species named @n@ whose @harvestable:@ SECTION
--   is @section@ verbatim (empty for a decorative plant that authors
--   none). Everything else is the smallest shape 'FloraYamlDef'
--   accepts, so nothing but that section can fail.
speciesWith ∷ String → String → String
speciesWith n section = unlines
    [ "flora:"
    , "  - name: \"" ⧺ n ⧺ "\""
    , "    type: \"shrub\""
    , "    texDir: \"assets/textures/flora/probe\""
    , "    lifecycle: perennial"
    , "    phases:"
    , "      - {tag: matured, texture: \"matured.png\", age: 0}"
    ] ⧺ section ⧺ unlines
    [ "    worldGen:"
    , "      category: shrub"
    , "      minTemp: -10"
    , "      maxTemp: 40"
    , "      idealTemp: 15"
    , "      minPrecip: 0.1"
    , "      maxPrecip: 3.0"
    , "      idealPrecip: 1.0"
    ]

-- | The same, under the name every rejection assertion looks for.
probeSpecies ∷ String → String
probeSpecies = speciesWith "probe_bramble"

-- | A well-formed @harvestable:@ block whose @regrowth_time@ is @v@
--   authored exactly as a content author writes it — including the
--   spellings ('null', a quoted string) that are not numbers at all.
harvestableWith ∷ String → String
harvestableWith v = unlines
    [ "    harvestable:"
    , "      tags: [fruit]"
    , "      yield:"
    , "        - id: wild_berries"
    , "          count: [1, 3]"
    , "      regrowth_time: " ⧺ v
    ]

-- | The same block with the @regrowth_time@ key omitted entirely.
harvestableWithoutTimer ∷ String
harvestableWithoutTimer = unlines
    [ "    harvestable:"
    , "      tags: [fruit]"
    , "      yield:"
    , "        - id: wild_berries"
    , "          count: [1, 3]"
    ]

-- | A second, entirely VALID species appended after the first — the
--   witness for whole-FILE rejection. A per-entry skip would leave this
--   one registered.
validSibling ∷ String
validSibling = unlines
    [ "  - name: \"probe_sound\""
    , "    type: \"shrub\""
    , "    texDir: \"assets/textures/flora/probe\""
    , "    lifecycle: perennial"
    , "    phases:"
    , "      - {tag: matured, texture: \"matured.png\", age: 0}"
    , "    harvestable:"
    , "      tags: [fruit]"
    , "      regrowth_time: 86400"
    , "    worldGen:"
    , "      category: shrub"
    , "      minTemp: -10"
    , "      maxTemp: 40"
    , "      idealTemp: 15"
    , "      minPrecip: 0.1"
    , "      maxPrecip: 3.0"
    , "      idealPrecip: 1.0"
    ]

-- * Assertions

-- | Load @src@ through the REAL loader and require the established
--   whole-file rejection: an empty list plus exactly one 'CatAsset'
--   'LevelWarn' whose message names the file, the species, the key, and
--   every token in @tokens@.
--
--   Tokens are matched as whole WORDS of a punctuation-scrubbed
--   message, not substrings, so @finite@ cannot be satisfied by a
--   message that only ever says @infinite@. The scrub deliberately
--   leaves @.@ and @-@ alone: they are inside the values (@-1.0@,
--   @1.0e100@) the tokens have to match.
rejectsNaming ∷ [String] → String → Expectation
rejectsNaming tokens src =
    withTempYaml "probe_flora.yaml" src $ \path → do
        (logger, entriesRef) ← callbackLogger
        defs ← loadFloraYaml logger path
        map fydName defs `shouldBe` []
        entries ← readIORef entriesRef
        case entries of
            [entry] → do
                leLevel entry `shouldBe` LevelWarn
                leCategory entry `shouldBe` CatAsset
                let msg     = T.unpack (leMessage entry)
                    ws      = words (map scrub msg)
                    wanted  = path : "probe_bramble" : "regrowth_time" : tokens
                    missing = [t | t ← wanted, t `notElem` ws]
                if null missing
                  then pure ()
                  else expectationFailure $
                      "rejected, but the warning does not name "
                      ⧺ show missing ⧺ ": " ⧺ msg
            other → expectationFailure $
                "expected exactly one captured log entry, got "
                ⧺ show (length other)
  where
    scrub c = if c `elem` ("'\"(),:;=\8212" ∷ String) then ' ' else c

-- | Load @src@ and require it to parse into exactly the named species
--   with exactly those regrowth durations.
acceptsAs ∷ [(Text, Float)] → String → Expectation
acceptsAs expected src =
    withTempYaml "probe_flora.yaml" src $ \path → do
        (logger, _) ← callbackLogger
        defs ← loadFloraYaml logger path
        [ (fydName d, fyhRegrowthTime h)
            | d ← defs, Just h ← [fydHarvest d] ] `shouldBe` expected

spec ∷ Spec
spec = do
    describe "rejected regrowth_time (requirements 1 and 2)" $ do
        it "rejects zero — the value that makes the very next harvest \
           \on the same tile spawn the full yield again" $
            rejectsNaming ["positive", "0.0"]
                (probeSpecies (harvestableWith "0"))

        it "rejects an explicitly floating zero the same way" $
            rejectsNaming ["positive", "0.0"]
                (probeSpecies (harvestableWith "0.0"))

        it "rejects a negative duration, reporting the authored value" $
            rejectsNaming ["positive", "-1.0"]
                (probeSpecies (harvestableWith "-1"))

        it "rejects a large negative duration" $
            rejectsNaming ["positive", "-86400.0"]
                (probeSpecies (harvestableWith "-86400"))

        it "rejects a finite YAML literal that OVERFLOWS the engine's \
           \32-bit Float to infinity — an infinite timer never expires, \
           \so it would reach gameplay as a silently one-shot plant" $
            rejectsNaming ["finite"]
                (probeSpecies (harvestableWith "1.0e+100"))

        it "rejects .inf, which YAML's scalar resolver hands over as a \
           \STRING rather than a number" $
            rejectsNaming ["number"]
                (probeSpecies (harvestableWith ".inf"))

        it "rejects a positive duration that UNDERFLOWS to zero in that \
           \same Float, reporting the effective 0.0 rather than the \
           \authored literal" $
            -- The mirror image of the overflow above: positivity is
            -- evaluated AFTER narrowing, so a Scientific the YAML
            -- parser is perfectly happy with cannot author a timer the
            -- runtime would never see as positive.
            rejectsNaming ["positive", "0.0"]
                (probeSpecies (harvestableWith "1.0e-60"))

        it "rejects an absent regrowth_time — it is required and has no \
           \default, exactly as before" $
            rejectsNaming ["required"] (probeSpecies harvestableWithoutTimer)

        it "rejects an authored null, which aeson reads as absent" $
            rejectsNaming ["required"] (probeSpecies (harvestableWith "null"))

        it "rejects a non-numeric regrowth_time by species rather than \
           \by list index" $
            rejectsNaming ["number"] (probeSpecies (harvestableWith "\"soon\""))

        it "rejects a harvestable: that is not a block at all, naming \
           \the species rather than aeson's bare expected-Object error" $
            rejectsNaming ["block", "23.0"]
                (probeSpecies "    harvestable: 23\n")

    describe "whole-file rejection (requirement 3)" $
        it "drops EVERY species in the file, not just the offending \
           \entry — the established Engine.Asset.YamlList contract" $
            rejectsNaming ["positive", "0.0"]
                (probeSpecies (harvestableWith "0") ⧺ validSibling)

    describe "accepted regrowth_time (requirement 5)" $ do
        it "accepts an ordinary game-day duration" $
            acceptsAs [("probe_bramble", 86400)]
                (probeSpecies (harvestableWith "86400"))

        it "accepts an arbitrarily small positive duration — the \
           \boundary is zero, and it is exclusive" $
            acceptsAs [("probe_bramble", 0.5)]
                (probeSpecies (harvestableWith "0.5"))

        it "accepts every species in a valid multi-species file" $
            acceptsAs [("probe_bramble", 43200), ("probe_sound", 86400)]
                (probeSpecies (harvestableWith "43200") ⧺ validSibling)

        it "leaves a species with NO harvestable: block alone — a \
           \decorative plant authors no timer and must not be forced to" $
            withTempYaml "probe_flora.yaml" (probeSpecies "") $ \path → do
                (logger, _) ← callbackLogger
                defs ← loadFloraYaml logger path
                map fydName defs `shouldBe` ["probe_bramble"]
                map fydHarvest defs `shouldBe` [Nothing]

        it "reads an explicitly null harvestable: as absent, exactly as \
           \aeson's .:? did before — the domain rule is about the \
           \block's CONTENT, not its presence" $
            withTempYaml "probe_flora.yaml"
                (probeSpecies "    harvestable: null\n") $ \path → do
                (logger, _) ← callbackLogger
                defs ← loadFloraYaml logger path
                map fydName defs `shouldBe` ["probe_bramble"]
                map fydHarvest defs `shouldBe` [Nothing]

    describe "the shipped corpus (requirement 4)" $ do
        it "every data/flora/*.yaml still loads, with its species \
           \unchanged" $ do
            (logger, _) ← callbackLogger
            loaded ← forM shippedFlora $ \(file, names) → do
                defs ← loadFloraYaml logger ("data/flora" </> file)
                pure (file, map fydName defs, names)
            forM_ loaded $ \(file, got, want) →
                (file, got) `shouldBe` (file, want)

        it "the total shipped species count is unchanged" $ do
            (logger, _) ← callbackLogger
            counts ← forM shippedFlora $ \(file, _) →
                length <$> loadFloraYaml logger ("data/flora" </> file)
            sum counts `shouldBe` sum (map (length ∘ snd) shippedFlora)

        it "every authored regrowth_time is finite and strictly \
           \positive — the property the decoder now enforces, checked \
           \against the shipped values themselves" $ do
            (logger, _) ← callbackLogger
            durations ← forM shippedFlora $ \(file, _) → do
                defs ← loadFloraYaml logger ("data/flora" </> file)
                pure [ fyhRegrowthTime h | d ← defs, Just h ← [fydHarvest d] ]
            let ds = concat durations
            sort ds `shouldBe` sort shippedRegrowthTimes
            ds `shouldSatisfy` all (\t → t > 0 ∧ not (isInfinite t)
                                            ∧ not (isNaN t))

-- | The shipped baseline this change must not move: every
--   @data/flora/*.yaml@ and the species it declares, in file order.
--   Pinned by NAME rather than by count so a swap is caught too.
shippedFlora ∷ [(FilePath, [Text])]
shippedFlora =
    [ ("boreal_evergreen.yaml",      ["scots_pine", "white_spruce"])
    , ("crops.yaml",                 ["tomato_plant", "wheat"])
    , ("saguaro.yaml",               ["saguaro"])
    , ("temperate_deciduous.yaml",   [ "white_oak", "paper_birch"
                                     , "weeping_willow", "sugar_maple" ])
    , ("temperate_shrubs.yaml",      ["bracken_fern", "red_raspberry"])
    , ("temperate_wildflowers.yaml", ["common_dandelion", "white_clover"])
    , ("tropical.yaml",              ["coconut_palm", "red_mangrove"])
    ]

-- | Every authored duration in that corpus. Two of the seven files
--   (@boreal_evergreen@, @tropical@) author none at all, which is why
--   this is a flat list rather than a per-file one.
shippedRegrowthTimes ∷ [Float]
shippedRegrowthTimes =
    [ 43200, 43200          -- crops.yaml
    , 345600, 259200, 345600 -- temperate_deciduous.yaml
    , 86400                 -- temperate_shrubs.yaml
    , 43200                 -- temperate_wildflowers.yaml
    ]

-- | A logger whose backend appends every emitted 'LogEntry' to an
--   'IORef'. 'CatAsset' debug logging stays OFF (the default) so a
--   rejection's warning is the only entry captured, which is what lets
--   the assertions above require exactly one.
callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    pure (logger, entriesRef)

withTempYaml ∷ FilePath → String → (FilePath → IO a) → IO a
withTempYaml name contents action =
    withExclusiveTempDirectory "synarchy-flora-regrowth-spec" $ \dir → do
        let path = dir </> name
        writeFile path contents
        action path
