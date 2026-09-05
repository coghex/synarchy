-- | The authored harvest-tag policy in a flora @harvestable:@ block
--   (#2212): @ungated_tags:@ and @phase_yield:@.
--
--   Before #2212 the growth-window bypass was not authored at all —
--   'Engine.Scripting.Lua.API.Forage.Harvest' skipped
--   'World.Flora.Growth.harvestOpen' for EVERY tagged call, which is a
--   wood-removal policy written as a property of "being tagged", and a
--   future @fruit@ or @grain@ tag would have inherited it silently.
--   Yield had no phase input at all, so a day-zero sprout dropped a
--   mature tree's logs, hid for its regrowth and came back a sprout: an
--   unbounded wood source.
--
--   Both halves are now AUTHORED, which puts their correctness at the
--   decoding boundary. These examples gate that boundary:
--
--     * __absent is the default__ — a block that says nothing decodes
--       to no exemption and no override, which is what makes a tagged
--       call growth-gated;
--     * __empty is not absent__ — @sprout: []@ decodes to a PRESENT key
--       holding an empty list, because that distinction is the only way
--       to author "this phase yields nothing";
--     * __rejection__ — an @ungated_tags@ entry the block's own @tags:@
--       does not declare, a @phase_yield@ key outside the
--       'World.Flora.Types.LifePhaseTag' vocabulary, a well-spelled key
--       naming a phase this species never enters, and a
--       @phase_yield:@ that is not a block. Each is asserted through
--       the real 'loadFloraYaml' on a real file, because whole-FILE
--       rejection is the established 'Engine.Asset.YamlList' contract
--       and half of what is under test;
--     * __the diagnostic__ — the warning names the FILE, the SPECIES
--       and the KEY, exactly as 'requireRegrowthTime' rejections do;
--     * __the shipped corpus__ — the three wood species, and only
--       those, carry the policy.
--
--   The RUNTIME half — that an exemption actually opens the window and
--   an empty override actually pays nothing — is
--   @--match "harvestOpen"@ (pure) and @--match "Chop tag policy"@
--   (a real engine).
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Asset.FloraHarvestPolicySchema"'@.
module Test.Headless.Asset.FloraHarvestPolicySchema (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import System.Directory
    (getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Engine.Asset.YamlFlora
    (FloraYamlDef(..), FloraYamlHarvest(..), FloraYamlYield(..), loadFloraYaml)
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState )
import World.Flora.Types (LifePhaseTag(..))

-- * Fixtures

-- | A minimally valid tree named @n@ declaring the sprout / matured /
--   dead phases the shipped wood species declare, whose @harvestable:@
--   SECTION is @section@ verbatim.
speciesWith ∷ String → String → String
speciesWith n section = unlines
    [ "flora:"
    , "  - name: \"" ⧺ n ⧺ "\""
    , "    type: \"deciduous_tree\""
    , "    texDir: \"assets/textures/flora/probe\""
    , "    lifecycle: perennial"
    , "    phases:"
    , "      - {tag: sprout, texture: \"sprout.png\", age: 0}"
    , "      - {tag: matured, texture: \"matured.png\", age: 720}"
    , "      - {tag: dead, texture: \"dead.png\", age: 36000}"
    ] ⧺ section ⧺ unlines
    [ "    worldGen:"
    , "      category: tree"
    , "      minTemp: -10"
    , "      maxTemp: 40"
    , "      idealTemp: 15"
    , "      minPrecip: 0.1"
    , "      maxPrecip: 3.0"
    , "      idealPrecip: 1.0"
    ]

probeSpecies ∷ String → String
probeSpecies = speciesWith "probe_oak"

-- | A @harvestable:@ block carrying @extra@ verbatim after the two
--   keys every block needs.
harvestableWith ∷ String → String
harvestableWith extra = unlines
    [ "    harvestable:"
    , "      tags: [wood]"
    , "      yield:"
    , "        - id: wood_log"
    , "          count: [3, 6]"
    , "      regrowth_time: 345600"
    ] ⧺ extra

-- | A second, entirely VALID species — the witness for whole-FILE
--   rejection. A per-entry skip would leave this one registered.
--
--   An ENTRY, not a second document: a second @flora:@ key would make
--   the two species collide on one mapping key rather than share one
--   list, and the first (offending) one would simply be gone.
validSibling ∷ String
validSibling = dropWhile (≢ ' ') (speciesWith "probe_sound"
                                      (harvestableWith ""))

-- * Assertions

-- | Load @src@ through the REAL loader and require whole-file
--   rejection: an empty list plus exactly one 'CatAsset' 'LevelWarn'
--   naming the file, the species and every token in @tokens@.
--
--   Tokens are matched as whole WORDS of a punctuation-scrubbed
--   message rather than as substrings, so @sprout@ cannot be satisfied
--   by a message that only ever says @sprouty@.
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
                    wanted  = path : "probe_oak" : tokens
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
    scrub c = if c `elem` ("'\"(),:;=[]\8212" ∷ String) then ' ' else c

-- | Load @src@ and hand the ONE decoded harvest block to @k@.
withHarvest ∷ String → (FloraYamlHarvest → Expectation) → Expectation
withHarvest src k =
    withTempYaml "probe_flora.yaml" src $ \path → do
        (logger, entriesRef) ← callbackLogger
        defs ← loadFloraYaml logger path
        entries ← readIORef entriesRef
        -- Accepting silently is half the contract: a decoder that
        -- WARNED and still handed the block back would pass every
        -- assertion below.
        map (T.unpack . leMessage) entries `shouldBe` []
        case [h | d ← defs, Just h ← [fydHarvest d]] of
            [h]   → k h
            other → expectationFailure $
                "expected exactly one harvestable block, got "
                ⧺ show (length other)

-- | The decoded @phase_yield@ as (phase, item ids) pairs.
phaseIds ∷ FloraYamlHarvest → [(LifePhaseTag, [Text])]
phaseIds h = sort [ (tag, map fyyId ys)
                  | (tag, ys) ← HM.toList (fyhPhaseYield h) ]

spec ∷ Spec
spec = do
    describe "absent is the growth-gated default (requirement 1)" $ do
        it "a block authoring neither key decodes to no exemption and no \
           \override, so its tagged harvest is window-gated" $
            withHarvest (probeSpecies (harvestableWith "")) $ \h → do
                fyhUngatedTags h `shouldBe` []
                phaseIds h `shouldBe` []

        it "an explicitly null ungated_tags reads as the empty list — for \
           \THIS key absent and empty say the same thing, so there is \
           \nothing for #1191's present-but-malformed rule to protect" $
            withHarvest
                (probeSpecies (harvestableWith "      ungated_tags: null\n"))
                $ \h → fyhUngatedTags h `shouldBe` []

    describe "empty is not absent (requirement 4)" $ do
        it "phase_yield sprout: [] decodes to a PRESENT key holding an \
           \empty list — the only way to author a phase that pays nothing" $
            withHarvest (probeSpecies (harvestableWith (unlines
                [ "      phase_yield:"
                , "        sprout: []" ]))) $ \h → do
                    phaseIds h `shouldBe` [(PhaseSprout, [])]
                    -- The BLOCK default is untouched, which is what the
                    -- unauthored phases go on inheriting.
                    map fyyId (fyhYield h) `shouldBe` ["wood_log"]

        it "an override with entries decodes them, and count ranges carry" $
            withHarvest (probeSpecies (harvestableWith (unlines
                [ "      phase_yield:"
                , "        sprout: []"
                , "        dead:"
                , "          - id: dry_kindling"
                , "            count: [1, 2]" ]))) $ \h → do
                    phaseIds h `shouldBe`
                        [ (PhaseSprout, []), (PhaseDead, ["dry_kindling"]) ]
                    [ (fyyMin y, fyyMax y)
                        | ys ← maybeToList (HM.lookup PhaseDead
                                               (fyhPhaseYield h))
                        , y ← ys ] `shouldBe` [(1, 2)]

        it "an empty phase_yield block is legal and authors no override" $
            withHarvest (probeSpecies (harvestableWith
                "      phase_yield: {}\n")) $ \h → phaseIds h `shouldBe` []

    describe "ungated_tags must name a tag this block declares" $ do
        it "accepts an exemption for a declared tag" $
            withHarvest
                (probeSpecies (harvestableWith "      ungated_tags: [wood]\n"))
                $ \h → fyhUngatedTags h `shouldBe` ["wood"]

        it "rejects a MISSPELLED tag, which would otherwise look authored \
           \while the chop stayed growth-gated" $
            rejectsNaming ["ungated_tags", "wodo", "wood"]
                (probeSpecies (harvestableWith
                    "      ungated_tags: [wodo]\n"))

        it "rejects an exemption on a block that declares no tags at all, \
           \naming the empty declared set rather than an empty list" $
            rejectsNaming ["ungated_tags", "none"] $ probeSpecies $ unlines
                [ "    harvestable:"
                , "      ungated_tags: [wood]"
                , "      regrowth_time: 345600" ]

        it "drops EVERY species in the file, not just the offending one" $
            rejectsNaming ["ungated_tags", "wodo"]
                (probeSpecies (harvestableWith
                    "      ungated_tags: [wodo]\n") ⧺ validSibling)

    describe "phase_yield keys are a closed, DECLARED vocabulary" $ do
        it "rejects a key outside the life-phase vocabulary, naming the \
           \vocabulary so the fix is in the message" $
            rejectsNaming ["sprouting", "sprout", "matured", "dead"]
                (probeSpecies (harvestableWith (unlines
                    [ "      phase_yield:"
                    , "        sprouting: []" ])))

        it "rejects a WELL-SPELLED phase this species never declares — \
           \the same dead end a cycleOverrides selector on an undeclared \
           \phase is" $
            -- The path is one dotted token in the message, and the
            -- scrub deliberately leaves '.' alone (values carry it).
            rejectsNaming ["harvestable.phase_yield", "flowering", "phases"]
                (probeSpecies (harvestableWith (unlines
                    [ "      phase_yield:"
                    , "        flowering: []" ])))

        it "rejects a phase_yield that is not a block at all" $
            rejectsNaming ["phase_yield", "block"]
                (probeSpecies (harvestableWith "      phase_yield: 3\n"))

        it "rejects an authored null phase_yield — unlike ungated_tags, \
           \absent and empty-block are different statements here, so a \
           \null has no defensible reading" $
            rejectsNaming ["phase_yield", "block", "null"]
                (probeSpecies (harvestableWith "      phase_yield: null\n"))

        it "rejects an entry whose VALUE is not a list of yields" $
            rejectsNaming ["phase_yield", "sprout", "list"]
                (probeSpecies (harvestableWith (unlines
                    [ "      phase_yield:"
                    , "        sprout: 7" ])))

    describe "the shipped corpus (requirement 2)" $ do
        it "exactly white_oak, paper_birch and sugar_maple author the \
           \wood exemption, so every tree the chop drag-box selects \
           \today stays designatable as a sprout or standing dead" $ do
            (logger, _) ← callbackLogger
            declared ← forM shippedFloraFiles $ \file → do
                defs ← loadFloraYaml logger ("data/flora" </> file)
                pure [ fydName d
                     | d ← defs, Just h ← [fydHarvest d]
                     , not (null (fyhUngatedTags h)) ]
            sort (concat declared)
                `shouldBe` sort ["paper_birch", "sugar_maple", "white_oak"]

        it "each of them exempts exactly its own wood tag" $ do
            (logger, _) ← callbackLogger
            defs ← loadFloraYaml logger "data/flora/temperate_deciduous.yaml"
            [ (fydName d, fyhTags h, fyhUngatedTags h)
                | d ← defs, Just h ← [fydHarvest d] ]
              `shouldBe`
                [ ("white_oak",   ["wood"], ["wood"])
                , ("paper_birch", ["wood"], ["wood"])
                , ("sugar_maple", ["wood"], ["wood"]) ]

        it "each of them authors an EMPTY sprout yield and no other \
           \override, so matured and dead inherit the species roll" $ do
            (logger, _) ← callbackLogger
            defs ← loadFloraYaml logger "data/flora/temperate_deciduous.yaml"
            [ (fydName d, phaseIds h)
                | d ← defs, Just h ← [fydHarvest d] ]
              `shouldBe`
                [ ("white_oak",   [(PhaseSprout, [])])
                , ("paper_birch", [(PhaseSprout, [])])
                , ("sugar_maple", [(PhaseSprout, [])]) ]

        it "no OTHER shipped species gained an exemption or an override — \
           \the forage tags stay growth-gated, which is the regression \
           \#2212 exists to prevent" $ do
            (logger, _) ← callbackLogger
            others ← forM shippedFloraFiles $ \file → do
                defs ← loadFloraYaml logger ("data/flora" </> file)
                pure [ (fydName d, fyhTags h)
                     | d ← defs, Just h ← [fydHarvest d]
                     , null (fyhUngatedTags h) ]
            sort (concat others) `shouldBe` sort
                [ ("tomato_plant",     ["fruit"])
                , ("wheat",            ["grain"])
                , ("red_raspberry",    ["fruit"])
                , ("white_clover",     ["leaves"]) ]
            -- ... and none of them authors a phase override either.
            overrides ← forM shippedFloraFiles $ \file → do
                defs ← loadFloraYaml logger ("data/flora" </> file)
                pure [ fydName d
                     | d ← defs, Just h ← [fydHarvest d]
                     , null (fyhUngatedTags h)
                     , not (HM.null (fyhPhaseYield h)) ]
            concat overrides `shouldBe` []

-- | Every @data/flora/*.yaml@, so a new file cannot silently escape the
--   corpus assertions above.
shippedFloraFiles ∷ [FilePath]
shippedFloraFiles =
    [ "boreal_evergreen.yaml", "crops.yaml", "saguaro.yaml"
    , "temperate_deciduous.yaml", "temperate_shrubs.yaml"
    , "temperate_wildflowers.yaml", "tropical.yaml", "wetlands.yaml" ]

-- | A logger whose backend appends every emitted 'LogEntry' to an
--   'IORef'. 'CatAsset' debug logging stays OFF (the default) so a
--   rejection's warning is the only entry captured.
callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    pure (logger, entriesRef)

withTempYaml ∷ FilePath → String → (FilePath → IO a) → IO a
withTempYaml name contents action = do
    tmp ← getTemporaryDirectory
    let dir  = tmp </> "synarchy-flora-harvest-policy-spec"
        path = dir </> name
    createDirectoryIfMissing True dir
    writeFile path contents
    action path `finally` removeDirectoryRecursive dir
