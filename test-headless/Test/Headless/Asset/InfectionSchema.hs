-- | The authored domain of an infection definition — its bands, its
--   weights and multipliers, and its closed vocabulary tokens (#2346).
--
--   @data/infections/*.yaml@ used to be accepted verbatim: any 'Float'
--   passed for @base_weight@, @aggressiveness@, @infectability@,
--   @cure_rate@ and @transmissibility@, and a @pair@ helper RESHAPED a
--   climate band instead of checking it — @[a, b, c]@ truncated, @[a]@
--   borrowed the default upper bound, @[]@ became the full range. None
--   of the consumers can defend itself: 'selectInfectionType' draws
--   @randomR (0, total)@ over @base_weight × climateMatchWeight@ (a NaN
--   weight makes @total ≤ 0@ false and the pick degenerate), @bandMatch@
--   treats a band as ordered, @cure_rate@ multiplies the antibiotic
--   reduction AFTER the capability clamp, and @site@ / @curable_by@ are
--   matched by exact token so one misspelling silently drops the
--   definition from every pool or makes it incurable.
--
--   The fix is at the AUTHORING boundary and nowhere else (requirement
--   5): this spec therefore gates the DECODER, and there is deliberately
--   no consumer clamp to test. Four parts:
--
--     * __rejection__ — every requirement-1 branch, asserted through
--       the real 'loadInfectionYamlOutcome' on a real file, because
--       whole-FILE rejection (the established
--       'Engine.Asset.YamlList' contract) is half of what is under
--       test: the loader must hand back 'Nothing' AND warn.
--     * __the diagnostic__ — the warning names the FILE (from
--       'Engine.Asset.YamlList.loadYamlListOutcome'), the definition
--       @id@, the FIELD, and the offending VALUE (requirement 4). A
--       message missing any of those leaves an author unable to find
--       what to fix.
--     * __present versus absent__ (requirement 2) — an OMITTED optional
--       key keeps its documented default; an explicitly authored @null@
--       is present-but-malformed and is rejected, which is what aeson's
--       @.:?@ could not express.
--     * __the shipped corpus__ (requirement 6) — @data/infections/
--       bacteria.yaml@ still loads, with its definition count and every
--       band, weight and multiplier unchanged.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Asset.InfectionSchema"'@.
module Test.Headless.Asset.InfectionSchema (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.Text as T
import System.FilePath ((</>))
import Engine.Asset.YamlInfection
    (InfectionYamlDef(..), loadInfectionYamlOutcome)
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogLevel(..), LogEntry(..), LoggerState )
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)

-- * Fixtures
--
--   Raw source text rather than constructed values, because half of
--   what is under test is how YAML's scalar resolver and the 'Float'
--   narrowing interact: @1.0e+100@ resolving to an ordinary
--   'Scientific' that overflows to @Infinity@, and @.nan@ / @.inf@
--   resolving to a STRING, are facts about the source text and are
--   invisible to a fixture built from Haskell values.

-- | A file holding one definition named @probe_bug@ whose body lines
--   are exactly @body@. Only @id@ and @name@ are supplied, so a fixture
--   that omits @category@ is testing exactly that omission.
probeWith ∷ [String] → String
probeWith body = unlines $
    [ "infections:"
    , "  - id: probe_bug"
    , "    name: \"Probe infection\""
    ] ⧺ body

-- | The smallest body a valid definition needs, with @extra@ appended.
validBody ∷ [String] → [String]
validBody extra = "    category: bacterial" : extra

-- | A second, entirely VALID definition appended after the first — the
--   witness for whole-FILE rejection. A per-definition skip would leave
--   this one registered.
validSibling ∷ [String]
validSibling =
    [ "  - id: probe_sound"
    , "    name: \"Sound infection\""
    , "    category: bacterial"
    , "    site: [surface]"
    , "    base_weight: 2.0"
    , "    climate: { temp: [0, 30], moisture: [0.1, 0.9] }"
    ]

-- * Assertions

-- | Load @src@ through the REAL loader and require whole-file
--   rejection: the outcome is 'Nothing' — which is what makes
--   @engine.loadInfectionYaml@ register nothing and the startup loader
--   treat the file as a parse failure (#2203) — plus exactly one
--   'CatAsset' 'LevelWarn' naming the file, the definition, and every
--   token in @tokens@.
--
--   Tokens are matched as whole WORDS of a punctuation-scrubbed
--   message, not substrings, so @finite@ cannot be satisfied by a
--   message that only ever says @infinite@. The scrub takes the
--   brackets too, because a reported band is written @[50.0, -20.0]@
--   and the bound has to match as its own word, and the BACKSLASH,
--   because 'Data.Yaml.ParseException' renders the aeson failure
--   through 'show' — so a value this decoder quotes arrives as
--   @\\\"surfance\\\"@. It deliberately leaves @.@ and @-@ alone, since
--   those are inside the values (@-1.0@, @.nan@, @1.0e100@) the tokens
--   have to match.
rejectsNaming ∷ [String] → String → Expectation
rejectsNaming tokens src =
    withTempYaml "probe_infections.yaml" src $ \path → do
        (logger, entriesRef) ← callbackLogger
        outcome ← loadInfectionYamlOutcome logger path
        outcome `shouldBe` Nothing
        entries ← readIORef entriesRef
        case entries of
            [entry] → do
                leLevel entry `shouldBe` LevelWarn
                leCategory entry `shouldBe` CatAsset
                let msg     = T.unpack (leMessage entry)
                    ws      = words (map scrub msg)
                    wanted  = path : "probe_bug" : tokens
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
    scrub c = if c `elem` ("'\"(),:;=[]\\" ∷ String) then ' ' else c

-- | Load @src@ and require it to decode into exactly @expected@ — every
--   field, so an accepted fixture pins its defaults as well as the
--   values it authored.
acceptsAs ∷ [InfectionYamlDef] → String → Expectation
acceptsAs expected src =
    withTempYaml "probe_infections.yaml" src $ \path → do
        (logger, _) ← callbackLogger
        outcome ← loadInfectionYamlOutcome logger path
        outcome `shouldBe` Just expected

-- | @probe_bug@ with every optional field at its documented default —
--   the baseline the minimally authored fixture must decode to.
probeDefaults ∷ InfectionYamlDef
probeDefaults = InfectionYamlDef
    { iyId = "probe_bug", iyName = "Probe infection"
    , iyIcon = "bacterial_infection", iyCategory = "bacterial"
    , iySites = [], iyBaseWeight = 1.0
    , iyTempMin = -50, iyTempMax = 50
    , iyMoistMin = 0, iyMoistMax = 1
    , iyAggressiveness = 1.0, iyInfectability = 1.0
    , iyCurableBy = [], iyCureRate = 1.0
    , iyWoundInfectable = True, iyEffects = []
    , iyTransmissibility = 0.0, iyTransmission = []
    }

spec ∷ Spec
spec = do
    describe "climate bands (requirement 1)" $ do
        it "rejects a three-element band rather than TRUNCATING it to \
           \the first two, which is what the old pair helper did" $
            rejectsNaming ["temp", "two", "3"] $ probeWith $ validBody
                [ "    climate: { temp: [1, 2, 3] }" ]

        it "rejects a singleton band rather than silently borrowing the \
           \default upper bound" $
            rejectsNaming ["temp", "two", "1"] $ probeWith $ validBody
                [ "    climate: { temp: [5] }" ]

        it "rejects an empty band rather than silently becoming the \
           \full default range" $
            rejectsNaming ["moisture", "two", "0"] $ probeWith $ validBody
                [ "    climate: { moisture: [] }" ]

        it "rejects an inverted band — bandMatch treats it as ordered, \
           \so an inverted one matches nothing and falls off in both \
           \directions at once" $
            rejectsNaming ["ordered", "50.0", "-20.0"] $ probeWith $ validBody
                [ "    climate: { temp: [50, -20] }" ]

        it "rejects a moisture bound above one" $
            rejectsNaming ["moisture", "max", "1.5"] $ probeWith $ validBody
                [ "    climate: { moisture: [0.2, 1.5] }" ]

        it "rejects a moisture bound below zero" $
            rejectsNaming ["moisture", "min", "-0.5"] $ probeWith $ validBody
                [ "    climate: { moisture: [-0.5, 0.8] }" ]

        it "rejects a non-finite band member, which YAML's scalar \
           \resolver hands over as a STRING rather than a number" $
            rejectsNaming ["temp", "min", "finite", ".inf"] $
                probeWith $ validBody
                    [ "    climate: { temp: [.inf, 50] }" ]

        it "rejects a band member that OVERFLOWS the engine's 32-bit \
           \Float to infinity — the check has to run after narrowing" $
            rejectsNaming ["temp", "max", "finite", "1.0e100"] $
                probeWith $ validBody
                    [ "    climate: { temp: [0, 1.0e+100] }" ]

        it "rejects a band that is not a list at all" $
            rejectsNaming ["temp", "two"] $ probeWith $ validBody
                [ "    climate: { temp: 12 }" ]

        it "rejects a climate: that is not a block at all" $
            rejectsNaming ["climate", "block", "12.0"] $
                probeWith $ validBody [ "    climate: 12" ]

    describe "weights and multipliers (requirement 1)" $ do
        it "rejects a negative base_weight — selectInfectionType \
           \subtracts cumulative weights, so a negative one biases the \
           \draw toward its neighbour" $
            rejectsNaming ["base_weight", "-1.0"] $ probeWith $ validBody
                [ "    base_weight: -1" ]

        it "rejects a NaN cure_rate, naming the field and the spelling \
           \the file used — .nan reaches the decoder as a STRING, and \
           \the old structural failure named neither" $
            rejectsNaming ["cure_rate", "finite", ".nan"] $
                probeWith $ validBody [ "    cure_rate: .nan" ]

        it "rejects a negative cure_rate — it multiplies the antibiotic \
           \reduction after the capability clamp, so a negative one \
           \makes a dose WORSEN the infection" $
            rejectsNaming ["cure_rate", "-0.5"] $ probeWith $ validBody
                [ "    cure_rate: -0.5" ]

        it "rejects a base_weight that is a finite YAML literal but \
           \OVERFLOWS to infinity in the engine's 32-bit Float" $
            rejectsNaming ["base_weight", "finite", "1.0e100"] $
                probeWith $ validBody [ "    base_weight: 1.0e+100" ]

        it "rejects a negative aggressiveness" $
            rejectsNaming ["aggressiveness", "-2.0"] $ probeWith $ validBody
                [ "    aggressiveness: -2" ]

        it "rejects a non-finite infectability" $
            rejectsNaming ["infectability", "finite", ".inf"] $
                probeWith $ validBody [ "    infectability: .inf" ]

        it "rejects a negative transmissibility" $
            rejectsNaming ["transmissibility", "-1.0"] $
                probeWith $ validBody [ "    transmissibility: -1" ]

        it "rejects a governed field that is not a number at all" $
            rejectsNaming ["base_weight", "number", "heavy"] $
                probeWith $ validBody [ "    base_weight: \"heavy\"" ]

    describe "vocabulary tokens (requirement 1)" $ do
        it "rejects a misspelled site token, which would otherwise drop \
           \the definition out of every selection pool in silence" $
            rejectsNaming ["site", "surfance"] $ probeWith $ validBody
                [ "    site: [surfance]" ]

        it "rejects a misspelled site token even beside a valid one" $
            rejectsNaming ["site", "dep"] $ probeWith $ validBody
                [ "    site: [surface, dep]" ]

        it "rejects an unknown category" $
            rejectsNaming ["category", "sorcery"] $ probeWith
                [ "    category: sorcery" ]

        it "rejects an unknown curable_by treatment, which would \
           \otherwise make the infection permanently incurable" $
            rejectsNaming ["curable_by", "poultice"] $ probeWith $ validBody
                [ "    curable_by: [poultice]" ]

        it "rejects a site list whose entries are not tokens" $
            rejectsNaming ["site", "tokens"] $ probeWith $ validBody
                [ "    site: [3]" ]

        it "rejects a site that is not a list at all" $
            rejectsNaming ["site", "list", "surface"] $
                probeWith $ validBody [ "    site: surface" ]

        it "rejects a missing category by definition id rather than by \
           \list index" $
            rejectsNaming ["category", "required"] $ probeWith
                [ "    base_weight: 2.0" ]

    describe "present versus absent (requirement 2)" $ do
        it "keeps every documented default for a minimally authored \
           \definition — an omitted optional key is not a rejection" $
            acceptsAs [probeDefaults] (probeWith (validBody []))

        it "rejects an explicitly authored null cure_rate rather than \
           \defaulting it to 1.0, which is what aeson's .:? did" $
            rejectsNaming ["cure_rate", "number", "null"] $
                probeWith $ validBody [ "    cure_rate: null" ]

        it "rejects an explicitly authored null climate rather than \
           \defaulting both bands to the full range" $
            rejectsNaming ["climate", "block", "null"] $
                probeWith $ validBody [ "    climate: null" ]

        it "rejects an explicitly authored null band rather than \
           \defaulting it" $
            rejectsNaming ["moisture", "two", "null"] $
                probeWith $ validBody [ "    climate: { moisture: null }" ]

        it "rejects an explicitly authored null site list rather than \
           \defaulting it to the empty pool" $
            rejectsNaming ["site", "list", "null"] $
                probeWith $ validBody [ "    site: null" ]

        it "rejects an explicitly authored null effects list, even \
           \though the tokens themselves stay free-form" $
            rejectsNaming ["effects", "list", "null"] $
                probeWith $ validBody [ "    effects: null" ]

        it "leaves the free-form effects and transmission vocabularies \
           \open — only their SHAPE is checked (out of scope)" $
            acceptsAs
                [ probeDefaults { iyEffects = ["ichor", "moonrot"]
                                , iyTransmission = ["by_owl"] } ] $
                probeWith $ validBody
                    [ "    effects: [ichor, moonrot]"
                    , "    transmission: [by_owl]" ]

    describe "whole-file rejection (requirement 3)" $
        it "drops EVERY definition in the file, not just the offending \
           \one — the established Engine.Asset.YamlList contract, and \
           \what makes engine.loadInfectionYaml register nothing" $
            rejectsNaming ["cure_rate", "-0.5"] $
                probeWith (validBody [ "    cure_rate: -0.5" ] ⧺ validSibling)

    describe "the shipped corpus (requirement 6)" $ do
        it "data/infections/bacteria.yaml still loads, with every \
           \definition, band, weight and multiplier unchanged" $ do
            (logger, _) ← callbackLogger
            outcome ← loadInfectionYamlOutcome logger shippedPath
            outcome `shouldBe` Just shippedInfections

        it "its definition count is unchanged" $ do
            (logger, _) ← callbackLogger
            outcome ← loadInfectionYamlOutcome logger shippedPath
            fmap length outcome `shouldBe` Just 8

        it "every shipped definition satisfies the domain the decoder \
           \now enforces — checked against the shipped values \
           \themselves, not just against the pinned baseline" $ do
            (logger, _) ← callbackLogger
            defs ← fromMaybe [] ⊚ loadInfectionYamlOutcome logger shippedPath
            defs `shouldSatisfy` all inDomain

-- | The property requirement 1 states, read back off a decoded
--   definition. Deliberately spelled out here rather than reusing the
--   decoder's own helpers, so a domain that silently loosened would
--   still be caught.
inDomain ∷ InfectionYamlDef → Bool
inDomain d = all governed
                [ iyBaseWeight d, iyAggressiveness d, iyInfectability d
                , iyCureRate d, iyTransmissibility d ]
    ∧ ordered (iyTempMin d) (iyTempMax d)
    ∧ ordered (iyMoistMin d) (iyMoistMax d)
    ∧ iyMoistMin d ≥ 0 ∧ iyMoistMax d ≤ 1
    ∧ iyCategory d `elem` ["bacterial", "parasitic", "fungal", "viral", "prion"]
    ∧ all (`elem` ["surface", "deep"]) (iySites d)
    ∧ all (`elem` ["antibiotics"]) (iyCurableBy d)
  where
    governed x  = x ≥ 0 ∧ not (isNaN x) ∧ not (isInfinite x)
    ordered a b = a ≤ b ∧ not (isNaN a) ∧ not (isNaN b)
                        ∧ not (isInfinite a) ∧ not (isInfinite b)

shippedPath ∷ FilePath
shippedPath = "data" </> "infections" </> "bacteria.yaml"

-- | The shipped baseline this change must not move: every definition in
--   @data/infections/bacteria.yaml@, in file order, with every field it
--   authors. Pinned in full rather than by count so a swapped band or a
--   shifted multiplier is caught too.
shippedInfections ∷ [InfectionYamlDef]
shippedInfections =
    [ InfectionYamlDef
        { iyId = "staph", iyName = "Staph infection"
        , iyIcon = "bacterial_infection", iyCategory = "bacterial"
        , iySites = ["surface"], iyBaseWeight = 6.0
        , iyTempMin = -20, iyTempMax = 45
        , iyMoistMin = 0.0, iyMoistMax = 1.0
        , iyAggressiveness = 1.0, iyInfectability = 1.2
        , iyCurableBy = ["antibiotics"], iyCureRate = 1.0
        , iyWoundInfectable = True, iyEffects = []
        , iyTransmissibility = 1.0, iyTransmission = ["contact"]
        }
    , InfectionYamlDef
        { iyId = "strep", iyName = "Strep infection"
        , iyIcon = "bacterial_infection", iyCategory = "bacterial"
        , iySites = ["surface"], iyBaseWeight = 2.5
        , iyTempMin = -10, iyTempMax = 45
        , iyMoistMin = 0.0, iyMoistMax = 1.0
        , iyAggressiveness = 1.3, iyInfectability = 1.1
        , iyCurableBy = ["antibiotics"], iyCureRate = 1.0
        , iyWoundInfectable = True, iyEffects = ["necrosis"]
        , iyTransmissibility = 1.0, iyTransmission = ["contact", "fluid"]
        }
    , InfectionYamlDef
        { iyId = "ecoli", iyName = "E. coli infection"
        , iyIcon = "bacterial_infection", iyCategory = "bacterial"
        , iySites = ["surface", "deep"], iyBaseWeight = 1.0
        , iyTempMin = 10, iyTempMax = 45
        , iyMoistMin = 0.4, iyMoistMax = 1.0
        , iyAggressiveness = 1.1, iyInfectability = 1.0
        , iyCurableBy = ["antibiotics"], iyCureRate = 1.0
        , iyWoundInfectable = True, iyEffects = []
        , iyTransmissibility = 1.0
        , iyTransmission = ["contact", "fecal", "fluid"]
        }
    , InfectionYamlDef
        { iyId = "clostridium", iyName = "Gas gangrene"
        , iyIcon = "bacterial_infection", iyCategory = "bacterial"
        , iySites = ["deep"], iyBaseWeight = 3.0
        , iyTempMin = 5, iyTempMax = 45
        , iyMoistMin = 0.2, iyMoistMax = 1.0
        , iyAggressiveness = 1.7, iyInfectability = 1.5
        , iyCurableBy = ["antibiotics"], iyCureRate = 0.85
        , iyWoundInfectable = True, iyEffects = ["necrosis", "gas"]
        , iyTransmissibility = 0.0, iyTransmission = []
        }
    , InfectionYamlDef
        { iyId = "bacteroides", iyName = "Bacteroides infection"
        , iyIcon = "bacterial_infection", iyCategory = "bacterial"
        , iySites = ["deep"], iyBaseWeight = 2.0
        , iyTempMin = -5, iyTempMax = 45
        , iyMoistMin = 0.0, iyMoistMax = 1.0
        , iyAggressiveness = 1.3, iyInfectability = 1.0
        , iyCurableBy = ["antibiotics"], iyCureRate = 1.0
        , iyWoundInfectable = True, iyEffects = []
        , iyTransmissibility = 0.0, iyTransmission = []
        }
    , InfectionYamlDef
        { iyId = "peptostreptococcus", iyName = "Anaerobic strep"
        , iyIcon = "bacterial_infection", iyCategory = "bacterial"
        , iySites = ["deep"], iyBaseWeight = 1.5
        , iyTempMin = -5, iyTempMax = 45
        , iyMoistMin = 0.0, iyMoistMax = 1.0
        , iyAggressiveness = 1.2, iyInfectability = 0.9
        , iyCurableBy = ["antibiotics"], iyCureRate = 1.0
        , iyWoundInfectable = True, iyEffects = []
        , iyTransmissibility = 0.0, iyTransmission = []
        }
    , InfectionYamlDef
        { iyId = "fusobacterium", iyName = "Fusobacterium infection"
        , iyIcon = "bacterial_infection", iyCategory = "bacterial"
        , iySites = ["deep"], iyBaseWeight = 1.0
        , iyTempMin = 0, iyTempMax = 45
        , iyMoistMin = 0.2, iyMoistMax = 1.0
        , iyAggressiveness = 1.4, iyInfectability = 1.1
        , iyCurableBy = ["antibiotics"], iyCureRate = 0.95
        , iyWoundInfectable = True, iyEffects = ["necrosis"]
        , iyTransmissibility = 0.0, iyTransmission = []
        }
    , InfectionYamlDef
        { iyId = "prion_spongiform", iyName = "Spongiform infection"
        , iyIcon = "parasitic_infection", iyCategory = "prion"
        , iySites = [], iyBaseWeight = 0.0
        , iyTempMin = -50, iyTempMax = 50
        , iyMoistMin = 0.0, iyMoistMax = 1.0
        , iyAggressiveness = 0.4, iyInfectability = 0.3
        , iyCurableBy = [], iyCureRate = 0.0
        , iyWoundInfectable = False, iyEffects = ["neuro", "chronic"]
        , iyTransmissibility = 1.0
        , iyTransmission = ["fecal", "meat", "fluid"]
        }
    ]

-- | A logger whose backend appends every emitted 'LogEntry' to an
--   'IORef'. 'CatAsset' debug logging stays OFF (the default) so a
--   rejection's warning is the only entry captured, which is what lets
--   'rejectsNaming' require exactly one.
callbackLogger ∷ IO (LoggerState, IORef [LogEntry])
callbackLogger = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    pure (logger, entriesRef)

withTempYaml ∷ FilePath → String → (FilePath → IO a) → IO a
withTempYaml name contents action =
    withExclusiveTempDirectory "synarchy-infection-schema-spec" $ \dir → do
        let path = dir </> name
        writeFile path contents
        action path
