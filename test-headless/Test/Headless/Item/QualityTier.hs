-- | Pure tests for the quality→label tier resolution (#345): the
--   default band table's boundaries, an item def's own
--   `quality_tiers:` override taking precedence, and the YAML schema
--   for that override list.
--
--   Since #1739 the schema half is a real CONTRACT rather than two
--   required fields. 'Item.Types.qualityTierLabel' replaces
--   'Item.Types.defaultQualityTiers' WHOLESALE for any non-empty
--   override — the default's 0-floor band is never supplied as a
--   fallback — so an override that cannot label every quality resolves
--   to 'Nothing' over the rest of the range, and 'Nothing' is what all
--   four reader sites turn into an OMITTED tier field. Malformed
--   authoring was therefore indistinguishable from an item with no
--   tiers at all: the suffix simply disappeared, with no error
--   anywhere. Five faults produced exactly that symptom, and
--   'Engine.Asset.YamlItems.parseItemYamlQualityTiers' now rejects each
--   at content load.
--
--   Every negative case below decodes a COMPLETE @items:@ file, not a
--   standalone band, because that is where the fix lives: the owning
--   definition's name (which every diagnostic must carry) and the
--   @quality:@/@quality_tiers:@ cross-field rule are both reachable
--   only from the aggregate parser. Each fixture is authored so the
--   other four rules ACCEPT it, so no single rule can mask another.
module Test.Headless.Item.QualityTier (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import Item.Types (ItemDef(..), QualityTier(..), qualityTierLabel)
import Engine.Asset.YamlItems
    (ItemYamlDef(..), ItemYamlFile(..), ItemYamlQualityTier(..))
import Engine.Asset.Handle (TextureHandle(..))

-- | A minimal ItemDef stand-in — only idQualityTiers matters here.
blankDef ∷ [QualityTier] → ItemDef
blankDef tiers = ItemDef
    { idName = "test_item", idDisplayName = "Test Item"
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0, idWeight = 0, idWeightSpec = Nothing
    , idBulk = 1.0, idStorage = Nothing
    , idKind = "misc", idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Just (0, 100)
    , idQualityTiers = tiers
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Nothing, idWeapon = Nothing, idArmor = Nothing
    , idUnequippable = False, idBuffs = [], idInsulation = 0
    , idSourcePath = "test-fixture"
    }

decode ∷ BS.ByteString → Either String ItemYamlQualityTier
decode = either (Left . show) Right . Yaml.decodeEither'

-- * YAML fixtures
--
--   Raw source text rather than constructed values, because half of
--   what is under test is how the YAML scalar resolver and the 'Float'
--   narrowing interact: @1.0e+100@ resolving to a perfectly ordinary
--   'Scientific' that becomes 'Infinity' only in @iyqtMin@, and
--   @50.0000001@ resolving to a 'Scientific' distinct from @50@ that
--   collapses onto it in a 32-bit float, are both facts about the
--   source text and invisible to a fixture built from Haskell values.

-- | A minimally valid definition named @n@, with @extra@ appended
--   verbatim so a @quality:@ / @quality_tiers:@ block (or a
--   deliberately malformed one) is expressible exactly as an author
--   would write it.
probeNamed ∷ String → String → String
probeNamed n extra = unlines
    [ "items:"
    , "  - name: \"" ⧺ n ⧺ "\""
    , "    sprite: \"assets/textures/items/probe.png\""
    , "    weight: 0.5"
    , "    bulk: 1.0"
    ] ⧺ extra

-- | The same, under the name every rejection assertion looks for.
probeDef ∷ String → String
probeDef = probeNamed "probe_blade"

-- | The @quality:@ roll spec a tier table needs to be able to take
--   effect at all (requirement 5).
qualityBlock ∷ String
qualityBlock = unlines
    [ "    quality:"
    , "      min: 0"
    , "      max: 100"
    ]

-- | A @quality_tiers:@ list whose entries are @bands@ verbatim.
tiersBlock ∷ [String] → String
tiersBlock bands =
    "    quality_tiers:\n" ⧺ concatMap (\b → "      - " ⧺ b ⧺ "\n") bands

-- | The common shape: a rollable item whose tier table is @bands@.
rollableWith ∷ [String] → String
rollableWith bands = probeDef (qualityBlock ⧺ tiersBlock bands)

decodeDef ∷ String → Either String ItemYamlDef
decodeDef src = case Yaml.decodeEither' (BS.pack src) of
    Left err                 → Left (show err)
    Right (ItemYamlFile [d]) → Right d
    Right (ItemYamlFile ds)  →
        Left ("expected exactly one definition, got " ⧺ show (length ds))

-- | Did the decode fail, and does the message name the definition AND
--   every token the reader needs?
--
--   Both halves are load-bearing (requirement 6). A rejection that does
--   not name @probe_blade@ is the unusable @$.items[0].quality_tiers@
--   diagnostic this exists to rule out; one that does not report the
--   offending VALUE leaves an author scanning a table by eye for which
--   band is wrong — and for the overflow case the effective value
--   differs from what was authored, so the message has to carry the
--   authored one.
--
--   Tokens are matched as whole WORDS of a punctuation-scrubbed
--   message, not substrings, so @quality@ cannot be satisfied by a
--   message that only ever says @quality_tiers@. The scrub deliberately
--   leaves @.@ and @-@ alone: they are inside the values (@-5.0@,
--   @1.0e100@, @-.inf@) the tokens have to match. It DOES drop the
--   backslash, which 'show'ing the exception adds around a rendered
--   'Aeson.String' and which is no part of the authored token.
rejectsNaming ∷ [String] → String → Expectation
rejectsNaming tokens src = case decodeDef src of
    Right d → expectationFailure $
        "expected a rejection, but the definition parsed: " ⧺ show d
    Left err →
        let ws      = words (map scrub err)
            missing = [t | t ← "probe_blade" : tokens, t `notElem` ws]
        in if null missing
             then pure ()
             else expectationFailure $
                 "rejected, but the message does not name "
                 ⧺ show missing ⧺ ": " ⧺ err
  where
    scrub c = if c `elem` ("'\"\\(),:;=\8212" ∷ String) then ' ' else c

-- | The decoded tier table of a definition that is expected to PARSE.
tiersOf ∷ String → Either String [ItemYamlQualityTier]
tiersOf = fmap iydQualityTiers ∘ decodeDef

-- | The YAML→registry mapping 'Engine.Scripting.Lua.API.Items.Defs'
--   applies verbatim, so a table proven here is the table the resolver
--   will actually see. Kept pure: reaching @itemDefFromYaml@ itself
--   would need a live engine for the texture handle, and none of the
--   rest of that mapping is under test.
installedTiers ∷ [ItemYamlQualityTier] → [QualityTier]
installedTiers = map (\t → QualityTier (iyqtMin t) (iyqtLabel t))

spec ∷ Spec
spec = do
    describe "qualityTierLabel (default table)" $ do
        let def = blankDef []
        it "labels 100 as excellent" $
            qualityTierLabel def 100 `shouldBe` Just "excellent"
        it "labels the 90 boundary as excellent" $
            qualityTierLabel def 90 `shouldBe` Just "excellent"
        it "labels just under 90 as good" $
            qualityTierLabel def 89.9 `shouldBe` Just "good"
        it "labels the 75 boundary as good" $
            qualityTierLabel def 75 `shouldBe` Just "good"
        it "labels just under 75 as average" $
            qualityTierLabel def 74.9 `shouldBe` Just "average"
        it "labels the 50 boundary as average" $
            qualityTierLabel def 50 `shouldBe` Just "average"
        it "labels just under 50 as bad" $
            qualityTierLabel def 49.9 `shouldBe` Just "bad"
        it "labels the 25 boundary as bad" $
            qualityTierLabel def 25 `shouldBe` Just "bad"
        it "labels just under 25 as atrocious" $
            qualityTierLabel def 24.9 `shouldBe` Just "atrocious"
        it "labels 0 as atrocious" $
            qualityTierLabel def 0 `shouldBe` Just "atrocious"

    describe "qualityTierLabel (per-def override)" $ do
        let custom = [ QualityTier 80 "masterwork", QualityTier 0 "crude" ]
            def    = blankDef custom
        it "uses the def's own table instead of the default" $
            qualityTierLabel def 95 `shouldBe` Just "masterwork"
        it "falls through the override table's own bands" $
            qualityTierLabel def 10 `shouldBe` Just "crude"
        it "ignores an unrelated quality with an empty override" $
            -- An empty override list still falls back to the default —
            -- there's always a 0-floor band to land on.
            qualityTierLabel (blankDef []) 60 `shouldBe` Just "average"

    describe "ItemYamlQualityTier YAML parsing" $ do
        it "parses a { min, label } entry" $
            decode "{ min: 90, label: excellent }"
              `shouldBe` Right ItemYamlQualityTier
                  { iyqtMin = 90, iyqtLabel = "excellent" }
        it "requires both min and label" $
            decode "{ min: 90 }"
              `shouldSatisfy` either (const True) (const False)

    -- * #1739: the authored table is a contract, not two fields

    describe "quality_tiers: rejected authoring" $ do
        it "requirement 1: a table with no 0-floor band, naming the \
           \lowest band it does author" $
            -- Finite, in range, distinct, labelled — the ONLY fault is
            -- that quality 39 would resolve to no label at all.
            rejectsNaming ["0", "40.0"] (rollableWith
                [ "{ min: 80, label: masterwork }"
                , "{ min: 40, label: fair }"
                ])

        it "requirement 2: a min that overflows to Infinity, naming \
           \both the authored number and the effective one" $
            -- `.nan` / `.inf` never get this far — YAML's scalar
            -- resolver reads them as STRINGS, so the { min, label }
            -- decode rejects them as type errors. A perfectly ordinary
            -- 1.0e+100 is a valid Scientific that becomes Infinity only
            -- once narrowed to iyqtMin's 32-bit Float, which is why
            -- finiteness is tested after narrowing.
            rejectsNaming ["1.0e100", "Infinity"] (rollableWith
                [ "{ min: 1.0e+100, label: masterwork }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 2: .nan is a STRING to YAML, and is rejected \
           \naming the definition and the authored token" $
            -- The other half of requirement 2, and a genuinely
            -- different fault from the overflow above: YAML's scalar
            -- resolver only recognizes ordinary numeric syntax, so
            -- `.nan` never reaches a number at all. Delegating the band
            -- decode to ItemYamlQualityTier's FromJSON instance would
            -- surface this as a bare aeson type error naming neither
            -- the definition nor the token, which is what requirement 6
            -- rules out.
            -- The token can only come from the PRINTED value: the
            -- diagnostic deliberately never spells .nan or .inf again
            -- in its explanation, so this cannot pass vacuously.
            rejectsNaming ["String", ".nan"] (rollableWith
                [ "{ min: .nan, label: masterwork }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 2: .inf is a STRING too" $
            rejectsNaming ["String", ".inf"] (rollableWith
                [ "{ min: .inf, label: masterwork }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 2: and so is -.inf" $
            rejectsNaming ["String", "-.inf"] (rollableWith
                [ "{ min: -.inf, label: broken }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 6: a band with no min names the definition" $
            rejectsNaming ["min", "band"] (rollableWith
                [ "{ label: masterwork }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 6: a band with no label names the definition \
           \and the band's min" $
            rejectsNaming ["label", "80.0"] (rollableWith
                [ "{ min: 80 }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 6: a non-textual label names the definition \
           \and the offending value" $
            rejectsNaming ["label", "80.0", "Number"] (rollableWith
                [ "{ min: 80, label: 90 }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 6: a band that is not a block at all" $
            rejectsNaming ["band", "Number"] (rollableWith
                [ "80"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 3: a min below 0" $
            rejectsNaming ["-5.0", "0..100"] (rollableWith
                [ "{ min: -5, label: broken }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 3: a min above 100" $
            rejectsNaming ["150.0", "0..100"] (rollableWith
                [ "{ min: 150, label: divine }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 3: two bands sharing a min exactly, naming the \
           \duplicated threshold" $
            rejectsNaming ["50.0"] (rollableWith
                [ "{ min: 50, label: fair }"
                , "{ min: 50, label: good }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 3: two distinct YAML numbers that collapse to \
           \one Float still tie" $
            -- 50.0000001 is a Scientific distinct from 50, but a
            -- 32-bit Float's ulp near 50 is ~3.8e-6, so both narrow to
            -- exactly 50.0 and would resolve by author order.
            rejectsNaming ["50.0"] (rollableWith
                [ "{ min: 50, label: fair }"
                , "{ min: 50.0000001, label: good }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 4: an empty label, naming the band's min" $
            -- The label is what the message would normally identify the
            -- band by, and a blank one prints as nothing, so the min is
            -- the findable half.
            rejectsNaming ["80.0", "label"] (rollableWith
                [ "{ min: 80, label: \"\" }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 4: a whitespace-only label — the test is on the \
           \TRIMMED label" $
            rejectsNaming ["80.0", "label"] (rollableWith
                [ "{ min: 80, label: \"   \" }"
                , "{ min: 0, label: crude }"
                ])

        it "requirement 5: an otherwise-valid table on a definition \
           \with no quality: spec" $
            -- Every other rule accepts this table; it simply could
            -- never take effect, because all four readers gate on the
            -- quality spec before consulting it at all.
            rejectsNaming ["quality", "quality_tiers", "2"]
                (probeDef (tiersBlock
                    [ "{ min: 80, label: masterwork }"
                    , "{ min: 0, label: crude }"
                    ]))

        it "requirement 5: quality: null is no quality spec, because \
           \that is the field the runtime sees" $
            rejectsNaming ["quality", "quality_tiers"]
                (probeDef ("    quality:\n" ⧺ tiersBlock
                    [ "{ min: 80, label: masterwork }"
                    , "{ min: 0, label: crude }"
                    ]))

        it "a quality_tiers: that is not a list at all" $
            rejectsNaming ["quality_tiers"]
                (probeDef (qualityBlock ⧺ "    quality_tiers: 80\n"))

    describe "quality_tiers: accepted authoring" $ do
        it "requirement 7: an accepted table labels every quality in \
           \0..100 with exactly one non-blank label" $ do
            let src = rollableWith
                    [ "{ min: 80, label: masterwork }"
                    , "{ min: 40, label: fair }"
                    , "{ min: 0, label: crude }"
                    ]
            case decodeDef src of
                Left err → expectationFailure
                    ("expected the table to parse, got " ⧺ err)
                Right d  → do
                    let def = blankDef (installedTiers (iydQualityTiers d))
                        qs  = [ fromIntegral n / 4 ∷ Float
                              | n ← [0 .. 400 ∷ Int] ]
                        bad = [ (q, qualityTierLabel def q)
                              | q ← qs
                              , case qualityTierLabel def q of
                                    Nothing → True
                                    Just l  → l ≡ ""
                              ]
                    bad `shouldBe` []

        it "requirement 7: and it resolves to the authored bands, not \
           \the default table" $ do
            let src = rollableWith
                    [ "{ min: 80, label: masterwork }"
                    , "{ min: 40, label: fair }"
                    , "{ min: 0, label: crude }"
                    ]
            case decodeDef src of
                Left err → expectationFailure
                    ("expected the table to parse, got " ⧺ err)
                Right d  → do
                    let def = blankDef (installedTiers (iydQualityTiers d))
                    map (qualityTierLabel def) [0, 39.9, 40, 79.9, 80, 100]
                        `shouldBe` map Just
                            [ "crude", "crude", "fair"
                            , "fair", "masterwork", "masterwork" ]

        it "requirement 3: bands may be authored in any order, since \
           \resolution sorts them" $
            tiersOf (rollableWith
                [ "{ min: 0, label: crude }"
                , "{ min: 80, label: masterwork }"
                ]) `shouldBe` Right
                    [ ItemYamlQualityTier 0 "crude"
                    , ItemYamlQualityTier 80 "masterwork" ]

        it "requirement 3: the 0 and 100 bounds are both INCLUSIVE" $
            tiersOf (rollableWith
                [ "{ min: 100, label: flawless }"
                , "{ min: 0, label: crude }"
                ]) `shouldBe` Right
                    [ ItemYamlQualityTier 100 "flawless"
                    , ItemYamlQualityTier 0 "crude" ]

        it "requirement 4: a label with surrounding whitespace is kept \
           \verbatim — only a BLANK one is rejected" $
            tiersOf (rollableWith [ "{ min: 0, label: \" crude \" }" ])
                `shouldBe` Right [ ItemYamlQualityTier 0 " crude " ]

    describe "quality_tiers: unchanged compatibility" $ do
        -- Requirements 5 and 8 apply to NON-EMPTY overrides only: an
        -- absent or empty table selects defaultQualityTiers, which
        -- satisfies the same contract, so there is nothing whose effect
        -- could be lost and no quality: spec to require.
        it "an omitted quality_tiers: still decodes to []" $
            tiersOf (probeDef qualityBlock) `shouldBe` Right []

        it "an omitted quality_tiers: decodes on a definition with no \
           \quality: spec either" $
            tiersOf (probeDef "") `shouldBe` Right []

        it "an explicit empty list decodes to []" $
            tiersOf (probeDef (qualityBlock ⧺ "    quality_tiers: []\n"))
                `shouldBe` Right []

        it "an explicit empty list decodes with no quality: spec too" $
            tiersOf (probeDef "    quality_tiers: []\n") `shouldBe` Right []

        it "an explicit null decodes to [], exactly as .:? always read \
           \it" $
            tiersOf (probeDef (qualityBlock ⧺ "    quality_tiers:\n"))
                `shouldBe` Right []

        it "an empty override still falls back to the default table's \
           \own 0 floor" $
            qualityTierLabel (blankDef []) 3 `shouldBe` Just "atrocious"
