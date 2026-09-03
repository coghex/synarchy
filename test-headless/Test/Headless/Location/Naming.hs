{-# LANGUAGE Strict #-}
-- | "Location naming" (#1101): a placed location named in its world's
--   own generated language. Pure — no engine. The production concept
--   catalogue is read straight from @data/language/concepts.yaml@ and
--   the production ruin definition's authored pools are mirrored here,
--   so the scheme these specs exercise is the one that ships.
module Test.Headless.Location.Naming (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BC
import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Semantic.Catalogue ( conceptCataloguePath
                                   , conceptOrdinalPath, loadCatalogue
                                   , parseCatalogue )
import Language.Generated.Types
    ( LanguageProvenance(..), LangSeed(..), GeneratorVersion(..)
    , currentGeneratorVersion, generatorErrorText )
import Language.Generated.Profile (generateProfile)
import Language.Generated.Root (assignLanguageRoots)
import Test.Headless.Language.Generated.Support (expectRoots)
import Language.Generated.Bound (LanguageRoots(..))
import Location.Bounds (RelBounds(..))
import Location.Instance
import Location.Naming
import Test.Headless.Location.Fixture (expectGeometry)
import Location.Overlay.Types (LocationOverlay)
import Location.Types
import World.Chunk.Types (ChunkCoord(..))
import qualified Data.HashMap.Strict as HM

-- * Fixtures ---------------------------------------------------------

-- | The production ruin's authored pools, mirrored. If the shipped YAML
--   drifts from this the specs still pass — they are about the naming
--   MACHINE, not the content — but the machine is exercised against a
--   realistically-sized scheme rather than a one-entry toy.
ruinNaming ∷ LocationNaming
ruinNaming = LocationNaming
    { lnHeads     = map ConceptId
        [ "KEEP", "TOWER", "HAVEN", "SANCTUARY", "GATE", "THRESHOLD"
        , "HOLLOW", "CAVERN", "HEARTH", "WALL", "SPIRE", "THRONE" ]
    , lnModifiers = map ConceptId
        [ "ASH", "STONE", "IRON", "SAND", "RUIN", "SILENCE", "SORROW"
        , "FROST", "SHADOW", "HOLLOW", "BONE", "OATH" ]
    }

ruinDef ∷ LocationDef
ruinDef = LocationDef
    { ldId              = "ruin_small"
    , ldLabel           = "Small Ruin"
    , ldType            = "ruin"
    , ldBuilder         = "room_small_damaged"
    , ldAnchor          = []
    , ldMaxCount        = 6
    , ldMinSpacing      = 5
    , ldContents        = []
    , ldBounds          = RelBounds (-2) (-2) 2 2
    , ldMapIcon         = Nothing
    , ldNaming          = ruinNaming
    }

registry ∷ LocationRegistry
registry = registerLocation ruinDef emptyLocationRegistry

-- | Three placed ruins, so a world has more than one name to compare.
overlay ∷ LocationOverlay
overlay = HM.fromList
    [ (ChunkCoord 0 0, "ruin_small")
    , (ChunkCoord 4 1, "ruin_small")
    , (ChunkCoord (-3) 7, "ruin_small") ]

-- | Two DIFFERENT languages. Distinct seeds at the current generator.
provA, provB ∷ LanguageProvenance
provA = LanguageProvenance (LangSeed 0x5EED0000000000A1) currentGeneratorVersion
provB = LanguageProvenance (LangSeed 0x0FF1CE0000000B2C) currentGeneratorVersion

spec ∷ Spec
spec = describe "Location naming" $ do
    prodCatE ← runIO $ loadCatalogue conceptCataloguePath conceptOrdinalPath
    let cat = either (error ∘ T.unpack ∘ catalogueErrorText) id prodCatE
        namerOf prov = case mkLocationNamer cat prov of
            Left e  → error ("mkLocationNamer failed: " <> show e)
            Right n → n
        namerA  = namerOf provA
        namerB  = namerOf provB
        builtA  = expectGeometry
                      (buildLocationInstances (Just namerA) registry overlay)
        builtB  = expectGeometry
                      (buildLocationInstances (Just namerB) registry overlay)
        builtNone = expectGeometry
                      (buildLocationInstances Nothing registry overlay)
        namesOf = map liDisplayName ∘ instancesToList
        glossesOf = map liGloss ∘ instancesToList

    describe "a world with a language" $ do
        it "names every placed location natively, not from the label" $ do
            namesOf builtA `shouldSatisfy` all (≢ ldLabel ruinDef)
            namesOf builtA `shouldSatisfy` all (not ∘ T.null)

        it "stores an English gloss for every generated name" $
            glossesOf builtA `shouldSatisfy` all
                (maybe False (not ∘ T.null))

        it "the gloss is the SAME expression's English rendering -- two \
           \words, modifier then head, both from the authored pools" $
            forM_ (instancesToList builtA) $ \inst →
                case liGloss inst of
                    Nothing → expectationFailure "expected a gloss"
                    Just g  → case map T.toLower (T.words g) of
                        [m, h] → do
                            m `shouldSatisfy` (`elem` modifierForms)
                            h `shouldSatisfy` (`elem` headForms)
                        other  → expectationFailure
                            ("expected a two-word gloss, got " <> show other)

        it "two locations in ONE world share the language: every root a \
           \name is built from comes from that language's own assignment" $ do
            let roots = lrFree (expectRoots
                            (assignLanguageRoots (profileOf provA)
                                                  (catOrdinals cat)
                                                  (conceptIds cat)))
                -- Every rendered name is a compound of two of this
                -- language's roots, so each name must contain at least
                -- one of them as a substring (join style and boundary
                -- repair may alter the seam, never the leading root).
                anyRootIn nm = any (\r → T.toLower r `T.isInfixOf` T.toLower nm)
                                   (M.elems roots)
            namesOf builtA `shouldSatisfy` all anyRootIn

        -- GOLDEN VECTOR (#1383). The exact text this fixture renders:
        -- provA at 'currentGeneratorVersion', over the three placements
        -- above, in canonical instance-id order. Written out by hand --
        -- never recomputed from 'buildLocationInstances' or
        -- 'nameLocationInstance', which is what makes it an oracle
        -- rather than a restatement.
        --
        -- A failure means this fixture's INTEGRATED output changed, and
        -- the cause is not always the same one. It can come from
        -- 'locationNameExpr' (@src/Location/Naming.hs@), the catalogue
        -- draw and root assignment (@src/Language/Naming.hs@), instance
        -- ordering or id allocation (@src/Location/Instance.hs@), the
        -- shipped @data/language/concepts.yaml@, or the authored pools
        -- mirrored at the top of this module.
        --
        -- When the cause is a change to VERSIONED profile, root or
        -- rendering behaviour, that change is deliberate and
        -- 'currentGeneratorVersion' must be bumped with it: a stored
        -- name is decomposable later (#1104) only because its recorded
        -- version says which generator rendered it, so a rendering
        -- change that lands without a bump silently makes every already
        -- persisted name undecodable. Every other cause must still be
        -- investigated before this vector is re-blessed. It is NOT a
        -- number to refresh until the suite goes green.
        it "renders exactly this name and gloss for each of the three \
           \instances -- the pinned vector for provA at the current \
           \generator version" $ do
            namesOf builtA `shouldBe`
                [ "Leraj-yroeb", "Jdyebto-efbne", "Fyąyn-fkofbe" ]
            glossesOf builtA `shouldBe`
                [ Just "Ashen Sanctuary"
                , Just "Hollow Throne"
                , Just "Iron Cavern" ]

        it "different instances of the same definition get different \
           \names -- the choice is driven by the instance id, not the def" $
            length (nub (namesOf builtA)) `shouldBe` 3

    describe "two worlds with different languages" $ do
        it "name the same placements differently" $
            namesOf builtA `shouldSatisfy` \a → and (zipWith (≢) a (namesOf builtB))

        it "gloss them differently too -- the concept draw is part of the \
           \language, so two worlds are not the same ruins respelled" $
            glossesOf builtA `shouldSatisfy` (≢ glossesOf builtB)

    describe "a world with NO language (#1101 requirement 6)" $ do
        it "falls back to the definition's label" $
            namesOf builtNone `shouldBe` replicate 3 (ldLabel ruinDef)

        it "stores no gloss -- a label has no meaning to explain" $
            glossesOf builtNone `shouldBe` replicate 3 Nothing

        it "the pre-#911 legacy migration names from the label too, and \
           \never infers a language" $ do
            let migrated = expectGeometry
                    (resolveLegacyLocationInstances registry overlay
                        (pendingLegacyFlags mempty mempty))
            namesOf migrated `shouldBe` replicate 3 (ldLabel ruinDef)
            glossesOf migrated `shouldBe` replicate 3 Nothing

    describe "write-once (#1101 requirement 4)" $
        it "a re-render of an EXISTING table is a no-op: the only \
           \registry-consulting path after placement refuses to touch a \
           \resolved table" $
            resolveLegacyLocationInstances registry overlay builtA
                `shouldBe` Right builtA

    describe "determinism from the instance's own identity" $ do
        it "the id decides the name: an instance allocated at id 2 gets \
           \the same name whichever chunk it sits in" $ do
            let nameAt coord =
                    let (iid, lis) = expectGeometry
                            (allocateLocationInstance (Just namerA)
                                coord ruinDef emptyLocationInstances)
                        (_, lis') = expectGeometry
                            (allocateLocationInstance (Just namerA)
                                coord ruinDef lis)
                    in ( fmap liDisplayName (lookupLocationInstance iid lis')
                       , map liDisplayName (instancesToList lis') )
            snd (nameAt (ChunkCoord 0 0)) `shouldBe`
                snd (nameAt (ChunkCoord 12 (-9)))

        it "matches what a fresh placement of the same overlay allocates" $
            namesOf builtA `shouldBe`
                [ nm
                | n ← [1, 2, 3]
                , let (nm, _, _) = nameLocationInstance (Just namerA) ruinDef n ]

    describe "authored-scheme validation" $ do
        it "accepts the production ruin's scheme" $
            locationNamingErrors cat ruinDef `shouldBe` []

        it "rejects an empty head pool, naming the definition and field" $
            locationNamingErrors cat (withNaming (ruinNaming { lnHeads = [] }))
                `shouldSatisfy` \errs →
                    any (\e → "ruin_small" `T.isInfixOf` e
                            ∧ "naming.heads" `T.isInfixOf` e) errs

        it "rejects an empty modifier pool" $
            locationNamingErrors cat
                    (withNaming (ruinNaming { lnModifiers = [] }))
                `shouldSatisfy` \errs →
                    any ("naming.modifiers" `T.isInfixOf`) errs

        it "rejects an unknown concept id" $
            locationNamingErrors cat (withNaming
                    (ruinNaming { lnHeads = [ConceptId "NOT_A_CONCEPT"] }))
                `shouldSatisfy` \errs →
                    any ("unknown concept" `T.isInfixOf`) errs

        it "rejects a modifier concept with no modifier form -- the slot's \
           \required lexical form, not merely a known id" $ do
            -- A concept that exists but authors only a singular.
            let partialOrdinals =
                    either (error ∘ T.unpack ∘ catalogueErrorText) id $
                        mkConceptOrdinals [ (ConceptId "KEEP", 0)
                                          , (ConceptId "ASH", 1) ]
                partial = either (error ∘ T.unpack ∘ catalogueErrorText) id $
                    parseCatalogue partialOrdinals $ BC.pack $ unlines
                        [ "version: 1"
                        , "concepts:"
                        , "  - id: KEEP"
                        , "    domain: place"
                        , "    singular: keep"
                        , "  - id: ASH"
                        , "    domain: element"
                        , "    singular: ash"
                        ]
                def = withNaming (LocationNaming [ConceptId "KEEP"]
                                                 [ConceptId "ASH"])
            locationNamingErrors partial def `shouldSatisfy` \errs →
                any (\e → "ASH" `T.isInfixOf` e
                        ∧ "modifier form" `T.isInfixOf` e) errs
            -- and the head, which needs only the mandatory singular, is
            -- accepted by the same call
            locationNamingErrors partial def `shouldSatisfy` \errs →
                not (any ("KEEP" `T.isInfixOf`) errs)

    describe "an unconstructible generator version" $
        it "is refused rather than silently rendered in another language" $
            case mkLocationNamer cat
                    (LanguageProvenance (LangSeed 1) (GeneratorVersion 9999)) of
                Left _  → pure ()
                Right _ → expectationFailure
                    "expected an unsupported-version error"

    -- #2206. A version this build CAN construct whose profile still has
    -- no room for the catalogue's 151 concepts. The refusal is what
    -- routes such a page onto the no-namer fallback the group above
    -- pins — before the capacity gate, this call never returned.
    describe "a language whose root space cannot name the catalogue" $ do
        let shortProv = LanguageProvenance (LangSeed 1116)
                                            currentGeneratorVersion
        it "is refused, naming its shortfall" $
            case mkLocationNamer cat shortProv of
                Right _ → expectationFailure
                    "built a location namer from a 144-root language"
                Left err → generatorErrorText err
                    `shouldSatisfy` T.isInfixOf "shortfall 7"

        -- And the refusal really does land on the label fallback: the
        -- caller's only two options are a namer or 'Nothing', and
        -- 'Nothing' is what 'builtNone' above is built from.
        it "leaves such a page on the definition-label fallback" $ do
            namesOf builtNone `shouldBe` replicate 3 (ldLabel ruinDef)
            glossesOf builtNone `shouldBe` replicate 3 Nothing
            map liEtymology (instancesToList builtNone)
                `shouldBe` replicate 3 Nothing
  where
    withNaming n = ruinDef { ldNaming = n }

    profileOf prov = case generateProfile (lpVersion prov) (lpSeed prov) of
        Left e  → error ("generateProfile failed: " <> show e)
        Right p → p

    headForms =
        [ "keep", "tower", "haven", "sanctuary", "gate", "threshold"
        , "hollow", "cavern", "hearth", "wall", "spire", "throne" ]
    modifierForms =
        [ "ashen", "stone", "iron", "sandy", "ruined", "silent"
        , "sorrowful", "frozen", "shadowy", "hollow", "bone", "sworn" ]
