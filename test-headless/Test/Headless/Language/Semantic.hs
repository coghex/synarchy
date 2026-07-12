{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Semantic proper names (#709): the pure meaning layer of the
--   world-naming arc (#708) — structured 'NameExpr's over stable
--   concept ids, the versioned English concept catalogue, and the
--   deterministic gloss renderer. Everything here is engine-free; the
--   production catalogue is read straight from
--   @data/language/concepts.yaml@ (tests run from the repo root).
module Test.Headless.Language.Semantic (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Semantic.Catalogue
import Language.Semantic.English

-- Fixture: a deliberately partial catalogue — WOLF is fully authored,
-- HEART is singular-only — so missing-form errors can be exercised
-- without touching the (intentionally total) production catalogue.
partialCat ∷ Catalogue
partialCat = either (error ∘ T.unpack ∘ catalogueErrorText) id $
    parseCatalogue $ yamlOf
        [ "version: 1"
        , "concepts:"
        , "  - id: WOLF"
        , "    domain: creature"
        , "    singular: wolf"
        , "    plural: wolves"
        , "    modifier: wolf"
        , "    possessive: \"wolf's\""
        , "  - id: HEART"
        , "    domain: creature"
        , "    singular: heart"
        ]

yamlOf ∷ [String] → BS.ByteString
yamlOf = BC.pack ∘ unlines

cid ∷ Text → ConceptId
cid = ConceptId

spec ∷ Spec
spec = describe "Semantic proper names" $ do
    prodBytes ← runIO $ BS.readFile conceptCataloguePath
    let prodCat = either (error ∘ T.unpack ∘ catalogueErrorText) id
                         (parseCatalogue prodBytes)
        gloss   = renderGloss prodCat

    describe "production catalogue" $ do
        it "parses and validates" $
            case parseCatalogue prodBytes of
                Left err → expectationFailure $ T.unpack (catalogueErrorText err)
                Right _  → pure ()

        it "is version 1" $
            catVersion prodCat `shouldBe` 1

        it "holds at least 150 unique concepts" $
            conceptCount prodCat `shouldSatisfy` (≥ 150)

        it "spans all six naming domains" $ do
            let domains = nub $ sort $ map ceDomain $ M.elems (catConcepts prodCat)
            domains `shouldBe` [minBound .. maxBound]

        -- The 20-30 balance range is a #713 rule for the six ORIGINAL
        -- domains only; a future optional new domain (#713 req 2) is
        -- exempt from it, so this list is hardcoded rather than
        -- [minBound .. maxBound] (which would wrongly pull any later
        -- domain into the same range).
        it "keeps each of the six original domains within the 20-30 concept balance range" $ do
            let originalDomains =
                    [ DomainPlace, DomainElement, DomainCelestial
                    , DomainCreature, DomainEmotion, DomainMythic ]
                counts = M.fromListWith (+)
                    [ (ceDomain ce, 1 ∷ Int) | ce ← M.elems (catConcepts prodCat) ]
                outOfRange = [ (d, c)
                             | d ← originalDomains
                             , let c = M.findWithDefault 0 d counts
                             , c < 20 ∨ c > 30 ]
            outOfRange `shouldBe` []

        it "authors all four forms for every concept (so #710 can sample any name form)" $ do
            let missing = [ (c, k)
                          | (c, ce) ← M.toList (catConcepts prodCat)
                          , k ← [FormSingular, FormPlural, FormModifier, FormPossessive]
                          , isNothing (formOf k ce) ]
            missing `shouldBe` []

    describe "English glosses (pinned acceptance forms)" $ do
        it "Bare(SILENCE) → Silence" $
            gloss (Bare (cid "SILENCE")) `shouldBe` Right "Silence"

        it "Modifier(ASH, LAND) → Ashen Land" $
            gloss (Modifier (cid "ASH") (cid "LAND")) `shouldBe` Right "Ashen Land"

        it "Of(EYE, plural STORM) → Eye of Storms" $
            gloss (Of (cid "EYE") Plural (cid "STORM")) `shouldBe` Right "Eye of Storms"

        it "Possessive(WOLF, HEART) → Wolf's Heart" $
            gloss (Possessive (cid "WOLF") (cid "HEART")) `shouldBe` Right "Wolf's Heart"

    describe "English glosses (authored forms, not spelling rules)" $ do
        it "irregular plural: Of(EYE, plural MEMORY) → Eye of Memories" $
            gloss (Of (cid "EYE") Plural (cid "MEMORY"))
                `shouldBe` Right "Eye of Memories"

        it "irregular plural: Of(HEART, plural WOLF) → Heart of Wolves" $
            gloss (Of (cid "HEART") Plural (cid "WOLF"))
                `shouldBe` Right "Heart of Wolves"

        it "zero plural for a mass noun: Of(THRONE, plural ICE) → Throne of Ice" $
            gloss (Of (cid "THRONE") Plural (cid "ICE"))
                `shouldBe` Right "Throne of Ice"

        it "singular complement keeps 'of' lowercase: Of(GATE, singular WINTER) → Gate of Winter" $
            gloss (Of (cid "GATE") Singular (cid "WINTER"))
                `shouldBe` Right "Gate of Winter"

        it "suppletive modifier: Modifier(OATH, GATE) → Sworn Gate" $
            gloss (Modifier (cid "OATH") (cid "GATE"))
                `shouldBe` Right "Sworn Gate"

        it "suppletive modifier: Modifier(GOD, THRONE) → Divine Throne" $
            gloss (Modifier (cid "GOD") (cid "THRONE"))
                `shouldBe` Right "Divine Throne"

        it "noun-adjunct modifier: Modifier(IRON, GATE) → Iron Gate" $
            gloss (Modifier (cid "IRON") (cid "GATE"))
                `shouldBe` Right "Iron Gate"

        it "possessive keeps its apostrophe: Possessive(RAVEN, CROWN) → Raven's Crown" $
            gloss (Possessive (cid "RAVEN") (cid "CROWN"))
                `shouldBe` Right "Raven's Crown"

    describe "determinism" $ do
        it "rendering the same expression twice is byte-identical" $ do
            let e = Modifier (cid "ASH") (cid "LAND")
            gloss e `shouldBe` gloss e

        it "reparsing the same catalogue bytes yields an equal catalogue and equal glosses" $ do
            let reparsed = either (error ∘ T.unpack ∘ catalogueErrorText) id
                                  (parseCatalogue prodBytes)
            reparsed `shouldBe` prodCat
            renderGloss reparsed (Of (cid "EYE") Plural (cid "STORM"))
                `shouldBe` gloss (Of (cid "EYE") Plural (cid "STORM"))

    describe "catalogue validation failures" $ do
        it "rejects a duplicate concept id" $ do
            let r = parseCatalogue $ yamlOf
                    [ "version: 1"
                    , "concepts:"
                    , "  - { id: WOLF, domain: creature, singular: wolf }"
                    , "  - { id: WOLF, domain: creature, singular: hound }"
                    ]
            r `shouldBe` Left (DuplicateConceptId (cid "WOLF"))

        it "rejects an empty lexical form, naming the concept and the form" $ do
            let r = parseCatalogue $ yamlOf
                    [ "version: 1"
                    , "concepts:"
                    , "  - { id: WOLF, domain: creature, singular: wolf, plural: \"\" }"
                    ]
            r `shouldBe` Left (InvalidLexicalForm (cid "WOLF") FormPlural
                                                  "form is empty")
            case r of
                Left err → do
                    catalogueErrorText err `shouldSatisfy` T.isInfixOf "WOLF"
                    catalogueErrorText err `shouldSatisfy` T.isInfixOf "plural"
                Right _ → expectationFailure "catalogue should have been rejected"

        it "rejects a lexical form containing whitespace" $ do
            let r = parseCatalogue $ yamlOf
                    [ "version: 1"
                    , "concepts:"
                    , "  - { id: WOLF, domain: creature, singular: \"dire wolf\" }"
                    ]
            r `shouldBe` Left (InvalidLexicalForm (cid "WOLF") FormSingular
                                                  "form contains whitespace")

        it "rejects an unknown domain" $ do
            let r = parseCatalogue $ yamlOf
                    [ "version: 1"
                    , "concepts:"
                    , "  - { id: WOLF, domain: color, singular: wolf }"
                    ]
            r `shouldBe` Left (UnknownDomain (cid "WOLF") "color")

        it "rejects a malformed concept id" $ do
            let r = parseCatalogue $ yamlOf
                    [ "version: 1"
                    , "concepts:"
                    , "  - { id: wolf, domain: creature, singular: wolf }"
                    ]
            case r of
                Left (InvalidConceptId raw _) → raw `shouldBe` "wolf"
                other → expectationFailure $ "expected InvalidConceptId, got " ⧺ show other

        it "rejects a non-positive version" $ do
            let r = parseCatalogue $ yamlOf
                    [ "version: 0"
                    , "concepts: []"
                    ]
            r `shouldBe` Left (InvalidVersion 0)

        it "rejects unparseable YAML descriptively" $ do
            let r = parseCatalogue "version: ["
            case r of
                Left (CatalogueYamlError _) → pure ()
                other → expectationFailure $ "expected CatalogueYamlError, got " ⧺ show other

        it "rejects an entry missing the mandatory singular form" $ do
            let r = parseCatalogue $ yamlOf
                    [ "version: 1"
                    , "concepts:"
                    , "  - { id: WOLF, domain: creature }"
                    ]
            case r of
                Left (CatalogueYamlError _) → pure ()
                other → expectationFailure $ "expected CatalogueYamlError, got " ⧺ show other

        it "rejects two concepts sharing an identical singular form" $ do
            let r = parseCatalogue $ yamlOf
                    [ "version: 1"
                    , "concepts:"
                    , "  - { id: WOLF, domain: creature, singular: wolf }"
                    , "  - { id: HOUND, domain: creature, singular: wolf }"
                    ]
            r `shouldBe` Left (DuplicateSingularForm "wolf" (cid "WOLF") (cid "HOUND"))
            case r of
                Left err → do
                    catalogueErrorText err `shouldSatisfy` T.isInfixOf "WOLF"
                    catalogueErrorText err `shouldSatisfy` T.isInfixOf "HOUND"
                Right _ → expectationFailure "catalogue should have been rejected"

        it "rejects a duplicate singular form even when the case differs" $ do
            let r = parseCatalogue $ yamlOf
                    [ "version: 1"
                    , "concepts:"
                    , "  - { id: WOLF, domain: creature, singular: wolf }"
                    , "  - { id: HOUND, domain: creature, singular: Wolf }"
                    ]
            r `shouldBe` Left (DuplicateSingularForm "Wolf" (cid "WOLF") (cid "HOUND"))

    describe "rendering failures (no silent fallback)" $ do
        it "an unknown concept id is a descriptive error, not raw-id text" $ do
            let r = renderGloss partialCat (Bare (cid "MOON"))
            r `shouldBe` Left (UnknownConcept (cid "MOON"))
            renderErrorText (UnknownConcept (cid "MOON"))
                `shouldSatisfy` T.isInfixOf "MOON"

        it "a missing modifier form fails naming the concept and the form" $ do
            let r = renderGloss partialCat (Modifier (cid "HEART") (cid "WOLF"))
            r `shouldBe` Left (MissingForm (cid "HEART") FormModifier)
            case r of
                Left err → do
                    renderErrorText err `shouldSatisfy` T.isInfixOf "HEART"
                    renderErrorText err `shouldSatisfy` T.isInfixOf "modifier"
                Right g → expectationFailure $ "unexpected gloss: " ⧺ T.unpack g

        it "a missing possessive form fails" $
            renderGloss partialCat (Possessive (cid "HEART") (cid "WOLF"))
                `shouldBe` Left (MissingForm (cid "HEART") FormPossessive)

        it "a missing plural form fails" $
            renderGloss partialCat (Of (cid "WOLF") Plural (cid "HEART"))
                `shouldBe` Left (MissingForm (cid "HEART") FormPlural)

        it "the singular-only concept still renders where only a singular is needed" $
            renderGloss partialCat (Of (cid "HEART") Plural (cid "WOLF"))
                `shouldBe` Right "Heart of Wolves"
