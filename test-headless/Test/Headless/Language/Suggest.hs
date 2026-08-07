-- | Language-derived world-name suggestions (#1106): the producer the
--   Create World dice button calls. Everything here is pure and
--   engine-free — the production catalogue is read straight from
--   @data/language/concepts.yaml@ (tests run from the repo root), and
--   the suggester is driven exactly as the Lua bridge drives it.
--
--   What these pin is the behavior the UI cannot check for itself: the
--   sequence is deterministic, a reroll changes the meaning without
--   changing the language, and a different seed is a different
--   language. The Lua-side half of the contract — which names carry
--   provenance, and what a manual edit erases — is in
--   "Test.Headless.UI.CreateWorldControls".
module Test.Headless.Language.Suggest (spec) where

import UPrelude
import Test.Hspec
import Data.List (nub, sort)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Semantic.Catalogue
import Language.Semantic.English (renderGloss)
import Language.Generated.Types
import Language.Generated.Profile (generateProfile)
import Language.Generated.Signature (profileSignature)
import Language.Generated.Render (renderNative)
import Language.Generated.Orthography (outputInventory)
import Language.Suggest
import Engine.Scripting.Lua.Types (LanguageCache(..))
import Engine.Scripting.Lua.API.World.Lifecycle
    (suggestionStep, suggestionStepLabel, readCatalogueForSuggestions)

-- | Provenance for one world seed at the current generator.
provFor ∷ Word64 → LanguageProvenance
provFor s = LanguageProvenance
    { lpSeed = worldLanguageSeed s, lpVersion = currentGeneratorVersion }

-- | A spread of world seeds standing in for player input: the corners
--   of the 8-hex-digit space Create World produces, plus an arbitrary
--   scatter through the middle.
sampleSeeds ∷ [Word64]
sampleSeeds = [0, 1, 2, 42, 1337, 0x00A3F7C9, 0x7FFFFFFF, 0xFFFFFFFF]
           ⧺ [ fromIntegral (i * 7919 `mod` 0xFFFFFFFF ∷ Int)
             | i ← [1 .. 24 ∷ Int] ]

suggesterFor ∷ Catalogue → Word64 → NameSuggester
suggesterFor cat s = case mkNameSuggester cat (provFor s) of
    Left err  → error (T.unpack (suggestErrorText err))
    Right sgr → sgr

suggestionsFor ∷ Catalogue → Word64 → Int → [NameSuggestion]
suggestionsFor cat s n =
    [ either (error ∘ T.unpack ∘ suggestErrorText) id (suggestNameAt sgr k)
    | k ← [0 .. n - 1] ]
  where sgr = suggesterFor cat s

firstSuggestion ∷ Catalogue → Word64 → NameSuggestion
firstSuggestion cat s =
    either (error ∘ T.unpack ∘ suggestErrorText) id
           (suggestNameAt (suggesterFor cat s) 0)

spec ∷ Spec
spec = describe "world-name suggestions" $ do
    prodBytes ← runIO $ BS.readFile conceptCataloguePath
    let prodCat = either (error ∘ T.unpack ∘ catalogueErrorText) id
                         (parseCatalogue prodBytes)

    describe "language seed derived from the world seed" $ do
        -- Requirement 3, and the reviewer's injectivity clause: two
        -- different world seeds are two different languages, not merely
        -- probably-different ones. A collision would silently make two
        -- worlds share a language.
        it "is injective across the supported seed space" $ do
            let seeds = [0 .. 4095] ⧺ sampleSeeds
                           ⧺ [0xFFFFFF00 .. 0xFFFFFFFF] ∷ [Word64]
                langs = map (langSeedWord ∘ worldLanguageSeed) seeds
            length (nub langs) `shouldBe` length (nub seeds)

        it "is stable for a given world seed" $
            map (langSeedWord ∘ worldLanguageSeed) sampleSeeds
                `shouldBe` map (langSeedWord ∘ worldLanguageSeed) sampleSeeds

        -- Equivalent SPELLINGS of a seed are normalized to one number
        -- before reaching here (settingsTab.seedNumber / generation.lua
        -- both use tonumber(text, 16)), so this layer only has to agree
        -- that one number is one language.
        it "is not the world seed itself" $
            filter (\s → langSeedWord (worldLanguageSeed s) ≡ s) sampleSeeds
                `shouldBe` []

    describe "determinism" $ do
        it "offers the same sequence for the same seed" $
            forM_ sampleSeeds $ \s →
                suggestionsFor prodCat s 12 `shouldBe` suggestionsFor prodCat s 12

        it "renders both readings from one expression" $
            forM_ sampleSeeds $ \s → do
                let sgr = suggesterFor prodCat s
                forM_ [0 .. 7] $ \k → case suggestNameAt sgr k of
                    Left err → expectationFailure (T.unpack (suggestErrorText err))
                    Right sug → do
                        renderNative (nsuProfile sgr) (nsuRoots sgr) (nsExpr sug)
                            `shouldBe` Right (nsName sug)
                        renderGloss prodCat (nsExpr sug)
                            `shouldBe` Right (nsGloss sug)

        it "agrees with suggestionExprAt" $
            forM_ sampleSeeds $ \s → do
                let sgr = suggesterFor prodCat s
                forM_ [0 .. 7] $ \k →
                    either (const Nothing) (Just ∘ nsExpr) (suggestNameAt sgr k)
                        `shouldBe` suggestionExprAt sgr k

    describe "rerolling" $ do
        -- Requirement 2, the single most important behavior in the
        -- issue: press the dice repeatedly and the meaning changes
        -- while the language does not.
        it "changes the meaning on every adjacent reroll" $
            forM_ sampleSeeds $ \s → do
                let sugs = suggestionsFor prodCat s 40
                forM_ (zip sugs (drop 1 sugs)) $ \(a, b) → do
                    nsExpr a `shouldNotBe` nsExpr b
                    nsGloss a `shouldNotBe` nsGloss b

        it "keeps one language across the whole sequence" $
            forM_ sampleSeeds $ \s → do
                let sugs = suggestionsFor prodCat s 40
                    prov = provFor s
                map nsSeed sugs `shouldBe` map (const (lpSeed prov)) sugs
                map nsVersion sugs `shouldBe` map (const (lpVersion prov)) sugs

        -- The phonology is what a player HEARS staying put. Pinning the
        -- profile signature makes "same language" mean the whole style,
        -- not just a matching seed field.
        it "keeps one phonology across the whole sequence" $
            forM_ sampleSeeds $ \s → do
                let sgr = suggesterFor prodCat s
                case generateProfile currentGeneratorVersion
                                     (worldLanguageSeed s) of
                    Left err → expectationFailure
                        (T.unpack (generatorErrorText err))
                    Right prof → profileSignature (nsuProfile sgr)
                        `shouldBe` profileSignature prof

        it "reaches every name form over a long enough sequence" $ do
            let shapes = [ shapeOf (nsExpr sug)
                         | s ← sampleSeeds
                         , sug ← suggestionsFor prodCat s 40 ]
            sort (nub shapes) `shouldBe`
                ["bare", "modifier", "of_plural", "of_singular", "possessive"]

        it "never names a concept as its own modifier" $
            forM_ sampleSeeds $ \s →
                forM_ (suggestionsFor prodCat s 40) $ \sug →
                    slotsOf (nsExpr sug) `shouldSatisfy`
                        (\cs → length (nub cs) ≡ length cs)

    describe "different seeds" $ do
        it "give different languages" $ do
            let seeds = nub (take 24 sampleSeeds)
                langs = map (nsSeed ∘ firstSuggestion prodCat) seeds
            length (nub (map langSeedWord langs)) `shouldBe` length seeds

        -- Requirement 3's player-visible half: changing the seed offers
        -- a genuinely different name, not the same one respelled.
        it "give different first suggestions" $ do
            let firsts = map (nsName ∘ firstSuggestion prodCat)
                             (nub sampleSeeds)
            length (nub firsts) `shouldSatisfy` (≥ length firsts - 1)

    describe "rendered output" $ do
        it "stays inside the renderer's own length bounds" $
            forM_ sampleSeeds $ \s →
                forM_ (suggestionsFor prodCat s 20) $ \sug → do
                    T.length (nsName sug) `shouldSatisfy` (≥ 3)
                    T.length (nsName sug) `shouldSatisfy` (≤ 32)

        it "always supplies a nonempty gloss" $
            forM_ sampleSeeds $ \s →
                forM_ (suggestionsFor prodCat s 20) $ \sug →
                    nsGloss sug `shouldSatisfy` (not ∘ T.null)

        -- The World Name field admits exactly `outputInventory`
        -- (through world.generatedNameCharacters), so a suggestion that
        -- reached outside it would be a name the player could see but
        -- not retype (#1106 requirement 4). This is what ties the two
        -- ends of that claim together.
        it "uses only characters the name field admits" $ do
            let admitted = outputInventory
            forM_ sampleSeeds $ \s →
                forM_ (suggestionsFor prodCat s 20) $ \sug →
                    forM_ (T.unpack (nsName sug)) $ \c →
                        (nsName sug, c) `shouldSatisfy`
                            (\(_, ch) → ch `elem` admitted)

        -- Not vacuous: the shipped generator really does emit both a
        -- mark and a non-ASCII letter, which is why widening the field
        -- was necessary rather than cosmetic.
        it "reaches beyond plain ASCII letters in practice" $ do
            let names = [ nsName sug
                        | s ← sampleSeeds, sug ← suggestionsFor prodCat s 20 ]
                chars = concatMap T.unpack names
            filter (\c → c ≡ '\'' ∨ c ≡ '-') chars `shouldSatisfy` (not ∘ null)
            filter (> '\x007F') chars `shouldSatisfy` (not ∘ null)

    describe "failure reporting" $ do
        -- A version this build cannot construct is refused, never
        -- silently substituted with the current one — that would suggest
        -- names in a different language than the one being recorded.
        it "refuses an unconstructible generator version" $ do
            let bad = LanguageProvenance
                    { lpSeed = worldLanguageSeed 42
                    , lpVersion = GeneratorVersion 9999 }
            case mkNameSuggester prodCat bad of
                Right _  → expectationFailure "built a profile for version 9999"
                Left err → suggestErrorText err `shouldSatisfy`
                    T.isInfixOf "unsupported language-generator version"

        it "refuses a catalogue with no concepts" $ do
            let empty = Catalogue { catVersion = 1, catConcepts = mempty }
            case mkNameSuggester empty (provFor 42) of
                Right _  → expectationFailure "suggested from an empty catalogue"
                Left err → suggestErrorText err `shouldSatisfy`
                    T.isInfixOf "empty"

    -- Requirement 8: the dice button runs synchronously on the UI's own
    -- thread. Exactly one press per session may reach the filesystem —
    -- reading `data/language/concepts.yaml` on every press is what the
    -- cache exists to prevent, and a press that FAILS to read it is not
    -- exempt (review round 2).
    describe "the suggestion cache's step decision" $ do
        let step p c = suggestionStepLabel (suggestionStep p c)
            prov42   = provFor 42
            prov7    = provFor 7
            sgr42    = suggesterFor prodCat 42

        it "reads the catalogue only when nothing is cached" $
            step prov42 Nothing `shouldBe` "read"

        it "reuses a cached suggester for the same language" $
            step prov42 (Just (LanguageCache (Right prodCat)
                                             (Just (prov42, sgr42))))
                `shouldBe` "reuse"

        -- Editing the seed is a different language, so the suggester is
        -- rebuilt — but from the cached catalogue, never off disk.
        it "rebuilds from the cached catalogue for a different language" $
            step prov7 (Just (LanguageCache (Right prodCat)
                                            (Just (prov42, sgr42))))
                `shouldBe` "build"

        it "rebuilds without re-reading when no suggester was cached" $
            step prov42 (Just (LanguageCache (Right prodCat) Nothing))
                `shouldBe` "build"

        -- The round-2 blocker: a catalogue that would not load used to
        -- go uncached, so every later press retried the disk read.
        it "reports a cached catalogue failure without re-reading" $ do
            let broken = Just (LanguageCache (Left "no catalogue") Nothing)
            step prov42 broken `shouldBe` "failed"
            step prov7 broken `shouldBe` "failed"

        -- Round 3: caching that failure only helps if the read RETURNS
        -- one. loadCatalogue reports a file it parsed and rejected, but
        -- a missing or unreadable one throws out of BS.readFile — which
        -- would escape past the cache write and put the disk back on
        -- every press.
        it "turns an unreadable catalogue into a value, not an exception" $ do
            result ← readCatalogueForSuggestions
                        "data/language/does_not_exist.yaml"
            case result of
                Right _  → expectationFailure "read a nonexistent catalogue"
                Left msg → do
                    msg `shouldSatisfy` T.isInfixOf "does_not_exist.yaml"
                    msg `shouldSatisfy` T.isInfixOf "could not be loaded"

        it "reports a catalogue it could read but not validate" $ do
            result ← readCatalogueForSuggestions "data/units/acolyte.yaml"
            case result of
                Right _  → expectationFailure "validated a unit definition"
                Left msg → msg `shouldSatisfy` T.isInfixOf "could not be loaded"

        it "still reads the real catalogue" $ do
            result ← readCatalogueForSuggestions conceptCataloguePath
            fmap catVersion result `shouldBe` Right (catVersion prodCat)

-- | Which of the five shapes an expression took, as report text.
shapeOf ∷ NameExpr → String
shapeOf e = case e of
    Bare{}            → "bare"
    Modifier{}        → "modifier"
    Of _ Singular _   → "of_singular"
    Of _ Plural _     → "of_plural"
    Possessive{}      → "possessive"

-- | The concepts an expression names, so a slot repeat is visible.
slotsOf ∷ NameExpr → [ConceptId]
slotsOf e = case e of
    Bare c           → [c]
    Modifier m h     → [m, h]
    Of h _ c         → [h, c]
    Possessive o h   → [o, h]
