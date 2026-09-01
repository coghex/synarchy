-- | Version 2's admissible two-consonant onset relation (#1094):
--   relation generation and density, @CCV@ rendering through the
--   relation, and the consonant/vowel/dual-role @y@ behaviour.
module Test.Headless.Language.Generated.Onset
    ( spec
    ) where

import UPrelude
import Test.Hspec
import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Generated.Types
import Language.Generated.Onset
import Language.Generated.Profile
import Language.Generated.Root
import Language.Generated.Signature
import Test.Headless.Language.Generated.Support

-- | The one syllable shape with an in-syllable two-consonant onset —
--   the shape #1094 constrains.
ccvShape ∷ SyllableShape
ccvShape = SyllableShape [ConsonantSlot, ConsonantSlot, VowelSlot]

-- | Force every syllable of a profile through the @CCV@ path, so a
--   generated root exercises the REAL 'renderShape' onset selection on
--   every syllable rather than only where the profile's own shape draw
--   happened to land on @CCV@. Nothing else about the profile changes,
--   so the onsets produced must satisfy that same profile's exported
--   relation.
forceCCV ∷ Profile → Profile
forceCCV p = p { profSyllableShapes = [ccvShape] }

-- | The onset of each 3-character syllable in a root rendered by a
--   'forceCCV' profile.
syllableOnsets ∷ Text → [(Char, Char)]
syllableOnsets = go ∘ T.unpack
  where
    go (a : b : _ : rest) = (a, b) : go rest
    go _                  = []

spec ∷ Ctx → Spec
spec Ctx{..} = do
    -- #1094: version 2's admissible two-consonant onset relation.
    describe "admissible onsets (#1094 requirements 3, 4, 5, 7)" $ do
        it "is irreflexive, and every stored pair lies inside the \
           \profile's own consonant inventory" $ do
            let offenders =
                    [ (profSeed p, a, b)
                    | p ← v2Profiles
                    , (a, b) ← onsetPairs (profOnset p)
                    , a ≡ b
                      ∨ not (a `elem` profConsonants p)
                      ∨ not (b `elem` profConsonants p) ]
                reflexive =
                    [ (profSeed p, c)
                    | p ← v2Profiles, c ← profConsonants p
                    , admissibleOnset p c c ]
            offenders `shouldBe` []
            reflexive `shouldBe` []

        it "rejects any character outside the profile's consonant \
           \inventory, in either position" $ do
            let probes = ['a' .. 'z'] <> "AQ'-0 "
                offenders =
                    [ (profSeed p, x, c)
                    | p ← take 64 v2Profiles
                    , x ← probes
                    , not (consonantCapable p x)
                    , c ← profConsonants p
                    , admissibleOnset p x c ∨ admissibleOnset p c x ]
            offenders `shouldBe` []

        it "admits between 25% and 45% of each profile's n*(n-1) \
           \ordered pairs, and is never empty" $ do
            let offenders =
                    [ (profSeed p, onsetPairCount (profOnset p), onsetTotalPairs p)
                    | p ← v2Profiles
                    , not (onsetDensityOk p) ∨ onsetPairCount (profOnset p) ≡ 0 ]
            offenders `shouldBe` []
            -- A relation admitting every distinct pair would pass an
            -- irreflexivity test but defeat the whole issue, so pin the
            -- band's arithmetic itself rather than only its outcome.
            onsetDensityBounds 30 `shouldBe` (8, 13)
            onsetDensityBounds 132 `shouldBe` (33, 59)

        it "every profile offering CCV has a pair the renderer can \
           \select" $ do
            let offering = [ p | p ← v2Profiles
                           , ccvShape `elem` profSyllableShapes p ]
            offering `shouldSatisfy` not ∘ null
            filter (null ∘ onsetPairs ∘ profOnset) offering `shouldBe` []

        it "the same visible pair is admissible in some languages and \
           \inadmissible in others (cross-seed diversity)" $ do
            -- A relation keyed only on letters would give every
            -- language identical phonotactics and score zero here.
            let tally = M.fromListWith plus
                    [ ((a, b), (1 ∷ Int, if admissibleOnset p a b then 1 else 0 ∷ Int))
                    | p ← v2Profiles
                    , let inv = sort (nub (profConsonants p))
                    , a ← inv, b ← inv, a ≢ b ]
                plus (s1, k1) (s2, k2) = (s1 + s2, k1 + k2)
                qualifying = [ v | v@(shared, _) ← M.elems tally, shared ≥ 8 ]
                disagreeing = [ () | (shared, adm) ← qualifying
                              , adm > 0, adm < shared ]
            length qualifying `shouldSatisfy` (> 0)
            (2 * length disagreeing) `shouldSatisfy` (≥ length qualifying)

        it "participates in the profile signature" $ do
            let prof = buildProfileV2 (LangSeed 3)
            profileSignature (prof { profOnset = emptyOnsetRelation })
                `shouldNotBe` profileSignature prof

        it "version 1 constrains nothing, and the query is still total \
           \there" $ do
            -- #1092 keeps historical versions constructible, and L1c
            -- consumes this query without knowing which version built
            -- the profile — so a v1 profile must answer, not diverge.
            let offenders =
                    [ (s, a, b)
                    | s ← [0, 1, 42, 12345 ∷ Word64]
                    , let p = buildProfileV1 (LangSeed s)
                    , a ← profConsonants p, b ← profConsonants p
                    , admissibleOnset p a b ]
            offenders `shouldBe` []
            map (onsetPairs ∘ profOnset ∘ buildProfileV1 ∘ LangSeed)
                [0, 1, 42, 12345 ∷ Word64]
                `shouldBe` replicate 4 []

    describe "version-2 CCV rendering selects from the relation (#1094 requirement 5)" $ do
        it "every onset the real CCV rendering path produces is \
           \admissible under the exported relation" $ do
            -- Drives the production renderShape with every syllable
            -- forced through CCV, so this covers the actual selection
            -- code L1c's contract depends on, not a reimplementation.
            let ids = take 40 (conceptIds prodCat)
                rootsUnder s =
                    let forced = forceCCV (buildProfileV2 (LangSeed s))
                    in [ (forced, generateRoot forced c attempt)
                       | c ← ids, attempt ← [0 .. 2 ∷ Int] ]
                everyRoot = concatMap rootsUnder [0 .. 63 ∷ Word64]
                misChunked = [ r | (_, r) ← everyRoot
                             , T.length r `mod` 3 ≢ 0 ]
                offenders = [ (profSeed p, r, a, b)
                            | (p, r) ← everyRoot
                            , (a, b) ← syllableOnsets r
                            , not (admissibleOnset p a b) ]
            everyRoot `shouldSatisfy` not ∘ null
            -- Guards the 3-character chunking the assertion relies on.
            misChunked `shouldBe` []
            offenders `shouldBe` []

        it "makes identical-consonant onsets impossible" $ do
            let ids = take 40 (conceptIds prodCat)
                offenders =
                    [ (s, r, a)
                    | s ← [0 .. 63 ∷ Word64]
                    , let forced = forceCCV (buildProfileV2 (LangSeed s))
                    , c ← ids, attempt ← [0 .. 2 ∷ Int]
                    , let r = generateRoot forced c attempt
                    , (a, b) ← syllableOnsets r
                    , a ≡ b ]
            offenders `shouldBe` []

        it "no canonical version-2 name begins with an inadmissible or \
           \repeated two-consonant onset" $ do
            -- The same word-initial scoping tools/language_report.py
            -- gates, run against real (unforced) profiles.
            let checked =
                    [ (s, w, a, b)
                    | s ← [0 .. 63 ∷ Word64]
                    , let p = buildProfileV2 (LangSeed s)
                    , Right w ← nativeRenderingsV2 s
                    , (a, b) ← wordInitialOnsets p w ]
                offenders =
                    [ (s, w, a, b)
                    | (s, w, a, b) ← checked
                    , let p = buildProfileV2 (LangSeed s)
                    , a ≡ b ∨ not (admissibleOnset p a b) ]
            checked `shouldSatisfy` not ∘ null
            offenders `shouldBe` []

    describe "version-2 'y' roles (#1094 requirements 6, 7)" $ do
        it "assigns every profile exactly one of the three roles — \
           \never 'neither'" $ do
            filter (isNothing ∘ profileYRole) v2Profiles `shouldBe` []
            let mismatched =
                    [ profSeed p
                    | p ← v2Profiles
                    , let inCons = 'y' `elem` profConsonants p
                    , let inVow  = 'y' `elem` profVowels p
                    , case profileYRole p of
                        Just YConsonantOnly → not inCons ∨ inVow
                        Just YVowelOnly     → inCons ∨ not inVow
                        Just YBothRoles     → not (inCons ∧ inVow)
                        Nothing             → True ]
            mismatched `shouldBe` []

        it "seeds 0:255 include a profile in each of the three states" $
            forM_ [YConsonantOnly, YVowelOnly, YBothRoles] $ \r →
                v2ProfilesWithRole r `shouldSatisfy` not ∘ null

        it "surface-glyph capability follows the role" $ do
            forM_ (v2ProfilesWithRole YConsonantOnly) $ \p → do
                consonantCapable p 'y' `shouldBe` True
                vowelCapable p 'y' `shouldBe` False
            forM_ (v2ProfilesWithRole YVowelOnly) $ \p → do
                consonantCapable p 'y' `shouldBe` False
                vowelCapable p 'y' `shouldBe` True
            forM_ (v2ProfilesWithRole YBothRoles) $ \p → do
                consonantCapable p 'y' `shouldBe` True
                vowelCapable p 'y' `shouldBe` True

        it "a vowel-only 'y' is never consonant-capable, so no onset \
           \query involving it can succeed" $ do
            let offenders =
                    [ (profSeed p, c)
                    | p ← v2ProfilesWithRole YVowelOnly
                    , c ← profConsonants p
                    , admissibleOnset p 'y' c ∨ admissibleOnset p c 'y' ]
            offenders `shouldBe` []

        it "a dual-role 'y' still takes the consonant interpretation \
           \beside another consonant-capable glyph" $ do
            -- Requirement 7's surface-glyph semantics: L1c sees flat
            -- text, so a dual-role 'y' must be answerable as a cluster
            -- member. At least one such language really admits it.
            let admittingY =
                    [ profSeed p
                    | p ← v2ProfilesWithRole YBothRoles
                    , (a, b) ← onsetPairs (profOnset p)
                    , a ≡ 'y' ∨ b ≡ 'y' ]
            admittingY `shouldSatisfy` not ∘ null
