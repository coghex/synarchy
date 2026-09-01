-- | Version 3's morpheme-boundary phonology (#1095): the repair
--   rules, the historical raw-join behaviour of versions 1 and 2,
--   triple-run prevention, the minimum-length top-up, grammatical
--   marking and separators, and per-language repair state.
module Test.Headless.Language.Generated.Boundary
    ( spec
    ) where

import UPrelude
import Test.Hspec
import Data.List (nub)
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Generated.Types
import Language.Generated.Onset
import Language.Generated.Boundary
import Language.Generated.Profile
import Language.Generated.Root
import Language.Generated.Render
import Language.Generated.Signature
import Test.Headless.Language.Generated.Support

-- | A fixture whose @y@ is BOTH a consonant and a vowel and whose first
--   linking consonant is that same @y@ — the one shape in which the
--   primary linker would itself repeat the segment it is separating, so
--   the distinct alternative has to take over.
dualRoleFixture ∷ Profile
dualRoleFixture = (boundaryFixture BoundaryEpenthetic)
    { profConsonants = "byk"
    , profVowels     = "ay"
    , profOnset      = emptyOnsetRelation
    , profBoundary   = BoundaryMediated BoundaryRepair
        { brRule       = BoundaryEpenthetic
        , brEpenthetic = 'a'
        , brLinker     = 'y'
        , brLinkerAlt  = 'k'
        }
    }

-- | A version-3 profile whose ONSET relation is empty, so @CCV@ falls
--   back to #1094's historical independent-draw path and a syllable can
--   really come out as @bba@. Beside @CVC@ — which ends in a consonant —
--   that puts a genuine @b|bb@ junction within reach, so ordinary
--   multi-syllable root construction really can produce a triple: the
--   case #1095's reviewed spec requires covering beyond the four
--   morpheme sites. Only two consonants, so identical draws are common.
tripleProneRoot ∷ BoundaryPolicy → Profile
tripleProneRoot policy = (boundaryFixture BoundaryEpenthetic)
    { profConsonants     = "bk"
    , profVowels         = "ao"
    , profSyllableShapes =
        [ SyllableShape [ConsonantSlot, VowelSlot, ConsonantSlot]
        , SyllableShape [ConsonantSlot, ConsonantSlot, VowelSlot] ]
    , profMinSyllables   = 3
    , profMaxSyllables   = 3
    , profOnset          = emptyOnsetRelation
    , profBoundary       = policy
    }

-- | A version-3 profile every one of whose raw roots is exactly two
--   characters, so EVERY root goes through 'ensureMinLength''s top-up —
--   the fourth of #1095's named boundaries. Both a vowel-final @VC@ and
--   a consonant-initial @CV@ syllable are reachable, so the top-up
--   really can present the existing text with an inadmissible consonant
--   cluster rather than only with a potential triple.
topUpFixture ∷ BoundaryPolicy → Profile
topUpFixture policy = (boundaryFixture BoundaryEpenthetic)
    { profSyllableShapes = [ SyllableShape [VowelSlot, ConsonantSlot]
                           , SyllableShape [ConsonantSlot, VowelSlot] ]
    , profMinSyllables   = 1
    , profMaxSyllables   = 1
    , profBoundary       = policy
    }

-- | Whether the two glyphs at a root's top-up junction (the raw root is
--   two characters, so index 1 meets index 2) form a two-consonant
--   cluster this profile's own relation rejects.
illegalTopUpCluster ∷ Profile → Text → Bool
illegalTopUpCluster prof r = case T.unpack r of
    (_ : a : b : _) →
        a ≢ b
        ∧ consonantCapable prof a ∧ consonantCapable prof b
        ∧ not (vowelCapable prof a) ∧ not (vowelCapable prof b)
        ∧ not (admissibleOnset prof a b)
    _ → False

spec ∷ Ctx → Spec
spec Ctx{..} = do
    -- #1095: version 3's morpheme-boundary phonology.
    describe "boundary phonology (#1095 requirements 1, 2, 4, 5, 7)" $ do
        it "leaves a boundary its own onset relation admits untouched" $ do
            -- The reviewed spec: an admissible boundary MAY remain
            -- unchanged, and only the triple-run invariant can force a
            -- change. 'bh' is admissible in the fixture.
            forM_ [BoundaryEpenthetic, BoundaryHarmonic, BoundarySimplifying] $ \rule →
                joinMorphemes (boundaryFixture rule) "kab" "ha"
                    `shouldBe` "kabha"

        it "breaks an identical segment pair with the language's own \
           \epenthetic vowel" $
            -- The 'Eytoc-hohh' shape: a root ending in 'h' meeting the
            -- 'h' possessive affix. The root stays a prefix and the
            -- one-letter mark survives.
            joinMorphemes (boundaryFixture BoundaryEpenthetic) "hoh" "h"
                `shouldBe` "hohah"

        it "assimilates the inserted vowel to the left morpheme's own \
           \nucleus under the harmonic rule" $ do
            joinMorphemes (boundaryFixture BoundaryHarmonic) "hoh" "h"
                `shouldBe` "hohoh"
            -- Same left shape, different nucleus: the copied vowel
            -- really tracks the left morpheme rather than being fixed.
            joinMorphemes (boundaryFixture BoundaryHarmonic) "heh" "h"
                `shouldBe` "heheh"
            -- And it differs from the fixed-epenthesis language's answer
            -- for the identical input, so the rule is per-language.
            joinMorphemes (boundaryFixture BoundaryEpenthetic) "heh" "h"
                `shouldBe` "hehah"

        it "simplifies an inadmissible cluster by dropping the right \
           \morpheme's initial segment" $
            -- 'bs' is not admissible in the fixture; trimming leaves
            -- 'a', and 'b'/'a' needs no repair.
            joinMorphemes (boundaryFixture BoundarySimplifying) "keb" "sa"
                `shouldBe` "keba"

        it "falls back to epenthesis rather than erasing a one-letter \
           \grammatical mark" $
            -- The simplifying rule cannot delete the whole affix, so the
            -- mark survives and the boundary is broken instead.
            joinMorphemes (boundaryFixture BoundarySimplifying) "hoh" "h"
                `shouldBe` "hohah"

        it "breaks an identical VOWEL pair with a linking consonant, \
           \not another vowel" $
            joinMorphemes (boundaryFixture BoundaryEpenthetic) "ka" "ab"
                `shouldBe` "kakab"

        it "uses the alternative linker when the primary one would \
           \itself repeat the segment it separates" $
            -- A dual-role 'y' takes the vowel interpretation here, and
            -- the primary linker IS 'y' — inserting it would build the
            -- very triple the repair exists to prevent.
            joinMorphemes dualRoleFixture "sy" "yt" `shouldBe` "sykyt"

        it "repairs an inadmissible cluster in every language, by \
           \whichever rule that language chose" $ do
            -- One input, three languages, three distinct answers: the
            -- mediation is per-language rather than universal.
            let joins = [ joinMorphemes (boundaryFixture r) "keb" "sa"
                        | r ← [BoundaryEpenthetic, BoundaryHarmonic, BoundarySimplifying] ]
            joins `shouldBe` ["kebasa", "kebesa", "keba"]
            length (nub joins) `shouldBe` 3

        it "never modifies the left morpheme, so a bare stem stays a \
           \prefix of every repaired join" $ do
            let lefts  = ["hoh", "keb", "ka", "kabb", "kess", "bo"]
                rights = ["h", "s", "ab", "bo", "sa", "ha", "ok"]
                offenders =
                    [ (rule, l, r, joined)
                    | rule ← [BoundaryEpenthetic, BoundaryHarmonic, BoundarySimplifying]
                    , l ← lefts, r ← rights
                    , let joined = joinMorphemes (boundaryFixture rule) l r
                    , not (l `T.isPrefixOf` joined)
                      ∨ T.length joined ≤ T.length l ]
            offenders `shouldBe` []

        it "preserves a doubled letter that lies wholly inside either \
           \morpheme, repaired boundary or not" $ do
            -- An admissible boundary: nothing is touched at all.
            joinMorphemes (boundaryFixture BoundaryEpenthetic) "kobb" "ha"
                `shouldBe` "kobbha"
            -- A REPAIRED boundary: the repair breaks the run the join
            -- would have created without disturbing the 'bb' the left
            -- morpheme already carried.
            joinMorphemes (boundaryFixture BoundaryEpenthetic) "kabb" "bo"
                `shouldBe` "kabbabo"
            forM_ [BoundaryEpenthetic, BoundaryHarmonic, BoundarySimplifying] $ \rule →
                joinMorphemes (boundaryFixture rule) "kobb" "ha"
                    `shouldSatisfy` T.isInfixOf "bb"

        it "mediates a syllable join only where a triple would form" $ do
            let prof = boundaryFixture BoundaryEpenthetic
            -- A plain double across a syllable join is ordinary
            -- orthography and must survive untouched.
            joinSyllables prof "ab" "ba" `shouldBe` "abba"
            -- A THIRD identical segment is, from either side of the
            -- junction.
            joinSyllables prof "abb" "ba" `shouldBe` "abbaba"
            joinSyllables prof "ab" "bba" `shouldBe` "ababba"

        it "recognizes a triple-letter run case-insensitively, with \
           \punctuation interrupting it" $ do
            map hasTripleRun ["aaa", "Aaa", "aAa", "kaaan", "zoccce"]
                `shouldBe` replicate 5 True
            -- A hyphen join's a-a and an apostrophe affix's h'h are not
            -- contiguous letters, and a double is not a triple.
            map hasTripleRun ["a-aa", "aa-a", "h'hh", "abba", "kobbha", ""]
                `shouldBe` replicate 6 False

        it "reports a boundary as needing repair exactly where the \
           \shared admissibility relation says so" $ do
            let prof = boundaryFixture BoundaryEpenthetic
            -- Admissible cluster: no repair. Inadmissible: repair.
            boundaryNeedsRepair prof "kab" "ha" `shouldBe` False
            boundaryNeedsRepair prof "keb" "sa" `shouldBe` True
            boundaryNeedsRepair prof "hoh" "h"  `shouldBe` True
            -- An empty side is not a boundary, and a historical profile
            -- has no boundary phonology to apply.
            boundaryNeedsRepair prof "" "ha" `shouldBe` False
            boundaryNeedsRepair (buildProfileV2 (LangSeed 42)) "hoh" "h"
                `shouldBe` False

    describe "versions 1 and 2 keep joining morphemes raw (#1095)" $
        it "carries no boundary policy, so historical output is \
           \byte-identical" $ do
            let historical = [ buildProfileV1 (LangSeed s) | s ← [0 .. 63] ]
                          <> [ buildProfileV2 (LangSeed s) | s ← [0 .. 63] ]
            filter ((≢ BoundaryUnmediated) ∘ profBoundary) historical
                `shouldBe` []
            filter ((≡ BoundaryUnmediated) ∘ profBoundary) v3Profiles
                `shouldBe` []

    describe "no triple-letter run survives any join (#1095 requirement 3)" $ do
        it "holds for every canonical version-3 name across many seeds, \
           \covering all four join sites" $ do
            let named = [ (s, w) | s ← [0 .. 127 ∷ Word64]
                        , Right w ← nativeRenderingsV3 s ]
                offenders = [ (s, w) | (s, w) ← named, hasTripleRun w ]
            -- The sample is only meaningful if it actually reaches every
            -- site: both compound join styles, an apostrophe-bearing
            -- possessive, and a plain-letter affix all appear.
            named `shouldSatisfy` ((≥ 600) ∘ length)
            named `shouldSatisfy` any (T.isInfixOf "-" ∘ snd)
            named `shouldSatisfy` any (T.isInfixOf "'" ∘ snd)
            named `shouldSatisfy` any (not ∘ T.isInfixOf "-" ∘ snd)
            offenders `shouldBe` []

        it "holds for bare roots built by ordinary syllable \
           \concatenation and min-length top-up" $ do
            let ids = take 60 (conceptIds prodCat)
                rootsOf p = [ generateRoot p c attempt
                            | c ← ids, attempt ← [0 .. 2 ∷ Int] ]
                everyRoot = [ (s, r) | s ← [0 .. 63 ∷ Word64]
                            , r ← rootsOf (buildProfileV3 (LangSeed s)) ]
            everyRoot `shouldSatisfy` not ∘ null
            filter (hasTripleRun ∘ snd) everyRoot `shouldBe` []

        it "holds even for a profile whose syllables really can produce \
           \one — the mediation, not the shape vocabulary, is what \
           \prevents it" $ do
            -- An empty onset relation puts CCV back on #1094's
            -- independent-draw path, so a 'bba' syllable beside a 'b'
            -- coda is reachable. The UNMEDIATED twin proves the fixture
            -- is genuinely adversarial rather than vacuously clean.
            let ids = take 60 (conceptIds prodCat)
                rootsUnder policy =
                    [ generateRoot (tripleProneRoot policy) c attempt
                    | c ← ids, attempt ← [0 .. 4 ∷ Int] ]
                raw = rootsUnder BoundaryUnmediated
                mediated = rootsUnder (profBoundary
                                        (boundaryFixture BoundaryEpenthetic))
            filter hasTripleRun raw `shouldSatisfy` not ∘ null
            filter hasTripleRun mediated `shouldBe` []

    describe "min-length top-up is a full morpheme boundary (#1095)" $ do
        it "repairs an inadmissible cluster there, not merely a triple" $ do
            -- The top-up is one of the issue's four NAMED sites, so it
            -- consults the admissibility relation like the affix and
            -- compound joins do — the root's own interior syllable joins
            -- are the only place the weaker triple-only guard applies.
            -- The unmediated twin proves the fixture really presents
            -- illegal clusters rather than passing vacuously.
            let ids = take 60 (conceptIds prodCat)
                rootsOf p = [ generateRoot p c attempt
                            | c ← ids, attempt ← [0 .. 4 ∷ Int] ]
                rawProf = topUpFixture BoundaryUnmediated
            filter (illegalTopUpCluster rawProf) (rootsOf rawProf)
                `shouldSatisfy` not ∘ null
            forM_ [BoundaryEpenthetic, BoundaryHarmonic, BoundarySimplifying] $ \rule → do
                let prof = topUpFixture (profBoundary (boundaryFixture rule))
                    roots = rootsOf prof
                roots `shouldSatisfy` not ∘ null
                filter (illegalTopUpCluster prof) roots `shouldBe` []
                filter ((< minNativeWordLength) ∘ T.length) roots `shouldBe` []
                filter hasTripleRun roots `shouldBe` []

    describe "grammatical marking survives boundary repair (#1095)" $ do
        it "keeps the bare root a prefix and the mark nonempty, for \
           \every version-3 language" $ do
            let stems = ["hoh", "karad", "sess", "bo", "ky"]
                offenders =
                    [ (profSeed p, stem, marked)
                    | p ← take 128 v3Profiles, stem ← stems
                    , marked ← [applyPluralMark p stem, applyPossessiveMark p stem]
                    , not (stem `T.isPrefixOf` marked)
                      ∨ T.length marked ≤ T.length stem
                      ∨ hasTripleRun marked ]
            offenders `shouldBe` []

        it "never leaves a root-final segment touching an identical \
           \affix-initial one" $ do
            -- The 'Eytoc-hohh' acceptance shape, built deliberately for
            -- every language's OWN real affixes rather than a fixture's:
            -- a stem ending in exactly the letter that affix starts with.
            -- An apostrophe-leading possessive is excluded because its
            -- own separator already keeps the letters apart.
            let checked =
                    [ (profSeed p, a0, stem, mark p stem)
                    | p ← take 128 v3Profiles
                    , (affix, mark) ← [ (plmAffix (profPlural p), applyPluralMark)
                                      , (pmAffix (profPossessive p), applyPossessiveMark) ]
                    , Just (a0, _) ← [T.uncons affix]
                    , a0 ≢ '\''
                    -- A filler distinct from a0, so the stem carries no
                    -- double of its own.
                    , filler ← take 1 [ c | c ← profConsonants p, c ≢ a0 ]
                    , let stem = T.pack [filler, a0] ]
                -- The exact defect: whatever follows the intact stem must
                -- not repeat its final letter. Testing for the doubled
                -- pair ANYWHERE would instead flag an affix like "yy"
                -- that carries its own legal double (a dual-role 'y' is
                -- in both inventories, so genAffix can draw it twice).
                offenders = [ e | e@(_, a0, stem, marked) ← checked
                            , T.take 1 (T.drop (T.length stem) marked)
                                ≡ T.singleton a0 ]
            checked `shouldSatisfy` ((≥ 128) ∘ length)
            offenders `shouldBe` []

    describe "separators survive boundary phonology (#1095 requirement 5)" $ do
        it "a hyphen-joining language still emits exactly one hyphen per \
           \compound, and never a doubled one" $ do
            let hyphenated =
                    [ w | p ← take 128 v3Profiles, profJoin p ≡ JoinHyphen
                        , Right w ← [renderNative p (rootsFor p)
                                                 (Modifier (cid "ASH") (cid "LAND"))] ]
            hyphenated `shouldSatisfy` not ∘ null
            filter ((≢ 1) ∘ T.count "-") hyphenated `shouldBe` []

        it "an apostrophe-bearing possessive affix keeps its apostrophe, \
           \exactly once and never leading or trailing" $ do
            let apostrophe =
                    [ (profSeed p, applyPossessiveMark p "karad")
                    | p ← take 128 v3Profiles
                    , "'" `T.isPrefixOf` pmAffix (profPossessive p) ]
            apostrophe `shouldSatisfy` not ∘ null
            filter (\(_, w) → T.count "'" w ≢ 1) apostrophe `shouldBe` []
            filter (\(_, w) → "'" `T.isPrefixOf` w ∨ "'" `T.isSuffixOf` w)
                   apostrophe `shouldBe` []

    describe "boundary phonology is per-language style state (#1095)" $ do
        it "seeds 0:255 use all three repair rules" $ do
            let ruleOf p = case profBoundary p of
                    BoundaryUnmediated      → Nothing
                    BoundaryMediated rep    → Just (brRule rep)
            forM_ [BoundaryEpenthetic, BoundaryHarmonic, BoundarySimplifying] $ \r →
                filter ((≡ Just r) ∘ ruleOf) v3Profiles
                    `shouldSatisfy` not ∘ null

        it "draws its segments from the profile's own inventories, with \
           \two DISTINCT linking consonants" $ do
            let offenders =
                    [ profSeed p
                    | p ← v3Profiles
                    , BoundaryMediated rep ← [profBoundary p]
                    , not (brEpenthetic rep `elem` profVowels p)
                      ∨ not (brLinker rep `elem` profConsonants p)
                      ∨ not (brLinkerAlt rep `elem` profConsonants p)
                      ∨ brLinker rep ≡ brLinkerAlt rep ]
            offenders `shouldBe` []

        it "participates in the profile signature" $ do
            let prof = buildProfileV3 (LangSeed 3)
            profileSignature (prof { profBoundary = BoundaryUnmediated })
                `shouldNotBe` profileSignature prof
            case profBoundary prof of
                BoundaryUnmediated   → expectationFailure "expected a policy"
                BoundaryMediated rep → do
                    let other = rep { brRule = if brRule rep ≡ BoundaryEpenthetic
                                                then BoundaryHarmonic
                                                else BoundaryEpenthetic }
                    profileSignature (prof { profBoundary = BoundaryMediated other })
                        `shouldNotBe` profileSignature prof

        it "keeps version 3's non-boundary style identical to version \
           \2's for the same seed" $ do
            -- The boundary draw is APPENDED at a fresh step index, so
            -- version 3 differs from version 2 in exactly one field —
            -- which is what makes its goldens attributable to boundary
            -- phonology rather than to a reshuffled profile.
            let stripped p = p { profVersion = GeneratorVersion 0
                               , profBoundary = BoundaryUnmediated }
                offenders =
                    [ s | s ← [0 .. 127 ∷ Word64]
                        , stripped (buildProfileV3 (LangSeed s))
                            ≢ stripped (buildProfileV2 (LangSeed s)) ]
            offenders `shouldBe` []

    describe "boundary phonology is deterministic (#1095 requirement 7)" $ do
        it "draws a PINNED policy for each of four fixed seeds" $ do
            -- The draw itself, against values fixed at authoring time:
            -- a generator that consistently picked a different rule or
            -- different segments satisfies any same-input comparison
            -- and fails these. The NAMES these four languages render
            -- are pinned separately, in the version-3 golden block
            -- below.
            let policyOf s = profBoundary (buildProfileV3 (LangSeed s))
            policyOf 0 `shouldBe` BoundaryMediated BoundaryRepair
                { brRule = BoundaryEpenthetic, brEpenthetic = 'o'
                , brLinker = 'b', brLinkerAlt = 'c' }
            policyOf 1 `shouldBe` BoundaryMediated BoundaryRepair
                { brRule = BoundaryHarmonic, brEpenthetic = 'a'
                , brLinker = 'k', brLinkerAlt = 'g' }
            policyOf 42 `shouldBe` BoundaryMediated BoundaryRepair
                { brRule = BoundaryHarmonic, brEpenthetic = 'i'
                , brLinker = 'y', brLinkerAlt = 't' }
            policyOf (12345 ∷ Word64) `shouldBe` BoundaryMediated BoundaryRepair
                { brRule = BoundaryHarmonic, brEpenthetic = 'i'
                , brLinker = 'r', brLinkerAlt = 'y' }

        it "repairs a boundary as a pure function of (profile, the two \
           \pieces), pinned across all three rules" $ do
            -- The SAME two pieces through three languages that drew
            -- three different rules. The repair follows the profile, so
            -- the three results differ from one another, and each is a
            -- fixed value rather than a second call to itself.
            let epenthetic  = buildProfileV3 (LangSeed 0)
                harmonic    = buildProfileV3 (LangSeed 9)
                simplifying = buildProfileV3 (LangSeed 3)
                profiles    = [epenthetic, harmonic, simplifying]
            map (boundaryRuleText ∘ profBoundary) profiles
                `shouldBe` ["epenthetic", "harmonic", "simplifying"]
            map (\p → (joinMorphemes p "hoh" "h", joinSyllables p "abb" "ba"))
                profiles
                `shouldBe` [ ("hohoh", "abboba")
                           , ("hohzh", "abbzba")
                           , ("hohgh", "abbgba") ]
