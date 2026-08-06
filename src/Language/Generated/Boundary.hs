{-# LANGUAGE Strict #-}
-- | Morpheme-boundary phonology (#1095): what happens where two
--   independently-generated pieces of a name meet.
--
--   Versions 1 and 2 concatenated every join raw — compound joins,
--   plural and possessive affixation, min-length top-up, and ordinary
--   syllable concatenation were all a bare @a \<\> b@. Nothing looked at
--   what sat on either side, so a root ending in @h@ met an @h@
--   possessive affix as @hohh@ and a language could emit a TRIPLE
--   (@Wunilunonnuwwwi@). Version 3 mediates every one of those joins.
--
--   #1095 requirement 1's three phenomena map onto 'BoundaryRule', one
--   of which each language draws for itself: assimilation across the
--   boundary is 'BoundaryHarmonic' (the inserted vowel copies the left
--   morpheme's own nucleus), cluster simplification is
--   'BoundarySimplifying', and plain epenthesis is 'BoundaryEpenthetic'.
--   Whichever rule a language holds, an identical segment pair is always
--   broken — that is the @Eytoc-hohh@ shape the issue names.
--
--   Three properties shape the whole module:
--
--   * __One notion of legality.__ Whether a boundary consonant cluster
--     is admissible is decided by #1094's own
--     'Language.Generated.Onset.admissibleOnset' relation
--     (#1095 requirement 2), never by a second rule invented here — so
--     a syllable onset and a boundary repair cannot disagree about what
--     a given language allows.
--   * __The left morpheme is never modified.__ Every repair is an
--     insertion or an edit to the RIGHT morpheme's initial segment. That
--     is what keeps #710 requirement 9's "the bare root is always a
--     prefix of its marked form" true after boundary phonology exists,
--     at the plural and possessive sites where it is a compatibility
--     contract.
--   * __No triple can survive a join.__ Given two triple-free pieces,
--     every branch below produces a triple-free result:
--
--       - left unchanged: the branch is only taken when the two facing
--         segments differ, so no run spans the junction;
--       - insertion of @s@: @s@ is chosen to differ from BOTH facing
--         segments, so none of @l-1 l s@, @l s r@, @s r r+1@ can be a
--         run;
--       - simplification: it is only taken when the post-deletion
--         junction itself needs no repair, which means the facing
--         segments differ there too.
--
--     Roots fold 'joinSyllables' over their syllables and names fold
--     'joinMorphemes' over their roots and affixes, so the property is
--     inductive over the whole rendering. Comparison is case-insensitive
--     throughout, which is why capitalizing the first letter last
--     ('Language.Generated.Render.capitalizeWord') cannot introduce an
--     @Aaa@ the lowercase pass would have missed.
--
--   Everything here is a pure function of (profile, the two pieces):
--   deterministic, allocation-light, and free of search, rejection
--   sampling, or backtracking (#1095 requirement 7). The only lookahead
--   is cluster simplification's single check of the boundary it would
--   leave behind.
module Language.Generated.Boundary
    ( buildBoundaryPolicy
    , joinMorphemes
    , joinSyllables
    , boundaryNeedsRepair
    , hasTripleRun
    ) where

import UPrelude
import Data.Char (toLower, isAsciiUpper, isAsciiLower)
import qualified Data.Text as T
import Language.Generated.Types
import Language.Generated.Hash
import Language.Generated.Onset (admissibleOnset, consonantCapable, vowelCapable)

-- | Build one language's boundary policy from its own inventories.
--   Deterministic in @(seed, inventories)@ alone, so repeated
--   construction of the same @(version, seed)@ is byte-identical.
--
--   A profile too small to supply two distinct linking consonants and
--   one epenthetic vowel gets 'BoundaryUnmediated' rather than a
--   degenerate policy: the distinctness of the two linkers is load
--   bearing (see 'joinMorphemes'), and every version-3 profile clears
--   the floor comfortably (at least six consonants and three vowels).
buildBoundaryPolicy ∷ Word64 → [Char] → [Char] → BoundaryPolicy
buildBoundaryPolicy seed consonants vowels
    | nCons < 2 ∨ nVow < 1 = BoundaryUnmediated
    | otherwise = BoundaryMediated BoundaryRepair
        { brRule       = rule
        , brEpenthetic = vowels !! pickIndex (draw seed 1) nVow
        , brLinker     = consonants !! i
        , brLinkerAlt  = consonants !! j
        }
  where
    nCons = length consonants
    nVow  = length vowels

    rule = case wordInRange (draw seed 0) 0 2 of
        0 → BoundaryEpenthetic
        1 → BoundaryHarmonic
        _ → BoundarySimplifying

    -- j is drawn in the (n-1) positions AFTER i, wrapped: distinct from
    -- i by construction, with no rejection sampling.
    i = pickIndex (draw seed 2) nCons
    j = (i + 1 + pickIndex (draw seed 3) (nCons - 1)) `mod` nCons

-- | Join two morphemes — the four #1095 sites: a compact compound join,
--   plural affixation, possessive affixation, and min-length top-up's
--   appended material.
--
--   A boundary is mediated only when it needs to be (#1095's reviewed
--   spec: an admissible boundary may remain unchanged): the facing
--   segments are the same letter, or they form a two-consonant cluster
--   this language's own onset relation does not admit. Everything else
--   passes through untouched, which is what keeps doubled letters that
--   were already legal — including a double formed by two different
--   morphemes whose facing segments differ — exactly as they were.
joinMorphemes ∷ Profile → Text → Text → Text
joinMorphemes = mediate MorphemeScope

-- | Join two already-rendered stretches of syllables inside ONE root.
--
--   Syllable concatenation is the root's own shape vocabulary, not a
--   morpheme join, so it is NOT re-mediated for cluster admissibility —
--   doing so would rewrite every root the shapes produce. It is
--   mediated only where the junction would produce a triple, which is
--   what makes the no-triple guarantee hold for a bare root rather than
--   only for compounds and affixed forms.
joinSyllables ∷ Profile → Text → Text → Text
joinSyllables = mediate SyllableScope

-- | Whether a triple-letter run is present: three contiguous ASCII
--   letters that are the same letter ignoring case. Punctuation
--   interrupts a run, so a hyphen join's @a-a@ and an apostrophe
--   affix's @h'h@ are not runs; case is ignored, so a capitalized
--   @Aaa@ is.
hasTripleRun ∷ Text → Bool
hasTripleRun = go ∘ T.unpack
  where
    go (a : b : c : rest)
        | isLetter a ∧ sameLetter a b ∧ sameLetter b c = True
        | otherwise                                    = go (b : c : rest)
    go _ = False
    isLetter ch = isAsciiUpper ch ∨ isAsciiLower ch

-- Which kind of junction is being mediated. Both share the repair
-- machinery; they differ only in what counts as needing repair, and in
-- whether cluster simplification is on the table.
data BoundaryScope = MorphemeScope | SyllableScope
    deriving (Eq)

mediate ∷ BoundaryScope → Profile → Text → Text → Text
mediate scope prof left right
    | T.null left ∨ T.null right = left <> right
    | otherwise = case profBoundary prof of
        BoundaryUnmediated → left <> right
        BoundaryMediated rep
            | needsRepair scope prof left right → repair scope prof rep left right
            | otherwise                          → left <> right

-- | Whether the boundary between two morphemes needs mediating, at
--   'MorphemeScope'. Exported for the report/test layers, which assert
--   the property this decides rather than re-deriving it.
--
--   False for an empty side (there is no boundary) and for any profile
--   whose version predates boundary phonology.
boundaryNeedsRepair ∷ Profile → Text → Text → Bool
boundaryNeedsRepair prof left right
    | T.null left ∨ T.null right = False
    | otherwise = case profBoundary prof of
        BoundaryUnmediated → False
        BoundaryMediated _ → needsRepair MorphemeScope prof left right

needsRepair ∷ BoundaryScope → Profile → Text → Text → Bool
needsRepair scope prof left right = case scope of
    -- A run already two long on either side, about to be extended by an
    -- identical facing segment, is the ONLY thing syllable joins repair.
    SyllableScope → sameLetter l r
        ∧ (maybe False (sameLetter l) (penultimate left)
           ∨ maybe False (sameLetter r) (secondChar right))
    -- Identical segments are already inadmissible under #1094's
    -- irreflexive relation, so the first disjunct is not a second notion
    -- of legality — it is the case that relation answers for a pair the
    -- cluster test below would not even reach (an identical VOWEL pair).
    MorphemeScope → sameLetter l r
        ∨ (bothConsonantal ∧ not (admissibleOnset prof l r))
  where
    l = T.last left
    r = T.head right
    -- "Unambiguously a consonant": a dual-role 'y' (#1094 requirement 6)
    -- sits in both inventories, and a flat rendered root carries no slot
    -- provenance, so a pair involving one is not treated as a cluster —
    -- the same scoping the onset gates already use.
    bothConsonantal = consonantOnly prof l ∧ consonantOnly prof r

-- | Apply the language's repair. Cluster simplification is a MORPHEME
--   repair only: a syllable join is being mediated purely to break a
--   run, and deleting a segment there would drop material the profile's
--   own shape vocabulary put in the root. Every other case, and every
--   syllable-scope case, inserts.
repair ∷ BoundaryScope → Profile → BoundaryRepair → Text → Text → Text
repair scope prof rep left right
    | scope ≡ MorphemeScope
    , brRule rep ≡ BoundarySimplifying
    , Just trimmed ← simplified prof left right
    = left <> trimmed
    | otherwise
    = left <> T.singleton (linkerFor prof rep left right) <> right

-- | Cluster simplification: the right morpheme without its initial
--   segment, when that leaves it NONEMPTY (a one-letter grammatical
--   affix must survive repair) and actually resolves the boundary. A
--   deletion that merely moved the problem — or created a new run by
--   exposing a matching segment — is refused, and the caller falls back
--   to epenthesis.
simplified ∷ Profile → Text → Text → Maybe Text
simplified prof left right
    | T.length right ≥ 2
    , let trimmed = T.tail right
    , not (needsRepair MorphemeScope prof left trimmed)
    = Just trimmed
    | otherwise
    = Nothing

-- | The segment inserted between two morphemes.
--
--   A consonant cluster is broken by a VOWEL and a vocalic boundary by a
--   CONSONANT, which is what makes the inserted segment differ from both
--   sides in every case:
--
--   * the cluster branch is only reached when both facing segments are
--     unambiguously consonants, and every vowel this can insert is
--     vowel-capable, so it cannot equal either of them;
--   * the vocalic branch is only reached when the two facing segments
--     are the SAME letter (a differing pair is either a cluster, handled
--     above, or admissible and never repaired), so at most one of the
--     two distinct linking consonants can collide with it and the other
--     is always available.
linkerFor ∷ Profile → BoundaryRepair → Text → Text → Char
linkerFor prof rep left right
    | consonantOnly prof l ∧ consonantOnly prof r = vowelLinker
    | otherwise                                    = consonantLinker
  where
    l = T.last left
    r = T.head right

    vowelLinker = case brRule rep of
        BoundaryHarmonic → fromMaybe (brEpenthetic rep) harmonicVowel
        _                → brEpenthetic rep

    -- Assimilation across the boundary: copy the left morpheme's own
    -- final nucleus, i.e. the segment its closing consonant follows.
    harmonicVowel = do
        c ← penultimate left
        guard (vowelCapable prof c)
        pure c

    consonantLinker
        | not (sameLetter (brLinker rep) l ∨ sameLetter (brLinker rep) r)
        = brLinker rep
        | otherwise
        = brLinkerAlt rep

consonantOnly ∷ Profile → Char → Bool
consonantOnly prof c = consonantCapable prof c ∧ not (vowelCapable prof c)

sameLetter ∷ Char → Char → Bool
sameLetter a b = toLower a ≡ toLower b

-- | The character before a text's last, if any.
penultimate ∷ Text → Maybe Char
penultimate t
    | T.length t ≥ 2 = Just (T.index t (T.length t - 2))
    | otherwise      = Nothing

-- | The character after a text's first, if any.
secondChar ∷ Text → Maybe Char
secondChar t
    | T.length t ≥ 2 = Just (T.index t 1)
    | otherwise      = Nothing
