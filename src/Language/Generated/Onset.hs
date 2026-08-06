{-# LANGUAGE Strict #-}
-- | Two-consonant onset admissibility (#1094): which ordered consonant
--   pairs a generated language allows at the start of a syllable.
--
--   Version 1 drew both consonants of a @CCV@ syllable independently
--   and uniformly, so repeated and otherwise difficult onsets
--   (@Cci@, @Llo@, @Fme@, @Vpe@) were exactly as likely as any other
--   pair. Version 2 gives each profile its OWN admissibility relation
--   and renders @CCV@ by selecting a pair from it directly — never by
--   drawing two consonants and hoping, never by rejection sampling or
--   retry (#1094 requirement 5).
--
--   Construction is a generated sonority/rank relation. Each profile
--   shuffles its own consonant inventory into a private sonority
--   ordering, and admits the pairs whose sonority RISES the most, up to
--   a profile-drawn count inside requirement 4's 25-45% density band.
--   Two consequences fall straight out of that shape and are what make
--   the relation worth having:
--
--   * The band's ceiling (45%) sits below the fraction of ordered pairs
--     that rise at all (50%), so an admitted pair ALWAYS rises — the
--     relation really is a sonority rule, not an arbitrary subset that
--     happens to be the right size.
--   * The ordering is per-profile, so the same visible pair is
--     admissible in some languages and not in others (requirement 4's
--     cross-seed diversity gate). A relation keyed only on letters would
--     give every language identical phonotactics.
--
--   The relation is ordinary profile state, not a query-time derivation
--   from 'profSeed': it is carried on the 'Profile', hashed into
--   'Language.Generated.Signature.profileSignature', and emitted in
--   report introspection, so the diversity metrics measure it.
--
--   Pure and integer-only like the rest of the layer (#710 requirement
--   12): the density band is evaluated in integer arithmetic, never as
--   a rounded percentage.
module Language.Generated.Onset
    ( buildOnsetRelation
    , admissibleOnset
    , consonantCapable
    , vowelCapable
    , consonantOnly
    , onsetTotalPairs
    , onsetDensityBounds
    , onsetDensityOk
    ) where

import UPrelude
import Data.List (elemIndex, sort, sortOn)
import qualified Data.Set as S
import Language.Generated.Types
import Language.Generated.Hash

-- | Build one profile's admissible-onset relation from its own
--   consonant inventory. Deterministic in @(seed, inventory)@ alone, so
--   repeated construction of the same @(version, seed)@ is
--   byte-identical (#1094 requirement 8).
buildOnsetRelation ∷ Word64 → [Char] → OnsetRelation
buildOnsetRelation seed consonants
    | total ≤ 0 = emptyOnsetRelation
    | otherwise = OnsetRelation (S.fromList (take k ordered))
  where
    -- Canonical (sorted) inventory: the relation must not depend on the
    -- order the inventory happens to have been drawn in.
    inv   = sort consonants
    total = onsetPairCountFor (length inv)

    -- This profile's private sonority ordering of its own consonants:
    -- position in the shuffle IS the sonority rank, so 'f' can be a low
    -- rank in one language and a high one in the next.
    ranked = shuffleBy seed 1 inv
    rankOf c = fromMaybe 0 (elemIndex c ranked)

    allPairs = [ (a, b) | a ← inv, b ← inv, a ≢ b ]

    (loK, hiK) = onsetDensityBounds total
    k = wordInRange (draw seed 0) loK hiK

    -- Steepest sonority rise first. Ties (a fixed rise pins @rankOf b@
    -- once @rankOf a@ is known) break on the first consonant's own rank
    -- rather than alphabetically, so the cutoff slice stays structural.
    ordered = sortOn priority allPairs
    priority (a, b) = (negate (rankOf b - rankOf a), rankOf a)

-- | How many DISTINCT ordered pairs an @n@-consonant inventory has —
--   requirement 4's @n × (n - 1)@ denominator. Identical pairs are
--   excluded because they are always inadmissible.
onsetTotalPairs ∷ Profile → Int
onsetTotalPairs = onsetPairCountFor ∘ length ∘ profConsonants

onsetPairCountFor ∷ Int → Int
onsetPairCountFor n = n * (n - 1)

-- | The inclusive admissible-pair count band for a given ordered-pair
--   total: requirement 4's 25%-45%, decided in integers so the bound is
--   exact rather than dependent on a rounded percentage.
--
--   The @max loK@ guard only matters for inventories far below the
--   version-2 floor of six consonants (where @⌈25%⌉@ can exceed
--   @⌊45%⌋@); at six or more the band is always well formed and always
--   nonempty.
onsetDensityBounds ∷ Int → (Int, Int)
onsetDensityBounds total = (loK, max loK hiK)
  where
    loK = (25 * total + 99) `div` 100  -- ceiling of 25%
    hiK = (45 * total) `div` 100       -- floor of 45%

-- | Whether a profile's relation sits inside the density band.
onsetDensityOk ∷ Profile → Bool
onsetDensityOk p =
    let total = onsetTotalPairs p
        (loK, hiK) = onsetDensityBounds total
        k = onsetPairCount (profOnset p)
    in k ≥ loK ∧ k ≤ hiK

-- | THE public admissibility query (#1094 requirements 3 and 7), and
--   the one L1c consumes for cross-syllable boundary repair.
--
--   Operates on VISIBLE characters, because a rendered root is flat
--   'Text' with no per-character slot provenance (requirement 7): a
--   glyph is consonant-capable iff it is in this profile's consonant
--   inventory, which is what makes a dual-role @y@ answer the consonant
--   interpretation when it sits beside another consonant-capable glyph.
--
--   Total for every constructible profile of every version, and false
--   whenever either character is outside this profile's consonant
--   inventory or the two are identical — so a version-1 profile (empty
--   relation) answers False everywhere without the caller needing to
--   know which version produced it.
admissibleOnset ∷ Profile → Char → Char → Bool
admissibleOnset prof a b =
    a ≢ b
    ∧ consonantCapable prof a
    ∧ consonantCapable prof b
    ∧ S.member (a, b) (onsetPairSet (profOnset prof))

-- | Whether a visible glyph can act as a consonant in this profile.
consonantCapable ∷ Profile → Char → Bool
consonantCapable prof c = c `elem` profConsonants prof

-- | Whether a visible glyph can act as a vowel in this profile. Only a
--   dual-role @y@ is ever both (requirement 6).
vowelCapable ∷ Profile → Char → Bool
vowelCapable prof c = c `elem` profVowels prof

-- | Whether a visible glyph is UNAMBIGUOUSLY a consonant in this
--   profile — consonant-capable and not also vowel-capable.
--
--   THE one definition of "this adjacency is a consonant cluster",
--   shared by #1095's boundary repair
--   ('Language.Generated.Boundary') and #1096's bound-form legality
--   ('Language.Generated.Bound'). A dual-role @y@ (requirement 6) sits
--   in both inventories and a rendered root is flat text with no slot
--   provenance, so a pair involving one is deliberately NOT treated as
--   a cluster; keeping that scoping in one place is what stops two
--   consumers of 'admissibleOnset' from disagreeing about which
--   adjacencies the relation is even being asked about.
consonantOnly ∷ Profile → Char → Bool
consonantOnly prof c = consonantCapable prof c ∧ not (vowelCapable prof c)
