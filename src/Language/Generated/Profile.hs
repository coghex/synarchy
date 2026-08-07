{-# LANGUAGE Strict #-}
-- | Deterministic profile generation (#710 requirements 1-3, 12, 14):
--   given a version and a language seed, derive a whole naming style —
--   consonant/vowel inventories, syllable shapes, root-length
--   tendencies, compound/genitive ordering, plural/possessive marking,
--   and a word-joining style. Every field is drawn from the seed via
--   'Language.Generated.Hash', so two seeds virtually never land on the
--   same style, and the same seed always reproduces the same profile.
module Language.Generated.Profile
    ( generateProfile
    , buildProfileV1
    , buildProfileV2
    , buildProfileV3
    , buildProfileV4
    , buildProfileV5
    ) where

import UPrelude
import qualified Data.Text as T
import Language.Generated.Types
import Language.Generated.Hash
import Language.Generated.Onset (buildOnsetRelation)
import Language.Generated.Boundary (buildBoundaryPolicy)
import Language.Generated.Orthography (extendedInventory)

-- | Generate a profile for an explicit version. Dispatch is per
--   VERSION, never by comparison with 'currentGeneratorVersion'
--   (#1092 requirement 4): a world records the version that named it,
--   and must stay reconstructible after the current version advances.
--   A version that never existed is a descriptive failure, never
--   silently treated as version 1.
generateProfile ∷ GeneratorVersion → LangSeed → Either GeneratorError Profile
generateProfile ver seed = case generatorVersionInt ver of
    1 → Right (buildProfileV1 seed)
    2 → Right (buildProfileV2 seed)
    3 → Right (buildProfileV3 seed)
    4 → Right (buildProfileV4 seed)
    5 → Right (buildProfileV5 seed)
    v → Left (UnsupportedGeneratorVersion v)

-- | The version-1 profile generator. Total: every drawn range is
--   nonempty by construction, so this never needs to fail.
--
--   Its stamped 'profVersion' is the LITERAL 'GeneratorVersion' 1, not
--   'currentGeneratorVersion' (#1092): 'Language.Generated.Root'
--   feeds @profVersion@ into its per-concept seed, so stamping the
--   mutable constant would silently re-render every reconstructed v1
--   root the moment the current version advanced.
buildProfileV1 ∷ LangSeed → Profile
buildProfileV1 seed@(LangSeed s0) =
    let -- Domain-separated from root generation (Language.Generated.Root
        -- derives its own per-concept seeds from the raw LangSeed), so
        -- profile style and root vocabulary vary independently.
        baseSeed = fmix64 (s0 `xor` 0xA5A5A5A5A5A5A5A5)

        consSeed   = draw baseSeed 1
        vowSeed    = draw baseSeed 2
        shapeSeed  = draw baseSeed 3
        sylSeed    = draw baseSeed 4
        orderSeed  = draw baseSeed 5
        possSeed   = draw baseSeed 6
        pluralSeed = draw baseSeed 7
        joinSeed   = draw baseSeed 8

        consCount = wordInRange (draw consSeed 0) minConsonants maxConsonants
        consonants = take consCount (shuffleBy consSeed 1 consonantPool)

        vowCount = wordInRange (draw vowSeed 0) minVowels maxVowels
        vowels = take vowCount (shuffleBy vowSeed 1 vowelPool)

        shapeCount = wordInRange (draw shapeSeed 0) minShapes maxShapes
        shapes = take shapeCount (shuffleBy shapeSeed 1 syllableShapePool)

        minSyll = wordInRange (draw sylSeed 0) 1 2
        maxSyll = minSyll + wordInRange (draw sylSeed 1) 0 1

        compoundOrder
            | wordInRange (draw orderSeed 0) 0 1 ≡ 0 = ModifierFirst
            | otherwise                               = HeadFirst
        genitiveOrder
            | wordInRange (draw orderSeed 1) 0 1 ≡ 0 = OwnerFirst
            | otherwise                               = HeadFirstGenitive

        possAffix   = genAffix possSeed consonants vowels True
        pluralAffix = genAffix pluralSeed consonants vowels False

        joinStyle
            | wordInRange (draw joinSeed 0) 0 1 ≡ 0 = JoinCompact
            | otherwise                               = JoinHyphen

    in Profile
        { profVersion        = GeneratorVersion 1
        , profSeed           = seed
        , profConsonants     = consonants
        , profVowels         = vowels
        , profSyllableShapes = shapes
        , profMinSyllables   = minSyll
        , profMaxSyllables   = maxSyll
        , profCompoundOrder  = compoundOrder
        , profPossessive     = PossessiveMarking genitiveOrder possAffix
        , profPlural         = PluralMarking pluralAffix
        , profJoin           = joinStyle
        -- Version 1 constrains nothing: its CCV rendering draws both
        -- consonants independently and must stay byte-identical
        -- (#1094 requirement 1). An empty relation is exactly what
        -- makes Language.Generated.Root take the historical path.
        , profOnset          = emptyOnsetRelation
        -- Same reasoning for boundaries (#1095): version 1 joins every
        -- morpheme raw and must keep doing so.
        , profBoundary       = BoundaryUnmediated
        }

-- | The version-2 profile generator (#1094). Same style vocabulary as
--   version 1, plus the two things this version exists for:
--
--   * an admissible two-consonant onset relation, which @CCV@ rendering
--     selects from directly (requirements 3-5);
--   * a deterministic role for @y@ — consonant-only, vowel-only, or
--     both — reflected in actual inventory membership, never merely in
--     the pools drawn from (requirement 6).
--
--   Like 'buildProfileV1' this stamps a LITERAL 'GeneratorVersion', not
--   'currentGeneratorVersion': 'Language.Generated.Root' mixes
--   @profVersion@ into its per-concept seed, so a version-3 bump must
--   not re-render version-2 roots.
--
--   The draws shared with version 1 are deliberately REPEATED here
--   rather than factored into a shared helper. Each version's body is
--   frozen output (#710 requirement 15, #1092 requirement 4): a shared
--   helper is a live edge between them, down which a later version's
--   tweak silently re-renders every older world's names.
buildProfileV2 ∷ LangSeed → Profile
buildProfileV2 seed@(LangSeed s0) =
    let baseSeed = fmix64 (s0 `xor` 0xA5A5A5A5A5A5A5A5)

        consSeed   = draw baseSeed 1
        vowSeed    = draw baseSeed 2
        shapeSeed  = draw baseSeed 3
        sylSeed    = draw baseSeed 4
        orderSeed  = draw baseSeed 5
        possSeed   = draw baseSeed 6
        pluralSeed = draw baseSeed 7
        joinSeed   = draw baseSeed 8
        yRoleSeed  = draw baseSeed 9
        onsetSeed  = draw baseSeed 10

        yRole = case wordInRange (draw yRoleSeed 0) 0 2 of
            0 → YConsonantOnly
            1 → YVowelOnly
            _ → YBothRoles
        yIsConsonant = yRole ≢ YVowelOnly
        yIsVowel     = yRole ≢ YConsonantOnly

        -- 'y' is placed into the inventory it has a role in, rather than
        -- merely left in the pool to be drawn or missed: requirement 6's
        -- role table IS inventory membership, so a "consonant-only y"
        -- profile whose shuffle happened not to select y would be a
        -- fourth, forbidden "neither" state.
        consCount = wordInRange (draw consSeed 0) minConsonants maxConsonants
        consDrawn = take (if yIsConsonant then consCount - 1 else consCount)
                          (shuffleBy consSeed 1 consonantPoolNoY)
        consonants
            | yIsConsonant = insertAt (pickIndex (draw consSeed 100)
                                                  (length consDrawn + 1))
                                       'y' consDrawn
            | otherwise    = consDrawn

        vowCount = wordInRange (draw vowSeed 0) minVowels maxVowels
        vowDrawn = take (if yIsVowel then vowCount - 1 else vowCount)
                         (shuffleBy vowSeed 1 vowelPool)
        vowels
            | yIsVowel  = insertAt (pickIndex (draw vowSeed 100)
                                               (length vowDrawn + 1))
                                    'y' vowDrawn
            | otherwise = vowDrawn

        shapeCount = wordInRange (draw shapeSeed 0) minShapes maxShapes
        shapes = take shapeCount (shuffleBy shapeSeed 1 syllableShapePool)

        minSyll = wordInRange (draw sylSeed 0) 1 2
        maxSyll = minSyll + wordInRange (draw sylSeed 1) 0 1

        compoundOrder
            | wordInRange (draw orderSeed 0) 0 1 ≡ 0 = ModifierFirst
            | otherwise                               = HeadFirst
        genitiveOrder
            | wordInRange (draw orderSeed 1) 0 1 ≡ 0 = OwnerFirst
            | otherwise                               = HeadFirstGenitive

        possAffix   = genAffix possSeed consonants vowels True
        pluralAffix = genAffix pluralSeed consonants vowels False

        joinStyle
            | wordInRange (draw joinSeed 0) 0 1 ≡ 0 = JoinCompact
            | otherwise                               = JoinHyphen

    in Profile
        { profVersion        = GeneratorVersion 2
        , profSeed           = seed
        , profConsonants     = consonants
        , profVowels         = vowels
        , profSyllableShapes = shapes
        , profMinSyllables   = minSyll
        , profMaxSyllables   = maxSyll
        , profCompoundOrder  = compoundOrder
        , profPossessive     = PossessiveMarking genitiveOrder possAffix
        , profPlural         = PluralMarking pluralAffix
        , profJoin           = joinStyle
        , profOnset          = buildOnsetRelation onsetSeed consonants
        -- Version 2 still joins morphemes raw (#1095 arrived in version
        -- 3), and its pinned goldens include the triple 'Zoccce-payi'g'
        -- inherited from version 1's own CCV path.
        , profBoundary       = BoundaryUnmediated
        }

-- | The version-3 profile generator (#1095). Same style vocabulary as
--   version 2, plus the one thing this version exists for: a
--   'BoundaryPolicy', so every morpheme join is mediated instead of
--   being a bare concatenation.
--
--   Like its predecessors this stamps a LITERAL 'GeneratorVersion' and
--   deliberately REPEATS the draws it shares with them rather than
--   factoring them into a helper — each version's body is frozen output
--   (#710 requirement 15, #1092 requirement 4), and a shared helper is a
--   live edge down which a later version's tweak silently re-renders
--   every older world's names.
--
--   The boundary draw is appended at a FRESH step index rather than
--   inserted among the existing ones, so a version-3 profile keeps
--   version 2's inventories, shapes, orders, affixes, join style, and
--   onset relation for the same seed; the boundary phonology is the only
--   difference between the two versions' output.
buildProfileV3 ∷ LangSeed → Profile
buildProfileV3 seed@(LangSeed s0) =
    let baseSeed = fmix64 (s0 `xor` 0xA5A5A5A5A5A5A5A5)

        consSeed     = draw baseSeed 1
        vowSeed      = draw baseSeed 2
        shapeSeed    = draw baseSeed 3
        sylSeed      = draw baseSeed 4
        orderSeed    = draw baseSeed 5
        possSeed     = draw baseSeed 6
        pluralSeed   = draw baseSeed 7
        joinSeed     = draw baseSeed 8
        yRoleSeed    = draw baseSeed 9
        onsetSeed    = draw baseSeed 10
        boundarySeed = draw baseSeed 11

        yRole = case wordInRange (draw yRoleSeed 0) 0 2 of
            0 → YConsonantOnly
            1 → YVowelOnly
            _ → YBothRoles
        yIsConsonant = yRole ≢ YVowelOnly
        yIsVowel     = yRole ≢ YConsonantOnly

        consCount = wordInRange (draw consSeed 0) minConsonants maxConsonants
        consDrawn = take (if yIsConsonant then consCount - 1 else consCount)
                          (shuffleBy consSeed 1 consonantPoolNoY)
        consonants
            | yIsConsonant = insertAt (pickIndex (draw consSeed 100)
                                                  (length consDrawn + 1))
                                       'y' consDrawn
            | otherwise    = consDrawn

        vowCount = wordInRange (draw vowSeed 0) minVowels maxVowels
        vowDrawn = take (if yIsVowel then vowCount - 1 else vowCount)
                         (shuffleBy vowSeed 1 vowelPool)
        vowels
            | yIsVowel  = insertAt (pickIndex (draw vowSeed 100)
                                               (length vowDrawn + 1))
                                    'y' vowDrawn
            | otherwise = vowDrawn

        shapeCount = wordInRange (draw shapeSeed 0) minShapes maxShapes
        shapes = take shapeCount (shuffleBy shapeSeed 1 syllableShapePool)

        minSyll = wordInRange (draw sylSeed 0) 1 2
        maxSyll = minSyll + wordInRange (draw sylSeed 1) 0 1

        compoundOrder
            | wordInRange (draw orderSeed 0) 0 1 ≡ 0 = ModifierFirst
            | otherwise                               = HeadFirst
        genitiveOrder
            | wordInRange (draw orderSeed 1) 0 1 ≡ 0 = OwnerFirst
            | otherwise                               = HeadFirstGenitive

        possAffix   = genAffix possSeed consonants vowels True
        pluralAffix = genAffix pluralSeed consonants vowels False

        joinStyle
            | wordInRange (draw joinSeed 0) 0 1 ≡ 0 = JoinCompact
            | otherwise                               = JoinHyphen

    in Profile
        { profVersion        = GeneratorVersion 3
        , profSeed           = seed
        , profConsonants     = consonants
        , profVowels         = vowels
        , profSyllableShapes = shapes
        , profMinSyllables   = minSyll
        , profMaxSyllables   = maxSyll
        , profCompoundOrder  = compoundOrder
        , profPossessive     = PossessiveMarking genitiveOrder possAffix
        , profPlural         = PluralMarking pluralAffix
        , profJoin           = joinStyle
        , profOnset          = buildOnsetRelation onsetSeed consonants
        , profBoundary       = buildBoundaryPolicy boundarySeed consonants vowels
        }

-- | The version-4 profile generator (#1096). Structurally IDENTICAL to
--   version 3 — same inventories, shapes, orders, affixes, join style,
--   onset relation, and boundary policy for the same seed — because the
--   thing this version exists for is not a profile field at all.
--
--   Bound morphology (#1096) lives in the concept→morpheme assignment
--   ('Language.Generated.Bound'), which reads the version off the
--   profile it is handed. So version 4's ONLY profile-level difference
--   is its stamped version, and that difference is load bearing twice
--   over: 'Language.Generated.Root' mixes @profVersion@ into its
--   per-concept seed (so version 4's free roots are its own, not
--   version 3's), and 'Language.Generated.Bound.formsBoundMorphemes'
--   reads it to decide whether the language has bound forms at all.
--
--   The draws are REPEATED here rather than shared with
--   'buildProfileV3' for the reason every version before it repeated
--   them: each version's body is frozen output (#710 requirement 15,
--   #1092 requirement 4), and a shared helper is a live edge down which
--   a later version's tweak silently re-renders every older world's
--   names.
buildProfileV4 ∷ LangSeed → Profile
buildProfileV4 seed@(LangSeed s0) =
    let baseSeed = fmix64 (s0 `xor` 0xA5A5A5A5A5A5A5A5)

        consSeed     = draw baseSeed 1
        vowSeed      = draw baseSeed 2
        shapeSeed    = draw baseSeed 3
        sylSeed      = draw baseSeed 4
        orderSeed    = draw baseSeed 5
        possSeed     = draw baseSeed 6
        pluralSeed   = draw baseSeed 7
        joinSeed     = draw baseSeed 8
        yRoleSeed    = draw baseSeed 9
        onsetSeed    = draw baseSeed 10
        boundarySeed = draw baseSeed 11

        yRole = case wordInRange (draw yRoleSeed 0) 0 2 of
            0 → YConsonantOnly
            1 → YVowelOnly
            _ → YBothRoles
        yIsConsonant = yRole ≢ YVowelOnly
        yIsVowel     = yRole ≢ YConsonantOnly

        consCount = wordInRange (draw consSeed 0) minConsonants maxConsonants
        consDrawn = take (if yIsConsonant then consCount - 1 else consCount)
                          (shuffleBy consSeed 1 consonantPoolNoY)
        consonants
            | yIsConsonant = insertAt (pickIndex (draw consSeed 100)
                                                  (length consDrawn + 1))
                                       'y' consDrawn
            | otherwise    = consDrawn

        vowCount = wordInRange (draw vowSeed 0) minVowels maxVowels
        vowDrawn = take (if yIsVowel then vowCount - 1 else vowCount)
                         (shuffleBy vowSeed 1 vowelPool)
        vowels
            | yIsVowel  = insertAt (pickIndex (draw vowSeed 100)
                                               (length vowDrawn + 1))
                                    'y' vowDrawn
            | otherwise = vowDrawn

        shapeCount = wordInRange (draw shapeSeed 0) minShapes maxShapes
        shapes = take shapeCount (shuffleBy shapeSeed 1 syllableShapePool)

        minSyll = wordInRange (draw sylSeed 0) 1 2
        maxSyll = minSyll + wordInRange (draw sylSeed 1) 0 1

        compoundOrder
            | wordInRange (draw orderSeed 0) 0 1 ≡ 0 = ModifierFirst
            | otherwise                               = HeadFirst
        genitiveOrder
            | wordInRange (draw orderSeed 1) 0 1 ≡ 0 = OwnerFirst
            | otherwise                               = HeadFirstGenitive

        possAffix   = genAffix possSeed consonants vowels True
        pluralAffix = genAffix pluralSeed consonants vowels False

        joinStyle
            | wordInRange (draw joinSeed 0) 0 1 ≡ 0 = JoinCompact
            | otherwise                               = JoinHyphen

    in Profile
        { profVersion        = GeneratorVersion 4
        , profSeed           = seed
        , profConsonants     = consonants
        , profVowels         = vowels
        , profSyllableShapes = shapes
        , profMinSyllables   = minSyll
        , profMaxSyllables   = maxSyll
        , profCompoundOrder  = compoundOrder
        , profPossessive     = PossessiveMarking genitiveOrder possAffix
        , profPlural         = PluralMarking pluralAffix
        , profJoin           = joinStyle
        , profOnset          = buildOnsetRelation onsetSeed consonants
        , profBoundary       = buildBoundaryPolicy boundarySeed consonants vowels
        }

-- | The version-5 profile generator (#1100). Same style vocabulary as
--   version 4, plus the one thing this version exists for: per-language
--   extended orthography. A language draws a diacritic family and a few
--   marked letters from it, and those letters JOIN its inventories —
--   they are phonemes, not decoration applied afterwards.
--
--   That placement is the whole design. Everything downstream reads the
--   inventories and nothing reads a separate "accents" field, so the
--   extended letters participate in #1094's onset relation, #1095's
--   boundary repair, #1096's bound forms, and affix generation
--   identically to ASCII ones, with no rule anywhere spelling out an
--   exception for them. It is also why the marks are drawn BEFORE the
--   affixes, onset relation and boundary policy below: those three are
--   built from the FINAL inventories, so an accented letter can be an
--   affix letter, a member of an admissible onset pair, or a linking
--   segment.
--
--   Like every version before it this stamps a LITERAL
--   'GeneratorVersion' and deliberately REPEATS the draws it shares with
--   its predecessors rather than factoring them into a helper — each
--   version's body is frozen output (#710 requirement 15, #1092
--   requirement 4), and a shared helper is a live edge down which a
--   later version's tweak silently re-renders every older world's names.
--
--   The orthography draw is appended at a FRESH step index, so a
--   version-5 profile keeps version 4's ASCII inventories, shapes,
--   orders and syllable counts for the same seed; the marked letters
--   (and what the onset relation, boundary policy and affixes make of
--   them) are the only difference between the two versions.
buildProfileV5 ∷ LangSeed → Profile
buildProfileV5 seed@(LangSeed s0) =
    let baseSeed = fmix64 (s0 `xor` 0xA5A5A5A5A5A5A5A5)

        consSeed     = draw baseSeed 1
        vowSeed      = draw baseSeed 2
        shapeSeed    = draw baseSeed 3
        sylSeed      = draw baseSeed 4
        orderSeed    = draw baseSeed 5
        possSeed     = draw baseSeed 6
        pluralSeed   = draw baseSeed 7
        joinSeed     = draw baseSeed 8
        yRoleSeed    = draw baseSeed 9
        onsetSeed    = draw baseSeed 10
        boundarySeed = draw baseSeed 11
        orthoSeed    = draw baseSeed 12

        yRole = case wordInRange (draw yRoleSeed 0) 0 2 of
            0 → YConsonantOnly
            1 → YVowelOnly
            _ → YBothRoles
        yIsConsonant = yRole ≢ YVowelOnly
        yIsVowel     = yRole ≢ YConsonantOnly

        consCount = wordInRange (draw consSeed 0) minConsonants maxConsonants
        consDrawn = take (if yIsConsonant then consCount - 1 else consCount)
                          (shuffleBy consSeed 1 consonantPoolNoY)
        baseConsonants
            | yIsConsonant = insertAt (pickIndex (draw consSeed 100)
                                                  (length consDrawn + 1))
                                       'y' consDrawn
            | otherwise    = consDrawn

        vowCount = wordInRange (draw vowSeed 0) minVowels maxVowels
        vowDrawn = take (if yIsVowel then vowCount - 1 else vowCount)
                         (shuffleBy vowSeed 1 vowelPool)
        baseVowels
            | yIsVowel  = insertAt (pickIndex (draw vowSeed 100)
                                               (length vowDrawn + 1))
                                    'y' vowDrawn
            | otherwise = vowDrawn

        -- The marked letters are APPENDED rather than inserted at drawn
        -- positions. Every selection downstream is a uniform
        -- 'pickIndex' over the whole list, so a tail position is no
        -- less likely than any other — and appending keeps the ASCII
        -- prefix of a version-5 inventory identical to version 4's,
        -- which is what makes the two versions comparable seed by seed.
        (extVowels, extConsonants) =
            extendedInventory orthoSeed baseVowels baseConsonants
        consonants = baseConsonants <> extConsonants
        vowels     = baseVowels <> extVowels

        shapeCount = wordInRange (draw shapeSeed 0) minShapes maxShapes
        shapes = take shapeCount (shuffleBy shapeSeed 1 syllableShapePool)

        minSyll = wordInRange (draw sylSeed 0) 1 2
        maxSyll = minSyll + wordInRange (draw sylSeed 1) 0 1

        compoundOrder
            | wordInRange (draw orderSeed 0) 0 1 ≡ 0 = ModifierFirst
            | otherwise                               = HeadFirst
        genitiveOrder
            | wordInRange (draw orderSeed 1) 0 1 ≡ 0 = OwnerFirst
            | otherwise                               = HeadFirstGenitive

        possAffix   = genAffix possSeed consonants vowels True
        pluralAffix = genAffix pluralSeed consonants vowels False

        joinStyle
            | wordInRange (draw joinSeed 0) 0 1 ≡ 0 = JoinCompact
            | otherwise                               = JoinHyphen

    in Profile
        { profVersion        = GeneratorVersion 5
        , profSeed           = seed
        , profConsonants     = consonants
        , profVowels         = vowels
        , profSyllableShapes = shapes
        , profMinSyllables   = minSyll
        , profMaxSyllables   = maxSyll
        , profCompoundOrder  = compoundOrder
        , profPossessive     = PossessiveMarking genitiveOrder possAffix
        , profPlural         = PluralMarking pluralAffix
        , profJoin           = joinStyle
        , profOnset          = buildOnsetRelation onsetSeed consonants
        , profBoundary       = buildBoundaryPolicy boundarySeed consonants vowels
        }

insertAt ∷ Int → α → [α] → [α]
insertAt i x xs = take i xs <> (x : drop i xs)

-- | A short affix (1-2 letters, alternating consonant/vowel so it stays
--   pronounceable) appended directly to a root. Possessive affixes may
--   additionally lead with a single apostrophe; plural affixes never do
--   (#710 requirement 6 — no leading/trailing/repeated punctuation, and
--   the affix is always appended after a root's own letters, so a
--   leading apostrophe here can never become the first character of a
--   rendered word).
genAffix ∷ Word64 → [Char] → [Char] → Bool → Text
genAffix seed cons vow allowApostrophe =
    let useApostrophe = allowApostrophe ∧ (wordInRange (draw seed 0) 0 2 ≡ 0)
        letterCount = wordInRange (draw seed 1) 1 2
        letters = [ pickLetter i | i ← [0 .. letterCount - 1] ]
        body = T.pack letters
    in if useApostrophe then T.cons '\'' body else body
  where
    pickLetter i
        | even i    = cons !! pickIndex (draw seed (10 + i)) (length cons)
        | otherwise = vow  !! pickIndex (draw seed (10 + i)) (length vow)

-- Consonant/vowel pools and syllable-shape catalogue. Shapes are capped
-- at 3 segments and syllable counts at [1,3] so a single root (and a
-- fortiori a two-root compound plus a short affix) stays comfortably
-- inside the 3-32 character contract (#710 requirement 6) without ever
-- needing to truncate generated output.
consonantPool ∷ [Char]
consonantPool = "bcdfghjklmnprstvwyz"

-- | Version 2's consonant pool: 'consonantPool' without @y@, which that
--   version places explicitly according to its drawn 'YRole' rather
--   than leaving to the shuffle. Version 1 keeps the historical pool
--   above, where @y@ is consonant-only and may simply not be drawn.
consonantPoolNoY ∷ [Char]
consonantPoolNoY = "bcdfghjklmnprstvwz"

vowelPool ∷ [Char]
vowelPool = "aeiou"

syllableShapePool ∷ [SyllableShape]
syllableShapePool =
    [ SyllableShape [ConsonantSlot, VowelSlot]
    , SyllableShape [VowelSlot, ConsonantSlot]
    , SyllableShape [ConsonantSlot, VowelSlot, ConsonantSlot]
    , SyllableShape [ConsonantSlot, ConsonantSlot, VowelSlot]
    ]

minConsonants, maxConsonants, minVowels, maxVowels, minShapes, maxShapes ∷ Int
minConsonants = 6
maxConsonants = 12
minVowels = 3
maxVowels = 5
minShapes = 2
maxShapes = 4
