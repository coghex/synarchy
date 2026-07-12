{-# LANGUAGE Strict, UnicodeSyntax #-}
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
    ) where

import UPrelude
import qualified Data.Text as T
import Language.Generated.Types
import Language.Generated.Hash

-- | Generate a profile for an explicit version. Only version 1 is
--   defined (#710 requirement 2) — an unsupported version is a
--   descriptive failure, never silently treated as version 1.
generateProfile ∷ GeneratorVersion → LangSeed → Either GeneratorError Profile
generateProfile ver seed
    | generatorVersionInt ver ≡ generatorVersionInt currentGeneratorVersion =
        Right (buildProfileV1 seed)
    | otherwise = Left (UnsupportedGeneratorVersion (generatorVersionInt ver))

-- | The version-1 profile generator. Total: every drawn range is
--   nonempty by construction, so this never needs to fail.
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
        { profVersion        = currentGeneratorVersion
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
        }

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
