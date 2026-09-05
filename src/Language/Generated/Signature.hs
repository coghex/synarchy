{-# LANGUAGE Strict #-}
-- | Profile signatures (#710 requirement 19 amendment): a deterministic
--   digest over exactly the style-bearing fields a profile is built
--   from — consonant/vowel inventories, syllable shapes, root-length
--   tendencies, compound/genitive ordering, plural/possessive marking,
--   and join style. Deliberately excludes the language seed, generator
--   version, concept-root assignments, and rendered names, so the
--   report's distinct-signature count measures naming-STYLE diversity
--   across seeds rather than trivially re-encoding the seed itself.
module Language.Generated.Signature
    ( profileSignature
    ) where

import UPrelude
import qualified Data.Text as T
import Language.Generated.Types
import Language.Generated.Hash (textSeed, fmix64)

profileSignature ∷ Profile → Text
profileSignature = tshow ∘ canonHash

canonHash ∷ Profile → Word64
canonHash p = foldl' mix 0xcbf29ce484222325 (scalarFields <> shapeFields)
  where
    mix acc w = fmix64 (acc `xor` w)

    scalarFields =
        [ textSeed (T.pack (profConsonants p))
        , textSeed (T.pack (profVowels p))
        , fromIntegral (profMinSyllables p)
        , fromIntegral (profMaxSyllables p)
        , compoundOrderCode (profCompoundOrder p)
        , genitiveOrderCode (pmOrder (profPossessive p))
        , textSeed (pmAffix (profPossessive p))
        , textSeed (plmAffix (profPlural p))
        , joinStyleCode (profJoin p)
        -- The admissible-onset relation is style-bearing state (#1094
        -- requirement 3), so it is hashed like any other style field —
        -- otherwise the report's distinct-signature count would call
        -- two languages with different phonotactics the same style.
        -- The count is mixed alongside the canonical text for the same
        -- reason the shape list carries a leading length below: it
        -- keeps a pair set distinct from any prefix of itself.
        , fromIntegral (onsetPairCount (profOnset p))
        , textSeed (onsetPairText (profOnset p))
        -- The boundary policy is style-bearing state too (#1095
        -- requirement 1 — the mediation is per-language, not universal),
        -- so it is hashed like the rest. Rule and segments are separate
        -- elements: the segment text is empty for an unmediated policy,
        -- which would otherwise make every historical profile's boundary
        -- contribution indistinguishable from a mediated one that
        -- happened to hash to the same value.
        , boundaryRuleCode (profBoundary p)
        , textSeed (boundarySegmentText (profBoundary p))
        ]

    -- Each shape is mixed in as its OWN element rather than
    -- concatenated into one string first: shape codes are variable
    -- length ("CV"/"VC" are 2 chars, "CVC"/"CCV" are 3), so
    -- concatenating them ahead of hashing is ambiguous — e.g.
    -- [CV, CVC] and [CVC, VC] both concatenate to "CVCVC" and would
    -- hash identically despite being different profiles. A leading
    -- shape-count element also keeps a shape list distinct from any
    -- of its own prefixes.
    shapeFields = fromIntegral (length (profSyllableShapes p))
                : map (textSeed ∘ T.pack ∘ shapeCode) (profSyllableShapes p)

    shapeCode = map segCode ∘ shapeSegments
    segCode ConsonantSlot = 'C'
    segCode VowelSlot     = 'V'

    compoundOrderCode ∷ CompoundOrder → Word64
    compoundOrderCode ModifierFirst = 0
    compoundOrderCode HeadFirst     = 1

    genitiveOrderCode ∷ GenitiveOrder → Word64
    genitiveOrderCode OwnerFirst        = 0
    genitiveOrderCode HeadFirstGenitive = 1

    joinStyleCode ∷ JoinStyle → Word64
    joinStyleCode JoinCompact = 0
    joinStyleCode JoinHyphen  = 1

    boundaryRuleCode ∷ BoundaryPolicy → Word64
    boundaryRuleCode BoundaryUnmediated = 0
    boundaryRuleCode (BoundaryMediated rep) = case brRule rep of
        BoundaryEpenthetic  → 1
        BoundaryHarmonic    → 2
        BoundarySimplifying → 3
