{-# LANGUAGE Strict, UnicodeSyntax #-}
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
profileSignature = T.pack ∘ show ∘ canonHash

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
