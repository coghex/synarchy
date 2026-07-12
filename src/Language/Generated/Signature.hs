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
canonHash p = foldl' mix 0xcbf29ce484222325
    [ textSeed (T.pack (profConsonants p))
    , textSeed (T.pack (profVowels p))
    , textSeed (T.pack (concatMap shapeCode (profSyllableShapes p)))
    , fromIntegral (profMinSyllables p)
    , fromIntegral (profMaxSyllables p)
    , compoundOrderCode (profCompoundOrder p)
    , genitiveOrderCode (pmOrder (profPossessive p))
    , textSeed (pmAffix (profPossessive p))
    , textSeed (plmAffix (profPlural p))
    , joinStyleCode (profJoin p)
    ]
  where
    mix acc w = fmix64 (acc `xor` w)

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
