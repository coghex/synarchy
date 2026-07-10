{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Deterministic English gloss rendering for semantic proper names
--   (#709). The gloss is one of two renderings of the same 'NameExpr'
--   — the other being #710's generated-language native name — so this
--   module deliberately knows nothing about language profiles, seeds,
--   or randomness: same catalogue + same expression → same gloss, every
--   time.
--
--   Capitalization contract: every lexical word is capitalized on its
--   first character only (authored forms are lowercase; @wolf's@ →
--   @Wolf's@), and the genitive particle @of@ stays lowercase.
module Language.Semantic.English
    ( renderGloss
    ) where

import UPrelude
import Data.Char (toUpper)
import qualified Data.Text as T
import Language.Semantic.Types

-- | Render a name expression as an English proper-name gloss, or a
--   descriptive error if the catalogue cannot support it. Never falls
--   back to raw concept ids or fabricated English.
renderGloss ∷ Catalogue → NameExpr → Either RenderError Text
renderGloss cat expr = case expr of
    Bare c → do
        w ← form FormSingular c
        pure $ capitalizeWord w
    Modifier m h → do
        mw ← form FormModifier m
        hw ← form FormSingular h
        pure $ capitalizeWord mw <> " " <> capitalizeWord hw
    Of h n c → do
        hw ← form FormSingular h
        cw ← form (numberFormKind n) c
        pure $ capitalizeWord hw <> " of " <> capitalizeWord cw
    Possessive o h → do
        ow ← form FormPossessive o
        hw ← form FormSingular h
        pure $ capitalizeWord ow <> " " <> capitalizeWord hw
  where
    form ∷ FormKind → ConceptId → Either RenderError Text
    form k cid = case lookupConcept cid cat of
        Nothing → Left (UnknownConcept cid)
        Just ce → case formOf k ce of
            Nothing → Left (MissingForm cid k)
            Just w  → Right w

capitalizeWord ∷ Text → Text
capitalizeWord w = case T.uncons w of
    Nothing      → w
    Just (c, cs) → T.cons (toUpper c) cs
