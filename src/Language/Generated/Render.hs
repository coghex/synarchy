{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Native-name rendering (#710 requirements 10, 11, 13): turn a #709
--   'NameExpr' into one orthographic native word under a generated
--   'Profile' and its assigned concept roots. Pure and total given a
--   root for every referenced concept — the only failure mode is a
--   'NameExpr' pointing at a concept absent from the roots supplied
--   (mirrors 'Language.Semantic.English.renderGloss''s no-fallback
--   contract: a missing concept is a descriptive error, never raw-id
--   text or a fabricated root).
module Language.Generated.Render
    ( NativeRenderError(..)
    , nativeRenderErrorText
    , renderNative
    , applyPluralMark
    , applyPossessiveMark
    ) where

import UPrelude
import Data.Char (toUpper)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Generated.Types

-- | Why a 'NameExpr' could not be natively rendered.
newtype NativeRenderError = NativeUnknownConcept ConceptId
    deriving (Show, Eq)

nativeRenderErrorText ∷ NativeRenderError → Text
nativeRenderErrorText (NativeUnknownConcept (ConceptId cid)) =
    "unknown concept id " <> cid <> " in native name expression"

-- | Render one native proper name. @roots@ is the concept→root
--   assignment for this profile's language ('Language.Generated.Root.assignRoots'
--   over the caller's catalogue) — looking up a concept absent from it
--   is the only way this can fail.
--
--   Ordering (#710 requirement 11): 'Modifier' and 'Of' are both
--   descriptive compounds and share 'profCompoundOrder'; 'Possessive'
--   is a genitive relation with its own independent order and affix
--   (see 'Language.Generated.Types' for the rationale). 'Of's explicit
--   number applies the profile's plural marking to the complement, the
--   same marking 'applyPluralMark' exposes for direct testing.
renderNative ∷ Profile → M.Map ConceptId Text → NameExpr → Either NativeRenderError Text
renderNative prof roots expr = case expr of
    Bare c → do
        r ← look c
        pure (capitalizeWord r)
    Modifier m h → do
        rm ← look m
        rh ← look h
        pure $ capitalizeWord (orderPair prof rm rh)
    Of h num c → do
        rh ← look h
        rc ← look c
        pure $ capitalizeWord (orderPair prof (applyNumber prof num rc) rh)
    Possessive o h → do
        ro ← look o
        rh ← look h
        pure $ capitalizeWord (orderGenitive prof ro rh)
  where
    look cid = case M.lookup cid roots of
        Nothing → Left (NativeUnknownConcept cid)
        Just r  → Right r

-- | Apply explicit-number marking to a root: unchanged when singular,
--   the profile's plural affix appended when plural. The bare root is
--   always a prefix of the result (#710 requirement 9 — grammatical
--   marking affixes, never replaces, the stem).
applyNumber ∷ Profile → GramNumber → Text → Text
applyNumber _    Singular r = r
applyNumber prof Plural   r = applyPluralMark prof r

applyPluralMark ∷ Profile → Text → Text
applyPluralMark prof r = r <> plmAffix (profPlural prof)

-- | Apply possessive marking to an owner's root. The bare root is
--   always a prefix of the result, same guarantee as 'applyPluralMark'.
applyPossessiveMark ∷ Profile → Text → Text
applyPossessiveMark prof r = r <> pmAffix (profPossessive prof)

-- | Order a (modifier-slot, head-slot) pair per 'profCompoundOrder' and
--   join them per 'profJoin'.
orderPair ∷ Profile → Text → Text → Text
orderPair prof modSlot headSlot = case profCompoundOrder prof of
    ModifierFirst → joinWords prof modSlot headSlot
    HeadFirst     → joinWords prof headSlot modSlot

-- | Order a marked-owner/head pair per 'profPossessive''s independent
--   genitive order, then join them per 'profJoin'.
orderGenitive ∷ Profile → Text → Text → Text
orderGenitive prof ownerRoot headRoot =
    let ownerMarked = applyPossessiveMark prof ownerRoot
    in case pmOrder (profPossessive prof) of
        OwnerFirst        → joinWords prof ownerMarked headRoot
        HeadFirstGenitive → joinWords prof headRoot ownerMarked

joinWords ∷ Profile → Text → Text → Text
joinWords prof a b = case profJoin prof of
    JoinCompact → a <> b
    JoinHyphen  → a <> "-" <> b

capitalizeWord ∷ Text → Text
capitalizeWord w = case T.uncons w of
    Nothing      → w
    Just (c, cs) → T.cons (toUpper c) cs
