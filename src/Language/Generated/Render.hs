{-# LANGUAGE Strict #-}
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
import Language.Generated.Boundary (joinMorphemes)
import Language.Generated.Bound (LanguageRoots(..))

-- | Why a 'NameExpr' could not be natively rendered.
newtype NativeRenderError = NativeUnknownConcept ConceptId
    deriving (Show, Eq)

nativeRenderErrorText ∷ NativeRenderError → Text
nativeRenderErrorText (NativeUnknownConcept (ConceptId cid)) =
    "unknown concept id " <> cid <> " in native name expression"

-- | Render one native proper name. @roots@ is the concept→morpheme
--   assignment for this profile's language
--   ('Language.Generated.Root.assignLanguageRoots' over the caller's
--   catalogue) — looking up a concept absent from its FREE map is the
--   only way this can fail.
--
--   Ordering (#710 requirement 11): 'Modifier' and 'Of' are both
--   descriptive compounds and share 'profCompoundOrder'; 'Possessive'
--   is a genitive relation with its own independent order and affix
--   (see 'Language.Generated.Types' for the rationale). 'Of's explicit
--   number applies the profile's plural marking to the complement, the
--   same marking 'applyPluralMark' exposes for direct testing.
--
--   Bound forms (#1096 requirement 6) are selected by SEMANTIC ROLE,
--   not by rendered word order: the dependent slot of each compound
--   form takes the concept's bound form when it has one, the head slot
--   always takes the free form, and 'Bare' — which has no dependent
--   slot at all — is always free. Whether the profile then writes the
--   dependent side first or last is 'orderPair'/'orderGenitive''s
--   business and changes only display order, never which slot was
--   dependent.
--
--   Requirement 7's ordering falls out of where the selection sits:
--   'dependent' resolves the morpheme BEFORE 'applyNumber' or
--   'orderGenitive''s possessive marking ever see it, so a mark is
--   always applied to the already-selected form, and the join it then
--   meets goes through #1095's boundary repair exactly as before.
--
--   Every lookup here is an ordinary map lookup on an assignment built
--   ahead of time (#1096 requirement 8). Nothing in this module scans a
--   catalogue, derives a shortening, or retries: a concept with no
--   stored bound form simply falls back to its free root.
renderNative ∷ Profile → LanguageRoots → NameExpr → Either NativeRenderError Text
renderNative prof roots expr = case expr of
    Bare c → do
        r ← look c
        pure (capitalizeWord r)
    Modifier m h → do
        rm ← dependent m
        rh ← look h
        pure $ capitalizeWord (orderPair prof rm rh)
    Of h num c → do
        rh ← look h
        rc ← dependent c
        pure $ capitalizeWord (orderPair prof (applyNumber prof num rc) rh)
    Possessive o h → do
        ro ← dependent o
        rh ← look h
        pure $ capitalizeWord (orderGenitive prof ro rh)
  where
    look cid = case M.lookup cid (lrFree roots) of
        Nothing → Left (NativeUnknownConcept cid)
        Just r  → Right r

    -- The free lookup runs FIRST even when a bound form exists, so an
    -- unknown concept stays the descriptive failure it always was
    -- rather than being masked by a bound map that happened to carry it.
    dependent cid = do
        r ← look cid
        pure (fromMaybe r (M.lookup cid (lrBound roots)))

-- | Apply explicit-number marking to a root: unchanged when singular,
--   the profile's plural affix appended when plural. The bare root is
--   always a prefix of the result (#710 requirement 9 — grammatical
--   marking affixes, never replaces, the stem).
applyNumber ∷ Profile → GramNumber → Text → Text
applyNumber _    Singular r = r
applyNumber prof Plural   r = applyPluralMark prof r

applyPluralMark ∷ Profile → Text → Text
applyPluralMark prof r = affixMark prof r (plmAffix (profPlural prof))

-- | Apply possessive marking to an owner's root. The bare root is
--   always a prefix of the result, same guarantee as 'applyPluralMark'.
applyPossessiveMark ∷ Profile → Text → Text
applyPossessiveMark prof r = affixMark prof r (pmAffix (profPossessive prof))

-- | Attach a grammatical affix to a root through #1095's boundary
--   phonology.
--
--   A possessive affix that leads with its own apostrophe already
--   separates the two morphemes' letters, so it is appended unchanged —
--   the apostrophe survives exactly once, and no letters face each other
--   across the boundary for a repair to mediate. Every other affix meets
--   the root's final letter directly and goes through 'joinMorphemes',
--   which never touches the LEFT side: the bare root therefore stays a
--   prefix of the marked form, and a repair can only insert a linking
--   segment or trim the affix's own initial one — never erase the mark.
affixMark ∷ Profile → Text → Text → Text
affixMark prof r affix = case T.uncons affix of
    Just ('\'', _) → r <> affix
    _              → joinMorphemes prof r affix

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

-- | Join two compound elements per 'profJoin'.
--
--   A compact join puts the two elements' letters in direct contact, so
--   it is mediated by #1095's boundary phonology. A hyphen join does not:
--   the separator is itself the boundary marker, it is preserved exactly
--   once, and no letter run can span it (three contiguous LETTERS is what
--   a triple run means, so @a-a@ is not one). Boundary rules apply around
--   a separator, never by removing it — nothing here can delete a hyphen.
joinWords ∷ Profile → Text → Text → Text
joinWords prof a b = case profJoin prof of
    JoinCompact → joinMorphemes prof a b
    JoinHyphen  → a <> "-" <> b

capitalizeWord ∷ Text → Text
capitalizeWord w = case T.uncons w of
    Nothing      → w
    Just (c, cs) → T.cons (toUpper c) cs
