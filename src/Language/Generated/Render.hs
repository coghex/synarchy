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
      -- * Explanatory trace (#1104)
    , SlotRole(..)
    , SlotMark(..)
    , SurfacePiece(..)
    , pieceSurface
    , renderNativeTrace
    , traceSurface
    , applyPluralMark
    , applyPossessiveMark
    ) where

import UPrelude
import Data.Char (toUpper)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Generated.Types
import Language.Generated.Boundary (joinMorphemesTrace)
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
renderNative prof roots expr = traceSurface ⊚ renderNativeTrace prof roots expr

-- * Explanatory trace (#1104) ----------------------------------------

-- | Which SEMANTIC slot of an expression a rendered piece came from.
--   Attached to the concept, not to a position, so it stays correct
--   under either 'profCompoundOrder' or either 'pmOrder' (#1104
--   requirement 6: \"semantic roles remain attached to their concepts
--   even when surface ordering changes\").
data SlotRole
    = SlotHead        -- ^ the head of every form, always the FREE root
    | SlotModifier    -- ^ 'Modifier''s dependent slot
    | SlotComplement  -- ^ 'Of''s explicitly-numbered dependent slot
    | SlotOwner       -- ^ 'Possessive''s dependent slot
    deriving (Show, Eq)

-- | Grammatical marking applied to one slot AFTER its lexical form was
--   selected — the ordering 'renderNative' has always had, restated
--   here because an explanation must separate \"which morpheme\" from
--   \"what was done to it\".
data SlotMark = MarkPlural | MarkPossessive
    deriving (Show, Eq)

-- | One realized piece of a rendered name, in surface order.
--
--   'PieceMorpheme' and 'PieceMark' carry text that came from the
--   lexicon; 'PieceLink' and 'PieceSeparator' carry text the language's
--   own joining behavior contributed and no morpheme owns. Keeping the
--   latter two as pieces rather than folding them into a neighbour is
--   what lets a caller reconstruct the stored name EXACTLY while still
--   reporting each morpheme's own spelling honestly.
data SurfacePiece
    = PieceMorpheme !SlotRole !ConceptId !Bool !Text
      -- ^ slot, concept, whether the realized form is that concept's
      --   BOUND form (#1096), realized surface
    | PieceMark !SlotRole !SlotMark !Text
      -- ^ the slot marked, which marking, and the affix as realized
    | PieceLink !Text
      -- ^ a segment #1095's boundary repair inserted
    | PieceSeparator !Text
      -- ^ a 'JoinHyphen' profile's compound separator
    deriving (Show, Eq)

pieceSurface ∷ SurfacePiece → Text
pieceSurface (PieceMorpheme _ _ _ t) = t
pieceSurface (PieceMark _ _ t)       = t
pieceSurface (PieceLink t)           = t
pieceSurface (PieceSeparator t)      = t

-- | The rendered name as its ordered realized pieces — the explanatory
--   half of 'renderNative', and its IMPLEMENTATION rather than a
--   parallel one. 'renderNative' is @'traceSurface' <$>@ this, so the
--   two cannot drift and \"concatenating the trace reproduces the
--   stored name\" is true by construction, not by convention (#1104
--   requirement 3).
--
--   Capitalization is a surface-POSITION effect: 'capitalizeWord'
--   uppercases the first character of the whole joined word, so the
--   leading piece carries it while every concept's canonical free
--   spelling stays the unmarked root. Nothing downstream may treat that
--   difference as lexical.
renderNativeTrace
    ∷ Profile → LanguageRoots → NameExpr
    → Either NativeRenderError [SurfacePiece]
renderNativeTrace prof roots expr = capitalizeTrace ⊚ case expr of
    Bare c → do
        r ← look c
        pure [PieceMorpheme SlotHead c False r]
    Modifier m h → do
        sm ← dependentStretch SlotModifier m
        sh ← headStretch h
        pure $ orderPair prof sm sh
    Of h num c → do
        sh ← headStretch h
        sc ← dependentStretch SlotComplement c
        pure $ orderPair prof (applyNumberTrace prof SlotComplement num sc) sh
    Possessive o h → do
        so ← dependentStretch SlotOwner o
        sh ← headStretch h
        pure $ orderGenitive prof SlotOwner so sh
  where
    look cid = case M.lookup cid (lrFree roots) of
        Nothing → Left (NativeUnknownConcept cid)
        Just r  → Right r

    headStretch cid = do
        r ← look cid
        pure [PieceMorpheme SlotHead cid False r]

    -- The free lookup runs FIRST even when a bound form exists, so an
    -- unknown concept stays the descriptive failure it always was
    -- rather than being masked by a bound map that happened to carry it.
    dependentStretch role cid = do
        r ← look cid
        pure $ case M.lookup cid (lrBound roots) of
            Just b  → [PieceMorpheme role cid True b]
            Nothing → [PieceMorpheme role cid False r]

-- | A trace's realized text — what the name actually is.
traceSurface ∷ [SurfacePiece] → Text
traceSurface = T.concat ∘ map pieceSurface

-- | Uppercase the first character of the first piece that has one,
--   exactly as 'capitalizeWord' does over the joined word. Only the
--   leading piece can be affected: every piece after it is preceded by
--   at least one character.
capitalizeTrace ∷ [SurfacePiece] → [SurfacePiece]
capitalizeTrace [] = []
capitalizeTrace (p : ps)
    | T.null (pieceSurface p) = p : capitalizeTrace ps
    | otherwise               = mapPiece capitalizeWord p : ps

mapPiece ∷ (Text → Text) → SurfacePiece → SurfacePiece
mapPiece f (PieceMorpheme role cid bound t) = PieceMorpheme role cid bound (f t)
mapPiece f (PieceMark role mark t)          = PieceMark role mark (f t)
mapPiece f (PieceLink t)                    = PieceLink (f t)
mapPiece f (PieceSeparator t)               = PieceSeparator (f t)

-- | Join two already-realized stretches through 'joinMorphemesTrace',
--   which reports what the repair inserted and how the right stretch
--   actually surfaces. A simplification shortens the right stretch by
--   one character, and that character always belongs to its FIRST
--   nonempty piece.
joinStretch ∷ Profile → [SurfacePiece] → [SurfacePiece] → [SurfacePiece]
joinStretch prof left right
    | T.null link ∧ realized ≡ traceSurface right = left ⧺ right
    | T.null link                                 = left ⧺ dropLeading right
    | otherwise                                   = left ⧺ [PieceLink link] ⧺ right
  where
    (link, realized) = joinMorphemesTrace prof (traceSurface left) (traceSurface right)

    dropLeading []       = []
    dropLeading (p : ps)
        | T.null (pieceSurface p) = p : dropLeading ps
        | otherwise               = mapPiece (T.drop 1) p : ps

-- | Apply explicit-number marking to a slot: unchanged when singular,
--   the profile's plural affix appended when plural. The bare root is
--   always a prefix of the result (#710 requirement 9 — grammatical
--   marking affixes, never replaces, the stem).
applyNumberTrace
    ∷ Profile → SlotRole → GramNumber → [SurfacePiece] → [SurfacePiece]
applyNumberTrace _    _    Singular s = s
applyNumberTrace prof role Plural   s =
    markTrace prof role MarkPlural (plmAffix (profPlural prof)) s

-- | Attach a grammatical affix to an already-realized stem through
--   #1095's boundary phonology, reporting the affix as its own piece so
--   an explanation can separate the lexical form from the marking.
--
--   A possessive affix that leads with its own apostrophe already
--   separates the two morphemes' letters, so it is appended unchanged —
--   the apostrophe survives exactly once, and no letters face each other
--   across the boundary for a repair to mediate. Every other affix meets
--   the stem's final letter directly and goes through 'joinStretch',
--   which never touches the LEFT side: the bare root therefore stays a
--   prefix of the marked form, and a repair can only insert a linking
--   segment or trim the affix's own initial one — never erase the mark.
markTrace
    ∷ Profile → SlotRole → SlotMark → Text → [SurfacePiece] → [SurfacePiece]
markTrace prof role mark affix stem = case T.uncons affix of
    Just ('\'', _) → stem ⧺ [PieceMark role mark affix]
    _              → joinStretch prof stem [PieceMark role mark affix]

applyPluralMark ∷ Profile → Text → Text
applyPluralMark prof = markedText prof MarkPlural (plmAffix (profPlural prof))

-- | Apply possessive marking to an owner's root. The bare root is
--   always a prefix of the result, same guarantee as 'applyPluralMark'.
applyPossessiveMark ∷ Profile → Text → Text
applyPossessiveMark prof =
    markedText prof MarkPossessive (pmAffix (profPossessive prof))

-- | The marked form of a bare stem, as text — the two exported
--   single-root helpers' shared body, so affix attachment has exactly
--   ONE implementation whether it is reached through a rendered name or
--   directly. The stem is wrapped as a single piece purely so
--   'markTrace' can run over it and only its concatenated text escapes;
--   which piece kind carries it is unobservable here, and 'joinStretch'
--   never rewrites its LEFT side, so the stem is returned intact.
markedText ∷ Profile → SlotMark → Text → Text → Text
markedText prof mark affix stem =
    traceSurface (markTrace prof SlotHead mark affix [PieceLink stem])

-- | Order a (modifier-slot, head-slot) pair per 'profCompoundOrder' and
--   join them per 'profJoin'.
orderPair ∷ Profile → [SurfacePiece] → [SurfacePiece] → [SurfacePiece]
orderPair prof modSlot headSlot = case profCompoundOrder prof of
    ModifierFirst → joinWords prof modSlot headSlot
    HeadFirst     → joinWords prof headSlot modSlot

-- | Order a marked-owner/head pair per 'profPossessive''s independent
--   genitive order, then join them per 'profJoin'.
orderGenitive
    ∷ Profile → SlotRole → [SurfacePiece] → [SurfacePiece] → [SurfacePiece]
orderGenitive prof role owner headSlot =
    let ownerMarked =
            markTrace prof role MarkPossessive
                      (pmAffix (profPossessive prof)) owner
    in case pmOrder (profPossessive prof) of
        OwnerFirst        → joinWords prof ownerMarked headSlot
        HeadFirstGenitive → joinWords prof headSlot ownerMarked

-- | Join two compound elements per 'profJoin'.
--
--   A compact join puts the two elements' letters in direct contact, so
--   it is mediated by #1095's boundary phonology. A hyphen join does not:
--   the separator is itself the boundary marker, it is preserved exactly
--   once, and no letter run can span it (three contiguous LETTERS is what
--   a triple run means, so @a-a@ is not one). Boundary rules apply around
--   a separator, never by removing it — nothing here can delete a hyphen.
joinWords ∷ Profile → [SurfacePiece] → [SurfacePiece] → [SurfacePiece]
joinWords prof a b = case profJoin prof of
    JoinCompact → joinStretch prof a b
    JoinHyphen  → a ⧺ [PieceSeparator "-"] ⧺ b

capitalizeWord ∷ Text → Text
capitalizeWord w = case T.uncons w of
    Nothing      → w
    Just (c, cs) → T.cons (toUpper c) cs
