{-# LANGUAGE Strict #-}
-- | Explaining a generated name (#1104, epic #708's final surface): the
--   ONE canonical decomposition path world, location, and river names
--   all feed, turning a stored name plus its persisted
--   'EtymologySource' into an ordered account of which morpheme meant
--   what.
--
--   /Read-only, and provably about the stored name./ Nothing here
--   writes. The reconstruction is re-rendered from the source through
--   'Language.Generated.Render.renderNativeTrace' — the same function
--   'Language.Generated.Render.renderNative' itself is defined in terms
--   of — and is then CHECKED against the authoritative stored text
--   before any of it is shown. A mismatch reports
--   'EtyUnavailable' rather than a plausible-looking
--   decomposition of a name the player is not actually looking at
--   (requirement 2). That check is what makes this safe to run against
--   names rendered by historical generator versions: if this build
--   would render the expression differently, it says so instead of
--   explaining the wrong word.
--
--   /Morpheme identity is semantic./ Two morphemes are the same
--   morpheme when their @(generated-language identity, 'ConceptId')@
--   pairs match, never when their letters do (requirement 4). That
--   single rule is what makes #1096's @kar-@ and @kara@ one morpheme
--   while two languages' accidental homographs stay unrelated — and it
--   is why 'MorphemeIdentity' carries the whole
--   'LanguageProvenance', generator version included: the same seed
--   under a different version is a different language, so its names do
--   not link (requirement 4's direct consequence).
--
--   /Nothing is parsed./ Roots are never recovered from the displayed
--   string. #1095's boundary repair inserts and deletes segments and
--   #1096's bound forms are shortenings, so the letters on screen are
--   not the letters in the lexicon; the expression is stored precisely
--   so this layer never has to guess.
--
--   Pure: no engine, world, Lua, IO, or wall-clock state. Callers
--   supply the 'Catalogue' and the entity's own stored values.
module Language.Etymology
    ( -- * Morpheme identity
      MorphemeIdentity(..)
    , morphemeIdentityText
      -- * Canonical result
    , EtyRole(..)
    , etyRoleText
    , EtyMark(..)
    , etyMarkText
    , EtyMorpheme(..)
    , EtyToken(..)
    , etyTokenKindText
    , etyTokenText
    , EtyForm(..)
    , etyFormText
    , Etymology(..)
    , EtyUnavailable(..)
    , etyUnavailableReason
    , etyUnavailableText
    , EtymologyResult(..)
      -- * Decomposition
    , decomposeName
    , decomposeEntityName
    , sourceMatchesPage
    , etymologyIdentities
    , etymologyMentions
    ) where

import UPrelude
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Semantic.Types
import Language.Generated.Types
    ( GeneratorError(..), LanguageProvenance(..), Profile
    , generatorErrorText, generatorVersionInt, langSeedText )
import Language.Generated.Profile (generateProfile)
import Language.Generated.Root (assignLanguageRoots)
import Language.Generated.Bound (LanguageRoots(..))
import Language.Generated.Render
    ( SlotMark(..), SlotRole(..), SurfacePiece(..)
    , nativeRenderErrorText, renderNativeTrace, traceSurface )
import Language.Etymology.Source (EtymologySource(..))

-- * Morpheme identity -------------------------------------------------

-- | What makes two morphemes the same morpheme (#1104 requirement 4):
--   the generated language they belong to, and the concept they mean.
--   Never the spelling.
--
--   The language half is the whole 'LanguageProvenance' — seed AND
--   generator version — because a version bump is a different
--   generator, hence a different language, hence a morpheme that only
--   coincidentally reads the same.
data MorphemeIdentity = MorphemeIdentity
    { miLanguage ∷ !LanguageProvenance
    , miConcept  ∷ !ConceptId
    } deriving (Show, Eq)

-- | A stable textual form of an identity, for the Lua boundary and for
--   diagnostics: @\<seed\>:\<version\>:\<CONCEPT\>@. The seed is decimal
--   TEXT because it is an unsigned 64-bit value (the same reason
--   @world.getLanguageProvenance@ reports one).
morphemeIdentityText ∷ MorphemeIdentity → Text
morphemeIdentityText mi = T.intercalate ":"
    [ langSeedText (lpSeed (miLanguage mi))
    , tshow (generatorVersionInt (lpVersion (miLanguage mi)))
    , conceptIdText (miConcept mi)
    ]

-- * Canonical result --------------------------------------------------

-- | The semantic role a morpheme fills in its expression. Mirrors
--   'SlotRole' at this layer's own vocabulary so consumers never import
--   the renderer.
data EtyRole = RoleHead | RoleModifier | RoleComplement | RoleOwner
    deriving (Show, Eq)

etyRoleText ∷ EtyRole → Text
etyRoleText RoleHead       = "head"
etyRoleText RoleModifier   = "modifier"
etyRoleText RoleComplement = "complement"
etyRoleText RoleOwner      = "owner"

-- | Grammatical marking applied to one slot after its lexical form was
--   selected (#1096 requirement 7's ordering, restated as explanation).
data EtyMark = EtyPlural | EtyPossessive
    deriving (Show, Eq)

etyMarkText ∷ EtyMark → Text
etyMarkText EtyPlural     = "plural"
etyMarkText EtyPossessive = "possessive"

-- | One explained morpheme (#1104 requirement 3's per-morpheme list).
data EtyMorpheme = EtyMorpheme
    { emIdentity   ∷ !MorphemeIdentity
      -- ^ stable identity — what recurrence matches on
    , emConcept    ∷ !ConceptId
    , emRole       ∷ !EtyRole
    , emSurface    ∷ !Text
      -- ^ realized spelling, exactly as it appears in the stored name
      --   (the leading morpheme carries the name's capitalization)
    , emFree       ∷ !Text
      -- ^ this concept's canonical FREE spelling in this language, so a
      --   bound form can be shown as a shortening of something
    , emBound      ∷ !Bool
      -- ^ whether 'emSurface' is the #1096 bound form
    , emLemma      ∷ !Text
      -- ^ English semantic lemma — the authored form this slot's own
      --   gloss rendering uses, so the reading matches the whole gloss
    , emMark       ∷ !(Maybe EtyMark)
    , emMarkSurface ∷ !(Maybe Text)
      -- ^ the marking affix as realized (a boundary repair may have
      --   trimmed its initial segment), present exactly when 'emMark' is
    } deriving (Show, Eq)

-- | One realized token of the final surface, in order. Concatenating
--   'etyTokenText' over the whole list reproduces the stored name
--   EXACTLY — capitalization, repaired boundaries, hyphens,
--   apostrophes, and grammatical markers included (requirement 3).
data EtyToken
    = TokenMorpheme !ConceptId !Text
    | TokenMark !EtyMark !Text
    | TokenLink !Text       -- ^ a segment #1095's boundary repair inserted
    | TokenSeparator !Text  -- ^ a hyphen-joining language's separator
    deriving (Show, Eq)

etyTokenKindText ∷ EtyToken → Text
etyTokenKindText TokenMorpheme{}  = "morpheme"
etyTokenKindText TokenMark{}      = "mark"
etyTokenKindText TokenLink{}      = "link"
etyTokenKindText TokenSeparator{} = "separator"

etyTokenText ∷ EtyToken → Text
etyTokenText (TokenMorpheme _ t)  = t
etyTokenText (TokenMark _ t)      = t
etyTokenText (TokenLink t)        = t
etyTokenText (TokenSeparator t)   = t

-- | Which #709 expression form the name was built from — reported so a
--   player can see that \"Eye of Storms\" and \"Wolf's Heart\" are
--   different constructions, not just different words.
data EtyForm = ExprBare | ExprModifier | ExprOfSingular | ExprOfPlural
             | ExprPossessive
    deriving (Show, Eq)

etyFormText ∷ EtyForm → Text
etyFormText ExprBare       = "bare"
etyFormText ExprModifier   = "modifier"
etyFormText ExprOfSingular = "of"
etyFormText ExprOfPlural   = "of-plural"
etyFormText ExprPossessive = "possessive"

formOfExpr ∷ NameExpr → EtyForm
formOfExpr Bare{}            = ExprBare
formOfExpr Modifier{}        = ExprModifier
formOfExpr (Of _ Singular _) = ExprOfSingular
formOfExpr (Of _ Plural _)   = ExprOfPlural
formOfExpr Possessive{}      = ExprPossessive

-- | A successful canonical decomposition (#1104 requirement 3).
data Etymology = Etymology
    { etyName      ∷ !Text
      -- ^ the UNCHANGED stored name, echoed rather than recomputed
    , etyGloss     ∷ !(Maybe Text)
      -- ^ the whole-name English gloss
    , etyLanguage  ∷ !LanguageProvenance
    , etyForm      ∷ !EtyForm
    , etyMorphemes ∷ ![EtyMorpheme]  -- ^ in rendered surface order
    , etyTokens    ∷ ![EtyToken]     -- ^ in rendered surface order
    } deriving (Show, Eq)

-- | Why a name cannot be explained (#1104 requirement 7). Every case is
--   reported honestly; none is papered over with a guess.
data EtyUnavailable
    = EtyCustomName
      -- ^ the name is a player's own, or a definition label — there is
      --   no generated language behind it (#708 principle 7)
    | EtyNoSource
      -- ^ no etymology source was persisted (a name predating #1104)
    | EtyNoProvenance
      -- ^ the entity's page records no language provenance
    | EtyForeignSource
      -- ^ the entity's stored source belongs to a DIFFERENT generated
      --   language than the page recorded (or to one at all, on a page
      --   that records none)
    | EtyUnsupportedVersion !Int
      -- ^ the stored generator version cannot be constructed
    | EtyInvalidConcept !ConceptId
      -- ^ a referenced concept is absent from the catalogue, or has no
      --   lexical form this expression needs
    | EtyReconstructionFailed !Text
    | EtySurfaceMismatch !Text !Text
      -- ^ stored name, reconstructed surface — the explanation would
      --   not have described the name actually on screen
    deriving (Show, Eq)

-- | A stable lowercase wire reason, for the Lua boundary and probes.
etyUnavailableReason ∷ EtyUnavailable → Text
etyUnavailableReason u = case u of
    EtyCustomName             → "custom"
    EtyNoSource               → "no_source"
    EtyNoProvenance           → "no_provenance"
    EtyForeignSource          → "foreign_source"
    EtyUnsupportedVersion _   → "unsupported_version"
    EtyInvalidConcept _       → "invalid_concept"
    EtyReconstructionFailed _ → "reconstruction_failed"
    EtySurfaceMismatch _ _    → "surface_mismatch"

-- | A player-facing explanation of the same. Never fabricates a
--   language, a meaning, or a partial decomposition.
etyUnavailableText ∷ EtyUnavailable → Text
etyUnavailableText u = case u of
    EtyCustomName →
        "this name was chosen rather than generated, so it has no roots"
    EtyNoSource →
        "this name was recorded before its meaning was kept"
    EtyNoProvenance →
        "this world has no generated language"
    EtyForeignSource →
        "this name's recorded meaning belongs to a different language \
        \than this world's, so it cannot explain it"
    EtyUnsupportedVersion v →
        "this name was written by language generator version "
        <> tshow v <> ", which this build cannot rebuild"
    EtyInvalidConcept (ConceptId cid) →
        "this name refers to the concept " <> cid
        <> ", which is no longer available"
    EtyReconstructionFailed why →
        "this name could not be rebuilt: " <> why
    EtySurfaceMismatch stored rebuilt →
        "rebuilding this name produced " <> rebuilt <> " rather than "
        <> stored <> ", so the explanation would not describe it"

data EtymologyResult
    = EtyAvailable !Etymology
    | EtyUnavailable !EtyUnavailable
    deriving (Show, Eq)

-- * Decomposition -----------------------------------------------------

-- | Explain one stored name.
--
--   @storedName@ and @storedGloss@ are the entity's own persisted
--   values and are echoed back untouched; @mSource@ is its optional
--   'EtymologySource'. Nothing else about the entity is consulted, and
--   nothing is written — which is what makes every adapter (world,
--   location, river) able to share this one path (requirement 3).
--
--   The order of the checks is the order requirement 7 lists them, and
--   the surface comparison is LAST: only a decomposition that has
--   already been rebuilt can be compared against the name it claims to
--   explain.
decomposeName
    ∷ Catalogue
    → Text                    -- ^ the authoritative stored name
    → Maybe Text              -- ^ the stored gloss, if any
    → Maybe EtymologySource
    → EtymologyResult
decomposeName _   _          _           Nothing    = EtyUnavailable EtyNoSource
decomposeName cat storedName storedGloss (Just src) =
    case generateProfile (lpVersion prov) (lpSeed prov) of
        Left (UnsupportedGeneratorVersion v) →
            EtyUnavailable (EtyUnsupportedVersion v)
        -- 'generateProfile' produces no other 'GeneratorError' — root
        -- capacity is decided later, by 'explain' — but the type admits
        -- one, so this branch exists rather than an incomplete match or
        -- an 'error'. It reports the same way a failed reconstruction
        -- does, keeping the wire reason set closed (#2206).
        Left err →
            EtyUnavailable (EtyReconstructionFailed (generatorErrorText err))
        Right prof → explain cat storedName storedGloss prov prof (esExpr src)
  where
    prov = esLanguage src

-- | Explain one stored name, given the language its PAGE records.
--
--   This is the entry point every adapter uses, and it adds the one
--   check 'decomposeName' structurally cannot make: that the source
--   sitting beside the name belongs to the same generated language the
--   page says named it.
--
--   The surface check alone is not enough. It proves the expression
--   renders to the stored text UNDER THE SOURCE'S OWN LANGUAGE — so a
--   source carrying some other language's provenance, whether stale,
--   hand-edited, or carried across from another page, is accepted the
--   moment it happens to reproduce those letters. What comes back then
--   looks fully validated while attributing every morpheme to a
--   language this world does not have, and #1104 requirement 4 keys
--   morpheme IDENTITY on that language: the recurrence links would be
--   computed in the wrong lexicon too.
--
--   A page with no provenance at all admits no source for the same
--   reason — there is nothing for one to agree with, and requirement 1
--   is explicit that absence is never repaired by inference.
decomposeEntityName
    ∷ Catalogue
    → Maybe LanguageProvenance  -- ^ the page's own recorded language
    → Text → Maybe Text → Maybe EtymologySource
    → EtymologyResult
decomposeEntityName cat pageProv storedName storedGloss mSource
    | Just src ← mSource
    , not (sourceMatchesPage pageProv src)
    = EtyUnavailable EtyForeignSource
    | otherwise
    = decomposeName cat storedName storedGloss mSource

-- | Whether a stored source belongs to the language the page recorded.
--   Exact equality on the whole 'LanguageProvenance' — seed AND
--   generator version — because two versions of one seed are two
--   languages (requirement 4), so a version drift is as much a mismatch
--   as a seed one.
sourceMatchesPage ∷ Maybe LanguageProvenance → EtymologySource → Bool
sourceMatchesPage pageProv src = pageProv ≡ Just (esLanguage src)

explain
    ∷ Catalogue → Text → Maybe Text → LanguageProvenance → Profile
    → NameExpr → EtymologyResult
explain cat storedName storedGloss prov prof expr
    -- A concept the catalogue no longer carries — or one missing the
    -- lexical form this slot needs — is reported as itself rather than
    -- as a generic rebuild failure, because it is the one unavailable
    -- case a content change can cause and a player-facing message can
    -- name.
    | (bad : _) ← missingConcepts = EtyUnavailable (EtyInvalidConcept bad)
    | otherwise = case assignLanguageRoots prof (catOrdinals cat)
                                            (conceptIds cat) of
        -- A profile whose root space cannot name the catalogue (#2206)
        -- has no assignment to rebuild against, so the etymology is
        -- unavailable — reported through the EXISTING
        -- @reconstruction_failed@ reason carrying the generator's own
        -- text, rather than through a new wire reason every consumer
        -- would have to learn.
        Left gErr → EtyUnavailable
            (EtyReconstructionFailed (generatorErrorText gErr))
        Right roots → case renderNativeTrace prof roots expr of
            Left err → EtyUnavailable
                (EtyReconstructionFailed (nativeRenderErrorText err))
            Right pieces
                | rebuilt ≢ storedName →
                    EtyUnavailable (EtySurfaceMismatch storedName rebuilt)
                | otherwise → EtyAvailable Etymology
                    { etyName      = storedName
                    , etyGloss     = storedGloss
                    , etyLanguage  = prov
                    , etyForm      = formOfExpr expr
                    , etyMorphemes = morphemesOf roots pieces
                    , etyTokens    = map toToken pieces
                    }
              where rebuilt = traceSurface pieces
  where
    -- Every (concept, lexical form) pair this expression's own gloss
    -- rendering demands, checked against the catalogue up front. The
    -- native side needs only a root, which the assignment above covers
    -- for every catalogue concept — so a concept that is present but
    -- lexically incomplete would otherwise surface as a missing lemma
    -- rather than as the unavailable case requirement 7 names.
    missingConcepts =
        [ cid
        | (cid, kind) ← slotForms expr
        , case lookupConcept cid cat of
            Nothing → True
            Just ce → maybe True T.null (formOf kind ce)
        ]

    toToken (PieceMorpheme _ cid _ t) = TokenMorpheme cid t
    toToken (PieceMark _ mark t)      = TokenMark (toMark mark) t
    toToken (PieceLink t)             = TokenLink t
    toToken (PieceSeparator t)        = TokenSeparator t

    -- One entry per MORPHEME piece, in surface order, each carrying the
    -- marking that landed on its own slot. The mark is found by role
    -- rather than by adjacency: a hyphen-joining or repaired boundary
    -- can put a link between a stem and its affix, and #1104
    -- requirement 6 keeps roles attached to concepts under either
    -- ordering, so role is the only association that stays correct.
    morphemesOf roots pieces =
        [ EtyMorpheme
            { emIdentity    = MorphemeIdentity prov cid
            , emConcept     = cid
            , emRole        = toRole role
            , emSurface     = surface
            , emFree        = freeForm roots cid
            , emBound       = bound
            , emLemma       = lemmaFor cid (slotFormKind expr role)
            , emMark        = fst <$> markFor role pieces
            , emMarkSurface = snd <$> markFor role pieces
            }
        | PieceMorpheme role cid bound surface ← pieces ]

    markFor role pieces = listToMaybe
        [ (toMark mark, t) | PieceMark r mark t ← pieces, r ≡ role ]

    -- The concept's own free root in THIS language. Total for every
    -- concept that reached a rendered piece: the trace could only have
    -- produced one by looking the concept up in exactly this map.
    freeForm roots cid = fromMaybe "" (M.lookup cid (lrFree roots))

    lemmaFor cid kind = case lookupConcept cid cat of
        Nothing → ""
        Just ce → fromMaybe "" (formOf kind ce)

toRole ∷ SlotRole → EtyRole
toRole SlotHead       = RoleHead
toRole SlotModifier   = RoleModifier
toRole SlotComplement = RoleComplement
toRole SlotOwner      = RoleOwner

toMark ∷ SlotMark → EtyMark
toMark MarkPlural     = EtyPlural
toMark MarkPossessive = EtyPossessive

-- | The (concept, English form) pairs an expression's gloss rendering
--   demands — the exact set 'Language.Semantic.English.renderGloss'
--   looks up, kept in step with it so \"invalid concept\" and \"the
--   gloss would not render\" can never disagree.
slotForms ∷ NameExpr → [(ConceptId, FormKind)]
slotForms (Bare c)         = [(c, FormSingular)]
slotForms (Modifier m h)   = [(m, FormModifier), (h, FormSingular)]
slotForms (Of h n c)       = [(h, FormSingular), (c, numberFormKind n)]
slotForms (Possessive o h) = [(o, FormPossessive), (h, FormSingular)]

-- | Which English form a given SLOT of a given expression reads
--   through. Mirrors 'slotForms' by role rather than by position.
slotFormKind ∷ NameExpr → SlotRole → FormKind
slotFormKind _              SlotHead       = FormSingular
slotFormKind _              SlotModifier   = FormModifier
slotFormKind (Of _ n _)     SlotComplement = numberFormKind n
slotFormKind _              SlotComplement = FormSingular
slotFormKind _              SlotOwner      = FormPossessive

-- | Every morpheme identity a decomposition contains, deduplicated and
--   in surface order — what a recurrence query matches against.
etymologyIdentities ∷ Etymology → [MorphemeIdentity]
etymologyIdentities = L.nub ∘ map emIdentity ∘ etyMorphemes

-- | Whether a decomposition contains a given morpheme (#1104
--   requirement 9). By identity alone: identical-looking text under a
--   different language or a different concept is not a recurrence.
etymologyMentions ∷ MorphemeIdentity → Etymology → Bool
etymologyMentions mi = any ((≡ mi) ∘ emIdentity) ∘ etyMorphemes
