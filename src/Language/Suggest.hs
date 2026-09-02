{-# LANGUAGE Strict #-}
-- | Suggesting a world's name in a generated language (#1106, epic
--   #708). This is the PRODUCER the naming arc was built toward: the
--   Create World dice button asks for suggestion /n/ of a world seed's
--   own language and gets back one 'NameExpr' rendered twice — natively
--   ("Karadun") and as its English gloss ("Ashen Land") — plus the
--   #1092 'LanguageProvenance' that says which language produced them,
--   so an accepted suggestion can be recorded and later explained.
--
--   The three layers below are already built and unchanged here: #709's
--   language-independent 'NameExpr' over stable 'ConceptId's, #710's
--   generated 'Profile' + concept roots, and #1092's provenance record.
--   This module only decides WHICH expression a given (language, reroll
--   ordinal) gets. It is the world-name counterpart of
--   "Location.Naming", which does the same job for a placed location.
--
--   /Deterministic sequence./ A suggestion is a pure function of the
--   generator version, the language seed, and the reroll ordinal — never
--   of a random generator, wall clock, or traversal order. The same
--   world seed therefore always offers the same suggestions in the same
--   order, and the language is fixed for the whole sequence: rerolling
--   changes the MEANING while the phonology stays put, which is what
--   communicates that a language exists at all (#1106 requirement 2).
--
--   /Adjacent rerolls always differ./ Rather than drawing freely and
--   hoping, the head concept of ordinal /n/ is drawn from the pool with
--   ordinal /n-1/'s head REMOVED ('headIndexAt'), so consecutive
--   suggestions can never share a head. That makes a repeat structurally
--   impossible instead of merely unlikely, in both renderings:
--
--     * the expressions differ because every 'NameExpr' form carries its
--       head in a field of its own;
--
--     * the glosses differ because
--       'Language.Semantic.English.renderGloss' always renders the head
--       through its @singular@ form and places it LAST, the catalogue
--       rejects two concepts sharing a singular case-insensitively, and
--       it rejects whitespace inside any authored form — so distinct
--       heads give distinct final words, and a gloss's word count
--       (1 for @Bare@, 2 for @Modifier@\/@Possessive@, 3 for @Of@)
--       cannot disguise that.
--
--   /Pure./ No engine, world, Lua, IO, wall clock, or floating point
--   (#710 requirement 12) — the caller supplies the 'Catalogue' and the
--   'LanguageProvenance', and everything else is integer hashing.
module Language.Suggest
    ( NameSuggester(..)
    , NameSuggestion(..)
    , SuggestError(..)
    , suggestErrorText
    , worldLanguageSeed
    , mkNameSuggester
    , suggestionExprAt
    , suggestNameAt
    ) where

import UPrelude
import Language.Semantic.Types
import Language.Semantic.English (renderGloss)
import Language.Generated.Types
import Language.Generated.Profile (generateProfile)
import Language.Generated.Root (assignLanguageRoots)
import Language.Generated.Bound (LanguageRoots)
import Language.Generated.Render (renderNative, NativeRenderError, nativeRenderErrorText)
import Language.Generated.Hash (fmix64, draw, pickIndex)

-- | Everything needed to suggest names in ONE language, resolved once
--   per language rather than once per dice press: the 'Profile', its
--   concept→morpheme assignment, the catalogue the gloss is rendered
--   from, and the per-lexical-form concept pools a suggestion draws on.
--
--   Roots are assigned over the WHOLE catalogue, exactly as
--   'Location.Naming.LocationNamer' does — root assignment resolves
--   collisions across the full concept set, so a partial set would
--   silently be a different language and a world's own name would stop
--   agreeing with its locations' names.
data NameSuggester = NameSuggester
    { nsuProfile     ∷ !Profile
    , nsuRoots       ∷ !LanguageRoots
    , nsuCatalogue   ∷ !Catalogue
    , nsuHeads       ∷ ![ConceptId]
      -- ^ Every concept: @singular@ is the one mandatory form, so every
      --   concept can fill a head slot.
    , nsuModifiers   ∷ ![ConceptId]  -- ^ Concepts with a @modifier@ form.
    , nsuPlurals     ∷ ![ConceptId]  -- ^ Concepts with a @plural@ form.
    , nsuPossessives ∷ ![ConceptId]  -- ^ Concepts with a @possessive@ form.
    , nsuBase        ∷ !Word64       -- ^ The sequence's base draw seed.
    }

-- | One suggestion: the meaning, both of its renderings, and the
--   language that produced them. Seed and version travel WITH the text
--   because a suggestion the player accepts must be recordable as
--   #1092 provenance — the text alone cannot be traced back to a
--   language, and the game never infers one (#708 principle 7).
data NameSuggestion = NameSuggestion
    { nsExpr    ∷ !NameExpr
    , nsName    ∷ !Text              -- ^ Native name, e.g. @Karadun@.
    , nsGloss   ∷ !Text              -- ^ English gloss, e.g. @Ashen Land@.
    , nsSeed    ∷ !LangSeed
    , nsVersion ∷ !GeneratorVersion
    } deriving (Show, Eq)

-- | Why a suggestion could not be produced. Every case is reported to
--   the caller rather than papered over: #1106 requirement 7 forbids
--   falling back to the removed dummy word-list generator, and a silent
--   fallback would hand the player a name with no language behind it.
data SuggestError
    = SuggestNoConcepts
      -- ^ The catalogue has no concepts to name anything with.
    | SuggestGenerator !GeneratorError
      -- ^ The provenance names a generator version this build cannot
      --   construct a profile for, or one whose profile has too small a
      --   root space to name the catalogue (#2206).
    | SuggestNative !NativeRenderError
    | SuggestGloss !RenderError

suggestErrorText ∷ SuggestError → Text
suggestErrorText err = case err of
    SuggestNoConcepts →
        "the concept catalogue is empty, so no name can be suggested"
    SuggestGenerator gErr → generatorErrorText gErr
    SuggestNative nErr    → nativeRenderErrorText nErr
    SuggestGloss rErr     → renderErrorText rErr

-- | The language seed a world seed names its language with.
--
--   The input is the NORMALIZED numeric world seed — the same value
--   Create World hands to @world.init@, so two spellings of one seed
--   (@a3f7@ \/ @A3F7@) identify the same language, and a world's name
--   and its locations' names come from that one language.
--
--   Injective, which is what makes "a different seed is a different
--   language" true rather than merely likely: xor with a fixed constant
--   is a bijection on 'Word64', and so is 'fmix64' (each of its steps —
--   @x xor (x >> 33)@ and multiplication by an odd constant — is
--   invertible modulo @2^64@), so their composition is one too. That
--   covers the supported 32-bit seed input space with room to spare.
--
--   This is a DERIVATION, not an identity: the resulting 'LangSeed' is
--   what gets recorded as provenance, and nothing ever recovers a world
--   seed from it or re-derives a stored provenance from terrain state.
worldLanguageSeed ∷ Word64 → LangSeed
worldLanguageSeed s = LangSeed (fmix64 (s `xor` worldLanguageDomain))
  where
    -- ASCII "WlangSd1": a fixed domain tag in the same spirit as
    -- 'Language.Generated.Hash.boundSeed''s, so a world's LANGUAGE seed
    -- is separated from every other value derived from the same world
    -- seed.
    worldLanguageDomain = 0x576C616E67536431

-- | Build a suggester for one language. Fails when the provenance names
--   an unconstructible generator version (#710 requirement 15 — never
--   silently substituted with the current version, which would suggest
--   names in a DIFFERENT language than the one being recorded), when
--   that version's profile has too small a root space to name the
--   catalogue (#2206), or when the catalogue is empty.
mkNameSuggester
    ∷ Catalogue → LanguageProvenance → Either SuggestError NameSuggester
mkNameSuggester cat prov
    | null ids  = Left SuggestNoConcepts
    | otherwise = case generateProfile (lpVersion prov) (lpSeed prov) of
        Left gErr → Left (SuggestGenerator gErr)
        Right prof → case assignLanguageRoots prof (catOrdinals cat) ids of
            Left gErr → Left (SuggestGenerator gErr)
            Right roots → Right NameSuggester
                { nsuProfile     = prof
                , nsuRoots       = roots
                , nsuCatalogue   = cat
                , nsuHeads       = ids
                , nsuModifiers   = withForm FormModifier
                , nsuPlurals     = withForm FormPlural
                , nsuPossessives = withForm FormPossessive
                , nsuBase        = suggestionBase prof
                }
  where
    ids = conceptIds cat
    withForm k =
        [ cid | cid ← ids
              , Just ce ← [lookupConcept cid cat]
              , Just _  ← [formOf k ce] ]

-- | The base a language's whole suggestion sequence draws from: its
--   seed and generator version, domain-separated from every other value
--   derived from the same pair. Mirrors
--   'Language.Generated.Hash.conceptSeed''s shape and constants.
suggestionBase ∷ Profile → Word64
suggestionBase prof =
    fmix64 $ langSeedWord (profSeed prof)
       `xor` fmix64 (fromIntegral (generatorVersionInt (profVersion prof))
                        * 0x9E3779B97F4A7C15)
       `xor` worldNameDomain
  where
    -- ASCII "WorldNm1".
    worldNameDomain = 0x576F726C644E6D31

-- | Which draw a given (ordinal, slot) uses. Slots are strided so two
--   ordinals can never consume the same draw, however the shapes below
--   branch.
slotStep ∷ Int → Int → Int
slotStep ordinal slot = ordinal * slotsPerOrdinal + slot

slotsPerOrdinal ∷ Int
slotsPerOrdinal = 4

slotHead, slotShape, slotDependent ∷ Int
slotHead      = 0
slotShape     = 1
slotDependent = 2

-- | The head concept's index at one ordinal.
--
--   Ordinal 0 draws freely. Every later ordinal draws from the pool with
--   its PREDECESSOR's head removed — an index in @[0, n-1)@ shifted past
--   the previous one — so @headIndexAt base n k ≠ headIndexAt base n
--   (k-1)@ holds by construction, with no retry loop and no rejection
--   sampling. That is what makes #1106 requirement 2's "reroll produces
--   a different meaning" a guarantee rather than a probability.
--
--   Walking the chain from 0 costs a handful of integer operations per
--   ordinal and no rendering at all, so the whole cost of the /k/th dice
--   press is still one render.
--
--   A one-concept catalogue has no second head to move to and pins the
--   index at 0; the shipped catalogue has 151.
headIndexAt ∷ Word64 → Int → Int → Int
headIndexAt base n ordinal
    | n ≤ 1     = 0
    | otherwise = go 1 (pickIndex (draw base (slotStep 0 slotHead)) n)
  where
    go k prev
        | k > ordinal = prev
        | otherwise =
            let j = pickIndex (draw base (slotStep k slotHead)) (n - 1)
                i = if j ≥ prev then j + 1 else j
            in go (k + 1) i

-- | The name forms a suggestion can take. @Of@ appears twice because
--   its complement's grammatical number decides which authored form the
--   complement must have, so the number is part of choosing the shape
--   rather than a separate draw made afterwards.
data NameShape
    = ShapeBare
    | ShapeModifier
    | ShapeOfSingular
    | ShapeOfPlural
    | ShapePossessive
    deriving (Show, Eq)

-- | The expression suggested at one reroll ordinal, or 'Nothing' for a
--   catalogue with no concepts at all.
--
--   The dependent slot never repeats the head, so no suggestion reads
--   \"Ashen Ash\". A shape whose dependent pool is empty once the head
--   is excluded is simply not offered, which keeps this total: @Bare@
--   needs no dependent and is always available.
suggestionExprAt ∷ NameSuggester → Int → Maybe NameExpr
suggestionExprAt s ordinal0 = case nsuHeads s of
    []    → Nothing
    heads → Just (build heads)
  where
    ordinal = max 0 ordinal0
    base    = nsuBase s

    build heads =
        let hIdx  = headIndexAt base (length heads) ordinal
            hCid  = heads !! hIdx
            avail = availableShapes hCid
            shape = avail !! pickIndex (draw base (slotStep ordinal slotShape))
                                       (length avail)
        in expr hCid shape

    without cid = filter (≢ cid)

    poolFor cid shape = case shape of
        ShapeBare       → []
        ShapeModifier   → without cid (nsuModifiers s)
        ShapeOfSingular → without cid (nsuHeads s)
        ShapeOfPlural   → without cid (nsuPlurals s)
        ShapePossessive → without cid (nsuPossessives s)

    availableShapes cid =
        ShapeBare : [ sh | sh ← [ ShapeModifier, ShapeOfSingular
                                , ShapeOfPlural, ShapePossessive ]
                         , not (null (poolFor cid sh)) ]

    dependent cid shape = case poolFor cid shape of
        []           → cid   -- unreachable: an empty pool is never offered
        pool@(_ : _) →
            pool !! pickIndex (draw base (slotStep ordinal slotDependent))
                              (length pool)

    expr cid shape = case shape of
        ShapeBare       → Bare cid
        ShapeModifier   → Modifier (dependent cid shape) cid
        ShapeOfSingular → Of cid Singular (dependent cid shape)
        ShapeOfPlural   → Of cid Plural (dependent cid shape)
        ShapePossessive → Possessive (dependent cid shape) cid

-- | One suggestion, both renderings taken from the SAME expression so
--   the gloss always explains the name beside it.
--
--   Neither renderer has a reachable failure here — every concept comes
--   from this catalogue, the roots cover all of them, and a shape is
--   only offered when its slots' authored forms exist — but both are
--   reported rather than swallowed, because a silent fallback is exactly
--   what #1106 requirement 7 rules out.
suggestNameAt ∷ NameSuggester → Int → Either SuggestError NameSuggestion
suggestNameAt s ordinal = case suggestionExprAt s ordinal of
    Nothing → Left SuggestNoConcepts
    Just e  → case renderNative (nsuProfile s) (nsuRoots s) e of
        Left nErr → Left (SuggestNative nErr)
        Right native → case renderGloss (nsuCatalogue s) e of
            Left rErr → Left (SuggestGloss rErr)
            Right gloss → Right NameSuggestion
                { nsExpr    = e
                , nsName    = native
                , nsGloss   = gloss
                , nsSeed    = profSeed (nsuProfile s)
                , nsVersion = profVersion (nsuProfile s)
                }
