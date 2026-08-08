{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | The persisted etymology source (#1104 requirement 1): the small,
--   optional record a generated name carries so its decomposition can
--   later be RECONSTRUCTED rather than guessed.
--
--   /Why anything is persisted at all./ A displayed name is one word,
--   and its English gloss is a translation of the whole — neither
--   states which morpheme meant what. Recovering that by parsing the
--   stored string would mean re-deriving roots from spelling, which
--   #1104 requirement 3 forbids and which cannot be done honestly
--   anyway: #1095's boundary repair inserts and deletes segments, and
--   #1096's bound forms are shortenings, so the letters on screen are
--   not the letters in the lexicon. The expression is therefore stored,
--   and the surface is re-rendered from it through the SAME versioned
--   morphology that produced it.
--
--   /Why the provenance travels with it./ An expression alone names
--   concepts, not a language; the same @Modifier(ASH, LAND)@ renders
--   differently under every seed and every generator version. Storing
--   the #1092 'LanguageProvenance' beside it is what makes the source
--   self-contained — \"enough identity information to resolve the exact
--   persisted language provenance that rendered it\" — so a river, a
--   location, and a world are all explained the same way with no
--   ambient lookup that could resolve to a DIFFERENT language than the
--   one that did the rendering.
--
--   /What is deliberately NOT here./ No precomputed morpheme list, no
--   displayed spellings, no gloss fragments (requirement 1). Those are
--   derived presentation, reconstructed on query.
--
--   /Optional by construction./ A custom name has no source, and every
--   save written before this landed decodes with the field absent. Both
--   are ordinary states, never repaired by inference (requirement 7).
--
--   This is a LEAF module by design: it imports only the two type
--   layers a source is made of, so 'World.Page.Types',
--   'Location.Instance', and 'World.River.Naming' can all embed it
--   without dragging the rendering machinery into their import graphs.
module Language.Etymology.Source
    ( EtymologySource(..)
      -- * Wire form
    , encodeNameExpr
    , decodeNameExpr
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import qualified Data.Text as T
import Language.Semantic.Types (ConceptId(..), GramNumber(..), NameExpr(..))
import Language.Generated.Types (LanguageProvenance)

-- | What a generated name remembers about how it came to be.
data EtymologySource = EtymologySource
    { esExpr     ∷ !NameExpr
      -- ^ the ORIGINAL expression this name was rendered from — the one
      --   #709 value both the native name and the English gloss came
      --   out of, never re-derived from either
    , esLanguage ∷ !LanguageProvenance
      -- ^ the language that rendered it, seed and generator version
      --   together (#1092), so the profile is reconstructible from the
      --   source alone
    } deriving (Show, Eq, Generic, NFData, Serialize)

-- * Wire form ---------------------------------------------------------

-- | An expression as one compact text token, for the surfaces that
--   cannot carry a structured value: @world.init@'s optional argument
--   and @world.suggestName@'s reply travel through Lua, where a
--   'NameExpr' has no representation.
--
--   The grammar is positional and colon-separated:
--
--   > Bare:LAND
--   > Modifier:ASH:LAND
--   > Of:EYE:plural:STORM
--   > Possessive:WOLF:HEART
--
--   A colon is a safe separator because a 'ConceptId' is uppercase
--   ASCII letters, digits, and underscores only ("Language.Semantic.Types"),
--   so no id can contain one.
--
--   This is a TRANSPORT encoding, not the persisted one: what lands in
--   a save is 'EtymologySource''s own @Generic Serialize@ shape.
encodeNameExpr ∷ NameExpr → Text
encodeNameExpr expr = case expr of
    Bare c         → T.intercalate ":" ["Bare", cid c]
    Modifier m h   → T.intercalate ":" ["Modifier", cid m, cid h]
    Of h n c       → T.intercalate ":" ["Of", cid h, num n, cid c]
    Possessive o h → T.intercalate ":" ["Possessive", cid o, cid h]
  where
    cid = conceptIdText
    num Singular = "singular"
    num Plural   = "plural"

-- | Inverse of 'encodeNameExpr'. 'Nothing' for anything that is not
--   exactly one of the four shapes above — a caller turns that into
--   \"this name has no etymology source\", never into a guessed
--   expression (#1104 requirement 7).
--
--   Concept ids are NOT validated against a catalogue here: this layer
--   has none, and an id that no longer exists is requirement 7's
--   \"referenced concept is unavailable or invalid\" case, reported at
--   decomposition time with the reason attached rather than silently
--   dropped at parse time.
decodeNameExpr ∷ Text → Maybe NameExpr
decodeNameExpr raw = case T.splitOn ":" raw of
    ["Bare", c]             → Bare <$> concept c
    ["Modifier", m, h]      → Modifier <$> concept m <*> concept h
    ["Of", h, n, c]         → Of <$> concept h <*> number n <*> concept c
    ["Possessive", o, h]    → Possessive <$> concept o <*> concept h
    _                       → Nothing
  where
    concept t
        | T.null t  = Nothing
        | otherwise = Just (ConceptId t)
    number "singular" = Just Singular
    number "plural"   = Just Plural
    number _          = Nothing
