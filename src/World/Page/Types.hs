{-# LANGUAGE Strict, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
module World.Page.Types
    ( WorldPageId(..)
    , WorldIdentity(..)
    , mkWorldIdentity
    , mkGeneratedWorldIdentity
    ) where

import UPrelude
import qualified Data.Text as T
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Language.Generated.Types (LanguageProvenance)
import Language.Etymology.Source (EtymologySource(..))
import Language.Semantic.Types (NameExpr)

-- | 'Serialize' is derived from the underlying 'Text' (instance in
--   UPrelude) so world-page ids can be persisted in saves — each page's
--   id plus the active/visible-page lists land in 'SaveData' (#215).
newtype WorldPageId = WorldPageId Text
    deriving (Show, Eq, Ord)
    deriving newtype (Hashable, Serialize)

-- | Optional, immutable player-facing identity of a world page (#707):
--   a non-empty display name plus an optional English gloss. This is
--   display TEXT, deliberately distinct from both the internal routing
--   'WorldPageId' (which load remaps — active page → @main_world@,
--   collisions → @\<id\>#N@) and the save-slot name validated by
--   'World.Save.Serialize.sanitizeSaveName' — no filename rules apply
--   here. Set only at page creation ('WorldInit') or by loading saved
--   state ('WorldPageSave'); there is no rename/setter API.
data WorldIdentity = WorldIdentity
    { wiName     ∷ !Text          -- ^ Non-empty display name (stripped).
    , wiGloss    ∷ !(Maybe Text)  -- ^ Optional English gloss (stripped).
    , wiLanguage ∷ !(Maybe LanguageProvenance)
        -- ^ Which generated language produced 'wiName'/'wiGloss', and
        --   under which generator version (#1092). Genuinely OPTIONAL:
        --   a player-entered name is stored verbatim and the game never
        --   infers meaning for it (#708 principle 7), so a custom-named
        --   world has a name and NO language. Absence is never papered
        --   over with a default seed, and provenance is never derived
        --   from the terrain seed or from the name text.
        --
        --   This exists to render NEW names in the same language and to
        --   explain existing ones — never to recompute them. 'wiName'
        --   and 'wiGloss' are already-rendered output (#708 principle
        --   5) and must not be regenerated from the recovered seed.
    , wiEtymology ∷ !(Maybe EtymologySource)
        -- ^ What 'wiName' was rendered FROM (#1104): the originating
        --   'Language.Semantic.Types.NameExpr' plus the provenance that
        --   rendered it, so the name can be decomposed into roots and
        --   meanings without parsing the displayed string.
        --
        --   Optional independently of 'wiLanguage', and narrower: a
        --   custom name has neither, while a generated name whose
        --   caller supplied provenance but no expression has language
        --   but no etymology. Absence is an ordinary state — every save
        --   written before #1104 decodes with it absent — and is never
        --   repaired by inferring an expression from the name, the
        --   gloss, or the world seed (#1104 requirement 1).
    } deriving (Show, Eq, Generic, Serialize)

-- | Normalize raw display-name / gloss input into an identity. Each
--   string is trimmed of leading/trailing Unicode whitespace
--   ('T.strip'); interior whitespace, punctuation, and capitalization
--   are preserved exactly. An omitted or whitespace-only display name
--   means NO identity — a gloss cannot exist alone, so any supplied
--   gloss is discarded with it. An omitted or whitespace-only gloss is
--   simply dropped from an otherwise-valid identity.
--
--   This is the CUSTOM-name path (the one @world.init@'s optional
--   display text takes), so the identity it builds always has ABSENT
--   language provenance — see 'mkGeneratedWorldIdentity' for the
--   generated one.
mkWorldIdentity ∷ Maybe Text → Maybe Text → Maybe WorldIdentity
mkWorldIdentity = mkIdentity Nothing Nothing

-- | The GENERATED-name path: identical normalization, plus the
--   provenance of the language that rendered the text (#1092). A
--   caller must supply provenance explicitly here — nothing infers it
--   from the name, the gloss, or any world-generation seed.
--
--   The originating expression (#1104) is supplied the same way and is
--   independently optional: a caller that knows the provenance but not
--   the expression records a language with no etymology rather than a
--   guessed one.
mkGeneratedWorldIdentity
    ∷ Maybe Text → Maybe Text → LanguageProvenance → Maybe NameExpr
    → Maybe WorldIdentity
mkGeneratedWorldIdentity mName mGloss prov mExpr =
    mkIdentity (Just prov) (mkSource <$> mExpr) mName mGloss
  where
    mkSource expr = EtymologySource { esExpr = expr, esLanguage = prov }

-- | Shared normalization for both construction paths, so they can
--   never drift in what counts as a valid name/gloss. Deliberately
--   NOT exported: provenance is a required argument here, so every
--   identity is built through a path that states it explicitly.
mkIdentity
    ∷ Maybe LanguageProvenance → Maybe EtymologySource → Maybe Text
    → Maybe Text → Maybe WorldIdentity
mkIdentity prov mSource mName mGloss = case fmap T.strip mName of
    Just n | not (T.null n) → Just (WorldIdentity n gloss prov mSource)
    _                       → Nothing
  where
    gloss = case fmap T.strip mGloss of
        Just g | not (T.null g) → Just g
        _                       → Nothing
