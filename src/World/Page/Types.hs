{-# LANGUAGE Strict, UnicodeSyntax, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
module World.Page.Types
    ( WorldPageId(..)
    , WorldIdentity(..)
    , mkWorldIdentity
    ) where

import UPrelude
import qualified Data.Text as T
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

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
    { wiName  ∷ !Text          -- ^ Non-empty display name (stripped).
    , wiGloss ∷ !(Maybe Text)  -- ^ Optional English gloss (stripped).
    } deriving (Show, Eq, Generic, Serialize)

-- | Normalize raw display-name / gloss input into an identity. Each
--   string is trimmed of leading/trailing Unicode whitespace
--   ('T.strip'); interior whitespace, punctuation, and capitalization
--   are preserved exactly. An omitted or whitespace-only display name
--   means NO identity — a gloss cannot exist alone, so any supplied
--   gloss is discarded with it. An omitted or whitespace-only gloss is
--   simply dropped from an otherwise-valid identity.
mkWorldIdentity ∷ Maybe Text → Maybe Text → Maybe WorldIdentity
mkWorldIdentity mName mGloss = case fmap T.strip mName of
    Just n | not (T.null n) → Just (WorldIdentity n gloss)
    _                       → Nothing
  where
    gloss = case fmap T.strip mGloss of
        Just g | not (T.null g) → Just g
        _                       → Nothing
