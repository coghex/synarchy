{-# LANGUAGE Strict, UnicodeSyntax, GeneralizedNewtypeDeriving, DerivingStrategies #-}
module World.Page.Types
    ( WorldPageId(..)
    ) where

import UPrelude
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)

-- | 'Serialize' is derived from the underlying 'Text' (instance in
--   UPrelude) so world-page ids can be persisted in saves — each page's
--   id plus the active/visible-page lists land in 'SaveData' (#215).
newtype WorldPageId = WorldPageId Text
    deriving (Show, Eq, Ord)
    deriving newtype (Hashable, Serialize)
