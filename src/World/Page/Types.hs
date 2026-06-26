{-# LANGUAGE Strict, UnicodeSyntax, GeneralizedNewtypeDeriving, DerivingStrategies #-}
module World.Page.Types
    ( WorldPageId(..)
    ) where

import UPrelude
import Data.Hashable (Hashable)

newtype WorldPageId = WorldPageId Text
    deriving (Show, Eq, Ord)
    deriving newtype (Hashable)
