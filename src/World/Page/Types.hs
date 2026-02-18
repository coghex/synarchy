{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Page.Types
    ( WorldPageId(..)
    ) where

import UPrelude

newtype WorldPageId = WorldPageId Text
    deriving (Show, Eq, Ord)
