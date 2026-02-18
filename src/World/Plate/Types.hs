{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Plate.Types
    ( TectonicPlate(..)
    ) where

import UPrelude
import World.Material (MaterialId(..))

data TectonicPlate = TectonicPlate
    { plateCenterX  ∷ !Int
    , plateCenterY  ∷ !Int
    , plateIsLand   ∷ !Bool
    , plateBaseElev ∷ !Int
    , plateMaterial ∷ !MaterialId
    , plateDensity  ∷ !Float
    , plateDriftX   ∷ !Float
    , plateDriftY   ∷ !Float
    } deriving (Show, Eq)
