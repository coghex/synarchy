{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.River
    ( evolveRiver
    , applyRiverCarve
    , applyRiverEvolution
    , carveFromSegment
    , computeDeltaDeposit'
    ) where

import World.Hydrology.River.Evolution (evolveRiver)
import World.Hydrology.River.Carving (applyRiverCarve, applyRiverEvolution
                                     , carveFromSegment, computeDeltaDeposit')
