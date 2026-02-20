{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.River
    ( applyRiverCarve
    , applyRiverEvolution
    , carveFromSegment
    , computeDeltaDeposit'
    ) where

import World.Hydrology.River.Carving (applyRiverCarve, applyRiverEvolution
                                     , carveFromSegment, computeDeltaDeposit')
