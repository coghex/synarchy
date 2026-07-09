{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Hydrology.Simulation
    ( simulateHydrology
    , FlowResult(..)
    , ElevGrid(..)
    , buildInitialElevGrid
    , updateElevGrid
    , fillDepressions
    ) where

import World.Hydrology.Simulation.Types (FlowResult(..), ElevGrid(..))
import World.Hydrology.Simulation.Grid (buildInitialElevGrid, updateElevGrid)
import World.Hydrology.Simulation.PriorityFlood (fillDepressions)
import World.Hydrology.Simulation.Flow (simulateHydrology)
