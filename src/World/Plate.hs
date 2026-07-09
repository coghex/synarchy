{-# LANGUAGE UnicodeSyntax #-}
-- | Tectonic plate generation, queries, and elevation, split (issue
--   #560) into focused submodules under "World.Plate.*":
--
--     * "World.Plate.Types" — the 'TectonicPlate' record.
--     * "World.Plate.Hash" — deterministic hashing / interpolation helpers.
--     * "World.Plate.Wrap" — cylindrical world-wrap coordinate math.
--     * "World.Plate.Noise" — wrap-exact value/ridge noise.
--     * "World.Plate.Generation" — deterministic plate generation.
--     * "World.Plate.Glacier" — polar glacier-border tests.
--     * "World.Plate.Query" — nearest-plate lookup and boundary classification.
--     * "World.Plate.Coast" — coastal steepness (#220) and inland rift (#223) fields.
--     * "World.Plate.Profiles" — boundary elevation profiles + continental shelf.
--     * "World.Plate.Elevation" — the global per-tile elevation query.
--
--   This module re-exports the public API unchanged.
module World.Plate
    ( -- * Plate data
      TectonicPlate(..)
    , generatePlates
    , defaultPlatesFor
      -- * Queries
    , twoNearestPlates
    , elevationAtGlobal
      -- * Boundary classification
    , BoundaryType(..)
    , classifyBoundary
    , isGlacierZone
    , isBeyondGlacier
      -- * Coastal tectonic steepness (#220)
    , coastCellSize
    , coastCellsInU
    , coastCellSteepness
    , coastSteepAt
    , coastTectonicSteepness
      -- * Inland rift intensity (#223)
    , riftCellIntensity
    , riftTectonicIntensity
    , riftFieldMemo
      -- * wrapping
    , wrapGlobalX
    , wrapGlobalU
    , worldWidthTiles
      -- * noise
    , wrappedValueNoise2D
    ) where

import World.Plate.Types (TectonicPlate(..))
import World.Plate.Generation (generatePlates, defaultPlatesFor)
import World.Plate.Query (twoNearestPlates, BoundaryType(..), classifyBoundary)
import World.Plate.Glacier (isGlacierZone, isBeyondGlacier)
import World.Plate.Coast
    ( coastCellSize
    , coastCellsInU
    , coastCellSteepness
    , coastSteepAt
    , coastTectonicSteepness
    , riftCellIntensity
    , riftTectonicIntensity
    , riftFieldMemo
    )
import World.Plate.Wrap (wrapGlobalX, wrapGlobalU, worldWidthTiles)
import World.Plate.Noise (wrappedValueNoise2D)
import World.Plate.Elevation (elevationAtGlobal)
