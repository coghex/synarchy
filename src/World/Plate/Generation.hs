{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Deterministic plate generation, split out of "World.Plate" (issue
--   #560).
module World.Plate.Generation
    ( defaultPlatesFor
    , generatePlates
    ) where

import UPrelude
import World.Material (matGranite, matDiorite, matGabbro)
import World.Scale (WorldScale(..), computeWorldScale, scaleElev)
import World.Chunk.Types (chunkSize)
import World.Plate.Types (TectonicPlate(..))
import World.Plate.Hash (plateHash, hashToFloat', hashToRange)

-- * Plate Generation

-- | Default plate count scaled to worldSize. Linear in side length
--   (sub-linear in area), so plates get bigger with larger worlds
--   but not as fast as the area itself — preventing 10 fixed plates
--   from producing tiny noisy plates at worldSize=64 or huge
--   featureless continents at worldSize=512 (audit #17).
--
--   Calibrated so worldSize=128 yields ~10 plates (the legacy
--   default). Floor at 4 keeps even tiny worlds from collapsing
--   to a single landmass.
defaultPlatesFor ∷ Int → Int
defaultPlatesFor worldSize = max 4 (worldSize `div` 13)

generatePlates ∷ Word64 → Int → Int → [TectonicPlate]
generatePlates seed worldSize plateCount =
    map (generateOnePlate seed worldSize) [0 .. plateCount - 1]

generateOnePlate ∷ Word64 → Int → Int → TectonicPlate
generateOnePlate seed worldSize plateIndex =
    let wsc = computeWorldScale worldSize
        halfTiles = (worldSize * chunkSize) `div` 2
        h1 = plateHash seed plateIndex 1
        h2 = plateHash seed plateIndex 2
        h3 = plateHash seed plateIndex 3
        h4 = plateHash seed plateIndex 4
        h5 = plateHash seed plateIndex 5
        h6 = plateHash seed plateIndex 6
        h7 = plateHash seed plateIndex 7
        h8 = plateHash seed plateIndex 8

        cx = hashToRange h1 (-halfTiles) (halfTiles - 1)
        cy = hashToRange h2 (-halfTiles) (halfTiles - 1)

        isLand = hashToFloat' h3 > 0.4

        -- Raw values are in meters. Convert to tiles at 10m/tile,
        -- then apply world-size scaling.
        --   Land:  200–1000m  → 20–100 tiles (200m floor keeps small worlds
        --                       above sea level — see plateBaseScale below)
        --   Ocean: -6000– -3000m → -600– -300 tiles
        --
        -- Base elevation scales sub-linearly with world size (sqrt) so a
        -- w32 world's plate sits at ~25 tiles rather than ~6, giving each
        -- plate a recognizable plateau above sea level. Mountains still
        -- shrink (see boundaryHeightScale in convergentEffect/divergentEffect)
        -- but plate baselines stay perceptible.
        metersPerTile = 10.0 ∷ Float
        plateBaseScale = sqrt (wsScale wsc) ∷ Float
        baseElev = if isLand
                   then round (plateBaseScale
                              * fromIntegral (hashToRange h4 200 1000)
                              / metersPerTile)
                   else round (scaleElev wsc
                              (fromIntegral (hashToRange h4 (-6000) (-3000))
                               / metersPerTile))

        matChoice = hashToRange h5 0 2
        material = case matChoice of
            0 → matGranite
            1 → matDiorite
            _ → matGabbro

        density = if isLand
                  then 2.7 + hashToFloat' h8 * 0.2
                  else 3.0 + hashToFloat' h8 * 0.3

        driftX = hashToFloat' h6 * 2.0 - 1.0
        driftY = hashToFloat' h7 * 2.0 - 1.0

    in TectonicPlate
        { plateCenterX  = cx
        , plateCenterY  = cy
        , plateIsLand   = isLand
        , plateBaseElev = baseElev
        , plateMaterial = material
        , plateDensity  = density
        , plateDriftX   = driftX
        , plateDriftY   = driftY
        }
