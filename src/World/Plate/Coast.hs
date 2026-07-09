{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Coastal tectonic steepness (#220) and inland rift intensity (#223)
--   fields, split out of "World.Plate" (issue #560).
module World.Plate.Coast
    ( -- * Coastal tectonic steepness (#220)
      coastCellSize
    , coastCellsInU
    , coastCellSteepness
    , coastSteepAt
    , coastTectonicSteepness
      -- * Inland rift intensity (#223)
    , riftCellIntensity
    , riftTectonicIntensity
    , riftFieldMemo
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import World.Scale (computeWorldScale, scaleDist)
import World.Plate.Types (TectonicPlate(..))
import World.Plate.Hash (smoothstep, lerp)
import World.Plate.Wrap (worldWidthTiles, wrapGlobalU)
import World.Plate.Query (BoundaryType(..), twoNearestPlates, classifyBoundary)

-- * Coastal Tectonic Steepness (#220)

-- | Cell size (tiles, in u/v space) of the coarse grid the coastal
--   steepness field is sampled on. Coarse enough that interpolation
--   between samples is smooth over many tiles — the June #220 attempt
--   proved per-tile boundary classification produces pillars and
--   floating lakes at margin transitions.
coastCellSize ∷ Int
coastCellSize = 24

-- | Number of u-cells: snapped so the cell grid tiles the cylinder
--   exactly (same trick as 'World.Plate.Noise.wrappedValueNoise2D') —
--   a tile and its wrap alias interpolate the same physical samples.
coastCellsInU ∷ Int → Int
coastCellsInU worldSize =
    let w = worldWidthTiles worldSize
    in max 1 (round (fromIntegral w / fromIntegral coastCellSize ∷ Float))

-- | How steep a coast the boundary type wants. Convergent margins
--   push mountains to the sea; divergent margins slope gently away.
boundarySteepness ∷ BoundaryType → Float
boundarySteepness (Convergent s) = 0.8 + 0.2 * min 1.0 s
boundarySteepness (Transform  _) = 0.5
boundarySteepness (Divergent  _) = 0.1

-- | Boundary classification + distance-to-boundary at one coarse
--   cell center, shared by the coastal-steepness and rift-intensity
--   cell samplers. @ju@ is the u-cell index (any integer — the
--   physical position wraps), @jv@ the v-cell index.
cellBoundarySample ∷ Word64 → Int → [TectonicPlate] → Int → Int
                   → (BoundaryType, Float)
cellBoundarySample seed worldSize plates ju jv =
    let w = worldWidthTiles worldSize
        effCell = fromIntegral w / fromIntegral (coastCellsInU worldSize) ∷ Float
        uC = (fromIntegral ju + 0.5) * effCell
        vC = (fromIntegral jv + 0.5) * fromIntegral coastCellSize ∷ Float
        gxC = round ((uC + vC) / 2.0)
        gyC = round ((vC - uC) / 2.0)
        (gx', gy') = wrapGlobalU worldSize gxC gyC
        ((plateA, distA), (plateB, distB)) =
            twoNearestPlates seed worldSize plates gx' gy'
        boundaryDist = abs (distB - distA) / 2.0
    in (classifyBoundary worldSize plateA plateB, boundaryDist)

-- | Steepness sample at one coarse cell center. Fades toward gentle
--   away from the plate boundary so coasts deep inside a plate ignore
--   their far-off margin type.
coastCellSteepness ∷ Word64 → Int → [TectonicPlate] → Int → Int → Float
coastCellSteepness seed worldSize plates ju jv =
    let wsc = computeWorldScale worldSize
        (boundary, boundaryDist) =
            cellBoundarySample seed worldSize plates ju jv
        base = boundarySteepness boundary
        nearR = scaleDist wsc 40.0
        fadeR = max 1.0 (scaleDist wsc 200.0)
        fade = 1.0 - smoothstep (clamp01 ((boundaryDist - nearR) / fadeR))
    in base * fade

-- | Interpolate a steepness field at a tile from a cell sampler.
--   Shared by the direct query ('coastTectonicSteepness') and the
--   memoized grid in the global coastal pass, so both see the exact
--   same field.
coastSteepAt ∷ (Int → Int → Float) → Int → Int → Int → Float
coastSteepAt sample worldSize gx gy =
    let w = worldWidthTiles worldSize
        effCell = fromIntegral w / fromIntegral (coastCellsInU worldSize) ∷ Float
        fu = fromIntegral (gx - gy) / effCell
        fv = fromIntegral (gx + gy) / fromIntegral coastCellSize ∷ Float
        iu = floor fu ∷ Int
        iv = floor fv ∷ Int
        tu = smoothstep (fu - fromIntegral iu)
        tv = smoothstep (fv - fromIntegral iv)
        s00 = sample iu       iv
        s10 = sample (iu + 1) iv
        s01 = sample iu       (iv + 1)
        s11 = sample (iu + 1) (iv + 1)
    in lerp tv (lerp tu s00 s10) (lerp tu s01 s11)

-- | Smooth, coherent "how tectonically steep is the coast here"
--   field in [0,1]. Pure function of seed + plates, so the base
--   terrain ('World.Plate.Elevation.continentalShelf' modulation) and
--   the global coastal pass agree on which margins keep their
--   mountains.
coastTectonicSteepness ∷ Word64 → Int → [TectonicPlate] → Int → Int → Float
coastTectonicSteepness seed worldSize plates =
    coastSteepAt (coastCellSteepness seed worldSize plates) worldSize

-- * Inland Rift Intensity (#223)

-- | How strongly a rifting/extensional margin expresses at a cell.
--   Only divergent boundaries rift; convergent and transform margins
--   contribute nothing.
riftBoundaryIntensity ∷ BoundaryType → Float
riftBoundaryIntensity (Divergent s) = 0.6 + 0.4 * min 1.0 s
riftBoundaryIntensity _             = 0.0

-- | Rift-intensity sample at one coarse cell center. Same cell grid
--   as the coastal steepness field but with a much tighter boundary
--   fade — rift valleys and graben lakes hug the spreading boundary
--   instead of colouring the whole margin the way coastal steepness
--   does.
riftCellIntensity ∷ Word64 → Int → [TectonicPlate] → Int → Int → Float
riftCellIntensity seed worldSize plates ju jv =
    let wsc = computeWorldScale worldSize
        (boundary, boundaryDist) =
            cellBoundarySample seed worldSize plates ju jv
        base = riftBoundaryIntensity boundary
        nearR = scaleDist wsc 16.0
        fadeR = max 1.0 (scaleDist wsc 64.0)
        fade = 1.0 - smoothstep (clamp01 ((boundaryDist - nearR) / fadeR))
    in base * fade

-- | Smooth, coherent "how tectonically rifted is this area" field in
--   [0,1]. Pure function of seed + plates, interpolated on the same
--   wrap-exact cell grid as 'coastTectonicSteepness' so a tile and
--   its u-alias read identical values.
riftTectonicIntensity ∷ Word64 → Int → [TectonicPlate] → Int → Int → Float
riftTectonicIntensity seed worldSize plates =
    coastSteepAt (riftCellIntensity seed worldSize plates) worldSize

-- | Memoized rift field: every cell of the coarse grid is sampled
--   once up front (a few thousand cells even at large world sizes),
--   and the returned closure interpolates per tile. Use this when
--   querying many tiles (river tracing, lake footprints) — the
--   direct 'riftTectonicIntensity' re-runs 'twoNearestPlates' for
--   every cell corner of every query.
riftFieldMemo ∷ Word64 → Int → [TectonicPlate] → (Int → Int → Float)
riftFieldMemo seed worldSize plates =
    let worldTiles = worldWidthTiles worldSize
        cellsU = coastCellsInU worldSize
        -- v = gx + gy spans [−worldTiles, worldTiles]; pad two cells
        -- each side so interpolation corners never read out of range.
        vCells = 2 * (worldTiles `div` coastCellSize) + 4
        jvOff  = worldTiles `div` coastCellSize + 2
        wrapJu ju = ((ju `mod` cellsU) + cellsU) `mod` cellsU
        clampJv jv = max (negate jvOff) (min (vCells - jvOff - 1) jv)
        cellIdx ju jv = (clampJv jv + jvOff) * cellsU + wrapJu ju
        cells = VU.generate (cellsU * vCells) $ \i →
            let (jvI, juI) = i `divMod` cellsU
            in riftCellIntensity seed worldSize plates juI (jvI - jvOff)
    in coastSteepAt (\ju jv → cells VU.! cellIdx ju jv) worldSize
