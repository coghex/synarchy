{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate
    ( -- * Generation
      generateChunk
      -- * Coordinate helpers
    , globalToChunk
    , chunkToGlobal
    , chunkWorldBounds
    , chunkLoadRadius
    , cameraChunkCoord
      -- * Constants
    , viewDepth
      -- * Timeline application
    , applyTimeline
      -- * Stratigraphy
    , materialAtDepth
      -- * Types re-export
    , ChunkCoord(..)
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Types (Tile(..), ChunkCoord(..), Chunk, WorldGenParams(..)
                   , GeoTimeline(..), GeoPeriod(..), GeoEvent(..)
                   , chunkSize)
import World.Material (MaterialId(..), matGlacier)
import World.Plate (TectonicPlate(..), generatePlates
                   , elevationAtGlobal, isBeyondGlacier, wrapGlobalX)
import World.Grid (worldToGrid)
import World.Geology (applyGeoEvent, GeoModification(..))
import World.Geology.Erosion (applyErosion)
import Engine.Graphics.Camera (CameraFacing(..))

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------

chunkLoadRadius ∷ Int
chunkLoadRadius = 2

-- | How many z-levels below the z-slice are rendered.
--   This is a RENDER window, not a generation limit.
viewDepth ∷ Int
viewDepth = 100

-----------------------------------------------------------
-- Coordinate Helpers
-----------------------------------------------------------

globalToChunk ∷ Int → Int → (ChunkCoord, (Int, Int))
globalToChunk gx gy =
    let cx = floorDiv gx chunkSize
        cy = floorDiv gy chunkSize
        lx = floorMod gx chunkSize
        ly = floorMod gy chunkSize
    in (ChunkCoord cx cy, (lx, ly))

chunkToGlobal ∷ ChunkCoord → Int → Int → (Int, Int)
chunkToGlobal (ChunkCoord cx cy) lx ly =
    (cx * chunkSize + lx, cy * chunkSize + ly)

chunkWorldBounds ∷ ChunkCoord → ((Int, Int), (Int, Int))
chunkWorldBounds (ChunkCoord cx cy) =
    let minX = cx * chunkSize
        minY = cy * chunkSize
        maxX = minX + chunkSize - 1
        maxY = minY + chunkSize - 1
    in ((minX, minY), (maxX, maxY))

cameraChunkCoord ∷ CameraFacing → Float → Float → ChunkCoord
cameraChunkCoord facing camX camY =
    let (gx, gy) = worldToGrid facing camX camY
        (coord, _) = globalToChunk gx gy
    in coord

floorDiv ∷ Int → Int → Int
floorDiv a b = floor (fromIntegral a / fromIntegral b ∷ Double)

floorMod ∷ Int → Int → Int
floorMod a b = a - floorDiv a b * b

-----------------------------------------------------------
-- Chunk Generation
-----------------------------------------------------------

-- | Generate a single chunk. Pure and deterministic.
--   Returns (tiles, surfaceMap) where surfaceMap maps (lx,ly) -> surfaceZ.
--   Only produces tiles that are exposed to the surface:
--   the top tile of each column, plus any tiles whose side
--   face is visible because a neighbor column is shorter.
--
--   Each exposed tile gets its material from stratigraphy:
--   the geological timeline is replayed per-column to determine
--   what material exists at each Z-level, so cliff faces show
--   different rock layers at different depths.
generateChunk ∷ WorldGenParams → ChunkCoord → (Chunk, HM.HashMap (Int, Int) Int)
generateChunk params coord =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        timeline = wgpGeoTimeline params
        plates = generatePlates seed worldSize (wgpPlateCount params)

        -- Wrap global X for border tiles that may cross the seam
        wrapGX gx = wrapGlobalX worldSize gx

        -- For each column (including 1-tile border for neighbor lookups),
        -- store both the final (elev, mat) from the timeline AND the raw
        -- plate base (elev, mat) needed for stratigraphy queries.
        --
        -- Layout: ((lx, ly), ((finalElev, finalMat), (baseElev, baseMat)))
        columns = [ ( (lx, ly)
                    , let raw = elevationAtGlobal seed plates worldSize (wrapGX gx) gy
                      in if snd raw ≡ matGlacier
                         then (raw, raw)
                         else (applyTimeline timeline worldSize (wrapGX gx) gy raw, raw)
                    )
                  | lx ← [-1 .. chunkSize]
                  , ly ← [-1 .. chunkSize]
                  , let (gx, gy) = chunkToGlobal coord lx ly
                  , not (isBeyondGlacier worldSize (wrapGX gx) gy)
                  ]
        elevMap = HM.fromList columns

        lookupElev lx ly = case HM.lookup (lx, ly) elevMap of
            Just ((z, _), _) → z
            Nothing          → 0

        -- Build the surface elevation map for this chunk's own columns
        surfaceMap = HM.fromList
            [ ((lx, ly), surfZ)
            | lx ← [0 .. chunkSize - 1]
            , ly ← [0 .. chunkSize - 1]
            , let (gx, gy) = chunkToGlobal coord lx ly
            , not (isBeyondGlacier worldSize (wrapGX gx) gy)
            , let surfZ = lookupElev lx ly
            ]

        tiles = [ tile
                | lx ← [0 .. chunkSize - 1]
                , ly ← [0 .. chunkSize - 1]
                , let (gx, gy) = chunkToGlobal coord lx ly
                , not (isBeyondGlacier worldSize (wrapGX gx) gy)
                , let ((surfZ, surfMat), base) =
                          case HM.lookup (lx, ly) elevMap of
                              Just v  → v
                              Nothing → ((0, MaterialId 1), (0, MaterialId 1))
                      neighborMinZ = minimum
                          [ lookupElev (lx - 1) ly
                          , lookupElev (lx + 1) ly
                          , lookupElev lx (ly - 1)
                          , lookupElev lx (ly + 1)
                          ]
                      -- Generate tiles all the way down to the shortest neighbor.
                      -- No viewDepth cap here — viewDepth only limits rendering.
                      exposeFrom = min surfZ neighborMinZ
                      -- Per-Z material lookup: glacier columns skip stratigraphy,
                      -- everything else replays the timeline to find the material
                      -- deposited at each depth.
                      lookupMat z
                          | surfMat ≡ matGlacier = matGlacier
                          | otherwise            = materialAtDepth timeline
                                                       worldSize (wrapGX gx)
                                                       gy base z
                , tile ← generateExposedColumn lx ly surfZ exposeFrom lookupMat
                ]
    in (HM.fromList tiles, surfaceMap)

-- | Generate only the exposed tiles for a column.
--   Always includes the surface tile. Below that, includes
--   tiles down to exposeFrom (where a neighbor's surface is lower).
--   Each tile gets its material from the per-Z lookup function,
--   which consults the geological stratigraphy.
generateExposedColumn ∷ Int → Int → Int → Int → (Int → MaterialId)
                      → [((Int, Int, Int), Tile)]
generateExposedColumn lx ly surfaceZ exposeFrom lookupMat =
    [ ((lx, ly, z), Tile (unMaterialId (lookupMat z)) 0)
    | z ← [exposeFrom .. surfaceZ]
    ]

-----------------------------------------------------------
-- Timeline Application (surface-level, unchanged)
-----------------------------------------------------------

-- | Walk the geological timeline, applying each period's events
--   and erosion to get the final elevation and material.
--   This gives the SURFACE result only — used for elevation maps
--   and for backwards compatibility.
applyTimeline ∷ GeoTimeline → Int → Int → Int → (Int, MaterialId) → (Int, MaterialId)
applyTimeline timeline worldSize gx gy (baseElev, baseMat) =
    foldl' applyPeriod (baseElev, baseMat) (gtPeriods timeline)
  where
    applyPeriod (elev, mat) period =
        let -- Apply each event in this period
            (elev', mat') = foldl' applyOneEvent (elev, mat) (gpEvents period)
            -- Apply erosion for this period
            erosionMod = applyErosion (gpErosion period) worldSize gx gy elev'
            elev'' = elev' + gmElevDelta erosionMod
            mat'' = case gmMaterialOverride erosionMod of
                Just m  → MaterialId m
                Nothing → mat'
        in (elev'', mat'')

    applyOneEvent (elev, mat) event =
        let mod' = applyGeoEvent event worldSize gx gy elev
            elev' = elev + gmElevDelta mod'
            mat'  = case gmMaterialOverride mod' of
                Just m  → MaterialId m
                Nothing → mat
        in (elev', mat')

-----------------------------------------------------------
-- Stratigraphy: per-Z material query
-----------------------------------------------------------

-- | Determine what material exists at a specific Z-level in a column.
--   Replays the geological timeline for (gx, gy), tracking how each
--   event deposits or erodes material at different elevation bands.
--
--   The idea: each event that raises the surface by +delta deposits
--   its material in the range [oldElev+1 .. oldElev+delta]. Each
--   event that lowers the surface erodes from the top down. By
--   replaying in order, the last event whose deposited range
--   includes queryZ determines the material there.
--
--   If queryZ is below all deposited layers, the plate's base
--   material is returned (the bedrock).
--
--   Pure and deterministic — called once per exposed tile.
materialAtDepth ∷ GeoTimeline → Int → Int → Int
                → (Int, MaterialId)   -- ^ (baseElev, baseMat) from plates
                → Int                 -- ^ Z-level to query
                → MaterialId
materialAtDepth timeline worldSize gx gy (baseElev, baseMat) queryZ =
    let (_, _, result) = foldl' applyPeriodStrata
                                (baseElev, baseMat, baseMat)
                                (gtPeriods timeline)
    in result
  where
    -- Accumulator: (currentElev, currentSurfaceMat, materialAtQueryZ)
    --
    -- currentElev: tracks the running surface elevation as events
    --   deposit and erode material.
    -- currentSurfaceMat: the material at the current surface
    --   (used when an event has no material override).
    -- materialAtQueryZ: the answer we're building — updated whenever
    --   an event deposits material over queryZ or erosion reveals
    --   what's underneath.

    applyPeriodStrata (elev, surfMat, zMat) period =
        let -- Apply each event in this period
            (elev', surfMat', zMat') =
                foldl' applyOneEventStrata (elev, surfMat, zMat) (gpEvents period)
            -- Apply erosion for this period
            erosionMod = applyErosion (gpErosion period) worldSize gx gy elev'
            erosionDelta = gmElevDelta erosionMod
            erosionMatId = case gmMaterialOverride erosionMod of
                Just m  → MaterialId m
                Nothing → surfMat'
            (surfMat'', zMat'') =
                applyDelta elev' erosionDelta erosionMatId surfMat' zMat'
            elev'' = elev' + erosionDelta
        in (elev'', surfMat'', zMat'')

    applyOneEventStrata (elev, surfMat, zMat) event =
        let mod' = applyGeoEvent event worldSize gx gy elev
            delta = gmElevDelta mod'
            -- If the event specifies a material, use it for the deposited
            -- range. Otherwise, the deposit inherits the current surface
            -- material (same as the old applyTimeline behavior).
            eventMat = case gmMaterialOverride mod' of
                Just m  → MaterialId m
                Nothing → surfMat
            (surfMat', zMat') = applyDelta elev delta eventMat surfMat zMat
            elev' = elev + delta
        in (elev', surfMat', zMat')

    -- | Given the elevation BEFORE this event, the delta (+deposit/-erode),
    --   and the material being deposited, update the surface material and
    --   the material at queryZ.
    --
    --   Deposition (delta > 0):
    --     The event deposits eventMat in [elevBefore+1 .. elevBefore+delta].
    --     If queryZ falls in that range, materialAtQueryZ becomes eventMat.
    --     The surface material becomes eventMat.
    --
    --   Erosion (delta < 0):
    --     The event removes [elevBefore+delta+1 .. elevBefore].
    --     If queryZ is now above the new surface, it doesn't matter
    --     (the tile won't be generated). The surface material is tricky:
    --     after erosion, the surface exposes whatever was deposited at the
    --     new (lower) elevation. But since we track materialAtQueryZ and
    --     the surface mat independently, erosion just keeps surfMat as-is.
    --     (When we query a z-level that was already deposited by a prior
    --     event, that prior event already set zMat correctly.)
    --
    --   No change (delta == 0):
    --     The event may still override the surface material (e.g., a
    --     material-only change). If queryZ == elevBefore (the surface),
    --     and the event overrides the material, update zMat too.
    applyDelta ∷ Int → Int → MaterialId → MaterialId → MaterialId
               → (MaterialId, MaterialId)
    applyDelta elevBefore delta eventMat surfMat zMat
        | delta > 0 =
            -- Deposition: eventMat fills [elevBefore+1 .. elevBefore+delta]
            let newZMat = if queryZ > elevBefore ∧ queryZ ≤ elevBefore + delta
                          then eventMat
                          else zMat
            in (eventMat, newZMat)
        | delta < 0 =
            -- Erosion: removes from the top. The material at queryZ is
            -- unchanged (it was set by a prior depositional event).
            -- Surface material stays as-is — the actual surface material
            -- after erosion depends on what was deposited at the new top,
            -- but we've already tracked that via prior applyDelta calls.
            (surfMat, zMat)
        | otherwise =
            -- No elevation change, but may override surface material.
            -- If queryZ is exactly at the current surface, update it.
            let newZMat = if queryZ ≡ elevBefore ∧ eventMat ≠ surfMat
                          then eventMat
                          else zMat
            in (eventMat, newZMat)
