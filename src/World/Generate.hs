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
--   Each event returns (elevDelta, materialOverride, intrusionDepth):
--     - intrusionDepth tells us how much of the elevDelta is NEW material.
--     - The remainder is uplift: existing strata pushed upward.
--
--   Example: elevDelta=+200, mat=basalt, intrusion=60
--     Before: surface at Z=500
--     After:  surface at Z=700
--       Z=641..700: basalt (60 tiles of new intrusion)
--       Z=501..640: whatever was at Z=361..500 before (uplifted 140 tiles)
--       Z≤500: unchanged
--
--   This means to query "what's at Z=600?", we need to know
--   it was uplifted from Z=600-140=460, and recursively ask
--   what was there before. We track this as a cumulative
--   "uplift offset" — how much the material at queryZ has
--   been shifted upward by prior events.
--
--   Pure and deterministic — called once per exposed tile.
materialAtDepth ∷ GeoTimeline → Int → Int → Int
                → (Int, MaterialId)   -- ^ (baseElev, baseMat) from plates
                → Int                 -- ^ Z-level to query
                → MaterialId
materialAtDepth timeline worldSize gx gy (baseElev, baseMat) queryZ =
    let (_, _, _, result) = foldl' applyPeriodStrata
                                   (baseElev, baseMat, 0, baseMat)
                                   (gtPeriods timeline)
    in result
  where
    -- Accumulator: (currentElev, currentSurfaceMat, upliftAccum, materialAtQueryZ)
    --
    -- currentElev: running surface elevation
    -- currentSurfaceMat: material at the current surface
    -- upliftAccum: how much queryZ has been pushed upward by
    --   uplift events. When we check if queryZ is "in the base
    --   strata", we compare against (baseElev + upliftAccum).
    -- materialAtQueryZ: the answer we're building

    applyPeriodStrata (elev, surfMat, uplift, zMat) period =
        let (elev', surfMat', uplift', zMat') =
                foldl' applyOneEventStrata (elev, surfMat, uplift, zMat)
                       (gpEvents period)
            erosionMod = applyErosion (gpErosion period) worldSize gx gy elev'
            erosionDelta = gmElevDelta erosionMod
            erosionMatId = case gmMaterialOverride erosionMod of
                Just m  → MaterialId m
                Nothing → surfMat'
            -- Erosion: no intrusion, treat as delta with 0 intrusion
            (surfMat'', uplift'', zMat'') =
                applyDelta elev' erosionDelta erosionMatId 0 surfMat' uplift' zMat'
            elev'' = elev' + erosionDelta
        in (elev'', surfMat'', uplift'', zMat'')

    applyOneEventStrata (elev, surfMat, uplift, zMat) event =
        let mod' = applyGeoEvent event worldSize gx gy elev
            delta = gmElevDelta mod'
            intrusion = gmIntrusionDepth mod'
            eventMat = case gmMaterialOverride mod' of
                Just m  → MaterialId m
                Nothing → surfMat
            (surfMat', uplift', zMat') =
                applyDelta elev delta eventMat intrusion surfMat uplift zMat
            elev' = elev + delta
        in (elev', surfMat', uplift', zMat')

    -- | Core stratigraphy logic.
    --
    --   Given elevation before the event, the delta, the material,
    --   the intrusion depth, update everything.
    --
    --   For delta > 0:
    --     upliftAmount = delta - intrusion  (how much existing strata shift up)
    --     intrusionBottom = elevBefore + upliftAmount + 1
    --     intrusionTop    = elevBefore + delta
    --     If queryZ is in [intrusionBottom..intrusionTop]: material is eventMat
    --     If queryZ is in the uplifted zone: it was pushed up by upliftAmount,
    --       so accumulate that into upliftAccum for future base-strata checks.
    --
    --   For delta < 0 (erosion/depression):
    --     No new material deposited. Intrusion is 0.
    --     Surface material may change (override at new surface).
    --
    --   For delta == 0:
    --     Surface material override only.
    applyDelta ∷ Int → Int → MaterialId → Int → MaterialId → Int → MaterialId
               → (MaterialId, Int, MaterialId)
    applyDelta elevBefore delta eventMat intrusion surfMat uplift zMat
        | delta > 0 =
            let clampedIntrusion = min intrusion delta
                upliftAmount = delta - clampedIntrusion
                intrusionBottom = elevBefore + upliftAmount + 1
                intrusionTop = elevBefore + delta
                inIntrusion = clampedIntrusion > 0
                            ∧ queryZ ≥ intrusionBottom
                            ∧ queryZ ≤ intrusionTop
                inUplift = upliftAmount > 0
                         ∧ queryZ > elevBefore
                         ∧ queryZ ≤ elevBefore + upliftAmount
                newUplift = if inUplift then uplift + upliftAmount else uplift
                newZMat = if inIntrusion then eventMat else zMat
                newSurf = eventMat
            in (newSurf, newUplift, newZMat)
        | delta < 0 =
            -- Depression with possible surface material override.
            -- The event carves downward, exposing existing strata in the
            -- cliff walls. The NEW surface (at elevBefore + delta) gets
            -- the event's material if there's an override.
            -- This handles crater bowls (impactite), crater centers
            -- (meteorite), caldera floors (magma), collapse pits (basalt).
            let newSurfZ = elevBefore + delta
                newSurf = eventMat
                -- queryZ at the new surface gets the override material.
                -- queryZ above the new surface but below the old surface
                -- is now "inside the cliff" of this depression — those
                -- tiles belong to neighboring columns and will be handled
                -- by their own materialAtDepth call.
                newZMat = if queryZ ≡ newSurfZ
                          then eventMat
                          else zMat
            in (newSurf, uplift, newZMat)
        | otherwise =
            let newZMat = if queryZ ≡ elevBefore ∧ eventMat ≠ surfMat
                          then eventMat
                          else zMat
            in (eventMat, uplift, newZMat)
