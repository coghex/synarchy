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
import Control.Monad.ST (runST)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import World.Types
import World.Material (MaterialId(..), matGlacier, getMaterialProps, MaterialProps(..)
                      , matAir)
import World.Plate (TectonicPlate(..), generatePlates
                   , elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Grid (worldToGrid)
import World.Geology (applyGeoEvent, GeoModification(..))
import World.Geology.Types (eventBBox, bboxOverlapsChunk)
import World.Geology.Erosion (applyErosion)
import World.Scale (computeWorldScale, WorldScale(..))
import World.Slope (computeChunkSlopes)
import World.Fluids (isOceanChunk, computeChunkFluid, computeChunkLava
                    , computeChunkLakes, computeChunkRivers, unionFluidMap)
import Engine.Graphics.Camera (CameraFacing(..))

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------

chunkLoadRadius ∷ Int
chunkLoadRadius = 2

-- | How many z-levels below the z-slice are rendered.
--   This is a RENDER window, not a generation limit.
viewDepth ∷ Int
viewDepth = 25

-- | Border size around each chunk for neighbor lookups.
--   Needs to be large enough for erosion neighbor access.
--   4 tiles gives comfortable margin for smoothing.
chunkBorder ∷ Int
chunkBorder = 4

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
--
--   Erosion is computed per-period across all columns in the chunk,
--   using a shared elevation map so each tile can read its neighbors'
--   post-event elevations. This gives physically-based smoothing
--   that respects material hardness and geological time.
--
--   The border is expanded to chunkBorder tiles so erosion at
--   chunk edges has valid neighbor data.
generateChunk ∷ WorldGenParams → ChunkCoord
  → (Chunk, VU.Vector Int, VU.Vector Int, V.Vector (Maybe FluidCell))
generateChunk params coord =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        timeline = wgpGeoTimeline params
        plates = wgpPlates params
        wsc = computeWorldScale worldSize
        oceanMap = wgpOceanMap params

        borderSize = chunkSize + 2 * chunkBorder
        borderArea = borderSize * borderSize

        toIndex lx ly =
            let bx = lx + chunkBorder
                by = ly + chunkBorder
            in by * borderSize + bx

        fromIndex idx =
            let (by, bx) = idx `divMod` borderSize
            in (bx - chunkBorder, by - chunkBorder)

        inBorder lx ly =
            lx ≥ negate chunkBorder ∧ lx < chunkSize + chunkBorder ∧
            ly ≥ negate chunkBorder ∧ ly < chunkSize + chunkBorder

        -- Base elevation/material grids (with border) built in one pass
        (baseElevVec, baseMatVec) = runST $ do
            elevM ← VUM.new borderArea
            matM  ← VUM.new borderArea
            forM_ [0 .. borderArea - 1] $ \idx → do
                let (lx, ly) = fromIndex idx
                    (gx, gy) = chunkToGlobal coord lx ly
                    (gx', gy') = wrapGlobalU worldSize gx gy
                if isBeyondGlacier worldSize gx' gy'
                    then do
                        VUM.write elevM idx 0
                        VUM.write matM  idx (MaterialId 1)
                    else do
                        let (elev, mat) =
                                elevationAtGlobal seed plates worldSize gx' gy'
                        VUM.write elevM idx elev
                        VUM.write matM  idx mat
            elevF ← VU.unsafeFreeze elevM
            matF  ← VU.unsafeFreeze matM
            pure (elevF, matF)

        lookupBase lx ly =
            if inBorder lx ly
            then ( baseElevVec VU.! toIndex lx ly
                 , baseMatVec  VU.! toIndex lx ly
                 )
            else (0, MaterialId 1)

        -- Apply timeline using split vectors
        (finalElevVec, finalMatVec) =
            applyTimelineChunk timeline worldSize wsc coord
                (baseElevVec, baseMatVec)

        lookupFinal lx ly =
            if inBorder lx ly
            then ( finalElevVec VU.! toIndex lx ly
                 , finalMatVec  VU.! toIndex lx ly
                 )
            else (0, MaterialId 1)

        lookupElev lx ly = fst (lookupFinal lx ly)

        lookupElevOr lx ly fallback =
            if inBorder lx ly
            then finalElevVec VU.! toIndex lx ly
            else fallback

        -- Terrain surface map (vector)
        terrainSurfaceMap = VU.generate (chunkSize * chunkSize) $ \idx ->
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
                (gx, gy) = chunkToGlobal coord lx ly
                (gx', gy') = wrapGlobalU worldSize gx gy
            in if isBeyondGlacier worldSize gx' gy'
               then minBound
               else lookupElev lx ly

        -- Fluids
        oceanFluidMap = computeChunkFluid oceanMap coord terrainSurfaceMap
        features = gtFeatures timeline
        lavaFluidMap = computeChunkLava features seed plates worldSize
                                        coord terrainSurfaceMap
        lakeFluidMap = computeChunkLakes features seed plates worldSize
                                         coord terrainSurfaceMap
        riverFluidMap = computeChunkRivers features seed plates worldSize
                                           coord terrainSurfaceMap

        fluidMap = unionFluidMap oceanFluidMap
                $ unionFluidMap lavaFluidMap
                $ unionFluidMap lakeFluidMap riverFluidMap

        -- Surface map with fluids
        surfaceMap = VU.imap (\idx surfZ ->
            case fluidMap V.! idx of
                Just fc → max surfZ (fcSurface fc)
                Nothing → surfZ
          ) terrainSurfaceMap

        tiles = [ tile
                | lx ← [0 .. chunkSize - 1]
                , ly ← [0 .. chunkSize - 1]
                , let (gx, gy) = chunkToGlobal coord lx ly
                      (gx', gy') = wrapGlobalU worldSize gx gy
                , not (isBeyondGlacier worldSize gx' gy')
                , let (surfZ, surfMat) = lookupFinal lx ly
                      base = lookupBase lx ly
                      baseN = fst (lookupBase lx (ly - 1))
                      baseS = fst (lookupBase lx (ly + 1))
                      baseE = fst (lookupBase (lx + 1) ly)
                      baseW = fst (lookupBase (lx - 1) ly)
                      neighborMinZ = minimum
                          [ lookupElevOr (lx - 1) ly surfZ
                          , lookupElevOr (lx + 1) ly surfZ
                          , lookupElevOr lx (ly - 1) surfZ
                          , lookupElevOr lx (ly + 1) surfZ
                          ]
                      exposeFrom = min surfZ neighborMinZ
                      lookupMat z
                          | surfMat ≡ matGlacier = matGlacier
                          | z ≡ surfZ            = surfMat
                          | otherwise            = materialAtDepth timeline
                                                       worldSize gx'
                                                       gy' base
                                                       (baseN, baseS, baseE, baseW)
                                                       z
                , tile ← generateExposedColumn lx ly surfZ exposeFrom lookupMat
                ]

        rawTiles = HM.fromList tiles

        noNeighborLookup ∷ ChunkCoord → Maybe (VU.Vector Int)
        noNeighborLookup _ = Nothing

        slopedTiles = computeChunkSlopes seed coord terrainSurfaceMap
                                         fluidMap rawTiles noNeighborLookup
    in (slopedTiles, surfaceMap, terrainSurfaceMap, fluidMap)

-- | Generate only the exposed tiles for a column.
--   Skips air tiles (MaterialId 0) to create caves and overhangs.
generateExposedColumn ∷ Int → Int → Int → Int → (Int → MaterialId)
                      → [((Int, Int, Int), Tile)]
generateExposedColumn lx ly surfaceZ exposeFrom lookupMat =
    [ ((lx, ly, z), Tile (unMaterialId mat) 0)
    | z ← [exposeFrom .. surfaceZ]
    , let mat = lookupMat z
    , mat ≠ matAir
    ]

-----------------------------------------------------------
-- Per-Period Timeline Application (chunk-level with erosion)
-----------------------------------------------------------

-- | Walk the geological timeline period by period, applying events
--   and erosion across all columns simultaneously.
--   Events are per-tile (no neighbor dependency).
--   Erosion smooths each tile toward its neighbors' post-event elevations,
--   using the shared elevation map for neighbor lookups.
applyTimelineChunk ∷ GeoTimeline → Int → WorldScale → ChunkCoord
                   → (VU.Vector Int, VU.Vector MaterialId)
                   → (VU.Vector Int, VU.Vector MaterialId)
applyTimelineChunk timeline worldSize wsc coord (baseElevVec, baseMatVec) =
    let ChunkCoord cx cy = coord
        chunkMinGX = cx * chunkSize - chunkBorder
        chunkMinGY = cy * chunkSize - chunkBorder
        chunkMaxGX = cx * chunkSize + chunkSize + chunkBorder - 1
        chunkMaxGY = cy * chunkSize + chunkSize + chunkBorder - 1
    in foldl' (applyOnePeriod chunkMinGX chunkMinGY chunkMaxGX chunkMaxGY)
              (baseElevVec, baseMatVec) (gtPeriods timeline)
  where
    borderSize = chunkSize + 2 * chunkBorder

    toIndex lx ly =
        let bx = lx + chunkBorder
            by = ly + chunkBorder
        in by * borderSize + bx

    fromIndex idx =
        let (by, bx) = idx `divMod` borderSize
        in (bx - chunkBorder, by - chunkBorder)

    inBorder lx ly =
        lx ≥ negate chunkBorder ∧ lx < chunkSize + chunkBorder ∧
        ly ≥ negate chunkBorder ∧ ly < chunkSize + chunkBorder

    lookupElev vec lx ly fallback =
        if inBorder lx ly
        then vec VU.! toIndex lx ly
        else fallback

    applyOnePeriod cMinGX cMinGY cMaxGX cMaxGY (elevVec, matVec) period =
        let relevantEvents = filter (\evt →
                let bb = eventBBox evt worldSize
                in bboxOverlapsChunk worldSize bb cMinGX cMinGY cMaxGX cMaxGY
                ) (gpEvents period)

            (postElev, postMat) =
                if null relevantEvents
                then (elevVec, matVec)
                else
                    let elev' = VU.imap (\idx elev ->
                            let (lx, ly) = fromIndex idx
                                mat = matVec VU.! idx
                                (gx, gy) = chunkToGlobal coord lx ly
                                (gx', gy') = wrapGlobalU worldSize gx gy
                                (elevOut, _) =
                                    if mat ≡ matGlacier
                                    then (elev, mat)
                                    else foldl' (applyOneEvent worldSize gx' gy')
                                                (elev, mat) relevantEvents
                            in elevOut
                          ) elevVec
                        mat' = VU.imap (\idx elev ->
                            let (lx, ly) = fromIndex idx
                                mat = matVec VU.! idx
                                (gx, gy) = chunkToGlobal coord lx ly
                                (gx', gy') = wrapGlobalU worldSize gx gy
                                (_, matOut) =
                                    if mat ≡ matGlacier
                                    then (elev, mat)
                                    else foldl' (applyOneEvent worldSize gx' gy')
                                                (elev, mat) relevantEvents
                            in matOut
                          ) elevVec
                    in (elev', mat')

            (finalElev, finalMat) =
                let elev' = VU.imap (\idx elev ->
                        let mat = postMat VU.! idx
                        in if mat ≡ matGlacier
                           then elev
                           else
                               let (lx, ly) = fromIndex idx
                                   neighbors =
                                       ( lookupElev postElev lx (ly - 1) elev
                                       , lookupElev postElev lx (ly + 1) elev
                                       , lookupElev postElev (lx + 1) ly elev
                                       , lookupElev postElev (lx - 1) ly elev
                                       )
                                   erosionMod = applyErosion
                                       (gpErosion period)
                                       worldSize
                                       (gpDuration period)
                                       (wsScale wsc)
                                       (unMaterialId mat)
                                       elev
                                       neighbors
                               in elev + gmElevDelta erosionMod
                      ) postElev

                    mat' = VU.imap (\idx elev ->
                        let mat = postMat VU.! idx
                        in if mat ≡ matGlacier
                           then mat
                           else
                               let (lx, ly) = fromIndex idx
                                   neighbors =
                                       ( lookupElev postElev lx (ly - 1) elev
                                       , lookupElev postElev lx (ly + 1) elev
                                       , lookupElev postElev (lx + 1) ly elev
                                       , lookupElev postElev (lx - 1) ly elev
                                       )
                                   erosionMod = applyErosion
                                       (gpErosion period)
                                       worldSize
                                       (gpDuration period)
                                       (wsScale wsc)
                                       (unMaterialId mat)
                                       elev
                                       neighbors
                               in case gmMaterialOverride erosionMod of
                                      Just m  → MaterialId m
                                      Nothing → mat
                      ) postElev
                in (elev', mat')
        in (finalElev, finalMat)

    applyOneEvent ws gx gy (elev, mat) event =
        let mod' = applyGeoEvent event ws gx gy elev
            elev' = elev + gmElevDelta mod'
            mat'  = case gmMaterialOverride mod' of
                Just m  → MaterialId m
                Nothing → mat
        in (elev', mat')

-----------------------------------------------------------
-- Legacy Timeline Application (single-column, for external callers)
-----------------------------------------------------------

applyTimeline ∷ GeoTimeline → Int → Int → Int → (Int, MaterialId) → (Int, MaterialId)
applyTimeline timeline worldSize gx gy (baseElev, baseMat) =
    let wsc = computeWorldScale worldSize
    in foldl' (applyPeriodSingle worldSize wsc gx gy)
              (baseElev, baseMat) (gtPeriods timeline)

applyPeriodSingle ∷ Int → WorldScale → Int → Int
                  → (Int, MaterialId) → GeoPeriod → (Int, MaterialId)
applyPeriodSingle worldSize wsc gx gy (elev, mat) period =
    let (elev', mat') = foldl' applyOneEvent (elev, mat) (gpEvents period)
        erosionMod = applyErosion
            (gpErosion period)
            worldSize
            (gpDuration period)
            (wsScale wsc)
            (unMaterialId mat')
            elev'
            (elev', elev', elev', elev')
        elev'' = elev' + gmElevDelta erosionMod
        mat'' = case gmMaterialOverride erosionMod of
            Just m  → MaterialId m
            Nothing → mat'
    in (elev'', mat'')
  where
    applyOneEvent (e, m) event =
        let mod' = applyGeoEvent event worldSize gx gy e
            e' = e + gmElevDelta mod'
            m' = case gmMaterialOverride mod' of
                Just mm → MaterialId mm
                Nothing → m
        in (e', m')

-----------------------------------------------------------
-- Stratigraphy: per-Z material query
-----------------------------------------------------------

materialAtDepth ∷ GeoTimeline → Int → Int → Int
                → (Int, MaterialId)
                → (Int, Int, Int, Int)
                → Int
                → MaterialId
materialAtDepth timeline worldSize gx gy (baseElev, baseMat) (nBaseN, nBaseS, nBaseE, nBaseW) queryZ =
    let wsc = computeWorldScale worldSize

        initState = StrataState
            { ssElev     = baseElev
            , ssSurfMat  = baseMat
            , ssUplift   = 0
            , ssZMat     = baseMat
            , ssNeighbors = (nBaseN, nBaseS, nBaseE, nBaseW)
            }

        finalState = foldl' (applyPeriodStrata worldSize wsc gx gy queryZ)
                            initState (gtPeriods timeline)
    in ssZMat finalState

data StrataState = StrataState
    { ssElev      ∷ !Int
    , ssSurfMat   ∷ !MaterialId
    , ssUplift    ∷ !Int
    , ssZMat      ∷ !MaterialId
    , ssNeighbors ∷ !(Int, Int, Int, Int)
    }

applyPeriodStrata ∷ Int → WorldScale → Int → Int → Int
                  → StrataState → GeoPeriod → StrataState
applyPeriodStrata worldSize wsc gx gy queryZ state period =
    let afterEvents = foldl' (applyEventStrata worldSize gx gy queryZ)
                             state (gpEvents period)

        (nN, nS, nE, nW) = ssNeighbors afterEvents
        advanceNeighbor nElev ngx ngy =
            foldl' (\e ev → e + gmElevDelta (applyGeoEvent ev worldSize ngx ngy e))
                   nElev (gpEvents period)
        nN' = advanceNeighbor nN gx (gy - 1)
        nS' = advanceNeighbor nS gx (gy + 1)
        nE' = advanceNeighbor nE (gx + 1) gy
        nW' = advanceNeighbor nW (gx - 1) gy

        elev' = ssElev afterEvents
        surfMat' = ssSurfMat afterEvents
        erosionMod = applyErosion
            (gpErosion period)
            worldSize
            (gpDuration period)
            (wsScale wsc)
            (unMaterialId surfMat')
            elev'
            (nN', nS', nE', nW')
        erosionDelta = gmElevDelta erosionMod
        erosionMatId = case gmMaterialOverride erosionMod of
            Just m  → MaterialId m
            Nothing → surfMat'
        (surfMat'', uplift'', zMat'') =
            applyDelta queryZ elev' erosionDelta erosionMatId
                       (gmIntrusionDepth erosionMod)
                       surfMat' (ssUplift afterEvents) (ssZMat afterEvents)
        elev'' = elev' + erosionDelta
    in StrataState
        { ssElev     = elev''
        , ssSurfMat  = surfMat''
        , ssUplift   = uplift''
        , ssZMat     = zMat''
        , ssNeighbors = (nN', nS', nE', nW')
        }

applyEventStrata ∷ Int → Int → Int → Int → StrataState → GeoEvent → StrataState
applyEventStrata worldSize gx gy queryZ state event =
    let mod' = applyGeoEvent event worldSize gx gy (ssElev state)
        delta = gmElevDelta mod'
        intrusion = gmIntrusionDepth mod'
        eventMat = case gmMaterialOverride mod' of
            Just m  → MaterialId m
            Nothing → ssSurfMat state
        (surfMat', uplift', zMat') =
            applyDelta queryZ (ssElev state) delta eventMat intrusion
                       (ssSurfMat state) (ssUplift state) (ssZMat state)
        elev' = ssElev state + delta
    in state
        { ssElev    = elev'
        , ssSurfMat = surfMat'
        , ssUplift  = uplift'
        , ssZMat    = zMat'
        }

applyDelta ∷ Int → Int → Int → MaterialId → Int → MaterialId → Int → MaterialId
           → (MaterialId, Int, MaterialId)
applyDelta queryZ elevBefore delta eventMat intrusion surfMat uplift zMat
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
        let newSurfZ = elevBefore + delta
            newSurf = eventMat
            inFill = intrusion > 0
                   ∧ queryZ ≥ newSurfZ
                   ∧ queryZ < newSurfZ + intrusion
            atSurface = queryZ ≡ newSurfZ
            newZMat = if inFill ∨ atSurface
                      then eventMat
                      else zMat
        in (newSurf, uplift, newZMat)
    | otherwise =
        let newZMat = if queryZ ≡ elevBefore ∧ eventMat ≠ surfMat
                      then eventMat
                      else zMat
        in (eventMat, uplift, newZMat)
