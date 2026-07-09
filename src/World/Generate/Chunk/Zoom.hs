{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Zoom-cache terrain generation: the same bordered-region pipeline
--   as the detail chunk path (timeline + coastal erosion + seabed +
--   river/lake carve + magma/lava + island smoothing), skipping strata,
--   slopes, and per-column material stacks the zoom cache doesn't need.
--   Split out of 'World.Generate.Chunk' (#549) — a pure move, no
--   behavior change.
module World.Generate.Chunk.Zoom
    ( generateZoomTerrain
    ) where

import UPrelude
import Control.Monad.ST (runST)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import World.Types
import World.Material (MaterialId(..), matGlacier, MaterialRegistry)
import World.Fluid.Lake.Types (WorldLakes(..))
import World.Fluid.River.Types (WorldRivers(..))
import World.Plate (elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Scale (computeWorldScale)
import World.Magma.Init (discoverChunkLava)
import World.Hydrology.WaterTable (computeWaterTable)
import World.Vegetation (computeChunkVegetation)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)
import World.Generate.InitTerrain (BorderedTerrainCache)
import World.Generate.Timeline (applyTimelineChunk, removeElevationSpikes)
import World.Geology.Coastal (applyCoastalTable)
import World.Fluid.Seabed (applySeabedTable)
import World.Generate.Chunk.Fluid
    ( chunkWaterSurfMap, chunkOrNeighborOceanic, poolRimCaps, mergeRimCaps
    , applyBasaltCaps, composeFluidMap, lavaShellMask, applyLavaShell
    , smoothIslandColumns, mkSurfaceMap
    )
import World.Generate.Chunk.SoilGates (surfaceDemotion, applyFluidWt)

-- * Zoom-Optimized Terrain Generation

-- | Generate terrain + fluid for the zoom cache using the same pipeline
--   as the detail world (bordered region + full timeline + coastal erosion).
--   Skips strata, slopes, vegetation, and flora — the zoom cache computes
--   those itself.
--
--   Returns (elevation, materialId, fluidMap) for the 16×16 interior.
generateZoomTerrain ∷ MaterialRegistry → WorldGenParams
                    → Maybe BorderedTerrainCache
                    → ChunkCoord
                    → ( VU.Vector Int                -- surface (mkSurfaceMap)
                      , VU.Vector Word8              -- surface materials
                      , VU.Vector Word8              -- vegetation IDs
                      , V.Vector (Maybe FluidCell) ) -- fluid
generateZoomTerrain registry params mBorderedCache coord =
    let seed      = wgpSeed params
        worldSize = wgpWorldSize params
        timeline  = wgpGeoTimeline params
        plates    = wgpPlates params
        _oceanMap  = wgpOceanMap params
        wsc       = computeWorldScale worldSize

        borderSize = chunkSize + 2 * chunkBorder
        borderArea = borderSize * borderSize

        fromIndex idx =
            let (by, bx) = idx `divMod` borderSize
            in (bx - chunkBorder, by - chunkBorder)

        inBorder lx ly =
            lx ≥ negate chunkBorder ∧ lx < chunkSize + chunkBorder ∧
            ly ≥ negate chunkBorder ∧ ly < chunkSize + chunkBorder

        toIndex lx ly =
            let bx = lx + chunkBorder
                by = ly + chunkBorder
            in by * borderSize + bx

        -- Either read the bordered pipeline output from the init-time
        -- cache (set by 'buildTimeline' on a fresh world) or
        -- recompute it from scratch (the loaded-save path).
        --
        -- 'computePipelineFromScratch' must NOT be evaluated when the
        -- cache hits — but the module has @LANGUAGE Strict@, so a
        -- top-level @let@ binding would run regardless of whether the
        -- @case@ below selects it. Wrapping the recompute in a
        -- function arg makes its evaluation entry-on-call, so the
        -- cache-hit path skips it entirely (saving ~50% of zoom-cache
        -- build time at init).
        (cacheElev, cacheMat) = case mBorderedCache of
            Just cache | Just cached ← HM.lookup coord cache → cached
            _ → computePipelineFromScratch ()

        -- Seabed pass on top of the cached (post-coastal+despike)
        -- bordered terrain — mirrors 'generateChunk' so the zoom map
        -- and detail world show the same ocean floor + materials.
        (despikedElev, finalMatVec) =
            applySeabedTable (gtSeabed timeline) coord (cacheElev, cacheMat)

        computePipelineFromScratch () =
            let (baseElevVec, baseMatVec) = runST $ do
                    elevM ← VUM.new borderArea
                    matM  ← VUM.new borderArea
                    forM_ [0 .. borderArea - 1] $ \idx → do
                        let (lx, ly) = fromIndex idx
                            (gx, gy) = chunkToGlobal coord lx ly
                            (gx', gy') = wrapGlobalU worldSize gx gy
                        if isBeyondGlacier worldSize gx' gy'
                            then do
                                VUM.write elevM idx (seaLevel + 100)
                                VUM.write matM  idx matGlacier
                            else do
                                let (elev, mat) =
                                        elevationAtGlobal seed plates worldSize gx' gy'
                                VUM.write elevM idx elev
                                VUM.write matM  idx mat
                    elevF ← VU.unsafeFreeze elevM
                    matF  ← VU.unsafeFreeze matM
                    pure (elevF, matF)
                (timelineElevVec, timelineMatVec) =
                    applyTimelineChunk timeline worldSize registry wsc coord
                        (baseElevVec, baseMatVec)
                (postCoastElev, finalMat') =
                    applyCoastalTable (gtCoastal timeline) coord
                        (timelineElevVec, timelineMatVec)
                (despikedElev', _) =
                    removeElevationSpikes 12 4 (chunkSize + 2 * chunkBorder)
                                          (postCoastElev, finalMat')
            in (despikedElev', finalMat')

        -- River + lake carves baked into the bordered region: see the
        -- matching block in 'generateChunk' for the rationale,
        -- including the wrap-seam invariant for the unwrapped chunk
        -- key (alias double-coverage of the carve tables).
        carveAt gx gy =
            let cx = gx `div` chunkSize
                cy = gy `div` chunkSize
                ilx = ((gx `mod` chunkSize) + chunkSize) `mod` chunkSize
                ily = ((gy `mod` chunkSize) + chunkSize) `mod` chunkSize
                li  = ily * chunkSize + ilx
                dr  = case HM.lookup (ChunkCoord cx cy)
                                     (wrCarveDelta (gtWorldRivers timeline)) of
                        Just dv → dv VU.! li
                        Nothing → 0
                dl  = case HM.lookup (ChunkCoord cx cy)
                                     (wlCarveDelta (gtWorldLakes timeline)) of
                        Just dv → dv VU.! li
                        Nothing → 0
            in max dr dl
        carvedElevVec = VU.generate borderArea $ \idx →
            let z = despikedElev VU.! idx
            in if z ≡ minBound
               then z
               else
                 let (lx, ly) = fromIndex idx
                     (gx, gy) = chunkToGlobal coord lx ly
                 in z - carveAt gx gy

        -- Second despike post-carve — mirrors 'generateChunk' (see
        -- the comment there); required here too so the zoom map and
        -- the detail chunks agree (chunk/fast parity).
        (finalElevVec, _) =
            removeElevationSpikes 12 4 (chunkSize + 2 * chunkBorder)
                                  (carvedElevVec, finalMatVec)

        -- Extract interior 16×16 from bordered region; carve already baked.
        chunkArea = chunkSize * chunkSize
        interiorElev = VU.generate chunkArea $ \idx →
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
                (gx', gy') = wrapGlobalU worldSize
                    (fst (chunkToGlobal coord lx ly))
                    (snd (chunkToGlobal coord lx ly))
            in if isBeyondGlacier worldSize gx' gy'
               then minBound
               else if inBorder lx ly
                    then finalElevVec VU.! toIndex lx ly
                    else 0
        interiorMat = VU.generate chunkArea $ \idx →
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
            in if inBorder lx ly
               then unMaterialId (finalMatVec VU.! toIndex lx ly)
               else 1

        -- Mirror the detail path's ocean test + cap pipeline so the
        -- zoom map agrees with the world view on which sub-ocean
        -- chambers become basalt seamounts (no lava) vs. above-water
        -- vents (lava emerges).
        zoomChunkIsOceanic = chunkOrNeighborOceanic params coord
        zoomMagmaBase = discoverChunkLava (wgpVolcanoCtx params) coord
                                       interiorElev
                                       (chunkWaterSurfMap params coord)
        -- Containment rim, mirroring the detail path (parity).
        zoomRimCaps = poolRimCaps params coord
                          (\lx ly → if inBorder lx ly
                                    then Just (finalElevVec VU.! toIndex lx ly)
                                    else Nothing)
        zoomMagma = mergeRimCaps zoomMagmaBase zoomRimCaps
        cappedZoomElev = applyBasaltCaps coord zoomMagma interiorElev
        rawZoomFluid = composeFluidMap params coord cappedZoomElev

        -- Mirror the detail path's lava-water boundary shell so the
        -- zoom map agrees: any lava tile adjacent to water becomes
        -- bare terrain (the zoom map's per-tile colour blend then
        -- picks the material colour, which is basalt's dark grey,
        -- so the contact rim shows as a thin dark border between
        -- lava and water).
        zoomLavaShell = lavaShellMask params coord
                            (\lx ly → if inBorder lx ly
                                      then Just (finalElevVec VU.! toIndex lx ly)
                                      else Nothing)
                            rawZoomFluid
        shellZoomFluid = applyLavaShell zoomLavaShell cappedZoomElev
                                         zoomChunkIsOceanic rawZoomFluid

        -- Apply the same island-column smoother the detail path runs,
        -- otherwise dry tiles surrounded by lake render as land in
        -- the zoom map but as smoothed lake in the world view. The
        -- smoother both updates the fluid map (extra lake cells) and
        -- lowers the terrain at the smoothed tiles — return both so
        -- the zoom view matches.
        (smoothedElev, zoomFluid) =
            smoothIslandColumns cappedZoomElev shellZoomFluid

        -- Match the detail world's surface map: river tiles report the
        -- (flat) water-surface z, not the carved channel-floor z, so
        -- the zoom map paints river tiles at the right elevation. For
        -- other tiles the surface is @max(terrain, fluid.surface)@,
        -- which is what 'mkSurfaceMap' computes.
        zoomSurface = mkSurfaceMap smoothedElev zoomFluid

        -- Mirror the detail path's wetland-soil demotion (see
        -- 'demoteWetland') so the zoom map shows clay where the world
        -- view demoted dry/sloped wetland soils. Same wt construction
        -- as generateChunk: climate baseline + fluid-aware bump/halo.
        zoomWtBase = computeWaterTable (wgpClimateState params)
                                       worldSize coord smoothedElev
        zoomWt = applyFluidWt zoomFluid zoomWtBase
        zoomOutElev olx oly =
            if inBorder olx oly
            then Just (finalElevVec VU.! toIndex olx oly)
            else Nothing
        zoomMat = VU.imap (\idx m →
            case surfaceDemotion zoomOutElev smoothedElev zoomWt idx m of
                Just demoted → demoted
                Nothing      → m
            ) interiorMat

        -- Vegetation via the SAME per-tile function the detail world
        -- uses ('computeChunkVegetation': deep fluid → none, shallow
        -- lake/river → marsh, else biome selection). The zoom cache
        -- previously rolled its own veg with a chunk-level ocean gate
        -- (elev ≤ seaLevel ∧ chunkOcean ⇒ no veg), which stripped
        -- vegetation from entire DRY below-sea-level chunks near
        -- coasts — they rendered as solid bare-material diamonds
        -- (clay = brown) with hard chunk-boundary edges while the
        -- world view grew grass there (user repro: seed 1840733254
        -- chunk (9,-2)). Slopes are passed as zero — the zoom map is
        -- 1px/tile, the slope-only moss/ivy variants don't read at
        -- that scale, and computing real slopes needs the column
        -- strata the zoom path deliberately skips.
        zoomVeg = computeChunkVegetation seed worldSize coord
                      smoothedElev zoomMat
                      (VU.replicate (chunkSize * chunkSize) 0)
                      zoomFluid (wgpClimateState params)

    in (zoomSurface, zoomMat, zoomVeg, zoomFluid)
