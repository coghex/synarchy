{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Chunk
    ( generateChunk
    , generateExposedColumn
    , generateZoomTerrain
    ) where

import UPrelude
import Data.Bits (shiftR, (.&.))
import Control.Monad (when, forM_)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import World.Types
import World.Material (MaterialId(..), matGlacier, getMaterialProps
                      , MaterialProps(..), matAir, matBasalt
                      , MaterialRegistry(..))
import World.Plate (TectonicPlate(..), generatePlates
                   , elevationAtGlobal, isBeyondGlacier, wrapGlobalU)
import World.Geology (applyGeoEvent, GeoModification(..))
import World.Geology.Timeline.Types (GeoEvent(..), GeoTimeline(..))
import World.Constants (seaLevel)
import World.Scale (computeWorldScale, WorldScale(..))
import World.Slope (computeChunkSlopes)
import World.Fluids (hasAnyOceanFluid)
import World.Ocean.Types (oceanDistAt)
import World.Fluid.Internal (emptyFluidMap, lavaOverrides
                            , wrapChunkCoordU, wrappedDeltaUVFluid)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.River (riverNearChunk)
import World.Fluid.Ice (computeChunkIce)
import World.Magma.Types (MagmaOverlay(..))
import World.Magma.Init (discoverChunkLava)
import World.Hydrology.Types (HydroFeature(..), RiverParams(..)
                             , RiverSegment(..))
import World.Geology.Timeline.Types (GeoEvent(..), GeoPeriod(..))
import World.Hydrology.WaterTable (computeWaterTable)
import World.Fluid.Lake.Types
    ( WorldLakes(..), LakeChunkEntry(..), lakesInChunk )
import qualified World.Fluid.Lake.Types as WL
import World.Fluid.River.Types
    ( WorldRivers(..), RiverChunkEntry(..), riversInChunk )
import World.Fluid.Types (emptyIceMap)
import World.Vegetation (computeChunkVegetation, vegSnow, vegHash)
import World.Flora.Placement (computeChunkFlora)
import World.Generate.Constants (chunkBorder)
import World.Generate.Coordinates (chunkToGlobal)
import World.Generate.InitTerrain (BorderedTerrainCache)
import World.Generate.Timeline (applyTimelineChunk, removeElevationSpikes)
import World.Geology.Coastal (applyCoastalErosion)
import World.Generate.Strata
    ( buildStrataCache
    , buildColumnStrata
    )

-- * Fluid composition — global lake table (Phase 2)
--
-- Per-chunk surface fluid placement now reads the global 'WorldLakes'
-- table on the timeline. The table is built once at world init by a
-- tile-resolution priority flood (see "World.Fluid.Lake.Identify");
-- here each tile is classified as:
--
--   * Ocean  — terrain ≤ seaLevel in an ocean-BFS-reachable chunk;
--              surface = seaLevel.
--   * River  — tile inside a river-segment channel mask. Currently
--              never fires (rivers off).
--   * Lake   — any lake's per-chunk bitmask flags this tile AND the
--              chunk's real terrain is at or below the lake's
--              uniform 'lkSurface'. Surface = lake's spillway.
--   * dry    — otherwise.
--
-- The 'waterTableMap' arg is no longer used for surface placement;
-- it stays computed and stored on 'LoadedChunk' so that the
-- subsurface dig path can still ask "is this buried tile saturated?"
composeFluidMap ∷ WorldGenParams → ChunkCoord → VU.Vector Int
                → VU.Vector Int
                → Maybe MagmaOverlay
                → V.Vector (Maybe FluidCell)
composeFluidMap params coord terrainMap _channelMask _mMagma =
    let worldSize   = wgpWorldSize params
        timeline    = wgpGeoTimeline params
        oceanDist   = wgpOceanDist params
        chunkArea   = chunkSize * chunkSize
        worldLakes  = gtWorldLakes timeline
        worldRivers = gtWorldRivers timeline

        -- Chunk-level ocean BFS: is this chunk reachable from a
        -- world-edge ocean chunk via chunk-resolution flood?
        --
        -- Known limitation: the global lake identifier uses a
        -- TILE-resolution worldOcean BFS, which can flag tiles that
        -- this chunk-level check disagrees with (a chunk that's
        -- mostly land but has one sub-sea inlet at its corner). For
        -- those rare cases the tile renders as dry land rather than
        -- ocean. Fixing properly needs propagating worldOcean from
        -- the identifier through to here — left for later.
        chunkIsOceanic = chunkOrNeighborOceanic params coord

        -- Per-tile lake surface lookup. We pre-fold the chunk's
        -- 'LakeChunkEntry' vector into a single per-tile lake surface
        -- (or 'minBound' for no-lake). With at most a handful of
        -- lakes per chunk (typically 1–2), the fold is cheap.
        lakeSurfMap ∷ VU.Vector Int
        lakeSurfMap = VU.create $ do
            v ← VUM.replicate chunkArea minBound
            V.forM_ (lakesInChunk worldLakes coord) $ \lce → do
                let lid  = lceLakeId lce
                    bm   = lceBitmask lce
                    surf = WL.lkSurface (wlLakes worldLakes V.! lid)
                forM_ [0 .. chunkArea - 1] $ \i →
                    when (bm VU.! i) $ do
                        cur ← VUM.read v i
                        -- If multiple lakes claim a tile (shouldn't
                        -- happen — global flood gives one label per
                        -- tile — but defensive): take the LOWER
                        -- surface to keep water within terrain.
                        when (cur ≡ minBound ∨ surf < cur) $
                            VUM.write v i surf
            pure v

        -- Per-tile river surface lookup. Same shape as 'lakeSurfMap'
        -- but reads each river entry's per-tile quantised surface z
        -- instead of a single lake-wide value. Defensive lowest-wins
        -- merge mirrors the lake path.
        riverSurfMap ∷ VU.Vector Int
        riverSurfMap = VU.create $ do
            v ← VUM.replicate chunkArea minBound
            V.forM_ (riversInChunk worldRivers coord) $ \rce → do
                let bm   = rceBitmask rce
                    surfs = rcePerTileSurfZ rce
                forM_ [0 .. chunkArea - 1] $ \i →
                    when (bm VU.! i) $ do
                        cur ← VUM.read v i
                        let s = surfs VU.! i
                        when (cur ≡ minBound ∨ s < cur) $
                            VUM.write v i s
            pure v

        -- Per-tile lava-pool surface, same shape as 'lakeSurfMap'.
        -- Pools come from the global 'gtWorldLavaPools' table
        -- ('World.Magma.Pool.identifyLavaPools') — flat lava lakes
        -- pooled in depressions at the breach cluster's lowest
        -- opening. Highest fluid priority: lava beats water.
        lavaSurfMap ∷ VU.Vector Int
        lavaSurfMap = VU.create $ do
            v ← VUM.replicate chunkArea minBound
            V.forM_ (lakesInChunk (gtWorldLavaPools timeline) coord) $ \lce → do
                let lid  = lceLakeId lce
                    bm   = lceBitmask lce
                    surf = WL.lkSurface
                        (wlLakes (gtWorldLavaPools timeline) V.! lid)
                forM_ [0 .. chunkArea - 1] $ \i →
                    when (bm VU.! i) $ do
                        cur ← VUM.read v i
                        when (cur ≡ minBound ∨ surf < cur) $
                            VUM.write v i surf
            pure v

        waterFluid = V.generate chunkArea $ \idx →
            let terrZ   = terrainMap VU.! idx
                isOcean = terrZ ≤ seaLevel ∧ chunkIsOceanic
                rvSurf  = riverSurfMap VU.! idx
                lkSurf  = lakeSurfMap  VU.! idx
                lvSurf  = lavaSurfMap  VU.! idx
            in if terrZ ≡ minBound
               then Nothing
               -- Lava: highest priority. Pool tiles (global table)
               -- beat every water class; the pool identifier never
               -- floods into water, so a conflict here means a
               -- shoreline tile both tables claim — lava wins and
               -- the shell mask downstream turns the rim to basalt.
               else if lvSurf ≠ minBound ∧ lvSurf ≥ terrZ
                    then Just (FluidCell Lava lvSurf)
               else if isOcean
                    then Just (FluidCell Ocean seaLevel)
                    else
                      -- River > Lake. By construction river tiles
                      -- aren't inside any lake, but defensive priority
                      -- keeps the picture consistent at edges.
                      if rvSurf ≠ minBound ∧ rvSurf ≥ terrZ
                      then Just (FluidCell River rvSurf)
                      else if lkSurf ≠ minBound ∧ lkSurf ≥ terrZ
                           then Just (FluidCell Lake lkSurf)
                           else Nothing

    -- Surface lava now comes from the pool table above; the magma
    -- overlay's moSurface is no longer populated (caps only), so no
    -- per-chunk overlay pass remains.
    in waterFluid

-- | Per-tile water surface from the global lake + river tables,
--   independent of terrain ('minBound' = no water claims the tile;
--   lower-wins merge mirrors 'composeFluidMap'). Used by
--   'discoverChunkLava' for the water-body-aware basalt-cap rule:
--   a chamber breaching below a LAKE or RIVER surface gets capped
--   the same way sub-sea breaches always have, instead of emitting
--   lava into the water column.
chunkWaterSurfMap ∷ WorldGenParams → ChunkCoord → VU.Vector Int
chunkWaterSurfMap params coord = VU.create $ do
    let timeline   = wgpGeoTimeline params
        worldLakes = gtWorldLakes timeline
        chunkArea  = chunkSize * chunkSize
    v ← VUM.replicate chunkArea minBound
    V.forM_ (lakesInChunk worldLakes coord) $ \lce → do
        let surf = WL.lkSurface
                (wlLakes worldLakes V.! lceLakeId lce)
            bm = lceBitmask lce
        forM_ [0 .. chunkArea - 1] $ \i →
            when (bm VU.! i) $ do
                cur ← VUM.read v i
                when (cur ≡ minBound ∨ surf < cur) $
                    VUM.write v i surf
    V.forM_ (riversInChunk (gtWorldRivers timeline) coord) $ \rce → do
        let bm    = rceBitmask rce
            surfs = rcePerTileSurfZ rce
        forM_ [0 .. chunkArea - 1] $ \i →
            when (bm VU.! i) $ do
                cur ← VUM.read v i
                let s = surfs VU.! i
                when (cur ≡ minBound ∨ s < cur) $
                    VUM.write v i s
    pure v

-- | Raise the per-tile terrain surface where 'discoverChunkLava'
--   marked a basalt cap. The cap value is the target terrain Z; we
--   take 'max' with the original terrain so the cap can only RAISE,
--   never lower (defensive — under normal conditions the cap is
--   always above original surface since the chamber breaches it).
--   Tiles not in the cap map are left unchanged.
applyBasaltCaps ∷ ChunkCoord → Maybe MagmaOverlay
                → VU.Vector Int → VU.Vector Int
applyBasaltCaps _ Nothing terrain = terrain
applyBasaltCaps coord (Just mo) terrain
    | HM.null (moBasaltCap mo) = terrain
    | otherwise = runST $ do
        mv ← VU.thaw terrain
        let ChunkCoord cx cy = coord
            baseGX = cx * chunkSize
            baseGY = cy * chunkSize
        forM_ (HM.toList (moBasaltCap mo)) $ \((gx, gy), capZ) → do
            let lx = gx - baseGX
                ly = gy - baseGY
            when (lx ≥ 0 ∧ lx < chunkSize ∧ ly ≥ 0 ∧ ly < chunkSize) $ do
                let idx = ly * chunkSize + lx
                cur ← VUM.read mv idx
                when (capZ > cur) (VUM.write mv idx capZ)
        VU.freeze mv

-- | Mark the lava tiles that lie on a chunk boundary with any
--   non-lava water (Ocean / Lake / River). Within-chunk neighbours
--   read the local fluid map (which already reflects the lava
--   overlay, so lava-vs-lava adjacencies don't trigger a shell);
--   cross-chunk neighbours fall through to 'isWaterAtGlobal' which
--   queries the global lake / river / ocean tables directly. This
--   matters because chunk-gen doesn't have the neighbour chunks'
--   fluid maps available — we'd otherwise miss every lava-water
--   boundary that happens to fall on a chunk edge.
lavaShellMask ∷ WorldGenParams → ChunkCoord
              → V.Vector (Maybe FluidCell)
              → VU.Vector Bool
lavaShellMask params coord fluid =
    VU.generate (chunkSize * chunkSize) isShell
  where
    ChunkCoord cx cy = coord
    baseGX = cx * chunkSize
    baseGY = cy * chunkSize
    isShell idx = case fluid V.! idx of
        Just fc | fcType fc ≡ Lava →
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
            -- All 8 neighbours: a diagonal-only contact still puts
            -- lava and water corner-to-corner on screen (the user's
            -- "single lava tile on top of a column of water"), so
            -- diagonals rim to basalt too.
            in or [ adjWater (lx + dx) (ly + dy)
                  | dx ← [-1, 0, 1], dy ← [-1, 0, 1]
                  , (dx, dy) ≠ (0, 0)
                  ]
        _ → False
    -- Within-chunk: read local fluid (authoritative, sees overlay).
    -- Cross-chunk: query the global fluid tables for the tile.
    adjWater nx ny
        | nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize =
            case fluid V.! (ny * chunkSize + nx) of
                Just fc → fcType fc ≢ Lava
                Nothing → False
        | otherwise =
            isWaterAtGlobal params (baseGX + nx) (baseGY + ny)

-- | "This chunk OR any 4-cardinal neighbour is oceanic per the
--   chunk-level BFS." Loosens the strict @oceanDistAt … ≡ 0@ test
--   so coastal chunks the BFS happens to miss (commonly: chunks
--   near volcanic activity that raises local terrain enough to
--   fail the BFS predicate) still get their sub-sea tiles
--   classified as ocean. Without this, those chunks render as
--   visible square gaps in the surrounding ocean.
--
--   Per-tile sub-sea check still gates the actual ocean fill, so
--   relaxing the chunk-level test never adds ocean above sea level.
chunkOrNeighborOceanic ∷ WorldGenParams → ChunkCoord → Bool
chunkOrNeighborOceanic params coord =
    let worldSize = wgpWorldSize params
        oceanDist = wgpOceanDist params
        check cc =
            oceanDistAt oceanDist (wrapChunkCoordU worldSize cc) ≡ 0
        ChunkCoord cx cy = coord
    in check coord
       ∨ check (ChunkCoord (cx + 1) cy)
       ∨ check (ChunkCoord (cx - 1) cy)
       ∨ check (ChunkCoord cx (cy + 1))
       ∨ check (ChunkCoord cx (cy - 1))

-- | True iff @(gx, gy)@ is a water tile (Ocean / Lake / River)
--   according to the global fluid tables. Used by 'lavaShellMask'
--   for cross-chunk neighbour queries where we don't have the
--   neighbour chunk's final fluid map.
--
--   The lake/river checks are precise — they read the per-chunk
--   bitmasks the same way 'composeFluidMap' does, then compare each
--   table's surface elevation against an approximate terrain z
--   sampled via 'elevationAtGlobal' (raw plate elevation,
--   pre-erosion). The ocean check uses 'chunkOrNeighborOceanic' so
--   coastal chunks misclassified by the chunk-level BFS still
--   report their sub-sea tiles as ocean.
--
--   Approximation impact: erosion can shift the post-init terrain a
--   few tiles up or down vs. 'elevationAtGlobal', which can
--   occasionally over- or under-classify edge tiles as water. The
--   visible effect is a cosmetic 1-tile shell mismatch at the
--   affected chunk-border tiles — never a missing or extra fluid
--   region.
isWaterAtGlobal ∷ WorldGenParams → Int → Int → Bool
isWaterAtGlobal params gx gy =
    let worldSize  = wgpWorldSize params
        timeline   = wgpGeoTimeline params
        lakes      = gtWorldLakes timeline
        rivers     = gtWorldRivers timeline
        seed       = wgpSeed params
        plates     = wgpPlates params
        floorDivCS a =
            let (q, r) = a `divMod` chunkSize
            in if r < 0 then q - 1 else q
        cx = floorDivCS gx
        cy = floorDivCS gy
        lx = gx - cx * chunkSize  -- always in [0, chunkSize)
        ly = gy - cy * chunkSize
        idx = ly * chunkSize + lx
        cc = wrapChunkCoordU worldSize (ChunkCoord cx cy)
        baseElev =
            fst (elevationAtGlobal seed plates worldSize gx gy)
        chunkIsOcean = chunkOrNeighborOceanic params (ChunkCoord cx cy)
        isOcean = chunkIsOcean ∧ baseElev ≤ seaLevel
        -- Lake hit: any lake's bitmask bit set AND surface ≥ terrain.
        isLake = V.any
            (\lce →
                let bm = WL.lceBitmask lce
                    s  = WL.lkSurface
                            (wlLakes lakes V.! WL.lceLakeId lce)
                in bm VU.! idx ∧ s ≥ baseElev)
            (lakesInChunk lakes cc)
        -- River hit: any river's bitmask bit set AND per-tile
        -- quantised surface ≥ terrain.
        isRiver = V.any
            (\rce →
                let bm = rceBitmask rce
                    s  = rcePerTileSurfZ rce VU.! idx
                in bm VU.! idx ∧ s ≥ baseElev)
            (riversInChunk rivers cc)
    in isOcean ∨ isLake ∨ isRiver

-- | Apply the shell mask: drop the lava cell at every shell tile so
--   the renderer paints bare basalt terrain there. Interior lava
--   stays — the chamber's surface lake is preserved, only the
--   contact edge becomes solid rock.
--
--   Safety net: if a shell tile sits at or below 'seaLevel' in an
--   oceanic chunk, restore an Ocean cell after clearing the lava.
--   Without this, a chamber that emits lava below sea level (which
--   the per-tile cap logic in 'discoverChunkLava' tries to prevent
--   but can't always — see the @shapeTopAtXY@ note) would leave a
--   bare-terrain tile under the sea after the shell strips its
--   lava: visible as a chunk-edge hole through the ocean surface.
applyLavaShell ∷ VU.Vector Bool → VU.Vector Int → Bool
               → V.Vector (Maybe FluidCell)
               → V.Vector (Maybe FluidCell)
applyLavaShell shell terrain isOceanic fluid
    | VU.all not shell = fluid
    | otherwise = V.imap clear fluid
  where
    clear idx cell
        | shell VU.! idx =
            let terrZ = terrain VU.! idx
            in if isOceanic ∧ terrZ ≤ seaLevel ∧ terrZ ≠ minBound
               then Just (FluidCell Ocean seaLevel)
               else Nothing
        | otherwise = cell

-- | Maximum z by which a dry "island column" can peek above its
--   surrounding lake's surface and still get smoothed down. Larger
--   than this and we treat it as a legitimate island.
maxColumnPeek ∷ Int
maxColumnPeek = 5

-- | Detect and smooth dry "island column" tiles whose terrain pokes
--   1..'maxColumnPeek' z above a surrounding lake's surface — visual
--   spikes that fall under the global despike threshold. A tile
--   qualifies when:
--
--     * It currently renders dry ('fluidMap[idx] = Nothing'), and
--     * Three or four of its cardinal in-chunk neighbors render as
--       Lake at the same 'fcSurface', and
--     * The tile's terrain is between @surface + 1@ and @surface +
--       maxColumnPeek@ inclusive.
--
--   When this fires we (1) override 'fluidMap[idx]' to a Lake cell at
--   that surface so the renderer paints water, and (2) drop the
--   tile's terrain to @surface − 1@ in the returned terrain map so
--   the surface map + slope + material lookups all agree.
smoothIslandColumns
    ∷ VU.Vector Int               -- ^ raw terrain surface map
    → V.Vector (Maybe FluidCell)  -- ^ raw fluid map
    → (VU.Vector Int, V.Vector (Maybe FluidCell))
smoothIslandColumns terr fluid = runST $ do
    let area = chunkSize * chunkSize
    mTerr  ← VU.thaw terr
    mFluid ← V.thaw fluid
    let neighborSurf nx ny
            | nx < 0 ∨ nx ≥ chunkSize
            ∨ ny < 0 ∨ ny ≥ chunkSize = pure Nothing
            | otherwise = do
                c ← MV.read mFluid (ny * chunkSize + nx)
                pure $ case c of
                    Just fc | fcType fc ≡ Lake → Just (fcSurface fc)
                    _                          → Nothing
        -- Iterate until convergence (in practice 1-2 passes): a tile
        -- that gets smoothed becomes a Lake neighbor for tiles
        -- processed earlier in the same pass. Repeating catches
        -- those.
        pass = do
            changedRef ← newSTRef (0 ∷ Int)
            forM_ [0 .. area - 1] $ \idx → do
                cur ← MV.read mFluid idx
                case cur of
                    Just _  → pure ()
                    Nothing → do
                        let lx = idx `mod` chunkSize
                            ly = idx `div` chunkSize
                            t  = terr VU.! idx
                        ms ← sequence
                                [ neighborSurf (lx - 1) ly
                                , neighborSurf (lx + 1) ly
                                , neighborSurf lx       (ly - 1)
                                , neighborSurf lx       (ly + 1)
                                ]
                        let ns = [ s | Just s ← ms ]
                            countBy z = length (filter (≡ z) ns)
                            uniques = foldr
                                (\z acc → if z `elem` acc then acc else z : acc)
                                [] ns
                            hits = [ (z, countBy z) | z ← uniques ]
                            candidate = case filter ((≥ 3) . snd) hits of
                                ((z, _):_) → Just z
                                []         → Nothing
                        case candidate of
                            Just s
                              | t > s ∧ t ≤ s + maxColumnPeek → do
                                  MV.write mFluid idx
                                           (Just (FluidCell Lake s))
                                  VUM.write mTerr  idx (s - 1)
                                  modifySTRef' changedRef (+1)
                            _ → pure ()
            readSTRef changedRef
        loop n
            | n ≤ 0 = pure ()
            | otherwise = do
                changes ← pass
                if changes > 0 then loop (n - 1) else pure ()
    loop 8
    finalTerr  ← VU.unsafeFreeze mTerr
    finalFluid ← V.unsafeFreeze mFluid
    pure (finalTerr, finalFluid)

-- | Sentinel for "this tile is not in any channel" in the channel-floor
--   map. Picked so it's an obviously-impossible elevation and easy to
--   detect with @≡ noChannel@.
noChannel ∷ Int
noChannel = maxBound

-- | Slice the chunk interior out of a bordered-region channel-floor map.
sliceInteriorMask ∷ VU.Vector Int → VU.Vector Int
sliceInteriorMask bordered =
    let bSize = chunkSize + 2 * chunkBorder
    in VU.generate (chunkSize * chunkSize) $ \i →
        let lx = i `mod` chunkSize
            ly = i `div` chunkSize
            bidx = (ly + chunkBorder) * bSize + (lx + chunkBorder)
        in bordered VU.! bidx

-- | Bordered channel-floor map: for each tile in the bordered region,
--   the segment-interpolated channel-floor elevation (= the elevation
--   the water surface should sit at), or @noChannel@ if no segment
--   covers this tile.
--
--   This is what gets pinned in the water-table compute and what
--   classifies tiles as river in the fluid composer. By storing the
--   channel-floor (not just a bool), the water table at every tile in
--   a channel cross-section is the SAME value — the river surface is
--   flat across its width even though terrain slopes up the banks.
--
--   Terrain-aware: only includes tiles whose actual terrain is at or
--   below the segment's reference elevation (with rsDepth+4 lower
--   tolerance, more for coastal segments). This excludes valley walls
--   and ridges that sit perpendicularly close to a segment centerline.
--
--   When multiple segments overlap a tile (confluence / braided rivers),
--   the LOWEST channel-floor wins — that's the deeper water, and
--   propagating that downstream produces continuous surfaces at junctions.
computeBorderedChannelMask ∷ Int → ChunkCoord → [RiverParams]
                           → VU.Vector Int → VU.Vector Int
computeBorderedChannelMask worldSize coord rivers borderedTerrain =
    let ChunkCoord cx cy = coord
        bSize = chunkSize + 2 * chunkBorder
        bArea = bSize * bSize
        chunkMinGX = cx * chunkSize
        chunkMinGY = cy * chunkSize
        nearby = filter (riverNearChunk worldSize chunkMinGX chunkMinGY) rivers
        nearbySegs = concatMap (V.toList . rpSegments) nearby

        floorFromSeg gx gy terrainHere seg =
            case tileInChannelMask worldSize gx gy terrainHere seg of
                Nothing → noChannel
                Just f  → f

        minFloor a b = if b < a then b else a

    in VU.generate bArea $ \idx →
        let bx = idx `mod` bSize
            by = idx `div` bSize
            gx = chunkMinGX + bx - chunkBorder
            gy = chunkMinGY + by - chunkBorder
            terrainHere = borderedTerrain VU.! idx
        in foldl' (\acc seg → minFloor acc
                              (floorFromSeg gx gy terrainHere seg))
                  noChannel nearbySegs

-- | Membership test for a tile against one river segment. Returns the
--   interpolated channel-floor elevation if the tile is in the mask,
--   or @Nothing@ otherwise.
--
--   Cuts:
--     1. Longitudinal: tile must project within the segment, with a
--        small overshoot at endpoints to prevent gaps. Coastal segments
--        (ending near sea level) get a larger downstream overshoot to
--        reach into ocean across coastal-erosion-lowered tiles.
--     2. Horizontal: perpendicular distance ≤ rsWidth (or 2× for coastal).
--     3. Vertical: tile terrain ≤ refElev and ≥ refElev − maxFillDepth.
tileInChannelMask ∷ Int → Int → Int → Int → RiverSegment → Maybe Int
tileInChannelMask worldSize gx gy terrainHere seg =
    let GeoCoord sx sy = rsStart seg
        GeoCoord ex ey = rsEnd seg
        (dxi, dyi) = wrappedDeltaUVFluid worldSize sx sy ex ey
        dx' = fromIntegral dxi ∷ Float
        dy' = fromIntegral dyi ∷ Float
        segLen2 = dx' * dx' + dy' * dy'
    in if segLen2 < 1.0
       then Nothing
       else
         let segLen = sqrt segLen2
             isCoastalSeg = rsEndElev seg ≤ seaLevel + 5
             upstreamOver   = 0.05
             downstreamOver = if isCoastalSeg
                              then min 2.0 (12.0 / segLen)
                              else 0.05
             (pxi, pyi) = wrappedDeltaUVFluid worldSize sx sy gx gy
             px = fromIntegral pxi ∷ Float
             py = fromIntegral pyi ∷ Float
             tRaw = (px * dx' + py * dy') / segLen2
         in if tRaw < negate upstreamOver ∨ tRaw > 1.0 + downstreamOver
            then Nothing
            else
              let perpDist = abs ((px * dy' - py * dx') / segLen)
                  baseHalf = fromIntegral (rsWidth seg) ∷ Float
                  -- Clamp mask reach to the carved valley extent. If
                  -- the mask reaches further than carving (rsValleyWidth/2),
                  -- tiles get classified as river but their terrain
                  -- wasn't lowered — producing dry "columns" sticking
                  -- up through the river surface. Coastal segments get
                  -- 2× width but still capped by the carved valley.
                  carveHalf = fromIntegral (rsValleyWidth seg) / 2.0 ∷ Float
                  rawMaskHalf = if isCoastalSeg then baseHalf * 2.0 else baseHalf
                  maskHalf = min rawMaskHalf carveHalf
                  tClamped = max 0.0 (min 1.0 tRaw)
                  startE = fromIntegral (rsStartElev seg) ∷ Float
                  endE   = fromIntegral (rsEndElev seg)   ∷ Float
                  refElev = floor (startE + tClamped * (endE - startE)) ∷ Int
                  maxFillDepth = rsDepth seg + (if isCoastalSeg then 12 else 4)
                  -- Channel-floor at this point along the segment. This
                  -- is the value water sits at, regardless of how far
                  -- across the cross-section this particular tile is.
                  channelFloor = max (seaLevel - 1) (refElev - rsDepth seg)
              in if perpDist ≤ maskHalf
                  ∧ terrainHere ≤ refElev
                  ∧ terrainHere ≥ refElev - maxFillDepth
                 then Just channelFloor
                 else Nothing

-- | Merge a river-flat surface rule into surface-map computation.
-- For River fluid, surface = fluid surface (hides terrain protrusions).
-- For other fluid types, surface = max(terrain, fluid).
-- Same rule lives in Sim/Thread.hs::writeDirtyFluids (sim writeback)
-- and World/Edit/Apply.hs::applyEdit (player edits). ChunkLoading
-- uses mkSurfaceMap's output directly — no re-derivation.
mkSurfaceMap ∷ VU.Vector Int → V.Vector (Maybe FluidCell) → VU.Vector Int
mkSurfaceMap terrain fluid =
    VU.imap (\idx surfZ →
        case fluid V.! idx of
            Just fc | fcType fc ≡ River → fcSurface fc
            Just fc                     → max surfZ (fcSurface fc)
            Nothing                     → surfZ
      ) terrain

-- * Chunk Generation

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
generateChunk ∷ MaterialRegistry → FloraCatalog → WorldGenParams
  → ChunkCoord → (Chunk, VU.Vector Int, VU.Vector Int
                 , V.Vector (Maybe FluidCell), IceMap, FloraChunkData
                 , VU.Vector Int, Maybe MagmaOverlay)
generateChunk registry catalog params coord =
    let seed = wgpSeed params
        worldSize = wgpWorldSize params
        timeline = wgpGeoTimeline params
        plates = wgpPlates params
        wsc = computeWorldScale worldSize
        oceanMap = wgpOceanMap params

        -- True if this chunk or any of its 8 neighbors is oceanic.
        -- Used to extend strata to sea level for all coastal columns,
        -- preventing cliff-face voids at chunk boundaries.
        isCoastalChunk = hasAnyOceanFluid worldSize oceanMap coord

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
        -- Beyond-glacier tiles use matGlacier + high elevation so the
        -- timeline preserves them and coastal erosion doesn't treat the
        -- world boundary as a coastline.
        (baseElevVec, baseMatVec) = runST $ do
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

        lookupBase lx ly =
            if inBorder lx ly
            then ( baseElevVec VU.! toIndex lx ly
                 , baseMatVec  VU.! toIndex lx ly
                 )
            else (0, MaterialId 1)

        -- Apply timeline using split vectors
        (timelineElevVec, timelineMatVec) =
            applyTimelineChunk timeline worldSize registry wsc coord
                (baseElevVec, baseMatVec)

        -- Post-timeline coastal erosion: lower coastal terrain,
        -- deposit sand/gravel/wetland materials based on plate tectonics
        -- and river mouth proximity
        (postCoastElev, finalMatVec) =
            applyCoastalErosion seed worldSize plates registry timeline oceanMap coord
                (timelineElevVec, timelineMatVec)

        -- Despike: remove single-tile elevation outliers that survived
        -- timeline events and coastal erosion. These are typically
        -- 1-tile-wide diagonal mountain ridges (features aligned with
        -- the u-v isometric axes that end up 1 tile wide in xy) where
        -- coastal erosion lowered the cardinal neighbors but the spike
        -- itself was outside the coastal range.
        (despikedElev, _) = removeElevationSpikes 12 4 (chunkSize + 2 * chunkBorder)
                                                  (postCoastElev, finalMatVec)

        -- River and lake carves applied to the ENTIRE bordered region
        -- (not just the interior). The carve delta for each tile lives
        -- in the chunk that owns that tile — for border tiles this
        -- might be a neighbouring chunk. Per-tile lookup combines the
        -- river and lake deltas via @max@: rivers carve channels;
        -- coastal lakes whose surface has been clamped to sea level
        -- carve their basin floor sub-sea so water actually fills.
        -- Reading post-carve elevations everywhere keeps strata,
        -- slope, and column construction consistent with what the
        -- owning chunk actually renders.
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

        -- Second despike, post-carve. The pass above ran BEFORE the
        -- global river/lake carve deltas were subtracted — a natural
        -- cliff-edge tile beside a deeply carved channel is not a
        -- spike pre-carve (its high neighbour hides it) but becomes
        -- a 100z+ pillar once the neighbour is carved away (seed 7
        -- w128 @(-96,159): 6→132→177 natural, 177 carved to 29 ⇒
        -- 132 left standing — TERRAIN_SPIKE). Re-running the same
        -- bordered despike on the carved elevations collapses these;
        -- it only fires on 1-tile pillars >12 above ALL cardinal
        -- neighbours, so untouched natural terrain is unaffected.
        -- Mirrored in 'generateZoomTerrain' (chunk/fast parity).
        (finalElevVec, _) =
            removeElevationSpikes 12 4 (chunkSize + 2 * chunkBorder)
                                  (carvedElevVec, finalMatVec)

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

        -- (timeline elevation lookup removed — strata now use clamped
        -- post-coastal neighbors to prevent over-erosion without the
        -- mismatch that creates air-tile gaps near the surface)

        -- Pre-compute wrapped coordinates for the 16×16 chunk interior.
        -- Used by terrainSurfaceMap and strataCache to avoid redundant
        -- chunkToGlobal + wrapGlobalU + isBeyondGlacier calls.
        chunkArea = chunkSize * chunkSize

        (coordGX, coordGY, coordBeyond) = runST $ do
            gxM     ← VUM.new chunkArea
            gyM     ← VUM.new chunkArea
            beyondM ← VUM.new chunkArea
            forM_ [0 .. chunkArea - 1] $ \idx → do
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    (gx, gy) = chunkToGlobal coord lx ly
                    (gx', gy') = wrapGlobalU worldSize gx gy
                VUM.write gxM     idx gx'
                VUM.write gyM     idx gy'
                VUM.write beyondM idx (isBeyondGlacier worldSize gx' gy')
            gxF     ← VU.unsafeFreeze gxM
            gyF     ← VU.unsafeFreeze gyM
            beyondF ← VU.unsafeFreeze beyondM
            pure (gxF, gyF, beyondF)

        -- Terrain surface map (vector). This is the RAW chunk-gen
        -- terrain before island-column smoothing; the smoothed
        -- version is bound below as 'terrainSurfaceMap' and is what
        -- the rest of the pipeline consumes. River carve is already
        -- baked into 'finalElevVec' above, so 'lookupElev' returns
        -- the post-carve elevation directly.
        rawTerrainSurfaceMap = VU.generate chunkArea $ \idx →
            if coordBeyond VU.! idx
            then minBound
            else lookupElev (idx `mod` chunkSize) (idx `div` chunkSize)

        -- Channel mask: river pinning is intentionally disabled. We're
        -- focusing on basin-fill from climate + sink-pin; rivers are
        -- deferred. Pass an empty rivers list so the bordered mask is
        -- 'noChannel' everywhere, which leaves the water-table compute
        -- and the fluid composer's River branch as no-ops. Terrain
        -- carving from river events still happens earlier in the
        -- timeline; we just don't render river surface fluid here.
        borderedChannelMask = computeBorderedChannelMask worldSize coord
                                                         []
                                                         finalElevVec
        interiorChannelMask = sliceInteriorMask borderedChannelMask

        -- Same per-chunk ocean test that 'composeFluidMap' uses
        -- internally — lifted here so we know whether the ocean
        -- column will sit above any cap the magma overlay leaves
        -- (used below for the lava-shell safety net only).
        chunkIsOceanicHere = chunkOrNeighborOceanic params coord

        -- Pure-function lava overlay: which surface tiles in this
        -- chunk have a chamber or chute breaking through? Per-tile
        -- decision: above water → lava cell in @moSurface@; sub-sea
        -- → basalt cap entry in @moBasaltCap@ (terrain raised +
        -- matBasalt at top so the ocean fills above instead of a
        -- black gap, or the cap stays exposed as a basalt outcrop
        -- in inland sub-sea pockets).
        magmaOverlay = discoverChunkLava (wgpVolcanoCtx params) coord
                                          rawTerrainSurfaceMap
                                          (chunkWaterSurfMap params coord)

        -- Patch terrain to raise capped tiles BEFORE composeFluidMap
        -- so the ocean classification + smoother see the new (sealed)
        -- terrain. Above-cap tiles stay at their original z.
        cappedTerrainMap = applyBasaltCaps coord magmaOverlay
                                            rawTerrainSurfaceMap

        -- All fluid placement — global lake table + ocean + lava.
        -- Shared with generateZoomTerrain so the two views agree about
        -- which tiles are water (and lava).
        rawFluidMap = composeFluidMap params coord cappedTerrainMap
                                      interiorChannelMask magmaOverlay

        -- Lava-water boundary shell: any lava tile 4-cardinally
        -- adjacent to a water tile (Ocean/Lake/River) gets cleared
        -- and the column-build below stamps matBasalt on top, so
        -- lava and water never sit edge-to-edge. Interior lava is
        -- preserved — only the contact rim becomes solid rock.
        lavaShell = lavaShellMask params coord rawFluidMap
        shellFluidMap = applyLavaShell lavaShell cappedTerrainMap
                                        chunkIsOceanicHere rawFluidMap

        -- Smooth "island column" artifacts: dry tiles surrounded by
        -- ≥3 lake-of-same-surface neighbors whose terrain peeks 1-K z
        -- above that surface. These are usually noise-driven 1-tile
        -- spikes that fall under the global despike threshold. We
        -- override their fluid cell to Lake at the neighbors' surface
        -- and lower their rendered terrain to surface-1 so the
        -- renderer paints them as ordinary lake tiles. Downstream
        -- (slope, surface materials, surface vegetation, fluid map
        -- output) all read the smoothed binding.
        (terrainSurfaceMap, fluidMap) =
            smoothIslandColumns cappedTerrainMap shellFluidMap

        finalTerrain = terrainSurfaceMap

        -- Surface map: rivers render flat (water surface hides any
        -- minor terrain protrusions in the carved channel); other
        -- fluids use max(terrain, water).
        surfaceMap = mkSurfaceMap finalTerrain fluidMap

        -- Build per-column tile data directly, fusing stratigraphy
        -- computation with ColumnTiles construction to avoid an
        -- intermediate V.Vector ColumnStrata allocation.
        rawChunk = V.generate chunkArea $ \idx →
            if coordBeyond VU.! idx
            then ColumnTiles
                { ctStartZ = 0
                , ctMats   = VU.empty
                , ctSlopes = VU.empty
                , ctVeg    = VU.empty
                }
            else
                let lx = idx `mod` chunkSize
                    ly = idx `div` chunkSize
                    gx' = coordGX VU.! idx
                    gy' = coordGY VU.! idx
                    (rawSurfZ, rawSurfMat) = lookupFinal lx ly
                    base = lookupBase lx ly
                    -- Basalt-cap lookup: if 'discoverChunkLava'
                    -- decided this tile should seal a sub-ocean
                    -- chamber, 'capTopZ' is the raised terrain z and
                    -- the column extends with matBasalt above the
                    -- original strata.
                    capTopZ = case magmaOverlay of
                        Just mo → HM.lookupDefault rawSurfZ (gx', gy')
                                                    (moBasaltCap mo)
                        Nothing → rawSurfZ
                    capRaise = max 0 (capTopZ - rawSurfZ)
                    surfZ = rawSurfZ + capRaise
                    isShellTile = lavaShell VU.! idx
                    surfMat
                        | capRaise > 0 = matBasalt
                        | isShellTile  = matBasalt
                        | otherwise    = rawSurfMat
                    -- Post-coastal neighbor elevations for determining how
                    -- far down to expose strata (cliff face visibility).
                    finalN = lookupElevOr lx (ly - 1) rawSurfZ
                    finalS = lookupElevOr lx (ly + 1) rawSurfZ
                    finalE = lookupElevOr (lx + 1) ly rawSurfZ
                    finalW = lookupElevOr (lx - 1) ly rawSurfZ
                    neighborMinZ = min finalN (min finalS (min finalE finalW))
                    exposeFromRaw = min rawSurfZ neighborMinZ
                    exposeFrom = exposeFromRaw
                    -- Clamp neighbor elevations for the strata cache.
                    -- Coastal erosion can lower neighbors 30+ tiles below
                    -- a cliff column. The cache uses neighbors to compute
                    -- erosion at each geological period — extreme drops
                    -- cause over-erosion that produces air tiles in the
                    -- strata, which render as black voids. Clamping to
                    -- rawSurfZ-20 limits the erosion to realistic levels.
                    -- This doesn't change ctStartZ or strata range.
                    clampN n = max (rawSurfZ - 20) n
                    cache = buildStrataCache timeline worldSize wsc
                                             gx' gy' registry base
                                             (clampN finalN, clampN finalS
                                             , clampN finalE, clampN finalW)
                    -- Strata built using the ORIGINAL (un-capped) surface
                    -- so the cache's per-period erosion math stays valid.
                    -- The cap is then appended as a pure basalt extension.
                    mats = buildColumnStrata cache base exposeFrom rawSurfZ
                    -- Correct the surface material to match the authoritative
                    -- timeline path. buildStrataCache uses final neighbor
                    -- elevations as an approximation, which can cause its
                    -- accumulated elevation to diverge from rawSurfZ by ±1-2
                    -- tiles. This means the strata material at rawSurfZ may be
                    -- from the wrong layer, creating a checkerboard pattern
                    -- in surface materials and vegetation.
                    rawSurfIdx = rawSurfZ - exposeFrom
                    matsAtRaw =
                        if rawSurfIdx ≥ 0 ∧ rawSurfIdx < VU.length mats
                        then mats VU.// [(rawSurfIdx, rawSurfMat)]
                        else mats
                    -- For capped tiles, extend the column with matBasalt
                    -- from rawSurfZ+1 up to capTopZ — the rim of the
                    -- seamount that seals the chamber under the ocean.
                    capTiles = VU.replicate capRaise (unMaterialId matBasalt)
                    finalMats = VU.map unMaterialId matsAtRaw VU.++ capTiles
                    -- Stamp the top with surfMat (matBasalt when capped,
                    -- original surfMat otherwise) so the surface layer
                    -- agrees with everything downstream.
                    finalSurfIdx = surfZ - exposeFrom
                    matsWithSurfTop =
                        if finalSurfIdx ≥ 0 ∧ finalSurfIdx < VU.length finalMats
                        then finalMats VU.// [(finalSurfIdx, unMaterialId surfMat)]
                        else finalMats
                    -- Solidify the visible cliff face for any lava /
                    -- shell / cap tile: override the column's strata
                    -- from sea level up to surfZ with matBasalt. The
                    -- strata cache can produce air voids near the
                    -- top when this tile sits next to a much deeper
                    -- sea-floor neighbour (over-erosion past the
                    -- 20-tile clamp), which renders as a black gap
                    -- along the cliff face. Above-sea tiles next to
                    -- lava + capped seamounts always face this risk;
                    -- forcing basalt for those columns above sea
                    -- level matches the user-visible expectation
                    -- (volcanic cliffs are solid basalt rock).
                    isLavaTile = case rawFluidMap V.! idx of
                        Just fc → fcType fc ≡ Lava
                        _       → False
                    solidify = capRaise > 0 ∨ isShellTile ∨ isLavaTile
                    matIds
                      | not solidify = matsWithSurfTop
                      | otherwise =
                          let lo = max exposeFrom seaLevel
                              hi = surfZ
                              updates =
                                  [ (z - exposeFrom, unMaterialId matBasalt)
                                  | z ← [lo .. hi]
                                  , let i = z - exposeFrom
                                  , i ≥ 0 ∧ i < VU.length matsWithSurfTop
                                  ]
                          in if null updates
                             then matsWithSurfTop
                             else matsWithSurfTop VU.// updates
                in ColumnTiles
                    { ctStartZ = exposeFrom
                    , ctMats   = matIds
                    , ctSlopes = VU.replicate (VU.length matIds) 0
                    , ctVeg    = VU.replicate (VU.length matIds) 0
                    }

        noNeighborLookup ∷ ChunkCoord → Maybe (VU.Vector Int)
        noNeighborLookup _ = Nothing

        slopedTiles = computeChunkSlopes seed coord terrainSurfaceMap registry
                                         fluidMap rawChunk noNeighborLookup

        -- Extract surface material and slope for vegetation computation
        surfaceMatsRaw = VU.generate chunkArea $ \idx →
            let col = slopedTiles V.! idx
                surfZ = terrainSurfaceMap VU.! idx
                i = surfZ - ctStartZ col
            in if i ≥ 0 ∧ i < VU.length (ctMats col)
               then ctMats col VU.! i
               else 0

        -- Corrected surface materials (see 'demoteWetland'): vegetation
        -- and flora read these, so swamp species follow the demoted
        -- soils automatically.
        surfaceMats = VU.imap (\idx m →
            case demoteWetland m of
                Just demoted
                  | not (wetlandKeep terrainSurfaceMap waterTableMap idx)
                  → demoted
                _ → m
            ) surfaceMatsRaw

        surfaceSlopes = VU.generate chunkArea $ \idx →
            let col = slopedTiles V.! idx
                surfZ = terrainSurfaceMap VU.! idx
                i = surfZ - ctStartZ col
            in if i ≥ 0 ∧ i < VU.length (ctSlopes col)
               then ctSlopes col VU.! i
               else 0

        baseVegIds = computeChunkVegetation seed worldSize coord
                        terrainSurfaceMap surfaceMats surfaceSlopes
                        fluidMap (wgpClimateState params)
        -- Snow on ice-covered tiles: ice is an overlay not in the
        -- terrain material system, so inject snow vegetation here.
        vegIds = VU.imap (\idx v →
            case iceMap V.! idx of
                Just _  →
                    let ChunkCoord cx cy = coord
                        lx = idx `mod` chunkSize
                        ly = idx `div` chunkSize
                        gx = cx * chunkSize + lx
                        gy = cy * chunkSize + ly
                        h = vegHash seed gx gy
                        variant = fromIntegral ((h `shiftR` 8) .&. 0x03) ∷ Word8
                    in vegSnow + variant
                Nothing → v
            ) baseVegIds
        -- Flora sprites (trees, shrubs, wildflowers)
        floraData = computeChunkFlora seed worldSize coord
                        terrainSurfaceMap surfaceMats surfaceSlopes
                        fluidMap (wgpClimateState params) catalog

        -- Inject veg IDs into column tiles AND truncate strata above
        -- the smoothed surface. The raw column built earlier ran from
        -- 'exposeFrom' up to the RAW pre-smoothing terrain top; for
        -- island-column tiles 'smoothIslandColumns' lowered the
        -- terrain surface to @lake.surface − 1@ but the rock blocks
        -- at z ∈ (smoothed, raw] are still in 'ctMats'. The main
        -- rendering loop in 'World.Render.Quads' iterates up to
        -- @ctStartZ + length(ctMats) − 1@; without truncation those
        -- rock blocks reappear as a tiny grass cap above the water
        -- whenever the camera's zSlice rises above the lake surface.
        -- Setting them to 'matAir' (= MaterialId 0) makes the renderer
        -- skip them (it short-circuits on @mat ≡ 0@).
        finalTiles = V.imap (\idx col →
            let vegId    = vegIds VU.! idx
                matsLen  = VU.length (ctMats col)
                surfZ    = terrainSurfaceMap VU.! idx
                i        = surfZ - ctStartZ col
                truncatedMats =
                    if surfZ ≡ minBound ∨ i < 0 ∨ i ≥ matsLen - 1
                    then ctMats col
                    else
                      let topIdxs = [(j, 0) | j ← [i + 1 .. matsLen - 1]]
                      in ctMats col VU.// topIdxs
                -- Wetland demotion (see surfaceMats above): rewrite the
                -- contiguous wetland veneer at the surface so the
                -- rendered column matches the corrected surface material
                -- that vegetation/flora already saw.
                demotedMats =
                    if i < 0 ∨ i ≥ VU.length truncatedMats
                    then truncatedMats
                    else
                      let orig = truncatedMats VU.! i
                      in case demoteWetland orig of
                          Just demoted
                            | not (wetlandKeep terrainSurfaceMap waterTableMap idx) →
                              let go j | j ≥ 0 ∧ truncatedMats VU.! j ≡ orig = go (j - 1)
                                       | otherwise = j + 1
                                  runStart = go i
                              in truncatedMats VU.// [ (j, demoted) | j ← [runStart .. i] ]
                          _ → truncatedMats
                vegVec   = VU.replicate matsLen 0
                vegVec'  = if i ≥ 0 ∧ i < matsLen ∧ vegId > 0
                           then vegVec VU.// [(i, vegId)]
                           else vegVec
            in col { ctMats = demotedMats, ctVeg = vegVec' }
            ) slopedTiles

        -- Ice overlay: computed from climate, altitude, and the global
        -- ice level grid (pre-computed at world init in gtIceLevel).
        -- Sits on top of terrain or fluid for frozen ocean/lake.
        iceMap = computeChunkIce seed plates (wgpClimateState params)
                                 worldSize coord (gtIceLevel timeline)
                                 terrainSurfaceMap fluidMap

        -- Water table: subsurface saturation baseline from climate
        -- (Phase 2 simplified). For tiles under a lake surface, we
        -- bump the wt to the lake's surface so a dig through the bed
        -- still exposes water.
        wtBase = computeWaterTable (wgpClimateState params)
                                   worldSize coord terrainSurfaceMap
        waterTableMap = VU.imap (\idx wt →
            case fluidMap V.! idx of
                Just fc | fcType fc ≡ Lake → max wt (fcSurface fc)
                _                          → wt
            ) wtBase

    in (finalTiles, surfaceMap, finalTerrain, fluidMap, iceMap, floraData
       , waterTableMap, magmaOverlay)

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

-- | Wetland-soil reality check (demotion only). The climate classifier
--   ('soilFromClimate') paints muck/peat wherever the climate is
--   hot/wet, with no physical context — its slope input is curvature
--   (uniform hillsides read as flat) and the water table doesn't exist
--   yet at classification time. Gate after generation instead, where
--   the final terrain and water table are ground truth: a wetland soil
--   survives only where 'wetlandKeep' holds; otherwise it demotes to
--   the classifier's own next rung. Same post-classification pattern
--   as the Coastal beach demotion (peat/muck → sand). Used by both the
--   detail path ('generateChunk') and the zoom path
--   ('generateZoomTerrain') so the two views agree.
demoteWetland ∷ Word8 → Maybe Word8
demoteWetland 62 = Just 57   -- peat       → clay loam
demoteWetland 63 = Just 58   -- mucky peat → silty clay
demoteWetland 64 = Just 50   -- muck       → clay
demoteWetland _  = Nothing

-- | Keep a wetland soil only on a near-flat tile (in-chunk max
--   4-neighbour |Δterrain| ≤ 2 — lenient at chunk borders, matching
--   the in-chunk-only neighbour convention of computeChunkSlopes at
--   gen time) whose groundwater reaches the surface (wt ≥ terrain−1;
--   away from open water the wt map is cap-bound below terrain, so in
--   practice this keeps riparian and lakeside ground).
wetlandKeep ∷ VU.Vector Int → VU.Vector Int → Int → Bool
wetlandKeep terrain wt idx =
    let tz = terrain VU.! idx
        lx = idx `mod` chunkSize
        ly = idx `div` chunkSize
        nbrD dlx dly =
            let lx' = lx + dlx
                ly' = ly + dly
            in if lx' < 0 ∨ lx' ≥ chunkSize ∨ ly' < 0 ∨ ly' ≥ chunkSize
               then 0
               else abs (tz - terrain VU.! (ly' * chunkSize + lx'))
        flat = max (max (nbrD 1 0) (nbrD (-1) 0))
               (max (nbrD 0 1) (nbrD 0 (-1))) ≤ 2
        wet  = wt VU.! idx ≥ tz - 1
    in flat ∧ wet


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
                    → (VU.Vector Int, VU.Vector Word8, V.Vector (Maybe FluidCell))
generateZoomTerrain registry params mBorderedCache coord =
    let seed      = wgpSeed params
        worldSize = wgpWorldSize params
        timeline  = wgpGeoTimeline params
        plates    = wgpPlates params
        oceanMap  = wgpOceanMap params
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
        (despikedElev, finalMatVec) = case mBorderedCache of
            Just cache | Just cached ← HM.lookup coord cache → cached
            _ → computePipelineFromScratch ()

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
                    applyCoastalErosion seed worldSize plates registry timeline
                        oceanMap coord (timelineElevVec, timelineMatVec)
                (despikedElev', _) =
                    removeElevationSpikes 12 4 (chunkSize + 2 * chunkBorder)
                                          (postCoastElev, finalMat')
            in (despikedElev', finalMat')

        -- River + lake carves baked into the bordered region: see the
        -- matching block in 'generateChunk' for the rationale.
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

        -- All fluid placement — same composeFluidMap as the detail
        -- world, so the zoom map shows rivers, lakes, and lava in the
        -- same tiles where the world view shows them. Water table and
        -- channel mask are recomputed from the zoom path's bordered
        -- terrain so the two views see identical wt (deterministic).
        -- Same river-skipping rationale as the detail path above.
        zoomBorderedMask = computeBorderedChannelMask worldSize coord
                                                      []
                                                      finalElevVec
        zoomInteriorMask = sliceInteriorMask zoomBorderedMask

        -- Mirror the detail path's ocean test + cap pipeline so the
        -- zoom map agrees with the world view on which sub-ocean
        -- chambers become basalt seamounts (no lava) vs. above-water
        -- vents (lava emerges).
        zoomChunkIsOceanic = chunkOrNeighborOceanic params coord
        zoomMagma = discoverChunkLava (wgpVolcanoCtx params) coord
                                       interiorElev
                                       (chunkWaterSurfMap params coord)
        cappedZoomElev = applyBasaltCaps coord zoomMagma interiorElev
        rawZoomFluid = composeFluidMap params coord cappedZoomElev
                                       zoomInteriorMask zoomMagma

        -- Mirror the detail path's lava-water boundary shell so the
        -- zoom map agrees: any lava tile adjacent to water becomes
        -- bare terrain (the zoom map's per-tile colour blend then
        -- picks the material colour, which is basalt's dark grey,
        -- so the contact rim shows as a thin dark border between
        -- lava and water).
        zoomLavaShell = lavaShellMask params coord rawZoomFluid
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
        -- as generateChunk: climate baseline + lake-surface bump.
        zoomWtBase = computeWaterTable (wgpClimateState params)
                                       worldSize coord smoothedElev
        zoomWt = VU.imap (\idx wt →
            case zoomFluid V.! idx of
                Just fc | fcType fc ≡ Lake → max wt (fcSurface fc)
                _                          → wt
            ) zoomWtBase
        zoomMat = VU.imap (\idx m →
            case demoteWetland m of
                Just demoted
                  | not (wetlandKeep smoothedElev zoomWt idx)
                  → demoted
                _ → m
            ) interiorMat

    in (zoomSurface, zoomMat, zoomFluid)

