{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Generate.Chunk
    ( generateChunk
    , generateLoadedChunk
    , generateExposedColumn
    , generateZoomTerrain
    -- Post-classification soil gates (exposed for unit testing).
    , surfaceDemotion
    , demoteWetland
    , wetlandKeep
    , saltFlatKeep
    , nearFlat
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
import Structure.Types (emptyChunkStructures)
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
import World.Geology.Coastal (applyCoastalTable)
import World.Fluid.Seabed (applySeabedTable)
import World.Fluid.OceanMask (oceanBitInChunk)
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
--   * River  — tile flagged by a 'RiverChunkEntry' bitmask in the
--              global 'WorldRivers' table; surface = the entry's
--              per-tile quantised surface z.
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
        -- The coarse chunk-flood. ORed below with the tile-resolution
        -- 'gtWorldOcean' mask (the fix for whole chunks rendering dry
        -- inside an edge-connected sea, where this chunk-flood couldn't
        -- propagate through a chunk-scale sill).
        chunkIsOceanic = chunkOrNeighborOceanic params coord
        worldOceanBit  = oceanBitInChunk (gtWorldOcean timeline) coord

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
                -- Ocean = the coarse chunk-flood OR the tile-resolution
                -- edge-connected ocean ('gtWorldOcean'). The OR is the
                -- fix for whole chunks rendering dry inside a sea: the
                -- chunk-flood ('chunkIsOceanic') can't propagate through
                -- a chunk-scale sill, so sub-sea tiles it missed used to
                -- render dry at a chunk boundary; the tile mask catches
                -- them. Only ADDS ocean tiles (union) — no regression.
                isOcean = terrZ ≤ seaLevel
                          ∧ (chunkIsOceanic ∨ worldOceanBit idx)
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

-- | Mark the OUTERMOST lava tiles — any lava tile 8-adjacent to a
--   non-lava tile (water OR dry land). These become the basalt crust
--   rim ('applyLavaShell' + the column build's @matBasalt@ stamp):
--   pools read as natural volcanic features with a solidified edge
--   instead of liquid lava standing in a sharp cliff against grass
--   (user decision 2026-06-06; previously the shell fired only on
--   water contact and dry rims showed bare floating lava edges).
--
--   Within-chunk neighbours read the local fluid map (which already
--   reflects pool placement, so lava-vs-lava stays liquid);
--   cross-chunk neighbours check the global pool table at exact
--   bordered terrain (@terrAt@ — the shell only looks 1 tile out,
--   well inside the 4-tile border).
lavaShellMask ∷ WorldGenParams → ChunkCoord
              → (Int → Int → Maybe Int)
              -- ^ bordered terrain lookup (chunk-local coords; Just
              --   only within the bordered region)
              → V.Vector (Maybe FluidCell)
              → VU.Vector Bool
lavaShellMask params coord terrAt fluid =
    VU.generate (chunkSize * chunkSize) isShell
  where
    ChunkCoord cx cy = coord
    baseGX = cx * chunkSize
    baseGY = cy * chunkSize
    isShell idx = case fluid V.! idx of
        Just fc | fcType fc ≡ Lava →
            let lx = idx `mod` chunkSize
                ly = idx `div` chunkSize
            in or [ adjNonLava (lx + dx) (ly + dy)
                  | dx ← [-1, 0, 1], dy ← [-1, 0, 1]
                  , (dx, dy) ≠ (0, 0)
                  ]
        _ → False
    -- Within-chunk: read local fluid (authoritative, sees pools).
    -- Cross-chunk: the global pool table tells us whether the
    -- neighbour is lava; anything else (dry, water, cap) rims.
    adjNonLava nx ny
        | nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize =
            case fluid V.! (ny * chunkSize + nx) of
                Just fc → fcType fc ≢ Lava
                Nothing → True
        | otherwise =
            not (isLavaAtGlobal params (baseGX + nx) (baseGY + ny)
                                 (terrAt nx ny))

-- | Pool surface at @(gx, gy)@ from the global pool table, when a
--   pool bitmask covers the tile (lowest-wins like 'composeFluidMap').
--   'Nothing' = no pool claims the tile.
poolSurfAtGlobal ∷ WorldGenParams → Int → Int → Maybe Int
poolSurfAtGlobal params gx gy =
    let pools = gtWorldLavaPools (wgpGeoTimeline params)
        floorDivCS a =
            let (q, r) = a `divMod` chunkSize
            in if r < 0 then q - 1 else q
        cx = floorDivCS gx
        cy = floorDivCS gy
        lx = gx - cx * chunkSize
        ly = gy - cy * chunkSize
        idx = ly * chunkSize + lx
        surfs = [ WL.lkSurface (wlLakes pools V.! WL.lceLakeId lce)
                | lce ← V.toList
                    (lakesInChunk pools (ChunkCoord cx cy))
                , WL.lceBitmask lce VU.! idx
                ]
    in case surfs of
        [] → Nothing
        ss → Just (minimum ss)

-- | True iff the global pool table places lava at @(gx, gy)@:
--   a pool bitmask covers the tile and the pool surface is at or
--   above the tile's terrain. Mirrors the placement rule in
--   'composeFluidMap'. @mExactTerr@ should be the bordered carved
--   terrain when available.
isLavaAtGlobal ∷ WorldGenParams → Int → Int → Maybe Int → Bool
isLavaAtGlobal params gx gy mExactTerr =
    let tz = case mExactTerr of
            Just z | z ≠ minBound → z
            _ → fst (elevationAtGlobal (wgpSeed params)
                                       (wgpPlates params)
                                       (wgpWorldSize params) gx gy)
    in maybe False (≥ tz) (poolSurfAtGlobal params gx gy)

-- | Merge containment-rim caps into a chunk's magma overlay (max
--   wins where a chamber cap and a rim cap both claim a tile).
mergeRimCaps ∷ Maybe MagmaOverlay → HM.HashMap (Int, Int) Int
             → Maybe MagmaOverlay
mergeRimCaps mo rim
    | HM.null rim = mo
    | otherwise = Just $ case mo of
        Nothing → MagmaOverlay
            { moSurface   = HM.empty
            , moBasaltCap = rim
            , moRevealed  = HM.empty
            }
        Just o → o { moBasaltCap =
                        HM.unionWith max (moBasaltCap o) rim }

-- | Containment-rim caps: the OUTERMOST pool tiles (pool-covered,
--   with any 8-neighbour not pool-covered) become basalt-cap entries
--   at the POOL SURFACE elevation. 'applyBasaltCaps' then raises
--   their terrain to lava level with a basalt column; compose puts a
--   zero-depth lava film on top (pool surface ≡ raised terrain) and
--   'lavaShellMask' strips it — leaving a basalt wall FLUSH with the
--   lava surface. Without this the rim sat at the original (lower)
--   terrain and the pool's liquid edge towered over it with exposed
--   lava side faces (user report 2026-06-06: "this terrain needs to
--   contain the lava, at the same elev").
--
--   @terrAt@ is the bordered carved-terrain lookup (chunk-local
--   coords) so cross-chunk neighbours resolve exactly.
poolRimCaps ∷ WorldGenParams → ChunkCoord
            → (Int → Int → Maybe Int)
            → HM.HashMap (Int, Int) Int
poolRimCaps params coord terrAt = HM.fromList
    [ ((gx, gy), surf)
    | ly ← [0 .. chunkSize - 1]
    , lx ← [0 .. chunkSize - 1]
    , let gx = baseGX + lx
          gy = baseGY + ly
          mTz = terrAt lx ly
    , Just tz ← [mTz]
    , tz ≠ minBound
    , Just surf ← [poolSurfAtGlobal params gx gy]
    , surf ≥ tz                      -- pool actually places lava here
    , any (\(dx, dy) →
            not (isLavaAtGlobal params (gx + dx) (gy + dy)
                                 (terrAt (lx + dx) (ly + dy))))
          [ (dx, dy) | dx ← [-1, 0, 1], dy ← [-1, 0, 1]
                     , (dx, dy) ≠ (0, 0) ]
    ]
  where
    ChunkCoord cx cy = coord
    baseGX = cx * chunkSize
    baseGY = cy * chunkSize

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
--   The lake/river checks read the per-chunk bitmasks the same way
--   'composeFluidMap' does, comparing each table's surface against
--   the tile's terrain. When the caller supplies @mExactTerr@ (the
--   bordered region's carved terrain — 'lavaShellMask' always can,
--   its queries are 1 tile out) the comparison is exact; otherwise
--   it falls back to 'elevationAtGlobal' (raw plate elevation,
--   pre-erosion), which can disagree with final terrain by a few z
--   at eroded/carved tiles and cause a cosmetic 1-tile shell
--   mismatch.
isWaterAtGlobal ∷ WorldGenParams → Int → Int → Maybe Int → Bool
isWaterAtGlobal params gx gy mExactTerr =
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
        -- Exact carved terrain when the caller has it (bordered
        -- region lookups); raw plate elevation as the fallback —
        -- the approximation can disagree with final terrain by a
        -- few z at eroded/carved tiles.
        baseElev = case mExactTerr of
            Just tz | tz ≠ minBound → tz
            _ → fst (elevationAtGlobal seed plates worldSize gx gy)
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
        -- Converge fully (no arbitrary cap). Each pass only turns
        -- Nothing→Lake — strictly monotonic — so the dry-tile count
        -- decreases every productive pass and the loop must terminate at
        -- the fixpoint (≤ area passes; 1-2 in practice). A cap could only
        -- truncate a long cascade, never prevent a runaway (there is none).
        loop = do
            changes ← pass
            if changes > 0 then loop else pure ()
    loop
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

-- | 'generateChunk' wrapped into a 'LoadedChunk'. The canonical
--   tuple→record assembly — chunk loading and the zoom-map ore survey
--   both go through here so the field mapping can't drift between
--   call sites. Side decorations start empty (the loading pipeline
--   computes them later; irrelevant for transient consumers).
generateLoadedChunk ∷ MaterialRegistry → FloraCatalog → WorldGenParams
                    → ChunkCoord → LoadedChunk
generateLoadedChunk registry catalog params coord =
    let (chunkTiles, surfMap, tMap, fluidMap, iceMap, flora, wtMap, magma) =
            generateChunk registry catalog params coord
    in LoadedChunk
        { lcCoord      = coord
        , lcTiles      = chunkTiles
        , lcSurfaceMap = surfMap
        , lcTerrainSurfaceMap = tMap
        , lcFluidMap   = fluidMap
        , lcIceMap     = iceMap
        , lcFlora      = flora
        , lcSideDeco   = VU.replicate (chunkSize * chunkSize) 0
        , lcWaterTableMap = wtMap
        , lcMagma      = magma
        , lcStructures = emptyChunkStructures
        }

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

        -- Post-timeline coastal erosion: apply the GLOBAL coastal
        -- table (computed once at world init on the stitched terrain,
        -- 'identifyCoastalErosion') — lowered coastal terrain +
        -- sand/gravel/wetland material rewrites. Border tiles read
        -- their owning chunk's entry, so adjacent chunks always agree
        -- on the coastline (the old per-window pass diverged up to
        -- ~18z at seams).
        (postCoastElev, finalMatVec) =
            applyCoastalTable (gtCoastal timeline) coord
                (timelineElevVec, timelineMatVec)

        -- Despike: remove single-tile elevation outliers that survived
        -- timeline events and coastal erosion. These are typically
        -- 1-tile-wide diagonal mountain ridges (features aligned with
        -- the u-v isometric axes that end up 1 tile wide in xy) where
        -- coastal erosion lowered the cardinal neighbors but the spike
        -- itself was outside the coastal range.
        (despikedElev, _) = removeElevationSpikes 12 4 (chunkSize + 2 * chunkBorder)
                                                  (postCoastElev, finalMatVec)

        -- Seabed pass: ocean-floor relief + materials + bedrock
        -- outcrops (global 'gtSeabed' table, computed once at init).
        -- Applied here — after the first despike, before the river
        -- carve — so the rest of the pipeline (carve, strata, slope,
        -- columns) sees the seabed terrain and materials. Supersedes
        -- the old flat seaLevel−1 lake carve (whose deltas are now
        -- empty). 'seabedMatVec' is the coastal material with seabed
        -- surface materials layered on; used for the column build.
        (seabedElev, seabedMatVec) =
            applySeabedTable (gtSeabed timeline) coord
                (despikedElev, finalMatVec)

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
        --
        -- WRAP-SEAM INVARIANT (why the chunk key below is derived from
        -- the UNWRAPPED gx/gy — "no u-wrap on the keys", same as
        -- 'World.Geology.Coastal.Types'): the carve tables are built
        -- from the stitched square grid, which DOUBLE-COVERS the seam
        -- region — every near-seam physical tile appears at both its
        -- canonical position and its u-alias, and the tables hold
        -- entries under BOTH chunk keys with identical content. A
        -- bordered lookup from a canonical chunk near the seam
        -- therefore hits the alias entry directly; wrapping the key
        -- would be redundant. The only unkeyed reach is a border that
        -- exits the square grid entirely, which by the diamond
        -- geometry is glacier-corner territory where no carve content
        -- exists (lookup misses resolve to delta 0, correctly).
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
            let z = seabedElev VU.! idx
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
                                  (carvedElevVec, seabedMatVec)

        lookupFinal lx ly =
            if inBorder lx ly
            then ( finalElevVec VU.! toIndex lx ly
                 , seabedMatVec VU.! toIndex lx ly
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
        magmaOverlayBase = discoverChunkLava (wgpVolcanoCtx params) coord
                                          rawTerrainSurfaceMap
                                          (chunkWaterSurfMap params coord)

        -- Containment rim: outermost pool tiles raised to the pool
        -- surface as basalt caps (see 'poolRimCaps').
        rimCaps = poolRimCaps params coord
                      (\lx ly → if inBorder lx ly
                                then Just (finalElevVec VU.! toIndex lx ly)
                                else Nothing)
        magmaOverlay = mergeRimCaps magmaOverlayBase rimCaps

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

        -- Lava-water boundary shell: any lava tile 8-adjacent to a
        -- non-lava tile (water OR dry land — see 'lavaShellMask')
        -- gets cleared and the column-build below stamps matBasalt
        -- on top, so lava never sits edge-to-edge with water or
        -- bare ground. Interior lava is preserved — only the
        -- contact rim becomes solid rock.
        lavaShell = lavaShellMask params coord
                        (\lx ly → if inBorder lx ly
                                  then Just (finalElevVec VU.! toIndex lx ly)
                                  else Nothing)
                        rawFluidMap
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
                    -- Final visible terrain for determining how far down
                    -- to expose strata (cliff face visibility). In-chunk
                    -- neighbours must read 'terrainSurfaceMap', not the
                    -- pre-smoothing bordered terrain, because
                    -- 'smoothIslandColumns' may lower a lake-adjacent tile
                    -- after 'finalElevVec' is computed.
                    finalSelf = terrainSurfaceMap VU.! idx
                    visibleTerrainOr olx oly fallback =
                        if olx ≥ 0 ∧ olx < chunkSize
                           ∧ oly ≥ 0 ∧ oly < chunkSize
                        then terrainSurfaceMap VU.! columnIndex olx oly
                        else lookupElevOr olx oly fallback
                    exposeN = visibleTerrainOr lx (ly - 1) rawSurfZ
                    exposeS = visibleTerrainOr lx (ly + 1) rawSurfZ
                    exposeE = visibleTerrainOr (lx + 1) ly rawSurfZ
                    exposeW = visibleTerrainOr (lx - 1) ly rawSurfZ
                    neighborMinZ = min exposeN (min exposeS (min exposeE exposeW))
                    exposeFrom = minimum [rawSurfZ, finalSelf, neighborMinZ]
                    -- Post-coastal neighbor elevations for the strata cache.
                    finalN = lookupElevOr lx (ly - 1) rawSurfZ
                    finalS = lookupElevOr lx (ly + 1) rawSurfZ
                    finalE = lookupElevOr (lx + 1) ly rawSurfZ
                    finalW = lookupElevOr (lx - 1) ly rawSurfZ
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
        --
        -- Cross-border elevations for the wetland flat test come from
        -- the bordered post-carve vector, so the gate sees the same
        -- neighbours the renderer does.
        wetOutElev olx oly =
            if inBorder olx oly
            then Just (finalElevVec VU.! toIndex olx oly)
            else Nothing
        surfaceMats = VU.imap (\idx m →
            case surfaceDemotion wetOutElev terrainSurfaceMap waterTableMap idx m of
                Just demoted → demoted
                Nothing      → m
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
                      in case surfaceDemotion wetOutElev terrainSurfaceMap waterTableMap idx orig of
                          Just demoted →
                              let go j | j ≥ 0 ∧ truncatedMats VU.! j ≡ orig = go (j - 1)
                                       | otherwise = j + 1
                                  runStart = go i
                              in truncatedMats VU.// [ (j, demoted) | j ← [runStart .. i] ]
                          Nothing → truncatedMats
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
        -- (Phase 2 simplified), made fluid-aware by 'applyFluidWt' —
        -- under-bed bump (lake/river/ocean) + fresh-water shore halo.
        wtBase = computeWaterTable (wgpClimateState params)
                                   worldSize coord terrainSurfaceMap
        waterTableMap = applyFluidWt fluidMap wtBase

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

-- | All post-classification soil reality checks in one place: given a
--   surface material and the final terrain context, return the demoted
--   material if the climate-only classifier's pick can't physically
--   survive here, or 'Nothing' if it stays. Currently gates wetland
--   soils (need flat + wet ground) and salt flat (needs a flat basin
--   floor). Shared by the detail ('generateChunk') and zoom
--   ('generateZoomTerrain') paths so the two views agree.
surfaceDemotion ∷ (Int → Int → Maybe Int) → VU.Vector Int → VU.Vector Int
                → Int → Word8 → Maybe Word8
surfaceDemotion outElev terrain wt idx m
    | Just demoted ← demoteWetland m
    , not (wetlandKeep outElev terrain wt idx) = Just demoted
    | m ≡ 67                                            -- salt flat
    , not (saltFlatKeep outElev terrain idx)   = Just 66 -- → light gravel
    | otherwise                                = Nothing

-- | Keep a wetland soil only on a near-flat tile (max 4-neighbour
--   |Δterrain| ≤ 2) whose groundwater reaches the surface
--   (wt ≥ terrain−1; the climate baseline tops out at terrain−2, so
--   only tiles inside 'applyFluidWt's under-fluid bump or fresh-water
--   halo can pass — i.e. this keeps riparian and lakeside ground).
--
--   @outElev@ supplies elevations for neighbours OUTSIDE the chunk
--   interior (chunk-local coords; both call paths pass their bordered
--   post-carve vector). The flat test used to be in-chunk-lenient,
--   which was invisible while no dry wetland could pass the wet test;
--   once the halo restored shore wetlands, border tiles whose
--   cross-border neighbour sat 3+ lower slipped through as
--   wetland-on-cliff (audit WETLAND_ON_SLOPE, 29 hits seed 42 w64 —
--   all at lx/ly ∈ {0,15}).
wetlandKeep ∷ (Int → Int → Maybe Int) → VU.Vector Int → VU.Vector Int
            → Int → Bool
wetlandKeep outElev terrain wt idx =
    let tz  = terrain VU.! idx
        wet = wt VU.! idx ≥ tz - 1
    -- Sub-sea tiles are seabed, not land: the ocean-floor pass
    -- ('World.Fluid.Seabed') places muck on the deep floor by design,
    -- so the wetland demotion (a dry-hillside concern) must leave it
    -- alone — otherwise deep seabed muck (64) gets demoted to clay
    -- (50) on every steep stretch of sea floor.
    in tz ≤ seaLevel ∨ (nearFlat outElev terrain idx ∧ wet)

-- | Keep salt flat (matId 67) only on a near-flat basin floor. The
--   climate classifier paints salt flat on any cold + hyper-arid tile
--   regardless of slope, but an evaporite pan is a flat basin feature —
--   on a slope it reads as a stripe of pan up a hillside. Gate it the
--   same way as the wetland soils and demote a sloped salt flat to the
--   classifier's own next rung (light gravel, 66). No wet test: salt
--   pans are dry by definition. Sub-sea tiles are left to the seabed
--   pass, as in 'wetlandKeep'.
saltFlatKeep ∷ (Int → Int → Maybe Int) → VU.Vector Int → Int → Bool
saltFlatKeep outElev terrain idx =
    let tz = terrain VU.! idx
    in tz ≤ seaLevel ∨ nearFlat outElev terrain idx

-- | Near-flat test shared by the soil gates: max 4-neighbour
--   |Δterrain| ≤ 2. @outElev@ supplies elevations for neighbours
--   outside the chunk interior (chunk-local coords); both call paths
--   pass their bordered post-carve vector so the gate sees the same
--   neighbours the renderer does. The 'minBound' beyond-glacier
--   sentinel reads as flat (Δ 0).
nearFlat ∷ (Int → Int → Maybe Int) → VU.Vector Int → Int → Bool
nearFlat outElev terrain idx =
    let tz = terrain VU.! idx
        lx = idx `mod` chunkSize
        ly = idx `div` chunkSize
        nbrD dlx dly =
            let lx' = lx + dlx
                ly' = ly + dly
            in if lx' < 0 ∨ lx' ≥ chunkSize ∨ ly' < 0 ∨ ly' ≥ chunkSize
               then case outElev lx' ly' of
                      Just z | z ≠ minBound → abs (tz - z)
                      _                     → 0
               else abs (tz - terrain VU.! (ly' * chunkSize + lx'))
    in max (max (nbrD 1 0) (nbrD (-1) 0))
           (max (nbrD 0 1) (nbrD 0 (-1))) ≤ 2

-- | Fluid-aware water table: lift the climate baseline where surface
--   water dictates the local groundwater level.
--
--   1. UNDER-FLUID BUMP — tiles under any water body (lake, river,
--      ocean) get @wt ≥ fluid surface@, so a dig through the bed
--      exposes water. Lava pools get no bump.
--   2. FRESH-WATER HALO — dry tiles within 'wtHaloRadius' (Chebyshev)
--      of a lake or river tile get @wt = surface + 1 − distance@:
--      riparian groundwater that decays away from the shore. This is
--      what lets 'wetlandKeep' pass on dry land at all — the climate
--      baseline is at best @terrain − 2@ against a @terrain − 1@ wet
--      test, so without the halo wetland soils survive only
--      underwater. Fresh water only: ocean shores keep their beaches
--      (the Coastal pass demotes wetland soils to sand there) and
--      stay out of the wetland gate.
--
--   In-chunk only — fluid just across a chunk border doesn't halo in,
--   the same leniency convention 'wetlandKeep' uses for its flat
--   test. 'minBound' sentinel (beyond-glacier) passes through.
--   Shared by 'generateChunk' and 'generateZoomTerrain' so the two
--   views demote identically.
applyFluidWt ∷ V.Vector (Maybe FluidCell) → VU.Vector Int → VU.Vector Int
applyFluidWt fluid wtBase = VU.generate (chunkSize * chunkSize) $ \idx →
    let wt0 = wtBase VU.! idx
    in if wt0 ≡ minBound
       then wt0
       else case fluid V.! idx of
            Just fc | fcType fc ≠ Lava → max wt0 (fcSurface fc)
                    | otherwise        → wt0
            Nothing → case haloSurfs idx of
                        [] → wt0
                        ss → max wt0 (maximum ss)
  where
    haloSurfs idx =
        let lx = idx `mod` chunkSize
            ly = idx `div` chunkSize
        in [ fcSurface fc + 1 - d
           | dy ← [-wtHaloRadius .. wtHaloRadius]
           , dx ← [-wtHaloRadius .. wtHaloRadius]
           , let d = max (abs dx) (abs dy)
           , d > 0
           , let lx' = lx + dx
                 ly' = ly + dy
           , lx' ≥ 0, lx' < chunkSize, ly' ≥ 0, ly' < chunkSize
           , Just fc ← [fluid V.! (ly' * chunkSize + lx')]
           , fcType fc ≡ Lake ∨ fcType fc ≡ River
           ]

-- | How far the fresh-water groundwater halo reaches from a lake or
--   river tile (Chebyshev tiles). With the @surface + 1 − d@ decay, a
--   flat bank one z above the water passes the wet test at distance
--   1, and at-water-level flats pass out to distance 2.
wtHaloRadius ∷ Int
wtHaloRadius = 3


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
                                       zoomInteriorMask zoomMagma

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
