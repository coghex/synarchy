{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-chunk fluid/lava composition: global lake/river/ocean/lava-pool
--   surface placement, the basalt containment shell around lava, and
--   the dry "island column" smoother that follows it. Split out of
--   'World.Generate.Chunk' (#549) — a pure move, no behavior change.
module World.Generate.Chunk.Fluid
    ( composeFluidMap
    , chunkWaterSurfMap
    , applyBasaltCaps
    , lavaShellMask
    , poolSurfAtGlobal
    , isLavaAtGlobal
    , mergeRimCaps
    , poolRimCaps
    , chunkOrNeighborOceanic
    , applyLavaShell
    , maxColumnPeek
    , smoothIslandColumns
    , mkSurfaceMap
    ) where

import UPrelude
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import World.Types
import World.Plate (elevationAtGlobal)
import World.Magma.Types (MagmaOverlay(..))
import World.Fluid.Lake.Types
    ( WorldLakes(..), LakeChunkEntry(..), lakesInChunk )
import qualified World.Fluid.Lake.Types as WL
import World.Fluid.River.Types
    ( WorldRivers(..), RiverChunkEntry(..), riversInChunk )
import World.Fluid.OceanMask (oceanBitInChunk)

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
                → V.Vector (Maybe FluidCell)
composeFluidMap params coord terrainMap =
    let timeline    = wgpGeoTimeline params
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
