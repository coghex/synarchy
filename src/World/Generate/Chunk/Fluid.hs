{-# LANGUAGE Strict #-}
-- | Per-chunk fluid/lava composition: global lake/river/ocean/lava-pool
--   surface placement, the basalt containment shell around lava, and
--   the dry "island column" smoother that follows it. Split out of
--   'World.Generate.Chunk' (#549) — a pure move, no behavior change.
module World.Generate.Chunk.Fluid
    ( composeFluidMap
    , chunkWaterSurfMap
    , applyBasaltCaps
    , lavaShellMask
    , mergeRimCaps
    , poolRimCaps
    , chunkOrNeighborOceanic
    , applyLavaShell
    , maxColumnPeek
    , smoothIslandColumns
    , mkSurfaceMap
    , lakeSurfaceMap
    , riverSurfaceMap
    ) where

import UPrelude
import Control.Monad.ST (ST, runST)
import Data.List (group, sort)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import World.Types
import World.Generate.Coordinates (globalToChunk)
import World.Plate (elevationAtGlobal)
import World.Magma.Types (MagmaOverlay(..))
import World.Fluid.Lake.Types
    ( WorldLakes(..), LakeChunkEntry(..), lakesInChunk )
import qualified World.Fluid.Lake.Types as WL
import World.Fluid.River.Types
    ( WorldRivers(..), RiverChunkEntry(..), riversInChunk )
import World.Fluid.OceanMask (oceanBitInChunk)

-- * The per-tile fluid-surface fold
--
-- Every per-tile surface map below is the same fold: start from a
-- chunk-area vector of 'minBound', walk the chunk's entries in one
-- global fluid table, and on each tile an entry's bitmask claims,
-- keep the LOWEST surface seen so far. Lowest-wins keeps water within
-- terrain where two bodies would otherwise both claim a tile; the
-- global floods give one label per tile, so that overlap shouldn't
-- happen and the rule is defensive.
--
-- 'minBound' is the ABSENT sentinel, not a candidate value — and it is
-- also the smallest 'Int', so the merge is written against the
-- sentinel rather than as a plain 'min'. Combining two sources on one
-- tile:
--
--   * absent   + absent   → 'minBound' (nothing claims the tile)
--   * absent   + real @s@ → @s@
--   * real @a@ + real @b@ → @min a b@
--
-- Two shapes exist because the two tables carry their surfaces
-- differently: a lake-keyed entry has ONE surface for the whole body
-- ('WL.lkSurface'), a river-keyed entry a per-tile surface vector
-- ('rcePerTileSurfZ'). Both fold into the same accumulator, which is
-- what lets 'chunkWaterSurfMap' merge lakes and rivers without a
-- second copy of the rule.

-- | Tiles in one chunk — the length of every surface map here.
chunkArea ∷ Int
chunkArea = chunkSize * chunkSize

-- | A fresh fold accumulator: chunk-area long, every tile unclaimed.
newSurfaceAccum ∷ ST s (VUM.MVector s Int)
newSurfaceAccum = VUM.replicate chunkArea minBound

-- | Claim tile @i@ at surface @s@, keeping the lower value when the
--   tile is already claimed. The explicit sentinel test is what stops
--   'minBound' from winning as if it were a real surface.
claimSurface ∷ VUM.MVector s Int → Int → Int → ST s ()
claimSurface v i s = do
    cur ← VUM.read v i
    when (cur ≡ minBound ∨ s < cur) $ VUM.write v i s

-- | Fold a lake-keyed table's entries for one chunk into @v@.
foldLakeSurfaces ∷ WorldLakes → ChunkCoord → VUM.MVector s Int
                 → ST s ()
foldLakeSurfaces table coord v =
    V.forM_ (lakesInChunk table coord) $ \lce → do
        let bm   = lceBitmask lce
            surf = WL.lkSurface (wlLakes table V.! lceLakeId lce)
        forM_ [0 .. chunkArea - 1] $ \i →
            when (bm VU.! i) $ claimSurface v i surf

-- | Fold the river table's entries for one chunk into @v@.
foldRiverSurfaces ∷ WorldRivers → ChunkCoord → VUM.MVector s Int
                  → ST s ()
foldRiverSurfaces table coord v =
    V.forM_ (riversInChunk table coord) $ \rce → do
        let bm    = rceBitmask rce
            surfs = rcePerTileSurfZ rce
        forM_ [0 .. chunkArea - 1] $ \i →
            when (bm VU.! i) $ claimSurface v i (surfs VU.! i)

-- | The lake-keyed fold on its own. Serves real lakes and lava pools
--   alike — pools are carried in the same 'WorldLakes' shape.
lakeSurfaceMap ∷ WorldLakes → ChunkCoord → VU.Vector Int
lakeSurfaceMap table coord = VU.create $ do
    v ← newSurfaceAccum
    foldLakeSurfaces table coord v
    pure v

-- | The river-keyed fold on its own.
riverSurfaceMap ∷ WorldRivers → ChunkCoord → VU.Vector Int
riverSurfaceMap table coord = VU.create $ do
    v ← newSurfaceAccum
    foldRiverSurfaces table coord v
    pure v

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
composeFluidMap ∷ WorldGenParams → ChunkCoord → VU.Vector Int
                → V.Vector (Maybe FluidCell)
composeFluidMap params coord terrainMap =
    let timeline    = wgpGeoTimeline params
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

        -- Per-tile lake surface lookup: the chunk's 'LakeChunkEntry'
        -- vector pre-folded into a single per-tile lake surface (or
        -- 'minBound' for no-lake) by the shared fold above. With at
        -- most a handful of lakes per chunk (typically 1–2), it is
        -- cheap.
        lakeSurfMap ∷ VU.Vector Int
        lakeSurfMap = lakeSurfaceMap worldLakes coord

        -- Per-tile river surface lookup: the same fold over the river
        -- table, which carries a per-tile quantised surface z instead
        -- of one lake-wide value.
        riverSurfMap ∷ VU.Vector Int
        riverSurfMap = riverSurfaceMap worldRivers coord

        -- Per-tile lava-pool surface. Pools come from the global
        -- 'gtWorldLavaPools' table
        -- ('World.Magma.Pool.identifyLavaPools') — flat lava lakes
        -- pooled in depressions at the breach cluster's lowest
        -- opening — and are carried in the lake table's shape, so
        -- they take the lake-keyed fold. Highest fluid priority:
        -- lava beats water.
        lavaSurfMap ∷ VU.Vector Int
        lavaSurfMap = lakeSurfaceMap (gtWorldLavaPools timeline) coord

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
               else if lvSurf ≢ minBound ∧ lvSurf ≥ terrZ
                    then Just (FluidCell Lava lvSurf)
               else if isOcean
                    then Just (FluidCell Ocean seaLevel)
                    else
                      -- River > Lake. By construction river tiles
                      -- aren't inside any lake, but defensive priority
                      -- keeps the picture consistent at edges.
                      if rvSurf ≢ minBound ∧ rvSurf ≥ terrZ
                      then Just (FluidCell River rvSurf)
                      else if lkSurf ≢ minBound ∧ lkSurf ≥ terrZ
                           then Just (FluidCell Lake lkSurf)
                           else Nothing

    -- Surface lava comes entirely from the pool table above; the
    -- magma overlay carries basalt caps only, so no per-chunk overlay
    -- pass remains.
    in waterFluid

-- | Per-tile water surface from the global lake + river tables,
--   independent of terrain ('minBound' = no water claims the tile).
--   Both sources fold into ONE accumulator through the same helpers
--   'composeFluidMap' uses, so the two can no longer disagree about
--   where water is. The cross-source merge is lowest-real-surface —
--   deliberately NOT 'composeFluidMap's River-over-Lake
--   classification, because this map answers how HIGH the water is,
--   not which fluid class the tile renders as.
--
--   Used by 'discoverChunkLava' for the water-body-aware basalt-cap
--   rule: a chamber breaching below a LAKE or RIVER surface gets
--   capped the same way sub-sea breaches always have, instead of
--   emitting lava into the water column.
chunkWaterSurfMap ∷ WorldGenParams → ChunkCoord → VU.Vector Int
chunkWaterSurfMap params coord = VU.create $ do
    let timeline = wgpGeoTimeline params
    v ← newSurfaceAccum
    foldLakeSurfaces (gtWorldLakes timeline) coord v
    foldRiverSurfaces (gtWorldRivers timeline) coord v
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
                  , (dx, dy) ≢ (0, 0)
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
        (cc, (lx, ly)) = globalToChunk gx gy
        idx = ly * chunkSize + lx
        surfs = [ WL.lkSurface (wlLakes pools V.! WL.lceLakeId lce)
                | lce ← V.toList
                    (lakesInChunk pools cc)
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
            Just z | z ≢ minBound → z
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
            { moBasaltCap = rim
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
    , tz ≢ minBound
    , Just surf ← [poolSurfAtGlobal params gx gy]
    , surf ≥ tz                      -- pool actually places lava here
    , any (\(dx, dy) →
            not (isLavaAtGlobal params (gx + dx) (gy + dy)
                                 (terrAt (lx + dx) (ly + dy))))
          [ (dx, dy) | dx ← [-1, 0, 1], dy ← [-1, 0, 1]
                     , (dx, dy) ≢ (0, 0) ]
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
            in if isOceanic ∧ terrZ ≤ seaLevel ∧ terrZ ≢ minBound
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
                        -- Terrain is read from the SAME mutable vector
                        -- the smoothing writes to, so this loop has one
                        -- source of truth per map and no reader has to
                        -- reconstruct why an immutable read stayed
                        -- correct across passes (#1131).
                        t  ← VUM.read mTerr idx
                        ms ← sequence
                                [ neighborSurf (lx - 1) ly
                                , neighborSurf (lx + 1) ly
                                , neighborSurf lx       (ly - 1)
                                , neighborSurf lx       (ly + 1)
                                ]
                        -- A surface qualifies when it occurs in at
                        -- least three of the ≤ 4 valid cardinal
                        -- neighbors. With at most four samples no
                        -- second value can also reach three, so the
                        -- first qualifying group is the only one —
                        -- 'group' needs the 'sort' because equal
                        -- surfaces need not be adjacent in the sample
                        -- order.
                        let ns = [ s | Just s ← ms ]
                            candidate = listToMaybe
                                [ z | zs@(z : _) ← group (sort ns)
                                    , length zs ≥ 3 ]
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

-- | Render-surface map for a generated chunk: the river-flat rule
-- ('renderedSurfaceZ') applied per column.
--
-- The rule itself lives in 'World.Fluid.Types.renderedSurfaceZ' and is
-- written there ONCE (#1112); the sim writeback
-- ('Sim.Thread.emitWorldDirtyFluids') and every player-edit path
-- ('World.Edit.Apply') call the same function. Note that the chunk-load
-- seeding paths in 'World.Load.Stage' and 'World.Thread.Command.Init'
-- do re-touch this map with a type-agnostic @max@ against the fluid
-- surface; that pass is idempotent under this rule and makes no
-- River-versus-other decision of its own.
mkSurfaceMap ∷ VU.Vector Int → V.Vector (Maybe FluidCell) → VU.Vector Int
mkSurfaceMap terrain fluid =
    VU.imap (\idx surfZ → renderedSurfaceZ surfZ (fluid V.! idx)) terrain
