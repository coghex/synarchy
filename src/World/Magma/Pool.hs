{-# LANGUAGE Strict, UnicodeSyntax #-}

-- | Lava pools — surface lava placed with fluid semantics.
--
--   v1 placed a per-tile lava film at terrain z wherever a chamber or
--   chute crossed the surface, which draped staircases of lava down
--   volcano flanks (each tile at its own elevation). This module
--   replaces the film with pooling: lava exits a breach cluster at
--   its LOWEST opening (pressure equalises inside the chute) and
--   floods the connected region of terrain at or below that level —
--   a flat lava lake in the local depression.
--
--   Computed once at timeline build on the same stitched world
--   terrain the lake identifier uses, and stored as a 'WorldLakes'-
--   shaped table ('gtWorldLavaPools') so chunk gen reads pools
--   exactly like lakes: bitmask bit set ∧ pool surface ≥ terrain.
--   Cross-chunk agreement is automatic, same as lakes.
--
--   Water interaction (user decision 2026-06-05): water tiles are
--   flood BARRIERS — a pool stops at the shoreline and the chunk-gen
--   shell mask ('lavaShellMask', 8-neighbour) turns the contact rim
--   to basalt. Breach tiles under water (sea, lake, river) don't
--   join clusters at all — they take the basalt-cap path in
--   'discoverChunkLava'.
module World.Magma.Pool
    ( identifyLavaPools
    , defaultLavaPoolDepth
    , defaultLavaPoolRadius
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import World.Chunk.Types (ChunkCoord(..), chunkSize)
import World.Constants (seaLevel)
import World.Fluid.Lake.Types
    ( Lake(..), WorldLakes(..), LakeChunkEntry(..), emptyWorldLakes
    , lakesInChunk )
import World.Fluid.River.Types (WorldRivers, riversInChunk
    , RiverChunkEntry(..), wrCarveDelta)
import World.Geology.Timeline.Types (EventBBox(..))
import World.Magma.Types (VolcanoCtx(..), MagmaSource(..))
import World.Magma.Shape (pointInShape)

-- | Default pool depth and radius — the config-exposed levers
--   ('wgcLavaPoolDepth' / 'wgcLavaPoolRadius' in
--   "World.Generate.Config"). 'identifyLavaPools' takes explicit
--   values; these defaults exist for callers without a config.
--
--   Depth: how deep a pool may stand above the basin floor it
--   drains to — bounds the fill so a flank breach doesn't fill an
--   entire valley to its rim (lava cools; a fixed head above the
--   landing floor is the v1 stand-in for a volume model).
defaultLavaPoolDepth ∷ Int
defaultLavaPoolDepth = 4

-- | Default outer rim radius (tiles, Euclidean from the landing
--   point). On dead-flat ground the basin grow has no saddle to
--   stop at, so the radius is the effective bound there. The area
--   cap derives as ⌈π·r²⌉.
defaultLavaPoolRadius ∷ Int
defaultLavaPoolRadius = 18

-- | Max steepest-descent steps from a breach to its landing point.
--   Lava that would run further than this just pools where the walk
--   stops (cooling, again).
maxDescentSteps ∷ Int
maxDescentSteps = 96

-- | Per-tile hash jitter on the rim radius — roughens the rim by
--   ±3 tiles wherever the radius (rather than a saddle) is what
--   stops growth.
rimJitter ∷ Int
rimJitter = 3

-- | Identify all lava pools. Inputs mirror 'identifyWorldLakes':
--   the stitched world terrain grid (indexed
--   @(gy+half)*worldTiles + (gx+half)@, 'minBound' beyond glacier)
--   plus the already-built lake and river tables (water barriers).
--
--   @poolDepth@ / @poolRadius@ are the config levers
--   (@lava_pool_depth@ / @lava_pool_radius@ in world-gen config):
--   depth = max head above the landing-point floor, radius = max
--   Euclidean footprint (area cap derives as ⌈π·r²⌉).
identifyLavaPools
    ∷ Int               -- ^ worldSize (chunks per side)
    → Int               -- ^ pool depth lever
    → Int               -- ^ pool radius lever
    → VolcanoCtx
    → WorldLakes        -- ^ water barrier: lakes
    → WorldRivers       -- ^ water barrier: rivers
    → VU.Vector Int     -- ^ world terrain (worldTiles²)
    → WorldLakes        -- ^ pool table (reusing the lake shape)
identifyLavaPools worldSize poolDepth poolRadius ctx lakes rivers terrain
    | V.null (vcSources ctx) = emptyWorldLakes
    | otherwise =
        let pools = concatMap poolsForSource
                              (V.toList (vcSources ctx))
            -- Merge pools sharing tiles (two sources breaching the
            -- same basin): keep distinct pools; compose-time merge
            -- in 'composeFluidMap' resolves overlaps lowest-wins.
            lakesV = V.fromList (map poolToLake pools)
            byChunk = buildChunkIndex pools
        in if null pools
           then emptyWorldLakes
           else WorldLakes
                { wlLakes      = lakesV
                , wlByChunk    = byChunk
                , wlCarveDelta = HM.empty
                }
  where
    worldTiles = worldSize * chunkSize
    halfWorld  = worldTiles `div` 2

    -- Area cap derived from the radius lever: ⌈π·r²⌉, so the two
    -- caps always agree.
    poolArea ∷ Int
    poolArea = ceiling (pi * fromIntegral (poolRadius * poolRadius)
                        ∷ Double)

    -- CARVE-AWARE terrain: the stitched grid is pre-carve, but chunk
    -- gen renders @terrain − carveAt@ (river channels, clamped-lake
    -- basins). Pools identified on the raw grid float over carved
    -- gorges — observed as 19z-deep lava sheets burying rivers (seed
    -- 1840733254 w64 plates 10 @(-21..-10, 49)). Subtracting the
    -- same deltas compose subtracts keeps pools, barriers, and the
    -- rendered world in one reality.
    terrainAt ∷ Int → Int → Int
    terrainAt gx gy =
        let gxo = gx + halfWorld
            gyo = gy + halfWorld
        in if gxo < 0 ∨ gxo ≥ worldTiles ∨ gyo < 0 ∨ gyo ≥ worldTiles
           then minBound
           else
             let z = terrain VU.! (gyo * worldTiles + gxo)
             in if z ≡ minBound then z else z - carveDeltaAt gx gy

    -- Mirror of 'World.Generate.Chunk.carveAt' over the same global
    -- tables (max of river + lake delta per tile).
    carveDeltaAt ∷ Int → Int → Int
    carveDeltaAt gx gy =
        let (cc, li) = chunkOf gx gy
            dr = case HM.lookup cc (wrCarveDelta rivers) of
                Just dv → dv VU.! li
                Nothing → 0
            dl = case HM.lookup cc (wlCarveDelta lakes) of
                Just dv → dv VU.! li
                Nothing → 0
        in max dr dl

    -- Water barrier: rendered ocean proxy (sub-sea terrain), or a
    -- lake/river whose table surface covers this tile's terrain.
    -- Mirrors the per-tile classification in 'composeFluidMap'.
    isWater ∷ Int → Int → Bool
    isWater gx gy =
        let tz = terrainAt gx gy
        in tz ≡ minBound
           ∨ tz ≤ seaLevel
           ∨ lakeAt gx gy tz
           ∨ riverAt gx gy tz

    -- RAW chunk coords (no u-wrap) — the lake/river/carve tables and
    -- composeFluidMap's per-chunk lookups all key by the chunk's raw
    -- coord ('buildChunkIndex' convention); wrapping here would file
    -- pool entries under keys the reader never asks for.
    chunkOf ∷ Int → Int → (ChunkCoord, Int)
    chunkOf gx gy =
        let fd a = let (q, r) = a `divMod` chunkSize
                   in if r < 0 then q - 1 else q
            cx = fd gx
            cy = fd gy
            lx = gx - cx * chunkSize
            ly = gy - cy * chunkSize
        in ( ChunkCoord cx cy
           , ly * chunkSize + lx )

    lakeAt gx gy tz =
        let (cc, i) = chunkOf gx gy
        in V.any (\lce → lceBitmask lce VU.! i
                       ∧ lkSurface (wlLakes lakes
                                    V.! lceLakeId lce) ≥ tz)
                 (lakesInChunk lakes cc)

    riverAt gx gy tz =
        let (cc, i) = chunkOf gx gy
        in V.any (\rce → rceBitmask rce VU.! i
                       ∧ rcePerTileSurfZ rce VU.! i ≥ tz)
                 (riversInChunk rivers cc)

    -- All pools spawned by one source: breach scan over its bbox,
    -- 8-connected clustering, then a flood per cluster from its
    -- lowest opening.
    poolsForSource src =
        let shapes = msShapes src
            bb = msBBox src
            breaches = HS.fromList
                [ (gx, gy)
                | (gx, gy) ← bboxTiles bb
                , let tz = terrainAt gx gy
                , tz ≠ minBound
                , not (isWater gx gy)
                , any (pointInShape worldSize gx gy tz) shapes
                ]
        in [ pool
           | cluster ← clusters8 breaches
           , Just pool ← [floodPool cluster]
           ]

    bboxTiles (EventBBox xlo ylo xhi yhi) =
        [ (gx, gy) | gy ← [ylo .. yhi], gx ← [xlo .. xhi] ]

    -- 8-connected components of the breach set.
    clusters8 ∷ HS.HashSet (Int, Int) → [[(Int, Int)]]
    clusters8 s0 = go s0
      where
        go s = case take 1 (HS.toList s) of
            []      → []
            (t : _) →
                let comp = grow (HS.singleton t) [t]
                in HS.toList comp : go (HS.difference s comp)
              where
                grow acc [] = acc
                grow acc (p@(px, py) : rest) =
                    let nbrs = [ q
                               | dx ← [-1, 0, 1], dy ← [-1, 0, 1]
                               , (dx, dy) ≠ (0, 0)
                               , let q = (px + dx, py + dy)
                               , HS.member q s
                               , not (HS.member q acc)
                               ]
                    in grow (foldr HS.insert acc nbrs) (nbrs ⧺ rest)

    -- Pour semantics: lava exits the cluster at its lowest breach,
    -- runs steepest-descent to a landing point (local minimum, water
    -- shoreline, or the step cap), then basin-fills FROM THE FLOOR
    -- UP — level = floor + the pool-depth lever, clamped below the
    -- basin's spill saddle so the pool can never sheet downhill out
    -- of the depression (the bug in the first cut of this module:
    -- a sub-level-set flood at breach z covered every connected
    -- lower tile, producing 2000-tile lava plateaus standing high
    -- over open slopes).
    floodPool ∷ [(Int, Int)] → Maybe (Int, [(Int, Int)])
    floodPool [] = Nothing
    floodPool cluster =
        let breachZ = minimum [ terrainAt gx gy | (gx, gy) ← cluster ]
            exit = case [ p | p@(gx, gy) ← cluster
                            , terrainAt gx gy ≡ breachZ ] of
                (p : _) → p
                []      → minimum cluster  -- unreachable; breachZ ∈ cluster
            m = descend 0 exit
            mz = uncurry terrainAt m
            -- Priority-grow the basin of @m@ lowest-boundary-first.
            -- When growth pops a tile LOWER than the running level,
            -- we've crossed the spill saddle — stop there. Water
            -- tiles drain the basin the same way (barrier + spill).
            targetLevel = min (mz + poolDepth)
                              (breachZ + poolDepth)
            (level, region) = grow m targetLevel mz
                                   (HS.singleton m)
                                   [(mz, m)] 0 []
            tiles = [ p | p ← region
                        , uncurry terrainAt p ≤ level ]
        in if null tiles
           then Nothing
           else Just (level, tiles)
      where
        descend n p@(px, py)
            | n ≥ maxDescentSteps = p
            | otherwise =
                let tz = terrainAt px py
                    downs = [ (tzq, q)
                            | (dx, dy) ← [(1,0),(-1,0),(0,1),(0,-1)]
                            , let q = (px + dx, py + dy)
                            , let tzq = uncurry terrainAt q
                            , tzq ≠ minBound
                            , tzq < tz
                            ]
                in case downs of
                    [] → p
                    _  → let (_, q) = minimum downs
                         in if uncurry isWater q
                            -- Stop on the shore tile, not in water.
                            then p
                            else descend (n + 1) q

        -- Sorted-list priority queue: pools are small (≤ poolArea)
        -- so O(n) insertion is fine.
        --
        -- Tie order is LOAD-BEARING: equal-elevation tiles must queue
        -- FIFO ('insertQ' appends AFTER equal keys). With ties going
        -- to the FRONT the queue degenerates into a stack on flat
        -- terrain — depth-first growth that marches along whichever
        -- neighbour lands at the head, producing perfectly straight
        -- 1-tile-wide "lava rivers" along the +x axis (seed 7 w128:
        -- a 15×1 pool; seed 42: 38×1). Plateau-snapped terrain made
        -- flats ubiquitous, so this is the common case, not the edge
        -- case. FIFO ties = breadth-first = compact pools.
        --
        -- The jittered radius bound ('withinRim') keeps the rim
        -- organic when growth is radius-limited (large flats): a
        -- hash-perturbed Euclidean disc instead of a clean BFS
        -- diamond.
        grow m targetLevel curLevel visited frontier n acc =
            case popMin frontier of
                Nothing → (curLevel, acc)
                Just ((tz, p), rest)
                    | n ≥ poolArea → (curLevel, acc)
                    -- Crossing a saddle: the next-lowest boundary
                    -- tile is below the level we've already had to
                    -- rise to — lava would drain over it. Stop; the
                    -- pool stands at the level reached so far.
                    | tz < curLevel → (curLevel, acc)
                    | tz > targetLevel → (targetLevel, acc)
                    | uncurry isWater p → (min curLevel tz, acc)
                    | otherwise →
                        let nbrs = [ (uncurry terrainAt q, q)
                                   | (dx, dy) ← [(1,0),(-1,0),(0,1),(0,-1)]
                                   , let q = (fst p + dx, snd p + dy)
                                   , not (HS.member q visited)
                                   , uncurry terrainAt q ≠ minBound
                                   , withinRim m q
                                   ]
                            visited' = foldr (HS.insert . snd) visited nbrs
                        in grow m targetLevel (max curLevel tz)
                                visited' (foldr insertQ rest nbrs)
                                (n + 1) (p : acc)

        popMin [] = Nothing
        popMin (x : xs) = Just (x, xs)

        insertQ x [] = [x]
        insertQ x@(k, _) ys@(y@(k', _) : rest)
            | k < k'    = x : ys
            | otherwise = y : insertQ x rest

        -- Inside the pool's outer rim: Euclidean distance from the
        -- landing point, with a per-tile hash jitter of ±'rimJitter'
        -- tiles so the radius-limited edge reads as a natural pool
        -- margin rather than geometry.
        withinRim (mx, my) (qx, qy) =
            let dx = qx - mx
                dy = qy - my
                h  = tileHash qx qy
                r  = poolRadius + (h `mod` (2 * rimJitter + 1))
                                   - rimJitter
            in dx * dx + dy * dy ≤ r * r

        tileHash qx qy =
            let z = fromIntegral qx * 0x9E3779B97F4A7C15
                    `xor` fromIntegral qy * 0xC2B2AE3D27D4EB4F
                    `xor` vcSeed ctx
                z' = (z `xor` (z `shiftR` 31)) * 0xFF51AFD7ED558CCD
            in fromIntegral ((z' `shiftR` 33) `mod` 0x7FFFFFFF) ∷ Int

    poolToLake (zFill, tiles) =
        let xs = map fst tiles
            ys = map snd tiles
        in Lake
            { lkSurface  = zFill
            , lkFloor    = minimum
                [ terrainAt gx gy | (gx, gy) ← tiles ]
            , lkArea     = length tiles
            , lkBBoxMinX = minimum xs
            , lkBBoxMinY = minimum ys
            , lkBBoxMaxX = maximum xs
            , lkBBoxMaxY = maximum ys
            }

    -- Per-chunk bitmask entries, mirroring the lake table's layout.
    buildChunkIndex pools = HM.fromListWith (V.++)
        [ (cc, V.singleton (LakeChunkEntry pid bm))
        | (pid, (_, tiles)) ← zip [0 ..] pools
        , (cc, bm) ← HM.toList (chunkMasks tiles)
        ]

    chunkMasks tiles = HM.fromListWith orMask
        [ (cc, VU.generate (chunkSize * chunkSize) (≡ i))
        | (gx, gy) ← tiles
        , let (cc, i) = chunkOf gx gy
        ]
      where
        orMask = VU.zipWith (∨)
