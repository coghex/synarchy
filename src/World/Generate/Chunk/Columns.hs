{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-column tile-data construction: fuses stratigraphy (via
--   'World.Generate.Strata') with basalt-cap/lava-shell material
--   overrides into the final 'ColumnTiles' for every tile in a chunk.
--   Split out of 'World.Generate.Chunk' (#549) — a pure move (wrapped
--   as an explicit-argument top-level function in place of a chunk of
--   'generateChunk's local @let@), no behavior change.
module World.Generate.Chunk.Columns
    ( buildChunkColumns
    , generateExposedColumn
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import World.Types
import World.Material (MaterialId(..), matAir, matBasalt, MaterialRegistry)
import World.Scale (WorldScale)
import World.Magma.Types (MagmaOverlay(..))
import World.Generate.Strata (buildStrataCache, buildColumnStrata)

-- | Build per-column tile data directly, fusing stratigraphy
--   computation with 'ColumnTiles' construction to avoid an
--   intermediate @V.Vector ColumnStrata@ allocation.
--
--   Takes every value 'generateChunk's pipeline has already computed
--   by this point as an explicit argument (bordered lookups as
--   functions, per-tile maps as vectors) rather than closing over
--   them, since this used to be a @let@ binding inside that function.
buildChunkColumns
    ∷ GeoTimeline → Int → WorldScale → MaterialRegistry
    → VU.Vector Bool               -- ^ coordBeyond
    → VU.Vector Int                -- ^ coordGX (wrapped global x per tile)
    → VU.Vector Int                -- ^ coordGY (wrapped global y per tile)
    → (Int → Int → (Int, MaterialId)) -- ^ lookupFinal (post-carve elev/mat)
    → (Int → Int → (Int, MaterialId)) -- ^ lookupBase (pre-timeline elev/mat)
    → Maybe MagmaOverlay
    → VU.Vector Bool               -- ^ lavaShell
    → VU.Vector Int                -- ^ terrainSurfaceMap (post-smoothing)
    → (Int → Int → Int → Int)      -- ^ lookupElevOr (bordered elev, fallback)
    → V.Vector (Maybe FluidCell)   -- ^ rawFluidMap
    → Int                          -- ^ chunkArea
    → V.Vector ColumnTiles
buildChunkColumns timeline worldSize wsc registry coordBeyond coordGX coordGY
                  lookupFinal lookupBase magmaOverlay lavaShell
                  terrainSurfaceMap lookupElevOr rawFluidMap chunkArea =
    V.generate chunkArea $ \idx →
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
                --
                -- A beyond-glacier neighbour reads back as the
                -- 'minBound' sentinel (its column is empty / unrendered);
                -- it must NOT pull 'exposeFrom' down toward minBound, or
                -- 'buildColumnStrata' below would size a ~2^63-tall column
                -- (depth = rawSurfZ - exposeFrom) and overflow the heap.
                -- Treat it like an absent neighbour (fall back to this
                -- column's own surface — no downward exposure), the same
                -- way the slope / edge-strata passes special-case the
                -- sentinel. This is the world-edge case behind the
                -- glacier-rim chunk-load crash (#298): a chunk straddling
                -- the glacier diagonal has real columns bordering empty
                -- ones in-chunk.
                finalSelf0 = terrainSurfaceMap VU.! idx
                finalSelf = if finalSelf0 ≡ minBound then rawSurfZ
                            else finalSelf0
                visibleTerrainOr olx oly fallback =
                    if olx ≥ 0 ∧ olx < chunkSize
                       ∧ oly ≥ 0 ∧ oly < chunkSize
                    then let z = terrainSurfaceMap VU.! columnIndex olx oly
                         in if z ≡ minBound then fallback else z
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
