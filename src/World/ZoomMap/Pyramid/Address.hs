{-# LANGUAGE Strict #-}
-- | The canonical spatial addressing of the world map's page pyramid
--   (issue #2298, WML-5).
--
--   Everything here is __pure__ and __total__ over a normalized world
--   size: it either answers, or returns a typed 'MapAddressRefusal'
--   naming exactly what it refused. Nothing in this module allocates a
--   pixel, reads a chunk, or touches the renderer.
--
--   == The lattice this compresses
--
--   The map lives in the cylindrical @(u,v)@ space the zoom builder
--   already enumerates ("World.ZoomMap.Cache.BuildPixels"): a physical
--   chunk @(ccx, ccy)@ sits at
--
--   > u = ccx - ccy      v = ccx + ccy
--
--   @u@ is longitude and wraps with period @w@ (the lattice span, see
--   'mgLatticeSpan'); @v@ is latitude and is bounded, never wrapped.
--   Both run over @[-w\/2 .. w\/2 - 1]@, and a chunk exists at exactly
--   the coordinates where @u + v@ is EVEN — the other half of the
--   lattice is unreachable parity, which is why a raster indexed by
--   raw @(u,v)@ would spend half its cells on coordinates that can
--   never hold a chunk.
--
--   == The compression (D-1, requirement 1)
--
--   Shift to a non-negative raster frame @ru = u + h@, @rv = v + h@
--   (@h = w \/ 2@), which preserves parity because @2h@ is even. On row
--   @rv@ the valid @ru@ are exactly those with @ru ≡ rv (mod 2)@, so
--   halving them is lossless:
--
--   > cellU = (ru - (rv `mod` 2)) `div` 2        cellV = rv
--
--   That is a bijection between the @w²\/2@ physical chunks and the
--   dense @(w\/2) × w@ cell rectangle: rows alternate their starting
--   parity, giving the isometric brick stagger, and no cell is spent on
--   an impossible coordinate. Longitude aliasing survives it exactly —
--   @u + w@ maps to @cellU + w\/2@ — so the wrap stays a clean modular
--   shift on the compressed axis ('normalizeMapCell'). Latitude has no
--   alias: a @v@ outside the bound is REFUSED, never wrapped.
--
--   == Levels and pages (D-3, D-7, D-11)
--
--   Level 0 is the finest: one 'zoomTileSize'-square cell per physical
--   chunk, so its raster is @cellsU * 32@ by @cellsV * 32@ texels. Each
--   next level is the adjacent 2×2 reduction of the previous one
--   ("World.ZoomMap.Pyramid.Reduce"), so level @l@ halves each axis
--   @l@ times. A page is a 'mapPagePayload'-square payload of that
--   level's raster plus a one-texel gutter
--   ('mapPageGutter'), and page keys are the spatial address
--   @(level, page-u, page-v)@ — never an index into a whole-world
--   per-chunk vector.
--
--   == What this raster is and is not
--
--   It is a dense SPATIAL raster in compressed @(u,v)@ map space: cells
--   adjacent in it are adjacent in the world, and the wrap and the
--   latitude bound are the world's own. It is not isometric screen
--   space. One cell COLUMN spans two lattice units of @u@ and one cell
--   ROW spans one of @v@, so the raster is 2:1 anisotropic against the
--   lattice, and consecutive rows stagger by half a column. That is
--   inherent to storing one square 'zoomTileSize' tile per physical
--   chunk, which requirement 2 of #2298 fixes; the draw mapping that
--   turns it back into interlocking diamonds under each camera facing
--   belongs to the renderer slice (WML-12), not here.
--
--   The ROOT is the smallest level whose longest logical raster axis is
--   at most 'mapRootAxisCap' (D-7's 2048), and the mandatory-coarse
--   CUTOFF is the smallest level whose longest axis is at most
--   'mapCoarseAxisCap' (the ~4096-pixel-equivalent level). Both are
--   derived from the world size, never tabulated per size, and if level
--   0 already satisfies a cap then level 0 is what that cap selects.
module World.ZoomMap.Pyramid.Address
    ( -- * Fixed page geometry
      mapPagePayload
    , mapPageGutter
    , mapPageEdge
    , mapRootAxisCap
    , mapCoarseAxisCap
    , mapDesignedMaxWorldSize
      -- * World geometry
    , MapGeometry(..)
    , mapGeometry
      -- * Cells
    , MapCell(..)
    , normalizeMapCell
    , finestCellOfUV
    , uvOfFinestCell
    , finestCellOfChunk
    , chunkOfFinestCell
      -- * Levels
    , mapLevelWidth
    , mapLevelHeight
    , mapLevelPagesU
    , mapLevelPagesV
    , mapRootLevel
    , mapCoarseCutoffLevel
      -- * Pages
    , MapPageKey(..)
    , mapPageOfFinestCell
    , mapPageFinestCells
    , mapPageTexelOrigin
    , mapPageParent
    , mapPageChildren
    , checkMapPageKey
      -- * Refusals
    , MapAddressRefusal(..)
    , mapAddressRefusalText
    ) where

import UPrelude
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Config.Normalize (minimumWorldSize)
import World.ZoomMap.Types (zoomTileSize)

-- * Fixed page geometry

-- | D-11's page payload edge, in texels. At the finest level this is
--   exactly 16 of the current 32-pixel physical-chunk cells.
mapPagePayload ∷ Int
mapPagePayload = 512

-- | The duplicated outer gutter on every side where linear sampling can
--   cross a page boundary (D-11).
mapPageGutter ∷ Int
mapPageGutter = 1

-- | The uploaded page edge: payload plus a gutter on both sides. 514,
--   which is 1,056,784 decoded RGBA8 bytes — but this module states the
--   DIMENSION only. The byte count belongs to "World.Map.ImagePlan"
--   and is planned in "World.ZoomMap.Pyramid.Inventory".
mapPageEdge ∷ Int
mapPageEdge = mapPagePayload + 2 * mapPageGutter

-- | D-7's resident-root bound: the root level's longest logical raster
--   axis may not exceed this.
mapRootAxisCap ∷ Int
mapRootAxisCap = 2048

-- | D-7's mandatory-coarse bound: the approximately 4096-pixel-
--   equivalent level. Every level from the cutoff through the root is
--   mandatory generated coverage.
mapCoarseAxisCap ∷ Int
mapCoarseAxisCap = 4096

-- | The world size this addressing was DESIGNED and tested through
--   (D-4). It is not a refusal bound: a larger normalized size is
--   planned by the same checked arithmetic and refused only if that
--   arithmetic actually fails. Nothing here claims the rest of the
--   engine can generate or render such a world.
mapDesignedMaxWorldSize ∷ Int
mapDesignedMaxWorldSize = 8192

-- * World geometry

-- | The derived shape of one world's map. Constructed only by
--   'mapGeometry', so holding one is evidence that the world size was
--   accepted.
data MapGeometry = MapGeometry
    { mgWorldSize   ∷ !Int
      -- ^ The normalized world size in chunks, as supplied.
    , mgLatticeSpan ∷ !Int
      -- ^ @w@: the span of the raw @(u,v)@ lattice on both axes.
    , mgCellsU      ∷ !Int
      -- ^ Compressed longitude cells, @w \/ 2@. Wraps.
    , mgCellsV      ∷ !Int
      -- ^ Latitude cells, @w@. Bounded; never wraps.
    , mgFinestCells ∷ !Int
      -- ^ @mgCellsU * mgCellsV@ — one per physical chunk.
    , mgFinestWidth ∷ !Int
      -- ^ Level 0's raster width in texels, @mgCellsU * zoomTileSize@.
    , mgFinestHeight ∷ !Int
      -- ^ Level 0's raster height in texels.
    } deriving (Eq, Show)

-- | Derive a world's map geometry, refusing a world size the map
--   cannot address.
--
--   The accepted domain is exactly
--   "World.Generate.Config.Normalize"'s: a positive multiple of
--   'minimumWorldSize'. That is also the domain
--   "World.Map.ImagePlan" enforces for the existing atlas, so a size
--   this accepts is a size the planner will price.
mapGeometry ∷ Int → Either MapAddressRefusal MapGeometry
mapGeometry worldSize
    | worldSize < minimumWorldSize ∨ worldSize `mod` minimumWorldSize ≢ 0 =
        Left $ MapAddressUnsupportedWorldSize worldSize $
            "world size must be a positive multiple of "
            <> tshow minimumWorldSize
            <> " (World.Generate.Config.Normalize.normalizeWorldSize)"
    | otherwise = do
        -- EXACTLY the zoom builder's own lattice span: it walks u,v
        -- over @[-halfSize .. halfSize - 1]@ on a @w = halfSize * 2@
        -- grid. Restating it as @worldSize@ would silently disagree for
        -- any size the guard above did not already reject.
        let w = (worldSize `div` 2) * 2
            cu = w `div` 2
        -- Every world-scale product is formed in 'Integer' and narrowed
        -- under a bound check. Level dimensions below are pure halvings
        -- of the two narrowed extents, and page counts are checked
        -- again where they are multiplied, so nothing downstream
        -- re-derives a product from the world size (requirement 8).
        cells ← narrowGeometry worldSize "finest cell count"
                    (toInteger cu * toInteger w)
        width ← narrowGeometry worldSize "finest raster width"
                    (toInteger cu * toInteger zoomTileSize)
        height ← narrowGeometry worldSize "finest raster height"
                    (toInteger w * toInteger zoomTileSize)
        pure MapGeometry
            { mgWorldSize    = worldSize
            , mgLatticeSpan  = w
            , mgCellsU       = cu
            , mgCellsV       = w
            , mgFinestCells  = cells
            , mgFinestWidth  = width
            , mgFinestHeight = height
            }

-- | Narrow a world-scale intermediate to an 'Int', refusing with the
--   exact value and the bound it overran.
narrowGeometry ∷ Int → Text → Integer → Either MapAddressRefusal Int
narrowGeometry worldSize quantity value
    | value > hostBound =
        Left $ MapAddressNotRepresentable worldSize quantity value hostBound
    | otherwise = Right (fromInteger value)
  where hostBound = toInteger (maxBound ∷ Int)

-- * Cells

-- | One finest-level cell: a compressed, parity-free spatial address
--   that holds exactly one physical chunk.
data MapCell = MapCell
    { mcCellU ∷ !Int   -- ^ Compressed longitude, @[0 .. mgCellsU - 1]@
    , mcCellV ∷ !Int   -- ^ Latitude, @[0 .. mgCellsV - 1]@
    } deriving (Eq, Ord, Show)

-- | Resolve a cell's longitude alias and check its latitude.
--
--   Longitude is cylindrical, so @cellU@ outside the canonical range is
--   an ALIAS and is wrapped. Latitude is bounded, so @cellV@ outside
--   its range is REFUSED — there is no world there to alias to.
normalizeMapCell ∷ MapGeometry → MapCell → Either MapAddressRefusal MapCell
normalizeMapCell geom (MapCell cu cv)
    | cv < 0 ∨ cv ≥ mgCellsV geom =
        Left $ MapAddressLatitudeOutOfBounds cv 0 (mgCellsV geom - 1)
    | otherwise = Right (MapCell (cu `mod` mgCellsU geom) cv)

-- | The finest cell holding the parity-valid lattice coordinate
--   @(u, v)@. Odd parity is REFUSED rather than silently addressed, and
--   an out-of-range @u@ is wrapped to its canonical longitude alias
--   exactly the way "World.Chunk.Types".@wrapChunkCoordU@ wraps it.
finestCellOfUV ∷ MapGeometry → Int → Int → Either MapAddressRefusal MapCell
finestCellOfUV geom u v
    | odd (u + v) = Left $ MapAddressOddParity u v
    | v < (-h) ∨ v ≥ h = Left $ MapAddressLatitudeOutOfBounds v (-h) (h - 1)
    | otherwise =
        let wrappedU = ((u + h) `mod` w + w) `mod` w - h
            ru = wrappedU + h
            rv = v + h
        in Right $ MapCell ((ru - (rv `mod` 2)) `div` 2) rv
  where
    w = mgLatticeSpan geom
    h = w `div` 2

-- | The canonical lattice coordinate a finest cell occupies. The exact
--   inverse of 'finestCellOfUV' on canonical input.
uvOfFinestCell ∷ MapGeometry → MapCell → Either MapAddressRefusal (Int, Int)
uvOfFinestCell geom cell = do
    MapCell cu cv ← normalizeMapCell geom cell
    let h  = mgLatticeSpan geom `div` 2
        ru = 2 * cu + (cv `mod` 2)
    pure (ru - h, cv - h)

-- | The finest cell holding a physical chunk.
finestCellOfChunk ∷ MapGeometry → ChunkCoord
                  → Either MapAddressRefusal MapCell
finestCellOfChunk geom (ChunkCoord ccx ccy) =
    finestCellOfUV geom (ccx - ccy) (ccx + ccy)

-- | The physical chunk a finest cell holds.
chunkOfFinestCell ∷ MapGeometry → MapCell
                  → Either MapAddressRefusal ChunkCoord
chunkOfFinestCell geom cell = do
    (u, v) ← uvOfFinestCell geom cell
    pure $ ChunkCoord ((u + v) `div` 2) ((v - u) `div` 2)

-- * Levels

-- | Halve a raster axis @l@ times, never below one texel.
shrinkAxis ∷ Int → Int → Int
shrinkAxis d 0 = d
shrinkAxis d l
    | l < 0     = d
    | otherwise = shrinkAxis (max 1 ((d + 1) `div` 2)) (l - 1)

-- | Level @l@'s logical raster width in texels.
mapLevelWidth ∷ MapGeometry → Int → Int
mapLevelWidth = shrinkAxis ∘ mgFinestWidth

-- | Level @l@'s logical raster height in texels.
mapLevelHeight ∷ MapGeometry → Int → Int
mapLevelHeight = shrinkAxis ∘ mgFinestHeight

-- | Pages across level @l@'s longitude axis.
mapLevelPagesU ∷ MapGeometry → Int → Int
mapLevelPagesU geom l = ceilDivPages (mapLevelWidth geom l)

-- | Pages down level @l@'s latitude axis.
mapLevelPagesV ∷ MapGeometry → Int → Int
mapLevelPagesV geom l = ceilDivPages (mapLevelHeight geom l)

ceilDivPages ∷ Int → Int
ceilDivPages d = (d + mapPagePayload - 1) `div` mapPagePayload

-- | The smallest level whose longest logical raster axis is at most
--   @cap@. Terminates because both axes shrink to one texel.
smallestLevelUnder ∷ MapGeometry → Int → Int
smallestLevelUnder geom cap = go 0
  where
    go l
      | max (mapLevelWidth geom l) (mapLevelHeight geom l) ≤ cap = l
      | otherwise = go (l + 1)

-- | The root level (D-7): the smallest level whose longest logical
--   raster axis is at most 'mapRootAxisCap'.
mapRootLevel ∷ MapGeometry → Int
mapRootLevel geom = smallestLevelUnder geom mapRootAxisCap

-- | The mandatory-coarse cutoff (D-7): the smallest level whose longest
--   logical raster axis is at most 'mapCoarseAxisCap'. Always at or
--   below 'mapRootLevel', because its cap is the looser one.
mapCoarseCutoffLevel ∷ MapGeometry → Int
mapCoarseCutoffLevel geom = smallestLevelUnder geom mapCoarseAxisCap

-- * Pages

-- | A page's spatial address. NOT an index into any vector.
data MapPageKey = MapPageKey
    { mpkLevel ∷ !Int   -- ^ 0 is finest; 'mapRootLevel' is coarsest
    , mpkPageU ∷ !Int
    , mpkPageV ∷ !Int
    } deriving (Eq, Ord, Show)

-- | Refuse a page key that names a level or a page outside this
--   world's pyramid. Page indices are CANONICAL: unlike a cell's
--   longitude, a page index is never aliased, because the texel wrap
--   period (@mgCellsU * zoomTileSize@) is page-aligned only when the
--   world size is a multiple of 32. Longitude aliasing therefore lives
--   in 'normalizeMapCell', where the wrap actually is.
checkMapPageKey ∷ MapGeometry → MapPageKey → Either MapAddressRefusal ()
checkMapPageKey geom key@(MapPageKey l pu pv)
    | l < 0 ∨ l > mapRootLevel geom =
        Left $ MapAddressLevelOutOfRange l (mapRootLevel geom)
    | pu < 0 ∨ pu ≥ pagesU ∨ pv < 0 ∨ pv ≥ pagesV =
        Left $ MapAddressPageOutOfRange key pagesU pagesV
    | otherwise = Right ()
  where
    pagesU = mapLevelPagesU geom l
    pagesV = mapLevelPagesV geom l

-- | The finest-level page covering a cell.
mapPageOfFinestCell ∷ MapGeometry → MapCell
                    → Either MapAddressRefusal MapPageKey
mapPageOfFinestCell geom cell = do
    MapCell cu cv ← normalizeMapCell geom cell
    pure $ MapPageKey 0 ((cu * zoomTileSize) `div` mapPagePayload)
                        ((cv * zoomTileSize) `div` mapPagePayload)

-- | The finest cells a level-0 page's PAYLOAD covers, paired with the
--   @(column, row)@ cell position they occupy inside it, row-major.
--
--   Rows beyond bounded latitude are OMITTED, because there is no world
--   there to cover. Columns are never omitted: longitude wraps, so a
--   payload column past the last one shows part of the world again, and
--   the cell it names is the normalized one it wrapped onto.
mapPageFinestCells ∷ MapGeometry → MapPageKey
                   → Either MapAddressRefusal [(Int, Int, MapCell)]
mapPageFinestCells geom key
    | mpkLevel key ≢ 0 = Left $ MapAddressNotFinestLevel (mpkLevel key)
    | otherwise = do
        checkMapPageKey geom key
        pure [ (col, row, MapCell ((cu0 + col) `mod` mgCellsU geom) (cv0 + row))
             | row ← [0 .. cellsPerPageEdge - 1]
             , cv0 + row ≥ 0, cv0 + row < mgCellsV geom
             , col ← [0 .. cellsPerPageEdge - 1] ]
  where
    cellsPerPageEdge = mapPagePayload `div` zoomTileSize
    cu0 = mpkPageU key * cellsPerPageEdge
    cv0 = mpkPageV key * cellsPerPageEdge

-- | The level-local texel coordinate of a page's payload origin. The
--   uploaded image starts one gutter texel before it on each axis.
mapPageTexelOrigin ∷ MapPageKey → (Int, Int)
mapPageTexelOrigin (MapPageKey _ pu pv) =
    (pu * mapPagePayload, pv * mapPagePayload)

-- | The coarser page covering this one, or 'Nothing' at the root.
--   Well-defined because 'mapPagePayload' is even, so a page's payload
--   halves onto exactly one parent page's half.
mapPageParent ∷ MapGeometry → MapPageKey → Maybe MapPageKey
mapPageParent geom (MapPageKey l pu pv)
    | l ≥ mapRootLevel geom = Nothing
    | otherwise = Just $ MapPageKey (l + 1) (pu `div` 2) (pv `div` 2)

-- | The finer pages this one covers, in canonical order, with
--   out-of-range children dropped (a partial edge page has fewer).
mapPageChildren ∷ MapGeometry → MapPageKey → [MapPageKey]
mapPageChildren geom (MapPageKey l pu pv)
    | l ≤ 0 = []
    | otherwise =
        [ key
        | dv ← [0, 1], du ← [0, 1]
        , let key = MapPageKey (l - 1) (2 * pu + du) (2 * pv + dv)
        , checkMapPageKey geom key ≡ Right () ]

-- * Refusals

-- | Why an address was refused. Each constructor carries exactly the
--   facts its own message needs.
data MapAddressRefusal
    = MapAddressUnsupportedWorldSize !Int !Text
      -- ^ The world size, and the constraint it violates.
    | MapAddressNotRepresentable !Int !Text !Integer !Integer
      -- ^ The world size, the quantity's name, its exact wide value,
      --   and the host 'Int' bound it overran.
    | MapAddressOddParity !Int !Int
      -- ^ A lattice @(u, v)@ whose sum is odd: no chunk can be there.
    | MapAddressLatitudeOutOfBounds !Int !Int !Int
      -- ^ The latitude, and the inclusive bound it left.
    | MapAddressLevelOutOfRange !Int !Int
      -- ^ The level, and this world's root level.
    | MapAddressNotFinestLevel !Int
      -- ^ A level-0-only query asked about a coarser level. Coarser
      --   levels are texel rasters; only the finest has cells.
    | MapAddressPageOutOfRange !MapPageKey !Int !Int
      -- ^ The key, and its level's page counts.
    deriving (Eq, Show)

-- | The log-facing text for an address refusal.
mapAddressRefusalText ∷ MapAddressRefusal → Text
mapAddressRefusalText (MapAddressUnsupportedWorldSize n constraint) =
    "Refusing map addressing for worldSize " <> tshow n <> ": "
    <> constraint <> "."
mapAddressRefusalText (MapAddressNotRepresentable n quantity value bound) =
    "Refusing map addressing for worldSize " <> tshow n <> ": "
    <> quantity <> " " <> tshow value
    <> " does not fit a host Int (bound " <> tshow bound <> ")."
mapAddressRefusalText (MapAddressOddParity u v) =
    "Refusing map lattice coordinate (" <> tshow u <> ", " <> tshow v
    <> "): u + v is odd, so no physical chunk can occupy it."
mapAddressRefusalText (MapAddressLatitudeOutOfBounds v lo hi) =
    "Refusing map latitude " <> tshow v <> ": outside the bounded range "
    <> tshow lo <> " … " <> tshow hi
    <> ". Latitude does not wrap; only longitude does."
mapAddressRefusalText (MapAddressLevelOutOfRange l root) =
    "Refusing map level " <> tshow l <> ": this world's pyramid runs 0 … "
    <> tshow root <> "."
mapAddressRefusalText (MapAddressNotFinestLevel l) =
    "Refusing a finest-cell query at map level " <> tshow l
    <> ": only level 0 has cells; coarser levels are texel rasters."
mapAddressRefusalText (MapAddressPageOutOfRange key pagesU pagesV) =
    "Refusing map page " <> tshow key <> ": level "
    <> tshow (mpkLevel key) <> " has " <> tshow pagesU <> "×"
    <> tshow pagesV <> " pages."
