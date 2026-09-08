{-# LANGUAGE Strict #-}
-- | Deterministic generation of one map-pyramid page (issue #2298,
--   WML-5; design decisions D-11 and D-16).
--
--   A page's uploaded image is 'mapPageEdge' square: a
--   'mapPagePayload'-square payload plus a one-texel duplicated gutter
--   on every side. Both are produced by ONE rule — sample the level's
--   logical raster over @[origin - 1 .. origin + payload]@ — so the
--   gutter is not a second code path that could disagree with the
--   payload it borders. That is what makes two independently generated
--   adjacent pages agree texel-for-texel across their shared gutter,
--   across the parity seam, and at the longitude wrap.
--
--   == The source neighbourhood
--
--   Longitude is cylindrical: a sample outside the raster wraps by the
--   raster width, which is exactly the lattice's own @u + w@ alias.
--   Latitude is bounded: a sample above or below the raster is
--   TRANSPARENT, and premultiplied reduction keeps that from becoming a
--   black fringe. Those two rules also decide the unused texels of a
--   partial edge page, so a partial page is as deterministic as a full
--   one.
--
--   == Streaming, not materialising
--
--   A level-@l@ page covers @514 * 2^l@ finest texels on each axis, so
--   a coarse page for a large world reads far more finest data than it
--   could ever hold. Generation therefore STREAMS: finest rows are
--   pushed through a reduction tree that keeps at most one pending row
--   per level, which is the same adjacent-level 2x2 sequence
--   'reduceMapRaster' performs on a whole raster and must agree with it
--   texel-for-texel.
--
--   == What this does NOT do
--
--   It never consults a 'World.Chunk.Types.LoadedChunk', never enqueues
--   chunk demand, and never imports a renderer module. The only thing
--   it knows how to do is ask a 'MapCellSource' for finest cells.
module World.ZoomMap.Pyramid.Page
    ( -- * Cell sources
      MapCellSource(..)
    , mapCellTileBytes
    , transparentCellTile
      -- * Pages
    , mapPageImage
      -- * Whole levels (small worlds and tests)
    , mapLevelRaster
    , mapRasterWindow
    ) where

import UPrelude
import Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.IntMap.Strict as IM
import World.Map.ImagePlan (checkUploadPayload)
import World.ZoomMap.Pyramid.Address
import World.ZoomMap.Pyramid.Inventory
import World.ZoomMap.Pyramid.Reduce
import World.ZoomMap.Types (zoomTileSize)

-- * Cell sources

-- | Where a finest cell's 'zoomTileSize'-square RGBA8 tile comes from.
--
--   The interface is a BATCH so a producer can amortise whatever halo
--   its cells need — the world-generation source in
--   "World.ZoomMap.Pyramid.Cells" needs each chunk's four neighbours —
--   without the page generator knowing anything about it. The
--   contract is exact: one tile per requested cell, in the same order,
--   each 'mapCellTileBytes' long. A source that breaks it is refused
--   rather than read past.
newtype MapCellSource = MapCellSource
    { mcsCellTiles ∷ [MapCell] → Either Text [BS.ByteString] }

-- | Decoded bytes in one finest cell's tile.
mapCellTileBytes ∷ Int
mapCellTileBytes = zoomTileSize * zoomTileSize * 4

-- | The tile used where there is no world: beyond bounded latitude.
transparentCellTile ∷ BS.ByteString
transparentCellTile = BS.replicate mapCellTileBytes 0

-- * Pages

-- | Generate one page's uploaded 'mapPageEdge'-square RGBA8 image.
--
--   The returned length is checked against the level's own accepted
--   'World.Map.ImagePlan' plan, so a page that is not exactly
--   1,056,784 bytes is a refusal rather than a short upload.
mapPageImage ∷ MapPyramidInventory → MapCellSource → MapPageKey
             → Either MapPyramidRefusal BS.ByteString
mapPageImage inv src key = do
    mapFirst MapPyramidAddress (checkMapPageKey geom key)
    levelRow ← mapPyramidLevel inv (mpkLevel key)
    let l     = mpkLevel key
        scale = 2 ^ l ∷ Int
        (payloadX, payloadY) = mapPageTexelOrigin key
        x0    = (payloadX - mapPageGutter) * scale
        y0    = (payloadY - mapPageGutter) * scale
        side  = mapPageEdge * scale
    raster ← streamFinestWindow geom src l x0 y0 side side
    let bytes = mapRasterBytes raster
    mapFirst MapPyramidImage $
        checkUploadPayload (mplPagePlan levelRow) (BS.length bytes)
    pure bytes
  where
    geom = mpiGeometry inv

-- * Whole levels

-- | One whole level as a single payload-space raster, with no gutter.
--
--   This is the un-streamed statement of the same pyramid: the finest
--   level is materialised whole and then reduced whole by
--   'reduceMapRaster'. It exists so the streamed, pagewise result can
--   be checked against an independently executed one, and for worlds
--   small enough that a whole level is an ordinary allocation. Fresh
--   world generation streams instead (D-3); nothing in the engine calls
--   this.
mapLevelRaster ∷ MapPyramidInventory → MapCellSource → Int
               → Either MapPyramidRefusal MapRaster
mapLevelRaster inv src l
    | l < 0 ∨ l > mpiRootLevel inv =
        Left $ MapPyramidAddress $
            MapAddressLevelOutOfRange l (mpiRootLevel inv)
    | otherwise = do
        base ← streamFinestWindow geom src 0 0 0
                    (mapLevelWidth geom 0) (mapLevelHeight geom 0)
        reduceRepeatedly l base
  where
    geom = mpiGeometry inv

reduceRepeatedly ∷ Int → MapRaster → Either MapPyramidRefusal MapRaster
reduceRepeatedly 0 r = Right r
reduceRepeatedly n r = do
    r' ← mapFirst MapPyramidReduce (reduceMapRaster r)
    reduceRepeatedly (n - 1) r'

-- | Cut a sub-rectangle out of a raster. Samples outside it are
--   transparent, so a caller cannot accidentally read a neighbour's
--   texels through this.
mapRasterWindow ∷ MapRaster → Int → Int → Int → Int → MapRaster
mapRasterWindow r x0 y0 w h = MapRaster w h $
    VU.generate (max 0 (w * h * 4)) $ \i →
        let component = i `mod` 4
            pixel = i `div` 4
            (rr, gg, bb, aa) = mapRasterTexel r
                (x0 + pixel `mod` w) (y0 + pixel `div` w)
        in case component of
            0 → rr
            1 → gg
            2 → bb
            _ → aa

-- * Streaming generation

-- | Sample a finest-level window and reduce it @levels@ times, keeping
--   at most one pending row per level in flight.
--
--   The window's width and height must both be divisible by
--   @2 ^ levels@, which every caller here satisfies by construction:
--   a page window is @'mapPageEdge' * 2^l@ on each axis, and a whole
--   level's window is the finest raster, whose axes stay even through
--   the root for every supported world size.
streamFinestWindow ∷ MapGeometry → MapCellSource → Int
                   → Int → Int → Int → Int
                   → Either MapPyramidRefusal MapRaster
streamFinestWindow geom src levels x0 y0 w h
    -- Both guards report the WINDOW that could not survive the
    -- requested halvings, which is the same condition
    -- 'reduceMapRaster' refuses one level at a time. Neither is
    -- reachable from this module's own callers: a page window is
    -- @'mapPageEdge' * 2^l@ and a whole level is streamed with no
    -- reduction at all.
    | w < 1 ∨ h < 1 ∨ levels < 0 =
        Left $ MapPyramidReduce (MapReduceOddRaster w h)
    | w `mod` step ≢ 0 ∨ h `mod` step ≢ 0 =
        Left $ MapPyramidReduce (MapReduceOddRaster w h)
    | otherwise = do
        finished ← foldM band (emptyTree levels) bandRows
        let rows = reverse (rtOutput finished)
            outW = w `div` step
            outH = h `div` step
        pure $ MapRaster outW outH (VU.concat rows)
  where
    step = 2 ^ levels ∷ Int
    cellPx = zoomTileSize
    cellRow0 = y0 `div` cellPx
    cellRow1 = (y0 + h - 1) `div` cellPx
    cellCol0 = x0 `div` cellPx
    cellCol1 = (x0 + w - 1) `div` cellPx
    columns = cellCol1 - cellCol0 + 1
    bandRows = [cellRow0 .. cellRow1]

    -- One band is the texel rows of a single finest cell row. Its
    -- tiles are fetched once and reused for every texel row in it,
    -- which is what keeps the source's per-cell work bounded.
    band tree cv = do
        tiles ← bandTiles cv
        let yLo = max y0 (cv * cellPx)
            yHi = min (y0 + h - 1) (cv * cellPx + cellPx - 1)
        Right (foldl' (\t y → pushRow t (texelRow tiles y)) tree [yLo .. yHi])

    bandTiles cv
        | cv < 0 ∨ cv ≥ mgCellsV geom =
            Right (V.replicate columns transparentCellTile)
        | otherwise = do
            let cells = [ MapCell ((cellCol0 + i) `mod` mgCellsU geom) cv
                        | i ← [0 .. columns - 1] ]
            supplied ← mapFirst MapPyramidCellSource (mcsCellTiles src cells)
            when (length supplied ≢ columns) $
                Left $ MapPyramidCellSource $
                    "answered " <> tshow (length supplied) <> " tile(s) for "
                    <> tshow columns <> " requested cell(s)"
            forM_ supplied $ \tile →
                when (BS.length tile ≢ mapCellTileBytes) $
                    Left $ MapPyramidCellSource $
                        "answered a " <> tshow (BS.length tile)
                        <> "-byte tile where " <> tshow mapCellTileBytes
                        <> " decoded bytes were required"
            Right (V.fromList supplied)

    texelRow tiles y = VU.generate (w * 4) $ \i →
        let component = i `mod` 4
            lx = i `div` 4
            x  = x0 + lx
            col = (x `div` cellPx) - cellCol0
            px = x `mod` cellPx
            py = y `mod` cellPx
        in BS.index (tiles V.! col) (((py * cellPx + px) * 4) + component)

-- | The streaming reduction tree: one optional pending row per level,
--   and the finished rows of the target level in reverse order.
--
--   A level holding a pending row is a level whose next row completes a
--   2x2 neighbourhood; completing it reduces the pair and pushes the
--   result one level up. Rows therefore pair in exactly the
--   adjacent-level order 'reduceMapRaster' uses on a whole raster, and
--   at most one row per level is ever resident.
data ReduceTree = ReduceTree
    { rtTarget  ∷ !Int
    , rtPending ∷ !(IM.IntMap (VU.Vector Word8))
    , rtOutput  ∷ ![VU.Vector Word8]
    }

emptyTree ∷ Int → ReduceTree
emptyTree target = ReduceTree target IM.empty []

-- | Push one finest-level row into the tree.
pushRow ∷ ReduceTree → VU.Vector Word8 → ReduceTree
pushRow tree = go tree 0
  where
    go t level row
      | level ≡ rtTarget t = t { rtOutput = row : rtOutput t }
      | otherwise = case IM.lookup level (rtPending t) of
          Nothing → t { rtPending = IM.insert level row (rtPending t) }
          Just upper →
              go t { rtPending = IM.delete level (rtPending t) }
                 (level + 1) (reduceRowPair upper row)

-- | Pair two rows of the same width into one row of half that width.
reduceRowPair ∷ VU.Vector Word8 → VU.Vector Word8 → VU.Vector Word8
reduceRowPair upper lower = VU.generate (VU.length upper `div` 2) $ \i →
    let component = i `mod` 4
        x = i `div` 4
        at v k = ( v VU.! (k * 4), v VU.! (k * 4 + 1)
                 , v VU.! (k * 4 + 2), v VU.! (k * 4 + 3) )
        (rr, gg, bb, aa) = reduceQuad
            (at upper (2 * x)) (at upper (2 * x + 1))
            (at lower (2 * x)) (at lower (2 * x + 1))
    in case component of
        0 → rr
        1 → gg
        2 → bb
        _ → aa

mapFirst ∷ (α → β) → Either α γ → Either β γ
mapFirst f (Left a)  = Left (f a)
mapFirst _ (Right c) = Right c
