{-# LANGUAGE Strict #-}
-- | The map pyramid's coarse-level reduction (issue #2298, WML-5;
--   design decision D-16).
--
--   == The rule, stated once
--
--   A coarser level is the repeated ADJACENT-level 2×2 box reduction of
--   the next finer raster, in the current UNORM colour space:
--
--     * RGB is accumulated PREMULTIPLIED by alpha —
--       @Σ (channel_i * alpha_i)@ — and divided by @Σ alpha_i@;
--     * alpha is the plain average @Σ alpha_i / 4@;
--     * a neighbourhood whose alphas all vanish produces a fully
--       transparent texel and no colour is invented for it;
--     * every division uses ONE rounding rule, 'divRoundHalfUp': exact
--       integer half-up, spelled @(2n + d) \`div\` (2d)@ so no
--       intermediate leaves 'Int' and no 'Double' is ever consulted.
--
--   Every arithmetic step is integer, so the result is fixed rather
--   than platform- or evaluation-order-dependent, and reducing the same
--   neighbourhood twice is the same answer on every machine.
--
--   Premultiplying is what keeps a page or world edge from gaining a
--   black fringe: a transparent neighbour contributes NOTHING to the
--   colour sum instead of dragging it toward zero, so an opaque texel
--   beside three transparent ones keeps its own colour and only loses
--   alpha.
--
--   == Repeated, not collapsed
--
--   D-16 specifies REPEATED ADJACENT reduction. Rounding at each
--   intermediate level is not associative, so a single wide box filter
--   over @2^l@ texels is a DIFFERENT function and is deliberately not
--   offered here. What must agree is every way of executing the same
--   adjacent-level sequence: whole-level, page-by-page, streamed, in any
--   evaluation order.
module World.ZoomMap.Pyramid.Reduce
    ( -- * The rule
      mapReductionRuleName
    , divRoundHalfUp
    , reduceQuad
      -- * Rasters
    , MapRaster(..)
    , mapRasterTexels
    , mapRasterBytes
    , mapRasterFromBytes
    , mapRasterTexel
      -- * Reduction
    , reduceMapRaster
      -- * Refusals
    , MapReduceRefusal(..)
    , mapReduceRefusalText
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as VU

-- * The rule

-- | The single name the code and its tests both use for the rounding
--   and accumulation rule above.
mapReductionRuleName ∷ Text
mapReductionRuleName =
    "premultiplied 2x2 box reduction with half-up integer rounding"

-- | Exact integer half-up division for non-negative @n@ and positive
--   @d@: @round (n / d)@ with ties going up, computed as
--   @(2n + d) \`div\` (2d)@ so it never leaves the integers.
--
--   Total: a non-positive @d@ answers @0@ rather than dividing by zero.
--   The only caller that can reach that is a fully transparent
--   neighbourhood, which 'reduceQuad' short-circuits first.
divRoundHalfUp ∷ Int → Int → Int
divRoundHalfUp n d
    | d ≤ 0     = 0
    | otherwise = (2 * n + d) `div` (2 * d)

-- | Reduce one 2×2 neighbourhood of straight RGBA8 texels to one.
reduceQuad ∷ (Word8, Word8, Word8, Word8) → (Word8, Word8, Word8, Word8)
           → (Word8, Word8, Word8, Word8) → (Word8, Word8, Word8, Word8)
           → (Word8, Word8, Word8, Word8)
reduceQuad q0 q1 q2 q3
    | alphaSum ≡ 0 = (0, 0, 0, 0)
    | otherwise =
        ( narrow (divRoundHalfUp (premul red)   alphaSum)
        , narrow (divRoundHalfUp (premul green) alphaSum)
        , narrow (divRoundHalfUp (premul blue)  alphaSum)
        , narrow (divRoundHalfUp alphaSum 4) )
  where
    quads = [q0, q1, q2, q3]
    alphaOf (_, _, _, a) = fromIntegral a ∷ Int
    red   (r, _, _, _) = fromIntegral r ∷ Int
    green (_, g, _, _) = fromIntegral g ∷ Int
    blue  (_, _, b, _) = fromIntegral b ∷ Int
    alphaSum = sum (map alphaOf quads)
    premul channel = sum [ channel q * alphaOf q | q ← quads ]
    -- Both quotients are bounded by 255 on their own arithmetic
    -- (a premultiplied sum is at most 255 * alphaSum, and alphaSum is
    -- at most 1020), so this clamp is a total narrowing rather than a
    -- correction.
    narrow v = fromIntegral (max 0 (min 255 v)) ∷ Word8

-- * Rasters

-- | A rectangle of straight RGBA8 texels in one level's raster space.
--
--   'Show' deliberately prints the SHAPE only: a page is a million
--   bytes, and a failing @shouldBe@ that dumps two of them is not a
--   diagnostic.
data MapRaster = MapRaster
    { mrWidth  ∷ !Int
    , mrHeight ∷ !Int
    , mrTexels ∷ !(VU.Vector Word8)  -- ^ Row-major RGBA8, @w * h * 4@
    } deriving (Eq)

instance Show MapRaster where
    show r = "MapRaster " ⧺ show (mrWidth r) ⧺ "x" ⧺ show (mrHeight r)
           ⧺ " (" ⧺ show (VU.length (mrTexels r)) ⧺ " bytes)"

-- | Texel count implied by a raster's dimensions.
mapRasterTexels ∷ MapRaster → Int
mapRasterTexels r = mrWidth r * mrHeight r

-- | The raster's decoded RGBA8 bytes.
mapRasterBytes ∷ MapRaster → BS.ByteString
mapRasterBytes r = fst $ BS.unfoldrN (VU.length texels) step 0
  where
    texels = mrTexels r
    step i = Just (texels VU.! i, i + 1)

-- | Build a raster from decoded RGBA8 bytes, refusing a length that
--   does not match the stated dimensions.
mapRasterFromBytes ∷ Int → Int → BS.ByteString
                   → Either MapReduceRefusal MapRaster
mapRasterFromBytes w h bytes
    | w < 1 ∨ h < 1 = Left $ MapReduceMalformedRaster w h (BS.length bytes)
    | BS.length bytes ≢ w * h * 4 =
        Left $ MapReduceMalformedRaster w h (BS.length bytes)
    | otherwise =
        Right $ MapRaster w h (VU.generate (BS.length bytes) (BS.index bytes))

-- | Read one texel. Out-of-range coordinates answer transparent, which
--   is what keeps this total; every caller here stays in range.
mapRasterTexel ∷ MapRaster → Int → Int → (Word8, Word8, Word8, Word8)
mapRasterTexel r x y
    | x < 0 ∨ x ≥ mrWidth r ∨ y < 0 ∨ y ≥ mrHeight r = (0, 0, 0, 0)
    | otherwise =
        let o = (y * mrWidth r + x) * 4
            at i = mrTexels r VU.! (o + i)
        in (at 0, at 1, at 2, at 3)

-- * Reduction

-- | One adjacent-level 2×2 reduction of a raster.
--
--   Both dimensions must be EVEN. That is not a convenience: an odd
--   axis has no fourth sample for its last neighbourhood, and inventing
--   one — by clamping, by wrapping to the antipode, or by treating it
--   as transparent — would each give a different, silently wrong
--   coarse texel. Refusing says so.
--
--   Every level of every supported world size has even dimensions
--   through its root (see the @map pyramid@ spec, which asserts it over
--   the whole normalized size domain), so this refusal is unreachable
--   from the inventory's own level range.
reduceMapRaster ∷ MapRaster → Either MapReduceRefusal MapRaster
reduceMapRaster r
    | VU.length (mrTexels r) ≢ mapRasterTexels r * 4 =
        Left $ MapReduceMalformedRaster (mrWidth r) (mrHeight r)
                   (VU.length (mrTexels r))
    | odd (mrWidth r) ∨ odd (mrHeight r) =
        Left $ MapReduceOddRaster (mrWidth r) (mrHeight r)
    | mrWidth r < 2 ∨ mrHeight r < 2 =
        Left $ MapReduceOddRaster (mrWidth r) (mrHeight r)
    | otherwise = Right $ MapRaster w' h' texels'
  where
    w' = mrWidth r `div` 2
    h' = mrHeight r `div` 2
    texels' = VU.generate (w' * h' * 4) $ \i →
        let component = i `mod` 4
            pixel = i `div` 4
            x = pixel `mod` w'
            y = pixel `div` w'
            (rr, gg, bb, aa) = reduceQuad
                (mapRasterTexel r (2 * x)     (2 * y))
                (mapRasterTexel r (2 * x + 1) (2 * y))
                (mapRasterTexel r (2 * x)     (2 * y + 1))
                (mapRasterTexel r (2 * x + 1) (2 * y + 1))
        in case component of
            0 → rr
            1 → gg
            2 → bb
            _ → aa

-- * Refusals

-- | Why a reduction was refused.
data MapReduceRefusal
    = MapReduceOddRaster !Int !Int
      -- ^ The raster's dimensions; at least one is odd or below two.
    | MapReduceMalformedRaster !Int !Int !Int
      -- ^ Stated dimensions and the actual byte count.
    deriving (Eq, Show)

-- | The log-facing text for a reduction refusal.
mapReduceRefusalText ∷ MapReduceRefusal → Text
mapReduceRefusalText (MapReduceOddRaster w h) =
    "Refusing to reduce a " <> tshow w <> "×" <> tshow h
    <> " raster: an adjacent-level 2×2 reduction needs both axes even "
    <> "and at least two texels."
mapReduceRefusalText (MapReduceMalformedRaster w h actual) =
    "Refusing a raster stated as " <> tshow w <> "×" <> tshow h
    <> " (" <> tshow (w * h * 4) <> " decoded bytes) whose payload is "
    <> tshow actual <> " bytes."
