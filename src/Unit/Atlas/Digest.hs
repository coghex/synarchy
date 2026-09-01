{-# LANGUAGE Strict, OverloadedStrings #-}
-- | The two digests @tools\/pack_atlas.py@ records in a compiled unit
--   index (#1259, TEX-3), reproduced exactly so the runtime can verify
--   them.
--
--   Both are @sha256@ over a canonical, length-prefixed stream of
--   labelled fields (the compiler's @digest_stream@): the domain tag,
--   then each field as @\<u64 LE length\>\<bytes\>@. The length prefixes
--   are what make the stream injective — a bare concatenation would let
--   a character move across a field boundary without changing the hash.
--
--     * 'atlasContentDigest' — over one atlas's decoded RGBA8 CONTENT
--       (dimensions + samples), never its file bytes, so it stays
--       meaningful across PNG encoders while still pinning every pixel.
--     * 'sourceDigest' — over everything one animation was COMPILED
--       FROM: its identity, its mirroring/timing declarations, its cell
--       geometry INCLUDING the extrusion gutter, and for each direction
--       in atlas order its declared frame paths and their decoded
--       pixels. The domain tag carries @v2@ for that gutter (#2076), so
--       no digest recorded before it can collide with one taken over
--       the same art at the padded stride.
--
--   Reproducing @source_digest@ means reproducing one awkward thing:
--   the compiler writes @fps@ as Python's @repr()@ of the narrowed
--   value. 'pythonFloatRepr' does that, and is pinned against
--   CPython-generated reference values in @Test.Headless.Unit.Atlas@ —
--   a formatting divergence there would REJECT valid art, which is a
--   far worse failure than the staleness it guards, so it is tested
--   rather than trusted.
module Unit.Atlas.Digest
    ( atlasContentDigest
    , SourceFrameInput(..)
    , SourceDirectionInput(..)
    , SourceAnimInput(..)
    , sourceDigest
    , pythonFloatRepr
    ) where

import UPrelude
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (floatToDigits, showHex)

-- | The compiler's @ATLAS_DIGEST_TAG@ and @SOURCE_DIGEST_TAG@. Each
--   carries its own version, so a change to what goes INTO a digest
--   invalidates every recorded one rather than silently producing a
--   colliding value from different inputs.
atlasDigestTag, sourceDigestTag ∷ BS.ByteString
atlasDigestTag  = "synarchy-atlas-content-v1"
sourceDigestTag = "synarchy-atlas-source-v2"

-- | The compiler's @content_digest@.
atlasContentDigest ∷ Int → Int → BS.ByteString → Text
atlasContentDigest w h pixels = digestStream atlasDigestTag
    [ ("width",  BC.pack (show w))
    , ("height", BC.pack (show h))
    , ("pixels", pixels)
    ]

-- | One declared source frame, exactly as the compiler saw it.
data SourceFrameInput = SourceFrameInput
    { sfiPath   ∷ !Text            -- ^ the DECLARED path, as the YAML wrote it
    , sfiWidth  ∷ !Int
    , sfiHeight ∷ !Int
    , sfiPixels ∷ !BS.ByteString   -- ^ canonical decoded RGBA8
    } deriving (Show, Eq)

-- | One direction's row, in atlas order.
data SourceDirectionInput = SourceDirectionInput
    { sdiDirection ∷ !Text         -- ^ the index's own token ("south-west")
    , sdiRow       ∷ !Int
    , sdiFrames    ∷ ![SourceFrameInput]
    } deriving (Show, Eq)

-- | Everything one animation was compiled from.
data SourceAnimInput = SourceAnimInput
    { saiUnit        ∷ !Text
    , saiName        ∷ !Text
    , saiFlip        ∷ !Bool
    , saiLoop        ∷ !Bool
    , saiFps         ∷ !Float
    , saiCellWidth   ∷ !Int
    , saiCellHeight  ∷ !Int
    , saiCellPadding ∷ !Int
      -- ^ The extrusion gutter per side (#2076). A digest input
      --   because it changes the artifact every other input would
      --   otherwise describe identically: the same frames at a
      --   different gutter compile to a different sheet.
    , saiColumns     ∷ !Int
    , saiDirections  ∷ ![SourceDirectionInput]
      -- ^ MUST be in the compiler's atlas direction order, which is the
      --   engine's own 'Unit.Direction.Direction' order restricted to
      --   the authored directions — i.e. ascending by constructor, which
      --   is what @Map.toAscList@ over a @Map Direction _@ already gives.
    } deriving (Show, Eq)

-- | The compiler's @source_digest@, field for field and in its order.
sourceDigest ∷ SourceAnimInput → Text
sourceDigest a = digestStream sourceDigestTag $
    [ ("unit",            TE.encodeUtf8 (saiUnit a))
    , ("animation",       TE.encodeUtf8 (saiName a))
    , ("flip",            bit (saiFlip a))
    , ("loop",            bit (saiLoop a))
    , ("fps",             TE.encodeUtf8 (pythonFloatRepr (saiFps a)))
    , ("cell",            dims (saiCellWidth a) (saiCellHeight a))
    , ("cell_padding",    BC.pack (show (saiCellPadding a)))
    , ("columns",         BC.pack (show (saiColumns a)))
    , ("direction_count", BC.pack (show (length (saiDirections a))))
    ] <> concatMap direction (saiDirections a)
  where
    bit b = if b then "1" else "0"
    dims w h = BC.pack (show w <> "x" <> show h)
    direction d =
        [ ("direction",   TE.encodeUtf8 (sdiDirection d))
        , ("row",         BC.pack (show (sdiRow d)))
        , ("frame_count", BC.pack (show (length (sdiFrames d))))
        ] <> concatMap frame (sdiFrames d)
    frame f =
        [ ("frame_path",   TE.encodeUtf8 (sfiPath f))
        , ("frame_size",   dims (sfiWidth f) (sfiHeight f))
        , ("frame_pixels", sfiPixels f)
        ]

-- * The stream

digestStream ∷ BS.ByteString → [(BS.ByteString, BS.ByteString)] → Text
digestStream tag fields =
    hex ∘ SHA256.finalize ∘ SHA256.updates SHA256.init $
        [lengthPrefix (BS.length tag), tag] <> concatMap field fields
  where
    field (label, value) =
        [ lengthPrefix (BS.length label), label
        , lengthPrefix (BS.length value), value ]

lengthPrefix ∷ Int → BS.ByteString
lengthPrefix n = BS.pack
    [ fromIntegral ((n `shiftR` (8 * i)) ⌃ 0xff) | i ← [0 .. 7 ∷ Int] ]

hex ∷ BS.ByteString → Text
hex = T.pack ∘ concatMap byte ∘ BS.unpack
  where
    byte b = let s = showHex b "" in if length s ≡ 1 then '0' : s else s

-- * Python's float repr

-- | CPython's @repr()@ of a float, for the narrowed value the compiler
--   records as @fps@.
--
--   The compiler writes @repr(narrow_to_runtime_float(fps))@ — the
--   @double@ that exactly equals the engine's 32-bit value — so this
--   widens and formats by CPython's own rules
--   (@format_float_short@, mode @\'r\'@):
--
--     * the digits are the SHORTEST decimal that round-trips the
--       double, which is exactly what 'floatToDigits' produces;
--     * with @value = 0.d1d2…dn * 10^decpt@, notation is scientific
--       when @decpt <= -4@ or @decpt > 16@ and positional otherwise —
--       a threshold pair Haskell's own 'show' does NOT share (it
--       switches at @0.1@ and @10^7@), which is the whole reason this
--       exists;
--     * a positional value with nothing after the point still gets a
--       trailing @.0@ (CPython's @Py_DTSF_ADD_DOT_0@);
--     * a scientific exponent is always signed and at least two digits.
pythonFloatRepr ∷ Float → Text
pythonFloatRepr x
    | isNaN x      = "nan"
    | isInfinite x = if x < 0 then "-inf" else "inf"
    | x < 0        = "-" <> render (negate (realToFrac x ∷ Double))
    | otherwise    = render (realToFrac x ∷ Double)
  where
    render d
        | d ≡ 0 = "0.0"
        | otherwise =
            let (ds, decpt) = floatToDigits 10 d
                digits = map intToDigitChar ds
            in T.pack $ if decpt ≤ -4 ∨ decpt > 16
                then scientific digits decpt
                else positional digits decpt

    intToDigitChar n = toEnum (fromEnum '0' + n)

    scientific digits decpt =
        let mantissa = case digits of
                (c:[])   → [c]
                (c:rest) → c : '.' : rest
                []       → "0"
            e = decpt - 1
            sign = if e < 0 then '-' else '+'
            body = show (abs e)
            padded = if length body < 2 then '0' : body else body
        in mantissa <> ('e' : sign : padded)

    positional digits decpt
        | decpt ≤ 0 = "0." <> replicate (negate decpt) '0' <> digits
        | decpt ≥ length digits =
            digits <> replicate (decpt - length digits) '0' <> ".0"
        | otherwise =
            let (whole, frac) = splitAt decpt digits
            in whole <> "." <> frac
