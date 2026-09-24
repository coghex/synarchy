{-# LANGUAGE Strict #-}
-- | The paged map artifact's page codec (issue #2693): lossless 8-bit
--   RGBA PNG through the JuicyPixels dependency the tree already has
--   (the owner's #2303 choice).
--
--   Decoding is gated in the order requirement 5 fixes. The declared
--   geometry is read from the PNG signature and IHDR — a fixed 33-byte
--   prefix — and must be exactly the accepted page plan's width and
--   height, 8-bit depth, colour type 6 (RGBA), deflate, adaptive
--   filtering and no interlace, all BEFORE the decoder sees a byte. Other
--   pixel types are refused rather than promoted: a page that is not
--   RGBA8 is not a page this format wrote.
--
--   The IHDR bounds the IMAGE, not the deflate stream: JuicyPixels
--   inflates every IDAT byte to one buffer before cutting the image out
--   of it, so a small, valid PNG could still make it allocate many times
--   a page. The concatenated IDAT data is therefore inflated first by a
--   streaming inflater that stops at the first output byte past the
--   exact non-interlaced RGBA8 length — one filter byte plus four bytes
--   per texel, per row — and requires exactly that length and nothing
--   after the stream. Only then does the native decoder run, inside an
--   exception boundary with its output fully forced, because it too
--   inflates through zlib, which can throw on a corrupt stream instead of
--   returning 'Left'.
module World.ZoomMap.PagedArtifact.Png
    ( encodePagePng
    , checkPagePngHeader
    , checkPageImageData
    , decodePagePng
    ) where

import UPrelude
import qualified Codec.Picture as JP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Vector.Storable as VS
import qualified Codec.Compression.Zlib.Internal as Z
import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad.ST.Lazy (ST, runST)
import World.Map.ImagePlan (MapImagePlan(..), checkUploadPayload)
import World.ZoomMap.PagedArtifact.Types

-- | Encode one page's exact decoded RGBA8 bytes. The length is checked
--   against the page plan first; the encoded size against
--   'mapPagePngMaxBytes' after.
encodePagePng ∷ Text → BS.ByteString → Either MapArtifactRefusal BS.ByteString
encodePagePng what rgba = do
    plan ← either (Left . MapArtifactImage) Right mapPagePlan
    either (Left . MapArtifactImage) Right $
        checkUploadPayload plan (BS.length rgba)
    let (fp, len) = BSI.toForeignPtr0 (BS.copy rgba)
        img = JP.Image (mipWidth plan) (mipHeight plan)
                  (VS.unsafeFromForeignPtr0 fp len) ∷ JP.Image JP.PixelRGBA8
        png = BL.toStrict (JP.encodePng img)
    when (BS.length png > mapPagePngMaxBytes) $
        Left $ MapArtifactOversized what (toInteger mapPagePngMaxBytes)
                                         (toInteger (BS.length png))
    pure png

-- | Refuse a PNG longer than 'mapPagePngMaxBytes', or whose signature
--   or IHDR is not exactly a page's, before any pixel is decoded or
--   allocated.
checkPagePngHeader ∷ Text → BS.ByteString → Either MapArtifactRefusal ()
checkPagePngHeader what png = do
    plan ← either (Left . MapArtifactImage) Right mapPagePlan
    when (BS.length png > mapPagePngMaxBytes) $
        Left $ MapArtifactOversized what (toInteger mapPagePngMaxBytes)
                                         (toInteger (BS.length png))
    when (BS.length png < 33) $
        Left $ MapArtifactPngHeader what "shorter than a PNG signature and IHDR"
    unless (BS.take 8 png ≡ pngSignature) $
        Left $ MapArtifactPngHeader what "no PNG signature"
    unless (word32At png 8 ≡ 13 ∧ BS.take 4 (BS.drop 12 png) ≡ "IHDR") $
        Left $ MapArtifactPngHeader what "the first chunk is not a 13-byte IHDR"
    let width  = word32At png 16
        height = word32At png 20
        expected = (mipWidth plan, mipHeight plan)
    unless (toInteger width ≡ toInteger (fst expected)
            ∧ toInteger height ≡ toInteger (snd expected)) $
        Left $ MapArtifactPngDimensions what expected
                   (fromIntegral width, fromIntegral height)
    let field name ix want = unless (BS.index png ix ≡ want) $
            Left $ MapArtifactPngHeader what $
                name <> " is " <> tshow (BS.index png ix)
                <> ", not " <> tshow want
    field "bit depth" 24 8
    field "colour type" 25 6
    field "compression method" 26 0
    field "filter method" 27 0
    field "interlace method" 28 0

pngSignature ∷ BS.ByteString
pngSignature = BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

-- | Bound what the PNG's image data inflates to, before any decoder
--   allocates for it: walk the chunk list to IEND, concatenate the IDAT
--   payloads (at most the encoded size, itself bounded), and inflate them
--   incrementally, refusing at the first output past the page's exact
--   scanline length. Assumes 'checkPagePngHeader' passed.
checkPageImageData ∷ Text → BS.ByteString → Either MapArtifactRefusal ()
checkPageImageData what png = do
    plan ← either (Left . MapArtifactImage) Right mapPagePlan
    idat ← pngImageData what png
    let expected = mipHeight plan * (1 + 4 * mipWidth plan)
    runST (inflateBounded what expected idat)

-- | The concatenated IDAT payloads, in file order, up to IEND.
pngImageData ∷ Text → BS.ByteString → Either MapArtifactRefusal BS.ByteString
pngImageData what png = go 8 []
  where
    total = toInteger (BS.length png)
    go off acc
        | toInteger off + 12 > total =
            Left $ MapArtifactPngHeader what "the chunk list ends without IEND"
        | otherwise = do
            let len = toInteger (word32At png off)
                kind = BS.take 4 (BS.drop (off + 4) png)
                start = off + 8
                next = toInteger start + len + 4
            when (next > total) $
                Left $ MapArtifactPngHeader what
                    ("chunk " <> tshow kind <> " runs past the end of the file")
            let body = BS.take (fromInteger len) (BS.drop start png)
            case kind of
                "IEND" → Right (BS.concat (reverse acc))
                "IDAT" → go (fromInteger next) (body : acc)
                _      → go (fromInteger next) acc

-- | Inflate a zlib stream, counting output and stopping at the first
--   chunk that takes the total past @expected@. The inflater's output
--   chunks are bounded by its buffer size, so the most ever held is one
--   chunk; nothing is retained.
inflateBounded ∷ Text → Int → BS.ByteString → ST s (Either MapArtifactRefusal ())
inflateBounded what expected idat =
    go 0 (Just idat) (Z.decompressST Z.zlibFormat Z.defaultDecompressParams)
  where
    sized n = MapArtifactInflatedSize what (toInteger expected) (toInteger n)
    go total input stream = case stream of
        Z.DecompressInputRequired supply → case input of
            -- The data, then one empty chunk (end of input), then nothing.
            Just chunk → supply chunk ≫= go total (if BS.null chunk then Nothing else Just BS.empty)
            Nothing → pure (Left (MapArtifactDecodeFailure what "the deflate stream is truncated"))
        Z.DecompressOutputAvailable out next
            | total + BS.length out > expected → pure (Left (sized (total + BS.length out)))
            | otherwise → next ≫= go (total + BS.length out) input
        Z.DecompressStreamEnd rest
            | not (BS.null rest) → pure $ Left $ MapArtifactMalformed what
                "the image data continues after its deflate stream ends"
            | total ≢ expected → pure (Left (sized total))
            | otherwise → pure (Right ())
        Z.DecompressStreamError e → pure (Left (MapArtifactDecodeFailure what (tshow e)))

word32At ∷ BS.ByteString → Int → Word32
word32At bytes i = foldl' (\acc k → acc `shiftL` 8 ⌄ fromIntegral (BS.index bytes (i + k)))
                          0 [0 .. 3]

-- | Decode a page PNG to its exact RGBA8 bytes: header gate, then the
--   bounded inflate of its image data, then the native decoder under an
--   exception boundary, then the decoded length against the page plan.
decodePagePng ∷ Text → BS.ByteString → IO (Either MapArtifactRefusal BS.ByteString)
decodePagePng what png = case checkPagePngHeader what png of
    Left r → pure (Left r)
    Right () → do
      bounded ← try (evaluate (checkPageImageData what png))
      case bounded of
       Left (e ∷ SomeException) → pure (Left (MapArtifactDecodeFailure what (tshow e)))
       Right (Left r) → pure (Left r)
       Right (Right ()) → do
        decoded ← try (evaluate (force (nativeDecode png)))
        pure $ case decoded of
            Left (e ∷ SomeException) → Left (MapArtifactDecodeFailure what (tshow e))
            Right (Left why) → Left (MapArtifactDecodeFailure what (T.pack why))
            Right (Right bytes) → do
                plan ← either (Left . MapArtifactImage) Right mapPagePlan
                either (Left . MapArtifactImage) Right $
                    checkUploadPayload plan (BS.length bytes)
                pure bytes

-- | JuicyPixels' decode, reduced to the one pixel type a page may be.
--   The result is COPIED out of the decoder's vector so it is a plain,
--   fully materialised strict ByteString when forced.
nativeDecode ∷ BS.ByteString → Either String BS.ByteString
nativeDecode png = case JP.decodePng png of
    Left why → Left why
    Right (JP.ImageRGBA8 img) →
        let (fp, len) = VS.unsafeToForeignPtr0 (JP.imageData img)
        in Right (BS.copy (BSI.fromForeignPtr fp 0 len))
    Right _ → Left "decoded to a pixel type other than RGBA8"
