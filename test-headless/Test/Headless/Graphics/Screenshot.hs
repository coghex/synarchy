-- | #643 debug.captureScreenshot — the pure, GPU-free half of the
--   capture pipeline: swapchain-format → channel-order mapping, and
--   the raw-grab → RGBA swizzle → PNG encode path. Channel swap and
--   row-order mistakes here corrupt every screenshot silently (the
--   issue's "half-pixel/channel-swap poisons everything" class), so
--   they're pinned by decoding the encoded PNG and checking pixels.
module Test.Headless.Graphics.Screenshot (spec) where

import UPrelude
import qualified Codec.Picture as JP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Engine.Graphics.Screenshot (grabToImage, grabToPngBytes)
import Engine.Graphics.Types (ScreenshotGrab(..), ScreenshotOrder(..))
import Engine.Graphics.Vulkan.Screenshot (screenshotOrderOf)
import Test.Hspec
import Vulkan.Core10 (Format(..))

-- | Build a grab from a list of 4-byte pixels (given in the grab's
--   native channel order), rows left-to-right, top row first.
mkGrab ∷ Int → Int → ScreenshotOrder → [[Word8]] → ScreenshotGrab
mkGrab w h order pxs = ScreenshotGrab
    { sgWidth  = w
    , sgHeight = h
    , sgOrder  = order
    , sgPixels = BS.pack (concat pxs)
    }

-- | Decode PNG bytes back to an RGBA8 image or fail the test.
decodeRGBA ∷ BSL.ByteString → IO (JP.Image JP.PixelRGBA8)
decodeRGBA bytes = case JP.decodePng (BSL.toStrict bytes) of
    Right dyn → pure (JP.convertRGBA8 dyn)
    Left err  → fail ("decodePng failed: " ⧺ err)

spec ∷ Spec
spec = do
    describe "screenshotOrderOf" $ do
        it "maps the preferred swapchain format (B8G8R8A8_UNORM) to BGRA" $
            screenshotOrderOf FORMAT_B8G8R8A8_UNORM `shouldBe` Just ScreenshotBGRA
        it "maps B8G8R8A8_SRGB to BGRA" $
            screenshotOrderOf FORMAT_B8G8R8A8_SRGB `shouldBe` Just ScreenshotBGRA
        it "maps R8G8B8A8_UNORM/SRGB to RGBA" $ do
            screenshotOrderOf FORMAT_R8G8B8A8_UNORM `shouldBe` Just ScreenshotRGBA
            screenshotOrderOf FORMAT_R8G8B8A8_SRGB `shouldBe` Just ScreenshotRGBA
        it "refuses formats it can't decode instead of guessing" $ do
            screenshotOrderOf FORMAT_R5G6B5_UNORM_PACK16 `shouldBe` Nothing
            screenshotOrderOf FORMAT_A2B10G10R10_UNORM_PACK32 `shouldBe` Nothing

    describe "grabToImage" $ do
        it "swaps BGRA to RGBA and forces alpha opaque" $ do
            -- red and blue pixels in BGRA byte order, with garbage alpha
            let grab = mkGrab 2 1 ScreenshotBGRA
                    [ [0, 0, 255, 7]     -- red   (B=0,G=0,R=255)
                    , [255, 0, 0, 200] ] -- blue  (B=255,G=0,R=0)
                img = grabToImage grab
            JP.pixelAt img 0 0 `shouldBe` JP.PixelRGBA8 255 0 0 255
            JP.pixelAt img 1 0 `shouldBe` JP.PixelRGBA8 0 0 255 255
        it "passes RGBA through unswapped, still forcing alpha" $ do
            let grab = mkGrab 2 1 ScreenshotRGBA
                    [ [255, 0, 0, 0]
                    , [10, 20, 30, 99] ]
                img = grabToImage grab
            JP.pixelAt img 0 0 `shouldBe` JP.PixelRGBA8 255 0 0 255
            JP.pixelAt img 1 0 `shouldBe` JP.PixelRGBA8 10 20 30 255
        it "keeps row 0 at the top (no vertical flip)" $ do
            let grab = mkGrab 1 2 ScreenshotBGRA
                    [ [0, 0, 255, 255]   -- top row: red
                    , [0, 255, 0, 255] ] -- bottom row: green
                img = grabToImage grab
            JP.pixelAt img 0 0 `shouldBe` JP.PixelRGBA8 255 0 0 255
            JP.pixelAt img 0 1 `shouldBe` JP.PixelRGBA8 0 255 0 255

    describe "grabToPngBytes" $ do
        it "round-trips a BGRA grab through PNG encode/decode" $ do
            let grab = mkGrab 2 2 ScreenshotBGRA
                    [ [0, 0, 255, 255], [0, 255, 0, 255]
                    , [255, 0, 0, 255], [255, 255, 255, 0] ]
            img ← decodeRGBA (grabToPngBytes grab)
            JP.imageWidth img `shouldBe` 2
            JP.imageHeight img `shouldBe` 2
            JP.pixelAt img 0 0 `shouldBe` JP.PixelRGBA8 255 0 0 255
            JP.pixelAt img 1 0 `shouldBe` JP.PixelRGBA8 0 255 0 255
            JP.pixelAt img 0 1 `shouldBe` JP.PixelRGBA8 0 0 255 255
            -- white with garbage alpha 0 comes out opaque white
            JP.pixelAt img 1 1 `shouldBe` JP.PixelRGBA8 255 255 255 255
