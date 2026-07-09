-- | Pure screenshot pixel plumbing for debug.captureScreenshot (#643):
--   raw framebuffer grab → RGBA image → PNG bytes. No Vulkan, no IO —
--   runs on the requesting (Lua) thread so the render loop never pays
--   for the encode, and unit-testable without a GPU
--   (Test.Headless.Graphics.Screenshot).
module Engine.Graphics.Screenshot
  ( grabToImage
  , grabToPngBytes
  ) where

import UPrelude
import qualified Codec.Picture as JP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector.Storable as VS
import Engine.Graphics.Types (ScreenshotGrab(..), ScreenshotOrder(..))

-- | Swizzle a raw grab into a JuicyPixels RGBA image. BGRA input (the
--   usual swapchain order) gets its red/blue channels swapped; alpha
--   is forced opaque in both orders — the swapchain composites opaque
--   (COMPOSITE_ALPHA_OPAQUE) so whatever the alpha channel holds is
--   garbage, not coverage. Row 0 of the grab is the top of the screen,
--   matching PNG row order, so no vertical flip is needed.
grabToImage ∷ ScreenshotGrab → JP.Image JP.PixelRGBA8
grabToImage grab =
    JP.Image (sgWidth grab) (sgHeight grab) $
        VS.generate (sgWidth grab * sgHeight grab * 4) pick
  where
    pixels = sgPixels grab
    pick i =
        let (px, c) = i `divMod` 4
        in case c of
            3 → 255
            _ → BS.index pixels (px * 4 + srcChannel c)
    srcChannel c = case sgOrder grab of
        ScreenshotRGBA → c
        ScreenshotBGRA → 2 - c

-- | Full grab → PNG file contents.
grabToPngBytes ∷ ScreenshotGrab → BSL.ByteString
grabToPngBytes = JP.encodePng . grabToImage
