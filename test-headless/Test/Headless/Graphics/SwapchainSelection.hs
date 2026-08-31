-- | The swapchain capability-selection contract (#1954), pinned with no
--   GPU, driver, window, or display.
--
--   PR #14 added the surface-format and present-mode fallbacks and
--   logged the result, but never distinguished a degraded selection
--   from a preferred one: the same record was emitted whether MAILBOX
--   was chosen because the driver offers it or FIFO was reached because
--   it offers neither low-latency mode. Nothing warned, and both
--   selectors were unexported, so nothing could assert the difference
--   either.
--
--   These specs pin the classification production now warns from. Each
--   case asserts BOTH the exact chosen value — the values handed to
--   @SwapchainCreateInfoKHR@ must not move — and whether that choice
--   was a fallback.
module Test.Headless.Graphics.SwapchainSelection (spec) where

import UPrelude
import qualified Data.Vector as V
import Engine.Graphics.Vulkan.Swapchain
  (PresentModeChoice(..), SurfaceFormatChoice(..), chooseSwapPresentMode
  ,chooseSwapSurfaceFormat, preferredSurfaceFormat)
import Test.Hspec
import Vulkan.Core10 (Format(..))
import Vulkan.Extensions.VK_KHR_surface
  (ColorSpaceKHR(..), SurfaceFormatKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (PresentModeKHR(..))
import Vulkan.Zero (zero)

-- | A surface-format pair, written out rather than reusing production's
--   'preferredSurfaceFormat' constructor, so this spec states the pairs
--   independently.
fmt ∷ Format → ColorSpaceKHR → SurfaceFormatKHR
fmt f c = zero { format = f, colorSpace = c }

srgbNL ∷ ColorSpaceKHR
srgbNL = COLOR_SPACE_SRGB_NONLINEAR_KHR

-- The preferred pair, spelled independently of production's binding.
preferred ∷ SurfaceFormatKHR
preferred = fmt FORMAT_B8G8R8A8_UNORM srgbNL

-- Two non-preferred pairs. The first differs in FORMAT, the second only
-- in COLOR SPACE — the pair is one capability, so a selector comparing
-- formats alone would wrongly accept the latter as preferred.
otherFmt, sameFormatOtherSpace ∷ SurfaceFormatKHR
otherFmt            = fmt FORMAT_R8G8B8A8_UNORM srgbNL
sameFormatOtherSpace = fmt FORMAT_B8G8R8A8_UNORM COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT

mailbox, immediate, fifo, fifoRelaxed ∷ PresentModeKHR
mailbox     = PRESENT_MODE_MAILBOX_KHR
immediate   = PRESENT_MODE_IMMEDIATE_KHR
fifo        = PRESENT_MODE_FIFO_KHR
fifoRelaxed = PRESENT_MODE_FIFO_RELAXED_KHR

spec ∷ Spec
spec = do
  describe "Graphics.SwapchainSelection (#1954)" $ do
    describe "chooseSwapSurfaceFormat" $ do
      it "reports the engine's preferred pair as B8G8R8A8_UNORM / SRGB_NONLINEAR" $
        preferredSurfaceFormat `shouldBe` preferred

      it "takes the preferred pair, unflagged, when it is advertised first" $
        chooseSwapSurfaceFormat (V.fromList [preferred, otherFmt])
          `shouldBe` (preferred, SurfaceFormatPreferred)

      it "takes the preferred pair, unflagged, when it is advertised AFTER another format" $
        chooseSwapSurfaceFormat (V.fromList [otherFmt, sameFormatOtherSpace, preferred])
          `shouldBe` (preferred, SurfaceFormatPreferred)

      it "falls back to the FIRST advertised pair, flagged, when the preferred one is absent" $
        chooseSwapSurfaceFormat (V.fromList [otherFmt, sameFormatOtherSpace])
          `shouldBe` (otherFmt, SurfaceFormatFirstAdvertised)

      it "is deterministic in which non-preferred pair it takes — the head, not the other one" $
        chooseSwapSurfaceFormat (V.fromList [sameFormatOtherSpace, otherFmt])
          `shouldBe` (sameFormatOtherSpace, SurfaceFormatFirstAdvertised)

      it "treats a matching format with a different colour space as NOT the preferred pair" $
        snd (chooseSwapSurfaceFormat (V.fromList [sameFormatOtherSpace]))
          `shouldBe` SurfaceFormatFirstAdvertised

      it "reports the empty advertisement distinctly, still returning the preferred pair" $
        chooseSwapSurfaceFormat V.empty
          `shouldBe` (preferred, SurfaceFormatNoneAdvertised)

      it "classifies the empty case apart from every non-empty one" $ do
        -- Requirement 2 exists because the empty vector returns the same
        -- VALUE as the preferred branch; only the classification separates
        -- them.
        fst (chooseSwapSurfaceFormat V.empty)
          `shouldBe` fst (chooseSwapSurfaceFormat (V.fromList [preferred]))
        snd (chooseSwapSurfaceFormat V.empty)
          `shouldNotBe` snd (chooseSwapSurfaceFormat (V.fromList [preferred]))

    describe "chooseSwapPresentMode" $ do
      it "requests FIFO, unflagged, when VSync is enabled" $
        chooseSwapPresentMode (V.fromList [mailbox, immediate, fifo]) True
          `shouldBe` (fifo, PresentModeRequested)

      it "requests FIFO, unflagged, when VSync is enabled and nothing else is advertised" $
        chooseSwapPresentMode (V.fromList [fifo]) True
          `shouldBe` (fifo, PresentModeRequested)

      it "prefers MAILBOX, unflagged, when both low-latency modes are advertised" $
        chooseSwapPresentMode (V.fromList [fifo, immediate, mailbox]) False
          `shouldBe` (mailbox, PresentModeLowLatency)

      it "accepts IMMEDIATE, unflagged, when MAILBOX is absent" $
        chooseSwapPresentMode (V.fromList [fifo, immediate]) False
          `shouldBe` (immediate, PresentModeLowLatency)

      it "falls back to FIFO, flagged, when neither low-latency mode is advertised" $
        chooseSwapPresentMode (V.fromList [fifo, fifoRelaxed]) False
          `shouldBe` (fifo, PresentModeFifoFallback)

      it "distinguishes the VSync-enabled FIFO request from the VSync-disabled FIFO fallback" $ do
        -- Both return FIFO; requirement 4 says only one of them is a
        -- degraded selection.
        let advertised = V.fromList [fifo]
        fst (chooseSwapPresentMode advertised True)
          `shouldBe` fst (chooseSwapPresentMode advertised False)
        snd (chooseSwapPresentMode advertised True)
          `shouldNotBe` snd (chooseSwapPresentMode advertised False)

      it "keeps VSync's answer independent of what is advertised" $
        forM_ [V.empty, V.fromList [mailbox], V.fromList [immediate, fifo]] $ \advertised →
          chooseSwapPresentMode advertised True `shouldBe` (fifo, PresentModeRequested)
