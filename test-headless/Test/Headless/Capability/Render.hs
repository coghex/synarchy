-- | Projection-aliasing coverage for the @render-gpu-asset@ capability
--   records (issue #891, E3 of the @EngineEnv@ capability split #537).
--
--   Both projections are documented as returning the __identical live
--   containers__ 'EngineEnv' already carries — never a copy, never a
--   snapshot. That property is what makes a capability record safe to
--   construct as often as a consumer likes, and it is exactly the kind
--   of thing a refactor can silently break: a projection that binds
--   'rcCameraRef' to @uiCameraRef env@ (or copies a container with
--   @newIORef =<< readIORef@) still typechecks, still passes the SS6
--   ratchet and the SS3 boundary audit, and still looks right in a
--   diff — while quietly changing runtime behaviour.
--
--   Every field of both records is either an 'Data.IORef.IORef'
--   (pointer 'Eq') or an 'Engine.Core.Queue.Queue' (which derives 'Eq'
--   through its 'TQueue'), so "same live container" is directly
--   assertable: compare the projected field against the same
--   'EngineEnv' field. A wrong-container binding fails the
--   corresponding example — including a SWAP between two fields of the
--   same type, since each field is checked against its own named
--   counterpart rather than merely "some field of the env".
--
--   Per SS6.3 test fixtures are outside the full-access ratchet, so
--   this module imports @EngineEnv(..)@ directly — that is the point:
--   it compares the capability's view against the unrestricted one.
module Test.Headless.Capability.Render (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.Render
  (RenderCapability(..), toRenderCapability)
import Engine.Core.Capability.RenderView
  (RenderViewCapability(..), toRenderViewCapability)

-- | Assert two live containers are the SAME one. Neither 'IORef' nor
--   'Engine.Core.Queue.Queue' has a 'Show' instance, so this is an
--   'Eq'-only assertion carrying its own failure message rather than
--   'shouldBe'\'s rendered-value diff.
sameContainer ∷ Eq α ⇒ α → α → Expectation
sameContainer projected live
  | projected == live = pure ()
  | otherwise = expectationFailure
      "projected field is NOT the live EngineEnv container -- the \
      \projection copied, swapped, or reconstructed it instead of \
      \aliasing it (see Engine.Core.Capability.Render's convention)"

spec ∷ SpecWith EngineEnv
spec = do
  describe "toRenderCapability (all 22 render-gpu-asset fields)" $ do
    let aliases name project field =
          it (name <> " aliases the live EngineEnv container") $ \env →
            sameContainer (project (toRenderCapability env)) (field env)

    -- The main-render-private pointer (SS3): present on this record,
    -- and deliberately absent from the worker view below.
    aliases "rcEngineStateRef"         rcEngineStateRef         engineStateRef
    aliases "rcVideoConfigRef"         rcVideoConfigRef         videoConfigRef
    aliases "rcWindowSizeRef"          rcWindowSizeRef          windowSizeRef
    aliases "rcWindowPosRef"           rcWindowPosRef           windowPosRef
    aliases "rcWindowStateRef"         rcWindowStateRef         windowStateRef
    aliases "rcFramebufferSizeRef"     rcFramebufferSizeRef     framebufferSizeRef
    aliases "rcFpsRef"                 rcFpsRef                 fpsRef
    aliases "rcBrightnessRef"          rcBrightnessRef          brightnessRef
    aliases "rcPixelSnapRef"           rcPixelSnapRef           pixelSnapRef
    aliases "rcTextureFilterRef"       rcTextureFilterRef       textureFilterRef
    aliases "rcAssetPoolRef"           rcAssetPoolRef           assetPoolRef
    aliases "rcTextureNameRegistryRef" rcTextureNameRegistryRef textureNameRegistryRef
    aliases "rcFontCacheRef"           rcFontCacheRef           fontCacheRef
    aliases "rcTextureSystemRef"       rcTextureSystemRef       textureSystemRef
    aliases "rcSamplerCacheRef"        rcSamplerCacheRef        samplerCacheRef
    aliases "rcTextureSizeRef"         rcTextureSizeRef         textureSizeRef
    -- Added by #2020: MainRender's Vulkan init is this field's only
    -- writer, and every worker reads it through the view below. A
    -- projection that copied it would leave those readers looking at a
    -- container Vulkan init never touches — i.e. a permanently absent
    -- device limit, which a GPU-capable mode turns into a refusal.
    aliases "rcMaxImageDimensionRef"   rcMaxImageDimensionRef   maxImageDimensionRef
    aliases "rcDefaultFaceMapSlotRef"  rcDefaultFaceMapSlotRef  defaultFaceMapSlotRef
    aliases "rcCameraRef"              rcCameraRef              cameraRef
    aliases "rcUiCameraRef"            rcUiCameraRef            uiCameraRef
    -- The one non-IORef field in the set — an STM queue, and the
    -- easiest of the 22 to omit from a hand-written check.
    aliases "rcScreenshotRequestQueue" rcScreenshotRequestQueue screenshotRequestQueue
    aliases "rcNextObjectIdRef"        rcNextObjectIdRef        nextObjectIdRef

    it "keeps the three same-typed geometry refs distinct" $ \env → do
      -- windowSizeRef, framebufferSizeRef and windowPosRef are the only
      -- fields in the set that share a type (IORef (Int, Int)), so they
      -- are the ones a projection could transpose and still COMPILE. The
      -- per-field examples above already pin each to its own named
      -- counterpart; this states the risk explicitly, and also asserts
      -- all three are genuinely different containers (they hold
      -- different values under a HiDPI/scaled framebuffer, and a window
      -- at any position but the origin).
      let cap = toRenderCapability env
      sameContainer (rcWindowSizeRef cap) (windowSizeRef env)
      sameContainer (rcFramebufferSizeRef cap) (framebufferSizeRef env)
      sameContainer (rcWindowPosRef cap) (windowPosRef env)
      (windowSizeRef env == framebufferSizeRef env) `shouldBe` False
      (windowSizeRef env == windowPosRef env) `shouldBe` False
      (framebufferSizeRef env == windowPosRef env) `shouldBe` False

  describe "toRenderViewCapability (the 16 worker-visible fields)" $ do
    let aliases name project field =
          it (name <> " aliases the live EngineEnv container") $ \env →
            sameContainer (project (toRenderViewCapability env)) (field env)

    aliases "rvVideoConfigRef"         rvVideoConfigRef         videoConfigRef
    aliases "rvWindowSizeRef"          rvWindowSizeRef          windowSizeRef
    aliases "rvWindowPosRef"           rvWindowPosRef           windowPosRef
    aliases "rvFramebufferSizeRef"     rvFramebufferSizeRef     framebufferSizeRef
    -- Added to the view by #1693: the InputThread writer that publishes
    -- the minimize edge reaches it through this same projection.
    aliases "rvFramebufferMinimizeGenRef" rvFramebufferMinimizeGenRef
                                          framebufferMinimizeGenRef
    aliases "rvPixelSnapRef"           rvPixelSnapRef           pixelSnapRef
    -- Added to the view by #893 (E5a): engine.getFPS is the
    -- LuaThread reader SS5 names, and API.Core is now narrowed.
    aliases "rvFpsRef"                 rvFpsRef                 fpsRef
    aliases "rvTextureFilterRef"       rvTextureFilterRef       textureFilterRef
    aliases "rvAssetPoolRef"           rvAssetPoolRef           assetPoolRef
    aliases "rvTextureNameRegistryRef" rvTextureNameRegistryRef textureNameRegistryRef
    aliases "rvFontCacheRef"           rvFontCacheRef           fontCacheRef
    aliases "rvTextureSystemRef"       rvTextureSystemRef       textureSystemRef
    aliases "rvTextureSizeRef"         rvTextureSizeRef         textureSizeRef
    -- Added to the view by #2020: the WorldThread and LuaThread readers
    -- of the device's maxImageDimension2D reach it through here rather
    -- than through main-render-private GraphicsState.
    aliases "rvMaxImageDimensionRef"   rvMaxImageDimensionRef   maxImageDimensionRef
    aliases "rvCameraRef"              rvCameraRef              cameraRef
    aliases "rvScreenshotRequestQueue" rvScreenshotRequestQueue screenshotRequestQueue

    it "shares the exact containers the full record exposes" $ \env → do
      -- The view is a projection of EngineEnv in its own right, not a
      -- derivative of RenderCapability — but both must land on the
      -- same live handles, or a MainRender write would not be visible
      -- to a worker-thread read (and vice versa).
      let full = toRenderCapability env
          view = toRenderViewCapability env
      sameContainer (rvTextureSystemRef view) (rcTextureSystemRef full)
      sameContainer (rvTextureSizeRef view) (rcTextureSizeRef full)
      -- #2020's cross-thread carrier is the sharpest case of this rule:
      -- MainRender writes it through the full record during Vulkan init
      -- and workers read it through the view, so the two landing on
      -- different containers would silently mean "no device limit was
      -- ever published" on every worker.
      sameContainer (rvMaxImageDimensionRef view) (rcMaxImageDimensionRef full)
      sameContainer (rvCameraRef view) (rcCameraRef full)
      sameContainer (rvWindowPosRef view) (rcWindowPosRef full)
      sameContainer (rvAssetPoolRef view) (rcAssetPoolRef full)
      sameContainer (rvScreenshotRequestQueue view) (rcScreenshotRequestQueue full)
