-- | The __worker-visible__ render view of the @render-gpu-asset@
--   capability (epic #537, issue #891 — E3): the strictly narrower
--   companion to "Engine.Core.Capability.Render", for production code
--   that runs on a thread other than @MainRender@.
--
--   == Why a second record exists
--
--   'docs/engineenv_capability_inventory.md' SS3 makes @EngineState@
--   main-render-thread-private, and the capability-record convention
--   (SS2.1) exports every capability record as @Capability(..)@ —
--   constructor and accessors alike. A single @render-gpu-asset@
--   record visible to worker threads would therefore hand
--   @WorldThread@\/@LuaThread@ code a way to /inspect/
--   @engineStateRef@, no matter what its Haddock said.
--   This record is the resolution: it exposes only the render fields
--   SS5 documents a non-@MainRender@ thread as a legitimate reader (or
--   writer) of, and it __contains no @engineStateRef@ field at all__,
--   so there is no path from here to the main-render-private state.
--
--   It is a projection of 'EngineEnv' in its own right — never derived
--   from, and never widened back into, 'Engine.Core.Capability.Render.RenderCapability'.
--
--   == What is deliberately absent
--
--   Beyond @engineStateRef@, the 22-field @render-gpu-asset@ set's
--   remaining @MainRender@-only fields are omitted because no
--   non-@MainRender@ consumer reads them (SS5's Readers\/Writers
--   cells): @windowStateRef@, @brightnessRef@, @samplerCacheRef@,
--   @defaultFaceMapSlotRef@ and @uiCameraRef@. @nextObjectIdRef@ has a
--   @LuaThread@ reader, but its only consumer is the
--   permanently-full-access @Engine.Scripting.Lua.Thread@, and E1's "no
--   unused capability records ahead of need" applies field-by-field
--   here too — a later migration that needs one adds it then.
--
--   @fpsRef@ was omitted on the same "no consumer yet" grounds when
--   #891 landed and is now present: #893 (E5a) narrowed
--   @Engine.Scripting.Lua.API.Core@, whose @engine.getFPS@ is the
--   @LuaThread@ reader SS5's @fpsRef@ row names — exactly the "a later
--   migration adds them when it has a real consumer" case SS7.2
--   anticipated.
--
--   Like the other capability modules, this one imports only the
--   narrow slice of @Engine.Core.State@ it needs rather than
--   @EngineEnv(..)@ or a bare import, so it is not itself a
--   full-@EngineEnv@-access consumer under
--   @tools/engine_env_capability_audit.py@'s ratchet.
module Engine.Core.Capability.RenderView
  ( RenderViewCapability(..)
  , toRenderViewCapability
  ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef)
import Engine.Asset.Handle (TextureHandle)
import Engine.Asset.Types (AssetPool)
import Engine.Asset.TextureNameRegistry (TextureNameRegistry)
import Engine.Core.Queue as Q
import Engine.Graphics.Camera (Camera2D)
import Engine.Graphics.Config (TextureFilter, VideoConfig)
import Engine.Graphics.Font.Data (FontCache)
import Engine.Graphics.Types (ScreenshotRequest)
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem)
import Engine.Core.State
  ( EngineEnv
  , videoConfigRef, windowSizeRef, windowPosRef, framebufferSizeRef
  , framebufferMinimizeGenRef, pixelSnapRef
  , fpsRef, textureFilterRef, assetPoolRef, textureNameRegistryRef, fontCacheRef
  , textureSystemRef, textureSizeRef, cameraRef, screenshotRequestQueue
  , maxImageDimensionRef
  )

-- | The worker-safe slice of @render-gpu-asset@: window\/framebuffer
--   geometry, the measured frame rate, the world camera, the
--   asset\/texture-name\/texture-size registries, the bindless texture
--   system, the font cache, the video settings a Lua settings call
--   writes, and the screenshot request queue a Lua verb enqueues onto.
--
--   Every field here is a container SS5 records a @WorldThread@,
--   @LuaThread@ or @InputThread@ reader or writer for. Writes stay
--   exactly as SS5 classifies them — this record grants no new write
--   authority, it only removes the ability to reach fields a worker
--   thread has no business touching.
data RenderViewCapability = RenderViewCapability
  { rvVideoConfigRef         ∷ IORef VideoConfig
  , rvWindowSizeRef          ∷ IORef (Int, Int)
  , rvWindowPosRef           ∷ IORef (Int, Int)
  , rvFramebufferSizeRef     ∷ IORef (Int, Int)
  , rvFramebufferMinimizeGenRef ∷ IORef Word64
  , rvPixelSnapRef           ∷ IORef Bool
  , rvFpsRef                 ∷ IORef Double
  , rvTextureFilterRef       ∷ IORef TextureFilter
  , rvAssetPoolRef           ∷ IORef AssetPool
  , rvTextureNameRegistryRef ∷ IORef TextureNameRegistry
  , rvFontCacheRef           ∷ IORef FontCache
  , rvTextureSystemRef       ∷ IORef (Maybe BindlessTextureSystem)
  , rvTextureSizeRef         ∷ IORef (HM.HashMap TextureHandle (Int, Int))
  , rvMaxImageDimensionRef   ∷ IORef (Maybe Int)
    -- ^ Read-only here: the device's real @maxImageDimension2D@, which
    --   the world thread needs before it generates a world's zoom-map
    --   pixels (#2020). This record is how it reaches that thread
    --   WITHOUT reaching @vulkanPDevice@, which lives in the
    --   main-render-private @GraphicsState@.
  , rvCameraRef              ∷ IORef Camera2D
  , rvScreenshotRequestQueue ∷ Q.Queue ScreenshotRequest
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied, and
--   nothing is routed through
--   'Engine.Core.Capability.Render.RenderCapability'.
toRenderViewCapability ∷ EngineEnv → RenderViewCapability
toRenderViewCapability env = RenderViewCapability
  { rvVideoConfigRef         = videoConfigRef env
  , rvWindowSizeRef          = windowSizeRef env
  , rvWindowPosRef           = windowPosRef env
  , rvFramebufferSizeRef     = framebufferSizeRef env
  , rvFramebufferMinimizeGenRef = framebufferMinimizeGenRef env
  , rvPixelSnapRef           = pixelSnapRef env
  , rvFpsRef                 = fpsRef env
  , rvTextureFilterRef       = textureFilterRef env
  , rvAssetPoolRef           = assetPoolRef env
  , rvTextureNameRegistryRef = textureNameRegistryRef env
  , rvFontCacheRef           = fontCacheRef env
  , rvTextureSystemRef       = textureSystemRef env
  , rvTextureSizeRef         = textureSizeRef env
  , rvMaxImageDimensionRef   = maxImageDimensionRef env
  , rvCameraRef              = cameraRef env
  , rvScreenshotRequestQueue = screenshotRequestQueue env
  }
