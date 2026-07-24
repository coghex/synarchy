{-# LANGUAGE UnicodeSyntax #-}
-- | The @render-gpu-asset@ __main-render__ capability record of the
--   @EngineEnv@ capability split (epic #537, issue #891 — E3): exactly
--   the 20 fields 'docs/engineenv_capability_inventory.md' SS5's
--   @render-gpu-asset@ table groups, in that table's own order.
--
--   Follows E1's convention (stated in full in
--   "Engine.Core.Capability.Core"): one record named
--   @\<Name\>Capability@ with fields prefixed by the record's own
--   initials (here @rc@), one total one-way @to\<Name\>Capability@
--   projection, every field the exact same live 'IORef'\/'Q.Queue'
--   handle 'EngineEnv' already carries (never a copy or a
--   reconstruction), and no import of any consumer of this module.
--
--   == The privacy-preserving split this capability adds to the convention
--
--   'rcEngineStateRef' points at the main-render-thread-private
--   'EngineState' (SS3): only @MainRender@ ever reads or writes it, and
--   worker threads must have no interface through which they can
--   construct or inspect a record containing it. Because E1's
--   convention exports each record as @Capability(..)@ — constructor
--   AND accessors — merely /documenting/ that invariant on a
--   worker-visible record would not preserve it.
--
--   So @render-gpu-asset@ is the first capability to be exposed as
--   __two__ interfaces:
--
--   * This module — the @MainRender@ interface, carrying all 20 fields
--     including 'rcEngineStateRef'. Importable only by production
--     modules classified @MainRender@ (see
--     @tools/engine_env_capability_audit.py@'s
--     @RENDER_MAIN_ONLY_MODULES@, which fails the build for any other
--     production importer of this module or mention of
--     'rcEngineStateRef').
--   * "Engine.Core.Capability.RenderView" — a strictly narrower
--     worker-visible projection that never contains
--     'rcEngineStateRef', for the @WorldThread@\/@LuaThread@\/
--     @InputThread@ consumers SS5 documents as legitimate readers (and,
--     for a few fields, writers) of render state.
--
--   __The general rule this establishes for E4-E8:__ a capability with
--   a thread-private field may expose one main-only record plus one or
--   more strictly narrower worker-safe projections. Every such
--   projection still obeys the one-way and shared-live-container
--   rules; the narrower views are projections of @EngineEnv@ in their
--   own right, never of the wider record (nothing reassembles or
--   widens a capability record).
--
--   Like the other capability modules, this one imports only the
--   narrow slice of @Engine.Core.State@ it needs (the bare 'EngineEnv'
--   and 'EngineState'\/'WindowState' types plus the 20 field
--   accessors) rather than @EngineEnv(..)@ or a bare import, so it is
--   not itself a full-@EngineEnv@-access consumer under
--   @tools/engine_env_capability_audit.py@'s ratchet.
module Engine.Core.Capability.Render
  ( RenderCapability(..)
  , toRenderCapability
  ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef)
import Engine.Asset.Handle (TextureHandle)
import Engine.Asset.Types (AssetPool)
import Engine.Asset.YamlTextures (TextureNameRegistry)
import Engine.Core.Queue as Q
import Engine.Graphics.Camera (Camera2D, UICamera)
import Engine.Graphics.Config (TextureFilter, VideoConfig)
import Engine.Graphics.Font.Data (FontCache)
import Engine.Graphics.Types (ScreenshotRequest)
import Engine.Graphics.Vulkan.Sampler.Types (SamplerCache)
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem)
import Engine.Core.State
  ( EngineEnv, EngineState, WindowState
  , engineStateRef, videoConfigRef, windowSizeRef, windowStateRef
  , framebufferSizeRef, fpsRef, brightnessRef, pixelSnapRef
  , textureFilterRef, assetPoolRef, textureNameRegistryRef, fontCacheRef
  , textureSystemRef, samplerCacheRef, textureSizeRef
  , defaultFaceMapSlotRef, cameraRef, uiCameraRef, screenshotRequestQueue
  , nextObjectIdRef
  )

-- | The @render-gpu-asset@ capability as the __main render thread__
--   sees it: the live Vulkan\/GLFW engine state, the window\/video
--   settings, the asset and texture registries, the font cache, the
--   world\/UI cameras, the screenshot request queue, and the Lua
--   object-id allocator. See
--   'docs/engineenv_capability_inventory.md' SS5 @render-gpu-asset@,
--   SS3 (the 'rcEngineStateRef' ownership rule) and SS7.2.
--
--   Non-@MainRender@ consumers take
--   "Engine.Core.Capability.RenderView"'s narrower record instead —
--   this one is unavailable to them by construction, not by
--   convention.
data RenderCapability = RenderCapability
  { -- | __@MainRender@-private__ (SS3): the pointer to the engine's
    --   Vulkan\/GLFW 'EngineState'. Only the main render thread may
    --   read or write it, and no worker-visible interface exposes it —
    --   "Engine.Core.Capability.RenderView" deliberately omits it.
    rcEngineStateRef          ∷ IORef EngineState
  , rcVideoConfigRef          ∷ IORef VideoConfig
  , rcWindowSizeRef           ∷ IORef (Int, Int)
  , rcWindowStateRef          ∷ IORef WindowState
  , rcFramebufferSizeRef      ∷ IORef (Int, Int)
  , rcFpsRef                  ∷ IORef Double
  , rcBrightnessRef           ∷ IORef Int
  , rcPixelSnapRef            ∷ IORef Bool
  , rcTextureFilterRef        ∷ IORef TextureFilter
  , rcAssetPoolRef            ∷ IORef AssetPool
  , rcTextureNameRegistryRef  ∷ IORef TextureNameRegistry
  , rcFontCacheRef            ∷ IORef FontCache
  , rcTextureSystemRef        ∷ IORef (Maybe BindlessTextureSystem)
  , rcSamplerCacheRef         ∷ IORef SamplerCache
  , rcTextureSizeRef          ∷ IORef (HM.HashMap TextureHandle (Int, Int))
  , rcDefaultFaceMapSlotRef   ∷ IORef Word32
  , rcCameraRef               ∷ IORef Camera2D
  , rcUiCameraRef             ∷ IORef UICamera
  , rcScreenshotRequestQueue  ∷ Q.Queue ScreenshotRequest
  , rcNextObjectIdRef         ∷ IORef Word32
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied.
toRenderCapability ∷ EngineEnv → RenderCapability
toRenderCapability env = RenderCapability
  { rcEngineStateRef         = engineStateRef env
  , rcVideoConfigRef         = videoConfigRef env
  , rcWindowSizeRef          = windowSizeRef env
  , rcWindowStateRef         = windowStateRef env
  , rcFramebufferSizeRef     = framebufferSizeRef env
  , rcFpsRef                 = fpsRef env
  , rcBrightnessRef          = brightnessRef env
  , rcPixelSnapRef           = pixelSnapRef env
  , rcTextureFilterRef       = textureFilterRef env
  , rcAssetPoolRef           = assetPoolRef env
  , rcTextureNameRegistryRef = textureNameRegistryRef env
  , rcFontCacheRef           = fontCacheRef env
  , rcTextureSystemRef       = textureSystemRef env
  , rcSamplerCacheRef        = samplerCacheRef env
  , rcTextureSizeRef         = textureSizeRef env
  , rcDefaultFaceMapSlotRef  = defaultFaceMapSlotRef env
  , rcCameraRef              = cameraRef env
  , rcUiCameraRef            = uiCameraRef env
  , rcScreenshotRequestQueue = screenshotRequestQueue env
  , rcNextObjectIdRef        = nextObjectIdRef env
  }
