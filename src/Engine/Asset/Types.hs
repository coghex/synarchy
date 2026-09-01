{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Engine.Asset.Types where

import UPrelude
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef)
import Vulkan.Core10
import Engine.Asset.Base (AssetId, AssetStatus)
import Engine.Asset.Handle
import Engine.Graphics.Vulkan.Base (TextureInfo)
import Engine.Graphics.Vulkan.Texture.Policy (TextureCacheKey(..))

-- | Central registry of every loaded texture and font, plus the
--   'IORef' counters used for atomic handle\/ID generation
data AssetPool = AssetPool
  { apTextureAtlases    ∷ Map.Map AssetId TextureAtlas
  , apFonts             ∷ Map.Map AssetId Font
  , apAssetPaths        ∷ Map.Map TextureCacheKey AssetId
    -- ^ The upload path's canonical-slot cache: one entry per
    --   @(path, upload policy)@ pair (#2075).
    --
    --   The POLICY is half the key because a slot's sampler is fixed by
    --   whichever upload registered it, so one entry per path cannot
    --   describe a texture both the UI and the world draw. Reusing an
    --   entry across that boundary hands the new handle the wrong
    --   filtering in both directions, so the lookup is policy-scoped
    --   and each policy keeps its own reusable canonical.
    --
    --   Not an enumeration of loaded FILES: @engine.getLoadedTexturePaths@
    --   still reports one entry per distinct path
    --   ('Engine.Scripting.Lua.API.Graphics'), which is what
    --   @tools\/preview_probe.py@ checks against its allowlist.
  , apNextAssetId       ∷ IORef Int
  , apNextTextureHandle ∷ IORef Int
  , apNextFontHandle    ∷ IORef Int
  , apTextureHandles    ∷ IORef (Map.Map TextureHandle (AssetState AssetId))
  , apFontHandles       ∷ IORef (Map.Map FontHandle (AssetState AssetId))
  , apHandlesSpentReported ∷ IORef Bool
    -- ^ Has the ONE report that 'apNextTextureHandle' has run past the
    --   shader's handle→slot table already been claimed (#1699)?
    --
    --   The counter is monotonic and nothing resets it, so exhaustion
    --   is permanent for the rest of the process: a per-frame consumer
    --   that restated it would print the same line forever. Lives here
    --   because the counter it describes does, which is also what makes
    --   the report process-wide rather than once per world or per
    --   subsystem. Never persisted — no save has a texture handle in it.
  }

data GlyphInfo = GlyphInfo
  { giUVRect    ∷ (Float, Float, Float, Float)  -- ^ UV coordinates (u0, v0, u1, v1) in atlas
  , giSize      ∷ (Float, Float)                -- ^ Glyph dimensions (width, height) in pixels
  , giBearing   ∷ (Float, Float)                -- ^ Offset from baseline (x, y)
  , giAdvance   ∷ Float                         -- ^ Horizontal advance to next glyph
  } deriving (Show, Eq)

defaultAssetPool ∷ IO AssetPool
defaultAssetPool = do
  nextAssetIdRef ← newIORef 0
  -- Texture ids start at 'firstAllocatableTextureHandle', never 0:
  -- handle 0 is the reserved missing-texture sentinel (#1696).
  nextTextureHandleRef ← newIORef firstAllocatableTextureHandle
  nextFontHandleRef ← newIORef 0
  textureHandlesRef ← newIORef Map.empty
  fontHandlesRef ← newIORef Map.empty
  handlesSpentReportedRef ← newIORef False

  pure $ AssetPool
    { apTextureAtlases = Map.empty
    , apFonts          = Map.empty
    , apAssetPaths     = Map.empty
    , apNextAssetId    = nextAssetIdRef
    , apNextTextureHandle = nextTextureHandleRef
    , apNextFontHandle    = nextFontHandleRef
    , apTextureHandles    = textureHandlesRef
    , apFontHandles       = fontHandlesRef
    , apHandlesSpentReported = handlesSpentReportedRef
    }

data AtlasMetadata = AtlasMetadata
  { amDimensions    ∷ (Word32, Word32)  -- ^ Width and height in pixels
  , amFormat        ∷ Format
  , amSubTextures   ∷ Map.Map Text SubTextureInfo
  } deriving (Show)

data SubTextureInfo = SubTextureInfo
  { stiPosition     ∷ (Float, Float)    -- ^ (x, y) offset in atlas
  , stiDimensions   ∷ (Float, Float)    -- ^ (width, height) in atlas
  , stiRotated      ∷ Bool              -- ^ Whether the sub-texture is rotated 90°
  } deriving (Show)

data TextureAtlas = TextureAtlas
  { taId           ∷ AssetId
  , taName         ∷ Text
  , taPath         ∷ Text
  , taMetadata     ∷ AtlasMetadata
  , taInfo         ∷ Maybe TextureInfo   -- ^ Vulkan image\/view\/sampler; 'Nothing' until loaded
  , taRefCount     ∷ Word32
  , taCleanup      ∷ Maybe (IO ())       -- ^ Destroy Vulkan resources on unload
  , taBindlessSlot ∷ Maybe Word32        -- ^ Index into the bindless descriptor array
  , taTextureHandle ∷ TextureHandle      -- ^ Handle for bindless lookup
  }

data Font = Font
  { fId         ∷ AssetId
  , fName       ∷ Text
  , fPath       ∷ Text
  , fSize       ∷ Word32                   -- ^ Rasterisation size in pixels
  , fStatus     ∷ AssetStatus
  , fAtlasId    ∷ Maybe AssetId            -- ^ Backing texture atlas with glyph bitmaps
  , fGlyphMap   ∷ Map.Map Char GlyphInfo   -- ^ Character → glyph metrics lookup
  , fRefCount   ∷ Word32
  , fCleanup    ∷ Maybe (IO ())
  }
instance Show Font where
  show f = "Font { fId = " <> show (fId f)
         <> ", fName = " <> show (fName f)
         <> ", fPath = " <> show (fPath f)
         <> ", fSize = " <> show (fSize f)
         <> ", fStatus = " <> show (fStatus f)
         <> ", fAtlasId = " <> show (fAtlasId f)
         <> ", fGlyphMap = <" <> show (Map.size (fGlyphMap f)) <> " glyphs>"
         <> ", fRefCount = " <> show (fRefCount f)
         <> ", fCleanup = "
         <> (if isJust (fCleanup f) then "<present>" else "<absent>")
         <> " }"
