{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Material → TextureHandle dispatch for the zoom and background layers.
module World.Render.Zoom.Textures
    ( getZoomTexture
    , getBgTexture
    ) where

import UPrelude
import Engine.Asset.Handle (TextureHandle(..))
import World.Types

-----------------------------------------------------------
-- Texture Pickers
-----------------------------------------------------------

getZoomTexture ∷ WorldTextures → Word8 → Int → TextureHandle
getZoomTexture textures 250 _  = wtZoomGlacier textures
getZoomTexture textures 0   _  = wtZoomOcean textures
getZoomTexture textures 1   _  = wtZoomGranite textures
getZoomTexture textures 2   _  = wtZoomDiorite textures
getZoomTexture textures 3   _  = wtZoomGabbro textures
getZoomTexture textures 4   _  = wtZoomBasalt textures
getZoomTexture textures 5   _  = wtZoomObsidian textures
getZoomTexture textures 10  _  = wtZoomSandstone textures
getZoomTexture textures 11  _  = wtZoomLimestone textures
getZoomTexture textures 12  _  = wtZoomShale textures
getZoomTexture textures 20  _  = wtZoomImpactite textures
getZoomTexture textures 30  _  = wtZoomIron textures
getZoomTexture textures 31  _  = wtZoomOlivine textures
getZoomTexture textures 32  _  = wtZoomPyroxene textures
getZoomTexture textures 33  _  = wtZoomFeldspar textures
getZoomTexture textures 100 _  = wtZoomLava textures
getZoomTexture textures _   _  = wtZoomGranite textures

getBgTexture ∷ WorldTextures → Word8 → Int → TextureHandle
getBgTexture textures 250 _  = wtBgGlacier textures
getBgTexture textures 0   _  = wtBgOcean textures
getBgTexture textures 1   _  = wtBgGranite textures
getBgTexture textures 2   _  = wtBgDiorite textures
getBgTexture textures 3   _  = wtBgGabbro textures
getBgTexture textures 4   _  = wtBgBasalt textures
getBgTexture textures 5   _  = wtBgObsidian textures
getBgTexture textures 10  _  = wtBgSandstone textures
getBgTexture textures 11  _  = wtBgLimestone textures
getBgTexture textures 12  _  = wtBgShale textures
getBgTexture textures 20  _  = wtBgImpactite textures
getBgTexture textures 30  _  = wtBgIron textures
getBgTexture textures 31  _  = wtBgOlivine textures
getBgTexture textures 32  _  = wtBgPyroxene textures
getBgTexture textures 33  _  = wtBgFeldspar textures
getBgTexture textures 100 _  = wtBgLava textures
getBgTexture textures _   _  = wtBgGranite textures
