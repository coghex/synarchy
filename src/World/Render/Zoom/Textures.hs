{-# LANGUAGE Strict #-}
module World.Render.Zoom.Textures
    ( getZoomTexture
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))
import World.Types

-- * Texture Pickers

getZoomTexture ∷ WorldTextures → Word8 → Int → TextureHandle
getZoomTexture textures 0   _ = wtOceanTexture textures    -- ocean (air matId)
getZoomTexture textures 250 _ = wtGlacierTexture textures  -- glacier
getZoomTexture textures matId _ =
    case HM.lookup matId (wtZoomTextures textures) of
        Just h  → h
        Nothing → case HM.lookup 1 (wtZoomTextures textures) of
            Just h  → h
            Nothing → wtNoTexture textures
