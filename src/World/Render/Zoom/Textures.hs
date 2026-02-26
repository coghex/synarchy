{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Zoom.Textures
    ( getZoomTexture
    , getBgTexture
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))
import World.Types

getZoomTexture ∷ WorldTextures → Word8 → Int → TextureHandle
getZoomTexture textures matId _ =
    case HM.lookup matId (wtZoomTextures textures) of
        Just h  → h
        Nothing → case HM.lookup 1 (wtZoomTextures textures) of
            Just h  → h                    -- fallback to granite
            Nothing → wtNoTexture textures -- nothing loaded yet

getBgTexture ∷ WorldTextures → Word8 → Int → TextureHandle
getBgTexture textures matId _ =
    case HM.lookup matId (wtBgTextures textures) of
        Just h  → h
        Nothing → case HM.lookup 1 (wtBgTextures textures) of
            Just h  → h
            Nothing → wtNoTexture textures
