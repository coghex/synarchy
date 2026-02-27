{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Textures
    ( getTileTexture
    , getTileFaceMapTexture
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Types
import World.Slope (slopeToFaceMapIndex)
import Engine.Asset.Handle (TextureHandle(..))

getTileTexture ∷ WorldTextures → Word8 → TextureHandle
getTileTexture _        0 = TextureHandle 0
getTileTexture textures matId =
    case HM.lookup matId (wtTileTextures textures) of
        Just h  → h
        Nothing → wtNoTexture textures

getTileFaceMapTexture ∷ WorldTextures → Word8 → Word8 → TextureHandle
getTileFaceMapTexture textures _mat slopeId =
    case slopeToFaceMapIndex slopeId of
        0  → wtIsoFaceMap textures
        1  → wtSlopeFaceMapN textures
        2  → wtSlopeFaceMapE textures
        3  → wtSlopeFaceMapNE textures
        4  → wtSlopeFaceMapS textures
        5  → wtSlopeFaceMapNS textures
        6  → wtSlopeFaceMapES textures
        7  → wtSlopeFaceMapNES textures
        8  → wtSlopeFaceMapW textures
        9  → wtSlopeFaceMapNW textures
        10 → wtSlopeFaceMapEW textures
        11 → wtSlopeFaceMapNEW textures
        12 → wtSlopeFaceMapSW textures
        13 → wtSlopeFaceMapNSW textures
        14 → wtSlopeFaceMapESW textures
        15 → wtSlopeFaceMapNESW textures
        _  → wtIsoFaceMap textures
