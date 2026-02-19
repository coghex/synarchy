{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Textures
    ( getTileTexture
    , getTileFaceMapTexture
    ) where

import UPrelude
import World.Types
import World.Slope (slopeToFaceMapIndex)
import Engine.Asset.Handle (TextureHandle(..))

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

getTileTexture ∷ WorldTextures → Word8 → TextureHandle
getTileTexture textures 1   = wtGraniteTexture textures
getTileTexture textures 2   = wtDioriteTexture textures
getTileTexture textures 3   = wtGabbroTexture textures
getTileTexture textures 4   = wtBasaltTexture textures
getTileTexture textures 5   = wtObsidianTexture textures
getTileTexture textures 10  = wtSandstoneTexture textures
getTileTexture textures 11  = wtLimestoneTexture textures
getTileTexture textures 12  = wtShaleTexture textures
getTileTexture textures 20  = wtImpactiteTexture textures
getTileTexture textures 30  = wtIronTexture textures
getTileTexture textures 31  = wtOlivineTexture textures
getTileTexture textures 32  = wtPyroxeneTexture textures
getTileTexture textures 33  = wtFeldsparTexture textures
getTileTexture textures 100 = wtLavaTexture textures
getTileTexture textures 250 = wtGlacierTexture textures
getTileTexture textures 255 = wtBlankTexture textures
getTileTexture textures _   = wtNoTexture textures

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
