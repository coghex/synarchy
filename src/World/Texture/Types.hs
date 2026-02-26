module World.Texture.Types where

import UPrelude

data WorldTextureType
    -- Structural (no material ID)
    = OceanTexture
    | GlacierTexture
    | LavaTexture
    | BlankTexture
    | NoTexture
    -- Facemaps
    | IsoFaceMap
    | SlopeFaceMapN
    | SlopeFaceMapE
    | SlopeFaceMapNE
    | SlopeFaceMapS
    | SlopeFaceMapNS
    | SlopeFaceMapES
    | SlopeFaceMapNES
    | SlopeFaceMapW
    | SlopeFaceMapNW
    | SlopeFaceMapEW
    | SlopeFaceMapNEW
    | SlopeFaceMapSW
    | SlopeFaceMapNSW
    | SlopeFaceMapESW
    | SlopeFaceMapNESW
    | NoFaceMap
    -- Material textures: layer + material ID
    | MatTileTexture  !Word8   -- ^ tile layer for material N
    | MatZoomTexture  !Word8   -- ^ zoom layer for material N
    | MatBgTexture    !Word8   -- ^ bg layer for material N
    deriving (Show, Eq)
