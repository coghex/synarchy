module World.Texture.Types where

import UPrelude

data WorldTextureType
    -- Structural (no material ID)
    = OceanTexture
    | GlacierTexture
    | IceTexture
    | LavaTexture
    | BlankTexture
    | NoTexture
    -- Facemaps (terrain — includes side faces)
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
    -- Side face maps (left/right only — no top face)
    | SideFaceMapLeft
    | SideFaceMapRight
    -- Vegetation facemaps (top face only — no side faces)
    | VegFaceMap
    | VegSlopeFaceMapN
    | VegSlopeFaceMapE
    | VegSlopeFaceMapNE
    | VegSlopeFaceMapS
    | VegSlopeFaceMapNS
    | VegSlopeFaceMapES
    | VegSlopeFaceMapNES
    | VegSlopeFaceMapW
    | VegSlopeFaceMapNW
    | VegSlopeFaceMapEW
    | VegSlopeFaceMapNEW
    | VegSlopeFaceMapSW
    | VegSlopeFaceMapNSW
    | VegSlopeFaceMapESW
    | VegSlopeFaceMapNESW
    -- Material textures: layer + material ID
    | MatTileTexture  !Word8
    | MatZoomTexture  !Word8
    | MatBgTexture    !Word8
    -- Vegetation textures: vegId → tile handle
    | VegTileTexture  !Word8
    -- Exact fluid masks (runtime handles, not serialized)
    | FluidLevelFaceMap1
    | FluidLevelFaceMap2
    | FluidLevelFaceMap3
    | FluidLevelFaceMap4
    | FluidLevelFaceMap5
    | FluidLevelFaceMap6
    | FluidLevelFaceMap7
    | FluidLevelFaceMap8
    deriving (Show, Eq)
