{-# LANGUAGE Strict #-}
module World.Render.Textures
    ( getTileTexture
    , getTileFaceMapTexture
    , getFluidFaceMapTexture
    , getVegFaceMapTexture
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Types
import World.Slope (slopeToFaceMapIndex)
import World.Fluid.Exact (exactTopLevel)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))

getTileTexture ∷ WorldTextures → Word8 → TextureHandle
getTileTexture _        0 = TextureHandle 0
getTileTexture textures matId =
    case HM.lookup matId (wtTileTextures textures) of
        Just h  → h
        Nothing → wtNoTexture textures

-- | Stored slope bits describe the world. Only mask selection changes with
-- the view: an E mask becomes N/W/S at West/North/East camera facings.
-- Keep invalid IDs invalid so the selectors retain their flat fallback.
viewSlopeId ∷ CameraFacing → Word8 → Word8
viewSlopeId facing bits
    | bits > 15 = bits
    | otherwise = ((bits `shiftR` turns) ⌄ (bits `shiftL` (4 - turns))) ⌃ 15
  where
    turns = case facing of
        FaceSouth → 0
        FaceWest → 1
        FaceNorth → 2
        FaceEast → 3

getTileFaceMapTexture ∷ WorldTextures → CameraFacing → Word8 → Word8 → TextureHandle
getTileFaceMapTexture textures facing _mat slopeId =
    case slopeToFaceMapIndex (viewSlopeId facing slopeId) of
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

getVegFaceMapTexture ∷ WorldTextures → CameraFacing → Word8 → TextureHandle
getVegFaceMapTexture textures facing slopeId =
    case slopeToFaceMapIndex (viewSlopeId facing slopeId) of
        0  → wtVegFaceMap textures
        1  → wtVegSlopeFaceMapN textures
        2  → wtVegSlopeFaceMapE textures
        3  → wtVegSlopeFaceMapNE textures
        4  → wtVegSlopeFaceMapS textures
        5  → wtVegSlopeFaceMapNS textures
        6  → wtVegSlopeFaceMapES textures
        7  → wtVegSlopeFaceMapNES textures
        8  → wtVegSlopeFaceMapW textures
        9  → wtVegSlopeFaceMapNW textures
        10 → wtVegSlopeFaceMapEW textures
        11 → wtVegSlopeFaceMapNEW textures
        12 → wtVegSlopeFaceMapSW textures
        13 → wtVegSlopeFaceMapNSW textures
        14 → wtVegSlopeFaceMapESW textures
        15 → wtVegSlopeFaceMapNESW textures
        _  → wtVegFaceMap textures

-- | Select by the signed exact plane, including full levels at multiples.
getFluidFaceMapTexture ∷ WorldTextures → Int → TextureHandle
getFluidFaceMapTexture textures exactSurface = case exactTopLevel exactSurface of
    1 → wtFluidLevelFaceMap1 textures
    2 → wtFluidLevelFaceMap2 textures
    3 → wtFluidLevelFaceMap3 textures
    4 → wtFluidLevelFaceMap4 textures
    5 → wtFluidLevelFaceMap5 textures
    6 → wtFluidLevelFaceMap6 textures
    7 → wtFluidLevelFaceMap7 textures
    _ → wtFluidLevelFaceMap8 textures
