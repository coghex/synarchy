{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Textures.Types
    ( WorldTextures(..)
    , defaultWorldTextures
    , TextureLayer(..)
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))

-- | Which layer a material texture belongs to
data TextureLayer
    = TileLayer    -- ^ world view (isometric tiles)
    | ZoomLayer    -- ^ zoom map overlay
    | BgLayer      -- ^ background / minimap
    deriving (Show, Eq)

data WorldTextures = WorldTextures
    { -- Structural (fixed, always needed)
      wtOceanTexture     ∷ TextureHandle
    , wtNoTexture        ∷ TextureHandle
    , wtBlankTexture     ∷ TextureHandle
    , wtGlacierTexture   ∷ TextureHandle
    , wtLavaTexture      ∷ TextureHandle
      -- Facemaps (fixed set of 16 + flat)
    , wtIsoFaceMap       ∷ TextureHandle
    , wtSlopeFaceMapN    ∷ TextureHandle
    , wtSlopeFaceMapE    ∷ TextureHandle
    , wtSlopeFaceMapNE   ∷ TextureHandle
    , wtSlopeFaceMapS    ∷ TextureHandle
    , wtSlopeFaceMapNS   ∷ TextureHandle
    , wtSlopeFaceMapES   ∷ TextureHandle
    , wtSlopeFaceMapNES  ∷ TextureHandle
    , wtSlopeFaceMapW    ∷ TextureHandle
    , wtSlopeFaceMapNW   ∷ TextureHandle
    , wtSlopeFaceMapEW   ∷ TextureHandle
    , wtSlopeFaceMapNEW  ∷ TextureHandle
    , wtSlopeFaceMapSW   ∷ TextureHandle
    , wtSlopeFaceMapNSW  ∷ TextureHandle
    , wtSlopeFaceMapESW  ∷ TextureHandle
    , wtSlopeFaceMapNESW ∷ TextureHandle
    , wtNoFaceMap        ∷ TextureHandle
      -- Vegetation facemaps (top-face only, no side faces)
    , wtVegFaceMap        ∷ TextureHandle   -- flat
    , wtVegSlopeFaceMapN  ∷ TextureHandle
    , wtVegSlopeFaceMapE  ∷ TextureHandle
    , wtVegSlopeFaceMapNE ∷ TextureHandle
    , wtVegSlopeFaceMapS  ∷ TextureHandle
    , wtVegSlopeFaceMapNS ∷ TextureHandle
    , wtVegSlopeFaceMapES ∷ TextureHandle
    , wtVegSlopeFaceMapNES ∷ TextureHandle
    , wtVegSlopeFaceMapW  ∷ TextureHandle
    , wtVegSlopeFaceMapNW ∷ TextureHandle
    , wtVegSlopeFaceMapEW ∷ TextureHandle
    , wtVegSlopeFaceMapNEW ∷ TextureHandle
    , wtVegSlopeFaceMapSW  ∷ TextureHandle
    , wtVegSlopeFaceMapNSW ∷ TextureHandle
    , wtVegSlopeFaceMapESW ∷ TextureHandle
    , wtVegSlopeFaceMapNESW ∷ TextureHandle
      -- Material texture maps: MaterialId (Word8) → TextureHandle
      -- Lua registers each material into the appropriate layer.
      -- Lookup is O(1) amortized via HashMap.
    , wtTileTextures     ∷ HM.HashMap Word8 TextureHandle
    , wtZoomTextures     ∷ HM.HashMap Word8 TextureHandle
    , wtBgTextures       ∷ HM.HashMap Word8 TextureHandle
    , wtVegTextures      ∷ HM.HashMap Word8 TextureHandle
    } deriving (Show, Eq)

defaultWorldTextures ∷ WorldTextures
defaultWorldTextures = WorldTextures
    { wtOceanTexture     = TextureHandle 0
    , wtNoTexture        = TextureHandle 0
    , wtBlankTexture     = TextureHandle 0
    , wtGlacierTexture   = TextureHandle 0
    , wtLavaTexture      = TextureHandle 0
    , wtIsoFaceMap       = TextureHandle 0
    , wtSlopeFaceMapN    = TextureHandle 0
    , wtSlopeFaceMapE    = TextureHandle 0
    , wtSlopeFaceMapNE   = TextureHandle 0
    , wtSlopeFaceMapS    = TextureHandle 0
    , wtSlopeFaceMapNS   = TextureHandle 0
    , wtSlopeFaceMapES   = TextureHandle 0
    , wtSlopeFaceMapNES  = TextureHandle 0
    , wtSlopeFaceMapW    = TextureHandle 0
    , wtSlopeFaceMapNW   = TextureHandle 0
    , wtSlopeFaceMapEW   = TextureHandle 0
    , wtSlopeFaceMapNEW  = TextureHandle 0
    , wtSlopeFaceMapSW   = TextureHandle 0
    , wtSlopeFaceMapNSW  = TextureHandle 0
    , wtSlopeFaceMapESW  = TextureHandle 0
    , wtSlopeFaceMapNESW = TextureHandle 0
    , wtNoFaceMap        = TextureHandle 0
    , wtVegFaceMap        = TextureHandle 0
    , wtVegSlopeFaceMapN  = TextureHandle 0
    , wtVegSlopeFaceMapE  = TextureHandle 0
    , wtVegSlopeFaceMapNE = TextureHandle 0
    , wtVegSlopeFaceMapS  = TextureHandle 0
    , wtVegSlopeFaceMapNS = TextureHandle 0
    , wtVegSlopeFaceMapES = TextureHandle 0
    , wtVegSlopeFaceMapNES = TextureHandle 0
    , wtVegSlopeFaceMapW  = TextureHandle 0
    , wtVegSlopeFaceMapNW = TextureHandle 0
    , wtVegSlopeFaceMapEW = TextureHandle 0
    , wtVegSlopeFaceMapNEW = TextureHandle 0
    , wtVegSlopeFaceMapSW  = TextureHandle 0
    , wtVegSlopeFaceMapNSW = TextureHandle 0
    , wtVegSlopeFaceMapESW = TextureHandle 0
    , wtVegSlopeFaceMapNESW = TextureHandle 0
    , wtTileTextures     = HM.empty
    , wtZoomTextures     = HM.empty
    , wtBgTextures       = HM.empty
    , wtVegTextures      = HM.empty
    }
