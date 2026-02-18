{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Textures.Types
    ( WorldTextures(..)
    , defaultWorldTextures
    ) where

import UPrelude
import Engine.Asset.Handle (TextureHandle(..))

data WorldTextures = WorldTextures
    { wtGraniteTexture   ∷ TextureHandle
    , wtGabbroTexture    ∷ TextureHandle
    , wtDioriteTexture   ∷ TextureHandle
    , wtOceanTexture     ∷ TextureHandle
    , wtNoTexture        ∷ TextureHandle
    , wtIsoFaceMap       ∷ TextureHandle
    , wtSlopeFaceMapN    ∷ TextureHandle   -- 1:  N
    , wtSlopeFaceMapE    ∷ TextureHandle   -- 2:  E
    , wtSlopeFaceMapNE   ∷ TextureHandle   -- 3:  N+E
    , wtSlopeFaceMapS    ∷ TextureHandle   -- 4:  S
    , wtSlopeFaceMapNS   ∷ TextureHandle   -- 5:  N+S
    , wtSlopeFaceMapES   ∷ TextureHandle   -- 6:  E+S
    , wtSlopeFaceMapNES  ∷ TextureHandle   -- 7:  N+E+S
    , wtSlopeFaceMapW    ∷ TextureHandle   -- 8:  W
    , wtSlopeFaceMapNW   ∷ TextureHandle   -- 9:  N+W
    , wtSlopeFaceMapEW   ∷ TextureHandle   -- 10: E+W
    , wtSlopeFaceMapNEW  ∷ TextureHandle   -- 11: N+E+W
    , wtSlopeFaceMapSW   ∷ TextureHandle   -- 12: S+W
    , wtSlopeFaceMapNSW  ∷ TextureHandle   -- 13: N+S+W
    , wtSlopeFaceMapESW  ∷ TextureHandle   -- 14: E+S+W
    , wtSlopeFaceMapNESW ∷ TextureHandle   -- 15: N+E+S+W
    , wtNoFaceMap        ∷ TextureHandle
    , wtZoomGranite      ∷ TextureHandle
    , wtZoomGabbro       ∷ TextureHandle
    , wtZoomDiorite      ∷ TextureHandle
    , wtZoomOcean        ∷ TextureHandle
    , wtZoomLava         ∷ TextureHandle
    , wtGlacierTexture   ∷ TextureHandle
    , wtLavaTexture      ∷ TextureHandle
    , wtZoomGlacier      ∷ TextureHandle
    , wtBlankTexture     ∷ TextureHandle
    , wtBgGranite        ∷ TextureHandle
    , wtBgGabbro         ∷ TextureHandle
    , wtBgDiorite        ∷ TextureHandle
    , wtBgOcean          ∷ TextureHandle
    , wtBgGlacier        ∷ TextureHandle
    , wtBgLava           ∷ TextureHandle
    , wtBasaltTexture    ∷ TextureHandle
    , wtObsidianTexture  ∷ TextureHandle
    , wtSandstoneTexture ∷ TextureHandle
    , wtLimestoneTexture ∷ TextureHandle
    , wtShaleTexture     ∷ TextureHandle
    , wtImpactiteTexture ∷ TextureHandle
    , wtIronTexture      ∷ TextureHandle
    , wtOlivineTexture   ∷ TextureHandle
    , wtPyroxeneTexture  ∷ TextureHandle
    , wtFeldsparTexture  ∷ TextureHandle
    , wtZoomBasalt       ∷ TextureHandle
    , wtZoomObsidian     ∷ TextureHandle
    , wtZoomImpactite    ∷ TextureHandle
    , wtBgBasalt         ∷ TextureHandle
    , wtBgImpactite      ∷ TextureHandle
    , wtBgObsidian       ∷ TextureHandle
    , wtZoomSandstone    ∷ TextureHandle
    , wtZoomLimestone    ∷ TextureHandle
    , wtZoomShale        ∷ TextureHandle
    , wtZoomIron         ∷ TextureHandle
    , wtZoomOlivine      ∷ TextureHandle
    , wtZoomPyroxene     ∷ TextureHandle
    , wtZoomFeldspar     ∷ TextureHandle
    , wtBgSandstone      ∷ TextureHandle
    , wtBgLimestone      ∷ TextureHandle
    , wtBgShale          ∷ TextureHandle
    , wtBgIron           ∷ TextureHandle
    , wtBgOlivine        ∷ TextureHandle
    , wtBgPyroxene       ∷ TextureHandle
    , wtBgFeldspar       ∷ TextureHandle
    } deriving (Show, Eq)

defaultWorldTextures ∷ WorldTextures
defaultWorldTextures = WorldTextures
    { wtGraniteTexture  = TextureHandle 0
    , wtGabbroTexture   = TextureHandle 0
    , wtDioriteTexture  = TextureHandle 0
    , wtOceanTexture    = TextureHandle 0
    , wtNoTexture       = TextureHandle 0
    , wtIsoFaceMap      = TextureHandle 0
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
    , wtNoFaceMap       = TextureHandle 0
    , wtZoomGranite     = TextureHandle 0
    , wtZoomGabbro      = TextureHandle 0
    , wtZoomDiorite     = TextureHandle 0
    , wtZoomOcean       = TextureHandle 0
    , wtGlacierTexture  = TextureHandle 0
    , wtLavaTexture     = TextureHandle 0
    , wtZoomGlacier     = TextureHandle 0
    , wtZoomLava         = TextureHandle 0
    , wtBlankTexture    = TextureHandle 0
    , wtBgGranite       = TextureHandle 0
    , wtBgGabbro        = TextureHandle 0
    , wtBgDiorite       = TextureHandle 0
    , wtBgOcean         = TextureHandle 0
    , wtBgGlacier       = TextureHandle 0
    , wtBgLava         = TextureHandle 0
    , wtBasaltTexture    = TextureHandle 0
    , wtObsidianTexture  = TextureHandle 0
    , wtSandstoneTexture = TextureHandle 0
    , wtLimestoneTexture = TextureHandle 0
    , wtShaleTexture     = TextureHandle 0
    , wtImpactiteTexture = TextureHandle 0
    , wtIronTexture      = TextureHandle 0
    , wtOlivineTexture   = TextureHandle 0
    , wtPyroxeneTexture  = TextureHandle 0
    , wtFeldsparTexture  = TextureHandle 0
    , wtZoomBasalt       = TextureHandle 0
    , wtZoomObsidian     = TextureHandle 0
    , wtZoomImpactite    = TextureHandle 0
    , wtBgBasalt         = TextureHandle 0
    , wtBgImpactite      = TextureHandle 0
    , wtBgObsidian       = TextureHandle 0
    , wtZoomSandstone    = TextureHandle 0
    , wtZoomLimestone    = TextureHandle 0
    , wtZoomShale        = TextureHandle 0
    , wtZoomIron         = TextureHandle 0
    , wtZoomOlivine      = TextureHandle 0
    , wtZoomPyroxene     = TextureHandle 0
    , wtZoomFeldspar     = TextureHandle 0
    , wtBgSandstone      = TextureHandle 0
    , wtBgLimestone      = TextureHandle 0
    , wtBgShale          = TextureHandle 0
    , wtBgIron           = TextureHandle 0
    , wtBgOlivine        = TextureHandle 0
    , wtBgPyroxene       = TextureHandle 0
    , wtBgFeldspar       = TextureHandle 0
    }
