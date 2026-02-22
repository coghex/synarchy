{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Command.Types
    ( WorldTextureType(..)
    , WorldCommand(..)
    ) where

import UPrelude
import Engine.Asset.Handle (TextureHandle(..))
import World.Page.Types (WorldPageId(..))
import World.Render.Zoom.Types (ZoomMapMode(..))

data WorldTextureType
    = GraniteTexture
    | DioriteTexture
    | GabbroTexture
    | NoTexture
    | OceanTexture
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
    | ZoomGraniteTexture
    | ZoomDioriteTexture
    | ZoomGabbroTexture
    | ZoomOceanTexture
    | GlacierTexture
    | LavaTexture
    | ZoomGlacierTexture
    | ZoomLavaTexture
    | BlankTexture
    | BgGraniteTexture
    | BgGabbroTexture
    | BgDioriteTexture
    | BgOceanTexture
    | BgGlacierTexture
    | BgLavaTexture
    | BasaltTexture
    | ObsidianTexture
    | SandstoneTexture
    | LimestoneTexture
    | ShaleTexture
    | ImpactiteTexture
    | IronTexture
    | OlivineTexture
    | PyroxeneTexture
    | FeldsparTexture
    | ZoomBasaltTexture
    | ZoomObsidianTexture
    | ZoomImpactiteTexture
    | BgBasaltTexture
    | BgImpactiteTexture
    | BgObsidianTexture
    | ZoomSandstoneTexture
    | ZoomLimestoneTexture
    | ZoomShaleTexture
    | ZoomIronTexture
    | ZoomOlivineTexture
    | ZoomPyroxeneTexture
    | ZoomFeldsparTexture
    | BgSandstoneTexture
    | BgLimestoneTexture
    | BgShaleTexture
    | BgIronTexture
    | BgOlivineTexture
    | BgPyroxeneTexture
    | BgFeldsparTexture
    deriving (Show, Eq)

data WorldCommand
    = WorldInit WorldPageId Word64 Int Int
    | WorldShow WorldPageId
    | WorldHide WorldPageId
    | WorldTick Double
    | WorldSetTexture WorldPageId WorldTextureType TextureHandle
    | WorldSetCamera WorldPageId Float Float
    | WorldSetTime WorldPageId Int Int         -- ^ hour minute
    | WorldSetDate WorldPageId Int Int Int     -- ^ year month day
    | WorldSetTimeScale WorldPageId Float      -- ^ game-minutes per real-second
    | WorldSetMapMode WorldPageId ZoomMapMode      -- ^ map mode
    | WorldSetZoomCursorHover WorldPageId Int Int
    | WorldSetZoomCursorSelectTexture WorldPageId TextureHandle
    | WorldSetZoomCursorHoverTexture WorldPageId TextureHandle
    deriving (Show, Eq)
