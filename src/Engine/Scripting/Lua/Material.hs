module Engine.Scripting.Lua.Material
    ( parseTextureType
    ) where

import UPrelude
import qualified Data.Text as T
import Text.Read (readMaybe)
import World.Command.Types (WorldTextureType(..))

parseTextureType ∷ Text → WorldTextureType
-- Structural
parseTextureType "ocean"          = OceanTexture
parseTextureType "glacier"        = GlacierTexture
parseTextureType "lava"           = LavaTexture
parseTextureType "blank"          = BlankTexture
parseTextureType "notexture"      = NoTexture
-- Facemaps
parseTextureType "iso_facemap"              = IsoFaceMap
parseTextureType "iso_slope_facemap_n"      = SlopeFaceMapN
parseTextureType "iso_slope_facemap_e"      = SlopeFaceMapE
parseTextureType "iso_slope_facemap_ne"     = SlopeFaceMapNE
parseTextureType "iso_slope_facemap_s"      = SlopeFaceMapS
parseTextureType "iso_slope_facemap_ns"     = SlopeFaceMapNS
parseTextureType "iso_slope_facemap_es"     = SlopeFaceMapES
parseTextureType "iso_slope_facemap_nes"    = SlopeFaceMapNES
parseTextureType "iso_slope_facemap_w"      = SlopeFaceMapW
parseTextureType "iso_slope_facemap_nw"     = SlopeFaceMapNW
parseTextureType "iso_slope_facemap_ew"     = SlopeFaceMapEW
parseTextureType "iso_slope_facemap_new"    = SlopeFaceMapNEW
parseTextureType "iso_slope_facemap_sw"     = SlopeFaceMapSW
parseTextureType "iso_slope_facemap_nsw"    = SlopeFaceMapNSW
parseTextureType "iso_slope_facemap_esw"    = SlopeFaceMapESW
parseTextureType "iso_slope_facemap_nesw"   = SlopeFaceMapNESW
parseTextureType "nofacemap"                = NoFaceMap
-- Material textures: "mat_tile_N", "mat_zoom_N", "mat_bg_N"
parseTextureType t
    | Just suffix ← T.stripPrefix "mat_tile_" t
    , Just n ← readMaybe (T.unpack suffix)
    = MatTileTexture n
    | Just suffix ← T.stripPrefix "mat_zoom_" t
    , Just n ← readMaybe (T.unpack suffix)
    = MatZoomTexture n
    | Just suffix ← T.stripPrefix "mat_bg_" t
    , Just n ← readMaybe (T.unpack suffix)
    = MatBgTexture n
    | otherwise = NoTexture
