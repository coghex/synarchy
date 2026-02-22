module Engine.Scripting.Lua.Material
    ( parseTextureType
    ) where

import UPrelude
import World.Command.Types (WorldTextureType(..))

parseTextureType ∷ Text → WorldTextureType
parseTextureType "granite"        = GraniteTexture
parseTextureType "diorite"        = DioriteTexture
parseTextureType "gabbro"         = GabbroTexture
parseTextureType "glacier"        = GlacierTexture
parseTextureType "ocean"          = OceanTexture
parseTextureType "lava"           = LavaTexture
parseTextureType "blank"          = BlankTexture
parseTextureType "notexture"      = NoTexture
parseTextureType "iso_facemap"             = IsoFaceMap
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
parseTextureType "nofacemap"               = NoFaceMap
parseTextureType "zoom_granite"   = ZoomGraniteTexture
parseTextureType "zoom_diorite"   = ZoomDioriteTexture
parseTextureType "zoom_gabbro"    = ZoomGabbroTexture
parseTextureType "zoom_ocean"     = ZoomOceanTexture
parseTextureType "zoom_glacier"   = ZoomGlacierTexture
parseTextureType "zoom_lava"      = ZoomLavaTexture
parseTextureType "zoom_sandstone" = ZoomSandstoneTexture
parseTextureType "zoom_limestone" = ZoomLimestoneTexture
parseTextureType "zoom_shale"     = ZoomShaleTexture
parseTextureType "zoom_iron"      = ZoomIronTexture
parseTextureType "zoom_olivine"   = ZoomOlivineTexture
parseTextureType "zoom_pyroxene"  = ZoomPyroxeneTexture
parseTextureType "zoom_feldspar"  = ZoomFeldsparTexture
parseTextureType "bg_granite"     = BgGraniteTexture
parseTextureType "bg_diorite"     = BgDioriteTexture
parseTextureType "bg_gabbro"      = BgGabbroTexture
parseTextureType "bg_ocean"       = BgOceanTexture
parseTextureType "bg_glacier"     = BgGlacierTexture
parseTextureType "bg_lava"        = BgLavaTexture
parseTextureType "basalt"         = BasaltTexture
parseTextureType "obsidian"       = ObsidianTexture
parseTextureType "sandstone"      = SandstoneTexture
parseTextureType "limestone"      = LimestoneTexture
parseTextureType "shale"          = ShaleTexture
parseTextureType "impactite"      = ImpactiteTexture
parseTextureType "iron"           = IronTexture
parseTextureType "olivine"        = OlivineTexture
parseTextureType "pyroxene"       = PyroxeneTexture
parseTextureType "feldspar"       = FeldsparTexture
parseTextureType "zoom_basalt"    = ZoomBasaltTexture
parseTextureType "zoom_obsidian"  = ZoomObsidianTexture
parseTextureType "zoom_impactite" = ZoomImpactiteTexture
parseTextureType "bg_basalt"      = BgBasaltTexture
parseTextureType "bg_impactite"   = BgImpactiteTexture
parseTextureType "bg_obsidian"    = BgObsidianTexture
parseTextureType "bg_sandstone"   = BgSandstoneTexture
parseTextureType "bg_limestone"   = BgLimestoneTexture
parseTextureType "bg_shale"       = BgShaleTexture
parseTextureType "bg_iron"        = BgIronTexture
parseTextureType "bg_olivine"     = BgOlivineTexture
parseTextureType "bg_pyroxene"    = BgPyroxeneTexture
parseTextureType "bg_feldspar"    = BgFeldsparTexture
parseTextureType _                = NoTexture
