-- World View - UI and HUD for world display
local worldManager = require("scripts.world_manager")

local worldView = {}

worldView.page = nil
worldView.visible = false
worldView.fbW = 0
worldView.fbH = 0

-----------------------------------------------------------
-- Material Table
--
-- Single source of truth for material ID → texture paths.
-- To add a new material, add one row here. No Haskell changes.
--
-- Each entry:
--   id   = MaterialId (Word8) matching Haskell's Material.hs
--   name = human-readable name (for logging)
--   tile = path to isometric tile texture
--   zoom = path to zoom map chunk texture
--   bg   = path to zoom map background texture
--
-- ID Layout:
--   0         Air / Ocean floor
--   1-9       Igneous intrusive
--   10-19     Igneous extrusive
--   20-29     Sedimentary clastic
--   30-39     Sedimentary chemical / organic
--   40-49     Metamorphic
--   50-59     Soils — mineral (clay-sand axis)
--   60-69     Soils — silt & special
--   70-79     Carbonaceous (coal series)
--   80-89     Ores / metals
--   90-99     Impact
--   100-109   Volcanic active
--   110-119   Glacial deposits
--   240-249   Fluids / markers
--   250-255   Special
-----------------------------------------------------------

worldView.materialDefs = {

    ---------------------------------------------------
    -- 1-9: Igneous Intrusive
    ---------------------------------------------------
    { id = 1,   name = "granite",
      tile = "assets/textures/world/granite/granite.png",
      zoom = "assets/textures/world/zoommap/granite_chunk.png",
      bg   = "assets/textures/world/zoommap/granite_chunk_background.png" },
    { id = 2,   name = "diorite",
      tile = "assets/textures/world/diorite/diorite.png",
      zoom = "assets/textures/world/zoommap/diorite_chunk.png",
      bg   = "assets/textures/world/zoommap/diorite_chunk_background.png" },
    { id = 3,   name = "gabbro",
      tile = "assets/textures/world/gabbro/gabbro.png",
      zoom = "assets/textures/world/zoommap/gabbro_chunk.png",
      bg   = "assets/textures/world/zoommap/gabbro_chunk_background.png" },
    { id = 6,   name = "anorthosite",
      tile = "assets/textures/world/anorthosite/anorthosite.png",
      zoom = "assets/textures/world/zoommap/anorthosite_chunk.png",
      bg   = "assets/textures/world/zoommap/anorthosite_chunk_background.png" },
    { id = 7,   name = "peridotite",
      tile = "assets/textures/world/peridotite/peridotite.png",
      zoom = "assets/textures/world/zoommap/peridotite_chunk.png",
      bg   = "assets/textures/world/zoommap/peridotite_chunk_background.png" },
    { id = 8,   name = "pegmatite",
      tile = "assets/textures/world/pegmatite/pegmatite.png",
      zoom = "assets/textures/world/zoommap/pegmatite_chunk.png",
      bg   = "assets/textures/world/zoommap/pegmatite_chunk_background.png" },

    ---------------------------------------------------
    -- 10-19: Igneous Extrusive
    ---------------------------------------------------
    { id = 10,  name = "basalt",
      tile = "assets/textures/world/basalt/basalt.png",
      zoom = "assets/textures/world/zoommap/basalt_chunk.png",
      bg   = "assets/textures/world/zoommap/basalt_chunk_background.png" },
    { id = 11,  name = "obsidian",
      tile = "assets/textures/world/obsidian/obsidian.png",
      zoom = "assets/textures/world/zoommap/obsidian_chunk.png",
      bg   = "assets/textures/world/zoommap/obsidian_chunk_background.png" },
    { id = 12,  name = "rhyolite",
      tile = "assets/textures/world/rhyolite/rhyolite.png",
      zoom = "assets/textures/world/zoommap/rhyolite_chunk.png",
      bg   = "assets/textures/world/zoommap/rhyolite_chunk_background.png" },
    { id = 13,  name = "andesite",
      tile = "assets/textures/world/andesite/andesite.png",
      zoom = "assets/textures/world/zoommap/andesite_chunk.png",
      bg   = "assets/textures/world/zoommap/andesite_chunk_background.png" },
    { id = 14,  name = "tuff",
      tile = "assets/textures/world/tuff/tuff.png",
      zoom = "assets/textures/world/zoommap/tuff_chunk.png",
      bg   = "assets/textures/world/zoommap/tuff_chunk_background.png" },
    { id = 15,  name = "pumice",
      tile = "assets/textures/world/pumice/pumice.png",
      zoom = "assets/textures/world/zoommap/pumice_chunk.png",
      bg   = "assets/textures/world/zoommap/pumice_chunk_background.png" },
    { id = 16,  name = "scoria",
      tile = "assets/textures/world/scoria/scoria.png",
      zoom = "assets/textures/world/zoommap/scoria_chunk.png",
      bg   = "assets/textures/world/zoommap/scoria_chunk_background.png" },

    ---------------------------------------------------
    -- 20-29: Sedimentary Clastic
    ---------------------------------------------------
    { id = 20,  name = "sandstone",
      tile = "assets/textures/world/sandstone/sandstone.png",
      zoom = "assets/textures/world/zoommap/sandstone_chunk.png",
      bg   = "assets/textures/world/zoommap/sandstone_chunk_background.png" },
    { id = 21,  name = "siltstone",
      tile = "assets/textures/world/siltstone/siltstone.png",
      zoom = "assets/textures/world/zoommap/siltstone_chunk.png",
      bg   = "assets/textures/world/zoommap/siltstone_chunk_background.png" },
    { id = 22,  name = "shale",
      tile = "assets/textures/world/shale/shale.png",
      zoom = "assets/textures/world/zoommap/shale_chunk.png",
      bg   = "assets/textures/world/zoommap/shale_chunk_background.png" },
    { id = 23,  name = "conglomerate",
      tile = "assets/textures/world/conglomerate/conglomerate.png",
      zoom = "assets/textures/world/zoommap/conglomerate_chunk.png",
      bg   = "assets/textures/world/zoommap/conglomerate_chunk_background.png" },
    { id = 24,  name = "mudstone",
      tile = "assets/textures/world/mudstone/mudstone.png",
      zoom = "assets/textures/world/zoommap/mudstone_chunk.png",
      bg   = "assets/textures/world/zoommap/mudstone_chunk_background.png" },
    { id = 25,  name = "claystone",
      tile = "assets/textures/world/claystone/claystone.png",
      zoom = "assets/textures/world/zoommap/claystone_chunk.png",
      bg   = "assets/textures/world/zoommap/claystone_chunk_background.png" },

    ---------------------------------------------------
    -- 30-39: Sedimentary Chemical / Organic
    ---------------------------------------------------
    { id = 30,  name = "limestone",
      tile = "assets/textures/world/limestone/limestone.png",
      zoom = "assets/textures/world/zoommap/limestone_chunk.png",
      bg   = "assets/textures/world/zoommap/limestone_chunk_background.png" },
    { id = 31,  name = "chalk",
      tile = "assets/textures/world/chalk/chalk.png",
      zoom = "assets/textures/world/zoommap/chalk_chunk.png",
      bg   = "assets/textures/world/zoommap/chalk_chunk_background.png" },
    { id = 32,  name = "chert",
      tile = "assets/textures/world/chert/chert.png",
      zoom = "assets/textures/world/zoommap/chert_chunk.png",
      bg   = "assets/textures/world/zoommap/chert_chunk_background.png" },
    { id = 33,  name = "rock_salt",
      tile = "assets/textures/world/rock_salt/rock_salt.png",
      zoom = "assets/textures/world/zoommap/rock_salt_chunk.png",
      bg   = "assets/textures/world/zoommap/rock_salt_chunk_background.png" },
    { id = 34,  name = "gypsum",
      tile = "assets/textures/world/gypsum/gypsum.png",
      zoom = "assets/textures/world/zoommap/gypsum_chunk.png",
      bg   = "assets/textures/world/zoommap/gypsum_chunk_background.png" },
    { id = 35,  name = "dolomite",
      tile = "assets/textures/world/dolomite/dolomite.png",
      zoom = "assets/textures/world/zoommap/dolomite_chunk.png",
      bg   = "assets/textures/world/zoommap/dolomite_chunk_background.png" },

    ---------------------------------------------------
    -- 40-49: Metamorphic
    ---------------------------------------------------
    { id = 40,  name = "marble",
      tile = "assets/textures/world/marble/marble.png",
      zoom = "assets/textures/world/zoommap/marble_chunk.png",
      bg   = "assets/textures/world/zoommap/marble_chunk_background.png" },
    { id = 41,  name = "quartzite",
      tile = "assets/textures/world/quartzite/quartzite.png",
      zoom = "assets/textures/world/zoommap/quartzite_chunk.png",
      bg   = "assets/textures/world/zoommap/quartzite_chunk_background.png" },
    { id = 42,  name = "slate",
      tile = "assets/textures/world/slate/slate.png",
      zoom = "assets/textures/world/zoommap/slate_chunk.png",
      bg   = "assets/textures/world/zoommap/slate_chunk_background.png" },
    { id = 43,  name = "schist",
      tile = "assets/textures/world/schist/schist.png",
      zoom = "assets/textures/world/zoommap/schist_chunk.png",
      bg   = "assets/textures/world/zoommap/schist_chunk_background.png" },
    { id = 44,  name = "gneiss",
      tile = "assets/textures/world/gneiss/gneiss.png",
      zoom = "assets/textures/world/zoommap/gneiss_chunk.png",
      bg   = "assets/textures/world/zoommap/gneiss_chunk_background.png" },
    { id = 45,  name = "phyllite",
      tile = "assets/textures/world/phyllite/phyllite.png",
      zoom = "assets/textures/world/zoommap/phyllite_chunk.png",
      bg   = "assets/textures/world/zoommap/phyllite_chunk_background.png" },

    ---------------------------------------------------
    -- 50-59: Soils — Mineral (clay-sand axis)
    ---------------------------------------------------
    { id = 50,  name = "clay",
      tile = "assets/textures/world/clay/clay.png",
      zoom = "assets/textures/world/zoommap/clay_chunk.png",
      bg   = "assets/textures/world/zoommap/clay_chunk_background.png" },
    { id = 51,  name = "sandy_clay",
      tile = "assets/textures/world/sandy_clay/sandy_clay.png",
      zoom = "assets/textures/world/zoommap/sandy_clay_chunk.png",
      bg   = "assets/textures/world/zoommap/sandy_clay_chunk_background.png" },
    { id = 52,  name = "sandy_clay_loam",
      tile = "assets/textures/world/sandy_clay_loam/sandy_clay_loam.png",
      zoom = "assets/textures/world/zoommap/sandy_clay_loam_chunk.png",
      bg   = "assets/textures/world/zoommap/sandy_clay_loam_chunk_background.png" },
    { id = 53,  name = "sandy_loam",
      tile = "assets/textures/world/sandy_loam/sandy_loam.png",
      zoom = "assets/textures/world/zoommap/sandy_loam_chunk.png",
      bg   = "assets/textures/world/zoommap/sandy_loam_chunk_background.png" },
    { id = 54,  name = "loamy_sand",
      tile = "assets/textures/world/loamy_sand/loamy_sand.png",
      zoom = "assets/textures/world/zoommap/loamy_sand_chunk.png",
      bg   = "assets/textures/world/zoommap/loamy_sand_chunk_background.png" },
    { id = 55,  name = "sand",
      tile = "assets/textures/world/sand/sand.png",
      zoom = "assets/textures/world/zoommap/sand_chunk.png",
      bg   = "assets/textures/world/zoommap/sand_chunk_background.png" },
    { id = 56,  name = "loam",
      tile = "assets/textures/world/loam/loam.png",
      zoom = "assets/textures/world/zoommap/loam_chunk.png",
      bg   = "assets/textures/world/zoommap/loam_chunk_background.png" },
    { id = 57,  name = "clay_loam",
      tile = "assets/textures/world/clay_loam/clay_loam.png",
      zoom = "assets/textures/world/zoommap/clay_loam_chunk.png",
      bg   = "assets/textures/world/zoommap/clay_loam_chunk_background.png" },
    { id = 58,  name = "silty_clay",
      tile = "assets/textures/world/silty_clay/silty_clay.png",
      zoom = "assets/textures/world/zoommap/silty_clay_chunk.png",
      bg   = "assets/textures/world/zoommap/silty_clay_chunk_background.png" },
    { id = 59,  name = "silty_clay_loam",
      tile = "assets/textures/world/silty_clay_loam/silty_clay_loam.png",
      zoom = "assets/textures/world/zoommap/silty_clay_loam_chunk.png",
      bg   = "assets/textures/world/zoommap/silty_clay_loam_chunk_background.png" },

    ---------------------------------------------------
    -- 60-69: Soils — Silt & Special
    ---------------------------------------------------
    { id = 60,  name = "silt_loam",
      tile = "assets/textures/world/silt_loam/silt_loam.png",
      zoom = "assets/textures/world/zoommap/silt_loam_chunk.png",
      bg   = "assets/textures/world/zoommap/silt_loam_chunk_background.png" },
    { id = 61,  name = "silt",
      tile = "assets/textures/world/silt/silt.png",
      zoom = "assets/textures/world/zoommap/silt_chunk.png",
      bg   = "assets/textures/world/zoommap/silt_chunk_background.png" },
    { id = 62,  name = "peat",
      tile = "assets/textures/world/peat/peat.png",
      zoom = "assets/textures/world/zoommap/peat_chunk.png",
      bg   = "assets/textures/world/zoommap/peat_chunk_background.png" },
    { id = 63,  name = "mucky_peat",
      tile = "assets/textures/world/mucky_peat/mucky_peat.png",
      zoom = "assets/textures/world/zoommap/mucky_peat_chunk.png",
      bg   = "assets/textures/world/zoommap/mucky_peat_chunk_background.png" },
    { id = 64,  name = "muck",
      tile = "assets/textures/world/muck/muck.png",
      zoom = "assets/textures/world/zoommap/muck_chunk.png",
      bg   = "assets/textures/world/zoommap/muck_chunk_background.png" },
    { id = 65,  name = "heavy_gravel",
      tile = "assets/textures/world/heavy_gravel/heavy_gravel.png",
      zoom = "assets/textures/world/zoommap/heavy_gravel_chunk.png",
      bg   = "assets/textures/world/zoommap/heavy_gravel_chunk_background.png" },
    { id = 66,  name = "light_gravel",
      tile = "assets/textures/world/light_gravel/light_gravel.png",
      zoom = "assets/textures/world/zoommap/light_gravel_chunk.png",
      bg   = "assets/textures/world/zoommap/light_gravel_chunk_background.png" },
    { id = 67,  name = "salt_flat",
      tile = "assets/textures/world/salt_flat/salt_flat.png",
      zoom = "assets/textures/world/zoommap/salt_flat_chunk.png",
      bg   = "assets/textures/world/zoommap/salt_flat_chunk_background.png" },

    ---------------------------------------------------
    -- 70-79: Carbonaceous (coal series)
    ---------------------------------------------------
    { id = 70,  name = "lignite",
      tile = "assets/textures/world/lignite/lignite.png",
      zoom = "assets/textures/world/zoommap/lignite_chunk.png",
      bg   = "assets/textures/world/zoommap/lignite_chunk_background.png" },
    { id = 71,  name = "bituminous_coal",
      tile = "assets/textures/world/bituminous_coal/bituminous_coal.png",
      zoom = "assets/textures/world/zoommap/bituminous_coal_chunk.png",
      bg   = "assets/textures/world/zoommap/bituminous_coal_chunk_background.png" },
    { id = 72,  name = "anthracite",
      tile = "assets/textures/world/anthracite/anthracite.png",
      zoom = "assets/textures/world/zoommap/anthracite_chunk.png",
      bg   = "assets/textures/world/zoommap/anthracite_chunk_background.png" },

    ---------------------------------------------------
    -- 80-89: Ores / Metals
    ---------------------------------------------------
    { id = 80,  name = "iron_ore",
      tile = "assets/textures/world/iron_ore/iron_ore.png",
      zoom = "assets/textures/world/zoommap/iron_ore_chunk.png",
      bg   = "assets/textures/world/zoommap/iron_ore_chunk_background.png" },
    { id = 81,  name = "olivine",
      tile = "assets/textures/world/olivine/olivine.png",
      zoom = "assets/textures/world/zoommap/olivine_chunk.png",
      bg   = "assets/textures/world/zoommap/olivine_chunk_background.png" },
    { id = 82,  name = "pyroxene",
      tile = "assets/textures/world/pyroxene/pyroxene.png",
      zoom = "assets/textures/world/zoommap/pyroxene_chunk.png",
      bg   = "assets/textures/world/zoommap/pyroxene_chunk_background.png" },
    { id = 83,  name = "feldspar",
      tile = "assets/textures/world/feldspar/feldspar.png",
      zoom = "assets/textures/world/zoommap/feldspar_chunk.png",
      bg   = "assets/textures/world/zoommap/feldspar_chunk_background.png" },
    { id = 84,  name = "copper_ore",
      tile = "assets/textures/world/copper_ore/copper_ore.png",
      zoom = "assets/textures/world/zoommap/copper_ore_chunk.png",
      bg   = "assets/textures/world/zoommap/copper_ore_chunk_background.png" },
    { id = 85,  name = "tin_ore",
      tile = "assets/textures/world/tin_ore/tin_ore.png",
      zoom = "assets/textures/world/zoommap/tin_ore_chunk.png",
      bg   = "assets/textures/world/zoommap/tin_ore_chunk_background.png" },
    { id = 86,  name = "gold_ore",
      tile = "assets/textures/world/gold_ore/gold_ore.png",
      zoom = "assets/textures/world/zoommap/gold_ore_chunk.png",
      bg   = "assets/textures/world/zoommap/gold_ore_chunk_background.png" },

    ---------------------------------------------------
    -- 90-99: Impact
    ---------------------------------------------------
    { id = 90,  name = "impactite",
      tile = "assets/textures/world/impactite/impactite.png",
      zoom = "assets/textures/world/zoommap/impactite_chunk.png",
      bg   = "assets/textures/world/zoommap/impactite_chunk_background.png" },
    { id = 91,  name = "tektite",
      tile = "assets/textures/world/tektite/tektite.png",
      zoom = "assets/textures/world/zoommap/tektite_chunk.png",
      bg   = "assets/textures/world/zoommap/tektite_chunk_background.png" },

    ---------------------------------------------------
    -- 100-109: Volcanic Active
    ---------------------------------------------------
    { id = 100, name = "lava",
      tile = "assets/textures/world/lava/lava.png",
      zoom = "assets/textures/world/zoommap/lava_chunk.png",
      bg   = "assets/textures/world/zoommap/lava_chunk_background.png" },
    { id = 101, name = "magma",
      tile = "assets/textures/world/magma/magma.png",
      zoom = "assets/textures/world/zoommap/magma_chunk.png",
      bg   = "assets/textures/world/zoommap/magma_chunk_background.png" },
    { id = 102, name = "volcanic_ash",
      tile = "assets/textures/world/volcanic_ash/volcanic_ash.png",
      zoom = "assets/textures/world/zoommap/volcanic_ash_chunk.png",
      bg   = "assets/textures/world/zoommap/volcanic_ash_chunk_background.png" },
    { id = 103, name = "tephra",
      tile = "assets/textures/world/tephra/tephra.png",
      zoom = "assets/textures/world/zoommap/tephra_chunk.png",
      bg   = "assets/textures/world/zoommap/tephra_chunk_background.png" },

    ---------------------------------------------------
    -- 110-119: Glacial Deposits
    ---------------------------------------------------
    { id = 110, name = "till",
      tile = "assets/textures/world/till/till.png",
      zoom = "assets/textures/world/zoommap/till_chunk.png",
      bg   = "assets/textures/world/zoommap/till_chunk_background.png" },
    { id = 111, name = "moraine",
      tile = "assets/textures/world/moraine/moraine.png",
      zoom = "assets/textures/world/zoommap/moraine_chunk.png",
      bg   = "assets/textures/world/zoommap/moraine_chunk_background.png" },
    { id = 112, name = "glacial_clay",
      tile = "assets/textures/world/glacial_clay/glacial_clay.png",
      zoom = "assets/textures/world/zoommap/glacial_clay_chunk.png",
      bg   = "assets/textures/world/zoommap/glacial_clay_chunk_background.png" },
    { id = 113, name = "outwash_gravel",
      tile = "assets/textures/world/outwash_gravel/outwash_gravel.png",
      zoom = "assets/textures/world/zoommap/outwash_gravel_chunk.png",
      bg   = "assets/textures/world/zoommap/outwash_gravel_chunk_background.png" },

    ---------------------------------------------------
    -- 250-255: Special
    ---------------------------------------------------
    { id = 250, name = "glacier",
      tile = "assets/textures/world/glacier/glacier.png",
      zoom = "assets/textures/world/zoommap/glacier_chunk.png",
      bg   = "assets/textures/world/zoommap/glacier_chunk_background.png" },
    { id = 251, name = "mantle",
      tile = "assets/textures/world/mantle/mantle.png",
      zoom = "assets/textures/world/zoommap/mantle_chunk.png",
      bg   = "assets/textures/world/zoommap/mantle_chunk_background.png" },
    { id = 255, name = "ocean",
      tile = "assets/textures/world/ocean/ocean.png",
      zoom = "assets/textures/world/zoommap/ocean_chunk.png",
      bg   = "assets/textures/world/zoommap/ocean_chunk_background.png" },
}

-----------------------------------------------------------
-- Texture Storage
--
-- Structural textures: named fields (small fixed set)
-- Material textures:   materialTextures[id] = { tile=h, zoom=h, bg=h }
-----------------------------------------------------------

worldView.structuralTextures = {
    ocean          = -1,
    glacier        = -1,
    lava           = -1,
    noTexture      = -1,
    blankTexture   = -1,
    isoFaceMap     = -1,
    isoSlopeFaceMapN    = -1,
    isoSlopeFaceMapE    = -1,
    isoSlopeFaceMapNE   = -1,
    isoSlopeFaceMapS    = -1,
    isoSlopeFaceMapNS   = -1,
    isoSlopeFaceMapES   = -1,
    isoSlopeFaceMapNES  = -1,
    isoSlopeFaceMapW    = -1,
    isoSlopeFaceMapNW   = -1,
    isoSlopeFaceMapEW   = -1,
    isoSlopeFaceMapNEW  = -1,
    isoSlopeFaceMapSW   = -1,
    isoSlopeFaceMapNSW  = -1,
    isoSlopeFaceMapESW  = -1,
    isoSlopeFaceMapNESW = -1,
    noFaceMap      = -1,
}

-- materialTextures[matId] = { tile = handle, zoom = handle, bg = handle }
worldView.materialTextures = {}

-- All loaded handles for asset-loaded tracking
worldView.allHandles = {}

worldView.texturesNeeded = 0
worldView.texturesLoadedCount = 0

-- Whether we've been asked to generate (from create_world_menu)
-- but are still waiting for textures to load
worldView.pendingGeneration = false

-----------------------------------------------------------
-- Init
-----------------------------------------------------------

function worldView.init(width, height)
    worldView.fbW = width
    worldView.fbH = height
    worldView.allHandles = {}

    -- Count how many textures we need to load
    local count = 0

    -- Load structural textures
    local st = worldView.structuralTextures
    st.ocean          = engine.loadTexture("assets/textures/world/zoommap/ocean_chunk.png")
    st.glacier        = engine.loadTexture("assets/textures/world/zoommap/glacier_chunk.png")
    st.lava           = engine.loadTexture("assets/textures/world/zoommap/lava_chunk.png")
    st.noTexture      = engine.loadTexture("assets/textures/world/notexture.png")
    st.blankTexture   = engine.loadTexture("assets/textures/world/blanktexture.png")
    st.isoFaceMap     = engine.loadTexture("assets/textures/world/facemap/isoface.png")
    st.isoSlopeFaceMapN    = engine.loadTexture("assets/textures/world/facemap/isoface_slope_n.png")
    st.isoSlopeFaceMapE    = engine.loadTexture("assets/textures/world/facemap/isoface_slope_e.png")
    st.isoSlopeFaceMapNE   = engine.loadTexture("assets/textures/world/facemap/isoface_slope_ne.png")
    st.isoSlopeFaceMapS    = engine.loadTexture("assets/textures/world/facemap/isoface_slope_s.png")
    st.isoSlopeFaceMapNS   = engine.loadTexture("assets/textures/world/facemap/isoface_slope_ns.png")
    st.isoSlopeFaceMapES   = engine.loadTexture("assets/textures/world/facemap/isoface_slope_es.png")
    st.isoSlopeFaceMapNES  = engine.loadTexture("assets/textures/world/facemap/isoface_slope_nes.png")
    st.isoSlopeFaceMapW    = engine.loadTexture("assets/textures/world/facemap/isoface_slope_w.png")
    st.isoSlopeFaceMapNW   = engine.loadTexture("assets/textures/world/facemap/isoface_slope_nw.png")
    st.isoSlopeFaceMapEW   = engine.loadTexture("assets/textures/world/facemap/isoface_slope_ew.png")
    st.isoSlopeFaceMapNEW  = engine.loadTexture("assets/textures/world/facemap/isoface_slope_new.png")
    st.isoSlopeFaceMapSW   = engine.loadTexture("assets/textures/world/facemap/isoface_slope_sw.png")
    st.isoSlopeFaceMapNSW  = engine.loadTexture("assets/textures/world/facemap/isoface_slope_nsw.png")
    st.isoSlopeFaceMapESW  = engine.loadTexture("assets/textures/world/facemap/isoface_slope_esw.png")
    st.isoSlopeFaceMapNESW = engine.loadTexture("assets/textures/world/facemap/isoface_slope_nesw.png")
    st.noFaceMap      = engine.loadTexture("assets/textures/world/facemap/noface.png")

    for _, handle in pairs(st) do
        worldView.allHandles[handle] = true
        count = count + 1
    end

    -- Load material textures from the table
    worldView.materialTextures = {}
    for _, def in ipairs(worldView.materialDefs) do
        local tileH = engine.loadTexture(def.tile)
        local zoomH = engine.loadTexture(def.zoom)
        local bgH   = engine.loadTexture(def.bg)
        worldView.materialTextures[def.id] = {
            tile = tileH,
            zoom = zoomH,
            bg   = bgH,
        }
        worldView.allHandles[tileH] = true
        worldView.allHandles[zoomH] = true
        worldView.allHandles[bgH]   = true
        count = count + 3
    end

    worldView.texturesNeeded = count
    worldView.texturesLoadedCount = 0

    engine.logInfo("World view initialized, loading " .. count .. " textures")
end

-----------------------------------------------------------
-- Asset Loading Callback
-----------------------------------------------------------

function worldView.onAssetLoaded(assetType, handle, path)
    if assetType ~= "texture" then return end

    if not worldView.allHandles[handle] then return end

    worldView.texturesLoadedCount = worldView.texturesLoadedCount + 1
    engine.logDebug("World texture loaded: handle=" .. tostring(handle)
        .. " (" .. worldView.texturesLoadedCount .. "/" .. worldView.texturesNeeded .. ")")

    -- Check if all textures are now loaded
    if worldView.texturesLoadedCount >= worldView.texturesNeeded then
        if worldView.visible then
            engine.logInfo("All world textures loaded, creating world...")
            worldView.createWorld()
        end
        if worldView.pendingGeneration then
            worldView.pendingGeneration = false
            engine.logInfo("Pending generation triggered, creating world...")
            worldView.createWorld()
        end
    end
end

-----------------------------------------------------------
-- Start Generation (called from create_world_menu)
-----------------------------------------------------------

function worldView.startGeneration()
    if worldView.texturesLoadedCount >= worldView.texturesNeeded then
        worldView.createWorld()
    else
        worldView.pendingGeneration = true
        engine.logInfo("World generation deferred, waiting for textures... ("
            .. worldView.texturesLoadedCount .. "/" .. worldView.texturesNeeded .. ")")
    end
end

-----------------------------------------------------------
-- Create World
-----------------------------------------------------------

function worldView.createWorld()
    if worldView.texturesLoadedCount < worldView.texturesNeeded then
        engine.logWarn("Cannot create world, textures not loaded yet ("
            .. worldView.texturesLoadedCount .. "/" .. worldView.texturesNeeded .. ")")
        return
    end

    if worldManager.isActive() then
        engine.logWarn("World already active, skipping creation")
        return
    end

    local wp = worldView.worldParams or {}
    local seed = wp.seed or 42
    local worldSize = wp.worldSize or 128
    local plateCount = wp.plateCount or 10

    worldManager.createWorld({
        worldId    = "main_world",
        seed       = seed,
        worldSize  = worldSize,
        plateCount = plateCount,
        structural = worldView.structuralTextures,
        materials  = worldView.materialTextures,
    })
    worldManager.showWorld()
end

-----------------------------------------------------------
-- Create UI
-----------------------------------------------------------

function worldView.createUI()
    if worldView.page then
        UI.deletePage(worldView.page)
    end

    worldView.page = UI.newPage("world_view_hud", "hud")

    engine.logInfo("World view UI created")
end

-----------------------------------------------------------
-- Show / Hide
-----------------------------------------------------------

function worldView.show()
    if not worldView.page then
        worldView.createUI()
    end

    worldView.visible = true
    UI.showPage(worldView.page)

    if worldManager.getCurrentWorld() then
        worldManager.showWorld()
    elseif worldView.texturesLoadedCount >= worldView.texturesNeeded then
        worldView.createWorld()
    else
        engine.logInfo("World view shown, waiting for textures... ("
            .. worldView.texturesLoadedCount .. "/" .. worldView.texturesNeeded .. ")")
    end

    engine.logInfo("World view shown")
end

function worldView.hide()
    worldManager.hideWorld()

    worldView.visible = false
    if worldView.page then
        UI.hidePage(worldView.page)
    end

    engine.logInfo("World view hidden")
end

-----------------------------------------------------------
-- Update
-----------------------------------------------------------

function worldView.update(dt)
    if not worldView.visible then return end
    worldManager.update(dt)
end

-----------------------------------------------------------
-- Resize
-----------------------------------------------------------

function worldView.onFramebufferResize(width, height)
    worldView.fbW = width
    worldView.fbH = height
    if worldView.visible then
        worldView.createUI()
    end
end

-----------------------------------------------------------
-- Camera Zoom (normal scroll, no shift, no UI focus)
-----------------------------------------------------------

local zoomImpulseScale = 0.4

function worldView.onScroll(dx, dy)
    if not worldView.visible then return end

    local current = camera.getZoomVelocity()
    local zoom = camera.getZoom()
    local impulse = zoomImpulseScale * zoom
    if dy > 0 then
        camera.setZoomVelocity(current - impulse)
    elseif dy < 0 then
        camera.setZoomVelocity(current + impulse)
    end
end

-----------------------------------------------------------
-- Z-Slice Control (shift+scroll)
-----------------------------------------------------------

function worldView.onZSliceScroll(dx, dy)
    if not worldView.visible then return end

    camera.setZTracking(false)

    local current = camera.getZSlice()
    if dy > 0 then
        camera.setZSlice(current + 1)
    elseif dy < 0 then
        camera.setZSlice(current - 1)
    end
end

-----------------------------------------------------------
-- Key Input
-----------------------------------------------------------

function worldView.onKeyDown(key)
    if not worldView.visible then return end

    if key == "Q" then
        camera.rotateCCW()
        engine.logDebug("Camera rotated CCW, facing=" .. tostring(camera.getFacing()))
    elseif key == "E" then
        camera.rotateCW()
        engine.logDebug("Camera rotated CW, facing=" .. tostring(camera.getFacing()))
    elseif key == "Home" then
        camera.setZTracking(true)
        engine.logDebug("Z-slice tracking re-enabled")
    end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------

function worldView.shutdown()
    if worldView.page then
        UI.deletePage(worldView.page)
    end

    worldManager.destroyWorld()
end

-----------------------------------------------------------
-- Utility
-----------------------------------------------------------

function worldView.saveGame(saveName)
    local worldId = worldManager.getCurrentWorld()
    if worldId then
        engine.saveWorld(worldId, saveName or "quicksave")
        engine.logInfo("Game saved: " .. (saveName or "quicksave"))
    end
end

-----------------------------------------------------------
-- Send textures to an existing world (for loaded saves)
-----------------------------------------------------------

function worldView.sendTexturesToWorld(worldId)
    if worldView.texturesLoadedCount < worldView.texturesNeeded then
        engine.logWarn("Cannot send textures, not all loaded yet ("
            .. worldView.texturesLoadedCount .. "/" .. worldView.texturesNeeded .. ")")
        return false
    end

    engine.logInfo("Sending textures to loaded world: " .. worldId)

    -- Structural
    local st = worldView.structuralTextures
    world.setTexture(worldId, "ocean",     st.ocean)
    world.setTexture(worldId, "glacier",   st.glacier)
    world.setTexture(worldId, "lava",      st.lava)
    world.setTexture(worldId, "blank",     st.blankTexture)
    world.setTexture(worldId, "notexture", st.noTexture)
    world.setTexture(worldId, "iso_facemap",           st.isoFaceMap)
    world.setTexture(worldId, "iso_slope_facemap_n",   st.isoSlopeFaceMapN)
    world.setTexture(worldId, "iso_slope_facemap_e",   st.isoSlopeFaceMapE)
    world.setTexture(worldId, "iso_slope_facemap_ne",  st.isoSlopeFaceMapNE)
    world.setTexture(worldId, "iso_slope_facemap_s",   st.isoSlopeFaceMapS)
    world.setTexture(worldId, "iso_slope_facemap_ns",  st.isoSlopeFaceMapNS)
    world.setTexture(worldId, "iso_slope_facemap_es",  st.isoSlopeFaceMapES)
    world.setTexture(worldId, "iso_slope_facemap_nes", st.isoSlopeFaceMapNES)
    world.setTexture(worldId, "iso_slope_facemap_w",   st.isoSlopeFaceMapW)
    world.setTexture(worldId, "iso_slope_facemap_nw",  st.isoSlopeFaceMapNW)
    world.setTexture(worldId, "iso_slope_facemap_ew",  st.isoSlopeFaceMapEW)
    world.setTexture(worldId, "iso_slope_facemap_new", st.isoSlopeFaceMapNEW)
    world.setTexture(worldId, "iso_slope_facemap_sw",  st.isoSlopeFaceMapSW)
    world.setTexture(worldId, "iso_slope_facemap_nsw", st.isoSlopeFaceMapNSW)
    world.setTexture(worldId, "iso_slope_facemap_esw", st.isoSlopeFaceMapESW)
    world.setTexture(worldId, "iso_slope_facemap_nesw",st.isoSlopeFaceMapNESW)
    world.setTexture(worldId, "nofacemap",             st.noFaceMap)

    -- Materials (loop over the loaded handles)
    for matId, handles in pairs(worldView.materialTextures) do
        world.setTexture(worldId, "mat_tile_" .. matId, handles.tile)
        world.setTexture(worldId, "mat_zoom_" .. matId, handles.zoom)
        world.setTexture(worldId, "mat_bg_"   .. matId, handles.bg)
    end

    engine.logInfo("All textures sent to world: " .. worldId)
    return true
end

return worldView
