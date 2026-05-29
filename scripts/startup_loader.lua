-- Startup Loader - drives the initial asset-loading queue.
-- Used by loading_screen in "startup" mode (alongside its existing
-- world-gen mode). Each item in the queue is a single piece of work
-- (one YAML file, one texture); the loading_screen polls progress
-- and shows a green bar + per-phase status text.
--
-- Built up at first show; items run a few per tick so the frame
-- doesn't stall on a 100-texture load.
local startupLoader = {}

startupLoader.items         = {}
startupLoader.processed     = 0
startupLoader.currentLabel  = "Initializing..."
startupLoader.built         = false
startupLoader.done          = false
startupLoader.itemsPerTick  = 4

local function addItem(label, fn)
    startupLoader.items[#startupLoader.items + 1] = {label = label, fn = fn}
end

local function addYamlDir(dir, label, loaderFn)
    local files = engine.listFiles(dir, ".yaml")
    if not files then return end
    for _, fname in ipairs(files) do
        local path = dir .. "/" .. fname
        addItem(label, function() loaderFn(path) end)
    end
end

local function addTextureList(label, paths)
    for _, p in ipairs(paths) do
        addItem(label, function() engine.loadTexture(p) end)
    end
end

local function addTextureDir(dir, label)
    local files = engine.listFiles(dir, ".png")
    if not files then return end
    for _, fname in ipairs(files) do
        local path = dir .. "/" .. fname
        addItem(label, function() engine.loadTexture(path) end)
    end
end

-----------------------------------------------------------
-- Hardcoded texture lists
-- (paths that don't live in a flat dir we can listFiles on)
-----------------------------------------------------------

-- World structural textures consumed by world_view. These end up
-- as cache-hits when world_view.init re-requests them, so no
-- duplicate work happens.
local worldStructuralPaths = {
    "assets/textures/world/zoommap/ocean_chunk.png",
    "assets/textures/world/zoommap/glacier_chunk.png",
    "assets/textures/world/zoommap/lava_chunk.png",
    "assets/textures/world/notexture.png",
    "assets/textures/world/blanktexture.png",
    "assets/textures/world/facemap/isoface.png",
    "assets/textures/world/facemap/isoface_slope_n.png",
    "assets/textures/world/facemap/isoface_slope_e.png",
    "assets/textures/world/facemap/isoface_slope_ne.png",
    "assets/textures/world/facemap/isoface_slope_s.png",
    "assets/textures/world/facemap/isoface_slope_ns.png",
    "assets/textures/world/facemap/isoface_slope_es.png",
    "assets/textures/world/facemap/isoface_slope_nes.png",
    "assets/textures/world/facemap/isoface_slope_w.png",
    "assets/textures/world/facemap/isoface_slope_nw.png",
    "assets/textures/world/facemap/isoface_slope_ew.png",
    "assets/textures/world/facemap/isoface_slope_new.png",
    "assets/textures/world/facemap/isoface_slope_sw.png",
    "assets/textures/world/facemap/isoface_slope_nsw.png",
    "assets/textures/world/facemap/isoface_slope_esw.png",
    "assets/textures/world/facemap/isoface_slope_nesw.png",
    "assets/textures/world/facemap/noface.png",
    "assets/textures/world/facemap/isoface_left.png",
    "assets/textures/world/facemap/isoface_right.png",
    "assets/textures/world/facemap/vegface.png",
    "assets/textures/world/facemap/vegface_slope_n.png",
    "assets/textures/world/facemap/vegface_slope_e.png",
    "assets/textures/world/facemap/vegface_slope_ne.png",
    "assets/textures/world/facemap/vegface_slope_s.png",
    "assets/textures/world/facemap/vegface_slope_ns.png",
    "assets/textures/world/facemap/vegface_slope_es.png",
    "assets/textures/world/facemap/vegface_slope_nes.png",
    "assets/textures/world/facemap/vegface_slope_w.png",
    "assets/textures/world/facemap/vegface_slope_nw.png",
    "assets/textures/world/facemap/vegface_slope_ew.png",
    "assets/textures/world/facemap/vegface_slope_new.png",
    "assets/textures/world/facemap/vegface_slope_sw.png",
    "assets/textures/world/facemap/vegface_slope_nsw.png",
    "assets/textures/world/facemap/vegface_slope_esw.png",
    "assets/textures/world/facemap/vegface_slope_nesw.png",
}

-- HUD textures consumed by hud.init. Same cache-hit story as the
-- world structural paths above.
local hudPaths = {
    "assets/textures/hud/map_default.png",
    "assets/textures/hud/map_default_selected.png",
    "assets/textures/hud/map_temp.png",
    "assets/textures/hud/map_temp_selected.png",
    "assets/textures/hud/map_seatemp.png",
    "assets/textures/hud/map_seatemp_selected.png",
    "assets/textures/hud/map_pressure.png",
    "assets/textures/hud/map_pressure_selected.png",
    "assets/textures/hud/map_humidity.png",
    "assets/textures/hud/map_humidity_selected.png",
    "assets/textures/hud/map_precipitation.png",
    "assets/textures/hud/map_precipitation_selected.png",
    "assets/textures/hud/map_preciptype.png",
    "assets/textures/hud/map_preciptype_selected.png",
    "assets/textures/hud/map_evaporation.png",
    "assets/textures/hud/map_evaporation_selected.png",
    "assets/textures/hud/tool_default.png",
    "assets/textures/hud/tool_default_selected.png",
    "assets/textures/hud/tool_info.png",
    "assets/textures/hud/tool_info_selected.png",
    "assets/textures/hud/tool_mine.png",
    "assets/textures/hud/tool_mine_selected.png",
    "assets/textures/hud/tool_build.png",
    "assets/textures/hud/tool_build_selected.png",
    "assets/textures/hud/utility/zoom_select.png",
    "assets/textures/hud/utility/zoom_hover.png",
    "assets/textures/hud/utility/world_select.png",
    "assets/textures/hud/utility/world_select_bg.png",
    "assets/textures/hud/utility/world_hover.png",
    "assets/textures/hud/utility/world_hover_bg.png",
    "assets/textures/hud/event_log.png",
    "assets/textures/hud/event_log_selected.png",
    "assets/textures/hud/combat_log.png",
    "assets/textures/hud/combat_log_selected.png",
}

-----------------------------------------------------------
-- Build the queue
-----------------------------------------------------------

function startupLoader.build()
    startupLoader.items     = {}
    startupLoader.processed = 0
    startupLoader.done      = false

    -- YAML phases. Each YAML loader internally calls engine.loadTexture
    -- for the textures it references, so the texture cache warms up
    -- as we go.
    addYamlDir("data/materials",  "Loading materials...",  engine.loadMaterialYaml)
    addYamlDir("data/vegetation", "Loading vegetation...", engine.loadVegetationYaml)
    addYamlDir("data/flora",      "Loading flora...",      engine.loadFloraYaml)
    addYamlDir("data/substances", "Loading substances...", engine.loadSubstanceYaml)
    addYamlDir("data/items",      "Loading items...",      engine.loadItemYaml)
    addYamlDir("data/equipment",  "Loading equipment...",  engine.loadEquipmentYaml)
    addYamlDir("data/buildings",  "Loading buildings...",  engine.loadBuildingYaml)
    addYamlDir("data/units",      "Loading units...",      engine.loadUnitYaml)

    -- Texture-only phases.
    addTextureDir("assets/textures/icons", "Loading icons...")
    addTextureList("Loading HUD...",   hudPaths)
    addTextureList("Loading world...", worldStructuralPaths)

    startupLoader.built = true
    startupLoader.currentLabel = "Loading..."
    engine.logInfo("Startup loader queued " .. #startupLoader.items .. " items")
end

-----------------------------------------------------------
-- Tick
-----------------------------------------------------------

function startupLoader.tick(dt)
    if startupLoader.done then return end
    if not startupLoader.built then return end

    for _ = 1, startupLoader.itemsPerTick do
        local idx = startupLoader.processed + 1
        if idx > #startupLoader.items then
            startupLoader.done = true
            startupLoader.currentLabel = "Complete!"
            return
        end
        local item = startupLoader.items[idx]
        startupLoader.currentLabel = item.label
        item.fn()
        startupLoader.processed = idx
    end
end

-----------------------------------------------------------
-- Progress queries (loading_screen polls these)
-----------------------------------------------------------

function startupLoader.getProgress()
    local total = #startupLoader.items
    if total == 0 then return 0.0, startupLoader.currentLabel end
    return startupLoader.processed / total, startupLoader.currentLabel
end

function startupLoader.isDone()
    return startupLoader.done
end

function startupLoader.reset()
    startupLoader.items        = {}
    startupLoader.processed    = 0
    startupLoader.currentLabel = "Initializing..."
    startupLoader.built        = false
    startupLoader.done         = false
end

return startupLoader
