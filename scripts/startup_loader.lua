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
startupLoader.profile       = "normal"
startupLoader.itemsPerTick  = 4

local function addItem(label, fn)
    startupLoader.items[#startupLoader.items + 1] = {label = label, fn = fn}
end

-----------------------------------------------------------
-- Per-family load aggregates (#1930)
--
-- The engine's `engine.load*Yaml` bindings each keep their per-file
-- success detail at CatAsset DEBUG. This module is the owner of the
-- one INFO line per registry family: it sums what those calls actually
-- returned and reports the total once, after that family's queued
-- files have run.
-----------------------------------------------------------

-- The stable identifier each YAML registry family reports under, keyed
-- by its DIRECTORY rather than passed as a fourth argument to
-- addYamlDir/addYamlTree. Those two call shapes are parsed verbatim by
-- `tools/save_compat_migration_probe.py`, which derives production's
-- registry bootstrap from `queueNormalProfile`'s source; keeping them
-- at exactly three arguments is what keeps that check working.
--
-- A directory absent from this table still aggregates — under its own
-- path — so a family added later reports something honest rather than
-- silently reporting nothing.
local yamlFamilies = {
    ["data/materials"]   = "material",
    ["data/vegetation"]  = "vegetation",
    ["data/flora"]       = "flora",
    ["data/substances"]  = "substance",
    ["data/infections"]  = "infection",
    ["data/recipes"]     = "recipe",
    ["data/items"]       = "item",
    ["data/equipment"]   = "equipment",
    ["data/buildings"]   = "building",
    ["data/units"]       = "unit",
    ["data/loot_tables"] = "loot_table",
    ["data/locations"]   = "location",
}

-- One binding's return value as a count.
--
-- `engine.load*Yaml` pushes a Lua NUMBER, which under Lua 5.4 is a
-- FLOAT: concatenating it directly would render 39 as "39.0". Floored
-- here so the aggregate reads as the integer it is, and a nil/garbage
-- return contributes 0 rather than raising inside the queue.
local function asCount(value)
    local n = tonumber(value)
    if not n then return 0 end
    return math.floor(n)
end

-- THE spelling of the aggregate line, exposed so a test or probe reads
-- it from here instead of restating it.
--
-- Deliberately says only "loaded N", never "N definitions": the value
-- summed is whatever that family's binding returns to Lua, and that is
-- not one quantity across the twelve — materials and vegetation return
-- a TEXTURE total, flora a texture total too, loot tables 0 or 1 per
-- file, and the rest a definition count. `files` distinguishes a family
-- whose directory held nothing from one whose files all returned zero.
function startupLoader.aggregateMessage(family, total, files)
    return string.format("Startup assets: %s loaded %d from %d file(s)",
                         family, math.floor(total), math.floor(files))
end

-- Enqueue one family: each of `paths` in the order given, then exactly
-- one aggregate.
--
-- The aggregate is its own queue entry rather than a tail-call off the
-- last file, because requirement 4 wants one even when the family
-- discovered NO files at all. That lengthens the queue (and so the
-- loading screen's progress denominator) by one per family, which #1930
-- explicitly permits; per-file invocation count, per-file order and
-- family order are untouched.
local function addYamlFamily(dir, label, loaderFn, paths)
    local family = yamlFamilies[dir] or dir
    local total, seen = 0, 0
    for _, path in ipairs(paths) do
        addItem(label, function()
            total = total + asCount(loaderFn(path))
            seen  = seen + 1
        end)
    end
    addItem(label, function()
        engine.logInfo(startupLoader.aggregateMessage(family, total, seen))
    end)
end

local function addYamlDir(dir, label, loaderFn)
    local files = engine.listFiles(dir, ".yaml")
    local paths = {}
    if files then
        for _, fname in ipairs(files) do
            paths[#paths + 1] = dir .. "/" .. fname
        end
    end
    addYamlFamily(dir, label, loaderFn, paths)
end

-- Order two `/`-separated relative paths by the UTF-8 BYTES of the
-- path itself (#1232 requirement 2). Written out rather than left to
-- `a < b`, which Lua answers with strcoll: that agrees with byte order
-- under the "C" locale but is not guaranteed to under another, and the
-- load order of a data tree must not depend on the host's locale.
local function pathLess(a, b)
    local shorter = #a < #b and #a or #b
    for i = 1, shorter do
        local ca, cb = a:byte(i), b:byte(i)
        if ca ~= cb then return ca < cb end
    end
    return #a < #b
end

-- The canonical total order over a tree's discovered files: ascending
-- `pathLess` on each path relative to the tree root.
--
-- A PURE transformation over an already-enumerated list, deliberately:
-- it is what makes the load order testable against two different
-- underlying enumeration orders (#1232 requirement 11), which is
-- impossible when a walk sorts opaquely inside itself. Returns a new
-- array; the caller's list is left alone.
function startupLoader.canonicalFileOrder(relPaths)
    local ordered = {}
    for i, rel in ipairs(relPaths) do ordered[i] = rel end
    table.sort(ordered, pathLess)
    return ordered
end

-- Recursive counterpart of addYamlDir: enqueues every YAML under `dir`
-- at ANY depth, ONE queue entry per file (so `loaderFn` still sees each
-- file exactly once and the loading screen keeps its per-file progress
-- granularity), in canonicalFileOrder.
--
-- Only trees whose contents may be organized into subdirectories use
-- this. Everything else stays on addYamlDir's flat, OS-ordered
-- engine.listFiles: flora IDs are allocated in load order and salt
-- worldgen placement.
local function addYamlTree(dir, label, loaderFn)
    local rels = engine.listFilesRecursive(dir, ".yaml")
    local paths = {}
    if rels then
        for _, rel in ipairs(startupLoader.canonicalFileOrder(rels)) do
            paths[#paths + 1] = dir .. "/" .. rel
        end
    end
    addYamlFamily(dir, label, loaderFn, paths)
end

-- `policy` is engine.loadTexture's upload policy (#2075) and is passed
-- through verbatim, nil included. A PRELOAD must declare the SAME policy
-- its eventual consumer declares: the cache is keyed by (path, policy),
-- so preloading under the wrong one uploads a slot nobody will ever
-- sample and leaves the consumer to upload the real one anyway.
local function addTextureList(label, paths, policy)
    for _, p in ipairs(paths) do
        addItem(label, function() engine.loadTexture(p, policy) end)
    end
end

local function addTextureDir(dir, label, policy)
    local files = engine.listFiles(dir, ".png")
    if not files then return end
    for _, fname in ipairs(files) do
        local path = dir .. "/" .. fname
        addItem(label, function() engine.loadTexture(path, policy) end)
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
    "assets/textures/utility/notexture.png",
    "assets/textures/utility/blanktexture.png",
    "assets/textures/facemap/isoface.png",
    "assets/textures/facemap/isoface_slope_n.png",
    "assets/textures/facemap/isoface_slope_e.png",
    "assets/textures/facemap/isoface_slope_ne.png",
    "assets/textures/facemap/isoface_slope_s.png",
    "assets/textures/facemap/isoface_slope_ns.png",
    "assets/textures/facemap/isoface_slope_es.png",
    "assets/textures/facemap/isoface_slope_nes.png",
    "assets/textures/facemap/isoface_slope_w.png",
    "assets/textures/facemap/isoface_slope_nw.png",
    "assets/textures/facemap/isoface_slope_ew.png",
    "assets/textures/facemap/isoface_slope_new.png",
    "assets/textures/facemap/isoface_slope_sw.png",
    "assets/textures/facemap/isoface_slope_nsw.png",
    "assets/textures/facemap/isoface_slope_esw.png",
    "assets/textures/facemap/isoface_slope_nesw.png",
    "assets/textures/facemap/noface.png",
    "assets/textures/facemap/isoface_left.png",
    "assets/textures/facemap/isoface_right.png",
    "assets/textures/facemap/vegface.png",
    "assets/textures/facemap/vegface_slope_n.png",
    "assets/textures/facemap/vegface_slope_e.png",
    "assets/textures/facemap/vegface_slope_ne.png",
    "assets/textures/facemap/vegface_slope_s.png",
    "assets/textures/facemap/vegface_slope_ns.png",
    "assets/textures/facemap/vegface_slope_es.png",
    "assets/textures/facemap/vegface_slope_nes.png",
    "assets/textures/facemap/vegface_slope_w.png",
    "assets/textures/facemap/vegface_slope_nw.png",
    "assets/textures/facemap/vegface_slope_ew.png",
    "assets/textures/facemap/vegface_slope_new.png",
    "assets/textures/facemap/vegface_slope_sw.png",
    "assets/textures/facemap/vegface_slope_nsw.png",
    "assets/textures/facemap/vegface_slope_esw.png",
    "assets/textures/facemap/vegface_slope_nesw.png",
}

-- HUD CHROME consumed by hud.init: toolbar buttons, map-mode buttons and
-- the log toggles, all drawn by the UI layer. Same cache-hit story as
-- the world structural paths above, and preloaded under the SAME "ui"
-- policy hud.init declares (#2075) so these are genuine hits rather than
-- a discarded second copy under the scene policy.
local hudUiPaths = {
    "assets/textures/ui/hud/map_default.png",
    "assets/textures/ui/hud/map_default_selected.png",
    "assets/textures/ui/hud/map_temp.png",
    "assets/textures/ui/hud/map_temp_selected.png",
    "assets/textures/ui/hud/map_seatemp.png",
    "assets/textures/ui/hud/map_seatemp_selected.png",
    "assets/textures/ui/hud/map_pressure.png",
    "assets/textures/ui/hud/map_pressure_selected.png",
    "assets/textures/ui/hud/map_humidity.png",
    "assets/textures/ui/hud/map_humidity_selected.png",
    "assets/textures/ui/hud/map_precipitation.png",
    "assets/textures/ui/hud/map_precipitation_selected.png",
    "assets/textures/ui/hud/map_preciptype.png",
    "assets/textures/ui/hud/map_preciptype_selected.png",
    "assets/textures/ui/hud/map_evaporation.png",
    "assets/textures/ui/hud/map_evaporation_selected.png",
    "assets/textures/ui/hud/tool_default.png",
    "assets/textures/ui/hud/tool_default_selected.png",
    "assets/textures/ui/hud/tool_info.png",
    "assets/textures/ui/hud/tool_info_selected.png",
    "assets/textures/ui/hud/tool_mine.png",
    "assets/textures/ui/hud/tool_mine_selected.png",
    "assets/textures/ui/hud/tool_build.png",
    "assets/textures/ui/hud/tool_build_selected.png",
    "assets/textures/ui/hud/event_log.png",
    "assets/textures/ui/hud/event_log_selected.png",
    "assets/textures/ui/hud/combat_log.png",
    "assets/textures/ui/hud/combat_log_selected.png",
}

-- The cursor overlays hud.init hands to world.setZoomCursor*Texture /
-- world.setWorldCursor*Texture. They live under assets/textures/ui/ and
-- are loaded in the same function as the chrome above, but they are
-- drawn IN THE WORLD at world scale, so they are scene art and follow
-- the player's filter setting (#2075). This is exactly why the policy is
-- declared at the call site and never derived from the path.
local hudScenePaths = {
    "assets/textures/ui/hud/utility/zoom_select.png",
    "assets/textures/ui/hud/utility/zoom_hover.png",
    "assets/textures/ui/hud/utility/world_select.png",
    "assets/textures/ui/hud/utility/world_select_bg.png",
    "assets/textures/ui/hud/utility/world_hover.png",
    "assets/textures/ui/hud/utility/world_hover_bg.png",
}

-----------------------------------------------------------
-- Build the queue
-----------------------------------------------------------

local function queueNormalProfile()
    addYamlDir("data/materials",  "Loading materials...",  engine.loadMaterialYaml)
    addYamlDir("data/vegetation", "Loading vegetation...", engine.loadVegetationYaml)
    addYamlDir("data/flora",      "Loading flora...",      engine.loadFloraYaml)
    addYamlDir("data/substances", "Loading substances...", engine.loadSubstanceYaml)
    addYamlDir("data/infections", "Loading infections...", engine.loadInfectionYaml)
    addYamlDir("data/recipes",    "Loading recipes...",    engine.loadRecipeYaml)
    -- Items are the one data family whose definitions may be organized
    -- into logical subdirectories (#1232); their ids come from each
    -- definition's own `name:`, never from its path.
    addYamlTree("data/items",     "Loading items...",      engine.loadItemYaml)
    addYamlDir("data/equipment",  "Loading equipment...",  engine.loadEquipmentYaml)
    addYamlDir("data/buildings",  "Loading buildings...",  engine.loadBuildingYaml)
    addYamlDir("data/units",      "Loading units...",      engine.loadUnitYaml)
    addYamlDir("data/loot_tables", "Loading loot tables...", engine.loadLootTableYaml)
    -- The one active tutorial tree (#957). A DIRECTORY verb, not
    -- addYamlDir: this slice supports exactly one tree, and neither
    -- "a tree is present" nor "there is only one" can be checked from
    -- inside a single file, so the engine enumerates data/tutorials/
    -- itself in one call. Self-contained authored data — references no
    -- other registry — so its position here is free; it only has to
    -- come before the tutorial runtime reads it.
    addItem("Loading tutorial...", function()
        engine.loadTutorialDir("data/tutorials")
    end)
    -- Locations load LAST (their content ids, incl. loot_table ids,
    -- reference the registries above; resolved at spawn time, #90).
    addYamlDir("data/locations",  "Loading locations...",  engine.loadLocationYaml)

    -- Texture-only phases.
    -- Icons are organized into kind subfolders; addTextureDir is not
    -- recursive, so enqueue each subfolder.
    -- These six families are unit_info_v2_panel_engine's row icons, drawn
    -- by the UI layer -- NOT assets/textures/icons/location, which is
    -- drawn on the zoom map and is loaded from the location YAML rather
    -- than here.
    for _, sub in ipairs({ "stat", "skill", "status", "injury", "infection", "knowledge" }) do
        addTextureDir("assets/textures/icons/" .. sub, "Loading icons...", "ui")
    end
    addTextureList("Loading HUD...",   hudUiPaths,    "ui")
    addTextureList("Loading HUD...",   hudScenePaths, "scene")
    addTextureList("Loading world...", worldStructuralPaths, "scene")
end

local function queueArenaProfile()
    -- Arena/dev boot only needs the registries and runtime definitions
    -- required by the debug overlay, build tool, and flat arena world.
    -- Everything else can stream in later on first use.
    addYamlDir("data/materials",  "Loading materials...",  engine.loadMaterialYaml)
    -- Arena surface tiles spawn with grass vegetation IDs 5-8, so the
    -- vegetation registry has to exist up front or every visible tile
    -- resolves to the undefined magenta checkerboard.
    addYamlDir("data/vegetation", "Loading vegetation...", engine.loadVegetationYaml)
    addYamlDir("data/substances", "Loading substances...", engine.loadSubstanceYaml)
    addYamlDir("data/infections", "Loading infections...", engine.loadInfectionYaml)
    addYamlDir("data/recipes",    "Loading recipes...",    engine.loadRecipeYaml)
    addYamlTree("data/items",     "Loading items...",      engine.loadItemYaml)
    addYamlDir("data/equipment",  "Loading equipment...",  engine.loadEquipmentYaml)
    addYamlDir("data/buildings",  "Loading buildings...",  engine.loadBuildingYaml)
    addYamlDir("data/units",      "Loading units...",      engine.loadUnitYaml)
    addYamlDir("data/loot_tables", "Loading loot tables...", engine.loadLootTableYaml)
    addYamlDir("data/locations",  "Loading locations...",  engine.loadLocationYaml)
end

function startupLoader.build(profile)
    startupLoader.items     = {}
    startupLoader.processed = 0
    startupLoader.done      = false
    startupLoader.profile   = profile or "normal"

    if startupLoader.profile == "arena" then
        queueArenaProfile()
    else
        queueNormalProfile()
    end

    startupLoader.built = true
    startupLoader.currentLabel = "Loading..."
    engine.logInfo("Startup loader queued " .. #startupLoader.items
        .. " items for profile " .. startupLoader.profile)
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

function startupLoader.runAll()
    if startupLoader.done or not startupLoader.built then return end
    while not startupLoader.done do
        startupLoader.tick(0)
    end
end

function startupLoader.reset()
    startupLoader.items        = {}
    startupLoader.processed    = 0
    startupLoader.currentLabel = "Initializing..."
    startupLoader.built        = false
    startupLoader.done         = false
    startupLoader.profile      = "normal"
end

return startupLoader
