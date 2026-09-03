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
-- #2203: the retained TERMINAL failure, or nil. Set by exactly one
-- family aggregate (the first to fail) and cleared only by build/reset.
startupLoader.failure       = nil

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
-- addYamlDir/addYamlDirCanonical/addYamlTree. Those three call shapes
-- are parsed verbatim by `tools/save_compat_migration_probe.py`, which
-- derives production's registry bootstrap from `queueNormalProfile`'s
-- source; keeping them at exactly three arguments is what keeps that
-- check working.
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
    local failed, refused = {}, {}
    for _, path in ipairs(paths) do
        addItem(label, function()
            -- #2203: the SECOND argument opts this ONE call site in to
            -- the binding's parse outcome. Every other caller passes
            -- the path alone and still gets exactly one number back.
            --
            -- #2241 added an optional THIRD value, pushed only when a
            -- binding refused a whole file on a post-decode SEMANTIC
            -- collision (today: a duplicate flora name). `parsed` keeps
            -- its decode-only meaning -- such a file decoded perfectly
            -- well -- so the arity a healthy call answers with is
            -- unchanged at two, and eleven of the twelve families never
            -- push a third value at all.
            local n, parsed, refusal = loaderFn(path, true)
            total = total + asCount(n)
            seen  = seen + 1
            -- `~= true`, not `== false`: a queued YAML binding that
            -- answers no outcome at all has not reported that its file
            -- parsed, and this loader's whole job is to stop guessing.
            if parsed ~= true then
                failed[#failed + 1] = path
            elseif refusal then
                refused[#refused + 1] = { path = path,
                                          name = tostring(refusal) }
            end
        end)
    end
    addItem(label, function()
        -- The family boundary is the fail-fast boundary (#2203): every
        -- discovered file has already run, so the aggregate goes out
        -- FIRST, unchanged in spelling and in what it counts -- with
        -- the healthy totals and the original discovered-file count --
        -- and only then does the queue stop. A zero-file family emits
        -- its zero aggregate the same way before failing.
        engine.logInfo(startupLoader.aggregateMessage(family, total, seen))
        if #failed > 0 then
            startupLoader.fail({ family = family, dir = dir,
                                 kind = "parse", paths = failed,
                                 path = failed[1], failedCount = #failed,
                                 files = seen })
        elseif #refused > 0 then
            -- A refused file registered NOTHING, so continuing would
            -- reach the main menu with a family the author believes is
            -- loaded. Terminal, exactly like a parse failure -- and the
            -- message names both the file and the colliding name,
            -- because "some file in data/flora has a duplicate" is not
            -- a diagnostic anyone can act on.
            startupLoader.fail({ family = family, dir = dir,
                                 kind = "duplicate",
                                 path = refused[1].path,
                                 name = refused[1].name,
                                 paths = refused,
                                 failedCount = #refused,
                                 files = seen })
        elseif seen == 0 then
            startupLoader.fail({ family = family, dir = dir,
                                 kind = "empty", failedCount = 0,
                                 files = 0 })
        end
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

-- Flat counterpart of addYamlTree: one directory, no recursion, but
-- the same canonicalFileOrder over what engine.listFiles enumerated
-- (#2241).
--
-- A family uses this when its per-file load ORDER is observable in the
-- shipped product. data/flora is the one that is: its definitions are
-- assigned sequential FloraIds as they register, those ids are what a
-- save's numeric flora references name, and engine.listFiles hands
-- back raw filesystem order, which differs between machines. Sorting
-- happens HERE, in flora's own consumer, rather than inside
-- engine.listFiles -- every other flat family keeps the enumeration it
-- has always had.
--
-- Deliberately spelled as an `addYaml...(dir, label, loaderFn)` call
-- with exactly three arguments, like its two siblings, because
-- `tools/save_compat_migration_probe.py` reads queueNormalProfile's
-- source to derive production's registry bootstrap.
local function addYamlDirCanonical(dir, label, loaderFn)
    local files = engine.listFiles(dir, ".yaml")
    local paths = {}
    if files then
        for _, fname in ipairs(startupLoader.canonicalFileOrder(files)) do
            paths[#paths + 1] = dir .. "/" .. fname
        end
    end
    addYamlFamily(dir, label, loaderFn, paths)
end

-- Recursive counterpart of addYamlDir: enqueues every YAML under `dir`
-- at ANY depth, ONE queue entry per file (so `loaderFn` still sees each
-- file exactly once and the loading screen keeps its per-file progress
-- granularity), in canonicalFileOrder.
--
-- Only trees whose contents may be organized into subdirectories use
-- this. A flat family whose load order is observable uses
-- addYamlDirCanonical instead; everything else stays on addYamlDir's
-- flat, OS-ordered engine.listFiles, because its ids come from each
-- definition's own `name:` and nothing downstream can see the order.
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

-- `policy` is engine.loadTexture's upload policy (#2075) and is
-- MANDATORY here, passed through verbatim. A preload must declare the
-- SAME policy its eventual consumer declares: the cache is keyed by
-- (path, policy), so preloading under the wrong one uploads a slot
-- nobody will ever sample and leaves the consumer to upload the real one
-- anyway. Required rather than defaulted because engine.loadTexture
-- refuses a present-but-nil policy — a forgotten argument here would
-- otherwise turn into a whole texture family that silently never loads.
local function requirePolicy(who, policy)
    if policy ~= "ui" and policy ~= "scene" then
        error(who .. ": upload policy must be \"ui\" or \"scene\", got "
              .. tostring(policy), 2)
    end
end

local function addTextureList(label, paths, policy)
    requirePolicy("addTextureList", policy)
    for _, p in ipairs(paths) do
        addItem(label, function() engine.loadTexture(p, policy) end)
    end
end

local function addTextureDir(dir, label, policy)
    requirePolicy("addTextureDir", policy)
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
    -- Flora is the one FLAT family loaded in canonical byte order
    -- (#2241): its FloraIds are allocated as definitions register, and
    -- a save's WorldEdit/CropPlot/PlantDesignation rows carry those
    -- numbers, so an OS-dependent enumeration would have made the same
    -- shipped catalog mean different things on different machines.
    addYamlDirCanonical("data/flora", "Loading flora...",     engine.loadFloraYaml)
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
    startupLoader.failure   = nil
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
    -- #2203: a failed startup is TERMINAL. Returning here is what keeps
    -- a repeated tick -- the loading screen calls one every frame --
    -- from advancing progress past the family that failed or logging
    -- its error a second time.
    if startupLoader.failure then return end
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
        -- Checked per ITEM, not per tick: nothing queued after the
        -- failing family -- another family's files, the tutorial tree,
        -- a texture preload -- may run, and itemsPerTick is 4.
        if startupLoader.failure then
            startupLoader.currentLabel = startupLoader.failure.message
            return
        end
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

-- THE spelling of the terminal failure line (#2203), exposed for the
-- same reason aggregateMessage is: a test or probe reads it from here
-- rather than restating it. Both shapes name the FAMILY; a parse
-- failure names the file that failed, and a family that discovered
-- nothing names the directory it looked in, because there is no file.
function startupLoader.failureMessage(info)
    if info.kind == "empty" then
        return string.format(
            "Startup failed: %s discovered no YAML files in %s",
            info.family, info.dir)
    end
    if info.kind == "duplicate" then
        return string.format(
            "Startup failed: %s refused %s (duplicate definition name "
            .. "'%s'; %d of %d file(s) refused)",
            info.family, info.path, info.name, info.failedCount, info.files)
    end
    return string.format(
        "Startup failed: %s could not parse %s (%d of %d file(s) failed)",
        info.family, info.path, info.failedCount, info.files)
end

-- Enter the terminal failed state. Idempotent by design: the FIRST
-- family to fail is the one reported, and its one error-level line is
-- logged exactly once.
function startupLoader.fail(info)
    if startupLoader.failure then return end
    info.message = startupLoader.failureMessage(info)
    startupLoader.failure = info
    startupLoader.currentLabel = info.message
    engine.logError(info.message)
end

function startupLoader.isFailed()
    return startupLoader.failure ~= nil
end

-- The retained failure, or nil. Fields: `family`, `dir`, `kind`
-- ("empty" | "parse"), `files` (how many were discovered),
-- `failedCount`, `message`, and for a parse failure `path` (the first
-- failure in the family's own queue order) and `paths` (all of them).
function startupLoader.getFailure()
    return startupLoader.failure
end

-- Drain the whole queue synchronously (the arena profile's boot). The
-- failure test is what makes this terminate rather than spin: `done`
-- deliberately never becomes true once a family has failed.
function startupLoader.runAll()
    if startupLoader.done or not startupLoader.built then return end
    while not startupLoader.done and not startupLoader.failure do
        startupLoader.tick(0)
    end
end

function startupLoader.reset()
    startupLoader.items        = {}
    startupLoader.processed    = 0
    startupLoader.currentLabel = "Initializing..."
    startupLoader.built        = false
    startupLoader.done         = false
    startupLoader.failure      = nil
    startupLoader.profile      = "normal"
end

return startupLoader
