-- Locations module
--
-- Premade structures ("locations") stamped into the world: an
-- underground room to start, with outposts / villages / dungeons to
-- follow. Each location is built from `world.setCell` terrain edits
-- (the WeSetCell primitive — interior air, walls, ceilings, stairs;
-- persists via the edit log) plus content spawns (buildings / units /
-- ground items, which persist via their own snapshots).
--
-- Phase 2 scaffolding: the debug overlay's "locations" section calls
-- `locations.list()` to enumerate these and `locations.stamp(name, gx,
-- gy, worldId)` when the user clicks the ground. The per-location
-- geometry (the `builders.*` functions) is authored on top of the
-- terrain primitives below. Definitions are now data-driven: they come
-- from data/locations/*.yaml, loaded at boot via engine.loadLocationYaml
-- and read back through engine.listLocationDefs (#88).
--
-- Content spawning (#90): scripts/location_stamper.lua calls
-- locations.spawnContents(id, gx, gy, worldId) once per chunk load,
-- independent of whether the geometry was (re)built this call. It
-- dispatches each `contents` entry to unit.spawn / item.spawnGround /
-- building.spawn / loot.roll / a builder (for nested "structure"
-- content), gated by its own one-time engine flag
-- (world.hasSpawnedLocationContents) so contents are never re-spawned.

local locations = {}

-----------------------------------------------------------
-- Definition registry (engine-backed, #88)
-----------------------------------------------------------
-- The defs live in the engine LocationRegistry (loaded from
-- data/locations/*.yaml). We query the engine each call so the list
-- always reflects what's registered — no local cache to invalidate.
-- Each engine LocationDef table is:
--   { id, label, type, builder, anchor={tag,…},
--     bounds={min_x,min_y,max_x,max_y}, discovery_margin,
--     contents={{kind,id,count},…} }.
-- `bounds` (#777) is the authoritative footprint, relative to the
-- anchor tile: the builders and content scatter below both derive
-- their geometry from it instead of an independent radius constant.

-- All registered location defs, in registration order.
function locations.listDefs()
    return engine.listLocationDefs() or {}
end

-- A single def by id, or nil.
function locations.getDef(id)
    for _, d in ipairs(locations.listDefs()) do
        if d.id == id then return d end
    end
    return nil
end

-----------------------------------------------------------
-- World-gen placement overlay (#89)
-----------------------------------------------------------
-- The engine places locations into chunks during world generation
-- (deterministic from the seed) and carries the result in the world's
-- gen params, so it survives save/load. listPlaced() reads back that
-- overlay for the ACTIVE world. Each entry:
--   { cx, cy,    -- chunk coordinate
--     gx, gy,    -- chunk-centre tile (anchor for stamping)
--     id,        -- LocationDef id (join with locations.getDef for
--                --   label/type/builder)
--     bounds,    -- absolute, inclusive tile bounds (#777), or nil if
--                --   `id` has no matching registered def
--     discovery_margin }
-- With no argument the active world is read; pass a page id to read a
-- specific world's overlay. Returns {} when no such world or nothing placed.
function locations.listPlaced(worldId)
    return world.listPlacedLocations(worldId) or {}
end

-- Debug-overlay list shape: { name=id, label, note }. The overlay keys
-- armed locations + stamp() on `name`, so name carries the def id.
function locations.list()
    local out = {}
    for _, d in ipairs(locations.listDefs()) do
        out[#out + 1] = {
            name  = d.id,
            label = d.label,
            note  = d.type,
        }
    end
    return out
end

-----------------------------------------------------------
-- Terrain primitives
-----------------------------------------------------------
-- All take an explicit `worldId` (page) so they work on the flat arena
-- and a generated world alike. z grows upward; mat "air" (or 0) clears
-- a cell. These wrap world.setCell (WeSetCell): every call lands in the
-- edit log and persists like a player edit.

function locations.setCell(worldId, gx, gy, z, mat)
    world.setCell(worldId, gx, gy, z, mat)
end

-- Fill the solid box [x0..x1] x [y0..y1] x [z0..z1] with `mat`.
-- Use mat = "air" to carve empty space.
function locations.fillBox(worldId, x0, y0, z0, x1, y1, z1, mat)
    for gx = x0, x1 do
        for gy = y0, y1 do
            for z = z0, z1 do
                world.setCell(worldId, gx, gy, z, mat)
            end
        end
    end
end

-- Carve the box to air (convenience wrapper over fillBox).
function locations.carveBox(worldId, x0, y0, z0, x1, y1, z1)
    locations.fillBox(worldId, x0, y0, z0, x1, y1, z1, "air")
end

-- Lay a single flat z-layer [x0..x1] x [y0..y1] at height z of `mat`
-- (floors, ceilings).
function locations.fillLayer(worldId, x0, y0, x1, y1, z, mat)
    locations.fillBox(worldId, x0, y0, z, x1, y1, z, mat)
end

-- Build the four vertical walls of the box [x0..x1] x [y0..y1] from
-- z0..z1 of `mat` (a hollow rectangular shell, no floor/ceiling — those
-- are fillLayer). The interior is left untouched (carve it separately).
function locations.wallRing(worldId, x0, y0, x1, y1, z0, z1, mat)
    for z = z0, z1 do
        for gx = x0, x1 do
            world.setCell(worldId, gx, y0, z, mat)
            world.setCell(worldId, gx, y1, z, mat)
        end
        for gy = y0, y1 do
            world.setCell(worldId, x0, gy, z, mat)
            world.setCell(worldId, x1, gy, z, mat)
        end
    end
end

-- Level the terrain across the box [x0..x1]×[y0..y1] to the LOWEST base
-- elevation in it, so a stamped room sits flat instead of following the bumps
-- under it. The base elevation is the terrain-only surface (getTerrainAt's 2nd
-- value), so sub-tile slopes ON TOP are excluded from the min; every cell
-- above the target level is carved to air and any surface slope is dropped.
-- Returns the level it flattened to — the structure floor then sits one above.
--
-- NB the tile edits are async (queued to the world thread), so getTerrainAt
-- still reads the OLD heights for the rest of THIS call. Builders must place
-- their pieces at the returned level explicitly rather than re-reading terrain.
function locations.flattenFootprint(worldId, x0, y0, x1, y1)
    local lo, hi
    for gx = x0, x1 do
        for gy = y0, y1 do
            local _, tz = world.getTerrainAt(gx, gy, worldId)
            tz = tz or 0
            if lo == nil or tz < lo then lo = tz end
            if hi == nil or tz > hi then hi = tz end
        end
    end
    lo = lo or 0
    hi = hi or lo
    for gx = x0, x1 do
        for gy = y0, y1 do
            for z = lo + 1, hi do
                world.setCell(worldId, gx, gy, z, "air")
            end
            world.setSlope(worldId, gx, gy, lo, 0)   -- 0 bits = flat
        end
    end
    return lo
end

-----------------------------------------------------------
-- Builders
-----------------------------------------------------------
-- One function per location id, authored on top of the primitives
-- above. Anchor is the clicked tile (gx, gy); the builder decides how
-- the structure is laid out relative to it. Content spawning (#90) is
-- a separate concern — see locations.spawnContents below.

local builders = {}

-- A small rectangular room built from the STRUCTURE pieces (floor / wall /
-- post / ceiling — the RCT-style edge subsystem), NOT terrain voxels. The
-- click tile is the room CENTRE. Order matters: floors first (posts gate on a
-- floor), then corner posts (each caps the two perimeter walls meeting there),
-- then the inward-facing perimeter walls (which cap to those posts).
--
-- Perimeter edge per side: −gx = nw, +gx = se, −gy = ne, +gy = sw.
-- worldId selects the page; baseZ (from flattenFootprint) is the levelled
-- ground the pieces sit on. `def` is the resolved LocationDef (buildAt
-- always supplies one — this is only ever reached through locations.build/
-- stamp); its `bounds` (#777) gives the footprint, so a 5x5 room is simply
-- whatever bounds the def declares, not a hardcoded radius.
function builders.room_small(worldId, gx, gy, def)
    local S = require("scripts.structures")
    local b = def.bounds
    local x0, x1 = gx + b.min_x, gx + b.max_x
    local y0, y1 = gy + b.min_y, gy + b.max_y

    -- 0. level the ground so the room is flat: flatten the footprint to its
    --    lowest base elevation and build every piece at that explicit z. (The
    --    flatten edits are async, so re-reading terrain this call would still
    --    see the old bumps — hence baseZ is threaded through, not re-read.)
    local baseZ = locations.flattenFootprint(worldId, x0, y0, x1, y1)

    -- 1. floor across the whole footprint at the levelled height
    for x = x0, x1 do
        for y = y0, y1 do S.floor(x, y, worldId, baseZ) end
    end

    -- 2. corner posts (cap the two perimeter walls that meet at each)
    S.post(x0, y0, "n", worldId, baseZ)   -- nw + ne meet
    S.post(x1, y0, "e", worldId, baseZ)   -- ne + se meet
    S.post(x1, y1, "s", worldId, baseZ)   -- se + sw meet
    S.post(x0, y1, "w", worldId, baseZ)   -- sw + nw meet

    -- 3. perimeter walls (after posts so they cap to them)
    for y = y0, y1 do
        S.wall(x0, y, "nw", worldId, baseZ)   -- −gx side
        S.wall(x1, y, "se", worldId, baseZ)   -- +gx side
    end
    for x = x0, x1 do
        S.wall(x, y0, "ne", worldId, baseZ)   -- −gy side
        S.wall(x, y1, "sw", worldId, baseZ)   -- +gy side
    end

    -- No ceiling by default, so the interior stays visible while iterating.

    engine.logInfo(string.format("locations: room_small %dx%d at %d,%d",
        x1 - x0 + 1, y1 - y0 + 1, gx, gy))
end

-- A partially-collapsed room_small (#91): same 5×5 footprint and piece
-- order, but every piece uses the pack's "damaged" variant art, and the
-- perimeter is BREACHED — one side loses a contiguous run of 2–3 wall
-- segments, 1–2 stray segments fall elsewhere, and one corner post is
-- gone (leaving its walls' ends uncapped, which reads as a broken edge).
-- Wall pieces are cosmetic overlays (no collision), so every gap is
-- walkable. All 25 floors are kept — texture damage only — because the
-- stamper keys "already materialized" on the ANCHOR floor, and content
-- scatter expects the interior intact.
--
-- The collapse pattern is a deterministic function of the anchor (a tiny
-- Park–Miller PRNG seeded from gx,gy), so each ruin falls apart in its
-- own way, but a rebuild of the same ruin collapses identically.
local function collapseRng(gx, gy)
    local s = (gx * 73856093 + gy * 19349663) % 2147483647
    if s <= 0 then s = s + 2147483646 end
    return function(n)   -- uniform 1..n
        s = (s * 48271) % 2147483647
        return (s % n) + 1
    end
end

-- `def` is the resolved LocationDef (buildAt always supplies one); its
-- `bounds` (#777) is this ruin's authoritative footprint — the same box
-- reported by engine.listLocationDefs / world.listPlacedLocations, not
-- a second, independently-tracked radius.
function builders.room_small_damaged(worldId, gx, gy, def)
    local S = require("scripts.structures")
    local b = def.bounds
    local x0, x1 = gx + b.min_x, gx + b.max_x
    local y0, y1 = gy + b.min_y, gy + b.max_y
    local rand = collapseRng(gx, gy)
    local baseZ = locations.flattenFootprint(worldId, x0, y0, x1, y1)

    -- 1. floor across the whole footprint (damaged art, none missing)
    for x = x0, x1 do
        for y = y0, y1 do S.floor(x, y, worldId, baseZ, "damaged") end
    end

    -- 2. corner posts, minus one collapsed corner (1=n 2=e 3=s 4=w)
    local lostPost = rand(4)
    local posts = { { x0, y0, "n" }, { x1, y0, "e" },
                    { x1, y1, "s" }, { x0, y1, "w" } }
    for i, p in ipairs(posts) do
        if i ~= lostPost then S.post(p[1], p[2], p[3], worldId, baseZ, "damaged") end
    end

    -- 3. perimeter walls, minus the breach + strays. Sides are indexed
    --    1=nw 2=se 3=ne 4=sw; segment index i runs 0..4 along the side.
    local breachSide = rand(4)
    local breachLen  = 1 + rand(2)              -- 2..3 contiguous segments
    local breachAt   = rand(6 - breachLen) - 1  -- 0-based start, fits in 0..4
    local strays = {}
    for _ = 1, rand(2) do
        strays[#strays + 1] = { side = rand(4), i = rand(5) - 1 }
    end
    local function collapsed(side, i)
        if side == breachSide and i >= breachAt and i < breachAt + breachLen then
            return true
        end
        for _, st in ipairs(strays) do
            if st.side == side and st.i == i then return true end
        end
        return false
    end
    for y = y0, y1 do
        if not collapsed(1, y - y0) then S.wall(x0, y, "nw", worldId, baseZ, "damaged") end
        if not collapsed(2, y - y0) then S.wall(x1, y, "se", worldId, baseZ, "damaged") end
    end
    for x = x0, x1 do
        if not collapsed(3, x - x0) then S.wall(x, y0, "ne", worldId, baseZ, "damaged") end
        if not collapsed(4, x - x0) then S.wall(x, y1, "sw", worldId, baseZ, "damaged") end
    end
    -- no ceiling — the roof fell in long ago

    engine.logInfo(string.format(
        "locations: room_small_damaged %dx%d at %d,%d (breach side %d len %d)",
        x1 - x0 + 1, y1 - y0 + 1, gx, gy, breachSide, breachLen))
end

-- Resolve location `id` to its def, then call the builder it names.
-- Returns true if the id was recognised and built.
local function buildAt(id, gx, gy, worldId)
    local def = locations.getDef(id)
    if not def then
        engine.logWarn("locations: unknown location '" .. tostring(id) .. "'")
        return false
    end
    local b = builders[def.builder]
    if not b then
        engine.logWarn("locations: location '" .. id ..
            "' names unknown builder '" .. tostring(def.builder) .. "'")
        return false
    end
    b(worldId, math.floor(gx), math.floor(gy), def)
    return true
end

-- locations.build(id, gx, gy) — look up the def by id and call its
-- builder, stamping on the active world page (#88).
function locations.build(id, gx, gy)
    local hud = require("scripts.hud")
    local worldId = (hud and hud.worldId) or "test_arena"
    return buildAt(id, gx, gy, worldId)
end

-- Stamp location `id`, anchored at tile (gx, gy) on an explicit page
-- `worldId`. The debug-overlay entry point (it knows the page).
function locations.stamp(id, gx, gy, worldId)
    return buildAt(id, gx, gy, worldId)
end

-----------------------------------------------------------
-- Content spawning (#90)
-----------------------------------------------------------
-- Each LocationDef.contents entry (see data/locations/*.yaml):
--   { kind, id, count, rolls, position = {x,y} | nil, faction | nil }
-- `position` is a fixed offset from the anchor; when absent the entry
-- scatters randomly within the location's footprint instead (a fresh
-- roll per instance). `count` is how many to place ("loot_table" uses
-- `rolls` instead — how many times to roll the table). `faction` is
-- unit-only and defaults to "hostile".
--
-- Called once per chunk load by scripts/location_stamper.lua,
-- regardless of whether the geometry was (re)built this call — gated
-- by its OWN one-time engine flag (world.hasSpawnedLocationContents),
-- independent of the structure.hasAt check that gates re-stamping.
-- That independence matters: a floor-less location type would
-- otherwise re-run every load, and a player demolishing the floor
-- would otherwise re-trigger every content spawn too.

-- Scatter within the def's own authoritative bounds (#777) — no
-- independent per-builder radius table to keep in sync with it.
local function contentOffset(def, entry)
    if entry.position then
        return entry.position.x or 0, entry.position.y or 0
    end
    local b = def.bounds
    return math.random(b.min_x, b.max_x), math.random(b.min_y, b.max_y)
end

local function spawnUnitContent(def, entry, gx, gy, worldId)
    local faction = entry.faction or "hostile"
    for _ = 1, (entry.count or 1) do
        local ox, oy = contentOffset(def, entry)
        local uid = unit.spawn(entry.id, gx + ox, gy + oy, nil, faction, worldId)
        if uid == -1 then
            engine.logWarn("locations: unknown unit content '" ..
                tostring(entry.id) .. "'")
        end
    end
end

-- NB item.spawnGround takes an explicit pageId (#90) so this works on
-- a hidden secondary page, same as unit.spawn / structure.place.
local function spawnItemContent(def, entry, gx, gy, worldId)
    for _ = 1, (entry.count or 1) do
        local ox, oy = contentOffset(def, entry)
        local gid = item.spawnGround(entry.id, gx + ox, gy + oy, nil, worldId)
        if not gid then
            engine.logWarn("locations: unknown item content '" ..
                tostring(entry.id) .. "'")
        end
    end
end

local function spawnLootTableContent(def, entry, gx, gy, worldId)
    for _ = 1, (entry.rolls or 1) do
        local itemId = loot.roll(entry.id)
        if not itemId then
            engine.logWarn("locations: unknown loot table '" ..
                tostring(entry.id) .. "'")
        else
            local ox, oy = contentOffset(def, entry)
            local gid = item.spawnGround(itemId, gx + ox, gy + oy, nil, worldId)
            if not gid then
                engine.logWarn("locations: loot table '" .. tostring(entry.id) ..
                    "' rolled unknown item id '" .. tostring(itemId) .. "'")
            end
        end
    end
end

-- building.spawn takes an explicit pageId (#90) so this validates
-- occupancy/terrain-Z against — and spawns onto — the location's own
-- page, same as unit.spawn / item.spawnGround / structure.place.
local function spawnBuildingContent(def, entry, gx, gy, worldId)
    for _ = 1, (entry.count or 1) do
        local ox, oy = contentOffset(def, entry)
        local bid = building.spawn(entry.id, gx + ox, gy + oy, worldId)
        if not bid then
            engine.logWarn("locations: building content '" ..
                tostring(entry.id) .. "' failed to spawn (unknown id or unplaceable)")
        end
    end
end

-- A "structure" content entry nests another builder's geometry at an
-- offset from the anchor — `id` names a scripts.locations builder
-- (the same names LocationDef.builder uses), not a location id.
local function spawnStructureContent(def, entry, gx, gy, worldId)
    local b = builders[entry.id]
    if not b then
        engine.logWarn("locations: content structure names unknown builder '" ..
            tostring(entry.id) .. "'")
        return
    end
    local ox, oy = contentOffset(def, entry)
    -- The nested builder has no LocationDef of its own (`id` here names a
    -- builder function, not a registered location) — its `bounds`-driven
    -- geometry uses the OUTER def's, the only bounds in scope at a nested
    -- content entry.
    b(worldId, gx + ox, gy + oy, def)
end

local function dispatchContent(def, entry, gx, gy, worldId)
    local kind = entry.kind
    if kind == "unit" then
        spawnUnitContent(def, entry, gx, gy, worldId)
    elseif kind == "item" then
        spawnItemContent(def, entry, gx, gy, worldId)
    elseif kind == "loot_table" then
        spawnLootTableContent(def, entry, gx, gy, worldId)
    elseif kind == "building" then
        spawnBuildingContent(def, entry, gx, gy, worldId)
    elseif kind == "structure" then
        spawnStructureContent(def, entry, gx, gy, worldId)
    else
        engine.logWarn("locations: unknown content kind '" ..
            tostring(kind) .. "'")
    end
end

-- Spawn location `id`'s contents, anchored at (gx, gy) on page
-- `worldId` — once, ever, for this chunk. Safe to call on every chunk
-- load: a no-op once world.hasSpawnedLocationContents is true.
function locations.spawnContents(id, gx, gy, worldId)
    gx, gy = math.floor(gx), math.floor(gy)
    if world.hasSpawnedLocationContents(gx, gy, worldId) then return end
    local def = locations.getDef(id)
    if def then
        for _, entry in ipairs(def.contents or {}) do
            dispatchContent(def, entry, gx, gy, worldId)
        end
    else
        engine.logWarn("locations: unknown location '" .. tostring(id) ..
            "' (content spawn)")
    end
    world.markLocationContentsSpawned(gx, gy, worldId)
end

return locations
