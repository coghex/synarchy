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

local locations = {}

-----------------------------------------------------------
-- Definition registry (engine-backed, #88)
-----------------------------------------------------------
-- The defs live in the engine LocationRegistry (loaded from
-- data/locations/*.yaml). We query the engine each call so the list
-- always reflects what's registered — no local cache to invalidate.
-- Each engine LocationDef table is:
--   { id, label, type, builder, anchor={tag,…},
--     contents={{kind,id,count},…} }.

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
--     id }       -- LocationDef id (join with locations.getDef for
--                --   label/type/builder)
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

-----------------------------------------------------------
-- Builders
-----------------------------------------------------------
-- One function per location id. Authored on top of the primitives
-- above + content spawns (building.spawn / item.spawnGround /
-- unit.spawn). Anchor is the clicked tile (gx, gy); the builder decides
-- how the structure is laid out relative to it.

local builders = {}

-- A small rectangular room built from the STRUCTURE pieces (floor / wall /
-- post / ceiling — the RCT-style edge subsystem), NOT terrain voxels. The
-- click tile is the room CENTRE. Order matters: floors first (posts gate on a
-- floor), then corner posts (each caps the two perimeter walls meeting there),
-- then the inward-facing perimeter walls (which cap to those posts).
--
-- Perimeter edge per side: −gx = nw, +gx = se, −gy = ne, +gy = sw.
-- worldId is unused for now — the structure store is global (Stage-1 limit).
-- Ceiling is OFF by default so the interior stays visible while iterating.
local ROOM_SMALL_RADIUS = 2   -- → 5×5 footprint

function builders.room_small(worldId, gx, gy, withCeiling)
    local S = require("scripts.structures")
    local r = ROOM_SMALL_RADIUS
    local x0, x1 = gx - r, gx + r
    local y0, y1 = gy - r, gy + r

    -- 1. floor across the whole footprint. worldId threads through so a
    --    location stamped on a hidden/non-active page reads THAT page's
    --    terrain height, not the active world's (#89 multiworld).
    for x = x0, x1 do
        for y = y0, y1 do S.floor(x, y, worldId) end
    end

    -- 2. corner posts (cap the two perimeter walls that meet at each)
    S.post(x0, y0, "n", worldId)   -- nw + ne meet
    S.post(x1, y0, "e", worldId)   -- ne + se meet
    S.post(x1, y1, "s", worldId)   -- se + sw meet
    S.post(x0, y1, "w", worldId)   -- sw + nw meet

    -- 3. perimeter walls (after posts so they cap to them)
    for y = y0, y1 do
        S.wall(x0, y, "nw", worldId)   -- −gx side
        S.wall(x1, y, "se", worldId)   -- +gx side
    end
    for x = x0, x1 do
        S.wall(x, y0, "ne", worldId)   -- −gy side
        S.wall(x, y1, "sw", worldId)   -- +gy side
    end

    -- 4. ceiling (optional)
    if withCeiling then
        for x = x0, x1 do
            for y = y0, y1 do S.ceiling(x, y, worldId) end
        end
    end

    engine.logInfo(string.format("locations: room_small %dx%d at %d,%d%s",
        x1 - x0 + 1, y1 - y0 + 1, gx, gy,
        withCeiling and " (+ceiling)" or ""))
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
    b(worldId, math.floor(gx), math.floor(gy))
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

return locations
