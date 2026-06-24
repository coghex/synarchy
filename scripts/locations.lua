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
-- terrain primitives below. Phase 3 replaces the hardcoded DEFS with
-- data-driven defs from data/locations/*.yaml.

local locations = {}

-- Registry of available locations. `name` is the id passed to stamp();
-- `label` / `note` are for the debug list. Hardcoded for now.
local DEFS = {
    { name  = "room_small",
      label = "room_small",
      note  = "small underground room: stairs down, walls/floor/ceiling, chest" },
}

function locations.list()
    return DEFS
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

-- STUB — the room layout is authored here. It needs the dungeon
-- materials (dungeon_floor / dungeon_wall / ...) and the door / chest /
-- torch defs to exist before it can reference them, so for now it just
-- logs. Fill in: carve interior, lay floor / walls / ceiling, punch a
-- staircase up to the surface, place door + chest + torch.
function builders.room_small(worldId, gx, gy)
    engine.logInfo("locations: room_small stamp at " .. gx .. "," .. gy
                   .. " on '" .. tostring(worldId)
                   .. "' — geometry TODO (awaiting dungeon textures/defs)")
    -- TODO(room layout)
end

-- Stamp location `name`, anchored at tile (gx, gy) on page `worldId`.
-- Returns true if the location id was recognised.
function locations.stamp(name, gx, gy, worldId)
    local b = builders[name]
    if not b then
        engine.logWarn("locations: unknown location '" .. tostring(name) .. "'")
        return false
    end
    b(worldId, math.floor(gx), math.floor(gy))
    return true
end

return locations
