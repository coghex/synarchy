-- Location stamper (#89)
--
-- The engine places data-driven locations (#88) into chunks during world
-- generation and carries the resulting overlay in the world's gen params.
-- This module materializes that overlay into actual geometry: on the
-- engine's onWorldReady broadcast (fired ONLY for a freshly generated
-- world, never a save-load) it records the world's placements and, as each
-- location's chunk loads, calls the #88 Lua builder through locations.stamp.
--
-- The stamps land in the edit log (world.setCell / structure.place), so they
-- persist with the save and replay on chunk reload — which is exactly why a
-- loaded world must NOT be re-stamped: the engine never fires onWorldReady
-- for the save-load path, so its restored edits are the single source.
--
-- Stamping needs the location's chunk loaded — the builders read the terrain
-- surface for the floor height — so this runs from update(): force-load each
-- pending chunk, then stamp once it reports loaded. We only act on the world
-- that is currently ACTIVE, because the per-tile reads the builders use
-- (getTerrainAt / getChunkInfo) resolve the active page.

local stamper = {}

local locations = require("scripts.locations")

-- Pending stamps: list of { id, gx, gy, cx, cy, worldId, requested }.
stamper.pending = {}

-- Fired by the engine when a fresh world finishes generating.
function stamper.onWorldReady(worldId)
    local placed = world.listPlacedLocations(worldId) or {}
    for _, e in ipairs(placed) do
        stamper.pending[#stamper.pending + 1] = {
            id = e.id, gx = e.gx, gy = e.gy, cx = e.cx, cy = e.cy,
            worldId = worldId, requested = false,
        }
    end
    if #placed > 0 then
        engine.logInfo("location_stamper: queued " .. #placed
            .. " location(s) for " .. tostring(worldId))
    end
end

function stamper.update(dt)
    if #stamper.pending == 0 then return end
    local active = world.getActiveWorldId()
    local still = {}
    for _, p in ipairs(stamper.pending) do
        if p.worldId ~= active then
            -- Not the active page; per-tile reads would hit the wrong
            -- world. Wait until this page is shown.
            still[#still + 1] = p
        else
            local info = world.getChunkInfo(p.cx, p.cy)
            if info and info.loaded then
                locations.stamp(p.id, p.gx, p.gy, p.worldId)
            else
                if not p.requested then
                    world.loadChunksInRegion(p.cx, p.cy, p.cx, p.cy)
                    p.requested = true
                end
                still[#still + 1] = p   -- still loading; retry next tick
            end
        end
    end
    stamper.pending = still
end

-- Count of locations still waiting to stamp (used by the headless probe).
function stamper.pendingCount()
    return #stamper.pending
end

return stamper
