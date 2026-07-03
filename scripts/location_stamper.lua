-- Location stamper (#89)
--
-- The engine places data-driven locations (#88) into chunks during world
-- generation and carries the resulting overlay in the world's gen params
-- (which serializes into the save). This module materializes that overlay
-- into geometry: the engine dispatches onStampLocation for every load of a
-- chunk that hosts a placed location, so locations stamp lazily as their
-- chunks load, in any session, driven only by the persisted overlay.
--
-- That is what makes a location robust to save timing: there is no async
-- queue to drain. Even a world saved before a chunk was ever stamped
-- re-materializes that location when its chunk next loads — the overlay
-- always rides the save, and the chunk-load trigger always consults it.
--
-- Idempotency (#424): a dedicated persisted marker — world.hasStampedLocation
-- / world.markLocationStamped, keyed by chunk like the #90 content-spawn
-- flag below — tracks whether this chunk's location has been stamped.
-- Earlier this inferred "already stamped" from structure.hasAt(gx, gy,
-- "floor"), which stamping's own edit-log replay keeps true across a normal
-- reload — but a player who later clears the anchor floor tile (an
-- ordinary, otherwise-editable structure piece) made that check go false
-- again, so the next chunk load re-ran the builder and clobbered whatever
-- of the location the player had edited. The dedicated flag is set once,
-- on first stamp, and is never touched by structure edits.
--
-- Multiworld: the builders read terrain with an explicit page id
-- (locations.stamp -> the #88 builder -> world.getTerrainAt(gx,gy,pageId)),
-- so a location materializes on its own page even when that page is hidden /
-- not the active one — there is no active-page gate here.
--
-- Content spawning (#90): spawnContents is called EVERY time, regardless
-- of whether stamp() ran this call — it has its own persisted one-time
-- flag (world.hasSpawnedLocationContents), independent of structure.hasAt.
-- A geometry-only skip does not imply contents already spawned: a
-- floor-less location type never satisfies structure.hasAt, and a player
-- demolishing the floor would otherwise re-trigger a full re-stamp.

local stamper = {}

local locations = require("scripts.locations")

-- Fired by the engine for a just-loaded chunk that hosts a placed location.
function stamper.onStampLocation(pageId, locId, gx, gy)
    gx, gy = math.floor(gx), math.floor(gy)
    -- Already materialized ON THIS PAGE (stamped earlier this session, or
    -- on a prior load this session/save)? Then this is a repeat load —
    -- skip. The pageId is essential: without it the check resolves to the
    -- active world, so unrelated state there could suppress a valid stamp
    -- on a hidden secondary page.
    if not world.hasStampedLocation(gx, gy, pageId) then
        locations.stamp(locId, gx, gy, pageId)
        world.markLocationStamped(gx, gy, pageId)
    end
    locations.spawnContents(locId, gx, gy, pageId)
end

return stamper
