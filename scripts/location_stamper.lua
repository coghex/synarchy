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
-- Idempotency: stamping lands in the edit log (structure.place / setCell),
-- which replays on chunk reload. So a chunk that loads again already carries
-- its geometry; we detect that with structure.hasAt and skip — never
-- stamping twice, and never clobbering a location the player has edited.
--
-- Multiworld: the builders read terrain with an explicit page id
-- (locations.stamp -> the #88 builder -> world.getTerrainAt(gx,gy,pageId)),
-- so a location materializes on its own page even when that page is hidden /
-- not the active one — there is no active-page gate here.

local stamper = {}

local locations = require("scripts.locations")

-- Fired by the engine for a just-loaded chunk that hosts a placed location.
function stamper.onStampLocation(pageId, locId, gx, gy)
    gx, gy = math.floor(gx), math.floor(gy)
    -- Already materialized (stamped earlier this session, or its edits
    -- replayed on reload)? Then this is a repeat load — nothing to do.
    if structure.hasAt(gx, gy, "floor") then return end
    locations.stamp(locId, gx, gy, pageId)
end

return stamper
