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
-- flag below — tracks whether this chunk's location has been COMPLETELY
-- stamped (#1719: every placement the builder attempted succeeded, not
-- merely that the builder ran).
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
-- demolishing the floor would otherwise re-trigger a full re-stamp. That
-- independence also holds the other way (#1719): contents spawn on a
-- dispatch whose geometry stamp FAILED, exactly as they do on one that
-- was skipped, because the two flags answer different questions.
--
-- Partial stamps (#1719): locations.stamp returns (ok, failedCount), and
-- the marker is written only when ok. A failed or partial stamp leaves
-- the chunk unmarked, so the every-load dispatch in
-- World.Thread.ChunkLoading re-attempts it on the next load of this
-- chunk — the retry IS the recovery, and it is idempotent because both
-- the Lua staging overlay and the authoritative edit apply key a piece
-- by canonical tile and slot, so re-issuing a piece that already
-- succeeded replaces it rather than adding a second one.
--
-- Accepted is not committed (#2051): structure.place returns true as soon
-- as the piece is staged and its WorldSetStructure queued, and the world
-- thread checks residency AGAIN before committing. A chunk that evicts in
-- that window is declined there — no overlay entry, no edit — long after
-- `ok` above said the geometry materialized. So the marker is gated a
-- second time, on the world thread: structure.stageWatermark is read
-- either side of the builder run, and the pair rides
-- world.markLocationStamped as the span of attempts this invocation
-- accepted. The engine withholds the marker when any of them was
-- declined, and this every-load dispatch retries the whole builder next
-- load exactly as it does for a synchronous failure.

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
        -- Read before the builder and again after it: everything staged
        -- on THIS page in between is exactly what this invocation
        -- accepted. Both reads must land for the pair to mean anything,
        -- so a nil from either (an unresolvable page) passes no window
        -- rather than half of one.
        local fromTok = structure.stageWatermark(pageId)
        local ok, failed = locations.stamp(locId, gx, gy, pageId)
        local toTok = structure.stageWatermark(pageId)
        if ok then
            world.markLocationStamped(gx, gy, pageId, fromTok, toTok)
        elseif (failed or 0) > 0 then
            -- One aggregate warning per unsuccessful ATTEMPT (so a retry
            -- that fails again warns again), never one per piece. The
            -- unknown-id / unknown-builder paths attempt nothing and
            -- already warn inside locations.stamp, so they are excluded
            -- here rather than summarised twice.
            engine.logWarn(string.format(
                "locations: stamp of '%s' on page '%s' at %d,%d failed %d " ..
                "placement(s) — chunk left unmarked, will retry on next load",
                tostring(locId), tostring(pageId), gx, gy, failed))
        end
    end
    locations.spawnContents(locId, gx, gy, pageId)
end

return stamper
