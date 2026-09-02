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
-- building.spawn / loot.rollFor, gated by its own one-time engine flag
-- (world.hasSpawnedLocationContents) so contents are never re-spawned.
-- Those four kinds are the whole vocabulary, and it is CLOSED at the
-- YAML boundary (#1708): Engine.Asset.YamlLocations' validContentKinds
-- fails the file's load on anything else, so a def that reaches here
-- can only carry kinds dispatchContent handles. The nested "structure"
-- kind was removed there — it re-translated the outer def's bounds
-- around a shifted anchor, stamping geometry outside the box #777 made
-- authoritative.
-- The loot draw is seed-stable per placed instance (#948) — see the
-- "Content spawning" section below — so a ruin's rewards are as
-- reproducible from the world seed as its geometry already was.

local locations = {}

-----------------------------------------------------------
-- Definition registry (engine-backed, #88)
-----------------------------------------------------------
-- The defs live in the engine LocationRegistry (loaded from
-- data/locations/*.yaml). We query the engine each call so the list
-- always reflects what's registered — no local cache to invalidate.
-- Each engine LocationDef table is:
--   { id, label, type, builder, anchor={tag,…},
--     bounds={min_x,min_y,max_x,max_y},
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
--     bounds }   -- absolute, inclusive tile bounds (#777), or nil if
--                --   `id` has no matching registered def
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

-- Placement-result aggregation (#1719). `structures.lua`'s floor / post /
-- wall / ceiling wrappers each return `structure.place`'s own bool, which
-- can be false for four independent reasons (unparseable slot, omitted
-- texture/facemap paths, no resolvable page, target chunk not loaded).
-- A builder threads every one of those through this counter so
-- `buildAt` can report whether the geometry it just issued actually
-- materialized, instead of reporting that it bothered to call.
--
-- `attempt` deliberately does NOT short-circuit: an earlier false must
-- not skip a later piece, because the success path's byte-for-byte
-- compatibility and the partial progress a retry builds on both depend
-- on the existing call sequence being issued in full every attempt.
-- A piece a builder deliberately omits (a damaged room's collapsed wall)
-- is never an attempt, so it never counts.
local function attemptCounter()
    local failed = 0
    local function attempt(ok)
        if not ok then failed = failed + 1 end
        return ok
    end
    return attempt, function() return failed end
end

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
--
-- Returns the number of ATTEMPTED placements that failed (#1719) — see
-- `attemptCounter` for why a failure never short-circuits the rest.
function builders.room_small(worldId, gx, gy, def)
    local S = require("scripts.structures")
    local b = def.bounds
    local x0, x1 = gx + b.min_x, gx + b.max_x
    local y0, y1 = gy + b.min_y, gy + b.max_y
    local attempt, failedCount = attemptCounter()

    -- 0. level the ground so the room is flat: flatten the footprint to its
    --    lowest base elevation and build every piece at that explicit z. (The
    --    flatten edits are async, so re-reading terrain this call would still
    --    see the old bumps — hence baseZ is threaded through, not re-read.)
    local baseZ = locations.flattenFootprint(worldId, x0, y0, x1, y1)

    -- 1. floor across the whole footprint at the levelled height
    for x = x0, x1 do
        for y = y0, y1 do attempt(S.floor(x, y, worldId, baseZ)) end
    end

    -- 2. corner posts (cap the two perimeter walls that meet at each)
    attempt(S.post(x0, y0, "n", worldId, baseZ))   -- nw + ne meet
    attempt(S.post(x1, y0, "e", worldId, baseZ))   -- ne + se meet
    attempt(S.post(x1, y1, "s", worldId, baseZ))   -- se + sw meet
    attempt(S.post(x0, y1, "w", worldId, baseZ))   -- sw + nw meet

    -- 3. perimeter walls (after posts so they cap to them)
    for y = y0, y1 do
        attempt(S.wall(x0, y, "nw", worldId, baseZ))   -- −gx side
        attempt(S.wall(x1, y, "se", worldId, baseZ))   -- +gx side
    end
    for x = x0, x1 do
        attempt(S.wall(x, y0, "ne", worldId, baseZ))   -- −gy side
        attempt(S.wall(x, y1, "sw", worldId, baseZ))   -- +gy side
    end

    -- No ceiling by default, so the interior stays visible while iterating.

    engine.logInfo(string.format("locations: room_small %dx%d at %d,%d",
        x1 - x0 + 1, y1 - y0 + 1, gx, gy))
    return failedCount()
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
--
-- Returns the number of ATTEMPTED placements that failed (#1719). The
-- pieces the collapse pattern omits were never attempted, so a fully
-- successful ruin still reports zero however much of it fell down.
function builders.room_small_damaged(worldId, gx, gy, def)
    local S = require("scripts.structures")
    local b = def.bounds
    local x0, x1 = gx + b.min_x, gx + b.max_x
    local y0, y1 = gy + b.min_y, gy + b.max_y
    local rand = collapseRng(gx, gy)
    local attempt, failedCount = attemptCounter()
    local baseZ = locations.flattenFootprint(worldId, x0, y0, x1, y1)

    -- 1. floor across the whole footprint (damaged art, none missing)
    for x = x0, x1 do
        for y = y0, y1 do attempt(S.floor(x, y, worldId, baseZ, "damaged")) end
    end

    -- 2. corner posts, minus one collapsed corner (1=n 2=e 3=s 4=w).
    --    The collapsed corner is never issued, so it is not an attempt
    --    and cannot count as a failure.
    local lostPost = rand(4)
    local posts = { { x0, y0, "n" }, { x1, y0, "e" },
                    { x1, y1, "s" }, { x0, y1, "w" } }
    for i, p in ipairs(posts) do
        if i ~= lostPost then attempt(S.post(p[1], p[2], p[3], worldId, baseZ, "damaged")) end
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
        if not collapsed(1, y - y0) then attempt(S.wall(x0, y, "nw", worldId, baseZ, "damaged")) end
        if not collapsed(2, y - y0) then attempt(S.wall(x1, y, "se", worldId, baseZ, "damaged")) end
    end
    for x = x0, x1 do
        if not collapsed(3, x - x0) then attempt(S.wall(x, y0, "ne", worldId, baseZ, "damaged")) end
        if not collapsed(4, x - x0) then attempt(S.wall(x, y1, "sw", worldId, baseZ, "damaged")) end
    end
    -- no ceiling — the roof fell in long ago

    engine.logInfo(string.format(
        "locations: room_small_damaged %dx%d at %d,%d (breach side %d len %d)",
        x1 - x0 + 1, y1 - y0 + 1, gx, gy, breachSide, breachLen))
    return failedCount()
end

-- Resolve location `id` to its def, then call the builder it names.
--
-- Returns (ok, failedPlacementCount). `ok` is true only when the def
-- resolved, the builder ran, AND every placement it attempted was
-- ACCEPTED (#1719) — the first of the two gates
-- scripts/location_stamper.lua puts in front of the durable marker. It
-- is a synchronous answer, so it cannot see a placement the world thread
-- later declines; #2051's commit window is the second gate, and it runs
-- on the world thread. An unknown id or an unknown builder still returns
-- false, with a count of zero: nothing was attempted, and each of those
-- paths logs its own warning here, so the stamper must not summarise
-- them a second time.
local function buildAt(id, gx, gy, worldId)
    local def = locations.getDef(id)
    if not def then
        engine.logWarn("locations: unknown location '" .. tostring(id) .. "'")
        return false, 0
    end
    local b = builders[def.builder]
    if not b then
        engine.logWarn("locations: location '" .. id ..
            "' names unknown builder '" .. tostring(def.builder) .. "'")
        return false, 0
    end
    local failed = tonumber(b(worldId, math.floor(gx), math.floor(gy), def)) or 0
    return failed == 0, failed
end

-- locations.build(id, gx, gy) — look up the def by id and call its
-- builder, stamping on the active world page (#88). Returns buildAt's
-- (ok, failedPlacementCount) pair unchanged.
function locations.build(id, gx, gy)
    local hud = require("scripts.hud")
    local worldId = (hud and hud.worldId) or "test_arena"
    return buildAt(id, gx, gy, worldId)
end

-- Stamp location `id`, anchored at tile (gx, gy) on an explicit page
-- `worldId`. The debug-overlay entry point (it knows the page).
-- Returns buildAt's (ok, failedPlacementCount) pair unchanged (#1719):
-- `ok` is the completion answer, not a call-happened flag.
function locations.stamp(id, gx, gy, worldId)
    return buildAt(id, gx, gy, worldId)
end

-----------------------------------------------------------
-- Content spawning (#90)
-----------------------------------------------------------
-- Each LocationDef.contents entry (see data/locations/*.yaml):
--   { kind, id, count, count_range, clearance, rolls, significant,
--     position = {x,y} | nil, faction | nil }
-- `position` is a fixed offset from the anchor; when absent the entry
-- scatters randomly within the location's footprint instead (a fresh
-- roll per instance). `count` is how many to place ("loot_table" uses
-- `rolls` instead — how many times to roll the table). `faction` is
-- unit-only and defaults to "hostile". `significant` (#917) is
-- item-only — the YAML boundary refuses it on any other kind — and
-- marks a GUARANTEED item the location's clearance predicate waits on;
-- such an entry is spawned by spawnSignificantContent against the
-- instance's own persisted obligations, never by dispatchContent.
--
-- Called once per chunk load by scripts/location_stamper.lua,
-- regardless of whether the geometry was (re)built this call — gated
-- by its OWN one-time engine flag (world.hasSpawnedLocationContents),
-- independent of the structure.hasAt check that gates re-stamping.
-- That independence matters: a floor-less location type would
-- otherwise re-run every load, and a player demolishing the floor
-- would otherwise re-trigger every content spawn too.
--
-- Loot determinism (#948): a `loot_table` entry rolls through
-- `loot.rollFor`, NOT the shared-RNG `loot.roll`. Its draw is a pure
-- function of the world page's persisted seed, the placed location's
-- stable instance id (#911), the entry's POSITIONAL index in
-- `contents`, and the roll number — so the same ruin in the same world
-- yields the same items in any process, whatever order chunks and
-- locations load in, and whether or not the world was saved before its
-- contents first spawned. Only the scatter COORDINATES below still use
-- math.random; the selected item ids do not.

-- Scatter within the def's own authoritative bounds (#777) — no
-- independent per-builder radius table to keep in sync with it.
local function contentOffset(def, entry)
    if entry.position then
        return entry.position.x or 0, entry.position.y or 0
    end
    local b = def.bounds
    return math.random(b.min_x, b.max_x), math.random(b.min_y, b.max_y)
end

local function spawnUnitContent(def, entry, gx, gy, worldId, count)
    -- Named factionTag, not `faction`: `faction` is the engine's global
    -- relation/property table (#912), and shadowing it inside a spawn
    -- helper is a trap waiting for the next edit here.
    local factionTag = entry.faction or "hostile"
    local occupants = {}
    for _ = 1, (count or entry.count or 1) do
        local ox, oy = contentOffset(def, entry)
        local homeX, homeY = gx + ox, gy + oy
        local uid = unit.spawn(entry.id, homeX, homeY, nil,
                               factionTag, worldId)
        if uid == -1 then
            engine.logWarn("locations: unknown unit content '" ..
                tostring(entry.id) .. "'")
        else
            occupants[#occupants + 1] = {
                uid = uid, home_x = homeX, home_y = homeY,
            }
        end
    end
    return occupants
end

-- Ranged encounter occupants need distinct, replay-stable home tiles without
-- consuming math.random's shared gameplay stream. The ruin builder lays a
-- walkable floor on every tile in its authoritative bounds (its walls are
-- cosmetic overlays), so a deterministic permutation of that rectangle is
-- both the walkability set and the distinctness proof for ruin_small. The
-- YAML boundary rejects a range whose maximum exceeds this capacity.
local function encounterOffsets(def, rollCtx)
    local offsets = {}
    for ox = def.bounds.min_x, def.bounds.max_x do
        for oy = def.bounds.min_y, def.bounds.max_y do
            offsets[#offsets + 1] = { x = ox, y = oy }
        end
    end
    local state = (math.abs(rollCtx.instance) * 73856093
        + rollCtx.index * 19349663 + (rollCtx.seed or 0) % 2147483647)
        % 2147483647
    if state <= 0 then state = state + 2147483646 end
    for i = #offsets, 2, -1 do
        state = (state * 48271) % 2147483647
        local j = (state % i) + 1
        offsets[i], offsets[j] = offsets[j], offsets[i]
    end
    return offsets
end

local function spawnEncounterUnitContent(def, entry, gx, gy, worldId,
                                         count, rollCtx, existing, persist)
    local factionTag = entry.faction or "hostile"
    local occupants = {}
    local seen = {}
    local offsets = encounterOffsets(def, rollCtx)
    -- A prior interrupted attempt may already have durably registered a
    -- prefix. Preserve those exact ids/homes and allocate only missing slots.
    for _, occupant in ipairs(existing or {}) do
        if #occupants >= count or seen[occupant.uid] then
            -- A malformed overlong/duplicate roster stays incomplete. Never
            -- reinterpret it as a valid prefix and silently burn the marker.
            return false
        end
        seen[occupant.uid] = true
        occupants[#occupants + 1] = {
            uid = occupant.uid,
            home_x = occupant.home_x,
            home_y = occupant.home_y,
        }
    end
    for index = #occupants + 1, count do
        local offset = offsets[index]
        if not offset then break end
        local homeX, homeY = gx + offset.x, gy + offset.y
        local uid = unit.spawn(entry.id, homeX, homeY, nil,
                               factionTag, worldId)
        if uid == -1 then
            engine.logWarn("locations: unknown unit content '" ..
                tostring(entry.id) .. "'")
        else
            occupants[#occupants + 1] = {
                uid = uid, home_x = homeX, home_y = homeY,
            }
            -- Register every successful prefix immediately. A later retry
            -- reads it from the placed instance and resumes at the next slot,
            -- so an interruption cannot duplicate an already-created nomad.
            persist(occupants)
        end
    end
    return #occupants == count
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

-- Spawn the guaranteed SIGNIFICANT items one placed instance still owes
-- (#917), resuming rather than repeating.
--
-- The obligations themselves were created with the instance, at
-- PLACEMENT — this only fills them in. Each is addressed by its stable
-- slot, and a slot that already names an item is skipped, so an
-- interrupted spawn that got half way through resumes at the next empty
-- slot instead of spawning a second copy of what it already made. That
-- is the same resume discipline spawnEncounterUnitContent uses for a
-- ranged roster, and for the same reason: contents_spawned is only
-- marked once EVERY obligation is filled.
--
-- Returns false when any obligation could not be filled — an unknown
-- item id, or a registration the engine refused. The caller then leaves
-- contents_spawned unmarked and returns, so the next chunk load retries.
-- Warning and skipping instead would burn the location's exactly-once
-- content lifecycle on a location that could then never be cleared,
-- which is precisely what a guaranteed item must not allow.
local function spawnSignificantContent(def, gx, gy, worldId, placed)
    -- No placed instance means no owner: a hand-stamped debug ruin (the
    -- console, the overlay, a probe) has no LocationInstanceId, so
    -- there is nothing to bind provenance to and nothing that could
    -- ever be cleared. Its incidental contents still spawn; it simply
    -- owes nothing. Same rule the ranged encounter follows above.
    if not placed then return true end
    local owed = placed.significant or {}
    if #owed == 0 then return true end

    -- The OBLIGATION is the authority, not today's YAML. Each one
    -- already carries the item def name it was created with (#911's
    -- read-the-stored-values rule), so a definition edited since
    -- placement can neither change what a materialized world owes nor
    -- block its contents from spawning at all.
    --
    -- The authored entries are consulted for ONE thing: a fixed
    -- `position`. They are rebuilt in the same order the engine derived
    -- the slots from (Location.Instance's significantItemsFromDef —
    -- authored contents order, then each entry's own count), so slot N
    -- here is slot N there. A slot with no matching authored entry
    -- simply scatters within the def's bounds, exactly as an entry with
    -- no `position` does.
    local authored = {}
    for _, entry in ipairs(def.contents or {}) do
        if entry.significant and entry.kind == "item" then
            for _ = 1, (entry.count or 1) do
                authored[#authored + 1] = entry
            end
        end
    end

    for _, obligation in ipairs(owed) do
        if not obligation.item_instance_id then
            local slot = obligation.slot
            local ox, oy = contentOffset(def, authored[slot] or {})
            -- ONE engine call spawns the item and binds it to the slot.
            -- This script never chooses WHICH item fills an obligation
            -- — only where it lands: the engine takes the definition
            -- from the obligation's own persisted record and binds the
            -- instance it just created. A two-step spawn-then-bind
            -- would let any caller substitute an unrelated item of the
            -- right kind, and the location would then never spawn its
            -- own guaranteed one.
            --
            -- Filled one at a time, so an interruption leaves a bound
            -- prefix a retry resumes past rather than an orphaned item
            -- with no provenance.
            if not world.spawnLocationSignificantItem(
                    placed.instance_id, slot, gx + ox, gy + oy, worldId) then
                engine.logWarn("locations: could not spawn significant item '"
                    .. tostring(obligation.item) .. "' for slot " ..
                    tostring(slot))
                return false
            end
        end
    end
    return true
end

-- `rollCtx` is the stable per-entry roll context built by
-- locations.spawnContents (#948): { seed, instance, index }. Each roll
-- adds its own 1-based roll number, so the entry's rolls are
-- independent draws rather than one result repeated.
local function spawnLootTableContent(def, entry, gx, gy, worldId, rollCtx)
    for roll = 1, (entry.rolls or 1) do
        local itemId = loot.rollFor(entry.id, rollCtx.seed, rollCtx.instance,
                                    rollCtx.index, roll)
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

-- An entry the significant pass owns (#917). Both the encounter roster
-- and the significant items are spawned by their own passes, against
-- persisted per-instance obligations, so the ordinary content loop must
-- skip them or they would spawn twice.
local function isOwnedByDedicatedPass(entry)
    return (entry.kind == "unit" and entry.count_range ~= nil)
        or (entry.significant and entry.kind == "item")
end

local function dispatchContent(def, entry, gx, gy, worldId, rollCtx)
    local kind = entry.kind
    if kind == "unit" then
        return spawnUnitContent(def, entry, gx, gy, worldId)
    elseif kind == "item" then
        spawnItemContent(def, entry, gx, gy, worldId)
    elseif kind == "loot_table" then
        spawnLootTableContent(def, entry, gx, gy, worldId, rollCtx)
    elseif kind == "building" then
        spawnBuildingContent(def, entry, gx, gy, worldId)
    else
        -- Unreachable from authored data: every def here came through
        -- engine.loadLocationYaml, whose closed validContentKinds
        -- vocabulary already failed the file (#1708). Kept as the
        -- backstop for a def injected some other way — warning and
        -- skipping one entry beats a nil-index crash mid-stamp.
        engine.logWarn("locations: unknown content kind '" ..
            tostring(kind) .. "'")
    end
end

local function placedInstanceAt(gx, gy, worldId)
    for _, e in ipairs(world.listPlacedLocations(worldId) or {}) do
        if e.gx == gx and e.gy == gy and e.instance_id then
            return e
        end
    end
    return nil
end

-- The stable identity this anchor's loot rolls key on (#948).
--
-- A location placed by the world-gen overlay owns a persisted instance
-- id (#911), allocated at placement in deterministic order — that is
-- the identity requirement 1 wants, and it survives save/load and chunk
-- eviction unchanged. A location stamped BY HAND (the debug overlay,
-- the console, the probes) has no placed instance at all, so fall back
-- to a stable function of its anchor tile — the same mixing collapseRng
-- uses — pushed into the NEGATIVE range, which allocated instance ids
-- (they start at 1) can never occupy. Either way the id is durable
-- state, never a counter or a load order.
local function lootInstanceId(gx, gy, worldId, placed)
    if placed then return placed.instance_id end
    return -(1 + ((gx * 73856093 + gy * 19349663) % 2147483647))
end

-- Spawn location `id`'s contents, anchored at (gx, gy) on page
-- `worldId` — once, ever, for this chunk. Safe to call on every chunk
-- load: a no-op once world.hasSpawnedLocationContents is true.
function locations.spawnContents(id, gx, gy, worldId)
    gx, gy = math.floor(gx), math.floor(gy)
    if world.hasSpawnedLocationContents(gx, gy, worldId) then return end
    local def = locations.getDef(id)
    if def then
        local placed = placedInstanceAt(gx, gy, worldId)
        -- Resolved once per spawn, after the one-time gate: both halves
        -- are durable per-page state, and neither depends on which
        -- chunks have loaded so far. A page with no gen params yet
        -- (an arena) reports no seed — 0 keeps the draw defined and
        -- still entropy-free.
        local rollCtx = {
            seed     = world.getSeed(worldId) or 0,
            instance = lootInstanceId(gx, gy, worldId, placed),
        }
        local encounterReady = true
        -- Reserve/recover the ranged roster first even when its authored
        -- entry follows ordinary content. The original entry indices stay
        -- intact for #948's loot identity, while an incomplete encounter can
        -- return without having spawned earlier loot that a retry would copy.
        for index, entry in ipairs(def.contents or {}) do
            rollCtx.index = index
            if entry.kind == "unit" and entry.count_range then
                -- A ranged encounter belongs to a persisted placed
                -- instance. Hand-stamped debug ruins have no such owner and
                -- deliberately choose the zero-occupant outcome.
                if placed and placed.encounter then
                    encounterReady = spawnEncounterUnitContent(
                        def, entry, gx, gy, worldId,
                        placed.encounter.rolled_count, rollCtx,
                        placed.encounter.occupants,
                        function(roster)
                            world.registerLocationEncounterOccupants(
                                placed.instance_id, roster, worldId)
                        end)
                end
            end
        end
        if not encounterReady then return end
        -- #917: the guaranteed significant items come next, before any
        -- incidental content. Like the roster above they belong to a
        -- persisted obligation and must all be filled before the
        -- one-time marker below is written — an unfilled one returns
        -- here, leaving the whole spawn to be retried, rather than
        -- burning the marker on a location that could then never clear.
        if not spawnSignificantContent(def, gx, gy, worldId, placed) then
            return
        end
        for index, entry in ipairs(def.contents or {}) do
            if not isOwnedByDedicatedPass(entry) then
                rollCtx.index = index
                dispatchContent(def, entry, gx, gy, worldId, rollCtx)
            end
        end
    else
        engine.logWarn("locations: unknown location '" .. tostring(id) ..
            "' (content spawn)")
    end
    world.markLocationContentsSpawned(gx, gy, worldId)
end

return locations
