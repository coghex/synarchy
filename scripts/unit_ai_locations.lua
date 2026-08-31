-- Per-unit location knowledge (#915).
--
-- Synarchy keeps TWO independent models of "knowing where something is",
-- and this module owns the second one for locations:
--
--   * CARTOGRAPHIC (player-wide) -- a placed location's own
--     Location.Instance lifecycle, promoted to "discovered" the first
--     time any player-owned unit enters its margin (#780/#911). That is
--     a fact about what the PLAYER has mapped, and it is what drives the
--     zoom-map icons (#781). Nothing here touches it.
--   * EXPERIENTIAL (per-unit) -- "this acolyte knows where the ruin is".
--     Stored on aiState[uid].knownLocations, mirroring the shape and
--     lifecycle of unit_ai_core.lua's knownWaterSources: add, nearest
--     lookup, has-any, and an explicit forget for invalidation.
--
-- Neither layer derives from the other. A location the player mapped
-- years ago is still unknown to an acolyte who has never been there;
-- an acolyte who walks into an already-mapped ruin still learns it.
--
-- ONE structural difference from water memory, and it matters: water
-- sources dedup by DISTANCE (two tiles six apart are the same body of
-- water), locations dedup by IDENTITY. Two locations are never "the
-- same location", however close their anchors are, so the key is the
-- durable (page, instance id) pair #911 introduced -- never coordinates,
-- and never a bare instance id, since instance ids are allocated PER
-- PAGE and the same number legitimately names different ruins on two
-- different worlds.
--
-- Lives in its own submodule rather than unit_ai_core.lua because that
-- file is at its 500-line budget (#538, tools/lua_module_budget.py).
-- Deliberately requires nothing at module scope, so it can be exercised
-- in a bare Lua VM (see Test.Headless.Lua.UnitAiLocations); the engine
-- globals it does use (world/unit/engine) are reached only from inside
-- the functions that need them.

local M = {}

-----------------------------------------------------------
-- Memory shape
--
--   aiState[uid].knownLocations = {
--     { page = "<world page id>", id = <instance id>,
--       x = <anchor gx>, y = <anchor gy> }, ...
--   }
--
-- The anchor is remembered alongside the identity on purpose: it is
-- what the unit actually learned (where the place IS), and it keeps the
-- nearest lookup a pure walk over the memory list, exactly like
-- nearestWaterSource -- no engine round-trip per candidate.
-----------------------------------------------------------

local function indexOf(s, page, id)
    local list = s.knownLocations
    if not list then return nil end
    for i, k in ipairs(list) do
        if k.page == page and k.id == id then return i end
    end
    return nil
end

-- Half of a page's cylindrical u-wrap PERIOD, in tiles -- the one alias
-- step World.Generate.Coordinates.tileAliasStep and
-- Location.Bounds.seamAliases shift by. `wrapWidth` is the FULL period
-- world.getWrapWidth reports; zero for a period that is absent,
-- non-positive, or not a number, which collapses every comparison below
-- to the plain Cartesian one.
local function aliasStep(wrapWidth)
    if type(wrapWidth) ~= "number" or wrapWidth <= 0 then return 0 end
    return math.floor(wrapWidth / 2)
end

-- Euclidean distance from (ax, ay) to (bx, by), minimised over b's
-- cylindrical u-images: itself plus one shift each way along (+u, -v) by
-- `step`. That is the same three-image set localizeTileToAnchor and
-- seamAliases search, so a remembered anchor directly across the U seam
-- is measured at its real physical distance rather than at the width of
-- the world (#1175, #1944).
--
-- Identity when step is 0 -- an arena, a non-wrapping page, or a period
-- the engine could not supply -- where the loop runs exactly once.
local function distance(ax, ay, bx, by, step)
    local lo, hi = 0, 0
    if step > 0 then lo, hi = -1, 1 end
    local best = math.huge
    for k = lo, hi do
        local dx = ax - (bx + k * step)
        local dy = ay - (by - k * step)
        local d = math.sqrt(dx * dx + dy * dy)
        if d < best then best = d end
    end
    return best
end

-- The page's wrap period, for the page NAMED in the call -- never the
-- active or visible one, since two live pages can have different world
-- sizes and world.getWrapWidth is page-scoped precisely so a caller need
-- not guess. Zero (today's plain Euclidean ranking) whenever the engine,
-- the verb, or the page cannot answer; a nil page falls through to the
-- verb's own active-world reading.
local function wrapPeriodFor(page)
    if type(world) ~= "table" or type(world.getWrapWidth) ~= "function" then
        return 0
    end
    local ok, w = pcall(world.getWrapWidth, page)
    if not ok or type(w) ~= "number" then return 0 end
    return w
end

-- Remember one location. Returns true when this is genuinely new
-- knowledge, false when the unit already knew it (or the identity was
-- malformed). Dedup is by (page, id) ONLY -- see the header.
function M.addKnownLocation(s, page, id, x, y)
    if type(page) ~= "string" or page == "" then return false end
    if type(id) ~= "number" or id ~= math.floor(id) then return false end
    s.knownLocations = s.knownLocations or {}
    if indexOf(s, page, id) then return false end
    table.insert(s.knownLocations, { page = page, id = id, x = x, y = y })
    return true
end

-- The nearest location this unit knows ON `page`, or nil. Page-scoped
-- because a memory of another world's ruin is not a candidate for
-- anything the unit can walk to, and because two pages' instance ids
-- collide by construction.
--
-- `wrapWidth` is that page's FULL cylindrical u-wrap period in tiles
-- (what world.getWrapWidth returns). It is an EXPLICIT input rather than
-- an engine round-trip so this primitive stays callable from a bare Lua
-- VM; the uid-keyed wrapper below is what obtains it. Omitted, zero, or
-- non-numeric ranks by plain Euclidean distance, exactly as before.
function M.nearestKnownLocation(s, page, fromX, fromY, wrapWidth)
    local list = s.knownLocations
    if not list or #list == 0 then return nil end
    local step = aliasStep(wrapWidth)
    local best, bestD = nil, math.huge
    for _, k in ipairs(list) do
        if k.page == page then
            local d = distance(fromX, fromY, k.x, k.y, step)
            if d < bestD then best, bestD = k, d end
        end
    end
    return best
end

-- Drop one specific memory (the location was demolished, the save it
-- came from no longer contains it, ...). Leaves every sibling intact.
-- Returns true when something was actually removed.
function M.forgetKnownLocation(s, page, id)
    local i = indexOf(s, page, id)
    if not i then return false end
    table.remove(s.knownLocations, i)
    return true
end

function M.knowsLocation(s, page, id)
    return indexOf(s, page, id) ~= nil
end

function M.hasKnownLocation(s)
    return s.knownLocations ~= nil and #s.knownLocations > 0
end

-----------------------------------------------------------
-- Acquisition
--
-- world.getLocationAwareness() reports every PLAYER-OWNED unit that can
-- currently SEE a placed location (#1230: its night-aware visible-tile
-- set intersects the instance's own stored bounds), on every LOADED
-- page. It shares its predicate with the player-wide discovery tick by
-- construction (both are Location.Discovery.sightContactsWhere), so
-- the two layers can never disagree about geometry or about which units
-- count -- but unlike that tick it reports EVERY qualifying unit and
-- ignores lifecycle, which is what makes a second acolyte arriving at
-- an already-mapped ruin learn it too.
--
-- Called from unit_ai.lua's update BEFORE its pause guard, so
-- acquisition stays as pause-independent as the engine-side discovery
-- tick it mirrors.
-----------------------------------------------------------

-- Returns how many genuinely-new memories were recorded this call.
function M.ingestAwareness(ensureState)
    if type(world) ~= "table" or type(world.getLocationAwareness) ~= "function" then
        return 0
    end
    local rows = world.getLocationAwareness()
    if type(rows) ~= "table" then return 0 end
    local learned = 0
    for _, row in ipairs(rows) do
        if type(row) == "table" and type(row.uid) == "number" then
            local s = ensureState(row.uid)
            if M.addKnownLocation(s, row.page, row.instance_id,
                                  row.gx, row.gy) then
                learned = learned + 1
            end
        end
    end
    return learned
end

-----------------------------------------------------------
-- Persisted wire form (#764's typed-reference discipline).
--
-- Every OTHER unit_ai reference field is a bare id boxed into
-- {__ref=kind, id=N} at snapshot time. A location memory is already a
-- record, so the ENTRY ITSELF is the typed reference and only gains a
-- `__ref` tag:
--
--   live: { page = "main_world", id = 3, x = 8, y = 8 }
--   wire: { __ref = "location_instance",
--           page = "main_world", id = 3, x = 8, y = 8 }
--
-- That keeps the wire shape self-describing for
-- unit_ai_save_refs.lua's checkRefTag (which inspects exactly `__ref`
-- and `id`) without inventing a second record layer holding only those
-- same two keys, and it carries `page` as real payload -- mandatory,
-- since instance ids are allocated PER PAGE and a bare id would alias
-- across worlds. aiState's LIVE shape never grows `__ref`.
--
-- Lives here rather than in unit_ai_save_refs.lua so the module that
-- owns the field's meaning owns its encoding too (and so that file
-- stays inside its 500-line budget).
-----------------------------------------------------------

M.REF_KIND = "location_instance"

local function copyEntries(list, tag)
    if list == nil then return nil end
    local out = {}
    for i, k in ipairs(list) do
        local t = {}
        for f, v in pairs(k) do t[f] = v end
        t.__ref = tag
        out[i] = t
    end
    return out
end

function M.wrapForSave(list) return copyEntries(list, M.REF_KIND) end
function M.unwrapFromSave(list) return copyEntries(list, nil) end

-----------------------------------------------------------
-- Load reconciliation
--
-- A memory naming an instance that is absent from the RESTORED session
-- is dropped with a diagnostic and its siblings left alone -- the same
-- tolerated-dangling-reference contract every other Lua AI reference
-- follows (#761/#764): a demolished or no-longer-present location's
-- lingering memory is gameplay, not corruption, so it never blocks a
-- load. The engine-side integrity graph reports the same edge as a
-- non-blocking "lua component 'unit_ai' ... location_instance ..."
-- diagnostic (World.Save.Integrity.luaReferenceErrors); this is the
-- reconcile-time scrub that actually removes it.
--
-- Absence means "not in this page's restored instance table" -- NOT
-- "its chunk isn't loaded". #911 stores every instance on the page's
-- gen params independently of chunk residency, so an evicted chunk's
-- location still resolves here.
-----------------------------------------------------------

function M.scrubStaleKnownLocations(uid, s)
    local list = s.knownLocations
    if not list then return 0 end
    if type(world) ~= "table" or type(world.getLocationInstance) ~= "function" then
        return 0
    end
    local kept, dropped = {}, 0
    for _, k in ipairs(list) do
        if world.getLocationInstance(k.id, k.page) ~= nil then
            kept[#kept + 1] = k
        else
            dropped = dropped + 1
            if engine and engine.logWarn then
                engine.logWarn("unit_ai: unit " .. tostring(uid)
                    .. " knownLocations references location_instance "
                    .. "page=" .. tostring(k.page) .. ",id=" .. tostring(k.id)
                    .. " which is absent from the restored session -- "
                    .. "memory dropped")
            end
        end
    end
    s.knownLocations = kept
    return dropped
end

-----------------------------------------------------------
-- Public API, attached to the scripts.unit_ai singleton by unit_ai.lua's
-- init (the same shape unit_ai_save.register uses). Requirement 7's
-- "consulted the way nearestWaterSource is": the module-level helpers
-- above take a state table directly, for AI candidates that already
-- hold one; these uid-keyed wrappers are for the console, panels, and
-- satellite scripts that don't.
-----------------------------------------------------------

function M.register(unitAi, aiState)
    -- Every location this unit knows, as a list of
    -- { page, id, x, y } -- an empty list when it knows none.
    function unitAi.getKnownLocations(uid)
        local s = aiState[uid]
        return (s and s.knownLocations) or {}
    end

    -- The nearest location this unit knows on `page` (defaulting to the
    -- active world), measured from (fromX, fromY) -- defaulting to the
    -- unit's own tile. nil when it knows none there.
    --
    -- Ranking is on that page's own cylinder: the wrap period is read
    -- AFTER `page` is resolved, so a query about a loaded-but-not-visible
    -- page is measured against its world size and not the active one.
    function unitAi.nearestKnownLocation(uid, page, fromX, fromY)
        local s = aiState[uid]
        if not s then return nil end
        if page == nil and world and world.getActiveWorldId then
            page = world.getActiveWorldId()
        end
        if fromX == nil or fromY == nil then
            local info = unit.getInfo(uid)
            if not info then return nil end
            fromX, fromY = info.gridX, info.gridY
        end
        return M.nearestKnownLocation(s, page, fromX, fromY,
                                      wrapPeriodFor(page))
    end

    function unitAi.knowsLocation(uid, page, id)
        local s = aiState[uid]
        return s ~= nil and M.knowsLocation(s, page, id)
    end

    function unitAi.forgetLocation(uid, page, id)
        local s = aiState[uid]
        return s ~= nil and M.forgetKnownLocation(s, page, id)
    end
end

return M
