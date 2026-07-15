-- Wire structure placement + connection-aware autotile (#359, power
-- epic #357).
--
-- Wire is a structure piece ("wire" pack, "wire" kind, no edge — see
-- data/structure_packs/wire.yaml) that occupies the tile top like a
-- floor (Structure.Types.SWire). Its rendered variant is DERIVED from
-- which of the tile's 4 cardinal neighbours also carry wire, so a laid
-- run reads as one continuous line. Placing a wire tile re-derives its
-- OWN shape and re-caps every WIRED neighbour (their shape may have
-- changed too) — mirrors scripts/structures.lua's post/wall
-- cap-recompute (recapTileCorner).
--
-- No M.clear: there is no demolish/removal flow for any structure piece
-- yet (#359's "removal/rerouting" is explicitly deferred), and
-- structure.clear's async apply means an immediate neighbour re-cap
-- would still see the piece via the authoritative overlay (the staging
-- cache only fast-paths ADDS, not clears) — add it once a real caller
-- needs it, matching against that behaviour rather than guessing at it.
--
-- Placement always targets the ACTIVE world: the only callers are the
-- interactive build-tool picker and the construct_job build AI, neither
-- of which needs the worldId/baseZ escape hatches structures.lua's
-- location-stamping builders carry.

local M = {}

local PACK_PATH = "data/structure_packs/wire.yaml"

local packCache = nil
local function packDef()
    if packCache then return packCache end
    packCache = engine.loadYaml(PACK_PATH)
    if not packCache then
        engine.logWarn("wire: failed to load pack '" .. PACK_PATH .. "'")
    end
    return packCache
end

local handleCache = nil
local function handles()
    if handleCache then return handleCache end
    local pack = packDef()
    if not pack then return { conn = {}, connPath = {} } end
    local conn, connPath = {}, {}
    for name, path in pairs(pack.connections or {}) do
        conn[name] = engine.loadTexture(path)
        connPath[name] = path
    end
    handleCache = { conn = conn, connPath = connPath,
                    face = engine.loadTexture(pack.facemap),
                    facePath = pack.facemap }
    return handleCache
end

-- Cardinal neighbour offsets, matching the engine's slope-bit convention
-- (World.Slope: bit0=N(gy-1) bit1=E(gx+1) bit2=S(gy+1) bit3=W(gx-1)).
local NEIGHBOR_OFFSETS = { {0, -1}, {1, 0}, {0, 1}, {-1, 0} }

local function neighborsAt(gx, gy)
    return structure.hasAt(gx, gy - 1, "wire"),
           structure.hasAt(gx + 1, gy, "wire"),
           structure.hasAt(gx, gy + 1, "wire"),
           structure.hasAt(gx - 1, gy, "wire")
end

-- Which connection-shape a tile's 4-neighbour wire presence maps to.
-- Corner/tee names follow the wall-edge vocabulary (ne/nw/se/sw); a tee
-- is named by its MISSING side (e.g. tee_n = connected E+S+W, the open
-- side facing N).
local function shapeFor(n, e, s, w)
    local count = (n and 1 or 0) + (e and 1 or 0) + (s and 1 or 0) + (w and 1 or 0)
    if count == 0 then return "isolated" end
    if count == 4 then return "cross" end
    if count == 1 then
        if n then return "end_n"
        elseif e then return "end_e"
        elseif s then return "end_s"
        else return "end_w" end
    end
    if count == 3 then
        if not n then return "tee_n"
        elseif not e then return "tee_e"
        elseif not s then return "tee_s"
        else return "tee_w" end
    end
    -- count == 2
    if n and s then return "straight_ns" end
    if e and w then return "straight_ew" end
    if n and e then return "corner_ne" end
    if n and w then return "corner_nw" end
    if s and e then return "corner_se" end
    return "corner_sw"   -- s and w
end

-- (Re)place the wire piece at (gx,gy) with the shape its CURRENT
-- neighbours dictate. Does not touch neighbours — M.place re-caps those
-- separately below. Returns true/false (+ a reason on false) so
-- M.place can report the outcome of the tile the player actually
-- committed (a re-cap of an already-wired neighbour is incidental, not
-- the commit itself). Propagates structure.place's own result — it
-- returns false without placing anything when there's no active world
-- or the target chunk is unloaded (review round 7: this was previously
-- discarded, so a failed placement still recorded "accepted").
local function placeSelf(gx, gy)
    local h = handles()
    local shape = shapeFor(neighborsAt(gx, gy))
    local tex, path = h.conn[shape], h.connPath[shape]
    if not tex or not path then
        return false, "wire connection texture pack failed to load"
    end
    local z = (world.getTerrainAt(gx, gy) or 0) + 1
    local placed = structure.place(gx, gy, "wire", tex, h.face, z, path, h.facePath)
    if not placed then
        return false, "no active world or unloaded target chunk"
    end
    return true
end

-- Place wire at (gx,gy), deriving its shape from current neighbours, and
-- re-cap any wired neighbours whose own shape now includes this tile.
-- Order-independent, like structures.lua's wall/post placement.
function M.place(gx, gy)
    -- placeSelf's success path returns a single value (true, no second
    -- return), so failReason is already nil whenever ok is true —
    -- `reason = failReason` is correct as-is. Do NOT write this as
    -- `ok and nil or failReason`/`ok and nil or "some constant"`: with a
    -- non-nil constant fallback that idiom always selects the constant
    -- regardless of ok, because `ok and nil` collapses to a falsy value
    -- either way (review round 7 — a successful placement recorded a
    -- failure reason).
    local ok, failReason = placeSelf(gx, gy)
    debug.recordOutcome{
        kind = "wire.place", outcome = ok and "accepted" or "rejected",
        where = { x = gx, y = gy },
        reason = failReason,
    }
    for _, o in ipairs(NEIGHBOR_OFFSETS) do
        local nx, ny = gx + o[1], gy + o[2]
        if structure.hasAt(nx, ny, "wire") then
            placeSelf(nx, ny)
        end
    end
    return ok
end

return M
