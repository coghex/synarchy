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

-- Which neighbours a wire at (gx,gy) connects to. The engine owns this
-- (#1842) so the construction render pass and the placer cannot drift:
-- one canonical, seam-aware (#1175) lookup of the same placed + staged
-- wire `structure.hasAt` reads. The designation-aware variant is the
-- render pass's — `structure.wireNeighbors(gx, gy, page, true)` — and
-- placement must NOT use it, or laying a run would recap neighbours
-- against wire that is not there yet.
local function neighborsAt(gx, gy)
    local n = structure.wireNeighbors(gx, gy)
    return n.n, n.e, n.s, n.w
end

-- Which connection-shape a tile's 4-neighbour wire presence maps to.
-- Also engine-side now (#1842): ONE sixteen-way rule, shared with the
-- render pass, rather than a second table to keep in step. Corner/tee
-- names follow the wall-edge vocabulary (ne/nw/se/sw); a tee is named by
-- its MISSING side (e.g. tee_n = connected E+S+W, the open side facing N).
local function shapeFor(n, e, s, w)
    return structure.wireShape(n, e, s, w)
end

-- Every connection variant the pack must declare, in the shape rule's
-- own vocabulary.
local WIRE_SHAPES = {
    "isolated", "end_n", "end_e", "end_s", "end_w",
    "straight_ns", "straight_ew",
    "corner_ne", "corner_nw", "corner_se", "corner_sw",
    "tee_n", "tee_e", "tee_s", "tee_w", "cross",
}

-- Declare the wire pack's per-shape art for UNPLACED pieces (#1842), so
-- the construction render pass can resolve what a wire designation would
-- be BUILT with without calling into Lua. One call per session,
-- registered all-or-nothing engine-side; a shape the pack never declared
-- is OMITTED rather than sent with a nil path, so the engine's refusal
-- names the missing connection instead of rejecting the payload as
-- unreadable. It reports the failure itself — nothing warns here.
--
-- The pack NAME is the literal "wire": that is what a wire designation
-- carries (scripts/build_tool.lua's `pack = "wire"`), and the catalogue
-- is keyed by the designation's own spelling.
local registeredArt = false
function M.registerPackArt()
    if registeredArt or not structure.registerPackArt then return end
    local pack = packDef()
    if not pack then return end
    registeredArt = true
    local h = handles()
    local b = pack.build and pack.build.wire
    local art = {}
    for _, name in ipairs(WIRE_SHAPES) do
        if h.connPath[name] and h.facePath then
            art[#art + 1] = { kind = "wire", shape = name,
                              texture = h.connPath[name], texHandle = h.conn[name],
                              facemap = h.facePath, faceHandle = h.face }
        end
    end
    -- #1844: a buildable kind states its exact cost, which is what the
    -- engine charges and what a refund receipt records.
    local buildable = (b ~= nil) and (b.build_work ~= nil)
                      and (b.materials ~= nil)
    local kindEntry = { kind = "wire", buildable = buildable }
    if buildable then
        kindEntry.build_work = b.build_work
        kindEntry.materials = b.materials
    end
    structure.registerPackArt{
        pack  = "wire",
        kinds = { kindEntry },
        art   = art,
    }
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
