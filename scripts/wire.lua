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

-- The pack this module loads. A field rather than a local constant for
-- the same reason scripts/structures.lua's `packDir` is one: a spec
-- points the REAL loader at a fixture pack to exercise parsing, texture
-- loading and registration together.
M.packPath = "data/structure_packs/wire.yaml"

local packCache = nil
local function packDef()
    if packCache then return packCache end
    packCache = engine.loadYaml(M.packPath)
    if not packCache then
        engine.logWarn("wire: failed to load pack '" .. M.packPath .. "'")
    end
    return packCache
end

-- #2488's one declaration rule, shared with scripts/structures.lua.
local frames = require("scripts.structure_frames")

-- A `connections.<name>` entry. Wire is the one pack whose appearances
-- each carry their OWN facemap (#2516): the static sprite, every
-- construction frame and every destruction frame is a colour image paired
-- with a facemap that describes exactly that image, so the declaring form
-- is the only complete one.
--
--   cross:
--     texture: assets/.../cross.png
--     facemap: assets/.../facemap/wire/cross.png
--     construction: [ .../build/cross_0.png, ... ]
--     construction_facemaps: [ .../facemap/wire/build/cross_0.png, ... ]
--     destruction:
--       fps: 10
--       frames:   [ .../break/cross_0.png, ... ]
--       facemaps: [ .../facemap/wire/break/cross_0.png, ... ]
--
-- The legacy scalar form (`cross: assets/.../cross.png`) is still READ, so
-- an old pack is reported precisely rather than as an unreadable file, but
-- it declares no facemap and the engine refuses the pack for that: there is
-- no shared-map fallback for Wire. Anything else -- a table with no
-- `texture`, a number, a boolean -- is a malformed pack and is reported
-- rather than silently skipped, because a missing connection makes the
-- engine refuse the whole pack with a less specific complaint.
local function connectionEntry(name, entry)
    if type(entry) == "string" then
        return entry, nil, nil, nil, nil
    elseif type(entry) == "table" and type(entry.texture) == "string" then
        return entry.texture, entry.facemap, entry.construction,
               entry.construction_facemaps, entry.destruction
    end
    engine.logWarn("wire: connection '" .. tostring(name) ..
        "' is neither a texture path nor a table with a `texture` path")
    return nil, nil, nil, nil, nil
end

-- Load a declared facemap path, or hand the engine none when the path is
-- not one a pack may name (it refuses the declaration, naming the escape)
-- or none was declared (it refuses the pack, naming the connection).
local function loadFacemap(path)
    if type(path) ~= "string" then return nil end
    if structure.isSafeArtPath and not structure.isSafeArtPath(path) then
        return nil
    end
    return engine.loadTexture(path)
end

local handleCache = nil
local function handles()
    if handleCache then return handleCache end
    local pack = packDef()
    if not pack then
        return { conn = {}, connPath = {}, face = {}, facePath = {},
                 connBuild = {}, connWreck = {} }
    end
    local conn, connPath, face, facePath, connBuild, connWreck =
        {}, {}, {}, {}, {}, {}
    for name, entry in pairs(pack.connections or {}) do
        local path, facemap, construction, constructionFaces, destruction =
            connectionEntry(name, entry)
        if path then
            conn[name] = engine.loadTexture(path)
            connPath[name] = path
            if type(facemap) == "string" then
                face[name] = loadFacemap(facemap)
                facePath[name] = facemap
            end
            connBuild[name] = frames.load(construction, constructionFaces)
            connWreck[name] = frames.loadDestruction(destruction)
        end
    end
    handleCache = { conn = conn, connPath = connPath, face = face,
                    facePath = facePath, connBuild = connBuild,
                    connWreck = connWreck }
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
    local art, construction, destruction = {}, {}, {}
    for _, name in ipairs(WIRE_SHAPES) do
        -- A connection with no facemap of its own is OMITTED, not given
        -- another's: the engine's refusal then names the missing
        -- connection, and there is no shared-map fallback (#2516).
        if h.connPath[name] and h.facePath[name] then
            art[#art + 1] = { kind = "wire", shape = name,
                              texture = h.connPath[name], texHandle = h.conn[name],
                              facemap = h.facePath[name],
                              faceHandle = h.face[name] }
        end
        -- #2488: each connection's own sequence, keyed to that exact
        -- shape. A connection with none resolves none — a run being
        -- built never borrows another shape's frames.
        --
        -- `~= nil`, not truthiness: `construction: false` decodes to Lua
        -- `false`, which structure_frames.load hands back unchanged for
        -- the engine to refuse as a non-array. Dropping it here would
        -- turn a malformed declaration into an absent one and let the
        -- pack register. Only `nil` is absence.
        if h.connPath[name] and h.connBuild[name] ~= nil then
            construction[#construction + 1] =
                { kind = "wire", shape = name,
                  texture = h.connPath[name], texHandle = h.conn[name],
                  frames = h.connBuild[name] }
        end
        -- #2491: the same, for the teardown clip. A connection with none
        -- resolves none — a cut wire never plays another shape's
        -- collapse — and the `fps` is forwarded exactly as authored.
        if h.connPath[name] and h.connWreck[name] ~= nil then
            local wreck = h.connWreck[name]
            destruction[#destruction + 1] =
                { kind = "wire", shape = name,
                  texture = h.connPath[name], texHandle = h.conn[name],
                  fps = (type(wreck) == "table") and wreck.fps or nil,
                  frames = (type(wreck) == "table") and wreck.frames or wreck }
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
        pack         = "wire",
        kinds        = { kindEntry },
        art          = art,
        construction = construction,
        destruction  = destruction,
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
    local face, facePath = h.face[shape], h.facePath[shape]
    if not tex or not path or not face or not facePath then
        return false, "wire connection texture pack failed to load"
    end
    local z = (world.getTerrainAt(gx, gy) or 0) + 1
    local placed = structure.place(gx, gy, "wire", tex, face, z, path, facePath)
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
