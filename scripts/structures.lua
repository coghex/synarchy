-- Structures debug builder support.
--
-- Loads the dungeon_1 pack (textures + facemaps), maps a clicked tile
-- quarter to a wall edge, and places pieces via the engine `structure`
-- API. Used by the debug overlay's "structures" section + init.lua's
-- click dispatch. Throwaway-ish debug tooling; data-driven packs come
-- later.

local M = {}

-- Pack def is data-driven: data/structure_packs/<pack>.yaml gives every
-- texture/facemap PATH (read once via engine.loadYaml). A new pack = a new
-- yaml; no Lua changes. Walls carry 4 cap-variant facemaps keyed "<left><right>"
-- (1 = pillar notch carved that end; 00 = full wall).
M.pack  = "dungeon_1"
local PACK_DIR  = "data/structure_packs/"
local WALL_DIRS = { "ne", "nw", "se", "sw" }
local WALL_CAPS = { "00", "01", "10", "11" }

-- The placeable kinds shown in the debug list.
M.kinds = { "wall", "floor", "ceiling", "post" }

-- Temporary: log post/wall placement nodes to diagnose cap mismatches.
M.debug = false

local packCache = nil
local function packDef()
    if packCache then return packCache end
    packCache = engine.loadYaml(PACK_DIR .. M.pack .. ".yaml")
    if not packCache then
        engine.logWarn("structures: failed to load pack '" .. M.pack .. "'")
    end
    return packCache
end

-- Handle caches keyed by variant name ("" = the default art). A variant
-- (#91: pack yaml `variants.<name>`) overrides any subset of piece/wall
-- textures (and optionally facemaps); everything it doesn't list falls
-- back to the default, so an unknown or partial variant still renders.
local cache = {}
local function handles(variant)
    local key = variant or ""
    if cache[key] then return cache[key] end
    local pack = packDef()
    if not pack then return { walls = {} } end
    local over = { pieces = {}, walls = {} }
    if variant then
        local v = pack.variants and pack.variants[variant]
        if v then
            over = { pieces = v.pieces or {}, walls = v.walls or {} }
        else
            engine.logWarn("structures: pack '" .. M.pack ..
                "' has no variant '" .. tostring(variant) .. "' — using default art")
        end
    end
    local h = { walls = {} }
    -- non-wall pieces: handle + PATH for both texture and facemap (the path is
    -- passed to structure.place too → interned into the save palette).
    for slot, p in pairs(pack.pieces) do
        local o = over.pieces[slot] or {}
        local texPath, facePath = o.texture or p.texture, o.facemap or p.facemap
        h[slot] = { tex = engine.loadTexture(texPath), texPath = texPath,
                    face = engine.loadTexture(facePath), facePath = facePath }
    end
    -- walls: one sprite + the 4 cap facemap variants (handles + paths)
    for _, e in ipairs(WALL_DIRS) do
        local w = pack.walls[e]
        local o = over.walls[e] or {}
        local texPath = o.texture or w.texture
        local faces, facePaths = {}, {}
        for _, c in ipairs(WALL_CAPS) do
            local fp = (o.facemaps and o.facemaps[c]) or w.facemaps[c]
            faces[c]     = engine.loadTexture(fp)
            facePaths[c] = fp
        end
        h.walls[e] = { tex = engine.loadTexture(texPath), texPath = texPath,
                       face = faces, facePath = facePaths }
    end
    cache[key] = h
    return h
end

-- Map a fractional in-tile hover position to the nearest diamond edge
-- (FaceSouth screen convention): +gx = screen SE, +gy = screen SW.
function M.quarterEdge(hx, hy)
    local fx = hx - math.floor(hx)
    local fy = hy - math.floor(hy)
    local dSE = 1 - fx   -- toward high gx
    local dNW = fx       -- toward low gx
    local dSW = 1 - fy   -- toward high gy
    local dNE = fy       -- toward low gy
    local m = math.min(dSE, dNW, dSW, dNE)
    if     m == dSE then return "se"
    elseif m == dNW then return "nw"
    elseif m == dSW then return "sw"
    else                 return "ne" end
end

-- Map a fractional in-tile hover position to the nearest CORNER (diamond
-- vertex). N=(low gx, low gy), E=(high gx, low gy), S=(high gx, high gy),
-- W=(low gx, high gy).
function M.quarterCorner(hx, hy)
    local fx = hx - math.floor(hx)
    local fy = hy - math.floor(hy)
    local cx = fx > 0.5   -- toward high gx
    local cy = fy > 0.5   -- toward high gy
    if     (not cx) and (not cy) then return "n"
    elseif cx and (not cy)       then return "e"
    elseif cx and cy             then return "s"
    else                              return "w" end
end

-- A wall edge -> its two end corners, ordered {left,right} by canvas-x so they
-- map to the facemap's _<left><right> suffix. N/S sit at canvas centre (x48),
-- E at x96, W at x0 — so the lower-x vertex is the "left" end. If a cap lands
-- on the wrong end of some direction, swap that pair.
local WALL_ENDS = { ne = {"n","e"}, nw = {"w","n"},
                    se = {"s","e"}, sw = {"w","s"} }

-- A tile corner letter -> the two wall edges of THAT SAME tile which end at it.
-- Used to re-cap a tile's own walls when a post is added/removed at a corner.
local CORNER_WALLS = { n = {"ne","nw"}, e = {"ne","se"},
                       s = {"se","sw"}, w = {"nw","sw"} }

-- Place (or re-place) the wall on edge `e` of tile (gx,gy). Caps each end ONLY
-- from THIS tile's own corner post — a post on a neighbouring tile (which
-- shares the end node) must NOT cap this wall, else a post placed for the next
-- segment bleeds across a gap onto this wall's clean end.
-- worldId (optional) targets a specific world page's terrain — locations
-- stamped on a hidden/non-active page must read THAT page's height, not the
-- active world's (#89). nil → the active world (the click-placement path).
-- variant (optional, #91) selects the pack's variant art (e.g. "damaged");
-- nil → the default. NB a re-cap re-places the wall with the CALLER's
-- variant — pass the variant the wall was built with.
local function placeWall(gx, gy, e, worldId, baseZ, variant)
    local h = handles(variant)
    local z = (baseZ or world.getTerrainAt(gx, gy, worldId) or 0) + 1
    local ends = WALL_ENDS[e]   -- {leftCorner, rightCorner}
    local capL = structure.hasAt(gx, gy, "post_" .. ends[1], worldId)
    local capR = structure.hasAt(gx, gy, "post_" .. ends[2], worldId)
    local suffix = (capL and "1" or "0") .. (capR and "1" or "0")
    if M.debug then
        engine.logInfo(string.format("[wall] tile %d,%d %s -> _%s  (L=%s post_%s  R=%s post_%s)",
            gx, gy, e, suffix, tostring(capL), ends[1], tostring(capR), ends[2]))
    end
    local w = h.walls[e]
    structure.place(gx, gy, "wall_" .. e, w.tex, w.face[suffix], z,
                    w.texPath, w.facePath[suffix], worldId)
end

-- (M.wall / recapTileCorner pass the levelled baseZ through; the
-- click-placement path omits it and reads the active world's terrain.)

-- A post just changed at tile (gx,gy)'s `corner`: re-cap that tile's own two
-- walls touching the corner, so wall-then-post and post-then-wall converge.
local function recapTileCorner(gx, gy, corner, worldId, baseZ, variant)
    for _, e in ipairs(CORNER_WALLS[corner]) do
        if structure.hasAt(gx, gy, "wall_" .. e, worldId) then
            placeWall(gx, gy, e, worldId, baseZ, variant)
        end
    end
end

-- Place `kind` at tile (gx,gy). hx/hy = fractional hover (for the wall edge /
-- post corner). Returns the slot placed, or nil. Order-independent: placing a
-- post re-caps the walls around its node, and a wall caps to existing posts.
function M.placeKind(gx, gy, kind, hx, hy)
    local h = handles()
    -- surface + 1: structures sit in the air cell ON TOP of the solid terrain
    -- (a floor laid on the ground), not at the terrain tile's own z level.
    local z = (world.getTerrainAt(gx, gy) or 0) + 1
    if kind == "floor" then
        structure.place(gx, gy, "floor", h.floor.tex, h.floor.face, z,
                    h.floor.texPath, h.floor.facePath)
        return "floor"
    elseif kind == "ceiling" then
        structure.place(gx, gy, "ceiling", h.ceiling.tex, h.ceiling.face, z + 1,
                        h.ceiling.texPath, h.ceiling.facePath)
        return "ceiling"
    elseif kind == "post" then
        -- Posts ONLY render the corners of an existing FLOOR. Gate on a floor
        -- being present and take ITS z, so the post sits on the floor (never
        -- on bare terrain, which is what made stray posts float off-grid).
        local fz = structure.floorZAt(gx, gy)
        if not fz then return nil end
        local corner = M.quarterCorner(hx or (gx + 0.5), hy or (gy + 0.5))
        structure.place(gx, gy, "post_" .. corner, h.post.tex, h.post.face, fz,
                    h.post.texPath, h.post.facePath)
        if M.debug then
            engine.logInfo(string.format("[post] tile %d,%d corner %s", gx, gy, corner))
        end
        -- re-cap THIS tile's own walls touching the corner (order-independence)
        recapTileCorner(gx, gy, corner)
        return "post_" .. corner
    elseif kind == "wall" then
        local e = M.quarterEdge(hx or (gx + 0.5), hy or (gy + 0.5))
        placeWall(gx, gy, e)
        return "wall_" .. e
    end
    return nil
end

-----------------------------------------------------------
-- Programmatic placement (no hover) — for builders like locations.room_small.
-- These name the exact slot/edge/corner instead of deriving it from a click.
-----------------------------------------------------------

-- The programmatic builders take an optional trailing `worldId` (the page
-- to author on / read terrain from); nil → the active world. Location
-- stamping passes it so a hidden page's room reads that page's terrain.
-- The programmatic builders take an optional trailing baseZ — the levelled
-- ground a stamped room sits on (locations.flattenFootprint). When given, the
-- piece is placed at that explicit z instead of re-reading terrain (which,
-- right after the async flatten edits, would still report the old bumps).
-- The optional trailing `variant` (#91) selects the pack's variant art
-- (e.g. "damaged"); nil → the default. The chosen texture PATH is what the
-- piece persists (structure palette), so a variant survives save/load.
function M.floor(gx, gy, worldId, baseZ, variant)
    local h = handles(variant)
    local z = (baseZ or world.getTerrainAt(gx, gy, worldId) or 0) + 1
    structure.place(gx, gy, "floor", h.floor.tex, h.floor.face, z,
                    h.floor.texPath, h.floor.facePath, worldId)
end

function M.ceiling(gx, gy, worldId, baseZ, variant)
    local h = handles(variant)
    local z = (baseZ or world.getTerrainAt(gx, gy, worldId) or 0) + 2   -- one level above the floor
    structure.place(gx, gy, "ceiling", h.ceiling.tex, h.ceiling.face, z,
                    h.ceiling.texPath, h.ceiling.facePath, worldId)
end

-- corner ∈ "n"/"e"/"s"/"w". Gated to a floor (like click placement); re-caps
-- this tile's walls touching the corner. Returns true if placed. The post z
-- comes from the existing floor (read from the same page), so it needs no
-- terrain read.
function M.post(gx, gy, corner, worldId, baseZ, variant)
    local fz = structure.floorZAt(gx, gy, worldId)
    if not fz then return false end
    local h = handles(variant)
    structure.place(gx, gy, "post_" .. corner, h.post.tex, h.post.face, fz,
                    h.post.texPath, h.post.facePath, worldId)
    recapTileCorner(gx, gy, corner, worldId, baseZ, variant)
    return true
end

-- edge ∈ "ne"/"nw"/"se"/"sw". Caps to existing posts on this tile.
function M.wall(gx, gy, edge, worldId, baseZ, variant)
    placeWall(gx, gy, edge, worldId, baseZ, variant)
end

function M.clear() structure.clearAll() end

-- Resolve any texture-palette ids that lack a runtime handle (after a load the
-- engine clears the session-local handle map; structures replay from sdEdits
-- but their handles must be re-loaded for THIS session). Cheap when there's
-- nothing pending — the common steady-state. Call each tick.
function M.resolvePending()
    local u = structure.unresolvedPaletteIds()
    for _, e in ipairs(u) do
        structure.setPaletteHandle(e.id, engine.loadTexture(e.path))
    end
end

return M
