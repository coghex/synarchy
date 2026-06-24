-- Structures debug builder support.
--
-- Loads the dungeon_1 pack (textures + facemaps), maps a clicked tile
-- quarter to a wall edge, and places pieces via the engine `structure`
-- API. Used by the debug overlay's "structures" section + init.lua's
-- click dispatch. Throwaway-ish debug tooling; data-driven packs come
-- later.

local M = {}

local DIR  = "assets/textures/buildings/dungeon_1/"
local FDIR = "assets/textures/world/facemap/"

-- engine slot -> { sprite, facemap } filenames.
local PIECES = {
    floor   = { DIR .. "floor.png",   FDIR .. "floorface.png" },
    ceiling = { DIR .. "ceiling.png", FDIR .. "ceilingface.png" },
    wall_ne = { DIR .. "wall_ne.png", FDIR .. "wallface_ne.png" },
    wall_nw = { DIR .. "wall_nw.png", FDIR .. "wallface_nw.png" },
    wall_se = { DIR .. "wall_se.png", FDIR .. "wallface_se.png" },
    wall_sw = { DIR .. "wall_sw.png", FDIR .. "wallface_sw.png" },
    post    = { DIR .. "post.png",    FDIR .. "postface.png" },
}

-- The placeable kinds shown in the debug list (pack "dungeon_1").
M.pack  = "dungeon_1"
M.kinds = { "wall", "floor", "ceiling", "post" }

local cache = nil
local function handles()
    if cache then return cache end
    cache = {}
    for slot, p in pairs(PIECES) do
        cache[slot] = { tex = engine.loadTexture(p[1]),
                        face = engine.loadTexture(p[2]) }
    end
    return cache
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

-- Place `kind` at tile (gx,gy). hx/hy = fractional hover (for the wall
-- quarter). Floor sits at the tile surface; walls at the surface; ceiling
-- one z up; post at the back corner. Returns the slot placed, or nil.
function M.placeKind(gx, gy, kind, hx, hy)
    local h = handles()
    -- surface + 1: structures sit in the air cell ON TOP of the solid terrain
    -- (a floor laid on the ground), not at the terrain tile's own z level.
    local z = (world.getTerrainAt(gx, gy) or 0) + 1
    -- `slot` is the engine slot; `texkey` is the texture to draw with (all 4
    -- corner posts share the one symmetric post sprite — the render insets it).
    local slot, texkey
    if kind == "floor" then
        slot = "floor"; texkey = "floor"
    elseif kind == "ceiling" then
        slot = "ceiling"; texkey = "ceiling"; z = z + 1
    elseif kind == "post" then
        -- Posts ONLY render the corners of an existing FLOOR. Gate on a floor
        -- being present and take ITS z, so the post sits on the floor (never
        -- on bare terrain, which is what made stray posts float off-grid).
        local fz = structure.floorZAt(gx, gy)
        if not fz then return nil end
        z = fz
        slot = "post_" .. M.quarterCorner(hx or (gx + 0.5), hy or (gy + 0.5))
        texkey = "post"
    elseif kind == "wall" then
        local e = M.quarterEdge(hx or (gx + 0.5), hy or (gy + 0.5))
        slot = "wall_" .. e; texkey = "wall_" .. e
    else
        return nil
    end
    local p = h[texkey]
    if not p then return nil end
    structure.place(gx, gy, slot, p.tex, p.face, z)
    return slot
end

function M.clear() structure.clearAll() end

return M
