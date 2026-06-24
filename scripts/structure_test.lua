-- Manual test harness for the structures render path (Stage 1).
--
-- Load once:   engine.loadScript('scripts/structure_test.lua', 0.1)
-- Then call (in the debug console / shell):
--   require('scripts.structure_test').cell(gx, gy)   -- full room cell
--   require('scripts.structure_test').wall(gx, gy, 'ne')
--   require('scripts.structure_test').place(gx, gy, 'floor')
--   require('scripts.structure_test').clear()        -- wipe all
--
-- Loads the dungeon_1 pack once (cached) so you can eyeball the
-- wall/floor/ceiling rendering, the facemap sun-shading, and the
-- per-slot draw order. This is throwaway scaffolding — the real
-- debug-layer "structures" builder (Stage 2) replaces it.

local M = {}

local DIR  = "assets/textures/buildings/dungeon_1/"
local FDIR = "assets/textures/world/facemap/"

-- slot -> { sprite, facemap } filenames
local PIECES = {
    floor   = { "floor.png",   "floorface.png" },
    ceiling = { "ceiling.png", "ceilingface.png" },
    wall_ne = { "wall_ne.png", "wallface_ne.png" },
    wall_nw = { "wall_nw.png", "wallface_nw.png" },
    wall_se = { "wall_se.png", "wallface_se.png" },
    wall_sw = { "wall_sw.png", "wallface_sw.png" },
}

local cache = nil
local function handles()
    if cache then return cache end
    cache = {}
    for slot, paths in pairs(PIECES) do
        cache[slot] = {
            tex  = engine.loadTexture(DIR  .. paths[1]),
            face = engine.loadTexture(FDIR .. paths[2]),
        }
    end
    return cache
end

-- Place a single piece (z defaults to 0).
function M.place(gx, gy, slot, z)
    local h = handles()[slot]
    if not h then
        engine.logWarn("structure_test: unknown slot " .. tostring(slot))
        return false
    end
    return structure.place(gx, gy, slot, h.tex, h.face, z or 0)
end

-- Place one wall on edge "ne"/"nw"/"se"/"sw".
function M.wall(gx, gy, edge, z)
    return M.place(gx, gy, "wall_" .. edge, z)
end

-- Place a full room cell at (gx,gy): floor, all 4 walls, ceiling one z up.
function M.cell(gx, gy)
    M.place(gx, gy, "floor", 0)
    M.wall(gx, gy, "ne", 0); M.wall(gx, gy, "nw", 0)
    M.wall(gx, gy, "se", 0); M.wall(gx, gy, "sw", 0)
    M.place(gx, gy, "ceiling", 1)
    return structure.count()
end

function M.clear() structure.clearAll() end

return M
