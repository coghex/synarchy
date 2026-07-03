-- Movement test arena — no-generator obstacle courses for exercising and
-- refining unit movement (pathing, climbs, falls, ramps), headless or in-GUI.
-- Companion harness: tools/movement_probe.py.
--
-- The arena starts as flat loam at z=0 (world.initArena). Course builders
-- sculpt it with the tile-edit Lua API:
--   world.addTile(page,gx,gy,mat)        raise a column +1z (stacks = cliff/wall)
--   world.deleteTile(page,gx,gy)         lower a column
--   world.setFluidTile(page,gx,gy,kind)  "ocean"/"lava" = impassable,
--                                         "river"/"lake" = wadeable (penalty)
--   world.setSlope(page,gx,gy,z,bits)    author a walkable 1-z ramp.
--                                         bits: 1=N 2=E 4=S 8=W (a set bit marks
--                                         that cardinal neighbour as a ramp DOWN).
--
-- addTile only ever produces flat tops (slope 0 = cliff), so setSlope is the
-- ONLY way to make a step walkable — that's why the engine primitive exists.
--
-- Every course builder returns { name, sx, sy, gx, gy, note } so the harness
-- knows where to spawn the unit (sx,sy) and where to send it (gx,gy).
--
-- Interactive use (GUI shell or netcat):
--   require('scripts.movement_arena').buildCourse('corner_trap')
--   unit.spawn('acolyte', 0, 0); unit.moveTo(<uid>, 4, 0, 2.0)

local M = {}

M.page    = "move_test"
M.baseZ   = 0          -- seaLevel: arena flat-ground elevation
M.created = false

local LOAM    = 56
local GRANITE = 1

-- Slope bits, matching isCliffStep (bit0=N bit1=E bit2=S bit3=W).
local SLOPE_N, SLOPE_E, SLOPE_S, SLOPE_W = 1, 2, 4, 8

----------------------------------------------------------------------
-- Low-level builders (also handy interactively)
----------------------------------------------------------------------

-- Raise one column by `height` tiles of `mat` (stacks form a cliff/wall).
function M.raise(gx, gy, height, mat)
    mat = mat or LOAM
    for _ = 1, (height or 1) do
        world.addTile(M.page, gx, gy, mat)
    end
end

-- Apply f(gx,gy) over an inclusive rectangle.
function M.rect(x1, y1, x2, y2, f)
    for gx = x1, x2 do
        for gy = y1, y2 do
            f(gx, gy)
        end
    end
end

-- Raise a rectangular plateau to `height`.
function M.plateau(x1, y1, x2, y2, height, mat)
    M.rect(x1, y1, x2, y2, function(gx, gy) M.raise(gx, gy, height, mat) end)
end

function M.fluid(gx, gy, kind)
    world.setFluidTile(M.page, gx, gy, kind or "ocean")
end

function M.fluidRect(x1, y1, x2, y2, kind)
    M.rect(x1, y1, x2, y2, function(gx, gy) M.fluid(gx, gy, kind) end)
end

function M.setSlope(gx, gy, z, bits)
    world.setSlope(M.page, gx, gy, z, bits)
end

----------------------------------------------------------------------
-- Arena lifecycle
----------------------------------------------------------------------

-- Create the flat arena world. `withTextures` is only needed for visual
-- (GUI) use — headless has no GPU and skips it. Best-effort: reuses
-- test_arena's texture set if available.
function M.create(withTextures)
    world.initArena(M.page)
    if withTextures then
        pcall(function()
            require("scripts.test_arena").sendTextures(M.page)
        end)
    end
    world.initArenaDone(M.page)
    world.show(M.page)
    M.created = true
end

-- Tear down and recreate a clean flat arena (for running several courses
-- in one session).
function M.reset(withTextures)
    if M.created then
        pcall(function() world.destroy(M.page) end)
        M.created = false
    end
    M.create(withTextures)
end

----------------------------------------------------------------------
-- Courses. Each assumes a fresh flat arena and sculpts it in place.
----------------------------------------------------------------------

M.courses = {}

-- Baseline: a straight flat walk. Sanity that movement works at all and
-- the unit arrives + goes idle.
M.courses.flat = function()
    return { name = "flat", sx = -3, sy = 0, gx = 6, gy = 0,
             note = "straight flat walk, no obstacles" }
end

-- Corner trap (symptom #2). An ocean wall straddles the straight line
-- from start to goal, so the unit must round one end of the wall. Rounding
-- the corner forces a diagonal step whose axis-neighbour is ocean — the
-- exact case where, pre-fix, A* proposed a corner-cut the stepper rejected,
-- so the unit froze in the walking animation replanning forever. Post-fix
-- A* routes cardinally around the corner and the unit reaches the goal.
M.courses.corner_trap = function()
    for gy = -1, 1 do M.fluid(2, gy, "ocean") end
    return { name = "corner_trap", sx = 0, sy = 0, gx = 4, gy = 0,
             note = "ocean wall x=2,y=-1..1; round an end (diagonal corner-cut)" }
end

-- Cliff climb (symptoms #4/#6). A 3-high plateau east of the start; the
-- goal sits on top, so the unit walks to the cliff base and climbs
-- (Climbing → Crawling → Standing pose chain).
M.courses.cliff = function()
    M.plateau(3, -3, 7, 3, 3, LOAM)
    return { name = "cliff", sx = 0, sy = 0, gx = 5, gy = 0,
             note = "3-high plateau x>=3; unit climbs the cliff to a goal on top" }
end

-- 1-z cliff. With height-aware climbing, a tall unit (acolyte ~1.8 m,
-- reach ~1 z) should do ~no vertical wall-climb and go almost straight
-- into the pullup, mantling up + forward onto the ledge.
M.courses.cliff1 = function()
    M.plateau(3, -3, 7, 3, 1, LOAM)
    return { name = "cliff1", sx = 0, sy = 0, gx = 5, gy = 0,
             note = "1-high plateau x>=3 (cliff, no ramp); acolyte should pullup almost immediately" }
end

-- Fall edge. The unit starts on a 3-high plateau and the goal is on the
-- low ground east of it, so it walks off the edge and falls (Standing →
-- Falling → landing outcome).
M.courses.fall_edge = function()
    M.plateau(-7, -3, 0, 3, 3, LOAM)
    return { name = "fall_edge", sx = -3, sy = 0, gx = 4, gy = 0,
             note = "start on a 3-high plateau (x<=0); walk east off the edge, fall to z=0" }
end

-- Ramp (symptom #3). A 1-high plateau whose west edge is authored as a
-- WALKABLE ramp via setSlope. The unit should WALK up the ramp (no climb
-- transition), not treat it as a cliff. Exercises isCliffStep + the ramp
-- cost the planner sees.
M.courses.ramp = function()
    M.plateau(3, -3, 7, 3, 1, LOAM)
    -- West edge (x=3) ramps down toward the start: src is W of dst, so the
    -- dst tile needs the W slope bit, set at the plateau top z (baseZ+1).
    for gy = -3, 3 do M.setSlope(3, gy, M.baseZ + 1, SLOPE_W) end
    return { name = "ramp", sx = 0, sy = 0, gx = 5, gy = 0,
             note = "1-high plateau with a walkable ramp on its west edge; should WALK up, not climb" }
end

-- Ramp-vs-cliff detour (symptom #3, stronger). A long 1-high wall blocks
-- the straight path; only a narrow section is a walkable ramp. The unit
-- should prefer the ramp over climbing the cliff face elsewhere.
M.courses.ramp_detour = function()
    M.plateau(3, -4, 3, 4, 1, LOAM)          -- 1-high wall at x=3, y=-4..4
    M.plateau(4, -4, 7, 4, 1, LOAM)          -- plateau body east of the wall
    -- Only y=0 is a walkable ramp up; the rest of x=3 stays a cliff.
    M.setSlope(3, 0, M.baseZ + 1, SLOPE_W)
    return { name = "ramp_detour", sx = 0, sy = 0, gx = 6, gy = 0,
             note = "1-high wall x=3; only y=0 is a ramp — unit should funnel to it, not climb" }
end

-- Sustained uphill (#375). A 6-step staircase of walkable ramps rising
-- 1 z per tile (x=3..8), topped by a flat plateau — every step is a
-- ramp, so the unit WALKS the whole ascent (no climb transitions) but
-- spends a long stretch on a full uphill grade. The flat approach
-- (x<3) gives the harness a same-run baseline speed to compare the
-- ascent against, and the sustained grade is what drains stamina in
-- the uphill stamina phase.
M.courses.ramp_climb = function()
    for i = 0, 5 do
        -- Column x=3+i stands i+1 high; its top is a W-facing ramp
        -- tapering down to the previous step.
        M.plateau(3 + i, -3, 3 + i, 3, i + 1, LOAM)
        for gy = -3, 3 do M.setSlope(3 + i, gy, M.baseZ + i + 1, SLOPE_W) end
    end
    M.plateau(9, -3, 12, 3, 6, LOAM)   -- flat summit for the goal
    return { name = "ramp_climb", sx = -3, sy = 0, gx = 11, gy = 0,
             note = "6-tile ramp staircase x=3..8 (+1z each); walk the whole ascent, slower than the flat approach" }
end

-- Ramp-vs-cliff CHOICE (symptom #3). A 1-high plateau whose face is a
-- cliff straight ahead of the unit, but with a single walkable ramp a
-- short detour to the north. With the cost fix (ramps cheap, cliffs
-- dear) the unit should detour to the ramp and WALK up (no climb pose);
-- pre-fix it charged the ramp like a cliff and just climbed the nearer
-- face. Validates that the planner prefers ramps over cliffs.
M.courses.ramp_choice = function()
    M.plateau(3, -4, 7, 4, 1, LOAM)
    -- One walkable ramp: west face of (3,-2) tapers down to (2,-2).
    -- src is W of dst, so the dst tile needs the W slope bit (8).
    M.setSlope(3, -2, M.baseZ + 1, SLOPE_W)
    return { name = "ramp_choice", sx = 0, sy = 0, gx = 5, gy = 0,
             note = "cliff straight ahead + a ramp 2 tiles north; should detour to the ramp, not climb" }
end

-- Build a named course on a fresh arena. Returns the course descriptor
-- table (auto-serialized to JSON over the debug console), or nil if the
-- course name is unknown.
function M.buildCourse(name, withTextures)
    if not M.created then M.create(withTextures) end
    local c = M.courses[name]
    if not c then return nil end
    return c()
end

-- List available course names (for the harness / interactive discovery).
function M.listCourses()
    local names = {}
    for k, _ in pairs(M.courses) do names[#names + 1] = k end
    table.sort(names)
    return names
end

return M
