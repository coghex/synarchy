-- Test fixture for Test.Headless.Lua.TickInterval's #2205 specs — NOT
-- part of the game.
--
-- #2205 is about what the scheduler stores for a script whose schedule
-- was changed by a callback of the SAME pass. Observing that needs a
-- module whose update() runs an arbitrary engine call the spec chose,
-- so the spec can drive engine.setTickInterval, engine.pauseScript,
-- engine.resumeScript, engine.killScript and engine.loadScript from
-- inside a real runDueScripts pass rather than around one. It counts
-- its update() calls separately from the reentrant bodies it ran, so
-- "exactly one callback this pass" is checked rather than assumed, and
-- records the dt it was handed, so a mid-pass reschedule is shown NOT
-- to retime the callback the pass was already going to make.
--
-- Deliberately NOT a package.loaded singleton (as
-- lua_tick_interval_fixture.lua is not): markers live on a _G table so
-- they outlive the module, and each spec example builds its own Lua
-- backend — and therefore its own fresh _G.

local marks = _G.luaTickReentrancyFixture
if not marks then
    marks = { loads = 0, inits = 0, updates = 0, actions = 0 }
    _G.luaTickReentrancyFixture = marks
end

marks.loads = marks.loads + 1

local M = {}

function M.init()
    marks.inits = marks.inits + 1
end

-- The engine's timed callback. `marks.action`, when the spec has
-- installed one, is the reentrant body: it receives this script's own
-- id (the spec stores it as `marks.sid` after loading, since the module
-- is never told its id) and the dt this update was handed.
function M.update(dt)
    marks.updates = marks.updates + 1
    marks.lastDt = dt
    local action = marks.action
    if action then
        marks.actions = marks.actions + 1
        action(marks.sid, dt)
    end
end

return M
