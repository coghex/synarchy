-- Test fixture for Test.Headless.Lua.TickInterval's #1695 specs — NOT
-- part of the game.
--
-- The tick-interval policy distinguishes a script that never ticks on a
-- timer (interval 0, "event-only") from one that does, while leaving
-- BOTH reachable by broadcast. Observing that needs a module that
-- records its update() calls separately from its broadcast callbacks,
-- so a spec can assert "never updated, still broadcast to" rather than
-- only that a load succeeded. It also records the dt it was handed, so
-- the interval reaching update() unchanged is checked rather than
-- assumed.
--
-- Deliberately NOT a package.loaded singleton (as
-- lua_script_state_fixture.lua is not): markers live on a _G table so
-- they outlive the module, and each spec example builds its own Lua
-- backend — and therefore its own fresh _G.

local marks = _G.luaTickIntervalFixture
if not marks then
    marks = { loads = 0, inits = 0, updates = 0, broadcasts = 0 }
    _G.luaTickIntervalFixture = marks
end

marks.loads = marks.loads + 1

local M = {}

function M.init()
    marks.inits = marks.inits + 1
end

-- The engine's timed callback.
function M.update(dt)
    marks.updates = marks.updates + 1
    marks.lastDt = dt
end

-- An ordinary engine broadcast, which every loaded module receives
-- regardless of its interval.
function M.onTickIntervalProbe()
    marks.broadcasts = marks.broadcasts + 1
end

return M
