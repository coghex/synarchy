-- Test fixture for Test.Headless.Lua.ScriptState's #1059 specs — NOT
-- part of the game.
--
-- engine.loadScript must run a module's chunk on the Lua backend's own
-- canonical state (lbsLuaState), NOT on whatever state the handler was
-- invoked with. Those two differ exactly when a script calls
-- engine.loadScript from inside a coroutine, so this fixture records —
-- at chunk-execution time, and again inside init/shutdown — whether it
-- is running on the main state. That lets the test tell the two states
-- apart instead of only observing that the load "worked".
--
-- Deliberately NOT a package.loaded singleton (unlike every real script
-- module): the specs load, kill, and reload this exact path, and a
-- fresh module table per load is what makes the reload observable.
-- Markers live on a _G table instead, so they survive the kill.

local marks = _G.luaScriptStateFixture
if not marks then
    marks = { loads = 0, inits = 0, shutdowns = 0 }
    _G.luaScriptStateFixture = marks
end

local _, chunkOnMain = coroutine.running()
marks.loads = marks.loads + 1
marks.loadedOnMain = chunkOnMain

local M = {}

function M.init()
    local _, initOnMain = coroutine.running()
    marks.inits = marks.inits + 1
    marks.initOnMain = initOnMain
end

function M.shutdown()
    local _, shutdownOnMain = coroutine.running()
    marks.shutdowns = marks.shutdowns + 1
    marks.shutdownOnMain = shutdownOnMain
end

return M
