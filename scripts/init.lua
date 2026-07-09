-- Main initialization script. Thin top-level orchestrator (#543): it
-- owns the engine-facing `game` entrypoint and delegates script
-- loading/shutdown, gameplay mouse routing, and gameplay key routing
-- to smaller modules so this file stays reviewable.
local game = {}

function game.init(scriptId)
    require("scripts.init_loader").load()
end

function game.update(dt)
    -- Lazily resolve any structure texture-palette ids that need a runtime
    -- handle (after a save/load). No-op in steady state.
    require("scripts.structures").resolvePending()
end

function game.onMouseDown(button, x, y)
    require("scripts.init_mouse").onMouseDown(button, x, y)
end

function game.onMouseUp(button, x, y, downRoute)
    require("scripts.init_mouse").onMouseUp(button, x, y, downRoute)
end

function game.onKeyDown(key)
    require("scripts.init_keys").onKeyDown(key)
end

function game.onKeyUp(key)
    require("scripts.init_keys").onKeyUp(key)
end

function game.shutdown()
    require("scripts.init_loader").shutdown()
end

return game
