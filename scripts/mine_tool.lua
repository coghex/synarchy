-- Mine designation tool (DF-style two-click rectangle).
--
--   * Toolbar "tool_mine" puts the world in mine tool mode; the world
--     render pass shows the hover cursor.
--   * First left-click anchors the rectangle (world.setMineAnchor);
--     the render pass previews anchor→hover as a grid of cursors.
--   * Second left-click commits (world.designateMine) — the world
--     thread stores every loaded tile in the rect with its surface z
--     and renders a marker over each until something consumes them
--     (the future mining job system).
--   * Right-click / Escape cancels the pending anchor; switching
--     tools cancels too (hud.lua routes onToolMode here).
--
-- Handlers are named handle* (not on*) deliberately: this module is
-- forwarded to from init.lua's ordered chains; on*-named functions
-- would ALSO fire on every engine broadcast and double-handle clicks
-- (same convention as build_tool.lua).
local mineTool = {}

-- Mouse button codes are module-locals by convention (each script
-- defines its own — they are NOT globals; see build_tool.lua).
local MOUSE_LEFT  = 1
local MOUSE_RIGHT = 2

mineTool.hud    = nil   -- hud module reference (read worldId at call time)
mineTool.anchor = nil   -- {gx, gy} after the first click

function mineTool.setup(ctx)
    mineTool.hud = ctx.hud
end

local function worldId()
    return mineTool.hud and mineTool.hud.worldId
end

-- The tool only acts in the zoomed-in world view with the mine tool
-- selected; everywhere else the chains fall through to other handlers.
local function active()
    return worldId() ~= nil
       and mineTool.hud.currentView == "zoomed_in"
       and world.getToolMode and world.getToolMode() == "mine"
end

function mineTool.cancel()
    mineTool.anchor = nil
    local wid = worldId()
    if wid then
        world.clearMineAnchor(wid)
    end
end

function mineTool.handleMouseDown(button, x, y)
    if not active() then return false end

    if button == MOUSE_LEFT then
        -- Live pick at the click coords, not the 0.1s-cached hover, so a
        -- fast move-then-click anchors/commits on the clicked tile (#123).
        local gx, gy = world.pickTile(x, y)
        if not gx or not gy then
            -- Clicked off-world: swallow so the click doesn't fall
            -- through, but don't disturb a pending anchor.
            return true
        end
        gx, gy = math.floor(gx), math.floor(gy)
        if not mineTool.anchor then
            mineTool.anchor = { gx, gy }
            world.setMineAnchor(worldId(), gx, gy)
        else
            -- Commit; WorldDesignateMine also clears the world-side
            -- anchor, so Lua and render state can't desync.
            world.designateMine(worldId(),
                mineTool.anchor[1], mineTool.anchor[2], gx, gy)
            mineTool.anchor = nil
        end
        return true
    elseif button == MOUSE_RIGHT then
        if mineTool.anchor then
            mineTool.cancel()
            return true
        end
        return false
    end

    return false
end

function mineTool.handleKeyDown(key)
    if key == "Escape" and mineTool.anchor and active() then
        mineTool.cancel()
        return true
    end
    return false
end

-- Tool-mode change callback, wired from hud.lua's toolbar onChange
-- (direct call, not a broadcast — safe under the on* convention).
function mineTool.onToolMode(toolName)
    if toolName ~= "tool_mine" then
        mineTool.cancel()
    end
end

package.loaded["scripts.mine_tool"] = mineTool
return mineTool
