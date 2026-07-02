-- Chop designation tool (#97, DF-style two-click rectangle).
--
--   * Toolbar "tool_chop" puts the world in chop tool mode; the world
--     render pass shows the hover cursor.
--   * First left-click anchors the rectangle (chop.setAnchor); the
--     render pass previews anchor→hover as a grid of cursors.
--   * Second left-click commits (chop.designate) — the world thread
--     keeps only tiles holding a currently-harvestable wood-tagged
--     flora species (trees), and renders a marker over each until the
--     chop AI (scripts/unit_ai.lua) fells them.
--   * Right-click / Escape cancels the pending anchor; switching
--     tools cancels too (hud.lua routes onToolMode here).
--
-- Handlers are named handle* (not on*) deliberately: this module is
-- forwarded to from init.lua's ordered chains; on*-named functions
-- would ALSO fire on every engine broadcast and double-handle clicks
-- (same convention as mine_tool.lua / build_tool.lua).
local chopTool = {}

-- Mouse button codes are module-locals by convention (each script
-- defines its own — they are NOT globals; see build_tool.lua).
local MOUSE_LEFT  = 1
local MOUSE_RIGHT = 2

chopTool.hud    = nil   -- hud module reference (read worldId at call time)
chopTool.anchor = nil   -- {gx, gy} after the first click

function chopTool.setup(ctx)
    chopTool.hud = ctx.hud
end

local function worldId()
    return chopTool.hud and chopTool.hud.worldId
end

-- The tool only acts in the zoomed-in world view with the chop tool
-- selected; everywhere else the chains fall through to other handlers.
local function active()
    return worldId() ~= nil
       and chopTool.hud.currentView == "zoomed_in"
       and world.getToolMode and world.getToolMode() == "chop"
end

function chopTool.cancel()
    chopTool.anchor = nil
    local wid = worldId()
    if wid then
        chop.clearAnchor(wid)
    end
end

function chopTool.handleMouseDown(button, x, y)
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
        if not chopTool.anchor then
            chopTool.anchor = { gx, gy }
            chop.setAnchor(worldId(), gx, gy)
        else
            -- Commit; WorldDesignateChop also clears the world-side
            -- anchor, so Lua and render state can't desync.
            chop.designate(worldId(),
                chopTool.anchor[1], chopTool.anchor[2], gx, gy)
            chopTool.anchor = nil
        end
        return true
    elseif button == MOUSE_RIGHT then
        if chopTool.anchor then
            chopTool.cancel()
            return true
        end
        return false
    end

    return false
end

function chopTool.handleKeyDown(key)
    if key == "Escape" and chopTool.anchor and active() then
        chopTool.cancel()
        return true
    end
    return false
end

-- Tool-mode change callback, wired from hud.lua's toolbar onChange
-- (direct call, not a broadcast — safe under the on* convention).
function chopTool.onToolMode(toolName)
    if toolName ~= "tool_chop" then
        chopTool.cancel()
    end
end

package.loaded["scripts.chop_tool"] = chopTool
return chopTool
