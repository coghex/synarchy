-- Till designation tool (#333, DF-style two-click rectangle).
--
--   * Toolbar "tool_till" puts the world in till tool mode; the world
--     render pass shows the hover cursor.
--   * First left-click anchors the rectangle (till.setAnchor); the
--     render pass previews anchor→hover as a grid of cursors, per-z-
--     level like the mine/build tools (a farmed field is flat ground).
--   * Second left-click commits (till.designate) — the world thread
--     keeps only tillable tiles (no fluid, no flora, not already
--     tilled) at the anchor's surface z, and renders a marker over each
--     until the till AI (scripts/unit_ai.lua) tills them.
--   * Right-click / Escape cancels the pending anchor; switching tools
--     cancels too (hud.lua routes onToolMode here).
--
-- Handlers are named handle* (not on*) deliberately: this module is
-- forwarded to from init.lua's ordered chains; on*-named functions
-- would ALSO fire on every engine broadcast and double-handle clicks
-- (same convention as chop_tool.lua / mine_tool.lua / build_tool.lua).
local tillTool = {}

-- Mouse button codes are module-locals by convention (each script
-- defines its own — they are NOT globals; see build_tool.lua).
local MOUSE_LEFT  = 1
local MOUSE_RIGHT = 2

tillTool.hud    = nil   -- hud module reference (read worldId at call time)
tillTool.anchor = nil   -- {gx, gy} after the first click

function tillTool.setup(ctx)
    tillTool.hud = ctx.hud
end

local function worldId()
    return tillTool.hud and tillTool.hud.worldId
end

-- The tool only acts in the zoomed-in world view with the till tool
-- selected; everywhere else the chains fall through to other handlers.
local function active()
    return worldId() ~= nil
       and tillTool.hud.currentView == "zoomed_in"
       and world.getToolMode and world.getToolMode() == "till"
end

function tillTool.cancel()
    tillTool.anchor = nil
    local wid = worldId()
    if wid then
        till.clearAnchor(wid)
    end
end

function tillTool.handleMouseDown(button, x, y)
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
        if not tillTool.anchor then
            tillTool.anchor = { gx, gy }
            till.setAnchor(worldId(), gx, gy)
        else
            -- Commit; WorldDesignateTill also clears the world-side
            -- anchor, so Lua and render state can't desync.
            till.designate(worldId(),
                tillTool.anchor[1], tillTool.anchor[2], gx, gy)
            tillTool.anchor = nil
        end
        return true
    elseif button == MOUSE_RIGHT then
        if tillTool.anchor then
            tillTool.cancel()
            return true
        end
        return false
    end

    return false
end

function tillTool.handleKeyDown(key)
    if key == "Escape" and tillTool.anchor and active() then
        tillTool.cancel()
        return true
    end
    return false
end

-- Tool-mode change callback, wired from hud.lua's toolbar onChange
-- (direct call, not a broadcast — safe under the on* convention).
function tillTool.onToolMode(toolName)
    if toolName ~= "tool_till" then
        tillTool.cancel()
    end
end

package.loaded["scripts.till_tool"] = tillTool
return tillTool
