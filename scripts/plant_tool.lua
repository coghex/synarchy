-- Plant designation tool (#335, single-tile — no anchor).
--
--   * Toolbar "tool_plant" puts the world in plant tool mode; the world
--     render pass shows the hover cursor.
--   * Left-click a TILLED tile (world.isPlantable) opens the planting
--     screen (scripts/plant_panel.lua) for that tile — suitability-
--     sorted crop list with filter/sort/search. Clicking off a tilled
--     tile, or right-click / Escape, closes the panel if it's open.
--   * Choosing a crop in the panel commits plant.designate immediately
--     (mirrors save_browser's click-to-act list convention) and closes
--     the panel; the render pass then shows a marker over the tile
--     until the farm AI (#336) plants it.
--
-- No anchor state at all (unlike mine/chop/till): the planting screen
-- already scopes the player to one tile before a crop is chosen, so
-- there's no pending rectangle to preview or cancel.
--
-- Handlers are named handle* (not on*) deliberately: this module is
-- forwarded to from init.lua's ordered chains; on*-named functions
-- would ALSO fire on every engine broadcast and double-handle clicks
-- (same convention as chop_tool.lua / till_tool.lua / mine_tool.lua).
local plantTool = {}

local MOUSE_LEFT  = 1
local MOUSE_RIGHT = 2

plantTool.hud = nil   -- hud module reference (read worldId at call time)

function plantTool.setup(ctx)
    plantTool.hud = ctx.hud
end

local function worldId()
    return plantTool.hud and plantTool.hud.worldId
end

-- The tool only acts in the zoomed-in world view with the plant tool
-- selected; everywhere else the chains fall through to other handlers.
local function active()
    return worldId() ~= nil
       and plantTool.hud.currentView == "zoomed_in"
       and world.getToolMode and world.getToolMode() == "plant"
end

function plantTool.handleMouseDown(button, x, y)
    if not active() then return false end

    if button == MOUSE_LEFT then
        -- Live pick at the click coords, not the 0.1s-cached hover, so a
        -- fast move-then-click opens the screen for the clicked tile,
        -- the same #123 fix till_tool/chop_tool rely on.
        local gx, gy = world.pickTile(x, y)
        local plantPanel = require("scripts.plant_panel")
        if not gx or not gy then
            -- Clicked off-world: swallow so the click doesn't fall
            -- through, but don't disturb an already-open panel.
            return true
        end
        gx, gy = math.floor(gx), math.floor(gy)
        if world.isPlantable(gx, gy) then
            plantPanel.show(worldId(), gx, gy)
        else
            plantPanel.closeIfOpen()
        end
        return true
    elseif button == MOUSE_RIGHT then
        local plantPanel = require("scripts.plant_panel")
        if plantPanel.isOpen() then
            plantPanel.closeIfOpen()
            return true
        end
        return false
    end

    return false
end

function plantTool.handleKeyDown(key)
    if key == "Escape" and active() then
        local plantPanel = require("scripts.plant_panel")
        if plantPanel.isOpen() then
            plantPanel.closeIfOpen()
            return true
        end
    end
    return false
end

-- Tool-mode change callback, wired from hud.lua's toolbar onChange
-- (direct call, not a broadcast — safe under the on* convention).
function plantTool.onToolMode(toolName)
    if toolName ~= "tool_plant" then
        require("scripts.plant_panel").closeIfOpen()
    end
end

package.loaded["scripts.plant_tool"] = plantTool
return plantTool
