-- Construction designation tool (issue #95).
--
-- DF-style two-click rectangle, mirroring scripts/mine_tool.lua, but it
-- DESIGNATES rather than digs: each committed tile becomes a pending
-- construction job (engine wsConstructDesignationsRef) that the build AI
-- (#96) executes later. Nothing is built here — this is the designation
-- layer only.
--
-- Sub-selection (what to build) reuses the context-menu widget: selecting
-- the tool pops a menu of structure pieces (floor / wall edges / ceiling
-- / post) plus a Building submenu listing placeable building defs. The
-- chosen target is held in constructTool.target until changed.
--
-- Singleton via package.loaded so init.lua's input hooks see the same
-- state the engine-ticked update() uses (same pattern as mine_tool).

local constructTool = package.loaded["scripts.construct_tool"] or {}
package.loaded["scripts.construct_tool"] = constructTool

local cm = require("scripts.ui.context_menu")

constructTool.hud    = nil   -- hud module reference (read worldId at call time)
constructTool.anchor = nil   -- {gx, gy} after the first click
-- Current build target. Defaults to a floor so the tool is immediately
-- usable even before the player opens the picker. A "structure" target is
-- {kind="structure", pack=..., piece=..., edge=...}; a "building" target
-- is {kind="building", def=...}.
constructTool.target = constructTool.target
    or { kind = "structure", pack = "dungeon_1", piece = "floor", edge = nil }

-- The structure pack the designation picker offers. Matches the pack
-- scripts/structures.lua builds with; a multi-pack picker is a follow-up.
local PACK = "dungeon_1"

local MOUSE_LEFT  = 1
local MOUSE_RIGHT = 2

function constructTool.setup(ctx)
    constructTool.hud = ctx.hud
end

local function worldId()
    return constructTool.hud and constructTool.hud.worldId
end

local function active()
    return worldId() ~= nil
       and constructTool.hud.currentView == "zoomed_in"
       and world.getToolMode and world.getToolMode() == "construct"
end

-----------------------------------------------------------
-- Sub-selection (what to build)
-----------------------------------------------------------

-- Human-readable label for the current target (for logs / HUD hints).
function constructTool.targetLabel()
    local t = constructTool.target
    if not t then return "?" end
    if t.kind == "building" then
        return "Building: " .. tostring(t.def)
    end
    if t.piece == "wall" then
        return "Wall " .. string.upper(t.edge or "?")
    end
    return (t.piece:gsub("^%l", string.upper))
end

local function setStructure(piece, edge)
    constructTool.target =
        { kind = "structure", pack = PACK, piece = piece, edge = edge }
    engine.logDebug("construct: target = " .. constructTool.targetLabel())
end

local function setBuilding(def)
    -- A building has no rectangle, so clear any armed structure anchor the
    -- moment the player picks one — otherwise the stale anchor->hover
    -- preview lingers until the next world click, and the picker is
    -- reopenable mid-rectangle (re-clicking the construct toolbar slot).
    if constructTool.anchor then constructTool.cancel() end
    constructTool.target = { kind = "building", def = def }
    engine.logDebug("construct: target = " .. constructTool.targetLabel())
end

-- Build the context-menu item list. One level of submenu is supported,
-- so structure pieces sit at the top level and buildings live under a
-- "Building" submenu.
local function pickerItems()
    local items = {
        { label = "Floor",   callback = function() setStructure("floor") end },
        { label = "Wall NE", callback = function() setStructure("wall", "ne") end },
        { label = "Wall NW", callback = function() setStructure("wall", "nw") end },
        { label = "Wall SE", callback = function() setStructure("wall", "se") end },
        { label = "Wall SW", callback = function() setStructure("wall", "sw") end },
        { label = "Ceiling", callback = function() setStructure("ceiling") end },
        { label = "Post",    callback = function() setStructure("post") end },
    }
    -- Use the build tool's visibility filter so the designation picker
    -- offers exactly the buildings the player could place right now
    -- (starting buildings until placed; the rest only once a starting
    -- building exists). Otherwise fresh worlds could queue invalid jobs
    -- (e.g. cargo before a portal) that would persist in saves (#95).
    local bt = package.loaded["scripts.build_tool"]
    local defs = (bt and bt.visibleDefs and bt.visibleDefs())
        or building.listDefs() or {}
    if #defs > 0 then
        local sub = {}
        for _, d in ipairs(defs) do
            local name = d.name
            table.insert(sub, {
                label = d.displayName or name,
                callback = function() setBuilding(name) end,
            })
        end
        table.insert(items, { label = "Building", submenu = sub })
    end
    return items
end

-- Pop the sub-selection menu. Anchored at the tool toolbar (bottom-left)
-- so it sits next to the toolbar like the build picker.
function constructTool.openPicker()
    if not worldId() then return end
    local x = 80
    local y = (constructTool.hud and constructTool.hud.fbH or 720) - 320
    cm.show(pickerItems(), x, y)
end

-----------------------------------------------------------
-- Designation interaction (two-click rectangle)
-----------------------------------------------------------

function constructTool.cancel()
    constructTool.anchor = nil
    local wid = worldId()
    if wid then
        construction.clearAnchor(wid)
    end
end

local function commit(gx, gy)
    local wid = worldId()
    local a = constructTool.anchor
    local t = constructTool.target
    if not (wid and a and t) then return end
    if t.kind == "building" then
        construction.designate(wid, a[1], a[2], gx, gy, "building", t.def)
    else
        construction.designate(wid, a[1], a[2], gx, gy,
            "structure", t.pack, t.piece, t.edge)
    end
    constructTool.anchor = nil
end

function constructTool.handleMouseDown(button, x, y)
    if not active() then return false end
    -- A click while the picker is open feeds the menu, not the world.
    if cm.isOpen and cm.isOpen() then return false end
    if button == MOUSE_LEFT then
        local gx, gy = world.pickTile(x, y)
        if not gx or not gy then
            return true
        end
        gx, gy = math.floor(gx), math.floor(gy)
        local t = constructTool.target
        if t and t.kind == "building" then
            -- A building is one footprint, not a rectangle of buildings,
            -- so it designates on a single click. No anchor is set, so no
            -- (misleading) anchor->hover rectangle preview appears, and
            -- the single committed tile matches what the player saw. Drop
            -- any structure anchor left pending from a target switch.
            if constructTool.anchor then constructTool.cancel() end
            construction.designate(worldId(), gx, gy, gx, gy, "building", t.def)
        elseif not constructTool.anchor then
            constructTool.anchor = { gx, gy }
            construction.setAnchor(worldId(), gx, gy)
        else
            commit(gx, gy)
        end
        return true
    elseif button == MOUSE_RIGHT then
        if constructTool.anchor then
            -- First right-click cancels a pending rectangle.
            constructTool.cancel()
            return true
        else
            -- Otherwise remove the designation under the cursor (cancel
            -- mode): right-click erases a single blueprint tile.
            local gx, gy = world.pickTile(x, y)
            if gx and gy then
                construction.cancelDesignation(math.floor(gx), math.floor(gy))
            end
            return true
        end
    end
    return false
end

function constructTool.handleKeyDown(key)
    if not active() then return false end
    if key == "Escape" and constructTool.anchor then
        constructTool.cancel()
        return true
    end
    return false
end

-- Tool-mode router (from hud.lua onChange). Opening the construction tool
-- pops the sub-selection menu; leaving it cancels any pending anchor.
function constructTool.onToolMode(toolName)
    if toolName == "tool_construct" then
        constructTool.openPicker()
    else
        constructTool.cancel()
    end
end

return constructTool
