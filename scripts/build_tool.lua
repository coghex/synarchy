-- Build Tool
--
-- Owns the popup picker that lists available starting buildings and
-- the "placement mode" state machine that draws a ghost preview at
-- the hovered tile until the player commits or cancels.
--
-- Lifecycle:
--   * Selecting tool_build in the HUD toolbar → showPicker()
--   * Picking an entry in the picker → enterPlacement(defName)
--   * In placement mode, each tick we snap world.getHoverTile() to
--     int coords, ask building.canPlaceAt, and call building.setGhost.
--   * Left-click on a valid tile → building.spawn + exitPlacement.
--   * Right-click / Esc → exitPlacement (no spawn).
--
-- Singleton via package.loaded so init.lua's input hooks see the same
-- state that the engine-ticked update() uses. Same pattern as
-- scripts/unit_ai.lua and scripts/debug.lua.

local buildTool = package.loaded["scripts.build_tool"] or {}
package.loaded["scripts.build_tool"] = buildTool

local panel = require("scripts.ui.panel")
local list  = require("scripts.ui.list")
local label = require("scripts.ui.label")
local scale = require("scripts.ui.scale")

-- Module state. Hung off the module table so it survives reloads.
--
-- Save/load contract: this state is intentionally NOT registered with
-- saveModules. It represents an in-progress UI action ("I am picking /
-- positioning a building right now"), not a durable player decision.
-- Within-session loads keep the state via the package.loaded singleton;
-- update() re-establishes the engine-side ghost on the next tick.
-- Fresh-session loads start at the default {mode="off"} — the player
-- re-enters the build tool via the toolbar. The Haskell-side
-- `buildingGhostRef` mirrors this: not in SaveData, defaults to Nothing
-- in a fresh engine. See [[project-bugs-open]] item 6 for context.
buildTool.state = buildTool.state or {
    mode          = "off",   -- "off" / "picker" / "placement"
    selectedDef   = nil,     -- when in placement
    panelId       = nil,     -- popup panel id
    listId        = nil,     -- list element id
    lastHoverTile = nil,     -- {gx, gy}; last hover seen by update().
                             -- Kept up-to-date but NOT used as a dedup
                             -- guard — update() calls setGhost every
                             -- tick while in placement mode.
}

-- Render-config passed in from hud.lua at boot.
buildTool.hud = nil

local MOUSE_LEFT  = 1
local MOUSE_RIGHT = 2

-----------------------------------------------------------
-- HUD hookup. Called once from hud.lua after the toolbar is built.
-----------------------------------------------------------
function buildTool.setup(opts)
    buildTool.hud = opts
end

-----------------------------------------------------------
-- Picker popup
-----------------------------------------------------------
local function destroyPicker()
    local s = buildTool.state
    if s.listId  then list.destroy(s.listId);  s.listId  = nil end
    if s.panelId then panel.destroy(s.panelId); s.panelId = nil end
end

function buildTool.showPicker()
    if buildTool.state.mode == "picker" then return end
    destroyPicker()

    local h = buildTool.hud
    if not h or not h.page then
        engine.logWarn("BuildTool: showPicker called before setup()")
        return
    end

    local items = building.getStartingBuildings() or {}
    if #items == 0 then
        engine.logWarn("BuildTool: no starting buildings available")
        return
    end

    local uiscale     = scale.get()
    -- Width: building names can be long ("acolyte_portal" + future
    -- entries). 320 base gives comfortable horizontal padding so the
    -- text never gets clipped or feels cramped against the panel edge.
    local pickerW     = math.floor(320 * uiscale)
    -- Row height: 40 (base) gives the centered text breathing room
    -- above and below. The list module scales by uiscale internally,
    -- so we pass the BASE value to it and use the scaled value below
    -- only for the outer panel sizing.
    local rowHBase    = 40
    local rowH        = math.floor(rowHBase * uiscale)
    -- Inner padding around the list — top is heavier than bottom to
    -- mirror the bottom-left tool palette button's visual mass.
    local padTopBase    = 20
    local padBottomBase = 10
    local pickerH     = math.floor((#items * rowH)
                          + (padTopBase + padBottomBase) * uiscale)

    -- Anchor: bottom-left toolbar lives at
    --     toolAnchorY = fbH - margin - btnSize    (top of the BOTTOM button)
    -- The build button sits above it; toggle stacks upward with a
    -- small gap, so the build button's top is roughly:
    --     fbH - margin - 2*btnSize - 8
    -- Aligning the picker's TOP with the build-button TOP feels right
    -- visually, with a small horizontal gap to the right of the button.
    local margin       = math.floor(16 * uiscale)
    local btnSize      = h.buttonSize and math.floor(h.buttonSize * uiscale) or 64
    local stackGap     = math.floor(8 * uiscale)
    local pickerX      = margin + btnSize + stackGap
    local buildBtnTopY = h.fbH - margin - 2 * btnSize - stackGap
    local pickerY      = buildBtnTopY

    buildTool.state.panelId = panel.new({
        name       = "build_tool_picker",
        page       = h.page,
        x          = pickerX,
        y          = pickerY,
        width      = pickerW,
        height     = pickerH,
        textureSet = h.boxTexSet,
        color      = {0.1, 0.1, 0.1, 0.9},
        tileSize   = 64,
        zIndex     = 120,
        padding    = { top = padTopBase, bottom = padBottomBase,
                       left = 10, right = 10 },
        uiscale    = uiscale,
    })

    local pbounds = panel.getContentBounds(buildTool.state.panelId)

    local listItems = {}
    for _, name in ipairs(items) do
        table.insert(listItems, { text = name, value = name })
    end

    -- Inset the list horizontally inside the panel's content area so
    -- the highlight rectangle doesn't bleed past the panel border. The
    -- panel itself has a textured/rounded edge; the highlight is a flat
    -- sprite, so without this it overhangs visibly on the sides.
    local hlInset = math.floor(10 * uiscale)

    buildTool.state.listId = list.new({
        name       = "build_tool_picker_list",
        page       = h.page,
        font       = h.menuFont,
        fontSize   = 16,
        items      = listItems,
        x          = pickerX + pbounds.x + hlInset,
        y          = pickerY + pbounds.y,
        width      = pbounds.width - 2 * hlInset,
        height     = pbounds.height,
        itemHeight = rowHBase,    -- list module scales this internally
        textAlign  = "center",
        zIndex     = 121,
        uiscale    = uiscale,
        onSelect   = function(_, value)
            buildTool.enterPlacement(value)
        end,
    })

    buildTool.state.mode = "picker"
end

function buildTool.hidePicker()
    destroyPicker()
    if buildTool.state.mode == "picker" then
        buildTool.state.mode = "off"
    end
end

-----------------------------------------------------------
-- Placement mode
-----------------------------------------------------------
function buildTool.enterPlacement(defName)
    destroyPicker()
    buildTool.state.mode          = "placement"
    buildTool.state.selectedDef   = defName
    buildTool.state.lastHoverTile = nil
end

function buildTool.exitPlacement()
    building.clearGhost()
    buildTool.state.mode          = "off"
    buildTool.state.selectedDef   = nil
    buildTool.state.lastHoverTile = nil
end

-----------------------------------------------------------
-- Per-tick: drive the ghost preview while in placement mode
-----------------------------------------------------------
function buildTool.update(dt)
    if buildTool.state.mode ~= "placement" then return end

    local defName = buildTool.state.selectedDef
    if not defName then return end

    local gx, gy = world.getHoverTile()
    if not gx or not gy then
        -- Mouse left the world view; hide the ghost rather than show
        -- it stuck on the last valid tile.
        building.clearGhost()
        return
    end

    -- Snap to integer tile coords (hover already returns tile coords
    -- but be defensive — future engine changes might switch to floats).
    local igx = math.floor(gx)
    local igy = math.floor(gy)

    local valid = building.canPlaceAt(defName, igx, igy)
    building.setGhost(defName, igx, igy, valid)
    buildTool.state.lastHoverTile = { igx, igy }
end

-----------------------------------------------------------
-- Mouse hooks. Called from init.lua's onMouseDown.
-- Return true if we consumed the click.
-----------------------------------------------------------
function buildTool.onMouseDown(button, x, y)
    if buildTool.state.mode ~= "placement" then return false end

    if button == MOUSE_LEFT then
        local tile = buildTool.state.lastHoverTile
        if not tile then return true end
        local valid = building.canPlaceAt(buildTool.state.selectedDef,
                                          tile[1], tile[2])
        if valid then
            local id = building.spawn(buildTool.state.selectedDef,
                                      tile[1], tile[2])
            if id then
                engine.logInfo("BuildTool: placed " ..
                    buildTool.state.selectedDef ..
                    " (id=" .. tostring(id) ..
                    ") at " .. tile[1] .. "," .. tile[2])
            end
            -- Starting buildings: one-shot. Exit to default tool.
            buildTool.exitPlacement()
            if buildTool.hud and buildTool.hud.selectDefaultTool then
                buildTool.hud.selectDefaultTool()
            end
        end
        return true
    elseif button == MOUSE_RIGHT then
        buildTool.exitPlacement()
        if buildTool.hud and buildTool.hud.selectDefaultTool then
            buildTool.hud.selectDefaultTool()
        end
        return true
    end

    return false
end

function buildTool.onKeyDown(key)
    if key == "Escape" and buildTool.state.mode ~= "off" then
        buildTool.hidePicker()
        buildTool.exitPlacement()
        if buildTool.hud and buildTool.hud.selectDefaultTool then
            buildTool.hud.selectDefaultTool()
        end
        return true
    end
    return false
end

-----------------------------------------------------------
-- Tool-mode change callback. Wired up by hud.lua via setup().
-----------------------------------------------------------
function buildTool.onToolMode(toolName)
    if toolName == "tool_build" then
        buildTool.showPicker()
    else
        buildTool.hidePicker()
        buildTool.exitPlacement()
    end
end

-----------------------------------------------------------
-- Engine script hooks (loaded via engine.loadScript)
-----------------------------------------------------------
function buildTool.init(scriptId)
    engine.logInfo("Build tool initializing...")
end

function buildTool.shutdown()
    destroyPicker()
    buildTool.exitPlacement()
    engine.logInfo("Build tool shut down")
end

return buildTool
