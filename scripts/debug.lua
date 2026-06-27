-- Debug overlay module
--
-- Toggled with F8. Shows green diagnostic text in the top-left.
--
-- Spawn UI (under the FPS line):
--   * "spawn" label is clickable. Click toggles a vertical list of
--     available unit definition names.
--   * Click a name to arm spawn mode with that def. The armed entry
--     gains a "> " prefix and a yellow highlight. List stays open.
--   * While armed: left-clicks on the map call `unit.spawn` at the
--     hover tile and stay armed (multi-spawn).
--   * Exits: right-click, ESC, click the spawn label, F8.
--
-- Click handling is intentionally parallel to the UI manager — debug
-- text labels are plain text (no UI clickability) and `tryClaimClick`
-- does its own AABB hit-test against rectangles we track ourselves.
-- That way the debug layer keeps working even if the UI system breaks.
-- `game.onMouseDown` and `uiManager.onMouseDown` consult
-- `tryClaimClick` first; if it returns true, they skip the rest of
-- their click handling for that event.

local scale = require("scripts.ui.scale")
local label = require("scripts.ui.label")

-- This module is loaded twice if we're not careful: once by
-- `engine.loadScript` (which uses dofile and does NOT touch
-- package.loaded) and once by `require("scripts.debug")` (which
-- caches via package.loaded). Without the self-registration below,
-- those two paths produce *different* tables, so callers using
-- require see an uninitialized copy (visible=false, no rects). Reuse
-- whatever's already cached, otherwise register ourselves so future
-- requires share state.
local debugOverlay = package.loaded["scripts.debug"] or {}
package.loaded["scripts.debug"] = debugOverlay

debugOverlay.page = nil
debugOverlay.fpsLabelId = nil
debugOverlay.spawnButtonId = nil
debugOverlay.spawnEntries = {}        -- array of { id, defName }
debugOverlay.spawnListVisible = false
debugOverlay.armedDef = nil
-- Fluid spawn mode. Same UX as unit spawn but armed value is a string
-- ("water" / "lava") passed to world.setFluidTile.
debugOverlay.fluidButtonId = nil
debugOverlay.fluidEntries = {}
debugOverlay.fluidListVisible = false
debugOverlay.armedFluidType = nil
debugOverlay.fluidKinds = { "water", "lava" }
-- Item spawn mode. Same UX as unit spawn; armed value is an item def
-- name passed to item.spawnGround at the hover tile.
debugOverlay.itemButtonId = nil
debugOverlay.itemEntries = {}
debugOverlay.itemListVisible = false
debugOverlay.armedItemDef = nil
-- Terrain placement mode. Armed value is a material id passed to
-- world.addTile at the hover tile (raises the column one z; lands in
-- the edit log so it persists like any player edit).
debugOverlay.terrainButtonId = nil
debugOverlay.terrainEntries = {}
debugOverlay.terrainListVisible = false
debugOverlay.armedTerrainId = nil
-- Location placement mode. Armed value is a location def name passed to
-- locations.stamp at the hover tile (stamps a premade room/structure
-- via world.setCell + content spawns; terrain lands in the edit log).
debugOverlay.locationButtonId = nil
debugOverlay.locationEntries = {}
debugOverlay.locationListVisible = false
debugOverlay.armedLocation = nil
-- Structure placement mode. Armed value is a kind name ("wall"/"floor"/
-- "ceiling"/"post") placed via scripts.structures at the hover tile; walls
-- go in the clicked quarter of the tile.
debugOverlay.structureButtonId = nil
debugOverlay.structureEntries = {}
debugOverlay.structureListVisible = false
debugOverlay.armedStructure = nil
debugOverlay.debugFont = nil
debugOverlay.visible = false
debugOverlay.uiCreated = false

-- Parallel hit-test state (independent of the UI manager).
debugOverlay.clickableRects = {}      -- array of { x, y, w, h, action }
debugOverlay.lastClaim = nil          -- {button, x, y} for idempotency

local MOUSE_LEFT = 1

debugOverlay.baseSizes = {
    fontSize    = 32,
    margin      = 10,
    rowSpacing  = 6,
}

local COLOR_DIM    = {0.0, 1.0, 0.0, 1.0}   -- standard green (FPS, list, button)
local COLOR_BRIGHT = {1.0, 1.0, 0.4, 1.0}   -- armed-entry highlight

function debugOverlay.init(scriptId)
    engine.logInfo("Debug overlay initializing...")
    debugOverlay.debugFont = engine.loadFont(
        "assets/fonts/shell.ttf", debugOverlay.baseSizes.fontSize)
    engine.logDebug("Debug overlay initialized")
end

local function destroySpawnList()
    for _, entry in ipairs(debugOverlay.spawnEntries) do
        if entry.id then label.destroy(entry.id) end
    end
    debugOverlay.spawnEntries = {}
end

local function destroyFluidList()
    for _, entry in ipairs(debugOverlay.fluidEntries) do
        if entry.id then label.destroy(entry.id) end
    end
    debugOverlay.fluidEntries = {}
end

local function destroyItemList()
    for _, entry in ipairs(debugOverlay.itemEntries) do
        if entry.id then label.destroy(entry.id) end
    end
    debugOverlay.itemEntries = {}
end

local function destroyTerrainList()
    for _, entry in ipairs(debugOverlay.terrainEntries) do
        if entry.id then label.destroy(entry.id) end
    end
    debugOverlay.terrainEntries = {}
end

local function destroyLocationList()
    for _, entry in ipairs(debugOverlay.locationEntries) do
        if entry.id then label.destroy(entry.id) end
    end
    debugOverlay.locationEntries = {}
end

local function destroyStructureList()
    for _, entry in ipairs(debugOverlay.structureEntries) do
        if entry.id then label.destroy(entry.id) end
    end
    debugOverlay.structureEntries = {}
end

local function formatEntry(defName, isArmed)
    if isArmed then return "> " .. defName
    else            return "  " .. defName end
end

local function refreshEntries()
    for _, entry in ipairs(debugOverlay.spawnEntries) do
        local isArmed = (entry.defName == debugOverlay.armedDef)
        label.setText(entry.id, formatEntry(entry.defName, isArmed))
        label.setColor(entry.id, isArmed and COLOR_BRIGHT or COLOR_DIM)
    end
    for _, entry in ipairs(debugOverlay.fluidEntries) do
        local isArmed = (entry.kind == debugOverlay.armedFluidType)
        label.setText(entry.id, formatEntry(entry.kind, isArmed))
        label.setColor(entry.id, isArmed and COLOR_BRIGHT or COLOR_DIM)
    end
    for _, entry in ipairs(debugOverlay.itemEntries) do
        local isArmed = (entry.defName == debugOverlay.armedItemDef)
        label.setText(entry.id, formatEntry(entry.defName, isArmed))
        label.setColor(entry.id, isArmed and COLOR_BRIGHT or COLOR_DIM)
    end
    for _, entry in ipairs(debugOverlay.terrainEntries) do
        local isArmed = (entry.matId == debugOverlay.armedTerrainId)
        label.setText(entry.id, formatEntry(entry.matName, isArmed))
        label.setColor(entry.id, isArmed and COLOR_BRIGHT or COLOR_DIM)
    end
    for _, entry in ipairs(debugOverlay.locationEntries) do
        if entry.name then
            local isArmed = (entry.name == debugOverlay.armedLocation)
            label.setText(entry.id, formatEntry(entry.name, isArmed))
            label.setColor(entry.id, isArmed and COLOR_BRIGHT or COLOR_DIM)
        end
    end
    for _, entry in ipairs(debugOverlay.structureEntries) do
        if entry.name then
            local isArmed = (entry.name == debugOverlay.armedStructure)
            label.setText(entry.id, formatEntry(entry.name, isArmed))
            label.setColor(entry.id, isArmed and COLOR_BRIGHT or COLOR_DIM)
        end
    end
end

-- Compute a generous rectangle covering a row of debug text at the
-- given baseline. Text renders above the baseline so we anchor the
-- rect's bottom near the baseline + a small descender allowance.
local function rowRect(s, baselineY, text)
    local w = engine.getTextWidth(debugOverlay.debugFont, text, s.fontSize)
    return {
        x = s.margin,
        y = baselineY - s.fontSize,
        w = w + s.fontSize,            -- a bit of slack on the right
        h = s.fontSize + s.rowSpacing, -- full row height
    }
end

-- Returns the Y baseline of the fluid button, which sits below the
-- spawn button + (if open) the spawn list. Lets rebuild and createUI
-- agree on layout without duplicating arithmetic.
local function fluidButtonY(s)
    local y = s.margin + 2 * (s.fontSize + s.rowSpacing)  -- spawn baseline
    -- One row below spawn baseline. When the spawn list is open, push
    -- below that too so the lists don't overlap.
    y = y + (s.fontSize + s.rowSpacing)
    if debugOverlay.spawnListVisible then
        local listRows = math.max(1, #debugOverlay.spawnEntries)
        y = y + listRows * (s.fontSize + s.rowSpacing)
    end
    return y
end

-- Item button sits below the fluid button (and its open list).
local function itemButtonY(s)
    local y = fluidButtonY(s) + (s.fontSize + s.rowSpacing)
    if debugOverlay.fluidListVisible then
        local listRows = math.max(1, #debugOverlay.fluidEntries)
        y = y + listRows * (s.fontSize + s.rowSpacing)
    end
    return y
end

-- Terrain button sits below the items button (and its open list).
local function terrainButtonY(s)
    local y = itemButtonY(s) + (s.fontSize + s.rowSpacing)
    if debugOverlay.itemListVisible then
        local listRows = math.max(1, #debugOverlay.itemEntries)
        y = y + listRows * (s.fontSize + s.rowSpacing)
    end
    return y
end

-- Locations button sits below the terrain button (and its open list).
local function locationButtonY(s)
    local y = terrainButtonY(s) + (s.fontSize + s.rowSpacing)
    if debugOverlay.terrainListVisible then
        local listRows = math.max(1, #debugOverlay.terrainEntries)
        y = y + listRows * (s.fontSize + s.rowSpacing)
    end
    return y
end

-- Structures button sits below the locations button (and its open list).
local function structureButtonY(s)
    local y = locationButtonY(s) + (s.fontSize + s.rowSpacing)
    if debugOverlay.locationListVisible then
        local listRows = math.max(1, #debugOverlay.locationEntries)
        y = y + listRows * (s.fontSize + s.rowSpacing)
    end
    return y
end

-- Rebuild clickable rects from current label state. Called whenever
-- the layout changes (createUI, list open/close, arm change).
local function rebuildClickableRects()
    debugOverlay.clickableRects = {}
    if not debugOverlay.uiCreated then return end

    local s = scale.applyAll(debugOverlay.baseSizes)
    local spawnY = s.margin + 2 * (s.fontSize + s.rowSpacing)

    -- Spawn button rect
    local r = rowRect(s, spawnY, "spawn")
    r.action = function()
        if debugOverlay.spawnListVisible or debugOverlay.armedDef then
            debugOverlay.clearArmed()
            debugOverlay.closeSpawnList()
        else
            debugOverlay.openSpawnList()
        end
    end
    table.insert(debugOverlay.clickableRects, r)

    -- Spawn list entry rects (only if list is open)
    if debugOverlay.spawnListVisible then
        local baseY = s.margin + 2 * (s.fontSize + s.rowSpacing) + s.fontSize
        for i, entry in ipairs(debugOverlay.spawnEntries) do
            if entry.defName then  -- skip empty placeholder
                local rowY = baseY + (i - 1) * (s.fontSize + s.rowSpacing)
                local rect = rowRect(s, rowY,
                                     formatEntry(entry.defName, false))
                local def = entry.defName
                rect.action = function()
                    debugOverlay.setArmed(def)
                end
                table.insert(debugOverlay.clickableRects, rect)
            end
        end
    end

    -- Fluid button rect
    local fbY = fluidButtonY(s)
    local fr = rowRect(s, fbY, "fluid")
    fr.action = function()
        if debugOverlay.fluidListVisible or debugOverlay.armedFluidType then
            debugOverlay.clearArmedFluid()
            debugOverlay.closeFluidList()
        else
            debugOverlay.openFluidList()
        end
    end
    table.insert(debugOverlay.clickableRects, fr)

    -- Fluid list entry rects (only if list is open)
    if debugOverlay.fluidListVisible then
        local baseY = fbY + s.fontSize
        for i, entry in ipairs(debugOverlay.fluidEntries) do
            local rowY = baseY + (i - 1) * (s.fontSize + s.rowSpacing)
            local rect = rowRect(s, rowY, formatEntry(entry.kind, false))
            local kind = entry.kind
            rect.action = function()
                debugOverlay.setArmedFluid(kind)
            end
            table.insert(debugOverlay.clickableRects, rect)
        end
    end

    -- Item button rect
    local ibY = itemButtonY(s)
    local ir = rowRect(s, ibY, "items")
    ir.action = function()
        if debugOverlay.itemListVisible or debugOverlay.armedItemDef then
            debugOverlay.clearArmedItem()
            debugOverlay.closeItemList()
        else
            debugOverlay.openItemList()
        end
    end
    table.insert(debugOverlay.clickableRects, ir)

    -- Item list entry rects (only if list is open)
    if debugOverlay.itemListVisible then
        local baseY = ibY + s.fontSize
        for i, entry in ipairs(debugOverlay.itemEntries) do
            if entry.defName then
                local rowY = baseY + (i - 1) * (s.fontSize + s.rowSpacing)
                local rect = rowRect(s, rowY,
                                     formatEntry(entry.defName, false))
                local def = entry.defName
                rect.action = function()
                    debugOverlay.setArmedItem(def)
                end
                table.insert(debugOverlay.clickableRects, rect)
            end
        end
    end

    -- Terrain button rect
    local tbY = terrainButtonY(s)
    local tr = rowRect(s, tbY, "terrain")
    tr.action = function()
        if debugOverlay.terrainListVisible or debugOverlay.armedTerrainId then
            debugOverlay.clearArmedTerrain()
            debugOverlay.closeTerrainList()
        else
            debugOverlay.openTerrainList()
        end
    end
    table.insert(debugOverlay.clickableRects, tr)

    -- Terrain list entry rects (only if list is open)
    if debugOverlay.terrainListVisible then
        local baseY = tbY + s.fontSize
        for i, entry in ipairs(debugOverlay.terrainEntries) do
            if entry.matId then
                local rowY = baseY + (i - 1) * (s.fontSize + s.rowSpacing)
                local rect = rowRect(s, rowY,
                                     formatEntry(entry.matName, false))
                local mid = entry.matId
                rect.action = function()
                    debugOverlay.setArmedTerrain(mid)
                end
                table.insert(debugOverlay.clickableRects, rect)
            end
        end
    end

    -- Locations button rect
    local lbY = locationButtonY(s)
    local lr = rowRect(s, lbY, "locations")
    lr.action = function()
        if debugOverlay.locationListVisible or debugOverlay.armedLocation then
            debugOverlay.clearArmedLocation()
            debugOverlay.closeLocationList()
        else
            debugOverlay.openLocationList()
        end
    end
    table.insert(debugOverlay.clickableRects, lr)

    -- Location list entry rects (only if list is open)
    if debugOverlay.locationListVisible then
        local baseY = lbY + s.fontSize
        for i, entry in ipairs(debugOverlay.locationEntries) do
            if entry.name then
                local rowY = baseY + (i - 1) * (s.fontSize + s.rowSpacing)
                local rect = rowRect(s, rowY,
                                     formatEntry(entry.name, false))
                local nm = entry.name
                rect.action = function()
                    debugOverlay.setArmedLocation(nm)
                end
                table.insert(debugOverlay.clickableRects, rect)
            end
        end
    end

    -- Structures button rect
    local sbY = structureButtonY(s)
    local sr = rowRect(s, sbY, "structures")
    sr.action = function()
        if debugOverlay.structureListVisible or debugOverlay.armedStructure then
            debugOverlay.clearArmedStructure()
            debugOverlay.closeStructureList()
        else
            debugOverlay.openStructureList()
        end
    end
    table.insert(debugOverlay.clickableRects, sr)

    -- Structure list entry rects (only if list is open)
    if debugOverlay.structureListVisible then
        local baseY = sbY + s.fontSize
        for i, entry in ipairs(debugOverlay.structureEntries) do
            if entry.name then
                local rowY = baseY + (i - 1) * (s.fontSize + s.rowSpacing)
                local rect = rowRect(s, rowY,
                                     formatEntry(entry.name, false))
                local nm = entry.name
                rect.action = function()
                    debugOverlay.setArmedStructure(nm)
                end
                table.insert(debugOverlay.clickableRects, rect)
            end
        end
    end
end

local function buildItemList()
    destroyItemList()
    local s = scale.applyAll(debugOverlay.baseSizes)
    local uiscale = scale.get()
    local defs = item.listDefs() or {}
    local baseY = itemButtonY(s) + s.fontSize

    if #defs == 0 then
        local lblId = label.new({
            name     = "item_list_empty",
            text     = "  (no items defined)",
            font     = debugOverlay.debugFont,
            fontSize = debugOverlay.baseSizes.fontSize,
            color    = COLOR_DIM,
            page     = debugOverlay.page,
            uiscale  = uiscale,
            x        = s.margin,
            y        = baseY,
            zIndex   = 1000,
        })
        table.insert(debugOverlay.itemEntries,
            { id = lblId, defName = nil })
        return
    end

    for i, def in ipairs(defs) do
        local isArmed = (def.name == debugOverlay.armedItemDef)
        local lblId = label.new({
            name     = "item_entry_" .. def.name,
            text     = formatEntry(def.name, isArmed),
            font     = debugOverlay.debugFont,
            fontSize = debugOverlay.baseSizes.fontSize,
            color    = isArmed and COLOR_BRIGHT or COLOR_DIM,
            page     = debugOverlay.page,
            uiscale  = uiscale,
            x        = s.margin,
            y        = baseY + (i - 1) * (s.fontSize + s.rowSpacing),
            zIndex   = 1000,
        })
        table.insert(debugOverlay.itemEntries,
            { id = lblId, defName = def.name })
    end
end

local function buildTerrainList()
    destroyTerrainList()
    local s = scale.applyAll(debugOverlay.baseSizes)
    local uiscale = scale.get()
    local mats = world.listMaterials() or {}
    local baseY = terrainButtonY(s) + s.fontSize

    if #mats == 0 then
        local lblId = label.new({
            name     = "terrain_list_empty",
            text     = "  (no materials loaded)",
            font     = debugOverlay.debugFont,
            fontSize = debugOverlay.baseSizes.fontSize,
            color    = COLOR_DIM,
            page     = debugOverlay.page,
            uiscale  = uiscale,
            x        = s.margin,
            y        = baseY,
            zIndex   = 1000,
        })
        table.insert(debugOverlay.terrainEntries,
            { id = lblId, matId = nil, matName = nil })
        return
    end

    for i, m in ipairs(mats) do
        local isArmed = (m.id == debugOverlay.armedTerrainId)
        local lblId = label.new({
            name     = "terrain_entry_" .. m.name,
            text     = formatEntry(m.name, isArmed),
            font     = debugOverlay.debugFont,
            fontSize = debugOverlay.baseSizes.fontSize,
            color    = isArmed and COLOR_BRIGHT or COLOR_DIM,
            page     = debugOverlay.page,
            uiscale  = uiscale,
            x        = s.margin,
            y        = baseY + (i - 1) * (s.fontSize + s.rowSpacing),
            zIndex   = 1000,
        })
        table.insert(debugOverlay.terrainEntries,
            { id = lblId, matId = m.id, matName = m.name })
    end
end

local function buildLocationList()
    destroyLocationList()
    local s = scale.applyAll(debugOverlay.baseSizes)
    local uiscale = scale.get()
    local locations = require("scripts.locations")
    local defs = locations.list() or {}
    local baseY = locationButtonY(s) + s.fontSize

    if #defs == 0 then
        local lblId = label.new({
            name     = "location_list_empty",
            text     = "  (no locations defined)",
            font     = debugOverlay.debugFont,
            fontSize = debugOverlay.baseSizes.fontSize,
            color    = COLOR_DIM,
            page     = debugOverlay.page,
            uiscale  = uiscale,
            x        = s.margin,
            y        = baseY,
            zIndex   = 1000,
        })
        table.insert(debugOverlay.locationEntries,
            { id = lblId, name = nil })
        return
    end

    for i, d in ipairs(defs) do
        local isArmed = (d.name == debugOverlay.armedLocation)
        local lblId = label.new({
            name     = "location_entry_" .. d.name,
            text     = formatEntry(d.name, isArmed),
            font     = debugOverlay.debugFont,
            fontSize = debugOverlay.baseSizes.fontSize,
            color    = isArmed and COLOR_BRIGHT or COLOR_DIM,
            page     = debugOverlay.page,
            uiscale  = uiscale,
            x        = s.margin,
            y        = baseY + (i - 1) * (s.fontSize + s.rowSpacing),
            zIndex   = 1000,
        })
        table.insert(debugOverlay.locationEntries,
            { id = lblId, name = d.name })
    end
end

local function buildStructureList()
    destroyStructureList()
    local s = scale.applyAll(debugOverlay.baseSizes)
    local uiscale = scale.get()
    local structures = require("scripts.structures")
    local kinds = structures.kinds or {}
    local baseY = structureButtonY(s) + s.fontSize

    for i, kind in ipairs(kinds) do
        local isArmed = (kind == debugOverlay.armedStructure)
        local lblId = label.new({
            name     = "structure_entry_" .. kind,
            text     = formatEntry(kind, isArmed),
            font     = debugOverlay.debugFont,
            fontSize = debugOverlay.baseSizes.fontSize,
            color    = isArmed and COLOR_BRIGHT or COLOR_DIM,
            page     = debugOverlay.page,
            uiscale  = uiscale,
            x        = s.margin,
            y        = baseY + (i - 1) * (s.fontSize + s.rowSpacing),
            zIndex   = 1000,
        })
        table.insert(debugOverlay.structureEntries,
            { id = lblId, name = kind })
    end
end

local function buildFluidList()
    destroyFluidList()
    local s = scale.applyAll(debugOverlay.baseSizes)
    local uiscale = scale.get()
    local baseY = fluidButtonY(s) + s.fontSize
    for i, kind in ipairs(debugOverlay.fluidKinds) do
        local isArmed = (kind == debugOverlay.armedFluidType)
        local lblId = label.new({
            name     = "fluid_entry_" .. kind,
            text     = formatEntry(kind, isArmed),
            font     = debugOverlay.debugFont,
            fontSize = debugOverlay.baseSizes.fontSize,
            color    = isArmed and COLOR_BRIGHT or COLOR_DIM,
            page     = debugOverlay.page,
            uiscale  = uiscale,
            x        = s.margin,
            y        = baseY + (i - 1) * (s.fontSize + s.rowSpacing),
            zIndex   = 1000,
        })
        table.insert(debugOverlay.fluidEntries,
            { id = lblId, kind = kind })
    end
end

local function buildSpawnList()
    destroySpawnList()
    local s = scale.applyAll(debugOverlay.baseSizes)
    local uiscale = scale.get()
    local defs = unit.listDefs() or {}
    table.sort(defs)

    if #defs == 0 then
        local lblId = label.new({
            name     = "spawn_list_empty",
            text     = "  (no units defined)",
            font     = debugOverlay.debugFont,
            fontSize = debugOverlay.baseSizes.fontSize,
            color    = COLOR_DIM,
            page     = debugOverlay.page,
            uiscale  = uiscale,
            x        = s.margin,
            y        = s.margin + 3 * (s.fontSize + s.rowSpacing),
            zIndex   = 1000,
        })
        table.insert(debugOverlay.spawnEntries,
            { id = lblId, defName = nil })
        return
    end

    local baseY = s.margin + 2 * (s.fontSize + s.rowSpacing) + s.fontSize
    for i, defName in ipairs(defs) do
        local isArmed = (defName == debugOverlay.armedDef)
        local lblId = label.new({
            name     = "spawn_entry_" .. defName,
            text     = formatEntry(defName, isArmed),
            font     = debugOverlay.debugFont,
            fontSize = debugOverlay.baseSizes.fontSize,
            color    = isArmed and COLOR_BRIGHT or COLOR_DIM,
            page     = debugOverlay.page,
            uiscale  = uiscale,
            x        = s.margin,
            y        = baseY + (i - 1) * (s.fontSize + s.rowSpacing),
            zIndex   = 1000,
        })
        table.insert(debugOverlay.spawnEntries,
            { id = lblId, defName = defName })
    end
end

function debugOverlay.createUI()
    if debugOverlay.uiCreated and debugOverlay.page then
        UI.deletePage(debugOverlay.page)
        if debugOverlay.fpsLabelId then
            label.destroy(debugOverlay.fpsLabelId)
            debugOverlay.fpsLabelId = nil
        end
        if debugOverlay.spawnButtonId then
            label.destroy(debugOverlay.spawnButtonId)
            debugOverlay.spawnButtonId = nil
        end
        if debugOverlay.fluidButtonId then
            label.destroy(debugOverlay.fluidButtonId)
            debugOverlay.fluidButtonId = nil
        end
        if debugOverlay.itemButtonId then
            label.destroy(debugOverlay.itemButtonId)
            debugOverlay.itemButtonId = nil
        end
        if debugOverlay.terrainButtonId then
            label.destroy(debugOverlay.terrainButtonId)
            debugOverlay.terrainButtonId = nil
        end
        if debugOverlay.locationButtonId then
            label.destroy(debugOverlay.locationButtonId)
            debugOverlay.locationButtonId = nil
        end
        if debugOverlay.structureButtonId then
            label.destroy(debugOverlay.structureButtonId)
            debugOverlay.structureButtonId = nil
        end
        destroySpawnList()
        destroyFluidList()
        destroyItemList()
        destroyTerrainList()
        destroyLocationList()
        destroyStructureList()
    end

    local s = scale.applyAll(debugOverlay.baseSizes)
    local uiscale = scale.get()
    debugOverlay.page = UI.newPage("debug_overlay", "overlay")

    debugOverlay.fpsLabelId = label.new({
        name     = "fps_text",
        text     = "FPS: --",
        font     = debugOverlay.debugFont,
        fontSize = debugOverlay.baseSizes.fontSize,
        color    = COLOR_DIM,
        page     = debugOverlay.page,
        uiscale  = uiscale,
        x        = s.margin,
        y        = s.margin + s.fontSize,
        zIndex   = 1000,
    })

    debugOverlay.spawnButtonId = label.new({
        name     = "spawn_button",
        text     = "spawn",
        font     = debugOverlay.debugFont,
        fontSize = debugOverlay.baseSizes.fontSize,
        color    = COLOR_DIM,
        page     = debugOverlay.page,
        uiscale  = uiscale,
        x        = s.margin,
        y        = s.margin + 2 * (s.fontSize + s.rowSpacing),
        zIndex   = 1000,
    })

    if debugOverlay.spawnListVisible then
        buildSpawnList()
    end

    -- Fluid button — placed below the spawn button (and its open list
    -- if any). The label position is computed by fluidButtonY so a
    -- single source of truth handles the shifting layout.
    local s2 = scale.applyAll(debugOverlay.baseSizes)
    debugOverlay.fluidButtonId = label.new({
        name     = "fluid_button",
        text     = "fluid",
        font     = debugOverlay.debugFont,
        fontSize = debugOverlay.baseSizes.fontSize,
        color    = COLOR_DIM,
        page     = debugOverlay.page,
        uiscale  = uiscale,
        x        = s2.margin,
        y        = fluidButtonY(s2),
        zIndex   = 1000,
    })

    if debugOverlay.fluidListVisible then
        buildFluidList()
    end

    -- Item button — below the fluid section, same shifting-layout
    -- rule via itemButtonY.
    debugOverlay.itemButtonId = label.new({
        name     = "item_button",
        text     = "items",
        font     = debugOverlay.debugFont,
        fontSize = debugOverlay.baseSizes.fontSize,
        color    = COLOR_DIM,
        page     = debugOverlay.page,
        uiscale  = uiscale,
        x        = s2.margin,
        y        = itemButtonY(s2),
        zIndex   = 1000,
    })

    if debugOverlay.itemListVisible then
        buildItemList()
    end

    -- Terrain button — below the items section, same shifting-layout
    -- rule via terrainButtonY.
    debugOverlay.terrainButtonId = label.new({
        name     = "terrain_button",
        text     = "terrain",
        font     = debugOverlay.debugFont,
        fontSize = debugOverlay.baseSizes.fontSize,
        color    = COLOR_DIM,
        page     = debugOverlay.page,
        uiscale  = uiscale,
        x        = s2.margin,
        y        = terrainButtonY(s2),
        zIndex   = 1000,
    })

    if debugOverlay.terrainListVisible then
        buildTerrainList()
    end

    -- Locations button — below the terrain section, same shifting-layout
    -- rule via locationButtonY.
    debugOverlay.locationButtonId = label.new({
        name     = "location_button",
        text     = "locations",
        font     = debugOverlay.debugFont,
        fontSize = debugOverlay.baseSizes.fontSize,
        color    = COLOR_DIM,
        page     = debugOverlay.page,
        uiscale  = uiscale,
        x        = s2.margin,
        y        = locationButtonY(s2),
        zIndex   = 1000,
    })

    if debugOverlay.locationListVisible then
        buildLocationList()
    end

    -- Structures button — below the locations section.
    debugOverlay.structureButtonId = label.new({
        name     = "structure_button",
        text     = "structures",
        font     = debugOverlay.debugFont,
        fontSize = debugOverlay.baseSizes.fontSize,
        color    = COLOR_DIM,
        page     = debugOverlay.page,
        uiscale  = uiscale,
        x        = s2.margin,
        y        = structureButtonY(s2),
        zIndex   = 1000,
    })

    if debugOverlay.structureListVisible then
        buildStructureList()
    end

    debugOverlay.uiCreated = true
    rebuildClickableRects()
end

function debugOverlay.update(dt)
    local fps = engine.getFPS()
    if debugOverlay.fpsLabelId then
        label.setText(debugOverlay.fpsLabelId, "FPS: " .. tostring(math.floor(fps)))
    end
end

-----------------------------------------------------------
-- Visibility
-----------------------------------------------------------
-- The overlay is only meaningful in the gameplay world view, but F8
-- keydown is broadcast to every Lua module unconditionally, so without
-- this gate the user could re-summon the overlay onto Settings, the
-- pause menu, the zoom map, or the fade zone right after a teardown path
-- hid it (#147). Refuse to OPEN it outside gameplay; hiding always works.
function debugOverlay.canShow()
    local ok, allowed = pcall(function()
        -- Blocks menus/overlays: Settings, pause menu, main menu, etc.
        if not require("scripts.ui_manager").isGameplayInputActive() then
            return false
        end
        -- Only the zoomed-in view is gameplay; block the zoom map and
        -- fade zone. Both the full world view and the test arena drive
        -- hud.currentView through the same hud.onScroll machinery
        -- (uiManager.onScroll forwards to it in both views), so this one
        -- check covers them uniformly.
        return require("scripts.hud").currentView == "zoomed_in"
    end)
    -- Never let the gate itself break F8: fall back to allowing on error.
    if not ok then return true end
    return allowed
end

function debugOverlay.show()
    if not debugOverlay.canShow() then return end
    if not debugOverlay.uiCreated then debugOverlay.createUI() end
    debugOverlay.visible = true
    if debugOverlay.page then UI.showPage(debugOverlay.page) end
end

function debugOverlay.hide()
    debugOverlay.visible = false
    debugOverlay.clearArmed()
    debugOverlay.clearArmedFluid()
    debugOverlay.clearArmedItem()
    debugOverlay.clearArmedTerrain()
    debugOverlay.clearArmedLocation()
    debugOverlay.clearArmedStructure()
    debugOverlay.closeSpawnList()
    debugOverlay.closeFluidList()
    debugOverlay.closeItemList()
    debugOverlay.closeTerrainList()
    debugOverlay.closeLocationList()
    debugOverlay.closeStructureList()
    debugOverlay.clickableRects = {}
    if debugOverlay.page then UI.hidePage(debugOverlay.page) end
end

function debugOverlay.toggle()
    if debugOverlay.visible then debugOverlay.hide() else debugOverlay.show() end
end

function debugOverlay.isVisible() return debugOverlay.visible end

-----------------------------------------------------------
-- Spawn UI state
-----------------------------------------------------------
-- Reposition the fluid button + its open list (if any) so it sits
-- below the spawn list. Called whenever the spawn list opens/closes
-- so the fluid section follows the layout shift. fluidButtonY already
-- knows about spawnListVisible, so this is just label.setPosition
-- calls — no recomputation.
local function repositionFluidUI()
    if not debugOverlay.uiCreated then return end
    local s = scale.applyAll(debugOverlay.baseSizes)
    local fbY = fluidButtonY(s)
    if debugOverlay.fluidButtonId then
        label.setPosition(debugOverlay.fluidButtonId, s.margin, fbY)
    end
    if debugOverlay.fluidListVisible then
        local baseY = fbY + s.fontSize
        for i, entry in ipairs(debugOverlay.fluidEntries) do
            if entry.id then
                label.setPosition(entry.id, s.margin,
                    baseY + (i - 1) * (s.fontSize + s.rowSpacing))
            end
        end
    end
    -- Item section trails the fluid section.
    local ibY = itemButtonY(s)
    if debugOverlay.itemButtonId then
        label.setPosition(debugOverlay.itemButtonId, s.margin, ibY)
    end
    if debugOverlay.itemListVisible then
        local baseY = ibY + s.fontSize
        for i, entry in ipairs(debugOverlay.itemEntries) do
            if entry.id then
                label.setPosition(entry.id, s.margin,
                    baseY + (i - 1) * (s.fontSize + s.rowSpacing))
            end
        end
    end
    -- Terrain section trails the items section.
    local tbY = terrainButtonY(s)
    if debugOverlay.terrainButtonId then
        label.setPosition(debugOverlay.terrainButtonId, s.margin, tbY)
    end
    if debugOverlay.terrainListVisible then
        local baseY = tbY + s.fontSize
        for i, entry in ipairs(debugOverlay.terrainEntries) do
            if entry.id then
                label.setPosition(entry.id, s.margin,
                    baseY + (i - 1) * (s.fontSize + s.rowSpacing))
            end
        end
    end
    -- Location section trails the terrain section.
    local lbY = locationButtonY(s)
    if debugOverlay.locationButtonId then
        label.setPosition(debugOverlay.locationButtonId, s.margin, lbY)
    end
    if debugOverlay.locationListVisible then
        local baseY = lbY + s.fontSize
        for i, entry in ipairs(debugOverlay.locationEntries) do
            if entry.id then
                label.setPosition(entry.id, s.margin,
                    baseY + (i - 1) * (s.fontSize + s.rowSpacing))
            end
        end
    end
    -- Structure section trails the location section.
    local sbY = structureButtonY(s)
    if debugOverlay.structureButtonId then
        label.setPosition(debugOverlay.structureButtonId, s.margin, sbY)
    end
    if debugOverlay.structureListVisible then
        local baseY = sbY + s.fontSize
        for i, entry in ipairs(debugOverlay.structureEntries) do
            if entry.id then
                label.setPosition(entry.id, s.margin,
                    baseY + (i - 1) * (s.fontSize + s.rowSpacing))
            end
        end
    end
end

function debugOverlay.openSpawnList()
    if debugOverlay.spawnListVisible then return end
    debugOverlay.spawnListVisible = true
    buildSpawnList()
    repositionFluidUI()
    rebuildClickableRects()
end

function debugOverlay.closeSpawnList()
    if not debugOverlay.spawnListVisible then return end
    debugOverlay.spawnListVisible = false
    destroySpawnList()
    repositionFluidUI()
    rebuildClickableRects()
end

function debugOverlay.setArmed(defName)
    debugOverlay.armedDef = defName
    -- Mutually exclusive with the other arm modes.
    debugOverlay.armedFluidType = nil
    debugOverlay.armedItemDef = nil
    debugOverlay.armedTerrainId = nil
    debugOverlay.armedLocation = nil
    refreshEntries()
end

function debugOverlay.clearArmed()
    if debugOverlay.armedDef == nil then return end
    debugOverlay.armedDef = nil
    refreshEntries()
end

function debugOverlay.openFluidList()
    if debugOverlay.fluidListVisible then return end
    debugOverlay.fluidListVisible = true
    buildFluidList()
    -- Spawn list sits ABOVE fluid so it doesn't move; the item
    -- section below does (repositionFluidUI handles both sections).
    repositionFluidUI()
    rebuildClickableRects()
end

function debugOverlay.closeFluidList()
    if not debugOverlay.fluidListVisible then return end
    debugOverlay.fluidListVisible = false
    destroyFluidList()
    repositionFluidUI()
    rebuildClickableRects()
end

function debugOverlay.setArmedFluid(kind)
    debugOverlay.armedFluidType = kind
    debugOverlay.armedDef = nil
    debugOverlay.armedItemDef = nil
    debugOverlay.armedTerrainId = nil
    debugOverlay.armedLocation = nil
    refreshEntries()
end

function debugOverlay.clearArmedFluid()
    if debugOverlay.armedFluidType == nil then return end
    debugOverlay.armedFluidType = nil
    refreshEntries()
end

function debugOverlay.openItemList()
    if debugOverlay.itemListVisible then return end
    debugOverlay.itemListVisible = true
    buildItemList()
    repositionFluidUI()   -- terrain section below shifts
    rebuildClickableRects()
end

function debugOverlay.closeItemList()
    if not debugOverlay.itemListVisible then return end
    debugOverlay.itemListVisible = false
    destroyItemList()
    repositionFluidUI()   -- terrain section below shifts
    rebuildClickableRects()
end

function debugOverlay.setArmedItem(defName)
    debugOverlay.armedItemDef = defName
    -- Mutually exclusive with the other arm modes.
    debugOverlay.armedDef = nil
    debugOverlay.armedFluidType = nil
    debugOverlay.armedTerrainId = nil
    debugOverlay.armedLocation = nil
    refreshEntries()
end

function debugOverlay.clearArmedItem()
    if debugOverlay.armedItemDef == nil then return end
    debugOverlay.armedItemDef = nil
    refreshEntries()
end

function debugOverlay.openTerrainList()
    if debugOverlay.terrainListVisible then return end
    debugOverlay.terrainListVisible = true
    buildTerrainList()
    repositionFluidUI()   -- location section below shifts
    rebuildClickableRects()
end

function debugOverlay.closeTerrainList()
    if not debugOverlay.terrainListVisible then return end
    debugOverlay.terrainListVisible = false
    destroyTerrainList()
    repositionFluidUI()   -- location section below shifts
    rebuildClickableRects()
end

function debugOverlay.setArmedTerrain(matId)
    debugOverlay.armedTerrainId = matId
    -- Mutually exclusive with the other arm modes.
    debugOverlay.armedDef = nil
    debugOverlay.armedFluidType = nil
    debugOverlay.armedItemDef = nil
    debugOverlay.armedLocation = nil
    refreshEntries()
end

function debugOverlay.clearArmedTerrain()
    if debugOverlay.armedTerrainId == nil then return end
    debugOverlay.armedTerrainId = nil
    refreshEntries()
end

function debugOverlay.openLocationList()
    if debugOverlay.locationListVisible then return end
    debugOverlay.locationListVisible = true
    buildLocationList()
    repositionFluidUI()   -- structure section below shifts
    rebuildClickableRects()
end

function debugOverlay.closeLocationList()
    if not debugOverlay.locationListVisible then return end
    debugOverlay.locationListVisible = false
    destroyLocationList()
    repositionFluidUI()   -- structure section below shifts
    rebuildClickableRects()
end

function debugOverlay.setArmedLocation(name)
    debugOverlay.armedLocation = name
    -- Mutually exclusive with the other arm modes.
    debugOverlay.armedDef = nil
    debugOverlay.armedFluidType = nil
    debugOverlay.armedItemDef = nil
    debugOverlay.armedTerrainId = nil
    debugOverlay.armedStructure = nil
    refreshEntries()
end

function debugOverlay.clearArmedLocation()
    if debugOverlay.armedLocation == nil then return end
    debugOverlay.armedLocation = nil
    refreshEntries()
end

function debugOverlay.openStructureList()
    if debugOverlay.structureListVisible then return end
    debugOverlay.structureListVisible = true
    buildStructureList()
    rebuildClickableRects()
end

function debugOverlay.closeStructureList()
    if not debugOverlay.structureListVisible then return end
    debugOverlay.structureListVisible = false
    destroyStructureList()
    rebuildClickableRects()
end

function debugOverlay.setArmedStructure(name)
    debugOverlay.armedStructure = name
    -- Mutually exclusive with the other arm modes.
    debugOverlay.armedDef = nil
    debugOverlay.armedFluidType = nil
    debugOverlay.armedItemDef = nil
    debugOverlay.armedTerrainId = nil
    debugOverlay.armedLocation = nil
    refreshEntries()
end

function debugOverlay.clearArmedStructure()
    if debugOverlay.armedStructure == nil then return end
    debugOverlay.armedStructure = nil
    refreshEntries()
end

-----------------------------------------------------------
-- Parallel click dispatch
-----------------------------------------------------------
-- True if (button, x, y) lands inside one of our clickable rects.
-- Fires the rect's action on first call within a click event;
-- subsequent calls with the same (button, x, y) return true without
-- re-firing — that's how multiple onMouseDown subscribers (game,
-- uiManager, ...) can all check and skip their work in unison.
function debugOverlay.tryClaimClick(button, x, y)
    if not debugOverlay.visible then return false end
    if button ~= MOUSE_LEFT then return false end

    if debugOverlay.lastClaim
       and debugOverlay.lastClaim.button == button
       and debugOverlay.lastClaim.x == x
       and debugOverlay.lastClaim.y == y then
        return true
    end

    -- onMouseDown coords are window-pixels; our rects are in
    -- framebuffer-pixels (built off `scale`). Convert before comparing.
    local fx, fy = x, y
    local ww, wh = engine.getWindowSize()
    local fbW, fbH = engine.getFramebufferSize()
    if ww and wh and ww > 0 and wh > 0 then
        fx = x * (fbW / ww)
        fy = y * (fbH / wh)
    end

    for _, rect in ipairs(debugOverlay.clickableRects) do
        if fx >= rect.x and fx <= rect.x + rect.w
           and fy >= rect.y and fy <= rect.y + rect.h then
            rect.action()
            debugOverlay.lastClaim = { button = button, x = x, y = y }
            return true
        end
    end
    return false
end

-- Clear the claim dedup when the physical button releases. Without
-- this, lastClaim persisted across clicks, so a SECOND click at the
-- exact same pixel (common: toggling the spawn list without moving
-- the mouse) matched the stale claim — swallowed but never fired.
-- onMouseUp is an engine broadcast and fires on every release.
function debugOverlay.onMouseUp(button, x, y, downRoute)
    debugOverlay.lastClaim = nil
end

-----------------------------------------------------------
-- Input hooks
-----------------------------------------------------------
function debugOverlay.onKeyDown(key)
    if key == "F8" then
        debugOverlay.toggle()
    elseif key == "Escape" then
        if debugOverlay.armedDef then debugOverlay.clearArmed() end
        if debugOverlay.armedFluidType then
            debugOverlay.clearArmedFluid()
        end
        if debugOverlay.armedItemDef then
            debugOverlay.clearArmedItem()
        end
        if debugOverlay.armedTerrainId then
            debugOverlay.clearArmedTerrain()
        end
        if debugOverlay.armedLocation then
            debugOverlay.clearArmedLocation()
        end
        if debugOverlay.armedStructure then
            debugOverlay.clearArmedStructure()
        end
    end
end

function debugOverlay.onFramebufferResize(width, height)
    if debugOverlay.uiCreated then
        debugOverlay.createUI()
        if debugOverlay.visible then UI.showPage(debugOverlay.page) end
    end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------
function debugOverlay.shutdown()
    destroySpawnList()
    destroyFluidList()
    destroyItemList()
    destroyTerrainList()
    destroyLocationList()
    destroyStructureList()
    if debugOverlay.spawnButtonId then
        label.destroy(debugOverlay.spawnButtonId)
        debugOverlay.spawnButtonId = nil
    end
    if debugOverlay.fluidButtonId then
        label.destroy(debugOverlay.fluidButtonId)
        debugOverlay.fluidButtonId = nil
    end
    if debugOverlay.itemButtonId then
        label.destroy(debugOverlay.itemButtonId)
        debugOverlay.itemButtonId = nil
    end
    if debugOverlay.terrainButtonId then
        label.destroy(debugOverlay.terrainButtonId)
        debugOverlay.terrainButtonId = nil
    end
    if debugOverlay.locationButtonId then
        label.destroy(debugOverlay.locationButtonId)
        debugOverlay.locationButtonId = nil
    end
    if debugOverlay.structureButtonId then
        label.destroy(debugOverlay.structureButtonId)
        debugOverlay.structureButtonId = nil
    end
    if debugOverlay.fpsLabelId then
        label.destroy(debugOverlay.fpsLabelId)
        debugOverlay.fpsLabelId = nil
    end
    if debugOverlay.page then
        UI.hidePage(debugOverlay.page)
        UI.deletePage(debugOverlay.page)
        debugOverlay.page = nil
    end
    debugOverlay.clickableRects = {}
end

return debugOverlay
