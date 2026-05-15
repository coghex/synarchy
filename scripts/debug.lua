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

    -- List entry rects (only if list is open)
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
        destroySpawnList()
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
function debugOverlay.show()
    if not debugOverlay.uiCreated then debugOverlay.createUI() end
    debugOverlay.visible = true
    if debugOverlay.page then UI.showPage(debugOverlay.page) end
end

function debugOverlay.hide()
    debugOverlay.visible = false
    debugOverlay.clearArmed()
    debugOverlay.closeSpawnList()
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
function debugOverlay.openSpawnList()
    if debugOverlay.spawnListVisible then return end
    debugOverlay.spawnListVisible = true
    buildSpawnList()
    rebuildClickableRects()
end

function debugOverlay.closeSpawnList()
    if not debugOverlay.spawnListVisible then return end
    debugOverlay.spawnListVisible = false
    destroySpawnList()
    rebuildClickableRects()
end

function debugOverlay.setArmed(defName)
    debugOverlay.armedDef = defName
    refreshEntries()
end

function debugOverlay.clearArmed()
    if debugOverlay.armedDef == nil then return end
    debugOverlay.armedDef = nil
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

-----------------------------------------------------------
-- Input hooks
-----------------------------------------------------------
function debugOverlay.onKeyDown(key)
    if key == "F8" then
        debugOverlay.toggle()
    elseif key == "Escape" and debugOverlay.armedDef then
        debugOverlay.clearArmed()
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
    if debugOverlay.spawnButtonId then
        label.destroy(debugOverlay.spawnButtonId)
        debugOverlay.spawnButtonId = nil
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
