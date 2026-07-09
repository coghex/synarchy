-- Debug overlay module
--
-- Toggled with F8. Shows green diagnostic text in the top-left: an FPS
-- line, then one button per category (spawn/fluid/item/terrain/
-- location/structure). Clicking a category's button opens a vertical
-- list of its entries; clicking an entry arms that category (a "> "
-- prefix + yellow highlight) and left-clicks on the map perform that
-- category's world action at the hover tile (see scripts/init_mouse.lua,
-- which reads the armed* fields and calls unit.spawn / item.spawnGround
-- / world.setFluidTile / world.addTile / locations.stamp / the
-- structures module). Exits: right-click, ESC, click the button again,
-- F8.
--
-- This file is the overlay shell/coordinator (#545): lifecycle,
-- visibility gating, the parallel click hit-test, and mutual-exclusion
-- wiring between categories. Each category's list-building, armed-state
-- handling, and layout math live in scripts/debug/ (mode.lua = one
-- category's behavior, modes.lua = the six concrete categories,
-- layout.lua = the vertical-stacking math shared by all of them).
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
local Mode = require("scripts.debug.mode")
local layout = require("scripts.debug.layout")
local modeSpecs = require("scripts.debug.modes")

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
debugOverlay.debugFont = nil
debugOverlay.visible = false
debugOverlay.uiCreated = false

-- Parallel hit-test state (independent of the UI manager).
debugOverlay.clickableRects = {}      -- array of { x, y, w, h, action }
debugOverlay.lastClaim = nil          -- {button, x, y} for idempotency

debugOverlay.baseSizes = {
    fontSize    = 32,
    margin      = 10,
    rowSpacing  = 6,
}

-- `scripts.debug.modes` is a plain require-cached module, so on a
-- second load (see the self-registration comment above) it would hand
-- back the SAME Mode instances — including whatever entries/armed
-- state a previous load left behind. Reset the mutable bits explicitly
-- every load, same as the monolith used to reset each armed*/*Entries
-- field directly.
debugOverlay.modeOrder = modeSpecs
for _, mode in ipairs(debugOverlay.modeOrder) do
    mode.entries = {}
    mode.listVisible = false
    mode.buttonId = nil
    debugOverlay[mode.armedField] = nil
end

local MOUSE_LEFT = 1

function debugOverlay.init(scriptId)
    engine.logInfo("Debug overlay initializing...")
    debugOverlay.debugFont = engine.loadFont(
        "assets/fonts/shell.ttf", debugOverlay.baseSizes.fontSize)
    engine.logDebug("Debug overlay initialized")
end

-----------------------------------------------------------
-- Mode orchestration (mutual exclusion, open/close, click rects)
-----------------------------------------------------------
-- Forward-declared: rebuildClickableRects and openMode/closeMode call
-- into each other (a click on a mode's button opens/closes it, which
-- rebuilds the rects), so Lua's usual top-to-bottom local scoping can't
-- express the pair directly.
local rebuildClickableRects
local repositionBelowSpawn
local openMode, closeMode, setArmedMode, clearArmedMode

local function refreshAllEntries()
    for _, mode in ipairs(debugOverlay.modeOrder) do
        mode:refreshEntries(debugOverlay)
    end
end

function setArmedMode(mode, value)
    debugOverlay[mode.armedField] = value
    for _, otherField in ipairs(mode.exclusiveWith) do
        debugOverlay[otherField] = nil
    end
    refreshAllEntries()
end

function clearArmedMode(mode)
    if debugOverlay[mode.armedField] == nil then return end
    debugOverlay[mode.armedField] = nil
    refreshAllEntries()
end

-- Reposition every category below spawn (spawn's own button/list never
-- move — nothing sits above it) after any list opens/closes below it.
repositionBelowSpawn = function()
    if not debugOverlay.uiCreated then return end
    local s = scale.applyAll(debugOverlay.baseSizes)
    local ys = layout.computeYs(debugOverlay.modeOrder, s)
    for i = 2, #debugOverlay.modeOrder do
        local mode = debugOverlay.modeOrder[i]
        local y = ys[mode.key]
        if mode.buttonId then label.setPosition(mode.buttonId, s.margin, y) end
        if mode.listVisible then
            local baseY = y + s.fontSize
            for j, entry in ipairs(mode.entries) do
                if entry.id then
                    label.setPosition(entry.id, s.margin,
                        baseY + (j - 1) * (s.fontSize + s.rowSpacing))
                end
            end
        end
    end
end

openMode = function(mode)
    if mode.listVisible then return end
    mode.listVisible = true
    local s = scale.applyAll(debugOverlay.baseSizes)
    local y = layout.computeYs(debugOverlay.modeOrder, s)[mode.key]
    mode:buildList(debugOverlay, y + s.fontSize)
    repositionBelowSpawn()
    rebuildClickableRects()
end

closeMode = function(mode)
    if not mode.listVisible then return end
    mode.listVisible = false
    mode:destroyList()
    repositionBelowSpawn()
    rebuildClickableRects()
end

-- Rebuild clickable rects from current label state. Called whenever the
-- layout changes (createUI, list open/close, arm change).
rebuildClickableRects = function()
    debugOverlay.clickableRects = {}
    if not debugOverlay.uiCreated then return end

    local s = scale.applyAll(debugOverlay.baseSizes)
    local ys = layout.computeYs(debugOverlay.modeOrder, s)

    for _, mode in ipairs(debugOverlay.modeOrder) do
        local y = ys[mode.key]
        local rect = layout.rowRect(debugOverlay.debugFont, s, y, mode.label)
        rect.action = function()
            if mode.listVisible or debugOverlay[mode.armedField] then
                clearArmedMode(mode)
                closeMode(mode)
            else
                openMode(mode)
            end
        end
        table.insert(debugOverlay.clickableRects, rect)

        if mode.listVisible then
            local baseY = y + s.fontSize
            for i, entry in ipairs(mode.entries) do
                if entry.value ~= nil then
                    local rowY = baseY + (i - 1) * (s.fontSize + s.rowSpacing)
                    local text = Mode.formatEntry(entry.text, false)
                    local rect2 = layout.rowRect(debugOverlay.debugFont, s,
                                                  rowY, text)
                    local value = entry.value
                    rect2.action = function() setArmedMode(mode, value) end
                    table.insert(debugOverlay.clickableRects, rect2)
                end
            end
        end
    end
end

-- Public per-category names. These are load-bearing: scripts/init_mouse.lua
-- and others call debugOverlay.clearArmed()/clearArmedItem()/etc. and
-- debugOverlay.openSpawnList()/etc. by these exact names.
local WRAPPER_NAMES = {
    spawn     = { open = "openSpawnList",     close = "closeSpawnList",
                  setArmed = "setArmed",          clearArmed = "clearArmed" },
    fluid     = { open = "openFluidList",     close = "closeFluidList",
                  setArmed = "setArmedFluid",     clearArmed = "clearArmedFluid" },
    item      = { open = "openItemList",      close = "closeItemList",
                  setArmed = "setArmedItem",      clearArmed = "clearArmedItem" },
    terrain   = { open = "openTerrainList",   close = "closeTerrainList",
                  setArmed = "setArmedTerrain",    clearArmed = "clearArmedTerrain" },
    location  = { open = "openLocationList",  close = "closeLocationList",
                  setArmed = "setArmedLocation",   clearArmed = "clearArmedLocation" },
    structure = { open = "openStructureList", close = "closeStructureList",
                  setArmed = "setArmedStructure",  clearArmed = "clearArmedStructure" },
}

for _, mode in ipairs(debugOverlay.modeOrder) do
    local names = WRAPPER_NAMES[mode.key]
    debugOverlay[names.open]       = function() openMode(mode) end
    debugOverlay[names.close]      = function() closeMode(mode) end
    debugOverlay[names.setArmed]   = function(value) setArmedMode(mode, value) end
    debugOverlay[names.clearArmed] = function() clearArmedMode(mode) end
end

-----------------------------------------------------------
-- UI creation
-----------------------------------------------------------
function debugOverlay.createUI()
    if debugOverlay.uiCreated and debugOverlay.page then
        UI.deletePage(debugOverlay.page)
        if debugOverlay.fpsLabelId then
            label.destroy(debugOverlay.fpsLabelId)
            debugOverlay.fpsLabelId = nil
        end
        for _, mode in ipairs(debugOverlay.modeOrder) do
            mode:destroyButton()
            mode:destroyList()
        end
    end

    local s = scale.applyAll(debugOverlay.baseSizes)
    local uiscale = scale.get()
    debugOverlay.page = UI.newPage("debug_overlay", "overlay")

    debugOverlay.fpsLabelId = label.new({
        name     = "fps_text",
        text     = "FPS: --",
        font     = debugOverlay.debugFont,
        fontSize = debugOverlay.baseSizes.fontSize,
        color    = Mode.COLOR_DIM,
        page     = debugOverlay.page,
        uiscale  = uiscale,
        x        = s.margin,
        y        = s.margin + s.fontSize,
        zIndex   = 1000,
    })

    -- Each category's button sits below the previous one (and its open
    -- list, if any) — layout.advance is the single source of truth for
    -- that shifting arithmetic.
    local y = layout.anchorY(s)
    for _, mode in ipairs(debugOverlay.modeOrder) do
        mode:createButton(debugOverlay, uiscale, s, y)
        if mode.listVisible then
            mode:buildList(debugOverlay, y + s.fontSize)
        end
        y = layout.advance(y, s, mode)
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
-- True when the overlay is valid in the CURRENT view: gameplay input is
-- active (not Settings/pause/main menu/etc.) AND we're zoomed in (not the
-- zoom map or fade zone). The single source of truth for "should the
-- overlay be live here", used both to gate F8 opening (canShow) and to
-- gate the parallel click hit-test (tryClaimClick) so a stale-visible
-- overlay left behind by a zoom/menu transition can neither be re-opened
-- nor swallow clicks outside gameplay (#147, #151).
function debugOverlay.inGameplayView()
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
    -- Never let the gate itself break the debug overlay: allow on error.
    if not ok then return true end
    return allowed
end

-- The overlay is only meaningful in the gameplay world view, but F8
-- keydown is broadcast to every Lua module unconditionally, so without
-- this gate the user could re-summon the overlay onto Settings, the
-- pause menu, the zoom map, or the fade zone right after a teardown path
-- hid it (#147). Refuse to OPEN it outside gameplay; hiding always works.
function debugOverlay.canShow()
    return debugOverlay.inGameplayView()
end

function debugOverlay.show()
    if not debugOverlay.canShow() then return end
    if not debugOverlay.uiCreated then debugOverlay.createUI() end
    debugOverlay.visible = true
    if debugOverlay.page then UI.showPage(debugOverlay.page) end
end

function debugOverlay.hide()
    debugOverlay.visible = false
    for _, mode in ipairs(debugOverlay.modeOrder) do
        clearArmedMode(mode)
    end
    for _, mode in ipairs(debugOverlay.modeOrder) do
        closeMode(mode)
    end
    debugOverlay.clickableRects = {}
    if debugOverlay.page then UI.hidePage(debugOverlay.page) end
end

function debugOverlay.toggle()
    if debugOverlay.visible then debugOverlay.hide() else debugOverlay.show() end
end

function debugOverlay.isVisible() return debugOverlay.visible end

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
    -- Defense in depth (#151): the `visible` flag is cleared by the
    -- teardown paths (hud.hide / uiManager.showMenu, #147), but if any
    -- transition ever leaves it set, claiming on `visible` alone lets the
    -- overlay's rects swallow clicks on the zoom map or a menu opened over
    -- a hidden world. Gate on the same current-view predicate as F8 so the
    -- claim path can only fire while the overlay is actually valid here.
    if not debugOverlay.inGameplayView() then return false end
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
        for _, mode in ipairs(debugOverlay.modeOrder) do
            clearArmedMode(mode)
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
    for _, mode in ipairs(debugOverlay.modeOrder) do
        mode:destroyList()
    end
    for _, mode in ipairs(debugOverlay.modeOrder) do
        mode:destroyButton()
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
