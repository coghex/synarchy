-- Debug Anim Panel
--
-- When a unit is selected, pops a green-text overlay panel to the LEFT
-- of `unit_info_v2`'s right-edge pane. Currently has one tab — "Anim"
-- — listing every animation declared on the unit's def. Clicking an
-- entry "arms" that animation: the unit stops, the row turns yellow,
-- and the engine cycles the unit's facing through all 8 directions
-- while playing the armed animation. Click the same row again (or
-- click another row) to un-arm / switch. Selection change or empty
-- click clears arming.
--
-- Hides automatically when no unit is selected. Rebuilds on selection
-- change so the listed animations match the active unit.
--
-- The presentation is intentionally rough — ASCII tab strip + ASCII
-- divider rule, no 9-patches, no textures. Debug aesthetic only.
--
-- Click handling parallels `debug.lua`'s `tryClaimClick` pattern:
-- maintains its own `clickableRects` list, called from
-- `game.onMouseDown` in `init.lua` before normal selection logic.
-- Coords from onMouseDown are WINDOW pixels; our rects are
-- FRAMEBUFFER pixels (built off `scale`). We convert before hit-test.

local label = require("scripts.ui.label")
local scale = require("scripts.ui.scale")

local panel = package.loaded["scripts.debug_anim_panel"] or {}
package.loaded["scripts.debug_anim_panel"] = panel

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------
panel.page          = nil
panel.font          = nil
panel.visible       = false
panel.activeUid     = nil      -- which uid we last rebuilt for
panel.activeTab     = "Anim"
panel.tabs          = { "Anim" }
panel.labelIds      = {}       -- ALL label IDs (for teardown)
panel.animLabels    = {}       -- { [animName] = labelId } for re-color
panel.clickableRects = {}

-- Arming: the animation being previewed. While armed, the unit is
-- frozen via `unit.setFrozen` so the sim loop can't stomp our
-- setAnim/setFacing writes, and `tickCycle` advances the facing
-- direction. Clearing arming unfreezes the unit and resumes AI.
panel.armedAnim     = nil
panel.armedUid      = nil      -- which uid is frozen (may outlive selection)
panel.cycle         = nil      -- { dirIdx, dirStartedAt } when armed

-- 8 directions in clockwise order starting at south. Matches the
-- engine's `Direction` enum so a full cycle visits each facing once.
local CYCLE_DIRS = { "S", "SW", "W", "NW", "N", "NE", "E", "SE" }

-- Display time per direction in seconds. Most acolyte / bear anims
-- finish in well under 1.5s at their declared fps, so this gives the
-- player time to inspect each facing before the next.
local DIR_SECONDS   = 1.5

-----------------------------------------------------------
-- Layout (base; uiscale applied via scale.applyAll)
-----------------------------------------------------------
local INFO_PANEL_W       = 340   -- must match unit_info_v2.PANEL_W
local PANEL_W            = 320
local PANEL_MARGIN_RIGHT = 8
local PANEL_MARGIN_TOP   = 10
local PANEL_PAD_X        = 8
local PANEL_PAD_TOP      = 6
local ROW_H              = 18
local TAB_ROW_H          = 22
local DIVIDER_ROW_H      = 14
local FONT_SIZE          = 16

local COLOR_DIM    = {0.0, 1.0, 0.0, 1.0}   -- bright green (rows, inactive)
local COLOR_BRIGHT = {0.6, 1.0, 0.6, 1.0}   -- pale green (active tab)
local COLOR_BORDER = {0.0, 0.7, 0.0, 1.0}   -- darker green (dividers)
local COLOR_ARMED  = {1.0, 1.0, 0.4, 1.0}   -- yellow (armed anim row)

local MOUSE_LEFT = 1

-----------------------------------------------------------
-- Teardown
-----------------------------------------------------------
local function destroyLabels()
    for _, id in ipairs(panel.labelIds) do
        label.destroy(id)
    end
    panel.labelIds = {}
    panel.animLabels = {}
    panel.clickableRects = {}
end

local function destroyPage()
    destroyLabels()
    if panel.page then
        UI.deletePage(panel.page)
        panel.page = nil
    end
end

-----------------------------------------------------------
-- Build helpers
-----------------------------------------------------------
local function emitLabel(text, x, y, color)
    local id = label.new({
        name     = "debug_anim_lbl",
        text     = text,
        font     = panel.font,
        fontSize = FONT_SIZE,
        color    = color,
        page     = panel.page,
        uiscale  = scale.get(),
        x        = x,
        y        = y,
        zIndex   = 1100,
    })
    table.insert(panel.labelIds, id)
    return id
end

local function tabStripText()
    local parts = {}
    for _, name in ipairs(panel.tabs) do
        if name == panel.activeTab then
            table.insert(parts, "[ " .. name .. " ]")
        else
            table.insert(parts, "  " .. name .. "  ")
        end
    end
    return table.concat(parts, " ")
end

local function dividerLine(ch, targetW)
    local oneW = engine.getTextWidth(panel.font, ch, FONT_SIZE) or 8
    local n    = math.max(8, math.floor(targetW / oneW))
    return string.rep(ch, n)
end

-- Re-color every anim row label based on the current armedAnim.
local function refreshAnimRowColors()
    for name, id in pairs(panel.animLabels) do
        local c = (name == panel.armedAnim) and COLOR_ARMED or COLOR_DIM
        label.setColor(id, c)
    end
end

-----------------------------------------------------------
-- Arming
-----------------------------------------------------------
-- Clear the cycle driver and unfreeze the unit so AI resumes control.
local function disarm()
    if panel.armedUid then
        unit.setFrozen(panel.armedUid, false)
        unit.setForceLoop(panel.armedUid, false)
    end
    panel.armedAnim = nil
    panel.armedUid  = nil
    panel.cycle     = nil
    refreshAnimRowColors()
end

-- Arm an animation for preview-cycling on the given uid. Freezes the
-- unit (so the sim/AI loop can't overwrite our setAnim/setFacing
-- writes), force-loops the anim so one-shots (attacks, transitions,
-- death) cycle continuously within a direction window, and starts
-- the direction cycle at S.
local function arm(uid, animName)
    -- If a previous unit was armed, unfreeze it first so we don't
    -- leak frozen-state across switches.
    if panel.armedUid and panel.armedUid ~= uid then
        unit.setFrozen(panel.armedUid, false)
        unit.setForceLoop(panel.armedUid, false)
    end
    panel.armedAnim = animName
    panel.armedUid  = uid
    panel.cycle = {
        dirIdx       = 1,
        dirStartedAt = engine.gameTime(),
    }
    unit.stop(uid)
    unit.setFrozen(uid, true)
    unit.setForceLoop(uid, true)
    unit.setFacing(uid, CYCLE_DIRS[1])
    unit.setAnim(uid, animName)
    refreshAnimRowColors()
end

-----------------------------------------------------------
-- Cycle driver — fires every panel.update(dt) tick while armed.
-- Advances the direction every DIR_SECONDS. Only calls setAnim /
-- setFacing AT the direction transition (and at initial arm) — the
-- freeze flag keeps the sim loop from stomping the values in
-- between, so per-tick reassertion would just reset uiAnimStart
-- every 0.1s and pin the playback at frame 0 forever.
-----------------------------------------------------------
local function tickCycle(uid)
    if not panel.armedAnim or not panel.cycle then return end
    local now     = engine.gameTime()
    local elapsed = now - panel.cycle.dirStartedAt
    if elapsed < DIR_SECONDS then return end

    panel.cycle.dirIdx       = (panel.cycle.dirIdx % #CYCLE_DIRS) + 1
    panel.cycle.dirStartedAt = now
    local dir = CYCLE_DIRS[panel.cycle.dirIdx]
    unit.setFacing(uid, dir)
    -- setAnim resets uiAnimStart = now, so the new direction restarts
    -- the animation from frame 0. Wanted behaviour: each facing gets a
    -- full play-through of the animation.
    unit.setAnim(uid, panel.armedAnim)
end

-----------------------------------------------------------
-- Build / rebuild the panel for the given uid.
-----------------------------------------------------------
local function buildFor(uid)
    destroyLabels()

    local fbW, _ = engine.getFramebufferSize()
    if not fbW then return end

    local s        = scale.applyAll({ fontSize = FONT_SIZE })
    local infoW    = math.floor(INFO_PANEL_W       * scale.get())
    local myW      = math.floor(PANEL_W            * scale.get())
    local marginR  = math.floor(PANEL_MARGIN_RIGHT * scale.get())
    local marginT  = math.floor(PANEL_MARGIN_TOP   * scale.get())
    local padX     = math.floor(PANEL_PAD_X        * scale.get())
    local padTop   = math.floor(PANEL_PAD_TOP      * scale.get())
    local rowH     = math.floor(ROW_H              * scale.get())
    local tabRowH  = math.floor(TAB_ROW_H          * scale.get())
    local divH     = math.floor(DIVIDER_ROW_H      * scale.get())

    local x0 = fbW - infoW - marginR - myW
    local y0 = marginT
    local contentX = x0 + padX

    emitLabel("== DEBUG ANIM PANEL ==", contentX,
              y0 + padTop + s.fontSize, COLOR_BORDER)

    local tabY = y0 + padTop + s.fontSize + tabRowH
    emitLabel(tabStripText(), contentX, tabY, COLOR_BRIGHT)

    local divY = tabY + divH
    emitLabel(dividerLine("=", myW - 2 * padX), contentX, divY, COLOR_BORDER)

    if panel.activeTab == "Anim" then
        local anims = unit.listAnimations(uid) or {}
        table.sort(anims)
        if #anims == 0 then
            emitLabel("(no animations declared)", contentX,
                      divY + rowH, COLOR_DIM)
        else
            for i, name in ipairs(anims) do
                local rowY = divY + rowH * i
                local color = (name == panel.armedAnim)
                              and COLOR_ARMED or COLOR_DIM
                local id = emitLabel(name, contentX, rowY, color)
                panel.animLabels[name] = id
                table.insert(panel.clickableRects, {
                    x = contentX - 2,
                    y = rowY - s.fontSize,
                    w = myW - 2 * padX,
                    h = rowH,
                    action = function()
                        local sel = unit.getSelected()
                        local target = sel and sel[1] or nil
                        if not target then return end
                        if panel.armedAnim == name then
                            disarm()
                        else
                            arm(target, name)
                        end
                    end,
                })
            end
        end
    end
end

-----------------------------------------------------------
-- Public API
-----------------------------------------------------------

function panel.tryClaimClick(button, x, y)
    if not panel.visible then return false end
    if button ~= MOUSE_LEFT then return false end

    -- onMouseDown delivers WINDOW pixels; our rects are in
    -- FRAMEBUFFER pixels. Convert before AABB-test (same fix as
    -- `debug.lua`).
    local ww, wh = engine.getWindowSize()
    local fbW, fbH = engine.getFramebufferSize()
    local fx, fy = x, y
    if ww and wh and ww > 0 and wh > 0 then
        fx = x * (fbW / ww)
        fy = y * (fbH / wh)
    end

    for _, r in ipairs(panel.clickableRects) do
        if fx >= r.x and fx < r.x + r.w
           and fy >= r.y and fy < r.y + r.h then
            if r.action then r.action() end
            return true
        end
    end
    return false
end

local function show(uid)
    if not panel.page then
        panel.page = UI.newPage("debug_anim_panel", "overlay")
    end
    panel.visible = true
    UI.showPage(panel.page)
    buildFor(uid)
end

local function hide()
    panel.visible = false
    panel.activeUid = nil
    disarm()
    destroyLabels()
    if panel.page then UI.hidePage(panel.page) end
end

-----------------------------------------------------------
-- Engine hooks
-----------------------------------------------------------
function panel.init(scriptId)
    panel.font = engine.loadFont("assets/fonts/shell.ttf", FONT_SIZE)
    engine.logInfo("DebugAnimPanel: initialized")
end

function panel.update(dt)
    local sel = unit.getSelected() or {}
    local uid = sel[1]

    if not uid then
        if panel.visible then hide() end
        return
    end

    if uid ~= panel.activeUid then
        panel.activeUid = uid
        -- New unit selected. Drop any previous arm and rebuild.
        disarm()
        show(uid)
    end

    -- Drive the direction-cycle if armed. tickCycle is a no-op when
    -- panel.armedAnim is nil, so non-armed selection is free.
    tickCycle(uid)
end

function panel.shutdown()
    -- Clear any armed preview first so the affected unit is unfrozen and
    -- un-force-looped — otherwise unloading the panel mid-preview leaves
    -- the unit stuck (frozen + force-looped) with no way to recover.
    disarm()
    destroyPage()
    engine.logInfo("DebugAnimPanel: shut down")
end

return panel
