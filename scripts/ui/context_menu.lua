-- Context Menu
--
-- A singleton popup menu shown at a screen position. Built to match
-- the textbox/dropdown widget look (textbox 9-patch box, highlight.png
-- on hover) so right-click menus visually belong with the settings
-- and create-world panels.
--
-- Usage:
--   local cm = require("scripts.ui.context_menu")
--   cm.show({
--       { label = "Equip",  callback = function() equipItem() end },
--       { separator = true },
--       { label = "Drop",   callback = function() dropItem() end },
--   }, mouseX, mouseY)
--
-- Item shape:
--   { label    = string,
--     icon     = optional textureHandle,
--     enabled  = optional bool (default true),
--     callback = function() … end,
--     submenu  = optional { item, item, … },  -- v2, not yet implemented
--   }
--   or: { separator = true }
--
-- Lifecycle:
--   show() builds the page + backdrop + row elements at the click site.
--   hide() tears everything down.
--   ESC, click outside the menu, or clicking a leaf item all close it.

local boxTextures = require("scripts.ui.box_textures")
local scale       = require("scripts.ui.scale")

local cm = {}

-----------------------------------------------------------
-- Constants (multiplied by uiscale at draw time)
-----------------------------------------------------------

local ROW_H        = 26
local ROW_PAD_X    = 10   -- horizontal pad inside a row around label/icon
local ROW_GAP      = 0    -- gap between adjacent rows (0 = flush)
local SEPARATOR_H  = 1
local SEPARATOR_PAD_Y = 4 -- vertical space above + below a separator
local ICON_SIZE    = 16
local ICON_GAP     = 6    -- gap between icon and label
local MENU_PAD     = 6    -- inner pad inside the 9-patch box
local TILE_SIZE    = 8    -- 9-patch corner size for the textbox tex set
local FONT_SIZE    = 14
local MIN_WIDTH    = 120

local TEXT_COLOR_ON      = { 0.0, 0.0, 0.0, 1.0 }  -- enabled, not hovered
local TEXT_COLOR_HOVER   = { 1.0, 1.0, 1.0, 1.0 }  -- enabled, hovered
local TEXT_COLOR_OFF     = { 0.4, 0.4, 0.4, 1.0 }  -- disabled
local HIGHLIGHT_COLOR    = { 0.3, 0.5, 0.8, 0.8 }  -- matches dropdown
local SEPARATOR_COLOR    = { 0.5, 0.5, 0.5, 0.6 }
local BACKDROP_COLOR     = { 0.0, 0.0, 0.0, 0.0 }  -- fully transparent

local ITEM_CALLBACK     = "onContextMenuItemClick"
local BACKDROP_CALLBACK = "onContextMenuBackdrop"

-----------------------------------------------------------
-- Module state (singleton — only one menu open at a time)
-----------------------------------------------------------

cm.assetsLoaded     = false
cm.boxTexSet        = nil
cm.highlightTex     = nil
cm.whitePixelTex    = nil   -- for separators + backdrop
cm.font             = nil

cm.page             = nil   -- UI page handle while open; nil when closed
cm.backdropId       = nil
cm.boxId            = nil
cm.rows             = {}    -- per-row: {kind="row"/"sep", bgId, hlId, iconId, lblId, callback, enabled}
cm.hoveredIndex     = nil

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function cm.init()
    if cm.assetsLoaded then return end
    cm.boxTexSet     = boxTextures.load("assets/textures/ui/textbox", "textbox")
    cm.highlightTex  = engine.loadTexture("assets/textures/ui/highlight.png")
    cm.whitePixelTex = engine.loadTexture("assets/textures/hud/utility/white.png")
    -- Use the menu font already loaded by the HUD (avoids redundant loads).
    -- Caller is expected to have hud bootstrapped before opening menus.
    local hud = package.loaded["scripts.hud"]
    cm.font = hud and hud.menuFont or nil
    cm.assetsLoaded = true
end

-- A separate ensureReady() check defends against opening a menu before
-- the HUD has loaded its font — rare in practice but cheap to guard.
local function ensureReady()
    if not cm.assetsLoaded then cm.init() end
    if not cm.font then
        local hud = package.loaded["scripts.hud"]
        cm.font = hud and hud.menuFont or nil
    end
    return cm.font ~= nil
end

-----------------------------------------------------------
-- Geometry — measure rows once so the menu box can size itself
-----------------------------------------------------------

-- Returns { totalW, totalH, rowYs } describing how the menu lays out.
-- rowYs[i] is the top-Y of row i relative to the menu's inner area.
local function measure(items, uiscale)
    local rowH    = math.floor(ROW_H * uiscale)
    local sepH    = math.floor(SEPARATOR_H * uiscale)
    local sepPad  = math.floor(SEPARATOR_PAD_Y * uiscale)
    local rowGap  = math.floor(ROW_GAP * uiscale)
    local padX    = math.floor(ROW_PAD_X * uiscale)
    local iconSz  = math.floor(ICON_SIZE * uiscale)
    local iconGap = math.floor(ICON_GAP * uiscale)
    local fs      = math.floor(FONT_SIZE * uiscale)
    local menuPad = math.floor(MENU_PAD * uiscale)

    -- Widest row decides menu width. Width = padX + (icon + iconGap)? + text + padX.
    local maxTextW = 0
    local anyIcon  = false
    for _, it in ipairs(items) do
        if not it.separator then
            if it.icon then anyIcon = true end
            local tw = engine.getTextWidth(cm.font,
                                           it.label or "", fs)
            if tw > maxTextW then maxTextW = tw end
        end
    end
    local rowContentW = maxTextW + (anyIcon and (iconSz + iconGap) or 0)
    local innerW = math.max(rowContentW + 2 * padX,
                            math.floor(MIN_WIDTH * uiscale))
    local totalW = innerW + 2 * menuPad

    local cursorY = menuPad
    local rowYs = {}
    for i, it in ipairs(items) do
        rowYs[i] = cursorY
        if it.separator then
            cursorY = cursorY + sepPad + sepH + sepPad
        else
            cursorY = cursorY + rowH + rowGap
        end
    end
    local totalH = cursorY - rowGap + menuPad  -- trim trailing gap

    return {
        totalW   = totalW,
        totalH   = totalH,
        innerW   = innerW,
        rowYs    = rowYs,
        rowH     = rowH,
        sepH     = sepH,
        sepPad   = sepPad,
        padX     = padX,
        iconSz   = iconSz,
        iconGap  = iconGap,
        fs       = fs,
        menuPad  = menuPad,
        anyIcon  = anyIcon,
    }
end

-----------------------------------------------------------
-- Public API
-----------------------------------------------------------

function cm.isOpen()
    return cm.page ~= nil
end

function cm.hide()
    if not cm.page then return end
    -- Page deletion cleans up every element / sprite registered to it.
    UI.deletePage(cm.page)
    cm.page         = nil
    cm.backdropId   = nil
    cm.boxId        = nil
    cm.rows         = {}
    cm.hoveredIndex = nil
end

function cm.show(items, x, y)
    if not ensureReady() then
        engine.logWarn("contextMenu: HUD font not ready, skipping show()")
        return
    end
    if not items or #items == 0 then return end

    -- Close any previously open menu before opening a new one. This is
    -- also the path for "user right-clicked again on something else" —
    -- the old menu vanishes and the new one takes its place.
    if cm.page then cm.hide() end

    local uiscale = scale.get()
    local m = measure(items, uiscale)

    -- Clamp the menu so it stays fully on-screen — shift left / up if
    -- the click site would push the bottom-right corner off-edge.
    local fbW, fbH = engine.getFramebufferSize()
    local mx = math.max(0, math.min(x, fbW - m.totalW))
    local my = math.max(0, math.min(y, fbH - m.totalH))

    -- Dedicated page so hide() can wipe everything in a single call,
    -- and so the menu can sit above other UI without juggling z's.
    -- "modal" layer keeps it above HUD/menu pages (info panel, etc.)
    -- while staying below tooltips. UI.newPage does NOT auto-show — we
    -- need an explicit UI.showPage after we've built our elements.
    cm.page = UI.newPage("context_menu", "modal")

    -- Full-screen backdrop catches clicks outside the menu. Transparent
    -- so it doesn't visually intrude; clickable so the click is eaten
    -- by the UI hit-test (and never reaches game.onMouseDown).
    cm.backdropId = UI.newSprite(
        "context_menu_backdrop", fbW, fbH,
        cm.whitePixelTex,
        BACKDROP_COLOR[1], BACKDROP_COLOR[2],
        BACKDROP_COLOR[3], BACKDROP_COLOR[4],
        cm.page)
    UI.addToPage(cm.page, cm.backdropId, 0, 0)
    UI.setZIndex(cm.backdropId, 10)
    UI.setClickable(cm.backdropId, true)
    UI.setOnClick(cm.backdropId, BACKDROP_CALLBACK)
    -- Right-clicks on the backdrop close too (matches "any click
    -- outside closes" UX).
    UI.setOnRightClick(cm.backdropId, BACKDROP_CALLBACK)

    -- 9-patch box behind all rows.
    cm.boxId = UI.newBox(
        "context_menu_box", m.totalW, m.totalH,
        cm.boxTexSet, TILE_SIZE,
        1.0, 1.0, 1.0, 1.0, 0,
        cm.page)
    UI.addToPage(cm.page, cm.boxId, mx, my)
    UI.setZIndex(cm.boxId, 11)

    -- Build rows
    cm.rows = {}
    for i, it in ipairs(items) do
        local rowY = my + m.rowYs[i]
        if it.separator then
            local sepId = UI.newSprite(
                "context_menu_sep_" .. i,
                m.innerW, m.sepH,
                cm.whitePixelTex,
                SEPARATOR_COLOR[1], SEPARATOR_COLOR[2],
                SEPARATOR_COLOR[3], SEPARATOR_COLOR[4],
                cm.page)
            UI.addToPage(cm.page, sepId,
                mx + m.menuPad, rowY + m.sepPad)
            UI.setZIndex(sepId, 12)
            cm.rows[i] = { kind = "sep", spriteId = sepId }
        else
            local enabled = (it.enabled ~= false)

            -- Hover highlight (hidden until enter)
            local hlId = UI.newSprite(
                "context_menu_hl_" .. i,
                m.innerW, m.rowH,
                cm.highlightTex,
                HIGHLIGHT_COLOR[1], HIGHLIGHT_COLOR[2],
                HIGHLIGHT_COLOR[3], HIGHLIGHT_COLOR[4],
                cm.page)
            UI.addToPage(cm.page, hlId, mx + m.menuPad, rowY)
            UI.setZIndex(hlId, 12)
            UI.setVisible(hlId, false)

            -- Optional icon
            local iconId = nil
            if it.icon then
                iconId = UI.newSprite(
                    "context_menu_icon_" .. i,
                    m.iconSz, m.iconSz,
                    it.icon,
                    1.0, 1.0, 1.0, enabled and 1.0 or 0.5,
                    cm.page)
                UI.addToPage(cm.page, iconId,
                    mx + m.menuPad + m.padX,
                    rowY + math.floor((m.rowH - m.iconSz) / 2))
                UI.setZIndex(iconId, 13)
            end

            -- Label
            local labelX = mx + m.menuPad + m.padX
                         + (m.anyIcon and (m.iconSz + m.iconGap) or 0)
            local labelY = rowY + math.floor(m.rowH / 2)
                         + math.floor(m.fs * 0.3) + 2
            local lblId = UI.newText(
                "context_menu_lbl_" .. i,
                it.label or "",
                cm.font, m.fs,
                enabled and TEXT_COLOR_ON[1] or TEXT_COLOR_OFF[1],
                enabled and TEXT_COLOR_ON[2] or TEXT_COLOR_OFF[2],
                enabled and TEXT_COLOR_ON[3] or TEXT_COLOR_OFF[3],
                enabled and TEXT_COLOR_ON[4] or TEXT_COLOR_OFF[4],
                cm.page)
            UI.addToPage(cm.page, lblId, labelX, labelY)
            UI.setZIndex(lblId, 13)

            -- Hit-zone box on top — receives clicks + hover. Disabled
            -- rows still get the click handler so the menu doesn't
            -- accidentally close from a "missed click" through them.
            local bgId = UI.newSprite(
                "context_menu_row_" .. i,
                m.innerW, m.rowH,
                cm.whitePixelTex,
                1.0, 1.0, 1.0, 0.0,
                cm.page)
            UI.addToPage(cm.page, bgId, mx + m.menuPad, rowY)
            UI.setZIndex(bgId, 14)
            UI.setClickable(bgId, true)
            UI.setOnClick(bgId, ITEM_CALLBACK)

            cm.rows[i] = {
                kind     = "row",
                bgId     = bgId,
                hlId     = hlId,
                iconId   = iconId,
                lblId    = lblId,
                callback = it.callback,
                enabled  = enabled,
            }
        end
    end

    -- Nothing renders until the page is explicitly shown.
    UI.showPage(cm.page)
end

-----------------------------------------------------------
-- Hover handling — toggle highlight + text color on enter / leave
-----------------------------------------------------------

local function setRowHovered(i, hovered)
    local r = cm.rows[i]
    if not r or r.kind ~= "row" then return end
    UI.setVisible(r.hlId, hovered and r.enabled)
    local c = (hovered and r.enabled) and TEXT_COLOR_HOVER
              or (r.enabled and TEXT_COLOR_ON or TEXT_COLOR_OFF)
    UI.setColor(r.lblId, c[1], c[2], c[3], c[4])
end

function cm.onHoverEnter(elemHandle)
    if not cm.page then return end
    for i, r in ipairs(cm.rows) do
        if r.bgId == elemHandle then
            if cm.hoveredIndex and cm.hoveredIndex ~= i then
                setRowHovered(cm.hoveredIndex, false)
            end
            cm.hoveredIndex = i
            setRowHovered(i, true)
            return
        end
    end
end

function cm.onHoverLeave(elemHandle)
    if not cm.page then return end
    for i, r in ipairs(cm.rows) do
        if r.bgId == elemHandle and cm.hoveredIndex == i then
            setRowHovered(i, false)
            cm.hoveredIndex = nil
            return
        end
    end
end

-- Routing hook for ui_manager — answers "is this callback name one of
-- ours?" so the dispatcher knows to forward hover events to us.
function cm.isContextMenuCallback(name)
    return name == ITEM_CALLBACK or name == BACKDROP_CALLBACK
end

-----------------------------------------------------------
-- Click + ESC handling
-----------------------------------------------------------

-- ui_manager routes "onContextMenuItemClick" here.
function cm.handleItemClick(elemHandle)
    if not cm.page then return false end
    for _, r in ipairs(cm.rows) do
        if r.kind == "row" and r.bgId == elemHandle then
            if not r.enabled then
                -- Disabled rows swallow the click but don't close the
                -- menu — lets the player retry without re-opening.
                return true
            end
            local cb = r.callback
            cm.hide()
            -- Callback fires AFTER hide() so the callback can re-open
            -- a different menu without colliding with the current one.
            if cb then cb() end
            return true
        end
    end
    return false
end

-- ui_manager routes "onContextMenuBackdrop" here.
function cm.handleBackdropClick(elemHandle)
    if not cm.page then return false end
    if elemHandle == cm.backdropId then
        cm.hide()
        return true
    end
    return false
end

-- ESC closes the menu. Wire from init.lua's game.onKeyDown.
function cm.handleEscape()
    if not cm.page then return false end
    cm.hide()
    return true
end

return cm
