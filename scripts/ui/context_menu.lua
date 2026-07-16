-- Context Menu
--
-- A singleton popup menu shown at a screen position. Built to match
-- the textbox/dropdown widget look (textbox 9-patch box, highlight.png
-- on hover) so right-click menus visually belong with the settings
-- and create-world panels.
--
-- Supports one level of submenus — items with a `submenu = { … }`
-- field open a child panel to the right (or to the left if the parent
-- is too close to the screen's right edge) on hover OR click.
--
-- Usage:
--   local cm = require("scripts.ui.context_menu")
--   cm.show({
--       { label = "Equip", submenu = {
--           { label = "Right hand", callback = function() … end },
--           { label = "Left hand",  callback = function() … end },
--       }},
--       { label = "Drop", callback = function() … end },
--   }, mouseX, mouseY)
--
-- Item shape:
--   { label    = string,
--     icon     = optional textureHandle,
--     enabled  = optional bool (default true),
--     callback = function() … end,           -- ignored when submenu is set
--     submenu  = optional { item, item, … }, -- one level deep
--   }
--   or: { separator = true }

local boxTextures = require("scripts.ui.box_textures")
local scale       = require("scripts.ui.scale")

local cm = {}

-----------------------------------------------------------
-- Constants (multiplied by uiscale at draw time)
-----------------------------------------------------------

local ROW_H        = 26
local ROW_PAD_X    = 10
local ROW_GAP      = 0
local SEPARATOR_H  = 1
local SEPARATOR_PAD_Y = 4
local ICON_SIZE    = 16
local ICON_GAP     = 6
local MENU_PAD     = 6
local TILE_SIZE    = 8
local FONT_SIZE    = 14
local MIN_WIDTH    = 120
local ARROW_GAP    = 8   -- room reserved at the right edge of rows that
                         -- have a submenu (for the ▶ indicator)
local SUBMENU_GAP  = 2   -- horizontal offset between parent panel right
                         -- edge and the child panel left edge

local TEXT_COLOR_ON      = { 0.0, 0.0, 0.0, 1.0 }
local TEXT_COLOR_HOVER   = { 1.0, 1.0, 1.0, 1.0 }
local TEXT_COLOR_OFF     = { 0.4, 0.4, 0.4, 1.0 }
local HIGHLIGHT_COLOR    = { 0.3, 0.5, 0.8, 0.8 }
local SEPARATOR_COLOR    = { 0.5, 0.5, 0.5, 0.6 }
local BACKDROP_COLOR     = { 0.0, 0.0, 0.0, 0.0 }

local ITEM_CALLBACK     = "onContextMenuItemClick"
local BACKDROP_CALLBACK = "onContextMenuBackdrop"

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

cm.assetsLoaded  = false
cm.boxTexSet     = nil
cm.highlightTex  = nil
cm.whitePixelTex = nil
cm.font          = nil

-- Page + backdrop are owned by the root menu. Submenus reuse the same
-- page so a single hide() teardown covers everything.
cm.page          = nil
cm.backdropId    = nil

-- Root panel state. nil when the menu isn't open.
cm.rootPanel     = nil   -- { boxId, rows, x, y, m, items }

-- Optional submenu panel. nil when no submenu is open.
cm.subPanel       = nil  -- { boxId, rows, x, y, m, items }
cm.subParentIndex = nil  -- which root row owns the current sub

-- Hover index per panel. Both can be active simultaneously while the
-- mouse is moving from parent → sub (parent stays highlighted).
cm.rootHovered = nil
cm.subHovered  = nil

-----------------------------------------------------------
-- Initialization
-----------------------------------------------------------

function cm.init()
    if cm.assetsLoaded then return end
    cm.boxTexSet     = boxTextures.load("assets/textures/ui/textbox", "textbox")
    cm.highlightTex  = engine.loadTexture("assets/textures/ui/highlight.png")
    cm.whitePixelTex = engine.loadTexture("assets/textures/utility/white.png")
    local hud = package.loaded["scripts.hud"]
    cm.font = hud and hud.menuFont or nil
    cm.assetsLoaded = true
end

local function ensureReady()
    if not cm.assetsLoaded then cm.init() end
    if not cm.font then
        local hud = package.loaded["scripts.hud"]
        cm.font = hud and hud.menuFont or nil
    end
    return cm.font ~= nil
end

-----------------------------------------------------------
-- Geometry
-----------------------------------------------------------

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
    local arrowGap = math.floor(ARROW_GAP * uiscale)

    local maxTextW = 0
    local anyIcon  = false
    local anySub   = false
    for _, it in ipairs(items) do
        if not it.separator then
            if it.icon then anyIcon = true end
            if it.submenu then anySub = true end
            local tw = engine.getTextWidth(cm.font, it.label or "", fs)
            if tw > maxTextW then maxTextW = tw end
        end
    end
    -- Rows with submenus reserve an arrow column at the right edge so
    -- the arrow doesn't collide with the label. The whole panel uses
    -- the wider sizing whenever ANY row has a submenu (otherwise the
    -- column width would visibly shift between hovers).
    local arrowCol = anySub and arrowGap or 0
    local rowContentW = maxTextW
                      + (anyIcon and (iconSz + iconGap) or 0)
                      + arrowCol
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
    local totalH = cursorY - rowGap + menuPad

    return {
        totalW = totalW, totalH = totalH, innerW = innerW,
        rowYs  = rowYs,
        rowH   = rowH, sepH = sepH, sepPad = sepPad,
        padX   = padX, iconSz = iconSz, iconGap = iconGap,
        fs     = fs,   menuPad = menuPad,
        anyIcon = anyIcon, anySub = anySub,
        arrowGap = arrowGap,
    }
end

-----------------------------------------------------------
-- Panel builder — used for both root and submenu
-----------------------------------------------------------

-- Position the panel at (anchorX, anchorY), clamped to the screen via
-- the #747 shared placement contract ("anchored" — no directional
-- preference, place exactly at the point, then clamp both axes).
-- `namePrefix` distinguishes element names so root and sub don't
-- collide (sprite names need to be unique within a page).
local function buildPanel(items, anchorX, anchorY, namePrefix)
    local uiscale = scale.get()
    local m = measure(items, uiscale)

    local x, y = UI.placePopup(anchorX, anchorY, 0, 0, m.totalW, m.totalH, "anchored")

    local boxId = UI.newBox(
        namePrefix .. "_box", m.totalW, m.totalH,
        cm.boxTexSet, TILE_SIZE,
        1.0, 1.0, 1.0, 1.0, 0,
        cm.page)
    UI.addToPage(cm.page, boxId, x, y)
    UI.setZIndex(boxId, 11)

    local rows = {}
    for i, it in ipairs(items) do
        local rowY = y + m.rowYs[i]
        if it.separator then
            local sepId = UI.newSprite(
                namePrefix .. "_sep_" .. i,
                m.innerW, m.sepH,
                cm.whitePixelTex,
                SEPARATOR_COLOR[1], SEPARATOR_COLOR[2],
                SEPARATOR_COLOR[3], SEPARATOR_COLOR[4],
                cm.page)
            UI.addToPage(cm.page, sepId, x + m.menuPad, rowY + m.sepPad)
            UI.setZIndex(sepId, 12)
            rows[i] = { kind = "sep", spriteId = sepId }
        else
            local enabled = (it.enabled ~= false)
            local hasSub  = (it.submenu ~= nil and #it.submenu > 0)

            local hlId = UI.newSprite(
                namePrefix .. "_hl_" .. i,
                m.innerW, m.rowH,
                cm.highlightTex,
                HIGHLIGHT_COLOR[1], HIGHLIGHT_COLOR[2],
                HIGHLIGHT_COLOR[3], HIGHLIGHT_COLOR[4],
                cm.page)
            UI.addToPage(cm.page, hlId, x + m.menuPad, rowY)
            UI.setZIndex(hlId, 12)
            UI.setVisible(hlId, false)

            local iconId = nil
            if it.icon then
                iconId = UI.newSprite(
                    namePrefix .. "_icon_" .. i,
                    m.iconSz, m.iconSz,
                    it.icon,
                    1.0, 1.0, 1.0, enabled and 1.0 or 0.5,
                    cm.page)
                UI.addToPage(cm.page, iconId,
                    x + m.menuPad + m.padX,
                    rowY + math.floor((m.rowH - m.iconSz) / 2))
                UI.setZIndex(iconId, 13)
            end

            local labelX = x + m.menuPad + m.padX
                         + (m.anyIcon and (m.iconSz + m.iconGap) or 0)
            local labelY = rowY + math.floor(m.rowH / 2)
                         + math.floor(m.fs * 0.3) + 2
            local lblId = UI.newText(
                namePrefix .. "_lbl_" .. i,
                it.label or "",
                cm.font, m.fs,
                enabled and TEXT_COLOR_ON[1] or TEXT_COLOR_OFF[1],
                enabled and TEXT_COLOR_ON[2] or TEXT_COLOR_OFF[2],
                enabled and TEXT_COLOR_ON[3] or TEXT_COLOR_OFF[3],
                enabled and TEXT_COLOR_ON[4] or TEXT_COLOR_OFF[4],
                cm.page)
            UI.addToPage(cm.page, lblId, labelX, labelY)
            UI.setZIndex(lblId, 13)

            -- Submenu indicator arrow. Right-aligned inside the row.
            local arrowId = nil
            if hasSub then
                arrowId = UI.newText(
                    namePrefix .. "_arrow_" .. i,
                    ">",
                    cm.font, m.fs,
                    enabled and TEXT_COLOR_ON[1] or TEXT_COLOR_OFF[1],
                    enabled and TEXT_COLOR_ON[2] or TEXT_COLOR_OFF[2],
                    enabled and TEXT_COLOR_ON[3] or TEXT_COLOR_OFF[3],
                    enabled and TEXT_COLOR_ON[4] or TEXT_COLOR_OFF[4],
                    cm.page)
                UI.addToPage(cm.page, arrowId,
                    x + m.totalW - m.menuPad - m.padX
                      - math.floor(m.arrowGap * 0.6),
                    labelY)
                UI.setZIndex(arrowId, 13)
            end

            local bgId = UI.newSprite(
                namePrefix .. "_row_" .. i,
                m.innerW, m.rowH,
                cm.whitePixelTex,
                1.0, 1.0, 1.0, 0.0,
                cm.page)
            UI.addToPage(cm.page, bgId, x + m.menuPad, rowY)
            UI.setZIndex(bgId, 14)
            UI.setClickable(bgId, true)
            UI.setOnClick(bgId, ITEM_CALLBACK)

            rows[i] = {
                kind     = "row",
                bgId     = bgId,
                hlId     = hlId,
                iconId   = iconId,
                lblId    = lblId,
                arrowId  = arrowId,
                callback = it.callback,
                enabled  = enabled,
                hasSub   = hasSub,
                submenu  = it.submenu,
            }
        end
    end

    return {
        boxId = boxId,
        rows  = rows,
        x     = x,
        y     = y,
        m     = m,
        items = items,
    }
end

-- Tear down a panel (boxId + per-row elements). Page deletion would
-- handle this too, but we need finer control to dispose of just the
-- submenu without taking the root down.
local function destroyPanel(panel)
    if not panel then return end
    for _, r in ipairs(panel.rows) do
        if r.kind == "sep" then
            UI.deleteElement(r.spriteId)
        else
            UI.deleteElement(r.bgId)
            UI.deleteElement(r.hlId)
            if r.iconId  then UI.deleteElement(r.iconId)  end
            if r.lblId   then UI.deleteElement(r.lblId)   end
            if r.arrowId then UI.deleteElement(r.arrowId) end
        end
    end
    UI.deleteElement(panel.boxId)
end

-----------------------------------------------------------
-- Public API
-----------------------------------------------------------

function cm.isOpen()
    return cm.page ~= nil
end

function cm.hide()
    if not cm.page then return end
    UI.deletePage(cm.page)
    cm.page           = nil
    cm.backdropId     = nil
    cm.rootPanel      = nil
    cm.subPanel       = nil
    cm.subParentIndex = nil
    cm.rootHovered    = nil
    cm.subHovered     = nil
end

function cm.show(items, x, y)
    if not ensureReady() then
        engine.logWarn("contextMenu: HUD font not ready, skipping show()")
        return
    end
    if not items or #items == 0 then return end

    if cm.page then cm.hide() end

    cm.page = UI.newPage("context_menu", "modal")

    local fbW, fbH = engine.getFramebufferSize()
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
    UI.setOnRightClick(cm.backdropId, BACKDROP_CALLBACK)

    cm.rootPanel = buildPanel(items, x, y, "context_menu_root")

    UI.showPage(cm.page)
end

-----------------------------------------------------------
-- Submenu management
-----------------------------------------------------------

local function closeSubMenu()
    if not cm.subPanel then return end
    destroyPanel(cm.subPanel)
    cm.subPanel       = nil
    cm.subParentIndex = nil
    cm.subHovered     = nil
end

-- Open a submenu adjacent to the parent row at `parentIndex` in the
-- root panel. #747: uses the shared placement contract — prefers the
-- right of the root panel, flips left when that would clip the
-- screen, and clamps vertically only (the row-aligned Y is never
-- flipped, since the preferred axis here is horizontal).
local function openSubMenu(parentIndex)
    if not cm.rootPanel then return end
    local r = cm.rootPanel.rows[parentIndex]
    if not r or r.kind ~= "row" or not r.hasSub then return end
    if cm.subParentIndex == parentIndex then return end  -- already open

    closeSubMenu()

    local rootM = cm.rootPanel.m
    local rootRowYTop = cm.rootPanel.y + rootM.rowYs[parentIndex]
    local subY = rootRowYTop - rootM.menuPad  -- align sub's top with parent row

    local items = r.submenu
    local uiscale = scale.get()
    local subM = measure(items, uiscale)

    -- The anchor is the root panel's rect widened by SUBMENU_GAP on
    -- both sides, so the generic right/left fallback reproduces the
    -- exact gap on whichever side is actually used (right = anchor's
    -- right edge, left fallback = anchor's left edge minus content).
    local subX, subYPlaced = UI.placePopup(
        cm.rootPanel.x - SUBMENU_GAP, subY,
        rootM.totalW + 2 * SUBMENU_GAP, 0,
        subM.totalW, subM.totalH,
        "right")

    cm.subPanel = buildPanel(items, subX, subYPlaced, "context_menu_sub")
    cm.subParentIndex = parentIndex
end

-----------------------------------------------------------
-- Hover handling
-----------------------------------------------------------

local function setRowHovered(panel, i, hovered)
    if not panel then return end
    local r = panel.rows[i]
    if not r or r.kind ~= "row" then return end
    UI.setVisible(r.hlId, hovered and r.enabled)
    local c = (hovered and r.enabled) and TEXT_COLOR_HOVER
              or (r.enabled and TEXT_COLOR_ON or TEXT_COLOR_OFF)
    UI.setColor(r.lblId, c[1], c[2], c[3], c[4])
    if r.arrowId then
        UI.setColor(r.arrowId, c[1], c[2], c[3], c[4])
    end
end

-- Look up an elemHandle in either panel. Returns (panel, index) or nil.
local function findPanelRow(elemHandle)
    if cm.rootPanel then
        for i, r in ipairs(cm.rootPanel.rows) do
            if r.bgId == elemHandle then
                return "root", cm.rootPanel, i
            end
        end
    end
    if cm.subPanel then
        for i, r in ipairs(cm.subPanel.rows) do
            if r.bgId == elemHandle then
                return "sub", cm.subPanel, i
            end
        end
    end
    return nil
end

function cm.onHoverEnter(elemHandle)
    if not cm.page then return end
    local which, panel, i = findPanelRow(elemHandle)
    if not which then return end

    if which == "root" then
        -- Unhighlight any previous root row UNLESS the previous row
        -- owns the currently-open submenu (we want the parent of an
        -- open sub to keep its highlight while the user moves into the
        -- sub).
        if cm.rootHovered and cm.rootHovered ~= i
           and cm.rootHovered ~= cm.subParentIndex then
            setRowHovered(cm.rootPanel, cm.rootHovered, false)
        end
        cm.rootHovered = i
        setRowHovered(cm.rootPanel, i, true)
        local row = panel.rows[i]
        if row.hasSub and row.enabled then
            openSubMenu(i)
        else
            closeSubMenu()
        end
    else
        -- Hovering inside the sub. Keep the sub open. Highlight the
        -- sub row.
        if cm.subHovered and cm.subHovered ~= i then
            setRowHovered(cm.subPanel, cm.subHovered, false)
        end
        cm.subHovered = i
        setRowHovered(cm.subPanel, i, true)
    end
end

function cm.onHoverLeave(elemHandle)
    if not cm.page then return end
    local which, panel, i = findPanelRow(elemHandle)
    if not which then return end

    if which == "root" then
        -- Don't unhighlight the parent of an open submenu — the user
        -- is likely moving toward the sub. The sub-or-elsewhere hover
        -- will reset the visual state when it lands.
        if i == cm.subParentIndex then return end
        setRowHovered(cm.rootPanel, i, false)
        if cm.rootHovered == i then cm.rootHovered = nil end
    else
        setRowHovered(cm.subPanel, i, false)
        if cm.subHovered == i then cm.subHovered = nil end
    end
end

function cm.isContextMenuCallback(name)
    return name == ITEM_CALLBACK or name == BACKDROP_CALLBACK
end

-----------------------------------------------------------
-- Click + ESC handling
-----------------------------------------------------------

function cm.handleItemClick(elemHandle)
    if not cm.page then return false end
    local which, panel, i = findPanelRow(elemHandle)
    if not which then return false end
    local row = panel.rows[i]
    if not row.enabled then return true end

    if row.hasSub then
        -- Clicking a parent that owns a submenu opens (or refocuses)
        -- the sub, but doesn't fire a callback — submenus are for
        -- picking among options, not for executing the parent's
        -- intent directly.
        openSubMenu(i)
        return true
    end

    local cb = row.callback
    cm.hide()
    if cb then cb() end
    return true
end

function cm.handleBackdropClick(elemHandle)
    if not cm.page then return false end
    if elemHandle == cm.backdropId then
        cm.hide()
        return true
    end
    return false
end

function cm.handleEscape()
    if not cm.page then return false end
    -- If a submenu is open, ESC closes only the sub (leaves root open
    -- so the user can pick a different item). Second ESC closes the
    -- root.
    if cm.subPanel then
        closeSubMenu()
        return true
    end
    cm.hide()
    return true
end

return cm
