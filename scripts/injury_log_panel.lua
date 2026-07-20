-- Injury Log Panel
--
-- Sibling to combat_log.lua / event_log.lua. Owned by the HUD's log-mode
-- picker (a third mode alongside Combat and Event). Shows an "All" tab
-- (RAM-only FIFO of the latest events) plus one tab PER INJURED UNIT —
-- the analogue of the combat log's per-battle tabs, but grouped by the
-- victim. The data source is the NON-combat injury stream
-- (injury.drainEvents) — falls, hazards, and wound-caused deaths.
-- Combat wounds stay in the combat log by design.
--
-- This is a clone/adaptation of combat_log.lua: same chrome, tab strip,
-- scrollbar, and wrapped rich-text rendering. Differences are the event
-- source, the per-VICTIM grouping, and the prose (injury_log.eventLine).

local scale       = require("scripts.ui.scale")
local panel       = require("scripts.ui.panel")
local label       = require("scripts.ui.label")
local button      = require("scripts.ui.button")
local boxTextures = require("scripts.ui.box_textures")
local scrollbar   = require("scripts.ui.scrollbar")

-- Singleton (matches combat_log so engine.loadScript and require both
-- reach the same table; see gotcha_dofile_module_state).
local injuryLog = package.loaded["scripts.injury_log_panel"] or {}
package.loaded["scripts.injury_log_panel"] = injuryLog

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

injuryLog.pageId       = injuryLog.pageId       or nil
injuryLog.boxTexSet    = injuryLog.boxTexSet    or nil
injuryLog.btnTexSet    = injuryLog.btnTexSet    or nil
injuryLog.tabSelTexSet   = injuryLog.tabSelTexSet   or nil
injuryLog.tabUnselTexSet = injuryLog.tabUnselTexSet or nil
injuryLog.font         = injuryLog.font         or nil
injuryLog.fbW          = injuryLog.fbW          or 0
injuryLog.fbH          = injuryLog.fbH          or 0
injuryLog.bootstrapped = injuryLog.bootstrapped or false
injuryLog.visible      = injuryLog.visible      or false
injuryLog.uiCreated    = injuryLog.uiCreated    or false

injuryLog.chromeLabels  = injuryLog.chromeLabels  or {}
injuryLog.chromePanels  = injuryLog.chromePanels  or {}
injuryLog.chromeButtons = injuryLog.chromeButtons or {}

injuryLog.tabLabels  = injuryLog.tabLabels  or {}
injuryLog.tabButtons = injuryLog.tabButtons or {}

-- Tab-click overlay boxes: elemHandle → logId ("all" or numeric).
injuryLog.tabClickBoxes = injuryLog.tabClickBoxes or {}

local ALL_RING_CAP = 200
-- Recency: a unit-log with no new injury for this long fades red→grey.
local LOG_QUIESCENCE_SEC = 30
-- "Closed for good": a unit-log silent longer than this won't absorb a
-- new event, so a reused unit id can't get glued onto a dead unit's log
-- (the engine reassigns ids on destroy/spawn — same guard as combat_log).
local LOG_REJOIN_MAX_SEC = 120
injuryLog.allEvents = injuryLog.allEvents or {}   -- {ev, ev, ...}
-- Each unit-log: { id, uid, name, active, events={ev,...}, lastEventAt }.
injuryLog.unitLogs   = injuryLog.unitLogs   or {}
injuryLog.nextLogId  = injuryLog.nextLogId  or 1
injuryLog.activeTabId  = injuryLog.activeTabId or "all"
injuryLog.scrollOffset = injuryLog.scrollOffset or 0
injuryLog.tabMaxScroll = injuryLog.tabMaxScroll or 0
injuryLog.contentScroll    = injuryLog.contentScroll    or 0
injuryLog.justifyBottom    = true
injuryLog.syncingScrollbar = false
injuryLog.scrollbarId      = injuryLog.scrollbarId      or nil
injuryLog.dirty        = injuryLog.dirty        or false

injuryLog.layout = injuryLog.layout or {}

-- Click-callback names routed by ui_manager.
local INJURY_LOG_TAB_CALLBACK  = "onInjuryLogTabClick"
local INJURY_LOG_PREV_CALLBACK = "onInjuryLogScrollPrev"
local INJURY_LOG_NEXT_CALLBACK = "onInjuryLogScrollNext"

-----------------------------------------------------------
-- Sizing constants (base, before uiscale)
-----------------------------------------------------------

injuryLog.baseSizes = {
    fontSize     = 18,
    titleSize    = 28,
    titleBarH    = 48,
    closeBtnSize = 36,
    padX         = 24,
    padY         = 20,
    rowHeight    = 22,
    tabWidth     = 80,
    tabPad       = 10,
    tabHeight    = 36,
    tabFontSize  = 14,
    tabTileSize  = 16,
    tabGap       = 4,
    arrowWidth   = 28,
    arrowHeight  = 36,
    tabNameMax   = 12,
    tileSize     = 64,
    panelFrac    = 0.85,
}

-- Recently-injured units get a saturated-red tab; quiescent ones the
-- normal greyish color. The All tab is always white.
local COLOR_ACTIVE = { 1.0, 0.30, 0.30, 1.0 }
local COLOR_CLOSED = { 0.85, 0.85, 0.85, 1.0 }
local COLOR_ALL    = { 1.0, 1.0, 1.0, 1.0 }
local COLOR_DIM    = { 0.45, 0.45, 0.45, 1.0 }  -- greyed scroll arrow

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------

local function destroyChrome()
    for _, id in ipairs(injuryLog.chromeLabels)  do label.destroy(id)  end
    for _, id in ipairs(injuryLog.chromeButtons) do button.destroy(id) end
    for _, id in ipairs(injuryLog.chromePanels)  do panel.destroy(id)  end
    if injuryLog.scrollbarId then
        scrollbar.destroy(injuryLog.scrollbarId)
        injuryLog.scrollbarId = nil
    end
    -- #750: the content clip viewport — destroyTransient (called first
    -- by every real teardown path) already destroyed its children;
    -- UI.deleteElement is idempotent either way.
    if injuryLog.contentViewportId then
        UI.deleteElement(injuryLog.contentViewportId)
        injuryLog.contentViewportId = nil
    end
    injuryLog.chromeLabels  = {}
    injuryLog.chromeButtons = {}
    injuryLog.chromePanels  = {}
end

local function destroyTransient()
    for _, id in ipairs(injuryLog.tabLabels)  do label.destroy(id)  end
    for _, id in ipairs(injuryLog.tabButtons) do button.destroy(id) end
    for handle, _ in pairs(injuryLog.tabClickBoxes) do
        UI.deleteElement(handle)
    end
    injuryLog.tabLabels     = {}
    injuryLog.tabButtons    = {}
    injuryLog.tabClickBoxes = {}
end

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

local function truncateTabName(text)
    text = text or ""
    local maxLen = injuryLog.baseSizes.tabNameMax
    if #text <= maxLen then return text end
    return string.sub(text, 1, maxLen - 3) .. "..."
end

local function maxScrollOffset()
    return injuryLog.tabMaxScroll or 0
end

-----------------------------------------------------------
-- Build the panel chrome (panel + title + close X)
-----------------------------------------------------------

local renderContent
local createUI

createUI = function()
    destroyChrome()
    destroyTransient()

    local uiscale = scale.get()
    local s = scale.applyAllWith(injuryLog.baseSizes, uiscale)

    local panelW = math.floor(injuryLog.fbW * injuryLog.baseSizes.panelFrac)
    local panelH = math.floor(injuryLog.fbH * injuryLog.baseSizes.panelFrac)
    local panelX = math.floor((injuryLog.fbW - panelW) / 2)
    local panelY = math.floor((injuryLog.fbH - panelH) / 2)

    local panelId = panel.new({
        name       = "injury_log_panel",
        page       = injuryLog.pageId,
        x          = panelX,
        y          = panelY,
        width      = panelW,
        height     = panelH,
        textureSet = injuryLog.boxTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = injuryLog.baseSizes.tileSize,
        zIndex     = 500,
        padding    = { top = s.padY, bottom = s.padY,
                       left = s.padX, right = s.padX },
        uiscale    = 1.0,
    })
    table.insert(injuryLog.chromePanels, panelId)
    -- #743: explicit scroll-capture + pointer-block, replacing the old
    -- setClickable(true)-with-no-callback attempt (which never actually
    -- worked — wheel routing required a click callback too) so a mouse
    -- wheel over the content routes to injuryLog.onScroll, and blank
    -- panel space doesn't leak clicks through to gameplay underneath.
    pcall(function()
        local boxHandle = panel.getBoxHandle(panelId)
        UI.setScrollCapture(boxHandle, true)
        UI.setPointerBlocking(boxHandle, true)
    end)

    local titleId = label.new({
        name     = "injury_log_title",
        text     = "Injury Log",
        font     = injuryLog.font,
        fontSize = injuryLog.baseSizes.titleSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = injuryLog.pageId,
        uiscale  = uiscale,
    })
    table.insert(injuryLog.chromeLabels, titleId)
    UI.addToPage(injuryLog.pageId, label.getElementHandle(titleId),
        panelX + s.padX, panelY + s.padY + s.titleSize)
    UI.setZIndex(label.getElementHandle(titleId), 502)

    local closeBtnId = button.new({
        name       = "injury_log_close",
        page       = injuryLog.pageId,
        x          = panelX + panelW - s.padX - s.closeBtnSize,
        y          = panelY + s.padY,
        width      = injuryLog.baseSizes.closeBtnSize,
        height     = injuryLog.baseSizes.closeBtnSize,
        fontSize   = injuryLog.baseSizes.fontSize,
        font       = injuryLog.font,
        text       = "X",
        textColor  = {1.0, 1.0, 1.0, 1.0},
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textureSet = injuryLog.btnTexSet or injuryLog.boxTexSet,
        onClick    = function() injuryLog.hide() end,
        uiscale    = uiscale,
        zIndex     = 502,
    })
    table.insert(injuryLog.chromeButtons, closeBtnId)

    local closeBtnX = panelX + panelW - s.padX - s.closeBtnSize
    local tabY        = panelY + s.titleBarH
    local allTabX     = panelX + s.padX
    local tabStripRight = closeBtnX - s.tabGap

    local textPad   = math.floor(10 * uiscale)
    local sbBtnSize = math.floor(18 * uiscale)
    local sbCapH    = math.floor(4 * uiscale)
    local sbGap     = math.floor(8 * uiscale)
    local contentX     = panelX + s.padX + textPad
    local contentY     = tabY + s.tabHeight + s.padY + textPad
    local contentRight = panelX + panelW - s.padX - textPad
    local contentH     = (panelY + panelH - s.padY - textPad) - contentY
    local regionW      = contentRight - contentX
    local contentW     = regionW - sbBtnSize - sbGap

    injuryLog.layout = {
        s                  = s,
        uiscale            = uiscale,
        tabY               = tabY,
        allTabX            = allTabX,
        tabStripRight      = tabStripRight,
        contentX           = contentX,
        contentY           = contentY,
        contentW           = contentW,
        contentH           = contentH,
    }

    -- #750/#747: clipping viewport for the scrollable prose content only
    -- (title/victim-tab strip/scrollbar stay page-attached chrome,
    -- outside it) — same pattern as scripts/event_log.lua /
    -- scripts/combat_log.lua.
    -- #750 round-3 review: leave the viewport's own zIndex at its
    -- UI.newElement default (0) — see scripts/event_log.lua's identical
    -- comment for why a nonzero value here would accumulate onto every
    -- reparented row's own z via UI.Manager.Query's elementPaintKey.
    injuryLog.contentViewportId = UI.newElement(
        "injury_log_content_viewport", contentW, contentH, injuryLog.pageId)
    UI.addToPage(injuryLog.pageId, injuryLog.contentViewportId, contentX, contentY)
    UI.setClipChildren(injuryLog.contentViewportId, true)

    local sbTrackH = math.max(math.floor(12 * uiscale),
                              contentH - 2 * sbBtnSize - 2 * sbCapH)
    injuryLog.scrollbarId = scrollbar.new({
        name         = "injury_log_scrollbar",
        page         = injuryLog.pageId,
        x            = contentRight - sbBtnSize,
        y            = contentY,
        buttonSize   = sbBtnSize,
        trackHeight  = sbTrackH,
        capHeight    = sbCapH,
        tileSize     = math.floor(8 * uiscale),
        totalItems   = 0,
        visibleItems = 1,
        uiscale      = uiscale,
        zIndex       = { track = 502, button = 503, tab = 504 },
        onScroll     = function(offset) injuryLog.onContentScroll(offset) end,
    })
    scrollbar.setVisible(injuryLog.scrollbarId, false)

    renderContent()

    injuryLog.uiCreated = true
end

-----------------------------------------------------------
-- Event ingestion
-----------------------------------------------------------

local function formatGameTimeHMS(t)
    local secs = math.floor(t or 0)
    if secs < 0 then secs = 0 end
    local hh = math.floor(secs / 3600)
    local mm = math.floor((secs % 3600) / 60)
    local ss = secs % 60
    return string.format("%02d:%02d:%02d", hh, mm, ss)
end

local function formatGameTimeHM(t)
    local secs = math.floor(t or 0)
    if secs < 0 then secs = 0 end
    local hh = math.floor(secs / 3600)
    local mm = math.floor((secs % 3600) / 60)
    return string.format("%02d:%02d", hh, mm)
end

local DISPLAY_NAMES = {
    bear_brown = "the brown bear",
    technomule = "the technomule",
}
local function displayName(uid)
    if not uid then return "?" end
    local info = unit.getInfo(uid)
    -- A named unit (acolyte) reads as its personal name (#264).
    if info and info.name and info.name ~= "" then return info.name end
    if info then
        local mapped = info.defName and DISPLAY_NAMES[info.defName]
        if mapped then return mapped end
        -- Species label (display_name / prettified def name) so unmapped
        -- units don't surface as a raw "Red_squirrel" (#264).
        if info.displayName and info.displayName ~= "" then
            return info.displayName
        end
    end
    return "unit_" .. tostring(uid)
end

local TAB_SHORT_NAMES = {
    bear_brown = "Bear",
    technomule = "Mule",
}
local function tabUnitName(uid)
    if not uid then return "Unit" end
    local info = unit.getInfo(uid)
    -- Named unit: first token of the personal name keeps the tab short (#264).
    if info and info.name and info.name ~= "" then
        return info.name:match("^(%S+)") or info.name
    end
    if not info then return "Unit" end
    local short = info.defName and TAB_SHORT_NAMES[info.defName]
    if short then return short end
    -- Species label (display_name / prettified def name) for unmapped defs.
    if info.displayName and info.displayName ~= "" then return info.displayName end
    local def = info.defName
    if not def then return "Unit" end
    return def:sub(1, 1):upper() .. def:sub(2)
end

-- Find the (recent enough) unit-log for a victim uid. Skips logs silent
-- past the rejoin window so a reused id can't merge onto a dead unit's log.
local function findUnitLog(victim)
    if not victim then return nil end
    local now = engine.gameTime()
    for _, lg in ipairs(injuryLog.unitLogs) do
        if lg.uid == victim
           and (now - (lg.lastEventAt or 0)) <= LOG_REJOIN_MAX_SEC then
            return lg
        end
    end
    return nil
end

local function newUnitLog(victim, gameTime)
    local id = injuryLog.nextLogId
    injuryLog.nextLogId = id + 1
    local baseName = formatGameTimeHM(gameTime) .. " " .. tabUnitName(victim)
    local name = baseName
    local n = 2
    local function nameTaken(nm)
        for _, lg in ipairs(injuryLog.unitLogs) do
            if lg.name == nm then return true end
        end
        return false
    end
    while nameTaken(name) do
        name = baseName .. " (" .. n .. ")"
        n = n + 1
    end
    local lg = {
        id          = id,
        uid         = victim,
        name        = name,
        active      = true,
        events      = {},
        lastEventAt = gameTime,
    }
    table.insert(injuryLog.unitLogs, lg)
    return lg
end

-- Push an injury event into the All-ring and the victim's unit-log.
local function processEvent(ev)
    table.insert(injuryLog.allEvents, 1, ev)
    while #injuryLog.allEvents > ALL_RING_CAP do
        table.remove(injuryLog.allEvents)
    end

    local victim = ev.target
    local lg = findUnitLog(victim)
    if lg then
        lg.active = true
    else
        lg = newUnitLog(victim, ev.ts or 0)
    end
    table.insert(lg.events, 1, ev)
    lg.lastEventAt = ev.ts or 0

    if injuryLog.activeTabId == "all"
       or injuryLog.activeTabId == lg.id then
        injuryLog.dirty = true
        injuryLog.justifyBottom = true
    end
end

-- Returns the event list the active tab currently displays.
function injuryLog.activeTabEvents()
    if injuryLog.activeTabId == "all" then
        return injuryLog.allEvents
    end
    for _, lg in ipairs(injuryLog.unitLogs) do
        if lg.id == injuryLog.activeTabId then
            return lg.events
        end
    end
    return {}
end

-- Format one injury event for display. Returns (text, color).
function injuryLog.formatEvent(ev)
    local ts   = formatGameTimeHMS(ev.ts or 0)
    local name = ev.target and displayName(ev.target) or "?"
    local injuryText = require("scripts.injury_log")
    -- Uppercase the first letter so a leading "the brown bear" reads "The …".
    local function up1(s) return (s:gsub("^%l", string.upper)) end
    local line = up1(injuryText.eventLine(name, ev))
    local color = (ev.kind == "death") and {1.0, 0.4, 0.4, 1.0}
                                        or  {1.0, 0.85, 0.7, 1.0}
    return string.format("[%s] %s", ts, line), color
end

-- Rendered injury-log entries for victim `uid`, for the per-unit log
-- panel. Returns { {ts, text, color}, ... } newest-first.
function injuryLog.unitEntries(uid)
    local out = {}
    if not uid then return out end
    for _, ev in ipairs(injuryLog.allEvents) do
        if ev.target == uid then
            local text, color = injuryLog.formatEvent(ev)
            out[#out + 1] = { ts = ev.ts or 0, text = text, color = color }
        end
    end
    return out
end

-----------------------------------------------------------
-- Tab strip + content rendering
-----------------------------------------------------------

local function tabPixelWidth(text, s)
    local w = engine.getTextWidth(injuryLog.font, text, s.tabFontSize)
    return math.max(s.tabWidth, w + 2 * s.tabPad)
end

local function spawnTab(name, displayText, x, color, logId, width)
    local L = injuryLog.layout
    local s = L.s
    width = width or s.tabWidth
    local isSel = (logId == injuryLog.activeTabId)
    local tabTex = isSel and injuryLog.tabSelTexSet
                          or injuryLog.tabUnselTexSet
    local clickBox = UI.newBox(
        name .. "_box",
        width, s.tabHeight,
        tabTex,
        s.tabTileSize,
        1.0, 1.0, 1.0, 1.0,
        0,
        injuryLog.pageId)
    UI.addToPage(injuryLog.pageId, clickBox, x, L.tabY)
    UI.setClickable(clickBox, true)
    UI.setOnClick(clickBox, INJURY_LOG_TAB_CALLBACK)
    UI.setZIndex(clickBox, 503)
    injuryLog.tabClickBoxes[clickBox] = logId

    local lbl = label.new({
        name     = name .. "_label",
        text     = displayText,
        font     = injuryLog.font,
        fontSize = injuryLog.baseSizes.tabFontSize,
        color    = color,
        page     = injuryLog.pageId,
        uiscale  = L.uiscale,
    })
    local labelW = engine.getTextWidth(injuryLog.font, displayText,
                                       s.tabFontSize)
    local labelX = x + math.floor((width - labelW) / 2)
    local labelY = L.tabY + math.floor((s.tabHeight + s.tabFontSize) / 2)
    UI.addToPage(injuryLog.pageId, label.getElementHandle(lbl),
        labelX, labelY)
    UI.setZIndex(label.getElementHandle(lbl), 504)
    table.insert(injuryLog.tabLabels, lbl)
end

local function spawnArrow(name, glyph, x, callbackName, greyed)
    local L = injuryLog.layout
    local color = greyed and COLOR_DIM or {1.0, 1.0, 1.0, 1.0}
    local btnId = button.new({
        name       = name,
        page       = injuryLog.pageId,
        x          = x,
        y          = L.tabY,
        width      = injuryLog.baseSizes.arrowWidth,
        height     = injuryLog.baseSizes.arrowHeight,
        fontSize   = injuryLog.baseSizes.tabFontSize,
        font       = injuryLog.font,
        text       = glyph,
        textColor  = color,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textureSet = injuryLog.btnTexSet or injuryLog.boxTexSet,
        callbackName = callbackName,
        uiscale    = L.uiscale,
        zIndex     = 503,
    })
    table.insert(injuryLog.tabButtons, btnId)
end

local function wrapText(text, maxW, font, fontSize)
    local function fits(str)
        return engine.getTextWidth(font, str, fontSize) <= maxW
    end
    local lines, cur = {}, ""
    for word in text:gmatch("%S+") do
        local trial = (cur == "") and word or (cur .. " " .. word)
        if fits(trial) then
            cur = trial
        else
            if cur ~= "" then lines[#lines + 1] = cur; cur = "" end
            if fits(word) then
                cur = word
            else
                local chunk = ""
                for ch in word:gmatch(".") do
                    if fits(chunk .. ch) then
                        chunk = chunk .. ch
                    else
                        if chunk ~= "" then lines[#lines + 1] = chunk end
                        chunk = ch
                    end
                end
                cur = chunk
            end
        end
    end
    if cur ~= "" then lines[#lines + 1] = cur end
    if #lines == 0 then lines[1] = "" end
    return lines
end

renderContent = function()
    destroyTransient()
    local L = injuryLog.layout
    if not L then return end
    local s = L.s

    local allW = tabPixelWidth("All", s)
    spawnTab("injury_log_tab_all", "All", L.allTabX, COLOR_ALL, "all", allW)

    local logs       = injuryLog.unitLogs
    local nB         = #logs
    local stripLeft  = L.allTabX + allW + s.tabGap
    local stripRight = L.tabStripRight

    local widths = {}
    for i = 1, nB do widths[i] = tabPixelWidth(logs[i].name, s) end
    local function rangeWidth(a, b)
        local w = 0
        for i = a, b do w = w + widths[i] + (i > a and s.tabGap or 0) end
        return w
    end

    local spawnLog = function(i, x)
        local lg = logs[i]
        spawnTab("injury_log_tab_" .. tostring(lg.id), lg.name, x,
                 lg.active and COLOR_ACTIVE or COLOR_CLOSED, lg.id, widths[i])
    end

    if nB == 0 or stripLeft + rangeWidth(1, nB) <= stripRight then
        injuryLog.scrollOffset = 0
        injuryLog.tabMaxScroll = 0
        local x = stripLeft
        for i = 1, nB do
            spawnLog(i, x)
            x = x + widths[i] + s.tabGap
        end
    else
        local availLeft  = stripLeft + s.arrowWidth + s.tabGap
        local availRight = stripRight - s.arrowWidth - s.tabGap
        local avail      = availRight - availLeft
        local start, w = nB, widths[nB]
        while start > 1 and (w + s.tabGap + widths[start - 1]) <= avail do
            start = start - 1
            w = w + s.tabGap + widths[start]
        end
        injuryLog.tabMaxScroll = start - 1
        injuryLog.scrollOffset =
            math.max(0, math.min(injuryLog.scrollOffset, injuryLog.tabMaxScroll))

        spawnArrow("injury_log_scroll_prev", "<", stripLeft,
                   INJURY_LOG_PREV_CALLBACK, injuryLog.scrollOffset <= 0)
        spawnArrow("injury_log_scroll_next", ">", availRight + s.tabGap,
                   INJURY_LOG_NEXT_CALLBACK,
                   injuryLog.scrollOffset >= injuryLog.tabMaxScroll)

        local x = availLeft
        for i = injuryLog.scrollOffset + 1, nB do
            if x + widths[i] > availRight then break end
            spawnLog(i, x)
            x = x + widths[i] + s.tabGap
        end
    end

    local events = injuryLog.activeTabEvents()
    if #events == 0 then
        local emptyText = "No injuries yet."
        local emptyW = engine.getTextWidth(injuryLog.font, emptyText,
                                           s.fontSize)
        local emptyId = label.new({
            name     = "injury_log_empty",
            text     = emptyText,
            font     = injuryLog.font,
            fontSize = injuryLog.baseSizes.fontSize,
            color    = {0.6, 0.6, 0.6, 1.0},
            page     = injuryLog.pageId,
            uiscale  = L.uiscale,
        })
        table.insert(injuryLog.tabLabels, emptyId)
        UI.addChild(injuryLog.contentViewportId,
            label.getElementHandle(emptyId),
            math.floor((L.contentW - emptyW) / 2),
            math.floor(L.contentH / 2))
        UI.setZIndex(label.getElementHandle(emptyId), 504)
        return
    end

    local lineH = s.rowHeight
    local maxLines = math.max(1, math.floor(L.contentH / lineH))
    local wrapped = {}
    for i = #events, 1, -1 do
        local text, color = injuryLog.formatEvent(events[i])
        for _, wl in ipairs(wrapText(text, L.contentW, injuryLog.font,
                                     s.fontSize)) do
            wrapped[#wrapped + 1] = { text = wl, color = color }
        end
    end

    local total     = #wrapped
    local maxOffset = math.max(0, total - maxLines)
    if injuryLog.justifyBottom then
        injuryLog.contentScroll = maxOffset
        injuryLog.justifyBottom = false
    else
        injuryLog.contentScroll =
            math.max(0, math.min(injuryLog.contentScroll, maxOffset))
    end
    if injuryLog.scrollbarId then
        injuryLog.syncingScrollbar = true
        scrollbar.setContentSize(injuryLog.scrollbarId,
                                 math.max(total, 1), maxLines)
        scrollbar.setScrollOffset(injuryLog.scrollbarId, injuryLog.contentScroll)
        scrollbar.setVisible(injuryLog.scrollbarId, total > maxLines)
        injuryLog.syncingScrollbar = false
    end

    local row = 0
    for i = injuryLog.contentScroll + 1,
            math.min(injuryLog.contentScroll + maxLines, total) do
        row = row + 1
        local w = wrapped[i]
        local lbl = label.new({
            name     = "injury_log_row_" .. row,
            text     = w.text,
            font     = injuryLog.font,
            fontSize = injuryLog.baseSizes.fontSize,
            color    = w.color,
            page     = injuryLog.pageId,
            uiscale  = L.uiscale,
        })
        table.insert(injuryLog.tabLabels, lbl)
        UI.addChild(injuryLog.contentViewportId, label.getElementHandle(lbl),
            0,
            (row - 1) * lineH + s.fontSize)
        UI.setZIndex(label.getElementHandle(lbl), 504)
    end
end

-----------------------------------------------------------
-- Click dispatch (routed via ui_manager)
-----------------------------------------------------------

function injuryLog.onTabClick(elemHandle)
    local logId = injuryLog.tabClickBoxes[elemHandle]
    if logId == nil then return false end
    injuryLog.activeTabId = logId
    renderContent()
    return true
end

function injuryLog.onScrollPrev()
    if injuryLog.scrollOffset > 0 then
        injuryLog.scrollOffset = injuryLog.scrollOffset - 1
        renderContent()
    end
    return true
end

function injuryLog.onScrollNext()
    local maxOff = maxScrollOffset()
    if injuryLog.scrollOffset < maxOff then
        injuryLog.scrollOffset = injuryLog.scrollOffset + 1
        renderContent()
    end
    return true
end

function injuryLog.onContentScroll(offset)
    if injuryLog.syncingScrollbar then return end
    offset = math.floor(offset or 0)
    if offset == injuryLog.contentScroll then return end
    injuryLog.contentScroll = offset
    renderContent()
end

function injuryLog.handleScrollCallback(callbackName, elemHandle)
    if not injuryLog.scrollbarId then return false end
    local sbId = scrollbar.findByElementHandle(elemHandle)
    if sbId ~= injuryLog.scrollbarId then return false end
    if callbackName == "onScrollUp" then
        scrollbar.scrollUp(sbId); return true
    elseif callbackName == "onScrollDown" then
        scrollbar.scrollDown(sbId); return true
    end
    return false
end

function injuryLog.onScroll(elemHandle, dx, dy)
    if not (injuryLog.visible and injuryLog.scrollbarId) then return false end
    local over = false
    for _, id in ipairs(injuryLog.chromePanels) do
        if panel.getBoxHandle(id) == elemHandle then over = true; break end
    end
    if not over and scrollbar.findByElementHandle(elemHandle)
                    == injuryLog.scrollbarId then
        over = true
    end
    if not over then return false end
    if dy > 0 then scrollbar.scrollUp(injuryLog.scrollbarId)
    elseif dy < 0 then scrollbar.scrollDown(injuryLog.scrollbarId) end
    return true
end

-----------------------------------------------------------
-- Lifecycle
-----------------------------------------------------------

function injuryLog.init(scriptId)
    if injuryLog.pageId == nil then
        injuryLog.pageId = UI.newPage("injury_log", "modal")
    end
end

function injuryLog.bootstrap(boxTex, btnTex, font, w, h)
    injuryLog.boxTexSet = boxTex
    injuryLog.btnTexSet = btnTex
    injuryLog.font      = font
    injuryLog.fbW       = w
    injuryLog.fbH       = h
    injuryLog.tabSelTexSet =
        boxTextures.load("assets/textures/ui/tabselected", "tabselected")
    injuryLog.tabUnselTexSet =
        boxTextures.load("assets/textures/ui/tabunselected", "tabunselected")
    if injuryLog.pageId == nil then
        injuryLog.pageId = UI.newPage("injury_log", "modal")
    end
    injuryLog.bootstrapped = true
end

function injuryLog.show()
    if not injuryLog.bootstrapped then return end
    createUI()
    UI.showPage(injuryLog.pageId)
    injuryLog.visible = true
end

function injuryLog.hide()
    injuryLog.visible = false
    if injuryLog.pageId then
        UI.hidePage(injuryLog.pageId)
    end
end

function injuryLog.toggle()
    if injuryLog.visible then
        injuryLog.hide()
    else
        injuryLog.show()
    end
end

function injuryLog.isVisible()
    return injuryLog.visible and true or false
end

-- Drain new injury events, route to All-ring + per-unit logs, fade
-- quiescent logs, repaint when the active tab changed. Called from the
-- Lua thread at the script's tick interval (0.1s).
function injuryLog.update(dt)
    local events = injury.drainEvents() or {}
    for _, ev in ipairs(events) do
        processEvent(ev)
    end

    local now = engine.gameTime()
    local hadActiveChange = false
    for _, lg in ipairs(injuryLog.unitLogs) do
        if lg.active and (now - (lg.lastEventAt or 0))
                        > LOG_QUIESCENCE_SEC then
            lg.active = false
            hadActiveChange = true
        end
    end

    if injuryLog.visible and injuryLog.uiCreated
       and (injuryLog.dirty or hadActiveChange) then
        injuryLog.dirty = false
        renderContent()
    end
end

function injuryLog.onFramebufferResize(width, height)
    -- #750: a 0x0 minimize must not become the stored geometry or trigger
    -- a rebuild against a degenerate framebuffer; keep the last valid
    -- fbW/fbH and skip rebuilding, same as C2's responsive.notifyResize
    -- guard for menu screens.
    if width <= 0 or height <= 0 then return end
    injuryLog.fbW = width
    injuryLog.fbH = height
    if injuryLog.visible and injuryLog.uiCreated then
        createUI()
    end
end

function injuryLog.shutdown()
    destroyChrome()
    destroyTransient()
    injuryLog.visible   = false
    injuryLog.uiCreated = false
end

return injuryLog
