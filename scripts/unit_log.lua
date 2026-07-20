-- Per-Unit Log Panel
--
-- Opened by the "Log" button in the unit-info panel. Collates the four
-- logs — Event, Combat, Injury, Thought — for ONE unit into a single
-- window, with fixed tabs (All / Event / Combat / Injury / Thought). It
-- owns no event stream of its own: it READS the already-populated stores
-- of the sibling logs
--   * combat_log.unitEntries(uid)        (attacker or target == uid)
--   * injury_log_panel.unitEntries(uid)  (victim == uid)
--   * thought_log.unitEntries(uid)       (the thinking unit == uid, #351)
--   * engine.getEventLog() filtered by ev.uid == uid (events tagged via
--     engine.emitEventForUnit)
-- and merges them by timestamp for the All tab. Entries keep their source
-- colour so a mixed list reads colour-coded.

local scale       = require("scripts.ui.scale")
local panel       = require("scripts.ui.panel")
local label       = require("scripts.ui.label")
local button      = require("scripts.ui.button")
local boxTextures = require("scripts.ui.box_textures")
local scrollbar   = require("scripts.ui.scrollbar")

local unitLog = package.loaded["scripts.unit_log"] or {}
package.loaded["scripts.unit_log"] = unitLog

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

unitLog.pageId       = unitLog.pageId       or nil
unitLog.boxTexSet    = unitLog.boxTexSet    or nil
unitLog.btnTexSet    = unitLog.btnTexSet    or nil
unitLog.tabSelTexSet   = unitLog.tabSelTexSet   or nil
unitLog.tabUnselTexSet = unitLog.tabUnselTexSet or nil
unitLog.font         = unitLog.font         or nil
unitLog.fbW          = unitLog.fbW          or 0
unitLog.fbH          = unitLog.fbH          or 0
unitLog.bootstrapped = unitLog.bootstrapped or false
unitLog.visible      = unitLog.visible      or false
unitLog.uiCreated    = unitLog.uiCreated    or false

unitLog.uid          = unitLog.uid          or nil   -- subject unit

unitLog.chromeLabels  = unitLog.chromeLabels  or {}
unitLog.chromePanels  = unitLog.chromePanels  or {}
unitLog.chromeButtons = unitLog.chromeButtons or {}
unitLog.tabLabels  = unitLog.tabLabels  or {}
unitLog.tabClickBoxes = unitLog.tabClickBoxes or {}   -- elemHandle → tabKey

unitLog.activeTabKey = unitLog.activeTabKey or "all"
unitLog.contentScroll    = unitLog.contentScroll    or 0
unitLog.justifyBottom    = true
unitLog.lastMaxOffset    = unitLog.lastMaxOffset    or 0
unitLog.syncingScrollbar = false
unitLog.scrollbarId      = unitLog.scrollbarId      or nil

unitLog.layout = unitLog.layout or {}

local UNIT_LOG_TAB_CALLBACK = "onUnitLogTabClick"

-- Fixed tab set, in display order.
local TABS = {
    { key = "all",     name = "All"     },
    { key = "event",   name = "Event"   },
    { key = "combat",  name = "Combat"  },
    { key = "injury",  name = "Injury"  },
    { key = "thought", name = "Thought" },
}

unitLog.baseSizes = {
    fontSize     = 18,
    titleSize    = 26,
    titleBarH    = 48,
    closeBtnSize = 36,
    padX         = 24,
    padY         = 20,
    rowHeight    = 22,
    tabWidth     = 80,
    tabPad       = 12,
    tabHeight    = 36,
    tabFontSize  = 14,
    tabTileSize  = 16,
    tabGap       = 4,
    tileSize     = 64,
    panelFrac    = 0.7,
}

local COLOR_SEL   = { 1.0, 1.0, 1.0, 1.0 }
local COLOR_UNSEL = { 0.85, 0.85, 0.85, 1.0 }
-- Source tint for event-log entries (combat / injury carry their own).
local COLOR_EVENT = { 0.70, 0.85, 1.0, 1.0 }

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------

local function destroyChrome()
    for _, id in ipairs(unitLog.chromeLabels)  do label.destroy(id)  end
    for _, id in ipairs(unitLog.chromeButtons) do button.destroy(id) end
    for _, id in ipairs(unitLog.chromePanels)  do panel.destroy(id)  end
    if unitLog.scrollbarId then
        scrollbar.destroy(unitLog.scrollbarId)
        unitLog.scrollbarId = nil
    end
    -- #750: the content clip viewport — destroyTransient (called first
    -- by every real teardown path) already destroyed its children;
    -- UI.deleteElement is idempotent either way.
    if unitLog.contentViewportId then
        UI.deleteElement(unitLog.contentViewportId)
        unitLog.contentViewportId = nil
    end
    unitLog.chromeLabels  = {}
    unitLog.chromeButtons = {}
    unitLog.chromePanels  = {}
end

local function destroyTransient()
    for _, id in ipairs(unitLog.tabLabels) do label.destroy(id) end
    for handle, _ in pairs(unitLog.tabClickBoxes) do
        UI.deleteElement(handle)
    end
    unitLog.tabLabels     = {}
    unitLog.tabClickBoxes = {}
end

-----------------------------------------------------------
-- Helpers / data
-----------------------------------------------------------

local function formatGameTimeHMS(t)
    local secs = math.floor(t or 0)
    if secs < 0 then secs = 0 end
    local hh = math.floor(secs / 3600)
    local mm = math.floor((secs % 3600) / 60)
    local ss = secs % 60
    return string.format("%02d:%02d:%02d", hh, mm, ss)
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

-- Per-category text colour (so a red unit_warning reads red here too,
-- not the generic event tint). Built fresh from the notification cfg.
local function categoryColors()
    local m = {}
    for _, c in ipairs(engine.getNotificationCfg() or {}) do
        local tc = c.textColor
        if c.id and tc then
            m[c.id] = { tc.r or 1.0, tc.g or 1.0, tc.b or 1.0, tc.a or 1.0 }
        end
    end
    return m
end

-- Event-log entries about this unit (tagged via emitEventForUnit).
local function eventEntries(uid)
    local out = {}
    local colors = categoryColors()
    local log = engine.getEventLog() or {}
    for _, ev in ipairs(log) do
        if ev.uid == uid then
            local txt = ev.text or ""
            if (ev.count or 1) > 1 then
                txt = txt .. " (x" .. ev.count .. ")"
            end
            out[#out + 1] = {
                ts    = ev.gameTime or 0,
                text  = string.format("[%s] %s",
                    formatGameTimeHMS(ev.gameTime or 0), txt),
                color = colors[ev.category] or COLOR_EVENT,
            }
        end
    end
    return out
end

local function combatEntries(uid)
    local m = package.loaded["scripts.combat_log"]
    if m and m.unitEntries then return m.unitEntries(uid) end
    return {}
end

local function injuryEntries(uid)
    local m = package.loaded["scripts.injury_log_panel"]
    if m and m.unitEntries then return m.unitEntries(uid) end
    return {}
end

local function thoughtEntries(uid)
    local m = package.loaded["scripts.thought_log"]
    if m and m.unitEntries then return m.unitEntries(uid) end
    return {}
end

-- Entries for the active tab, NEWEST-FIRST.
local function entriesFor(uid, tabKey)
    if tabKey == "event"   then return eventEntries(uid)   end
    if tabKey == "combat"  then return combatEntries(uid)  end
    if tabKey == "injury"  then return injuryEntries(uid)  end
    if tabKey == "thought" then return thoughtEntries(uid) end
    -- "all": merge the four and sort newest-first by ts.
    local all = {}
    for _, e in ipairs(eventEntries(uid))   do all[#all + 1] = e end
    for _, e in ipairs(combatEntries(uid))  do all[#all + 1] = e end
    for _, e in ipairs(injuryEntries(uid))  do all[#all + 1] = e end
    for _, e in ipairs(thoughtEntries(uid)) do all[#all + 1] = e end
    table.sort(all, function(a, b) return (a.ts or 0) > (b.ts or 0) end)
    return all
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

-----------------------------------------------------------
-- Build the panel chrome
-----------------------------------------------------------

local renderContent
local createUI

createUI = function()
    destroyChrome()
    destroyTransient()

    local uiscale = scale.get()
    local s = scale.applyAllWith(unitLog.baseSizes, uiscale)

    local panelW = math.floor(unitLog.fbW * unitLog.baseSizes.panelFrac)
    local panelH = math.floor(unitLog.fbH * unitLog.baseSizes.panelFrac)
    local panelX = math.floor((unitLog.fbW - panelW) / 2)
    local panelY = math.floor((unitLog.fbH - panelH) / 2)

    local panelId = panel.new({
        name       = "unit_log_panel",
        page       = unitLog.pageId,
        x          = panelX,
        y          = panelY,
        width      = panelW,
        height     = panelH,
        textureSet = unitLog.boxTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = unitLog.baseSizes.tileSize,
        zIndex     = 500,
        padding    = { top = s.padY, bottom = s.padY,
                       left = s.padX, right = s.padX },
        uiscale    = 1.0,
    })
    table.insert(unitLog.chromePanels, panelId)
    -- #743: explicit scroll-capture + pointer-block, replacing the old
    -- setClickable(true)-with-no-callback attempt (which never actually
    -- worked — wheel routing required a click callback too) so a mouse
    -- wheel over the content routes to unitLog.onScroll, and blank
    -- panel space doesn't leak clicks through to gameplay underneath.
    pcall(function()
        local boxHandle = panel.getBoxHandle(panelId)
        UI.setScrollCapture(boxHandle, true)
        UI.setPointerBlocking(boxHandle, true)
    end)

    local titleId = label.new({
        name     = "unit_log_title",
        text     = displayName(unitLog.uid) .. " — Log",
        font     = unitLog.font,
        fontSize = unitLog.baseSizes.titleSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = unitLog.pageId,
        uiscale  = uiscale,
    })
    table.insert(unitLog.chromeLabels, titleId)
    UI.addToPage(unitLog.pageId, label.getElementHandle(titleId),
        panelX + s.padX, panelY + s.padY + s.titleSize)
    UI.setZIndex(label.getElementHandle(titleId), 502)

    local closeBtnId = button.new({
        name       = "unit_log_close",
        page       = unitLog.pageId,
        x          = panelX + panelW - s.padX - s.closeBtnSize,
        y          = panelY + s.padY,
        width      = unitLog.baseSizes.closeBtnSize,
        height     = unitLog.baseSizes.closeBtnSize,
        fontSize   = unitLog.baseSizes.fontSize,
        font       = unitLog.font,
        text       = "X",
        textColor  = {1.0, 1.0, 1.0, 1.0},
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textureSet = unitLog.btnTexSet or unitLog.boxTexSet,
        onClick    = function() unitLog.hide() end,
        uiscale    = uiscale,
        zIndex     = 502,
    })
    table.insert(unitLog.chromeButtons, closeBtnId)

    local tabY    = panelY + s.titleBarH
    local tabX0   = panelX + s.padX

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

    unitLog.layout = {
        s = s, uiscale = uiscale,
        tabY = tabY, tabX0 = tabX0,
        contentX = contentX, contentY = contentY,
        contentW = contentW, contentH = contentH,
    }

    -- #750/#747: clipping viewport for the scrollable prose content only
    -- (title/unit-tab strip/scrollbar stay page-attached chrome, outside
    -- it) — same pattern as scripts/event_log.lua / combat_log.lua /
    -- injury_log_panel.lua.
    -- #750 round-3 review: leave the viewport's own zIndex at its
    -- UI.newElement default (0) — see scripts/event_log.lua's identical
    -- comment for why a nonzero value here would accumulate onto every
    -- reparented row's own z via UI.Manager.Query's elementPaintKey.
    unitLog.contentViewportId = UI.newElement(
        "unit_log_content_viewport", contentW, contentH, unitLog.pageId)
    UI.addToPage(unitLog.pageId, unitLog.contentViewportId, contentX, contentY)
    UI.setClipChildren(unitLog.contentViewportId, true)

    local sbTrackH = math.max(math.floor(12 * uiscale),
                              contentH - 2 * sbBtnSize - 2 * sbCapH)
    unitLog.scrollbarId = scrollbar.new({
        name         = "unit_log_scrollbar",
        page         = unitLog.pageId,
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
        onScroll     = function(offset) unitLog.onContentScroll(offset) end,
    })
    scrollbar.setVisible(unitLog.scrollbarId, false)

    renderContent()
    unitLog.uiCreated = true
end

-----------------------------------------------------------
-- Tab strip + content
-----------------------------------------------------------

local function tabPixelWidth(text, s)
    local w = engine.getTextWidth(unitLog.font, text, s.tabFontSize)
    return math.max(s.tabWidth, w + 2 * s.tabPad)
end

local function spawnTab(name, displayText, x, color, tabKey, width)
    local L = unitLog.layout
    local s = L.s
    local isSel = (tabKey == unitLog.activeTabKey)
    local tabTex = isSel and unitLog.tabSelTexSet or unitLog.tabUnselTexSet
    local clickBox = UI.newBox(
        name .. "_box", width, s.tabHeight, tabTex, s.tabTileSize,
        1.0, 1.0, 1.0, 1.0, 0, unitLog.pageId)
    UI.addToPage(unitLog.pageId, clickBox, x, L.tabY)
    UI.setClickable(clickBox, true)
    UI.setOnClick(clickBox, UNIT_LOG_TAB_CALLBACK)
    UI.setZIndex(clickBox, 503)
    unitLog.tabClickBoxes[clickBox] = tabKey

    local lbl = label.new({
        name     = name .. "_label",
        text     = displayText,
        font     = unitLog.font,
        fontSize = unitLog.baseSizes.tabFontSize,
        color    = color,
        page     = unitLog.pageId,
        uiscale  = L.uiscale,
    })
    local labelW = engine.getTextWidth(unitLog.font, displayText, s.tabFontSize)
    local labelX = x + math.floor((width - labelW) / 2)
    local labelY = L.tabY + math.floor((s.tabHeight + s.tabFontSize) / 2)
    UI.addToPage(unitLog.pageId, label.getElementHandle(lbl), labelX, labelY)
    UI.setZIndex(label.getElementHandle(lbl), 504)
    table.insert(unitLog.tabLabels, lbl)
end

renderContent = function()
    destroyTransient()
    local L = unitLog.layout
    if not L then return end
    local s = L.s

    -- Fixed tabs, left to right.
    local x = L.tabX0
    for _, t in ipairs(TABS) do
        local w = tabPixelWidth(t.name, s)
        local sel = (t.key == unitLog.activeTabKey)
        spawnTab("unit_log_tab_" .. t.key, t.name, x,
                 sel and COLOR_SEL or COLOR_UNSEL, t.key, w)
        x = x + w + s.tabGap
    end

    local events = entriesFor(unitLog.uid, unitLog.activeTabKey)
    if #events == 0 then
        local emptyText = "No log entries for this unit."
        local emptyW = engine.getTextWidth(unitLog.font, emptyText, s.fontSize)
        local emptyId = label.new({
            name     = "unit_log_empty",
            text     = emptyText,
            font     = unitLog.font,
            fontSize = unitLog.baseSizes.fontSize,
            color    = {0.6, 0.6, 0.6, 1.0},
            page     = unitLog.pageId,
            uiscale  = L.uiscale,
        })
        table.insert(unitLog.tabLabels, emptyId)
        UI.addChild(unitLog.contentViewportId, label.getElementHandle(emptyId),
            math.floor((L.contentW - emptyW) / 2),
            math.floor(L.contentH / 2))
        UI.setZIndex(label.getElementHandle(emptyId), 504)
        return
    end

    -- Wrap in CHRONOLOGICAL order (entries are newest-first; iterate
    -- backwards so newest sits at the bottom).
    local lineH = s.rowHeight
    local maxLines = math.max(1, math.floor(L.contentH / lineH))
    local wrapped = {}
    for i = #events, 1, -1 do
        local e = events[i]
        for _, wl in ipairs(wrapText(e.text, L.contentW, unitLog.font,
                                     s.fontSize)) do
            wrapped[#wrapped + 1] = { text = wl, color = e.color }
        end
    end

    local total       = #wrapped
    local maxOffset   = math.max(0, total - maxLines)
    local wasAtBottom = unitLog.justifyBottom
        or unitLog.contentScroll >= unitLog.lastMaxOffset
    if wasAtBottom then
        unitLog.contentScroll = maxOffset
        unitLog.justifyBottom = false
    else
        unitLog.contentScroll =
            math.max(0, math.min(unitLog.contentScroll, maxOffset))
    end
    unitLog.lastMaxOffset = maxOffset
    if unitLog.scrollbarId then
        unitLog.syncingScrollbar = true
        scrollbar.setContentSize(unitLog.scrollbarId, math.max(total, 1), maxLines)
        scrollbar.setScrollOffset(unitLog.scrollbarId, unitLog.contentScroll)
        scrollbar.setVisible(unitLog.scrollbarId, total > maxLines)
        unitLog.syncingScrollbar = false
    end

    local row = 0
    for i = unitLog.contentScroll + 1,
            math.min(unitLog.contentScroll + maxLines, total) do
        row = row + 1
        local w = wrapped[i]
        local lbl = label.new({
            name     = "unit_log_row_" .. row,
            text     = w.text,
            font     = unitLog.font,
            fontSize = unitLog.baseSizes.fontSize,
            color    = w.color,
            page     = unitLog.pageId,
            uiscale  = L.uiscale,
        })
        table.insert(unitLog.tabLabels, lbl)
        UI.addChild(unitLog.contentViewportId, label.getElementHandle(lbl),
            0, (row - 1) * lineH + s.fontSize)
        UI.setZIndex(label.getElementHandle(lbl), 504)
    end
end

-----------------------------------------------------------
-- Click dispatch (routed via ui_manager)
-----------------------------------------------------------

function unitLog.onTabClick(elemHandle)
    local tabKey = unitLog.tabClickBoxes[elemHandle]
    if tabKey == nil then return false end
    unitLog.activeTabKey = tabKey
    unitLog.justifyBottom = true
    renderContent()
    return true
end

function unitLog.onContentScroll(offset)
    if unitLog.syncingScrollbar then return end
    offset = math.floor(offset or 0)
    if offset == unitLog.contentScroll then return end
    unitLog.contentScroll = offset
    renderContent()
end

function unitLog.handleScrollCallback(callbackName, elemHandle)
    if not unitLog.scrollbarId then return false end
    local sbId = scrollbar.findByElementHandle(elemHandle)
    if sbId ~= unitLog.scrollbarId then return false end
    if callbackName == "onScrollUp" then
        scrollbar.scrollUp(sbId); return true
    elseif callbackName == "onScrollDown" then
        scrollbar.scrollDown(sbId); return true
    end
    return false
end

function unitLog.onScroll(elemHandle, dx, dy)
    if not (unitLog.visible and unitLog.scrollbarId) then return false end
    local over = false
    for _, id in ipairs(unitLog.chromePanels) do
        if panel.getBoxHandle(id) == elemHandle then over = true; break end
    end
    if not over and scrollbar.findByElementHandle(elemHandle)
                    == unitLog.scrollbarId then
        over = true
    end
    if not over then return false end
    if dy > 0 then scrollbar.scrollUp(unitLog.scrollbarId)
    elseif dy < 0 then scrollbar.scrollDown(unitLog.scrollbarId) end
    return true
end

-----------------------------------------------------------
-- Lifecycle
-----------------------------------------------------------

function unitLog.init(scriptId)
    if unitLog.pageId == nil then
        unitLog.pageId = UI.newPage("unit_log", "modal")
    end
end

function unitLog.bootstrap(boxTex, btnTex, font, w, h)
    unitLog.boxTexSet = boxTex
    unitLog.btnTexSet = btnTex
    unitLog.font      = font
    unitLog.fbW       = w
    unitLog.fbH       = h
    unitLog.tabSelTexSet =
        boxTextures.load("assets/textures/ui/tabselected", "tabselected")
    unitLog.tabUnselTexSet =
        boxTextures.load("assets/textures/ui/tabunselected", "tabunselected")
    if unitLog.pageId == nil then
        unitLog.pageId = UI.newPage("unit_log", "modal")
    end
    unitLog.bootstrapped = true
end

-- Open the panel for a specific unit (called from the unit-info Log button).
function unitLog.show(uid)
    if not unitLog.bootstrapped then return end
    if uid then unitLog.uid = uid end
    if not unitLog.uid then return end
    unitLog.justifyBottom = true
    createUI()
    UI.showPage(unitLog.pageId)
    unitLog.visible = true
end

function unitLog.hide()
    unitLog.visible = false
    if unitLog.pageId then
        UI.hidePage(unitLog.pageId)
    end
end

function unitLog.toggle(uid)
    if unitLog.visible and (uid == nil or uid == unitLog.uid) then
        unitLog.hide()
    else
        unitLog.show(uid)
    end
end

function unitLog.isVisible()
    return unitLog.visible and true or false
end

-- Re-render while visible so new entries appear live. (No stream of its
-- own to drain — it reads the sibling logs' stores + the event log.)
-- renderContent keeps following the tail only if the user was already at
-- the bottom; once they scroll up, their position stays put.
function unitLog.update(dt)
    if unitLog.visible and unitLog.uiCreated then
        renderContent()
    end
end

function unitLog.onFramebufferResize(width, height)
    -- #750: a 0x0 minimize must not become the stored geometry or trigger
    -- a rebuild against a degenerate framebuffer; keep the last valid
    -- fbW/fbH and skip rebuilding, same as C2's responsive.notifyResize
    -- guard for menu screens.
    if width <= 0 or height <= 0 then return end
    unitLog.fbW = width
    unitLog.fbH = height
    if unitLog.visible and unitLog.uiCreated then
        createUI()
    end
end

function unitLog.shutdown()
    destroyChrome()
    destroyTransient()
    unitLog.visible   = false
    unitLog.uiCreated = false
end

return unitLog
