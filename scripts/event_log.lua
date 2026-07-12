-- Event Log Panel
--
-- A non-blocking, near-fullscreen window that shows the player-events
-- ring buffer. Tabs across the top filter the list to a single
-- category (plus an "All" tab). Newest events at top.
--
-- Lifecycle:
--   * engine.loadScript registers the module (so it receives any
--     future engine broadcasts; today there are none).
--   * uiManager.ensurePopupsAndLogs() calls event_log.bootstrap(...)
--     once fonts and box textures are ready.
--   * hud.lua provides a top-left toggle button that calls
--     event_log.toggle(). The panel does NOT pause the engine
--     (per user choice 2026-05-18) — gameplay continues while open.
--   * Tab clicks come in via ui_manager.onTabClick → tabbar internal
--     handling → our onChange callback re-renders the content.
--
-- Content refresh strategy: the event list is rebuilt on (a) show,
-- (b) tab change. Events emitted while the panel is open won't show
-- until the panel is reopened. Phase 2 #4 (keyboard shortcuts) and
-- live-tail refresh are out of scope for now.

local scale     = require("scripts.ui.scale")
local panel     = require("scripts.ui.panel")
local label     = require("scripts.ui.label")
local button    = require("scripts.ui.button")
local tabbar    = require("scripts.ui.tabbar")
local scrollbar = require("scripts.ui.scrollbar")
local utf8Safe  = require("scripts.ui.utf8_safe")

-- Singleton (mirrors pause.lua / popup.lua so engine.loadScript and
-- require both reach the same table).
local eventLog = package.loaded["scripts.event_log"] or {}
package.loaded["scripts.event_log"] = eventLog

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

eventLog.pageId       = eventLog.pageId       or nil
eventLog.boxTexSet    = eventLog.boxTexSet    or nil  -- panel background
eventLog.btnTexSet    = eventLog.btnTexSet    or nil  -- close button background
eventLog.font         = eventLog.font         or nil
eventLog.fbW          = eventLog.fbW          or 0
eventLog.fbH          = eventLog.fbH          or 0
eventLog.bootstrapped = eventLog.bootstrapped or false
eventLog.visible      = eventLog.visible      or false
eventLog.uiCreated    = eventLog.uiCreated    or false

-- Element ownership (rebuilt on createUI).
eventLog.ownedLabels  = eventLog.ownedLabels  or {}
eventLog.ownedPanels  = eventLog.ownedPanels  or {}
eventLog.ownedButtons = eventLog.ownedButtons or {}
eventLog.ownedTabbars = eventLog.ownedTabbars or {}
eventLog.ownedScrollbars = eventLog.ownedScrollbars or {}

-- Per-row elements for the currently-rendered tab (cleared + rebuilt
-- on tab switch and on show). Each entry: { rowLabels = {labelIds},
-- clickBoxHandle = elemHandle }.
eventLog.rowEntries = eventLog.rowEntries or {}

-- Click dispatch state: filtered event list captured at last
-- renderRows, plus a map from clickable-box elemHandle → index
-- into filteredEvents. Used by onRowClick (dispatched via
-- ui_manager.onEventLogRowClick) to repop the popup for that event.
eventLog.filteredEvents = eventLog.filteredEvents or {}
eventLog.rowClickBoxes  = eventLog.rowClickBoxes  or {}

-- Click-callback name routed by ui_manager.onEventLogRowClick.
local EVENT_LOG_ROW_CALLBACK = "onEventLogRowClick"

eventLog.tabbarId   = eventLog.tabbarId   or nil
eventLog.scrollbarId = eventLog.scrollbarId or nil
eventLog.activeTabKey = eventLog.activeTabKey or "all"
eventLog.scrollOffset = eventLog.scrollOffset or 0

-- Geometry cached for the active createUI pass — used by the
-- content renderer (P2#3.4) for row layout + scrollbar bounds.
eventLog.layout = eventLog.layout or {}

-----------------------------------------------------------
-- Sizing constants (base, before uiscale)
-----------------------------------------------------------

eventLog.baseSizes = {
    fontSize     = 18,
    titleSize    = 28,
    tabHeight    = 36,
    tabFontSize  = 18,
    titleBarH    = 48,
    closeBtnSize = 36,
    padX         = 24,
    padY         = 20,
    rowHeight    = 22,
    -- Widened the time column to fit HH:MM:SS comfortably (the
    -- 64px value here previously overflowed once game time crossed
    -- an hour, bleeding into the category column).
    timeColW     = 96,
    catColW      = 200,
    -- Explicit inter-column gap so values never butt up flush
    -- against the next column even if a value happens to fit
    -- exactly within its column width.
    columnGap    = 20,
    tileSize     = 64,
    panelFrac    = 0.85,  -- panel dimensions as fraction of framebuffer
}

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------

local function destroyOwned()
    for _, id in ipairs(eventLog.ownedLabels)  do label.destroy(id)  end
    for _, id in ipairs(eventLog.ownedButtons) do button.destroy(id) end
    for _, id in ipairs(eventLog.ownedTabbars) do tabbar.destroy(id) end
    for _, id in ipairs(eventLog.ownedScrollbars) do scrollbar.destroy(id) end
    for _, id in ipairs(eventLog.ownedPanels)  do panel.destroy(id)  end
    -- Row labels are owned by rowEntries (renderRows destroys + rebuilds
    -- them per scroll); they are NOT in ownedLabels — see renderRows.
    for _, entry in ipairs(eventLog.rowEntries) do
        for _, lid in ipairs(entry.rowLabels or {}) do label.destroy(lid) end
    end
    -- Click overlay boxes are raw UI elements (not wrapped); delete
    -- them directly. The same handles live as keys in rowClickBoxes.
    for handle, _ in pairs(eventLog.rowClickBoxes) do
        UI.deleteElement(handle)
    end
    eventLog.ownedLabels    = {}
    eventLog.ownedButtons   = {}
    eventLog.ownedTabbars   = {}
    eventLog.ownedScrollbars = {}
    eventLog.ownedPanels    = {}
    eventLog.rowEntries     = {}
    eventLog.rowClickBoxes  = {}
    eventLog.filteredEvents = {}
    eventLog.tabbarId       = nil
    eventLog.scrollbarId    = nil
end

-----------------------------------------------------------
-- Build the panel chrome (title bar, close button, tab bar
-- placeholder). Content rendering is filled in by renderContent()
-- which is invoked on show and on tab change.
-----------------------------------------------------------

local function buildTabsList()
    -- "All" tab first, then one per category in YAML registry order.
    local tabs = { { name = "All", key = "all" } }
    local cfg = engine.getNotificationCfg() or {}
    for _, c in ipairs(cfg) do
        table.insert(tabs, { name = c.displayName or c.id, key = c.id })
    end
    return tabs
end

-- Forward declarations (defined below). renderRows is forward-
-- declared so the scrollbar's onScroll callback (created inside
-- createUI) can resolve it via the local scope chain rather than
-- the global environment.
local renderRows
local renderContent
local createUI

createUI = function()
    destroyOwned()

    local uiscale = scale.get()
    local s = scale.applyAllWith(eventLog.baseSizes, uiscale)

    -- Panel dimensions: panelFrac of framebuffer, centered.
    local panelW = math.floor(eventLog.fbW * eventLog.baseSizes.panelFrac)
    local panelH = math.floor(eventLog.fbH * eventLog.baseSizes.panelFrac)
    local panelX = math.floor((eventLog.fbW - panelW) / 2)
    local panelY = math.floor((eventLog.fbH - panelH) / 2)

    local panelId = panel.new({
        name       = "event_log_panel",
        page       = eventLog.pageId,
        x          = panelX,
        y          = panelY,
        width      = panelW,
        height     = panelH,
        textureSet = eventLog.boxTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = eventLog.baseSizes.tileSize,
        zIndex     = 500,
        padding    = { top = s.padY, bottom = s.padY,
                       left = s.padX, right = s.padX },
        uiscale    = 1.0,  -- panel dimensions already scaled
    })
    table.insert(eventLog.ownedPanels, panelId)
    -- #743: explicit scroll-capture + pointer-block on the panel
    -- background, so a mouse wheel over blank row space routes to
    -- eventLog.onScroll (below) instead of falling through to world
    -- zoom, and blank panel space doesn't leak clicks through to
    -- gameplay underneath.
    pcall(function()
        local boxHandle = panel.getBoxHandle(panelId)
        UI.setScrollCapture(boxHandle, true)
        UI.setPointerBlocking(boxHandle, true)
    end)

    -- Title (top-left)
    local titleId = label.new({
        name     = "event_log_title",
        text     = "Event Log",
        font     = eventLog.font,
        fontSize = eventLog.baseSizes.titleSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = eventLog.pageId,
        uiscale  = uiscale,
    })
    table.insert(eventLog.ownedLabels, titleId)
    UI.addToPage(eventLog.pageId, label.getElementHandle(titleId),
        panelX + s.padX, panelY + s.padY + s.titleSize)
    UI.setZIndex(label.getElementHandle(titleId), 502)

    -- Close button (top-right). Label "X" centered.
    local closeBtnId = button.new({
        name       = "event_log_close",
        page       = eventLog.pageId,
        x          = panelX + panelW - s.padX - s.closeBtnSize,
        y          = panelY + s.padY,
        width      = eventLog.baseSizes.closeBtnSize,
        height     = eventLog.baseSizes.closeBtnSize,
        fontSize   = eventLog.baseSizes.fontSize,
        font       = eventLog.font,
        text       = "X",
        textColor  = {1.0, 1.0, 1.0, 1.0},
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textureSet = eventLog.btnTexSet or eventLog.boxTexSet,
        onClick    = function() eventLog.hide() end,
        uiscale    = uiscale,
        zIndex     = 502,
    })
    table.insert(eventLog.ownedButtons, closeBtnId)

    -- Tab bar: anchored below the title bar. The tabbar component
    -- also creates a "frame" box that sits below the tab strip —
    -- we'll position our event rows inside that frame's bounds.
    local tabY = panelY + s.titleBarH
    local tabFrameH = panelH - (tabY - panelY) - s.padY
    local tabs = buildTabsList()

    eventLog.tabbarId = tabbar.new({
        name        = "event_log_tabs",
        page        = eventLog.pageId,
        font        = eventLog.font,
        fontSize    = eventLog.baseSizes.tabFontSize,
        tabHeight   = eventLog.baseSizes.tabHeight,
        x           = panelX + s.padX,
        y           = tabY,
        width       = panelW - 2 * s.padX,
        frameHeight = tabFrameH - s.tabHeight,
        tabs        = tabs,
        textColor          = {0.85, 0.85, 0.85, 1.0},
        selectedTextColor  = {1.0, 1.0, 1.0, 1.0},
        uiscale     = uiscale,
        zIndex      = 503,
        onChange = function(tabKey, index, tabbarHandle)
            eventLog.activeTabKey = tabKey or "all"
            eventLog.scrollOffset = 0
            renderContent()
        end,
    })
    table.insert(eventLog.ownedTabbars, eventLog.tabbarId)

    -- Cache geometry for the content renderer.
    local contentX = panelX + s.padX + s.padX
    local contentY = tabY + s.tabHeight + s.padY
    local sbWidth  = math.floor(24 * uiscale) + math.floor(8 * uiscale)
    local contentW = panelW - 4 * s.padX - sbWidth
    local contentH = (panelY + panelH - s.padY) - contentY
    local visibleRows = math.max(1, math.floor(contentH / s.rowHeight))
    eventLog.layout = {
        contentX    = contentX,
        contentY    = contentY,
        contentW    = contentW,
        contentH    = contentH,
        rowHeight   = s.rowHeight,
        timeColW    = s.timeColW,
        catColW     = s.catColW,
        columnGap   = s.columnGap,
        visibleRows = visibleRows,
        uiscale     = uiscale,
        s           = s,
    }

    -- Persistent scrollbar to the right of the content area. Its
    -- onScroll callback only mutates scrollOffset + re-renders rows
    -- (never destroys itself).
    local sbButtonSize  = math.floor(24 * uiscale)
    local sbCap         = math.floor(4 * uiscale)
    local sbTrackHeight = math.max(24, contentH - 2 * sbButtonSize - 2 * sbCap)
    eventLog.scrollbarId = scrollbar.new({
        name         = "event_log_scrollbar",
        page         = eventLog.pageId,
        x            = contentX + contentW + math.floor(8 * uiscale),
        y            = contentY,
        buttonSize   = sbButtonSize,
        trackHeight  = sbTrackHeight,
        capHeight    = sbCap,
        tileSize     = math.floor(8 * uiscale),
        totalItems   = 1,
        visibleItems = visibleRows,
        uiscale      = uiscale,
        zIndex       = 503,
        onScroll     = function(offset, _id, _name)
            eventLog.scrollOffset = offset
            renderRows()
        end,
    })
    table.insert(eventLog.ownedScrollbars, eventLog.scrollbarId)

    -- Activate "All" tab on every fresh open so the user lands on
    -- the full history rather than wherever they were last time.
    eventLog.activeTabKey = "all"
    eventLog.scrollOffset = 0
    renderContent()

    eventLog.uiCreated = true
end

-----------------------------------------------------------
-- Content rendering — implemented in P2#3.4
-----------------------------------------------------------

-- Format gameTime (seconds since world start) as MM:SS or HH:MM:SS.
local function formatGameTime(t)
    local secs = math.floor(t or 0)
    if secs < 0 then secs = 0 end
    local hh = math.floor(secs / 3600)
    local mm = math.floor((secs % 3600) / 60)
    local ss = secs % 60
    if hh > 0 then
        return string.format("%d:%02d:%02d", hh, mm, ss)
    else
        return string.format("%02d:%02d", mm, ss)
    end
end

-- Truncate text to fit within maxWidthPx, suffixing "...". Uses the
-- engine's text-measurement so the result is pixel-accurate rather
-- than guessing by character count. Every candidate cut point is snapped
-- to a complete UTF-8 character boundary (utf8Safe) so a multi-byte
-- character is never split into a dangling lead byte -- string.sub cuts
-- by byte offset, not codepoint.
local function truncateToWidth(text, font, fontSize, maxWidthPx)
    if engine.getTextWidth(font, text, fontSize) <= maxWidthPx then
        return text
    end
    local ellipsis = "..."
    -- Binary search the cut length.
    local lo, hi = 1, #text
    while lo < hi do
        local mid = math.floor((lo + hi + 1) / 2)
        local cut = utf8Safe.snapToCharBoundary(text, mid)
        local candidate = string.sub(text, 1, cut) .. ellipsis
        if engine.getTextWidth(font, candidate, fontSize) <= maxWidthPx then
            lo = mid
        else
            hi = mid - 1
        end
    end
    return string.sub(text, 1, utf8Safe.snapToCharBoundary(text, lo)) .. ellipsis
end

-- Position the visible row labels for the current activeTabKey +
-- scrollOffset. Never touches the scrollbar — safe to call from
-- the scrollbar's onScroll callback without recursing.
renderRows = function()
    -- Clear prior row labels + clickable overlay boxes (scrollbar,
    -- tabbar, panel persist across renders — only the per-event
    -- elements rebuild).
    for _, entry in ipairs(eventLog.rowEntries) do
        for _, lid in ipairs(entry.rowLabels or {}) do label.destroy(lid) end
        if entry.clickBoxHandle then
            UI.deleteElement(entry.clickBoxHandle)
        end
    end
    eventLog.rowEntries    = {}
    eventLog.rowClickBoxes = {}

    local L = eventLog.layout
    if not L then return end

    -- Build a category-id → {displayName, color} lookup for the row
    -- middle column. Read each render rather than caching so adding
    -- a new YAML category and re-opening the panel picks it up.
    local catLookup = {}
    local cfg = engine.getNotificationCfg() or {}
    for _, c in ipairs(cfg) do
        catLookup[c.id] = {
            displayName = c.displayName or c.id,
            color = {
                (c.textColor and c.textColor.r) or 1.0,
                (c.textColor and c.textColor.g) or 1.0,
                (c.textColor and c.textColor.b) or 1.0,
                (c.textColor and c.textColor.a) or 1.0,
            },
        }
    end

    -- Fetch + reverse + filter. engine.getEventLog returns
    -- oldest-first; we want newest at the top. Stash the filtered
    -- list on the module so onRowClick can look up the clicked
    -- event without recomputing.
    local log = engine.getEventLog() or {}
    local filtered = {}
    for i = #log, 1, -1 do
        local ev = log[i]
        if eventLog.activeTabKey == "all"
           or ev.category == eventLog.activeTabKey then
            table.insert(filtered, ev)
        end
    end
    eventLog.filteredEvents = filtered

    local visible = L.visibleRows
    local totalRows = #filtered

    if totalRows == 0 then
        local emptyId = label.new({
            name     = "evlog_empty",
            text     = "(no events for this category)",
            font     = eventLog.font,
            fontSize = eventLog.baseSizes.fontSize,
            color    = {0.6, 0.6, 0.6, 1.0},
            page     = eventLog.pageId,
            uiscale  = L.uiscale,
        })
        UI.addToPage(eventLog.pageId,
            label.getElementHandle(emptyId),
            L.contentX, L.contentY + L.s.fontSize)
        UI.setZIndex(label.getElementHandle(emptyId), 504)
        table.insert(eventLog.rowEntries, { rowLabels = { emptyId } })
        return
    end

    -- Render the visible window of rows. Each column is followed by
    -- a fixed `columnGap` of whitespace so the time / category /
    -- text values can never run flush into the next column.
    local gap     = L.columnGap
    local catX    = L.contentX + L.timeColW + gap
    local textX   = catX + L.catColW + gap
    local textColW = L.contentW - L.timeColW - L.catColW - 2 * gap
    local fontSize = eventLog.baseSizes.fontSize

    for i = 0, visible - 1 do
        local idx = eventLog.scrollOffset + i + 1
        if idx > totalRows then break end
        local ev = filtered[idx]
        local rowY = L.contentY + i * L.rowHeight

        local timeStr = formatGameTime(ev.gameTime)
        local lookup  = catLookup[ev.category]
                        or { displayName = ev.category,
                             color = {1.0, 1.0, 1.0, 1.0} }

        -- Transparent clickable box covering the full row.
        -- Click → onEventLogRowClick → re-pops the popup for this
        -- event. Created before the text labels so the labels
        -- render on top (z-order doesn't affect clickability —
        -- labels aren't clickable themselves, so clicks pass
        -- through to this box).
        local boxName = "evlog_row_click_" .. tostring(idx)
        local clickBox = UI.newBox(
            boxName,
            L.contentW, L.rowHeight,
            eventLog.boxTexSet,
            eventLog.baseSizes.tileSize,
            1.0, 1.0, 1.0, 0.0,   -- fully transparent
            0,
            eventLog.pageId)
        UI.addToPage(eventLog.pageId, clickBox, L.contentX, rowY)
        UI.setClickable(clickBox, true)
        UI.setOnClick(clickBox, EVENT_LOG_ROW_CALLBACK)
        UI.setZIndex(clickBox, 503)
        eventLog.rowClickBoxes[clickBox] = idx

        local rowLabels = {}
        local function place(name, text, x, color)
            local id = label.new({
                name     = name,
                text     = text,
                font     = eventLog.font,
                fontSize = fontSize,
                color    = color,
                page     = eventLog.pageId,
                uiscale  = L.uiscale,
            })
            UI.addToPage(eventLog.pageId,
                label.getElementHandle(id),
                x, rowY + L.s.fontSize)
            UI.setZIndex(label.getElementHandle(id), 504)
            -- Tracked via rowLabels only: renderRows destroys these on
            -- every re-render, so also appending to ownedLabels just
            -- accumulated stale ids for the life of the panel.
            table.insert(rowLabels, id)
        end

        place("evlog_time_" .. tostring(idx),
              truncateToWidth(timeStr, eventLog.font,
                              L.s.fontSize, L.timeColW),
              L.contentX, {0.7, 0.7, 0.7, 1.0})
        place("evlog_cat_"  .. tostring(idx),
              truncateToWidth(lookup.displayName, eventLog.font,
                              L.s.fontSize, L.catColW),
              catX, lookup.color)
        -- Coalesced repeats show "(xN)"; the message takes the category
        -- colour so failures (red unit_warning) stand out, not just the
        -- category label.
        local msg = ev.text or ""
        if (ev.count or 1) > 1 then
            msg = msg .. " (x" .. ev.count .. ")"
        end
        place("evlog_text_" .. tostring(idx),
              truncateToWidth(msg, eventLog.font,
                              L.s.fontSize, textColW),
              textX, lookup.color)

        table.insert(eventLog.rowEntries, {
            rowLabels      = rowLabels,
            clickBoxHandle = clickBox,
        })
    end
end

-- Sync the scrollbar's content size + reset its scroll position,
-- then re-render rows. Used on (a) initial show, (b) tab change.
-- The scrollbar's onScroll callback dispatches to renderRows itself,
-- so we never call it from inside this function — that's what was
-- causing the recursive stack overflow at scrollbar.lua:319.
renderContent = function()
    local L = eventLog.layout
    if not L then return end

    -- Recount filtered events to size the scrollbar accurately.
    local log = engine.getEventLog() or {}
    local totalRows = 0
    for i = 1, #log do
        local ev = log[i]
        if eventLog.activeTabKey == "all"
           or ev.category == eventLog.activeTabKey then
            totalRows = totalRows + 1
        end
    end

    local visible = L.visibleRows
    local maxOffset = math.max(0, totalRows - visible)
    if eventLog.scrollOffset > maxOffset then
        eventLog.scrollOffset = maxOffset
    end

    if eventLog.scrollbarId then
        scrollbar.setContentSize(eventLog.scrollbarId,
            math.max(1, totalRows), visible)
        -- setScrollOffset fires onScroll → renderRows (see createUI).
        -- We don't call renderRows directly here.
        scrollbar.setScrollOffset(eventLog.scrollbarId,
            eventLog.scrollOffset)
    else
        renderRows()
    end
end

-----------------------------------------------------------
-- Lifecycle
-----------------------------------------------------------

-- engine.loadScript hook. Creates the UI page lazily — actual
-- elements get built once fonts are ready via bootstrap.
function eventLog.init(scriptId)
    if eventLog.pageId == nil then
        eventLog.pageId = UI.newPage("event_log", "modal")
    end
end

-- Called from uiManager.ensurePopupsAndLogs() once textures + fonts are
-- loaded. Build the panel chrome but leave it hidden — the player opens it
-- via the HUD log button. boxTex is the panel-style 9-box (used for
-- the panel background); btnTex is the button-style 9-box (used for
-- the close X and any future action buttons inside the panel).
function eventLog.bootstrap(boxTex, btnTex, font, w, h)
    eventLog.boxTexSet = boxTex
    eventLog.btnTexSet = btnTex
    eventLog.font      = font
    eventLog.fbW       = w
    eventLog.fbH       = h
    if eventLog.pageId == nil then
        eventLog.pageId = UI.newPage("event_log", "modal")
    end
    eventLog.bootstrapped = true
end

function eventLog.show()
    if not eventLog.bootstrapped then return end
    -- Rebuild the UI every show so resize / new categories are
    -- picked up. Cheap — only ~9 tabs + a panel + a few labels.
    createUI()
    UI.showPage(eventLog.pageId)
    eventLog.visible = true
end

function eventLog.hide()
    eventLog.visible = false
    if eventLog.pageId then
        UI.hidePage(eventLog.pageId)
    end
end

-- Click dispatch: ui_manager.onEventLogRowClick forwards here when
-- a row's transparent overlay box receives a click. We resolve the
-- elemHandle back to a filteredEvents index, then call the popup
-- module's onShowPopup directly with the same shape the engine
-- broadcasts (so the popup re-spawns at the next free slot with the
-- original category color + coords).
function eventLog.onRowClick(elemHandle)
    local idx = eventLog.rowClickBoxes[elemHandle]
    if not idx then return false end
    local ev = eventLog.filteredEvents[idx]
    if not ev then return false end

    -- Look up the text color from the (possibly-updated) cfg —
    -- if the player toggled a category's color in YAML between
    -- emit and click, the repop reflects the current setting.
    local r, g, b, a = 1.0, 1.0, 1.0, 1.0
    local cfg = engine.getNotificationCfg() or {}
    for _, c in ipairs(cfg) do
        if c.id == ev.category and c.textColor then
            r = c.textColor.r or 1.0
            g = c.textColor.g or 1.0
            b = c.textColor.b or 1.0
            a = c.textColor.a or 1.0
            break
        end
    end

    local popupMod = require("scripts.popup")
    popupMod.onShowPopup(ev.category, ev.text, r, g, b, a, ev.coords)
    return true
end

-- #743: ui_manager routes mouse-WHEEL scroll here (dy>0 up / dy<0
-- down), mirroring combat_log/injury_log_panel/unit_log's onScroll —
-- scrolls when the cursor is over the panel background (now an
-- explicit scroll-capture surface, see createUI) or the scrollbar itself.
function eventLog.onScroll(elemHandle, dx, dy)
    if not (eventLog.visible and eventLog.scrollbarId) then return false end
    local over = false
    for _, id in ipairs(eventLog.ownedPanels) do
        if panel.getBoxHandle(id) == elemHandle then over = true; break end
    end
    if not over and scrollbar.findByElementHandle(elemHandle)
                    == eventLog.scrollbarId then
        over = true
    end
    if not over then return false end
    if dy > 0 then scrollbar.scrollUp(eventLog.scrollbarId)
    elseif dy < 0 then scrollbar.scrollDown(eventLog.scrollbarId) end
    return true
end

function eventLog.toggle()
    if eventLog.visible then
        eventLog.hide()
    else
        eventLog.show()
    end
end

function eventLog.isVisible()
    return eventLog.visible and true or false
end

function eventLog.update(dt)
    -- No per-frame work. Tick interval just keeps the module
    -- registered with the Lua thread.
end

function eventLog.onFramebufferResize(width, height)
    eventLog.fbW = width
    eventLog.fbH = height
    if eventLog.visible and eventLog.uiCreated then
        createUI()
    end
end

function eventLog.shutdown()
    destroyOwned()
    if eventLog.pageId then
        UI.deletePage(eventLog.pageId)
        eventLog.pageId = nil
    end
    eventLog.bootstrapped = false
    eventLog.visible      = false
    eventLog.uiCreated    = false
end

return eventLog
