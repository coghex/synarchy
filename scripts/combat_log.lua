-- Combat Log Panel
--
-- Sibling to event_log.lua. Owned by the HUD's log-mode picker
-- (right-click on the event-log icon → choose Combat Log). Shows a
-- "All" tab (RAM-only FIFO of the latest 200 events, never persisted)
-- plus one tab per ongoing/closed battle. Per-battle backlog lives in
-- JSONL files under saves/<world>/combat_logs/battle_NNNN.jsonl —
-- this skeleton phase doesn't load or write them yet.
--
-- This phase ships the empty chrome: panel, pinned All tab, fixed-
-- width tab strip with < / > scroll buttons (both greyed since there
-- are no battles), and an empty content area. The data path that
-- drains combat.drainEvents() and groups by battle lands in step 4+.

local scale       = require("scripts.ui.scale")
local panel       = require("scripts.ui.panel")
local label       = require("scripts.ui.label")
local button      = require("scripts.ui.button")
local boxTextures = require("scripts.ui.box_textures")

-- Singleton (matches event_log so engine.loadScript and require
-- both reach the same table; see gotcha_dofile_module_state).
local combatLog = package.loaded["scripts.combat_log"] or {}
package.loaded["scripts.combat_log"] = combatLog

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

combatLog.pageId       = combatLog.pageId       or nil
combatLog.boxTexSet    = combatLog.boxTexSet    or nil
combatLog.btnTexSet    = combatLog.btnTexSet    or nil
-- 9-box texture sets for tab cells. Loaded lazily on bootstrap so
-- they share cache entries with the tabbar widget (event_log uses
-- the same assets).
combatLog.tabSelTexSet   = combatLog.tabSelTexSet   or nil
combatLog.tabUnselTexSet = combatLog.tabUnselTexSet or nil
combatLog.font         = combatLog.font         or nil
combatLog.fbW          = combatLog.fbW          or 0
combatLog.fbH          = combatLog.fbH          or 0
combatLog.bootstrapped = combatLog.bootstrapped or false
combatLog.visible      = combatLog.visible      or false
combatLog.uiCreated    = combatLog.uiCreated    or false

-- Ownership split into "chrome" (panel + title + close — survives
-- tab/scroll re-renders) and "transient" (tab labels, scroll arrows,
-- content rows — rebuilt by renderContent). Keeping them separate
-- means a tab click doesn't have to re-spawn the title every frame.
combatLog.chromeLabels  = combatLog.chromeLabels  or {}
combatLog.chromePanels  = combatLog.chromePanels  or {}
combatLog.chromeButtons = combatLog.chromeButtons or {}

combatLog.tabLabels  = combatLog.tabLabels  or {}
combatLog.tabButtons = combatLog.tabButtons or {}

-- Tab-click overlay boxes: elemHandle → battleId ("all" or numeric).
combatLog.tabClickBoxes = combatLog.tabClickBoxes or {}

-- "All" tab: in-RAM ring buffer, capped at ALL_RING_CAP. Never
-- persisted. Battle tabs: one entry per (open or recently-closed)
-- battle in the order they opened. The All tab is always rendered
-- first and is not part of the battles table.
local ALL_RING_CAP = 200
-- Battle quiescence: if a battle hasn't seen an event for this many
-- seconds, mark it inactive (red → grey).
local BATTLE_QUIESCENCE_SEC = 15
combatLog.allEvents = combatLog.allEvents or {}   -- {ev, ev, ...}
-- Each battle: { id, name, active, participants={[uid]=true,...},
--                events={ev,...}, lastEventAt }
combatLog.battles      = combatLog.battles      or {}
combatLog.nextBattleId = combatLog.nextBattleId or 1
combatLog.activeTabId  = combatLog.activeTabId or "all"
combatLog.scrollOffset = combatLog.scrollOffset or 0  -- battle-tab strip scroll
-- Set to true by event-processing when the active tab's contents
-- changed, so update() can decide whether to repaint.
combatLog.dirty        = combatLog.dirty        or false

-- Cached geometry (the renderer reads this; createUI writes it).
combatLog.layout = combatLog.layout or {}

-- Click-callback names routed by ui_manager.
local COMBAT_LOG_TAB_CALLBACK  = "onCombatLogTabClick"
local COMBAT_LOG_PREV_CALLBACK = "onCombatLogScrollPrev"
local COMBAT_LOG_NEXT_CALLBACK = "onCombatLogScrollNext"

-----------------------------------------------------------
-- Sizing constants (base, before uiscale)
-----------------------------------------------------------

combatLog.baseSizes = {
    fontSize     = 18,
    titleSize    = 28,
    titleBarH    = 48,
    closeBtnSize = 36,
    padX         = 24,
    padY         = 20,
    rowHeight    = 22,
    -- Fixed-width tabs: every battle tab and the All tab use the
    -- same 80px slot so the strip looks like a tape, not a flowing
    -- tabbar. tabTileSize matches the tabbar widget's default so
    -- the 9-box corners of tabselected / tabunselected render at
    -- the same scale as event_log's tabs.
    tabWidth     = 80,
    tabHeight    = 36,
    tabFontSize  = 14,
    tabTileSize  = 16,
    tabGap       = 4,
    -- Scroll buttons that bracket the battle-tab strip.
    arrowWidth   = 28,
    arrowHeight  = 36,
    -- 12-char display cap per design; truncateTabName enforces.
    tabNameMax   = 12,
    tileSize     = 64,
    panelFrac    = 0.85,
}

-- Active battles get a saturated-red tab; closed battles use the
-- normal greyish color. The All tab is always white.
local COLOR_ACTIVE = { 1.0, 0.30, 0.30, 1.0 }
local COLOR_CLOSED = { 0.85, 0.85, 0.85, 1.0 }
local COLOR_ALL    = { 1.0, 1.0, 1.0, 1.0 }
local COLOR_DIM    = { 0.45, 0.45, 0.45, 1.0 }  -- greyed scroll arrow

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------

local function destroyChrome()
    for _, id in ipairs(combatLog.chromeLabels)  do label.destroy(id)  end
    for _, id in ipairs(combatLog.chromeButtons) do button.destroy(id) end
    for _, id in ipairs(combatLog.chromePanels)  do panel.destroy(id)  end
    combatLog.chromeLabels  = {}
    combatLog.chromeButtons = {}
    combatLog.chromePanels  = {}
end

local function destroyTransient()
    for _, id in ipairs(combatLog.tabLabels)  do label.destroy(id)  end
    for _, id in ipairs(combatLog.tabButtons) do button.destroy(id) end
    for handle, _ in pairs(combatLog.tabClickBoxes) do
        UI.deleteElement(handle)
    end
    combatLog.tabLabels     = {}
    combatLog.tabButtons    = {}
    combatLog.tabClickBoxes = {}
end

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- Hard char cap, suffixing "..." when truncated. Battle tab names
-- come in as "HH:MM Name" — truncating by character (not pixel) is
-- fine because the font is roughly fixed-width at 14pt and the 80px
-- slot comfortably fits 12 chars plus ellipsis.
local function truncateTabName(text)
    text = text or ""
    local maxLen = combatLog.baseSizes.tabNameMax
    if #text <= maxLen then return text end
    return string.sub(text, 1, maxLen - 3) .. "..."
end

-- Max scroll offset for the battle strip: how many tabs fit, vs.
-- how many we have. Returned in tab units (not pixels).
local function maxScrollOffset()
    local L = combatLog.layout
    if not L or L.battleStripVisible == nil then return 0 end
    local total = #combatLog.battles
    return math.max(0, total - L.battleStripVisible)
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
    local s = scale.applyAllWith(combatLog.baseSizes, uiscale)

    local panelW = math.floor(combatLog.fbW * combatLog.baseSizes.panelFrac)
    local panelH = math.floor(combatLog.fbH * combatLog.baseSizes.panelFrac)
    local panelX = math.floor((combatLog.fbW - panelW) / 2)
    local panelY = math.floor((combatLog.fbH - panelH) / 2)

    local panelId = panel.new({
        name       = "combat_log_panel",
        page       = combatLog.pageId,
        x          = panelX,
        y          = panelY,
        width      = panelW,
        height     = panelH,
        textureSet = combatLog.boxTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = combatLog.baseSizes.tileSize,
        zIndex     = 500,
        padding    = { top = s.padY, bottom = s.padY,
                       left = s.padX, right = s.padX },
        uiscale    = 1.0,
    })
    table.insert(combatLog.chromePanels, panelId)

    -- Title (top-left).
    local titleId = label.new({
        name     = "combat_log_title",
        text     = "Combat Log",
        font     = combatLog.font,
        fontSize = combatLog.baseSizes.titleSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = combatLog.pageId,
        uiscale  = uiscale,
    })
    table.insert(combatLog.chromeLabels, titleId)
    UI.addToPage(combatLog.pageId, label.getElementHandle(titleId),
        panelX + s.padX, panelY + s.padY + s.titleSize)
    UI.setZIndex(label.getElementHandle(titleId), 502)

    -- Close X (top-right).
    local closeBtnId = button.new({
        name       = "combat_log_close",
        page       = combatLog.pageId,
        x          = panelX + panelW - s.padX - s.closeBtnSize,
        y          = panelY + s.padY,
        width      = combatLog.baseSizes.closeBtnSize,
        height     = combatLog.baseSizes.closeBtnSize,
        fontSize   = combatLog.baseSizes.fontSize,
        font       = combatLog.font,
        text       = "X",
        textColor  = {1.0, 1.0, 1.0, 1.0},
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textureSet = combatLog.btnTexSet or combatLog.boxTexSet,
        onClick    = function() combatLog.hide() end,
        uiscale    = uiscale,
        zIndex     = 502,
    })
    table.insert(combatLog.chromeButtons, closeBtnId)

    -- Tab strip geometry:
    --   [All tab][gap][< arrow][gap][battle tabs strip][gap][> arrow]
    --
    -- The battle-tab strip is the slack region between the two
    -- arrows; visible tab count == floor(stripW / (tabW + gap)).
    local tabY        = panelY + s.titleBarH
    local allTabX     = panelX + s.padX
    local prevArrowX  = allTabX + s.tabWidth + s.tabGap
    local nextArrowX  = panelX + panelW - s.padX - s.arrowWidth
    local stripX      = prevArrowX + s.arrowWidth + s.tabGap
    local stripW      = nextArrowX - stripX - s.tabGap
    local battleStripVisible = math.max(0,
        math.floor((stripW + s.tabGap) / (s.tabWidth + s.tabGap)))

    -- Content area below the tabs.
    local contentX = panelX + s.padX
    local contentY = tabY + s.tabHeight + s.padY
    local contentW = panelW - 2 * s.padX
    local contentH = (panelY + panelH - s.padY) - contentY

    combatLog.layout = {
        s                  = s,
        uiscale            = uiscale,
        tabY               = tabY,
        allTabX            = allTabX,
        prevArrowX         = prevArrowX,
        nextArrowX         = nextArrowX,
        stripX             = stripX,
        stripW             = stripW,
        battleStripVisible = battleStripVisible,
        contentX           = contentX,
        contentY           = contentY,
        contentW           = contentW,
        contentH           = contentH,
    }

    renderContent()

    combatLog.uiCreated = true
end

-----------------------------------------------------------
-- Event ingestion
-----------------------------------------------------------

-- Format game time as HH:MM:SS for tab names and row prefixes.
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

-- Display name for a uid. Falls back to `unit_<uid>` if the engine
-- doesn't know the unit (it might be already dead/despawned by the
-- time we render).
local function displayName(uid)
    if not uid then return "?" end
    local info = unit.getInfo(uid)
    if info and info.defName then
        -- Capitalise: "acolyte" → "Acolyte"
        local name = info.defName
        return name:sub(1, 1):upper() .. name:sub(2)
    end
    return "unit_" .. tostring(uid)
end

-- Try to find a battle that contains either participant. Returns
-- the battle table, or nil.
local function findBattle(atk, tgt)
    for _, b in ipairs(combatLog.battles) do
        if (atk and b.participants[atk])
           or (tgt and b.participants[tgt]) then
            return b
        end
    end
    return nil
end

-- Make a new battle for the given (attacker, target) at game time.
-- Name is "HH:MM <attacker>" or fallback "<defender>" or UNKNOWN.
local function newBattle(atk, tgt, gameTime)
    local id = combatLog.nextBattleId
    combatLog.nextBattleId = id + 1
    local primary = atk or tgt
    local who = primary and displayName(primary) or "UNKNOWN"
    -- 12-char cap on the participant slug; truncateTabName adds
    -- ellipsis if needed.
    local slug = who
    if #slug > 8 then slug = slug:sub(1, 7) .. "…" end
    local baseName = formatGameTimeHM(gameTime) .. " " .. slug
    -- Disambiguate same-time-same-name collisions with (2), (3)…
    local name = baseName
    local n = 2
    local function nameTaken(nm)
        for _, b in ipairs(combatLog.battles) do
            if b.name == nm then return true end
        end
        return false
    end
    while nameTaken(name) do
        name = baseName .. " (" .. n .. ")"
        n = n + 1
    end
    local b = {
        id          = id,
        name        = name,
        active      = true,
        participants = {},
        events      = {},
        lastEventAt = gameTime,
    }
    if atk then b.participants[atk] = true end
    if tgt then b.participants[tgt] = true end
    table.insert(combatLog.battles, b)
    return b
end

-- Push an event into the All-tab ring (capped) and the appropriate
-- battle. Marks the active tab dirty when the new event affects it.
local function processEvent(ev)
    -- All-tab ring, newest-first.
    table.insert(combatLog.allEvents, 1, ev)
    while #combatLog.allEvents > ALL_RING_CAP do
        table.remove(combatLog.allEvents)
    end

    local atk = ev.attacker
    local tgt = ev.target
    local b = findBattle(atk, tgt)
    if b then
        -- Re-activate if quiescence had closed it.
        b.active = true
        if atk then b.participants[atk] = true end
        if tgt then b.participants[tgt] = true end
    else
        b = newBattle(atk, tgt, ev.ts or 0)
    end
    table.insert(b.events, 1, ev)
    b.lastEventAt = ev.ts or 0

    -- Active tab affected?
    if combatLog.activeTabId == "all"
       or combatLog.activeTabId == b.id then
        combatLog.dirty = true
    end
end

-- Returns the event list the active tab currently displays.
function combatLog.activeTabEvents()
    if combatLog.activeTabId == "all" then
        return combatLog.allEvents
    end
    for _, b in ipairs(combatLog.battles) do
        if b.id == combatLog.activeTabId then
            return b.events
        end
    end
    return {}
end

-- Format one event for display. Returns (text, color).
function combatLog.formatEvent(ev)
    local ts = formatGameTimeHMS(ev.ts or 0)
    local atk = ev.attacker and displayName(ev.attacker) or "?"
    local tgt = ev.target   and displayName(ev.target)   or "?"
    local payload = ev.payload or {}
    if ev.kind == "miss" then
        return string.format("[%s] %s's swing misses %s",
            ts, atk, tgt), {0.7, 0.7, 0.7, 1.0}
    elseif ev.kind == "hit" then
        local part = payload.part or "?"
        local kind = payload.kind or "blow"
        local sev  = tonumber(payload.severity) or 0
        local sevWord =
              (sev < 0.25) and "light"
           or (sev < 0.50) and "moderate"
           or (sev < 0.75) and "severe"
           or                   "critical"
        -- Hand-roll the present-tense verb per kind. Naive
        -- pluralization (kind .. "es") gave "stabes"; better to
        -- be explicit.
        local verb = "hits"
        if kind == "slash" then verb = "slashes"
        elseif kind == "stab" then verb = "stabs"
        elseif kind == "blunt" then verb = "bashes" end
        return string.format("[%s] %s %s %s's %s — %s wound",
            ts, atk, verb, tgt, part, sevWord),
            {1.0, 1.0, 1.0, 1.0}
    elseif ev.kind == "death" then
        local cause = payload.cause or "unknown"
        return string.format("[%s] %s dies, killed by %s",
            ts, tgt, cause), {1.0, 0.4, 0.4, 1.0}
    end
    -- Unknown kind: dump raw.
    return string.format("[%s] %s %s %s", ts, atk, ev.kind, tgt),
        {0.7, 0.7, 0.7, 1.0}
end

-----------------------------------------------------------
-- Tab strip + content rendering
-----------------------------------------------------------

-- Spawn one tab cell at (x, tabY). The 9-box uses tabselected when
-- this tab matches activeTabId, tabunselected otherwise — same
-- assets the tabbar widget uses, so the combat log's hand-rolled
-- tabs visually match event_log's tabbar. The transparent click
-- overlay sits on top of the box; the label sits on top of both.
local function spawnTab(name, displayText, x, color, battleId)
    local L = combatLog.layout
    local s = L.s
    local isSel = (battleId == combatLog.activeTabId)
    local tabTex = isSel and combatLog.tabSelTexSet
                          or combatLog.tabUnselTexSet
    local clickBox = UI.newBox(
        name .. "_box",
        s.tabWidth, s.tabHeight,
        tabTex,
        s.tabTileSize,
        1.0, 1.0, 1.0, 1.0,
        0,
        combatLog.pageId)
    UI.addToPage(combatLog.pageId, clickBox, x, L.tabY)
    UI.setClickable(clickBox, true)
    UI.setOnClick(clickBox, COMBAT_LOG_TAB_CALLBACK)
    UI.setZIndex(clickBox, 503)
    combatLog.tabClickBoxes[clickBox] = battleId

    local lbl = label.new({
        name     = name .. "_label",
        text     = displayText,
        font     = combatLog.font,
        fontSize = combatLog.baseSizes.tabFontSize,
        color    = color,
        page     = combatLog.pageId,
        uiscale  = L.uiscale,
    })
    local labelW = engine.getTextWidth(combatLog.font, displayText,
                                       s.tabFontSize)
    local labelX = x + math.floor((s.tabWidth - labelW) / 2)
    local labelY = L.tabY + math.floor((s.tabHeight + s.tabFontSize) / 2)
    UI.addToPage(combatLog.pageId, label.getElementHandle(lbl),
        labelX, labelY)
    UI.setZIndex(label.getElementHandle(lbl), 504)
    table.insert(combatLog.tabLabels, lbl)
end

-- Spawn one scroll arrow ("<" or ">"). Greyed when there's nothing
-- to scroll in that direction.
local function spawnArrow(name, glyph, x, callbackName, greyed)
    local L = combatLog.layout
    local color = greyed and COLOR_DIM or {1.0, 1.0, 1.0, 1.0}
    local btnId = button.new({
        name       = name,
        page       = combatLog.pageId,
        x          = x,
        y          = L.tabY,
        width      = combatLog.baseSizes.arrowWidth,
        height     = combatLog.baseSizes.arrowHeight,
        fontSize   = combatLog.baseSizes.tabFontSize,
        font       = combatLog.font,
        text       = glyph,
        textColor  = color,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textureSet = combatLog.btnTexSet or combatLog.boxTexSet,
        callbackName = callbackName,
        uiscale    = L.uiscale,
        zIndex     = 503,
    })
    table.insert(combatLog.tabButtons, btnId)
end

renderContent = function()
    destroyTransient()
    local L = combatLog.layout
    if not L then return end
    local s = L.s

    -- All tab (pinned leftmost, never scrolled).
    spawnTab("combat_log_tab_all", "All", L.allTabX, COLOR_ALL, "all")

    -- Scroll arrows. Greyed when nothing to scroll in that direction.
    local maxOff = maxScrollOffset()
    spawnArrow("combat_log_scroll_prev", "<",
               L.prevArrowX, COMBAT_LOG_PREV_CALLBACK,
               combatLog.scrollOffset <= 0)
    spawnArrow("combat_log_scroll_next", ">",
               L.nextArrowX, COMBAT_LOG_NEXT_CALLBACK,
               combatLog.scrollOffset >= maxOff)

    -- Battle tabs (skeleton: none yet, so this loop is a no-op).
    for i = 1, L.battleStripVisible do
        local idx = combatLog.scrollOffset + i
        local b = combatLog.battles[idx]
        if not b then break end
        local x = L.stripX + (i - 1) * (s.tabWidth + s.tabGap)
        local color = b.active and COLOR_ACTIVE or COLOR_CLOSED
        spawnTab("combat_log_tab_" .. tostring(b.id),
                 truncateTabName(b.name), x, color, b.id)
    end

    -- Content: pick the active tab's event list and render one
    -- line per event, newest-first, capped to what fits in the
    -- panel (no scrollbar in 2.1 — Phase 2.2 polish).
    local events = combatLog.activeTabEvents()
    if #events == 0 then
        local emptyText = "No combat events yet."
        local emptyW = engine.getTextWidth(combatLog.font, emptyText,
                                           s.fontSize)
        local emptyId = label.new({
            name     = "combat_log_empty",
            text     = emptyText,
            font     = combatLog.font,
            fontSize = combatLog.baseSizes.fontSize,
            color    = {0.6, 0.6, 0.6, 1.0},
            page     = combatLog.pageId,
            uiscale  = L.uiscale,
        })
        table.insert(combatLog.tabLabels, emptyId)
        UI.addToPage(combatLog.pageId,
            label.getElementHandle(emptyId),
            L.contentX + math.floor((L.contentW - emptyW) / 2),
            L.contentY + math.floor(L.contentH / 2))
        UI.setZIndex(label.getElementHandle(emptyId), 504)
        return
    end

    -- Render newest-first lines, top to bottom.
    local lineH = s.rowHeight
    local maxLines = math.max(1, math.floor(L.contentH / lineH))
    for i = 1, math.min(maxLines, #events) do
        local ev = events[i]
        local text, color = combatLog.formatEvent(ev)
        local lbl = label.new({
            name     = "combat_log_row_" .. i,
            text     = text,
            font     = combatLog.font,
            fontSize = combatLog.baseSizes.fontSize,
            color    = color,
            page     = combatLog.pageId,
            uiscale  = L.uiscale,
        })
        table.insert(combatLog.tabLabels, lbl)
        UI.addToPage(combatLog.pageId, label.getElementHandle(lbl),
            L.contentX,
            L.contentY + (i - 1) * lineH + s.fontSize)
        UI.setZIndex(label.getElementHandle(lbl), 504)
    end
end

-----------------------------------------------------------
-- Click dispatch (routed via ui_manager)
-----------------------------------------------------------

function combatLog.onTabClick(elemHandle)
    local battleId = combatLog.tabClickBoxes[elemHandle]
    if battleId == nil then return false end
    combatLog.activeTabId = battleId
    renderContent()
    return true
end

function combatLog.onScrollPrev()
    if combatLog.scrollOffset > 0 then
        combatLog.scrollOffset = combatLog.scrollOffset - 1
        renderContent()
    end
    return true
end

function combatLog.onScrollNext()
    local maxOff = maxScrollOffset()
    if combatLog.scrollOffset < maxOff then
        combatLog.scrollOffset = combatLog.scrollOffset + 1
        renderContent()
    end
    return true
end

-----------------------------------------------------------
-- Lifecycle
-----------------------------------------------------------

function combatLog.init(scriptId)
    if combatLog.pageId == nil then
        combatLog.pageId = UI.newPage("combat_log", "modal")
    end
end

function combatLog.bootstrap(boxTex, btnTex, font, w, h)
    combatLog.boxTexSet = boxTex
    combatLog.btnTexSet = btnTex
    combatLog.font      = font
    combatLog.fbW       = w
    combatLog.fbH       = h
    -- Cache-hit if the tabbar widget already loaded these (it does,
    -- via ui_manager initializing tabbar before any panels bootstrap).
    combatLog.tabSelTexSet =
        boxTextures.load("assets/textures/ui/tabselected", "tabselected")
    combatLog.tabUnselTexSet =
        boxTextures.load("assets/textures/ui/tabunselected", "tabunselected")
    if combatLog.pageId == nil then
        combatLog.pageId = UI.newPage("combat_log", "modal")
    end
    combatLog.bootstrapped = true
end

function combatLog.show()
    if not combatLog.bootstrapped then return end
    createUI()
    UI.showPage(combatLog.pageId)
    combatLog.visible = true
end

function combatLog.hide()
    combatLog.visible = false
    if combatLog.pageId then
        UI.hidePage(combatLog.pageId)
    end
end

function combatLog.toggle()
    if combatLog.visible then
        combatLog.hide()
    else
        combatLog.show()
    end
end

function combatLog.isVisible()
    return combatLog.visible and true or false
end

-- Drain new combat events, route to All-ring + per-battle lists,
-- expire quiescent battles, repaint when the active tab changed.
-- Called from the Lua thread at the script's tick interval (0.1s).
function combatLog.update(dt)
    -- Drain events even when the panel is hidden — the All-ring
    -- and battle list need to keep up so they're correct when the
    -- player opens the panel.
    local events = combat.drainEvents() or {}
    for _, ev in ipairs(events) do
        processEvent(ev)
    end

    -- Battle quiescence: any battle whose last event is more than
    -- BATTLE_QUIESCENCE_SEC ago becomes inactive (visual: tab text
    -- flips from red to grey).
    local now = engine.gameTime()
    local hadActiveChange = false
    for _, b in ipairs(combatLog.battles) do
        if b.active and (now - (b.lastEventAt or 0))
                        > BATTLE_QUIESCENCE_SEC then
            b.active = false
            hadActiveChange = true
        end
    end

    -- Re-render only when (a) the panel is visible and (b) the
    -- active tab actually changed contents (new event) or a tab's
    -- active flag flipped (which changes its color).
    if combatLog.visible and combatLog.uiCreated
       and (combatLog.dirty or hadActiveChange) then
        combatLog.dirty = false
        renderContent()
    end
end

function combatLog.onFramebufferResize(width, height)
    combatLog.fbW = width
    combatLog.fbH = height
    if combatLog.visible and combatLog.uiCreated then
        createUI()
    end
end

function combatLog.shutdown()
    destroyChrome()
    destroyTransient()
    combatLog.visible   = false
    combatLog.uiCreated = false
end

return combatLog
