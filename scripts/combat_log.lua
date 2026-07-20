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
local scrollbar   = require("scripts.ui.scrollbar")

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
-- Battle "closed for good": a battle silent longer than this can no
-- longer absorb a new event, so it won't be re-joined. This guards the
-- unit-id-REUSE hazard — the engine reassigns ids on destroy/spawn, and
-- without this a brand-new unit that inherits a dead one's id would get
-- its events glued onto the dead unit's long-quiescent battle. Set well
-- beyond any real combat lull so a paused-then-resumed fight still
-- rejoins; only stale-id collisions fall past it.
local BATTLE_REJOIN_MAX_SEC = 120
combatLog.allEvents = combatLog.allEvents or {}   -- {ev, ev, ...}
-- Each battle: { id, name, active, participants={[uid]=true,...},
--                events={ev,...}, lastEventAt }
combatLog.battles      = combatLog.battles      or {}
combatLog.nextBattleId = combatLog.nextBattleId or 1
combatLog.activeTabId  = combatLog.activeTabId or "all"
combatLog.scrollOffset = combatLog.scrollOffset or 0  -- battle-tab strip scroll
combatLog.tabMaxScroll = combatLog.tabMaxScroll or 0  -- set by renderContent
-- Content (history) scroll: index of the top visible WRAPPED LINE. The
-- history scrollbar drives it; `justifyBottom` snaps it to the newest line
-- on every new event (chat-style auto-follow). `syncingScrollbar` guards
-- the renderer's setScrollOffset call from re-entering onContentScroll.
combatLog.contentScroll    = combatLog.contentScroll    or 0
combatLog.justifyBottom    = true
combatLog.syncingScrollbar = false
combatLog.scrollbarId      = combatLog.scrollbarId      or nil
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
    tabWidth     = 80,    -- minimum tab width; tabs auto-size up to fit text
    tabPad       = 10,    -- horizontal padding inside an auto-sized tab
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
    if combatLog.scrollbarId then
        scrollbar.destroy(combatLog.scrollbarId)
        combatLog.scrollbarId = nil
    end
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

-- Max scroll offset for the battle strip (tab units). Computed by
-- renderContent from the variable tab widths; 0 when everything fits.
local function maxScrollOffset()
    return combatLog.tabMaxScroll or 0
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
    -- #743: explicit scroll-capture + pointer-block, replacing the old
    -- setClickable(true)-with-no-callback attempt (which never actually
    -- worked — wheel routing required a click callback too) so a mouse
    -- wheel over the content routes to combatLog.onScroll, and blank
    -- panel space doesn't leak clicks through to gameplay underneath.
    pcall(function()
        local boxHandle = panel.getBoxHandle(panelId)
        UI.setScrollCapture(boxHandle, true)
        UI.setPointerBlocking(boxHandle, true)
    end)

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

    -- Tab strip: the All tab is pinned left; battle tabs flow after it,
    -- each auto-sized to its text. The < / > scroll arrows appear only
    -- when the tabs overflow (renderContent does the variable layout).
    local closeBtnX = panelX + panelW - s.padX - s.closeBtnSize
    local tabY        = panelY + s.titleBarH
    local allTabX     = panelX + s.padX
    local tabStripRight = closeBtnX - s.tabGap   -- don't run under the close X

    -- Content area below the tabs, inset by `textPad` on all four sides for
    -- breathing room, with a reserved column on the right for the history
    -- scrollbar so the wrapped text never runs under it.
    local textPad   = math.floor(10 * uiscale)
    local sbBtnSize = math.floor(18 * uiscale)
    local sbCapH    = math.floor(4 * uiscale)
    local sbGap     = math.floor(8 * uiscale)
    local contentX     = panelX + s.padX + textPad
    local contentY     = tabY + s.tabHeight + s.padY + textPad
    local contentRight = panelX + panelW - s.padX - textPad
    local contentH     = (panelY + panelH - s.padY - textPad) - contentY
    local regionW      = contentRight - contentX
    local contentW     = regionW - sbBtnSize - sbGap   -- text width sans bar

    combatLog.layout = {
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

    -- History scrollbar, pinned to the right edge of the content area.
    -- Content size + visibility are set in renderContent (they depend on
    -- the wrapped-line count). Buttons (onScrollUp/Down) + draggable tab
    -- route through ui_manager → combatLog.handleScrollCallback.
    local sbTrackH = math.max(math.floor(12 * uiscale),
                              contentH - 2 * sbBtnSize - 2 * sbCapH)
    combatLog.scrollbarId = scrollbar.new({
        name         = "combat_log_scrollbar",
        page         = combatLog.pageId,
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
        onScroll     = function(offset) combatLog.onContentScroll(offset) end,
    })
    scrollbar.setVisible(combatLog.scrollbarId, false)

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
-- Per-species combat-log display name. Wildlife reads with the definite
-- article ("the brown bear"); named/class units capitalise their defName
-- ("Acolyte"). Extend freely — an absent entry falls back to capitalising
-- the defName.
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
        -- Mapped wildlife reads with the definite article in prose
        -- ("the brown bear"); anything else uses the def's species label
        -- (display_name / prettified def name) so unmapped units don't
        -- surface as a raw "Red_squirrel" (#264).
        local mapped = info.defName and DISPLAY_NAMES[info.defName]
        if mapped then return mapped end
        if info.displayName and info.displayName ~= "" then
            return info.displayName
        end
    end
    return "unit_" .. tostring(uid)
end

-- Try to find a battle that contains either participant. Returns
-- the battle table, or nil. Battles silent longer than
-- BATTLE_REJOIN_MAX_SEC are skipped: they're considered closed, so a
-- reused unit id can't get its events merged into a dead unit's battle.
local function findBattle(atk, tgt)
    local now = engine.gameTime()
    for _, b in ipairs(combatLog.battles) do
        if (now - (b.lastEventAt or 0)) <= BATTLE_REJOIN_MAX_SEC
           and ((atk and b.participants[atk])
                or (tgt and b.participants[tgt])) then
            return b
        end
    end
    return nil
end

-- Make a new battle for the given (attacker, target) at game time.
-- Name is "HH:MM <attacker>" or fallback "<defender>" or UNKNOWN.
-- Short, capitalised unit name for a battle tab ("Bear", "Acolyte") —
-- distinct from the content narration's displayName ("the brown bear").
local TAB_SHORT_NAMES = {
    bear_brown = "Bear",
    technomule = "Mule",
}
local function tabUnitName(uid)
    if not uid then return nil end
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

local function newBattle(atk, tgt, gameTime)
    local id = combatLog.nextBattleId
    combatLog.nextBattleId = id + 1
    -- "HH:MM Attacker v Target" (or just one name if a solo event).
    local an, tn = tabUnitName(atk), tabUnitName(tgt)
    local who
    if an and tn then who = an .. " v " .. tn
    else who = an or tn or "Unknown" end
    local baseName = formatGameTimeHM(gameTime) .. " " .. who
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

    -- Active tab affected? Repaint, and snap the history scroll to the
    -- bottom so the newest event is always in view (chat-style follow).
    if combatLog.activeTabId == "all"
       or combatLog.activeTabId == b.id then
        combatLog.dirty = true
        combatLog.justifyBottom = true
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
    -- Uppercase the first letter of a sentence (so a leading "the brown
    -- bear" reads "The brown bear …").
    local function up1(s) return (s:gsub("^%l", string.upper)) end
    if ev.kind == "miss" then
        local missText
        if payload.dodge then
            -- The blow was on target; the defender saw it and slipped it.
            missText = payload.lunge
                and string.format("%s leaps at %s, who slips aside", atk, tgt)
                or  string.format("%s dodges %s's attack", tgt, atk)
        elseif payload.lunge then
            missText = string.format("%s lunges at %s but fails to connect", atk, tgt)
        else
            missText = string.format("%s's swing misses %s", atk, tgt)
        end
        return string.format("[%s] %s", ts, up1(missText)),
            {0.7, 0.7, 0.7, 1.0}
    elseif ev.kind == "hit" then
        -- Rich, clinical per-layer narration (scripts/injury_log.lua).
        local injuryLog = require("scripts.injury_log")
        return string.format("[%s] %s", ts,
            up1(injuryLog.hitLine(atk, tgt, payload))),
            {1.0, 1.0, 1.0, 1.0}
    elseif ev.kind == "death" then
        -- Report the most immediate plausible cause among the corpse's
        -- injuries (scripts/injuries.lua deathCause), falling back to the
        -- engine's raw cause payload.
        local injuries = require("scripts.injuries")
        local cause = injuries.deathCause(ev.target) or payload.cause
        -- An empty cause means something didn't tag the death — surface it
        -- as a mystery rather than swallowing it. Reads in-fiction AND
        -- flags a bug to investigate.
        local line
        if cause and cause ~= "" then
            line = string.format("%s has died from %s", tgt, cause)
        else
            line = string.format("%s has died under mysterious circumstances", tgt)
        end
        return string.format("[%s] %s", ts, up1(line)), {1.0, 0.4, 0.4, 1.0}
    end
    -- Unknown kind: dump raw.
    return string.format("[%s] %s %s %s", ts, atk, ev.kind, tgt),
        {0.7, 0.7, 0.7, 1.0}
end

-- Rendered combat-log entries involving `uid` (attacker OR target), for
-- the per-unit log panel. Returns { {ts, text, color}, ... } newest-first
-- (allEvents is already newest-first).
function combatLog.unitEntries(uid)
    local out = {}
    if not uid then return out end
    for _, ev in ipairs(combatLog.allEvents) do
        if ev.attacker == uid or ev.target == uid then
            local text, color = combatLog.formatEvent(ev)
            out[#out + 1] = { ts = ev.ts or 0, text = text, color = color }
        end
    end
    return out
end

-----------------------------------------------------------
-- Tab strip + content rendering
-----------------------------------------------------------

-- Spawn one tab cell at (x, tabY). The 9-box uses tabselected when
-- this tab matches activeTabId, tabunselected otherwise — same
-- assets the tabbar widget uses, so the combat log's hand-rolled
-- tabs visually match event_log's tabbar. The transparent click
-- overlay sits on top of the box; the label sits on top of both.
-- Pixel width of a tab sized to fit `text` (label width + horizontal
-- padding), floored at a sensible minimum.
local function tabPixelWidth(text, s)
    local w = engine.getTextWidth(combatLog.font, text, s.tabFontSize)
    return math.max(s.tabWidth, w + 2 * s.tabPad)
end

local function spawnTab(name, displayText, x, color, battleId, width)
    local L = combatLog.layout
    local s = L.s
    width = width or s.tabWidth
    local isSel = (battleId == combatLog.activeTabId)
    local tabTex = isSel and combatLog.tabSelTexSet
                          or combatLog.tabUnselTexSet
    local clickBox = UI.newBox(
        name .. "_box",
        width, s.tabHeight,
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
    local labelX = x + math.floor((width - labelW) / 2)
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

-- Word-wrap `text` to fit `maxW` pixels at the given font/size. Breaks on
-- spaces; hard-splits a single word wider than the panel. Returns a list of
-- line strings (always at least one).
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
                -- A single word wider than the panel: hard-break it.
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
    local L = combatLog.layout
    if not L then return end
    local s = L.s

    -- All tab (pinned leftmost), auto-sized to its label.
    local allW = tabPixelWidth("All", s)
    spawnTab("combat_log_tab_all", "All", L.allTabX, COLOR_ALL, "all", allW)

    -- Battle tabs: auto-sized to "HH:MM Atk v Tgt", flowing after the All
    -- tab. The < / > scroll arrows appear ONLY when they overflow the strip.
    local battles    = combatLog.battles
    local nB         = #battles
    local stripLeft  = L.allTabX + allW + s.tabGap
    local stripRight = L.tabStripRight

    local widths = {}
    for i = 1, nB do widths[i] = tabPixelWidth(battles[i].name, s) end
    local function rangeWidth(a, b)
        local w = 0
        for i = a, b do w = w + widths[i] + (i > a and s.tabGap or 0) end
        return w
    end

    local spawnBattle = function(i, x)
        local b = battles[i]
        spawnTab("combat_log_tab_" .. tostring(b.id), b.name, x,
                 b.active and COLOR_ACTIVE or COLOR_CLOSED, b.id, widths[i])
    end

    if nB == 0 or stripLeft + rangeWidth(1, nB) <= stripRight then
        -- Everything fits — no arrows.
        combatLog.scrollOffset = 0
        combatLog.tabMaxScroll = 0
        local x = stripLeft
        for i = 1, nB do
            spawnBattle(i, x)
            x = x + widths[i] + s.tabGap
        end
    else
        -- Overflow — reserve arrow columns and scroll WHOLE tabs.
        local availLeft  = stripLeft + s.arrowWidth + s.tabGap
        local availRight = stripRight - s.arrowWidth - s.tabGap
        local avail      = availRight - availLeft
        -- Largest offset that still shows the final tab (tail flush).
        local start, w = nB, widths[nB]
        while start > 1 and (w + s.tabGap + widths[start - 1]) <= avail do
            start = start - 1
            w = w + s.tabGap + widths[start]
        end
        combatLog.tabMaxScroll = start - 1
        combatLog.scrollOffset =
            math.max(0, math.min(combatLog.scrollOffset, combatLog.tabMaxScroll))

        spawnArrow("combat_log_scroll_prev", "<", stripLeft,
                   COMBAT_LOG_PREV_CALLBACK, combatLog.scrollOffset <= 0)
        spawnArrow("combat_log_scroll_next", ">", availRight + s.tabGap,
                   COMBAT_LOG_NEXT_CALLBACK,
                   combatLog.scrollOffset >= combatLog.tabMaxScroll)

        local x = availLeft
        for i = combatLog.scrollOffset + 1, nB do
            if x + widths[i] > availRight then break end
            spawnBattle(i, x)
            x = x + widths[i] + s.tabGap
        end
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

    -- Build wrapped lines in CHRONOLOGICAL order (oldest→newest) so the
    -- newest event sits at the BOTTOM — the way a human reads a log — and
    -- each long sentence wraps to the panel width instead of running off.
    local lineH = s.rowHeight
    local maxLines = math.max(1, math.floor(L.contentH / lineH))
    local wrapped = {}   -- { {text=, color=}, … } chronological
    for i = #events, 1, -1 do
        local text, color = combatLog.formatEvent(events[i])
        for _, wl in ipairs(wrapText(text, L.contentW, combatLog.font,
                                     s.fontSize)) do
            wrapped[#wrapped + 1] = { text = wl, color = color }
        end
    end

    -- Resolve the scroll window. `justifyBottom` (set on every new event)
    -- snaps to the newest line; otherwise the user's scroll position holds
    -- (clamped to the current content). Keep the scrollbar in sync — guarded
    -- so its onScroll callback doesn't re-enter this render.
    local total     = #wrapped
    local maxOffset = math.max(0, total - maxLines)
    if combatLog.justifyBottom then
        combatLog.contentScroll = maxOffset
        combatLog.justifyBottom = false
    else
        combatLog.contentScroll =
            math.max(0, math.min(combatLog.contentScroll, maxOffset))
    end
    if combatLog.scrollbarId then
        combatLog.syncingScrollbar = true
        scrollbar.setContentSize(combatLog.scrollbarId,
                                 math.max(total, 1), maxLines)
        scrollbar.setScrollOffset(combatLog.scrollbarId, combatLog.contentScroll)
        scrollbar.setVisible(combatLog.scrollbarId, total > maxLines)
        combatLog.syncingScrollbar = false
    end

    -- Draw the visible window, top→bottom (newest at the bottom when at the
    -- foot of the log).
    local row = 0
    for i = combatLog.contentScroll + 1,
            math.min(combatLog.contentScroll + maxLines, total) do
        row = row + 1
        local w = wrapped[i]
        local lbl = label.new({
            name     = "combat_log_row_" .. row,
            text     = w.text,
            font     = combatLog.font,
            fontSize = combatLog.baseSizes.fontSize,
            color    = w.color,
            page     = combatLog.pageId,
            uiscale  = L.uiscale,
        })
        table.insert(combatLog.tabLabels, lbl)
        UI.addToPage(combatLog.pageId, label.getElementHandle(lbl),
            L.contentX,
            L.contentY + (row - 1) * lineH + s.fontSize)
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

-- History scrollbar moved (button / drag / wheel). Guarded so the
-- renderer's own setScrollOffset sync doesn't recurse back into a render.
function combatLog.onContentScroll(offset)
    if combatLog.syncingScrollbar then return end
    offset = math.floor(offset or 0)
    if offset == combatLog.contentScroll then return end
    combatLog.contentScroll = offset
    renderContent()
end

-- ui_manager routes scrollbar up/down BUTTON clicks here.
function combatLog.handleScrollCallback(callbackName, elemHandle)
    if not combatLog.scrollbarId then return false end
    local sbId = scrollbar.findByElementHandle(elemHandle)
    if sbId ~= combatLog.scrollbarId then return false end
    if callbackName == "onScrollUp" then
        scrollbar.scrollUp(sbId); return true
    elseif callbackName == "onScrollDown" then
        scrollbar.scrollDown(sbId); return true
    end
    return false
end

-- ui_manager routes mouse-WHEEL scroll here (dy>0 up / dy<0 down). Scrolls
-- when the cursor is over the panel or the scrollbar itself.
function combatLog.onScroll(elemHandle, dx, dy)
    if not (combatLog.visible and combatLog.scrollbarId) then return false end
    local over = false
    for _, id in ipairs(combatLog.chromePanels) do
        if panel.getBoxHandle(id) == elemHandle then over = true; break end
    end
    if not over and scrollbar.findByElementHandle(elemHandle)
                    == combatLog.scrollbarId then
        over = true
    end
    if not over then return false end
    if dy > 0 then scrollbar.scrollUp(combatLog.scrollbarId)
    elseif dy < 0 then scrollbar.scrollDown(combatLog.scrollbarId) end
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
    -- #750: a 0x0 minimize must not become the stored geometry or trigger
    -- a rebuild against a degenerate framebuffer; keep the last valid
    -- fbW/fbH and skip rebuilding, same as C2's responsive.notifyResize
    -- guard for menu screens.
    if width <= 0 or height <= 0 then return end
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
