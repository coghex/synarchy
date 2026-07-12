-- Unit info v2 stats section orchestration (#542 split from
-- unit_info_v2.lua).
--
-- The sub-tab strip (Status / Physical / Mental / Skill / Knowledge)
-- inside the stats section, the per-panel dispatch table, and the
-- rebuild-vs-refresh-in-place decision (panelShapeSig). Panel content
-- itself lives in unit_info_v2_status.lua (Status) and
-- unit_info_v2_panels.lua (the other four).

local unitInfoV2 = package.loaded["scripts.unit_info_v2"]
local hud       = require("scripts.hud")
local label     = require("scripts.ui.label")
local scrollbar = require("scripts.ui.scrollbar")
local injuries  = require("scripts.injuries")
local L         = require("scripts.unit_info_v2_layout")
local status    = require("scripts.unit_info_v2_status")
local panels    = require("scripts.unit_info_v2_panels")

local M = {}

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

-- Sub-tabs inside the stats section. Each entry: {name, bgId, labelId}.
-- The active sub-tab persists across unit switches so comparisons
-- between units stay on the same panel.
unitInfoV2.subTabs        = {}
unitInfoV2.activeSubTab   = "Status"
unitInfoV2.statsRect      = nil   -- whole stats section rect
unitInfoV2.statsContentRect = nil -- below the sub-tab rows
unitInfoV2.statsContentElements = {}  -- elements created by the active panel
unitInfoV2.statsRefresh   = nil   -- per-panel "refresh values" callback
unitInfoV2.lastContentUid = nil   -- (uid, subtab) we last built content for
unitInfoV2.lastContentTab = nil

-----------------------------------------------------------
-- Sub-tab strip
-----------------------------------------------------------

local function applySubTabStyling()
    local selSet   = unitInfoV2.subTabSelectedTexSet
    local unselSet = unitInfoV2.subTabUnselectedTexSet
    for _, t in ipairs(unitInfoV2.subTabs) do
        local isActive = (t.name == unitInfoV2.activeSubTab)
        if selSet and unselSet then
            UI.setBoxTextures(t.bgId, isActive and selSet or unselSet)
        end
        local c = isActive and L.SUB_TAB_SEL_TEXT_COLOR or L.SUB_TAB_TEXT_COLOR
        local lblH = label.getElementHandle(t.labelId)
        UI.setColor(lblH, c[1], c[2], c[3], c[4])
    end
end

-- Create the sub-tab strip inside the stats section. Tabs wrap when
-- the row would exceed the available width — typical layout is two
-- rows of three for six text-labelled tabs. Returns the Y position
-- below the last row so the content area can start there.
function M.rebuildSubTabs()
    if not unitInfoV2.statsRect then return end
    local rect = unitInfoV2.statsRect

    -- Measure each tab text once so wrapping can be planned without
    -- having to instantiate elements first.
    local tabWidths = {}
    for i, name in ipairs(L.SUB_TAB_LIST) do
        local tw = engine.getTextWidth(hud.menuFont, name, L.SUB_TAB_FONT_SIZE)
        tabWidths[i] = math.floor(tw) + 2 * L.SUB_TAB_TEXT_PAD
    end

    local rows = L.planSubTabRows(rect, tabWidths)

    -- Lay each row out centred horizontally within the section.
    local cursorY = rect.y + L.SUB_TAB_TOP_PAD
    for _, r in ipairs(rows) do
        local rowStartX = rect.x + math.floor((rect.w - r.totalW) / 2)
        local cx = rowStartX
        for i = r.startIdx, r.endIdx do
            local name = L.SUB_TAB_LIST[i]
            local tw = tabWidths[i]

            local bgId = UI.newBox(
                "unit_info_v2_subtab_bg_" .. i,
                tw, L.SUB_TAB_ROW_H,
                unitInfoV2.subTabUnselectedTexSet,
                L.SUB_TAB_TILE,
                1.0, 1.0, 1.0, 1.0,
                0,
                unitInfoV2.page
            )
            UI.addToPage(unitInfoV2.page, bgId, cx, cursorY)
            UI.setZIndex(bgId, 11)
            UI.setClickable(bgId, true)
            UI.setOnClick(bgId, "onUnitInfoSubTabClick")

            local lblId = label.new({
                name     = "unit_info_v2_subtab_lbl_" .. i,
                text     = name,
                font     = hud.menuFont,
                fontSize = L.SUB_TAB_FONT_SIZE,
                color    = L.SUB_TAB_TEXT_COLOR,
                page     = unitInfoV2.page,
                uiscale  = 1.0,
            })
            local lblH = label.getElementHandle(lblId)
            local lblW = select(1, label.getSize(lblId))
            local labelX = cx + math.floor((tw - lblW) / 2)
            -- +2 baseline nudge — the 0.3 fontSize offset alone lands
            -- the text visually above center; the extra 2px brings it
            -- onto the box's vertical midline.
            local labelY = cursorY + math.floor(L.SUB_TAB_ROW_H / 2)
                         + math.floor(L.SUB_TAB_FONT_SIZE * 0.3) + 2
            UI.addToPage(unitInfoV2.page, lblH, labelX, labelY)
            UI.setZIndex(lblH, 12)

            unitInfoV2.subTabs[#unitInfoV2.subTabs + 1] = {
                name    = name,
                bgId    = bgId,
                labelId = lblId,
            }
            cx = cx + tw + L.SUB_TAB_GAP
        end
        cursorY = cursorY + L.SUB_TAB_ROW_H + L.SUB_TAB_ROW_GAP
    end

    -- Content area: from below the last row to the bottom of the section.
    unitInfoV2.statsContentRect = {
        x = rect.x,
        y = cursorY + L.SUB_TAB_ROW_GAP,
        w = rect.w,
        h = rect.y + rect.h - (cursorY + L.SUB_TAB_ROW_GAP),
    }

    applySubTabStyling()
end

-----------------------------------------------------------
-- Panel dispatch + rebuild-vs-refresh
-----------------------------------------------------------

local PANEL_BUILDERS = {
    Status    = status.buildStatusPanel,
    Physical  = panels.buildPhysicalPanel,
    Mental    = panels.buildMentalPanel,
    Skill     = panels.buildSkillPanel,
    Knowledge = panels.buildKnowledgePanel,
}

-- Per-panel "what's its current shape?" hash. The signature includes
-- scrollOffset so onScroll-driven changes trigger a rebuild without
-- the per-tick rebuild storm we'd get if we keyed only on (uid, tab).
-- For static panels the row set is fixed per (uid, tab); for the
-- Skill panel we include the skill-name set (NOT values — values get
-- pushed in-place by statsRefresh).
local function panelShapeSig(panel, uid)
    local base
    if panel == "Skill" then
        local all = unit.getAllSkills(uid) or {}
        local names = {}
        for n, _ in pairs(all) do names[#names + 1] = n end
        table.sort(names)
        base = "skill:" .. table.concat(names, "|")
    elseif panel == "Knowledge" then
        -- The row SET changes only when a knowledge is learned/lost (a key
        -- appears/disappears); trained levels are pushed in-place by the
        -- refresh closure, so they don't force a rebuild.
        local names = {}
        for _, e in ipairs(unit.getKnowledgeList(uid) or {}) do
            names[#names + 1] = e.name
        end
        table.sort(names)
        base = "knowledge:" .. table.concat(names, "|")
    elseif panel == "Status" then
        -- The Status row SET changes when a condition appears/clears or
        -- an injury is gained/healed, so its signature tracks both (the
        -- vitals' live numbers are pushed in-place and don't rebuild).
        -- Severity is bucketed (~0.1) so passive healing only rebuilds on
        -- a visible step, not every tick.
        local parts = {}
        for _, c in ipairs(status.unitConditions(uid)) do
            parts[#parts + 1] = "c:" .. c.name
        end
        for _, inj in ipairs(injuries.list(uid)) do
            parts[#parts + 1] = string.format("i:%s:%s:%d:%s:%d",
                inj.kind or "?", inj.part or "?",
                math.floor((inj.severity or 0) * 10),
                inj.dressing or "",
                math.floor((inj.necrosis or 0) * 10))
        end
        -- Infections: their own rows; bucket the level (~0.1) so passive
        -- growth/clearance only rebuilds on a visible step.
        for _, inf in ipairs(injuries.infectionList(uid)) do
            parts[#parts + 1] = string.format("f:%s:%s:%d",
                inf.part or "?", inf.infectionType or inf.name or "?",
                math.floor((inf.level or 0) * 10))
        end
        -- Scars change the row set when one appears (a wound healed out).
        for _, sc in ipairs(injuries.scarList(uid)) do
            parts[#parts + 1] = "s:" .. (sc.kind or "?") .. ":" .. (sc.part or "?")
        end
        -- Immunities: bucket the level so the slow decay doesn't rebuild every
        -- tick, but a new immunity / a visible step does.
        for _, im in ipairs(unit.getImmunities(uid) or {}) do
            parts[#parts + 1] = string.format("m:%s:%d",
                im.type or "?", math.floor((im.level or 0) * 10))
        end
        base = "status:" .. table.concat(parts, "|")
    else
        base = "static"
    end
    return base .. ":" .. tostring(unitInfoV2.statsScrollOffset or 0)
end

-- Clears the current panel's elements and builds the new one for
-- (activeUid, activeSubTab). Cheap when the active selection hasn't
-- changed because we cache (uid, subtab) and only refresh values via
-- the panel's refresh callback.
function M.rebuildStatsContent()
    if not unitInfoV2.statsContentRect then return end
    local uid = unitInfoV2.activeUid
    if not uid then return end

    local panel = unitInfoV2.activeSubTab
    local tabChanged = unitInfoV2.lastContentTab ~= panel
    local uidChanged = unitInfoV2.lastContentUid ~= uid

    -- Tab / uid change → tear down persistent elements (scrollbar +
    -- bg click box) so the next build creates a fresh pair sized
    -- for the new panel. Reset scrollOffset so we start at the top.
    if tabChanged or uidChanged then
        if unitInfoV2.statsScrollbarId then
            scrollbar.destroy(unitInfoV2.statsScrollbarId)
            unitInfoV2.statsScrollbarId = nil
        end
        if unitInfoV2.statsBgClickId then
            UI.deleteElement(unitInfoV2.statsBgClickId)
            unitInfoV2.statsBgClickId = nil
        end
        unitInfoV2.statsScrollOffset = 0
    end

    local sig = panelShapeSig(panel, uid)
    local sameContext = (not tabChanged) and (not uidChanged)
                      and unitInfoV2.lastContentSig == sig
    if sameContext then return end

    -- Clear ROWS (not scrollbar, not bg box — those persist across
    -- same-tab rebuilds so an in-flight drag isn't broken).
    for _, e in ipairs(unitInfoV2.statsContentElements) do
        if e.kind == "label" then label.destroy(e.id)
        elseif e.kind == "scrollbar" then scrollbar.destroy(e.id)
        else                       UI.deleteElement(e.id)
        end
    end
    unitInfoV2.statsContentElements = {}
    unitInfoV2.statsRefresh = nil

    local builder = PANEL_BUILDERS[panel]
    if not builder then return end
    unitInfoV2.statsRefresh = builder(unitInfoV2.statsContentRect, uid)
    unitInfoV2.lastContentUid = uid
    unitInfoV2.lastContentTab = panel
    unitInfoV2.lastContentSig = sig
end

-- Full teardown for rebuildLayout / shutdown: the sub-tab strip AND
-- the current panel's content (rows + scrollbar + bg-click box).
function M.clearAll()
    for _, t in ipairs(unitInfoV2.subTabs) do
        if t.labelId then label.destroy(t.labelId) end
        if t.bgId    then UI.deleteElement(t.bgId)  end
    end
    unitInfoV2.subTabs = {}

    for _, e in ipairs(unitInfoV2.statsContentElements) do
        if e.kind == "label" then label.destroy(e.id)
        elseif e.kind == "scrollbar" then scrollbar.destroy(e.id)
        else                       UI.deleteElement(e.id)
        end
    end
    unitInfoV2.statsContentElements = {}
    -- Persistent stat-panel elements (survive same-tab rebuilds —
    -- see rebuildStatsContent — so we tear them down explicitly on
    -- full clearOwned). Safe to call destroy with nil refs.
    if unitInfoV2.statsScrollbarId then
        scrollbar.destroy(unitInfoV2.statsScrollbarId)
        unitInfoV2.statsScrollbarId = nil
    end
    if unitInfoV2.statsBgClickId then
        UI.deleteElement(unitInfoV2.statsBgClickId)
        unitInfoV2.statsBgClickId = nil
    end
    unitInfoV2.statsRefresh = nil
    unitInfoV2.lastContentUid = nil
    unitInfoV2.lastContentTab = nil
    unitInfoV2.lastContentSig = nil
    unitInfoV2.statsScrollOffset = 0
end

-----------------------------------------------------------
-- Click / scroll callbacks (routed via ui_manager)
-----------------------------------------------------------

-- Clicking a sub-tab (Status / Stats / Mental / …) switches which
-- panel content is visible. The active sub-tab persists across unit
-- switches so comparisons stay on the same panel.
function unitInfoV2.handleSubTabClick(elemHandle)
    for _, t in ipairs(unitInfoV2.subTabs) do
        if t.bgId == elemHandle then
            if unitInfoV2.activeSubTab ~= t.name then
                unitInfoV2.activeSubTab = t.name
                applySubTabStyling()
                M.rebuildStatsContent()
            end
            return true
        end
    end
    return false
end

-- Routed by uiManager.onScrollUp / onScrollDown. The stats-panel
-- scrollbar's up/down arrow buttons fire those callbacks when
-- clicked; we resolve the handle back to our scrollbar id and
-- forward to the widget's scrollUp/scrollDown helpers (which
-- update the offset + fire the onScroll callback, which in turn
-- bumps statsScrollOffset → next rebuild redraws the visible
-- slice).
function unitInfoV2.handleScrollCallback(callbackName, elemHandle)
    local sbId = unitInfoV2.statsScrollbarId
    if not sbId then return false end
    local foundId, _ = scrollbar.findByElementHandle(elemHandle)
    if foundId ~= sbId then return false end
    if callbackName == "onScrollUp" then
        scrollbar.scrollUp(sbId)
        return true
    elseif callbackName == "onScrollDown" then
        scrollbar.scrollDown(sbId)
        return true
    end
    return false
end

-- Mouse wheel over the stats panel or the scrollbar itself moves
-- the visible window one row per notch. Routed by uiManager.onUIScroll.
-- elemHandle is whatever was under the cursor at the wheel event;
-- we accept it if it belongs to our scrollbar or to any element we
-- currently own in the stats content area.
function unitInfoV2.onScroll(elemHandle, dx, dy)
    local sbId = unitInfoV2.statsScrollbarId
    if not sbId then return false end

    -- Wheel is "ours" if cursor is on the scrollbar, the bg click
    -- box, or any of the currently-displayed row elements.
    local isOurs = false
    if elemHandle == unitInfoV2.statsBgClickId then
        isOurs = true
    elseif scrollbar.findByElementHandle(elemHandle) == sbId then
        isOurs = true
    else
        for _, e in ipairs(unitInfoV2.statsContentElements) do
            if e.kind == "label" then
                if label.getElementHandle(e.id) == elemHandle then
                    isOurs = true; break
                end
            elseif e.kind ~= "scrollbar" then
                if e.id == elemHandle then
                    isOurs = true; break
                end
            end
        end
    end
    if not isOurs then return false end

    if     dy > 0 then scrollbar.scrollUp(sbId)
    elseif dy < 0 then scrollbar.scrollDown(sbId)
    end
    return true
end

return M
