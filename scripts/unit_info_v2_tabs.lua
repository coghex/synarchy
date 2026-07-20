-- Unit info v2 multi-unit sprite tab strip (#542 split from unit_info_v2.lua).
--
-- One tab per selected unit across the top of the pane: portrait
-- sprite + shaped active-tab backdrop, with scroll arrows when the
-- row doesn't fit. Owns the tab-strip half of the module's shared
-- state (unitInfoV2.tabs / activeUid / scrollOffset / lastSelKey /
-- tabsRect / tabLayout / scrollLeftId / scrollRightId /
-- ownedScrollLbls) via the singleton table, same convention as every
-- other unit_info_v2_*.lua submodule.

local unitInfoV2 = package.loaded["scripts.unit_info_v2"]
local hud   = require("scripts.hud")
local label = require("scripts.ui.label")

local M = {}

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

-- Sprite tabs: one entry per selected unit, in selection order.
-- Each entry: { uid, boxId, spriteId, lastTex }.
unitInfoV2.tabs           = {}
unitInfoV2.activeUid      = nil   -- whose info the lower sections show
unitInfoV2.scrollOffset   = 0     -- how many tabs scrolled past on the left
unitInfoV2.lastSelKey     = ""    -- stable identity of the current selection
unitInfoV2.tabsRect       = nil   -- { x, y, w, h } of the tab strip area
unitInfoV2.tabLayout      = nil   -- last result of computeTabLayout
unitInfoV2.scrollLeftId   = nil
unitInfoV2.scrollRightId  = nil
unitInfoV2.ownedScrollLbls = {}   -- arrow text labels (label.* IDs)

-----------------------------------------------------------
-- Layout constants (base size; multiplied by uiscale)
-----------------------------------------------------------

local TAB_GAP          = 4    -- horizontal gap between adjacent tabs
local TAB_INNER_PAD    = 4    -- padding inside a tab around its sprite
local TAB_TOP_PAD      = 12   -- extra space between section top and the tab row
local TAB_BOTTOM_PAD   = 4    -- space between the tab row and the bottom of the strip
local ARROW_W          = 24   -- scroll-arrow button width
local ARROW_GAP        = 4    -- gap between arrows and tab strip
local ARROW_PANEL_INSET = 12  -- extra inset so arrows sit visibly inside the pane's 9-patch border
-- Active tab's backdrop is a shaped sprite (assets/.../unittabselected.png).
-- Inactive tabs hide the backdrop entirely (alpha 0). The portrait
-- sprite is the click target and renders on top.
local ACTIVE_BG      = { 1.0, 1.0, 1.0, 1.0 }   -- show the texture untinted
local INACTIVE_BG    = { 1.0, 1.0, 1.0, 0.0 }
local ACTIVE_TINT    = { 1.0, 1.0, 1.0, 1.0 }
local INACTIVE_TINT  = { 1.0, 1.0, 1.0, 1.0 }
-- Arrows are click-only — no visible background. The chevron label
-- supplies the visual.
local ARROW_BG       = { 1.0, 1.0, 1.0, 0.0 }

-----------------------------------------------------------
-- Multi-unit sprite tabs
-----------------------------------------------------------

-- Stable identity for a selection — used to detect when the tab
-- strip needs rebuilding. Order-insensitive (sort uids first) so
-- box-select order quirks don't trigger spurious rebuilds.
function M.selectionKey(sel)
    if not sel or #sel == 0 then return "" end
    local copy = {}
    for _, u in ipairs(sel) do copy[#copy + 1] = u end
    table.sort(copy)
    return table.concat(copy, ",")
end

-- Compute tab dimensions and visibility for N units in a strip of
-- `contentW` × `tabH`. Tabs divide the available width up to their
-- square size; beyond that, they stay square and the surplus tabs
-- scroll. Returns:
--   { tabW, tabH, fitsAll, visibleCount, scrollAreaX, scrollAreaW }
-- where scrollAreaX is the local offset where the visible row starts
-- (0 when no scroll, ARROW_W + ARROW_GAP when scroll arrows present).
local function computeTabLayout(n, contentW, tabH, gap)
    if n <= 0 then return nil end
    local tabSquareW = tabH
    local naturalW = math.floor((contentW - (n - 1) * gap) / n)
    if naturalW >= tabSquareW then
        return {
            tabW         = naturalW,
            tabH         = tabH,
            fitsAll      = true,
            visibleCount = n,
            scrollAreaX  = 0,
            scrollAreaW  = contentW,
        }
    end
    -- Arrows are inset from the pane edge so they don't sit on top
    -- of the 9-patch border. The total "arrow column" on each side
    -- consumes the inset + the arrow width + the gap to the tab row.
    local arrowColumn = ARROW_PANEL_INSET + ARROW_W + ARROW_GAP
    local availableW = math.max(0, contentW - 2 * arrowColumn)
    local vis = math.floor((availableW + gap) / (tabSquareW + gap))
    vis = math.max(1, math.min(vis, n))
    return {
        tabW         = tabSquareW,
        tabH         = tabH,
        fitsAll      = false,
        visibleCount = vis,
        scrollAreaX  = arrowColumn,
        scrollAreaW  = availableW,
    }
end
M.computeTabLayout = computeTabLayout

-- Position / show / hide every tab + arrow based on activeUid and
-- scrollOffset. Called after rebuildTabs and after any state change
-- that affects visibility (tab click, scroll click).
local function applyTabPositions()
    if not unitInfoV2.tabsRect or not unitInfoV2.tabLayout then return end
    local rect    = unitInfoV2.tabsRect
    local layout  = unitInfoV2.tabLayout
    local tabW    = layout.tabW
    local tabH    = layout.tabH
    local visible = layout.visibleCount
    local nTabs   = #unitInfoV2.tabs
    if nTabs == 0 then return end

    -- Top-biased vertical placement so the active tab's white box
    -- has visible space above it (rather than sitting right against
    -- the pane's 9-patch border).
    local tabY = rect.y + TAB_TOP_PAD

    -- Row centring within the visible / scroll-area.
    local rowW       = visible * tabW + (visible - 1) * TAB_GAP
    local rowStartX  = rect.x + layout.scrollAreaX
                     + math.floor((layout.scrollAreaW - rowW) / 2)

    local startIdx = unitInfoV2.scrollOffset + 1
    local endIdx   = math.min(startIdx + visible - 1, nTabs)

    -- Both box and sprite are sprite-sized and stacked at the same
    -- centered position within each tab cell. Box is purely visual
    -- (white when active, transparent otherwise); sprite is the
    -- portrait and the click target.
    local spriteSize = math.min(tabW, tabH) - 2 * TAB_INNER_PAD
    for i, tab in ipairs(unitInfoV2.tabs) do
        if i >= startIdx and i <= endIdx then
            local visIdx = i - startIdx
            local tx = rowStartX + visIdx * (tabW + TAB_GAP)
            local sx = tx + math.floor((tabW - spriteSize) / 2)
            local sy = tabY + math.floor((tabH - spriteSize) / 2)
            UI.setVisible(tab.boxId,    true)
            UI.setVisible(tab.spriteId, true)
            UI.setSize(tab.boxId,    spriteSize, spriteSize)
            UI.setSize(tab.spriteId, spriteSize, spriteSize)
            UI.setPosition(tab.boxId,    sx, sy)
            UI.setPosition(tab.spriteId, sx, sy)
            if tab.uid == unitInfoV2.activeUid then
                UI.setColor(tab.boxId, ACTIVE_BG[1], ACTIVE_BG[2],
                                       ACTIVE_BG[3], ACTIVE_BG[4])
                UI.setColor(tab.spriteId, ACTIVE_TINT[1], ACTIVE_TINT[2],
                                          ACTIVE_TINT[3], ACTIVE_TINT[4])
            else
                UI.setColor(tab.boxId, INACTIVE_BG[1], INACTIVE_BG[2],
                                       INACTIVE_BG[3], INACTIVE_BG[4])
                UI.setColor(tab.spriteId, INACTIVE_TINT[1], INACTIVE_TINT[2],
                                          INACTIVE_TINT[3], INACTIVE_TINT[4])
            end
        else
            UI.setVisible(tab.boxId,    false)
            UI.setVisible(tab.spriteId, false)
        end
    end

    -- Scroll arrows — visible only when there's somewhere to scroll
    -- in that direction. Each chevron label is shown / hidden in
    -- lockstep with its arrow background.
    local maxOffset = math.max(0, nTabs - visible)
    if unitInfoV2.scrollLeftId then
        local atStart = unitInfoV2.scrollOffset <= 0
        UI.setVisible(unitInfoV2.scrollLeftId, not atStart)
        UI.setPosition(unitInfoV2.scrollLeftId, rect.x + ARROW_PANEL_INSET, tabY)
        UI.setSize(unitInfoV2.scrollLeftId, ARROW_W, tabH)
        UI.setColor(unitInfoV2.scrollLeftId,
            ARROW_BG[1], ARROW_BG[2], ARROW_BG[3], ARROW_BG[4])
        if unitInfoV2.ownedScrollLbls[1] then
            label.setVisible(unitInfoV2.ownedScrollLbls[1], not atStart)
        end
    end
    if unitInfoV2.scrollRightId then
        local atEnd = unitInfoV2.scrollOffset >= maxOffset
        local arrowX = rect.x + rect.w - ARROW_W - ARROW_PANEL_INSET
        UI.setVisible(unitInfoV2.scrollRightId, not atEnd)
        UI.setPosition(unitInfoV2.scrollRightId, arrowX, tabY)
        UI.setSize(unitInfoV2.scrollRightId, ARROW_W, tabH)
        UI.setColor(unitInfoV2.scrollRightId,
            ARROW_BG[1], ARROW_BG[2], ARROW_BG[3], ARROW_BG[4])
        if unitInfoV2.ownedScrollLbls[2] then
            label.setVisible(unitInfoV2.ownedScrollLbls[2], not atEnd)
        end
    end
end
M.applyTabPositions = applyTabPositions

-- Wipe tabs + arrows so rebuildTabs can repopulate without leaks.
local function clearTabs()
    for _, t in ipairs(unitInfoV2.tabs) do
        if t.spriteId then UI.deleteElement(t.spriteId) end
        if t.boxId    then UI.deleteElement(t.boxId)    end
    end
    unitInfoV2.tabs = {}
    if unitInfoV2.scrollLeftId then
        UI.deleteElement(unitInfoV2.scrollLeftId)
        unitInfoV2.scrollLeftId = nil
    end
    if unitInfoV2.scrollRightId then
        UI.deleteElement(unitInfoV2.scrollRightId)
        unitInfoV2.scrollRightId = nil
    end
    for _, lblId in ipairs(unitInfoV2.ownedScrollLbls) do
        label.destroy(lblId)
    end
    unitInfoV2.ownedScrollLbls = {}
end
M.clearTabs = clearTabs

-- Build tab elements for the current selection. Preserves activeUid
-- across rebuilds when the previously-active unit is still selected;
-- otherwise falls back to the first selected unit.
function M.rebuildTabs(sel)
    clearTabs()
    if not sel or #sel == 0 then return end
    if not unitInfoV2.tabsRect then return end

    local rect   = unitInfoV2.tabsRect
    local tabH   = rect.h - TAB_TOP_PAD - TAB_BOTTOM_PAD
    local layout = computeTabLayout(#sel, rect.w, tabH, TAB_GAP)
    if not layout then return end
    unitInfoV2.tabLayout = layout

    -- Selection-identity change always resets the active tab to the
    -- first unit. Previously-active state across selection changes
    -- caused the highlight to land on whichever tab the carried-over
    -- unit happened to occupy (often a middle one).
    unitInfoV2.activeUid = sel[1]

    -- Reset scroll offset (the previous offset isn't meaningful for
    -- a new selection composition).
    unitInfoV2.scrollOffset = 0

    -- Tab elements: a sprite-sized shaped backdrop that becomes
    -- visible when active, plus the unit portrait on top. The
    -- portrait sprite is the click target — sizing the click area to
    -- the visible sprite (rather than the full divided-tab cell) means
    -- the visual and the hit area agree, which matters most for the
    -- wide tabs of 1- or 2-unit selections.
    local spriteSize = math.min(layout.tabW, layout.tabH) - 2 * TAB_INNER_PAD
    local backdropTex = unitInfoV2.tabSelectedTex or unitInfoV2.whitePixelTex
    for i, uid in ipairs(sel) do
        local boxId = UI.newSprite(
            "unit_info_v2_tab_box_" .. i,
            spriteSize, spriteSize,
            backdropTex,
            INACTIVE_BG[1], INACTIVE_BG[2], INACTIVE_BG[3], INACTIVE_BG[4],
            unitInfoV2.page
        )
        UI.addToPage(unitInfoV2.page, boxId, 0, 0)
        UI.setZIndex(boxId, 11)

        local spriteId = UI.newSprite(
            "unit_info_v2_tab_sprite_" .. i,
            spriteSize, spriteSize,
            0,                                 -- texture filled on next update tick
            INACTIVE_TINT[1], INACTIVE_TINT[2],
            INACTIVE_TINT[3], INACTIVE_TINT[4],
            unitInfoV2.page
        )
        UI.addToPage(unitInfoV2.page, spriteId, 0, 0)
        UI.setZIndex(spriteId, 12)
        UI.setClickable(spriteId, true)
        UI.setOnClick(spriteId, "onUnitInfoTabClick")

        unitInfoV2.tabs[#unitInfoV2.tabs + 1] = {
            uid      = uid,
            boxId    = boxId,
            spriteId = spriteId,
            lastTex  = 0,
        }
    end

    -- Scroll arrows when the row doesn't fit. Plain coloured boxes
    -- with a "<" / ">" label on top; clickable.
    if not layout.fitsAll then
        unitInfoV2.scrollLeftId = UI.newSprite(
            "unit_info_v2_scroll_left",
            ARROW_W, tabH,
            unitInfoV2.whitePixelTex,
            ARROW_BG[1], ARROW_BG[2], ARROW_BG[3], ARROW_BG[4],
            unitInfoV2.page
        )
        UI.addToPage(unitInfoV2.page, unitInfoV2.scrollLeftId, 0, 0)
        UI.setZIndex(unitInfoV2.scrollLeftId, 12)
        UI.setClickable(unitInfoV2.scrollLeftId, true)
        UI.setOnClick(unitInfoV2.scrollLeftId, "onUnitInfoScrollLeft")

        unitInfoV2.scrollRightId = UI.newSprite(
            "unit_info_v2_scroll_right",
            ARROW_W, tabH,
            unitInfoV2.whitePixelTex,
            ARROW_BG[1], ARROW_BG[2], ARROW_BG[3], ARROW_BG[4],
            unitInfoV2.page
        )
        UI.addToPage(unitInfoV2.page, unitInfoV2.scrollRightId, 0, 0)
        UI.setZIndex(unitInfoV2.scrollRightId, 12)
        UI.setClickable(unitInfoV2.scrollRightId, true)
        UI.setOnClick(unitInfoV2.scrollRightId, "onUnitInfoScrollRight")

        -- Arrow chevrons. Labels are positioned roughly in the
        -- middle of each arrow box; centred by eye since label.getSize
        -- gives the text width.
        local leftLbl = label.new({
            name     = "unit_info_v2_scroll_left_chev",
            text     = "<",
            font     = hud.menuFont,
            fontSize = 20,
            color    = {0.9, 0.9, 0.9, 1.0},
            page     = unitInfoV2.page,
            uiscale  = 1.0,
        })
        unitInfoV2.ownedScrollLbls[#unitInfoV2.ownedScrollLbls + 1] = leftLbl
        local lH = label.getElementHandle(leftLbl)
        UI.addToPage(unitInfoV2.page, lH, 0, 0)
        UI.setZIndex(lH, 13)

        local rightLbl = label.new({
            name     = "unit_info_v2_scroll_right_chev",
            text     = ">",
            font     = hud.menuFont,
            fontSize = 20,
            color    = {0.9, 0.9, 0.9, 1.0},
            page     = unitInfoV2.page,
            uiscale  = 1.0,
        })
        unitInfoV2.ownedScrollLbls[#unitInfoV2.ownedScrollLbls + 1] = rightLbl
        local rH = label.getElementHandle(rightLbl)
        UI.addToPage(unitInfoV2.page, rH, 0, 0)
        UI.setZIndex(rH, 13)
    end

    applyTabPositions()

    -- Reposition the chevron labels now that arrow positions exist.
    if not layout.fitsAll then
        local tabY = rect.y + TAB_TOP_PAD
        local baselineOffset = math.floor(tabH * 0.5) + math.floor(20 * 0.35)
        local function placeChev(lblId, arrowX)
            local h = label.getElementHandle(lblId)
            local w = select(1, label.getSize(lblId))
            UI.setPosition(h,
                arrowX + math.floor((ARROW_W - w) / 2),
                tabY + baselineOffset)
        end
        placeChev(unitInfoV2.ownedScrollLbls[1], rect.x + ARROW_PANEL_INSET)
        placeChev(unitInfoV2.ownedScrollLbls[2],
                  rect.x + rect.w - ARROW_W - ARROW_PANEL_INSET)
    end
end

-- Restore a previously-active tab + scroll offset after rebuildTabs
-- reset both to the first unit / 0 (see rebuildTabs's own comment —
-- that reset is correct for a genuine selection-identity change). uid
-- is restored only when still present among the freshly-built tabs;
-- the scroll offset is clamped to whatever the new layout supports.
local function restoreActiveTab(uid, scrollOffset)
    if not unitInfoV2.tabLayout or #unitInfoV2.tabs == 0 then return end
    if uid then
        for _, t in ipairs(unitInfoV2.tabs) do
            if t.uid == uid then
                unitInfoV2.activeUid = uid
                break
            end
        end
    end
    local maxOffset = math.max(0,
        #unitInfoV2.tabs - unitInfoV2.tabLayout.visibleCount)
    unitInfoV2.scrollOffset = math.max(0, math.min(scrollOffset or 0, maxOffset))
    applyTabPositions()
end

-- #750 round-17 review: unitInfoV2.reflow() rebuilds the tab strip for
-- the SAME (unchanged) selection after a layout-only resize destroys
-- it — a plain rebuildTabs() call there would lose the player's active
-- tab/scroll via its own genuine-selection-change reset. Captures and
-- restores both around the rebuild, and updates lastSelKey so the next
-- update() tick doesn't redundantly rebuild again.
function M.reflowSelection()
    local savedActiveUid = unitInfoV2.activeUid
    local savedScrollOffset = unitInfoV2.scrollOffset
    local sel = unit.getSelected()
    M.rebuildTabs(sel)
    unitInfoV2.lastSelKey = M.selectionKey(sel)
    restoreActiveTab(savedActiveUid, savedScrollOffset)
end

-- Refresh every visible tab's portrait. Prefer the unit def's
-- authored portrait (static), falling back to the live animation
-- frame for defs that ship no `portrait:`. Skip setSpriteTexture
-- when the handle hasn't changed to avoid needless mutations — for
-- an authored portrait this means it's set once and then left
-- alone. We only refresh VISIBLE tabs — scrolled-off tabs would
-- just thrash invisibly.
function M.refreshPortraits()
    if not (unitInfoV2.tabLayout and #unitInfoV2.tabs > 0) then return end
    local visible = unitInfoV2.tabLayout.visibleCount
    local first   = unitInfoV2.scrollOffset + 1
    local last    = math.min(first + visible - 1, #unitInfoV2.tabs)
    for i = first, last do
        local tab = unitInfoV2.tabs[i]
        if tab then
            local tex = unit.getPortraitTexture(tab.uid)
            if not tex or tex == 0 then
                tex = unit.getFrameTexture(tab.uid)
            end
            if tex and tex > 0 and tex ~= tab.lastTex then
                UI.setSpriteTexture(tab.spriteId, tex)
                tab.lastTex = tex
            end
        end
    end
end

-----------------------------------------------------------
-- Click callbacks (routed via ui_manager)
-----------------------------------------------------------

-- Clicking a tab makes that unit the active one (highlight + drives
-- the lower data sections in subsequent steps). The portrait sprite
-- is the clickable element; we match by spriteId.
function unitInfoV2.handleTabClick(elemHandle)
    for _, tab in ipairs(unitInfoV2.tabs) do
        if tab.spriteId == elemHandle then
            if unitInfoV2.activeUid ~= tab.uid then
                unitInfoV2.activeUid = tab.uid
                applyTabPositions()
            end
            return true
        end
    end
    return false
end

function unitInfoV2.handleScrollLeft(elemHandle)
    if elemHandle ~= unitInfoV2.scrollLeftId then return false end
    if unitInfoV2.scrollOffset > 0 then
        unitInfoV2.scrollOffset = unitInfoV2.scrollOffset - 1
        applyTabPositions()
    end
    return true
end

function unitInfoV2.handleScrollRight(elemHandle)
    if elemHandle ~= unitInfoV2.scrollRightId then return false end
    if not unitInfoV2.tabLayout then return false end
    local maxOffset = math.max(0,
        #unitInfoV2.tabs - unitInfoV2.tabLayout.visibleCount)
    if unitInfoV2.scrollOffset < maxOffset then
        unitInfoV2.scrollOffset = unitInfoV2.scrollOffset + 1
        applyTabPositions()
    end
    return true
end

return M
