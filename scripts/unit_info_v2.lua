-- Unit Info v2 — full-height right-edge pane
--
-- Step 1 (this file): layout skeleton only. Sections are placeholder
-- boxes / placeholder text. Real content (sprite tabs, header values,
-- stats, equipment, inventory) gets wired in subsequent passes.
--
-- The old unit_info_panel.lua is suppressed while this module is
-- active (sets unitInfoWatch.suppressed = true on init). The current
-- HUD info panel still handles tile info; only the unit-info push is
-- skipped.
--
-- Lifecycle: loaded via engine.loadScript from init.lua. Polls
-- unit.getSelected() each tick; shows the page when >=1 unit is
-- selected, hides on deselect. All section content rebuilds when the
-- selection identity changes.

local hud         = require("scripts.hud")
local infoPanel   = require("scripts.hud.info_panel")
local label       = require("scripts.ui.label")
local scale       = require("scripts.ui.scale")
local boxTextures = require("scripts.ui.box_textures")
local scrollbar   = require("scripts.ui.scrollbar")
local brokenOverlay = require("scripts.ui.broken_overlay")
local stats       = require("scripts.unit_stats")
local injuries    = require("scripts.injuries")
local knowledge   = require("scripts.knowledge")

-- Singleton via package.loaded so ui_manager's click handlers (which
-- look us up via require / package.loaded) get the same module table
-- that engine.loadScript loaded with dofile. Without this, dofile
-- and require produce two different module instances and clicks fail
-- silently because uiManager.onUnitInfo* sees an empty table.
local unitInfoV2 = package.loaded["scripts.unit_info_v2"] or {}
package.loaded["scripts.unit_info_v2"] = unitInfoV2

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

unitInfoV2.scriptId      = nil
unitInfoV2.bootstrapped  = false
unitInfoV2.page          = nil
unitInfoV2.outerBoxId    = nil
unitInfoV2.dividerIds    = {}   -- thin sprite handles for inter-section rules
unitInfoV2.ownedLabels   = {}   -- label.* IDs to clean up
unitInfoV2.lastSelCount  = 0
unitInfoV2.lastWantVisible = false  -- last resolved pane-visibility gate (#137)
unitInfoV2.whitePixelTex = nil  -- 1×1 white texture for dividers
unitInfoV2.tabSelectedTex = nil -- shaped backdrop drawn behind the active tab's sprite
unitInfoV2.subTabSelectedTexSet   = nil  -- 9-patch box set for active sub-tab
unitInfoV2.subTabUnselectedTexSet = nil  -- 9-patch box set for inactive sub-tab

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

-- Header
unitInfoV2.headerNameLabelId   = nil  -- the name row; refreshed per active unit (#264)
unitInfoV2.headerTypeLabelId   = nil  -- the "acolyte" row; refreshed per active unit
unitInfoV2.headerActionLabelId = nil  -- the action row; refreshed per active unit from unit_ai

-- Equipment section. equipRect is the section rect; equipElements is
-- the list of sprites/labels rebuilt on unit change (and torn down
-- before the rebuild). lastEquipClass / lastEquipUid let us skip
-- rebuilds when nothing relevant has changed.
unitInfoV2.equipRect       = nil
unitInfoV2.equipElements   = {}
unitInfoV2.equipSlots      = {}    -- per-slot right-click metadata
unitInfoV2.accessoryRows   = {}    -- per-accessory-row right-click metadata
unitInfoV2.lastEquipClass  = nil
unitInfoV2.lastEquipUid    = nil
unitInfoV2.lastEquipKey    = nil   -- hash of the rendered loadout

-- Inventory section state. invTabs are the per-category tab buttons
-- in a strip across the top of the section; invListElements are the
-- per-row icon+text+highlight sprites. activeInvTab persists across
-- unit selections so the player stays on whichever tab they picked.
unitInfoV2.invRect         = nil
unitInfoV2.invTabs         = {}    -- { {name, count, bgId, labelId}, … }
unitInfoV2.activeInvTab    = "All"
unitInfoV2.invListElements = {}
unitInfoV2.invRows         = {}    -- per-row right-click metadata
unitInfoV2.lastInvKey      = nil   -- hash of the rendered (uid, tab, items)

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

local PANEL_W       = 340     -- pane width
local PANEL_PAD     = 12      -- outer padding inside the pane
local SECTION_PAD   = 18      -- horizontal padding around section text (clears the 9-patch border)
local SECTION_GAP   = 10      -- vertical gap between section content and divider

local TABS_H    = 88   -- room for extra top padding above the tab row
local HEADER_H  = 84   -- 4 rows × ~21px (Name / Type / Role / Action)
local STATS_H   = 280
local EQUIP_H   = 272   -- fits a 256-tall humanoid silhouette + 8px top/bot
-- Inventory section takes remaining vertical room.

-- Equipment section layout. Silhouette + slot grid on the left,
-- accessory list on the right. SILHOUETTE_PAD is the gap between the
-- pane border and the silhouette; ACCESSORY_GAP separates the
-- silhouette from the right-side accessory list.
local SILHOUETTE_PAD = 8
local ACCESSORY_GAP  = 12

-- Inventory section layout. Tab strip on top (re-uses the menu tab
-- 9-patch like the stats sub-tabs), scrollable item list below, total
-- weight footer at the bottom.
local INV_TAB_FONT_SIZE  = 13
local INV_TAB_TEXT_PAD   = 8     -- horizontal padding inside each tab
local INV_TAB_H          = 26
local INV_TAB_GAP        = 0     -- flush like the sub-tabs
local INV_TAB_TOP_PAD    = 6
local INV_TAB_BOTTOM_PAD = 6
local INV_ROW_H          = 32    -- one row per item (matches icon size)
local INV_ROW_PAD        = 2
local INV_ICON_SIZE      = 28
local INV_FOOTER_H       = 22
local INV_TEXT_PAD       = 8     -- horizontal pad inside a row
local INV_EQUIP_TINT     = { 1.0, 0.95, 0.55, 0.18 }  -- soft amber bg for equipped rows

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

local OUTER_TILE = 64           -- tile size for the big outer pane (uses bigger 9-patch corners)
local DIVIDER_THICKNESS = 2     -- horizontal rule between sections
local DIVIDER_INSET     = 24    -- horizontal gap between divider and pane edge
local DIVIDER_COLOR     = { 0.7, 0.7, 0.7, 1.0 }   -- same grey as tooltip separator

-- Sub-tabs (Status, Stats, Mental, …) inside the stats section.
-- Styled to match scripts/ui/tabbar.lua (settings + create-world menus):
-- 9-patch box, dark text on unselected, white text on selected.
local SUB_TAB_LIST       = { "Status", "Physical", "Mental", "Skill", "Knowledge" }
local SUB_TAB_FONT_SIZE  = 14
local SUB_TAB_TEXT_PAD   = 10   -- horizontal padding inside each sub-tab around the label
local SUB_TAB_ROW_H      = 32   -- per-row height — matches settingsMenu.baseSizes.tabHeight
local SUB_TAB_TILE       = 16   -- 9-patch corner size for the tab box textures
local SUB_TAB_GAP        = 0    -- menu-style tabs sit flush against each other
local SUB_TAB_ROW_GAP    = 4    -- vertical gap between wrapped rows
local SUB_TAB_TOP_PAD    = 6    -- gap below the section's top divider before the first row
local SUB_TAB_TEXT_COLOR     = { 0.0, 0.0, 0.0, 1.0 }
local SUB_TAB_SEL_TEXT_COLOR = { 1.0, 1.0, 1.0, 1.0 }

-- Content rows inside a stats panel (icon on the left, value on the right).
-- Hovering the icon brings up the stat's tooltip (name + description).
local CONTENT_ROW_H      = 48
local CONTENT_FONT_SIZE  = 20
-- Conditions / injuries use a smaller font than the big stat numbers — the
-- name reads better small, severity is shown by COLOUR (severityColor) with
-- the word + effects in the tooltip.
local CONDITION_FONT_SIZE = 14
local CONTENT_LEFT_PAD   = 16
local CONTENT_RIGHT_PAD  = 16
local CONTENT_DIM_COLOR  = { 0.78, 0.78, 0.78, 1.0 }
local CONTENT_VAL_COLOR  = { 1.0, 1.0, 1.0, 1.0 }
local CONTENT_TOP_PAD    = 8
local ICON_SIZE          = 40
-- Reserve this many pixels on the right when a scrollbar is needed.
-- Sized to fit the scrollbar widget (button 24 + gap 8) at uiscale 1.
local SCROLLBAR_RESERVE  = 32

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------

local function clearOwned()
    for _, lblId in ipairs(unitInfoV2.ownedLabels) do
        label.destroy(lblId)
    end
    unitInfoV2.ownedLabels = {}

    for _, divId in ipairs(unitInfoV2.dividerIds) do
        UI.deleteElement(divId)
    end
    unitInfoV2.dividerIds = {}

    for _, t in ipairs(unitInfoV2.tabs) do
        if t.spriteId then UI.deleteElement(t.spriteId) end
        if t.boxId    then UI.deleteElement(t.boxId)    end
    end
    unitInfoV2.tabs = {}

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

    for _, e in ipairs(unitInfoV2.equipElements) do
        if e.kind == "label" then label.destroy(e.id)
        else                       UI.deleteElement(e.id)
        end
    end
    unitInfoV2.equipElements   = {}
    unitInfoV2.equipSlots      = {}
    unitInfoV2.lastEquipClass  = nil
    unitInfoV2.lastEquipUid    = nil
    unitInfoV2.lastEquipKey    = nil

    for _, t in ipairs(unitInfoV2.invTabs) do
        if t.labelId then label.destroy(t.labelId) end
        if t.bgId    then UI.deleteElement(t.bgId)  end
    end
    unitInfoV2.invTabs = {}
    for _, e in ipairs(unitInfoV2.invListElements) do
        if e.kind == "label" then label.destroy(e.id)
        else                       UI.deleteElement(e.id)
        end
    end
    unitInfoV2.invListElements = {}
    unitInfoV2.invRows         = {}
    unitInfoV2.lastInvKey      = nil

    unitInfoV2.headerNameLabelId   = nil
    unitInfoV2.headerTypeLabelId   = nil
    unitInfoV2.headerActionLabelId = nil

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

    if unitInfoV2.outerBoxId then
        UI.deleteElement(unitInfoV2.outerBoxId)
        unitInfoV2.outerBoxId = nil
    end

    if unitInfoV2.logBtnBoxId then
        UI.deleteElement(unitInfoV2.logBtnBoxId)
        unitInfoV2.logBtnBoxId = nil
    end
end

-----------------------------------------------------------
-- Multi-unit sprite tabs
-----------------------------------------------------------

-- Stable identity for a selection — used to detect when the tab
-- strip needs rebuilding. Order-insensitive (sort uids first) so
-- box-select order quirks don't trigger spurious rebuilds.
local function selectionKey(sel)
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

-- Build tab elements for the current selection. Preserves activeUid
-- across rebuilds when the previously-active unit is still selected;
-- otherwise falls back to the first selected unit.
local function rebuildTabs(sel)
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

-- Place a centred placeholder label inside a virtual section rect.
-----------------------------------------------------------
-- Stats sub-tabs + per-panel content
-----------------------------------------------------------

-- Format a number for stat display: "?" when nil, 2 decimal places.
local function fmtNum(x)
    if x == nil then return "?" end
    return string.format("%.2f", x)
end

-- Truncate `text` to fit within `maxPx` of horizontal space at the
-- given font + size, appending ".." when truncated. Binary-searches
-- the prefix so this stays O(log n) per call even for long strings.
-- Returns the original text unchanged if it already fits.
local function truncateToWidth(text, font, fontSize, maxPx)
    if not text or text == "" or maxPx <= 0 then return text end
    local full = engine.getTextWidth(font, text, fontSize)
    if full <= maxPx then return text end
    local ellipsis = ".."
    local ellW = engine.getTextWidth(font, ellipsis, fontSize)
    if ellW > maxPx then return "" end  -- not even the dots fit
    local lo, hi = 0, #text
    while lo < hi do
        local mid = math.floor((lo + hi + 1) / 2)
        local sub = text:sub(1, mid)
        local w = engine.getTextWidth(font, sub, fontSize) + ellW
        if w <= maxPx then lo = mid else hi = mid - 1 end
    end
    return text:sub(1, lo) .. ellipsis
end

-----------------------------------------------------------
-- Stat metadata + icon cache
--
-- Per-stat name, icon basename (file in assets/textures/icons/),
-- and a one-line description shown as the tooltip hint. Hover the icon
-- to see name + description; the value text on the right is the live
-- number. Add new stats by appending here — the panels look entries
-- up by key, so the same key flows into icon/tooltip/value lookups.
-----------------------------------------------------------

local STAT_DEFS = {
    -- Status (current-state resources + derived feedback)
    stamina      = { icon = "stamina",      name = "Stamina",
        desc = "Drives sustained physical effort. Drops with action, regenerates with rest." },
    hunger       = { icon = "hunger",       name = "Hunger",
        desc = "Need for food. High hunger reduces stamina regen and eventually starves the unit." },
    hydration    = { icon = "hydration",    name = "Hydration",
        desc = "Need for water. Drops faster than hunger; critical in hot climates." },
    blood        = { icon = "blood",        name = "Blood",
        desc = "Blood volume in litres. Drained by bleeding wounds; below 30% triggers unconsciousness, ≤0 means death." },
    pain         = { icon = "pain",         name = "Pain",
        desc = "Accumulated pain from wounds (severity weighted by attack kind). High pain penalises hit chance and evasion." },
    body_temp    = { icon = "body_temp",    name = "Body Temp",
        desc = "Core body temperature (°C). Driven by the environment, the body's heat production, and insulation. Far from 37°C means hypothermia (cold) or heat stroke (hot)." },
    circulation  = { icon = "circulation",  name = "Circulation",
        desc = "How well the body perfuses blood to its tissues. Lowered by blood loss, obesity, frailty, sickness, and cold (vasoconstriction). Poor circulation freezes the extremities first." },
    salt         = { icon = "salt",         name = "Salt",
        desc = "Electrolyte (sodium) balance, as % of ideal. Sweat and water dilute it (cramps, hyponatremia); dehydration concentrates it (hypernatremia). Food restores salt — the kidneys can't make it. Both extremes are dangerous." },
    heart_rate   = { icon = "heart_rate",   name = "Heart Rate",
        desc = "Beats per minute. Rises with exertion, heat, blood loss, and low oxygen (the body compensating); cold slows it (bradycardia). Drives circulation." },
    blood_oxygen = { icon = "blood_oxygen", name = "Blood O₂",
        desc = "Blood oxygen saturation. Lung damage, massive blood loss, or a cold-slowed heart starve the blood of oxygen → hypoxia → suffocation." },
    consciousness = { icon = "consciousness", name = "Consciousness",
        desc = "Mental clarity, set by the worst of: temperature extremes, low blood oxygen, salt imbalance. Falling → confused → delirious (stumbles) → unconscious (collapses)." },
    carrying_capacity = { icon = "weight",  name = "Carry Load",
        desc = "Carried weight vs carrying capacity (from muscle mass and strength). Includes worn gear and container contents; over-capacity units refuse to pick anything else up." },

    -- Physical stats (rolled at spawn)
    strength     = { icon = "strength",     name = "Strength",
        desc = "Affects melee damage, carry capacity, and heavy-tool work speed." },
    endurance    = { icon = "endurance",    name = "Endurance",
        desc = "Sets the ceiling for stamina and slows its drain under load." },
    reflexes     = { icon = "reflexes",     name = "Reflexes",
        desc = "Affects dodge, parry, and the reaction window for ranged attacks." },
    constitution = { icon = "constitution", name = "Constitution",
        desc = "Slows bleeding from wounds and accelerates natural healing." },
    metabolism   = { icon = "metabolism",   name = "Metabolism",
        desc = "How quickly the unit burns calories. High metabolism eats more but recovers faster." },
    toughness    = { icon = "toughness",    name = "Toughness",
        desc = "Flat damage reduction on all incoming hits. Caps at 50% at toughness 10." },
    dexterity    = { icon = "dexterity",    name = "Dexterity",
        desc = "Fine motor control. Drives aim on offense and parry on defense." },
    agility      = { icon = "agility",      name = "Agility",
        desc = "Whole-body motion. Drives dodging incoming attacks." },
    height       = { icon = "height",       name = "Height",
        desc = "Affects reach, line of sight, and the cap for skeletal lean mass." },
    weight       = { icon = "weight",       name = "Weight",
        desc = "Total body mass. Heavier units move slower and apply more force in melee." },

    -- Mental stats
    perception   = { icon = "perception",   name = "Perception",
        desc = "Sight range, hearing, and chance to spot hidden things. Also helps spot incoming attacks." },
    intelligence = { icon = "intelligence", name = "Intelligence",
        desc = "Tactical decision-making. Smarter units pick vital low-resistance targets; dumber ones swing more randomly." },

    -- Skills (weapon classes + balance). All grow with XP.
    balance      = { icon = "balance",      name = "Balance",
        desc = "Footing on uneven terrain. Reduces falls, slips, and stagger from impacts; contributes to dodge." },
    dodge        = { icon = "agility",      name = "Dodge",
        desc = "Active evasion — slipping a blow you SEE coming. Only works against attacks you're aware of (in view, facing them); useless against an ambush from behind. Chance scales with agility and this skill; telegraphed lunges are easier to dodge." },
    dagger       = { icon = "dagger",       name = "Dagger",
        desc = "Skill with daggers and other short blades. Improves hit chance and damage with that weapon class." },
    unarmed      = { icon = "unarmed",      name = "Unarmed",
        desc = "Skill at fighting without a weapon — fists, claws, fangs. Used by all natural-weapon creatures (bears) plus unarmed humanoids." },
    -- Leap/grapple skills (combat: lunging + clinging). LEARNED proficiency;
    -- effective capability blends with the STATS noted — the skill/stat split,
    -- so a strong, agile novice still out-leaps a frail expert. Icons reuse
    -- agility/strength until dedicated art exists.
    jumping      = { icon = "agility",      name = "Jumping",
        desc = "Technique for leaping — gap-closing lunges that turn body momentum into a strike. Effective leap range and speed scale with the agility and strength stats." },
    grappling    = { icon = "strength",     name = "Grappling",
        desc = "Technique for grabbing and clinging — latching a grasping part (jaws, claws, hands) onto an enemy and holding through their thrashing. Scales with strength and dexterity." },
}

-- engine.loadTexture caches by path, but we keep a per-key map so each
-- key resolves to a (texture, def) pair once and we don't re-hit the
-- engine each rebuild. Missing icons silently fall back to text.
local iconCache = {}

-- Icons live in kind subfolders (assets/textures/icons/<kind>/<name>.png) but
-- are referenced by bare name (e.g. "strength"). Build a one-time
-- basename -> full-path index over the subfolders so callers stay name-only.
local ICON_SUBDIRS = { "stat", "skill", "status", "injury", "infection", "knowledge" }
local iconIndex = nil
local function buildIconIndex()
    iconIndex = {}
    for _, sub in ipairs(ICON_SUBDIRS) do
        local dir   = "assets/textures/icons/" .. sub
        local files = engine.listFiles(dir, ".png")
        if files then
            for _, fn in ipairs(files) do
                iconIndex[fn:gsub("%.png$", "")] = dir .. "/" .. fn
            end
        end
    end
end

local function loadIconFor(iconKey)
    if not iconKey then return nil end
    if iconCache[iconKey] ~= nil then
        return iconCache[iconKey] or nil
    end
    if not iconIndex then buildIconIndex() end
    local path = iconIndex[iconKey]
    local tex  = path and engine.loadTexture(path) or nil
    iconCache[iconKey] = tex or false
    return tex
end

-- Overlay broken_equipment.png over an item icon at (x,y,w,h) when the
-- item is broken (condition 0). z should sit just above the icon. The
-- overlay sprite is tracked in `track` so it tears down with the panel.
-- Cache + draw live in scripts.ui.broken_overlay (shared with the cargo
-- and item-container contents panels).
local function addBrokenOverlay(it, name, x, y, w, h, z, track)
    local oid = brokenOverlay.add(unitInfoV2.page, name,
                                  it and it.condition, x, y, w, h, z)
    if oid then table.insert(track, { kind = "sprite", id = oid }) end
end

-- Place a single content row: stat icon on the left, bright value on
-- the right. The icon owns the description tooltip (from STAT_DEFS or
-- `tooltipOverride`). The value gets its OWN tooltip via
-- `valueTooltip` (a {text, hint} table) — used to surface how the
-- effective stat was computed (e.g. base + modifier sources). Returns
-- the value-label id and its baseline-y so per-panel refresh closures
-- can re-rightalign the value on update.
-- Trim `text` with a trailing "…" so it fits `maxW` px at `fontSize`.
local function abbreviateToWidth(text, maxW, fontSize)
    if maxW <= 0 then return text end
    if engine.getTextWidth(hud.menuFont, text, fontSize) <= maxW then
        return text
    end
    local n = #text
    while n > 1 do
        local cand = text:sub(1, n) .. "…"
        if engine.getTextWidth(hud.menuFont, cand, fontSize) <= maxW then
            return cand
        end
        n = n - 1
    end
    return "…"
end

-- opts (optional): { fontSize, color, align = "left", abbreviate = true }.
-- Default (no opts) = big white value, right-aligned (the stat-number look).
-- align="left" puts a coloured, abbreviated name right after the icon — the
-- condition/injury look.
local function placeIconStatRow(rect, rowIndex, statKey, valueText,
                                  tooltipOverride, valueTooltip, opts)
    local rowTop = rect.y + CONTENT_TOP_PAD + rowIndex * CONTENT_ROW_H
    local y      = rowTop + CONTENT_FONT_SIZE
                 + math.floor((CONTENT_ROW_H - CONTENT_FONT_SIZE) / 2)
                 - math.floor(CONTENT_FONT_SIZE * 0.3)

    local def     = STAT_DEFS[statKey]
    local iconTex = loadIconFor(def and def.icon or statKey)
    if iconTex then
        local iconY = rowTop + math.floor((CONTENT_ROW_H - ICON_SIZE) / 2)
        local iconId = UI.newSprite(
            "unit_info_v2_stat_icon_" .. rowIndex,
            ICON_SIZE, ICON_SIZE, iconTex,
            1.0, 1.0, 1.0, 1.0,
            unitInfoV2.page)
        UI.addToPage(unitInfoV2.page, iconId,
            rect.x + CONTENT_LEFT_PAD, iconY)
        UI.setZIndex(iconId, 12)
        local tt = tooltipOverride or (def and
            { text = def.name, hint = def.desc })
        if tt then UI.setTooltipRich(iconId, tt) end
        table.insert(unitInfoV2.statsContentElements,
            { kind = "sprite", id = iconId })
    else
        -- No icon on disk for this stat yet — fall back to the old
        -- dim text label so the row isn't visually empty. Carry the
        -- same tooltip the icon would have, so hover info works
        -- before the icon art lands.
        local nameLbl = label.new({
            name     = "unit_info_v2_stat_lbl_" .. rowIndex,
            text     = (def and def.name) or statKey,
            font     = hud.menuFont,
            fontSize = CONTENT_FONT_SIZE,
            color    = CONTENT_DIM_COLOR,
            page     = unitInfoV2.page,
            uiscale  = 1.0,
        })
        local nameH = label.getElementHandle(nameLbl)
        UI.addToPage(unitInfoV2.page, nameH, rect.x + CONTENT_LEFT_PAD, y)
        UI.setZIndex(nameH, 12)
        local tt = tooltipOverride or (def and
            { text = def.name, hint = def.desc })
        if tt then label.setTooltipRich(nameLbl, tt) end
        table.insert(unitInfoV2.statsContentElements,
            { kind = "label", id = nameLbl })
    end

    local fontSize = (opts and opts.fontSize) or CONTENT_FONT_SIZE
    local color    = (opts and opts.color)    or CONTENT_VAL_COLOR
    -- Re-centre the value vertically for its (possibly smaller) font.
    local valY = rowTop + fontSize
               + math.floor((CONTENT_ROW_H - fontSize) / 2)
               - math.floor(fontSize * 0.3)

    local text = valueText
    local valX, leftMaxW
    if opts and opts.align == "left" then
        -- Coloured name flowing right from the icon; abbreviate to fit.
        valX = rect.x + CONTENT_LEFT_PAD + ICON_SIZE
             + math.floor(ICON_SIZE * 0.25)
        leftMaxW = (rect.x + rect.w - CONTENT_RIGHT_PAD) - valX
        if opts.abbreviate then
            text = abbreviateToWidth(text, leftMaxW, fontSize)
        end
    end

    local valLbl = label.new({
        name        = "unit_info_v2_stat_val_" .. rowIndex,
        text        = text,
        font        = hud.menuFont,
        fontSize    = fontSize,
        color       = color,
        page        = unitInfoV2.page,
        uiscale     = 1.0,
        tooltipRich = valueTooltip,
    })
    local valH = label.getElementHandle(valLbl)
    if not valX then
        -- Default: right-aligned (stat numbers).
        local valW = select(1, label.getSize(valLbl))
        valX = rect.x + rect.w - CONTENT_RIGHT_PAD - valW
    end
    UI.addToPage(unitInfoV2.page, valH, valX, valY)
    UI.setZIndex(valH, 12)
    table.insert(unitInfoV2.statsContentElements, { kind = "label", id = valLbl })

    -- Return layout info so the live-refresh closure can re-place the value
    -- correctly per alignment (left-aligned names keep their x + abbreviate;
    -- right-aligned numbers re-right-align to their new width).
    return valLbl, valY, {
        leftX    = (opts and opts.align == "left") and valX or nil,
        maxW     = leftMaxW,
        fontSize = fontSize,
    }
end

-- Format a "cur / max" pair: "0 / 10" with stat lookup. Returns "?"
-- when either value is missing. Uses the unit_stats wrapper so derived
-- stats (e.g. max_stamina = endurance * 10) resolve through the same
-- call as raw attributes.
local function fmtCurMax(uid, curName, maxName)
    local cur = stats.get(uid, curName)
    local mx  = stats.get(uid, maxName)
    if cur == nil or mx == nil then return "?" end
    return string.format("%.1f / %.1f", cur, mx)
end

-- Compute and capture sub-tab row breakdown given the section rect.
-- Returns a list of rows: { {start, end, totalW, count}, ... } using
-- pre-measured tab widths.
local function planSubTabRows(rect, tabWidths)
    local availW = rect.w - 2 * SECTION_PAD
    local rows = {}
    local cur = { startIdx = 1, totalW = 0, count = 0 }
    for i = 1, #tabWidths do
        local tw = tabWidths[i]
        local extra = (cur.count > 0) and SUB_TAB_GAP or 0
        if cur.count > 0 and cur.totalW + extra + tw > availW then
            cur.endIdx = i - 1
            rows[#rows + 1] = cur
            cur = { startIdx = i, totalW = tw, count = 1 }
        else
            cur.totalW = cur.totalW + extra + tw
            cur.count = cur.count + 1
        end
    end
    cur.endIdx = #tabWidths
    rows[#rows + 1] = cur
    return rows
end

local function applySubTabStyling()
    local selSet   = unitInfoV2.subTabSelectedTexSet
    local unselSet = unitInfoV2.subTabUnselectedTexSet
    for _, t in ipairs(unitInfoV2.subTabs) do
        local isActive = (t.name == unitInfoV2.activeSubTab)
        if selSet and unselSet then
            UI.setBoxTextures(t.bgId, isActive and selSet or unselSet)
        end
        local c = isActive and SUB_TAB_SEL_TEXT_COLOR or SUB_TAB_TEXT_COLOR
        local lblH = label.getElementHandle(t.labelId)
        UI.setColor(lblH, c[1], c[2], c[3], c[4])
    end
end

local rebuildStatsContent  -- forward declaration

-- Create the sub-tab strip inside the stats section. Tabs wrap when
-- the row would exceed the available width — typical layout is two
-- rows of three for six text-labelled tabs. Returns the Y position
-- below the last row so the content area can start there.
local function rebuildSubTabs()
    if not unitInfoV2.statsRect then return end
    local rect = unitInfoV2.statsRect

    -- Measure each tab text once so wrapping can be planned without
    -- having to instantiate elements first.
    local tabWidths = {}
    for i, name in ipairs(SUB_TAB_LIST) do
        local tw = engine.getTextWidth(hud.menuFont, name, SUB_TAB_FONT_SIZE)
        tabWidths[i] = math.floor(tw) + 2 * SUB_TAB_TEXT_PAD
    end

    local rows = planSubTabRows(rect, tabWidths)

    -- Lay each row out centred horizontally within the section.
    local cursorY = rect.y + SUB_TAB_TOP_PAD
    for _, r in ipairs(rows) do
        local rowStartX = rect.x + math.floor((rect.w - r.totalW) / 2)
        local cx = rowStartX
        for i = r.startIdx, r.endIdx do
            local name = SUB_TAB_LIST[i]
            local tw = tabWidths[i]

            local bgId = UI.newBox(
                "unit_info_v2_subtab_bg_" .. i,
                tw, SUB_TAB_ROW_H,
                unitInfoV2.subTabUnselectedTexSet,
                SUB_TAB_TILE,
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
                fontSize = SUB_TAB_FONT_SIZE,
                color    = SUB_TAB_TEXT_COLOR,
                page     = unitInfoV2.page,
                uiscale  = 1.0,
            })
            local lblH = label.getElementHandle(lblId)
            local lblW = select(1, label.getSize(lblId))
            local labelX = cx + math.floor((tw - lblW) / 2)
            -- +2 baseline nudge — the 0.3 fontSize offset alone lands
            -- the text visually above center; the extra 2px brings it
            -- onto the box's vertical midline.
            local labelY = cursorY + math.floor(SUB_TAB_ROW_H / 2)
                         + math.floor(SUB_TAB_FONT_SIZE * 0.3) + 2
            UI.addToPage(unitInfoV2.page, lblH, labelX, labelY)
            UI.setZIndex(lblH, 12)

            unitInfoV2.subTabs[#unitInfoV2.subTabs + 1] = {
                name    = name,
                bgId    = bgId,
                labelId = lblId,
            }
            cx = cx + tw + SUB_TAB_GAP
        end
        cursorY = cursorY + SUB_TAB_ROW_H + SUB_TAB_ROW_GAP
    end

    -- Content area: from below the last row to the bottom of the section.
    unitInfoV2.statsContentRect = {
        x = rect.x,
        y = cursorY + SUB_TAB_ROW_GAP,
        w = rect.w,
        h = rect.y + rect.h - (cursorY + SUB_TAB_ROW_GAP),
    }

    applySubTabStyling()
end

-----------------------------------------------------------
-- Panel renderers
-- Each builds elements + returns a refresh(uid) closure that updates
-- text values without recreating the labels.
-----------------------------------------------------------

-- Sum a stat's active modifiers into (deltaSum, percentSum) and the
-- effective total: (base + deltaSum) * (1 + percentSum). Mirrors the
-- engine's effectiveStat composition so the panel never disagrees
-- with unit.getStat.
local function statModifierTotals(uid, statName, base)
    local mods = unit.getModifiers(uid, statName) or {}
    local delta, percent = 0, 0
    for _, m in ipairs(mods) do
        delta   = delta   + (m.delta or 0)
        percent = percent + (m.percent or 0)
    end
    return mods, delta, percent, (base + delta) * (1 + percent)
end

-- Format a stat as "base (+bonus)" when modifiers are active for that
-- stat, else just "base". Bonus = effective - base, so percentage
-- modifiers show up too. Reads live each tick so accessory
-- equip/unequip updates the display without a panel rebuild.
local function fmtStatLive(uid, statName)
    local base = unit.getStatBase(uid, statName)
    if base == nil then return nil end
    local _, _, _, effective = statModifierTotals(uid, statName, base)
    local bonus = effective - base
    if math.abs(bonus) < 0.005 then
        return string.format("%.2f", base)
    end
    return string.format("%.2f (%+.2f)", base, bonus)
end

-- Build the icon tooltip for a stat row — name + base description.
-- Modifier sources don't go here; the player can find them on the
-- value tooltip (statValueTooltip).
local function statHoverTooltip(_uid, statKey)
    local def = STAT_DEFS[statKey]
    if not def then return nil end
    return { text = def.name, hint = def.desc }
end

-- Build the value tooltip for a stat row — title is the *effective*
-- total (base + sum of bonuses), hint multilines each active modifier
-- ("Technogoggles + 0.85"). The number shown in the row is the base
-- with the bonus in parens, so the tooltip title adds new info
-- (the total) rather than just repeating what's already on screen.
-- One hint line per modifier: "cybernetic enhancements +50%" for
-- percentage mods, "Technogoggles +0.85" for flat deltas, both parts
-- for mixed mods.
local function modifierLines(mods)
    local lines = {}
    for _, m in ipairs(mods) do
        local pct = m.percent or 0
        if math.abs(pct) >= 0.0005 then
            if math.abs(m.delta or 0) >= 0.005 then
                lines[#lines + 1] = string.format("%s %+.2f %+d%%",
                    m.source or "?", m.delta, pct * 100)
            else
                lines[#lines + 1] = string.format("%s %+d%%",
                    m.source or "?", pct * 100)
            end
        else
            lines[#lines + 1] = string.format("%s %+.2f",
                m.source or "?", m.delta or 0)
        end
    end
    return lines
end

local function statValueTooltip(uid, statKey)
    local base = unit.getStatBase(uid, statKey)
    if base == nil then return nil end
    local mods, _, _, effective = statModifierTotals(uid, statKey, base)
    if #mods == 0 then
        -- Nothing to explain; suppress the tooltip entirely so a
        -- bonus-less stat doesn't show an empty popup.
        return nil
    end
    return {
        text = string.format("%.2f", effective),
        hint = table.concat(modifierLines(mods), "\n"),
    }
end

-- Carry-load value tooltip: the panel row already shows the effective
-- total, so the tooltip shows where it came from — body-derived base
-- with the modifier bonus in absolute terms: "167.0 (+83.5)".
local function carryValueTooltip(uid)
    local base = unit.getStatBase(uid, "carrying_capacity")
    if base == nil then return nil end
    local mods, _, _, effective =
        statModifierTotals(uid, "carrying_capacity", base)
    if #mods == 0 then return nil end
    return {
        text = string.format("%.1f (%+.1f)", base, effective - base),
        hint = table.concat(modifierLines(mods), "\n"),
    }
end

-- Generic builder for stat-list panels (all of Status/Stats/Mental/
-- Attributes use this; Skill is similar but its row list is dynamic).
-- Each rowDef entry:
--   { key = "<STAT_DEFS key>",
--     value = function(uid) end,
--     tooltip = optional icon-tooltip ({text,hint} or function),
--     valueTooltip = optional value-tooltip (function(uid) → {text,hint}),
--   }
-- Rows whose value() returns nil are skipped so the layout stays compact.
local function buildIconStatPanel(rect, uid, rowDefs)
    local visibleRows = {}
    for _, r in ipairs(rowDefs) do
        if r.value(uid) ~= nil then
            visibleRows[#visibleRows + 1] = r
        end
    end

    -- Visible window: how many rows fit; clip to that with scroll
    -- offset preserved across rebuilds. Scrolling is wired through
    -- panelShapeSig — onScroll mutates statsScrollOffset which is
    -- part of the signature, so the next rebuildStatsContent tick
    -- redraws the visible slice.
    local totalRows = #visibleRows
    local capacity = math.max(1, math.floor(
        (rect.h - CONTENT_TOP_PAD) / CONTENT_ROW_H))
    local needsScroll = totalRows > capacity

    local scrollOffset = unitInfoV2.statsScrollOffset or 0
    local maxOffset    = math.max(0, totalRows - capacity)
    if scrollOffset > maxOffset then scrollOffset = maxOffset end
    if scrollOffset < 0           then scrollOffset = 0         end
    unitInfoV2.statsScrollOffset = scrollOffset

    -- Carve out scrollbar space on the right so value labels don't
    -- overlap the scrollbar widget.
    local rowRect = rect
    if needsScroll then
        rowRect = { x = rect.x, y = rect.y,
                    w = rect.w - SCROLLBAR_RESERVE, h = rect.h }
    end

    local refs = {}
    local visibleCount = math.min(capacity, totalRows - scrollOffset)
    for i = 1, visibleCount do
        local r = visibleRows[scrollOffset + i]
        local tt = r.tooltip
        if type(tt) == "function" then tt = tt(uid) end
        local vtt = nil
        if r.valueTooltip then vtt = r.valueTooltip(uid) end
        -- A colorFn lets a value recolour by danger level (vitals go amber
        -- → red as they near critical). Resolve the initial colour into opts.
        local rowOpts = r.opts
        if r.colorFn then
            rowOpts = {}
            if r.opts then for k, v in pairs(r.opts) do rowOpts[k] = v end end
            rowOpts.color = r.colorFn(uid) or rowOpts.color
        end
        local valLbl, y, lay = placeIconStatRow(rowRect, i - 1, r.key,
            r.value(uid) or "?", tt, vtt, rowOpts)
        refs[i] = {
            valLbl     = valLbl,
            fn         = r.value,
            y          = y,
            tooltipFn  = r.valueTooltip,
            lay        = lay,
            colorFn    = r.colorFn,
        }
    end

    -- Scrollbar lifecycle: PERSISTS across same-tab rebuilds so an
    -- in-flight drag (whose draggingId references the widget) doesn't
    -- get its target destroyed mid-swing. On scroll-only rebuilds we
    -- just resize + retarget the existing widget. The scrollbar is
    -- destroyed when the user changes tab/unit (handled in
    -- rebuildStatsContent before this builder runs).
    if needsScroll then
        local sbButton = 24
        local sbCap    = 4
        local sbX      = rect.x + rect.w - SCROLLBAR_RESERVE
        local sbY      = rect.y + CONTENT_TOP_PAD
        local trackH   = math.max(24,
            rect.h - 2 * CONTENT_TOP_PAD - 2 * sbButton - 2 * sbCap)
        if unitInfoV2.statsScrollbarId then
            -- Existing widget — just refresh sizing + offset. (No
            -- onScroll re-fire from setContentSize; setScrollOffset
            -- does fire it but at the same value, no-op.)
            scrollbar.setContentSize(unitInfoV2.statsScrollbarId,
                totalRows, capacity)
            scrollbar.setScrollOffset(unitInfoV2.statsScrollbarId,
                scrollOffset)
        else
            unitInfoV2.statsScrollbarId = scrollbar.new({
                name         = "unit_info_v2_stats_sb",
                page         = unitInfoV2.page,
                x            = sbX,  y = sbY,
                buttonSize   = sbButton,
                trackHeight  = trackH,
                capHeight    = sbCap,
                tileSize     = 8,
                totalItems   = totalRows,
                visibleItems = capacity,
                zIndex       = 12,
                onScroll     = function(offset)
                    unitInfoV2.statsScrollOffset = offset
                    -- panelShapeSig folds scrollOffset in so the
                    -- next update-tick re-runs this builder and
                    -- the visible window shifts.
                end,
            })
            scrollbar.setScrollOffset(unitInfoV2.statsScrollbarId,
                scrollOffset)
        end
    else
        -- Panel shrank below capacity — kill the scrollbar.
        if unitInfoV2.statsScrollbarId then
            scrollbar.destroy(unitInfoV2.statsScrollbarId)
            unitInfoV2.statsScrollbarId = nil
        end
    end

    -- Clickable transparent background: persists across same-tab
    -- rebuilds. Without this, wheel events over the panel body
    -- (between rows / over non-clickable icons) escape to the
    -- engine's findClickableElementAt → game-scroll → world zoom.
    -- With it, the hit-test finds the bg and routes via onUIScroll.
    if not unitInfoV2.statsBgClickId then
        local bgId = UI.newElement(
            "unit_info_v2_stats_bg",
            rect.w, rect.h,
            unitInfoV2.page)
        UI.addToPage(unitInfoV2.page, bgId, rect.x, rect.y)
        UI.setClickable(bgId, true)
        UI.setOnClick(bgId, "onStatsPanelBgClick")
        -- Below row icons (z=12); just needs to be findable by hit-
        -- test, not visible.
        UI.setZIndex(bgId, 5)
        unitInfoV2.statsBgClickId = bgId
    end

    return function (newUid)
        if not newUid then return end
        for _, ref in ipairs(refs) do
            local valH    = label.getElementHandle(ref.valLbl)
            local newText = ref.fn(newUid) or "?"
            local lay     = ref.lay
            if lay and lay.leftX then
                -- Left-aligned name: re-abbreviate to the fixed width and
                -- keep its x (don't yank it to the right edge).
                label.setText(ref.valLbl,
                    abbreviateToWidth(newText, lay.maxW, lay.fontSize))
                UI.setPosition(valH, lay.leftX, ref.y)
            else
                -- Right-aligned number: re-right-align to its new width.
                label.setText(ref.valLbl, newText)
                local valW = select(1, label.getSize(ref.valLbl))
                UI.setPosition(valH,
                    rowRect.x + rowRect.w - CONTENT_RIGHT_PAD - valW, ref.y)
            end
            -- Live re-colour by danger level (vitals reddening as they drop).
            if ref.colorFn then
                local c = ref.colorFn(newUid)
                if c then UI.setColor(valH, c[1], c[2], c[3], c[4] or 1.0) end
            end
            -- Keep the value tooltip live so modifier-source changes
            -- (e.g. equipping a buff item) show up without waiting
            -- for a sub-tab swap.
            if ref.tooltipFn then
                local tt = ref.tooltipFn(newUid)
                if tt then label.setTooltipRich(ref.valLbl, tt) end
            end
        end
    end
end

-- Status formatters: blood + pain aren't stats so they bypass
-- fmtCurMax. Blood is "cur / max L"; pain is the raw accumulator.
local function fmtBlood(uid)
    local b = unit.getBlood(uid)
    if not b then return "?" end
    return string.format("%.1f / %.1f L", b.current, b.max)
end

local function fmtPain(uid)
    local p = unit.getPain(uid)
    if not p then return "?" end
    return string.format("%.2f", p)
end

local thermo = require("scripts.thermo")
local function fmtBodyTemp(uid)
    return string.format("%.1f\194\176C", thermo.coreTemp(uid))
end
-- Green near 37, amber in the cold/hot bands, red in the danger zone.
local function bodyTempColor(uid)
    local c = thermo.coreTemp(uid)
    if c <= 35.0 or c >= 39.0 then return { 1.0, 0.30, 0.30, 1.0 } end
    if c <= 36.0 or c >= 38.0 then return { 1.0, 0.62, 0.25, 1.0 } end
    return { 0.75, 0.85, 0.70, 1.0 }
end

local function fmtCirculation(uid)
    return string.format("%d%%", math.floor((unit.getStat(uid, "circulation") or 1.0) * 100 + 0.5))
end
local function circulationColor(uid)
    local c = unit.getStat(uid, "circulation") or 1.0
    if c <= 0.4 then return { 1.0, 0.30, 0.30, 1.0 } end
    if c <= 0.65 then return { 1.0, 0.62, 0.25, 1.0 } end
    return { 0.75, 0.85, 0.70, 1.0 }
end

-- Salt concentration as % of ideal (100% = balanced); both directions bad.
local function fmtSalt(uid)
    return string.format("%d%%", math.floor((unit.getStat(uid, "salt_conc") or 1.0) * 100 + 0.5))
end
local function saltColor(uid)
    local c = unit.getStat(uid, "salt_conc") or 1.0
    if c <= 0.55 or c >= 1.6 then return { 1.0, 0.30, 0.30, 1.0 } end
    if c <= 0.8  or c >= 1.35 then return { 1.0, 0.62, 0.25, 1.0 } end
    return { 0.75, 0.85, 0.70, 1.0 }
end

local function fmtHeartRate(uid)
    return string.format("%d bpm", math.floor((unit.getStat(uid, "heart_rate") or 70) + 0.5))
end
local function heartRateColor(uid)
    local hr = unit.getStat(uid, "heart_rate") or 70
    if hr <= 40 or hr >= 160 then return { 1.0, 0.30, 0.30, 1.0 } end
    if hr <= 50 or hr >= 120 then return { 1.0, 0.62, 0.25, 1.0 } end
    return { 0.75, 0.85, 0.70, 1.0 }
end
local function fmtBloodOxygen(uid)
    return string.format("%d%%", math.floor((unit.getStat(uid, "blood_oxygen") or 1.0) * 100 + 0.5))
end
local function bloodOxygenColor(uid)
    local o = unit.getStat(uid, "blood_oxygen") or 1.0
    if o <= 0.6 then return { 1.0, 0.30, 0.30, 1.0 } end
    if o <= 0.85 then return { 1.0, 0.62, 0.25, 1.0 } end
    return { 0.75, 0.85, 0.70, 1.0 }
end
local function fmtConsciousness(uid)
    return string.format("%d%%", math.floor((unit.getStat(uid, "consciousness") or 1.0) * 100 + 0.5))
end
local function consciousnessColor(uid)
    local c = unit.getStat(uid, "consciousness") or 1.0
    if c < 0.4 then return { 1.0, 0.30, 0.30, 1.0 } end   -- delirious/out
    if c < 0.7 then return { 1.0, 0.62, 0.25, 1.0 } end   -- confused
    return { 0.75, 0.85, 0.70, 1.0 }
end

-- Conditions: the unit's transient STATES (as opposed to injuries, which
-- are wounds). Returns a worst-first list of { name, icon } — what's
-- keeping the unit down or threatening it — derived from pose + the
-- knockedDown flag + vitals + wounds. Shown as rows in the Status tab
-- alongside the vitals and the injury list, so the panel always answers
-- "why can / can't this unit act?".
local function lowFrac(uid, cur, maxName, frac)
    local c = stats.get(uid, cur)
    local m = stats.get(uid, maxName)
    return c and m and m > 0 and (c / m < frac)
end

local function unitConditions(uid)
    local out = {}
    local pose = unit.getPose(uid)
    local info = unit.getInfo(uid)

    if pose == "dead" then
        out[#out + 1] = { name = "Dead", icon = "death",
            hint = "This unit is dead." }
        return out   -- nothing else matters
    end

    if pose == "collapsed" then
        if info and info.knockedDown then
            out[#out + 1] = { name = "Knocked down", icon = "pain",
                hint = "Stunned by a fall. Recovers on its own after a moment." }
        else
            -- Survival collapse: name the gate keeping it down (order
            -- matches checkRevive: blood, then hydration, then stamina).
            local blood = unit.getBlood(uid)
            if blood and blood.max > 0 and blood.current / blood.max < 0.5 then
                out[#out + 1] = { name = "Unconscious (blood loss)", icon = "blood",
                    hint = "Out cold from blood loss. Wakes once blood recovers above 50%." }
            elseif lowFrac(uid, "hydration", "max_hydration", 0.5) then
                out[#out + 1] = { name = "Collapsed (dehydrated)", icon = "hydration",
                    hint = "Collapsed from thirst. Rises once hydration recovers above 50%." }
            elseif lowFrac(uid, "stamina", "max_stamina", 0.5) then
                out[#out + 1] = { name = "Collapsed (exhausted)", icon = "stamina",
                    hint = "Collapsed from exhaustion. Rises once stamina recovers above 50%." }
            else
                out[#out + 1] = { name = "Collapsed", icon = "pain",
                    hint = "On the ground, unable to act." }
            end
        end
    end

    -- Mental state from consciousness (brain.lua): confused / delirious. Out-
    -- cold shows as the collapse condition above. Driven by temp extremes, low
    -- blood oxygen, or salt imbalance.
    local cns = stats.get(uid, "consciousness") or 1.0
    if cns < 0.15 then
        -- unconscious — already shown via the collapsed condition.
    elseif cns < 0.40 then
        out[#out + 1] = { name = "Delirious", icon = "confusion",
            hint = "Addled and unable to act purposefully — stumbling around. "
                   .. "From temperature extremes, low oxygen, or salt imbalance." }
    elseif cns < 0.70 then
        out[#out + 1] = { name = "Confused", icon = "confusion",
            hint = "Mentally clouded. Worsens toward delirium and collapse." }
    end

    -- Failure meters (delayed-death pathways) — show the rising % so the
    -- player sees the clock and can treat in time. The tooltip carries the
    -- real numbers: current %, the per-second fill/recover rate, and what it
    -- kills you with. Bucketed to 10% in the row name so the panel rebuilds
    -- in steps, not every tick.
    local METER_CONDITIONS = {
        { stat = "hypoxia", label = "Suffocating",   icon = "hydration",
          title = "Cell hypoxia", fatal = "suffocation",
          desc  = "Lungs/airway can't oxygenate the blood." },
        { stat = "neuro",   label = "Brain failing", icon = "nerve_injury",
          title = "Neural shutdown", fatal = "brain death",
          desc  = "Catastrophic brain trauma is shutting the nervous system down." },
        { stat = "shock",   label = "In shock",      icon = "blood",
          title = "Systemic shock", fatal = "cardiac arrest",
          desc  = "The body's whole-system collapse under massive trauma." },
        { stat = "organ",   label = "Organ failure", icon = "festered_injury",
          title = "Organ failure", fatal = "sepsis",
          desc  = "Untreated visceral trauma festering (sepsis / encephalopathy)." },
        { stat = "sepsis",  label = "Septic",        icon = "festered_injury",
          title = "Sepsis", fatal = "sepsis",
          desc  = "Infection from untreated wounds has spread to the blood. "
                  .. "Antibiotics can still cure it." },
        { stat = "hypothermia", label = "Hypothermic", icon = "frostbite",
          title = "Hypothermia", fatal = "hypothermia",
          desc  = "Core body temperature is dangerously low. Get the unit "
                  .. "somewhere warm." },
        { stat = "hyperthermia", label = "Overheating", icon = "frostbite",
          title = "Hyperthermia", fatal = "heat stroke",
          desc  = "Core body temperature is dangerously high. Get the unit "
                  .. "out of the heat and rehydrated." },
        { stat = "salt_imbalance", label = "Electrolyte crisis", icon = "salt",
          title = "Electrolyte imbalance", fatal = "electrolyte imbalance",
          desc  = "Blood sodium is dangerously off balance (hypo- or "
                  .. "hypernatremia). Food restores salt; water rebalances it." },
    }
    local meterInfo = require("scripts.unit_resources").meterInfo(uid)
    for _, mc in ipairs(METER_CONDITIONS) do
        local v = stats.get(uid, mc.stat)
        if v and v > 0.02 then
            local mi   = meterInfo[mc.stat] or {}
            local rate = mi.rate or 0
            local rateLine
            if rate > 0 then
                rateLine = string.format(
                    "Rising %.2f%%/s → fatal (%s) at 100%%.", rate * 100, mc.fatal)
            else
                rateLine = string.format(
                    "Recovering %.2f%%/s (injury treated).", -rate * 100)
            end
            out[#out + 1] = {
                name = string.format("%s (%d%%)", mc.label, math.floor(v * 10) * 10),
                icon = mc.icon,
                hint = string.format("%s: %.0f%%\n%s\n%s",
                    mc.title, (mi.value or v) * 100, rateLine, mc.desc),
            }
        end
    end

    -- Bleeding: an open (cutting) wound that is actually seeping. Two
    -- gates, matching what bleedRateFor actually squares for live blood
    -- loss: (1) EFFECTIVE severity (severityEffective = max of acute trauma
    -- and the necrosis floor) so an open necrotic wound with little acute
    -- trauma left still counts; (2) the live seep fraction (1 − clot) ×
    -- bandage, so a clotted or dressed-shut wound doesn't, however severe.
    -- Together they avoid both the closed-wound false positive and the
    -- necrotic-wound false negative.
    local ws = unit.getWounds(uid)
    if type(ws) == "table" then
        for _, w in ipairs(ws) do
            local effSev = w.severityEffective or w.severity or 0
            local seep = (1 - (w.clot or 0)) * (w.bandage or 1)
            if (w.kind == "slash" or w.kind == "stab")
               and effSev >= 0.2 and seep > 0.02 then
                out[#out + 1] = { name = "Bleeding", icon = "blood",
                    hint = "Losing blood from open wounds.\n"
                        .. "Bleeds out at 0 blood; revives once it recovers." }
                break
            end
        end
    end

    -- Standing daily-need warnings.
    if pose ~= "collapsed" then
        if lowFrac(uid, "hydration", "max_hydration", 0.25) then
            out[#out + 1] = { name = "Dehydrated", icon = "hydration",
                hint = "Water below 25%. Collapses near empty; find a water source." }
        end
        local hu = stats.get(uid, "hunger")
        if hu and hu <= 0 then
            out[#out + 1] = { name = "Starving", icon = "hunger",
                hint = "Out of food — burning fat then muscle reserves to survive." }
        end
    end

    return out
end

-- Carry row: carried weight / effective capacity. The value tooltip
-- surfaces the capacity breakdown when modifiers are active — the
-- technomule's "cybernetic enhancements +50%" shows here.
local function fmtCarry(uid)
    local cap = unit.getStat(uid, "carrying_capacity")
    if not cap then return nil end
    local carried = unit.getCarryingWeight(uid) or 0
    return string.format("%.1f / %.1f kg", carried, cap)
end

-- Danger colour-coding for the vital numbers: amber as they enter the
-- warning band, red at critical. Returns white when safe (so the refresh
-- restores it as values recover).
local VITAL_CRIT_COLOR = { 1.0, 0.30, 0.30, 1.0 }
local VITAL_WARN_COLOR = { 1.0, 0.62, 0.25, 1.0 }
local function dangerColor(frac, crit, warn)
    if frac == nil  then return CONTENT_VAL_COLOR end
    if frac <= crit then return VITAL_CRIT_COLOR  end
    if frac <= warn then return VITAL_WARN_COLOR  end
    return CONTENT_VAL_COLOR
end
local function bloodColorFn(uid)
    local b = unit.getBlood(uid)
    if not (b and b.max and b.max > 0) then return CONTENT_VAL_COLOR end
    return dangerColor(b.current / b.max, 0.30, 0.50)
end
local function painColorFn(uid)
    local p = unit.getPain(uid) or 0
    if p >= 0.70 then return VITAL_CRIT_COLOR end
    if p >= 0.40 then return VITAL_WARN_COLOR end
    return CONTENT_VAL_COLOR
end
local function fracColorFn(cur, maxName, crit, warn)
    return function(uid)
        local c = stats.get(uid, cur)
        local m = stats.get(uid, maxName)
        if not (c and m and m > 0) then return CONTENT_VAL_COLOR end
        return dangerColor(c / m, crit, warn)
    end
end

-- Blood value tooltip: the live BLEED rate in ml/s (with %/s of total blood
-- in parens). engine.getBlood exposes bleedRate (L/s) summed over wounds.
local function bloodValueTooltip(uid)
    local b = unit.getBlood(uid)
    if not b then return nil end
    local rate = b.bleedRate or 0
    if rate <= 0 then
        return { text = "Blood", hint = "Not bleeding." }
    end
    local pctS = (b.max and b.max > 0) and (rate / b.max * 100) or 0
    return { text = "Blood",
             hint = string.format("Bleeding: %.0f ml/s (%.2f%%/s)",
                 rate * 1000, pctS) }
end

-- Status panel: the unit's whole current state in one scrollable list —
-- vitals (most-likely-to-kill first), then daily-need resources, then
-- the carry load, then any active CONDITIONS (knocked down, collapsed,
-- bleeding…) and finally the full INJURY list (icon + name + severity).
-- Conditions + injuries are dynamic rows appended to the fixed vitals,
-- each rendered icon-left / text-right by the shared row builder (its
-- `key` doubles as the icon basename, so an injury's catalog icon shows
-- without a STAT_DEFS entry). This is the one place to see why a unit
-- can or can't act.
local function buildStatusPanel(rect, uid)
    local rows = {
        { key = "blood",     value = fmtBlood, colorFn = bloodColorFn,
          valueTooltip = bloodValueTooltip },
        { key = "pain",      value = fmtPain,  colorFn = painColorFn },
        { key = "body_temp", value = fmtBodyTemp, colorFn = bodyTempColor },
        { key = "heart_rate", value = fmtHeartRate, colorFn = heartRateColor },
        { key = "blood_oxygen", value = fmtBloodOxygen, colorFn = bloodOxygenColor },
        { key = "consciousness", value = fmtConsciousness, colorFn = consciousnessColor },
        { key = "circulation", value = fmtCirculation, colorFn = circulationColor },
        { key = "salt",      value = fmtSalt, colorFn = saltColor },
        { key = "stamina",   value = function(u) return fmtCurMax(u, "stamina",   "max_stamina")   end,
          colorFn = fracColorFn("stamina",   "max_stamina",   0.10, 0.30) },
        { key = "hunger",    value = function(u) return fmtCurMax(u, "hunger",    "max_hunger")    end,
          colorFn = fracColorFn("hunger",    "max_hunger",    0.05, 0.25) },
        { key = "hydration", value = function(u) return fmtCurMax(u, "hydration", "max_hydration") end,
          colorFn = fracColorFn("hydration", "max_hydration", 0.10, 0.25) },
        { key          = "carrying_capacity",
          value        = fmtCarry,
          valueTooltip = carryValueTooltip },
    }

    -- Conditions (states): icon + small amber name. Captured per-build;
    -- the panel rebuilds when the condition/injury set changes (panelShapeSig).
    local CONDITION_COLOR = { 1.0, 0.62, 0.25, 1.0 }
    for _, c in ipairs(unitConditions(uid)) do
        local name = c.name
        local tt   = { text = name, hint = c.hint or "Current condition." }
        rows[#rows + 1] = {
            key          = c.icon,
            value        = function() return name end,
            opts         = { fontSize = CONDITION_FONT_SIZE,
                             color = CONDITION_COLOR,
                             align = "left", abbreviate = true },
            tooltip      = tt,
            valueTooltip = function() return tt end,
        }
    end

    -- Injuries (wounds): icon + small name, COLOUR-coded by severity, with
    -- the severity word + effects in the tooltip. Identical wounds STACK
    -- with a (xN) multiplier — consistent with item stacking — so a flurry
    -- of small cuts reads "Cut (x6)" instead of six rows. Wounds of
    -- different severity carry different tiered names, so they don't merge;
    -- only truly-identical (mostly minor) ones do. Worst in a group drives
    -- the colour.
    local groups, order = {}, {}
    for _, inj in ipairs(injuries.list(uid)) do   -- list is worst-first
        local g = groups[inj.name]
        if not g then
            g = { icon = inj.icon, count = 0, worst = inj }
            groups[inj.name] = g
            order[#order + 1] = inj.name
        end
        g.count = g.count + 1
        if (inj.severity or 0) > (g.worst.severity or 0) then g.worst = inj end
    end
    for _, nm in ipairs(order) do
        local g    = groups[nm]
        local inj  = g.worst
        local disp = (g.count > 1) and (nm .. " (x" .. g.count .. ")") or nm
        local pct     = string.format("%d%%",
                            math.floor((inj.severity or 0) * 100 + 0.5))
        local sevWord = injuries.severityLabel(inj.severity)
                            :gsub("^%l", string.upper)
        local loc     = injuries.locationName(inj.part):gsub("^%l", string.upper)
        if g.count > 1 then loc = loc .. " + " .. (g.count - 1) .. " more" end
        local hint = "Location: " .. loc
                     .. "\nSeverity: " .. sevWord .. " (" .. pct .. ")"
                     .. (inj.kind == "severed" and "\nPermanent."
                                                or  "\nHeals over time.")
        local effects = injuries.effects(inj.kind, inj.part, inj.severity,
                                         inj.infection)
        if #effects > 0 then
            hint = hint .. "\n\nEffects:\n• " .. table.concat(effects, "\n• ")
        end
        -- Dressing: a bandaged wound seeps only a fraction of its
        -- natural bleed (0% = sealed). Report it so the player sees
        -- how good the first-aid was.
        if inj.bandaged then
            local seepPct = math.floor((inj.seep or 0) * 100 + 0.5)
            local quality = (seepPct <= 0) and "Bleeding stopped"
                            or string.format("Bleeding cut to %d%%", seepPct)
            local kind = (inj.dressing == "tourniquet")
                         and "Makeshift tourniquet" or "Dressing"
            hint = hint .. "\n\n" .. kind .. ": " .. quality
        end
        -- Clotting progress (0 = open, 1.0 = clotted shut). Skip for
        -- kinds that don't externally bleed.
        local clotKind = inj.kind ~= "concussion" and inj.kind ~= "fracture"
        if clotKind then
            local clotPct = math.floor((inj.clot or 0) * 100 + 0.5)
            local clotLine = (clotPct >= 100) and "Clotted"
                             or string.format("Clotting: %d%%", clotPct)
            hint = hint .. "\nWound: " .. clotLine
        end
        local tt = { text = disp, hint = hint }
        rows[#rows + 1] = {
            key          = g.icon,
            value        = function() return disp end,
            opts         = { fontSize = CONDITION_FONT_SIZE,
                             color = injuries.severityColor(inj.severity),
                             align = "left", abbreviate = true },
            tooltip      = tt,
            valueTooltip = function() return tt end,
        }
    end

    -- Infections: their own section, below injuries. An infected wound shows
    -- the specific bug (e.g. "Gas gangrene (left thigh)") + its level, colour-
    -- coded by how bad it is. Cured by antibiotics; cleared by the immune
    -- response over time (which then leaves immunity, below the scars).
    for _, inf in ipairs(injuries.infectionList(uid)) do
        local pct = math.floor((inf.level or 0) * 100 + 0.5)
        local col = (inf.level >= 0.7) and { 1.00, 0.35, 0.35, 1.0 }   -- red
                 or (inf.level >= 0.4) and { 1.00, 0.55, 0.20, 1.0 }   -- orange
                 or { 0.95, 0.80, 0.45, 1.0 }                          -- amber
        local hint = inf.name .. "\nInfection level: " .. pct .. "%"
                     .. ((inf.category == "bacterial")
                         and "\nTreatable with antibiotics."
                         or  "\nThe immune system must fight this off.")
        local disp = inf.name .. " " .. pct .. "%"
        local tt = { text = inf.name, hint = hint }
        rows[#rows + 1] = {
            key          = inf.icon,
            value        = function() return disp end,
            opts         = { fontSize = CONDITION_FONT_SIZE, color = col,
                             align = "left", abbreviate = true },
            tooltip      = tt,
            valueTooltip = function() return tt end,
        }
    end

    -- Scars: permanent marks from healed severe wounds. Dim grey,
    -- below the active injuries, descriptive only.
    for _, sc in ipairs(injuries.scarList(uid)) do
        local loc = injuries.locationName(sc.part):gsub("^%l", string.upper)
        local hint = "Location: " .. loc
                     .. "\nA permanent scar from a healed "
                     .. (sc.kind or "wound") .. "."
        local tt = { text = sc.name, hint = hint }
        rows[#rows + 1] = {
            key          = sc.icon,
            value        = function() return sc.name end,
            opts         = { fontSize = CONDITION_FONT_SIZE,
                             color = { 0.55, 0.55, 0.55, 1.0 },
                             align = "left", abbreviate = true },
            tooltip      = tt,
            valueTooltip = function() return tt end,
        }
    end

    -- Immunities: acquired resistance from surviving infections. Bottom of
    -- the panel, dim blue-green, descriptive. Fades very slowly.
    for _, im in ipairs(unit.getImmunities(uid) or {}) do
        local pct = math.floor((im.level or 0) * 100 + 0.5)
        local disp = "Immunity (" .. im.name .. ") " .. pct .. "%"
        local hint = "Acquired immunity to " .. im.name
                     .. "\nStrength: " .. pct .. "%"
                     .. "\nResists re-infection; fades slowly over time."
        local tt = { text = disp, hint = hint }
        rows[#rows + 1] = {
            key          = im.icon,   -- "immunity"
            value        = function() return disp end,
            opts         = { fontSize = CONDITION_FONT_SIZE,
                             color = { 0.55, 0.80, 0.80, 1.0 },
                             align = "left", abbreviate = true },
            tooltip      = tt,
            valueTooltip = function() return tt end,
        }
    end

    return buildIconStatPanel(rect, uid, rows)
end

-- Row spec helper for engine-side stats. Icon tooltip = stat name +
-- description (no modifier sources). Value tooltip = base value as
-- title + modifier source list as hint — appears only when there are
-- active modifiers, so unmodified stats hover plainly.
local function statRow(key)
    return {
        key          = key,
        value        = function(u) return fmtStatLive(u, key) end,
        tooltip      = function(u) return statHoverTooltip(u, key) end,
        valueTooltip = function(u) return statValueTooltip(u, key) end,
    }
end

-- Body-composition breakdown for the weight row's hover tooltip:
-- splits body_mass into lean / fat / other. Defined above
-- buildPhysicalPanel because that's where it's referenced; Lua
-- resolves the closure's identifier at function-definition time,
-- so a definition-after-use leaves the closure binding to a nil
-- global instead of the local function.
local function weightHint(uid)
    local body = unit.getStat(uid, "body_mass")
    local lean = unit.getStat(uid, "lean_mass")
    local fat  = unit.getStat(uid, "fat_mass")
    if not (body and lean and fat) then
        return "(body composition not yet computed)"
    end
    local other = body - lean - fat
    return string.format(
        "Lean (muscle):  %.1f kg\n"
     .. "Fat:            %.1f kg\n"
     .. "Other (bone, organs, water): %.1f kg",
        lean, fat, other)
end

-- Physical panel: stat rows in roughly-importance order, then the
-- body-attribute rows (height + weight) absorbed from the former
-- "Attributes" panel. Combat-relevant stats up top; metabolism +
-- the body measurements get the bottom.
local function buildPhysicalPanel(rect, uid)
    return buildIconStatPanel(rect, uid, {
        statRow("strength"),
        statRow("endurance"),
        statRow("reflexes"),
        statRow("constitution"),
        statRow("toughness"),
        statRow("dexterity"),
        statRow("agility"),
        statRow("metabolism"),
        { key = "height", value = function(u)
            local h = unit.getStat(u, "height")
            return h and string.format("%.2f m", h) or "?"
        end },
        { key   = "weight",
          value = function(u)
              local m = unit.getStat(u, "body_mass")
              return m and string.format("%.1f kg", m) or "?"
          end,
          tooltip = function(u)
              return {
                  text = STAT_DEFS.weight.name,
                  hint = STAT_DEFS.weight.desc .. "\n\n" .. weightHint(u),
              }
          end,
        },
    })
end

local function buildMentalPanel(rect, uid)
    return buildIconStatPanel(rect, uid, {
        statRow("intelligence"),
        statRow("perception"),
    })
end

local function buildSkillPanel(rect, uid)
    -- Skills are dynamic — list whatever the unit has, sorted. Each
    -- skill name doubles as the icon basename and the STAT_DEFS key,
    -- so adding a skill icon makes it pick up automatically.
    local all = unit.getAllSkills(uid) or {}
    local names = {}
    for n, _ in pairs(all) do names[#names + 1] = n end
    table.sort(names)
    local rows = {}
    for _, n in ipairs(names) do
        rows[#rows + 1] = {
            key   = n,
            value = function(u)
                local s = (unit.getAllSkills(u) or {})[n]
                return s and fmtNum(s.level) or "?"
            end,
            tooltip = (not STAT_DEFS[n]) and {
                text = n:sub(1,1):upper() .. n:sub(2),
                hint = "Skill level. Improves with practice.",
            } or nil,
        }
    end
    return buildIconStatPanel(rect, uid, rows)
end

-- Compose the weight tooltip from the body-composition stats. body_mass
-- is the total; lean_mass is skeletal muscle only; fat_mass is body fat.
-- The remainder (bones, organs, water, viscera) goes into "Other" so
-- the four lines add up to body_mass. Recomputed per hover so values
-- track the unit's current composition.
-- (buildAttributesPanel was here; height + weight rows moved into
-- buildPhysicalPanel as part of the 2026-05 panel restructure;
-- weightHint moved above buildPhysicalPanel so its forward-reference
-- resolves at function-definition time.)


-- Knowledge panel: the catalogue of knowledge TYPES (scripts/knowledge.lua),
-- KNOWN first then unknown. A known knowledge shows its icon + trained
-- level (right-aligned, like a skill), with the effective value
-- (level × intelligence) in the tooltip. A knowledge the unit hasn't learned
-- shows the "unknown" icon + a dim "Unknown" — the player sees a slot exists
-- but not what it is until it's learned (from a book/teacher).
local function buildKnowledgePanel(rect, uid)
    local known, unknown = {}, {}
    for _, k in ipairs(knowledge.list()) do
        local kk = k   -- capture for the closures
        if unit.getKnowledge(uid, kk.id) ~= nil then
            known[#known + 1] = {
                key   = kk.icon,
                value = function(u)
                    local l = unit.getKnowledge(u, kk.id)
                    return l and fmtNum(l) or "?"
                end,
                tooltip = function(u)
                    local l   = unit.getKnowledge(u, kk.id) or 0
                    local int = unit.getStat(u, "intelligence") or 1.0
                    return { text = kk.name,
                             hint = kk.desc
                                 .. string.format(
                                    "\n\nLevel %s × intelligence %.2f = effective %s.",
                                    fmtNum(l), int, fmtNum(l * int)) }
                end,
            }
        else
            unknown[#unknown + 1] = {
                key   = knowledge.UNKNOWN_ICON,
                value = function() return "Unknown" end,
                opts  = { fontSize = CONDITION_FONT_SIZE,
                          color = CONTENT_DIM_COLOR, align = "left",
                          abbreviate = true },
                tooltip = { text = "Unknown",
                            hint = "This unit hasn't learned this knowledge.\n"
                                .. "It can be learned from a book or a teacher." },
            }
        end
    end
    -- Known capabilities lead; unlearned slots follow.
    local rows = {}
    for _, r in ipairs(known)   do rows[#rows + 1] = r end
    for _, r in ipairs(unknown) do rows[#rows + 1] = r end
    return buildIconStatPanel(rect, uid, rows)
end

local PANEL_BUILDERS = {
    Status    = buildStatusPanel,
    Physical  = buildPhysicalPanel,
    Mental    = buildMentalPanel,
    Skill     = buildSkillPanel,
    Knowledge = buildKnowledgePanel,
}

-- Forward-declared above. Clears the current panel's elements and
-- builds the new one for (activeUid, activeSubTab). Cheap when the
-- active selection hasn't changed because we cache (uid, subtab) and
-- only refresh values via the panel's refresh callback.
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
        for _, c in ipairs(unitConditions(uid)) do
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

rebuildStatsContent = function ()
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

local function placePlaceholder(name, x, y, w, h, text, fontSize)
    local lblId = label.new({
        name     = "unit_info_v2_" .. name,
        text     = text,
        font     = hud.menuFont,
        fontSize = fontSize or 14,
        color    = {0.65, 0.65, 0.65, 1.0},
        page     = unitInfoV2.page,
        uiscale  = 1.0,
    })
    table.insert(unitInfoV2.ownedLabels, lblId)
    local lblHandle = label.getElementHandle(lblId)
    local lblW = select(1, label.getSize(lblId))
    local lblFs = label.getFontSize(lblId)
    local lx = x + math.floor((w - lblW) / 2)
    local ly = y + math.floor(h / 2) + math.floor(lblFs * 0.3)
    UI.addToPage(unitInfoV2.page, lblHandle, lx, ly)
    UI.setZIndex(lblHandle, 12)
end

-- Horizontal rule between sections. Drawn as a thin tinted sprite
-- using the same 1×1 white pixel + grey tint pattern as the tooltip
-- separator, so the visual language matches. Inset on each side so
-- the rule visibly stops short of the pane's inner border instead of
-- running right up to (or through) the 9-patch corner pixels.
local function placeDivider(x, y, w, uiscale)
    if not unitInfoV2.whitePixelTex then return end
    local inset = math.floor(DIVIDER_INSET * uiscale)
    local sprId = UI.newSprite(
        "unit_info_v2_div_" .. tostring(#unitInfoV2.dividerIds + 1),
        w - 2 * inset, DIVIDER_THICKNESS,
        unitInfoV2.whitePixelTex,
        DIVIDER_COLOR[1], DIVIDER_COLOR[2],
        DIVIDER_COLOR[3], DIVIDER_COLOR[4],
        unitInfoV2.page
    )
    UI.addToPage(unitInfoV2.page, sprId, x + inset, y)
    UI.setZIndex(sprId, 11)
    table.insert(unitInfoV2.dividerIds, sprId)
end

-----------------------------------------------------------
-- Equipment section: silhouette on the left with clickable slot
-- overlays, accessory list on the right. The silhouette and slot
-- positions both come from the active unit's equipment class (looked
-- up via equipment.getClass), so changing the YAML re-lays out the
-- section with no Lua changes.
--
-- Phase 1 is read-only: slots are transparent hit-zones with tooltips,
-- there are no item icons yet, and the accessory list is a placeholder.
-- Phase 2 will hang equip popups off the slot click callbacks and draw
-- the equipped item's icon inside each slot rect.
-----------------------------------------------------------

-- Capitalize the first letter of a stat name for display
-- ("perception" → "Perception"). Used by the buff line and the stats
-- panel.
local function capitalizeStat(name)
    if not name or #name == 0 then return name or "" end
    return name:sub(1, 1):upper() .. name:sub(2)
end

-- Effective buff amount given the item's condition. For buffs with
-- `scalesWithCondition`, the amount is multiplied by condition/100;
-- otherwise it's the flat amount.
local function effectiveBuffAmount(buff, condition)
    local amt = buff.amount or 0
    if buff.scalesWithCondition then
        local cond = condition or 100
        return amt * (cond / 100)
    end
    return amt
end

-- Build the rich tooltip hint shown for an item — same content for
-- inventory rows AND silhouette slot icons. `it` accepts both shapes
-- (from unit.getInventory / equipment.getLoadout / getAccessories);
-- equippedSlot is the slot id string when the item lives in a slot,
-- "(worn)" for accessories, nil for inventory items.
local function buildItemHint(it, equippedSlot)
    local hintLines = { string.format("%.2f kg", it.weight or 0) }
    if it.make and it.make ~= "" then
        hintLines[#hintLines + 1] = "make: " .. it.make
    end
    if it.material and it.material ~= "" then
        hintLines[#hintLines + 1] = "material: " .. it.material
    end
    -- Container fill: shown for items that have a capacity (canteens
    -- etc.). Format is "(currentFill/capacity unit)" where unit
    -- defaults to L for fluids — fits most starting items.
    if it.capacity and it.capacity > 0 then
        hintLines[#hintLines + 1] = string.format("(%.1f/%.1f L)",
            it.currentFill or 0, it.capacity)
    end
    if it.quality then
        hintLines[#hintLines + 1] =
            string.format("quality: %d%%", math.floor(it.quality + 0.5))
    end
    if it.condition then
        hintLines[#hintLines + 1] =
            string.format("condition: %d%%", math.floor(it.condition + 0.5))
    end
    if it.weapon then
        -- Live effective sharpness on the def's engineering scale
        -- (lower = sharper), so distinct weapons stay distinct and a
        -- worn edge reads correctly. `it.sharpness` is the instance's
        -- 0..100 edge-wear % (100 = factory edge); mirror combat's
        -- derivation: effective = base * 100/wear (wear clamped 10..100,
        -- matching Combat.Resolution). Falls back to the raw base when
        -- the backend didn't supply the instance wear value.
        local base = it.weapon.baseSharpness or 0
        local wear = it.sharpness
        local sharp = base
        if wear ~= nil then
            -- Clamp 10..100 like Combat.Resolution, so a fully-dulled
            -- edge (wear 0) reads as the dullest case (base * 10), not a
            -- pristine fallback. Only a missing field falls back to base.
            local w = math.max(10, math.min(100, wear))
            sharp = base * (100.0 / w)
        end
        hintLines[#hintLines + 1] = string.format(
            "length %.0fcm  ·  sharpness %d",
            it.weapon.bladeLength or 0,
            math.floor(sharp + 0.5))
        hintLines[#hintLines + 1] = string.format(
            "stab %.2f  ·  slash %.2f  ·  blunt %.2f",
            it.weapon.stabEffectiveness or 0,
            it.weapon.slashEffectiveness or 0,
            it.weapon.bluntEffectiveness or 0)
    end
    if it.buffs then
        for _, b in ipairs(it.buffs) do
            local line = string.format("%s + %g",
                capitalizeStat(b.stat), b.amount)
            if b.scalesWithCondition and it.condition then
                line = line .. string.format(" (x%.2f)", it.condition / 100)
            end
            hintLines[#hintLines + 1] = line
        end
    end
    if equippedSlot then
        hintLines[#hintLines + 1] = "equipped: " .. equippedSlot
    end
    return table.concat(hintLines, "\n")
end

-- Stable hash of (uid, class, slot→defName pairs). When this changes
-- we rebuild; otherwise the previous frame's sprites are correct.
local function computeEquipKey(uid, clsName, loadout, accessories)
    local parts = { tostring(uid or ""), clsName or "" }
    if loadout then
        local pairsT = {}
        for slotId, item in pairs(loadout) do
            -- Include instance condition AND sharpness so an equipped item
            -- degraded by wear rebuilds (and its tooltip + broken overlay
            -- refresh) without needing an unrelated equipment change: a
            -- weapon dulled in combat changes sharpness, but armor
            -- (gambeson / gloves / boots) loses condition with no sharpness
            -- change, and the slot UI shows condition and a condition<=0
            -- broken overlay. Mirrors the accessory key below.
            pairsT[#pairsT + 1] = slotId .. "=" .. (item.defName or "?")
                                  .. "@" .. tostring(item.condition or 0)
                                  .. "/" .. tostring(item.sharpness or 0)
        end
        table.sort(pairsT)
        parts[#parts + 1] = table.concat(pairsT, ";")
    end
    if accessories then
        local accPart = {}
        for i, it in ipairs(accessories) do
            accPart[#accPart + 1] = i .. ":" .. (it.defName or "?")
                                    .. "@" .. tostring(it.condition or 0)
                                    .. "/" .. tostring(it.sharpness or 0)
        end
        parts[#parts + 1] = table.concat(accPart, ";")
    end
    return table.concat(parts, "|")
end

local function rebuildEquipmentSection()
    if not unitInfoV2.equipRect then return end
    local rect = unitInfoV2.equipRect
    local uid  = unitInfoV2.activeUid

    -- Look up the unit's equipment class. Skip silently when the unit
    -- has no class (e.g. a wandering animal) or no active selection.
    local info = uid and unit.getInfo(uid) or nil
    local clsName = info and info.equipmentClass or nil
    local cls = clsName and equipment.getClass(clsName) or nil
    local loadout    = uid and equipment.getLoadout(uid)    or nil
    local accessories = uid and equipment.getAccessories(uid) or nil

    -- Skip if nothing relevant changed since the last build. The hash
    -- folds in uid + class + every (slot, equipped-item def/condition/
    -- sharpness) pair + each accessory's def/condition/sharpness, so any
    -- equip/unequip OR a wear-driven condition/sharpness change — slot or
    -- accessory — invalidates it on the next tick.
    local key = computeEquipKey(uid, clsName, loadout, accessories)
    if key == unitInfoV2.lastEquipKey then return end

    for _, e in ipairs(unitInfoV2.equipElements) do
        if e.kind == "label" then label.destroy(e.id)
        else                       UI.deleteElement(e.id)
        end
    end
    unitInfoV2.equipElements   = {}
    unitInfoV2.equipSlots      = {}
    unitInfoV2.accessoryRows   = {}
    unitInfoV2.lastEquipUid    = uid
    unitInfoV2.lastEquipClass  = clsName
    unitInfoV2.lastEquipKey    = key

    if not cls then
        -- No class for this unit — show a quiet placeholder. The
        -- section divider above already separates this from stats.
        local lblId = label.new({
            name     = "unit_info_v2_equip_none",
            text     = "(no equipment)",
            font     = hud.menuFont,
            fontSize = 14,
            color    = {0.6, 0.6, 0.6, 1.0},
            page     = unitInfoV2.page,
            uiscale  = 1.0,
        })
        local h = label.getElementHandle(lblId)
        local lblW = select(1, label.getSize(lblId))
        UI.addToPage(unitInfoV2.page, h,
            rect.x + math.floor((rect.w - lblW) / 2),
            rect.y + math.floor(rect.h / 2))
        UI.setZIndex(h, 12)
        table.insert(unitInfoV2.equipElements,
            { kind = "label", id = lblId })
        return
    end

    local uiscale = scale.get()
    local silPad  = math.floor(SILHOUETTE_PAD * uiscale)
    local silW    = math.floor(cls.silhouetteW * uiscale)
    local silH    = math.floor(cls.silhouetteH * uiscale)
    local silX    = rect.x + silPad
    local silY    = rect.y + math.floor((rect.h - silH) / 2)

    -- Silhouette background. Untinted — the texture provides the art
    -- (grey humanoid outline with painted slot boxes).
    local silId = UI.newSprite(
        "unit_info_v2_equip_silhouette",
        silW, silH,
        cls.silhouette,
        1.0, 1.0, 1.0, 1.0,
        unitInfoV2.page)
    UI.addToPage(unitInfoV2.page, silId, silX, silY)
    UI.setZIndex(silId, 11)
    table.insert(unitInfoV2.equipElements,
        { kind = "sprite", id = silId })

    -- Slot overlays — transparent hit-zones positioned over the boxes
    -- painted into the silhouette texture. Each carries a tooltip with
    -- the slot's name + the item kind it accepts. When an item is
    -- equipped in the slot, its icon renders on top of the hit-zone
    -- with a richer tooltip (item display name + kind).
    loadout = loadout or {}
    if cls.slots then
        for i, s in ipairs(cls.slots) do
            local slotW = math.floor(s.w * uiscale)
            local slotH = math.floor(s.h * uiscale)
            local slotX = silX + math.floor(s.x * uiscale)
            local slotY = silY + math.floor(s.y * uiscale)
            local slotElemId = UI.newSprite(
                "unit_info_v2_equip_slot_" .. i,
                slotW, slotH,
                unitInfoV2.whitePixelTex,
                1.0, 1.0, 1.0, 0.0,  -- transparent — silhouette art shows
                unitInfoV2.page)
            UI.addToPage(unitInfoV2.page, slotElemId, slotX, slotY)
            UI.setZIndex(slotElemId, 12)
            UI.setTooltipRich(slotElemId, {
                text = s.name,
                hint = "Accepts: " .. (s.kind or "?"),
            })
            UI.setClickable(slotElemId, true)
            UI.setOnRightClick(slotElemId, "onEquipSlotRightClick")
            table.insert(unitInfoV2.equipElements,
                { kind = "sprite", id = slotElemId })

            -- Draw the equipped item's icon on top of the slot if
            -- present. Z+1 keeps it above the transparent hit-zone so
            -- the icon catches the right-click (right-click hit-test
            -- picks the topmost clickable element at the cursor).
            local eq = loadout[s.id]
            local iconElemId = nil
            if eq and eq.iconTex then
                iconElemId = UI.newSprite(
                    "unit_info_v2_equip_icon_" .. i,
                    slotW, slotH,
                    eq.iconTex,
                    1.0, 1.0, 1.0, 1.0,
                    unitInfoV2.page)
                UI.addToPage(unitInfoV2.page, iconElemId, slotX, slotY)
                UI.setZIndex(iconElemId, 13)
                -- Reuse the inventory's hint builder so equipped items
                -- surface the same details (make / material / quality /
                -- condition / weapon stats / equipped slot) here as
                -- they do in the inventory list.
                UI.setTooltipRich(iconElemId, {
                    text = eq.displayName or eq.defName or s.name,
                    hint = buildItemHint(eq, s.id),
                })
                UI.setClickable(iconElemId, true)
                UI.setOnRightClick(iconElemId, "onEquipSlotRightClick")
                table.insert(unitInfoV2.equipElements,
                    { kind = "sprite", id = iconElemId })
                addBrokenOverlay(eq, "unit_info_v2_equip_broken_" .. i,
                    slotX, slotY, slotW, slotH, 14, unitInfoV2.equipElements)
            end

            -- Per-slot right-click metadata. Two element handles map
            -- to one slot id: the transparent slot rect (always there)
            -- and the icon (only present when filled). The handler
            -- searches by either.
            unitInfoV2.equipSlots[#unitInfoV2.equipSlots + 1] = {
                slotId       = s.id,
                slot         = s,
                slotElemId   = slotElemId,
                iconElemId   = iconElemId,
                equippedItem = eq,
            }
        end
    end

    -- Accessory list on the right. Each row: 28×28 icon + display
    -- name. Hover surfaces the full item hint (buffs included);
    -- right-click opens the inventory's Equip / Unequip menu via the
    -- same handler. Empty list shows a quiet placeholder.
    local listX  = silX + silW + math.floor(ACCESSORY_GAP * uiscale)
    local listW  = rect.x + rect.w - listX - silPad
    local accSz  = math.floor(28 * uiscale)
    local accGap = math.floor(2  * uiscale)
    local accRowH = accSz + accGap
    local accs = equipment.getAccessories(uid) or {}
    if listW > 0 and #accs == 0 then
        local lblId = label.new({
            name     = "unit_info_v2_equip_accessories",
            text     = "(no accessories)",
            font     = hud.menuFont,
            fontSize = 14,
            color    = {0.6, 0.6, 0.6, 1.0},
            page     = unitInfoV2.page,
            uiscale  = 1.0,
        })
        local h = label.getElementHandle(lblId)
        local lblW = select(1, label.getSize(lblId))
        UI.addToPage(unitInfoV2.page, h,
            listX + math.floor((listW - lblW) / 2),
            rect.y + math.floor(rect.h / 2))
        UI.setZIndex(h, 12)
        table.insert(unitInfoV2.equipElements,
            { kind = "label", id = lblId })
    elseif listW > 0 then
        -- Icons only — no name labels. The item's name + full hint
        -- comes through on hover. Keeps the list compact and avoids
        -- text running off the panel edge.
        for i, it in ipairs(accs) do
            local rowY = rect.y + math.floor(SILHOUETTE_PAD * uiscale)
                       + (i - 1) * accRowH
            if it.iconTex then
                local iconId = UI.newSprite(
                    "unit_info_v2_acc_icon_" .. i,
                    accSz, accSz,
                    it.iconTex,
                    1.0, 1.0, 1.0, 1.0,
                    unitInfoV2.page)
                UI.addToPage(unitInfoV2.page, iconId, listX, rowY)
                UI.setZIndex(iconId, 13)
                UI.setClickable(iconId, true)
                UI.setOnRightClick(iconId, "onAccessoryRowRightClick")
                UI.setTooltipRich(iconId, {
                    text = it.displayName or it.defName,
                    hint = buildItemHint(it, "(worn)"),
                })
                table.insert(unitInfoV2.equipElements,
                    { kind = "sprite", id = iconId })
                addBrokenOverlay(it, "unit_info_v2_acc_broken_" .. i,
                    listX, rowY, accSz, accSz, 14, unitInfoV2.equipElements)
                unitInfoV2.accessoryRows = unitInfoV2.accessoryRows or {}
                unitInfoV2.accessoryRows[#unitInfoV2.accessoryRows + 1] = {
                    elemId        = iconId,
                    accessoryIndex = i,
                    item          = it,
                }
            end
        end
    end
end

-----------------------------------------------------------
-- Inventory section: dynamic tab strip (All + one per category) on
-- top, item rows in the middle, total-weight footer at the bottom.
-- Equipped items are merged into the All view (and into their
-- category) with a soft amber tint behind the row so the player sees
-- everything the unit owns regardless of slot state.
-----------------------------------------------------------

-- Pull inventory + equipment, tag each entry with equipped state, and
-- return a flat list in insertion order (inventory first, then
-- equipped slots in EquipmentClass slot order so the player sees a
-- stable layout). Each entry: { defName, displayName, weight,
--                               category, kind, iconTex, currentFill,
--                               equipped, equippedSlot }
local function collectInventoryAndEquipment(uid)
    local inv = unit.getInventory(uid) or {}
    local lo  = equipment.getLoadout(uid)  or {}
    local out = {}
    for _, it in ipairs(inv) do
        out[#out + 1] = {
            defName      = it.defName,
            instanceId   = it.instanceId,
            contentsKey  = it.contentsKey,
            displayName  = it.displayName or it.defName,
            weight       = it.weight or 0,
            category     = it.category or "Misc",
            kind         = it.kind or "misc",
            make         = it.make or "",
            material     = it.material or "",
            iconTex      = it.iconTex,
            currentFill  = it.currentFill or 0,
            capacity     = it.capacity,
            quality      = it.quality,
            condition    = it.condition,
            weapon       = it.weapon,
            sharpness    = it.sharpness,
            buffs        = it.buffs,
            unequippable = it.unequippable,
            equipped     = false,
        }
    end
    -- Walk equipment in the unit's class slot order so the equipped
    -- block reads predictably (helmet, armor, gauntlets, weapons, …).
    local info = unit.getInfo(uid)
    local cls  = info and info.equipmentClass
                 and equipment.getClass(info.equipmentClass) or nil
    local slotIds = {}
    if cls and cls.slots then
        for _, s in ipairs(cls.slots) do
            slotIds[#slotIds + 1] = s.id
        end
    end
    for _, slotId in ipairs(slotIds) do
        local it = lo[slotId]
        if it then
            out[#out + 1] = {
                defName       = it.defName,
                instanceId    = it.instanceId,
                contentsKey   = it.contentsKey,
                displayName   = it.displayName or it.defName,
                weight        = it.weight or 0,
                category      = it.category or "Misc",
                kind          = it.kind or "misc",
                make          = it.make or "",
                material      = it.material or "",
                iconTex       = it.iconTex,
                currentFill   = it.currentFill or 0,
                capacity      = it.capacity,
                quality       = it.quality,
                condition     = it.condition,
                weapon        = it.weapon,
                sharpness     = it.sharpness,
                equipped      = true,
                equippedSlot  = slotId,
            }
        end
    end
    -- Accessories — worn items that don't sit on the silhouette.
    -- Each carries its 1-based index (for unequipAccessory).
    local accs = equipment.getAccessories(uid) or {}
    for i, it in ipairs(accs) do
        out[#out + 1] = {
            defName        = it.defName,
            instanceId     = it.instanceId,
            contentsKey    = it.contentsKey,
            displayName    = it.displayName or it.defName,
            weight         = it.weight or 0,
            category       = it.category or "Misc",
            kind           = it.kind or "misc",
            make           = it.make or "",
            material       = it.material or "",
            iconTex        = it.iconTex,
            currentFill    = it.currentFill or 0,
            capacity       = it.capacity,
            quality        = it.quality,
            condition      = it.condition,
            weapon         = it.weapon,
            sharpness      = it.sharpness,
            buffs          = it.buffs,
            unequippable   = it.unequippable,
            equipped       = true,
            equippedSlot   = "(worn)",
            accessoryIndex = i,
        }
    end
    return out
end

-- Build the per-row stacking key. Returns nil for equipped items so
-- they never collapse into a stack (each occupies a distinct slot).
-- Non-equipped items only merge when their defName + quality +
-- condition match exactly — a 100% motor and a 99% motor stay on two
-- rows so the player sees the real spread of conditions. currentFill is
-- in the key so two canteens with different fill split into separate
-- rows (the fill is shown per row, and each row targets its own
-- instance — #67). weight is in the key too: raw gems roll a
-- per-instance weight, the row shows weight×stackCount, and the "Store"
-- action targets a representative — so a 0.05 kg and a 0.09 kg garnet
-- must stay on separate rows rather than merge and mis-report their
-- weight. Sharpness is added ONLY for weapons (the only items whose
-- tooltip shows it) so two weapons with differing edge wear likewise
-- split; armor and other gear also carry a combat-mutated iiSharpness
-- but never display it, so splitting their rows on it would be an
-- invisible, confusing reason. Items that DO merge are interchangeable,
-- so acting on the stack's representative instanceId is always correct.
local function stackKey(it)
    if it.equipped then return nil end
    return table.concat({
        it.defName,
        tostring(it.quality     or "_"),
        tostring(it.condition   or "_"),
        tostring(it.currentFill or "_"),
        tostring(it.weight      or "_"),
        it.weapon and tostring(it.sharpness or "_") or "_",
        -- Nested-contents signature: two first-aid kits whose internal
        -- supplies have diverged must NOT merge, so "Contents" / "Store"
        -- act on the kit the player sees (#67A). Empty for non-containers.
        tostring(it.contentsKey or ""),
    }, "|")
end

-- Collapse identical non-equipped entries into single rows tagged
-- with stackCount. The representative instance carries the visible
-- fields (icon, quality, condition); per-tooltip data uses it too.
-- Total-weight aggregation uses the RAW item list rather than this
-- one, so the footer stays accurate.
local function groupForDisplay(items)
    local groups = {}
    local seen   = {}    -- stackKey → index in groups
    for _, it in ipairs(items) do
        local key = stackKey(it)
        if key and seen[key] then
            groups[seen[key]].stackCount =
                groups[seen[key]].stackCount + 1
        else
            local copy = {}
            for k, v in pairs(it) do copy[k] = v end
            copy.stackCount = 1
            groups[#groups + 1] = copy
            if key then seen[key] = #groups end
        end
    end
    return groups
end

-- Compute the tab strip: "All" first, then per-category in the order
-- categories first appear in the merged list. Each tab carries its
-- count. The All tab counts everything.
local function computeInvTabs(items)
    local tabs = { { name = "All", count = #items } }
    local seen = { All = true }
    for _, it in ipairs(items) do
        if not seen[it.category] then
            seen[it.category] = true
            tabs[#tabs + 1] = { name = it.category, count = 0 }
        end
    end
    for _, it in ipairs(items) do
        for _, t in ipairs(tabs) do
            if t.name == it.category then
                t.count = t.count + 1
            end
        end
    end
    return tabs
end

local function applyInvTabStyling()
    local selSet   = unitInfoV2.subTabSelectedTexSet
    local unselSet = unitInfoV2.subTabUnselectedTexSet
    for _, t in ipairs(unitInfoV2.invTabs) do
        local active = (t.name == unitInfoV2.activeInvTab)
        if selSet and unselSet then
            UI.setBoxTextures(t.bgId, active and selSet or unselSet)
        end
        local c = active and SUB_TAB_SEL_TEXT_COLOR or SUB_TAB_TEXT_COLOR
        local lblH = label.getElementHandle(t.labelId)
        UI.setColor(lblH, c[1], c[2], c[3], c[4])
    end
end

-- Stable hash of (uid, activeTab, items signature) so we only rebuild
-- the list when something actually changed. Fill is included so a
-- depleting canteen redraws when its label needs to change, and
-- stackCount so consuming one item from a stack of identical rows
-- (grouped list keeps the same single entry) updates the "×N" label,
-- tab counts, and total-weight footer. condition is included so a
-- wear-degraded item refreshes its "condition: N%" tooltip and its
-- condition<=0 broken overlay: equipped items merged into the "All" view
-- are each their own row (stackKey returns nil for them), so a
-- condition-only drop on equipped armor (gambeson / gloves / boots)
-- wouldn't otherwise change any other field in this hash.
local function computeInvKey(uid, activeTab, items)
    local parts = { tostring(uid or ""), activeTab or "" }
    for _, it in ipairs(items) do
        parts[#parts + 1] = it.defName .. "/" .. tostring(it.currentFill)
            .. "/" .. (it.equipped and "e" or "i")
            .. "/" .. tostring(it.stackCount or 1)
            .. "/" .. tostring(it.sharpness or 0)
            .. "/" .. tostring(it.weight or 0)
            .. "/" .. tostring(it.condition or 0)
            .. "/" .. tostring(it.contentsKey or "")
    end
    return table.concat(parts, "|")
end

local function rebuildInventorySection()
    if not unitInfoV2.invRect then return end
    local rect = unitInfoV2.invRect
    local uid  = unitInfoV2.activeUid

    local rawItems = uid and collectInventoryAndEquipment(uid) or {}
    -- For display purposes we collapse identical non-equipped entries
    -- into stacks. RAW list still drives the total-weight footer so a
    -- "Steel Plate ×5" row contributes 5×1.2 kg to the total.
    local items = groupForDisplay(rawItems)
    local key = computeInvKey(uid, unitInfoV2.activeInvTab, items)
    if key == unitInfoV2.lastInvKey then return end

    -- Tear down previous build.
    for _, t in ipairs(unitInfoV2.invTabs) do
        if t.labelId then label.destroy(t.labelId) end
        if t.bgId    then UI.deleteElement(t.bgId)  end
    end
    unitInfoV2.invTabs = {}
    for _, e in ipairs(unitInfoV2.invListElements) do
        if e.kind == "label" then label.destroy(e.id)
        else                       UI.deleteElement(e.id)
        end
    end
    unitInfoV2.invListElements = {}
    unitInfoV2.invRows         = {}
    unitInfoV2.lastInvKey      = key

    if not uid then return end

    local uiscale = scale.get()
    local tabH    = math.floor(INV_TAB_H * uiscale)
    local topPad  = math.floor(INV_TAB_TOP_PAD * uiscale)
    local botPad  = math.floor(INV_TAB_BOTTOM_PAD * uiscale)
    local rowH    = math.floor(INV_ROW_H * uiscale)
    local rowPad  = math.floor(INV_ROW_PAD * uiscale)
    local iconSz  = math.floor(INV_ICON_SIZE * uiscale)
    local footerH = math.floor(INV_FOOTER_H * uiscale)
    local textPad = math.floor(INV_TEXT_PAD * uiscale)
    local sectPad = math.floor(SECTION_PAD * uiscale)

    -- 1. Tab strip
    local tabDefs = computeInvTabs(items)
    -- If the persisted active tab no longer exists (e.g. last weapon
    -- got dropped), fall back to All so the list isn't blank.
    local activeStillPresent = false
    for _, t in ipairs(tabDefs) do
        if t.name == unitInfoV2.activeInvTab then
            activeStillPresent = true; break
        end
    end
    if not activeStillPresent then
        unitInfoV2.activeInvTab = "All"
    end

    -- Pre-measure tab widths so we can plan row wraps without
    -- instantiating elements. Mirrors the sub-tab layout above.
    local tabTexts  = {}
    local tabWidths = {}
    for i, td in ipairs(tabDefs) do
        local s = td.name .. " (" .. td.count .. ")"
        tabTexts[i]  = s
        local tw     = engine.getTextWidth(hud.menuFont, s, INV_TAB_FONT_SIZE)
        tabWidths[i] = math.floor(tw) + 2 * INV_TAB_TEXT_PAD
    end

    -- Wrap plan: rows have (startIdx, endIdx, totalW). Reuses the
    -- sub-tab wrapper since the inv tabs share the same gap=0
    -- flush-tab styling.
    local rowGap    = math.floor(SUB_TAB_ROW_GAP * uiscale)
    local tabPlan   = planSubTabRows(rect, tabWidths)
    local cursorY   = rect.y + topPad
    for _, r in ipairs(tabPlan) do
        local rowStartX = rect.x + math.floor((rect.w - r.totalW) / 2)
        local cx = rowStartX
        for i = r.startIdx, r.endIdx do
            local label_text = tabTexts[i]
            local td         = tabDefs[i]
            local tabW       = tabWidths[i]

            local bgId = UI.newBox(
                "unit_info_v2_invtab_bg_" .. i,
                tabW, tabH,
                unitInfoV2.subTabUnselectedTexSet,
                SUB_TAB_TILE,
                1.0, 1.0, 1.0, 1.0, 0,
                unitInfoV2.page)
            UI.addToPage(unitInfoV2.page, bgId, cx, cursorY)
            UI.setZIndex(bgId, 11)
            UI.setClickable(bgId, true)
            UI.setOnClick(bgId, "onInventoryTabClick")

            local lblId = label.new({
                name     = "unit_info_v2_invtab_lbl_" .. i,
                text     = label_text,
                font     = hud.menuFont,
                fontSize = INV_TAB_FONT_SIZE,
                color    = SUB_TAB_TEXT_COLOR,
                page     = unitInfoV2.page,
                uiscale  = 1.0,
            })
            local lblH = label.getElementHandle(lblId)
            local lblW = select(1, label.getSize(lblId))
            UI.addToPage(unitInfoV2.page, lblH,
                cx + math.floor((tabW - lblW) / 2),
                cursorY + math.floor(tabH / 2)
                       + math.floor(INV_TAB_FONT_SIZE * 0.3) + 2)
            UI.setZIndex(lblH, 12)

            unitInfoV2.invTabs[#unitInfoV2.invTabs + 1] = {
                name = td.name, count = td.count,
                bgId = bgId, labelId = lblId,
            }
            cx = cx + tabW + INV_TAB_GAP
        end
        cursorY = cursorY + tabH + rowGap
    end
    -- Item-list area starts below the LAST tab row.
    local tabsBottomY = cursorY - rowGap
    applyInvTabStyling()

    -- 2. Item rows for the active tab
    local listX = rect.x + sectPad
    local listW = rect.w - 2 * sectPad
    local listY = tabsBottomY + botPad
    local maxRows = math.max(0, math.floor(
        (rect.y + rect.h - listY - footerH) / (rowH + rowPad)))

    local visibleItems = {}
    for _, it in ipairs(items) do
        if unitInfoV2.activeInvTab == "All"
           or it.category == unitInfoV2.activeInvTab then
            visibleItems[#visibleItems + 1] = it
        end
    end

    for i = 1, math.min(#visibleItems, maxRows) do
        local it    = visibleItems[i]
        local rowY  = listY + (i - 1) * (rowH + rowPad)

        -- Equipped highlight backdrop — a soft amber rect behind the
        -- whole row so equipped items pop without an extra icon.
        if it.equipped then
            local bgId = UI.newSprite(
                "unit_info_v2_inv_eqbg_" .. i,
                listW, rowH,
                unitInfoV2.whitePixelTex,
                INV_EQUIP_TINT[1], INV_EQUIP_TINT[2],
                INV_EQUIP_TINT[3], INV_EQUIP_TINT[4],
                unitInfoV2.page)
            UI.addToPage(unitInfoV2.page, bgId, listX, rowY)
            UI.setZIndex(bgId, 11)
            table.insert(unitInfoV2.invListElements,
                { kind = "sprite", id = bgId })
        end

        -- Icon (or transparent slot if texture missing). Tooltip + the
        -- right-click hit-zone go on a separate full-row sprite (below)
        -- so the whole row is a click target, not just the 28-pixel
        -- icon square.
        if it.iconTex then
            local iconY = rowY + math.floor((rowH - iconSz) / 2)
            local iconId = UI.newSprite(
                "unit_info_v2_inv_icon_" .. i,
                iconSz, iconSz,
                it.iconTex,
                1.0, 1.0, 1.0, 1.0,
                unitInfoV2.page)
            UI.addToPage(unitInfoV2.page, iconId, listX + textPad, iconY)
            UI.setZIndex(iconId, 12)
            table.insert(unitInfoV2.invListElements,
                { kind = "sprite", id = iconId })
            addBrokenOverlay(it, "unit_info_v2_inv_broken_" .. i,
                listX + textPad, iconY, iconSz, iconSz, 13,
                unitInfoV2.invListElements)
        end

        -- Weight (right-aligned) — built FIRST so we can measure its
        -- pixel width and use that to bound the name's available
        -- horizontal space below. Stacked rows multiply by stackCount
        -- so the line reads "Steel Plate ×5 ... 6.00 kg".
        local rowWeight = (it.weight or 0) * (it.stackCount or 1)
        local wText = string.format("%.2f kg", rowWeight)
        local wLbl = label.new({
            name     = "unit_info_v2_inv_w_" .. i,
            text     = wText,
            font     = hud.menuFont,
            fontSize = 14,
            color    = {0.85, 0.85, 0.85, 1.0},
            page     = unitInfoV2.page,
            uiscale  = 1.0,
        })
        local wH = label.getElementHandle(wLbl)
        local wW = select(1, label.getSize(wLbl))
        UI.addToPage(unitInfoV2.page, wH,
            listX + listW - textPad - wW,
            rowY + math.floor(rowH / 2) + math.floor(14 * 0.3))
        UI.setZIndex(wH, 12)
        table.insert(unitInfoV2.invListElements,
            { kind = "label", id = wLbl })

        -- Display name. Truncated with ".." when the name would
        -- otherwise run into the weight column. Available width =
        -- (weight's left edge) − (name's left edge) − a small gap.
        local nameX = listX + textPad + iconSz + textPad
        local nameMaxPx = (listX + listW - textPad - wW) - nameX
                        - math.floor(4 * uiscale)
        local rawName = it.displayName
        if (it.stackCount or 1) > 1 then
            rawName = string.format("%s ×%d", rawName, it.stackCount)
        end
        local nameText = truncateToWidth(rawName, hud.menuFont,
                                          14, nameMaxPx)
        local nameLbl = label.new({
            name     = "unit_info_v2_inv_name_" .. i,
            text     = nameText,
            font     = hud.menuFont,
            fontSize = 14,
            color    = it.equipped and {1.0, 0.95, 0.7, 1.0}
                                    or {1.0, 1.0, 1.0, 1.0},
            page     = unitInfoV2.page,
            uiscale  = 1.0,
        })
        local nameH = label.getElementHandle(nameLbl)
        UI.addToPage(unitInfoV2.page, nameH, nameX,
            rowY + math.floor(rowH / 2) + math.floor(14 * 0.3))
        UI.setZIndex(nameH, 12)
        table.insert(unitInfoV2.invListElements,
            { kind = "label", id = nameLbl })

        -- Full-row right-click hit-zone. Transparent sprite at z=14
        -- so it's above the icon + labels but doesn't visually
        -- intrude. Owns the row's tooltip too — since it sits on top
        -- of the icon, the icon's hover tooltip would otherwise be
        -- blocked. Click hit-test only fires for callbacks that are
        -- registered, so left-click on the row stays a no-op (no
        -- onClick registered).
        local hitId = UI.newSprite(
            "unit_info_v2_inv_hit_" .. i,
            listW, rowH,
            unitInfoV2.whitePixelTex,
            1.0, 1.0, 1.0, 0.0,
            unitInfoV2.page)
        UI.addToPage(unitInfoV2.page, hitId, listX, rowY)
        UI.setZIndex(hitId, 14)
        UI.setClickable(hitId, true)
        UI.setOnRightClick(hitId, "onInventoryItemRightClick")

        UI.setTooltipRich(hitId, {
            text = it.displayName,
            hint = buildItemHint(it, it.equipped and it.equippedSlot or nil),
        })

        table.insert(unitInfoV2.invListElements,
            { kind = "sprite", id = hitId })
        unitInfoV2.invRows[#unitInfoV2.invRows + 1] = {
            hitId = hitId,
            item  = it,
        }
    end

    -- 3. Footer: total weight across the FULL raw item set (not just
    -- the active tab; not the stacked groups). A row reading
    -- "Steel Plate ×5 = 6.00 kg" still contributes 6 kg to the total.
    local total = 0
    for _, it in ipairs(rawItems) do total = total + (it.weight or 0) end
    local footerLbl = label.new({
        name     = "unit_info_v2_inv_footer",
        text     = string.format("Total: %.2f kg", total),
        font     = hud.menuFont,
        fontSize = 14,
        color    = {0.85, 0.85, 0.85, 1.0},
        page     = unitInfoV2.page,
        uiscale  = 1.0,
    })
    local fH = label.getElementHandle(footerLbl)
    local fW = select(1, label.getSize(footerLbl))
    local footerY = rect.y + rect.h - footerH
                  + math.floor(footerH / 2) + math.floor(14 * 0.3)
    UI.addToPage(unitInfoV2.page, fH,
        listX + listW - textPad - fW, footerY)
    UI.setZIndex(fH, 12)
    table.insert(unitInfoV2.invListElements,
        { kind = "label", id = footerLbl })
end

-- Public click handler for the inventory tab strip. Mirrors the
-- pattern used by sub-tabs / scroll arrows.
function unitInfoV2.handleInvTabClick(elemHandle)
    for _, t in ipairs(unitInfoV2.invTabs) do
        if t.bgId == elemHandle then
            if unitInfoV2.activeInvTab ~= t.name then
                unitInfoV2.activeInvTab = t.name
                unitInfoV2.lastInvKey = nil  -- force rebuild next tick
            end
            return true
        end
    end
    return false
end

-- Built storage-capable buildings adjacent (Chebyshev ≤ 1) to the
-- unit. Used by the right-click "Store" path so the menu only offers
-- the player real, reachable targets.
local function findAdjacentStorageBuildings(uid)
    local info = unit.getInfo(uid)
    if not info then return {} end
    local utx = math.floor(info.gridX)
    local uty = math.floor(info.gridY)
    local listStr = building.list()
    if not listStr or listStr == "No buildings placed" then return {} end
    local result = {}
    for id in listStr:gmatch("id=(%d+)") do
        local bid = tonumber(id)
        if bid and building.getActivity(bid) == "built" then
            local cap = building.getStorageCapacity(bid)
            if cap and cap > 0 then
                local binfo = building.getInfo(bid)
                if binfo then
                    local tw = binfo.tileW or 1
                    local th = binfo.tileH or 1
                    local dx, dy = 0, 0
                    if utx < binfo.gridX then
                        dx = binfo.gridX - utx
                    elseif utx >= binfo.gridX + tw then
                        dx = utx - (binfo.gridX + tw - 1)
                    end
                    if uty < binfo.gridY then
                        dy = binfo.gridY - uty
                    elseif uty >= binfo.gridY + th then
                        dy = uty - (binfo.gridY + th - 1)
                    end
                    if math.max(dx, dy) <= 1 then
                        result[#result + 1] = {
                            bid = bid,
                            displayName = binfo.displayName or binfo.defName,
                        }
                    end
                end
            end
        end
    end
    return result
end

-- All slots on the unit's equipment class that accept items of the
-- given kind. Used by the right-click "Equip" path: if exactly one
-- slot matches we equip into it directly; if multiple match we surface
-- a submenu so the player picks. Returns a list of slot tables.
local function findEquipSlotsForKind(uid, itemKind)
    local info = uid and unit.getInfo(uid) or nil
    if not info or not info.equipmentClass then return {} end
    local cls = equipment.getClass(info.equipmentClass)
    if not cls or not cls.slots then return {} end
    local out = {}
    for _, s in ipairs(cls.slots) do
        if s.kind == itemKind then out[#out + 1] = s end
    end
    return out
end

-- For single-slot cases (or when the caller picks a specific slot),
-- choose between "empty matching" and "first matching" — preferring
-- empty when possible. Returns the chosen slot id.
local function preferredEmptySlot(uid, slots)
    local lo = equipment.getLoadout(uid) or {}
    for _, s in ipairs(slots) do
        if not lo[s.id] then return s.id end
    end
    return slots[1] and slots[1].id or nil
end

-- Right-click on an inventory row → context menu with Equip / Unequip.
-- Routed via uiManager.onInventoryItemRightClick (set by the row's
-- hit-zone in rebuildInventorySection).
function unitInfoV2.handleInvItemRightClick(elemHandle)
    local row
    for _, r in ipairs(unitInfoV2.invRows) do
        if r.hitId == elemHandle then row = r; break end
    end
    if not row then return false end

    local uid = unitInfoV2.activeUid
    if not uid then return false end
    local item = row.item

    local items = {}
    if item.equipped then
        -- Three flavours of "equipped":
        --  - slot equipment (item.accessoryIndex == nil, item.equippedSlot is a slot id)
        --  - accessory (item.accessoryIndex set; uses unequipAccessory)
        --  - either may be flagged `unequippable` — grey out instead.
        if item.unequippable then
            items[1] = { label = "Unequip", enabled = false }
        elseif item.accessoryIndex then
            local idx = item.accessoryIndex
            items[1] = {
                label    = "Unequip",
                callback = function()
                    equipment.unequipAccessory(uid, idx)
                    unitInfoV2.lastInvKey   = nil
                    unitInfoV2.lastEquipKey = nil
                end,
            }
        else
            items[1] = {
                label    = "Unequip",
                callback = function()
                    equipment.unequip(uid, item.equippedSlot)
                    unitInfoV2.lastInvKey   = nil
                    unitInfoV2.lastEquipKey = nil
                end,
            }
        end
    elseif item.kind == "accessory" then
        -- Accessories don't go in a silhouette slot — they append to
        -- the unit's accessory list. No submenu, no slot matching.
        items[1] = {
            label    = "Equip",
            callback = function()
                equipment.equipAccessory(uid, item.defName, item.instanceId)
                unitInfoV2.lastInvKey   = nil
                unitInfoV2.lastEquipKey = nil
            end,
        }
    else
        local matching = findEquipSlotsForKind(uid, item.kind)
        if #matching == 0 then
            items[1] = {
                label   = "Equip",
                enabled = false,
            }
        elseif #matching == 1 then
            local slotId = matching[1].id
            items[1] = {
                label    = "Equip",
                callback = function()
                    equipment.equip(uid, slotId, item.defName, item.instanceId)
                    unitInfoV2.lastInvKey   = nil
                    unitInfoV2.lastEquipKey = nil
                end,
            }
        else
            -- Multiple matching slots — surface each as a submenu item
            -- so the player picks. Slot display names (e.g. "Weapon
            -- (R)") come from the equipment class YAML.
            local sub = {}
            for _, s in ipairs(matching) do
                sub[#sub + 1] = {
                    label    = s.name,
                    callback = function()
                        equipment.equip(uid, s.id, item.defName, item.instanceId)
                        unitInfoV2.lastInvKey   = nil
                        unitInfoV2.lastEquipKey = nil
                    end,
                }
            end
            items[1] = { label = "Equip", submenu = sub }
        end
    end

    -- Contents: item-containers (first-aid kit / toolbox) open a
    -- floating list of what they hold. Available whether carried or
    -- equipped — you can inspect a kit without unslinging it.
    if item.kind == "container" then
        items[#items + 1] = {
            label    = "Contents",
            callback = function()
                local icp = require("scripts.item_contents_panel")
                local mx, my = engine.getMousePosition()
                local fbW, fbH = engine.getFramebufferSize()
                local ww, wh = engine.getWindowSize()
                if ww and wh and ww > 0 and wh > 0 then
                    mx = mx * (fbW / ww)
                    my = my * (fbH / wh)
                end
                icp.openFor(uid, item.defName, mx, my, item.instanceId)
            end,
        }
    end

    -- Storage: append "Store in <cargo>" entries for each adjacent
    -- built cargo. Equipped/accessory items can't be deposited
    -- directly (player must unequip first). The deposit API enforces
    -- capacity; menu entries don't pre-check, so a deposit that
    -- overflows the cargo will return false and the item stays put.
    if not item.equipped then
        for _, c in ipairs(findAdjacentStorageBuildings(uid)) do
            items[#items + 1] = {
                label    = "Store in " .. c.displayName,
                callback = function()
                    unit.depositToCargo(uid, c.bid, item.defName,
                                        item.instanceId)
                    unitInfoV2.lastInvKey = nil
                end,
            }
        end
    end

    -- Open the context menu at the current mouse position, in the
    -- same framebuffer-pixel space the widget expects.
    local contextMenu = require("scripts.ui.context_menu")
    local mx, my = engine.getMousePosition()
    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh = engine.getWindowSize()
    if ww and wh and ww > 0 and wh > 0 then
        mx = mx * (fbW / ww)
        my = my * (fbH / wh)
    end
    contextMenu.show(items, mx, my)
    return true
end

-- Right-click on a silhouette slot (or its equipped icon) → context
-- menu with Equip / Unequip. The Equip path lists matching-kind items
-- from the unit's inventory as a submenu; clicking one calls
-- equipment.equip into THIS slot (the swap-on-equip semantics in the
-- engine handle any currently-equipped item by returning it to the
-- inventory).
function unitInfoV2.handleEquipSlotRightClick(elemHandle)
    local rec
    for _, s in ipairs(unitInfoV2.equipSlots) do
        if s.slotElemId == elemHandle or s.iconElemId == elemHandle then
            rec = s; break
        end
    end
    if not rec then return false end

    local uid = unitInfoV2.activeUid
    if not uid then return false end

    -- Build the inventory-side picklist for "Equip into this slot":
    -- every inventory item whose kind matches this slot's accepted
    -- kind. Equipped items aren't included (they're already in a slot
    -- — the user would unequip first).
    local equipSubmenu = {}
    local inv = unit.getInventory(uid) or {}
    for _, it in ipairs(inv) do
        if it.kind == rec.slot.kind then
            local defName    = it.defName
            local instId     = it.instanceId
            local displayNm  = it.displayName or it.defName
            equipSubmenu[#equipSubmenu + 1] = {
                label    = displayNm,
                icon     = it.iconTex,
                callback = function()
                    equipment.equip(uid, rec.slotId, defName, instId)
                    unitInfoV2.lastInvKey   = nil
                    unitInfoV2.lastEquipKey = nil
                end,
            }
        end
    end

    local items = {}
    if rec.equippedItem then
        items[#items + 1] = {
            label    = "Unequip",
            callback = function()
                equipment.unequip(uid, rec.slotId)
                unitInfoV2.lastInvKey   = nil
                unitInfoV2.lastEquipKey = nil
            end,
        }
    end
    if #equipSubmenu > 0 then
        items[#items + 1] = { label = "Equip", submenu = equipSubmenu }
    else
        items[#items + 1] = {
            label   = "Equip",
            enabled = false,
        }
    end

    local contextMenu = require("scripts.ui.context_menu")
    local mx, my = engine.getMousePosition()
    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh = engine.getWindowSize()
    if ww and wh and ww > 0 and wh > 0 then
        mx = mx * (fbW / ww)
        my = my * (fbH / wh)
    end
    contextMenu.show(items, mx, my)
    return true
end

-- Right-click on an accessory row icon (right side of equipment
-- section) → Unequip menu. Greyed out for unequippable items.
function unitInfoV2.handleAccessoryRowRightClick(elemHandle)
    local row
    for _, r in ipairs(unitInfoV2.accessoryRows) do
        if r.elemId == elemHandle then row = r; break end
    end
    if not row then return false end
    local uid = unitInfoV2.activeUid
    if not uid then return false end

    local items
    if row.item.unequippable then
        items = { { label = "Unequip", enabled = false } }
    else
        local idx = row.accessoryIndex
        items = { {
            label    = "Unequip",
            callback = function()
                equipment.unequipAccessory(uid, idx)
                unitInfoV2.lastInvKey   = nil
                unitInfoV2.lastEquipKey = nil
            end,
        } }
    end

    local contextMenu = require("scripts.ui.context_menu")
    local mx, my = engine.getMousePosition()
    local fbW, fbH = engine.getFramebufferSize()
    local ww, wh = engine.getWindowSize()
    if ww and wh and ww > 0 and wh > 0 then
        mx = mx * (fbW / ww)
        my = my * (fbH / wh)
    end
    contextMenu.show(items, mx, my)
    return true
end

-----------------------------------------------------------
-- Header: stacks Name / Type / Role / Action rows in a virtual rect.
-- No box around it; section boundaries are marked by horizontal rules.
-- Name (#264), Type and Action are live — Name shows the unit's personal
-- name (or species label for the unnamed), Type shows the unit's def
-- name, Action shows the current AI action mapped through ACTION_DISPLAY
-- below. Role remains a placeholder (see #265).
-----------------------------------------------------------

-- Map unit_ai action names → human-readable display strings.
-- Anything missing falls back to the raw action name so a new action
-- shows up visibly instead of disappearing.
local ACTION_DISPLAY = {
    idle               = "Idling",
    wander             = "Wandering",
    follow_command     = "Following order",
    drink_from_canteen = "Drinking",
    eat_from_inventory = "Eating",
    refill_canteen     = "Refilling canteen",
    search_for_water   = "Searching for water",
    drink_from_source  = "Drinking from source",
    notify_allies      = "Notifying allies",
    build_nearby       = "Working",
    deliver_to_build_site = "Delivering materials",
    store_materials       = "Storing materials",
}

local function placeHeader(x, y, w, h)
    -- Row 4 starts as a dash; the tick refresh fills it in from the
    -- selected unit's currentAction.
    local rows = { "Name", "acolyte", "Role", "—" }
    local rowH = math.floor(h / #rows)
    local fontSize = 16
    for i, text in ipairs(rows) do
        local isLive = (i == 1 or i == 2 or i == 4)  -- name + type + action are real content
        local lblId = label.new({
            name     = "unit_info_v2_header_row" .. i,
            text     = text,
            font     = hud.menuFont,
            fontSize = fontSize,
            color    = isLive
                          and {1.0, 1.0, 1.0, 1.0}      -- bright
                          or  {0.75, 0.75, 0.75, 1.0},  -- placeholders: dim
            page     = unitInfoV2.page,
            uiscale  = 1.0,
        })
        table.insert(unitInfoV2.ownedLabels, lblId)
        local lblHandle = label.getElementHandle(lblId)
        local rowY = y + (i - 1) * rowH + math.floor(rowH * 0.5)
                     + math.floor(fontSize * 0.3)
        UI.addToPage(unitInfoV2.page, lblHandle, x + SECTION_PAD, rowY)
        UI.setZIndex(lblHandle, 12)
        if i == 1 then
            unitInfoV2.headerNameLabelId = lblId
        elseif i == 2 then
            unitInfoV2.headerTypeLabelId = lblId
        elseif i == 4 then
            unitInfoV2.headerActionLabelId = lblId
        end
    end

    -- "Log" button (top-right of the header): opens the per-unit log
    -- panel (Event/Combat/Injury collated for activeUid). Box + label +
    -- click callback, mirroring the sub-tab cells; routed through
    -- ui_manager.onUnitInfoLogClick → handleLogClick.
    local btnW = math.floor(56 * scale.get())
    local btnH = math.floor(rowH * 1.4)
    local btnX = x + w - btnW - SECTION_PAD
    local btnY = y + math.floor((h - btnH) / 2)
    unitInfoV2.logBtnBoxId = UI.newBox(
        "unit_info_v2_log_btn",
        btnW, btnH,
        unitInfoV2.subTabUnselectedTexSet,
        16,
        1.0, 1.0, 1.0, 1.0,
        0,
        unitInfoV2.page)
    UI.addToPage(unitInfoV2.page, unitInfoV2.logBtnBoxId, btnX, btnY)
    UI.setClickable(unitInfoV2.logBtnBoxId, true)
    UI.setOnClick(unitInfoV2.logBtnBoxId, "onUnitInfoLogClick")
    UI.setZIndex(unitInfoV2.logBtnBoxId, 12)

    local logLblId = label.new({
        name     = "unit_info_v2_log_btn_label",
        text     = "Log",
        font     = hud.menuFont,
        fontSize = 14,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = unitInfoV2.page,
        uiscale  = 1.0,
    })
    table.insert(unitInfoV2.ownedLabels, logLblId)
    local logLblHandle = label.getElementHandle(logLblId)
    local logLblW = engine.getTextWidth(hud.menuFont, "Log", 14)
    UI.addToPage(unitInfoV2.page, logLblHandle,
        btnX + math.floor((btnW - logLblW) / 2),
        btnY + math.floor((btnH + 14) / 2))
    UI.setZIndex(logLblHandle, 13)
end

-----------------------------------------------------------
-- Bootstrap: create the page + layout once HUD assets are ready.
-----------------------------------------------------------

local function bootstrap()
    if unitInfoV2.bootstrapped then return end
    if not (hud and hud.boxTexSet and hud.menuFont
            and hud.fbW and hud.fbW > 0) then
        return  -- HUD not ready yet, retry next tick
    end

    unitInfoV2.bootstrapped = true
    unitInfoV2.page = UI.newPage("unit_info_v2", "overlay")

    -- engine.loadTexture caches by path so this returns the same
    -- handle that drag_select / tooltip separators already use.
    unitInfoV2.whitePixelTex = engine.loadTexture(
        "assets/textures/ui/hud/utility/white.png")
    unitInfoV2.tabSelectedTex = engine.loadTexture(
        "assets/textures/ui/unittabselected.png")

    -- Sub-tab look (Status / Stats / Mental / …) re-uses the menu
    -- tab textures so it visually matches the settings + create-world
    -- panels. boxTextures.load caches by path so we're sharing the
    -- same texture set those menus loaded earlier.
    unitInfoV2.subTabSelectedTexSet =
        boxTextures.load("assets/textures/ui/tabselected", "tabselected")
    unitInfoV2.subTabUnselectedTexSet =
        boxTextures.load("assets/textures/ui/tabunselected", "tabunselected")

    -- Take over the unit-info display. The legacy unit watcher
    -- (unit_info_panel.lua) already self-suppresses via the
    -- __unit_info_v2_suppress sentinel, so it never pushes unit content
    -- into the shared HUD info panel while v2 is loaded. We do NOT blanket-
    -- suppress that shared panel here: it is still the renderer for tile /
    -- building / ground-item info, which must keep showing (#136). Instead
    -- suppression is driven dynamically in update() — the shared panel is
    -- hidden only while v2's pane is actually on screen (a unit is
    -- selected), so the two can never visually overlap, and released the
    -- moment the pane hides so tile/building/item readouts reappear.
end

-----------------------------------------------------------
-- Rebuild the layout. Called on bootstrap and on every selection
-- change (because content per-section will eventually depend on the
-- selected unit). For the skeleton, layout is constant.
-----------------------------------------------------------

local function rebuildLayout()
    clearOwned()

    local uiscale  = scale.get()
    local panelW   = math.floor(PANEL_W * uiscale)
    local outerPad = math.floor(PANEL_PAD * uiscale)
    local sectGap  = math.floor(SECTION_GAP * uiscale)

    local fbW = hud.fbW
    local fbH = hud.fbH
    local panelX = fbW - panelW
    local panelY = 0
    local panelH = fbH

    -- Outer pane
    unitInfoV2.outerBoxId = UI.newBox(
        "unit_info_v2_outer",
        panelW, panelH,
        hud.boxTexSet,
        OUTER_TILE,
        1.0, 1.0, 1.0, 1.0,
        0,
        unitInfoV2.page
    )
    UI.addToPage(unitInfoV2.page, unitInfoV2.outerBoxId, panelX, panelY)
    UI.setZIndex(unitInfoV2.outerBoxId, 10)

    -- Section sizes (scaled)
    local contentX = panelX + outerPad
    local contentW = panelW - 2 * outerPad
    local cursorY  = panelY + outerPad

    local tabsH   = math.floor(TABS_H   * uiscale)
    local headerH = math.floor(HEADER_H * uiscale)
    local statsH  = math.floor(STATS_H  * uiscale)
    local equipH  = math.floor(EQUIP_H  * uiscale)
    local dThick  = math.floor(DIVIDER_THICKNESS * uiscale)

    -- Helper: lay down a section's content, then a divider beneath it.
    -- The cursorY ends up below the divider, ready for the next
    -- section. The last section in the panel skips its divider.
    local function nextSection(content, h, drawDivider)
        content(contentX, cursorY, contentW, h)
        cursorY = cursorY + h + sectGap
        if drawDivider then
            placeDivider(contentX, cursorY, contentW, uiscale)
            cursorY = cursorY + dThick + sectGap
        end
    end

    -- Sprite tab strip. We just record the section's rect here;
    -- rebuildTabs() (driven by update() on selection-identity change)
    -- creates the actual tab + arrow elements inside this rect.
    nextSection(function(x, y, w, h)
        unitInfoV2.tabsRect = { x = x, y = y, w = w, h = h }
    end, tabsH, true)

    -- Header (Name / Type / Role rows)
    nextSection(function(x, y, w, h)
        placeHeader(x, y, w, h)
    end, headerH, true)

    -- Stats / Physical / Mental / Skills sub-tabs section. We just
    -- record the section's rect; rebuildSubTabs (driven by update())
    -- populates the sub-tab strip + content area inside it.
    nextSection(function(x, y, w, h)
        unitInfoV2.statsRect = { x = x, y = y, w = w, h = h }
    end, statsH, true)

    -- Equipment. Only record the rect here; rebuildEquipmentSection
    -- (driven by update() on selection-identity change) populates the
    -- silhouette + slot overlays + accessory list inside it.
    nextSection(function(x, y, w, h)
        unitInfoV2.equipRect = { x = x, y = y, w = w, h = h }
    end, equipH, true)

    -- Inventory: remaining height, no divider after. Just record the
    -- rect; rebuildInventorySection (driven by update()) populates the
    -- tab strip + item list + total-weight footer inside it.
    local invH = (panelY + panelH - outerPad) - cursorY
    if invH > 0 then
        unitInfoV2.invRect = { x = contentX, y = cursorY,
                               w = contentW, h = invH }
    end
end

-----------------------------------------------------------
-- Public lifecycle
-----------------------------------------------------------

function unitInfoV2.init(scriptId)
    unitInfoV2.scriptId = scriptId
    engine.logInfo("Unit info v2 initialising...")

    -- Suppress the old unit_info_panel push so the existing HUD info
    -- panel stays empty for unit selections (the new pane owns it).
    local oldWatch = package.loaded["scripts.unit_info_panel"]
    if oldWatch then
        oldWatch.suppressed = true
    else
        -- Old module hasn't been required yet; tag it through a
        -- pending flag the old module will check during its own init.
        -- We don't load it here because that'd start its update loop.
        package.loaded.__unit_info_v2_suppress = true
    end
end

function unitInfoV2.update(dt)
    bootstrap()
    if not unitInfoV2.page then return end

    -- Rebuild on first show. For now layout is static so we only need
    -- to build it once, but we re-call on selection-identity change
    -- in later passes so leave the hook here.
    if not unitInfoV2.outerBoxId then
        rebuildLayout()
        rebuildSubTabs()
        UI.hidePage(unitInfoV2.page)
    end

    local sel = unit.getSelected()
    local count = (sel and #sel) or 0

    -- Selection-identity change → rebuild the tab strip. selectionKey
    -- is order-insensitive so picking the same units in a different
    -- click order doesn't churn.
    local key = selectionKey(sel)
    if key ~= unitInfoV2.lastSelKey then
        rebuildTabs(sel)
        unitInfoV2.lastSelKey = key
        -- Selecting a unit takes ownership of the readout, so clear any
        -- world-tile cursor selection — exactly as the legacy unit
        -- watcher did (unit_info_panel.lua), which is suppressed under v2.
        -- Without this the tile-info watcher keeps a stale tile readout
        -- alive in the (suppressed) shared panel, which then resurfaces
        -- the moment the v2 pane hides and we unsuppress it (#136).
        if count > 0 and hud and hud.worldId then
            world.clearWorldCursorSelect(hud.worldId)
        end
    end

    -- Stats sub-panel: rebuild content when the active unit OR sub-tab
    -- changes; otherwise just refresh in-place values.
    if unitInfoV2.activeUid then
        rebuildStatsContent()
        if unitInfoV2.statsRefresh then
            unitInfoV2.statsRefresh(unitInfoV2.activeUid)
        end
    end

    -- Equipment section: rebuild when the active unit changes
    -- (silhouette + slot overlays per the unit's equipment class).
    rebuildEquipmentSection()

    -- Inventory section: dynamic tab strip + per-tab item list. Cached
    -- by a content-hash so the per-tick redraw is cheap when nothing
    -- has changed.
    rebuildInventorySection()

    -- Header name row (#264): the unit's personal name when it has one,
    -- else its species label (display_name / prettified def name).
    if unitInfoV2.activeUid and unitInfoV2.headerNameLabelId then
        local info = unit.getInfo(unitInfoV2.activeUid)
        local nameText
        if info and info.name and info.name ~= "" then
            nameText = info.name
        else
            nameText = (info and info.displayName) or "—"
        end
        label.setText(unitInfoV2.headerNameLabelId, nameText)
    end

    -- Header type row: rewrite each tick from the active unit's def.
    -- All units are "acolyte" right now so this is effectively a no-op
    -- until other unit types exist, but the wiring is in place.
    if unitInfoV2.activeUid and unitInfoV2.headerTypeLabelId then
        local info = unit.getInfo(unitInfoV2.activeUid)
        local typeName = info and info.defName or "?"
        label.setText(unitInfoV2.headerTypeLabelId, typeName)
    end

    -- Header action row: pull currentAction from unit_ai and map it
    -- through ACTION_DISPLAY. Unmapped names show raw so unknown
    -- actions are visible rather than blank.
    if unitInfoV2.activeUid and unitInfoV2.headerActionLabelId then
        local unitAi = require("scripts.unit_ai")
        local aiSt = unitAi.getState and unitAi.getState(unitInfoV2.activeUid)
        local action = aiSt and aiSt.currentAction
        local text = (action and (ACTION_DISPLAY[action] or action)) or "—"
        label.setText(unitInfoV2.headerActionLabelId, text)
    end

    -- Refresh every visible tab's portrait. Prefer the unit def's
    -- authored portrait (static), falling back to the live animation
    -- frame for defs that ship no `portrait:`. Skip setSpriteTexture
    -- when the handle hasn't changed to avoid needless mutations — for
    -- an authored portrait this means it's set once and then left
    -- alone. We only refresh VISIBLE tabs — scrolled-off tabs would
    -- just thrash invisibly.
    if unitInfoV2.tabLayout and #unitInfoV2.tabs > 0 then
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

    -- Visibility gate. The pane belongs to the zoomed-in gameplay view,
    -- so a selection alone is not enough to show it: it must also hide
    -- when gameplay UI isn't actually active or when the camera isn't in
    -- the zoomed-in band (zoomed-out map / fade band). Driving show/hide
    -- off a single predicate keeps it in sync with zoom + menu transitions
    -- instead of only selection count, so the pane can no longer persist
    -- over the zoomed-out map or non-gameplay menus (#137).
    --
    -- isGameplayInputActive() is the authoritative "gameplay UI is active"
    -- check (#182): it is false for any non-gameplay currentMenu AND while
    -- the pause overlay is up. That covers the modal paths that DON'T call
    -- hud.hide() — pauseMenu.show() and uiManager.showMenu("settings") in
    -- its keepWorld path — which a bare hud.visible check would miss.
    local uiManager = require("scripts.ui_manager")
    local want = count > 0
                 and uiManager.isGameplayInputActive()
                 and hud.currentView == "zoomed_in"
    if want ~= unitInfoV2.lastWantVisible then
        if want then
            UI.showPage(unitInfoV2.page)
            -- Pane is coming up over the right edge: hide the shared HUD
            -- info panel so a lingering tile/building/item readout can't
            -- overlap it (#136). Released again the moment the pane hides.
            infoPanel.suppress("unit_info_v2")
        else
            UI.hidePage(unitInfoV2.page)
            -- Pane gone: let the shared panel render tile/building/item
            -- info again (refresh re-derives from content + any other
            -- suppressors, so it only resurfaces if it has something).
            infoPanel.unsuppress("unit_info_v2")
        end
        unitInfoV2.lastWantVisible = want
    end
    unitInfoV2.lastSelCount = count
end

function unitInfoV2.onFramebufferResize(width, height)
    -- Layout depends on framebuffer dimensions, so rebuild on resize.
    if unitInfoV2.page then
        rebuildLayout()
        -- Tabs got cleared by clearOwned in rebuildLayout. Re-create
        -- them for the current selection (lastSelKey reset so the next
        -- update tick will see "new" selection and rebuild).
        unitInfoV2.lastSelKey = ""
        -- Re-apply the same visibility gate as update() so a resize while
        -- a menu / pause / settings overlay is open or the camera is zoomed
        -- out doesn't flash the pane back on (#137).
        local uiManager = require("scripts.ui_manager")
        local want = unitInfoV2.lastSelCount > 0
                     and uiManager.isGameplayInputActive()
                     and hud.currentView == "zoomed_in"
        if want then
            UI.showPage(unitInfoV2.page)
            infoPanel.suppress("unit_info_v2")
        else
            UI.hidePage(unitInfoV2.page)
            infoPanel.unsuppress("unit_info_v2")
        end
        unitInfoV2.lastWantVisible = want
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

-- "Log" button → open the per-unit collated log for the active unit.
function unitInfoV2.handleLogClick(elemHandle)
    if elemHandle ~= unitInfoV2.logBtnBoxId then return false end
    if not unitInfoV2.activeUid then return true end
    require("scripts.unit_log").show(unitInfoV2.activeUid)
    return true
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

-- Bg-box click is a no-op; we register the callback solely so the
-- element stays in the engine's clickable set, which is what the
-- wheel routing checks via findClickableElementAt.
function unitInfoV2.onStatsPanelBgClick(elemHandle)
    return elemHandle == unitInfoV2.statsBgClickId
end

-- Clicking a sub-tab (Status / Stats / Mental / …) switches which
-- panel content is visible. The active sub-tab persists across unit
-- switches so comparisons stay on the same panel.
function unitInfoV2.handleSubTabClick(elemHandle)
    for _, t in ipairs(unitInfoV2.subTabs) do
        if t.bgId == elemHandle then
            if unitInfoV2.activeSubTab ~= t.name then
                unitInfoV2.activeSubTab = t.name
                applySubTabStyling()
                rebuildStatsContent()
            end
            return true
        end
    end
    return false
end

function unitInfoV2.shutdown()
    -- Re-enable the old watcher in case we're being unloaded.
    local oldWatch = package.loaded["scripts.unit_info_panel"]
    if oldWatch then
        oldWatch.suppressed = false
    end
    -- Also clear the global suppression sentinel. init() sets this when the
    -- legacy watcher isn't loaded yet, and a LATER require of the watcher
    -- reads it to start suppressed — so leaving it set keeps the legacy
    -- panel suppressed forever after v2 shuts down (#87).
    package.loaded.__unit_info_v2_suppress = nil

    -- Restore the shared HUD info panel — we suppressed it on bootstrap
    -- so v2 could own the unit-info display. Releasing re-derives from
    -- content, so it only resurfaces if it has something to show (an
    -- unconditional show would pop an empty/stale panel, #134). If the
    -- HUD is also hidden, its own suppressor keeps the panel down.
    infoPanel.unsuppress("unit_info_v2")

    clearOwned()
    if unitInfoV2.page then
        UI.deletePage(unitInfoV2.page)
        unitInfoV2.page = nil
    end
    unitInfoV2.bootstrapped = false
    unitInfoV2.lastSelCount = 0
    unitInfoV2.lastWantVisible = false
end

return unitInfoV2
