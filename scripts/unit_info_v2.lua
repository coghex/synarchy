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
local label       = require("scripts.ui.label")
local scale       = require("scripts.ui.scale")
local boxTextures = require("scripts.ui.box_textures")
local stats       = require("scripts.unit_stats")

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
local SUB_TAB_LIST       = { "Status", "Stats", "Mental", "Skill", "Attributes", "Effects" }
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
local CONTENT_LEFT_PAD   = 16
local CONTENT_RIGHT_PAD  = 16
local CONTENT_DIM_COLOR  = { 0.78, 0.78, 0.78, 1.0 }
local CONTENT_VAL_COLOR  = { 1.0, 1.0, 1.0, 1.0 }
local CONTENT_TOP_PAD    = 8
local ICON_SIZE          = 40

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
        else                       UI.deleteElement(e.id)
        end
    end
    unitInfoV2.statsContentElements = {}
    unitInfoV2.statsRefresh = nil
    unitInfoV2.lastContentUid = nil
    unitInfoV2.lastContentTab = nil

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
    stamina      = { icon = "stamina",      name = "Stamina",
        desc = "Drives sustained physical effort. Drops with action, regenerates with rest." },
    hunger       = { icon = "hunger",       name = "Hunger",
        desc = "Need for food. High hunger reduces stamina regen and eventually starves the unit." },
    hydration    = { icon = "hydration",    name = "Hydration",
        desc = "Need for water. Drops faster than hunger; critical in hot climates." },
    strength     = { icon = "strength",     name = "Strength",
        desc = "Affects melee damage, carry capacity, and heavy-tool work speed." },
    endurance    = { icon = "endurance",    name = "Endurance",
        desc = "Sets the ceiling for stamina and slows its drain under load." },
    reflexes     = { icon = "reflexes",     name = "Reflexes",
        desc = "Affects dodge, parry, and the reaction window for ranged attacks." },
    constitution = { icon = "constitution", name = "Constitution",
        desc = "Resistance to injury, illness, and environmental damage." },
    metabolism   = { icon = "metabolism",   name = "Metabolism",
        desc = "How quickly the unit burns calories. High metabolism eats more but recovers faster." },
    perception   = { icon = "perception",   name = "Perception",
        desc = "Sight range, hearing, and chance to spot hidden things." },
    height       = { icon = "height",       name = "Height",
        desc = "Affects reach, line of sight, and the cap for skeletal lean mass." },
    weight       = { icon = "weight",       name = "Weight",
        desc = "Total body mass. Heavier units move slower and apply more force in melee." },
    balance      = { icon = "balance",      name = "Balance",
        desc = "Footing on uneven terrain. Reduces falls, slips, and stagger from impacts." },
}

-- engine.loadTexture caches by path, but we keep a per-key map so each
-- key resolves to a (texture, def) pair once and we don't re-hit the
-- engine each rebuild. Missing icons silently fall back to text.
local iconCache = {}

local function loadIconFor(iconKey)
    if not iconKey then return nil end
    if iconCache[iconKey] ~= nil then
        return iconCache[iconKey] or nil
    end
    local tex = engine.loadTexture(
        "assets/textures/icons/" .. iconKey .. ".png")
    iconCache[iconKey] = tex or false
    return tex
end

-- Place a single content row: stat icon on the left, bright value on
-- the right. The icon owns the description tooltip (from STAT_DEFS or
-- `tooltipOverride`). The value gets its OWN tooltip via
-- `valueTooltip` (a {text, hint} table) — used to surface how the
-- effective stat was computed (e.g. base + modifier sources). Returns
-- the value-label id and its baseline-y so per-panel refresh closures
-- can re-rightalign the value on update.
local function placeIconStatRow(rect, rowIndex, statKey, valueText,
                                  tooltipOverride, valueTooltip)
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
        -- dim text label so the row isn't visually empty.
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
        table.insert(unitInfoV2.statsContentElements,
            { kind = "label", id = nameLbl })
    end

    local valLbl = label.new({
        name        = "unit_info_v2_stat_val_" .. rowIndex,
        text        = valueText,
        font        = hud.menuFont,
        fontSize    = CONTENT_FONT_SIZE,
        color       = CONTENT_VAL_COLOR,
        page        = unitInfoV2.page,
        uiscale     = 1.0,
        tooltipRich = valueTooltip,
    })
    local valH = label.getElementHandle(valLbl)
    local valW = select(1, label.getSize(valLbl))
    UI.addToPage(unitInfoV2.page, valH,
        rect.x + rect.w - CONTENT_RIGHT_PAD - valW, y)
    UI.setZIndex(valH, 12)
    table.insert(unitInfoV2.statsContentElements, { kind = "label", id = valLbl })

    return valLbl, y
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

-- Format a stat as "base (+bonus)" when modifiers are active for that
-- stat, else just "base". Bonus = sum of modifier deltas. Reads
-- live each tick so accessory equip/unequip updates the display
-- without a panel rebuild.
local function fmtStatLive(uid, statName)
    local base = unit.getStatBase(uid, statName)
    if base == nil then return nil end
    local mods = unit.getModifiers(uid, statName) or {}
    local bonus = 0
    for _, m in ipairs(mods) do bonus = bonus + (m.delta or 0) end
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
local function statValueTooltip(uid, statKey)
    local base = unit.getStatBase(uid, statKey)
    if base == nil then return nil end
    local mods = unit.getModifiers(uid, statKey) or {}
    if #mods == 0 then
        -- Nothing to explain; suppress the tooltip entirely so a
        -- bonus-less stat doesn't show an empty popup.
        return nil
    end
    local bonus = 0
    for _, m in ipairs(mods) do bonus = bonus + (m.delta or 0) end
    local effective = base + bonus
    local lines = {}
    for _, m in ipairs(mods) do
        lines[#lines + 1] = string.format("%s %+.2f",
            m.source or "?", m.delta or 0)
    end
    return {
        text = string.format("%.2f", effective),
        hint = table.concat(lines, "\n"),
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
    local refs = {}
    for i, r in ipairs(visibleRows) do
        local tt = r.tooltip
        if type(tt) == "function" then tt = tt(uid) end
        local vtt = nil
        if r.valueTooltip then vtt = r.valueTooltip(uid) end
        local valLbl, y = placeIconStatRow(rect, i - 1, r.key,
            r.value(uid) or "?", tt, vtt)
        refs[i] = {
            valLbl     = valLbl,
            fn         = r.value,
            y          = y,
            tooltipFn  = r.valueTooltip,
        }
    end
    return function (newUid)
        if not newUid then return end
        for _, ref in ipairs(refs) do
            label.setText(ref.valLbl, ref.fn(newUid) or "?")
            local valH = label.getElementHandle(ref.valLbl)
            local valW = select(1, label.getSize(ref.valLbl))
            UI.setPosition(valH,
                rect.x + rect.w - CONTENT_RIGHT_PAD - valW, ref.y)
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

local function buildStatusPanel(rect, uid)
    return buildIconStatPanel(rect, uid, {
        { key = "stamina",   value = function(u) return fmtCurMax(u, "stamina",   "max_stamina")   end },
        { key = "hunger",    value = function(u) return fmtCurMax(u, "hunger",    "max_hunger")    end },
        { key = "hydration", value = function(u) return fmtCurMax(u, "hydration", "max_hydration") end },
    })
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

local function buildStatsPanel(rect, uid)
    return buildIconStatPanel(rect, uid, {
        statRow("strength"),
        statRow("endurance"),
        statRow("reflexes"),
        statRow("constitution"),
        statRow("metabolism"),
    })
end

local function buildMentalPanel(rect, uid)
    return buildIconStatPanel(rect, uid, {
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

local function buildAttributesPanel(rect, uid)
    return buildIconStatPanel(rect, uid, {
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

local function buildEffectsPanel(rect, uid)
    -- Placeholder. unit.getModifiers requires (uid, statName) — there's
    -- no API yet for "all active effects on a unit", so this always
    -- falls through to the empty message. When that API lands, the
    -- iteration below already handles the {name=..., delta=...} shape.
    local mods = unit.getModifiers(uid) or {}
    if #mods == 0 then
        local lblId = label.new({
            name     = "unit_info_v2_effects_empty",
            text     = "(no active effects)",
            font     = hud.menuFont,
            fontSize = CONTENT_FONT_SIZE,
            color    = {0.6, 0.6, 0.6, 1.0},
            page     = unitInfoV2.page,
            uiscale  = 1.0,
        })
        local h = label.getElementHandle(lblId)
        UI.addToPage(unitInfoV2.page, h,
            rect.x + CONTENT_LEFT_PAD,
            rect.y + CONTENT_TOP_PAD + CONTENT_FONT_SIZE)
        UI.setZIndex(h, 12)
        table.insert(unitInfoV2.statsContentElements, { kind = "label", id = lblId })
        return function () end
    end
    for i, m in ipairs(mods) do
        local y = rect.y + CONTENT_TOP_PAD + (i - 1) * CONTENT_ROW_H + CONTENT_FONT_SIZE
        local lblId = label.new({
            name     = "unit_info_v2_effect_" .. i,
            text     = tostring(m.name or "?"),
            font     = hud.menuFont,
            fontSize = CONTENT_FONT_SIZE,
            color    = CONTENT_VAL_COLOR,
            page     = unitInfoV2.page,
            uiscale  = 1.0,
        })
        local h = label.getElementHandle(lblId)
        UI.addToPage(unitInfoV2.page, h, rect.x + CONTENT_LEFT_PAD, y)
        UI.setZIndex(h, 12)
        table.insert(unitInfoV2.statsContentElements, { kind = "label", id = lblId })
    end
    return function () end
end

local PANEL_BUILDERS = {
    Status     = buildStatusPanel,
    Stats      = buildStatsPanel,
    Mental     = buildMentalPanel,
    Skill      = buildSkillPanel,
    Attributes = buildAttributesPanel,
    Effects    = buildEffectsPanel,
}

-- Forward-declared above. Clears the current panel's elements and
-- builds the new one for (activeUid, activeSubTab). Cheap when the
-- active selection hasn't changed because we cache (uid, subtab) and
-- only refresh values via the panel's refresh callback.
rebuildStatsContent = function ()
    if not unitInfoV2.statsContentRect then return end
    local uid = unitInfoV2.activeUid
    if not uid then return end

    -- Skill / Effects panels are content-shaped (their row count
    -- depends on the unit's state), so always rebuild for those.
    local panel = unitInfoV2.activeSubTab
    local dynamicShape = (panel == "Skill" or panel == "Effects")
    local sameContext = (not dynamicShape)
                      and unitInfoV2.lastContentUid == uid
                      and unitInfoV2.lastContentTab == panel
    if sameContext then return end

    -- Clear current
    for _, e in ipairs(unitInfoV2.statsContentElements) do
        if e.kind == "label" then label.destroy(e.id)
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
        hintLines[#hintLines + 1] = string.format(
            "length %.0fcm  ·  sharpness %d",
            it.weapon.bladeLength or 0,
            math.floor((it.weapon.baseSharpness or 0) + 0.5))
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
            pairsT[#pairsT + 1] = slotId .. "=" .. (item.defName or "?")
        end
        table.sort(pairsT)
        parts[#parts + 1] = table.concat(pairsT, ";")
    end
    if accessories then
        local accPart = {}
        for i, it in ipairs(accessories) do
            accPart[#accPart + 1] = i .. ":" .. (it.defName or "?")
                                    .. "@" .. tostring(it.condition or 0)
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
    -- folds in uid + class + every (slot, equipped-item) pair + each
    -- accessory's def/condition, so any equip/unequip — slot or
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
-- condition all match exactly — a 100% motor and a 99% motor stay
-- on two rows so the player sees the real spread of conditions.
local function stackKey(it)
    if it.equipped then return nil end
    return table.concat({
        it.defName,
        tostring(it.quality   or "_"),
        tostring(it.condition or "_"),
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
-- depleting canteen redraws when its label needs to change.
local function computeInvKey(uid, activeTab, items)
    local parts = { tostring(uid or ""), activeTab or "" }
    for _, it in ipairs(items) do
        parts[#parts + 1] = it.defName .. "/" .. tostring(it.currentFill)
            .. "/" .. (it.equipped and "e" or "i")
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
                equipment.equipAccessory(uid, item.defName)
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
                    equipment.equip(uid, slotId, item.defName)
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
                        equipment.equip(uid, s.id, item.defName)
                        unitInfoV2.lastInvKey   = nil
                        unitInfoV2.lastEquipKey = nil
                    end,
                }
            end
            items[1] = { label = "Equip", submenu = sub }
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
            local displayNm  = it.displayName or it.defName
            equipSubmenu[#equipSubmenu + 1] = {
                label    = displayNm,
                icon     = it.iconTex,
                callback = function()
                    equipment.equip(uid, rec.slotId, defName)
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
-- Name + Role are placeholders (real values aren't implemented yet);
-- Type and Action are live — Type shows the unit's def name, Action
-- shows the current AI action mapped through ACTION_DISPLAY below.
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
}

local function placeHeader(x, y, w, h)
    -- Row 4 starts as a dash; the tick refresh fills it in from the
    -- selected unit's currentAction.
    local rows = { "Name", "acolyte", "Role", "—" }
    local rowH = math.floor(h / #rows)
    local fontSize = 16
    for i, text in ipairs(rows) do
        local isLive = (i == 2 or i == 4)   -- type + action are real content
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
        if i == 2 then
            unitInfoV2.headerTypeLabelId = lblId
        elseif i == 4 then
            unitInfoV2.headerActionLabelId = lblId
        end
    end
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
        "assets/textures/hud/utility/white.png")
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

    -- Take over the unit-info display: hide the shared HUD info panel
    -- entirely while v2 is alive. Tile / building info also stop
    -- showing until those flows get migrated into v2 too.
    if hud.info_page then
        UI.hidePage(hud.info_page)
    end
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

    -- Refresh every visible tab's texture from its unit's current
    -- animation frame. Skip setSpriteTexture when the handle hasn't
    -- changed to avoid needless mutations. We only animate VISIBLE
    -- tabs — scrolled-off tabs would just thrash invisibly.
    if unitInfoV2.tabLayout and #unitInfoV2.tabs > 0 then
        local visible = unitInfoV2.tabLayout.visibleCount
        local first   = unitInfoV2.scrollOffset + 1
        local last    = math.min(first + visible - 1, #unitInfoV2.tabs)
        for i = first, last do
            local tab = unitInfoV2.tabs[i]
            if tab then
                local tex = unit.getFrameTexture(tab.uid)
                if tex and tex > 0 and tex ~= tab.lastTex then
                    UI.setSpriteTexture(tab.spriteId, tex)
                    tab.lastTex = tex
                end
            end
        end
    end

    if count > 0 and unitInfoV2.lastSelCount == 0 then
        UI.showPage(unitInfoV2.page)
    elseif count == 0 and unitInfoV2.lastSelCount > 0 then
        UI.hidePage(unitInfoV2.page)
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
        if unitInfoV2.lastSelCount > 0 then
            UI.showPage(unitInfoV2.page)
        else
            UI.hidePage(unitInfoV2.page)
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

    -- Restore the shared HUD info panel's visibility — we hid it on
    -- bootstrap so v2 could own the unit-info display.
    if hud and hud.info_page then
        UI.showPage(hud.info_page)
    end

    clearOwned()
    if unitInfoV2.page then
        UI.deletePage(unitInfoV2.page)
        unitInfoV2.page = nil
    end
    unitInfoV2.bootstrapped = false
    unitInfoV2.lastSelCount = 0
end

return unitInfoV2
