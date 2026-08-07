-- Unit info v2 inventory section (#542 split from unit_info_v2.lua).
--
-- Dynamic tab strip (All + one per category) on top, item rows in the
-- middle, total-weight footer at the bottom. Equipped items are
-- merged into the All view (and into their category) with a soft
-- amber tint behind the row so the player sees everything the unit
-- owns regardless of slot state.
--
-- Since #1088 the strip/rows/footer are rendered by the SHARED
-- item-list widget (scripts/ui/item_list.lua), which the cargo and
-- item-contents popups use too. What stays here is this section's own
-- policy: which items it collects (unit_info_v2_inventory_data.lua),
-- how a row reads (quality + repair suffixes, equipped tint, rich
-- tooltip), the total-weight footer text, and the local vertical fit
-- parameters that keep at least one row reachable at an extreme
-- responsive combination.

local unitInfoV2 = package.loaded["scripts.unit_info_v2"]
local hud   = require("scripts.hud")
local scale = require("scripts.ui.scale")
local qualityTier  = require("scripts.ui.quality_tier")
local repairStatus = require("scripts.ui.repair_status")
local itemList = require("scripts.ui.item_list")
local items = require("scripts.unit_info_v2_items")
local data  = require("scripts.unit_info_v2_inventory_data")
local L     = require("scripts.unit_info_v2_layout")

local M = {}

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

unitInfoV2.invRect      = nil
unitInfoV2.activeInvTab = "All"
unitInfoV2.invListId    = nil   -- shared item-list widget instance

-- Rendered rows ({hitId, item, index}) for unit_info_v2_context_menu's
-- right-click lookup. A live view of the widget's own rows, so the two
-- can't drift.
function M.rows()
    return itemList.getRows(unitInfoV2.invListId)
end

-- The full name a row shows: quality tier, then repair status.
local function rowDisplayName(it)
    return repairStatus.withSuffix(
        qualityTier.withSuffix(it.displayName, it), it)
end

-- The item-list parameters describing this section's data and
-- presentation policy. `rawItems` also drives the footer, so it is
-- collected once and passed in.
local function listDataParams(rect, rawItems)
    local total = 0
    for _, it in ipairs(rawItems) do total = total + (it.weight or 0) end
    return {
        items            = rawItems,
        separateEquipped = true,
        activeTab        = unitInfoV2.activeInvTab,
        uiscale          = scale.get(),
        -- Repair priority/claim state is produced by a callback
        -- (rowName / rowTooltip), so it cannot be seen in the row
        -- fields themselves — it reaches the rebuild comparison here.
        presentationKey  = (function()
            local parts = { tostring(unitInfoV2.activeUid or "") }
            for _, it in ipairs(rawItems) do
                parts[#parts + 1] = repairStatus.cacheKey(it)
            end
            return table.concat(parts, "|")
        end)(),
        tabs = {
            mode       = "wrapped",
            centerRows = true,
            wrapWidth  = rect.w - 2 * L.SECTION_PAD,
            tabHeight  = L.INV_TAB_H,
            tileSize   = L.SUB_TAB_TILE,
            fontSize   = L.INV_TAB_FONT_SIZE,
            textPad    = L.INV_TAB_TEXT_PAD,
            gap        = L.INV_TAB_GAP,
            rowGap     = L.SUB_TAB_ROW_GAP,
            textColor         = L.SUB_TAB_TEXT_COLOR,
            selectedTextColor = L.SUB_TAB_SEL_TEXT_COLOR,
        },
        -- Tab boxes/pads scale with the local vertical fit, but the tab
        -- LABEL stays a fixed size regardless — so the fit only ever
        -- shrinks the boxes toward the text, never past it.
        tabLabelUiscale = 1.0,
        tabTopPad       = L.INV_TAB_TOP_PAD,
        tabBottomPad    = L.INV_TAB_BOTTOM_PAD,
        fitVertical     = true,
        rowHeight       = L.INV_ROW_H,
        rowPad          = L.INV_ROW_PAD,
        iconSize        = L.INV_ICON_SIZE,
        textPad         = L.INV_TEXT_PAD,
        contentInset    = L.SECTION_PAD,
        nameRightGap    = 4,
        rowFontSize     = 14,
        rowLabelUiscale = 1.0,
        footerHeight    = L.INV_FOOTER_H,
        -- Total weight across the FULL raw item set (not just the
        -- active tab, and not the stacked groups): a row reading
        -- "Steel Plate ×5 = 6.00 kg" still contributes 6 kg.
        footer          = { text = string.format("Total: %.2f kg", total) },
        zBase           = 11,
        rowName = function(it)
            local n = rowDisplayName(it)
            if (it.count or 1) > 1 then
                n = string.format("%s ×%d", n, it.count)
            end
            return n
        end,
        rowWeightText = function(it)
            return string.format("%.2f kg", (it.weight or 0) * (it.count or 1))
        end,
        rowColor = function(it)
            if it.equipped then return { 1.0, 0.95, 0.7, 1.0 } end
            return { 1.0, 1.0, 1.0, 1.0 }
        end,
        -- Soft amber rect behind an equipped row so it pops without an
        -- extra icon.
        rowBackground = function(it)
            if it.equipped then return L.INV_EQUIP_TINT end
            return nil
        end,
        rowTooltip = function(it)
            return {
                text = rowDisplayName(it),
                hint = items.buildItemHint(it,
                    it.equipped and it.equippedSlot or nil),
            }
        end,
        onTabChange     = M.onTabChange,
        onRowRightClick = unitInfoV2.handleInvItemRightClick,
    }
end

function M.rebuildInventorySection()
    if not unitInfoV2.invRect then return end
    local rect = unitInfoV2.invRect
    local uid  = unitInfoV2.activeUid

    local rawItems = uid and data.collectInventoryAndEquipment(uid) or {}
    local params = listDataParams(rect, rawItems)
    params.x, params.y = rect.x, rect.y
    params.width, params.height = rect.w, rect.h
    if not itemList.isStale(unitInfoV2.invListId, params) then return end

    M.clearAll()
    if not uid then return end

    params.name = "unit_info_v2_inv"
    params.page = unitInfoV2.page
    params.font = hud.menuFont
    unitInfoV2.invListId = itemList.new(params)
    -- The widget snaps a no-longer-present selection back to All;
    -- mirror it so the section's own durable choice can't go stale.
    unitInfoV2.activeInvTab = itemList.getActiveTab(unitInfoV2.invListId)
end

-- Force the next rebuild regardless of whether anything the widget can
-- see changed — used by the row context menu so a player's own click
-- redraws the SAME frame.
function M.invalidate()
    itemList.invalidate(unitInfoV2.invListId)
end

-- Full teardown for rebuildLayout / shutdown.
function M.clearAll()
    if unitInfoV2.invListId then
        itemList.destroy(unitInfoV2.invListId)
        unitInfoV2.invListId = nil
    end
end

-- Tab selection. The strip is a real scripts/ui/tabbar, so the click
-- arrives through uiManager.onTabClick; this only records the choice
-- and lets the next tick rebuild around it.
function M.onTabChange(category)
    if unitInfoV2.activeInvTab == category then return end
    unitInfoV2.activeInvTab = category
    M.invalidate()
end

return M
