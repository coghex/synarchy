-- Unit info v2 inventory section (#542 split from unit_info_v2.lua).
--
-- Dynamic tab strip (All + one per category) on top, item rows in the
-- middle, total-weight footer at the bottom. Equipped items are
-- merged into the All view (and into their category) with a soft
-- amber tint behind the row so the player sees everything the unit
-- owns regardless of slot state. Data shaping (stacking, tab/key
-- computation) lives in unit_info_v2_inventory_data.lua.

local unitInfoV2 = package.loaded["scripts.unit_info_v2"]
local hud   = require("scripts.hud")
local label = require("scripts.ui.label")
local scale = require("scripts.ui.scale")
local qualityTier  = require("scripts.ui.quality_tier")
local repairStatus = require("scripts.ui.repair_status")
local items = require("scripts.unit_info_v2_items")
local data  = require("scripts.unit_info_v2_inventory_data")
local L     = require("scripts.unit_info_v2_layout")
local utf8Safe = require("scripts.ui.utf8_safe")

local M = {}

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

unitInfoV2.invRect         = nil
unitInfoV2.invTabs         = {}    -- { {name, count, bgId, labelId}, … }
unitInfoV2.activeInvTab    = "All"
unitInfoV2.invListElements = {}
unitInfoV2.invRows         = {}    -- per-row right-click metadata
unitInfoV2.lastInvKey      = nil   -- hash of the rendered (uid, tab, items)

-- Truncate `text` to fit within `maxPx` of horizontal space at the
-- given font + size, appending ".." when truncated. Binary-searches
-- the prefix so this stays O(log n) per call even for long strings.
-- Returns the original text unchanged if it already fits. Every
-- candidate cut point is snapped to a complete UTF-8 character boundary
-- (utf8Safe) so a multi-byte character is never split into a dangling
-- lead byte -- string.sub cuts by byte offset, not codepoint.
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
        local cut = utf8Safe.snapToCharBoundary(text, mid)
        local sub = text:sub(1, cut)
        local w = engine.getTextWidth(font, sub, fontSize) + ellW
        if w <= maxPx then lo = mid else hi = mid - 1 end
    end
    return text:sub(1, utf8Safe.snapToCharBoundary(text, lo)) .. ellipsis
end

local function applyInvTabStyling()
    local selSet   = unitInfoV2.subTabSelectedTexSet
    local unselSet = unitInfoV2.subTabUnselectedTexSet
    for _, t in ipairs(unitInfoV2.invTabs) do
        local active = (t.name == unitInfoV2.activeInvTab)
        if selSet and unselSet then
            UI.setBoxTextures(t.bgId, active and selSet or unselSet)
        end
        local c = active and L.SUB_TAB_SEL_TEXT_COLOR or L.SUB_TAB_TEXT_COLOR
        local lblH = label.getElementHandle(t.labelId)
        UI.setColor(lblH, c[1], c[2], c[3], c[4])
    end
end

function M.rebuildInventorySection()
    if not unitInfoV2.invRect then return end
    local rect = unitInfoV2.invRect
    local uid  = unitInfoV2.activeUid

    local rawItems = uid and data.collectInventoryAndEquipment(uid) or {}
    -- For display purposes we collapse identical non-equipped entries
    -- into stacks. RAW list still drives the total-weight footer so a
    -- "Steel Plate ×5" row contributes 5×1.2 kg to the total.
    local invItems = data.groupForDisplay(rawItems)
    local key = data.computeInvKey(uid, unitInfoV2.activeInvTab, invItems)
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
    local tabH    = math.floor(L.INV_TAB_H * uiscale)
    local topPad  = math.floor(L.INV_TAB_TOP_PAD * uiscale)
    local botPad  = math.floor(L.INV_TAB_BOTTOM_PAD * uiscale)
    local rowH    = math.floor(L.INV_ROW_H * uiscale)
    local rowPad  = math.floor(L.INV_ROW_PAD * uiscale)
    local iconSz  = math.floor(L.INV_ICON_SIZE * uiscale)
    local footerH = math.floor(L.INV_FOOTER_H * uiscale)
    local textPad = math.floor(L.INV_TEXT_PAD * uiscale)
    local sectPad = math.floor(L.SECTION_PAD * uiscale)

    -- 1. Tab strip
    local tabDefs = data.computeInvTabs(invItems)
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
        local tw     = engine.getTextWidth(hud.menuFont, s, L.INV_TAB_FONT_SIZE)
        tabWidths[i] = math.floor(tw) + 2 * L.INV_TAB_TEXT_PAD
    end

    -- Wrap plan: rows have (startIdx, endIdx, totalW). Reuses the
    -- sub-tab wrapper since the inv tabs share the same gap=0
    -- flush-tab styling.
    local rowGap    = math.floor(L.SUB_TAB_ROW_GAP * uiscale)
    local tabPlan   = L.planSubTabRows(rect, tabWidths)
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
                L.SUB_TAB_TILE,
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
                fontSize = L.INV_TAB_FONT_SIZE,
                color    = L.SUB_TAB_TEXT_COLOR,
                page     = unitInfoV2.page,
                uiscale  = 1.0,
            })
            local lblH = label.getElementHandle(lblId)
            local lblW = select(1, label.getSize(lblId))
            UI.addToPage(unitInfoV2.page, lblH,
                cx + math.floor((tabW - lblW) / 2),
                cursorY + math.floor(tabH / 2)
                       + math.floor(L.INV_TAB_FONT_SIZE * 0.3) + 2)
            UI.setZIndex(lblH, 12)

            unitInfoV2.invTabs[#unitInfoV2.invTabs + 1] = {
                name = td.name, count = td.count,
                bgId = bgId, labelId = lblId,
            }
            cx = cx + tabW + L.INV_TAB_GAP
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
    for _, it in ipairs(invItems) do
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
                L.INV_EQUIP_TINT[1], L.INV_EQUIP_TINT[2],
                L.INV_EQUIP_TINT[3], L.INV_EQUIP_TINT[4],
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
            items.addBrokenOverlay(unitInfoV2, it, "unit_info_v2_inv_broken_" .. i,
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
        local rawName = repairStatus.withSuffix(
            qualityTier.withSuffix(it.displayName, it), it)
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
            text = repairStatus.withSuffix(
                qualityTier.withSuffix(it.displayName, it), it),
            hint = items.buildItemHint(it, it.equipped and it.equippedSlot or nil),
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

-- Full teardown for rebuildLayout / shutdown.
function M.clearAll()
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

return M
