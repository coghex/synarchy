-- Unit info v2 equipment section (#542 split from unit_info_v2.lua).
--
-- Silhouette on the left with clickable slot overlays, accessory list
-- on the right. The silhouette and slot positions both come from the
-- active unit's equipment class (looked up via equipment.getClass),
-- so changing the YAML re-lays out the section with no Lua changes.

local unitInfoV2 = package.loaded["scripts.unit_info_v2"]
local hud         = require("scripts.hud")
local label       = require("scripts.ui.label")
local scale       = require("scripts.ui.scale")
local responsive  = require("scripts.ui.responsive")
local qualityTier = require("scripts.ui.quality_tier")
local repairStatus = require("scripts.ui.repair_status")
local items       = require("scripts.unit_info_v2_items")
local L           = require("scripts.unit_info_v2_layout")

local M = {}

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Layout constants (base size; multiplied by uiscale)
-----------------------------------------------------------

-- Silhouette + slot grid on the left, accessory list on the right.
-- SILHOUETTE_PAD is the gap between the pane border and the
-- silhouette; ACCESSORY_GAP separates the silhouette from the
-- right-side accessory list.
local SILHOUETTE_PAD = 8
local ACCESSORY_GAP  = 12

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
            -- Repair priority/claim state (#303 review) so a player
            -- toggle or the AI claiming/finishing the job invalidates
            -- this key the same way a condition/sharpness change does.
            pairsT[#pairsT + 1] = slotId .. "=" .. (item.defName or "?")
                                  .. "@" .. tostring(item.condition or 0)
                                  .. "/" .. tostring(item.sharpness or 0)
                                  .. "/" .. repairStatus.cacheKey(item)
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
                                    .. "/" .. repairStatus.cacheKey(it)
        end
        parts[#parts + 1] = table.concat(accPart, ";")
    end
    return table.concat(parts, "|")
end

function M.rebuildEquipmentSection()
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

    -- #750 round-17 review: L.fitVerticalSections (round-16) only
    -- shrinks this section's outer RECT, never the CONTENT scale — at
    -- a narrow, high-uiscale combination (e.g. 800x2160@4x) the
    -- silhouette still renders at the full uiscale and overflows the
    -- fitted rect into the sections stacked above/below it. Fit a
    -- local content scale to the actual rect on both axes (width
    -- matters too — a squeezed rect can also run the silhouette past
    -- the accessory list) before deriving any slot/silhouette
    -- geometry below, mirroring the fitScale technique used
    -- throughout this PR for the same class of gap.
    local fitted = math.min(
        responsive.fitScale(silH + 2 * silPad, rect.h, uiscale),
        responsive.fitScale(silW + 2 * silPad, rect.w, uiscale))
    if fitted < uiscale then
        uiscale = fitted
        silPad  = math.floor(SILHOUETTE_PAD * uiscale)
        silW    = math.floor(cls.silhouetteW * uiscale)
        silH    = math.floor(cls.silhouetteH * uiscale)
    end

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
                    text = repairStatus.withSuffix(qualityTier.withSuffix(
                        eq.displayName or eq.defName or s.name, eq), eq),
                    hint = items.buildItemHint(eq, s.id),
                })
                UI.setClickable(iconElemId, true)
                UI.setOnRightClick(iconElemId, "onEquipSlotRightClick")
                table.insert(unitInfoV2.equipElements,
                    { kind = "sprite", id = iconElemId })
                items.addBrokenOverlay(unitInfoV2, eq, "unit_info_v2_equip_broken_" .. i,
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
                    text = repairStatus.withSuffix(qualityTier.withSuffix(
                        it.displayName or it.defName, it), it),
                    hint = items.buildItemHint(it, "(worn)"),
                })
                table.insert(unitInfoV2.equipElements,
                    { kind = "sprite", id = iconId })
                items.addBrokenOverlay(unitInfoV2, it, "unit_info_v2_acc_broken_" .. i,
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

-- Full teardown for rebuildLayout / shutdown.
function M.clearAll()
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
end

return M
