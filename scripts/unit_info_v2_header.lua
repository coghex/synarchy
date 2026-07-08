-- Unit info v2 header rows (#542 split from unit_info_v2.lua).
--
-- Stacks Name / Type / Role / Action rows in a virtual rect, plus the
-- "Log" button. No box around it; section boundaries are marked by
-- horizontal rules. All four rows are live — Name shows the unit's
-- personal name (#264, or species label for the unnamed), Type shows
-- the unit's def name, Role shows the derived role (#265,
-- unitAi.getRole mapped through unit_roles.display; "—" for
-- non-workers), Action shows the current AI action mapped through
-- ACTION_DISPLAY below.

local unitInfoV2 = package.loaded["scripts.unit_info_v2"]
local hud   = require("scripts.hud")
local label = require("scripts.ui.label")
local scale = require("scripts.ui.scale")
local L     = require("scripts.unit_info_v2_layout")

local M = {}

-- Header
unitInfoV2.headerNameLabelId   = nil  -- the name row; refreshed per active unit (#264)
unitInfoV2.headerTypeLabelId   = nil  -- the "acolyte" row; refreshed per active unit
unitInfoV2.headerRoleLabelId   = nil  -- the role row; refreshed per active unit from unit_ai (#265)
unitInfoV2.headerActionLabelId = nil  -- the action row; refreshed per active unit from unit_ai

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
    construct_job      = "Constructing",
    craft_job          = "Crafting",
    dig_designation    = "Digging",
    chop_designation   = "Chopping",
    forage             = "Foraging",
    pickup_ground      = "Picking up",
    repair_job         = "Repairing",
    treat_ally         = "Treating ally",
    engage             = "Engaging",
    retreat            = "Retreating",
    attack_target      = "Fighting",
}

function M.place(x, y, w, h)
    -- Rows 3 and 4 start as dashes; the tick refresh fills them in
    -- from the selected unit's derived role and currentAction.
    local rows = { "Name", "acolyte", "—", "—" }
    local rowH = math.floor(h / #rows)
    local fontSize = 16
    for i, text in ipairs(rows) do
        local lblId = label.new({
            name     = "unit_info_v2_header_row" .. i,
            text     = text,
            font     = hud.menuFont,
            fontSize = fontSize,
            color    = {1.0, 1.0, 1.0, 1.0},
            page     = unitInfoV2.page,
            uiscale  = 1.0,
        })
        table.insert(unitInfoV2.ownedLabels, lblId)
        local lblHandle = label.getElementHandle(lblId)
        local rowY = y + (i - 1) * rowH + math.floor(rowH * 0.5)
                     + math.floor(fontSize * 0.3)
        UI.addToPage(unitInfoV2.page, lblHandle, x + L.SECTION_PAD, rowY)
        UI.setZIndex(lblHandle, 12)
        if i == 1 then
            unitInfoV2.headerNameLabelId = lblId
        elseif i == 2 then
            unitInfoV2.headerTypeLabelId = lblId
        elseif i == 3 then
            unitInfoV2.headerRoleLabelId = lblId
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
    local btnX = x + w - btnW - L.SECTION_PAD
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

-- Refresh the four header rows for the active unit. Called every tick
-- from unitInfoV2.update().
function M.refresh()
    local uid = unitInfoV2.activeUid
    if not uid then return end

    -- Name row (#264): the unit's personal name when it has one, else
    -- its species label (display_name / prettified def name).
    if unitInfoV2.headerNameLabelId then
        local info = unit.getInfo(uid)
        local nameText
        if info and info.name and info.name ~= "" then
            nameText = info.name
        else
            nameText = (info and info.displayName) or "—"
        end
        label.setText(unitInfoV2.headerNameLabelId, nameText)
    end

    -- Type row: rewrite each tick from the active unit's def. All
    -- units are "acolyte" right now so this is effectively a no-op
    -- until other unit types exist, but the wiring is in place.
    if unitInfoV2.headerTypeLabelId then
        local info = unit.getInfo(uid)
        local typeName = info and info.defName or "?"
        label.setText(unitInfoV2.headerTypeLabelId, typeName)
    end

    -- Role + action rows: pull the derived role (#265) and
    -- currentAction from unit_ai. Actions map through ACTION_DISPLAY;
    -- unmapped names show raw so unknown actions are visible rather
    -- than blank. Non-workers (wildlife, technomule) have no role and
    -- show "—".
    if unitInfoV2.headerRoleLabelId or unitInfoV2.headerActionLabelId then
        local unitAi = require("scripts.unit_ai")
        local aiSt = unitAi.getState and unitAi.getState(uid)
        if unitInfoV2.headerRoleLabelId then
            local unitRoles = require("scripts.unit_roles")
            local role = aiSt and aiSt.role
            label.setText(unitInfoV2.headerRoleLabelId,
                          unitRoles.display(role) or "—")
        end
        if unitInfoV2.headerActionLabelId then
            local action = aiSt and aiSt.currentAction
            local text = (action and (ACTION_DISPLAY[action] or action)) or "—"
            label.setText(unitInfoV2.headerActionLabelId, text)
        end
    end
end

return M
