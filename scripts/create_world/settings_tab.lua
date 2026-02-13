-- Settings Tab for Create World Menu
-- Creates widget rows for Name, Seed, and World Size.
-- Returns element handles for show/hide tab switching.
--
-- Row order:
--   1. Name       (randbox)
--   2. Seed       (randbox)
--   3. World Size (dropdown)
local label    = require("scripts.ui.label")
local randbox  = require("scripts.ui.randbox")
local dropdown = require("scripts.ui.dropdown")

local settingsTab = {}

-- Widget IDs stored here so create_world_menu can read values
settingsTab.nameRandBoxId  = nil
settingsTab.seedRandBoxId  = nil
settingsTab.sizeDropdownId = nil

-----------------------------------------------------------
-- World Size Options
-----------------------------------------------------------
settingsTab.worldSizeOptions = {
    { text = "Tiny (32)",       value = "32" },
    { text = "Small (64)",      value = "64" },
    { text = "Medium (128)",    value = "128" },
    { text = "Large (256)",     value = "256" },
    { text = "Huge (512)",      value = "512" },
    { text = "Massive (1024)",  value = "1024" },
}

-----------------------------------------------------------
-- Validators
-----------------------------------------------------------

function settingsTab.sizeValidator(char)
    return char:match("^%d$") ~= nil
end

function settingsTab.sizeMatcher(inputText, options)
    if not inputText or inputText == "" then return nil end
    for i, opt in ipairs(options) do
        if opt.value == inputText then
            return i
        end
    end
    local lower = inputText:lower()
    for i, opt in ipairs(options) do
        if opt.text:lower():sub(1, #lower) == lower then
            return i
        end
    end
    return nil
end

-----------------------------------------------------------
-- Create all rows
-----------------------------------------------------------

-- params = {
--   page, font, baseSizes, uiscale, s (scaled sizes),
--   contentX, contentY, contentW,
--   zContent, zWidgets,
--   pending,
--   trackLabel, trackRandBox, trackDropdown,
-- }
-- Returns: elements[] for show/hide
function settingsTab.create(params)
    local page     = params.page
    local font     = params.font
    local base     = params.baseSizes
    local uiscale  = params.uiscale
    local s        = params.s
    local cx       = params.contentX
    local cy       = params.contentY
    local cw       = params.contentW
    local zContent = params.zContent
    local zWidgets = params.zWidgets
    local pending  = params.pending

    local elements = {}
    local rowIndex = 0

    local function rowY(n)
        return cy + s.rowSpacing * n
    end

    ---------------------------------------------------------
    -- Row 1: World Name (randbox - wide)
    ---------------------------------------------------------
    local nameLabelId = params.trackLabel(label.new({
        name     = "world_name_label",
        text     = "Name",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    }))
    local nameLabelHandle = label.getElementHandle(nameLabelId)
    UI.addToPage(page, nameLabelHandle,
                 cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(nameLabelHandle, zContent)
    table.insert(elements, { type = "label", handle = nameLabelHandle })

    local nameW = math.floor(base.nameBoxWidth * uiscale)
    local nameBtnSize = math.floor(base.randboxHeight * uiscale)
    local nameTotalW = nameW + nameBtnSize

    settingsTab.nameRandBoxId = params.trackRandBox(randbox.new({
        name     = "world_name",
        width    = base.nameBoxWidth,
        height   = base.randboxHeight,
        page     = page,
        font     = font,
        fontSize = 24,
        uiscale  = uiscale,
        zIndex   = zWidgets,
        randType = randbox.Type.NAME,
        default  = pending.worldName ~= ""
                       and pending.worldName or nil,
        onChange  = function(value, id, name)
            pending.worldName = value
        end,
    }))

    randbox.setPosition(settingsTab.nameRandBoxId,
                        cx + cw - nameTotalW, rowY(rowIndex))
    table.insert(elements, { type = "randbox", id = settingsTab.nameRandBoxId })

    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 2: Seed (randbox)
    ---------------------------------------------------------
    local seedLabelId = params.trackLabel(label.new({
        name     = "world_seed_label",
        text     = "Seed",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    }))
    local seedLabelHandle = label.getElementHandle(seedLabelId)
    UI.addToPage(page, seedLabelHandle,
                 cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(seedLabelHandle, zContent)
    table.insert(elements, { type = "label", handle = seedLabelHandle })

    local rbW = math.floor(base.randboxWidth * uiscale)
    local rbBtnSize = math.floor(base.randboxHeight * uiscale)
    local rbTotalW = rbW + rbBtnSize

    settingsTab.seedRandBoxId = params.trackRandBox(randbox.new({
        name     = "world_seed",
        width    = base.randboxWidth,
        height   = base.randboxHeight,
        page     = page,
        font     = font,
        fontSize = 24,
        uiscale  = uiscale,
        zIndex   = zWidgets,
        randType = randbox.Type.HEX_SEED,
        default  = pending.seed ~= ""
                       and pending.seed or nil,
        onChange  = function(value, id, name)
            pending.seed = value
        end,
    }))

    randbox.setPosition(settingsTab.seedRandBoxId,
                        cx + cw - rbTotalW, rowY(rowIndex))
    table.insert(elements, { type = "randbox", id = settingsTab.seedRandBoxId })

    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Row 3: World Size (dropdown)
    ---------------------------------------------------------
    local sizeLabelId = params.trackLabel(label.new({
        name     = "world_size_label",
        text     = "Size",
        font     = font,
        fontSize = base.fontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = page,
        uiscale  = uiscale,
    }))
    local sizeLabelHandle = label.getElementHandle(sizeLabelId)
    UI.addToPage(page, sizeLabelHandle,
                 cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(sizeLabelHandle, zContent)
    table.insert(elements, { type = "label", handle = sizeLabelHandle })

    settingsTab.sizeDropdownId = params.trackDropdown(dropdown.new({
        name              = "world_size",
        options           = settingsTab.worldSizeOptions,
        default           = pending.worldSize,
        font              = font,
        fontSize          = 24,
        height            = base.dropdownHeight,
        page              = page,
        x = 0, y = 0,
        uiscale           = uiscale,
        zIndex            = zWidgets,
        validateChar      = settingsTab.sizeValidator,
        matchFn           = settingsTab.sizeMatcher,
        maxVisibleOptions = 6,
        onChange = function(value, text, id, name)
            pending.worldSize = value
            engine.logInfo("World size pending: " .. text)
        end,
    }))

    local ddSizeId = settingsTab.sizeDropdownId
    local ddSizeW, _ = dropdown.getSize(ddSizeId)
    dropdown.setPosition(ddSizeId, cx + cw - ddSizeW, rowY(rowIndex))
    table.insert(elements, { type = "dropdown", id = ddSizeId })

    return elements
end

-----------------------------------------------------------
-- Read current widget values (for generate)
-----------------------------------------------------------

function settingsTab.getWidgetValues()
    local vals = {}
    if settingsTab.sizeDropdownId then
        vals.worldSize = dropdown.getValue(settingsTab.sizeDropdownId)
    end
    return vals
end

return settingsTab
