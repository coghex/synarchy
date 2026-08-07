-- Settings Tab for Create World Menu
-- Creates widget rows for Name, Seed, and World Size.
-- Returns element handles for show/hide tab switching.
--
-- Row order:
--   1. Name       (randbox)
--   2. its gloss  (label — #1106, the meaning of a suggested name)
--   3. Seed       (randbox)
--   4. World Size (dropdown)
local label       = require("scripts.ui.label")
local randbox     = require("scripts.ui.randbox")
local dropdown    = require("scripts.ui.dropdown")
local responsive  = require("scripts.ui.responsive")
local nameSuggest = require("scripts.create_world.name_suggest")

local settingsTab = {}

-- Widget IDs stored here so create_world_menu can read values
settingsTab.nameRandBoxId  = nil
settingsTab.seedRandBoxId  = nil
settingsTab.sizeDropdownId = nil
settingsTab.glossLabelId   = nil

-- #1106: how a hex seed field becomes the number the language is
-- derived from. Identical to generation.lua's own `tonumber(p.seed, 16)
-- or 0`, on purpose — the language a name is suggested in must be the
-- language the generated world records, so both sides must normalize
-- the seed text the same way. Two spellings of one seed ("a3f7" /
-- "A3F7") are one number and therefore one language.
function settingsTab.seedNumber(seedText)
    return tonumber(seedText or "", 16) or 0
end

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
    local page      = params.page
    local font      = params.font
    local base      = params.baseSizes
    local uiscale   = params.uiscale
    local s         = params.s
    local cx        = params.contentX
    local cy        = params.contentY
    local cw        = params.contentW
    local zContent  = params.zContent
    local zWidgets  = params.zWidgets
    local pending   = params.pending
    local container = params.container

    local elements = {}
    local rowIndex = 0

    local function rowY(n)
        return cy + s.rowSpacing * n
    end

    -- #748 round 7: create_world_menu's computeContentScaleFactor
    -- reserves a LABEL_COLUMN_FRACTION-wide column for this tab's row
    -- labels (the shrunk control's own right-aligned left edge never
    -- passes cx+cw*0.35) — but that reservation is useless if the
    -- label itself still renders at the tab's full uiscale, which can
    -- still be far wider than its own reserved column at an extreme
    -- narrow width. Compute ONE effective, LOCAL uiscale for every row
    -- label in this tab from whichever label text is widest, fit
    -- against the SAME reserved column width.
    local LABEL_COLUMN_FRACTION = 0.35
    local labelFontSizePx = math.floor(base.fontSize * uiscale)
    local naturalLabelWidth = 0
    for _, t in ipairs({ "Name", "Seed", "Size" }) do
        local w = engine.getTextWidth(font, t, labelFontSizePx)
        if w > naturalLabelWidth then naturalLabelWidth = w end
    end
    local labelUiscale = responsive.fitScale(
        naturalLabelWidth, cw * LABEL_COLUMN_FRACTION, uiscale)

    -- #748 round 8: World Size (below) is a dropdown — its width is
    -- driven by OPTION TEXT metrics (dropdown.measureOptions) plus a
    -- fixed minWidth floor, neither of which is touched by
    -- computeContentScaleFactor's randbox/textbox-only shrink. Mirrors
    -- graphics_tab.lua's identical dropdownUiscale fix: one effective,
    -- LOCAL uiscale, mirroring dropdown.lua's own
    -- displayWidth+arrowSize formula, fit against the SAME reserved
    -- control column (cw*(1-LABEL_COLUMN_FRACTION)) the shrunk
    -- randbox rows already target.
    local sizeFontSizePx = math.floor(24 * uiscale)
    local sizeDropHeight = math.floor(base.dropdownHeight * uiscale)
    local naturalSizeDropdownWidth = math.max(
        dropdown.measureOptions(settingsTab.worldSizeOptions, font, sizeFontSizePx),
        math.floor(100 * uiscale)) + sizeDropHeight
    local sizeDropdownUiscale = responsive.fitScale(
        naturalSizeDropdownWidth, cw * (1 - LABEL_COLUMN_FRACTION), uiscale)

    -- #1106: the world seed must be FINAL before the Name row exists,
    -- because the name is suggested in that seed's own language. The
    -- Seed row is built second (the visual order is Name, Seed, Size),
    -- so the value it will display is settled here instead of inside
    -- its own randbox.new. A seed carried over from a previous visit or
    -- a rebuild is kept as-is; only a genuinely empty one is rolled.
    if not pending.seed or pending.seed == "" then
        pending.seed = randbox.newHexSeed()
    end

    -- The screen's FIRST world name, offered before the player has
    -- touched anything. Guarded on the sequence never having started
    -- (nameOrdinal is nil only on a fresh menu and after Defaults), NOT
    -- on the field merely being empty: a rebuild of a name the player
    -- deliberately cleared must leave it cleared, and the randbox's own
    -- fill-if-empty behaviour is therefore switched off below.
    if pending.nameOrdinal == nil
       and (pending.worldName == nil or pending.worldName == "") then
        nameSuggest.suggest(pending, settingsTab.seedNumber(pending.seed))
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
        uiscale  = labelUiscale,
    }))
    local nameLabelHandle = label.getElementHandle(nameLabelId)
    UI.addChild(container, nameLabelHandle,
                cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(nameLabelHandle, zContent)
    table.insert(elements, { type = "label", handle = nameLabelHandle })

    local nameW = math.floor(base.nameBoxWidth * uiscale)
    local nameBtnSize = math.floor(base.randboxHeight * uiscale)
    local nameTotalW = nameW + nameBtnSize
    local nameRowY = rowY(rowIndex)
    local nameBoxX = cx + cw - nameTotalW

    -- #1106 requirement 6: a native name with no visible meaning is
    -- indistinguishable from the word-list output this replaced, so the
    -- English gloss is shown beside it — "Karadun" over "Ashen Land".
    --
    -- It gets its own row slot rather than sharing the Name row. The
    -- space between the row's label column and the right-aligned name
    -- box is not reserved for anything and shrinks to nothing at the
    -- narrow end of the supported envelope (computeContentScaleFactor
    -- lets the control eat everything past cw*LABEL_COLUMN_FRACTION),
    -- so a gloss placed there would collide with the "Name" label
    -- exactly when the window is smallest. A row of its own always fits
    -- and stays legible.
    --
    -- Built before the randbox so the generator callback below can
    -- already refresh it. A single fixed font size is used, since a
    -- text element's size is fixed at creation — the label is
    -- right-aligned and its left edge clamped into the row, and the
    -- tab's own clipping viewport handles the remainder in the
    -- pathological case.
    local glossFontSize = math.floor(base.fontSize * 0.7)
    local glossRowY = rowY(rowIndex + 1)

    settingsTab.glossLabelId = params.trackLabel(label.new({
        name     = "world_name_gloss",
        text     = nameSuggest.gloss(pending) or "",
        font     = font,
        fontSize = glossFontSize,
        color    = {0.75, 0.75, 0.75, 1.0},
        page     = page,
        uiscale  = uiscale,
    }))
    local glossHandle = label.getElementHandle(settingsTab.glossLabelId)
    UI.addChild(container, glossHandle, cx, glossRowY + s.fontSize)
    UI.setZIndex(glossHandle, zContent)
    table.insert(elements, { type = "label", handle = glossHandle })

    -- Re-measure and re-place the gloss for whatever the name currently
    -- is. Hidden outright when there is none to show — a manual name
    -- has no meaning, and an empty label would leave a stale one behind.
    local function refreshGloss()
        local id = settingsTab.glossLabelId
        if not id then return end
        local text = nameSuggest.gloss(pending)
        if not text then
            label.setText(id, "")
            label.setVisible(id, false)
            return
        end
        label.setText(id, text)
        local width = engine.getTextWidth(
            font, text, math.floor(glossFontSize * uiscale))
        label.setPosition(id,
            math.max(cx, cx + cw - width), glossRowY + s.fontSize)
        label.setVisible(id, true)
    end
    settingsTab.refreshGloss = refreshGloss
    refreshGloss()

    settingsTab.nameRandBoxId = params.trackRandBox(randbox.new({
        name     = "world_name",
        width    = base.nameBoxWidth,
        height   = base.randboxHeight,
        page     = page,
        parent   = container,
        font     = font,
        fontSize = 24,
        uiscale  = uiscale,
        zIndex   = zWidgets,
        randType = randbox.Type.NAME,
        default  = pending.worldName ~= ""
                       and pending.worldName or nil,
        autoGenerate = false,
        -- #1106: the dice button. Advances this seed's suggestion
        -- sequence, so successive presses keep one language's sound
        -- while the meaning changes. Returning nil on failure leaves
        -- the field exactly as it was — there is no fallback generator.
        generate = function()
            local value = nameSuggest.suggest(
                pending, settingsTab.seedNumber(pending.seed))
            refreshGloss()
            return value
        end,
        onChange  = function(value, id, name)
            pending.worldName = value
        end,
        -- The moment the player takes over the field, the name stops
        -- being the language's: gloss and provenance go immediately
        -- (#708 principle 7), even if the typed text happens to match
        -- what was suggested.
        onUserEdit = function(value, id, name)
            pending.worldName = value
            nameSuggest.clear(pending)
            refreshGloss()
        end,
    }))

    randbox.setPosition(settingsTab.nameRandBoxId, nameBoxX, nameRowY)
    table.insert(elements, { type = "randbox", id = settingsTab.nameRandBoxId })

    -- Name row plus the gloss row beneath it.
    rowIndex = rowIndex + 2

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
        uiscale  = labelUiscale,
        tooltip  = "Random seed for world generation. The same seed always produces the same world. Use the dice button to randomise.",
    }))
    local seedLabelHandle = label.getElementHandle(seedLabelId)
    UI.addChild(container, seedLabelHandle,
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
        parent   = container,
        font     = font,
        fontSize = 24,
        uiscale  = uiscale,
        zIndex   = zWidgets,
        randType = randbox.Type.HEX_SEED,
        default  = pending.seed ~= ""
                       and pending.seed or nil,
        onChange  = function(value, id, name)
            pending.seed = value
            -- #1106 requirement 3: the language comes from the seed, so
            -- a new seed means a new language and a fresh suggestion —
            -- but only for a name that IS a suggestion. A name the
            -- player typed is theirs and is left alone.
            --
            -- nameSuggest.reseed decides "did the seed actually change"
            -- from the NUMERIC value, which is also what makes a resize
            -- rebuild inert here: restoreAll re-fires onChange with the
            -- same seed text, and re-rolling the name off that would
            -- destroy the very state the rebuild is preserving.
            if nameSuggest.reseed(pending, settingsTab.seedNumber(value)) then
                if settingsTab.nameRandBoxId then
                    randbox.setValue(settingsTab.nameRandBoxId,
                                     pending.worldName)
                end
                if settingsTab.refreshGloss then
                    settingsTab.refreshGloss()
                end
            end
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
        uiscale  = labelUiscale,
        tooltip  = "World size in chunks. Larger worlds take significantly longer to generate and stream.",
    }))
    local sizeLabelHandle = label.getElementHandle(sizeLabelId)
    UI.addChild(container, sizeLabelHandle,
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
        parent            = container,
        x = 0, y = 0,
        uiscale           = sizeDropdownUiscale,
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

    -- Name, its gloss, Seed, Size.
    return elements, 4
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
