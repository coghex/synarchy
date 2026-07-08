-- One F8 debug-overlay category (spawn/fluid/item/terrain/location/
-- structure). Factors out the open/close/build/destroy/arm pattern
-- shared by every category (#545) so scripts/debug.lua only has to
-- wire up mutual exclusion and layout, not repeat this per category.
--
-- Category-specific bits (list source, display text, which field on
-- the overlay mirrors the armed value, mutual-exclusion partners) come
-- from the spec table passed to Mode.new — see scripts/debug/modes.lua.
local scale = require("scripts.ui.scale")
local label = require("scripts.ui.label")

local COLOR_DIM    = {0.0, 1.0, 0.0, 1.0}   -- standard green (button, list)
local COLOR_BRIGHT = {1.0, 1.0, 0.4, 1.0}   -- armed-entry highlight

local Mode = {}
Mode.__index = Mode

Mode.COLOR_DIM = COLOR_DIM
Mode.COLOR_BRIGHT = COLOR_BRIGHT

local function formatEntry(text, isArmed)
    if isArmed then return "> " .. text
    else            return "  " .. text end
end
Mode.formatEntry = formatEntry

-- spec fields:
--   key           naming prefix for labels/rects ("spawn", "fluid", ...)
--   label         button text
--   armedField    overlay[] key mirroring the armed value. Kept on the
--                 overlay table itself (not inside the mode) because
--                 scripts/init.lua reads debugOverlay.armedDef /
--                 armedItemDef / armedFluidType / armedTerrainId /
--                 armedLocation / armedStructure directly.
--   exclusiveWith armedField names to clear when this mode arms
--   emptyText     placeholder text when fetch() returns no items (nil
--                 = no placeholder; matches fluid/structure, whose
--                 lists are static and never empty)
--   fetch()       returns the raw item list for this category
--   entryValue(i) the value armed/compared for one raw item (e.g. matId)
--   entryText(i)  the display string for one raw item (e.g. matName)
function Mode.new(spec)
    return setmetatable({
        key           = spec.key,
        label         = spec.label,
        armedField    = spec.armedField,
        exclusiveWith = spec.exclusiveWith,
        emptyText     = spec.emptyText,
        fetch         = spec.fetch,
        entryValue    = spec.entryValue,
        entryText     = spec.entryText,
        buttonId      = nil,
        entries       = {},        -- array of { id, value, text }
        listVisible   = false,
    }, Mode)
end

function Mode:createButton(overlay, uiscale, s, y)
    self.buttonId = label.new({
        name     = self.key .. "_button",
        text     = self.label,
        font     = overlay.debugFont,
        fontSize = overlay.baseSizes.fontSize,
        color    = COLOR_DIM,
        page     = overlay.page,
        uiscale  = uiscale,
        x        = s.margin,
        y        = y,
        zIndex   = 1000,
    })
end

function Mode:destroyButton()
    if self.buttonId then
        label.destroy(self.buttonId)
        self.buttonId = nil
    end
end

function Mode:destroyList()
    for _, entry in ipairs(self.entries) do
        if entry.id then label.destroy(entry.id) end
    end
    self.entries = {}
end

-- Rebuilds this category's list labels starting at `baseY`. Mirrors the
-- per-mode buildXList functions the monolith used to duplicate six times.
function Mode:buildList(overlay, baseY)
    self:destroyList()
    local s = scale.applyAll(overlay.baseSizes)
    local uiscale = scale.get()
    local items = self.fetch() or {}
    local armed = overlay[self.armedField]

    if #items == 0 then
        if self.emptyText then
            local lblId = label.new({
                name     = self.key .. "_list_empty",
                text     = self.emptyText,
                font     = overlay.debugFont,
                fontSize = overlay.baseSizes.fontSize,
                color    = COLOR_DIM,
                page     = overlay.page,
                uiscale  = uiscale,
                x        = s.margin,
                y        = baseY,
                zIndex   = 1000,
            })
            table.insert(self.entries, { id = lblId, value = nil })
        end
        return
    end

    for i, item in ipairs(items) do
        local value = self.entryValue(item)
        local text  = self.entryText(item)
        local isArmed = (value == armed)
        local lblId = label.new({
            name     = self.key .. "_entry_" .. text,
            text     = formatEntry(text, isArmed),
            font     = overlay.debugFont,
            fontSize = overlay.baseSizes.fontSize,
            color    = isArmed and COLOR_BRIGHT or COLOR_DIM,
            page     = overlay.page,
            uiscale  = uiscale,
            x        = s.margin,
            y        = baseY + (i - 1) * (s.fontSize + s.rowSpacing),
            zIndex   = 1000,
        })
        table.insert(self.entries, { id = lblId, value = value, text = text })
    end
end

-- Refresh every entry's "> " prefix / highlight to match the current
-- armed value, without rebuilding the list. Skips the empty placeholder
-- (value == nil).
function Mode:refreshEntries(overlay)
    local armed = overlay[self.armedField]
    for _, entry in ipairs(self.entries) do
        if entry.value ~= nil then
            local isArmed = (entry.value == armed)
            label.setText(entry.id, formatEntry(entry.text, isArmed))
            label.setColor(entry.id, isArmed and COLOR_BRIGHT or COLOR_DIM)
        end
    end
end

return Mode
