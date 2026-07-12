-- Unit info v2 generic stat-panel engine (#542 split from
-- unit_info_v2.lua).
--
-- The scrollable icon+value row list shared by every stats sub-tab
-- (Status/Physical/Mental/Skill/Knowledge): row placement, the icon
-- cache/lookup, and the scrollbar + wheel-scroll wiring. Section
-- content itself (which rows to show) lives in unit_info_v2_status.lua
-- and unit_info_v2_panels.lua; both call buildIconStatPanel here.

local unitInfoV2 = package.loaded["scripts.unit_info_v2"]
local hud       = require("scripts.hud")
local label     = require("scripts.ui.label")
local scrollbar = require("scripts.ui.scrollbar")
local L         = require("scripts.unit_info_v2_layout")
local statDefs  = require("scripts.unit_info_v2_stat_defs")

local M = {}

-- engine.loadTexture caches by path, but we keep a per-key map so each
-- key resolves to a (texture, def) pair once and we don't re-hit the
-- engine each rebuild. A miss with no per-family "<kind>_unknown" entry
-- falls back to text (#478).
local iconCache = {}

-- Icons live in kind subfolders (assets/textures/icons/<kind>/<name>.png) but
-- are referenced by bare name (e.g. "strength"). Build a one-time
-- basename -> full-path index over the subfolders so callers stay name-only.
local ICON_SUBDIRS = { "stat", "skill", "status", "injury", "infection", "knowledge", "effects" }
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

-- `kind` (one of ICON_SUBDIRS) is the row's icon family, used ONLY as a
-- last resort when iconKey itself has no match — picks that family's
-- "<kind>_unknown" placeholder (#478) instead of degrading to text.
-- Cache key includes kind since the same iconKey could miss under a
-- different family default from a different panel.
local function loadIconFor(iconKey, kind)
    if not iconKey then return nil end
    local cacheKey = iconKey .. "|" .. (kind or "")
    if iconCache[cacheKey] ~= nil then
        return iconCache[cacheKey] or nil
    end
    if not iconIndex then buildIconIndex() end
    local path = iconIndex[iconKey] or (kind and iconIndex[kind .. "_unknown"])
    local tex  = path and engine.loadTexture(path) or nil
    iconCache[cacheKey] = tex or false
    return tex
end

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

-- Place a single content row: stat icon on the left, bright value on
-- the right. The icon owns the description tooltip (from STAT_DEFS or
-- `tooltipOverride`). The value gets its OWN tooltip via
-- `valueTooltip` (a {text, hint} table) — used to surface how the
-- effective stat was computed (e.g. base + modifier sources). Returns
-- the value-label id and its baseline-y so per-panel refresh closures
-- can re-rightalign the value on update.
-- opts (optional): { fontSize, color, align = "left", abbreviate = true }.
-- Default (no opts) = big white value, right-aligned (the stat-number look).
-- align="left" puts a coloured, abbreviated name right after the icon — the
-- condition/injury look.
local function placeIconStatRow(rect, rowIndex, statKey, valueText,
                                  tooltipOverride, valueTooltip, opts, kind)
    local rowTop = rect.y + L.CONTENT_TOP_PAD + rowIndex * L.CONTENT_ROW_H
    local y      = rowTop + L.CONTENT_FONT_SIZE
                 + math.floor((L.CONTENT_ROW_H - L.CONTENT_FONT_SIZE) / 2)
                 - math.floor(L.CONTENT_FONT_SIZE * 0.3)

    local def     = statDefs.STAT_DEFS[statKey]
    local iconTex = loadIconFor(def and def.icon or statKey, kind)
    if iconTex then
        local iconY = rowTop + math.floor((L.CONTENT_ROW_H - L.ICON_SIZE) / 2)
        local iconId = UI.newSprite(
            "unit_info_v2_stat_icon_" .. rowIndex,
            L.ICON_SIZE, L.ICON_SIZE, iconTex,
            1.0, 1.0, 1.0, 1.0,
            unitInfoV2.page)
        UI.addToPage(unitInfoV2.page, iconId,
            rect.x + L.CONTENT_LEFT_PAD, iconY)
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
            fontSize = L.CONTENT_FONT_SIZE,
            color    = L.CONTENT_DIM_COLOR,
            page     = unitInfoV2.page,
            uiscale  = 1.0,
        })
        local nameH = label.getElementHandle(nameLbl)
        UI.addToPage(unitInfoV2.page, nameH, rect.x + L.CONTENT_LEFT_PAD, y)
        UI.setZIndex(nameH, 12)
        local tt = tooltipOverride or (def and
            { text = def.name, hint = def.desc })
        if tt then label.setTooltipRich(nameLbl, tt) end
        table.insert(unitInfoV2.statsContentElements,
            { kind = "label", id = nameLbl })
    end

    local fontSize = (opts and opts.fontSize) or L.CONTENT_FONT_SIZE
    local color    = (opts and opts.color)    or L.CONTENT_VAL_COLOR
    -- Re-centre the value vertically for its (possibly smaller) font.
    local valY = rowTop + fontSize
               + math.floor((L.CONTENT_ROW_H - fontSize) / 2)
               - math.floor(fontSize * 0.3)

    local text = valueText
    local valX, leftMaxW
    if opts and opts.align == "left" then
        -- Coloured name flowing right from the icon; abbreviate to fit.
        valX = rect.x + L.CONTENT_LEFT_PAD + L.ICON_SIZE
             + math.floor(L.ICON_SIZE * 0.25)
        leftMaxW = (rect.x + rect.w - L.CONTENT_RIGHT_PAD) - valX
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
        valX = rect.x + rect.w - L.CONTENT_RIGHT_PAD - valW
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

-- Generic builder for stat-list panels (all of Status/Stats/Mental/
-- Attributes use this; Skill is similar but its row list is dynamic).
-- Each rowDef entry:
--   { key = "<STAT_DEFS key>",
--     value = function(uid) end,
--     tooltip = optional icon-tooltip ({text,hint} or function),
--     valueTooltip = optional value-tooltip (function(uid) → {text,hint}),
--     kind = optional per-row icon family override (see `kind` param)
--   }
-- Rows whose value() returns nil are skipped so the layout stays compact.
-- `kind` (one of the ICON_SUBDIRS families) is this panel's default icon
-- family, used for the "<kind>_unknown" fallback on an icon miss (#478) —
-- a row's own `kind` field overrides it (buildStatusPanel mixes
-- status/injury/infection rows in one list).
function M.buildIconStatPanel(rect, uid, rowDefs, kind)
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
        (rect.h - L.CONTENT_TOP_PAD) / L.CONTENT_ROW_H))
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
                    w = rect.w - L.SCROLLBAR_RESERVE, h = rect.h }
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
            r.value(uid) or "?", tt, vtt, rowOpts, r.kind or kind)
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
        local sbX      = rect.x + rect.w - L.SCROLLBAR_RESERVE
        local sbY      = rect.y + L.CONTENT_TOP_PAD
        local trackH   = math.max(24,
            rect.h - 2 * L.CONTENT_TOP_PAD - 2 * sbButton - 2 * sbCap)
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

    -- Transparent background: persists across same-tab rebuilds.
    -- #743: explicit scroll-capture so wheel events over the panel body
    -- (between rows / over non-clickable icons) route via onUIScroll
    -- instead of escaping to game-scroll → world zoom, and explicit
    -- pointer-blocking so a click on blank panel space doesn't fall
    -- through to gameplay — no click callback needed for either policy
    -- (review round 3: the old onStatsPanelBgClick existed only to
    -- satisfy the pre-#743 clickable-implies-wheel-eligible coupling
    -- and was otherwise a dead no-op; removed).
    if not unitInfoV2.statsBgClickId then
        local bgId = UI.newElement(
            "unit_info_v2_stats_bg",
            rect.w, rect.h,
            unitInfoV2.page)
        UI.addToPage(unitInfoV2.page, bgId, rect.x, rect.y)
        UI.setPointerBlocking(bgId, true)
        UI.setScrollCapture(bgId, true)
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
                    rowRect.x + rowRect.w - L.CONTENT_RIGHT_PAD - valW, ref.y)
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

return M
