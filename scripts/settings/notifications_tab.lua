-- Notifications Tab
-- Builds the per-category settings grid:
--
--                | Event Log | Popup | Pause |
--   Save / Load  |    [X]    |  [X]  |  [ ]  |
--   Survival …   |    [X]    |  [X]  |  [X]  |
--   …
--
-- Row 0 is a header row with the three column labels. Rows 1..N are
-- one per category, with the category display_name (rendered in its
-- text_color so the player can see which color popups/log entries
-- will use) plus three checkboxes aligned to the columns above.
--
-- Apply timing: each checkbox click writes through immediately via
-- engine.setNotificationOverrides — the engine updates the in-memory
-- IORef and persists to config/notifications.yaml. There is no
-- "pending" state for this tab and no Apply/Cancel for notification
-- changes (consistent with the user's choice on 2026-05-18).

local label    = require("scripts.ui.label")
local checkbox = require("scripts.ui.checkbox")

local notificationsTab = {}

-- Track the checkbox IDs per row so widgetSetPosition / widgetSetVisible
-- callbacks can move them. Keyed by category id × flag.
-- shape: notificationsTab.checkboxes[catId] = { log=id, popup=id, pause=id }
notificationsTab.checkboxes = {}

-----------------------------------------------------------
-- Column geometry
-----------------------------------------------------------

-- Given the row's right edge (cx + cw), the scaled checkbox size, the
-- inter-column step, and a right-side padding (room for label
-- overhang past the rightmost checkbox), compute the X position for
-- each of the three column checkboxes. Columns step right-to-left.
local function columnXs(cx, cw, cbSize, colStep, rightPad)
    local pauseX = cx + cw - cbSize - rightPad
    local popupX = pauseX - colStep
    local logX   = popupX - colStep
    return logX, popupX, pauseX
end

-----------------------------------------------------------
-- Create all rows
-----------------------------------------------------------

-- params (same shape as graphics_tab.create):
--   page, font, baseSizes, uiscale, s, contentX, contentY, contentW,
--   zContent, zWidgets,
--   trackLabel, trackCheckbox
-- Returns: rowHandles[]
function notificationsTab.create(params)
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

    local rows = {}
    local rowIndex = 0
    notificationsTab.checkboxes = {}

    -- Pull the current category state from the engine.
    local cfg = engine.getNotificationCfg()
    if not cfg or #cfg == 0 then
        -- No categories registered (shouldn't happen unless the YAML
        -- registry is missing). Render a single explanatory row so
        -- the tab isn't empty.
        local lbl = params.trackLabel(label.new({
            name     = "notifications_empty",
            text     = "No notification categories configured.",
            font     = font,
            fontSize = base.fontSize,
            color    = {0.7, 0.7, 0.7, 1.0},
            page     = page,
            uiscale  = uiscale,
        }))
        local h = label.getElementHandle(lbl)
        UI.addToPage(page, h, cx, cy + s.fontSize)
        UI.setZIndex(h, zContent)
        return { { labelHandle = h, widgetHandles = {},
                   widgetSetPosition = nil, widgetSetVisible = nil } }
    end

    local cbSize = math.floor(base.checkboxSize * uiscale)
    -- Step between columns: measure the actual header label widths so
    -- the longest one ("Event Log") still has visible gap to its
    -- neighbours. The 24-px pad is the minimum slack between adjacent
    -- centred labels; we additionally enforce a floor of cbSize + 48
    -- so columns never collapse below the original spacing on very
    -- short labels.
    local headerFontSize = base.fontSize
    local headerMinPad   = math.floor(24 * uiscale)
    local maxHeaderW = 0
    for _, t in ipairs({ "Event Log", "Popup", "Pause" }) do
        local w = engine.getTextWidth(font, t, headerFontSize)
        if w > maxHeaderW then maxHeaderW = w end
    end
    local colStep = math.max(cbSize + math.floor(48 * uiscale),
                             maxHeaderW + headerMinPad)

    -- Right-side padding: the rightmost header ("Pause") is centred
    -- over its checkbox, so when the label is wider than the
    -- checkbox the label overhangs to the right by (labelW-cbSize)/2.
    -- Without padding that overhang runs past the panel edge. Shift
    -- the columns left by exactly the overhang plus a small breathing
    -- gap so the rightmost label sits inside the content area.
    local pauseLabelW   = engine.getTextWidth(font, "Pause", headerFontSize)
    local rightOverhang = math.max(0,
        math.floor((pauseLabelW - cbSize) / 2))
    local rightPad = rightOverhang + math.floor(8 * uiscale)

    -- Helper: compute Y position for row N (0-based)
    local function rowY(n)
        return cy + s.rowSpacing * n
    end

    ---------------------------------------------------------
    -- Row 0: column headers
    ---------------------------------------------------------
    local logX, popupX, pauseX = columnXs(cx, cw, cbSize, colStep, rightPad)

    local headerColor = {0.85, 0.85, 0.85, 1.0}

    -- Helper to make + position one column header
    local function makeHeader(name, text, colX)
        local id = params.trackLabel(label.new({
            name     = "notif_header_" .. name,
            text     = text,
            font     = font,
            fontSize = headerFontSize,
            color    = headerColor,
            page     = page,
            uiscale  = uiscale,
        }))
        local handle = label.getElementHandle(id)
        local lw, _ = label.getSize(id)
        -- Center the label over its checkbox column
        local lx = colX + math.floor((cbSize - lw) / 2)
        UI.addToPage(page, handle, lx, rowY(rowIndex) + s.fontSize)
        UI.setZIndex(handle, zContent)
        return { id = id, handle = handle, baseX = lx }
    end

    -- Left-side caption ("Category") so the row reads as a table
    local catHeaderId = params.trackLabel(label.new({
        name     = "notif_header_category",
        text     = "Category",
        font     = font,
        fontSize = headerFontSize,
        color    = headerColor,
        page     = page,
        uiscale  = uiscale,
    }))
    local catHeaderHandle = label.getElementHandle(catHeaderId)
    UI.addToPage(page, catHeaderHandle, cx, rowY(rowIndex) + s.fontSize)
    UI.setZIndex(catHeaderHandle, zContent)

    local logHeader   = makeHeader("log",   "Event Log", logX)
    local popupHeader = makeHeader("popup", "Popup",     popupX)
    local pauseHeader = makeHeader("pause", "Pause",     pauseX)

    table.insert(rows, {
        labelHandle = catHeaderHandle,
        widgetHandles = {
            logHeader.handle, popupHeader.handle, pauseHeader.handle,
        },
        widgetSetPosition = function(ry)
            -- Reposition each header by recalculating x using its
            -- stored center-offset (label widths don't change).
            UI.setPosition(logHeader.handle,   logHeader.baseX,   ry + s.fontSize)
            UI.setPosition(popupHeader.handle, popupHeader.baseX, ry + s.fontSize)
            UI.setPosition(pauseHeader.handle, pauseHeader.baseX, ry + s.fontSize)
        end,
        widgetSetVisible = function(vis)
            UI.setVisible(logHeader.handle,   vis)
            UI.setVisible(popupHeader.handle, vis)
            UI.setVisible(pauseHeader.handle, vis)
        end,
    })
    rowIndex = rowIndex + 1

    ---------------------------------------------------------
    -- Per-category rows
    ---------------------------------------------------------

    for _, cat in ipairs(cfg) do
        local catId = cat.id
        local catColor = (cat.textColor and {
            cat.textColor.r or 1.0,
            cat.textColor.g or 1.0,
            cat.textColor.b or 1.0,
            cat.textColor.a or 1.0,
        }) or {1.0, 1.0, 1.0, 1.0}

        -- Row label: category display name in its text color
        local rowLabelId = params.trackLabel(label.new({
            name     = "notif_row_" .. catId,
            text     = cat.displayName or catId,
            font     = font,
            fontSize = base.fontSize,
            color    = catColor,
            page     = page,
            uiscale  = uiscale,
        }))
        local rowLabelHandle = label.getElementHandle(rowLabelId)
        UI.addToPage(page, rowLabelHandle, cx, rowY(rowIndex) + s.fontSize)
        UI.setZIndex(rowLabelHandle, zContent)

        -- onChange handler factory: writes one flag through to the
        -- engine, which updates the IORef and the YAML overrides.
        -- Each callback closes over (catId, flag).
        local function onToggle(flag)
            return function(checked, _id, _name)
                local overrides = {}
                overrides[catId] = {}
                overrides[catId][flag] = checked
                engine.setNotificationOverrides(overrides)
            end
        end

        local logCbId = params.trackCheckbox(checkbox.new({
            name     = "notif_" .. catId .. "_log",
            size     = base.checkboxSize,
            uiscale  = uiscale,
            page     = page,
            x        = logX,
            y        = rowY(rowIndex),
            default  = cat.log,
            zIndex   = zWidgets,
            onChange = onToggle("log"),
        }))
        local popupCbId = params.trackCheckbox(checkbox.new({
            name     = "notif_" .. catId .. "_popup",
            size     = base.checkboxSize,
            uiscale  = uiscale,
            page     = page,
            x        = popupX,
            y        = rowY(rowIndex),
            default  = cat.popup,
            zIndex   = zWidgets,
            onChange = onToggle("popup"),
        }))
        local pauseCbId = params.trackCheckbox(checkbox.new({
            name     = "notif_" .. catId .. "_pause",
            size     = base.checkboxSize,
            uiscale  = uiscale,
            page     = page,
            x        = pauseX,
            y        = rowY(rowIndex),
            default  = cat.pause,
            zIndex   = zWidgets,
            onChange = onToggle("pause"),
        }))

        notificationsTab.checkboxes[catId] = {
            log = logCbId, popup = popupCbId, pause = pauseCbId,
        }

        table.insert(rows, {
            labelHandle = rowLabelHandle,
            widgetHandles = {
                checkbox.getElementHandle(logCbId),
                checkbox.getElementHandle(popupCbId),
                checkbox.getElementHandle(pauseCbId),
            },
            widgetSetPosition = function(ry)
                checkbox.setPosition(logCbId,   logX,   ry)
                checkbox.setPosition(popupCbId, popupX, ry)
                checkbox.setPosition(pauseCbId, pauseX, ry)
            end,
            widgetSetVisible = function(vis)
                checkbox.setVisible(logCbId,   vis)
                checkbox.setVisible(popupCbId, vis)
                checkbox.setVisible(pauseCbId, vis)
            end,
        })
        rowIndex = rowIndex + 1
    end

    return rows
end

return notificationsTab
