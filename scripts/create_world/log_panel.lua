-- Log Panel for Create World Menu
-- Creates the right-side panel with world preview, status label,
-- scrollable log output, and scrollbar.
local panel     = require("scripts.ui.panel")
local label     = require("scripts.ui.label")
local sprite    = require("scripts.ui.sprite")
local scrollbar = require("scripts.ui.scrollbar")

local logPanel = {}

-----------------------------------------------------------
-- Create the right panel (preview + log area + scrollbar)
-----------------------------------------------------------

-- params = {
--   menu       = createWorldMenu ref (for reading state),
--   page, panelTexSet, menuFont, baseSizes, uiscale, s,
--   panelX, panelY, bounds, contentStartY,
--   leftWidth, rightWidth, contentHeight,
--   worldPreviewTexture,
--   trackPanel, trackSprite, trackLabel,
--   zRightPanel, zPreview, zLogText,
--   zLogSbTrack, zLogSbButton, zLogSbTab,
-- }
-- Returns: {
--   rightPanelId, statusLabelId, logLabelIds,
--   logScrollbarId, logMaxVisible, logLineHeight,
--   logX, logStartY,
-- }
function logPanel.create(params)
    local page       = params.page
    local uiscale    = params.uiscale
    local s          = params.s
    local base       = params.baseSizes
    local contentStartY = params.contentStartY
    local contentHeight = params.contentHeight

    local rightX = params.panelX + params.bounds.x
                 + params.leftWidth + math.floor(20 * uiscale)
    local rightWidth = params.rightWidth

    local rightPanelId = params.trackPanel(panel.new({
        name       = "right_panel",
        page       = page,
        x          = rightX,
        y          = contentStartY,
        width      = rightWidth,
        height     = contentHeight,
        textureSet = params.panelTexSet,
        color      = {0.2, 0.2, 0.2, 1.0},
        tileSize   = 64,
        zIndex     = params.zRightPanel,
        padding    = { top = 10, bottom = 10, left = 10, right = 10 },
        uiscale    = uiscale,
    }))

    -- Make the right panel clickable so it receives scroll events
    local rightPanelHandle = panel.getBoxHandle(rightPanelId)
    UI.setClickable(rightPanelHandle, true)
    UI.setOnClick(rightPanelHandle, "onLogPanelScroll")

    local rightBounds = panel.getContentBounds(rightPanelId)

    ---------------------------------------------------------
    -- World preview image (upper portion)
    ---------------------------------------------------------
    local previewSize = math.min(rightBounds.width,
                                 rightBounds.height * 0.5) * 0.7
    local previewX = rightX + rightBounds.x
                   + (rightBounds.width - previewSize) / 2
    local previewY = contentStartY + rightBounds.y
                   + math.floor(20 * uiscale)

    if params.worldPreviewTexture then
        params.trackSprite(sprite.new({
            name    = "world_preview",
            page    = page,
            x       = previewX,
            y       = previewY,
            width   = previewSize,
            height  = previewSize,
            texture = params.worldPreviewTexture,
            color   = {1.0, 1.0, 1.0, 1.0},
            zIndex  = params.zPreview,
            uiscale = uiscale,
        }))
    end

    ---------------------------------------------------------
    -- Status label (below preview)
    ---------------------------------------------------------
    local logTopY = previewY + previewSize + math.floor(20 * uiscale)
    local logX    = rightX + rightBounds.x + math.floor(10 * uiscale)

    local statusLabelId = params.trackLabel(label.new({
        name     = "gen_status",
        text     = "",
        font     = params.menuFont,
        fontSize = base.logFontSize,
        color    = {0.7, 0.9, 0.7, 1.0},
        page     = page,
        uiscale  = uiscale,
    }))
    local statusHandle = label.getElementHandle(statusLabelId)
    UI.addToPage(page, statusHandle, logX, logTopY + s.logFontSize)
    UI.setZIndex(statusHandle, params.zLogText)

    ---------------------------------------------------------
    -- Log line label slots (fixed count, virtual-scrolled)
    ---------------------------------------------------------
    local logLineStartY = logTopY + s.logFontSize + math.floor(10 * uiscale)
    local logLineHeight = math.floor(base.logFontSize * 1.4 * uiscale)
    local availableHeight = contentStartY + contentHeight
                          - logLineStartY - math.floor(10 * uiscale)
    local maxLogLines = math.max(1, math.floor(availableHeight / logLineHeight))

    local logLabelIds = {}
    for i = 1, maxLogLines do
        local lid = params.trackLabel(label.new({
            name     = "gen_log_" .. i,
            text     = "",
            font     = params.menuFont,
            fontSize = base.logFontSize,
            color    = {0.6, 0.6, 0.6, 1.0},
            page     = page,
            uiscale  = uiscale,
        }))
        local lh = label.getElementHandle(lid)
        UI.addToPage(page, lh,
                     logX,
                     logLineStartY + (i - 1) * logLineHeight + s.logFontSize)
        UI.setZIndex(lh, params.zLogText)
        table.insert(logLabelIds, lid)
    end

    ---------------------------------------------------------
    -- Scrollbar (hidden until content overflows)
    ---------------------------------------------------------
    local sbBtnSize = math.floor(24 * uiscale)
    local sbCapH    = math.floor(4 * uiscale)
    local sbTrackH  = math.max(math.floor(20 * uiscale),
                               availableHeight - sbBtnSize * 2 - sbCapH * 2)
    local sbX       = rightX + rightBounds.x + rightBounds.width - sbBtnSize

    local logScrollbarId = scrollbar.new({
        name         = "log_scrollbar",
        page         = page,
        x            = sbX,
        y            = logLineStartY,
        buttonSize   = sbBtnSize,
        trackHeight  = sbTrackH,
        capHeight    = sbCapH,
        tileSize     = math.floor(8 * uiscale),
        totalItems   = 0,
        visibleItems = maxLogLines,
        uiscale      = uiscale,
        zIndex       = { track  = params.zLogSbTrack,
                         button = params.zLogSbButton,
                         tab    = params.zLogSbTab },
        onScroll = function(offset, sbId, sbName)
            if params.onLogScroll then
                params.onLogScroll(offset)
            end
        end,
    })
    scrollbar.setVisible(logScrollbarId, false)

    engine.logDebug("Log panel created: maxVisible=" .. maxLogLines
        .. " lineHeight=" .. logLineHeight)

    return {
        rightPanelId   = rightPanelId,
        statusLabelId  = statusLabelId,
        logLabelIds    = logLabelIds,
        logScrollbarId = logScrollbarId,
        logMaxVisible  = maxLogLines,
        logLineHeight  = logLineHeight,
        logX           = logX,
        logStartY      = logLineStartY,
    }
end

-----------------------------------------------------------
-- Log text helpers (operate on menu state passed in)
-----------------------------------------------------------

function logPanel.addLine(menu, text)
    table.insert(menu.logLines, text)
    logPanel.updateScrollbar(menu)
    -- Auto-scroll to bottom
    local totalLines = #menu.logLines
    if totalLines > menu.logMaxVisible then
        local maxOffset = totalLines - menu.logMaxVisible
        menu.logScrollOffset = maxOffset
        scrollbar.setScrollOffset(menu.logScrollbarId, maxOffset)
    end
    logPanel.refreshDisplay(menu)
end

function logPanel.clear(menu)
    menu.logLines = {}
    menu.logScrollOffset = 0
    if menu.logScrollbarId then
        scrollbar.setScrollOffset(menu.logScrollbarId, 0)
        scrollbar.setContentSize(menu.logScrollbarId, 0, menu.logMaxVisible)
        scrollbar.setVisible(menu.logScrollbarId, false)
    end
    logPanel.refreshDisplay(menu)
end

function logPanel.refreshDisplay(menu)
    local maxVisible = #menu.logLabelIds
    local offset = menu.logScrollOffset
    for i = 1, maxVisible do
        local lid = menu.logLabelIds[i]
        local lineIndex = offset + i
        local text = menu.logLines[lineIndex] or ""
        local lh = label.getElementHandle(lid)
        UI.setText(lh, text)
    end
end

function logPanel.setStatus(menu, text)
    if menu.statusLabelId then
        local sh = label.getElementHandle(menu.statusLabelId)
        UI.setText(sh, text)
    end
end

function logPanel.updateScrollbar(menu)
    if not menu.logScrollbarId then return end
    local totalLines = #menu.logLines
    if totalLines > menu.logMaxVisible then
        scrollbar.setContentSize(menu.logScrollbarId,
                                 totalLines, menu.logMaxVisible)
        scrollbar.setVisible(menu.logScrollbarId, true)
    else
        scrollbar.setContentSize(menu.logScrollbarId,
                                 totalLines, menu.logMaxVisible)
        scrollbar.setVisible(menu.logScrollbarId, false)
    end
end

function logPanel.onScrollChanged(menu, newOffset)
    menu.logScrollOffset = newOffset
    logPanel.refreshDisplay(menu)
end

return logPanel
