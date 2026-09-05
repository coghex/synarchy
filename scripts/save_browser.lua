-- Save Browser - lists saved worlds using the list widget
local scale      = require("scripts.ui.scale")
local responsive = require("scripts.ui.responsive")
local panel      = require("scripts.ui.panel")
local label      = require("scripts.ui.label")
local button     = require("scripts.ui.button")
local list       = require("scripts.ui.list")

local saveBrowser = {}

saveBrowser.page = nil
saveBrowser.panelId = nil
saveBrowser.listId = nil
saveBrowser.saves = {}
saveBrowser.onSelectCallback = nil
saveBrowser.onBackCallback = nil
saveBrowser.menuFont = nil
saveBrowser.boxTexSet = nil
saveBrowser.btnTexSet = nil
saveBrowser.fbW = 0
saveBrowser.fbH = 0
saveBrowser.uiCreated = false
saveBrowser.showMenuCallback = nil

-- The SLOT column of one listing row: the save-slot identity plus the
-- durable status tags and the timestamp. Pure (no UI state, no
-- widgets), so it is the same function the browser draws with and a
-- headless gate can call directly -- there is no second, drifting copy
-- of the label rule.
--
-- #913: the "[Autosave]" tag comes from the durable autosave/manual
-- classification carried in the save's own metadata, NOT from an
-- "autosave-" name check: a player is free to type that name into the
-- manual save box, and a row must say what the save actually IS.
--
-- #762/#1107: "[Recovered]" likewise reports a durable fact about the
-- slot -- this listing was read back from the slot's PREVIOUS
-- generation because the authoritative one was corrupt -- which
-- engine.listSaves() has always published and no UI showed until now.
function saveBrowser.rowLabel(save)
    local text = save.name
    if save.autosave then
        text = text .. "  [Autosave]"
    end
    if save.recovered then
        text = text .. "  [Recovered]"
    end
    if save.timestamp then
        text = text .. "  -  " .. save.timestamp
    end
    return text
end

-- The THREE distinct facts one listing row renders (#1107, epic #708
-- acceptance criterion 6): the save slot, the world's own name, and
-- that name's English reading. They stay separate strings all the way
-- to three separate text elements -- a save slot called
-- `before_the_raid` holding a world called Karadun ("Ashen Land") must
-- read as three facts, never one merged label.
--
-- Degrading is by ABSENCE, never by substitution. engine.listSaves()
-- omits both identity fields for an unnamed save (a pre-identity save
-- included) and omits worldGloss alone for a name with no stated
-- meaning -- a custom player-entered name (#708 principle 7). A missing
-- field yields an EMPTY column: the slot name is never repeated into
-- the world-name column, because "this world has no name" and "this
-- world is called before_the_raid" are different facts.
function saveBrowser.rowFields(save)
    return {
        slot      = saveBrowser.rowLabel(save),
        worldName = save.worldName or "",
        -- Quoted so the reading is visibly a gloss of the name beside
        -- it rather than a second name. A gloss cannot exist without a
        -- name (World.Save.Types guarantees it), so this column is
        -- empty whenever the world-name column is.
        gloss     = save.worldGloss and ('"' .. save.worldGloss .. '"') or "",
    }
end

-- Owned IDs for cleanup
saveBrowser.ownedLabels  = {}
saveBrowser.ownedButtons = {}
saveBrowser.ownedPanels  = {}
saveBrowser.ownedLists   = {}

-- Row layout (#1107). The world identity gets its OWN line under the
-- slot line rather than columns beside it: the slot line already ran
-- the full width at 1280x720 (slot name + tags + an ISO timestamp is
-- wider than the panel), so splitting it would have pushed the
-- timestamp out of view entirely — and requirement 4 is that the row
-- keeps everything it already carried. Line 2 then splits between the
-- name and its gloss, in fractions of the row's usable width so they
-- hold at every scale and framebuffer rather than reserving fixed
-- pixels a narrow window can't afford. The gloss gets the larger share
-- because an English reading is wordier than the name it explains.
saveBrowser.WORLD_NAME_FRACTION = 0.45
saveBrowser.GLOSS_FRACTION      = 0.55
-- The identity line reads as secondary to the slot it belongs to.
saveBrowser.IDENTITY_FONT_SCALE = 0.8

saveBrowser.baseSizes = {
    fontSize      = 24,
    titleFontSize = 32,
    -- Two text lines per row since #1107 (slot line + identity line);
    -- list.lua centers them in this height as one block.
    itemHeight    = 64,
    maxVisible    = 12,
    panelPadX     = 60,
    panelPadY     = 60,
    tileSize      = 64,
    btnHeight     = 52,
    btnSpacing    = 16,
    btnPaddingX   = 80,
    btnTileSize   = 64,
    btnOverflow   = 16,
    textPadding   = 14,
    scrollBtnSize = 24,
}

-----------------------------------------------------------
-- Init (called once from ui_manager.checkReady)
-----------------------------------------------------------

function saveBrowser.init(boxTex, btnTex, font, width, height)
    saveBrowser.boxTexSet = boxTex
    saveBrowser.btnTexSet = btnTex
    saveBrowser.menuFont  = font
    saveBrowser.fbW       = width
    saveBrowser.fbH       = height
end

function saveBrowser.setShowMenuCallback(callback)
    saveBrowser.showMenuCallback = callback
end

-----------------------------------------------------------
-- Cleanup
-----------------------------------------------------------

function saveBrowser.destroyOwned()
    for _, id in ipairs(saveBrowser.ownedLists)   do list.destroy(id)   end
    for _, id in ipairs(saveBrowser.ownedLabels)   do label.destroy(id)  end
    for _, id in ipairs(saveBrowser.ownedButtons)  do button.destroy(id) end
    for _, id in ipairs(saveBrowser.ownedPanels)   do panel.destroy(id)  end
    saveBrowser.ownedLists   = {}
    saveBrowser.ownedLabels  = {}
    saveBrowser.ownedButtons = {}
    saveBrowser.ownedPanels  = {}
    saveBrowser.listId = nil
end

function saveBrowser.shutdown()
    saveBrowser.destroyOwned()
    if saveBrowser.page then
        UI.deletePage(saveBrowser.page)
        saveBrowser.page = nil
    end
end

-----------------------------------------------------------
-- Show / Hide
-----------------------------------------------------------

function saveBrowser.show(saves, onSelect, onBack)
    saveBrowser.saves = saves or engine.listSaves() or {}
    saveBrowser.onSelectCallback = onSelect
    saveBrowser.onBackCallback   = onBack
    saveBrowser.createUI()
    if saveBrowser.page then
        UI.showPage(saveBrowser.page)
    end
end

function saveBrowser.hide()
    if saveBrowser.page then
        UI.hidePage(saveBrowser.page)
    end
end

-----------------------------------------------------------
-- UI Build
-----------------------------------------------------------

function saveBrowser.createUI()
    saveBrowser.destroyOwned()
    if saveBrowser.page then
        UI.deletePage(saveBrowser.page)
    end

    if not saveBrowser.menuFont or not saveBrowser.boxTexSet then
        engine.logWarn("Save browser not initialized, skipping UI creation")
        return
    end

    local uiscale = scale.get()
    local s = scale.applyAllWith(saveBrowser.baseSizes, uiscale)

    -- #748: compact fallback — at a high UI scale and a short/narrow
    -- framebuffer (e.g. the supported 800x2160@4x combination), the
    -- FIXED chrome alone can exceed the size cap in either dimension
    -- before any list rows are even considered, which the
    -- row-count-from-height fit below can't fix on its own (it only
    -- ever reduces rows to a minimum of 1, and doesn't touch width at
    -- all). Shrinks this screen's own effective scale, never the
    -- stored UI scale, against BOTH budgets — the panel's own WIDTH is
    -- a fixed 0.6 fraction of the framebuffer that does NOT scale with
    -- uiscale at all, while its side padding does, so a narrow
    -- framebuffer at a high scale can drive bounds.width to zero or
    -- negative independently of the height fit — taking whichever
    -- constraint is tighter so Back and the list stay reachable and
    -- in-frame.
    local maxPanelHeight = math.floor(saveBrowser.fbH * 0.85)
    local naturalFixedOverhead = (saveBrowser.baseSizes.panelPadY * 2
        + saveBrowser.baseSizes.titleFontSize
        + saveBrowser.baseSizes.btnSpacing * 2
        + saveBrowser.baseSizes.itemHeight
        + saveBrowser.baseSizes.btnHeight) * uiscale
    local scaleForHeight = responsive.fitScale(naturalFixedOverhead, maxPanelHeight, uiscale)

    local panelWidthNatural = math.floor(saveBrowser.fbW * 0.6)
    local CONTENT_WIDTH_MIN = 200
    local naturalHorizontalOverhead = saveBrowser.baseSizes.panelPadX * 2 * uiscale
    local maxHorizontalOverhead = panelWidthNatural - CONTENT_WIDTH_MIN
    local scaleForWidth = responsive.fitScale(
        naturalHorizontalOverhead, maxHorizontalOverhead, uiscale)

    uiscale = math.min(scaleForHeight, scaleForWidth)
    s = scale.applyAllWith(saveBrowser.baseSizes, uiscale)

    saveBrowser.page = UI.newPage("save_browser", "modal")

    local saves = saveBrowser.saves

    -- Build list items. `value` stays the SLOT name (#1107): the world
    -- identity is display-only, and onSelect must keep dispatching the
    -- key engine.loadSave() actually loads.
    local listItems = {}
    for i, save in ipairs(saves) do
        local fields = saveBrowser.rowFields(save)
        table.insert(listItems, {
            text      = fields.slot,
            worldName = fields.worldName,
            gloss     = fields.gloss,
            value     = save.name,
        })
    end

    -- Panel sizing (#748: derive the visible row count from the
    -- available height FIRST, rather than clamping the panel's outer
    -- height after sizing it for baseSizes.maxVisible rows unconditionally
    -- — the old order left the list + Back button overflowing whatever
    -- got clamped away, e.g. Back landing below the framebuffer at a
    -- long save list on an 800x600 window).
    local fixedOverhead = s.panelPadY * 2 + s.titleFontSize
                        + s.btnSpacing * 2 + s.btnHeight
    local heightVisibleCount = math.max(1,
        math.floor((maxPanelHeight - fixedOverhead) / s.itemHeight))
    local visibleCount = math.min(#listItems, saveBrowser.baseSizes.maxVisible,
        heightVisibleCount)
    if visibleCount < 1 then visibleCount = 1 end
    local listHeight = visibleCount * s.itemHeight

    local panelWidth  = math.floor(saveBrowser.fbW * 0.6)
    local contentHeight = s.titleFontSize + s.btnSpacing
                        + listHeight + s.btnSpacing
                        + s.btnHeight
    local panelHeight = math.min(s.panelPadY * 2 + contentHeight, maxPanelHeight)

    local panelX = (saveBrowser.fbW - panelWidth) / 2
    local panelY = (saveBrowser.fbH - panelHeight) / 2

    saveBrowser.panelId = panel.new({
        name       = "save_browser_panel",
        page       = saveBrowser.page,
        x = panelX, y = panelY,
        width      = panelWidth,
        height     = panelHeight,
        textureSet = saveBrowser.boxTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = s.tileSize,
        zIndex     = 1,
        padding    = {
            top = s.panelPadY, bottom = s.panelPadY,
            left = s.panelPadX, right = s.panelPadX,
        },
        uiscale = 1.0,
    })
    table.insert(saveBrowser.ownedPanels, saveBrowser.panelId)

    local baseZ  = panel.getZIndex(saveBrowser.panelId)
    local bounds = panel.getContentBounds(saveBrowser.panelId)

    -- Title
    local titleId = label.new({
        name     = "save_browser_title",
        text     = "Load Game",
        font     = saveBrowser.menuFont,
        fontSize = saveBrowser.baseSizes.titleFontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = saveBrowser.page,
        uiscale  = uiscale,
    })
    table.insert(saveBrowser.ownedLabels, titleId)

    local titleW, _ = label.getSize(titleId)
    local titleX = panelX + bounds.x + (bounds.width - titleW) / 2
    local titleY = panelY + bounds.y + s.titleFontSize
    UI.addToPage(saveBrowser.page,
        label.getElementHandle(titleId), titleX, titleY)
    UI.setZIndex(label.getElementHandle(titleId), baseZ + 1)

    -- List widget
    local listWidth = bounds.width - 20  -- leave room for potential scrollbar
    local listX = panelX + bounds.x + 10
    local listY = titleY + s.btnSpacing

    if #listItems > 0 then
        saveBrowser.listId = list.new({
            name           = "save_list",
            page           = saveBrowser.page,
            x              = listX,
            y              = listY,
            width          = listWidth,
            font           = saveBrowser.menuFont,
            fontSize       = saveBrowser.baseSizes.fontSize,
            itemHeight     = saveBrowser.baseSizes.itemHeight,
            textPadding    = saveBrowser.baseSizes.textPadding,
            scrollButtonSize = saveBrowser.baseSizes.scrollBtnSize,
            -- #748: the height-constrained count, not the raw
            -- baseSizes.maxVisible — otherwise the list widget could
            -- render more rows than the panel/Back-button math budgeted
            -- space for.
            maxVisible     = visibleCount,
            uiscale        = uiscale,
            zIndex         = baseZ + 2,
            items          = listItems,
            -- #1107: the world's own name and its English reading, on
            -- their own line under the slot line, each truncated to its
            -- allocation rather than left to run into its neighbour.
            -- The gloss reads dimmer than the name it explains.
            columns = {
                { key = "worldName", fraction = saveBrowser.WORLD_NAME_FRACTION,
                  line = 2, fontScale = saveBrowser.IDENTITY_FONT_SCALE,
                  color = {1.0, 1.0, 1.0, 1.0} },
                { key = "gloss",     fraction = saveBrowser.GLOSS_FRACTION,
                  line = 2, fontScale = saveBrowser.IDENTITY_FONT_SCALE,
                  color = {0.72, 0.72, 0.72, 1.0} },
            },
            textColor           = {1.0, 1.0, 1.0, 1.0},
            highlightColor      = {0.3, 0.5, 0.8, 0.8},
            highlightTextColor  = {1.0, 1.0, 1.0, 1.0},
            selectedColor       = {0.2, 0.4, 0.7, 1.0},
            selectedTextColor   = {1.0, 1.0, 1.0, 1.0},
            onSelect = function(value, text, index, listId, listName)
                engine.logInfo("Save selected: " .. value)
                if saveBrowser.onSelectCallback then
                    saveBrowser.onSelectCallback(value)
                end
            end,
        })
        table.insert(saveBrowser.ownedLists, saveBrowser.listId)
    else
        -- No saves: show a message
        local noSavesId = label.new({
            name     = "no_saves_label",
            text     = "No saved games found.",
            font     = saveBrowser.menuFont,
            fontSize = saveBrowser.baseSizes.fontSize,
            color    = {0.7, 0.7, 0.7, 1.0},
            page     = saveBrowser.page,
            uiscale  = uiscale,
        })
        table.insert(saveBrowser.ownedLabels, noSavesId)
        local nsW, _ = label.getSize(noSavesId)
        local nsX = panelX + bounds.x + (bounds.width - nsW) / 2
        UI.addToPage(saveBrowser.page,
            label.getElementHandle(noSavesId), nsX, listY + s.itemHeight / 2)
        UI.setZIndex(label.getElementHandle(noSavesId), baseZ + 2)
    end

    -- Back button
    local backText = "Back"
    local backBtnId = button.new({
        name       = "save_browser_back",
        text       = backText,
        width      = 120,
        height     = saveBrowser.baseSizes.btnHeight,
        fontSize   = saveBrowser.baseSizes.fontSize,
        tileSize   = saveBrowser.baseSizes.btnTileSize,
        overflow   = saveBrowser.baseSizes.btnOverflow,
        uiscale    = uiscale,
        page       = saveBrowser.page,
        font       = saveBrowser.menuFont,
        textureSet = saveBrowser.btnTexSet,
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textColor  = {1.0, 1.0, 1.0, 1.0},
        callbackName = "onSaveBrowserBack",
    })
    table.insert(saveBrowser.ownedButtons, backBtnId)

    local btnW, btnH = button.getSize(backBtnId)
    local btnX = panelX + bounds.x + (bounds.width - btnW) / 2
    local btnY = listY + listHeight + s.btnSpacing
    UI.addToPage(saveBrowser.page,
        button.getElementHandle(backBtnId), btnX, btnY)
    UI.setZIndex(button.getElementHandle(backBtnId), baseZ + 2)

    saveBrowser.uiCreated = true
    engine.logDebug("Save browser created with " .. #saves .. " saves")
end

-----------------------------------------------------------
-- Scroll events (forwarded from ui_manager)
-----------------------------------------------------------

function saveBrowser.onScroll(elemHandle, dx, dy)
    if saveBrowser.listId then
        return list.onScroll(elemHandle, dx, dy)
    end
    return false
end

function saveBrowser.handleScrollCallback(callbackName, elemHandle)
    if saveBrowser.listId then
        return list.handleCallback(callbackName, elemHandle)
    end
    return false
end

-----------------------------------------------------------
-- Resize
-----------------------------------------------------------

function saveBrowser.onFramebufferResize(width, height)
    saveBrowser.fbW = width
    saveBrowser.fbH = height
    if saveBrowser.uiCreated and saveBrowser.page then
        -- #748: preserve the selected save across a mere geometry
        -- rebuild — list.new() always starts with no selection, so
        -- without this a resize would silently deselect whatever the
        -- player had picked.
        local prevValue = saveBrowser.listId
            and list.getSelectedValue(saveBrowser.listId)

        -- #748 round 5: preserve keyboard CONTROL focus (#745) too,
        -- mirroring settings_menu/create_world_menu/main_menu/pause_menu.
        -- createUI() always deletes+recreates a fresh page (unlike those
        -- other screens' teardown-only rebuild), so the restore must
        -- wait until the fresh page is genuinely re-shown.
        local wasVisible = UI.isPageVisible(saveBrowser.page)
        local controlFocusName = wasVisible and responsive.snapshotControlFocusName()

        saveBrowser.createUI()

        if wasVisible and saveBrowser.page then
            UI.showPage(saveBrowser.page)
            responsive.restoreControlFocusName(controlFocusName)
        end

        if prevValue and saveBrowser.listId then
            for idx, save in ipairs(saveBrowser.saves) do
                if save.name == prevValue then
                    -- setSelectedIndex, not selectItem: restoring the
                    -- highlight must not re-fire onSelect (which loads
                    -- the save and transitions the whole game).
                    list.setSelectedIndex(saveBrowser.listId, idx)
                    break
                end
            end
        end
    end
end

return saveBrowser
