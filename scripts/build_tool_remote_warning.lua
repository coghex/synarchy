-- Remote-settlement confirmation modal (#779).
--
-- A valid starting-portal (acolyte_portal) placement whose footprint is
-- more than the remote threshold from every placed location on this
-- world page (Building.Placement.isRemote, via building.remoteCheck)
-- pauses for this two-button confirmation instead of spawning
-- instantly. Opening the warning does not spawn a portal, consume
-- resources, mark anything discovered, modify the world, or exit
-- placement mode — build_tool.lua stays armed with the portal target
-- selected throughout.
--
-- "Choose Another Site" (and Escape) cancels: closes the modal, spawns
-- nothing, leaves placement armed. "Establish Here" re-validates the
-- SAVED tile against the current building.canPlaceAt rules (terrain,
-- occupancy, location overlap, active-world state may have changed
-- while the modal was open) before delegating to
-- scripts.build_tool.commitStartingPlacement — the exact same
-- spawn/outcome/exitPlacement/selectDefaultTool path a non-remote
-- single click takes, run exactly once.
--
-- Not registered with saveModules — a pending confirmation is
-- in-progress UI state, same convention as build_tool.state
-- (build_tool.lua's own header comment). Never engine.loadScript'd
-- directly (no per-tick work of its own), so — unlike build_tool.lua —
-- it needs no package.loaded self-registration guard; require() alone
-- shares one table across every caller.
local buildToolRemoteWarning = {}

local panel = require("scripts.ui.panel")
local label = require("scripts.ui.label")
local scale = require("scripts.ui.scale")
local responsive = require("scripts.ui.responsive")

buildToolRemoteWarning.page = nil
buildToolRemoteWarning.panelId = nil
buildToolRemoteWarning.boxTexSet = nil
buildToolRemoteWarning.bodyFont = nil
buildToolRemoteWarning.titleFont = nil
buildToolRemoteWarning.fbW = 0
buildToolRemoteWarning.fbH = 0

buildToolRemoteWarning.ownedLabels = {}
buildToolRemoteWarning.ownedBoxes  = {}
-- Button box handle -> its child text element handle (#750 round-13:
-- these are raw UI.newText elements, not label.lua-wrapped, so they
-- don't fit ownedLabels' label.destroy(id) convention — cascade-deleted
-- along with the rest of the page on rebuild, same as every other
-- page-child element here; tracked only so callers/tests can introspect
-- the actual rendered button text geometry). Text elements always
-- report a zero-sized UI.getElementInfo bounding box (see label.lua's
-- own comment on the same fact), so buttonFontSizeByBox exists purely
-- so a caller/test can independently re-derive the real rendered width
-- via engine.getTextWidth(bodyFont, text, thatSize).
buildToolRemoteWarning.buttonTextByBox = {}
buildToolRemoteWarning.buttonFontSizeByBox = {}

-- { defName, gx, gy, worldId } for the tile the warning was opened
-- against, or nil when closed. distance/thresholdTiles are cached
-- purely for onFramebufferResize's rebuild — establishHere always
-- re-derives the live distance via a fresh canPlaceAt/remoteCheck-
-- adjacent revalidate, never trusts these for the actual decision.
-- worldId (#844) is the active world page captured at open() time —
-- establishHere() must reject rather than revalidate against a
-- DIFFERENT page's terrain/locations if the active world changed while
-- the modal was open.
buildToolRemoteWarning.pending = nil

buildToolRemoteWarning.clickHandlers = {}

local PANEL_W_BASE = 560

local baseSizes = {
    titleFontSize  = 26,
    bodyFontSize   = 18,
    buttonFontSize = 18,
    buttonHeight   = 56,
    buttonSpacing  = 24,
    buttonTileSize = 48,
    buttonPaddingX = 48,
    buttonOverflow = 12,
    panelPaddingX  = 44,
    panelPaddingY  = 36,
    panelTileSize  = 64,
    panelOverflow  = 0,
    titleGap       = 14,
    messageGap     = 28,
}

local function formatMessage(distance, thresholdTiles)
    if distance == nil then
        return "No locations have been mapped yet — this will be an isolated start."
    end
    return "The nearest mapped location is about " .. tostring(math.floor(distance))
        .. " tiles away (remote past " .. tostring(thresholdTiles) .. ")."
end

-----------------------------------------------------------
-- Lifecycle wiring (called from ui_manager_boot.lua alongside every
-- other modal-menu init — see uiManager.ensurePauseMenu).
-----------------------------------------------------------
function buildToolRemoteWarning.init(boxTex, bodyFont, titleFont, width, height)
    buildToolRemoteWarning.boxTexSet = boxTex
    buildToolRemoteWarning.bodyFont  = bodyFont
    buildToolRemoteWarning.titleFont = titleFont
    buildToolRemoteWarning.fbW = width
    buildToolRemoteWarning.fbH = height
end

local function destroyOwned()
    for _, id in ipairs(buildToolRemoteWarning.ownedLabels) do label.destroy(id) end
    for _, h  in ipairs(buildToolRemoteWarning.ownedBoxes)  do UI.deleteElement(h) end
    buildToolRemoteWarning.ownedLabels = {}
    buildToolRemoteWarning.ownedBoxes  = {}
    buildToolRemoteWarning.clickHandlers = {}
    buildToolRemoteWarning.buttonTextByBox = {}
    buildToolRemoteWarning.buttonFontSizeByBox = {}
end

local function createUI(distance, thresholdTiles)
    if buildToolRemoteWarning.page then
        UI.deletePage(buildToolRemoteWarning.page)
        destroyOwned()
    end

    local uiscale = scale.get()
    local s = scale.applyAll(baseSizes)

    buildToolRemoteWarning.page = UI.newPage("build_tool_remote_warning", "modal")

    local message = formatMessage(distance, thresholdTiles)

    local establishLabel = "Establish Here"
    local cancelLabel    = "Choose Another Site"
    local establishW = engine.getTextWidth(
        buildToolRemoteWarning.bodyFont, establishLabel, s.buttonFontSize)
        + s.buttonPaddingX
    local cancelW = engine.getTextWidth(
        buildToolRemoteWarning.bodyFont, cancelLabel, s.buttonFontSize)
        + s.buttonPaddingX
    local buttonsRowWidth = establishW + cancelW + s.buttonSpacing

    local msgW = engine.getTextWidth(
        buildToolRemoteWarning.bodyFont, message, s.bodyFontSize)
    local contentWidth = math.max(buttonsRowWidth, msgW)
    local panelWidth = math.max(PANEL_W_BASE, contentWidth + s.panelPaddingX * 2)

    local titleH = s.titleFontSize
    local msgH   = s.bodyFontSize
    local panelHeight = s.panelPaddingY * 2 + titleH + s.titleGap + msgH
        + s.messageGap + s.buttonHeight

    -- #750 round-6 review: cap against the actual framebuffer, same
    -- best-effort-degrade pattern as popup.lua/unit_info_v2.lua's
    -- earlier fixes for the identical class of gap. PANEL_W_BASE (560,
    -- deliberately NOT scaled by uiscale) plus the scaled button row/
    -- padding can still exceed a narrow, high-scale, still-C2-supported
    -- framebuffer (e.g. 800x2160@4x), leaving this modal's own
    -- Establish/Cancel buttons off-screen.
    panelWidth  = math.min(panelWidth, buildToolRemoteWarning.fbW)
    panelHeight = math.min(panelHeight, buildToolRemoteWarning.fbH)

    -- #750 round-7 review: capping the PANEL alone isn't enough — the
    -- Establish/Cancel button row's own width (establishW/cancelW,
    -- computed above from the UNCAPPED natural content) can still
    -- exceed the now-shrunk panel, leaving the buttons themselves
    -- extending past the panel (and framebuffer) edge even though the
    -- panel box itself fits. Shrink each button equally (never below a
    -- small floor) so the row fits the panel's actual content width —
    -- best-effort, may look cramped, but the buttons stay reachable.
    local naturalEstablishW = establishW
    local naturalCancelW = cancelW
    local availableButtonsW = panelWidth - 2 * s.panelPaddingX
    if buttonsRowWidth > availableButtonsW then
        local shrinkEach = (buttonsRowWidth - availableButtonsW) / 2
        establishW = math.max(20, establishW - shrinkEach)
        cancelW = math.max(20, cancelW - shrinkEach)
    end

    local panelX = (buildToolRemoteWarning.fbW - panelWidth) / 2
    local panelY = (buildToolRemoteWarning.fbH - panelHeight) / 2

    buildToolRemoteWarning.panelId = panel.new({
        name       = "build_tool_remote_warning_panel",
        page       = buildToolRemoteWarning.page,
        x = panelX, y = panelY,
        width      = panelWidth,
        height     = panelHeight,
        textureSet = buildToolRemoteWarning.boxTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = s.panelTileSize,
        overflow   = s.panelOverflow,
        zIndex     = 1,
        padding    = {
            top = s.panelPaddingY, bottom = s.panelPaddingY,
            left = s.panelPaddingX, right = s.panelPaddingX,
        },
        uiscale = 1.0,
    })

    local titleId = label.new({
        name     = "build_tool_remote_warning_title",
        text     = "Establish Colony Remotely?",
        font     = buildToolRemoteWarning.titleFont,
        fontSize = baseSizes.titleFontSize,
        color    = {1.0, 1.0, 1.0, 1.0},
        page     = buildToolRemoteWarning.page,
        uiscale  = uiscale,
    })
    table.insert(buildToolRemoteWarning.ownedLabels, titleId)
    panel.place(buildToolRemoteWarning.panelId, label.getElementHandle(titleId), {
        x = "50%", y = 0, origin = "top-center",
        width = 0, height = 0,
    })

    local msgId = label.new({
        name     = "build_tool_remote_warning_message",
        text     = message,
        font     = buildToolRemoteWarning.bodyFont,
        fontSize = baseSizes.bodyFontSize,
        color    = {0.85, 0.85, 0.85, 1.0},
        page     = buildToolRemoteWarning.page,
        uiscale  = uiscale,
    })
    table.insert(buildToolRemoteWarning.ownedLabels, msgId)
    panel.place(buildToolRemoteWarning.panelId, label.getElementHandle(msgId), {
        x = "50%", y = titleH + s.titleGap, origin = "top-center",
        width = 0, height = 0,
    })

    local buttonsY = titleH + s.titleGap + msgH + s.messageGap
    local baseZ = panel.getZIndex(buildToolRemoteWarning.panelId)

    -- #750 round-13 review: shrinking the button BOX alone left its
    -- child text rendering at the full, unshrunk s.buttonFontSize —
    -- with the shipped Press Start 2P font, "Choose Another Site" is
    -- wide enough at that size to render across (or off) an 800px-wide
    -- modal despite the click box itself staying in-frame. Scale each
    -- button's OWN font size by the ratio of its final (possibly
    -- shrunk) width to its natural (pre-shrink) width — mirrors the
    -- `labelUiscale = uiscale * shrink` technique
    -- cargo_inventory_panel.lua/build_tool.lua's picker tab labels
    -- already use for the identical class of gap (#750 round-12) —
    -- floored so text stays a nonzero, visible target.
    local function makeButton(name, text, width, naturalWidth, onClick)
        local boxH = UI.newBox(
            name .. "_box", width, s.buttonHeight,
            buildToolRemoteWarning.boxTexSet, s.buttonTileSize,
            1.0, 1.0, 1.0, 1.0, s.buttonOverflow,
            buildToolRemoteWarning.page
        )
        table.insert(buildToolRemoteWarning.ownedBoxes, boxH)
        UI.setClickable(boxH, true)
        UI.setOnClick(boxH, "onBuildToolRemoteWarningClick")
        buildToolRemoteWarning.clickHandlers[boxH] = onClick

        local fontScale = (naturalWidth > 0) and math.min(1.0, width / naturalWidth) or 1.0
        local buttonFontSize = math.max(6, math.floor(s.buttonFontSize * fontScale))
        local textH = UI.newText(
            name .. "_label", text,
            buildToolRemoteWarning.bodyFont, buttonFontSize,
            1.0, 1.0, 1.0, 1.0,
            buildToolRemoteWarning.page
        )
        local lblW = engine.getTextWidth(
            buildToolRemoteWarning.bodyFont, text, buttonFontSize)
        UI.addChild(boxH, textH, (width - lblW) / 2, (s.buttonHeight / 2) + (buttonFontSize / 2))
        UI.setZIndex(textH, 1)
        UI.setZIndex(boxH, baseZ + 1)
        buildToolRemoteWarning.buttonTextByBox[boxH] = textH
        buildToolRemoteWarning.buttonFontSizeByBox[boxH] = buttonFontSize
        return boxH, { width = width, height = s.buttonHeight }
    end

    local establishBox, establishSize = makeButton(
        "build_tool_remote_warning_establish", establishLabel, establishW, naturalEstablishW,
        function() buildToolRemoteWarning.establishHere() end)
    local cancelBox, cancelSize = makeButton(
        "build_tool_remote_warning_cancel", cancelLabel, cancelW, naturalCancelW,
        function() buildToolRemoteWarning.chooseAnotherSite() end)

    panel.placeRow(buildToolRemoteWarning.panelId,
        { establishBox, cancelBox },
        { establishSize, cancelSize },
        { x = "50%", y = buttonsY, origin = "top-center", spacing = s.buttonSpacing })
end

-----------------------------------------------------------
-- Click dispatch (called from uiManager.onBuildToolRemoteWarningClick)
-----------------------------------------------------------
function buildToolRemoteWarning.handleClick(elemHandle)
    local handler = buildToolRemoteWarning.clickHandlers[elemHandle]
    if handler then
        handler()
        return true
    end
    return false
end

-----------------------------------------------------------
-- Open / close
-----------------------------------------------------------

-- building.remoteCheck(defName, gx, gy) already resolved `remote` true
-- for this (defName, gx, gy); distance/thresholdTiles are display-only.
-- Repeated calls while already open are ignored — one click can't stack
-- a second modal or silently replace the pending tile.
function buildToolRemoteWarning.open(defName, gx, gy, distance, thresholdTiles)
    if buildToolRemoteWarning.pending then return end
    buildToolRemoteWarning.pending = {
        defName = defName, gx = gx, gy = gy,
        distance = distance, thresholdTiles = thresholdTiles,
        worldId = world.getActiveWorldId(),
    }
    createUI(distance, thresholdTiles)
    UI.showPage(buildToolRemoteWarning.page)
    debug.recordOutcome{
        kind = "buildTool.remoteWarning", outcome = "presented",
        where = { x = gx, y = gy },
    }
end

function buildToolRemoteWarning.isOpen()
    return buildToolRemoteWarning.pending ~= nil
end

-- Idempotent, no outcome recorded — the mechanical half of a close,
-- shared by the two player-facing actions below AND by the
-- view_teardown safety net (focus loss / page transitions must not
-- leave stale confirmation state behind, but a teardown sweep isn't a
-- player decision worth logging as canceled).
function buildToolRemoteWarning.closeIfOpen()
    if not buildToolRemoteWarning.pending then return end
    if buildToolRemoteWarning.page then
        UI.hidePage(buildToolRemoteWarning.page)
    end
    buildToolRemoteWarning.pending = nil
end

function buildToolRemoteWarning.chooseAnotherSite()
    local pending = buildToolRemoteWarning.pending
    if not pending then return end
    buildToolRemoteWarning.closeIfOpen()
    debug.recordOutcome{
        kind = "buildTool.remoteWarning", outcome = "canceled",
        where = { x = pending.gx, y = pending.gy },
    }
end

function buildToolRemoteWarning.establishHere()
    local pending = buildToolRemoteWarning.pending
    if not pending then return end
    buildToolRemoteWarning.closeIfOpen()
    debug.recordOutcome{
        kind = "buildTool.remoteWarning", outcome = "confirmed",
        where = { x = pending.gx, y = pending.gy },
    }
    -- Re-validate the SAVED tile now, not the state from when the
    -- warning opened — terrain/occupancy/location overlap/active-world
    -- state may have changed while the modal was up. building.canPlaceAt
    -- and commitStartingPlacement's building.spawn both resolve the
    -- CURRENTLY active world page implicitly, so if the active page
    -- changed while the modal was open, validating against it would
    -- silently spawn against the wrong page's terrain/locations instead
    -- of rejecting the stale confirmation — checked first, before
    -- canPlaceAt runs at all.
    if world.getActiveWorldId() ~= pending.worldId then
        debug.recordOutcome{
            kind = "buildTool.remoteWarning", outcome = "revalidationRejected",
            where = { x = pending.gx, y = pending.gy },
            reason = "active world changed",
        }
        return
    end
    local valid, invalidReason =
        building.canPlaceAt(pending.defName, pending.gx, pending.gy)
    if valid then
        require("scripts.build_tool").commitStartingPlacement(
            pending.defName, pending.gx, pending.gy)
    else
        debug.recordOutcome{
            kind = "buildTool.remoteWarning", outcome = "revalidationRejected",
            where = { x = pending.gx, y = pending.gy },
            reason = invalidReason or "invalid placement tile",
        }
        -- Build tool stays armed (exitPlacement was never called for
        -- the warning path) so the player can pick another tile.
    end
end

-----------------------------------------------------------
-- Escape dismiss (wired into init_keys.lua's cascade)
-----------------------------------------------------------
function buildToolRemoteWarning.handleKeyDown(key)
    if key == "Escape" and buildToolRemoteWarning.isOpen() then
        buildToolRemoteWarning.chooseAnotherSite()
        return true
    end
    return false
end

function buildToolRemoteWarning.onFramebufferResize(width, height)
    buildToolRemoteWarning.fbW = width
    buildToolRemoteWarning.fbH = height
    if buildToolRemoteWarning.pending then
        local pending = buildToolRemoteWarning.pending
        -- #750 round-13 review: createUI() deletes and recreates the
        -- whole page (including the Establish/Cancel boxes, both real
        -- keyboard-control-focusable elements per #745) — page deletion
        -- clears upmControlFocus with no restore, so a Tab-focused
        -- action silently lost focus on every resize/scale change.
        -- Same by-name snapshot/restore hud.lua's own onFramebufferResize
        -- fix (#750 round-10) already established, guarded on the page
        -- actually being visible beforehand (restoreControlFocusName
        -- searches UI.getVisibleElements(), which needs the rebuilt page
        -- shown again first).
        local wasVisible = buildToolRemoteWarning.page
            and UI.isPageVisible(buildToolRemoteWarning.page)
        local controlFocusName = wasVisible and responsive.snapshotControlFocusName()
        createUI(pending.distance, pending.thresholdTiles)
        UI.showPage(buildToolRemoteWarning.page)
        if wasVisible then
            responsive.restoreControlFocusName(controlFocusName)
        end
    end
end

function buildToolRemoteWarning.shutdown()
    destroyOwned()
    if buildToolRemoteWarning.page then
        UI.deletePage(buildToolRemoteWarning.page)
    end
    buildToolRemoteWarning.pending = nil
end

return buildToolRemoteWarning
