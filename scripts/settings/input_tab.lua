-- Input Tab — keybind editor
--
-- One row per rebindable action (curated {action,label} list). Each row
-- shows the action's friendly name, one settings-texture button per
-- currently-bound key, and a "+" button to add another key.
--
--   Move Up        [ Up ] [ W ] [+]
--   Rotate Clockwise  [ E ] [+]
--   …
--
-- Interaction (write-through, no Apply state — mirrors notifications_tab):
--   * Click a key button  → engine.removeActionKey + saveKeybinds, rebuild.
--   * Click "+"           → capture mode: the button shows its pressed
--                           texture and a "Press a key…" popup appears.
--                           The next onKeyDown (routed here through
--                           uiManager.onKeyDown → settingsMenu.onKeyCapture)
--                           appends that key (engine.addActionKey).
--   * Escape during capture cancels it (uiManager.onUIEscape →
--                           settingsMenu.cancelKeyCapture).
--   * Capturing a key already bound to another action opens a conflict
--     modal ("Key [KEY] is already bound to [ACTION]") with Cancel / Remap.
--
-- The keybind data lives in the engine (keyBindingsRef, persisted to
-- config/keybinds.yaml). This tab only reads engine.getKeybinds() and
-- edits through the add/remove/save API. Escape and Grave are engine-
-- reserved and the API refuses them, so they can never be captured here.

local label  = require("scripts.ui.label")
local button = require("scripts.ui.button")
local panel  = require("scripts.ui.panel")

local inputTab = {}

-----------------------------------------------------------
-- Curated action list (display order + friendly names)
-----------------------------------------------------------
inputTab.actions = {
    { action = "moveUp",         label = "Move Up" },
    { action = "moveDown",       label = "Move Down" },
    { action = "moveLeft",       label = "Move Left" },
    { action = "moveRight",      label = "Move Right" },
    { action = "rotateCCW",      label = "Rotate Counter-Clockwise" },
    { action = "rotateCW",       label = "Rotate Clockwise" },
    { action = "resetZTracking", label = "Reset Camera Height" },
    { action = "toggleEventLog", label = "Toggle Event Log" },
}

-- Actions the engine handles outside the binding table (mirror of
-- Engine.Input.Bindings.reservedActions) — never offered or treated as a
-- conflict source.
local RESERVED_ACTIONS = { escape = true, openShell = true }

-- Unscaled key/plus button geometry. Kept short so a row of a few keys
-- plus the "+" fits beside the action label.
local KEY_BTN_W  = 96
local PLUS_BTN_W = 40
local BTN_H      = 40
local BTN_FONT   = 20

-- z-index for the capture / conflict popups: above the bottom buttons
-- (Z_BUTTONS = 10) and scrollbars on the settings page.
local POPUP_Z = 30

-- Click-callback names for the modal layer. The backdrop sits just under
-- the popup (POPUP_Z - 1): clicking outside the dialog hits it and
-- cancels; clicking the dialog body hits the panel (swallow) so it does
-- nothing. Both block the tab bar / buttons beneath from being clicked
-- while a capture is in progress. Handled in ui_manager.
local BACKDROP_CALLBACK = "onKeybindPopupBackdrop"
local SWALLOW_CALLBACK  = "onKeybindPopupSwallow"

-----------------------------------------------------------
-- Module state (singleton via package.loaded)
-----------------------------------------------------------
-- ctx       : per-build context (page, textures, fonts, rebuild cb …)
-- capture   : { action, plusId } while waiting for a key press
-- conflict  : { action, key, oldAction } while the conflict modal is up
-- popup     : { panelIds, labelIds, buttonIds } currently-shown popup
inputTab.ctx      = nil
inputTab.capture  = nil
inputTab.conflict = nil
inputTab.popup    = nil
-- Row widget IDs (labels + key/plus buttons) for the current build. Owned
-- privately (not via the settings-menu global tracking) so the tab can be
-- torn down and rebuilt in place — see destroyWidgets — without disturbing
-- the other tabs' widgets.
inputTab.widgets  = { labelIds = {}, buttonIds = {} }

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

local function actionLabel(action)
    for _, def in ipairs(inputTab.actions) do
        if def.action == action then return def.label end
    end
    return action
end

-- True while a capture interaction is in progress (waiting for a key OR
-- the conflict modal is open). Gates onKeyDown routing + escape and makes
-- the row buttons behave modally.
function inputTab.captureActive()
    return inputTab.capture ~= nil or inputTab.conflict ~= nil
end

local function dismissPopups()
    local p = inputTab.popup
    if not p then return end
    for _, id in ipairs(p.buttonIds or {}) do button.destroy(id) end
    for _, id in ipairs(p.labelIds  or {}) do label.destroy(id)  end
    for _, id in ipairs(p.panelIds  or {}) do panel.destroy(id)  end
    -- Raw UI.newElement handles (the modal backdrop) — no widget wrapper.
    for _, h  in ipairs(p.rawIds    or {}) do UI.deleteElement(h)  end
    inputTab.popup = nil
end

-- Drop all capture state without touching engine UI handles. Called at
-- the start of every build: a rebuild deletes the old page (and thus any
-- popup elements), so the stale handles must simply be forgotten.
function inputTab.resetState()
    inputTab.capture  = nil
    inputTab.conflict = nil
    inputTab.popup    = nil
end

-- Destroy the row labels + key/plus buttons this tab created. Called from
-- the settings menu both for a full-page teardown (destroyOwned) and for
-- the in-place Input-tab refresh after a keybind change.
function inputTab.destroyWidgets()
    for _, id in ipairs(inputTab.widgets.buttonIds) do button.destroy(id) end
    for _, id in ipairs(inputTab.widgets.labelIds)  do label.destroy(id)  end
    inputTab.widgets = { labelIds = {}, buttonIds = {} }
end

-- Tear down popups + capture state on the current (still-live) page, then
-- trigger a full settings rebuild so the rows reflect the new bindings.
local function finishAndRebuild()
    inputTab.capture  = nil
    inputTab.conflict = nil
    dismissPopups()
    if inputTab.ctx and inputTab.ctx.rebuild then inputTab.ctx.rebuild() end
end

-----------------------------------------------------------
-- Popups
-----------------------------------------------------------

local function centeredPanel(name, w, h)
    local ctx = inputTab.ctx
    local us  = ctx.uiscale
    local pw  = math.floor(w * us)
    local ph  = math.floor(h * us)
    local px  = math.floor((ctx.fbW - pw) / 2)
    local py  = math.floor((ctx.fbH - ph) / 2)

    -- Full-screen invisible click blocker just under the popup, so the
    -- background controls can't be clicked while the dialog is up
    -- (UI.newElement has geometry but no texture → modal yet invisible).
    local backdrop = UI.newElement(name .. "_backdrop", ctx.fbW, ctx.fbH, ctx.page)
    UI.addToPage(ctx.page, backdrop, 0, 0)
    UI.setZIndex(backdrop, POPUP_Z - 1)
    UI.setClickable(backdrop, true)
    UI.setOnClick(backdrop, BACKDROP_CALLBACK)

    local pid = panel.new({
        name       = name,
        page       = ctx.page,
        x = px, y = py, width = pw, height = ph,
        textureSet = ctx.panelTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = 64,
        zIndex     = POPUP_Z,
        padding    = { top = 20, bottom = 20, left = 20, right = 20 },
        uiscale    = us,
    })
    -- Swallow clicks on the dialog body so they don't fall through to the
    -- backdrop (which would cancel).
    local box = panel.getBoxHandle(pid)
    if box then
        UI.setClickable(box, true)
        UI.setOnClick(box, SWALLOW_CALLBACK)
    end

    return pid, backdrop, px, py, pw, ph
end

local function popupLabel(text, cx, cy, color)
    local ctx = inputTab.ctx
    local id = label.new({
        name     = "keybind_popup_lbl",
        text     = text,
        font     = ctx.font,
        fontSize = ctx.baseSizes.fontSize,
        color    = color or {1.0, 1.0, 1.0, 1.0},
        page     = ctx.page,
        uiscale  = ctx.uiscale,
    })
    local h = label.getElementHandle(id)
    local w, _ = label.getSize(id)
    UI.addToPage(ctx.page, h, math.floor(cx - w / 2), cy)
    UI.setZIndex(h, POPUP_Z + 1)
    return id
end

-- "Press a key…" popup shown while a "+" capture is waiting.
local function showPressPopup(action)
    dismissPopups()
    local ctx = inputTab.ctx
    local pid, backdrop, px, py, pw, ph =
        centeredPanel("keybind_press_popup", 380, 130)
    local cx = px + pw / 2
    local l1 = popupLabel("Press a key for", cx, py + math.floor(40 * ctx.uiscale))
    local l2 = popupLabel(actionLabel(action), cx,
        py + math.floor(40 * ctx.uiscale) + ctx.baseSizes.fontSize
            + math.floor(8 * ctx.uiscale),
        {0.85, 0.85, 0.85, 1.0})
    inputTab.popup = { panelIds = { pid }, labelIds = { l1, l2 },
                       buttonIds = {}, rawIds = { backdrop } }
end

-- Conflict modal: key is already bound to another action.
local function showConflictPopup(action, key, oldAction)
    dismissPopups()
    local ctx = inputTab.ctx
    local us  = ctx.uiscale
    local pid, backdrop, px, py, pw, ph =
        centeredPanel("keybind_conflict_popup", 460, 200)
    local cx = px + pw / 2

    local l1 = popupLabel("Key \"" .. key .. "\" is already", cx,
        py + math.floor(36 * us))
    local l2 = popupLabel("bound to " .. actionLabel(oldAction), cx,
        py + math.floor(36 * us) + ctx.baseSizes.fontSize + math.floor(8 * us),
        {0.95, 0.85, 0.55, 1.0})

    -- Cancel / Remap buttons side by side near the bottom.
    local bw   = math.floor(150 * us)
    local bh   = math.floor(44 * us)
    local gap  = math.floor(16 * us)
    local by   = py + ph - bh - math.floor(20 * us)
    local startX = px + math.floor((pw - (bw * 2 + gap)) / 2)

    local cancelId = button.new({
        name = "keybind_conflict_cancel", text = "Cancel",
        width = 150, height = 44, fontSize = 24, uiscale = us,
        page = ctx.page, font = ctx.font, textureSet = ctx.buttonTexSet,
        bgColor = {1.0, 1.0, 1.0, 1.0}, textColor = {0.0, 0.0, 0.0, 1.0},
        zIndex = POPUP_Z + 1,
        onClick = function() inputTab.cancelCapture() end,
    })
    local remapId = button.new({
        name = "keybind_conflict_remap", text = "Remap",
        width = 150, height = 44, fontSize = 24, uiscale = us,
        page = ctx.page, font = ctx.font, textureSet = ctx.buttonTexSet,
        bgColor = {1.0, 1.0, 1.0, 1.0}, textColor = {0.0, 0.0, 0.0, 1.0},
        zIndex = POPUP_Z + 1,
        onClick = function()
            engine.removeActionKey(oldAction, key)
            engine.addActionKey(action, key)
            engine.saveKeybinds()
            finishAndRebuild()
        end,
    })
    button.setPosition(cancelId, startX, by)
    button.setPosition(remapId,  startX + bw + gap, by)

    inputTab.popup = {
        panelIds = { pid }, labelIds = { l1, l2 },
        buttonIds = { cancelId, remapId }, rawIds = { backdrop },
    }
end

-----------------------------------------------------------
-- Capture state machine (driven from uiManager via settingsMenu)
-----------------------------------------------------------

local function beginCapture(action, plusId)
    if inputTab.captureActive() then return end
    inputTab.capture = { action = action, plusId = plusId }
    button.setState(plusId, "clicked")
    showPressPopup(action)
end

-- Called for the next key press while waiting for a "+" capture. No-op
-- unless we are actually waiting (so stray keys during the conflict modal
-- are ignored). Escape never reaches here in practice (engine-reserved),
-- but is ignored defensively.
function inputTab.onKeyCapture(key)
    if not inputTab.capture then return end
    if key == "Escape" then return end

    local action = inputTab.capture.action
    if inputTab.capture.plusId then
        button.setState(inputTab.capture.plusId, "normal")
    end

    -- The onKeyDown string collapses left/right modifiers to a merged
    -- name ("Shift"); engine.getCurrentKeyName resolves the exact physical
    -- key (e.g. "LeftShift") so a captured key binds — and round-trips —
    -- to the precise key pressed. Falls back to the merged name outside a
    -- key-down dispatch.
    local bindKey = engine.getCurrentKeyName()
    if not bindKey or bindKey == "" then bindKey = key end

    -- Match against the engine's alias-aware resolver rather than raw
    -- string equality: this runs inside the onKeyDown dispatch, so
    -- engine.keyMatchesAction compares the exact physical key that was
    -- pressed (currentKeyDownRef) against each action's bindings, and
    -- correctly treats compatibility aliases (e.g. LeftShift vs the
    -- merged "Shift") as equal.

    -- Already bound to this action → nothing to do.
    if engine.keyMatchesAction(key, action) then
        inputTab.cancelCapture()
        return
    end

    -- Bound to another editable action → conflict modal.
    local owner = nil
    for _, def in ipairs(inputTab.actions) do
        if def.action ~= action and not RESERVED_ACTIONS[def.action]
           and engine.keyMatchesAction(key, def.action) then
            owner = def.action
            break
        end
    end
    if owner then
        inputTab.capture  = nil
        inputTab.conflict = { action = action, key = bindKey, oldAction = owner }
        showConflictPopup(action, bindKey, owner)
        return
    end

    -- Free key → bind and persist (the exact key pressed).
    engine.addActionKey(action, bindKey)
    engine.saveKeybinds()
    finishAndRebuild()
end

-- Cancel any in-progress capture (escape, or the conflict Cancel button).
-- Nothing changed, so no rebuild — just restore the "+" button and drop
-- the popup.
function inputTab.cancelCapture()
    if inputTab.capture and inputTab.capture.plusId then
        button.setState(inputTab.capture.plusId, "normal")
    end
    inputTab.capture  = nil
    inputTab.conflict = nil
    dismissPopups()
end

-- Remove one key from an action (clicking an existing key button).
local function removeKey(action, key)
    if inputTab.captureActive() then return end
    engine.removeActionKey(action, key)
    engine.saveKeybinds()
    if inputTab.ctx and inputTab.ctx.rebuild then inputTab.ctx.rebuild() end
end

-----------------------------------------------------------
-- Build
-----------------------------------------------------------

-- params (settings_menu tab create contract, plus keybind extras):
--   page, font, baseSizes, uiscale, s, contentX, contentY, contentW,
--   zContent, zWidgets, panelTexSet, buttonTexSet, fbW, fbH, rebuild
-- (This tab owns its widgets privately — see inputTab.widgets /
-- destroyWidgets — so it ignores the shared trackLabel/trackButton.)
-- Returns: rowHandles[]
function inputTab.create(params)
    -- A fresh build means a fresh page; forget any stale popup/capture
    -- handles (handles the resize-mid-capture case). The caller always
    -- destroys our previous widgets first (destroyOwned or the in-place
    -- refresh), so just start a clean tracking list.
    inputTab.resetState()
    inputTab.widgets = { labelIds = {}, buttonIds = {} }

    inputTab.ctx = {
        page         = params.page,
        font         = params.font,
        baseSizes    = params.baseSizes,
        uiscale      = params.uiscale,
        panelTexSet  = params.panelTexSet,
        buttonTexSet = params.buttonTexSet,
        fbW          = params.fbW,
        fbH          = params.fbH,
        rebuild      = params.rebuild,
    }

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

    -- Track our widgets privately so the tab can be rebuilt in place.
    local function trackLabel(id)
        table.insert(inputTab.widgets.labelIds, id); return id
    end
    local function trackButton(id)
        table.insert(inputTab.widgets.buttonIds, id); return id
    end

    local binds = engine.getKeybinds() or {}

    -- Buttons begin after a fixed label column.
    local labelColW = math.floor(cw * 0.42)
    local buttonsX  = cx + labelColW
    local gap       = math.floor(8 * uiscale)

    local rows = {}

    for i, def in ipairs(inputTab.actions) do
        local action = def.action
        local rowY0  = cy + s.rowSpacing * (i - 1)

        -- Action label (positioned/owned by the settings scroll model).
        local lblId = trackLabel(label.new({
            name     = "keybind_lbl_" .. action,
            text     = def.label,
            font     = font,
            fontSize = base.fontSize,
            color    = {1.0, 1.0, 1.0, 1.0},
            page     = page,
            uiscale  = uiscale,
        }))
        local lblHandle = label.getElementHandle(lblId)
        UI.addToPage(page, lblHandle, cx, rowY0 + s.fontSize)
        UI.setZIndex(lblHandle, zContent)

        -- One button per currently-bound key (click removes it), then "+".
        local rowButtonIds = {}
        local widgetHandles = {}

        for _, key in ipairs(binds[action] or {}) do
            local k = key  -- capture per-iteration for the closure
            local btnId = trackButton(button.new({
                name       = "keybind_" .. action .. "_" .. k,
                text       = k,
                width      = KEY_BTN_W, height = BTN_H, fontSize = BTN_FONT,
                uiscale    = uiscale,
                page       = page,
                font       = font,
                textureSet = inputTab.ctx.buttonTexSet,
                bgColor    = {1.0, 1.0, 1.0, 1.0},
                textColor  = {0.0, 0.0, 0.0, 1.0},
                zIndex     = zWidgets,
                onClick    = function() removeKey(action, k) end,
            }))
            table.insert(rowButtonIds, btnId)
            table.insert(widgetHandles, button.getElementHandle(btnId))
        end

        local plusId = trackButton(button.new({
            name       = "keybind_" .. action .. "_add",
            text       = "+",
            width      = PLUS_BTN_W, height = BTN_H, fontSize = BTN_FONT,
            uiscale    = uiscale,
            page       = page,
            font       = font,
            textureSet = inputTab.ctx.buttonTexSet,
            bgColor    = {1.0, 1.0, 1.0, 1.0},
            textColor  = {0.0, 0.0, 0.0, 1.0},
            zIndex     = zWidgets,
            onClick    = function() beginCapture(action, plusId) end,
        }))
        table.insert(rowButtonIds, plusId)
        table.insert(widgetHandles, button.getElementHandle(plusId))

        -- Lay the buttons out left-to-right at a given row top.
        local function layout(ry)
            local bx = buttonsX
            for _, bid in ipairs(rowButtonIds) do
                button.setPosition(bid, bx, ry)
                local bw, _ = button.getSize(bid)
                bx = bx + bw + gap
            end
        end
        layout(rowY0)

        table.insert(rows, {
            labelHandle   = lblHandle,
            widgetHandles = widgetHandles,
            widgetSetPosition = function(ry) layout(ry) end,
            widgetSetVisible  = function(vis)
                for _, bid in ipairs(rowButtonIds) do
                    button.setVisible(bid, vis)
                end
            end,
        })
    end

    return rows
end

return inputTab
