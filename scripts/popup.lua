-- Popup
--
-- Player-events popup module. Renders modal-layer popups that
-- accumulate event entries by category — within a category's
-- coalesce_window, repeated events fold into the same popup with
-- (xN) counters; outside the window, new events become new lines in
-- the same popup (up to popup.maxLines, oldest dropped). Each line
-- is clickable: pans the camera to the line's first stored coord,
-- subsequent clicks cycle through additional coords until the
-- player moves the camera (then the cycle resets).
--
-- One bottom button: OK, dismisses the popup. Navigation is handled
-- entirely by clicking a popup line (pans the camera to that event's
-- coords) — there is no per-category "go_to" button.

local scale    = require("scripts.ui.scale")
local panel    = require("scripts.ui.panel")
local label    = require("scripts.ui.label")
local button   = require("scripts.ui.button")
local utf8Safe = require("scripts.ui.utf8_safe")

local popup = package.loaded["scripts.popup"] or {}
package.loaded["scripts.popup"] = popup

-----------------------------------------------------------
-- Module state
-----------------------------------------------------------

popup.pageId        = popup.pageId        or nil
popup.boxTexSet     = popup.boxTexSet     or nil
popup.btnTexSet     = popup.btnTexSet     or nil
popup.font          = popup.font          or nil
popup.fbW           = popup.fbW           or 0
popup.fbH           = popup.fbH           or 0
popup.bootstrapped  = popup.bootstrapped  or false

-- Mute-toggle icon textures (a checkbox sitting in the title bar, to
-- the left of the X close button). Loaded lazily in bootstrap. If
-- either file is missing, renderPopup silently skips the icon — the
-- rest of the popup still works.
popup.texPopupEnabled  = popup.texPopupEnabled  or nil
popup.texPopupDisabled = popup.texPopupDisabled or nil

-- Reverse lookup: mute-toggle sprite handle → popup record. Populated
-- when the sprite is created in renderPopup; cleared in destroyVisuals.
-- Read by popup.onMuteToggleClick to resolve which popup was clicked.
popup.muteToggleByHandle = popup.muteToggleByHandle or {}

popup.queue   = popup.queue  or {}  -- pending entries waiting for a slot
popup.active  = popup.active or {}  -- currently-rendered popups

-- Per-category active popup: cat → popup record. Used for the
-- coalescing fast-path so we can find the existing popup to fold a
-- new event into. Cleared on dismiss (only if the entry still
-- points to the dismissed popup — defensive against tab-switches
-- in concurrent flows).
popup.activeByCategory = popup.activeByCategory or {}

-- Reverse map from click-box element handle → { popup, lineIdx }.
-- Populated when a line's overlay is created in renderPopup. Read
-- by popup.onLineClick to resolve which line/popup was clicked.
popup.lineByClickBox = popup.lineByClickBox or {}

popup.slotCount = 6
popup.maxActive = 6
popup.maxQueue  = 50   -- pending-queue backpressure cap
popup.maxLines  = 10   -- per-popup line cap; oldest line dropped beyond this

popup.baseSizes = {
    minWidth      = 360,
    padX          = 36,    -- outer horizontal padding inside panel
    padY          = 28,    -- outer vertical padding inside panel
    fontSize      = 20,
    titleSize     = 22,    -- title bar font (slightly larger than rows)
    lineSpacing   = 4,
    rowHeight     = 28,    -- one logical line in a coalesced popup
    headerH       = 36,    -- title bar height (title text + X button)
    headerGap     = 16,    -- gap between title bar and lines block
    footerGap     = 16,    -- gap between lines block and OK button
    closeBtnSize  = 36,    -- small X button (top-right)
    closeBtnFont  = 18,    -- X glyph size (smaller than body so it centres)
    buttonMinW    = 80,
    buttonH       = 36,
    buttonFont    = 16,
    buttonPadX    = 24,
    slotOffset    = 28,
    tileSize      = 64,
}

-- Popups can be wider than the legacy text-only popup since each
-- coalesced line is a single visual row; we never wrap event text.
-- The truncation cap keeps very long messages from blowing the
-- panel out across the screen.
popup.maxWidthFrac = 0.55

-- Click callback name routed by ui_manager.onPopupLineClick.
local LINE_CLICK_CALLBACK = "onPopupLineClick"

-- Click callback name for the mute-toggle icon, routed by
-- ui_manager.onPopupMuteToggleClick.
local MUTE_TOGGLE_CALLBACK = "onPopupMuteToggleClick"

-- Mute-toggle icon texture paths. The checkbox flips the category's
-- persistent `popup` notification setting via setNotificationOverrides
-- (same backend as the settings tab). Loaded in bootstrap; nil-safe.
local POPUP_TEX_ENABLED  = "assets/textures/ui/hud/popup_enabled.png"
local POPUP_TEX_DISABLED = "assets/textures/ui/hud/popup_disabled.png"

-----------------------------------------------------------
-- Forward declarations
-----------------------------------------------------------

local dismiss
local drainQueue
local renderPopup
local destroyVisuals
local foldEventIntoPopup
local spawnFromEntry

-----------------------------------------------------------
-- Slot picker
-----------------------------------------------------------

local function pickSlot()
    local counts = {}
    for _, p in ipairs(popup.active) do
        counts[p.slot] = (counts[p.slot] or 0) + 1
    end
    local bestSlot, bestCount = 1, counts[1] or 0
    for s = 2, popup.slotCount do
        local n = counts[s] or 0
        if n < bestCount then
            bestSlot, bestCount = s, n
        end
    end
    return bestSlot
end

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- Find a category's cfg by id. Returns nil if not registered.
local function findCategoryCfg(category)
    local cfg = engine.getNotificationCfg() or {}
    for _, c in ipairs(cfg) do
        if c.id == category then return c end
    end
    return nil
end

-- Format an elapsed-time double (seconds) as "M:SS" or "H:MM:SS".
local function formatElapsed(seconds)
    local s = math.floor((seconds or 0) + 0.5)
    if s < 0 then s = 0 end
    local h = math.floor(s / 3600)
    local m = math.floor((s % 3600) / 60)
    local sec = s % 60
    if h > 0 then
        return string.format("%d:%02d:%02d", h, m, sec)
    else
        return string.format("%d:%02d", m, sec)
    end
end

-- Compose the displayed text for one line: "M:SS text" or "M:SS
-- text (xN)" when N > 1. Timestamp is elapsed time since the
-- popup was first spawned (matches the user's mental model of
-- "ages since this popup opened" rather than wall clock).
local function lineDisplayText(p, line)
    local stamp = formatElapsed(line.firstEventTime - p.spawnTime)
    if line.count > 1 then
        return stamp .. " " .. (line.text or "")
            .. " (x" .. line.count .. ")"
    else
        return stamp .. " " .. (line.text or "")
    end
end

-- Truncate text to fit maxWidthPx, suffixing "..." (pixel-accurate
-- binary search using engine.getTextWidth). Every candidate cut point is
-- snapped to a complete UTF-8 character boundary (utf8Safe) so a
-- multi-byte character (e.g. an accented letter) is never split into a
-- dangling lead byte -- string.sub cuts by byte offset, not codepoint.
local function truncateToWidth(text, font, fontSize, maxWidthPx)
    if engine.getTextWidth(font, text, fontSize) <= maxWidthPx then
        return text
    end
    local lo, hi = 1, #text
    while lo < hi do
        local mid = math.floor((lo + hi + 1) / 2)
        local cut = utf8Safe.snapToCharBoundary(text, mid)
        local candidate = string.sub(text, 1, cut) .. "..."
        if engine.getTextWidth(font, candidate, fontSize) <= maxWidthPx then
            lo = mid
        else
            hi = mid - 1
        end
    end
    return string.sub(text, 1, utf8Safe.snapToCharBoundary(text, lo)) .. "..."
end

-----------------------------------------------------------
-- Render: destroy + rebuild every line/label/button from p.lines.
-- Called fresh each time a popup spawns OR an event folds into it.
-- Re-renders are cheap at typical line counts (1-10).
-----------------------------------------------------------

destroyVisuals = function(p)
    if p.okBtnId    then button.destroy(p.okBtnId);    p.okBtnId    = nil end
    if p.closeBtnId then button.destroy(p.closeBtnId); p.closeBtnId = nil end
    if p.muteToggleId then
        popup.muteToggleByHandle[p.muteToggleId] = nil
        UI.deleteElement(p.muteToggleId)
        p.muteToggleId = nil
    end
    if p.titleLblId then label.destroy(p.titleLblId);  p.titleLblId = nil end
    for _, line in ipairs(p.lines) do
        if line.labelId then
            label.destroy(line.labelId); line.labelId = nil
        end
        if line.clickBoxHandle then
            popup.lineByClickBox[line.clickBoxHandle] = nil
            UI.deleteElement(line.clickBoxHandle)
            line.clickBoxHandle = nil
        end
    end
    if p.panelId then panel.destroy(p.panelId); p.panelId = nil end
end

renderPopup = function(p)
    destroyVisuals(p)

    local uiscale = scale.get()
    local s = scale.applyAllWith(popup.baseSizes, uiscale)
    local rowH = s.rowHeight
    -- #750 round-19 review: the label's uiscale for its row (see the
    -- line-render loop below) — starts at the outer uiscale and only
    -- shrinks below it when the line block itself needs fitting.
    local lineUiscale = uiscale

    -- Compose + truncate per-line text; track max width for panel sizing.
    -- Reserve room for the X button on the right so titles never run
    -- under it. The line area uses the full inner width.
    local maxPanelW = math.floor(popup.fbW * popup.maxWidthFrac)
    local maxTextW  = maxPanelW - 2 * s.padX
    if maxTextW < 100 then maxTextW = 100 end

    local displayTexts = {}
    local maxLineW = 0
    for i, line in ipairs(p.lines) do
        local txt = lineDisplayText(p, line)
        local truncated = truncateToWidth(txt, popup.font, s.fontSize, maxTextW)
        displayTexts[i] = truncated
        local w = engine.getTextWidth(popup.font, truncated, s.fontSize)
        if w > maxLineW then maxLineW = w end
    end

    -- Title width: display name fits if we leave room for the X
    -- button (closeBtnSize + small gap) on the right of the title.
    -- When the mute-toggle icon is available, reserve room for it too
    -- (sits to the left of the X with a small gap).
    local titleRawW = engine.getTextWidth(popup.font, p.displayName,
                                          s.titleSize)
    local hasMuteToggle = popup.texPopupEnabled and popup.texPopupDisabled
    local muteToggleGap = math.floor(6 * uiscale)
    local muteToggleReserved = hasMuteToggle
        and (s.closeBtnSize + muteToggleGap) or 0
    local titleReservedRight = muteToggleReserved + s.closeBtnSize
                             + math.floor(12 * uiscale)
    local titleNeededW = titleRawW + titleReservedRight

    local okText = "OK"
    local okTextW = engine.getTextWidth(popup.font, okText, s.buttonFont)
    local btnW = math.max(s.buttonMinW, okTextW + 2 * s.buttonPadX)

    local contentW = math.max(maxLineW, btnW, titleNeededW)
    local panelW = math.max(s.minWidth, contentW + 2 * s.padX)

    -- #750 round-19 review: a max-lines (10) popup's natural line-block
    -- height alone can exceed a narrow, high-scale, still-C2-supported
    -- framebuffer (e.g. 800x1601@4x needs 1760px total) — the round-3
    -- panelH cap below only shrinks the PANEL, leaving rows laid out at
    -- their full natural rowH, so the last rows collide with the OK
    -- button (moved up to fit the capped panel) instead of landing
    -- inside it. Fit a local, line-block-only scale (rowH + its label's
    -- uiscale together, never the panel's other fixed chrome) so every
    -- row — and the OK button below it — stays inside the capped panel,
    -- the same fitScale technique used throughout this codebase for an
    -- analogous "fixed content doesn't fit the available space" gap.
    local responsive = require("scripts.ui.responsive")
    local fixedChromeH = 2 * s.padY + s.headerH + s.headerGap
                        + s.footerGap + s.buttonH
    local availLinesH = math.max(s.rowHeight, popup.fbH - fixedChromeH)
    local lineScale = responsive.fitScale(#p.lines * rowH, availLinesH, uiscale)
    if lineScale < uiscale then
        rowH = math.max(1, math.floor(popup.baseSizes.rowHeight * lineScale))
        lineUiscale = lineScale
    end

    local linesH = #p.lines * rowH
    local panelH = s.padY + s.headerH + s.headerGap
                   + linesH + s.footerGap + s.buttonH + s.padY
    -- #750 round-3 review: cap against the actual framebuffer.
    -- s.minWidth alone (scaled by uiscale) can exceed a narrow/high-scale
    -- but still C2-supported framebuffer (e.g. 800x2160@4x scales
    -- minWidth to 1440px), pushing the close/OK buttons off-screen
    -- regardless of avoidReserved's position clamp below (which only
    -- ever moves x/y, never shrinks w/h). Best-effort degrade, same
    -- spirit as settings_menu's own tabFrameHeight floor for its
    -- out-of-envelope exemplar (CLAUDE.md's C2 section).
    panelW = math.min(panelW, popup.fbW)
    panelH = math.min(panelH, popup.fbH)

    -- Lazily required to avoid a load cycle (hud.lua doesn't require
    -- popup.lua either way, but every other cross-module reach in this
    -- file follows the same lazy convention).
    local reservedRegions = require("scripts.ui.reserved_regions")
    local hud = require("scripts.hud")
    local toolbarRects = hud.getToolbarRects()

    -- #750 round-4 review: the framebuffer-width cap above isn't enough
    -- when a reserved region spans nearly the full height at the card's
    -- OWN vertical position (e.g. the tool toggle column) — no amount of
    -- x-nudging then fits a wide card beside it, so avoidReserved's
    -- position-only nudge below can end up right back on top of it once
    -- clamped to the screen. Shrink panelW to the widest horizontal gap
    -- actually free at this card's (pre-nudge, vertically centered) y —
    -- best-effort, same "may look cramped, never off-screen/unreachable"
    -- contract as the framebuffer cap above.
    local cy0 = math.floor((popup.fbH - panelH) / 2)
    local availW = reservedRegions.maxAvailableWidth(
        cy0, panelH, toolbarRects, popup.fbW)
    if availW > 0 then
        -- #750 round-14 review: a flat 20px floor (matching
        -- settings_menu's own defensive floor for an analogous
        -- out-of-envelope exemplar) is enough to keep panelW itself
        -- positive, but NOT enough to keep it USABLE — every content
        -- position below (the line click box's `panelW - 2*s.padX`,
        -- the title/OK button placement) assumes panelW comfortably
        -- exceeds the padding alone. At the issue's own 800x2160@4x, a
        -- card overlapping BOTH toolbar clusters can leave availW as
        -- small as their free gap (e.g. 64px) — well under 2*s.padX
        -- (288px at that scale) — which drove the click box negative
        -- and pushed the title/OK button outside the panel box
        -- entirely.
        --
        -- #750 round-16 review: a flat "padding + 20px" floor still
        -- wasn't enough — it kept panelW positive, but never accounted
        -- for the PANEL'S OWN FIXED CHROME that has to fit inside it:
        -- the OK button alone needs at least s.buttonMinW (320px at
        -- 4x), and when the mute-toggle icon exists it sits beside the
        -- close X, needing its own strip of width. Floor at whichever
        -- the panel's fixed content actually needs — 2*s.padX plus the
        -- WIDER of the OK button's own width or the close+mute icon
        -- strip's width — so both always land inside the panel
        -- regardless of how tight availW gets (the panel may still
        -- overlap a reserved region in a genuinely infeasible case,
        -- same "best-effort, never crashing/invalid" priority the rest
        -- of this contract already uses — but its own OK/close/mute
        -- controls never spill outside the panel box itself).
        local closeAndMuteW = muteToggleReserved + s.closeBtnSize
        local minUsableW = 2 * s.padX + math.max(btnW, closeAndMuteW, 20)
        panelW = math.min(panelW, math.max(minUsableW, availW))
    end

    -- Centre + slot diagonal offset
    local cx = math.floor((popup.fbW - panelW) / 2)
    local cy = math.floor((popup.fbH - panelH) / 2)
    local off = (p.slot - 1) * s.slotOffset
    local px, py = cx + off, cy + off

    -- #750: nudge the card off the always-reachable toolbar clusters
    -- instead of covering one — a small window + a corner-anchored
    -- toggle group + a centered/cascading popup can genuinely overlap.
    local nudged = reservedRegions.avoidReserved(
        { x = px, y = py, w = panelW, h = panelH },
        toolbarRects, popup.fbW, popup.fbH)
    px, py = nudged.x, nudged.y

    local baseZ = p.baseZ

    p.panelId = panel.new({
        name       = "popup_panel_" .. tostring(p.id),
        page       = popup.pageId,
        x          = px,
        y          = py,
        width      = panelW,
        height     = panelH,
        textureSet = popup.boxTexSet,
        color      = {1.0, 1.0, 1.0, 1.0},
        tileSize   = popup.baseSizes.tileSize,
        zIndex     = baseZ,
        padding    = { top    = popup.baseSizes.padY,
                       bottom = popup.baseSizes.padY,
                       left   = popup.baseSizes.padX,
                       right  = popup.baseSizes.padX },
        uiscale    = uiscale,
    })

    -- Title (top-left, in category color)
    local titleLbl = label.new({
        name     = "popup_title_" .. tostring(p.id),
        text     = p.displayName,
        font     = popup.font,
        fontSize = popup.baseSizes.titleSize,
        color    = p.color,
        page     = popup.pageId,
        uiscale  = uiscale,
    })
    UI.addToPage(popup.pageId, label.getElementHandle(titleLbl),
        px + s.padX, py + s.padY + s.titleSize)
    UI.setZIndex(label.getElementHandle(titleLbl), baseZ + 2)
    p.titleLblId = titleLbl

    -- X close button (top-right). Dismisses the popup, same as OK.
    -- A smaller font than the body keeps the glyph visually centered
    -- inside the box — at 1:1 button.lua's 9-box can leave too
    -- little vertical headroom for a body-size glyph.
    p.closeBtnId = button.new({
        name       = "popup_close_" .. tostring(p.id),
        page       = popup.pageId,
        x          = px + panelW - s.padX - s.closeBtnSize,
        y          = py + s.padY,
        width      = popup.baseSizes.closeBtnSize,
        height     = popup.baseSizes.closeBtnSize,
        fontSize   = popup.baseSizes.closeBtnFont,
        font       = popup.font,
        text       = "X",
        textColor  = {1.0, 1.0, 1.0, 1.0},
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textureSet = popup.btnTexSet or popup.boxTexSet,
        onClick    = function() dismiss(p) end,
        uiscale    = uiscale,
        zIndex     = baseZ + 2,
    })

    -- Mute-toggle icon (sits to the left of the X close button, same
    -- size + Y). Acts as a checkbox for the category's `popup`
    -- notification setting: clicking flips the persistent flag via
    -- setNotificationOverrides (same backend as the settings tab) and
    -- swaps the sprite texture to reflect the new state. The popup
    -- itself stays open after a toggle — the player closes it with
    -- OK or X. If either icon texture is missing the whole control
    -- is skipped (renderable popup still works during asset bring-up).
    if hasMuteToggle then
        local catCfg0 = findCategoryCfg(p.category)
        local popupOn = catCfg0 and catCfg0.popup or false
        local muteTex = popupOn
            and popup.texPopupEnabled or popup.texPopupDisabled
        local muteX = px + panelW - s.padX - s.closeBtnSize
                    - muteToggleGap - s.closeBtnSize
        local muteY = py + s.padY
        p.muteToggleId = UI.newSprite(
            "popup_mute_toggle_" .. tostring(p.id),
            s.closeBtnSize, s.closeBtnSize,
            muteTex,
            1.0, 1.0, 1.0, 1.0,
            popup.pageId)
        UI.addToPage(popup.pageId, p.muteToggleId, muteX, muteY)
        UI.setClickable(p.muteToggleId, true)
        UI.setOnClick(p.muteToggleId, MUTE_TOGGLE_CALLBACK)
        UI.setZIndex(p.muteToggleId, baseZ + 2)
        UI.setTooltip(p.muteToggleId, "Pause on notification")
        popup.muteToggleByHandle[p.muteToggleId] = p
    end

    -- Lines start below the title bar + headerGap.
    local linesStartY = py + s.padY + s.headerH + s.headerGap

    for i, line in ipairs(p.lines) do
        local rowY = linesStartY + (i - 1) * rowH

        -- Transparent click overlay (full row width inside padding)
        local clickBox = UI.newBox(
            "popup_line_box_" .. tostring(p.id) .. "_" .. tostring(i),
            panelW - 2 * s.padX, rowH,
            popup.boxTexSet,
            popup.baseSizes.tileSize,
            1.0, 1.0, 1.0, 0.0,
            0,
            popup.pageId)
        UI.addToPage(popup.pageId, clickBox, px + s.padX, rowY)
        UI.setClickable(clickBox, true)
        UI.setOnClick(clickBox, LINE_CLICK_CALLBACK)
        -- Click box must be z-ABOVE the label so the engine's
        -- hit-test returns it instead of the (non-clickable but
        -- still topmost) label. labels are not clickable so visually
        -- they still render normally — the box is transparent.
        UI.setZIndex(clickBox, baseZ + 3)
        line.clickBoxHandle = clickBox
        popup.lineByClickBox[clickBox] = { popup = p, lineIdx = i }

        -- Text label (z below click box; visible since clickBox is
        -- alpha=0 but engine hit-tests pick the topmost element)
        local lbl = label.new({
            name     = "popup_line_lbl_" .. tostring(p.id) .. "_" .. tostring(i),
            text     = displayTexts[i],
            font     = popup.font,
            fontSize = popup.baseSizes.fontSize,
            color    = p.color,
            page     = popup.pageId,
            uiscale  = lineUiscale,
        })
        -- #750 round-19 review: this baseline nudge used to add the
        -- UNSCALED base fontSize (always 20px) regardless of uiscale —
        -- at 4x the label rendered 60px too high, its glyphs bleeding
        -- into the row above (and its higher-z click box) instead of
        -- sitting inside its own row. Uses the SAME scale the label
        -- itself renders at (lineUiscale, above) so the nudge always
        -- matches the actual glyph size.
        UI.addToPage(popup.pageId, label.getElementHandle(lbl),
            px + s.padX, rowY + math.floor(popup.baseSizes.fontSize * lineUiscale))
        UI.setZIndex(label.getElementHandle(lbl), baseZ + 2)
        line.labelId = lbl
    end

    -- Bottom OK button
    local btnX = px + math.floor((panelW - btnW) / 2)
    local btnY = py + panelH - s.padY - s.buttonH
    p.okBtnId = button.new({
        name       = "popup_ok_" .. tostring(p.id),
        page       = popup.pageId,
        x          = btnX,
        y          = btnY,
        width      = btnW,
        height     = s.buttonH,
        fontSize   = s.buttonFont,
        font       = popup.font,
        text       = okText,
        textColor  = {1.0, 1.0, 1.0, 1.0},
        bgColor    = {1.0, 1.0, 1.0, 1.0},
        textureSet = popup.btnTexSet or popup.boxTexSet,
        onClick    = function() dismiss(p) end,
        uiscale    = 1.0,
        zIndex     = baseZ + 2,
    })
end

-----------------------------------------------------------
-- Coalescing logic
-----------------------------------------------------------

-- Fold a new event into an existing popup. If within the
-- category's coalesce window of the LAST line's last update, bump
-- that line's count + replace its text with the latest. Otherwise
-- append a new line (capping at popup.maxLines, oldest dropped).
foldEventIntoPopup = function(p, entry, window, now)
    local lastLine = p.lines[#p.lines]
    if lastLine
       and (now - lastLine.lastUpdateTime) < window then
        -- Same burst: bump existing line.
        lastLine.count = lastLine.count + 1
        lastLine.text = entry.text
        if entry.coords then
            table.insert(lastLine.coords, entry.coords)
        end
        lastLine.lastUpdateTime = now
        -- A new event invalidates any previous click-cycle the
        -- player did against this line — they'll want to restart
        -- from the new tail. Cheap to reset; user can always click
        -- to advance.
        lastLine.cycleIdx = 0
        lastLine.lastPannedX = nil
        lastLine.lastPannedY = nil
    else
        -- New burst (or never a last line): add a fresh row.
        local newLine = {
            text          = entry.text,
            count         = 1,
            coords        = (entry.coords and {entry.coords}) or {},
            cycleIdx      = 0,
            lastPannedX   = nil,
            lastPannedY   = nil,
            firstEventTime = now,
            lastUpdateTime = now,
            labelId       = nil,
            clickBoxHandle = nil,
        }
        table.insert(p.lines, newLine)
        -- Cap: drop the oldest line if we exceed maxLines.
        while #p.lines > popup.maxLines do
            local dropped = table.remove(p.lines, 1)
            if dropped.labelId then label.destroy(dropped.labelId) end
            if dropped.clickBoxHandle then
                popup.lineByClickBox[dropped.clickBoxHandle] = nil
                UI.deleteElement(dropped.clickBoxHandle)
            end
        end
    end
    renderPopup(p)
end

-----------------------------------------------------------
-- Spawn / dismiss
-----------------------------------------------------------

local nextPopupId = 1

spawnFromEntry = function(entry)
    local slot = pickSlot()
    local baseZ = 1000 + #popup.active * 10
    local id = nextPopupId; nextPopupId = nextPopupId + 1
    local now = engine.realTime()

    -- Look up the category's display name for the title bar. Use
    -- the id as a fallback if the category isn't in the registry
    -- (shouldn't happen for emitted events, but defensive).
    local catCfg = findCategoryCfg(entry.category)
    local displayName = (catCfg and catCfg.displayName) or entry.category

    local p = {
        id           = id,
        category     = entry.category,
        displayName  = displayName,
        slot         = slot,
        baseZ        = baseZ,
        color        = entry.color,
        spawnTime    = now,
        lines        = {},
        okBtnId      = nil,
        closeBtnId   = nil,
        muteToggleId = nil,
        titleLblId   = nil,
        panelId      = nil,
    }

    local initialLine = {
        text          = entry.text,
        count         = 1,
        coords        = (entry.coords and {entry.coords}) or {},
        cycleIdx      = 0,
        lastPannedX   = nil,
        lastPannedY   = nil,
        firstEventTime = now,
        lastUpdateTime = now,
        labelId       = nil,
        clickBoxHandle = nil,
    }
    table.insert(p.lines, initialLine)

    table.insert(popup.active, p)
    popup.activeByCategory[p.category] = p

    renderPopup(p)
end

dismiss = function(p)
    if not p then return end
    for i, x in ipairs(popup.active) do
        if x == p then table.remove(popup.active, i); break end
    end
    if popup.activeByCategory[p.category] == p then
        popup.activeByCategory[p.category] = nil
    end
    destroyVisuals(p)
    drainQueue()
end

drainQueue = function()
    if not popup.bootstrapped then return end
    while #popup.queue > 0 and #popup.active < popup.maxActive do
        local entry = table.remove(popup.queue, 1)
        -- The fold-vs-spawn decision could change between when the
        -- entry was queued and when it drains (if another popup of
        -- the same category was dismissed in the meantime), so we
        -- re-check activeByCategory here at drain time, not at
        -- enqueue.
        local catCfg = findCategoryCfg(entry.category)
        local window = (catCfg and catCfg.coalesceWindow) or 0
        local active = popup.activeByCategory[entry.category]
        if window > 0 and active then
            foldEventIntoPopup(active, entry, window, engine.realTime())
        else
            spawnFromEntry(entry)
        end
    end
end

-----------------------------------------------------------
-- Line-click handler — camera cycle through this line's coords
-----------------------------------------------------------

function popup.onLineClick(elemHandle)
    local entry = popup.lineByClickBox[elemHandle]
    if not entry then return false end
    local p     = entry.popup
    local line  = p.lines[entry.lineIdx]
    -- Lines without coords (events emitted via engine.emitEvent
    -- rather than engine.emitEventAt) are no-op on click — the
    -- line text still renders, just nothing to jump to.
    if not line or #line.coords == 0 then return true end

    -- If the camera is sitting where we last panned it, advance to
    -- the next coord. If it's moved (player WASD/mouse), reset to
    -- coord 1 — matches the player's intuition: "I moved, start
    -- the cycle over".
    local ok, cx, cy = pcall(camera.getPosition)
    if not ok then return true end

    local function approxEq(a, b)
        return math.abs((a or 0) - (b or 0)) < 0.5
    end

    if line.lastPannedX
       and approxEq(cx, line.lastPannedX)
       and approxEq(cy, line.lastPannedY) then
        -- Camera unchanged; advance modulo
        line.cycleIdx = (line.cycleIdx % #line.coords) + 1
    else
        -- Camera moved (or first click); restart
        line.cycleIdx = 1
    end

    local target = line.coords[line.cycleIdx]
    pcall(function()
        camera.goToTile(target.x, target.y)
    end)
    -- Snapshot the post-pan camera position so the next click can
    -- decide cycle-vs-reset.
    local ok2, nx, ny = pcall(camera.getPosition)
    if ok2 then
        line.lastPannedX = nx
        line.lastPannedY = ny
    end
    return true
end

-----------------------------------------------------------
-- Mute-toggle click handler — flips the category's `popup` setting
-----------------------------------------------------------

-- Called by ui_manager.onPopupMuteToggleClick when the in-title-bar
-- checkbox icon is clicked. Looks up the popup via the reverse map,
-- toggles the category's `popup` notification setting through the
-- existing setNotificationOverrides path (which writes through to
-- config/notifications.local.yaml on the next persist), and swaps the
-- sprite texture to reflect the new state. The popup stays open so
-- the player can still read or interact with the current event.
function popup.onMuteToggleClick(elemHandle)
    local p = popup.muteToggleByHandle[elemHandle]
    if not p then return false end
    local catCfg = findCategoryCfg(p.category)
    local currentlyEnabled = (catCfg and catCfg.popup) or false
    local newValue = not currentlyEnabled
    local overrides = {}
    overrides[p.category] = { popup = newValue }
    engine.setNotificationOverrides(overrides)
    local newTex = newValue
        and popup.texPopupEnabled or popup.texPopupDisabled
    if newTex then
        UI.setSpriteTexture(elemHandle, newTex)
    end
    return true
end

-----------------------------------------------------------
-- Engine broadcast handler
-----------------------------------------------------------

function popup.onShowPopup(category, text, r, g, b, a, coords)
    local entry = {
        category = category,
        text     = text,
        color    = { r or 1.0, g or 1.0, b or 1.0, a or 1.0 },
        coords   = coords,    -- nil or {x, y}
    }
    -- Coalesce path: if this category has an active popup AND the
    -- category opts in via coalesce_window > 0, fold the new event
    -- into the existing popup rather than queueing a fresh one.
    local catCfg = findCategoryCfg(category)
    local window = (catCfg and catCfg.coalesceWindow) or 0
    if window > 0 and popup.bootstrapped
       and popup.activeByCategory[category] then
        foldEventIntoPopup(popup.activeByCategory[category],
                           entry, window, engine.realTime())
        return
    end

    -- Otherwise the entry takes its place in the queue and waits
    -- for a slot.
    table.insert(popup.queue, entry)
    while #popup.queue > popup.maxQueue do
        table.remove(popup.queue, 1)
    end
    drainQueue()
end

-----------------------------------------------------------
-- Lifecycle
-----------------------------------------------------------

function popup.init(scriptId)
    if popup.pageId == nil then
        popup.pageId = UI.newPage("popups", "modal")
        -- #742: "modal" here is stacking only (baseZ per active card,
        -- see below) — notification cards are pass-through outside
        -- their own X/OK buttons, never a global modal blocker. A
        -- LayerModal page defaults input-exclusive, so opt back out
        -- explicitly.
        UI.setPageInputExclusive(popup.pageId, false)
    end
end

function popup.bootstrap(boxTex, btnTex, font, w, h)
    popup.boxTexSet = boxTex
    popup.btnTexSet = btnTex
    popup.font      = font
    popup.fbW       = w
    popup.fbH       = h
    -- Load the mute-toggle icon textures once. If either file is
    -- missing engine.loadTexture returns 0 (or a nil-equivalent); the
    -- renderPopup branch is gated on both being truthy, so a missing
    -- asset just means the icon is skipped.
    if not popup.texPopupEnabled then
        popup.texPopupEnabled = engine.loadTexture(POPUP_TEX_ENABLED)
    end
    if not popup.texPopupDisabled then
        popup.texPopupDisabled = engine.loadTexture(POPUP_TEX_DISABLED)
    end
    if popup.pageId == nil then
        popup.pageId = UI.newPage("popups", "modal")
        UI.setPageInputExclusive(popup.pageId, false)
    end
    UI.showPage(popup.pageId)
    popup.bootstrapped = true
    drainQueue()
end

function popup.update(dt) end

function popup.onFramebufferResize(width, height)
    -- #750: a 0x0 minimize must not become the stored geometry — a new
    -- notification created before the next real resize would size itself
    -- against a degenerate framebuffer (maxPanelW = floor(fbW*0.55) = 0).
    -- Keep the last valid fbW/fbH instead.
    if width <= 0 or height <= 0 then return end
    popup.fbW = width
    popup.fbH = height
    -- #750: reflow deliberately does NOT happen here. popup.lua is
    -- engine.loadScript'd with an EARLIER script id than
    -- scripts/ui_manager.lua, so the engine's automatic
    -- broadcastToModules calls this before uiManager.onFramebufferResize
    -- has run hud.onFramebufferResize — reflowing now would nudge cards
    -- against hud.getToolbarRects() from the STALE, pre-resize HUD.
    -- popup.reflow() is the real entry point, called explicitly by
    -- ui_manager_boot.lua's manual forward (after hud.onFramebufferResize)
    -- and by uiManager.notifyGameplayRescale (which already calls hud
    -- first), guaranteeing the toolbar rects it reads are current.
end

-- #750: reflow every active card so it stays centered, correctly scaled,
-- and reachable (its OK button on-screen) at the current geometry — a
-- card positioned against a stale fbW/fbH, or nudged against a stale
-- hud.getToolbarRects(), could otherwise render stale or fully
-- off-screen, or overlap the NEW toolbar position, after a real shrink
-- or a UI-scale change. renderPopup fully rebuilds a card's visuals from
-- its own record (the same function content updates already reuse, e.g.
-- a new coalesced line) — it never touches p.lines/category/target data,
-- only the on-screen position/size.
function popup.reflow()
    for _, p in ipairs(popup.active) do
        renderPopup(p)
    end
end

function popup.shutdown()
    for _, p in ipairs(popup.active) do
        destroyVisuals(p)
    end
    popup.active = {}
    popup.queue  = {}
    popup.activeByCategory = {}
    popup.lineByClickBox   = {}
    if popup.pageId then
        UI.deletePage(popup.pageId)
        popup.pageId = nil
    end
    popup.bootstrapped = false
end

-----------------------------------------------------------
-- Test / debug helpers (used by tools/test_lua_save_api.sh)
-----------------------------------------------------------

function popup.queueLength()
    return #popup.queue
end

function popup.activeCount()
    return #popup.active
end

function popup.isBootstrapped()
    return popup.bootstrapped and true or false
end

-- #750: real on-screen bounds of every currently-rendered card, for the
-- reserved-region introspection audit (and tests) — never a re-derived
-- estimate, since renderPopup already nudges px/py via
-- reserved_regions.avoidReserved before creating the panel.
function popup.getActiveBounds()
    local panelMod = require("scripts.ui.panel")
    local rects = {}
    for _, p in ipairs(popup.active) do
        if p.panelId then
            local x, y = panelMod.getPosition(p.panelId)
            local w, h = panelMod.getSize(p.panelId)
            if x then
                table.insert(rects, { id = p.id, x = x, y = y, w = w, h = h })
            end
        end
    end
    return rects
end

-- Number of lines in the active popup for a category, or 0 if no
-- active popup. Used by smoke tests to confirm coalescing.
function popup.activeLineCount(category)
    local p = popup.activeByCategory[category]
    if not p then return 0 end
    return #p.lines
end

-- Most recent line's count for a category's active popup, or 0
-- if no active popup.
function popup.activeLastLineCount(category)
    local p = popup.activeByCategory[category]
    if not p or #p.lines == 0 then return 0 end
    return p.lines[#p.lines].count
end

-----------------------------------------------------------
-- Keyboard dismissal helpers (called from init.lua Escape cascade)
-----------------------------------------------------------

function popup.dismissTopmost()
    if #popup.active == 0 then return false end
    dismiss(popup.active[#popup.active])
    return true
end

function popup.dismissAll()
    if #popup.active == 0 then return 0 end
    local snapshot = {}
    for i = 1, #popup.active do snapshot[i] = popup.active[i] end
    popup.queue = {}
    for _, p in ipairs(snapshot) do dismiss(p) end
    return #snapshot
end

return popup
