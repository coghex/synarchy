-- Etymology Panel (#1104, epic #708)
--
-- The ONE reusable gameplay surface that explains a generated name:
-- which morpheme means what, which realized spelling is a shortened
-- (bound) form of which free root, what grammatical marking was applied,
-- and where else on this world the same morpheme turns up.
--
-- ONE panel, three entry points. The world's own name, a discovered
-- location, and a selected river all call openFor(kind, id) and get this
-- same implementation over the same engine query (world.getEtymology) —
-- there are deliberately not three etymology views (#1104 requirement
-- 10). Everything entity-specific stopped at the engine adapter; from
-- here down a name is a name.
--
-- Read-only. Opening or scrolling this panel never writes a name, a
-- gloss, an etymology source, or a provenance (requirement 2). The panel
-- has no verb that could.
--
-- Honest absence. A name with no recoverable derivation renders its
-- stored text plus one plain sentence saying why there is none
-- (requirement 7). It never hides the name, guesses a language, or shows
-- part of a decomposition as though it had been validated — the engine
-- has already refused to hand over anything it could not prove explains
-- the stored name.
--
-- Public API:
--   openFor(kind, id)      -- "world" | "location" | "river"
--   closeIfOpen()          -- destroy the panel if shown
--   isOpen()               -- bool
--   reflow()               -- re-layout after hud geometry is current
--   onFramebufferResize(w, h)
--   handleKeyDown(key)     -- Escape closes
--   onScroll(elem, dx, dy) -- mouse wheel over the panel
--   handleScrollCallback(name, elem) -- its scrollbar's arrow buttons
--   dump()                 -- read-only introspection (F3-style)
--
-- Gameplay surface, NOT a responsive menu registry member: it is
-- reached through hud's manual forward and uiManager's gameplay rescale,
-- exactly like popup/unit_info_v2, so registering it with
-- responsive.register would double-fire it on every resize.
--
-- Registered in package.loaded so init.lua's key handler, hud setup, and
-- the name plate all see the same instance even though engine.loadScript
-- uses dofile.

local etymologyPanel = package.loaded["scripts.etymology_panel"] or {}
package.loaded["scripts.etymology_panel"] = etymologyPanel

local panel     = require("scripts.ui.panel")
local label     = require("scripts.ui.label")
local scale     = require("scripts.ui.scale")
local scrollbar = require("scripts.ui.scrollbar")
local textWrap  = require("scripts.ui.text_wrap")

-----------------------------------------------------------
-- Layout constants. Base units; uiscale applied at draw time.
-----------------------------------------------------------
local PANEL_W_BASE  = 440
local PANEL_PAD_X   = 32
local PANEL_PAD_TOP = 28
local PANEL_PAD_BOT = 20
local TITLE_FONT    = 16
local TITLE_H       = 22
local GLOSS_FONT    = 13
local GLOSS_H       = 18
local ROW_FONT      = 12
local ROW_H         = 16
local MAX_ROWS      = 14
local Z_PANEL       = 130
local Z_CONTENT     = 132

local TITLE_COL   = { 1.0, 1.0, 1.0, 1.0 }
local GLOSS_COL   = { 0.85, 0.85, 0.85, 1.0 }
local ROW_COL     = { 1.0, 1.0, 1.0, 1.0 }
local DIM_COL     = { 0.72, 0.72, 0.72, 1.0 }
local ABSENT_COL  = { 0.70, 0.70, 0.70, 1.0 }

-----------------------------------------------------------
-- State
-----------------------------------------------------------
etymologyPanel.state = etymologyPanel.state or {
    open         = false,
    kind         = nil,   -- "world" | "location" | "river"
    targetId     = nil,   -- instance id / feature id; nil for "world"
    scrollOffset = 0,
    data         = nil,   -- the last world.getEtymology reply
    panelId      = nil,
    titleId      = nil,
    glossId      = nil,
    viewportId   = nil,
    scrollbarId  = nil,
    rowIds       = {},
    rows         = {},    -- the flattened text rows currently described
}

etymologyPanel.hud = nil   -- assets set by setup()

-----------------------------------------------------------
-- HUD hookup
-----------------------------------------------------------
function etymologyPanel.setup(opts)
    etymologyPanel.hud = opts
end

-----------------------------------------------------------
-- Row construction
--
-- The engine hands back a validated decomposition; this turns it into
-- flat display rows. Nothing here re-derives meaning — every string
-- below is a field the engine already resolved, so the panel cannot
-- disagree with the query about what a morpheme means.
-----------------------------------------------------------
local function roleLabel(role)
    if role == "head"       then return "head"       end
    if role == "modifier"   then return "modifier"   end
    if role == "complement" then return "of"         end
    if role == "owner"      then return "possessor"  end
    return role or "?"
end

-- One morpheme's two lines: its realized spelling with its meaning, and
-- (only where it applies) the free form it is a shortening of plus the
-- grammatical marker that landed on it.
local function morphemeRows(m, out)
    out[#out + 1] = {
        text = string.format("%s  —  %s", m.surface or "?", m.lemma or "?"),
        color = ROW_COL,
    }
    local notes = {}
    notes[#notes + 1] = roleLabel(m.role)
    -- #1104 requirement 5: a bound form must read as one morpheme with
    -- its free form, never as an unrelated entry.
    if m.bound and m.free and m.free ~= "" then
        notes[#notes + 1] = string.format("bound form of %s", m.free)
    end
    if m.mark then
        notes[#notes + 1] = string.format("%s marked", m.mark)
        if m.markSurface and m.markSurface ~= "" then
            notes[#notes] = string.format("%s marked (-%s)",
                                          m.mark, m.markSurface)
        end
    end
    out[#out + 1] = { text = "    " .. table.concat(notes, ", "),
                      color = DIM_COL }
end

-- Recurrence rows for one morpheme: the eligible names that share its
-- identity. Only the entity's KIND and its already-visible stored name
-- appear (requirement 9) — no coordinates, ids, or lifecycle state.
local function recurrenceRows(data, concept, out)
    for _, link in ipairs(data.recurrence or {}) do
        if link.concept == concept then
            for _, e in ipairs(link.entries or {}) do
                out[#out + 1] = {
                    text = string.format("    also in %s: %s",
                                         e.kind or "?", e.name or "?"),
                    color = DIM_COL,
                }
            end
        end
    end
end

local function buildRows(data)
    local out = {}
    if not data or not data.available then
        out[#out + 1] = { text = "Etymology unavailable", color = ABSENT_COL }
        if data and data.reasonText and data.reasonText ~= "" then
            out[#out + 1] = { text = data.reasonText, color = ABSENT_COL,
                              wrap = true }
        end
        return out
    end
    for _, m in ipairs(data.morphemes or {}) do
        morphemeRows(m, out)
        recurrenceRows(data, m.concept, out)
    end
    return out
end

-- Wrap the rows that asked for it against the panel's real content
-- width. engine.getTextWidth measures 0 under the headless UI backend,
-- which makes every wrap a single line there; that is why the tests
-- assert geometry and clip state rather than line counts.
local function wrapRows(rows, contentW, font, fontSize)
    local out = {}
    for _, r in ipairs(rows) do
        if r.wrap then
            local lines = textWrap.byWord(r.text, contentW, font, fontSize)
            for _, ln in ipairs(lines) do
                out[#out + 1] = { text = ln, color = r.color }
            end
        else
            out[#out + 1] = r
        end
    end
    return out
end

-----------------------------------------------------------
-- Element teardown
-----------------------------------------------------------
local function destroyRows()
    local s = etymologyPanel.state
    for _, id in ipairs(s.rowIds) do label.destroy(id) end
    s.rowIds = {}
end

local function destroyAll()
    local s = etymologyPanel.state
    destroyRows()
    if s.titleId then label.destroy(s.titleId); s.titleId = nil end
    if s.glossId then label.destroy(s.glossId); s.glossId = nil end
    if s.scrollbarId then
        scrollbar.destroy(s.scrollbarId)
        s.scrollbarId = nil
    end
    s.scrollHandles = nil
    if s.viewportId then
        UI.deleteElement(s.viewportId)
        s.viewportId = nil
    end
    if s.panelId then panel.destroy(s.panelId); s.panelId = nil end
    -- s.rows is deliberately NOT cleared here: it is the row
    -- DESCRIPTION the layout is built FROM, not an element, and
    -- buildLayout() calls this to drop the previous elements before
    -- laying the same rows out again. Clearing it here would make every
    -- rebuild render an empty panel.
end

-----------------------------------------------------------
-- Layout
-----------------------------------------------------------
local function renderRows(contentX, contentY, contentW, visibleRows, uiscale)
    local s = etymologyPanel.state
    local h = etymologyPanel.hud
    destroyRows()
    local rowH = math.floor(ROW_H * uiscale)
    local maxOffset = math.max(0, #s.rows - visibleRows)
    if s.scrollOffset > maxOffset then s.scrollOffset = maxOffset end
    if s.scrollOffset < 0 then s.scrollOffset = 0 end
    for i = 1, visibleRows do
        local r = s.rows[i + s.scrollOffset]
        if r then
            local id = label.new({
                name     = "etymology_row_" .. i,
                text     = r.text,
                font     = h.menuFont,
                fontSize = ROW_FONT,
                color    = r.color,
                page     = h.page,
                uiscale  = uiscale,
            })
            local eh = label.getElementHandle(id)
            -- Rows are children of the clipping viewport, so overflowing
            -- content is clipped rather than painted over the HUD
            -- (#747). The viewport itself stays at zIndex 0 because
            -- zIndex ACCUMULATES through the parent chain.
            UI.addChild(s.viewportId, eh, 0,
                        (i - 1) * rowH + math.floor(ROW_FONT * 0.85))
            UI.setZIndex(eh, Z_CONTENT)
            s.rowIds[#s.rowIds + 1] = id
        end
    end
end

local function buildLayout()
    local s = etymologyPanel.state
    local h = etymologyPanel.hud
    if not h or not h.page then return end
    -- A minimized framebuffer has no geometry worth building against;
    -- rebuilding here would produce degenerate (or negative) bounds that
    -- a later real resize would have to undo (#1104 requirement 11's
    -- 0x0 no-op, the same rule every other gameplay surface follows).
    if not h.fbW or not h.fbH or h.fbW <= 0 or h.fbH <= 0 then return end

    local uiscale = scale.get()
    local panelW  = math.floor(PANEL_W_BASE * uiscale)
    local padTop  = math.floor(PANEL_PAD_TOP * uiscale)
    local padBot  = math.floor(PANEL_PAD_BOT * uiscale)
    local titleH  = math.floor(TITLE_H * uiscale)
    local glossH  = math.floor(GLOSS_H * uiscale)
    local rowH    = math.floor(ROW_H * uiscale)

    -- Cap to the framebuffer: BASE * uiscale can exceed a narrow,
    -- high-scale but still-supported combination, and the position
    -- clamp below only ever repositions, never shrinks.
    panelW = math.min(panelW, h.fbW)

    -- How many rows can actually be SHOWN. MAX_ROWS is the taste cap,
    -- but the framebuffer is the hard one: a short screen (or a high UI
    -- scale) caps the panel's height, and a row count derived only from
    -- the content would then paint more rows than the box can hold. So
    -- the fitting count is computed from the height that is really
    -- available and the content scrolls instead — requirement 11's
    -- "clip or scroll content that exceeds its bounds", applied to the
    -- bound the screen imposes rather than only to the one the content
    -- asks for.
    local chromeH  = padTop + titleH + glossH + 8 + padBot
    local fitRows  = math.floor(math.max(0, h.fbH - chromeH) / rowH)
    local visibleRows = math.max(1, math.min(#s.rows, MAX_ROWS, fitRows))
    local rowsH  = visibleRows * rowH
    local panelH = math.min(chromeH + rowsH, h.fbH)

    -- Anchored to the top-right, clear of the left-side toolbar
    -- clusters; clamped so it never opens partly off-screen.
    local px = math.max(0, h.fbW - panelW - math.floor(16 * uiscale))
    local py = math.floor(16 * uiscale)
    if py + panelH > h.fbH then py = math.max(0, h.fbH - panelH) end

    destroyAll()
    s.panelId = panel.new({
        name       = "etymology_panel",
        page       = h.page,
        x          = px,
        y          = py,
        width      = panelW,
        height     = panelH,
        textureSet = h.boxTexSet,
        color      = { 0.1, 0.1, 0.1, 0.95 },
        tileSize   = 64,
        zIndex     = Z_PANEL,
        padding    = { top = PANEL_PAD_TOP, bottom = PANEL_PAD_BOT,
                       left = PANEL_PAD_X,  right  = PANEL_PAD_X },
        uiscale    = uiscale,
    })
    -- Content geometry derives from the panel's REAL bounds, never an
    -- independently recomputed value that could drift from it.
    -- #744/#747: declare the panel box a scroll-CAPTURING surface, so
    -- routeScroll picks it as the topmost in-scope capturing element and
    -- delivers the wheel to uiManager.onUIScroll (and on to this
    -- module's own onScroll) instead of zooming the world behind it.
    -- Pointer-blocking too, for the same reason combat_log declares
    -- both: blank panel space must not leak clicks through to the world
    -- underneath a read-only overlay.
    local boxHandle = panel.getBoxHandle(s.panelId)
    if boxHandle then
        UI.setScrollCapture(boxHandle, true)
        UI.setPointerBlocking(boxHandle, true)
    end

    local pb = panel.getContentBounds(s.panelId)
    local cx, cy, cw = px + pb.x, py + pb.y, pb.width

    local data = s.data or {}

    -- The AUTHORITATIVE stored name, echoed exactly as the engine
    -- reported it. Shown in every state, including the unavailable one:
    -- the panel never hides the name it failed to explain.
    s.titleId = label.new({
        name = "etymology_title", text = data.name or "",
        font = h.menuFont, fontSize = TITLE_FONT, color = TITLE_COL,
        page = h.page, uiscale = uiscale,
    })
    local th = label.getElementHandle(s.titleId)
    UI.addToPage(h.page, th, cx, cy + math.floor(TITLE_FONT * 0.85))
    UI.setZIndex(th, Z_CONTENT)

    s.glossId = label.new({
        name = "etymology_gloss", text = data.gloss or "",
        font = h.menuFont, fontSize = GLOSS_FONT, color = GLOSS_COL,
        page = h.page, uiscale = uiscale,
    })
    local gh = label.getElementHandle(s.glossId)
    UI.addToPage(h.page, gh, cx, cy + titleH + math.floor(GLOSS_FONT * 0.85))
    UI.setZIndex(gh, Z_CONTENT)

    local contentY = cy + titleH + glossH + 8
    local contentH = visibleRows * rowH
    s.viewportId = UI.newElement("etymology_viewport", cw, contentH, h.page)
    UI.addToPage(h.page, s.viewportId, cx, contentY)
    UI.setClipChildren(s.viewportId, true)

    if #s.rows > visibleRows then
        local sbButton = math.floor(20 * uiscale)
        local sbCap    = math.floor(4 * uiscale)
        s.scrollbarId = scrollbar.new({
            name         = "etymology_scrollbar",
            page         = h.page,
            x            = cx + cw + math.floor(6 * uiscale),
            y            = contentY,
            buttonSize   = sbButton,
            trackHeight  = math.max(20, contentH - 2 * sbButton - 2 * sbCap),
            capHeight    = sbCap,
            tileSize     = math.floor(8 * uiscale),
            totalItems   = #s.rows,
            visibleItems = visibleRows,
            uiscale      = uiscale,
            zIndex       = Z_CONTENT,
            onScroll     = function(offset)
                s.scrollOffset = offset
                renderRows(cx, contentY, cw, visibleRows, uiscale)
            end,
        })
        scrollbar.setScrollOffset(s.scrollbarId, s.scrollOffset)
        s.scrollHandles = scrollbar.getElementHandles(s.scrollbarId)
    end

    s.visibleRows = visibleRows
    s.contentX, s.contentY, s.contentW = cx, contentY, cw
    s.uiscale = uiscale
    renderRows(cx, contentY, cw, visibleRows, uiscale)
end

-- Re-query and re-lay-out for the currently inspected target. The
-- INSPECTED TARGET and the scroll offset both survive, which is what
-- makes a geometry rebuild a layout change rather than a semantic
-- re-entry (#1104 requirement 11).
local function refresh()
    local s = etymologyPanel.state
    if not s.open or not s.kind then return end
    local ok, data
    if s.targetId ~= nil then
        ok, data = pcall(world.getEtymology, s.kind, s.targetId)
    else
        ok, data = pcall(world.getEtymology, s.kind)
    end
    s.data = ok and data or nil
    local h = etymologyPanel.hud
    local font = h and h.menuFont
    local width = s.contentW or math.floor(PANEL_W_BASE * scale.get())
    s.rows = wrapRows(buildRows(s.data), width, font, ROW_FONT)
    buildLayout()
end

-----------------------------------------------------------
-- Open / close
-----------------------------------------------------------

-- Open the panel on one entity. `kind` is "world", "location", or
-- "river"; `id` is the location's instance id or the river's feature id
-- (omitted for the world, which is the page itself).
--
-- A new request always tears down the prior view first (singleton), so a
-- stale target never lingers behind a new one.
function etymologyPanel.openFor(kind, id)
    if kind ~= "world" and kind ~= "location" and kind ~= "river" then
        return false
    end
    etymologyPanel.closeIfOpen()
    local s = etymologyPanel.state
    s.open         = true
    s.kind         = kind
    s.targetId     = id
    -- A genuine fresh open starts at the top; a resize (which calls
    -- refresh() directly) deliberately does not reset this.
    s.scrollOffset = 0
    refresh()
    return true
end

function etymologyPanel.closeIfOpen()
    local s = etymologyPanel.state
    if not s.open then
        -- Still idempotent about ELEMENTS: a failed open could have left
        -- some behind without ever setting open.
        destroyAll()
        s.rows = {}
        return
    end
    destroyAll()
    s.open     = false
    s.kind     = nil
    s.targetId = nil
    s.data     = nil
    s.rows     = {}
end

function etymologyPanel.isOpen()
    return etymologyPanel.state.open == true
end

-- What the panel is currently explaining, for the name plate's toggle
-- and for the resize snapshot.
function etymologyPanel.currentTarget()
    local s = etymologyPanel.state
    if not s.open then return nil end
    return s.kind, s.targetId
end

-----------------------------------------------------------
-- Lifecycle
-----------------------------------------------------------

-- Called by hud after IT has rebuilt, so this never reads stale hud
-- geometry (the same ordering popup/unit_info_v2 follow). Preserves the
-- inspected target and the scroll offset across the rebuild.
function etymologyPanel.reflow()
    local s = etymologyPanel.state
    if not s.open then return end
    refresh()
end

-- A real framebuffer resize. Stores nothing itself — hud owns the
-- geometry and forwards reflow() once its own rebuild is done — but
-- guards the 0x0 minimize case explicitly so a degenerate size can never
-- reach buildLayout through this path either.
function etymologyPanel.onFramebufferResize(width, height)
    if not width or not height or width <= 0 or height <= 0 then return end
    etymologyPanel.reflow()
end

-- Escape closes. Named handle* (not on*) deliberately: this module is
-- engine-loaded, so an on*-named function would fire on every engine
-- broadcast.
function etymologyPanel.handleKeyDown(key)
    if key == "Escape" and etymologyPanel.state.open then
        etymologyPanel.closeIfOpen()
        return true
    end
    return false
end

-- Move the view by one row, re-rendering in place. Shared by both input
-- routes below so the wheel and the scrollbar's arrows can never scroll
-- by different amounts or leave the scrollbar's own thumb out of step
-- with the rows.
local function scrollBy(step)
    local s = etymologyPanel.state
    if not s.open or not s.scrollbarId then return false end
    local maxOffset = math.max(0, #s.rows - (s.visibleRows or 1))
    local next = math.max(0, math.min(maxOffset, s.scrollOffset + step))
    if next ~= s.scrollOffset then
        s.scrollOffset = next
        scrollbar.setScrollOffset(s.scrollbarId, next)
        renderRows(s.contentX, s.contentY, s.contentW, s.visibleRows or 1,
                   s.uiscale or scale.get())
    end
    -- True even when the offset did not move: the event was OURS (the
    -- cursor was over this panel), and letting it fall through would
    -- zoom the world out from under an open panel at either end of the
    -- list.
    return true
end

-- Is this element part of the panel's own scroll surface? The panel box
-- itself and its scrollbar both count, mirroring event_log's rule, so a
-- wheel anywhere over the panel scrolls it rather than the world.
local function ownsScrollElement(elemHandle)
    local s = etymologyPanel.state
    if not s.open or not s.scrollbarId then return false end
    if s.panelId and panel.getBoxHandle(s.panelId) == elemHandle then
        return true
    end
    return scrollbar.findByElementHandle(elemHandle) == s.scrollbarId
end

-- Mouse WHEEL over the panel (uiManager.onUIScroll). Returns true when
-- it consumed the event.
function etymologyPanel.onScroll(elemHandle, dx, dy)
    if not ownsScrollElement(elemHandle) then return false end
    if (dy or 0) > 0 then return scrollBy(-1) end
    if (dy or 0) < 0 then return scrollBy(1) end
    return true
end

-- The scrollbar's own up/down ARROW buttons
-- (uiManager.onScrollUp / onScrollDown).
function etymologyPanel.handleScrollCallback(callbackName, elemHandle)
    local s = etymologyPanel.state
    if not s.open or not s.scrollbarId then return false end
    if scrollbar.findByElementHandle(elemHandle) ~= s.scrollbarId then
        return false
    end
    if callbackName == "onScrollUp"   then return scrollBy(-1) end
    if callbackName == "onScrollDown" then return scrollBy(1) end
    return false
end

-----------------------------------------------------------
-- Read-only introspection (the repository's F3-style dump convention,
-- same shape previewManager.dump() and scripts/ui/list.lua use). This is
-- what tools/etymology_probe.py asserts populated content through, so a
-- probe never needs screenshot OCR or a hardcoded coordinate.
-----------------------------------------------------------
function etymologyPanel.dump()
    local s = etymologyPanel.state
    local out = {
        open         = s.open == true,
        kind         = s.kind,
        targetId     = s.targetId,
        scrollOffset = s.scrollOffset,
        rowCount     = #s.rows,
        visibleRows  = s.visibleRows,
    }
    local d = s.data
    if d then
        out.available  = d.available == true
        out.name       = d.name
        out.gloss      = d.gloss
        out.form       = d.form
        out.reason     = d.reason
        out.reasonText = d.reasonText
        out.language   = d.language
        out.morphemes  = {}
        for _, m in ipairs(d.morphemes or {}) do
            out.morphemes[#out.morphemes + 1] = {
                concept = m.concept, role = m.role, surface = m.surface,
                free = m.free, bound = m.bound == true, lemma = m.lemma,
                mark = m.mark, markSurface = m.markSurface, id = m.id,
            }
        end
        -- The surface tokens, so a probe can verify for itself that they
        -- concatenate back to the stored name rather than trusting the
        -- engine's own claim that they do.
        out.tokens = {}
        for _, t in ipairs(d.tokens or {}) do
            out.tokens[#out.tokens + 1] = { kind = t.kind, text = t.text }
        end
        out.recurrence = {}
        for _, r in ipairs(d.recurrence or {}) do
            local entries = {}
            for _, e in ipairs(r.entries or {}) do
                entries[#entries + 1] = { kind = e.kind, name = e.name }
            end
            out.recurrence[#out.recurrence + 1] =
                { concept = r.concept, morpheme = r.morpheme,
                  entries = entries }
        end
    end
    -- Per-visible-row interactive bounds + handles, so a probe can click
    -- or scroll a located row without a hardcoded screen coordinate.
    out.rows = {}
    for i, id in ipairs(s.rowIds) do
        local eh = label.getElementHandle(id)
        local info = eh and UI.getElementInfo(eh)
        out.rows[i] = {
            text = label.getText(id), handle = eh,
            x = info and info.x, y = info and info.y,
            width = info and info.width, height = info and info.height,
        }
    end
    if s.panelId then
        local b = panel.getContentBounds(s.panelId)
        out.contentBounds = b
    end
    out.viewport = s.viewportId
    out.clipsChildren = s.viewportId ~= nil
    -- The scroll controls' own live handles, so a probe or a test can
    -- drive the wheel and the arrows at real elements rather than at
    -- guessed coordinates (same reason the row bounds are here).
    out.scrollbar = s.scrollbarId
    out.scrollHandles = s.scrollHandles or {}
    if s.panelId then out.box = panel.getBoxHandle(s.panelId) end
    return out
end

-----------------------------------------------------------
-- Engine script hooks
-----------------------------------------------------------
function etymologyPanel.init(scriptId)
    engine.logDebug("Etymology panel initializing...")
end

function etymologyPanel.shutdown()
    etymologyPanel.closeIfOpen()
    engine.logDebug("Etymology panel shut down")
end

return etymologyPanel
