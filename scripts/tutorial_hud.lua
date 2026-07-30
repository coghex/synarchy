-- Tutorial objective CHECKLIST HUD (#960, phase 3 of the tutorial epic
-- #956). #957 defined the tree, #958 owns progress and the view model,
-- #959 evaluates it; this module is the only thing that DRAWS any of
-- it, and it draws nothing else.
--
-- It is a gameplay HUD surface, not a settings screen, a modal, or an
-- interactive quest log. Three consequences shape the whole file:
--
--   * Everything rendered comes from `tutorialProgress.getViewModel()`.
--     This module never calls `engine.getTutorialTree()` and never
--     touches the tutorial registry: reveal/hide is #958's rule, stated
--     once, and duplicating it here is exactly the drift that split the
--     two slices apart. It also means the hspec gate can drive real
--     rendering by injecting a tree through `tutorialProgress.setTree`.
--   * Rows are DISPLAY-ONLY. They carry a tooltip and capture the
--     wheel, and that is all: no click callback and no pointer
--     blocking, so a click over the checklist reaches the terrain
--     underneath (#743's three input policies are independent —
--     src/UI/Manager/Query.hs's elementBlocksPointer /
--     elementCapturesScroll). Only the toggle is a real control.
--   * `completed` and `checked` are NEVER collapsed into one
--     "checkbox". A full/composite objective LATCHES (durable,
--     monotonic); a subobjective CHECKS (live, reversible, recomputed
--     every session). The view model reports exactly one of them per
--     row and so does this module's rendering (`[x]` vs `(x)`) and its
--     dump().
--
-- Presentation state (open/closed + scroll offset) lives here on the
-- package.loaded singleton and is deliberately NOT persisted -- but a
-- Lua singleton outlives a save load and a mid-process world
-- generation, so both entry points reset it explicitly:
-- `onSaveLoaded` (the one broadcast every load trigger reaches, menu-
-- driven or console) and `worldManager.createWorld` (the one funnel a
-- newly generated world goes through, which already resets #958's
-- progress for the same reason). Evaluation keeps running while the
-- panel is closed -- scripts/tutorial_eval.lua ticks independently of
-- anything here.

local tutorialHud = package.loaded["scripts.tutorial_hud"] or {}
package.loaded["scripts.tutorial_hud"] = tutorialHud

local scale           = require("scripts.ui.scale")
local reservedRegions = require("scripts.ui.reserved_regions")

-- Presentation state. Survives a rebuild, a resize, a UI-scale change,
-- a HUD hide/show and a zoom-band change; reset only by the two
-- session-entry hooks at the bottom of this file.
tutorialHud.open         = tutorialHud.open         or false
tutorialHud.scrollOffset = tutorialHud.scrollOffset or 0

tutorialHud.page = tutorialHud.page or nil
tutorialHud.fbW  = tutorialHud.fbW  or 0
tutorialHud.fbH  = tutorialHud.fbH  or 0

-- Requirement 7 introspection: how many times the surface has been
-- rebuilt from scratch. A real framebuffer resize must bump this by
-- exactly ONE across the whole two-step path below -- the engine
-- broadcast records the new size, and the single rebuild happens in
-- the post-hud `reflow()`.
tutorialHud.rebuildCount = tutorialHud.rebuildCount or 0

-- Live element bookkeeping (handles are invalid after a rebuild).
tutorialHud._els        = tutorialHud._els        or {}
tutorialHud._rows       = tutorialHud._rows       or {}
tutorialHud._toggle     = tutorialHud._toggle     or nil
tutorialHud._hudVisible = tutorialHud._hudVisible or false
tutorialHud._sig        = tutorialHud._sig        or nil
tutorialHud._toggleLabel  = tutorialHud._toggleLabel  or nil
tutorialHud._assetsReady  = tutorialHud._assetsReady  or false

local TOGGLE_CALLBACK = "onTutorialHudToggle"

-- Unscaled layout constants. Every runtime size is one of these times
-- the live UI scale -- never a measured text width: `engine.getTextWidth`
-- reports 0 in the bare-Lua headless UI fixture, so a measurement-driven
-- row height or scroll range would be untestable there.
local BASE = {
    panelW   = 240,
    rowH     = 20,
    fontSize = 14,
    indent   = 12,
    margin   = 8,
    gap      = 4,
    toggleW  = 132,
    toggleH  = 22,
}

-- The gameplay HUD, if it has been required at all. Read through
-- package.loaded rather than require() so this module never forces
-- scripts/hud.lua to load (world_manager.lua reaches us from its
-- createWorld funnel, which must not drag the HUD in behind it).
local function hudModule()
    return package.loaded["scripts.hud"]
end

-- Toolbar clusters currently on screen. hud.getToolbarRects() is the
-- authoritative list (log / map / tool); an empty result simply means
-- the toolbar does not exist yet, which is not an error here.
local function toolbarRects()
    local hudMod = hudModule()
    if hudMod and type(hudMod.getToolbarRects) == "function" then
        local ok, rects = pcall(hudMod.getToolbarRects)
        if ok and type(rects) == "table" then return rects end
    end
    return {}
end

-----------------------------------------------------------
-- Geometry
-----------------------------------------------------------

-- Right-anchored geometry for a given framebuffer and active-row count.
--
-- The toggle rect depends ONLY on the framebuffer, the UI scale, and
-- the toolbar clusters -- never on the open/closed state and never on
-- the row count. That is what makes "small reachable right-edge
-- toggle" verifiable: its bounds are assertably identical open and
-- closed, and the list is laid out so its BOTTOM edge meets the
-- toggle's top edge (rows stack upward from the toggle) instead of the
-- toggle sliding around underneath a growing list.
--
-- Out-of-envelope combinations degrade best-effort per
-- scripts/ui/responsive.lua: floors and clamps keep every rect
-- non-negative and inside the framebuffer rather than producing
-- invalid geometry.
local function computeLayout(fbW, fbH, rowCount)
    local s = scale.applyAll(BASE)
    local rowH    = math.max(1, s.rowH)
    local margin  = math.max(0, s.margin)
    local gap     = math.max(0, s.gap)
    local toggleH = math.max(1, math.min(s.toggleH, math.max(1, fbH)))

    -- Width: capped to the framebuffer and, like unit_info_v2's
    -- flush-right column, to the space remaining right of every
    -- toolbar cluster that is NOT the bottom-right map toggle (which
    -- this surface clears vertically instead, below).
    --
    -- Deliberately does NOT constrain against unit_info_v2's own
    -- flush-right, full-height pane (reserved_regions PRIORITY 90).
    -- That pane spans the entire height whenever a unit is selected,
    -- so yielding to it would mean either giving up the right edge
    -- entirely or moving the toggle whenever the selection changes --
    -- and a toggle whose screen bounds shift with unrelated gameplay
    -- state is exactly what requirement 2's "small reachable
    -- right-edge toggle" and requirement 6's reachability guarantee
    -- rule out. This is therefore an ACCEPTED, documented overlap,
    -- the same call unit_info_v2 itself made about its own
    -- pre-existing map_toggle overlap: the checklist is transparent,
    -- non-blocking text, so it never makes that pane unusable, and
    -- the checklist starts collapsed and is closed by one click.
    local rects = toolbarRects()
    local sideClusters = {}
    for _, rc in ipairs(rects) do
        if rc.name ~= "map_toggle" then
            sideClusters[#sideClusters + 1] = rc
        end
    end
    --
    -- The readable-width floor is itself capped to the framebuffer: a
    -- flat `math.max(20, ...)` would defeat the framebuffer cap on any
    -- positive width under 20px and push the toggle straight off the
    -- right edge (panelX/toggleX clamp to 0, so the extra width has
    -- nowhere to go) -- an INVALID out-of-envelope geometry rather
    -- than a degraded one.
    local minWidth = math.max(1, math.min(20, fbW))
    local panelW = math.min(s.panelW, fbW,
        reservedRegions.maxRightAnchoredWidth(0, fbH, sideClusters, fbW))
    panelW = math.max(minWidth, panelW)

    local toggleW = math.max(math.min(minWidth, panelW),
                              math.min(s.toggleW, panelW))
    local panelX  = math.max(0, fbW - panelW - margin)
    local toggleX = math.max(0, fbW - toggleW - margin)

    -- Sit above the bottom edge, and above any toolbar cluster whose
    -- horizontal span overlaps the toggle's (in practice the
    -- bottom-right map toggle). This is deterministic, unlike a
    -- minimal-displacement nudge, so the toggle never jumps sideways
    -- as unrelated clusters appear.
    local bottom = fbH - margin
    for _, rc in ipairs(rects) do
        if rc.x < toggleX + toggleW and toggleX < rc.x + rc.w then
            bottom = math.min(bottom, rc.y - gap)
        end
    end
    local toggleY = math.max(0, math.min(bottom - toggleH,
                                          math.max(0, fbH - toggleH)))

    -- Clamped at 0 so a framebuffer too short to hold the toggle plus a
    -- margin reports an empty viewport at the top edge rather than a
    -- negative one.
    local listBottom = math.max(0, toggleY - gap)
    local maxListH   = math.max(0, listBottom - margin)
    local capacity   = math.max(0, math.floor(maxListH / rowH))
    local visible    = math.min(rowCount, capacity)
    local listH      = visible * rowH

    return {
        uiscale     = scale.get(),
        panelX      = panelX,
        panelW      = panelW,
        rowH        = rowH,
        fontSize    = math.max(6, s.fontSize),
        indent      = math.max(0, s.indent),
        toggle      = { x = toggleX, y = toggleY, w = toggleW, h = toggleH },
        listBottom  = listBottom,
        listTop     = listBottom - listH,
        listH       = listH,
        capacity    = capacity,
        visibleRows = visible,
    }
end

-----------------------------------------------------------
-- Model
-----------------------------------------------------------

-- Exactly the rows the model reports as active, in the model's own
-- order. `active == false` rows are retained completed history and are
-- out of scope for this view; the order is #958's pre-order display
-- walk, already sorted by the loader, so it is never re-sorted here.
local function activeRows()
    local progress = package.loaded["scripts.tutorial_progress"]
    if progress == nil then
        local ok, mod = pcall(require, "scripts.tutorial_progress")
        if not ok then return {} end
        progress = mod
    end
    if type(progress.getViewModel) ~= "function" then return {} end
    local ok, model = pcall(progress.getViewModel)
    if not ok or type(model) ~= "table" or type(model.rows) ~= "table" then
        return {}
    end
    local out = {}
    for _, row in ipairs(model.rows) do
        if row.active == true then out[#out + 1] = row end
    end
    return out
end

-- The two progress semantics render differently on purpose: a square
-- bracket for a LATCHED full/composite objective, a round one for a
-- LIVE subobjective check that can still come back off.
local function markerFor(row)
    if row.kind == "subobjective" then
        return row.checked and "(x)" or "( )"
    end
    return row.completed and "[x]" or "[ ]"
end

-- The shared font and 9-slice box textures this surface draws with.
-- Both are owned by scripts/hud.lua and only exist once ui_manager has
-- run hud.init -- which happens well AFTER this module is
-- engine.loadScript'd (scripts/init_loader.lua loads it at ~line 164,
-- ui_manager at ~line 246, and hud.init is further gated on fontsReady,
-- i.e. a real GPU font atlas). So the very first rebuild legitimately
-- has neither, and produces an unlabelled, textureless toggle and no
-- row text.
local function hudAssets()
    local hudMod = hudModule()
    if hudMod == nil then return nil, nil end
    return hudMod.menuFont, hudMod.boxTexSet
end

local function contentSignature(rows)
    local font, boxTex = hudAssets()
    local parts = { tostring(tutorialHud.open), tostring(tutorialHud.scrollOffset),
                    tostring(tutorialHud.fbW), tostring(tutorialHud.fbH),
                    tostring(scale.get()),
                    -- ASSET READINESS is part of what was rendered, not
                    -- just of what it looked like: without it, a session
                    -- whose objectives happen not to change after boot
                    -- would keep the asset-less first build forever --
                    -- an invisible toggle over an invisible checklist.
                    -- Folding the handles in here makes the update tick
                    -- rebuild the moment hud.init supplies them.
                    tostring(font), tostring(boxTex) }
    for _, row in ipairs(rows) do
        parts[#parts + 1] = tostring(row.id) .. markerFor(row)
    end
    return table.concat(parts, "|")
end

-----------------------------------------------------------
-- Build / teardown
-----------------------------------------------------------

local function destroyElements()
    for _, h in ipairs(tutorialHud._els) do
        UI.deleteElement(h)
    end
    tutorialHud._els         = {}
    tutorialHud._rows        = {}
    tutorialHud._toggle      = nil
    tutorialHud._toggleLabel = nil
end

local function track(handle)
    if handle then tutorialHud._els[#tutorialHud._els + 1] = handle end
end

local function applyPageVisibility()
    if not tutorialHud.page then return end
    if tutorialHud._hudVisible then
        UI.showPage(tutorialHud.page)
    else
        UI.hidePage(tutorialHud.page)
    end
end

-- Destroy and recreate every element from the live model + geometry.
-- The single build path: opening/closing, scrolling, a content change,
-- a real resize and a scale-only rescale all land here, so there is
-- exactly one place where geometry and content can disagree, and none.
function tutorialHud.rebuild()
    if not tutorialHud.page then return false end
    local font, boxTex = hudAssets()
    tutorialHud._assetsReady = (font ~= nil) and (boxTex ~= nil)
    destroyElements()
    tutorialHud.rebuildCount = tutorialHud.rebuildCount + 1

    local rows = activeRows()
    local lay  = computeLayout(tutorialHud.fbW, tutorialHud.fbH, #rows)

    -- Clamp the preserved offset against the LIVE range on every
    -- rebuild, not just on a geometry change: subobjective checks are
    -- reversible, so rows appear and disappear during play and a
    -- stale offset would otherwise strand the remaining rows off the
    -- top of the viewport.
    local maxOffset = math.max(0, #rows - lay.capacity)
    if tutorialHud.scrollOffset > maxOffset then
        tutorialHud.scrollOffset = maxOffset
    end
    if tutorialHud.scrollOffset < 0 then tutorialHud.scrollOffset = 0 end

    tutorialHud._layout = lay
    tutorialHud._rowCount = #rows
    tutorialHud._maxOffset = maxOffset

    -- Toggle: the one real control. A box when the HUD's shared box
    -- textures are available, a bare element otherwise (headless /
    -- pre-texture boot) -- either way clickable, pointer-blocking by
    -- virtue of its callback, and never scroll-capturing, so the wheel
    -- over it still reaches gameplay.
    local t = lay.toggle
    local toggleH
    if boxTex then
        toggleH = UI.newBox("tutorial_hud_toggle", t.w, t.h,
            boxTex, math.max(1, math.floor(lay.fontSize / 2)),
            1.0, 1.0, 1.0, 1.0, 0, tutorialHud.page)
    else
        toggleH = UI.newElement("tutorial_hud_toggle", t.w, t.h, tutorialHud.page)
    end
    if toggleH then
        UI.addToPage(tutorialHud.page, toggleH, t.x, t.y)
        UI.setZIndex(toggleH, 10)
        UI.setClickable(toggleH, true)
        UI.setOnClick(toggleH, TOGGLE_CALLBACK)
        UI.setTooltip(toggleH, tutorialHud.open
            and "Hide the objective checklist"
            or  "Show the objective checklist")
        track(toggleH)
        tutorialHud._toggle = toggleH
        if font then
            local caption = (tutorialHud.open and "v " or "> ") .. "Objectives"
            local capH = UI.newText("tutorial_hud_toggle_label", caption, font,
                lay.fontSize, 1.0, 1.0, 1.0, 1.0, tutorialHud.page)
            if capH then
                UI.addToPage(tutorialHud.page, capH,
                    t.x + math.max(2, math.floor(lay.indent / 2)),
                    t.y + math.floor((t.h + lay.fontSize) / 2))
                UI.setZIndex(capH, 11)
                track(capH)
                tutorialHud._toggleLabel = capH
            end
        end
    end

    if not tutorialHud.open then
        tutorialHud._sig = contentSignature(rows)
        return true
    end

    -- Rows, top-down inside a viewport whose bottom edge is the
    -- toggle's top edge. No background element of any kind: the list
    -- is transparent over terrain, and the row hit boxes exist only to
    -- carry the YAML tooltip and to capture the wheel over exactly the
    -- visible list.
    for i = 1, lay.visibleRows do
        local row = rows[tutorialHud.scrollOffset + i]
        if row == nil then break end
        local rowY = lay.listTop + (i - 1) * lay.rowH
        local hit = UI.newElement("tutorial_hud_row_" .. tostring(row.id),
            lay.panelW, lay.rowH, tutorialHud.page)
        if hit then
            UI.addToPage(tutorialHud.page, hit, lay.panelX, rowY)
            UI.setZIndex(hit, 1)
            -- #743's independent policies: capture the wheel, block
            -- nothing, register no callback.
            UI.setScrollCapture(hit, true)
            if row.tooltip and row.tooltip ~= "" then
                UI.setTooltip(hit, row.tooltip)
            end
            track(hit)
        end
        local textH = nil
        if font then
            local indent = math.min(lay.indent * math.max(0, row.depth or 0),
                                     math.max(0, lay.panelW - lay.indent))
            textH = UI.newText("tutorial_hud_row_label_" .. tostring(row.id),
                markerFor(row) .. " " .. tostring(row.label or row.id),
                font, lay.fontSize, 1.0, 1.0, 1.0, 1.0, tutorialHud.page)
            if textH then
                UI.addToPage(tutorialHud.page, textH,
                    lay.panelX + indent,
                    rowY + math.floor((lay.rowH + lay.fontSize) / 2))
                UI.setZIndex(textH, 2)
                track(textH)
            end
        end
        tutorialHud._rows[#tutorialHud._rows + 1] = {
            id        = row.id,
            kind      = row.kind,
            label     = row.label,
            tooltip   = row.tooltip,
            depth     = row.depth,
            relation  = row.relation,
            completed = row.completed,
            checked   = row.checked,
            marker    = markerFor(row),
            handle    = hit,
            textHandle = textH,
            x = lay.panelX, y = rowY, w = lay.panelW, h = lay.rowH,
        }
    end

    tutorialHud._sig = contentSignature(rows)
    return true
end

-----------------------------------------------------------
-- Presentation controls
-----------------------------------------------------------

function tutorialHud.isOpen()
    return tutorialHud.open == true
end

function tutorialHud.setOpen(open)
    local want = open and true or false
    if tutorialHud.open == want then return false end
    tutorialHud.open = want
    tutorialHud.rebuild()
    return true
end

function tutorialHud.toggle()
    return tutorialHud.setOpen(not tutorialHud.open)
end

-- Collapse the panel and forget the scroll offset. PRESENTATION ONLY:
-- nothing about tutorial PROGRESS is touched (that is #958's, and its
-- own reset/apply paths own it).
function tutorialHud.resetPresentation()
    tutorialHud.open         = false
    tutorialHud.scrollOffset = 0
    tutorialHud.rebuild()
end

function tutorialHud.setScrollOffset(offset)
    local n = math.floor(tonumber(offset) or 0)
    local maxOffset = tutorialHud._maxOffset or 0
    if n < 0 then n = 0 end
    if n > maxOffset then n = maxOffset end
    if n == tutorialHud.scrollOffset then return false end
    tutorialHud.scrollOffset = n
    tutorialHud.rebuild()
    return true
end

-----------------------------------------------------------
-- Engine callbacks
-----------------------------------------------------------

-- The toggle's own click callback name, broadcast straight to this
-- module by Engine.Scripting.Lua.Thread.Dispatch's LuaUIClickEvent --
-- no ui_manager routing table entry needed, the same way
-- preview_manager.lua handles its own list clicks.
function tutorialHud.onTutorialHudToggle(elemHandle)
    if tutorialHud._toggle == nil or elemHandle ~= tutorialHud._toggle then
        return false
    end
    tutorialHud.toggle()
    return true
end

-- Wheel over a row hit box. `onUIScroll` is broadcast to every
-- loadScript'd module, so this needs no entry in
-- scripts/ui_manager_scroll.lua's require()-only chain; the handle
-- check is what keeps it scoped to this surface.
function tutorialHud.onUIScroll(elemHandle, _dx, dy)
    if not tutorialHud.open then return false end
    local owned = false
    for _, row in ipairs(tutorialHud._rows) do
        if row.handle == elemHandle then owned = true break end
    end
    if not owned then return false end
    local step = 0
    if dy > 0 then step = -1 elseif dy < 0 then step = 1 end
    if step == 0 then return false end
    return tutorialHud.setScrollOffset(tutorialHud.scrollOffset + step)
end

-- The engine's own broadcast to every loadScript'd module. It records
-- the new dimensions and DELIBERATELY DOES NOT REBUILD.
--
-- broadcastToModules fans out in script-map order, so this module can
-- (and does) receive the resize BEFORE ui_manager forwards it to
-- scripts/hud.lua -- and this surface anchors against
-- hud.getToolbarRects(), which until that forward still describes the
-- toolbar at the OLD framebuffer size. Rebuilding here would lay the
-- toggle out against stale toolbar bounds and could leave it sitting
-- on top of the newly positioned map toggle. The rebuild happens in
-- reflow() below instead, which ui_manager runs after hud has been
-- rebuilt at the new size -- exactly the split popup.lua and
-- unit_info_v2.lua already use (scripts/ui_manager_boot.lua), and the
-- reason this module still must NOT join that file's manual
-- onFramebufferResize forward set, which would double-fire it.
function tutorialHud.onFramebufferResize(width, height)
    if (width or 0) <= 0 or (height or 0) <= 0 then return end
    tutorialHud.fbW = width
    tutorialHud.fbH = height
end

-- The geometry-dependent half of a resize, run once hud's toolbar
-- clusters exist at the new size. Also the scale-only entry point
-- (uiManager.notifyGameplayRescale), which has no engine broadcast to
-- pair with and so passes the dimensions itself.
function tutorialHud.reflow(width, height)
    if (width or 0) > 0 and (height or 0) > 0 then
        tutorialHud.fbW = width
        tutorialHud.fbH = height
    end
    tutorialHud.rebuild()
end

-- A save load replaces the whole session but not this Lua singleton,
-- so the panel would otherwise come back open, scrolled to wherever
-- the pre-load session left it. Presentation only; #958's save
-- component restores the progress itself.
function tutorialHud.onSaveLoaded(_survUnitIds, _survBuildingIds)
    tutorialHud.resetPresentation()
end

-- hud.hide()'s teardown sweep reaches this through
-- scripts/ui/view_teardown.lua. Hiding the page is all it does -- the
-- open flag and the scroll offset must survive, so this is a
-- suppress, never a clear.
function tutorialHud.onHudHidden()
    tutorialHud._hudVisible = false
    applyPageVisibility()
end

function tutorialHud.update(_dt)
    if not tutorialHud.page then return end
    local hudMod     = hudModule()
    local hudVisible = (hudMod ~= nil) and (hudMod.visible == true)
    if hudVisible ~= tutorialHud._hudVisible then
        tutorialHud._hudVisible = hudVisible
        applyPageVisibility()
    end
    if not hudVisible then return end
    -- Content churn only: opening, scrolling and resizing rebuild
    -- directly. Objectives complete and subobjectives check/uncheck
    -- from #959's evaluation tick, which this module never drives.
    if contentSignature(activeRows()) ~= tutorialHud._sig then
        tutorialHud.rebuild()
    end
end

function tutorialHud.init(_scriptId)
    engine.logInfo("Tutorial HUD initializing...")
    if tutorialHud.page == nil then
        -- "overlay", the same pass-through band scripts/hud.lua's own
        -- pages use. NOT "modal": a modal page defaults to
        -- input-exclusive and its empty space would consume every
        -- gameplay click around the checklist.
        tutorialHud.page = UI.newPage("tutorial_hud_overlay", "overlay")
    end
    local w, h = engine.getFramebufferSize()
    tutorialHud.fbW = w or tutorialHud.fbW
    tutorialHud.fbH = h or tutorialHud.fbH
    -- Requirement 2: collapsed at fresh boot, whatever a hot-reloaded
    -- singleton was left holding.
    tutorialHud.open         = false
    tutorialHud.scrollOffset = 0
    tutorialHud._hudVisible  = false
    tutorialHud.rebuild()
    applyPageVisibility()
end

function tutorialHud.shutdown()
    destroyElements()
    if tutorialHud.page then
        UI.hidePage(tutorialHud.page)
        UI.deletePage(tutorialHud.page)
        tutorialHud.page = nil
    end
    tutorialHud._hudVisible = false
end

-----------------------------------------------------------
-- Introspection (requirement 7)
-----------------------------------------------------------

-- Everything a headless spec or an offscreen probe needs to assert
-- against, with `completed` and `checked` kept apart exactly as the
-- view model reports them (a full/composite row carries only
-- `completed`, a subobjective row only `checked`).
function tutorialHud.dump()
    local lay = tutorialHud._layout
        or computeLayout(tutorialHud.fbW, tutorialHud.fbH, 0)
    local rows, ids = {}, {}
    for i, row in ipairs(tutorialHud._rows) do
        rows[i] = {
            id        = row.id,
            kind      = row.kind,
            label     = row.label,
            tooltip   = row.tooltip,
            depth     = row.depth,
            relation  = row.relation,
            completed = row.completed,
            checked   = row.checked,
            marker    = row.marker,
            handle    = row.handle,
            textHandle = row.textHandle,
            x = row.x, y = row.y, w = row.w, h = row.h,
            pointerBlocking = row.handle and UI.isPointerBlocking(row.handle) or false,
            scrollCapture   = row.handle and UI.isScrollCapturing(row.handle) or false,
        }
        ids[i] = row.id
    end
    local activeIds = {}
    for i, row in ipairs(activeRows()) do activeIds[i] = row.id end
    return {
        open         = tutorialHud.open,
        hudVisible   = tutorialHud._hudVisible,
        visible      = tutorialHud.open and tutorialHud._hudVisible,
        page         = tutorialHud.page,
        rows         = rows,
        rowIds       = ids,
        activeIds    = activeIds,
        activeCount  = #activeIds,
        scrollOffset = tutorialHud.scrollOffset,
        scrollRange  = tutorialHud._maxOffset or 0,
        capacity     = lay.capacity,
        rebuildCount = tutorialHud.rebuildCount,
        -- Whether hud's shared font AND box textures existed at build
        -- time. False means this build predates hud.init and is
        -- deliberately unlabelled/textureless; the update tick rebuilds
        -- as soon as they arrive (see contentSignature).
        assetsReady  = tutorialHud._assetsReady == true,
        toggle       = {
            x = lay.toggle.x, y = lay.toggle.y,
            w = lay.toggle.w, h = lay.toggle.h,
            handle = tutorialHud._toggle,
            label  = tutorialHud._toggleLabel,
        },
        panelX  = lay.panelX,
        panelW  = lay.panelW,
        listTop = lay.listTop,
        listBottom = lay.listBottom,
        rowH    = lay.rowH,
        fbW     = tutorialHud.fbW,
        fbH     = tutorialHud.fbH,
        uiscale = lay.uiscale,
    }
end

return tutorialHud
