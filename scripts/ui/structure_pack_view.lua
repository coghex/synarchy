-- Structure pack viewer for --preview structures/<name> (#2495, BDA-17).
--
-- Owns everything to the RIGHT of scripts/ui/asset_browser.lua's list
-- while a structure PACK is browsed: the enlarged frame of the selected
-- appearance, a lifecycle row (static / construction / destruction), a
-- cap row for wall edges (00 / 10 / 01 / 11), and three info lines naming
-- what gameplay will light the displayed frame with.
--
-- The model -- which appearances exist, their order, every inherited
-- texture and facemap, every declared frame list, the effective fps, and
-- every missing/undeclared verdict -- is resolved pre-boot by
-- Engine.Preview.StructurePack and arrives verbatim through
-- engine.getPreviewBrowse(). Nothing here re-derives it; in particular
-- this module never decides whether a path exists: it is told.
--
-- Playback (#2495 requirement 4, #1833):
--   * Selecting an appearance shows its STATIC sprite.
--   * Selecting a lifecycle starts ONE wall clock for that lifecycle's
--     frames, replayed forever whatever the clip is (the preview always
--     replays; gameplay clamps construction by progress and expires a
--     destruction effect).
--   * A cap change, a zoom change and a resize touch none of that: the
--     texture, the lifecycle and the phase all carry on. A cap is a
--     LIGHTING variant, not an appearance, so it changes only the
--     reported facemap.
--
-- Missing and undeclared (#2495 requirements 3 and 5). A frame the
-- engine reported missing keeps its POSITION in the sequence -- the
-- count is always the declared one -- and is never requested; the view
-- hides the sprite and draws a textureless marker instead, so nothing
-- previously displayed lingers and nothing is substituted. An undeclared
-- lifecycle draws its own marker. Both are terminal states: the view is
-- ready as soon as it is drawing one.
--
-- Rendering (requirement 5): a frame is drawn RAW. The viewer does not
-- reimplement the world shader's facemap lighting; it reports the facemap
-- and alpha policy (facemap-alpha for the static sprite, frame-alpha for
-- lifecycle frames, D-10) beside the picture instead.
--
-- Zoom (#1907): the enlarged frame renders at the owner's session
-- multiplier times its fit to getZoomRegion(), the sub-rect above the
-- control rows. The multiplier belongs to the PACK -- one preview object
-- -- so nothing in here resets it.
local scale = require("scripts.ui.scale")
local previewZoom = require("scripts.ui.preview_zoom")

local structurePackView = {}

local views = {}
local nextId = 1

local LIFECYCLES = { "static", "construction", "destruction" }
local LIFECYCLE_CALLBACK = "onPreviewLifecycleClick"
local CAP_CALLBACK = "onPreviewCapClick"

local MISSING_MARK = "X"
local UNDECLARED_MARK = "undeclared"
local FAILED_MARK = "failed"

-- Must stay identical to Engine.Preview.BuildingMatrix.previewFrameIndexAt
-- (which Engine.Preview.StructurePack.lifecycleFrameIndexAt applies) and
-- scripts/ui/building_asset_view.lua's copy: floor(elapsed * fps) modulo
-- the DECLARED frame count, for every clip (#1833).
local function frameIndexAt(fps, frameCount, elapsed)
    if frameCount <= 1 then return 0 end
    local rate = math.max(0, fps or 0)
    local raw = math.floor(math.max(0, elapsed or 0) * rate)
    return raw % frameCount
end

-----------------------------------------------------------
-- Model accessors
-----------------------------------------------------------

local function lifecycleOf(appearance, name)
    for _, l in ipairs((appearance or {}).lifecycles or {}) do
        if l.name == name then return l end
    end
    return nil
end

local function isWall(appearance)
    return appearance ~= nil and appearance.edge ~= nil
end

local function playsClock(l)
    return l ~= nil and l.name ~= "static" and l.declared == true
        and (l.fps or 0) > 0
end

-- The facemap the displayed frame is lit with: the selected cap's for a
-- wall edge, the appearance's single one otherwise. A lifecycle frame
-- REUSES the resolved appearance's facemap (D-10), so this does not
-- depend on the lifecycle.
local function facemapOf(v)
    local faces = (v.appearance or {}).facemaps or {}
    if isWall(v.appearance) then
        for _, f in ipairs(faces) do
            if f.cap == v.cap then return f end
        end
        return nil
    end
    return faces[1]
end

-----------------------------------------------------------
-- Geometry
-----------------------------------------------------------

-- The panel split: the enlarged region on top, then the lifecycle row,
-- the cap row (walls only), and the info lines. Every dimension is
-- floored to at least 1 so a heavily shrunk window still yields valid,
-- never inverted, geometry (#748).
local function layout(v)
    local p = v.panel
    local s = v.uiscale
    local gap = math.max(2, math.floor(8 * s))
    local rowH = math.max(12, math.floor(28 * s))
    local lineH = math.max(8, math.floor(18 * s))
    local rows = isWall(v.appearance) and 2 or 1
    local bottom = rows * (rowH + gap) + 3 * lineH + gap
    local enlargedH = math.max(1, p.height - bottom - gap)

    local function rowCells(n, y)
        local w = math.floor((p.width - gap * (n - 1)) / n)
        w = math.max(8, math.min(w, math.floor(200 * s)))
        local out = {}
        for i = 1, n do
            out[i] = { x = p.x + (i - 1) * (w + gap), y = y, w = w, h = rowH }
        end
        return out
    end

    local lifeY = p.y + enlargedH + gap
    local capY = lifeY + rowH + gap
    local infoY = lifeY + rows * (rowH + gap)
    return {
        enlarged = { x = p.x, y = p.y, width = math.max(1, p.width),
                     height = enlargedH },
        lifecycleCells = rowCells(#LIFECYCLES, lifeY),
        capCells = isWall(v.appearance) and rowCells(4, capY) or {},
        infoY = infoY,
        lineH = lineH,
    }
end

-----------------------------------------------------------
-- Creation / teardown
-----------------------------------------------------------

-- params: page, font, panel, requestTexture(path) -> handle (the owner's
-- cache + trimmed-loading bookkeeping; NEVER called for a missing frame),
-- chromeTexture (the list's highlight.png, reused for markers and hit
-- boxes so no control adds a texture load), zoom, uiscale, zIndex, and
-- onDisplayChange() -- called whenever the DISPLAYED frame changes
-- (appearance, lifecycle or playback index), before anything for the
-- new frame is requested, so the owner can drop the previous frame's
-- handles and readiness: a failure the old frame suffered, or suffers
-- late, must not describe the new one.
function structurePackView.new(params)
    local id = nextId
    nextId = nextId + 1
    views[id] = {
        id = id,
        page = params.page,
        font = params.font,
        panel = params.panel,
        requestTexture = params.requestTexture,
        onDisplayChange = params.onDisplayChange,
        shownKey = nil,
        failedKey = nil,   -- the displayed frame whose upload failed
        chromeTexture = params.chromeTexture,
        zoom = previewZoom.clamp(params.zoom),
        uiscale = params.uiscale or scale.get(),
        zIndex = params.zIndex or 1,
        appearance = nil,
        lifecycle = "static",
        cap = "00",          -- remembered across wall edges
        clockStart = nil,
        frameIndex = 0,
        handle = nil,
        ready = false,
        fitKey = nil,
        lifeCells = {},
        capCells = {},
        infoIds = {},
    }
    return id
end

local function newCell(v, name, text, callback)
    local markerId = UI.newSprite(name .. "_marker", 1, 1, v.chromeTexture,
        0.3, 0.5, 0.8, 0.8, v.page)
    UI.addToPage(v.page, markerId, 0, 0)
    UI.setZIndex(markerId, v.zIndex)
    UI.setVisible(markerId, false)

    local labelId = UI.newText(name .. "_label", text, v.font,
        math.max(8, math.floor(14 * v.uiscale)), 1.0, 1.0, 1.0, 1.0, v.page)
    UI.addToPage(v.page, labelId, 0, 0)
    UI.setZIndex(labelId, v.zIndex + 2)

    local hitId = UI.newSprite(name .. "_hit", 1, 1, v.chromeTexture,
        0.0, 0.0, 0.0, 0.0, v.page)
    UI.addToPage(v.page, hitId, 0, 0)
    UI.setZIndex(hitId, v.zIndex + 3)
    UI.setClickable(hitId, true)
    UI.setOnClick(hitId, callback)
    return { markerId = markerId, labelId = labelId, hitId = hitId }
end

local function destroyCell(c)
    UI.deleteElement(c.markerId)
    UI.deleteElement(c.labelId)
    UI.deleteElement(c.hitId)
end

local function destroyCells(v)
    for _, c in ipairs(v.lifeCells) do destroyCell(c) end
    for _, c in ipairs(v.capCells) do destroyCell(c) end
    v.lifeCells, v.capCells = {}, {}
end

function structurePackView.destroy(id)
    local v = views[id]
    if not v then return end
    destroyCells(v)
    for _, e in ipairs({ v.spriteId, v.missingId }) do UI.deleteElement(e) end
    for _, e in ipairs(v.infoIds) do UI.deleteElement(e) end
    views[id] = nil
end

-- The lifecycle cell's caption: its name, plus a suffix a reviewer can
-- read without opening the dump -- "-" for undeclared, "!" when any
-- declared frame is missing.
local function lifecycleCaption(l, name)
    if not l or l.declared ~= true then return name .. " -" end
    for _, f in ipairs(l.frames or {}) do
        if f.missing then return name .. " !" end
    end
    return name
end

local function capCaption(f, cap)
    if not f or f.missing then return cap .. " !" end
    return cap
end

local function ensureElements(v)
    if v.spriteId then return end
    v.spriteId = UI.newSprite("preview_structure_sprite", 1, 1,
        v.chromeTexture, 1.0, 1.0, 1.0, 1.0, v.page)
    UI.addToPage(v.page, v.spriteId, 0, 0)
    UI.setZIndex(v.spriteId, v.zIndex)
    UI.setVisible(v.spriteId, false)

    v.missingId = UI.newText("preview_structure_missing", MISSING_MARK,
        v.font, math.max(12, math.floor(28 * v.uiscale)),
        1.0, 0.4, 0.4, 1.0, v.page)
    UI.addToPage(v.page, v.missingId, 0, 0)
    UI.setZIndex(v.missingId, v.zIndex + 1)
    UI.setVisible(v.missingId, false)

    for i = 1, 3 do
        local tid = UI.newText("preview_structure_info_" .. i, "", v.font,
            math.max(8, math.floor(14 * v.uiscale)), 0.9, 0.9, 0.9, 1.0, v.page)
        UI.addToPage(v.page, tid, 0, 0)
        UI.setZIndex(tid, v.zIndex + 1)
        v.infoIds[i] = tid
    end
end

local function buildCells(v)
    destroyCells(v)
    local a = v.appearance
    for i, name in ipairs(LIFECYCLES) do
        local c = newCell(v, "preview_lifecycle_" .. v.id .. "_" .. i,
            lifecycleCaption(lifecycleOf(a, name), name), LIFECYCLE_CALLBACK)
        c.name = name
        v.lifeCells[i] = c
    end
    if isWall(a) then
        for i, f in ipairs(a.facemaps or {}) do
            local cap = f.cap or tostring(i)
            local c = newCell(v, "preview_cap_" .. v.id .. "_" .. i,
                capCaption(f, cap), CAP_CALLBACK)
            c.cap = cap
            v.capCells[i] = c
        end
    end
end

-----------------------------------------------------------
-- Selection
-----------------------------------------------------------

local function restartClock(v, now)
    v.clockStart = now
    v.frameIndex = 0
    v.fitKey = nil
end

-- Select an appearance: its STATIC sprite, a fresh clock. The remembered
-- cap carries over to the next wall edge, since comparing the same cap
-- across edges is the natural review; it only falls back to the first
-- cap when the new edge does not declare the remembered one at all.
function structurePackView.setAppearance(id, appearance, now)
    local v = views[id]
    if not v then return end
    ensureElements(v)
    v.appearance = appearance
    v.lifecycle = "static"
    v.ready = false
    if isWall(appearance) then
        local known = false
        for _, f in ipairs(appearance.facemaps or {}) do
            if f.cap == v.cap then known = true end
        end
        if not known then
            local first = (appearance.facemaps or {})[1]
            v.cap = first and first.cap or v.cap
        end
    end
    buildCells(v)
    restartClock(v, now)
    structurePackView.reflow(id)
end

-- Select a lifecycle: ONE fresh clock over its frames. Selecting the
-- same lifecycle again restarts it, which is how a reviewer replays a
-- clip from its first frame.
function structurePackView.setLifecycle(id, name, now)
    local v = views[id]
    if not v or not v.appearance or not lifecycleOf(v.appearance, name) then
        return false
    end
    v.lifecycle = name
    v.ready = false
    restartClock(v, now)
    structurePackView.reflow(id)
    return true
end

-- Left/Right through the lifecycle row in displayed order, wrapping at
-- both ends -- the same shape as unit directions and building facings.
function structurePackView.selectAdjacentLifecycle(id, step, now)
    local v = views[id]
    if not v or not v.appearance or (step ~= -1 and step ~= 1) then
        return false
    end
    local current = 1
    for i, name in ipairs(LIFECYCLES) do
        if name == v.lifecycle then current = i end
    end
    local target = ((current - 1 + step) % #LIFECYCLES) + 1
    return structurePackView.setLifecycle(id, LIFECYCLES[target], now)
end

-- Select a wall cap. Deliberately touches NOTHING but the reported
-- facemap: not the texture, not the lifecycle, not the clock, not zoom.
function structurePackView.setCap(id, cap)
    local v = views[id]
    if not v or not isWall(v.appearance) then return false end
    for _, f in ipairs(v.appearance.facemaps or {}) do
        if f.cap == cap then
            v.cap = cap
            structurePackView.reflow(id)
            return true
        end
    end
    return false
end

-- Deliberately does NOT touch the clock: a resize preserves the phase.
function structurePackView.setPanel(id, panel)
    local v = views[id]
    if not v then return end
    v.panel = panel
    v.fitKey = nil
    structurePackView.reflow(id)
end

function structurePackView.setZoom(id, multiplier)
    local v = views[id]
    if not v then return end
    v.zoom = previewZoom.clamp(multiplier)
    v.fitKey = nil
    structurePackView.reflow(id)
end

function structurePackView.getZoomRegion(id)
    local v = views[id]
    if not v or not v.panel then return nil end
    return layout(v).enlarged
end

-----------------------------------------------------------
-- Geometry application
-----------------------------------------------------------

-- The displayed frame record, or nil for an undeclared lifecycle.
local function displayedFrame(v)
    local l = lifecycleOf(v.appearance, v.lifecycle)
    if not l or l.declared ~= true then return nil, l end
    local frames = l.frames or {}
    return frames[math.min(#frames, v.frameIndex + 1)], l
end

local function placeCell(c, rect, selected)
    UI.setSize(c.markerId, rect.w, rect.h)
    UI.setPosition(c.markerId, rect.x, rect.y)
    UI.setVisible(c.markerId, selected)
    UI.setSize(c.hitId, rect.w, rect.h)
    UI.setPosition(c.hitId, rect.x, rect.y)
    UI.setPosition(c.labelId, rect.x + 4,
                   rect.y + math.floor(rect.h * 0.7))
    c.bounds = rect
end

local function infoLines(v, frame, l)
    local count = l and #(l.frames or {}) or 0
    local first
    if not l or l.declared ~= true then
        first = v.lifecycle .. ": undeclared"
    elseif frame and frame.missing then
        first = string.format("%s %d/%d missing (%s): %s", v.lifecycle,
            v.frameIndex + 1, count, tostring(frame.reason), tostring(frame.path))
    else
        first = string.format("%s %d/%d: %s", v.lifecycle, v.frameIndex + 1,
            count, tostring(frame and frame.path))
    end
    local f = facemapOf(v)
    local face = "facemap: " .. ((f and f.path) or "undeclared")
    if isWall(v.appearance) then face = face .. " (cap " .. tostring(v.cap) .. ")" end
    if f and f.missing then face = face .. " missing (" .. tostring(f.reason) .. ")" end
    if f and f.inherited then face = face .. " [inherited]" end
    local timing = "alpha: " .. tostring(l and l.alphaPolicy)
    if playsClock(l) then
        timing = timing .. string.format("  fps: %g (%s)", l.fps,
                                         tostring(l.fpsSource))
    end
    return { first, face, timing }
end

-- Recompute every rect from the panel and push the displayed frame. A
-- texture whose size is not known yet leaves fitKey nil so update()
-- retries next tick; a missing or undeclared selection has nothing to
-- wait for and is ready at once.
function structurePackView.reflow(id)
    local v = views[id]
    if not v or not v.panel or not v.appearance then return end
    local g = layout(v)

    for i, c in ipairs(v.lifeCells) do
        placeCell(c, g.lifecycleCells[i], c.name == v.lifecycle)
    end
    for i, c in ipairs(v.capCells) do
        placeCell(c, g.capCells[i], c.cap == v.cap)
    end

    local frame, l = displayedFrame(v)
    -- Which frame is on screen, by identity. A cap change, a resize or a
    -- zoom step keeps it; anything else is a new frame for the owner.
    local shown = tostring(v.appearance.identity) .. "|" .. v.lifecycle
        .. "|" .. tostring(v.frameIndex)
    if shown ~= v.shownKey then
        v.shownKey = shown
        -- A failure describes the frame as it was displayed THEN; coming
        -- back to the same frame later is a fresh attempt.
        v.failedKey = nil
        if v.onDisplayChange then v.onDisplayChange() end
    end
    for i, text in ipairs(infoLines(v, frame, l)) do
        UI.setText(v.infoIds[i], text)
        UI.setPosition(v.infoIds[i], v.panel.x, g.infoY + i * g.lineH)
    end

    local centreX = g.enlarged.x + math.floor(g.enlarged.width / 2)
    local centreY = g.enlarged.y + math.floor(g.enlarged.height / 2)
    -- A frame whose upload FAILED is terminal for as long as it stays the
    -- displayed frame: never retried behind the owner's back (the owner
    -- has already settled on "empty", which a silent retry could never
    -- correct), and retried only through a genuine display change --
    -- another frame, lifecycle or appearance -- which also resets the
    -- owner's readiness.
    local failed = v.failedKey ~= nil and v.failedKey == v.shownKey
    if failed or not frame or frame.missing then
        -- Terminal: hide the sprite so nothing earlier lingers, and never
        -- request the path.
        v.handle = nil
        UI.setVisible(v.spriteId, false)
        UI.setText(v.missingId, failed and FAILED_MARK
            or (frame and MISSING_MARK or UNDECLARED_MARK))
        UI.setVisible(v.missingId, true)
        UI.setPosition(v.missingId, centreX, centreY)
        v.ready = true
        v.fitKey = tostring(v.lifecycle) .. "|" .. tostring(v.frameIndex)
            .. "|" .. tostring(v.cap) .. "|" .. tostring(v.panel.width)
            .. "x" .. tostring(v.panel.height)
        return
    end

    UI.setVisible(v.missingId, false)
    local handle = v.requestTexture(frame.path)
    v.handle = handle
    UI.setSpriteTexture(v.spriteId, handle)
    UI.setVisible(v.spriteId, true)
    local size = engine.getTextureSize(handle)
    local rect = size and previewZoom.fitRect(g.enlarged, size.width,
                                              size.height, v.zoom)
    if rect then
        UI.setSize(v.spriteId, rect.width, rect.height)
        UI.setPosition(v.spriteId, rect.x, rect.y)
        v.ready = true
        v.fitKey = tostring(v.lifecycle) .. "|" .. tostring(v.frameIndex)
            .. "|" .. tostring(v.cap) .. "|" .. tostring(v.panel.width)
            .. "x" .. tostring(v.panel.height) .. "@" .. tostring(v.zoom)
    else
        v.fitKey = nil
    end
end

-----------------------------------------------------------
-- Playback
-----------------------------------------------------------

function structurePackView.update(id, now)
    local v = views[id]
    if not v or not v.appearance then return end
    local l = lifecycleOf(v.appearance, v.lifecycle)
    if playsClock(l) and v.clockStart then
        local idx = frameIndexAt(l.fps, #(l.frames or {}), now - v.clockStart)
        if idx ~= v.frameIndex then
            v.frameIndex = idx
            v.fitKey = nil
        end
    end
    if not v.fitKey then structurePackView.reflow(id) end
end

-----------------------------------------------------------
-- Input
-----------------------------------------------------------

-- The owner's report that a texture upload terminally failed (#1690).
-- Only the handle the view is DISPLAYING matters; anything else belongs
-- to a frame no longer on screen. Returns true when it was the current
-- frame's.
function structurePackView.noteFailed(id, handle)
    local v = views[id]
    if not v or handle == nil or handle ~= v.handle then return false end
    v.failedKey = v.shownKey
    v.fitKey = nil
    structurePackView.reflow(id)
    return true
end

function structurePackView.isLifecycleCallback(name)
    return name == LIFECYCLE_CALLBACK
end

function structurePackView.isCapCallback(name)
    return name == CAP_CALLBACK
end

function structurePackView.handleLifecycleClick(id, elemHandle, now)
    local v = views[id]
    if not v then return nil end
    for _, c in ipairs(v.lifeCells) do
        if c.hitId == elemHandle then
            structurePackView.setLifecycle(id, c.name, now)
            return c.name
        end
    end
    return nil
end

function structurePackView.handleCapClick(id, elemHandle)
    local v = views[id]
    if not v then return nil end
    for _, c in ipairs(v.capCells) do
        if c.hitId == elemHandle then
            structurePackView.setCap(id, c.cap)
            return c.cap
        end
    end
    return nil
end

-----------------------------------------------------------
-- Introspection (#2495 requirement 7)
-----------------------------------------------------------

-- Bounds come from UI.getElementInfo, never this module's own layout
-- arithmetic: the engine is the authority on where a hit box really is,
-- so automated input clicking these coordinates exercises the real
-- element.
local function cellDump(c, extra)
    local info = UI.getElementInfo(c.hitId)
    local label = UI.getElementInfo(c.labelId)
    local out = {
        hitHandle = c.hitId,
        labelElement = c.labelId,
        caption = label and label.text or nil,
        bounds = info and { x = info.x, y = info.y, w = info.width,
                            h = info.height } or c.bounds,
    }
    for k, val in pairs(extra) do out[k] = val end
    return out
end

function structurePackView.dump(id)
    local v = views[id]
    if not v or not v.appearance then return nil end
    local frame, l = displayedFrame(v)
    local f = facemapOf(v)
    local out = {
        appearance = v.appearance.identity,
        variant = v.appearance.variant,
        cap = isWall(v.appearance) and v.cap or nil,
        lifecycle = v.lifecycle,
        declared = l ~= nil and l.declared == true,
        undeclared = l == nil or l.declared ~= true,
        frameIndex = v.frameIndex,
        frameCount = l and #(l.frames or {}) or 0,
        fps = playsClock(l) and l.fps or 0,
        fpsSource = l and l.fpsSource or nil,
        path = frame and frame.path or nil,
        missing = frame ~= nil and frame.missing == true,
        failed = v.failedKey ~= nil and v.failedKey == v.shownKey,
        missingReason = frame and frame.reason or nil,
        handle = v.handle,
        facemap = f and f.path or nil,
        facemapCap = f and f.cap or nil,
        facemapMissing = f == nil or f.missing == true,
        facemapReason = f and f.reason or (f == nil and "undeclared" or nil),
        facemapInherited = f ~= nil and f.inherited == true,
        alphaPolicy = l and l.alphaPolicy or nil,
        ready = v.ready,
        spriteElement = v.spriteId,
        missingElement = v.missingId,
        infoElements = v.infoIds,
    }
    out.animated = playsClock(l)
    out.lifecycleRow = {}
    for i, c in ipairs(v.lifeCells) do
        local cl = lifecycleOf(v.appearance, c.name)
        out.lifecycleRow[i] = cellDump(c, {
            lifecycle = c.name,
            selected = c.name == v.lifecycle,
            declared = cl ~= nil and cl.declared == true,
        })
    end
    out.capRow = {}
    for i, c in ipairs(v.capCells) do
        out.capRow[i] = cellDump(c, { cap = c.cap, selected = c.cap == v.cap })
    end
    local info = v.spriteId and UI.getElementInfo(v.spriteId)
    out.zoom = {
        multiplier = v.zoom,
        min = previewZoom.MIN,
        max = previewZoom.MAX,
        region = structurePackView.getZoomRegion(id),
        -- No sprite on screen for a missing/undeclared frame, so no rect:
        -- a hidden element's stale bounds must not satisfy a containment
        -- check.
        sprite = (v.handle and info) and {
            x = info.x, y = info.y, w = info.width, h = info.height,
        } or nil,
    }
    return out
end

return structurePackView
