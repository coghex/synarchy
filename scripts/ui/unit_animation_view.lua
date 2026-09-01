-- Unit animation viewer for --preview units/<name> (#887, Phase 3 of
-- the browser epic #427).
--
-- Owns everything to the RIGHT of scripts/ui/asset_browser.lua's list:
-- one enlarged sprite for the currently selected direction, a
-- horizontal row of every available direction animating beneath it, and
-- the single playback clock they all share. The animation MODEL
-- (membership, ordering, per-direction frames, mirroring, fps/loop) is
-- resolved pre-boot by Engine.Preview.Unit and arrives verbatim through
-- engine.getPreviewBrowse() — nothing here re-derives it.
--
-- A frame is a TABLE, not a path (#1260): { path, u0, v0, u1, v1,
-- width, height } — the compiled atlas, the cell to sample within it,
-- and that cell's own size. Every frame of an animation names the SAME
-- compiled image and differs only in its sub-rect, so a sprite must be
-- published with UI.setSpriteFrame — texture, sub-rect and mirror in
-- ONE manager transition. Setting the texture alone would draw the
-- whole sheet, and setting texture and UV separately leaves a window in
-- which the render thread pairs the new image with the previous frame's
-- rect. Since #1261 there is no other kind of unit animation frame.
--
-- Playback contract (#887 Requirement 5 + its review amendment):
--   * ONE clock per selected animation. Every cell computes its own
--     index from the SAME elapsed value against its OWN frame count, so
--     directions with unequal frame counts (four checked-in acolyte
--     animations have exactly that) stay phase-aligned instead of
--     drifting.
--   * Selecting a different ANIMATION resets the clock; enlarging a
--     different DIRECTION does not — the row keeps playing.
--   * End-of-clip policy (#1833): the preview ALWAYS replays. Frame
--     N-1 is followed, after its own normal duration, by frame 0
--     again, indefinitely — for every clip, whatever its authored
--     `loop` says. The wrap lives in the index computation, never in
--     the clock: animStart is not restarted at a cycle boundary, which
--     is what keeps the phase across a direction change and a resize.
--     The source `loop` value is still reported verbatim by dump()
--     (below); only gameplay (Unit.Render.pickFrame) still clamps.
--
-- Zoom (#1907): the ENLARGED sprite renders at the owner's session
-- multiplier times its fit to layout()'s `enlarged` sub-rect (which is
-- also the wheel's capture region -- getZoomRegion below). The
-- direction ROW is deliberately untouched: cells keep their existing
-- fixed sizing. This view never resets the multiplier -- a unit is ONE
-- preview object, so an animation or direction change preserves it.
local scale = require("scripts.ui.scale")
local previewZoom = require("scripts.ui.preview_zoom")

local unitAnimationView = {}

local views = {}
local nextId = 1

-- Short cell captions, keyed by the long folder-name spelling
-- Engine.Preview.Unit.directionDirName emits.
local SHORT_NAME = {
    ["south"] = "S",  ["south-west"] = "SW", ["west"] = "W",
    ["north-west"] = "NW", ["north"] = "N",  ["north-east"] = "NE",
    ["east"] = "E",   ["south-east"] = "SE",
}

local CELL_CALLBACK = "onPreviewDirectionClick"

-- Must stay identical to Engine.Preview.Unit.frameIndexAt — the probe
-- cross-checks the dump's reported index against wall time, so a
-- divergence here shows up as a failing gate rather than silently.
-- srcLoop is the clip's AUTHORED loop value; the preview replays
-- either way (#1833), so it is taken and deliberately never read,
-- exactly as the Haskell original takes and ignores it.
local function frameIndexAt(srcLoop, fps, frameCount, elapsed)
    if frameCount <= 1 then return 0 end
    local rate = math.max(0, fps or 0)
    local raw = math.floor(math.max(0, elapsed or 0) * rate)
    return raw % frameCount
end

-- Publish one frame onto a sprite: texture, sub-rect and mirror
-- together. See the module header for why this is never done in pieces.
local function applyFrame(v, elemId, frame, mirrored)
    if not elemId or not frame then return nil end
    local handle = v.requestTexture(frame.path)
    UI.setSpriteFrame(elemId, handle, frame.u0, frame.v0, frame.u1, frame.v1,
        mirrored == true)
    return handle
end

-- The frame's own pixel size, and nil until its texture is actually
-- RESIDENT.
--
-- Those are two separate questions and both have to be answered here.
-- engine.getTextureSize is the readiness handshake this view has always
-- used: it answers nil until the upload lands, which is what keeps
-- reflow() retrying and what gates v.ready (and so previewManager's
-- state == "ready"). A frame knows its own SIZE from the compiled index
-- without asking the engine anything — but the index says nothing about
-- whether the sheet has uploaded, so answering from it alone would
-- report a laid-out, ready view over a texture that is not there yet: a
-- blank panel, and a probe free to race its assertions. So ask the
-- engine first, then take the DIMENSIONS from the index, because the
-- resident image is the whole sheet and its size is not the frame's.
local function frameSize(frame, handle)
    if not handle then return nil end
    local resident = engine.getTextureSize(handle)
    if not resident or not resident.width or not resident.height
        or resident.width <= 0 or resident.height <= 0 then
        return nil
    end
    if not (frame and frame.width and frame.height) then return nil end
    return { width = frame.width, height = frame.height }
end

-- Fit (w,h) inside (boxW,boxH) preserving aspect ratio, CENTERED, at
-- 'multiplier' times the fitted scale — the same rule (and since #1907
-- the same implementation) previewManager and the buildings viewer use.
-- Requirement 3: nearest-neighbour is forced session-wide by
-- previewManager.init; aspect ratio and containment come from here.
--
-- The direction ROW passes multiplier 1 deliberately: #1907 zooms the
-- enlarged direction only, and cells keep their existing fixed sizing.
local function fitRect(box, w, h, multiplier)
    return previewZoom.fitRect(box, w, h, multiplier or previewZoom.MAX)
end

-----------------------------------------------------------
-- Geometry
-----------------------------------------------------------

-- Split the panel into the enlarged region (top) and the direction row
-- (bottom). Every dimension is floored to at least 1 so a degenerate
-- panel (a heavily shrunk preview window) still produces valid, never
-- inverted, geometry — the #748 responsive convention.
local function layout(v)
    local p = v.panel
    local uiscale = v.uiscale
    local gap = math.max(2, math.floor(8 * uiscale))
    local labelH = math.max(8, math.floor(14 * uiscale))
    local cells = math.max(1, #v.cells)

    -- The row must fit both its own height budget and the panel width.
    local byWidth = math.floor((p.width - gap * (cells + 1)) / cells)
    local byHeight = math.floor(p.height * 0.25) - labelH - gap
    local cellSize = math.max(8, math.min(byWidth, byHeight))
    local rowH = cellSize + labelH + gap

    local rowY = p.y + math.max(0, p.height - rowH)
    local rowW = cells * cellSize + gap * (cells - 1)
    local rowX = p.x + math.max(0, math.floor((p.width - rowW) / 2))

    return {
        gap = gap,
        labelH = labelH,
        cellSize = cellSize,
        rowX = rowX,
        rowY = rowY,
        enlarged = {
            x = p.x,
            y = p.y,
            width = math.max(1, p.width),
            height = math.max(1, rowY - p.y - gap),
        },
    }
end

-----------------------------------------------------------
-- Creation / teardown
-----------------------------------------------------------

-- params:
--   page, font, panel = {x,y,width,height}
--   requestTexture = function(path) -> textureHandle  (the owner's
--     cache + trimmed-loading bookkeeping; called once per frame path)
--   chromeTexture  = highlight.png handle, for the selected-cell marker
--   zoom = initial zoom multiplier (#1907); the OWNER holds the live
--     value (it survives an animation/direction change and a
--     resize), this view only renders at whatever it was last told.
--   uiscale, zIndex
function unitAnimationView.new(params)
    local id = nextId
    nextId = nextId + 1

    views[id] = {
        id = id,
        page = params.page,
        font = params.font,
        panel = params.panel,
        requestTexture = params.requestTexture,
        chromeTexture = params.chromeTexture,
        zoom = previewZoom.clamp(params.zoom),
        uiscale = params.uiscale or scale.get(),
        zIndex = params.zIndex or 1,
        anim = nil,          -- the PreviewAnim table from getPreviewBrowse
        animStart = nil,     -- wall-clock second the current clip began
        direction = nil,     -- enlarged direction (long folder spelling)
        cells = {},          -- per-direction element + playback state
        enlargedId = nil,
        frameIndex = 0,      -- the ENLARGED direction's index (dump field)
        fitKey = nil,        -- guards redundant geometry writes
        ready = false,
    }
    return id
end

local function destroyCells(v)
    for _, c in ipairs(v.cells) do
        if c.hitId then UI.deleteElement(c.hitId) end
        if c.spriteId then UI.deleteElement(c.spriteId) end
        if c.markerId then UI.deleteElement(c.markerId) end
        if c.labelId then UI.deleteElement(c.labelId) end
    end
    v.cells = {}
end

function unitAnimationView.destroy(id)
    local v = views[id]
    if not v then return end
    destroyCells(v)
    if v.enlargedId then UI.deleteElement(v.enlargedId) end
    views[id] = nil
end

-----------------------------------------------------------
-- Selection
-----------------------------------------------------------

local function buildCells(v)
    destroyCells(v)
    if not v.anim then return end

    for i, d in ipairs(v.anim.directions or {}) do
        local name = "preview_dir_" .. v.id .. "_" .. i

        local markerId = UI.newSprite(name .. "_marker", 1, 1,
            v.chromeTexture, 0.3, 0.5, 0.8, 0.8, v.page)
        UI.addToPage(v.page, markerId, 0, 0)
        UI.setZIndex(markerId, v.zIndex)
        UI.setVisible(markerId, false)

        local spriteId = UI.newSprite(name .. "_frame", 1, 1,
            v.requestTexture(d.frames[1].path), 1.0, 1.0, 1.0, 1.0, v.page)
        UI.addToPage(v.page, spriteId, 0, 0)
        UI.setZIndex(spriteId, v.zIndex + 1)
        -- Requirement 4: a mirrored cell must actually LOOK mirrored,
        -- not merely report a flag — the mirror rides in the same
        -- setSpriteFrame transition as the sub-rect, and reflects
        -- across the FRAME's own rect (#1259 generalized #887's
        -- flip-the-clipped-slice rule): reflecting across the whole
        -- image would land in a different atlas cell.
        applyFrame(v, spriteId, d.frames[1], d.mirrored)

        local labelId = UI.newText(name .. "_label",
            SHORT_NAME[d.direction] or d.direction, v.font,
            math.max(8, math.floor(14 * v.uiscale)),
            1.0, 1.0, 1.0, 1.0, v.page)
        UI.addToPage(v.page, labelId, 0, 0)
        UI.setZIndex(labelId, v.zIndex + 2)

        local hitId = UI.newSprite(name .. "_hit", 1, 1,
            v.chromeTexture, 0.0, 0.0, 0.0, 0.0, v.page)
        UI.addToPage(v.page, hitId, 0, 0)
        UI.setZIndex(hitId, v.zIndex + 3)
        UI.setClickable(hitId, true)
        UI.setOnClick(hitId, CELL_CALLBACK)

        table.insert(v.cells, {
            direction = d.direction,
            source = d.source,
            mirrored = d.mirrored == true,
            frames = d.frames,
            frameIndex = -1,
            markerId = markerId,
            spriteId = spriteId,
            labelId = labelId,
            hitId = hitId,
        })
    end
end

local function findCell(v, direction)
    for _, c in ipairs(v.cells) do
        if c.direction == direction then return c end
    end
    return nil
end

-- Which direction to enlarge when the requested one isn't available for
-- this animation: south when it exists (Requirement 2's default), else
-- the first cell — never nil while any cell exists.
local function resolveDirection(v, wanted)
    if wanted and findCell(v, wanted) then return wanted end
    if findCell(v, "south") then return "south" end
    local first = v.cells[1]
    return first and first.direction or nil
end

-- Select an animation. ALWAYS resets the playback clock (a new clip
-- starts at frame zero). 'keepDirection' carries the currently enlarged
-- direction across, so switching animations doesn't silently snap the
-- user back to south when the new clip also has their direction.
function unitAnimationView.setAnimation(id, anim, now, keepDirection)
    local v = views[id]
    if not v then return end
    v.anim = anim
    v.animStart = now
    v.ready = false
    v.fitKey = nil
    v.frameIndex = 0
    buildCells(v)
    v.direction = resolveDirection(v, keepDirection or v.direction)

    if not v.enlargedId then
        v.enlargedId = UI.newSprite("preview_unit_enlarged", 1, 1,
            v.chromeTexture, 1.0, 1.0, 1.0, 1.0, v.page)
        UI.addToPage(v.page, v.enlargedId, 0, 0)
        UI.setZIndex(v.enlargedId, v.zIndex)
    end
    UI.setVisible(v.enlargedId, v.direction ~= nil)
    unitAnimationView.reflow(id)
end

-- Enlarge a different direction. Deliberately does NOT touch animStart:
-- the row must keep playing through the change (#887 amendment).
function unitAnimationView.setDirection(id, direction)
    local v = views[id]
    if not v or not findCell(v, direction) then return false end
    v.direction = direction
    v.fitKey = nil
    unitAnimationView.reflow(id)
    return true
end

-- Move through the direction row in its rendered order, wrapping at
-- both ends (#2026). v.cells includes mirrored directions as first-class
-- displayed cells, so this visits exactly what the owner can see rather
-- than only authored source directions. Route through setDirection just
-- like a cell click: the animation clock and zoom multiplier stay put.
function unitAnimationView.selectAdjacentDirection(id, step)
    local v = views[id]
    if not v or (step ~= -1 and step ~= 1) or #v.cells == 0
        or not v.direction then
        return false
    end

    local current = nil
    for i, c in ipairs(v.cells) do
        if c.direction == v.direction then
            current = i
            break
        end
    end
    if not current then return false end

    local target = ((current - 1 + step) % #v.cells) + 1
    return unitAnimationView.setDirection(id, v.cells[target].direction)
end

function unitAnimationView.setPanel(id, panel)
    local v = views[id]
    if not v then return end
    v.panel = panel
    v.fitKey = nil
    unitAnimationView.reflow(id)
end

-- #1907. Deliberately does NOT touch animStart or the selection: zoom
-- follows the preview OBJECT (this unit), so it survives an animation
-- change, a direction change, playback and a resize, and only a new
-- preview session resets it.
function unitAnimationView.setZoom(id, multiplier)
    local v = views[id]
    if not v then return end
    v.zoom = previewZoom.clamp(multiplier)
    v.fitKey = nil
    unitAnimationView.reflow(id)
end

-- The rect the wheel zooms over, and the fit denominator the enlarged
-- sprite is sized against: layout()'s ENLARGED sub-rect, never the
-- whole panel. The panel also holds the direction row, which #1907
-- Requirement 3 forbids the enlarged sprite from overlapping and
-- Requirement 8 keeps at its existing fixed cell sizing.
function unitAnimationView.getZoomRegion(id)
    local v = views[id]
    if not v or not v.panel then return nil end
    return layout(v).enlarged
end

-----------------------------------------------------------
-- Geometry application
-----------------------------------------------------------

-- Recompute every rect from the panel. Texture uploads are async, so a
-- sprite whose natural size isn't known yet temporarily fills its cell;
-- 'v.fitKey' is only latched once EVERY sprite (cells and the enlarged
-- one) has a real size, which is what makes update() keep retrying
-- until the whole view is genuinely laid out rather than stopping as
-- soon as the first texture happens to land.
function unitAnimationView.reflow(id)
    local v = views[id]
    if not v or not v.panel then return end
    local g = layout(v)
    local allResolved = true

    for i, c in ipairs(v.cells) do
        local cx = g.rowX + (i - 1) * (g.cellSize + g.gap)
        local cy = g.rowY + g.labelH
        c.bounds = { x = cx, y = cy, w = g.cellSize, h = g.cellSize }

        UI.setSize(c.markerId, g.cellSize, g.cellSize)
        UI.setPosition(c.markerId, cx, cy)
        UI.setVisible(c.markerId, c.direction == v.direction)

        UI.setSize(c.hitId, g.cellSize, g.cellSize)
        UI.setPosition(c.hitId, cx, cy)

        UI.setPosition(c.labelId, cx, g.rowY + g.labelH - math.floor(g.labelH / 4))

        -- Cell sprites fit their own square; an unresolved texture size
        -- (the upload hasn't landed yet) just fills the cell until the
        -- next reflow picks up the real dimensions.
        local frameIdx = c.frameIndex >= 0 and c.frameIndex or 0
        local frame = c.frames[math.min(#c.frames, frameIdx + 1)]
        local size = frameSize(frame, v.requestTexture(frame.path))
        local rect = size and fitRect(
            { x = cx, y = cy, width = g.cellSize, height = g.cellSize },
            size.width, size.height)
        if rect then
            UI.setSize(c.spriteId, rect.width, rect.height)
            UI.setPosition(c.spriteId, rect.x, rect.y)
        else
            allResolved = false
            UI.setSize(c.spriteId, g.cellSize, g.cellSize)
            UI.setPosition(c.spriteId, cx, cy)
        end
    end

    local cell = v.direction and findCell(v, v.direction)
    if v.enlargedId and cell then
        local frameIdx = cell.frameIndex >= 0 and cell.frameIndex or 0
        local frame = cell.frames[math.min(#cell.frames, frameIdx + 1)]
        -- Push the frame unconditionally: setDirection routes through
        -- here, and its new direction's current frame must appear at
        -- once rather than waiting for the next index change.
        local handle = applyFrame(v, v.enlargedId, frame, cell.mirrored)
        local size = frameSize(frame, handle)
        local rect = size and fitRect(g.enlarged, size.width, size.height,
                                      v.zoom)
        if rect then
            UI.setSize(v.enlargedId, rect.width, rect.height)
            UI.setPosition(v.enlargedId, rect.x, rect.y)
            v.ready = true
        else
            allResolved = false
        end
    end

    -- Latch only on a fully-resolved layout; otherwise leave fitKey nil
    -- so update() retries next tick instead of freezing a placeholder.
    v.fitKey = allResolved
        and (tostring(v.direction) .. "|" .. tostring(v.panel.width)
             .. "x" .. tostring(v.panel.height)
             .. "@" .. tostring(v.zoom))
        or nil
end

-----------------------------------------------------------
-- Playback
-----------------------------------------------------------

-- Advance every cell (and the enlarged sprite) to the frame 'now'
-- implies. Cheap on a steady tick: a sprite's texture is only rewritten
-- when its index actually changed.
function unitAnimationView.update(id, now)
    local v = views[id]
    if not v or not v.anim or not v.animStart then return end

    local elapsed = now - v.animStart
    local fps = v.anim.fps or 8.0
    -- The clip's own authored value, passed through unchanged: the
    -- preview replays regardless (#1833), and dump() still reports it.
    local srcLoop = v.anim.loop ~= false

    for _, c in ipairs(v.cells) do
        local idx = frameIndexAt(srcLoop, fps, #c.frames, elapsed)
        if idx ~= c.frameIndex then
            c.frameIndex = idx
            applyFrame(v, c.spriteId, c.frames[idx + 1], c.mirrored)
            if c.direction == v.direction and v.enlargedId then
                v.frameIndex = idx
                applyFrame(v, v.enlargedId, c.frames[idx + 1], c.mirrored)
                -- An atlas animation's cells are uniform by
                -- construction, so frame zero's dimensions do hold for
                -- the whole clip — but refit on the frame change
                -- anyway: this costs nothing, and it is what keeps the
                -- view correct if a future storage ever relaxes that.
                v.fitKey = nil
            end
        elseif c.direction == v.direction then
            v.frameIndex = idx
        end
    end

    -- Also retries the aspect-preserving fit until the textures are
    -- actually uploaded (engine.getTextureSize only answers once they are).
    if not v.fitKey then unitAnimationView.reflow(id) end
end

-----------------------------------------------------------
-- Input
-----------------------------------------------------------

function unitAnimationView.isCellCallback(callbackName)
    return callbackName == CELL_CALLBACK
end

-- Resolve a clicked element handle to its direction and enlarge it.
-- Returns the direction name on a hit, nil when the handle isn't ours.
function unitAnimationView.handleCellClick(id, elemHandle)
    local v = views[id]
    if not v then return nil end
    for _, c in ipairs(v.cells) do
        if c.hitId == elemHandle then
            unitAnimationView.setDirection(id, c.direction)
            return c.direction
        end
    end
    return nil
end

-----------------------------------------------------------
-- Introspection (#887 Requirement 8)
-----------------------------------------------------------

function unitAnimationView.dump(id)
    local v = views[id]
    if not v then return nil end
    local cell = v.direction and findCell(v, v.direction)
    local dirs = {}
    for _, c in ipairs(v.cells) do
        -- Bounds come from UI.getElementInfo, not this module's own
        -- layout arithmetic (scripts/ui/list.lua's F3 dump does the
        -- same): the engine is the authority on where the hit box
        -- actually is, so a probe clicking these coordinates exercises
        -- the real element rather than a self-reported guess that a
        -- geometry bug could leave stale.
        local info = UI.getElementInfo(c.hitId)
        local shown = c.frames[math.max(1, c.frameIndex + 1)]
        table.insert(dirs, {
            direction = c.direction,
            source = c.source,
            mirrored = c.mirrored,
            frameIndex = c.frameIndex,
            frameCount = #c.frames,
            handle = c.hitId,
            -- The frame this cell is actually SHOWING (#1260): the
            -- texture it samples and the sub-rect within it, so a probe
            -- can prove the atlas path is on screen rather than trusting
            -- a storage label reported once at the animation level.
            texturePath = shown and shown.path or nil,
            uv = shown and { u0 = shown.u0, v0 = shown.v0,
                             u1 = shown.u1, v1 = shown.v1 } or nil,
            bounds = info and {
                x = info.x, y = info.y, w = info.width, h = info.height,
            } or c.bounds,
        })
    end
    local shownCell = cell and cell.frames[math.max(1, v.frameIndex + 1)]
    local out = {
        animation = v.anim and v.anim.name or nil,
        direction = v.direction,
        sourceDirection = cell and cell.source or nil,
        frameIndex = v.frameIndex,
        frameCount = cell and #cell.frames or 0,
        ready = v.ready,
        directions = dirs,
        -- Storage mode (#1260). Stated outright rather than left for a
        -- probe to infer from a path shape: `atlas` is the compiled
        -- image every frame of this clip samples and `cell` is the
        -- index's own frame geometry. Since #1261 every unit animation
        -- is atlas-backed, so this can only read "atlas" — but it stays
        -- DERIVED from what the engine actually pushed, so a missing
        -- atlas reports "legacy" and fails a probe rather than passing
        -- silently.
        storage = (v.anim and v.anim.atlas) and "atlas" or "legacy",
        atlas = v.anim and v.anim.atlas or nil,
        texturePath = shownCell and shownCell.path or nil,
        cell = (shownCell and shownCell.width)
            and { width = shownCell.width, height = shownCell.height } or nil,
    }
    -- #1907 Requirement 11: enough engine-authoritative zoom state for
    -- automated input to locate the real zoom surface and verify the
    -- result. `region` is the sub-rect the wheel zooms over AND the fit
    -- denominator (never panelBounds — see getZoomRegion); `sprite` is
    -- the enlarged element's ACTUAL rendered bounds read back from
    -- UI.getElementInfo, the same engine-is-the-authority rule the
    -- direction cells' bounds already follow, so a probe checks where
    -- the sprite really is rather than this module's own arithmetic.
    local enlargedInfo = v.enlargedId and UI.getElementInfo(v.enlargedId)
    out.zoom = {
        multiplier = v.zoom,
        min = previewZoom.MIN,
        max = previewZoom.MAX,
        region = unitAnimationView.getZoomRegion(id),
        sprite = enlargedInfo and {
            x = enlargedInfo.x, y = enlargedInfo.y,
            w = enlargedInfo.width, h = enlargedInfo.height,
        } or nil,
    }
    -- Assigned, never written as `x and y or z`: every field below can
    -- legitimately BE false, and Lua's and/or collapses that to the
    -- fallback — which reported loop=nil for a non-looping clip instead
    -- of loop=false, exactly the case a `loop: false` animation needs.
    out.mirrored = cell ~= nil and cell.mirrored == true
    if v.anim then
        out.fps = v.anim.fps
        out.loop = v.anim.loop ~= false
        out.flip = v.anim.flip == true
    end
    return out
end

return unitAnimationView
