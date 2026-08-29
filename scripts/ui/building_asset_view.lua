-- Building asset viewer for --preview buildings/<name> (#888, Phase 4 of
-- the browser epic #427).
--
-- Owns everything to the RIGHT of scripts/ui/asset_browser.lua's list:
-- one sprite showing the currently selected entry, fitted to the panel
-- with aspect ratio preserved, plus the playback clock an ANIMATION
-- entry runs on. The entry MODEL (membership, ordering, which
-- directories are animations, frame order, fps/loop, default selection)
-- is resolved pre-boot by Engine.Preview.Building and arrives verbatim
-- through engine.getPreviewBrowse() — nothing here re-derives it.
--
-- Playback contract (#888 Requirement 1 + its review amendment):
--   * ONE wall clock per selected animation, so the tick rate only
--     affects smoothness, never which frame is correct.
--   * Selecting a different ENTRY resets the clock; a resize (setPanel)
--     deliberately does not — the phase survives a reflow.
--   * End-of-clip policy (#1833): the preview ALWAYS replays. Frame
--     N-1 is followed, after its own normal duration, by frame 0
--     again, indefinitely — for every clip, whatever its authored
--     `loop` says, which matters most here because buildings default
--     to loop=false (Engine.Asset.YamlBuildings' own default), unlike
--     the units viewer's loop=true. The wrap lives in the index
--     computation, never in the clock: entryStart is not restarted at
--     a cycle boundary, which is what keeps the phase across a resize.
--     The source `loop` value is still reported verbatim by dump()
--     (below); only gameplay (Unit.Render.pickFrame) still clamps.
--   * A STATIC entry has no active playback at all: one frame, no clock
--     advance (previewManager.dump() reports no `playback` for it).
local scale = require("scripts.ui.scale")

local buildingAssetView = {}

local views = {}
local nextId = 1

-- Must stay identical to Engine.Preview.Unit.frameIndexAt (and
-- scripts/ui/unit_animation_view.lua's copy of it) — the probe
-- cross-checks the dump's reported index against wall time, so a
-- divergence here shows up as a failing gate rather than silently.
-- srcLoop is the entry's AUTHORED loop value; the preview replays
-- either way (#1833), so it is taken and deliberately never read,
-- exactly as the Haskell original takes and ignores it.
local function frameIndexAt(srcLoop, fps, frameCount, elapsed)
    if frameCount <= 1 then return 0 end
    local rate = math.max(0, fps or 0)
    local raw = math.floor(math.max(0, elapsed or 0) * rate)
    return raw % frameCount
end

-- Fit (w,h) inside (boxW,boxH) preserving aspect ratio — the same rule
-- previewManager.applyTexture uses for a focused simple-category
-- texture (nearest-neighbour is forced session-wide by
-- previewManager.init).
local function fitRect(box, w, h)
    if not w or not h or w <= 0 or h <= 0 then return nil end
    local s = math.min(box.width / w, box.height / h)
    local dw, dh = w * s, h * s
    return {
        x = box.x + (box.width - dw) / 2,
        y = box.y + (box.height - dh) / 2,
        width = dw, height = dh,
    }
end

-- params:
--   page, panel = {x,y,width,height}
--   requestTexture = function(path) -> textureHandle  (the owner's
--     cache + trimmed-loading bookkeeping; called once per frame path)
--   uiscale, zIndex
function buildingAssetView.new(params)
    local id = nextId
    nextId = nextId + 1

    views[id] = {
        id = id,
        page = params.page,
        panel = params.panel,
        requestTexture = params.requestTexture,
        uiscale = params.uiscale or scale.get(),
        zIndex = params.zIndex or 1,
        entry = nil,       -- the PreviewBuildingEntry table from getPreviewBrowse
        entryStart = nil,  -- wall-clock second the current clip began
        frameIndex = 0,
        spriteId = nil,
        fitKey = nil,      -- guards redundant geometry writes
        ready = false,
    }
    return id
end

function buildingAssetView.destroy(id)
    local v = views[id]
    if not v then return end
    if v.spriteId then UI.deleteElement(v.spriteId) end
    views[id] = nil
end

-- Recompute the sprite's texture and rect from the panel. Texture
-- uploads are async, so an unresolved size leaves fitKey nil and
-- update() retries next tick instead of freezing a placeholder.
function buildingAssetView.reflow(id)
    local v = views[id]
    if not v or not v.panel or not v.entry then return end
    local frames = v.entry.frames or {}
    local path = frames[math.min(#frames, v.frameIndex + 1)]
    if not path then return end

    local handle = v.requestTexture(path)
    if not v.spriteId then
        v.spriteId = UI.newSprite("preview_building_sprite", 1, 1,
            handle, 1.0, 1.0, 1.0, 1.0, v.page)
        UI.addToPage(v.page, v.spriteId, 0, 0)
        UI.setZIndex(v.spriteId, v.zIndex)
    else
        -- Pushed unconditionally: a selection change must appear at once
        -- rather than waiting for the next frame-index change.
        UI.setSpriteTexture(v.spriteId, handle)
    end
    UI.setVisible(v.spriteId, true)

    local size = engine.getTextureSize(handle)
    local rect = size and fitRect(v.panel, size.width, size.height)
    if rect then
        UI.setSize(v.spriteId, rect.width, rect.height)
        UI.setPosition(v.spriteId, rect.x, rect.y)
        v.ready = true
        v.fitKey = tostring(v.frameIndex) .. "|" .. tostring(v.panel.width)
            .. "x" .. tostring(v.panel.height)
    else
        v.fitKey = nil
    end
end

-- Select an entry. ALWAYS resets the playback clock (a new clip starts
-- at frame zero); a static entry simply never advances from it.
function buildingAssetView.setEntry(id, entry, now)
    local v = views[id]
    if not v then return end
    v.entry = entry
    v.entryStart = now
    v.frameIndex = 0
    v.ready = false
    v.fitKey = nil
    buildingAssetView.reflow(id)
end

-- Deliberately does NOT touch entryStart: a resize must preserve the
-- playback phase (#888 amendment), same as the units viewer's own
-- setPanel.
function buildingAssetView.setPanel(id, panel)
    local v = views[id]
    if not v then return end
    v.panel = panel
    v.fitKey = nil
    buildingAssetView.reflow(id)
end

-- Advance to the frame 'now' implies. Cheap on a steady tick: the
-- sprite is only rewritten when the index actually changed (or the
-- geometry hasn't resolved yet).
function buildingAssetView.update(id, now)
    local v = views[id]
    if not v or not v.entry then return end
    if v.entry.animated and v.entryStart then
        local frames = v.entry.frames or {}
        local idx = frameIndexAt(v.entry.loop == true, v.entry.fps or 8.0,
                                 #frames, now - v.entryStart)
        if idx ~= v.frameIndex then
            v.frameIndex = idx
            v.fitKey = nil
        end
    end
    if not v.fitKey then buildingAssetView.reflow(id) end
end

-- Introspection (#888 Requirement 4): the selected entry's identity,
-- its static/animation kind, and — for an animation — the live playback
-- state (effective fps/loop and the current frame index).
function buildingAssetView.dump(id)
    local v = views[id]
    if not v or not v.entry then return nil end
    local e = v.entry
    local out = {
        entry = e.label,
        frameIndex = v.frameIndex,
        frameCount = #(e.frames or {}),
        fps = e.fps,
        ready = v.ready,
    }
    -- Assigned, never written as `x and y or z`: both fields can
    -- legitimately BE false, and Lua's and/or collapses that to the
    -- fallback (the bug #887's own dump documents).
    out.animated = e.animated == true
    out.loop = e.loop == true
    return out
end

return buildingAssetView
