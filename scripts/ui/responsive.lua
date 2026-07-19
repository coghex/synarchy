-- Responsive menu lifecycle (#748): the supported envelope definition
-- plus the single shared framebuffer/UI-scale notification contract
-- every C2 menu screen (main, pause, settings, create-world, save
-- browser, loading) adopts instead of duplicating its own resize/
-- rescale plumbing. Replaces the old per-screen copies: ui_manager_boot's
-- hand-listed onFramebufferResize fan-out and settings_menu's
-- scaleChanged-only self-rebuild. See CLAUDE.md's UI system section for
-- the full envelope/lifecycle writeup.
local responsive = {}

-----------------------------------------------------------
-- Supported envelope
-----------------------------------------------------------

-- Formal minimum framebuffer. Below this, behavior only has to fail
-- safely (no crash / invalid geometry) — never "fully supported".
responsive.MIN_WIDTH  = 800
responsive.MIN_HEIGHT = 600

-- Height band -> [minScale, maxScale] fully supported at that band.
-- Bounds are inclusive on both ends; bands are contiguous and
-- non-overlapping (600-900 / 901-1200 / 1201-1600 / 1601-2160).
responsive.bands = {
    { minH = 600,  maxH = 900,  minScale = 0.5,  maxScale = 1.0 },
    { minH = 901,  maxH = 1200, minScale = 0.75, maxScale = 2.0 },
    { minH = 1201, maxH = 1600, minScale = 1.0,  maxScale = 3.0 },
    { minH = 1601, maxH = 2160, minScale = 1.5,  maxScale = 4.0 },
}

function responsive.bandFor(height)
    for _, b in ipairs(responsive.bands) do
        if height >= b.minH and height <= b.maxH then
            return b
        end
    end
    return nil
end

-- Classify a (framebuffer width, framebuffer height, UI scale) combo.
-- Returns { supported, reason, band } — `reason` is nil when supported,
-- else one of "below_minimum" / "height_out_of_range" / "scale_out_of_band".
-- Purely informational: never mutates or clamps anything. Outside-
-- envelope combos remain allowed best-effort (#748: "never silently
-- alter stored scale").
function responsive.classify(fbW, fbH, uiscale)
    if fbW < responsive.MIN_WIDTH or fbH < responsive.MIN_HEIGHT then
        return { supported = false, reason = "below_minimum", band = nil }
    end
    local band = responsive.bandFor(fbH)
    if not band then
        return { supported = false, reason = "height_out_of_range", band = nil }
    end
    if uiscale < band.minScale or uiscale > band.maxScale then
        return { supported = false, reason = "scale_out_of_band", band = band }
    end
    return { supported = true, reason = nil, band = band }
end

function responsive.isSupported(fbW, fbH, uiscale)
    return responsive.classify(fbW, fbH, uiscale).supported
end

-----------------------------------------------------------
-- Compact-fallback primitive
--
-- A menu whose natural size scales with item count x uiscale (main/
-- pause's vertical button stack) can overflow the framebuffer at a
-- high scale or with many items — main_menu's own panel going
-- off-screen at 3840x2160@4x with 5 items is the concrete case this
-- closes. `fitScale` returns an EFFECTIVE scale (<= the given uiscale)
-- that keeps `naturalSizeAtUiscale` within `maxSize`, for a caller to
-- re-derive its own scaled sizes from (e.g. via
-- `scale.applyAllWith(baseSizes, responsive.fitScale(...))`) — never
-- the stored/configured UI scale itself, only this one screen's own
-- layout. Returns `uiscale` unchanged when it already fits.
-----------------------------------------------------------

function responsive.fitScale(naturalSizeAtUiscale, maxSize, uiscale)
    if naturalSizeAtUiscale <= maxSize or naturalSizeAtUiscale <= 0 then
        return uiscale
    end
    return uiscale * (maxSize / naturalSizeAtUiscale)
end

-----------------------------------------------------------
-- Shared notification registry
--
-- A screen registers itself once (see scripts/ui_manager_boot.lua);
-- `notifyResize` fans a framebuffer size out to every registered screen
-- uniformly, guarding the 0x0-minimize case so no screen ever rebuilds
-- degenerate geometry. Re-invoking it with the SAME size (after a
-- UI-scale-only change, e.g. Settings Apply/Save) makes every already-
-- initialized screen pick up the new scale immediately, not just
-- whichever screen triggered the change.
-----------------------------------------------------------

local registered = {}    -- name -> module (must implement onFramebufferResize(w,h))
local lastGeometry = {}  -- name -> { fbW, fbH, uiScale, classification, pendingRestore }

function responsive.register(name, mod)
    registered[name] = mod
end

function responsive.unregister(name)
    registered[name] = nil
    lastGeometry[name] = nil
end

function responsive.isRegistered(name)
    return registered[name] ~= nil
end

-- Fan a framebuffer size out to every registered screen.
--
-- 0x0 minimize (#748: "0x0 minimize never builds invalid UI and restore
-- updates"): a zero/negative dimension is never forwarded to a screen's
-- own resize handler (which would otherwise rebuild off degenerate
-- geometry) — it's only remembered as pending. The next call with a
-- real size rebuilds normally, since every screen's own
-- onFramebufferResize already rebuilds unconditionally off whatever
-- real size it's given.
function responsive.notifyResize(fbW, fbH)
    local uiscale = engine.getUIScale()
    local minimized = (fbW or 0) <= 0 or (fbH or 0) <= 0
    for name, mod in pairs(registered) do
        if minimized then
            local g = lastGeometry[name] or {}
            g.pendingRestore = true
            lastGeometry[name] = g
        else
            if mod.onFramebufferResize then
                mod.onFramebufferResize(fbW, fbH)
            end
            lastGeometry[name] = {
                fbW = fbW,
                fbH = fbH,
                uiScale = uiscale,
                classification = responsive.classify(fbW, fbH, uiscale),
                pendingRestore = false,
            }
        end
    end
end

-----------------------------------------------------------
-- Introspection (headless tests / F3-style oracles)
-----------------------------------------------------------

function responsive.getGeometry(name)
    return lastGeometry[name]
end

function responsive.dump()
    local out = {}
    for name, g in pairs(lastGeometry) do
        out[name] = g
    end
    return out
end

return responsive
