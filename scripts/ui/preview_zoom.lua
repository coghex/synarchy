-- Centered, bounded zoom for the --preview asset panes (#1907).
--
-- The ONE place the preview's zoom limits, wheel response and
-- fit-to-region arithmetic live: scripts/preview_manager.lua (bare
-- simple-category list mode, focused-item mode, and the flora/
-- structures item folders #888 routes into the same browser),
-- scripts/ui/unit_animation_view.lua (the enlarged direction) and
-- scripts/ui/building_asset_view.lua (static entries and animations)
-- all fit through fitRect below, so a change here cannot leave one
-- pane on different arithmetic from another.
--
-- The asset viewer exists to inspect a texture's overall composition,
-- so this is deliberately NOT the gameplay camera's zoom (#596):
--   * MAX (1) is the initial multiplier AND the ceiling — the complete
--     texture aspect-fitted to as much of its region as its own aspect
--     ratio permits. There is no "zoom in past the fit", so the
--     complete texture is inside its region at every multiplier by
--     construction, never cropped and never spilling into the asset
--     list, the direction strip, or the window margin.
--   * MIN (1/8) is the floor: both rendered dimensions become exactly
--     one eighth of their fitted dimensions.
--   * Zoom is CENTERED on the region. There is no source point, no
--     anchor, and no translation state at all — the cursor's position
--     within the region cannot affect where the texture lands.
--
-- Wheel sign matches the gameplay convention rather than the list-
-- scroll one: Engine.Loop.Camera documents `dy > 0 zooms out, dy < 0
-- zooms in` (camZoom is the viewport half-height, so smaller is
-- closer), and scripts/world_view.lua feeds that same dy straight to
-- camera.applyScrollZoom. So here dy < 0 ENLARGES toward MAX and
-- dy > 0 SHRINKS toward MIN.
--
-- Delta magnitude is meaningful, not just its sign — the same decision
-- Engine.Loop.Camera.zoomScrollScale records for the camera ("Scaled by
-- the delta itself, not merely its sign"), and the reason
-- LuaUIScrollEvent carries raw doubles through to onUIScroll. A wheel
-- notch an OS splits into several smaller deltas therefore totals the
-- same as one clean delta of the same sum, because the response is
-- multiplicative: STEP^(-dy1) * STEP^(-dy2) == STEP^(-(dy1+dy2)).
local previewZoom = {}

-- Requirement 1's exact limits. MAX is both the initial and the maximum
-- multiplier; the rendered scale is always `multiplier * fit`.
previewZoom.MIN = 1 / 8
previewZoom.MAX = 1

-- Multiplier factor for one whole wheel notch (dy == -1 enlarges by
-- this, dy == 1 shrinks by it). Chosen so the full MIN..MAX range is
-- about nine notches: brisk enough to reach either limit without
-- grinding, fine enough to stop where you meant to.
local STEP = 1.25

-- Rejects nil, non-numbers, NaN and both infinities in one place, so no
-- caller below has to spell the NaN self-inequality trick again.
local function isFinite(x)
    return type(x) == "number" and x == x and x > -math.huge and x < math.huge
end

previewZoom.isFinite = isFinite

-- Clamp to [MIN, MAX]. A missing or non-finite multiplier reads as MAX
-- (the initial value), never as an unusable scale: this is the last
-- guard before the arithmetic that sizes a real element.
function previewZoom.clamp(multiplier)
    if not isFinite(multiplier) then return previewZoom.MAX end
    if multiplier < previewZoom.MIN then return previewZoom.MIN end
    if multiplier > previewZoom.MAX then return previewZoom.MAX end
    return multiplier
end

-- One wheel event's effect on the multiplier. Monotonic in dy and
-- clamped EXACTLY at both limits, so further input at a limit changes
-- nothing (the event is still consumed by the capturing surface, so it
-- never leaks into a list scroll or a camera action).
function previewZoom.step(multiplier, dy)
    local current = previewZoom.clamp(multiplier)
    if not isFinite(dy) or dy == 0 then return current end
    return previewZoom.clamp(current * (STEP ^ (-dy)))
end

-- Aspect-preserving fit of (w, h) inside 'box', scaled by 'multiplier'
-- and CENTERED — the single fit every preview pane uses.
--
-- Returns nil rather than a degenerate rect for any geometry that
-- cannot produce one: a missing box, a non-positive or non-finite box
-- extent, non-positive or non-finite source dimensions, or an
-- arithmetic result that is not finite and positive. A caller treats
-- nil the way the pre-#1907 code treated an unresolved texture size —
-- leave the previous geometry alone and retry — which is what keeps a
-- degenerate window from ever writing an inverted or negative rect.
function previewZoom.fitRect(box, w, h, multiplier)
    if type(box) ~= "table" then return nil end
    if not (isFinite(box.x) and isFinite(box.y)
            and isFinite(box.width) and isFinite(box.height)) then
        return nil
    end
    if box.width <= 0 or box.height <= 0 then return nil end
    if not (isFinite(w) and isFinite(h)) or w <= 0 or h <= 0 then return nil end

    local scale = math.min(box.width / w, box.height / h)
                  * previewZoom.clamp(multiplier)
    local dw, dh = w * scale, h * scale
    if not (isFinite(dw) and isFinite(dh)) or dw <= 0 or dh <= 0 then
        return nil
    end
    return {
        x = box.x + (box.width - dw) / 2,
        y = box.y + (box.height - dh) / 2,
        width = dw,
        height = dh,
    }
end

return previewZoom
