-- Vertical stacking geometry for the debug overlay's mode buttons and
-- open lists (#545). Each mode occupies one button row; when its list is
-- open, the list's rows push every mode below it further down the
-- screen. `anchorY` is the fixed position of the first (spawn) button —
-- nothing sits above it, so it never moves.
local layout = {}

function layout.anchorY(s)
    return s.margin + 2 * (s.fontSize + s.rowSpacing)
end

-- Y position immediately after `mode`'s button (and its list rows, if
-- open) — i.e. where the NEXT mode's button belongs.
function layout.advance(y, s, mode)
    y = y + (s.fontSize + s.rowSpacing)
    if mode.listVisible then
        local rows = math.max(1, #mode.entries)
        y = y + rows * (s.fontSize + s.rowSpacing)
    end
    return y
end

-- Button Y for every mode in `modeOrder`, keyed by mode.key. Assumes
-- each mode's entries/listVisible already reflect the state to lay out
-- (call this after building/destroying lists, not before).
function layout.computeYs(modeOrder, s)
    local y = layout.anchorY(s)
    local ys = {}
    for _, mode in ipairs(modeOrder) do
        ys[mode.key] = y
        y = layout.advance(y, s, mode)
    end
    return ys
end

-- A generous clickable rectangle covering one row of debug text at the
-- given baseline. Text renders above the baseline, so the rect's bottom
-- is anchored near baseline + a small descender allowance.
function layout.rowRect(font, s, baselineY, text)
    local w = engine.getTextWidth(font, text, s.fontSize)
    return {
        x = s.margin,
        y = baselineY - s.fontSize,
        w = w + s.fontSize,            -- a bit of slack on the right
        h = s.fontSize + s.rowSpacing, -- full row height
    }
end

return layout
