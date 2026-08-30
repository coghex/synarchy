-- Test fixture for Test.Headless.Input.Followup's #727 specs — NOT
-- part of the game. Unlike scripts/input_check_fixture.lua (which
-- needs a real UI page + assets for tools/input_check.py's graphical
-- checks), this is deliberately minimal: no UI, just the input
-- broadcasts a modifier-lifetime regression needs. Each handler
-- samples engine.isKeyDown("Shift") AT CALLBACK TIME, so the test can
-- assert what a REAL Lua callback observed — the actual #727 contract
-- — rather than only the published input state a callback-less test
-- can see.
--
-- Singleton via package.loaded, same convention as every other script
-- module (see scripts/unit_ai.lua's header comment).
local M = package.loaded["scripts.input_followup_fixture"] or {}
package.loaded["scripts.input_followup_fixture"] = M

-- The flat fields report the LAST callback of their kind. That is
-- enough for a click (its modifier release fires onKeyUp, not
-- onMouseUp), but not for a key gesture: #1927's split hold broadcasts
-- onKeyUp for its primary key and then, behind the #697 fence, for the
-- modifier it releases — so a flat keyUpShift only ever reports the
-- second one. The per-key tables keep both, indexed by the merged key
-- name onKeyDown/onKeyUp receive ("W", "Shift", ...).
function M.resetState()
    M.state = {
        mouseDownShift = nil,
        mouseUpShift   = nil,
        keyDownShift   = nil,
        keyUpShift     = nil,
        keyDownShiftBy = {},
        keyUpShiftBy   = {},
    }
end
M.resetState()

function M.onMouseDown(button, x, y)
    M.state.mouseDownShift = engine.isKeyDown("Shift")
end

function M.onMouseUp(button, x, y, downRoute)
    M.state.mouseUpShift = engine.isKeyDown("Shift")
end

function M.onKeyDown(key)
    local held = engine.isKeyDown("Shift")
    M.state.keyDownShift = held
    M.state.keyDownShiftBy[key] = held
end

function M.onKeyUp(key)
    local held = engine.isKeyDown("Shift")
    M.state.keyUpShift = held
    M.state.keyUpShiftBy[key] = held
end

return M
