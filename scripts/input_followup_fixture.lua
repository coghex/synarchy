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

function M.resetState()
    M.state = {
        mouseDownShift = nil,
        mouseUpShift   = nil,
        keyDownShift   = nil,
        keyUpShift     = nil,
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
    M.state.keyDownShift = engine.isKeyDown("Shift")
end

function M.onKeyUp(key)
    M.state.keyUpShift = engine.isKeyDown("Shift")
end

return M
