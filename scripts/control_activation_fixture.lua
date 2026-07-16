-- Test fixture for Test.Headless.UI.ControlActivation / FocusNavigation
-- (#745) — NOT part of the game. Registers a handful of DISTINCT
-- callback names, standing in for different widget families
-- (checkbox/toggle/tab/list/dropdown all just register a plain
-- UI.setOnClick callback the same way a button does), so a keyboard
-- Enter/Space activation reaching each one via the real broadcast
-- dispatch proves the mechanism is family-agnostic — the ONLY thing
-- #745 changes about activation firing.
--
-- Singleton via package.loaded, same convention as every other script
-- module (see scripts/unit_ai.lua's header comment).
local M = package.loaded["scripts.control_activation_fixture"] or {}
package.loaded["scripts.control_activation_fixture"] = M

-- #745 review round 6: also proxy the REAL production widget modules'
-- own registered callback names (onCheckboxClick, onSliderKnobClick/
-- onSliderTrackClick, onUIStep) to their real handlers, so a test can
-- drive a genuine checkbox.new()/slider.new() instance through the
-- real keyboard-activation path and assert real state changes
-- (checkbox.isChecked, slider.getValue) — not just that a callback
-- name got invoked, per the review's ask for real widget-family
-- coverage alongside the family-agnostic mechanism test above.
local checkbox = require("scripts.ui.checkbox")
local slider = require("scripts.ui.slider")

function M.onCheckboxClick(elemHandle)
    return checkbox.handleClickByElement(elemHandle)
end

function M.resetState()
    M.state = {
        checkboxClicks = 0,
        toggleClicks   = 0,
        tabClicks      = 0,
        listClicks     = 0,
        dropdownClicks = 0,
        controlFocus   = nil,
        stepEvents     = {},
    }
end
M.resetState()

function M.onFixtureCheckboxClick(elemHandle)
    M.state.checkboxClicks = M.state.checkboxClicks + 1
end

function M.onFixtureToggleClick(elemHandle)
    M.state.toggleClicks = M.state.toggleClicks + 1
end

function M.onFixtureTabClick(elemHandle)
    M.state.tabClicks = M.state.tabClicks + 1
end

function M.onFixtureListClick(elemHandle)
    M.state.listClicks = M.state.listClicks + 1
end

function M.onFixtureDropdownClick(elemHandle)
    M.state.dropdownClicks = M.state.dropdownClicks + 1
end

function M.onUIControlFocusChanged(elemHandle)
    M.state.controlFocus = elemHandle
end

function M.onUIStep(elemHandle, direction)
    table.insert(M.state.stepEvents, { handle = elemHandle, direction = direction })
    -- No-op for a fixture-created generic element (findByElementHandle
    -- returns nil); forwards for real slider.new() instances.
    slider.onStep(elemHandle, direction)
end

return M
