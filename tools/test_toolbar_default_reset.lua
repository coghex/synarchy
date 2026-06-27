-- Offline regression harness for the #103 follow-up: the build tool's
-- auto-exit (selectDefaultTool) must fire the toolbar onChange so the
-- engine-side ToolMode resets to default. A bare toggle.select is
-- visual-only and would leave world.getToolMode() (and the next save)
-- reporting "build" after a placement/cancel/Escape.
--
-- This stubs the UI/engine globals toggle.lua touches, rebuilds the real
-- toolbar group (mine / build / default+info-option), and asserts that
-- toggle.applyOptionByName("tool_default") — what hud.selectDefaultTool
-- now calls — fires onChange with "tool_default".
--
-- Run from the repo root: luajit tools/test_toolbar_default_reset.lua

local _sid = 0
UI = setmetatable({}, { __index = function() return function()
  _sid = _sid + 1; return _sid end end })
engine = setmetatable({}, { __index = function() return function() end end })

package.path = "./?.lua;" .. package.path
package.loaded["scripts.ui.scale"] = { get = function() return 1 end }
local toggle = require("scripts.ui.toggle")

local changes = {}
local function mkGroup()
  changes = {}
  return toggle.new({
    page = "p", items = {
      { name = "tool_mine",    texDefault = "m", texSelected = "ms" },
      { name = "tool_build",   texDefault = "b", texSelected = "bs" },
      { name = "tool_default", texDefault = "d", texSelected = "ds",
        options = { { name = "tool_info", texDefault = "i", texSelected = "is" } } },
    },
    selectedIndex = 3,
    onChange = function(idx, name) changes[#changes+1] = { idx = idx, name = name } end,
  })
end

local function assert_eq(a, b, msg)
  if a ~= b then error("FAIL: " .. msg .. " (got " .. tostring(a)
    .. ", want " .. tostring(b) .. ")") end
  print("ok: " .. msg)
end

-- Case 1: normal default slot. Pick build, then selectDefaultTool.
local g = mkGroup()
assert(toggle.applyOptionByName(g, "tool_build"), "select build")
assert_eq(changes[#changes].name, "tool_build", "build pick fires onChange tool_build")
assert(toggle.applyOptionByName(g, "tool_default"), "selectDefaultTool")
assert_eq(changes[#changes].name, "tool_default",
  "selectDefaultTool fires onChange tool_default (engine resets)")
assert_eq(toggle.getSelectedIndex(g), 3, "default slot selected")

-- Case 2: default slot currently holds the swapped-in tool_info identity
-- (user had picked the info sub-option). Exit must still reset to default.
local g2 = mkGroup()
assert(toggle.applyOptionByName(g2, "tool_info"), "pick info sub-option")
assert_eq(changes[#changes].name, "tool_info", "info pick fires onChange tool_info")
assert(toggle.applyOptionByName(g2, "tool_build"), "select build")
assert(toggle.applyOptionByName(g2, "tool_default"), "selectDefaultTool from info-swapped slot")
assert_eq(changes[#changes].name, "tool_default",
  "selectDefaultTool resets to tool_default even when slot was info")

-- Guard: the OLD behavior (bare toggle.select) fires NO onChange — the
-- exact desync this fix avoids.
local g3 = mkGroup()
toggle.applyOptionByName(g3, "tool_build")
local before = #changes
toggle.select(g3, 3)
assert_eq(#changes, before, "bare toggle.select fires no onChange (documents the bug)")

print("\nALL PASS")
