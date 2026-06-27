-- Offline regression harness for the #103 post-load toolbar reset
-- scoping + deferral. Loads the REAL scripts/hud.lua and scripts/ui/
-- toggle.lua (only stubbing the engine/UI globals + scale/info_panel),
-- wires hud.toolToggleId to a real toggle group, and drives the actual
-- hud.markLoadedToolReset / resetMainWorldToolIfDirty state machine.
--
-- Guards two review findings:
--   #5  a main_world load must NOT reset the toolbar while the HUD is
--       bound to the arena page (the toolbar onChange writes
--       world.setToolMode(hud.worldId, ...)).
--   #6  but an arena→menu→load→world_view round-trip (hud.worldId stale
--       at load time, rebinds to main_world later) must STILL reset.
--
-- Run from the repo root: luajit tools/test_load_tool_reset.lua

local _sid = 0
UI = setmetatable({}, { __index = function() return function()
  _sid = _sid + 1; return _sid end end })
engine = setmetatable({}, { __index = function() return function() end end })
world  = setmetatable({}, { __index = function() return function() end end })
camera = setmetatable({}, { __index = function() return function() end end })

package.path = "./?.lua;" .. package.path
package.loaded["scripts.ui.scale"] = { get = function() return 1 end }
package.loaded["scripts.hud.info_panel"] =
  setmetatable({}, { __index = function() return function() end end })

local toggle = require("scripts.ui.toggle")
local hud    = require("scripts.hud")

-- Real toggle group mirroring the toolbar; onChange records the picks
-- (this is the same callback the real toolbar uses to drive
-- world.setToolMode, so a recorded pick == the toolbar fired onChange).
local fired = {}
hud.toolToggleId = toggle.new({
  page = "world_page", items = {
    { name = "tool_mine",    texDefault = "m", texSelected = "ms" },
    { name = "tool_build",   texDefault = "b", texSelected = "bs" },
    { name = "tool_default", texDefault = "d", texSelected = "ds",
      options = { { name = "tool_info", texDefault = "i", texSelected = "is" } } },
  },
  selectedIndex = 3,
  onChange = function(idx, name) fired[#fired+1] = name end,
})

local function last() return fired[#fired] end
local function assert_eq(a, b, msg)
  if a ~= b then error("FAIL: " .. msg .. " (got " .. tostring(a)
    .. ", want " .. tostring(b) .. ")") end
  print("ok: " .. msg)
end

-- Case 1: HUD bound to main_world → load resets the toolbar immediately.
hud.worldId = "main_world"
hud.mainWorldToolDirty = false
assert(toggle.applyOptionByName(hud.toolToggleId, "tool_build"))
hud.markLoadedToolReset()
assert_eq(last(), "tool_default", "bound to main_world: load resets toolbar now")
assert_eq(hud.mainWorldToolDirty, false, "main_world: dirty cleared")

-- Case 2: HUD bound to the arena → a main_world load must NOT touch the
-- toolbar; the reset is deferred. (#5)
hud.worldId = "test_arena"
hud.mainWorldToolDirty = false
assert(toggle.applyOptionByName(hud.toolToggleId, "tool_build"))
local n = #fired
hud.markLoadedToolReset()
assert_eq(#fired, n, "bound to arena: load does NOT fire the toolbar onChange")
assert_eq(toggle.getSelectedIndex(hud.toolToggleId), 2, "arena: build still selected")
assert_eq(hud.mainWorldToolDirty, true, "arena: reset deferred (dirty stays set)")

-- Case 3: rebind to main_world (what hud.show does) consumes the
-- deferred reset. (#6)
hud.worldId = "main_world"
hud.resetMainWorldToolIfDirty()
assert_eq(last(), "tool_default", "rebind to main_world: deferred reset applies")
assert_eq(hud.mainWorldToolDirty, false, "rebind: dirty cleared")

-- Case 4: an idle hud.show with nothing dirty must be a no-op.
local n2 = #fired
hud.resetMainWorldToolIfDirty()
assert_eq(#fired, n2, "not dirty: resetMainWorldToolIfDirty is a no-op")

print("\nALL PASS")
