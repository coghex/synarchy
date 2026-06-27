-- Offline regression harness for issue #108: settings Revert/Back must
-- restore the LAST SAVED tooltip dwell / hint-delay values, not the
-- just-edited runtime values.
--
-- Dwell/hint are live-previewed straight to the engine, so the pre-fix
-- revert() read them back via engine.getTooltipDwellMs()/...HintDelayMs()
-- and wrote the edited value back to itself (a no-op revert). The fix
-- snapshots the saved values (mirroring savedBrightness) and restores
-- those.
--
-- Run from repo root:  luajit tools/test_settings_revert.lua
-- PASS = revert restores saved values. FAIL = pre-fix self-revert.

package.path = "./scripts/?.lua;" .. package.path

local state = {
  dwell = 400, hint = 400, brightness = 100,
  w = 1920, h = 1080, wm = "windowed", uiScale = 1.0, vs = true,
  frameLimit = 60, msaa = 1, pixelSnap = false, textureFilter = "nearest",
}

engine = {}
function engine.logInfo() end
function engine.logWarn() end
function engine.logDebug() end
function engine.getTooltipDwellMs() return state.dwell end
function engine.setTooltipDwellMs(v) state.dwell = v end
function engine.getTooltipHintDelayMs() return state.hint end
function engine.setTooltipHintDelayMs(v) state.hint = v end
function engine.setBrightness(v) state.brightness = v end
function engine.getVideoConfig()
  return state.w, state.h, state.wm, state.uiScale, state.vs,
         state.frameLimit, state.msaa, state.brightness,
         state.pixelSnap, state.textureFilter
end
function engine.saveVideoConfig() end
function engine.setResolution() end
function engine.setWindowMode() end
function engine.setUIScale() end
function engine.setVSync() end
function engine.setFrameLimit() end
function engine.setMSAA() end
function engine.setPixelSnap() end
function engine.setTextureFilter() end

local data = require("settings.data")

local failures = 0
local function check(label, got, want)
  if got ~= want then
    print(string.format("FAIL %-42s got=%s want=%s",
      label, tostring(got), tostring(want)))
    failures = failures + 1
  else
    print(string.format("PASS %-42s = %s", label, tostring(got)))
  end
end

-- Open settings (reload snapshots saved state) and edit + revert dwell.
data.reload()
local oldDwell = state.dwell
data.current.tooltipDwellMs = oldDwell + 123
engine.setTooltipDwellMs(oldDwell + 123)        -- live preview
data.revert()
check("dwell restored to saved", engine.getTooltipDwellMs(), oldDwell)

-- Same for hint delay.
data.reload()
local oldHint = state.hint
data.current.tooltipHintDelayMs = oldHint + 77
engine.setTooltipHintDelayMs(oldHint + 77)
data.revert()
check("hint delay restored to saved", engine.getTooltipHintDelayMs(), oldHint)

-- Save then edit then revert: revert must target the freshly SAVED value.
data.reload()
data.resetPending()
data.apply({ tooltipDwellMs = 500 })
data.save({})
data.apply({ tooltipDwellMs = 600 })
data.revert()
check("save-then-revert targets saved value", engine.getTooltipDwellMs(), 500)

-- Revert must leave data.current.brightness defined (not nil).
data.reload()
data.revert()
check("current.brightness defined after revert",
  data.current.brightness, state.brightness)

print("")
if failures == 0 then
  print("ALL PASS")
else
  print(failures .. " FAILURE(S)")
end
os.exit(failures == 0 and 0 or 1)
