-- UI Manager: gameplay rescale (#750, split from ui_manager_boot.lua to
-- stay under its line budget — tools/lua_module_budget.py). Attaches
-- onto the same uiManager singleton every ui_manager_*.lua split module
-- shares (scripts/ui_manager.lua).
local uiManager = package.loaded["scripts.ui_manager"]

local worldView = require("scripts.world_view")
local hud = require("scripts.hud")
local contextMenu = require("scripts.ui.context_menu")
local buildToolRemoteWarning = require("scripts.build_tool_remote_warning")
local popup = require("scripts.popup")
local eventLog = require("scripts.event_log")
local combatLog = require("scripts.combat_log")
local injuryLog = require("scripts.injury_log_panel")
local unitLog = require("scripts.unit_log")
local unitInfoV2 = require("scripts.unit_info_v2")
local debugOverlay = require("scripts.debug")

-- A UI-scale-only Settings Apply/Save/Back (same framebuffer size, new
-- engine.getUIScale()) never reached gameplay — responsive.notifyResize
-- only fans out to the six C2-registered menu screens, and shell.lua
-- gets its own direct call (scripts/settings_menu.lua); hud/worldView/
-- popup/logs/unit_info_v2/debug/contextMenu/buildToolRemoteWarning had
-- no equivalent, leaving an already-open gameplay HUD at the old scale
-- until the next real resize. Unlike a real resize, there's no engine
-- broadcast for this synthetic change, so every surface needs a direct
-- call here — including the loadScript'd ones
-- uiManager.onFramebufferResize (scripts/ui_manager_boot.lua)
-- deliberately no longer forwards, since here there's no automatic
-- broadcast to double against.
function uiManager.notifyGameplayRescale(width, height)
    if width <= 0 or height <= 0 then return end
    if uiManager.moduleReady.worldView then worldView.onFramebufferResize(width, height) end
    if uiManager.moduleReady.hud then hud.onFramebufferResize(width, height) end
    contextMenu.onFramebufferResize(width, height)
    if uiManager.moduleReady.buildToolRemoteWarning then
        buildToolRemoteWarning.onFramebufferResize(width, height)
    end
    if uiManager.moduleReady.popupsAndLogs then
        popup.onFramebufferResize(width, height)
        popup.reflow()
        eventLog.onFramebufferResize(width, height)
        combatLog.onFramebufferResize(width, height)
        injuryLog.onFramebufferResize(width, height)
        unitLog.onFramebufferResize(width, height)
    end
    -- unitInfoV2.onFramebufferResize is a no-op (see unit_info_v2.lua);
    -- reflow() is the real entry point, safe to call directly here since
    -- hud.onFramebufferResize above has already run.
    unitInfoV2.reflow()
    debugOverlay.onFramebufferResize(width, height)
end
