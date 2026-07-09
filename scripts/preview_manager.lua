-- Preview mode entry point (#632, Phase 1 of the --preview texture
-- browser epic #427).
--
-- Loaded by scripts/init.lua's game.init in place of the ~25 normal
-- gameplay/UI scripts whenever engine.getBootProfile() == "preview".
-- Phase 1 only proves the boot skeleton + trimmed font pipeline: load a
-- font, show one placeholder label naming the requested preview target,
-- on the grey (#828382) clear color that
-- Engine.Graphics.Vulkan.Command.Record already special-cases for the
-- BootPreview profile. Phase 2 replaces this placeholder with the real
-- vertical asset list — this module deliberately does NOT load any
-- data/*.yaml, HUD/world-structural textures, or browser-chrome
-- (ui/box, ui/button) textures; nothing here consumes them yet.
local label = require("scripts.ui.label")

local previewManager = {}

local FONT_SIZE = 24
local labelFont = nil
local page = nil
local labelId = nil

local function targetText()
    local target = engine.getPreviewTarget()
    if not target then
        return "Preview: (no target)"
    elseif target.item then
        return "Preview: " .. target.category .. "/" .. target.item
    else
        return "Preview: " .. target.category
    end
end

function previewManager.init(scriptId)
    labelFont = engine.loadFont("assets/fonts/arcade.ttf", FONT_SIZE)
end

-- Fonts load asynchronously (engine.loadFont just requests the load);
-- the label can only be created once the engine broadcasts it ready.
function previewManager.onAssetLoaded(assetType, handle, path)
    if assetType == "font" and handle == labelFont and not page then
        page = UI.newPage("preview_manager", "menu")
        labelId = label.new({
            name     = "preview_target_label",
            text     = targetText(),
            font     = labelFont,
            fontSize = FONT_SIZE,
            color    = {1.0, 1.0, 1.0, 1.0},
            page     = page,
            x        = 40,
            y        = 40,
        })
        UI.showPage(page)
    end
end

function previewManager.update(dt)
end

function previewManager.shutdown()
    if labelId then
        label.destroy(labelId)
        labelId = nil
    end
    if page then
        UI.deletePage(page)
        page = nil
    end
end

return previewManager
