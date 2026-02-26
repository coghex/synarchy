-- World Generation Logic for Create World Menu
-- Handles kicking off world generation, polling for completion,
-- and reading back widget values from the tab modules.
local advancedTab  = require("scripts.create_world.advanced_tab")
local worldManager = require("scripts.world_manager")

local generation = {}

-----------------------------------------------------------
-- Generation states (mirror the constants in create_world_menu)
-----------------------------------------------------------
generation.IDLE    = "idle"
generation.RUNNING = "running"
generation.DONE    = "done"

-----------------------------------------------------------
-- Start generation
-----------------------------------------------------------

-- menu = createWorldMenu table
-- logPanel = require("scripts.create_world.log_panel")
function generation.start(menu, logPanel)
    -- Read latest advanced-tab values
    local advVals = advancedTab.getWidgetValues()
    if advVals.plateCount then
        menu.pending.plateCount = advVals.plateCount
    end

    local p = menu.pending

    local seedNum  = tonumber(p.seed, 16) or 0
    local sizeNum  = tonumber(p.worldSize) or 128
    local plateNum = tonumber(p.plateCount) or 7
    if plateNum < 2  then plateNum = 2  end
    if plateNum > 20 then plateNum = 20 end

    engine.logInfo("Generating world: name=" .. p.worldName
        .. " seed=0x" .. p.seed
        .. " size=" .. tostring(sizeNum)
        .. " plates=" .. tostring(plateNum))

    -- Destroy any previous world
    if worldManager.isActive() then
        worldManager.destroyWorld()
    end

    -- Store params on worldView so textures get wired up
    local worldView = require("scripts.world_view")
    worldView.worldParams = {
        seed       = seedNum,
        worldSize  = sizeNum,
        plateCount = plateNum,
        worldName  = p.worldName,
    }

    -- Kick off
    menu.genState   = generation.RUNNING
    menu.genElapsed = 0
    logPanel.clear(menu)
    logPanel.setStatus(menu, "Generating world...")
    logPanel.addLine(menu, "Seed: 0x" .. (p.seed ~= "" and p.seed or "0"))
    logPanel.addLine(menu, "Size: " .. tostring(sizeNum))
    logPanel.addLine(menu, "Plates: " .. tostring(plateNum))
    logPanel.addLine(menu, "")

    worldView.startGeneration()
end

-----------------------------------------------------------
-- Poll (called every frame while RUNNING)
-----------------------------------------------------------

-- Returns the new genState
function generation.poll(menu, dt, logPanel, onDone)
    if menu.genState ~= generation.RUNNING then
        return menu.genState
    end

    menu.genElapsed = menu.genElapsed + dt

    -- Check actual chunk generation progress, not just world existence
    local remaining, total = world.getInitProgress()

    if total and total > 0 and remaining == 0 then
        -- All chunks generated — truly done
        menu.genState = generation.DONE
        local elapsed = string.format("%.1f", menu.genElapsed)
        logPanel.setStatus(menu, "World generated! (" .. elapsed .. "s)")
        logPanel.addLine(menu, "Generation complete.")
        engine.logInfo("World generation complete in " .. elapsed .. "s")
        if onDone then onDone() end
        return generation.DONE
    else
        local dots = string.rep(".",
            (math.floor(menu.genElapsed * 3) % 4))
        logPanel.setStatus(menu, "Generating world" .. dots)
        return generation.RUNNING
    end
end

return generation
