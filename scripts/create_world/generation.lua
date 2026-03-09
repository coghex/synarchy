-- World Generation Logic for Create World Menu
-- Handles kicking off world generation, polling for completion,
-- and reading back widget values from the tab modules.
local advancedTab  = require("scripts.create_world.advanced_tab")
local generalTab   = require("scripts.create_world.general_tab")
local climateTab   = require("scripts.create_world.climate_tab")
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
    -- Read latest widget values from all tabs
    local advVals = advancedTab.getWidgetValues()
    if advVals.plateCount then
        menu.pending.plateCount = advVals.plateCount
    end

    local genVals = generalTab.getWidgetValues()
    for k, v in pairs(genVals) do menu.pending[k] = v end

    local clVals = climateTab.getWidgetValues()
    for k, v in pairs(clVals) do menu.pending[k] = v end

    local p = menu.pending

    local seedNum  = tonumber(p.seed, 16) or 0
    local sizeNum  = tonumber(p.worldSize) or 128
    local plateNum = tonumber(p.plateCount) or 10
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

    -- Push all config params to Haskell before init
    world.setGenConfig({
        world_size  = sizeNum,
        plate_count = plateNum,
        calendar = {
            days_per_month   = tonumber(p.daysPerMonth) or 30,
            months_per_year  = tonumber(p.monthsPerYear) or 12,
            hours_per_day    = tonumber(p.hoursPerDay) or 24,
            minutes_per_hour = tonumber(p.minutesPerHour) or 60,
        },
        sun = {
            tilt_angle = tonumber(p.tiltAngle) or 0.4,
            day_length = tonumber(p.dayLength) or 0.5,
        },
        moon = {
            cycle_days   = tonumber(p.cycleDays) or 28,
            phase_offset = tonumber(p.phaseOffset) or 0.0,
        },
        climate = {
            iterations       = tonumber(p.climateIterations) or 50,
            coriolis_scale   = tonumber(p.coriolisScale) or 1.0,
            wind_drag        = tonumber(p.windDrag) or 0.3,
            thermal_inertia  = tonumber(p.thermalInertia) or 0.7,
            orographic_scale = tonumber(p.orographicScale) or 1.5,
            evap_scale       = tonumber(p.evapScale) or 1.0,
            albedo_feedback  = tonumber(p.albedoFeedback) or 0.5,
            thc_threshold    = tonumber(p.thcThreshold) or 1.025,
        },
    })

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

function generation.poll(menu, dt, logPanel, onDone)
    if menu.genState ~= generation.RUNNING then
        return menu.genState
    end

    menu.genElapsed = menu.genElapsed + dt

    local phase, current, total = world.getInitProgress()

    if phase == 3 then
        -- LoadDone
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
