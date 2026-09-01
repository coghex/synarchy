-- World Generation Logic for Create World Menu
-- Handles kicking off world generation, polling for completion,
-- and reading back widget values from the tab modules.
local advancedTab  = require("scripts.create_world.advanced_tab")
local generalTab   = require("scripts.create_world.general_tab")
local timelineTab  = require("scripts.create_world.timeline_tab")
local nameSuggest  = require("scripts.create_world.name_suggest")
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
    if advVals.erosionIntensity then
        menu.pending.erosionIntensity = advVals.erosionIntensity
    end
    if advVals.volcanicActivity then
        menu.pending.volcanicActivity = advVals.volcanicActivity
    end
    if advVals.waterfallQuantum then
        menu.pending.waterfallQuantum = advVals.waterfallQuantum
    end

    local genVals = generalTab.getWidgetValues()
    for k, v in pairs(genVals) do menu.pending[k] = v end

    local tlVals = timelineTab.getWidgetValues()
    if tlVals.eonCount  then menu.pending.timelineEon = tlVals.eonCount  end
    if tlVals.eraCount  then menu.pending.timelineEra = tlVals.eraCount  end
    if tlVals.periodMin then menu.pending.periodMin   = tlVals.periodMin end
    if tlVals.periodMax then menu.pending.periodMax   = tlVals.periodMax end
    if tlVals.epochMin  then menu.pending.epochMin    = tlVals.epochMin  end
    if tlVals.epochMax  then menu.pending.epochMax    = tlVals.epochMax  end
    if tlVals.ageMin    then menu.pending.ageMin      = tlVals.ageMin    end
    if tlVals.ageMax    then menu.pending.ageMax      = tlVals.ageMax    end

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

    -- #2020: ask whether this world size's map image can exist BEFORE
    -- anything is destroyed. It has to be here rather than on
    -- world.init's return value: destroyWorld() below is irreversible,
    -- and worldView.startGeneration() may DEFER the actual world.init
    -- until textures finish loading, so a refusal arriving from there
    -- would arrive after the player's world was already gone. The verb
    -- shares world.init's planner and device ceiling, so the two cannot
    -- disagree.
    local admitted, refusal = world.checkMapImagePlan(sizeNum)
    if admitted == false then
        local message = tostring(refusal or "world size is not supported")
        engine.logWarn("Create World refused: " .. message)
        logPanel.clear(menu)
        logPanel.setStatus(menu, "Cannot generate this world")
        logPanel.addLine(menu, message)
        -- F4 (#646): the reject half of this commit boundary. Nothing
        -- was destroyed, no world command was queued, and genState stays
        -- IDLE -- the player keeps whatever world they already had.
        debug.recordOutcome{
            kind = "createWorld.generate", outcome = "rejected",
        }
        return
    end

    -- Destroy any previous world
    if worldManager.isActive() then
        worldManager.destroyWorld()
    end

    -- Push all config params to Haskell before init
    world.setGenConfig({
        world_size  = sizeNum,
        plate_count = plateNum,
        erosion_intensity = tonumber(p.erosionIntensity) or 0.7,
        volcanic_activity = tonumber(p.volcanicActivity) or 1.0,
        waterfall_quantum = math.max(1, math.floor(tonumber(p.waterfallQuantum) or 12)),
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
        timeline = {
            eon_count  = math.max(1, math.floor(tonumber(p.timelineEon) or 1)),
            era_count  = math.max(1, math.floor(tonumber(p.timelineEra) or 2)),
            period_min = math.max(1, math.floor(tonumber(p.periodMin) or 1)),
            period_max = math.max(1, math.floor(tonumber(p.periodMax) or 3)),
            epoch_min  = math.max(1, math.floor(tonumber(p.epochMin) or 1)),
            epoch_max  = math.max(1, math.floor(tonumber(p.epochMax) or 3)),
            age_min    = math.max(1, math.floor(tonumber(p.ageMin) or 1)),
            age_max    = math.max(1, math.floor(tonumber(p.ageMax) or 3)),
        },
    })

    -- #1106: a name that is still a live SUGGESTION travels with its
    -- English gloss and the #1092 provenance of the language that
    -- rendered it; a name the player typed travels alone, with all
    -- three absent (#708 principle 7 — the game never infers a meaning
    -- or an etymology for player text). nameSuggest owns that
    -- distinction; nothing here re-derives it from the name string.
    local nameGloss, langSeed, langVersion, nameExpr = nameSuggest.identity(p)

    -- Store params on worldView so textures get wired up
    local worldView = require("scripts.world_view")
    worldView.worldParams = {
        seed       = seedNum,
        worldSize  = sizeNum,
        plateCount = plateNum,
        worldName  = p.worldName,
        worldGloss = nameGloss,
        languageSeed    = langSeed,
        languageVersion = langVersion,
        -- #1104: the expression the name was rendered from, forwarded
        -- with the rest of the generated identity under the same rule —
        -- verbatim, never defaulted, absent for a player-typed name.
        nameExpr        = nameExpr,
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

    -- F4 (#646): the cold-boot session's first commit boundary. Reached
    -- only once #2020's map-image admission above has ACCEPTED this
    -- world size; the refusal path returns early with outcome
    -- "rejected" and never gets here. Everything else about the inputs
    -- (a bad seed, an out-of-range plate count) still silently falls
    -- back to a default rather than refusing, so those remain
    -- "accepted".
    debug.recordOutcome{
        kind = "createWorld.generate", outcome = "accepted",
    }
end

-----------------------------------------------------------
-- Poll (called every frame while RUNNING)
-----------------------------------------------------------

-- #997: how many locations the world just generated actually got.
-- Returns nil when the answer is not knowable (the query failed, or the
-- page id is not recorded yet) so the caller can stay silent rather
-- than guess. The overlay lives in the page's gen params, so this reads
-- correctly before the world is ever shown.
local function placedLocationCount()
    local page = worldManager.currentWorld
    if not page then return nil end
    local ok, list = pcall(world.listPlacedLocations, page)
    if not ok or type(list) ~= "table" then return nil end
    return #list
end

-- #997: is any registered location definition actually allowed to
-- place? Mirrors Location.Overlay's own `placeable` filter exactly
-- (max_count > 0) -- a definition authored max_count 0 is an explicit
-- "do not place", not a generation failure. Returns nil when the answer
-- is not knowable, so the caller can stay silent rather than guess.
local function anyPlaceableLocationDef()
    local ok, defs = pcall(engine.listLocationDefs)
    if not ok or type(defs) ~= "table" then return nil end
    for _, d in ipairs(defs) do
        if (tonumber(d.max_count) or 0) > 0 then return true end
    end
    return false
end

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

        -- #997: once generation succeeds, computeLocationPlacement can
        -- return an empty overlay for exactly two reasons -- the world
        -- has no land (NoLand), or nothing was placeable to begin with
        -- (NoPlaceableDefinitions). The guarantee rules out every other
        -- case: a world with land AND a placeable definition always gets
        -- at least one location. So "empty list AND some definition is
        -- placeable" is an unambiguous no-land signal, while an empty
        -- list with nothing placeable is a content set that has no
        -- locations to place -- not a world worth telling the player to
        -- regenerate.
        --
        -- A no-land world has no ruin to travel to, discover, extract
        -- from or return with, which leaves the expedition arc
        -- unplayable on the save. Generation itself still completed, so
        -- the normal completion controls (Regenerate / Continue) are
        -- built as usual below and Regenerate stays the obvious next
        -- step.
        if placedLocationCount() == 0 and anyPlaceableLocationDef() then
            local msg = "This world has no land, so it has no locations "
                .. "-- Regenerate with a different seed or size."
            logPanel.setStatus(menu, "World generated, but it has no locations.")
            logPanel.addLine(menu, msg)
            engine.logWarn(msg)
        end

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
