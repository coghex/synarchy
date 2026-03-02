local floraCatalog = {}
floraCatalog.species = {}

-- Helper to reduce boilerplate.
-- Every species needs at minimum: a base texture, lifecycle, one
-- phase, one cycle stage, and worldgen registration.
local function quickSpecies(args)
    local base = engine.loadTexture(args.tex)
    local fid  = flora.register(args.name, base)
    floraCatalog.species[args.name] = fid

    flora.setLifecycle(fid, args.lifecycle or "evergreen",
        args.minLife, args.maxLife, args.deathChance)

    -- Minimal phases: sprout + mature
    local sproutTex  = args.sproutTex
                       and engine.loadTexture(args.sproutTex) or base
    local matureTex  = args.matureTex
                       and engine.loadTexture(args.matureTex) or base
    flora.addPhase(fid, "sprout",  sproutTex, 0)
    flora.addPhase(fid, "matured", matureTex, args.matureAge or 360)

    -- Minimal annual cycle: just dormant + flowering
    local dormantTex = args.dormantTex
                       and engine.loadTexture(args.dormantTex) or base
    local flowerTex  = args.flowerTex
                       and engine.loadTexture(args.flowerTex) or base
    flora.addCycleStage(fid, "dormant",   0,   dormantTex)
    flora.addCycleStage(fid, "flowering", 120, flowerTex)

    -- Worldgen
    flora.registerForWorldGen(fid, args.category,
        args.minTemp,    args.maxTemp,
        args.minPrecip,  args.maxPrecip,
        args.maxSlope    or 4,
        args.density     or 0.15,
        args.footprint   or 0,
        args.idealTemp,
        args.idealPrecip,
        args.minAlt      or -100,
        args.maxAlt      or 800,
        args.idealAlt    or 50,
        args.minHumidity or 0.1,
        args.maxHumidity or 0.9,
        args.idealHumidity or 0.5)

    return fid
end

function floraCatalog.init()

    ---------------------------------------------------------
    -- TEMPERATE
    ---------------------------------------------------------

    quickSpecies({
        name = "white_oak", category = "tree",
        tex = "assets/textures/world/flora/white_oak/matured.png",
        lifecycle = "evergreen", matureAge = 720,
        minTemp = 0, maxTemp = 32, idealTemp = 18,
        minPrecip = 0.4, maxPrecip = 1.0, idealPrecip = 0.7,
        minAlt = -20, maxAlt = 500, idealAlt = 100,
        minHumidity = 0.4, maxHumidity = 0.9, idealHumidity = 0.7,
        maxSlope = 2, density = 0.25, footprint = 18,
    })

    quickSpecies({
        name = "paper_birch", category = "tree",
        tex = "assets/textures/world/flora/paper_birch/matured.png",
        lifecycle = "evergreen", matureAge = 540,
        minTemp = -5, maxTemp = 25, idealTemp = 12,
        minPrecip = 0.3, maxPrecip = 0.9, idealPrecip = 0.5,
        minAlt = 0, maxAlt = 600, idealAlt = 150,
        minHumidity = 0.3, maxHumidity = 0.8, idealHumidity = 0.5,
        maxSlope = 3, density = 0.2, footprint = 14,
    })

    quickSpecies({
        name = "common_dandelion", category = "wildflower",
        tex = "assets/textures/world/flora/common_dandelion/flowering.png",
        lifecycle = "perennial", minLife = 1080, maxLife = 2520,
        deathChance = 0.3, matureAge = 30,
        minTemp = 2, maxTemp = 35, idealTemp = 15,
        minPrecip = 0.2, maxPrecip = 0.9, idealPrecip = 0.5,
        minAlt = -50, maxAlt = 300, idealAlt = 50,
        minHumidity = 0.3, maxHumidity = 0.8, idealHumidity = 0.5,
        maxSlope = 4, density = 0.2, footprint = 0,
    })

    quickSpecies({
        name = "white_clover", category = "wildflower",
        tex = "assets/textures/world/flora/white_clover/flowering.png",
        lifecycle = "perennial", minLife = 720, maxLife = 1800,
        deathChance = 0.4, matureAge = 20,
        minTemp = 5, maxTemp = 28, idealTemp = 16,
        minPrecip = 0.3, maxPrecip = 0.8, idealPrecip = 0.6,
        minAlt = -30, maxAlt = 250, idealAlt = 40,
        minHumidity = 0.4, maxHumidity = 0.8, idealHumidity = 0.6,
        maxSlope = 4, density = 0.18, footprint = 0,
    })

    ---------------------------------------------------------
    -- BOREAL / COLD
    ---------------------------------------------------------

    quickSpecies({
        name = "scots_pine", category = "tree",
        tex = "assets/textures/world/flora/scots_pine/matured.png",
        lifecycle = "evergreen", matureAge = 1080,
        minTemp = -15, maxTemp = 18, idealTemp = 5,
        minPrecip = 0.2, maxPrecip = 0.8, idealPrecip = 0.4,
        minAlt = 50, maxAlt = 700, idealAlt = 300,
        minHumidity = 0.2, maxHumidity = 0.7, idealHumidity = 0.4,
        maxSlope = 3, density = 0.3, footprint = 12,
    })

    quickSpecies({
        name = "white_spruce", category = "tree",
        tex = "assets/textures/world/flora/white_spruce/matured.png",
        lifecycle = "evergreen", matureAge = 1080,
        minTemp = -20, maxTemp = 15, idealTemp = 2,
        minPrecip = 0.3, maxPrecip = 0.9, idealPrecip = 0.5,
        minAlt = 100, maxAlt = 800, idealAlt = 400,
        minHumidity = 0.3, maxHumidity = 0.8, idealHumidity = 0.5,
        maxSlope = 3, density = 0.3, footprint = 10,
    })

    ---------------------------------------------------------
    -- TROPICAL / WARM
    ---------------------------------------------------------

    quickSpecies({
        name = "coconut_palm", category = "tree",
        tex = "assets/textures/world/flora/coconut_palm/matured.png",
        lifecycle = "evergreen", matureAge = 900,
        minTemp = 20, maxTemp = 45, idealTemp = 30,
        minPrecip = 0.5, maxPrecip = 1.0, idealPrecip = 0.8,
        minAlt = -20, maxAlt = 200, idealAlt = 20,
        minHumidity = 0.5, maxHumidity = 1.0, idealHumidity = 0.8,
        maxSlope = 2, density = 0.2, footprint = 16,
    })

    quickSpecies({
        name = "red_mangrove", category = "tree",
        tex = "assets/textures/world/flora/red_mangrove/matured.png",
        lifecycle = "evergreen", matureAge = 720,
        minTemp = 18, maxTemp = 40, idealTemp = 28,
        minPrecip = 0.6, maxPrecip = 1.0, idealPrecip = 0.9,
        minAlt = -30, maxAlt = 30, idealAlt = 0,
        minHumidity = 0.7, maxHumidity = 1.0, idealHumidity = 0.9,
        maxSlope = 1, density = 0.2, footprint = 14,
    })

    ---------------------------------------------------------
    -- ARID / DRY
    ---------------------------------------------------------

    ---------------------------------------------------------
    -- ALPINE / HIGH ELEVATION
    ---------------------------------------------------------

    ---------------------------------------------------------
    -- WETLAND
    ---------------------------------------------------------

    quickSpecies({
        name = "weeping_willow", category = "tree",
        tex = "assets/textures/world/flora/weeping_willow/matured.png",
        lifecycle = "evergreen", matureAge = 900,
        minTemp = 2, maxTemp = 30, idealTemp = 16,
        minPrecip = 0.6, maxPrecip = 1.0, idealPrecip = 0.9,
        minAlt = -40, maxAlt = 150, idealAlt = 10,
        minHumidity = 0.7, maxHumidity = 1.0, idealHumidity = 0.9,
        maxSlope = 2, density = 0.15, footprint = 16,
    })

    engine.logInfo("Flora catalog loaded: " .. tostring(21) .. " species")
end

return floraCatalog
