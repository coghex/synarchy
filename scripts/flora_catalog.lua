local floraCatalog = {}
floraCatalog.species = {}

-----------------------------------------------------------
-- Texture loading helper
-----------------------------------------------------------

local function T(path)
    return engine.loadTexture(path)
end

-----------------------------------------------------------
-- Deciduous tree helper
--
-- Deciduous trees are perennial (they die eventually) and
-- have a full annual cycle with per-phase overrides.
--
-- Required textures (lifecycle × season matrix):
--   {base}/sprout.png               -- sprout, default
--   {base}/sprout_dormant.png       -- sprout, winter
--   {base}/sprout_budding.png       -- sprout, spring
--   {base}/sprout_senescing.png     -- sprout, autumn
--   {base}/matured.png              -- matured, default (summer)
--   {base}/matured_dormant.png      -- matured, winter
--   {base}/matured_budding.png      -- matured, spring
--   {base}/matured_flowering.png    -- matured, full canopy
--   {base}/matured_senescing.png    -- matured, autumn
--   {base}/dead.png                 -- dead phase
-----------------------------------------------------------

local function deciduousTree(args)
    local base = args.texDir
    local baseTex = T(base .. "/matured.png")
    local fid = flora.register(args.name, baseTex)
    floraCatalog.species[args.name] = fid

    -- Lifecycle: perennial (trees die eventually)
    flora.setLifecycle(fid, "perennial",
        args.minLife or 7200, args.maxLife or 36000, args.deathChance or 0.05)

    -- Life phases
    local sproutTex  = T(base .. "/sprout.png")
    local maturedTex = T(base .. "/matured.png")
    local deadTex    = T(base .. "/dead.png")

    flora.addPhase(fid, "sprout",  sproutTex, 0)
    flora.addPhase(fid, "matured", maturedTex, args.matureAge or 720)
    flora.addPhase(fid, "dead",    deadTex,    args.deathAge or 36000)

    -- Annual cycle (these are the default textures per season)
    local dormantTex   = T(base .. "/matured_dormant.png")
    local buddingTex   = T(base .. "/matured_budding.png")
    local floweringTex = T(base .. "/matured_flowering.png")
    local senescingTex = T(base .. "/matured_senescing.png")

    flora.addCycleStage(fid, "dormant",   0,   dormantTex)
    flora.addCycleStage(fid, "budding",   60,  buddingTex)
    flora.addCycleStage(fid, "flowering", 120, floweringTex)
    flora.addCycleStage(fid, "senescing", 270, senescingTex)

    -- Sprout-phase cycle overrides (sapling looks different each season)
    local sprDormant  = T(base .. "/sprout_dormant.png")
    local sprBudding  = T(base .. "/sprout_budding.png")
    local sprSenescing = T(base .. "/sprout_senescing.png")

    flora.addCycleOverride(fid, "sprout", "dormant",   sprDormant)
    flora.addCycleOverride(fid, "sprout", "budding",   sprBudding)
    -- sprout + flowering: no override, use default flowering tex (fine for sapling)
    flora.addCycleOverride(fid, "sprout", "senescing", sprSenescing)

    -- Dead phase ignores annual cycle (dead trees don't change with seasons)
    -- No overrides needed  resolveFloraTexture uses the phase tex
    -- when the cycle stage has no override, but dead trees should
    -- always show the dead texture regardless of season.
    flora.addCycleOverride(fid, "dead", "dormant",   deadTex)
    flora.addCycleOverride(fid, "dead", "budding",   deadTex)
    flora.addCycleOverride(fid, "dead", "flowering", deadTex)
    flora.addCycleOverride(fid, "dead", "senescing", deadTex)

    -- Worldgen
    flora.registerForWorldGen(fid, "tree",
        args.minTemp,    args.maxTemp,
        args.minPrecip,  args.maxPrecip,
        args.maxSlope    or 2,
        args.density     or 0.2,
        args.footprint   or 16,
        args.idealTemp,
        args.idealPrecip,
        args.minAlt      or -20,
        args.maxAlt      or 500,
        args.idealAlt    or 100,
        args.minHumidity or 0.3,
        args.maxHumidity or 0.9,
        args.idealHumidity or 0.6)

    return fid
end

-----------------------------------------------------------
-- Evergreen tree helper
--
-- Evergreens don't change with seasons. They only need
-- life-phase textures: sprout, matured.
--
-- Required textures:
--   {base}/sprout.png
--   {base}/matured.png
-----------------------------------------------------------

local function evergreenTree(args)
    local base = args.texDir
    local baseTex = T(base .. "/matured.png")
    local fid = flora.register(args.name, baseTex)
    floraCatalog.species[args.name] = fid

    flora.setLifecycle(fid, "evergreen")

    local sproutTex  = T(base .. "/sprout.png")
    local maturedTex = T(base .. "/matured.png")

    flora.addPhase(fid, "sprout",  sproutTex, 0)
    flora.addPhase(fid, "matured", maturedTex, args.matureAge or 1080)

    -- No annual cycle — evergreens look the same year-round

    flora.registerForWorldGen(fid, "tree",
        args.minTemp,    args.maxTemp,
        args.minPrecip,  args.maxPrecip,
        args.maxSlope    or 3,
        args.density     or 0.25,
        args.footprint   or 12,
        args.idealTemp,
        args.idealPrecip,
        args.minAlt      or 0,
        args.maxAlt      or 700,
        args.idealAlt    or 300,
        args.minHumidity or 0.2,
        args.maxHumidity or 0.8,
        args.idealHumidity or 0.5)

    return fid
end

-----------------------------------------------------------
-- Perennial wildflower helper
--
-- Small plants with a full annual cycle.
-- No per-phase cycle overrides needed (one visual size).
--
-- Required textures:
--   {base}/sprout.png
--   {base}/dormant.png
--   {base}/budding.png
--   {base}/flowering.png
--   {base}/senescing.png
--   {base}/dead.png
-----------------------------------------------------------

local function perennialFlower(args)
    local base = args.texDir
    local baseTex = T(base .. "/flowering.png")
    local fid = flora.register(args.name, baseTex)
    floraCatalog.species[args.name] = fid

    flora.setLifecycle(fid, "perennial",
        args.minLife or 720, args.maxLife or 2520, args.deathChance or 0.3)

    local sproutTex = T(base .. "/sprout.png")
    local deadTex   = T(base .. "/dead.png")

    flora.addPhase(fid, "sprout",     sproutTex, 0)
    flora.addPhase(fid, "vegetating", T(base .. "/budding.png"), args.matureAge or 30)
    flora.addPhase(fid, "dead",       deadTex,   args.maxLife or 2520)

    -- Annual cycle
    flora.addCycleStage(fid, "dormant",   0,   T(base .. "/dormant.png"))
    flora.addCycleStage(fid, "budding",   60,  T(base .. "/budding.png"))
    flora.addCycleStage(fid, "flowering", 100, T(base .. "/flowering.png"))
    flora.addCycleStage(fid, "senescing", 200, T(base .. "/senescing.png"))

    -- Dead overrides (stay dead regardless of season)
    flora.addCycleOverride(fid, "dead", "dormant",   deadTex)
    flora.addCycleOverride(fid, "dead", "budding",   deadTex)
    flora.addCycleOverride(fid, "dead", "flowering", deadTex)
    flora.addCycleOverride(fid, "dead", "senescing", deadTex)

    flora.registerForWorldGen(fid, args.category or "wildflower",
        args.minTemp,    args.maxTemp,
        args.minPrecip,  args.maxPrecip,
        args.maxSlope    or 4,
        args.density     or 0.2,
        args.footprint   or 0,
        args.idealTemp,
        args.idealPrecip,
        args.minAlt      or -50,
        args.maxAlt      or 300,
        args.idealAlt    or 50,
        args.minHumidity or 0.3,
        args.maxHumidity or 0.8,
        args.idealHumidity or 0.5)

    return fid
end

-----------------------------------------------------------
-- Evergreen shrub/bush helper
--
-- No seasonal change, just sprout → matured.
--
-- Required textures:
--   {base}/sprout.png
--   {base}/matured.png
-----------------------------------------------------------

local function evergreenBush(args)
    local base = args.texDir
    local baseTex = T(base .. "/matured.png")
    local fid = flora.register(args.name, baseTex)
    floraCatalog.species[args.name] = fid

    flora.setLifecycle(fid, "evergreen")

    flora.addPhase(fid, "sprout",  T(base .. "/sprout.png"), 0)
    flora.addPhase(fid, "matured", baseTex, args.matureAge or 180)

    flora.registerForWorldGen(fid, args.category or "bush",
        args.minTemp,    args.maxTemp,
        args.minPrecip,  args.maxPrecip,
        args.maxSlope    or 5,
        args.density     or 0.15,
        args.footprint   or 6,
        args.idealTemp,
        args.idealPrecip,
        args.minAlt      or -20,
        args.maxAlt      or 400,
        args.idealAlt    or 80,
        args.minHumidity or 0.3,
        args.maxHumidity or 0.9,
        args.idealHumidity or 0.6)

    return fid
end

-----------------------------------------------------------
-----------------------------------------------------------
-- SPECIES DEFINITIONS
-----------------------------------------------------------
-----------------------------------------------------------

function floraCatalog.init()

    ---------------------------------------------------------
    -- TEMPERATE DECIDUOUS TREES
    ---------------------------------------------------------

    deciduousTree({
        name = "white_oak",
        texDir = "assets/textures/world/flora/white_oak",
        matureAge = 720, minLife = 10800, maxLife = 36000, deathChance = 0.02,
        deathAge = 36000,
        minTemp = 0, maxTemp = 32, idealTemp = 18,
        minPrecip = 0.4, maxPrecip = 1.0, idealPrecip = 0.7,
        minAlt = -20, maxAlt = 500, idealAlt = 100,
        minHumidity = 0.4, maxHumidity = 0.9, idealHumidity = 0.7,
        maxSlope = 2, density = 0.25, footprint = 18,
    })

    deciduousTree({
        name = "paper_birch",
        texDir = "assets/textures/world/flora/paper_birch",
        matureAge = 540, minLife = 5400, maxLife = 18000, deathChance = 0.05,
        deathAge = 18000,
        minTemp = -5, maxTemp = 25, idealTemp = 12,
        minPrecip = 0.3, maxPrecip = 0.9, idealPrecip = 0.5,
        minAlt = 0, maxAlt = 600, idealAlt = 150,
        minHumidity = 0.3, maxHumidity = 0.8, idealHumidity = 0.5,
        maxSlope = 3, density = 0.2, footprint = 14,
    })

    deciduousTree({
        name = "weeping_willow",
        texDir = "assets/textures/world/flora/weeping_willow",
        matureAge = 900, minLife = 7200, maxLife = 21600, deathChance = 0.04,
        deathAge = 21600,
        minTemp = 2, maxTemp = 30, idealTemp = 16,
        minPrecip = 0.6, maxPrecip = 1.0, idealPrecip = 0.9,
        minAlt = -40, maxAlt = 150, idealAlt = 10,
        minHumidity = 0.7, maxHumidity = 1.0, idealHumidity = 0.9,
        maxSlope = 2, density = 0.15, footprint = 16,
    })

    ---------------------------------------------------------
    -- BOREAL / EVERGREEN TREES
    ---------------------------------------------------------

    evergreenTree({
        name = "scots_pine",
        texDir = "assets/textures/world/flora/scots_pine",
        matureAge = 1080,
        minTemp = -15, maxTemp = 18, idealTemp = 5,
        minPrecip = 0.2, maxPrecip = 0.8, idealPrecip = 0.4,
        minAlt = 50, maxAlt = 700, idealAlt = 300,
        minHumidity = 0.2, maxHumidity = 0.7, idealHumidity = 0.4,
        maxSlope = 3, density = 0.3, footprint = 12,
    })

    evergreenTree({
        name = "white_spruce",
        texDir = "assets/textures/world/flora/white_spruce",
        matureAge = 1080,
        minTemp = -20, maxTemp = 15, idealTemp = 2,
        minPrecip = 0.3, maxPrecip = 0.9, idealPrecip = 0.5,
        minAlt = 100, maxAlt = 800, idealAlt = 400,
        minHumidity = 0.3, maxHumidity = 0.8, idealHumidity = 0.5,
        maxSlope = 3, density = 0.3, footprint = 10,
    })

    ---------------------------------------------------------
    -- TROPICAL TREES
    ---------------------------------------------------------

    evergreenTree({
        name = "coconut_palm",
        texDir = "assets/textures/world/flora/coconut_palm",
        matureAge = 900,
        minTemp = 20, maxTemp = 45, idealTemp = 30,
        minPrecip = 0.5, maxPrecip = 1.0, idealPrecip = 0.8,
        minAlt = -20, maxAlt = 200, idealAlt = 20,
        minHumidity = 0.5, maxHumidity = 1.0, idealHumidity = 0.8,
        maxSlope = 2, density = 0.2, footprint = 16,
    })

    evergreenTree({
        name = "red_mangrove",
        texDir = "assets/textures/world/flora/red_mangrove",
        matureAge = 720,
        minTemp = 18, maxTemp = 40, idealTemp = 28,
        minPrecip = 0.6, maxPrecip = 1.0, idealPrecip = 0.9,
        minAlt = -30, maxAlt = 30, idealAlt = 0,
        minHumidity = 0.7, maxHumidity = 1.0, idealHumidity = 0.9,
        maxSlope = 1, density = 0.2, footprint = 14,
    })

    ---------------------------------------------------------
    -- TEMPERATE WILDFLOWERS
    ---------------------------------------------------------

    perennialFlower({
        name = "common_dandelion",
        texDir = "assets/textures/world/flora/common_dandelion",
        matureAge = 30, minLife = 1080, maxLife = 2520, deathChance = 0.3,
        minTemp = 2, maxTemp = 35, idealTemp = 15,
        minPrecip = 0.2, maxPrecip = 0.9, idealPrecip = 0.5,
        minAlt = -50, maxAlt = 300, idealAlt = 50,
        minHumidity = 0.3, maxHumidity = 0.8, idealHumidity = 0.5,
        maxSlope = 4, density = 0.2, footprint = 0,
    })

    perennialFlower({
        name = "white_clover",
        texDir = "assets/textures/world/flora/white_clover",
        matureAge = 20, minLife = 720, maxLife = 1800, deathChance = 0.4,
        minTemp = 5, maxTemp = 28, idealTemp = 16,
        minPrecip = 0.3, maxPrecip = 0.8, idealPrecip = 0.6,
        minAlt = -30, maxAlt = 250, idealAlt = 40,
        minHumidity = 0.4, maxHumidity = 0.8, idealHumidity = 0.6,
        maxSlope = 4, density = 0.18, footprint = 0,
    })

    ---------------------------------------------------------
    -- ARID / DRY
    ---------------------------------------------------------

--    evergreenBush({
--        name = "barrel_cactus",
--        texDir = "assets/textures/world/flora/barrel_cactus",
--        category = "cactus", matureAge = 1440,
--        minTemp = 10, maxTemp = 45, idealTemp = 30,
--        minPrecip = 0.0, maxPrecip = 0.25, idealPrecip = 0.1,
--        minAlt = -20, maxAlt = 400, idealAlt = 100,
--        minHumidity = 0.0, maxHumidity = 0.3, idealHumidity = 0.1,
--        maxSlope = 3, density = 0.08, footprint = 8,
--    })
--
--    evergreenBush({
--        name = "creosote_bush",
--        texDir = "assets/textures/world/flora/creosote_bush",
--        category = "shrub", matureAge = 360,
--        minTemp = 8, maxTemp = 42, idealTemp = 25,
--        minPrecip = 0.05, maxPrecip = 0.3, idealPrecip = 0.15,
--        minAlt = -30, maxAlt = 500, idealAlt = 150,
--        minHumidity = 0.0, maxHumidity = 0.3, idealHumidity = 0.15,
--        maxSlope = 5, density = 0.1, footprint = 6,
--    })

    ---------------------------------------------------------
    -- ALPINE / HIGH ELEVATION
    ---------------------------------------------------------

--    perennialFlower({
--        name = "alpine_grass",
--        texDir = "assets/textures/world/flora/alpine_grass",
--        matureAge = 60, minLife = 1080, maxLife = 3600, deathChance = 0.2,
--        minTemp = -15, maxTemp = 12, idealTemp = 3,
--        minPrecip = 0.2, maxPrecip = 0.7, idealPrecip = 0.4,
--        minAlt = 500, maxAlt = 900, idealAlt = 650,
--        minHumidity = 0.2, maxHumidity = 0.6, idealHumidity = 0.4,
--        maxSlope = 8, density = 0.2, footprint = 0,
--    })
--
--    perennialFlower({
--        name = "edelweiss",
--        texDir = "assets/textures/world/flora/edelweiss",
--        matureAge = 90, minLife = 720, maxLife = 2160, deathChance = 0.3,
--        minTemp = -10, maxTemp = 10, idealTemp = 2,
--        minPrecip = 0.1, maxPrecip = 0.5, idealPrecip = 0.3,
--        minAlt = 600, maxAlt = 950, idealAlt = 750,
--        minHumidity = 0.1, maxHumidity = 0.5, idealHumidity = 0.3,
--        maxSlope = 6, density = 0.05, footprint = 0,
--    })

    ---------------------------------------------------------
    -- WETLAND
    ---------------------------------------------------------

--    evergreenBush({
--        name = "common_cattail",
--        texDir = "assets/textures/world/flora/common_cattail",
--        category = "bush", matureAge = 45,
--        minTemp = 0, maxTemp = 32, idealTemp = 18,
--        minPrecip = 0.7, maxPrecip = 1.0, idealPrecip = 0.95,
--        minAlt = -30, maxAlt = 50, idealAlt = 0,
--        minHumidity = 0.8, maxHumidity = 1.0, idealHumidity = 0.95,
--        maxSlope = 1, density = 0.2, footprint = 4,
--    })
--
--    ---------------------------------------------------------
--    -- TROPICAL GROUND COVER
--    ---------------------------------------------------------
--
--    evergreenBush({
--        name = "bird_of_paradise",
--        texDir = "assets/textures/world/flora/bird_of_paradise",
--        category = "bush", matureAge = 120,
--        minTemp = 22, maxTemp = 42, idealTemp = 32,
--        minPrecip = 0.7, maxPrecip = 1.0, idealPrecip = 0.9,
--        minAlt = -10, maxAlt = 250, idealAlt = 40,
--        minHumidity = 0.7, maxHumidity = 1.0, idealHumidity = 0.9,
--        maxSlope = 5, density = 0.2, footprint = 8,
--    })
--
--    ---------------------------------------------------------
--    -- BOREAL GROUND COVER
--    ---------------------------------------------------------
--
--    evergreenBush({
--        name = "sphagnum_moss",
--        texDir = "assets/textures/world/flora/sphagnum_moss",
--        category = "wildflower", matureAge = 90,
--        minTemp = -10, maxTemp = 20, idealTemp = 5,
--        minPrecip = 0.4, maxPrecip = 1.0, idealPrecip = 0.7,
--        minAlt = 0, maxAlt = 600, idealAlt = 200,
--        minHumidity = 0.5, maxHumidity = 1.0, idealHumidity = 0.8,
--        maxSlope = 6, density = 0.25, footprint = 0,
--    })

    ---------------------------------------------------------

    engine.logInfo("Flora catalog loaded: 17 species")
end

return floraCatalog
