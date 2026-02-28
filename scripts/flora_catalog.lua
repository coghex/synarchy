-- Flora Catalog
-- Defines all plant species for world generation and gameplay.
-- Called after world.init() so the world state exists.

local floraCatalog = {}

floraCatalog.species = {}

function floraCatalog.init()

    -----------------------------------------------------------
    -- Dandelion (perennial wildflower)
    --
    -- Textures (4):
    --   dandelion_sprout.png     - tiny green nub
    --   dandelion_budding.png    - green leaves, closed bud
    --   dandelion_flowering.png  - yellow flower open
    --   dandelion_dead.png       - brown dried stalk
    --
    -- Life phases control long-term growth.
    -- Annual cycle controls the yearly bud→flower→die-back.
    -- "dormant" reuses the sprout texture (underground/invisible
    --  in winter, but we just show the base nub).
    -----------------------------------------------------------

    local dandBase    = engine.loadTexture("assets/textures/world/flora/dandelion/sprout.png")
    local dandSprout  = engine.loadTexture("assets/textures/world/flora/dandelion/sprout.png")
    local dandBud     = engine.loadTexture("assets/textures/world/flora/dandelion/budding.png")
    local dandFlower  = engine.loadTexture("assets/textures/world/flora/dandelion/flowering.png")
    local dandDead    = engine.loadTexture("assets/textures/world/flora/dandelion/dead.png")

    local dandelion = flora.register("dandelion", dandBase)
    floraCatalog.species.dandelion = dandelion

    -- Lifecycle: perennial, lives 3-7 years (1080-2520 game-days),
    -- 30% chance to die each year once past minimum
    flora.setLifecycle(dandelion, "perennial", 1080, 2520, 0.3)

    -- Life phases (age-driven, happens once)
    flora.addPhase(dandelion, "sprout",     dandSprout,  0)
    flora.addPhase(dandelion, "vegetating", dandBud,     30)
    flora.addPhase(dandelion, "dead",       dandDead,    2520)

    -- Annual cycle (day-of-year driven, repeats each year)
    -- Day 0-59:    dormant (winter/early spring) — shows sprout tex
    -- Day 60-99:   budding (spring)  leaves and closed bud
    -- Day 100-159: flowering (late spring/summer) — yellow bloom
    -- Day 160-359: senescing (summer through winter) — dried stalk
    flora.addCycleStage(dandelion, "dormant",   0,   dandSprout)
    flora.addCycleStage(dandelion, "budding",   60,  dandBud)
    flora.addCycleStage(dandelion, "flowering", 100, dandFlower)
    flora.addCycleStage(dandelion, "senescing", 160, dandDead)

    -- World generation: temperate wildflower
    flora.registerForWorldGen(dandelion, "wildflower",
        5, 30,      -- temp range
        0.3, 1.0,   -- precip range
        4,           -- max slope
        0.15)        -- density

    -----------------------------------------------------------
    -- Oak (long-lived tree with seasonal appearance)
    --
    -- Textures (8):
    --   oak_sprout.png               - small sapling
    --   oak_matured.png              - full tree (summer default)
    --   oak_matured_budding.png      - spring buds / light green
    --   oak_matured_flowering.png    - full summer canopy
    --   oak_matured_senescing.png    - autumn orange/red
    --   oak_matured_dormant.png      - bare winter branches
    --   oak_sprout_budding.png       - sapling spring
    --   oak_sprout_dormant.png       - sapling winter (bare stick)
    --
    -- The oak uses Evergreen lifecycle (no age-based death)
    -- but has an annual cycle for seasonal textures.
    -----------------------------------------------------------

    local oakBase      = engine.loadTexture("assets/textures/world/flora/oak/matured.png")
    local oakSprout    = engine.loadTexture("assets/textures/world/flora/oak/sprout.png")
    local oakMatured   = engine.loadTexture("assets/textures/world/flora/oak/matured.png")

    -- Annual cycle base textures (used for matured phase)
    local oakDormant   = engine.loadTexture("assets/textures/world/flora/oak/matured_dormant.png")
    local oakBudding   = engine.loadTexture("assets/textures/world/flora/oak/matured_budding.png")
    local oakFlowering = engine.loadTexture("assets/textures/world/flora/oak/matured_flowering.png")
    local oakSenescing = engine.loadTexture("assets/textures/world/flora/oak/matured_senescing.png")

    -- Sprout-phase seasonal overrides
    local oakSprBud    = engine.loadTexture("assets/textures/world/flora/oak/sprout_budding.png")
    local oakSprDorm   = engine.loadTexture("assets/textures/world/flora/oak/sprout_dormant.png")

    local oak = flora.register("oak", oakBase)
    floraCatalog.species.oak = oak

    -- Lifecycle: evergreen (no age-based death, lives forever)
    -- This is the default so we don't need to call setLifecycle,
    -- but being explicit is fine:
    flora.setLifecycle(oak, "evergreen")

    -- Life phases
    flora.addPhase(oak, "sprout",  oakSprout,  0)
    flora.addPhase(oak, "matured", oakMatured,  720)  -- ~2 game-years

    -- Annual cycle (these are the default textures per cycle stage)
    flora.addCycleStage(oak, "dormant",   0,   oakDormant)
    flora.addCycleStage(oak, "budding",   60,  oakBudding)
    flora.addCycleStage(oak, "flowering", 120, oakFlowering)
    flora.addCycleStage(oak, "senescing", 270, oakSenescing)

    -- Cycle overrides for the sprout phase
    -- (sapling looks different than mature tree in each season)
    flora.addCycleOverride(oak, "sprout", "budding",  oakSprBud)
    flora.addCycleOverride(oak, "sprout", "dormant",  oakSprDorm)
    -- sprout + flowering/senescing: no override, falls back to
    -- the base cycle texture (oakFlowering / oakSenescing),
    -- which is fine for a sapling — same leaf color, just the
    -- sprout phase texture shape

    -- World generation: temperate deciduous tree
    flora.registerForWorldGen(oak, "tree",
        2, 30,       -- temp range
        0.3, 1.0,    -- precip range
        2,            -- max slope
        0.2)          -- density

    engine.logInfo("Flora catalog loaded: dandelion, oak ("
        .. tostring(dandelion) .. ", " .. tostring(oak) .. ")")
end

return floraCatalog
