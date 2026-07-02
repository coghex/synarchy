{-# LANGUAGE UnicodeSyntax #-}
-- | Flora growth runtime tests (#332).
--
--   The growth runtime is DERIVED state: the world date advances
--   (midnight rollover in tickWorldTime — pinned here through
--   'advanceWorldClock' / 'worldDateAddDays'), and a plant's age, life
--   phase, reseed generation and harvest window all derive from the
--   absolute world day plus the instance's deterministic placement
--   fields ('World.Flora.Growth'). These tests pin:
--
--     1. The clock: midnight carries into the date, multiple midnights
--        carry in one tick, months/years roll through the calendar,
--        and 'worldAbsoluteDay' is the monotonic day counter.
--     2. Growth: phases progress with elapsed days, health scales the
--        rate, mortal lifecycles wrap through a dead window into the
--        next generation (the reseed), evergreens don't.
--     3. The harvest window: a species with a @fruiting@ annual stage
--        yields only in season; one without stays open year-round;
--        dead plants and juveniles never yield.
module Test.Headless.World.FloraGrowth (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle(..))
import World.Time.Types
import World.Flora.Types
import World.Flora.Growth

daysPerYear ∷ Int
daysPerYear = calendarDaysPerYear defaultCalendarConfig

-- A raspberry-shaped test species: perennial, sprout→matured→dead
-- phases, an annual cycle with a fruiting window (day 180–269).
berry ∷ FloraSpecies
berry = (newFloraSpecies "test_berry" (TextureHandle 1))
    { fsLifecycle = Perennial 1080 3600 0.1
    , fsPhases = HM.fromList
        [ (PhaseSprout,  LifePhase PhaseSprout  0    (TextureHandle 2))
        , (PhaseMatured, LifePhase PhaseMatured 360  (TextureHandle 3))
        , (PhaseDead,    LifePhase PhaseDead    3600 (TextureHandle 4))
        ]
    , fsAnnualCycle =
        [ AnnualStage CycleDormant   0   (TextureHandle 5)
        , AnnualStage CycleFlowering 130 (TextureHandle 6)
        , AnnualStage CycleFruiting  180 (TextureHandle 7)
        , AnnualStage CycleSenescing 270 (TextureHandle 8)
        ]
    , fsHarvest = Just (FloraHarvest ["fruit"] [("wild_berries", 1, 3)]
                                     86400 (TextureHandle 9))
    }

-- A clover-shaped species: harvestable, annual cycle WITHOUT a
-- fruiting stage → open year-round.
clover ∷ FloraSpecies
clover = (newFloraSpecies "test_clover" (TextureHandle 1))
    { fsLifecycle = Perennial 720 1800 0.4
    , fsPhases = HM.fromList
        [ (PhaseSprout,     LifePhase PhaseSprout     0  (TextureHandle 2))
        , (PhaseVegetating, LifePhase PhaseVegetating 20 (TextureHandle 3))
        ]
    , fsAnnualCycle =
        [ AnnualStage CycleDormant   0   (TextureHandle 5)
        , AnnualStage CycleSenescing 200 (TextureHandle 8)
        ]
    , fsHarvest = Just (FloraHarvest ["leaves"] [("wild_greens", 1, 2)]
                                     43200 (TextureHandle 9))
    }

evergreen ∷ FloraSpecies
evergreen = (newFloraSpecies "test_pine" (TextureHandle 1))
    { fsLifecycle = Evergreen
    , fsPhases = HM.fromList
        [ (PhaseSprout,  LifePhase PhaseSprout  0   (TextureHandle 2))
        , (PhaseMatured, LifePhase PhaseMatured 900 (TextureHandle 3))
        ]
    }

-- A fresh instance: age 0 at the world epoch, full health.
seedling ∷ FloraInstance
seedling = FloraInstance
    { fiSpecies = FloraId 1, fiTileX = 3, fiTileY = 7
    , fiOffU = 0.1, fiOffV = -0.2, fiZ = 5
    , fiAge = 0.0, fiHealth = 1.0, fiVariant = 2, fiBaseWidth = 10.0
    }

fruitingDay, dormantDay ∷ Int
fruitingDay = 200   -- inside berry's fruiting window (180–269)
dormantDay  = 30    -- deep in the dormant stage

spec ∷ Spec
spec = do
    describe "advanceWorldClock" $ do
        it "leaves the date alone before midnight" $ do
            let (t, d, rolled) = advanceWorldClock defaultCalendarConfig
                    1.0 60.0 (WorldTime 10 0) (WorldDate 1 1 1)
            t `shouldBe` WorldTime 11 0
            d `shouldBe` WorldDate 1 1 1
            rolled `shouldBe` 0
        it "carries midnight into the next day" $ do
            let (t, d, rolled) = advanceWorldClock defaultCalendarConfig
                    1.0 120.0 (WorldTime 23 0) (WorldDate 1 1 1)
            t `shouldBe` WorldTime 1 0
            d `shouldBe` WorldDate 1 1 2
            rolled `shouldBe` 1
        it "carries several midnights crossed by one high-time-scale tick" $ do
            -- 3000 game-min/real-sec for 3 real-sec = 9000 min = 6.25 days
            let (_, d, rolled) = advanceWorldClock defaultCalendarConfig
                    3000.0 3.0 (WorldTime 0 0) (WorldDate 1 1 1)
            rolled `shouldBe` 6
            d `shouldBe` WorldDate 1 1 7
        it "does not advance when the scale is zero (paused)" $ do
            let (t, d, rolled) = advanceWorldClock defaultCalendarConfig
                    0.0 3600.0 (WorldTime 23 59) (WorldDate 1 12 30)
            t `shouldBe` WorldTime 23 59
            d `shouldBe` WorldDate 1 12 30
            rolled `shouldBe` 0

    describe "worldDateAddDays" $ do
        it "rolls months" $
            worldDateAddDays defaultCalendarConfig 1 (WorldDate 1 1 30)
                `shouldBe` WorldDate 1 2 1
        it "rolls years" $
            worldDateAddDays defaultCalendarConfig 1 (WorldDate 1 12 30)
                `shouldBe` WorldDate 2 1 1
        it "adds whole years across months" $
            worldDateAddDays defaultCalendarConfig (daysPerYear + 45)
                             (WorldDate 1 1 1)
                `shouldBe` WorldDate 2 2 16

    describe "worldAbsoluteDay" $ do
        it "is 0 at the epoch" $
            worldAbsoluteDay defaultCalendarConfig defaultWorldDate
                `shouldBe` 0
        it "counts whole years" $
            worldAbsoluteDay defaultCalendarConfig (WorldDate 3 1 1)
                `shouldBe` 2 * daysPerYear
        it "is consistent with worldDateAddDays" $ do
            let d = worldDateAddDays defaultCalendarConfig 1234 defaultWorldDate
            worldAbsoluteDay defaultCalendarConfig d `shouldBe` 1234

    describe "floraGrowth (derived age + phases)" $ do
        it "a fresh seedling is a sprout on day 0" $ do
            let g = floraGrowth berry 0 seedling
            fgAge g `shouldBe` 0.0
            growthPhaseTag berry g `shouldBe` Just PhaseSprout
            fgGeneration g `shouldBe` 0
        it "phases progress as world days elapse" $ do
            let g = floraGrowth berry 400 seedling
            growthPhaseTag berry g `shouldBe` Just PhaseMatured
        it "health slows the growth rate" $ do
            let weak = seedling { fiHealth = 0.0 }
                g = floraGrowth berry 400 weak
            -- 400 days at the 0.25 floor = age 100 — still a sprout
            fgAge g `shouldBe` 100.0
            growthPhaseTag berry g `shouldBe` Just PhaseSprout
        it "evergreens never die or wrap" $ do
            let g = floraGrowth evergreen 100000 seedling
            fgDead g `shouldBe` False
            fgGeneration g `shouldBe` 0
            growthPhaseTag evergreen g `shouldBe` Just PhaseMatured

    describe "floraGrowth (reseed wrap)" $ do
        -- Whatever this instance's hashed lifespan is, it lies in
        -- [1080, 3600]; past maxLife + the dead window (3660 days) it
        -- must have died and reseeded at least once.
        it "a perennial eventually wraps into the next generation" $ do
            let g = floraGrowth berry (12 * daysPerYear) seedling
            fgGeneration g `shouldSatisfy` (≥ 1)
        it "the dead window presents the dead phase, then a sprout follows" $ do
            let l = case instanceLifespan berry seedling of
                        Just x  → x
                        Nothing → error "perennial must have a lifespan"
                -- first day inside the dead window (full health → 1 day
                -- of age per world day)
                deadDay   = ceiling l
                -- first day of the next generation
                rebornDay = ceiling (l + deadWindowDays) + 1
                gDead   = floraGrowth berry deadDay seedling
                gReborn = floraGrowth berry rebornDay seedling
            fgDead gDead `shouldBe` True
            growthPhaseTag berry gDead `shouldBe` Just PhaseDead
            fgDead gReborn `shouldBe` False
            fgGeneration gReborn `shouldBe` 1
            growthPhaseTag berry gReborn `shouldBe` Just PhaseSprout
        it "lifespans are deterministic and within the species range" $ do
            let l1 = instanceLifespan berry seedling
                l2 = instanceLifespan berry seedling
            l1 `shouldBe` l2
            case l1 of
                Just l → do
                    l `shouldSatisfy` (≥ 1080)
                    l `shouldSatisfy` (≤ 3600)
                Nothing → expectationFailure "expected a lifespan"

    describe "harvestOpen (fruiting window)" $ do
        let mature = seedling { fiAge = 400.0 }
        it "a mature berry yields in the fruiting window" $
            harvestOpen berry fruitingDay (floraGrowth berry 0 mature)
                `shouldBe` True
        it "a mature berry does NOT yield out of season" $
            harvestOpen berry dormantDay (floraGrowth berry 0 mature)
                `shouldBe` False
        it "the window closes again at senescing (unharvested fruit is lost)" $
            harvestOpen berry 280 (floraGrowth berry 0 mature)
                `shouldBe` False
        it "a species without a fruiting stage is open year-round" $ do
            let g = floraGrowth clover 60 seedling
            harvestOpen clover dormantDay g `shouldBe` True
            harvestOpen clover 250 g `shouldBe` True
        it "sprouts are too young to yield even in season" $
            harvestOpen berry fruitingDay (floraGrowth berry 0 seedling)
                `shouldBe` False
        it "dead plants never yield" $ do
            let l = case instanceLifespan berry seedling of
                        Just x  → x
                        Nothing → error "perennial must have a lifespan"
                gDead = floraGrowth berry (ceiling l) seedling
            harvestOpen berry fruitingDay gDead `shouldBe` False

    describe "growth stage naming" $ do
        it "annual stage tracks the day-of-year" $ do
            activeStageTag berry fruitingDay `shouldBe` Just CycleFruiting
            activeStageTag berry dormantDay `shouldBe` Just CycleDormant
        it "species without a cycle have no stage" $
            activeStageTag evergreen 100 `shouldBe` Nothing
