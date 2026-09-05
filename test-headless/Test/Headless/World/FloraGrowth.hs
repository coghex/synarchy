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
--     4. 'instanceLifespan': the exact Annual/Biennial constants, and
--        — for perennials, whose lifetime is rolled from a hash of the
--        placement fields — the two properties the source actually
--        commits to. The mixer is documented as cosmetic
--        ('World.Flora.Growth'), so no exact perennial lifespan is
--        pinned here: what is contractual is that equal placement
--        gives an equal lifespan, and that varying any single mixed
--        field moves it.
module Test.Headless.World.FloraGrowth (spec) where

import UPrelude
import World.Flora.Identity (generatedFloraInstanceId)
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
    , fsHarvest = Just FloraHarvest
        { fhTags = ["fruit"], fhUngatedTags = []
        , fhYield = [("wild_berries", 1, 3)], fhPhaseYields = HM.empty
        , fhRegrowth = 86400, fhHarvestedTexture = TextureHandle 9 }
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
    , fsHarvest = Just FloraHarvest
        { fhTags = ["leaves"], fhUngatedTags = []
        , fhYield = [("wild_greens", 1, 2)], fhPhaseYields = HM.empty
        , fhRegrowth = 43200, fhHarvestedTexture = TextureHandle 9 }
    }

evergreen ∷ FloraSpecies
evergreen = (newFloraSpecies "test_pine" (TextureHandle 1))
    { fsLifecycle = Evergreen
    , fsPhases = HM.fromList
        [ (PhaseSprout,  LifePhase PhaseSprout  0   (TextureHandle 2))
        , (PhaseMatured, LifePhase PhaseMatured 900 (TextureHandle 3))
        ]
    }

-- An annual-shaped species. 'instanceLifespan' answers Annual with the
-- exact 360-day constant, so this fixture's lifecycle boundaries are
-- independent of the placement mixer — unlike the perennial ones,
-- which derive their boundaries from the very value under test.
annualHerb ∷ FloraSpecies
annualHerb = (newFloraSpecies "test_annual" (TextureHandle 1))
    { fsLifecycle = Annual
    , fsPhases = HM.fromList
        [ (PhaseSprout,  LifePhase PhaseSprout  0   (TextureHandle 2))
        , (PhaseMatured, LifePhase PhaseMatured 120 (TextureHandle 3))
        , (PhaseDead,    LifePhase PhaseDead    360 (TextureHandle 4))
        ]
    }

-- A biennial-shaped species: the other exact constant, 720 days.
biennialRoot ∷ FloraSpecies
biennialRoot = (newFloraSpecies "test_biennial" (TextureHandle 1))
    { fsLifecycle = Biennial
    , fsPhases = HM.fromList
        [ (PhaseSprout,  LifePhase PhaseSprout  0   (TextureHandle 2))
        , (PhaseMatured, LifePhase PhaseMatured 240 (TextureHandle 3))
        ]
    }

-- A tree-shaped species modelling the three shipped wood species
-- (#2212): wood-tagged, sprout → matured → dead, and it AUTHORS @wood@
-- as ungated with a sprout that yields nothing.
--
-- Annual rather than perennial on purpose: 'instanceLifespan' answers
-- Annual with the exact 360-day constant, so the dead-window boundary
-- below is a fixture constant rather than a value rolled from the very
-- placement mixer these examples must not depend on.
oak ∷ FloraSpecies
oak = (newFloraSpecies "test_oak" (TextureHandle 1))
    { fsLifecycle = Annual
    , fsPhases = HM.fromList
        [ (PhaseSprout,  LifePhase PhaseSprout  0   (TextureHandle 2))
        , (PhaseMatured, LifePhase PhaseMatured 60  (TextureHandle 3))
        , (PhaseDead,    LifePhase PhaseDead    360 (TextureHandle 4))
        ]
    , fsHarvest = Just FloraHarvest
        { fhTags = ["wood"], fhUngatedTags = ["wood"]
        , fhYield = [("test_log", 3, 6)]
        , fhPhaseYields = HM.fromList [(PhaseSprout, [])]
        , fhRegrowth = 345600, fhHarvestedTexture = TextureHandle 5 }
    }

-- The SAME tree with no authored exemption and no phase override: the
-- absent-schema default a species gets by saying nothing. Its tagged
-- harvest must be gated in exactly the states a bare one is, and every
-- phase must inherit the block's own roll.
elm ∷ FloraSpecies
elm = oak
    { fsName = "test_elm"
    , fsHarvest = (\fh → fh { fhUngatedTags = [], fhPhaseYields = HM.empty })
                      ⊚ fsHarvest oak
    }

-- Ages that put an instance of either tree in each of the three states
-- the chop tool must handle. Health is 1, so age advances one day per
-- day and each is reached at absolute day 0 from the placement age
-- alone: sprout (< 60), matured (≥ 60, < 360), and dead (past the
-- 360-day annual lifespan, inside the 60-day dead window).
oakSprout, oakMatured, oakDead ∷ FloraInstance
oakSprout  = seedling { fiAge = 0.0 }
oakMatured = seedling { fiAge = 100.0 }
oakDead    = seedling { fiAge = 380.0 }

-- A fresh instance: age 0 at the world epoch, full health.
seedling ∷ FloraInstance
seedling = FloraInstance
    { fiSpecies = FloraId 1, fiTileX = 3, fiTileY = 7
    , fiOffU = 0.1, fiOffV = -0.2, fiZ = 5
    , fiAge = 0.0, fiHealth = 1.0, fiVariant = 2, fiBaseWidth = 10.0
    , fiInstanceId = generatedFloraInstanceId "flora_growth_probe" 3 7
                         "probe_species" 0
    , fiChopDesignated = False
    }

-- The same placement as 'seedling', written out independently rather
-- than derived from it: chunk regeneration rebuilds an instance from
-- scratch, and the mixer's stability claim is about the FIELDS
-- agreeing, not about one value being reused.
regenerated ∷ FloraInstance
regenerated = FloraInstance
    { fiSpecies = FloraId 1, fiTileX = 3, fiTileY = 7
    , fiOffU = 0.1, fiOffV = -0.2, fiZ = 5
    , fiAge = 0.0, fiHealth = 1.0, fiVariant = 2, fiBaseWidth = 10.0
    , fiInstanceId = generatedFloraInstanceId "flora_growth_probe" 3 7
                         "probe_species" 0
    , fiChopDesignated = False
    }

-- The five placement fields 'instanceHashFrac' mixes, each varied ONE
-- at a time off 'seedling' with the species and the other four held
-- fixed — so a mixer that dropped any single field is caught by that
-- field's own case, with no reliance on the five results being
-- pairwise distinct.
--
-- The offsets are quantized into 1/1023 buckets before mixing, so both
-- moves cross a bucket boundary (fiOffU 0.1→0.3 is bucket 614→818,
-- fiOffV -0.2→0.25 is 307→767); an equal lifespan therefore cannot be
-- explained by an unchanged quantized input. The values were also
-- chosen so that no case collides in the mixer's 16-bit output; a
-- surprising failure here therefore means either the field stopped
-- being mixed or new mixing constants happen to collide on this
-- placement — check the mixer before adjusting the fixture.
mixedFieldVariants ∷ [(String, FloraInstance)]
mixedFieldVariants =
    [ ("fiOffU",    seedling { fiOffU    = 0.3 })
    , ("fiOffV",    seedling { fiOffV    = 0.25 })
    , ("fiTileX",   seedling { fiTileX   = 4 })
    , ("fiTileY",   seedling { fiTileY   = 9 })
    , ("fiVariant", seedling { fiVariant = 5 })
    ]

-- Force the Maybe before comparing. Comparing 'Maybe' values directly
-- would let a variant that returned Nothing read as "different", which
-- is not the sensitivity being asserted.
requireLifespan ∷ String → FloraSpecies → FloraInstance → IO Float
requireLifespan what sp fi = case instanceLifespan sp fi of
    Just l  → pure l
    Nothing → do
        expectationFailure (what <> ": expected a lifespan, got Nothing")
        pure 0.0

-- | A calendar whose year length is not computable: @ccDaysPerMonth *
--   ccMonthsPerYear@ wraps past 'Int' and lands on zero, which the
--   unchecked 'calendarDaysPerYear' would hand straight to a @divMod@.
wrappingCalendar ∷ CalendarConfig
wrappingCalendar = defaultCalendarConfig
    { ccDaysPerMonth  = maxBound `div` 2 + 1
    , ccMonthsPerYear = 4 }

-- | A 'Float' @NaN@, written so no literal division survives constant
--   folding into a different value.
nanScale ∷ Float
nanScale = 0 / 0

-- | The next 'Float' strictly above a positive finite @x@ — the
--   just-over-the-line value the ceiling examples need, computed from
--   'maxTimeScale' itself rather than from a copied literal.
nextUpFloat ∷ Float → Float
nextUpFloat x =
    let (mant, ex) = decodeFloat x
    in encodeFloat (mant + 1) ex

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

        -- #2280: the clock enforces the accepted scale domain itself, so
        -- a producer that goes around the Lua boundary still cannot
        -- corrupt it. Each of these hits a DIFFERENT guard, and every one
        -- must give back the exact input plus zero rolled days -- never a
        -- partially applied advance.
        forM_ [ ("NaN", nanScale)
              , ("+Infinity", 1 / 0)
              , ("-Infinity", -1 / 0)
              , ("a negative finite scale", -1.0)
              , ("a scale one ulp above the derived ceiling",
                 nextUpFloat maxTimeScale)
              ] $ \(label, scale) ->
            it ("refuses " ⧺ label ⧺ " and leaves the clock exactly alone") $ do
                let (t, d, rolled) = advanceWorldClock defaultCalendarConfig
                        scale 0.25 (WorldTime 23 59) (WorldDate 3 7 11)
                t `shouldBe` WorldTime 23 59
                d `shouldBe` WorldDate 3 7 11
                rolled `shouldBe` 0

        it "accepts the derived ceiling itself, and floors to exactly the \
           \day count the shared domain predicts" $ do
            -- The worst case the ceiling is derived FROM: the last minute
            -- of a day plus one whole maxElapsedStep.
            let (t, _, rolled) = advanceWorldClock defaultCalendarConfig
                    maxTimeScale 0.25 (WorldTime 23 59) (WorldDate 1 1 1)
            Just rolled `shouldBe` worstCaseDayCount maxTimeScale
            wtHour t `shouldSatisfy` (\h -> h >= 0 && h <= 23)
            wtMinute t `shouldSatisfy` (\m -> m >= 0 && m <= 59)

        it "puts the ceiling at the LARGEST safe scale, not a round number \
           \below it" $ do
            -- The bound is only correct if it is tight: one representable
            -- step above it must actually overflow the Int day count, and
            -- the value itself must not. A ceiling that merely happened to
            -- be safe (half the range, say) would pass the example above
            -- while needlessly refusing scales the clock handles fine.
            worstCaseDayCount maxTimeScale `shouldSatisfy` isJust
            worstCaseDayCount (nextUpFloat maxTimeScale) `shouldBe` Nothing
            forM_ [maxTimeScale / 2, maxTimeScale * 0.75, maxTimeScale] $
                \scale -> case worstCaseDayCount scale of
                    Nothing -> expectationFailure
                        ("worst-case day count overflowed at " ++ show scale)
                    Just days -> days `shouldSatisfy` (\d -> d >= 0)

        it "accepts the 50000 probe scale and carries its whole-day count" $ do
            -- tools/farm_ai_probe.py and tools/crop_probe.py both drive
            -- the world at this scale; requirement 5 keeps it accepted.
            let (t, d, rolled) = advanceWorldClock defaultCalendarConfig
                    50000.0 1.0 (WorldTime 0 0) (WorldDate 1 1 1)
            rolled `shouldBe` 34
            t `shouldBe` WorldTime 17 20
            d `shouldBe` WorldDate 1 2 5

        it "refuses a carry whose calendar result would not fit wdYear" $ do
            -- An accepted scale, an ordinary elapsed step, a representable
            -- day count -- and a year that cannot absorb the carry. The
            -- pre-#2280 clock wrapped straight into a negative year.
            let (t, d, rolled) = advanceWorldClock defaultCalendarConfig
                    3000.0 180.0 (WorldTime 6 0) (WorldDate maxBound 1 1)
            t `shouldBe` WorldTime 6 0
            d `shouldBe` WorldDate maxBound 1 1
            rolled `shouldBe` 0

        it "refuses a carry whose calendar year length cannot be computed" $ do
            -- CalendarConfig comes from world-gen data, not from a
            -- validated range: this one's ccDaysPerMonth * ccMonthsPerYear
            -- wraps to zero, which would turn the carry's divMod into a
            -- divide by zero. Totality means the unchanged clock, not a
            -- crash.
            calendarDaysPerYearChecked wrappingCalendar `shouldBe` Nothing
            let (t, d, rolled) = advanceWorldClock wrappingCalendar
                    1.0 3600.0 (WorldTime 23 0) (WorldDate 1 1 1)
            t `shouldBe` WorldTime 23 0
            d `shouldBe` WorldDate 1 1 1
            rolled `shouldBe` 0

        it "refuses a non-finite elapsed step" $ do
            -- The guard is over the values each floor actually receives,
            -- so a producer handing the clock a NaN dt is refused even at
            -- an accepted scale.
            let (t, d, rolled) = advanceWorldClock defaultCalendarConfig
                    1.0 nanScale (WorldTime 10 0) (WorldDate 1 1 1)
            t `shouldBe` WorldTime 10 0
            d `shouldBe` WorldDate 1 1 1
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
        it "an annual dies on its exact 360-day boundary, then reseeds" $ do
            -- Anchored on a lifespan the suite pins exactly (360), so
            -- unlike the perennial case above these boundaries are not
            -- computed from the value under test. Both examples stand:
            -- this one cannot be satisfied by a constant mixer, that
            -- one still covers the hashed-lifetime path.
            let gAlive  = floraGrowth annualHerb 359 seedling
                gDead   = floraGrowth annualHerb 360 seedling
                gReborn = floraGrowth annualHerb 421 seedling
            fgDead gAlive `shouldBe` False
            fgDead gDead `shouldBe` True
            growthPhaseTag annualHerb gDead `shouldBe` Just PhaseDead
            fgDead gReborn `shouldBe` False
            fgGeneration gReborn `shouldBe` 1
            growthPhaseTag annualHerb gReborn `shouldBe` Just PhaseSprout

    describe "instanceLifespan" $ do
        it "an annual lives exactly 360 game-days" $
            instanceLifespan annualHerb seedling `shouldBe` Just 360.0
        it "a biennial lives exactly 720 game-days" $
            instanceLifespan biennialRoot seedling `shouldBe` Just 720.0
        it "an evergreen has no lifespan at all" $
            instanceLifespan evergreen seedling `shouldBe` Nothing
        it "a perennial's rolled lifespan is within the species range" $ do
            l ← requireLifespan "berry/seedling" berry seedling
            l `shouldSatisfy` (≥ 1080)
            l `shouldSatisfy` (≤ 3600)
        it "equal placement fields give an equal lifespan" $ do
            l  ← requireLifespan "berry/seedling" berry seedling
            l' ← requireLifespan "berry/regenerated" berry regenerated
            l' `shouldBe` l
        it "varying any one mixed placement field moves the lifespan" $ do
            baseline ← requireLifespan "berry/seedling" berry seedling
            rolled ← forM mixedFieldVariants $ \(field, fi) → do
                l ← requireLifespan ("berry/" <> field) berry fi
                pure (field, l)
            -- Collected rather than asserted field by field so a mixer
            -- that dropped several fields names all of them at once.
            map fst (filter ((≡ baseline) . snd) rolled) `shouldBe` []

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

    describe "harvestOpen tag policy: the authored window exemption \
             \(#2212)" $ do
        -- Every example goes through 'floraHarvestAdmits', the ONE
        -- predicate the Chop hit test, the world-thread designation
        -- commit, both harvest verbs and the tagged finder consult.
        let admits sp tag i = floraHarvestAdmits sp tag dormantDay
                                  (floraGrowth sp 0 i)
        it "a species that AUTHORS the tag as ungated is taken in every \
           \growth state — a sprout and a standing-dead tree both chop" $ do
            admits oak (Just "wood") oakSprout  `shouldBe` True
            admits oak (Just "wood") oakMatured `shouldBe` True
            admits oak (Just "wood") oakDead    `shouldBe` True
        it "the fixture's three ages really are three different states, \
           \so the case above is not vacuously open" $ do
            -- Without this the exemption could be passing because every
            -- instance happens to be mature.
            harvestOpen oak dormantDay (floraGrowth oak 0 oakSprout)
                `shouldBe` False
            harvestOpen oak dormantDay (floraGrowth oak 0 oakMatured)
                `shouldBe` True
            harvestOpen oak dormantDay (floraGrowth oak 0 oakDead)
                `shouldBe` False
            growthPhaseTag oak (floraGrowth oak 0 oakSprout)
                `shouldBe` Just PhaseSprout
            growthPhaseTag oak (floraGrowth oak 0 oakMatured)
                `shouldBe` Just PhaseMatured
            growthPhaseTag oak (floraGrowth oak 0 oakDead)
                `shouldBe` Just PhaseDead
            fgDead (floraGrowth oak 0 oakDead) `shouldBe` True
        it "a species that authors NO exemption is refused in exactly the \
           \states a BARE call is — the absent-schema default" $ do
            -- The whole point of #2212: before it, ANY tagged call
            -- skipped the window, so a future fruit/grain tag would have
            -- silently disabled the #332 lifecycle gate.
            admits elm (Just "wood") oakSprout  `shouldBe` False
            admits elm (Just "wood") oakDead    `shouldBe` False
            -- ... and accepted inside it, so the exemption is what the
            -- rejections above turn on, not the tag itself.
            admits elm (Just "wood") oakMatured `shouldBe` True
        it "agrees with a bare call state for state on a non-declaring \
           \species" $
            forM_ [oakSprout, oakMatured, oakDead] $ \i →
                admits elm (Just "wood") i
                    `shouldBe` harvestOpen elm dormantDay (floraGrowth elm 0 i)
        it "refuses a tag the species does not carry, exemption or not" $ do
            admits oak (Just "fruit") oakMatured `shouldBe` False
            admits elm (Just "fruit") oakMatured `shouldBe` False
        it "an exemption cannot open a tag the species does not carry" $ do
            -- The decoder rejects this shape, but the predicate must not
            -- depend on the decoder for its own soundness.
            let liar = oak { fsHarvest = (\fh → fh
                              { fhTags = ["wood"]
                              , fhUngatedTags = ["wood", "fruit"] })
                                ⊚ fsHarvest oak }
            admits liar (Just "fruit") oakSprout `shouldBe` False
        it "leaves a decorative species (no harvest block) unharvestable" $ do
            admits evergreen (Just "wood") oakMatured `shouldBe` False
            admits evergreen Nothing oakMatured `shouldBe` False
        it "a BARE call is harvestOpen unchanged, on both species" $
            forM_ [(oak, oakSprout), (oak, oakMatured), (oak, oakDead)
                  ,(elm, oakSprout), (elm, oakMatured), (elm, oakDead)] $
                \(sp, i) →
                    admits sp Nothing i
                        `shouldBe` harvestOpen sp dormantDay (floraGrowth sp 0 i)
        it "still gates a bare fruiting-window forage by the season" $ do
            let mature = seedling { fiAge = 400.0 }
            floraHarvestAdmits berry Nothing fruitingDay
                (floraGrowth berry 0 mature) `shouldBe` True
            floraHarvestAdmits berry Nothing dormantDay
                (floraGrowth berry 0 mature) `shouldBe` False

    describe "harvestOpen phase yields: what an accepted harvest pays \
             \(#2212)" $ do
        let yieldOf sp i = case fsHarvest sp of
                Nothing → error "fixture species must author a harvest"
                Just fh → floraHarvestYield sp fh (floraGrowth sp 0 i)
        it "an authored EMPTY override pays nothing — a felled sprout" $
            yieldOf oak oakSprout `shouldBe` []
        it "an unauthored phase inherits the block's own roll" $ do
            yieldOf oak oakMatured `shouldBe` [("test_log", 3, 6)]
            yieldOf oak oakDead    `shouldBe` [("test_log", 3, 6)]
        it "a block with no overrides at all inherits in EVERY phase, \
           \the sprout included" $
            forM_ [oakSprout, oakMatured, oakDead] $ \i →
                yieldOf elm i `shouldBe` [("test_log", 3, 6)]
        it "a non-empty override REPLACES the roll rather than adding to \
           \it" $ do
            let bushy = oak { fsHarvest = (\fh → fh { fhPhaseYields =
                                HM.fromList [(PhaseMatured
                                             , [("test_twig", 1, 1)])] })
                                  ⊚ fsHarvest oak }
            yieldOf bushy oakMatured `shouldBe` [("test_twig", 1, 1)]
            yieldOf bushy oakSprout  `shouldBe` [("test_log", 3, 6)]
        it "a species with no phases has no key to hit and always \
           \inherits" $ do
            let phaseless = oak { fsPhases = HM.empty }
            growthPhaseTag phaseless (floraGrowth phaseless 0 oakSprout)
                `shouldBe` Nothing
            yieldOf phaseless oakSprout `shouldBe` [("test_log", 3, 6)]

    describe "growth stage naming" $ do
        it "annual stage tracks the day-of-year" $ do
            activeStageTag berry fruitingDay `shouldBe` Just CycleFruiting
            activeStageTag berry dormantDay `shouldBe` Just CycleDormant
        it "species without a cycle have no stage" $
            activeStageTag evergreen 100 `shouldBe` Nothing
