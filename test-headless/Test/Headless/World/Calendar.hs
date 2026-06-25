{-# LANGUAGE UnicodeSyntax #-}
-- | Calendar / annual-cycle selection tests.
--
--   Regression guard for the day-of-month-aliased-as-day-of-year bug:
--   the flora renderer used to feed 'wdDay' (a 1..31 day-of-month) into
--   annual-cycle stage selection, so any stage starting after the first
--   month was unreachable. These tests pin two things:
--
--     1. 'worldDateToDayOfYear' maps a 'WorldDate' to a zero-based
--        year-relative ordinal day using the calendar's fixed month
--        length, and clamps out-of-range components.
--     2. 'findActiveCycleStage' picks the right stage for ordinal days
--        well past day 31, and wraps for early-year days.
module Test.Headless.World.Calendar (spec) where

import UPrelude
import Test.Hspec
import Engine.Asset.Handle (TextureHandle(..))
import World.Time.Types
    (WorldDate(..), CalendarConfig(..), defaultCalendarConfig
    , worldDateToDayOfYear)
import World.Flora.Types (AnnualStage(..), AnnualStageTag(..))
import World.Flora.Render (findActiveCycleStage)

-- Default calendar is 30 days/month × 12 months = 360-day year.
daysPerYear ∷ Int
daysPerYear = ccDaysPerMonth defaultCalendarConfig
            * ccMonthsPerYear defaultCalendarConfig

doy ∷ Int → Int → Int
doy month day = worldDateToDayOfYear defaultCalendarConfig
                                     (WorldDate 1 month day)

-- A four-stage annual cycle authored 0-based, like the real flora data
-- (dormant 0, budding 60, flowering 120, senescing 270).
stages ∷ [AnnualStage]
stages =
    [ AnnualStage CycleDormant    0   (TextureHandle 1)
    , AnnualStage CycleBudding    60  (TextureHandle 2)
    , AnnualStage CycleFlowering  120 (TextureHandle 3)
    , AnnualStage CycleSenescing  270 (TextureHandle 4)
    ]

selectedTag ∷ Int → Maybe AnnualStageTag
selectedTag d = asTag <$> findActiveCycleStage stages d

spec ∷ Spec
spec = do
    describe "worldDateToDayOfYear" $ do
        it "maps the first day of the year to 0" $
            doy 1 1 `shouldBe` 0
        it "advances within the first month" $
            doy 1 2 `shouldBe` 1
        it "accounts for whole months elapsed" $ do
            doy 3 1  `shouldBe` 60    -- two 30-day months elapsed
            doy 5 1  `shouldBe` 120
            doy 10 1 `shouldBe` 270
        it "maps the last day of the year to daysPerYear - 1" $
            doy 12 30 `shouldBe` daysPerYear - 1
        it "produces values past day 31 (the day-of-month ceiling)" $
            doy 4 15 `shouldSatisfy` (> 31)
        it "clamps out-of-range month/day instead of going negative or past year-end" $ do
            worldDateToDayOfYear defaultCalendarConfig (WorldDate 1 0 0)
                `shouldBe` 0
            worldDateToDayOfYear defaultCalendarConfig (WorldDate 1 99 99)
                `shouldBe` daysPerYear - 1

    describe "findActiveCycleStage" $ do
        it "selects the dormant stage at the start of the year" $
            selectedTag (doy 1 1) `shouldBe` Just CycleDormant
        it "selects stages that begin after the first month" $ do
            selectedTag (doy 3 1)  `shouldBe` Just CycleBudding    -- day 60
            selectedTag 90         `shouldBe` Just CycleBudding
            selectedTag (doy 5 1)  `shouldBe` Just CycleFlowering  -- day 120
            selectedTag 300        `shouldBe` Just CycleSenescing
        it "wraps to the final stage for early-year days below all start days" $
            -- With a 0-day stage present this resolves to dormant; a
            -- cycle whose earliest start is > 0 must wrap to the last.
            (asTag <$> findActiveCycleStage
                [ AnnualStage CycleBudding   60  (TextureHandle 2)
                , AnnualStage CycleSenescing 270 (TextureHandle 4) ] 10)
                `shouldBe` Just CycleSenescing
        it "would never reach late stages if fed day-of-month (the bug)" $
            -- Any day-of-month (1..30) can only ever select dormant/budding,
            -- never flowering/senescing — which is exactly why the render
            -- site must convert through worldDateToDayOfYear.
            map selectedTag [1 .. 30]
                `shouldSatisfy` all (`elem` [Just CycleDormant, Just CycleBudding])
