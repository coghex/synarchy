{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}
-- | Player-facing Create World controls (#706). These source-level
--   contracts keep the Lua UI's declarations explicit without booting a
--   graphical engine, while the generation assertions protect the hidden
--   default payload that must remain backward compatible.
module Test.Headless.UI.CreateWorldControls (spec) where

import UPrelude
import Test.Hspec
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)

menuPath, settingsPath, generalPath, generationPath, climatePath ∷ FilePath
menuPath       = "scripts/create_world_menu.lua"
settingsPath   = "scripts/create_world/settings_tab.lua"
generalPath    = "scripts/create_world/general_tab.lua"
generationPath = "scripts/create_world/generation.lua"
climatePath    = "scripts/create_world/climate_tab.lua"

quotedAfter ∷ Text → Text → Maybe Text
quotedAfter marker line =
    let (_, suffix) = T.breakOn marker line
        afterMarker = T.drop (T.length marker) suffix
        afterQuote  = T.dropWhile (/= '"') afterMarker
    in if T.null suffix ∨ T.null afterQuote
       then Nothing
       else Just $ T.takeWhile (/= '"') $ T.drop 1 afterQuote

blockAfter ∷ Text → Text → Text → Text
blockAfter start end source =
    let (_, suffix) = T.breakOn start source
        body        = T.drop (T.length start) suffix
    in fst $ T.breakOn end body

tabEntries ∷ Text → [(Text, Text)]
tabEntries source = mapMaybe entry $ T.lines block
  where
    block = blockAfter "local tabDefs = {" "\n}" source
    entry line = do
        key  ← quotedAfter "key" line
        name ← quotedAfter "name" line
        pure (key, name)

settingsLabels ∷ Text → [Text]
settingsLabels = mapMaybe (quotedAfter "text     =") ∘ T.lines

generalLabels ∷ Text → [Text]
generalLabels = mapMaybe (quotedAfter "addRow(") ∘ T.lines

spec ∷ Spec
spec = do
    menuSource       ← runIO $ TIO.readFile menuPath
    settingsSource   ← runIO $ TIO.readFile settingsPath
    generalSource    ← runIO $ TIO.readFile generalPath
    generationSource ← runIO $ TIO.readFile generationPath

    it "exposes exactly General, Geology, and Timeline" $
        tabEntries menuSource `shouldBe`
            [ ("settings", "General")
            , ("advanced", "Geology")
            , ("timeline", "Timeline")
            ]

    it "exposes exactly the five active General controls" $
        settingsLabels settingsSource ++ generalLabels generalSource
            `shouldBe` ["Name", "Seed", "Size", "Days / Month", "Months / Year"]

    it "removes the obsolete Climate UI module and all orchestration wiring" $ do
        doesFileExist climatePath `shouldReturn` False
        T.isInfixOf "climateTab" menuSource `shouldBe` False
        T.isInfixOf "climateTab" generationSource `shouldBe` False

    it "continues submitting hidden clock, astronomy, and climate defaults" $
        forM_ [ "hours_per_day"
              , "minutes_per_hour"
              , "tilt_angle"
              , "day_length"
              , "cycle_days"
              , "phase_offset"
              , "iterations"
              , "coriolis_scale"
              , "wind_drag"
              , "thermal_inertia"
              , "orographic_scale"
              , "evap_scale"
              , "albedo_feedback"
              , "thc_threshold"
              ] $ \field → generationSource `shouldSatisfy` T.isInfixOf field
