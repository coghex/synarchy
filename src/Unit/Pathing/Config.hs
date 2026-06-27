{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Loadable tunables for the unit pathing cost function.
--
-- These used to be hard-coded constants in `Unit.Pathing.Cost`
-- (climb / ramp / fall / river / lake penalties + the replan
-- threshold). They now live in a record so they can be loaded from
-- @config/pathing.yaml@ and tuned without recompiling. The numbers in
-- `defaultPathingConfig` are the historical constants — an absent or
-- malformed config file falls back to them, so default behaviour is
-- byte-for-byte the old behaviour.
--
-- The record is deliberately a flat bag of scalars so future
-- material / weather / per-unit modifiers can be appended as new
-- fields (and new YAML keys) without reshaping the call sites that
-- thread it through `stepCost`.
module Unit.Pathing.Config
    ( PathingConfig(..)
    , defaultPathingConfig
    , loadPathingConfig
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:?), (.!=), FromJSON(..), withObject)
import System.Directory (doesFileExist)
import Engine.Core.Log (LoggerState, logWarn, LogCategory(..))

-- | Resolved pathing cost tunables. See the field comments in
--   `Unit.Pathing.Cost` (the constants these replaced) for the meaning
--   and tuning intent of each knob.
data PathingConfig = PathingConfig
    { pcClimbFactor         ∷ !Float -- ^ Cost per +1 z of a CLIFF climb.
    , pcRampFactor          ∷ !Float -- ^ Cost per +1 z of a walkable RAMP.
    , pcFallFactor          ∷ !Float -- ^ Base of the exponential fall cost.
    , pcFallTriggerDrop     ∷ !Int   -- ^ Drop (z) at which a descent
                                      --   becomes a costed/transitioned fall.
    , pcRiverPenalty        ∷ !Float -- ^ Cost added for wading a river tile.
    , pcLakePenalty         ∷ !Float -- ^ Cost added for wading a lake tile.
    , pcReplanCostThreshold ∷ !Float -- ^ Greedy-step cost above which the
                                      --   mover triggers a local A* detour.
    } deriving (Show, Eq)

-- | The historical hard-coded values. Used as the fallback when no
--   config file is present and as the per-field default for any key
--   omitted from the YAML (so a partial file only overrides what it
--   names).
defaultPathingConfig ∷ PathingConfig
defaultPathingConfig = PathingConfig
    { pcClimbFactor         = 10.0
    , pcRampFactor          = 0.5
    , pcFallFactor          = 5.0
    , pcFallTriggerDrop     = 2
    , pcRiverPenalty        = 8.0
    , pcLakePenalty         = 12.0
    , pcReplanCostThreshold = 5.0
    }

-- | Per-key optional parse: any omitted key keeps its default. (Using
--   `.:?`/`.!=` matters — a `.:` on a single missing key would fail the
--   whole document and silently discard every override.)
instance FromJSON PathingConfig where
    parseJSON = withObject "PathingConfig" $ \o → PathingConfig
        <$> o .:? "climb_factor"          .!= pcClimbFactor         defaultPathingConfig
        <*> o .:? "ramp_factor"           .!= pcRampFactor          defaultPathingConfig
        <*> o .:? "fall_factor"           .!= pcFallFactor          defaultPathingConfig
        <*> o .:? "fall_trigger_drop"     .!= pcFallTriggerDrop     defaultPathingConfig
        <*> o .:? "river_penalty"         .!= pcRiverPenalty        defaultPathingConfig
        <*> o .:? "lake_penalty"          .!= pcLakePenalty         defaultPathingConfig
        <*> o .:? "replan_cost_threshold" .!= pcReplanCostThreshold defaultPathingConfig

-- | Load pathing tunables from a YAML file. A missing file is normal
--   (defaults). A malformed file warns and falls back to defaults
--   rather than failing engine init.
loadPathingConfig ∷ LoggerState → FilePath → IO PathingConfig
loadPathingConfig logger path = do
    exists ← doesFileExist path
    if not exists
        then return defaultPathingConfig
        else do
            result ← Yaml.decodeFileEither path
            case result of
                Right pc → return pc
                Left err → do
                    logWarn logger CatInit $ "Error loading pathing config: "
                                           <> T.pack (show err)
                    return defaultPathingConfig
