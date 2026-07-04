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
    , normalizePathingConfig
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
    , pcMaterialReplanMargin ∷ !Float
      -- ^ Surface-material detour trigger (#312). The greedy mover runs a
      --   local A* detour-check when it steps onto ground whose @move_cost@
      --   exceeds firm ground (1.0) by at least this much (default 0.25 →
      --   move_cost ≥ 1.25) — so units skirt soft ground (sand/mud) when a
      --   firmer route is cheaper. Material costs are mild (far below
      --   `pcReplanCostThreshold`), so without this a greedy mover would
      --   only ever slow down, never reroute. A* still decides whether to
      --   actually detour; this only gates when we ask. Set very high to
      --   disable material-triggered detours.
    , pcUphillSpeedPenalty ∷ !Float
      -- ^ Fraction of move SPEED lost at full uphill grade (#375) —
      --   heading straight up the fall line of a walkable ramp. 0.5 →
      --   straight uphill at half speed; a diagonal traverse loses
      --   proportionally less (the penalty scales with the heading's
      --   uphill component). Routing costs are unaffected — this is the
      --   traversal-speed twin of `pcRampFactor`, applied at the movement
      --   call site like the #312 material factor.
    , pcDownhillSpeedBonus ∷ !Float
      -- ^ Fraction of move speed GAINED at full downhill grade (#375).
      --   Deliberately mild (default 0.1 → at most 10% faster) — downhill
      --   is neutral-to-slightly-faster, never a sprint multiplier.
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
    , pcMaterialReplanMargin = 0.25
    , pcUphillSpeedPenalty  = 0.5
    , pcDownhillSpeedBonus  = 0.1
    }

-- | Clamp a parsed config to runtime-safe ranges. Two invariants the
--   cost/movement code relies on:
--
--     * @fall_trigger_drop@ must be ≥ 1. The fall checks are
--       @drop ≥ pcFallTriggerDrop@; at 0 (or negative) a flat step
--       (@drop == 0@) reads as a fall, so the planner surcharges level
--       ground and the mover enters the Falling transition while
--       walking across same-height tiles.
--     * every cost/penalty must be ≥ 0. A* (and the greedy stepper's
--       replan trigger) assume non-negative edge weights; a negative
--       weight breaks the search's admissibility.
--
--     * the slope speed modifiers (#375) must keep the speed multiplier
--       positive and sane: an uphill penalty ≥ 1 would stop (or reverse)
--       a climbing unit dead, and an unbounded downhill bonus would turn
--       every descent into a sprint. Penalty capped at 0.9 (never slower
--       than 10% speed), bonus at 0.5.
--
--   Out-of-range values are clamped (not rejected) so a single bad knob
--   degrades to a sane default instead of failing engine init.
normalizePathingConfig ∷ PathingConfig → PathingConfig
normalizePathingConfig pc = pc
    { pcClimbFactor         = max 0 (pcClimbFactor pc)
    , pcRampFactor          = max 0 (pcRampFactor pc)
    , pcFallFactor          = max 0 (pcFallFactor pc)
    , pcFallTriggerDrop     = max 1 (pcFallTriggerDrop pc)
    , pcRiverPenalty        = max 0 (pcRiverPenalty pc)
    , pcLakePenalty         = max 0 (pcLakePenalty pc)
    , pcReplanCostThreshold = max 0 (pcReplanCostThreshold pc)
    , pcMaterialReplanMargin = max 0 (pcMaterialReplanMargin pc)
    , pcUphillSpeedPenalty  = min 0.9 (max 0 (pcUphillSpeedPenalty pc))
    , pcDownhillSpeedBonus  = min 0.5 (max 0 (pcDownhillSpeedBonus pc))
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
        <*> o .:? "material_replan_margin" .!= pcMaterialReplanMargin defaultPathingConfig
        <*> o .:? "uphill_speed_penalty"  .!= pcUphillSpeedPenalty  defaultPathingConfig
        <*> o .:? "downhill_speed_bonus"  .!= pcDownhillSpeedBonus  defaultPathingConfig

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
                Right pc → return (normalizePathingConfig pc)
                Left err → do
                    logWarn logger CatInit $ "Error loading pathing config: "
                                           <> T.pack (show err)
                    return defaultPathingConfig
