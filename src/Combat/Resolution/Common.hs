{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Small unit-stat accessors and derived queries shared across the
--   "Combat.Resolution" submodules — pain, stamina, body-part lookup,
--   and the already-dead race check. Split (issue #550) out of
--   "Combat.Resolution"; see that module's haddock for the overall
--   resolution flow.
module Combat.Resolution.Common
    ( painFor
    , isAlreadyDead
    , statOr
    , skillOr
    , maxStaminaFor
    , weightedReachFactor
    , bodyPartIndex
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Unit.Types (UnitInstance(..), UnitDef(..), BodyPart(..), Wound(..)
                  , woundEffSeverity)
import Combat.Resolution.Constants (kindPainFactor, painCeiling)

-- Pain eases as a wound heals (and floors on necrosis): drive it off
-- EFFECTIVE severity, the same quantity bleed/impairment use, so a
-- recovering unit regains composure rather than hurting at the full
-- inflicted level until the wound vanishes. The Lua `unit.getPain`
-- getter mirrors this formula and must change in lockstep.
painFor ∷ UnitInstance → Float
painFor inst =
    let raw = foldl'
              (\acc w → acc + woundEffSeverity w
                              * kindPainFactor (woundKind w))
              0 (uiWounds inst)
    in clamp 0.0 1.0 (raw / painCeiling)

-- | True if the unit is already dead by combat rules: either the
--   uiPose mirror has caught up (post UnitKill), or there's a
--   severity ≥ 1.0 wound on a vital body part (the lethal condition
--   that fires setDead in "Combat.Resolution"). The wound path closes
--   the 1–3 combat-tick race where the UnitKill command has been
--   queued but the unit thread hasn't yet snapped usPose → Dead.
isAlreadyDead ∷ UnitInstance → UnitDef → Bool
isAlreadyDead inst def =
    uiPose inst ≡ "dead"
  ∨ any (\w → woundSeverity w ≥ 1.0
            ∧ maybe False bpVital
                (HM.lookup (woundPart w) (bodyPartIndex def)))
        (uiWounds inst)

statOr ∷ Text → Float → UnitInstance → Float
statOr name def inst = HM.lookupDefault def name (uiStats inst)

skillOr ∷ Text → Float → UnitInstance → Float
skillOr name def inst = HM.lookupDefault def name (uiSkills inst)

-- | The unit's stamina pool size. max_stamina is canonically a DERIVED
--   stat in Lua's @scripts/unit_stats.lua@ (@stats.get@: an explicit
--   per-unit \"max_stamina\" attribute wins, else @endurance × 10@) and
--   is never written back into uiStats, so the combat thread can't just
--   read it — this helper mirrors that dispatch exactly. If the Lua
--   formula changes, change this in lockstep.
maxStaminaFor ∷ UnitInstance → Float
maxStaminaFor inst = case HM.lookup "max_stamina" (uiStats inst) of
    Just m  → m
    Nothing → statOr "endurance" 1.0 inst * 10.0

weightedReachFactor ∷ Float → Float
weightedReachFactor bladeCm = clamp 0.0 1.0 (bladeCm / 100.0)

bodyPartIndex ∷ UnitDef → HM.HashMap Text BodyPart
bodyPartIndex def = HM.fromList [(bpId p, p) | p ← udBodyParts def]
