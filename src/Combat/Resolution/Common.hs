{-# LANGUAGE Strict #-}

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
    , mentalEffectiveness
    , weightedReachFactor
    , bodyPartIndex
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Unit.Types (UnitInstance(..), UnitDef(..), BodyPart(..), Wound(..)
                  , woundEffSeverity)
import Unit.Stats (effectiveStat)
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

-- | The unit's stamina pool size at game time @now@. max_stamina is
--   canonically a DERIVED stat in Lua's @scripts/unit_stats.lua@
--   (@stats.get@: an explicit per-unit \"max_stamina\" attribute wins,
--   else @endurance × 10@) and is never written back into uiStats, so
--   the combat thread can't just read it — this helper mirrors that
--   dispatch exactly. If the Lua formula changes, change this in
--   lockstep.
--
--   #1735: the mirror covers the EFFECTIVE value, not just the
--   dispatch. Lua reads each input through @unit.getStat@, which is
--   base plus this unit's active 'uiModifiers' — so both arms here
--   resolve through 'Unit.Stats.effectiveStat', the same
--   @(base + Σdelta) × (1 + Σpercent)@ composition (clamped at 0) that
--   @unit.getStat@ applies. An equipped accessory's @buffs:@
--   (idBuffs) or a unit def's innate @modifiers:@ (udModifiers) on
--   \"max_stamina\" or \"endurance\" therefore moves combat's pool by
--   exactly what every Lua consumer sees. The explicit attribute still
--   wins outright: when it is present, an @endurance@ modifier is
--   irrelevant, exactly as in Lua.
--
--   @now@ is the ATTACK's captured game-time sample (read once in
--   'Combat.Resolution.resolveAttack'), so a modifier's expiry
--   boundary — active iff @now < smExpiry@, matching
--   'Unit.Stats.effectiveStat' — cannot resolve differently for the
--   two stamina consumers within one resolution.
--
--   What does NOT move the pool: wounds (nothing in the wound path
--   writes uiModifiers), concentration, and mental_state. A unit with
--   no modifier on either input gets exactly the pre-#1735 value.
maxStaminaFor ∷ Double → UnitInstance → Float
maxStaminaFor now inst = case HM.lookup "max_stamina" (uiStats inst) of
    Just m  → effectiveStat now m (modsOn "max_stamina")
    Nothing → effectiveStat now (statOr "endurance" 1.0 inst)
                                (modsOn "endurance")
                * 10.0
  where
    modsOn name = HM.lookupDefault [] name (uiModifiers inst)

-- | The #353 canonical mental-effectiveness multiplier — the ONE
--   authoritative calculation every combat (hit chance, active dodge),
--   craft-progress (Lua, via the @unit.getMentalEffectiveness@ verb
--   that calls straight into this function), and craft-quality
--   consumer reads, so none of the three can drift from the others.
--
--   @concentration@ is clamped to 0..1 (missing ⇒ 1.0, neutral); a
--   missing or non-euphoric @mental_state@ confers no bonus (0 =
--   STABLE in @scripts/mental_state.lua@; only 3 = EUPHORIC qualifies).
--   Normal concentration without euphoria is neutral at 1.00; zero
--   concentration bottoms out at a 25% penalty; euphoria is the sole
--   above-baseline bonus (capped at 1.10).
--
--   #1733: both clamps are 'clampFinite', so a stat map already
--   carrying a non-finite @concentration@ — from any route, not only
--   @unit.addXP@ — is CONTAINED here rather than passed on. A bare
--   'clamp' lets a NaN through untouched, and combat then reads
--   @roll > pHit@ and @dodgeRoll < pDodge@ as 'False', landing every
--   strike and disabling the active dodge. Finite inputs are unchanged.
mentalEffectiveness ∷ UnitInstance → Float
mentalEffectiveness inst =
    let concentration = clampFinite 0.0 1.0 (statOr "concentration" 1.0 inst)
        -- 3.0 = EUPHORIC. Mirrors the mental.STABLE/STRESSED/BREAK/
        -- EUPHORIC = 0,1,2,3 ordering in scripts/mental_state.lua —
        -- change both in lockstep; the Lua side carries the matching
        -- cross-reference on its own definition. An equality TEST needs
        -- no finiteness guard of its own: a non-finite mental_state is
        -- unequal to 3.0 and reads as non-euphoric, which is already
        -- the conservative answer.
        euphoric       = statOr "mental_state" 0.0 inst ≡ 3.0
        base           = 0.75 + 0.25 * concentration
        withEuphoria   = if euphoric then base * 1.10 else base
    in clampFinite 0.75 1.10 withEuphoria

weightedReachFactor ∷ Float → Float
weightedReachFactor bladeCm = clamp 0.0 1.0 (bladeCm / 100.0)

bodyPartIndex ∷ UnitDef → HM.HashMap Text BodyPart
bodyPartIndex def = HM.fromList [(bpId p, p) | p ← udBodyParts def]
