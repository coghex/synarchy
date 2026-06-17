{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Combat resolution: hit-roll → body-part pick → damage → wound →
--   death check. Called from `Combat.Thread.handleCommand` when a
--   `CombatAttack` arrives. Pushes `CombatEvent`s onto the engine's
--   event ring (drained by Lua's combat_log module).
--
-- ## Formula summary (all constants tunable at the top of the file)
--
-- **Reach band** (which body parts the attacker can target):
--   attacker_reach_low  = height × 0.1
--   attacker_reach_high = height × 1.1 + weapon_blade_length / 100
--   reachable(p) = p.height_low ≤ reach_high ∧ p.height_high ≥ reach_low
--
-- **Hit roll** (multi-stat, single RNG draw):
--   attacker_skill = 0.35·skill + 0.25·dex + 0.10·perc + 0.10·str_clip
--                  + 0.10·reach_factor - 0.001·mass_excess - 0.30·pain
--   defender_evasion = 0.25·agi + 0.25·refl + 0.15·bal + 0.15·perc
--                    + 0.10·dex - 0.001·mass_excess - 0.30·pain
--   p_hit = clamp(0.7 + (attacker - defender) × 0.3, 0.05, 0.95)
--
-- **Body-part picker** (intelligence-blended weighted random):
--   intel_factor = clamp(intelligence / 2.0, 0.0, 1.0)
--   score(p)     = p.area_weight × (1 - intel_factor)
--                + p.tactical_value × intel_factor
--
-- **Damage → severity** (Tier 3 real-units model; see the constant
--   block below for the full derivation and 'computeSeverity'):
--   E_swing  = eHuman × strength × modeWork × skillEff × stamina × (1−pain)   [J]
--   E_eff    = E_swing × η_kind × (1 − natural_resistance[kind])
--                                × (1 − clamp(toughness × 0.05, 0, 0.5))     [J]
--   severity = E_eff × kind_severity_factor / (part_max_hp × energyPerHp)
--   where η_kind is the per-kind lethality efficiency ('strikeEta'),
--   built from the weapon's resolved material + geometry (sharpness,
--   hardness, shear, fracture toughness, blade length, contact area,
--   mass) — uniform across manufactured and natural weapons.
--
-- **Death**: vital part wound at severity ≥ 1.0 → "death" event with
--   cause "<kind>_<part>". Bleed-out / shock paths live in
--   Combat.Wounds.
module Combat.Resolution
    ( resolveAttack
    -- Pure damage-model internals, exposed for unit testing.
    , ResolvedStrike(..)
    , swingKinematics
    , weaponPenetration
    , penetrate
    , woundFactor
    , weaponWear
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import Data.IORef (readIORef, atomicModifyIORef')
import Data.List (foldl')
import Data.Word (Word32)
import qualified System.Random as Random
import Combat.Types (CombatEvent(..), AttackMode(..), attackModeText)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import qualified Engine.Core.Queue as Q
import Item.Types (ItemDef(..), ItemWeapon(..), ItemArmor(..), ItemInstance(..)
                  , ItemManager(..), lookupItemDef)
import Substance.Types (SubstanceManager, SubstanceDef(..), lookupSubstance)
import Unit.Types (UnitId(..), UnitInstance(..), UnitDef(..)
                  , UnitManager(..), BodyPart(..)
                  , NaturalResistance(..), NaturalWeapon(..)
                  , StrikeProfile(..)
                  , Wound(..))
import Unit.Command.Types (UnitCommand(..))

-- ----- Tuning constants -----

-- ===================================================================
-- Tier 3 physical damage model (real-units kinematics). The wielder
-- does muscular WORK on the swing; that work becomes kinetic energy of
-- an effective striking mass at an impact velocity (capped by how fast
-- the limb can move). From the swing we read off ENERGY (what shears /
-- penetrates tissue) and MOMENTUM (what crushes):
--
--   work  = eHuman · strength · modeWork · skillEff · stamina · (1−pain)   [J]
--   m_eff = weaponMass + modeCoupling · bodyMass                          [kg]
--   v_max = vHuman · modeSpeed · (0.6 + 0.4·dexterity)                    [m/s]
--   v     = min(v_max, sqrt(2·work / m_eff))     -- work-limited OR capped
--   E     = ½·m_eff·v²        p = m_eff·v
--
-- Each kind converts its driver into a wound via a mechanism-specific
-- lethality efficiency η_kind (concentration + material gates):
--   stab / slash  — ENERGY-driven (cut/pierce = work to shear tissue)
--   blunt         — MOMENTUM-driven (crush/fracture peak force ∝ impulse)
--
--   delivered = driver · η_kind · (1 − natRes[kind]) · (1 − toughCut)
--   severity  = delivered · kindSeverityFactor[kind] / (partMaxHp · perHp)
--   perHp = energyPerHp (stab/slash) | momentumPerHp (blunt)
--
-- The velocity cap makes light weapons realistic: a strong arm can't
-- pour all its work into flicking a needle (capped → less E and p);
-- heavy weapons are work-limited (full energy + high momentum). Mass
-- thus lives in real momentum, not a fudge factor.
-- ===================================================================

-- | Muscular work (J) a baseline (strength-1.0) wielder commits to a
--   fully heavy swing. A committed human strike is ~150–250 J; 200 mid.
eHuman ∷ Float
eHuman = 200.0

-- | Fraction of the full swing-work each mode commits. Quick trades
--   energy for speed and control (reflected separately in hit chance).
modeWork ∷ AttackMode → Float
modeWork Heavy = 1.0
modeWork Quick = 0.5

-- | Inferred swinging-limb mass as a fraction of body mass (one arm /
--   foreleg ≈ 5%). This is the lever that carries the implement — the
--   real mass behind a blow, not the whole body.
armMassFrac ∷ Float
armMassFrac = 0.05

-- | Inferred swinging-limb length as a fraction of height (single-arm
--   reach ≈ 0.4·height). Sets the lever arm length.
armLengthFrac ∷ Float
armLengthFrac = 0.40

-- | Unloaded reference tip speed (m/s) of a baseline (dexterity-1.0)
--   wielder. Real strike tip speeds run ~8–12 m/s.
vHuman ∷ Float
vHuman = 12.0

-- | Mode adjustment to the speed cap: a quick flick tops out faster
--   than a committed (slower, heavier) heavy swing.
modeSpeed ∷ AttackMode → Float
modeSpeed Heavy = 0.8
modeSpeed Quick = 1.2

-- | Reference @base_sharpness@ of a keen edge/point — the
--   concentration yardstick. Sharper (lower value) → more penetrating
--   power, so the strike spends less energy cutting through each tissue
--   layer.
refSharpness ∷ Float
refSharpness = 100.0

-- | Reference weapon-material hardness (steel ≈ 600). A hard tip
--   penetrates layers more cheaply; a soft one (keratin) struggles.
refHardness ∷ Float
refHardness = 600.0

-- | Reference weapon-edge shear strength (steel ≈ 800 MPa). A strong
--   edge shears through layers; a weak one skids.
refShearWeapon ∷ Float
refShearWeapon = 800.0

-- | Reference layer thickness (mm). A layer this thick, at resistance
--   1.0, costs one unit of the absorb scales below.
layerRefThickness ∷ Float
layerRefThickness = 10.0

-- | Energy (J) a reference-thickness layer of resistance 1.0 absorbs
--   from a cut/pierce, before the weapon's penetration divides it.
--   Sized so a 7 mm skull stops a hand-knife stab but yields to a
--   heavy/spiked blow.
cutAbsorbScale ∷ Float
cutAbsorbScale = 40.0

-- | Momentum (kg·m/s) a reference-thickness layer of resistance 1.0
--   soaks from a blunt impact. Soft layers cushion; the rest transmits
--   inward to crush the core.
bluntAbsorbScale ∷ Float
bluntAbsorbScale = 20.0

-- | ENERGY anchor (stab / slash). Effective wound-energy (J) per point
--   of a body part's max-HP to reach severity 1.0 (part destruction).
--   Grounded in trauma biomechanics — a rib fractures around 30–50 J —
--   scaled so cumulative part destruction lands in the hundreds of J.
energyPerHp ∷ Float
energyPerHp = 2.0

-- | MOMENTUM anchor (blunt). Effective wound-momentum (kg·m/s) per point
--   of max-HP to reach severity 1.0. Crushing scales with impulse, not
--   energy (work-limited swings deliver ~equal energy regardless of
--   weapon mass, so energy can't tell a maul from a pommel — momentum
--   can). Calibrated so a solid heavy maul blow ≈ 0.3 of a human torso.
momentumPerHp ∷ Float
momentumPerHp = 2.5

-- | Stance (combat readiness, 0..1) spent per swing — alongside
--   stamina. A committed heavy swing leaves you far more open than a
--   controlled quick one. At stance 0 a unit can't attack (gated AI-
--   side); it recovers in unit_resources.lua.
stanceAttackCost ∷ AttackMode → Float
stanceAttackCost Heavy = 0.5
stanceAttackCost Quick = 0.25

-- | How much a landed wound rocks the victim's stance, per point of
--   severity. Blunt blows knock you off balance hardest; a clean stab
--   least. Final loss = severity · this (clamped into the 0..1 stance).
kindStanceFactor ∷ Text → Float
kindStanceFactor "blunt" = 1.0
kindStanceFactor "slash" = 0.6
kindStanceFactor "stab"  = 0.5
kindStanceFactor _       = 0.6

-- ----- Weapon wear (degradation per landed hit) -----
-- The hit 'load' on the weapon = the strike driver × how hard the
-- target was relative to the weapon's own material. Cutting flesh
-- barely loads the edge; bashing bone or (future) armour loads it
-- fully. From the load: the edge dulls (sharpness↓), micro-fractures
-- accrue (condition↓, faster for brittle materials), and the weapon
-- snaps if the load exceeds its effective yield.

-- | Edge-keenness lost per unit of hit load.
sharpWearScale ∷ Float
sharpWearScale = 0.6

-- | Structural condition lost per unit of (load / fracture_toughness).
condWearScale ∷ Float
condWearScale = 0.5

-- | Reference fracture toughness (steel ≈ 100). Higher-toughness
--   materials fracture slower and resist snapping.
refFractureToughness ∷ Float
refFractureToughness = 100.0

-- | Converts a material's yield strength (MPa) into the hit-load scale,
--   setting the snap threshold. Tuned so a sound steel blade survives
--   chopping bone but a worn one (or any blade against rigid armour)
--   can break.
breakLoadScale ∷ Float
breakLoadScale = 0.08

-- | Only RIGID materials (yield ≥ this) snap catastrophically under an
--   overload. Soft gear (wool gambeson, leather) just wears its
--   condition down to 0 — it tatters, it doesn't shatter.
minRigidYield ∷ Float
minRigidYield = 50.0

-- | One landed hit's wear on a weapon. Given the weapon's material, the
--   hit @load@, and the current @(sharpness, condition)@ (0..100),
--   returns the new @(sharpness, condition, justBroke)@. Breaking sets
--   condition to 0; fracture toughness both slows the condition loss and
--   raises the effective yield, so an already-fractured blade snaps
--   under less force.
weaponWear ∷ SubstanceDef → Float → Float → Float → (Float, Float, Bool)
weaponWear wsub load sharp cond =
    let ft       = max 0.1 (sbsFractureToughness wsub)
        yield    = sbsYieldStrength wsub
        dSharp   = sharpWearScale * load
        dCond    = condWearScale * load / ft
        effYield = yield * (cond / 100.0) * sqrt (ft / refFractureToughness)
        -- Rigid things snap under overload; soft things only wear out.
        overload = yield ≥ minRigidYield ∧ load > effYield * breakLoadScale
        cond1    = cond - dCond
        broke    = overload ∨ cond1 ≤ 0
        cond'    = if broke then 0.0 else cond1
        sharp'   = max 0.0 (sharp - dSharp)
    in (sharp', cond', broke)

kindSeverityFactor ∷ Text → Float
kindSeverityFactor "stab"  = 1.2
kindSeverityFactor "slash" = 1.0
kindSeverityFactor "blunt" = 0.7
kindSeverityFactor _       = 1.0

kindPainFactor ∷ Text → Float
kindPainFactor "slash" = 1.0
kindPainFactor "stab"  = 1.2
kindPainFactor "blunt" = 1.5
kindPainFactor _       = 1.0

-- | Pain ceiling; normalised pain = pain / ceiling, clamped 0..1.
painCeiling ∷ Float
painCeiling = 5.0

-- ----- Entry point -----

-- | Resolve one attack. No-ops cleanly if either unit is missing,
--   either side's def isn't registered, or either side is already
--   dead (the AI shouldn't be issuing swings then but races happen).
resolveAttack ∷ EngineEnv → Word32 → Word32 → AttackMode → IO ()
resolveAttack env atkRaw tgtRaw mode = do
    logger ← readIORef (loggerRef env)
    um ← readIORef (unitManagerRef env)
    im ← readIORef (itemManagerRef env)
    sm ← readIORef (substanceManagerRef env)
    gt ← readIORef (gameTimeRef env)
    let atkId = UnitId atkRaw
        tgtId = UnitId tgtRaw
    case (HM.lookup atkId (umInstances um),
          HM.lookup tgtId (umInstances um)) of
        (Just atk, Just tgt) →
            case (HM.lookup (uiDefName atk) (umDefs um),
                  HM.lookup (uiDefName tgt) (umDefs um)) of
                (Just adef, Just tdef)
                    -- The uiPose mirror lags the sim by up to one
                    -- unit-thread tick (~33 ms), so checking only
                    -- "uiPose ≠ dead" lets stale CombatAttacks land on
                    -- a target that's already been declared dead by
                    -- this thread. isAlreadyDead also consults the
                    -- wound list, which is updated atomically at the
                    -- moment of the kill.
                    | not (isAlreadyDead atk adef)
                    , not (isAlreadyDead tgt tdef) →
                        runResolution env logger im sm gt
                            atkRaw tgtRaw mode atk adef tgt tdef
                _ → pure ()
        _ → pure ()

runResolution
    ∷ EngineEnv → LoggerState → ItemManager → SubstanceManager → Double
    → Word32 → Word32 → AttackMode
    → UnitInstance → UnitDef
    → UnitInstance → UnitDef
    → IO ()
runResolution env logger im sm gt atkRaw tgtRaw mode atk adef tgt tdef = do
    let mEquipped = firstEquippedWeapon im (uiEquipment atk)
        mWeapon   = (\(_, _, w) → w) ⊚ mEquipped
        natW      = udNaturalWeapon adef
        bladeCm  = case mWeapon of
            Just w  → iwBladeLength w
            Nothing → maybe 0.0 nwEffectiveBladeLength natW
        atkH     = HM.lookupDefault 1.8 "height" (uiStats atk)
        reachLo  = atkH * 0.1
        reachHi  = atkH * 1.1 + bladeCm / 100.0
        reachable = filter
            (\p → bpHeightLow p ≤ reachHi ∧ bpHeightHigh p ≥ reachLo)
            (udBodyParts tdef)
        candidateParts = if null reachable
            then udBodyParts tdef     -- safety fallback
            else reachable

        pAtk = painFor atk
        pTgt = painFor tgt
        atkSkill = computeAttackerSkill atk mWeapon natW bladeCm pAtk mode
        defEva   = computeDefenderEvasion tgt pTgt
        -- Baseline 0.7 (most melee swings connect somehow); stat gap
        -- shifts ±0.30. Same RNG budget — one roll — but combat
        -- resolves in reasonable time even when defender stats are
        -- moderately better than attacker.
        pHit = clamp 0.05 0.95 (0.7 + (atkSkill - defEva) * 0.3)

    -- Single RNG transaction: draw hit-roll + (if hit) the joint
    -- body-part + wound-kind decision from the same generator,
    -- atomically. The joint picker (pickPartKind) blends a random
    -- score by area_weight × weapon.eff against a "smart" score by
    -- tactical_value × resistance-bypass × bleed_factor, using the
    -- attacker's intelligence as the blend coefficient. High-int
    -- attackers naturally target vital low-resistance combos; low-
    -- int attackers flail at whatever's biggest with whatever motion
    -- the weapon supports.
    rngOut ← atomicModifyIORef' (statRNGRef env) $ \rng0 →
        let (roll, rng1) = Random.uniformR (0.0 ∷ Float, 1.0) rng0
        in if roll > pHit
            then (rng1, Left ())               -- miss
            else
                let (partKind, rng2) =
                        pickPartKind rng1 atk tdef mWeapon natW
                            candidateParts
                in (rng2, Right partKind)

    case rngOut of
        Left () → do
            pushEvent env (missEvent gt atkRaw tgtRaw mode)
            logDebug logger CatThread $
                "miss (" <> attackModeText mode <> "): "
                          <> T.pack (show atkRaw) <> " → "
                          <> T.pack (show tgtRaw)
                          <> " (p_hit=" <> T.pack (show pHit) <> ")"
        Right (partId, kind) → do
            let (severity, rawDmg, effDmg, hitLoad, wHard) =
                    computeSeverity sm im atk tdef mEquipped natW
                                     tgt partId kind mode
                w = Wound
                    { woundPart     = partId
                    , woundKind     = kind
                    , woundSeverity = severity
                    , woundAt       = gt
                    }
            -- Append wound + stamp last-attacker memory atomically
            -- so the bear AI's retaliate candidate always sees a
            -- consistent (wound, attacker) pair on its next tick.
            -- Taking a hit also rocks the victim's stance, by severity
            -- × the kind's stance factor (blunt knocks hardest); it
            -- recovers in unit_resources.lua.
            let stanceHit = clamp 0.0 1.0 (severity * kindStanceFactor kind)
            atomicModifyIORef' (unitManagerRef env) $ \um' →
                let upd inst = inst
                        { uiWounds          = w : uiWounds inst
                        , uiLastAttackerUid = Just atkRaw
                        , uiLastAttackerAt  = gt
                        , uiStats           =
                            let s = HM.lookupDefault 1.0 "stance" (uiStats inst)
                            in HM.insert "stance" (max 0.0 (s - stanceHit))
                                                  (uiStats inst)
                        }
                    ins = HM.adjust upd (UnitId tgtRaw)
                                          (umInstances um')
                in (um' { umInstances = ins }, ())
            pushEvent env (hitEvent gt atkRaw tgtRaw partId kind
                                     severity rawDmg effDmg mode)

            -- Landed hit ⇒ the weapon takes wear (dulls, fractures, can
            -- break). Natural weapons don't wear.
            applyWeaponWear env logger im sm atkRaw hitLoad
            -- ...and any armour the blow struck takes wear too.
            applyArmorWear env logger im sm tgtRaw partId rawDmg wHard

            -- Vital-part instant-death check.
            let partMeta = HM.lookup partId (bodyPartIndex tdef)
                isVital  = maybe False bpVital partMeta
            if isVital ∧ severity ≥ 1.0
                then do
                    setDead env tgtRaw
                    let cause = kind <> "_" <> partId
                    pushEvent env (deathEvent gt atkRaw tgtRaw
                                                cause partId kind)
                    logDebug logger CatThread $
                        "death: " <> T.pack (show tgtRaw)
                            <> " by " <> cause
                else
                    logDebug logger CatThread $
                        "hit (" <> attackModeText mode <> "): "
                            <> T.pack (show atkRaw)
                            <> " → " <> T.pack (show tgtRaw)
                            <> " " <> kind <> "@" <> partId
                            <> " sev=" <> T.pack (show severity)

    -- Drain stamina on EVERY swing (hit or miss). The motion costs
    -- the same; landing the blow is a separate roll. Cost is a
    -- fraction of max_stamina so endurance drives absolute capacity
    -- without changing the per-swing fraction.
    applyStaminaDrain env atkRaw mode

-- ----- Helpers -----

painFor ∷ UnitInstance → Float
painFor inst =
    let raw = foldl'
              (\acc w → acc + woundSeverity w
                              * kindPainFactor (woundKind w))
              0 (uiWounds inst)
    in clamp 0.0 1.0 (raw / painCeiling)

-- | True if the unit is already dead by combat rules: either the
--   uiPose mirror has caught up (post UnitKill), or there's a
--   severity ≥ 1.0 wound on a vital body part (the lethal condition
--   that fires setDead in runResolution). The wound path closes the
--   1–3 combat-tick race where the UnitKill command has been queued
--   but the unit thread hasn't yet snapped usPose → Dead.
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

weightedReachFactor ∷ Float → Float
weightedReachFactor bladeCm = clamp 0.0 1.0 (bladeCm / 100.0)

computeAttackerSkill
    ∷ UnitInstance → Maybe ItemWeapon → Maybe NaturalWeapon
    → Float → Float → AttackMode → Float
computeAttackerSkill atk mWeapon natW bladeCm pain mode =
    let wepClass = case mWeapon of
            Just w  → iwWeaponClass w
            Nothing → maybe "unarmed" nwWeaponClass natW
        skill    = skillOr wepClass 0.0 atk
        dex      = statOr "dexterity" 1.0 atk
        perc     = statOr "perception" 1.0 atk
        str      = statOr "strength" 1.0 atk
        bodyMass = statOr "body_mass" 70.0 atk
        strClip  = min 2.0 (str * 0.5)
        massExc  = max 0.0 (bodyMass - 70.0)
        reach    = weightedReachFactor bladeCm
        -- Heavy commits the body forward — you can't redirect mid-swing
        -- the way you can with a quick stab/jab — so the dexterity
        -- contribution halves. The "control" of the swing degrades
        -- proportional to your dex; a high-dex unit pays more for
        -- going heavy than a low-dex unit does (which had less to lose).
        dexMult = case mode of
            Quick → 1.0
            Heavy → 0.5
    in   0.35 * (skill / 100.0)
       + 0.25 * dex * dexMult
       + 0.10 * perc
       + 0.10 * strClip
       + 0.10 * reach
       - 0.001 * massExc
       - 0.30 * pain

computeDefenderEvasion ∷ UnitInstance → Float → Float
computeDefenderEvasion tgt pain =
    let agi      = statOr "agility" 1.0 tgt
        refl     = statOr "reflexes" 1.0 tgt
        -- Balance is a trained skill, not a stat — read from
        -- uiSkills and normalise so level 50 ≈ 1.0 (matches the
        -- old stat-at-mean-1.0 contribution to evasion).
        bal      = skillOr "balance" 50.0 tgt / 50.0
        perc     = statOr "perception" 1.0 tgt
        dex      = statOr "dexterity" 1.0 tgt
        bodyMass = statOr "body_mass" 70.0 tgt
        massExc  = max 0.0 (bodyMass - 70.0)
    in   0.25 * agi
       + 0.25 * refl
       + 0.15 * bal
       + 0.15 * perc
       + 0.10 * dex
       - 0.001 * massExc
       - 0.30 * pain

bodyPartIndex ∷ UnitDef → HM.HashMap Text BodyPart
bodyPartIndex def = HM.fromList [(bpId p, p) | p ← udBodyParts def]

-- | Joint body-part + wound-kind picker. Intelligence-blended:
--
--   * random_score(p, k) = bpAreaWeight[p] × weapon.eff[k]
--     — "what the body and weapon naturally do." Big targets + the
--     weapon's preferred motion dominate.
--   * smart_score(p, k)  = bpTacticalValue[p]
--                        × (1 - target.natural_resistance[k])
--                        × kindSeverityFactor[k]
--                        × bpBleedFactor[p]
--     — "expected damage if I'm thinking it through." Vital parts
--     with low resistance to a sharp-enough kind win, weighted up
--     by bleed potential (a neck cut wins over a head bash).
--   * score(p, k) = random × (1 - intel_factor) + smart × intel_factor
--     where intel_factor = clamp(intelligence / 2.0, 0, 1).
--
--   Weighted-random over all (p, k) pairs where the weapon has
--   non-zero effectiveness for that kind. Single RNG draw.
pickPartKind
    ∷ Random.StdGen → UnitInstance → UnitDef
    → Maybe ItemWeapon → Maybe NaturalWeapon
    → [BodyPart] → ((Text, Text), Random.StdGen)
pickPartKind rng atk tdef mWeapon natW parts =
    let intel = clamp 0.0 1.0 (statOr "intelligence" 1.0 atk / 2.0)
        (wepStab, wepSlash, wepBlunt) = case mWeapon of
            Just w  → (iwStabEff w, iwSlashEff w, iwBluntEff w)
            Nothing → case natW of
                Just nw → ( spEff (nwStab nw)
                          , spEff (nwSlash nw)
                          , spEff (nwBlunt nw) )
                Nothing → (0.0, 0.0, 1.0)
        nres = udNaturalResistance tdef
        kindEff k = case k of
            "stab"  → wepStab
            "slash" → wepSlash
            "blunt" → wepBlunt
            _       → 0.0
        kindResistance k = case k of
            "stab"  → nrStab  nres
            "slash" → nrSlash nres
            "blunt" → nrBlunt nres
            _       → 0.0
        kinds = ["stab", "slash", "blunt"]
        pairs =
            [ ((bpId p, k), randomScore, smartScore)
            | p ← parts
            , k ← kinds
            , let eff = kindEff k
            , eff > 0.0    -- skip kinds the weapon can't deliver
            , let randomScore = bpAreaWeight p * eff
            , let smartScore =
                    bpTacticalValue p
                    * (1.0 - kindResistance k)
                    * kindSeverityFactor k
                    * bpBleedFactor p
            ]
        scored =
            [ (pk, randomScore * (1 - intel) + smartScore * intel)
            | (pk, randomScore, smartScore) ← pairs ]
        total = sum (map snd scored)
        (r, rng') = Random.uniformR (0.0 ∷ Float, max 0.001 total) rng
        -- Safety fallback for `scored == []` (weapon has zero
        -- effectiveness in every kind) or floating-point drift past
        -- the final cumulative weight. Pick a part the target
        -- actually has — literal "torso" was wrong for species that
        -- declare different body-part ids — and the weapon's best
        -- kind, defaulting to "blunt" only when nothing is set.
        fallbackPart = case parts of
            (p:_) → bpId p
            []    → "torso"
        fallbackKind
            | wepBlunt ≥ wepSlash, wepBlunt ≥ wepStab = "blunt"
            | wepSlash ≥ wepStab                      = "slash"
            | otherwise                               = "stab"
        pick _ [] = (fallbackPart, fallbackKind)
        pick acc ((pk, w) : rest)
            | acc + w ≥ r = pk
            | otherwise   = pick (acc + w) rest
    in (pick 0 scored, rng')

-- | A weapon facet resolved for ONE attack kind: material properties +
--   geometry + per-instance state, unified across manufactured and
--   natural weapons. The Tier 3 formula only ever sees one of these —
--   it doesn't know or care whether the source was a steel dagger or a
--   set of keratin claws.
data ResolvedStrike = ResolvedStrike
    { rsSub        ∷ !(Maybe SubstanceDef)  -- ^ material (Nothing ⇒ unknown)
    , rsBladeCm    ∷ !Float                 -- ^ edge/point length (stab, slash)
    , rsSharpness  ∷ !Float                 -- ^ lower = sharper (stab, slash)
    , rsImpactArea ∷ !Float                 -- ^ mm² contact patch (blunt)
    , rsMass       ∷ !Float                 -- ^ kg mass of the striking head
    , rsLength     ∷ !Float                 -- ^ cm; lever length of the implement
    , rsCoM        ∷ !Float                 -- ^ 0..1 centre-of-mass along length
    , rsEff        ∷ !Float                  -- ^ 0..1 weapon suitability for kind
    , rsQuality    ∷ !Float                  -- ^ 0..1 build quality
    , rsCondition  ∷ !Float                  -- ^ 0..1 wear
    }

-- | Resolve the strike for the chosen kind from whichever weapon the
--   attacker is using.
resolveStrike
    ∷ SubstanceManager
    → Maybe (ItemInstance, ItemDef, ItemWeapon) → Maybe NaturalWeapon
    → Text → Float → ResolvedStrike
resolveStrike sm mEquipped natW kind bodyMass = case mEquipped of
    Just (inst, idef, w) →
        let eff0 = case kind of
                "stab"  → iwStabEff w
                "slash" → iwSlashEff w
                _       → iwBluntEff w
            -- Effective sharpness: the def's base value, dulled by the
            -- instance's edge wear (100 = factory edge → base value;
            -- lower → a higher value = duller = less penetration).
            sharpFrac = clamp 10.0 100.0 (iiSharpness inst)
            effSharp0 = iwBaseSharpness w * (100.0 / sharpFrac)
            -- Broken (condition 0): a jagged stub — dull and feeble, but
            -- still in hand (per design: stays equipped, near-useless).
            broken    = iiCondition inst ≤ 0
            effSharp  = if broken then effSharp0 * 4.0 else effSharp0
            eff       = if broken then eff0 * 0.15 else eff0
        in ResolvedStrike
            { rsSub        = lookupSubstance (idMaterial idef) sm
            , rsBladeCm    = iwBladeLength w
            , rsSharpness  = effSharp
            -- Manufactured weapons don't declare a blunt contact patch;
            -- approximate from head mass (heavier ⇒ broader striking face).
            , rsImpactArea = max 1.0 (iiWeight inst * 50.0)
            , rsMass       = iiWeight inst
            , rsLength     = if iwLength w > 0 then iwLength w else iwBladeLength w
            , rsCoM        = iwCenterOfMass w
            , rsEff        = eff
            , rsQuality    = clamp 0.0 1.0 (iiQuality inst / 100.0)
            , rsCondition  = clamp 0.0 1.0 (iiCondition inst / 100.0)
            }
    Nothing → case natW of
        Just nw →
            let sp = case kind of
                    "stab"  → nwStab nw
                    "slash" → nwSlash nw
                    _       → nwBlunt nw
            in ResolvedStrike
                { rsSub        = lookupSubstance (spMaterial sp) sm
                , rsBladeCm    = spBladeCm sp
                , rsSharpness  = if spSharpness sp > 0 then spSharpness sp else 1000.0
                , rsImpactArea = if spImpactArea sp > 0 then spImpactArea sp else 30.0
                -- Appendage (paw/fang) head mass; the swinging LIMB mass
                -- is inferred from body_mass in the kinematics, so this
                -- is just the striking head (small).
                , rsMass       = if spMass sp > 0 then spMass sp else bodyMass * 0.015
                , rsLength     = if spLength sp > 0 then spLength sp
                                 else max 5.0 (spBladeCm sp)
                , rsCoM        = spCenterOfMass sp
                , rsEff        = spEff sp
                , rsQuality    = 1.0    -- natural weapons are "as grown"
                , rsCondition  = 1.0
                }
        -- Bare unarmed, no natural weapon (e.g. a human fist): a soft,
        -- dull blunt strike through flesh.
        Nothing → ResolvedStrike
            { rsSub = lookupSubstance "flesh" sm, rsBladeCm = 0.0
            , rsSharpness = 1000.0, rsImpactArea = 60.0
            , rsMass = bodyMass * 0.01, rsLength = 8.0, rsCoM = 0.5
            , rsEff = 0.3, rsQuality = 1.0, rsCondition = 1.0 }

-- | Weapon penetrating power for cut/pierce: how cheaply the strike
--   opens a channel through tissue (divides each layer's absorb cost).
--   A keen, hard/strong, well-kept, well-made edge penetrates more.
--   >1 = better than the reference weapon. Blunt ignores this (it
--   crushes, it doesn't cut a channel).
weaponPenetration ∷ ResolvedStrike → Text → Float
weaponPenetration rs kind =
    let hardness = maybe 0.0 sbsHardness      (rsSub rs)
        shear    = maybe 0.0 sbsShearStrength (rsSub rs)
        -- rsSharpness is the EFFECTIVE sharpness (the def's base value
        -- already dulled by the instance's edge wear, in resolveStrike),
        -- so condition no longer enters here — a cracked-but-keen blade
        -- still cuts; a dull one doesn't.
        sharpF   = refSharpness / max 1.0 (rsSharpness rs)
        qualityF = 0.6 + 0.4 * rsQuality rs
        matF     = case kind of
            "stab"  → sqrt (max 0.0 hardness / refHardness)   -- hard tip
            "slash" → sqrt (max 0.0 shear    / refShearWeapon) -- strong edge
            _       → 1.0
    in max 0.05 (sharpF * matF * qualityF)

-- | How lethal it is to deposit damage into a given tissue, per kind.
--   The vital innermost core (organ) is deadly; flesh bleeds; bone is
--   structural (a nicked bone is minor, a fractured one under a blunt
--   blow is serious); a hit landing on armour/skin-thin tissue barely
--   counts as a wound.
woundFactor ∷ Text → Text → Float
woundFactor mat kind = case mat of
    "organ"   → 1.5
    "flesh"   → if kind ≡ "blunt" then 0.3 else 0.7
    "bone"    → if kind ≡ "blunt" then 0.7 else 0.2
    "dentin"  → if kind ≡ "blunt" then 0.6 else 0.2
    "keratin" → 0.3
    "chitin"  → 0.2
    _         → 0.1   -- skin-thin / armour plate: the wound is deeper in

-- | Energy (cut/pierce) or momentum (blunt) one tissue layer soaks from
--   a strike. Cut/pierce: proportional to the layer's kind-resistance ×
--   thickness, made cheaper by weapon penetration. Blunt: soft layers
--   cushion (resistance × thickness), no penetration discount.
layerAbsorb ∷ Maybe SubstanceDef → Float → Text → Float → Float
layerAbsorb msub thick kind wp =
    let r = case kind of
            "stab"  → maybe 0.05 sbsStabResistance  msub
            "slash" → maybe 0.05 sbsSlashResistance msub
            _       → maybe 0.15 sbsBluntResistance msub
        thickF = max 0.0 thick / layerRefThickness
    in case kind of
        "blunt" → r * thickF * bluntAbsorbScale
        _       → r * thickF * cutAbsorbScale / max 0.05 wp

-- | Drive a strike through a body part's tissue stack (outer→inner) and
--   return the severity driver: the sum over layers of
--   @woundFactor(tissue) × (damage deposited there)@. The strike spends
--   its @budget@ crossing each layer; outer layers (skin, bone, armour)
--   absorb and can stop it before the vital core, while a strike that
--   powers through dumps its remainder into the deepest tissue. Energy
--   deposited ≈ tissue destroyed, so a deep gash through thick flesh
--   (neck) or a blow that reaches the brain scores high; a blade turned
--   by a skull scores low.
penetrate ∷ [(Maybe SubstanceDef, Float)] → Float → Float → Text → Float
penetrate layers0 budget0 wp kind = go layers0 budget0
  where
    go [] _ = 0.0
    go ((msub, thick) : rest) budget
        | budget ≤ 0 = 0.0
        | otherwise =
            let absorb = layerAbsorb msub thick kind wp
                isLast = null rest
                -- The core (innermost) layer takes whatever reaches it;
                -- an outer layer takes up to what it can absorb.
                dep    = if isLast then budget else min absorb budget
                wf     = woundFactor (maybe "" sbsName msub) kind
            in wf * dep + (if isLast then 0.0 else go rest (budget - dep))

-- | Resolve a body part's tissue layers to substances (outer→inner).
--   A part with no declared layers is treated as solid flesh.
resolvePartLayers ∷ SubstanceManager → UnitDef → Text
                  → [(Maybe SubstanceDef, Float)]
resolvePartLayers sm tdef partId =
    let raw = case HM.lookup partId (bodyPartIndex tdef) of
                Just bp | not (null (bpLayers bp)) → bpLayers bp
                _                                  → [("flesh", 40.0)]
    in [ (lookupSubstance m sm, t) | (m, t) ← raw ]

-- | Intact worn armour covering the struck part — the outer layers to
--   prepend to its tissue stack (and the instances to wear). Broken
--   armour (condition 0) no longer protects, so it's excluded.
defenderArmor ∷ SubstanceManager → ItemManager → UnitInstance → Text
              → [(Text, ItemInstance, Maybe SubstanceDef, Float)]
defenderArmor sm im tgt partId =
    [ (slot, it, lookupSubstance (idMaterial d) sm, iaThickness a)
    | (slot, it) ← HM.toList (uiEquipment tgt)
    , Just d ← [lookupItemDef (iiDefName it) im]
    , Just a ← [idArmor d]
    , partId `elem` iaCovers a
    , iiCondition it > 0
    ]

-- | Swing kinematics as a rotating lever about the shoulder/pivot. The
--   limb (arm/foreleg) is a rod of @armMass@ × @armLen@; the implement
--   is a point mass @wMass@ at its centre of mass (@wCoM@ along its
--   @wLen@), held at the end of the limb. Muscular @work@ (J) spins the
--   system: @ω = √(2·work/I)@, capped by how fast the limb tip can move
--   (@vMax@). Contact is at the tip (radius @R = armLen + wLen@).
--
--   Returns @(kineticEnergy [J], momentum [kg·m/s])@ — stab/slash spend
--   the energy, blunt spends the momentum. All lengths in cm (converted
--   to m here so energy lands in joules).
--
--   Centre of mass matters: a head-heavy implement (wCoM→1) sits at a
--   larger radius → larger moment of inertia → for the same work, lower
--   ω but MORE momentum (@p = √(2·work·I)/R@) than a balanced one of
--   equal mass. That's why a hammer crushes harder than an equal-weight
--   pipe.
swingKinematics
    ∷ Float    -- ^ muscular work (J)
    → Float    -- ^ implement mass (kg)
    → Float    -- ^ implement length (cm)
    → Float    -- ^ implement centre of mass (0..1 from grip)
    → Float    -- ^ limb (arm) length (cm)
    → Float    -- ^ limb (arm) mass (kg)
    → Float    -- ^ dexterity
    → AttackMode
    → (Float, Float)
swingKinematics work wMass wLen wCoM armLen armMass dexterity mode =
    let lw   = wLen   / 100.0                       -- m
        la   = armLen / 100.0                       -- m
        rCoM = la + clamp 0.0 1.0 wCoM * lw          -- implement CoM radius
        bigR = max 0.05 (la + lw)                    -- contact (tip) radius
        iArm = (1.0 / 3.0) * armMass * la * la        -- rod about one end
        iWep = wMass * rCoM * rCoM                    -- point mass at CoM
        inertia = max 1.0e-4 (iArm + iWep)
        vMax = vHuman * modeSpeed mode * (0.6 + 0.4 * dexterity)
        omegaMax = vMax / bigR
        omega = min omegaMax (sqrt (2.0 * work / inertia))
        vTip  = omega * bigR
        mEff  = inertia / (bigR * bigR)
    in (0.5 * inertia * omega * omega, mEff * vTip)

-- | Severity of one landed blow, via the Tier 3 kinematic swing +
--   layered-penetration target model. Returns @(severity, driver,
--   sevDriver, load, weaponHardness)@ — @driver@ is the swing's raw
--   energy (J, stab/slash) or momentum (kg·m/s, blunt); @sevDriver@ the
--   tissue-weighted damage that survived the layer stack; @load@ the
--   reaction stress on the weapon (feeds 'weaponWear'); @weaponHardness@
--   the attacker material's hardness (feeds armour wear).
computeSeverity
    ∷ SubstanceManager → ItemManager
    → UnitInstance → UnitDef
    → Maybe (ItemInstance, ItemDef, ItemWeapon) → Maybe NaturalWeapon
    → UnitInstance → Text → Text → AttackMode
    → (Float, Float, Float, Float, Float)
computeSeverity sm im atk tdef mEquipped natW tgt partId kind mode =
    let -- Muscular work committed to the swing (J).
        str      = statOr "strength" 1.0 atk
        wepClass = case mEquipped of
            Just (_, _, w) → iwWeaponClass w
            Nothing        → maybe "unarmed" nwWeaponClass natW
        skill    = skillOr wepClass 0.0 atk
        skillEff = 0.6 + 0.4 * clamp 0.0 1.0 (skill / 100.0)
        -- A winded fighter still hits (floor 0.3); absent stamina ⇒ full.
        staminaFrac = case HM.lookup "stamina" (uiStats atk) of
            Nothing → 1.0
            Just s  → let maxS = statOr "endurance" 1.0 atk * 10.0
                      in if maxS ≤ 0 then 1.0 else clamp 0.3 1.0 (s / maxS)
        pain     = painFor atk
        work     = eHuman * str * modeWork mode * skillEff * staminaFrac
                          * (1.0 - pain)

        -- Resolve the weapon facet for this kind.
        bodyMassA = statOr "body_mass" 70.0 atk
        dexA      = statOr "dexterity" 1.0 atk
        heightA   = statOr "height" 1.8 atk           -- metres
        strike    = resolveStrike sm mEquipped natW kind bodyMassA

        -- Swing kinematics: the implement on the lever of an inferred
        -- limb (mass ≈ 5% body, length ≈ 0.4·height). Yields the
        -- delivered energy + momentum.
        armLen  = armLengthFrac * heightA * 100.0     -- cm
        armMass = armMassFrac * bodyMassA             -- kg
        (eKin, pImp) = swingKinematics work (rsMass strike) (rsLength strike)
                                       (rsCoM strike) armLen armMass dexA mode

        -- Stab/slash spend ENERGY; blunt spends MOMENTUM.
        (driver, perHp) = if kind ≡ "blunt"
                          then (pImp, momentumPerHp)
                          else (eKin, energyPerHp)

        -- Weapon suitability + build set how much of the driver enters
        -- the body; the creature's hide (per-unit natural_resistance) is
        -- the outermost attenuation layer; toughness a general resilience.
        --
        -- INTENTIONAL — do NOT "fix" as double-counting: build quality
        -- (rsQuality) deliberately enters TWO distinct physical channels.
        -- (1) here in 'budget' (energy/momentum coupling into the body),
        -- and (2) again inside 'weaponPenetration' below (how cleanly a
        -- cutting EDGE concentrates that energy to open a channel). The
        -- penetration channel is edged-only — 'layerAbsorb' ignores wp
        -- for blunt — so quality compounds for stab/slash but applies
        -- only once for blunt. That asymmetry is the point: a blade gets
        -- markedly worse as quality drops, but a low-quality club is
        -- still a club (it crushes with its mass regardless of finish).
        -- weaponPenetration's quality term is pinned by a unit test
        -- ("a better-made weapon penetrates more", Combat/Damage.hs).
        qualityF = 0.6 + 0.4 * rsQuality strike
        natRes = case kind of
            "slash" → nrSlash (udNaturalResistance tdef)
            "stab"  → nrStab  (udNaturalResistance tdef)
            "blunt" → nrBlunt (udNaturalResistance tdef)
            _       → 0.0
        toughness = statOr "toughness" 1.0 tgt
        toughCut  = clamp 0.0 0.5 (toughness * 0.05)
        budget = driver * rsEff strike * qualityF
                        * (1.0 - natRes) * (1.0 - toughCut)

        -- Drive it through the struck part's tissue stack — with any
        -- worn armour covering that part prepended as the OUTERMOST
        -- layer(s), so the strike must beat the armour before tissue.
        wp         = weaponPenetration strike kind
        armorLayers = [ (msub, th)
                      | (_, _, msub, th) ← defenderArmor sm im tgt partId ]
        layers     = armorLayers ++ resolvePartLayers sm tdef partId
        sevDriver  = penetrate layers budget wp kind

        -- Reaction load on the weapon: the strike force × how hard the
        -- target was relative to the weapon (cutting flesh barely loads
        -- it; striking bone / rigid armour loads it fully). Feeds wear.
        --
        -- NOTE (intentional, not a unit bug): `driver` is energy (J) for
        -- cut/pierce but momentum (kg·m/s) for blunt, and both feed the
        -- one wear calibration in 'weaponWear'. That's deliberate — wear
        -- is a coarse "strike force" proxy, the two drivers share a
        -- magnitude range, and we only care about relative wear rates,
        -- not absolute units. The trade-off: retuning eHuman vs
        -- momentumPerHp independently would drift cut-vs-blunt break
        -- rates apart, so keep them in step if you rescale either.
        weaponHardness = maybe 1.0 sbsHardness (rsSub strike)
        targetHardness = maximum (1.0 : [ maybe 0.0 sbsHardness msub
                                        | (msub, _) ← layers ])
        load = driver * clamp 0.0 2.0 (targetHardness / max 1.0 weaponHardness)

        partMaxHp = case HM.lookup partId (bodyPartIndex tdef) of
            Just p  → max 0.5 (statOr "body_mass" 70.0 tgt
                              * bpMaxHealthFactor p)
            Nothing → 50.0
        sev = sevDriver * kindSeverityFactor kind / (partMaxHp * perHp)
    in (clamp 0.0 1.0 sev, driver, sevDriver, load, weaponHardness)

-- ----- Event constructors -----

missEvent ∷ Double → Word32 → Word32 → AttackMode → CombatEvent
missEvent gt atk tgt mode = CombatEvent
    { ceTs       = gt
    , ceKind     = "miss"
    , ceAttacker = Just atk
    , ceTarget   = Just tgt
    , cePayload  = HM.fromList
        [ ("mode", attackModeText mode) ]
    }

hitEvent
    ∷ Double → Word32 → Word32 → Text → Text
    → Float → Float → Float → AttackMode → CombatEvent
hitEvent gt atk tgt part kind sev rawDmg effDmg mode = CombatEvent
    { ceTs       = gt
    , ceKind     = "hit"
    , ceAttacker = Just atk
    , ceTarget   = Just tgt
    , cePayload  = HM.fromList
        [ ("part",     part)
        , ("kind",     kind)
        , ("severity", T.pack (show sev))
        , ("raw",      T.pack (show rawDmg))
        , ("eff",      T.pack (show effDmg))
        , ("mode",     attackModeText mode)
        ]
    }

deathEvent
    ∷ Double → Word32 → Word32 → Text → Text → Text → CombatEvent
deathEvent gt atk tgt cause part kind = CombatEvent
    { ceTs       = gt
    , ceKind     = "death"
    , ceAttacker = Just atk
    , ceTarget   = Just tgt
    , cePayload  = HM.fromList
        [ ("cause", cause)
        , ("part",  part)
        , ("kind",  kind)
        ]
    }

pushEvent ∷ EngineEnv → CombatEvent → IO ()
pushEvent env ev =
    atomicModifyIORef' (combatEventsRef env) $ \buf →
        (buf Seq.|> ev, ())

-- | Set a unit to Dead via the engine's UnitKill command path
--   (uiPose is a mirror of sim usPose — writing it directly is
--   overwritten by publishToRender each frame). UnitKill snaps the
--   sim-side pose and clears in-flight state.
setDead ∷ EngineEnv → Word32 → IO ()
setDead env tgtRaw =
    Q.writeQueue (unitQueue env) (UnitKill (UnitId tgtRaw))

-- | Stamina cost per swing as a fraction of the attacker's
--   max_stamina. Heavy costs 5× more than quick — that's the only
--   hard-coded ratio in the heavy/quick split; everything else
--   (damage, hit chance, recovery time) is stat-driven.
staminaCostFraction ∷ AttackMode → Float
staminaCostFraction Quick = 0.05
staminaCostFraction Heavy = 0.25

-- | Drain the attacker's stamina by `cost × max_stamina`. Floors at
--   0 — the collapse / kill thresholds are enforced by unit_resources.lua
--   (the same tick path that handles walking drain), so we don't fire
--   UnitCollapse / UnitKill from here.
--
--   max_stamina is recomputed live in Lua's unit_stats wrapper as
--   `endurance × 10`, but the engine doesn't have that derivation —
--   it only sees what's actually stored in uiStats. We use the live
--   "endurance" stat to compute the same value so a unit with a fresh
--   buff or wound to endurance pays the right fraction.
applyStaminaDrain ∷ EngineEnv → Word32 → AttackMode → IO ()
applyStaminaDrain env atkRaw mode =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        let uid = UnitId atkRaw
        in case HM.lookup uid (umInstances um) of
            Nothing → (um, ())
            Just inst →
                let stamina   = HM.lookupDefault 0.0 "stamina"
                                                  (uiStats inst)
                    endurance = HM.lookupDefault 1.0 "endurance"
                                                  (uiStats inst)
                    maxStam   = endurance * 10.0
                    cost      = staminaCostFraction mode * maxStam
                    new       = max 0.0 (stamina - cost)
                    -- Stance spent on the swing (absent ⇒ full 1.0).
                    -- Floors at 0; unit_resources.lua regenerates it.
                    stance    = HM.lookupDefault 1.0 "stance" (uiStats inst)
                    stance'   = max 0.0 (stance - stanceAttackCost mode)
                    inst'     = inst
                        { uiStats = HM.insert "stance" stance'
                                  $ HM.insert "stamina" new (uiStats inst) }
                    ins       = HM.insert uid inst' (umInstances um)
                in (um { umInstances = ins }, ())

-- | Apply one landed hit's wear to the attacker's equipped weapon —
--   dulls the edge (iiSharpness), accrues fractures (iiCondition), and
--   may break it (condition → 0). Natural weapons carry no instance and
--   so don't wear. Logs a break.
applyWeaponWear ∷ EngineEnv → LoggerState → ItemManager → SubstanceManager
                → Word32 → Float → IO ()
applyWeaponWear env logger im sm atkRaw load = do
    mBroke ← atomicModifyIORef' (unitManagerRef env) $ \um →
        case HM.lookup (UnitId atkRaw) (umInstances um) of
            Nothing   → (um, Nothing)
            Just inst → case findWeaponSlot (uiEquipment inst) of
                Nothing → (um, Nothing)
                Just (slot, witem) →
                    case lookupItemDef (iiDefName witem) im of
                        Just d
                          | Just _    ← idWeapon d
                          , Just wsub ← lookupSubstance (idMaterial d) sm →
                            let (sharp', cond', broke) =
                                    weaponWear wsub load (iiSharpness witem)
                                                         (iiCondition witem)
                                witem' = witem { iiSharpness = sharp'
                                               , iiCondition = cond' }
                                inst'  = inst { uiEquipment =
                                    HM.insert slot witem' (uiEquipment inst) }
                            in ( um { umInstances = HM.insert (UnitId atkRaw)
                                          inst' (umInstances um) }
                               , if broke then Just (iiDefName witem) else Nothing )
                        _ → (um, Nothing)
    case mBroke of
        Just nm → logDebug logger CatThread $
            "weapon broke: " <> nm <> " (unit " <> T.pack (show atkRaw) <> ")"
        Nothing → pure ()
  where
    findWeaponSlot eq = go ["right_hand", "left_hand"]
      where
        go [] = Nothing
        go (s : rest) = case HM.lookup s eq of
            Just it | Just d ← lookupItemDef (iiDefName it) im
                    , Just _ ← idWeapon d → Just (s, it)
            _ → go rest

-- | Apply one landed hit's wear to the defender's armour covering the
--   struck part. Each covering piece's load = the strike force scaled
--   by the weapon's hardness relative to the armour's (a hard weapon
--   stresses armour more; a soft one slides off). Soft armour wears its
--   condition down; rigid armour can snap. Broken pieces (condition 0)
--   stop protecting on the next hit (see 'defenderArmor'). Logs breaks.
applyArmorWear ∷ EngineEnv → LoggerState → ItemManager → SubstanceManager
               → Word32 → Text → Float → Float → IO ()
applyArmorWear env logger im sm tgtRaw partId driver weaponHardness = do
    broken ← atomicModifyIORef' (unitManagerRef env) $ \um →
        case HM.lookup (UnitId tgtRaw) (umInstances um) of
            Nothing   → (um, [])
            Just inst →
                let pieces = defenderArmor sm im inst partId
                    step (eq, brk) (slot, it, msub, _) = case msub of
                        Nothing   → (eq, brk)
                        Just asub →
                            let aHard = max 1.0 (sbsHardness asub)
                                aLoad = driver
                                      * clamp 0.1 1.5 (weaponHardness / aHard)
                                (sh', cn', bk) = weaponWear asub aLoad
                                    (iiSharpness it) (iiCondition it)
                                it' = it { iiSharpness = sh', iiCondition = cn' }
                            in ( HM.insert slot it' eq
                               , if bk then iiDefName it : brk else brk )
                    (eq', brk) = foldl' step (uiEquipment inst, []) pieces
                    inst' = inst { uiEquipment = eq' }
                in ( um { umInstances =
                            HM.insert (UnitId tgtRaw) inst' (umInstances um) }
                   , brk )
    mapM_ (\nm → logDebug logger CatThread
              ("armor broke: " <> nm <> " (unit " <> T.pack (show tgtRaw) <> ")"))
          broken

-- | The first equipped weapon, with its instance (for quality /
--   condition / weight) and def (for the material substance) — the
--   combat formula needs all three.
firstEquippedWeapon
    ∷ ItemManager → HM.HashMap Text ItemInstance
    → Maybe (ItemInstance, ItemDef, ItemWeapon)
firstEquippedWeapon im eq = go ["right_hand", "left_hand"]
  where
    go [] = Nothing
    go (slot:rest) = case HM.lookup slot eq of
        Nothing → go rest
        Just it → case lookupItemDef (iiDefName it) im of
            Just d | Just w ← idWeapon d → Just (it, d, w)
            _ → go rest
