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
import Data.List (maximumBy)
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
                  , Wound(..), woundEffSeverity)
import Unit.Injury (penetrate, penetrateDeposits, woundFactor, tissueInjuryKind
                   , injuryFloor, capInjurySeverity, allocateSubparts
                   , tissueCapacityWeight, defaultPartCapacity)
import Unit.Command.Types (UnitCommand(..))
import Unit.LineOfSight (unitAwareness)
import Blood.Impact (spawnImpactBlood)

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

-- | Fraction of a lunge's full-body kinetic energy / momentum that couples
--   into the strike (the rest is lost to the attacker's own deceleration,
--   imperfect contact, etc.). At 1.0 a ½·m·v² body-slam dwarfs a swing for
--   heavy units; 0.5 keeps a lunge a heavy hit, not an instant kill.
--   Negligible for light creatures (a squirrel's ½·m·v² is tiny — its lunge
--   is about REACH, not momentum). Tune for feel.
lungeMomentumScale ∷ Float
lungeMomentumScale = 0.5

-- | Scales a part's tissue CAPACITY (Σ tissue-weight × thickness mm, from
--   Unit.Injury.layerCapacity) into its severity-normalising "max HP".
--   Calibrated so the limb/torso capacities land near the old
--   body_mass×max_health_factor values (a baseline thigh ≈ 17 HP), then
--   tuned by feel against headless kills. This is the single global knob
--   that replaced the per-part max_health_factor.
capacityHpScale ∷ Float
capacityHpScale = 0.35

-- | Reference build (a default acolyte: body_mass 22·1.8²·1.0 ≈ 71 kg,
--   15 % fat) the per-layer thickness scaling normalises against, so the
--   yaml authors human-reference soft-tissue thicknesses and every other
--   species' fat/muscle depth derives from its own seeded fat_mass /
--   lean_mass. See `layerThickScale` in `computeSeverity`.
refBodyMass, refFatMass, refLeanMass ∷ Float
refBodyMass = 71.3
refFatMass  = 14.3
refLeanMass = 30.3

-- | Per-LAYER severity normaliser. A wound's severity is its deposited
--   energy over the capacity of the TISSUE that took it (summed across the
--   layers of that tissue type) — so a skin/fat/muscle cut reads as
--   "fraction of soft tissue opened", independent of the bone/organ bulk
--   behind it. This is what lets a dagger register a real laceration on a
--   bear; tuned so it does NOT also shatter an acolyte's skull in one hack.
layerHpScale ∷ Float
layerHpScale = 0.9

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

-- ----- Dodge (active, awareness-gated evasion) -----
-- A SECOND save after the hit roll: a would-be hit can still be slipped
-- if the defender SEES it coming. This is distinct from the passive
-- evasion baked into the hit roll ('computeDefenderEvasion') — that's
-- instinctive footwork/parrying that happens even unaware; THIS is a
-- deliberate sidestep, so it's gated on 'unitAwareness' (LOS + facing +
-- perception). Scales with agility + the learned `dodge` skill. A
-- telegraphed lunge is easier to read (if seen), so it dodges better.
dodgeBase, dodgeAgiScale, dodgeSkillScale, dodgePainScale ∷ Float
dodgeLungeMult, dodgeMaxChance ∷ Float
-- | Floor chance an aware, average (agility 1, no skill) defender dodges.
dodgeBase       = 0.10
-- | Added dodge chance per point of agility above the 1.0 baseline.
dodgeAgiScale   = 0.15
-- | Added dodge chance at full (level-100) dodge skill.
dodgeSkillScale = 0.35
-- | Dodge chance lost at full (normalised 1.0) pain — a hurt unit reacts
--   slower.
dodgePainScale  = 0.50
-- | A lunge is a committed, readable leap — multiply the dodge chance for
--   one (only matters when the defender is aware; an unseen pounce still
--   lands).
dodgeLungeMult  = 1.4
-- | Ceiling on dodge chance, before the awareness factor scales it down.
--   Even a master can't dodge everything.
dodgeMaxChance  = 0.60

kindSeverityFactor ∷ Text → Float
kindSeverityFactor "stab"  = 1.2
kindSeverityFactor "slash" = 1.0
kindSeverityFactor "blunt" = 0.7
kindSeverityFactor _       = 1.0

kindPainFactor ∷ Text → Float
kindPainFactor "slash"      = 1.0
kindPainFactor "stab"       = 1.2
kindPainFactor "blunt"      = 1.5
-- Shared injury kinds (also produced by falls): a broken bone is the
-- most painful common injury; a concussion brings a splitting headache.
kindPainFactor "fracture"   = 1.6
kindPainFactor "concussion" = 1.3
kindPainFactor "internal"   = 1.4
kindPainFactor "arterial"   = 1.2
kindPainFactor "severed"    = 2.0   -- losing a limb is the worst
kindPainFactor _            = 1.0

-- | Pain ceiling; normalised pain = pain / ceiling, clamped 0..1.
painCeiling ∷ Float
painCeiling = 5.0

-- ----- Entry point -----

-- | Resolve one attack. No-ops cleanly if either unit is missing,
--   either side's def isn't registered, or either side is already
--   dead (the AI shouldn't be issuing swings then but races happen).
resolveAttack ∷ EngineEnv → Word32 → Word32 → AttackMode → Float → Float → IO ()
resolveAttack env atkRaw tgtRaw mode reachBonus lungeSpeed = do
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
                            atkRaw tgtRaw mode reachBonus lungeSpeed atk adef tgt tdef
                _ → pure ()
        _ → pure ()

runResolution
    ∷ EngineEnv → LoggerState → ItemManager → SubstanceManager → Double
    → Word32 → Word32 → AttackMode → Float → Float
    → UnitInstance → UnitDef
    → UnitInstance → UnitDef
    → IO ()
runResolution env logger im sm gt atkRaw tgtRaw mode reachBonus lungeSpeed atk adef tgt tdef = do
    let mEquipped = firstEquippedWeapon im (uiEquipment atk)
        mWeapon   = (\(_, _, w) → w) ⊚ mEquipped
        natW      = udNaturalWeapon adef
        bladeCm  = case mWeapon of
            Just w  → iwBladeLength w
            Nothing → maybe 0.0 nwEffectiveBladeLength natW
        atkH     = HM.lookupDefault 1.8 "height" (uiStats atk)
        reachLo  = atkH * 0.1
        -- reachBonus (metres) lifts the top of the reach for a LUNGE — the
        -- leap's strike-reach lets a short attacker hit parts above its
        -- standing height (capped by the leap, so still no impossible hits).
        reachHi  = atkH * 1.1 + bladeCm / 100.0 + reachBonus
        -- Only TARGETABLE macro-parts are aimed at; subparts (skull,
        -- carotid, femur…) are never targeted directly — a hit on their
        -- macro-part is allocated down to them. A body plan with no
        -- subparts (every part targetable) behaves exactly as before.
        targetableParts = filter bpTargetable (udBodyParts tdef)
        reachable = filter
            (\p → bpHeightLow p ≤ reachHi ∧ bpHeightHigh p ≥ reachLo)
            targetableParts
        candidateParts = if null reachable
            then if null targetableParts then udBodyParts tdef
                 else targetableParts    -- safety fallback
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

    -- Active dodge: an aware defender gets a SECOND save against a
    -- would-be hit. Awareness (LOS + facing cone + perception range)
    -- gates and scales it — an unseen pounce can't be dodged. Computed
    -- before the RNG transaction (it reads world tiles); the roll itself
    -- is drawn inside the atomic block below.
    awareness ← unitAwareness env tgt atk
    let pDodge = awareness
               * defenderDodgeChance tgt (lungeSpeed > 0) pTgt

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
            then (rng1, Left False)            -- whiff (attacker missed)
            else
                -- The strike was on target — does the defender slip it?
                let (dodgeRoll, rngD) = Random.uniformR (0.0 ∷ Float, 1.0) rng1
                in if dodgeRoll < pDodge
                    then (rngD, Left True)      -- dodged (defender evaded)
                    else
                        let (partKind, rng2) =
                                pickPartKind rngD atk tdef mWeapon natW
                                    candidateParts
                            -- One extra roll for subpart allocation (the
                            -- 50/50 skull/jaw etc.) — drawn here in the
                            -- same atomic transaction so the whole
                            -- resolution is one RNG step.
                            (alloc, rng3) = Random.uniformR (0.0 ∷ Float, 1.0) rng2
                        in (rng3, Right (partKind, alloc))

    case rngOut of
        Left isDodge → do
            pushEvent env (missEvent gt atkRaw tgtRaw mode
                                     (lungeSpeed > 0) isDodge)
            logDebug logger CatThread $
                (if isDodge then "dodge (" else "miss (")
                          <> attackModeText mode <> "): "
                          <> T.pack (show atkRaw) <> " → "
                          <> T.pack (show tgtRaw)
                          <> " (p_hit=" <> T.pack (show pHit)
                          <> " p_dodge=" <> T.pack (show pDodge) <> ")"
        Right ((partId, kind), allocRoll) → do
            let -- Resolve the swing into one or more (kind, energy-fraction)
                -- COMPONENTS. A natural "paw" (combo_attack) fuses
                -- slash + blunt + a little stab into ONE swing; the dedicated
                -- bite stays a separate stab. Everything else is single-kind.
                isCombo = maybe False nwComboAttack natW
                -- Combo "paw" components are PRESENCE weights (how much of
                -- each mechanism the swing carries), not strict energy
                -- fractions — a paw rakes AND bludgeons hard with the same
                -- motion, so it shouldn't dilute to a third of each. The
                -- claw-tip stab is the minor component; the dedicated bite
                -- is the separate full-strength stab.
                components
                    | isCombo ∧ (kind ≡ "slash" ∨ kind ≡ "blunt") =
                        [("slash", 0.85), ("blunt", 0.70), ("stab", 0.20)]
                    | otherwise = [(kind, 1.0)]
                results =
                    [ (k, computeSeverity sm im atk tdef mEquipped natW
                                tgt partId k mode allocRoll w lungeSpeed)
                    | (k, w) ← components ]
                sevOf (_, (s,_,_,_,_,_,_)) = s
                -- Death/stance/verb-tier scalar = the worst single mechanism;
                -- the headline KIND is the PRIMARY (first) component so a paw
                -- reads as a maul, not a "jab" when the claw-tip stab happens
                -- to score highest. Wear sums; wounds + log detail concat.
                (_, (_, rawDmg, effDmg, _, wHard, _, _)) =
                    maximumBy (\a b → compare (sevOf a) (sevOf b)) results
                severity  = maximum (map sevOf results)
                headKind  = case components of ((k,_):_) → k; _ → kind
                hitLoad   = sum [ l | (_, (_,_,_,l,_,_,_)) ← results ]
                -- Merge the components: the same tissue mustn't be wounded
                -- once per mechanism. Keep the WORST severity per
                -- (subpart, wound-kind) for the wound list and per
                -- (subpart, layer) for the log — so a paw to the head reads
                -- "lacerating the scalp, shattering the skull, destroying the
                -- brain", not "the brain, the brain, and the brain".
                dist =
                    [ (p, k, s)
                    | ((p, k), s) ← HM.toList (HM.fromListWith max
                        [ ((p, k), s)
                        | (_, (_,_,_,_,_,d,_)) ← results, (p, k, s) ← d ]) ]
                logDetail =
                    [ (sub, lyr, mat, s)
                    | ((sub, lyr), (mat, s)) ← HM.toList (HM.fromListWith
                        (\(m, s1) (_, s2) → (m, max s1 s2))
                        [ ((sub, lyr), (mat, s))
                        | (_, (_,_,_,_,_,_,ld)) ← results
                        , (sub, lyr, mat, s) ← ld ]) ]
                -- Combat-log narration data: the limb name, the weapon, and
                -- the per-layer injury detail (serialized for the event
                -- payload as "subpart:layer:material:sevPct|…").
                limbName = maybe partId bpName
                              (HM.lookup partId (bodyPartIndex tdef))
                weaponName = case mEquipped of
                    Just (_, idef, _) → idDisplayName idef
                    Nothing → naturalAttackName natW kind isCombo
                detailStr = T.intercalate "|"
                    [ T.intercalate ":"
                        [ sub, lname, lmat
                        , T.pack (show (round (s * 100) ∷ Int)) ]
                    | (sub, lname, lmat, s) ← logDetail ]
                -- A landed hit produces a DISTRIBUTION of wounds, each on
                -- the SUBPART it landed in. A mixed paw merges its slash,
                -- blunt and stab wounds; the fallback is a single
                -- headline-kind wound on the macro-part (a graze).
                mkWound (pid, k, s) = Wound
                    { woundPart = pid, woundKind = k
                    , woundSeverity = s, woundAt = gt
                    , woundBandage = 1.0, woundClot = 0.0, woundHeal = 0.0, woundDressing = ""
                    , woundInfection = 0.0, woundClean = False, woundInfectionType = ""
                    , woundNecrosis = 0.0 }
                wounds = if null dist
                         then [ mkWound (partId, headKind, severity) ]
                         else map mkWound dist
            -- Append wounds + stamp last-attacker memory atomically so the
            -- bear AI's retaliate candidate always sees a consistent
            -- (wound, attacker) pair on its next tick. Taking a hit also
            -- rocks the victim's stance, by severity × the kind's stance
            -- factor (blunt knocks hardest); it recovers in
            -- unit_resources.lua. Stance + the death check use the OLD
            -- scalar `severity` (total tissue destruction), unchanged by
            -- the distribution — so combat lethality is exactly preserved.
            let stanceHit = clamp 0.0 1.0 (severity * kindStanceFactor headKind)
            atomicModifyIORef' (unitManagerRef env) $ \um' →
                let upd inst = inst
                        { uiWounds          = wounds <> uiWounds inst
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
            pushEvent env (hitEvent gt atkRaw tgtRaw partId headKind
                                     severity rawDmg effDmg mode
                                     limbName weaponName detailStr
                                     (lungeSpeed > 0))

            -- Impact blood (#607): ONE mark per landed hit, keyed off the
            -- SAME headline (kind, severity) the death-check/stance-hit/
            -- verb-tier narration already use — never per-wound, so a
            -- multi-layer paw hit stays bounded to a single decal
            -- (requirement 9). Direction is the real attacker→target
            -- vector (always available here, unlike a fall or a debug
            -- unit.injure call).
            let dx = uiGridX tgt - uiGridX atk
                dy = uiGridY tgt - uiGridY atk
                impactAngle = atan2 dy dx
                impactSeed  = round (impactAngle * 1000.0) ∷ Int
            spawnImpactBlood env (uiPage tgt) (uiGridX tgt) (uiGridY tgt)
                (uiGridZ tgt) headKind severity impactAngle impactSeed
                (Just (UnitId tgtRaw)) gt

            -- Landed hit ⇒ the weapon takes wear (dulls, fractures, can
            -- break). Natural weapons don't wear.
            applyWeaponWear env logger im sm atkRaw hitLoad
            -- ...and any armour the blow struck takes wear too.
            applyArmorWear env logger im sm tgtRaw partId rawDmg wHard

            -- Instant-death check, keyed PURELY off the struck part's
            -- engine `bpVital` flag (severity ≥ 1 on a vital part = outright
            -- kill). The distribution carries the subpart each wound landed
            -- in, so we scan it for a lethal vital injury; the macro
            -- `severity` scalar covers the no-subpart fallback.
            --
            -- DESIGN — NOT A BUG (don't "fix" by flagging brain/neck vital):
            -- for the acolyte ONLY the `heart` is `vital: true`. Destroying
            -- the brain, severing the neck, etc. is deliberately NOT an
            -- instant kill — the unit SURVIVES the moment (the sci-fi
            -- treatment-window conceit) and dies a few seconds later via the
            -- delayed failure meters (neuro / shock / suffocation / organ in
            -- unit_resources.lua) unless treated. Only a destroyed heart
            -- stops the pump immediately. Other body plans (a robot, say)
            -- may legitimately flag several parts vital — this rule is
            -- data-driven precisely so they can, without code changes.
            let isVitalId pid = maybe False bpVital
                                  (HM.lookup pid (bodyPartIndex tdef))
                lethalHit =
                    [ (pid, k) | (pid, k, s) ← dist, s ≥ 1.0, isVitalId pid ]
                macroLethal = isVitalId partId ∧ severity ≥ 1.0 ∧ null dist
            if not (null lethalHit) ∨ macroLethal
                then do
                    setDead env tgtRaw
                    let (cpart, ckind) = case lethalHit of
                            ((p, k) : _) → (p, k)
                            []           → (partId, kind)
                        cause = ckind <> "_" <> cpart
                    pushEvent env (deathEvent gt atkRaw tgtRaw
                                                cause cpart ckind)
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
                            <> " injuries=" <> T.pack (show dist)

    -- Drain stamina on EVERY swing (hit or miss). The motion costs
    -- the same; landing the blow is a separate roll. Cost is a
    -- fraction of max_stamina so endurance drives absolute capacity
    -- without changing the per-swing fraction.
    applyStaminaDrain env atkRaw mode

-- ----- Helpers -----

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

-- | Active-dodge chance BEFORE the awareness scale (caller multiplies by
--   'unitAwareness'). Agility + the learned `dodge` skill drive it; pain
--   slows it; a telegraphed lunge reads easier. Clamped to 'dodgeMaxChance'.
defenderDodgeChance ∷ UnitInstance → Bool → Float → Float
defenderDodgeChance tgt isLunge pain =
    let agi   = statOr "agility" 1.0 tgt
        dodge = skillOr "dodge" 0.0 tgt / 100.0
        base  = dodgeBase
              + dodgeAgiScale   * (agi - 1.0)
              + dodgeSkillScale * dodge
        lungeM = if isLunge then dodgeLungeMult else 1.0
    in clamp 0.0 dodgeMaxChance (base * lungeM - dodgePainScale * pain)

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
-- | Display name of the natural attack that delivered a given kind. A
--   combo paw (slash/blunt trigger) reads as the blunt facet's name
--   ("paw"); a stab is the stab facet ("fangs"); a plain slash the slash
--   facet ("claws"). Falls back to "fists" when unnamed.
naturalAttackName ∷ Maybe NaturalWeapon → Text → Bool → Text
naturalAttackName Nothing _ _ = "fists"
naturalAttackName (Just nw) kind combo =
    let nm sp = let n = spName sp in if T.null n then "fists" else n
    in if combo ∧ (kind ≡ "slash" ∨ kind ≡ "blunt")
       then nm (nwBlunt nw)
       else case kind of
           "stab"  → nm (nwStab nw)
           "slash" → nm (nwSlash nw)
           _       → nm (nwBlunt nw)

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

-- | Resolve a body part's tissue layers (outer→inner), keeping each
--   layer's DISPLAY NAME alongside the resolved substance + thickness so
--   the combat log can name the structures the strike crossed.
resolvePartLayersNamed ∷ SubstanceManager → UnitDef → Text
                       → [(Text, Maybe SubstanceDef, Float)]
resolvePartLayersNamed sm tdef partId =
    let raw = case HM.lookup partId (bodyPartIndex tdef) of
                Just bp | not (null (bpLayers bp)) → bpLayers bp
                _                                  → [("flesh", "flesh", 40.0)]
    in [ (nm, lookupSubstance m sm, t) | (nm, m, t) ← raw ]

-- | Substance + thickness only — the physics path (penetrate) doesn't
--   need the display names.
resolvePartLayers ∷ SubstanceManager → UnitDef → Text
                  → [(Maybe SubstanceDef, Float)]
resolvePartLayers sm tdef partId =
    [ (msub, t) | (_, msub, t) ← resolvePartLayersNamed sm tdef partId ]

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
    → UnitInstance → Text → Text → AttackMode → Float → Float → Float
    → ( Float, Float, Float, Float, Float
      , [(Text, Text, Float)]            -- wound distribution (subpart, kind, sev)
      , [(Text, Text, Text, Float)] )    -- log detail (subpartName, layer, material, sev)
-- The trailing Floats are `allocRoll`, `kindWeight` (the fraction of the
-- swing's energy delivered as THIS mechanism — 1.0 for a single-kind attack;
-- a combo "paw" splits across slash/blunt/stab components that each call in),
-- and `lungeSpeed` (m/s of the attacker's body at impact — 0 for a normal
-- swing; a lunge adds its full-body momentum on top of the swing).
computeSeverity sm im atk tdef mEquipped natW tgt partId kind mode allocRoll kindWeight lungeSpeed =
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
            Just s  → let maxS = maxStaminaFor atk
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

        -- LUNGE: the whole body slams in at `lungeSpeed` (m/s) on top of the
        -- limb's swing — FULL body mass, not the limb's. Adds ½·m·v² of
        -- energy (stab/slash channel) and m·v of momentum (blunt channel),
        -- which then flow through the SAME attenuation as the swing below
        -- (weapon eff, quality, hide, toughness, and kindWeight via budget).
        -- 0 lungeSpeed ⇒ no contribution (an ordinary swing). Scaled so a
        -- leap is a heavy hit without being a guaranteed one-shot.
        lungeKE = lungeMomentumScale * 0.5 * bodyMassA * lungeSpeed * lungeSpeed
        lungeP  = lungeMomentumScale * bodyMassA * lungeSpeed

        -- Stab/slash spend ENERGY; blunt spends MOMENTUM.
        (driver, perHp) = if kind ≡ "blunt"
                          then (pImp + lungeP,  momentumPerHp)
                          else (eKin + lungeKE, energyPerHp)

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
                        * (1.0 - natRes) * (1.0 - toughCut) * kindWeight

        -- Drive it through the struck part's tissue stack — with any
        -- worn armour covering that part prepended as the OUTERMOST
        -- layer(s), so the strike must beat the armour before tissue.
        wp         = weaponPenetration strike kind
        armorLayers = [ (msub, th)
                      | (_, _, msub, th) ← defenderArmor sm im tgt partId ]

        -- Body composition → per-LAYER thickness. The yaml authors
        -- REFERENCE-HUMAN base thicknesses (70 kg, 15 % fat); each layer is
        -- scaled to the actual target: skin/bone/organ by linear size
        -- (∛mass), fat additionally by relative bodyfat, muscle by relative
        -- leanness. A 280 kg bear thus grows a thick fat/muscle barrier from
        -- the SAME template a human uses — "thickness from fat/muscle mass".
        -- The seeded body-composition stats (body_mass = 22·h²·bulk,
        -- fat_mass, lean_mass = skeletal muscle). Fat/muscle layer thickness
        -- is the tissue mass spread over the body's surface area
        -- (∝ mass^⅔); skin/bone/organ scale with linear size (∝ mass^⅓).
        -- Normalised to a reference acolyte so the yaml authors human-scale
        -- thicknesses and every other species derives from its own mass.
        bodyMassT = statOr "body_mass" refBodyMass tgt
        fatMassT  = statOr "fat_mass"  (0.20 * bodyMassT) tgt
        leanMassT = statOr "lean_mass" (0.40 * bodyMassT) tgt
        surfArea m  = m ** (2.0/3.0)
        sizeF       = (bodyMassT / refBodyMass) ** (1.0/3.0)
        fatScale    = (fatMassT  / surfArea bodyMassT) / (refFatMass  / surfArea refBodyMass)
        muscleScale = (leanMassT / surfArea bodyMassT) / (refLeanMass / surfArea refBodyMass)
        layerThickScale mat = case mat of
            "fat"    → fatScale
            "muscle" → muscleScale
            _        → sizeF
        scalePair (msub, th) =
            (msub, th * layerThickScale (maybe "" sbsName msub))
        scaledLayers pid = map scalePair (resolvePartLayers sm tdef pid)

        -- Per-tissue-stack injury distribution. Each layer's wound severity
        -- is normalised by THAT LAYER'S own capacity (capWeight·thickness),
        -- not the whole part — so a dagger that fully pierces thin skin
        -- registers a real skin laceration even behind a 150 kg of bear,
        -- while the deep organ (its capacity ≈ the whole part) still needs a
        -- powerful strike. The whole-part scalar `sev` below (death / reach)
        -- is unchanged, so lethality calibration is preserved.
        layerHpMat mat th =
            max 0.05 (tissueCapacityWeight mat * th * layerHpScale)
        -- Each wound KIND is normalised by the summed capacity of the
        -- layers that produced it: the soft-tissue cut over all the
        -- skin/fat/muscle it crossed, the fracture over the bone, etc. (Not
        -- per-individual-layer — that double-counts a thin layer's tiny
        -- capacity and would let a graze "destroy" the skin.)
        distOf layerSet =
            let deps = penetrateDeposits layerSet budget wp kind
                perKind = HM.fromListWith
                    (\(c1, h1) (c2, h2) → (c1 + c2, h1 + h2))
                    [ (k, (c * kindSeverityFactor kind, layerHpMat tissue th))
                    | ((_, th), (tissue, c)) ← zip layerSet deps
                    , Just k ← [tissueInjuryKind tissue kind] ]
            in [ (k, capInjurySeverity k s)
               | (k, (csum, hsum)) ← HM.toList perKind
               , let s = csum / (max 0.05 hsum * perHp)
               , capInjurySeverity k s ≥ injuryFloor ]
        -- Part "max HP" — the severity normaliser — DERIVED from the part's
        -- COMPOSITION-SCALED tissue layers. Size enters through the layer
        -- THICKNESSES (above), not a separate body-mass multiplier — the old
        -- `massScale` factor double-counted it and made a big animal a sponge
        -- (a 280 kg bear was 4× HP, so a dagger couldn't register a wound).
        partHpId pid =
            let cap = sum [ tissueCapacityWeight (maybe "" sbsName msub) * th
                          | (msub, th) ← scaledLayers pid ]
                cap' = if cap ≤ 0 then defaultPartCapacity * sizeF else cap
            in max 0.5 (cap' * capacityHpScale)

        -- MACRO-part severity (the strike's reach + the death/wear scalar),
        -- on the macro-part's representative tissue stack.
        macroLayers = armorLayers ++ scaledLayers partId
        sevDriver   = penetrate macroLayers budget wp kind

        -- Reaction load on the weapon: strike force × target-vs-weapon
        -- hardness (feeds wear). `driver` is energy (cut) or momentum
        -- (blunt) — intentionally one wear proxy for both.
        weaponHardness = maybe 1.0 sbsHardness (rsSub strike)
        targetHardness = maximum (1.0 : [ maybe 0.0 sbsHardness msub
                                        | (msub, _) ← macroLayers ])
        load = driver * clamp 0.0 2.0 (targetHardness / max 1.0 weaponHardness)

        sev = sevDriver * kindSeverityFactor kind / (partHpId partId * perHp)
        -- Reach: how deep the strike penetrates (0 = surface, 1 = deepest
        -- subpart), from the macro severity. A solid hit reaches the deep
        -- structures; a glance only the shallow ones.
        reach = clamp 0.0 1.0 sev

        -- ALLOCATION to subparts. A hit on the macro-part is distributed to
        -- its (non-targetable) subparts; each runs its OWN tissue cascade
        -- and the wounds are stored on the subpart. A body plan with no
        -- subparts falls back to one wound-set on the macro-part itself
        -- (unchanged behaviour). The WHICH-subpart is weighted-random
        -- ("aim within the part" — the 50/50 skull/jaw); severity stays
        -- deterministic physics.
        subparts = [ p | p ← udBodyParts tdef
                       , bpParent p ≡ Just partId, not (bpTargetable p) ]
        selected = allocateSubparts kind reach allocRoll subparts
        subInjuries p =
            let sl = armorLayers ++ scaledLayers (bpId p)
            in [ (bpId p, k, s) | (k, s) ← distOf sl ]
        -- Scale a named layer's thickness the same way scalePair does.
        scaleNamed (nm, msub, th) =
            (nm, msub, th * layerThickScale (maybe "" sbsName msub))
        injuries = if null subparts
                   then [ (partId, k, s) | (k, s) ← distOf macroLayers ]
                   else concatMap subInjuries selected

        -- PER-LAYER log detail (for the combat-log narration): every named
        -- tissue layer the strike actually crossed in each injured subpart,
        -- with its display name + material + severity. (The `injuries` above
        -- aggregate by kind for the wound list; the log wants the layers
        -- individually — skin, fat, muscle, radius, ulna — so it can say
        -- "lacerating the forearm's skin, fat, and muscle and breaking the
        -- radius".) Returns (subpartDisplayName, layerName, material, sev).
        bpNameOf pid = maybe pid bpName (HM.lookup pid (bodyPartIndex tdef))
        -- One layer-detail path for both the per-subpart case (called with
        -- each selected subpart's id) and the no-subpart macro fallback
        -- (called with the macro part id). The two used to be structural
        -- clones differing only in which part id they resolved.
        layerDetailOfPart pid =
            let named = map scaleNamed (resolvePartLayersNamed sm tdef pid)
                armorNamed = [ ("armor", msub, th) | (msub, th) ← armorLayers ]
                full  = armorNamed ++ named
                deps  = penetrateDeposits [ (s, t) | (_, s, t) ← full ] budget wp kind
                sub   = bpNameOf pid
            in [ (sub, lname, lmat, capInjurySeverity k sv)
               | ((lname, _, th), (lmat, contrib)) ← zip full deps
               , Just k ← [tissueInjuryKind lmat kind]
               , let sv = capInjurySeverity k
                            (contrib * kindSeverityFactor kind
                               / (layerHpMat lmat th * perHp))
               , sv ≥ injuryFloor ]
        logDetail = if null subparts
                    then layerDetailOfPart partId
                    else concatMap (layerDetailOfPart . bpId) selected
    in (clamp 0.0 1.0 sev, driver, sevDriver, load, weaponHardness, injuries, logDetail)

-- ----- Event constructors -----

missEvent ∷ Double → Word32 → Word32 → AttackMode → Bool → Bool → CombatEvent
missEvent gt atk tgt mode isLunge isDodge = CombatEvent
    { ceTs       = gt
    , ceKind     = "miss"
    , ceAttacker = Just atk
    , ceTarget   = Just tgt
    , cePayload  = HM.fromList $
        [ ("mode", attackModeText mode) ]
        <> [ ("lunge", "1") | isLunge ]   -- a missed lunge → "lunges but…"
        <> [ ("dodge", "1") | isDodge ]   -- defender evaded → "X dodges…"
    }

hitEvent
    ∷ Double → Word32 → Word32 → Text → Text
    → Float → Float → Float → AttackMode
    → Text → Text → Text → Bool → CombatEvent
hitEvent gt atk tgt part kind sev rawDmg effDmg mode limb weapon detail isLunge = CombatEvent
    { ceTs       = gt
    , ceKind     = "hit"
    , ceAttacker = Just atk
    , ceTarget   = Just tgt
    , cePayload  = HM.fromList $
        [ ("part",     part)         -- macro-part id
        , ("limb",     limb)         -- macro-part display name ("left arm")
        , ("kind",     kind)         -- mechanism (slash/stab/blunt)
        , ("weapon",   weapon)       -- weapon display name / natural facet
        , ("severity", T.pack (show sev))
        , ("detail",   detail)       -- per-layer "subpart:layer:material:sevPct|…"
        , ("raw",      T.pack (show rawDmg))
        , ("eff",      T.pack (show effDmg))
        , ("mode",     attackModeText mode)
        ]
        <> [ ("lunge", "1") | isLunge ]  -- the combat log opens with a lunge line
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
--   max_stamina comes from 'maxStaminaFor' (the mirror of Lua's
--   unit_stats derivation), computed from the live stats so a unit
--   with a fresh buff or wound to endurance pays the right fraction.
applyStaminaDrain ∷ EngineEnv → Word32 → AttackMode → IO ()
applyStaminaDrain env atkRaw mode =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        let uid = UnitId atkRaw
        in case HM.lookup uid (umInstances um) of
            Nothing → (um, ())
            Just inst →
                let stamina   = HM.lookupDefault 0.0 "stamina"
                                                  (uiStats inst)
                    maxStam   = maxStaminaFor inst
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
