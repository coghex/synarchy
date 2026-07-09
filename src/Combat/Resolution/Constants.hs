{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Tuning constants for the Tier 3 combat damage model — swing
--   kinematics, tissue-capacity normalisation, weapon wear, and the
--   active-dodge save. Split (issue #550) out of "Combat.Resolution"
--   alongside its sibling submodules; see that module's haddock for
--   the model derivation these tunables feed.
module Combat.Resolution.Constants
    ( eHuman
    , modeWork
    , armMassFrac
    , armLengthFrac
    , vHuman
    , modeSpeed
    , refSharpness
    , refHardness
    , refShearWeapon
    , energyPerHp
    , momentumPerHp
    , lungeMomentumScale
    , capacityHpScale
    , refBodyMass, refFatMass, refLeanMass
    , layerHpScale
    , stanceAttackCost
    , kindStanceFactor
    , sharpWearScale
    , condWearScale
    , refFractureToughness
    , breakLoadScale
    , minRigidYield
    , dodgeBase, dodgeAgiScale, dodgeSkillScale, dodgePainScale
    , dodgeLungeMult, dodgeMaxChance
    , kindSeverityFactor
    , kindPainFactor
    , painCeiling
    ) where

import UPrelude
import Combat.Types (AttackMode(..))

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
--   lean_mass. See "layerThickScale" in "Combat.Resolution.Damage".
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
