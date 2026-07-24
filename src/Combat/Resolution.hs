{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Combat resolution: hit-roll → body-part pick → damage → wound →
--   death check. Called from `Combat.Thread.handleCommand` when a
--   `CombatAttack` arrives. Pushes `CombatEvent`s onto the engine's
--   event ring (drained by Lua's combat_log module).
--
-- ## Formula summary (all constants tunable in "Combat.Resolution.Constants")
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
--   raw_hit = 0.7 + (attacker - defender) × 0.3
--   p_hit = clamp(0.05 + mental_effectiveness × (raw_hit - 0.05), 0.05, 0.95)
--   (#353 — 'Combat.Resolution.Common.mentalEffectiveness'; = 1.00
--   reproduces the pre-#353 clamp(raw_hit, 0.05, 0.95) exactly)
--
-- **Body-part picker** (intelligence-blended weighted random):
--   intel_factor = clamp(intelligence / 2.0, 0.0, 1.0)
--   score(p)     = p.area_weight × (1 - intel_factor)
--                + p.tactical_value × intel_factor
--
-- **Damage → severity** (Tier 3 real-units model; see
--   "Combat.Resolution.Constants" for the full derivation and
--   'Combat.Resolution.Damage.computeSeverity'):
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
--
-- Split (issue #550) into focused submodules under "Combat.Resolution.*":
--
--     * "Combat.Resolution.Constants" — every tunable constant for the
--       kinematics, wear, and dodge models.
--     * "Combat.Resolution.Common" — small unit-stat accessors shared
--       across the split (pain, stamina pool, body-part index, the
--       already-dead race check).
--     * "Combat.Resolution.Strike" — the hit-roll skill/evasion terms,
--       the active dodge save, and the joint body-part + wound-kind
--       picker.
--     * "Combat.Resolution.Damage" — the Tier 3 damage model: weapon
--       facet resolution, swing kinematics, penetration, and the
--       per-layer tissue-cascade severity calculation.
--     * "Combat.Resolution.Wear" — weapon/armour wear from a landed
--       hit, and the attacker's stamina + stance drain.
--     * "Combat.Resolution.Events" — combat event construction and the
--       engine's event ring / unit-kill plumbing.
--
--   This module keeps the top-level orchestration ('resolveAttack' /
--   'runResolution') and re-exports the public API unchanged.
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
import Data.IORef (readIORef, atomicModifyIORef')
import Data.List (maximumBy)
import qualified System.Random as Random
import Combat.Types (AttackMode(..), attackModeText)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import Item.Types (ItemDef(..), ItemWeapon(..), ItemInstance(..)
                  , ItemManager(..), lookupItemDef)
import Substance.Types (SubstanceManager)
import Unit.Types (UnitId(..), UnitInstance(..), UnitDef(..)
                  , UnitManager(..), BodyPart(..)
                  , NaturalWeapon(..)
                  , Wound(..))
import Unit.Injury (penetrate, woundFactor)
import Unit.LineOfSight (unitAwareness)
import Blood.Impact (pickImpactWound, spawnImpactBlood)
import Combat.Resolution.Constants (kindStanceFactor)
import Combat.Resolution.Common
    ( painFor, isAlreadyDead, bodyPartIndex, mentalEffectiveness )
import Combat.Resolution.Strike
    ( computeAttackerSkill, computeDefenderEvasion, hitChance
    , defenderDodgeChance, naturalAttackName, pickPartKind )
import Combat.Resolution.Damage
    ( ResolvedStrike(..), swingKinematics, weaponPenetration, computeSeverity )
import Combat.Resolution.Wear
    ( weaponWear, applyWeaponWear, applyArmorWear, applyStaminaDrain )
import Combat.Resolution.Events
    ( missEvent, hitEvent, deathEvent, pushEvent, setDead )

-- ----- Entry point -----

-- | Resolve one attack. No-ops cleanly if either unit is missing,
--   either side's def isn't registered, or either side is already
--   dead (the AI shouldn't be issuing swings then but races happen).
resolveAttack ∷ EngineEnv → Word32 → Word32 → AttackMode → Float → Float → IO ()
resolveAttack env atkRaw tgtRaw mode reachBonus lungeSpeed = do
    logger ← readIORef (loggerRef env)
    um ← readIORef (unitManagerRef env)
    -- Weapon/armor item defs + their worked-material properties are
    -- reached through the `content-registries` capability (#890), not
    -- the broader EngineEnv this module still carries for unit/combat
    -- state (SS7.5 narrows the rest).
    let regs = toContentRegistriesCapability env
    im ← readIORef (crItemManagerRef regs)
    sm ← readIORef (crSubstanceManagerRef regs)
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
        -- #353: mental effectiveness (concentration + euphoria) scales
        -- both saves. Sampled once per side, straight off the same
        -- atk/tgt UnitInstance snapshots the rest of this resolution
        -- already uses.
        atkEff = mentalEffectiveness atk
        defEff = mentalEffectiveness tgt
        -- Baseline 0.7 (most melee swings connect somehow); stat gap
        -- shifts ±0.30. Same RNG budget — one roll — but combat
        -- resolves in reasonable time even when defender stats are
        -- moderately better than attacker.
        pHit = hitChance atkEff atkSkill defEva

    -- Active dodge: an aware defender gets a SECOND save against a
    -- would-be hit. Awareness (LOS + facing cone + perception range)
    -- gates and scales it — an unseen pounce can't be dodged. Computed
    -- before the RNG transaction (it reads world tiles); the roll itself
    -- is drawn inside the atomic block below.
    awareness ← unitAwareness env tgt atk
    let pDodge = awareness
               * defenderDodgeChance defEff tgt (lungeSpeed > 0) pTgt

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

            -- Impact blood (#607): ONE mark per landed hit, chosen from
            -- the ACTUAL per-wound kinds this hit produced — NOT the
            -- swing's headline mechanism. A tissue layer can register as
            -- "fracture"/"internal"/"arterial" even under a "blunt"/
            -- "slash" swing (Unit.Injury.tissueInjuryKind), so the
            -- headline kind/severity alone could both mask a catastrophic
            -- fracture buried in `wounds` and wrongly draw blood for a
            -- swing whose headline reads "blunt" but whose only wound is
            -- "internal". pickImpactWound resolves which single wound
            -- represents the hit (requirement 9: bounded per event, not
            -- per wound). Direction is the real attacker→target vector
            -- (always available here, unlike a fall or a debug
            -- unit.injure call).
            let dx = uiGridX tgt - uiGridX atk
                dy = uiGridY tgt - uiGridY atk
                impactAngle = atan2 dy dx
                impactSeed  = round (impactAngle * 1000.0) ∷ Int
            case pickImpactWound [ (woundKind w, woundSeverity w) | w ← wounds ] of
                Nothing → pure ()
                Just (kind, sev, _) →
                    spawnImpactBlood env (uiPage tgt) (uiGridX tgt) (uiGridY tgt)
                        (uiGridZ tgt) kind sev impactAngle impactSeed
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
