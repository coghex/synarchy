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
-- **Damage → severity**:
--   raw_damage = (blade_length / 100) × material_hardness
--              × sqrt(strength) × (1 + skill/100 × 0.3)
--   effective  = raw_damage × (1 - natural_resistance[kind])
--                            × (1 - clamp(toughness × 0.05, 0, 0.5))
--   severity   = effective × kind_severity_factor × severityScale
--              / part_max_hp
--
-- **Death**: vital part wound at severity ≥ 1.0 → "death" event with
--   cause "<kind>_<part>". Bleed-out / shock paths live in
--   Combat.Wounds.
module Combat.Resolution
    ( resolveAttack
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
import Item.Types (ItemDef(..), ItemWeapon(..), ItemInstance(..)
                  , ItemManager(..), lookupItemDef)
import Unit.Types (UnitId(..), UnitInstance(..), UnitDef(..)
                  , UnitManager(..), BodyPart(..)
                  , NaturalResistance(..), NaturalWeapon(..)
                  , Wound(..))
import Unit.Command.Types (UnitCommand(..))

-- ----- Tuning constants -----

-- | Multiplier on per-wound severity. Tuned (via smoke test) so a
--   steel-dagger acolyte vs default-roll bear builds severity-0.1
--   wounds per hit on average, killing via torso destruction in
--   ~10–14 hits or via blood-loss earlier when hitting major bleed
--   sites (neck/torso).
severityScale ∷ Float
severityScale = 30.0

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
                        runResolution env logger im gt
                            atkRaw tgtRaw mode atk adef tgt tdef
                _ → pure ()
        _ → pure ()

runResolution
    ∷ EngineEnv → LoggerState → ItemManager → Double
    → Word32 → Word32 → AttackMode
    → UnitInstance → UnitDef
    → UnitInstance → UnitDef
    → IO ()
runResolution env logger im gt atkRaw tgtRaw mode atk adef tgt tdef = do
    let mWeapon  = firstEquippedWeapon im (uiEquipment atk)
        natW     = udNaturalWeapon adef
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
            let (severity, rawDmg, effDmg) =
                    computeSeverity atk adef tdef mWeapon natW
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
            atomicModifyIORef' (unitManagerRef env) $ \um' →
                let upd inst = inst
                        { uiWounds          = w : uiWounds inst
                        , uiLastAttackerUid = Just atkRaw
                        , uiLastAttackerAt  = gt
                        }
                    ins = HM.adjust upd (UnitId tgtRaw)
                                          (umInstances um')
                in (um' { umInstances = ins }, ())
            pushEvent env (hitEvent gt atkRaw tgtRaw partId kind
                                     severity rawDmg effDmg mode)

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
                Just nw → (nwStabEff nw, nwSlashEff nw, nwBluntEff nw)
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

computeSeverity
    ∷ UnitInstance → UnitDef → UnitDef
    → Maybe ItemWeapon → Maybe NaturalWeapon
    → UnitInstance → Text → Text → AttackMode
    → (Float, Float, Float)
computeSeverity atk _adef tdef mWeapon natW tgt partId kind mode =
    let bladeCm = case mWeapon of
            Just w  → iwBladeLength w
            Nothing → maybe 0.0 nwEffectiveBladeLength natW
        materialHardness = 1.0   -- TODO Phase 3: substance-driven
        str   = statOr "strength" 1.0 atk
        wepClass = case mWeapon of
            Just w  → iwWeaponClass w
            Nothing → maybe "unarmed" nwWeaponClass natW
        skill = skillOr wepClass 0.0 atk
        -- Quick = sqrt(str): a controlled, finesse-leaning motion that
        -- only gets a square-root of raw strength into the swing.
        -- Heavy = str: full commitment, linear strength application.
        -- At str=1 the two are equal; at str=3 heavy is ~73% bigger.
        -- So heavy is strictly worth it for high-strength units (bears,
        -- ogres, …) and a wash for low-strength ones (acolytes choose
        -- quick because stamina + recovery still favour it). No
        -- artificial damage multiplier — the gap is the strength stat.
        strContribution = case mode of
            Quick → sqrt str
            Heavy → str
        rawDamage = (bladeCm / 100.0)
                  * materialHardness
                  * strContribution
                  * (1.0 + (skill / 100.0) * 0.3)
        natRes = case kind of
            "slash" → nrSlash (udNaturalResistance tdef)
            "stab"  → nrStab  (udNaturalResistance tdef)
            "blunt" → nrBlunt (udNaturalResistance tdef)
            _       → 0.0
        toughness = statOr "toughness" 1.0 tgt
        toughCut  = clamp 0.0 0.5 (toughness * 0.05)
        eff       = rawDamage * (1.0 - natRes) * (1.0 - toughCut)
        partMaxHp = case HM.lookup partId (bodyPartIndex tdef) of
            Just p  → max 0.5 (statOr "body_mass" 70.0 tgt
                              * bpMaxHealthFactor p)
            Nothing → 50.0
        sev = eff * kindSeverityFactor kind
                  * severityScale
                  / partMaxHp
    in (clamp 0.0 1.0 sev, rawDamage, eff)

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
                    inst'     = inst
                        { uiStats = HM.insert "stamina" new
                                              (uiStats inst) }
                    ins       = HM.insert uid inst' (umInstances um)
                in (um { umInstances = ins }, ())

firstEquippedWeapon
    ∷ ItemManager → HM.HashMap Text ItemInstance → Maybe ItemWeapon
firstEquippedWeapon im eq = go ["right_hand", "left_hand"]
  where
    go [] = Nothing
    go (slot:rest) = case HM.lookup slot eq of
        Nothing → go rest
        Just it → case lookupItemDef (iiDefName it) im of
            Just d | Just w ← idWeapon d → Just w
            _ → go rest
