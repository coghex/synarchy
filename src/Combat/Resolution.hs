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
--   p_hit = clamp(0.5 + (attacker - defender) × 0.5, 0.05, 0.95)
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
import Combat.Types (CombatEvent(..))
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
resolveAttack ∷ EngineEnv → Word32 → Word32 → IO ()
resolveAttack env atkRaw tgtRaw = do
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
                    | uiPose atk /= "dead"
                    , uiPose tgt /= "dead" →
                        runResolution env logger im gt
                            atkRaw tgtRaw atk adef tgt tdef
                _ → pure ()
        _ → pure ()

runResolution
    ∷ EngineEnv → LoggerState → ItemManager → Double
    → Word32 → Word32
    → UnitInstance → UnitDef
    → UnitInstance → UnitDef
    → IO ()
runResolution env logger im gt atkRaw tgtRaw atk adef tgt tdef = do
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
        atkSkill = computeAttackerSkill atk mWeapon natW bladeCm pAtk
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
            pushEvent env (missEvent gt atkRaw tgtRaw)
            logDebug logger CatThread $
                "miss: " <> T.pack (show atkRaw) <> " → "
                          <> T.pack (show tgtRaw)
                          <> " (p_hit=" <> T.pack (show pHit) <> ")"
        Right (partId, kind) → do
            let (severity, rawDmg, effDmg) =
                    computeSeverity atk adef tdef mWeapon natW
                                     tgt partId kind
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
                                     severity rawDmg effDmg)

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
                        "hit: " <> T.pack (show atkRaw)
                            <> " → " <> T.pack (show tgtRaw)
                            <> " " <> kind <> "@" <> partId
                            <> " sev=" <> T.pack (show severity)

-- ----- Helpers -----

painFor ∷ UnitInstance → Float
painFor inst =
    let raw = foldl'
              (\acc w → acc + woundSeverity w
                              * kindPainFactor (woundKind w))
              0 (uiWounds inst)
    in clamp 0.0 1.0 (raw / painCeiling)

statOr ∷ Text → Float → UnitInstance → Float
statOr name def inst = HM.lookupDefault def name (uiStats inst)

skillOr ∷ Text → Float → UnitInstance → Float
skillOr name def inst = HM.lookupDefault def name (uiSkills inst)

weightedReachFactor ∷ Float → Float
weightedReachFactor bladeCm = clamp 0.0 1.0 (bladeCm / 100.0)

computeAttackerSkill
    ∷ UnitInstance → Maybe ItemWeapon → Maybe NaturalWeapon
    → Float → Float → Float
computeAttackerSkill atk mWeapon natW bladeCm pain =
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
    in   0.35 * (skill / 100.0)
       + 0.25 * dex
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
        pick _ [] = ("torso", "blunt")   -- safety fallback
        pick acc ((pk, w) : rest)
            | acc + w ≥ r = pk
            | otherwise   = pick (acc + w) rest
    in (pick 0 scored, rng')

computeSeverity
    ∷ UnitInstance → UnitDef → UnitDef
    → Maybe ItemWeapon → Maybe NaturalWeapon
    → UnitInstance → Text → Text
    → (Float, Float, Float)
computeSeverity atk _adef tdef mWeapon natW tgt partId kind =
    let bladeCm = case mWeapon of
            Just w  → iwBladeLength w
            Nothing → maybe 0.0 nwEffectiveBladeLength natW
        materialHardness = 1.0   -- TODO Phase 3: substance-driven
        str   = statOr "strength" 1.0 atk
        wepClass = case mWeapon of
            Just w  → iwWeaponClass w
            Nothing → maybe "unarmed" nwWeaponClass natW
        skill = skillOr wepClass 0.0 atk
        rawDamage = (bladeCm / 100.0)
                  * materialHardness
                  * sqrt str
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

missEvent ∷ Double → Word32 → Word32 → CombatEvent
missEvent gt atk tgt = CombatEvent
    { ceTs       = gt
    , ceKind     = "miss"
    , ceAttacker = Just atk
    , ceTarget   = Just tgt
    , cePayload  = HM.empty
    }

hitEvent
    ∷ Double → Word32 → Word32 → Text → Text
    → Float → Float → Float → CombatEvent
hitEvent gt atk tgt part kind sev rawDmg effDmg = CombatEvent
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
