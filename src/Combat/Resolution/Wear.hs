{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Post-swing resource costs: weapon/armour wear from a landed hit,
--   and the attacker's stamina + stance drain from the swing itself.
--   Split (issue #550) out of "Combat.Resolution"; see that module's
--   haddock for the overall resolution flow this feeds.
module Combat.Resolution.Wear
    ( weaponWear
    , applyWeaponWear
    , applyArmorWear
    , staminaCostFraction
    , staminaDrainStats
    , applyStaminaDrain
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.IORef (atomicModifyIORef')
import Combat.Types (AttackMode(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import Item.Types (ItemDef(..), ItemInstance(..), ItemManager(..), lookupItemDef)
import Substance.Types (SubstanceManager, SubstanceDef(..), lookupSubstance)
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import Combat.Resolution.Common (maxStaminaFor)
import Combat.Resolution.Damage (defenderArmor)
import Combat.Resolution.Constants
    ( sharpWearScale, condWearScale, refFractureToughness
    , breakLoadScale, minRigidYield, stanceAttackCost )

-- ----- Weapon wear (degradation per landed hit) -----
-- The hit 'load' on the weapon = the strike driver × how hard the
-- target was relative to the weapon's own material. Cutting flesh
-- barely loads the edge; bashing bone or (future) armour loads it
-- fully. From the load: the edge dulls (sharpness↓), micro-fractures
-- accrue (condition↓, faster for brittle materials), and the weapon
-- snaps if the load exceeds its effective yield.

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
--   stop protecting on the next hit (see 'Combat.Resolution.Damage.defenderArmor').
--   Logs breaks.
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

-- | Stamina cost per swing as a fraction of the attacker's
--   max_stamina. Heavy costs 5× more than quick — that's the only
--   hard-coded ratio in the heavy/quick split; everything else
--   (damage, hit chance, recovery time) is stat-driven.
staminaCostFraction ∷ AttackMode → Float
staminaCostFraction Quick = 0.05
staminaCostFraction Heavy = 0.25

-- | Pure core of 'applyStaminaDrain' — the attacker's post-swing
--   stamina/stance, drained by `cost × max_stamina` and the mode's
--   flat stance cost. Floors at 0 each — the collapse / kill
--   thresholds are enforced by unit_resources.lua (the same tick path
--   that handles walking drain), so this doesn't fire UnitCollapse /
--   UnitKill. Split out (#353) so it's directly unit-testable without
--   an EngineEnv/IORef — e.g. to confirm mental effectiveness (which
--   this never reads) leaves it untouched.
--
--   max_stamina comes from 'Combat.Resolution.Common.maxStaminaFor' (the
--   mirror of Lua's unit_stats derivation), computed from the live stats
--   so a unit with a fresh buff or wound to endurance pays the right
--   fraction.
staminaDrainStats ∷ AttackMode → UnitInstance → HM.HashMap Text Float
staminaDrainStats mode inst =
    let stamina   = HM.lookupDefault 0.0 "stamina" (uiStats inst)
        maxStam   = maxStaminaFor inst
        cost      = staminaCostFraction mode * maxStam
        new       = max 0.0 (stamina - cost)
        -- Stance spent on the swing (absent ⇒ full 1.0).
        -- Floors at 0; unit_resources.lua regenerates it.
        stance    = HM.lookupDefault 1.0 "stance" (uiStats inst)
        stance'   = max 0.0 (stance - stanceAttackCost mode)
    in HM.insert "stance" stance' $ HM.insert "stamina" new (uiStats inst)

-- | Drain the attacker's stamina + stance from one swing (hit or
--   miss). See 'staminaDrainStats' for the pure formula.
applyStaminaDrain ∷ EngineEnv → Word32 → AttackMode → IO ()
applyStaminaDrain env atkRaw mode =
    atomicModifyIORef' (unitManagerRef env) $ \um →
        let uid = UnitId atkRaw
        in case HM.lookup uid (umInstances um) of
            Nothing → (um, ())
            Just inst →
                let inst' = inst { uiStats = staminaDrainStats mode inst }
                    ins   = HM.insert uid inst' (umInstances um)
                in (um { umInstances = ins }, ())
