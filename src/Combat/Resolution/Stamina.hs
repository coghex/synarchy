{-# LANGUAGE Strict #-}

-- | The stamina write contract and the combat-side stamina reads
--   (#2834, S8a of the physiology stat-modes epic #2827; design
--   @docs\/designs\/physiology_stat_modes_design.md@ S8.2, S8.3).
--
--   __One pure commit rule.__ 'staminaCore' is the single piece of
--   arithmetic every stamina WRITE shares: the pool tick and the
--   facade's debit\/credit (through @unit.commitStamina@), the combat
--   strike cost ('Combat.Resolution.Wear'), and, through the same
--   'Unit.Stats.invertEffectiveStatWhy' solve, a transition's seed.
--   Each applies it inside its caller's ONE unit-manager update, so a
--   strike committed between a script's read and its write is never
--   refunded.
--
--   __Effective units.__ The delta, every report and every consequence
--   (collapse, @kill_on_zero@, alerts) are EFFECTIVE stamina against the
--   effective maximum ('Combat.Resolution.Common.maxStaminaFor'). With
--   @d@ the sum of the active flat modifiers on @stamina@, @m = 1 + Σp@
--   and @eff b = max 0 ((b + d) × m)@: @before = eff b@, the target is
--   @clamp (before + Δ) 0 max@, and the STORED base is the non-negative
--   one whose effective value is nearest it. Every report is the
--   effective read of what is actually stored, so a report never
--   differs from what an effective read of storage returns:
--
--   > m > 0 : b' = target/m − d   (< 0 → b' = 0, status floored)
--   > m = 0 : b' = b, reports 0   (status zeroMultiplier)
--   > m < 0 : b' = target/m − d   (< 0 → b' = 0, status floored)
--
--   With no modifier this is exactly the old add\/clamp arithmetic.
--   Transition ELIGIBILITY is separate (D-5): an ordinary commit's
--   status never lets a seed through, which is why
--   "Unit.Physiology.Transition" refuses @m ≤ 0@ and a negative seed
--   base itself.
module Combat.Resolution.Stamina
    ( -- * The commit rule
      StaminaStatus(..)
    , staminaStatusText
    , StaminaReport(..)
    , staminaCore
    , staminaModeFor
    , definitionPhysiologyOf
    , staminaCommit
      -- * Combat's reads and writes
    , staminaWorkFraction
    , spendStaminaCost
    , markCombatEngaged
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Combat.Resolution.Common (isAlreadyDead, maxStaminaFor)
import Unit.Physiology
    ( registrySystem, resolveMode, engagementApplied, combatEngagedKey
    , declaresEngagement )
import Unit.Physiology.Types
import Unit.Stats (effectiveStat, modifierTotals)
import Unit.Types
    ( UnitInstance(..), UnitDef(..), UnitManager(..), UnitId, StatModifier )

-- | How an already-Simulated pool was updated.
data StaminaStatus
    = StaminaOk
    | StaminaFloored
      -- ^ the base is clamped at 0 and the modifiers' flat floor reads
      --   above the target
    | StaminaZeroMultiplier
      -- ^ @1 + Σp = 0@: the pool reads 0 whatever its base, which is
      --   left unchanged
    deriving (Show, Eq)

staminaStatusText ∷ StaminaStatus → Text
staminaStatusText StaminaOk             = "ok"
staminaStatusText StaminaFloored        = "floored"
staminaStatusText StaminaZeroMultiplier = "zeroMultiplier"

-- | What one commit did, as effective reads of storage.
data StaminaReport = StaminaReport
    { srBefore  ∷ !Float
    , srAfter   ∷ !Float
    , srMaximum ∷ !Float
    , srStatus  ∷ !StaminaStatus
    , srBase    ∷ !Float
      -- ^ the stored base after the commit
    } deriving (Show, Eq)

-- | The arithmetic: game time, the effective maximum (positive, finite),
--   the pool's own modifiers, the stored base and the effective delta.
--   'Left' only for a non-finite result, which no valid input produces.
staminaCore ∷ Double → Float → [StatModifier] → Float → Double
            → Either Text (Float, StaminaReport)
staminaCore now maxStam mods base delta
    | multiplier ≡ 0 =
        Right ( base, StaminaReport 0 0 maxStam StaminaZeroMultiplier base )
    | isNaN base' ∨ isInfinite base' = Left "unrepresentable_base"
    | otherwise = Right
        ( base'
        , StaminaReport before (effectiveStat now base' mods) maxStam status
                        base' )
  where
    (flat, percent) = modifierTotals now mods
    multiplier = 1 + percent ∷ Float
    before     = effectiveStat now base mods
    target     = min (realToFrac maxStam) (max 0 (realToFrac before + delta))
    raw        = target / realToFrac multiplier - realToFrac flat ∷ Double
    (base', status)
        | raw < 0   = (0, StaminaFloored)
        | otherwise = (realToFrac raw, StaminaOk)

-- | The stamina system's mode for one unit: its definition's physiology,
--   its override and its engagement marker through the one resolver. A
--   registry without the system (a hand-built fixture) leaves stamina
--   unowned, which keeps today's pool-required behavior: Simulated.
staminaModeFor ∷ PhysiologyRegistry → UnitPhysiology → UnitInstance
               → ResolvedMode
staminaModeFor reg up inst = case registrySystem reg "stamina" of
    Nothing  → ModeSimulated
    Just sys → resolveMode sys up (engagementApplied (uiStats inst))
                   (HM.lookup "stamina" (uiPhysioOverrides inst))

-- | What the unit's definition says about physiology; a unit whose
--   definition is not registered has no capabilities.
definitionPhysiologyOf ∷ UnitManager → UnitInstance → UnitPhysiology
definitionPhysiologyOf um inst =
    maybe noUnitPhysiology udPhysiology (HM.lookup (uiDefName inst) (umDefs um))

-- | An ordinary commit of an effective delta against one unit. The whole
--   transaction as a pure step: 'Left' is a named refusal that mutates
--   nothing. Refusals are the invalid calls (an unknown or dead unit, a
--   non-finite delta, a receiver that holds no pool: Nominal and
--   Unsupported each under their OWN reason, and a Simulated one whose
--   pool is absent, which is a broken invariant now that spawn, load and
--   transitions seed it) and an unusable stored value or maximum.
staminaCommit ∷ PhysiologyRegistry → Double → UnitManager → UnitId → Double
              → Either Text (UnitManager, StaminaReport)
staminaCommit reg now um uid delta
    | isNaN delta ∨ isInfinite delta = Left "invalid_amount"
    | otherwise = case HM.lookup uid (umInstances um) of
        Nothing → Left "no_such_unit"
        Just inst → do
            let up = definitionPhysiologyOf um inst
            case HM.lookup (uiDefName inst) (umDefs um) of
                Just def | isAlreadyDead inst def → Left "dead"
                _ → Right ()
            case staminaModeFor reg up inst of
                ModeUnsupported → Left "unsupported"
                ModeNominal     → Left "nominal"
                ModeSimulated   → Right ()
            let maxStam = maxStaminaFor now inst
            when (isNaN maxStam ∨ isInfinite maxStam ∨ maxStam ≤ 0) $
                Left "invalid_maximum"
            cur ← maybe (Left "missing_pool") Right
                        (HM.lookup "stamina" (uiStats inst))
            when (isNaN cur ∨ isInfinite cur) $ Left "invalid_stamina"
            (base', report) ← staminaCore now maxStam
                (HM.lookupDefault [] "stamina" (uiModifiers inst)) cur delta
            let um' | base' ≡ cur = um
                    | otherwise = um { umInstances = HM.insert uid
                        inst { uiStats = HM.insert "stamina" base'
                                                   (uiStats inst) }
                        (umInstances um) }
            pure (um', report)

-- | The attacker's work fraction (design S8.2): a Nominal or Unsupported
--   stamina system reads full (1.0), a Simulated pool its EFFECTIVE value
--   over the effective maximum with combat's 0.3 floor, and a Simulated
--   unit whose pool is absent reads full too (nothing to be winded by).
staminaWorkFraction ∷ PhysiologyRegistry → UnitPhysiology → Double
                    → UnitInstance → Float
staminaWorkFraction reg up now inst = case staminaModeFor reg up inst of
    ModeSimulated → case HM.lookup "stamina" (uiStats inst) of
        Nothing → 1.0
        Just s  →
            let maxS = maxStaminaFor now inst
                eff  = effectiveStat now s
                           (HM.lookupDefault [] "stamina" (uiModifiers inst))
            in if maxS ≤ 0 then 1.0 else clamp 0.3 1.0 (eff / maxS)
    _ → 1.0

-- | A strike's stamina charge: @fraction × effective maximum@ taken from
--   a Simulated pool through 'staminaCore', and nothing otherwise. A
--   Nominal or Unsupported attacker creates no @stamina@ key, and a
--   Simulated one whose pool is absent keeps it absent: the old
--   absent-as-zero insert is what left a poolless species winded after
--   its first swing (S8-F1). Returns the stats the charge leaves.
spendStaminaCost ∷ PhysiologyRegistry → UnitPhysiology → Double → Float
                 → UnitInstance → HM.HashMap Text Float
spendStaminaCost reg up now fraction inst
    | staminaModeFor reg up inst ≢ ModeSimulated = uiStats inst
    | otherwise = case HM.lookup "stamina" (uiStats inst) of
        Nothing → uiStats inst
        Just cur
            | isNaN cur ∨ isInfinite cur → uiStats inst
            | isNaN maxStam ∨ isInfinite maxStam ∨ maxStam ≤ 0 → uiStats inst
            | otherwise → case staminaCore now maxStam
                    (HM.lookupDefault [] "stamina" (uiModifiers inst)) cur
                    (negate (realToFrac (fraction * maxStam))) of
                Left _ → uiStats inst
                Right (base', _) → HM.insert "stamina" base' (uiStats inst)
  where
    maxStam = maxStaminaFor now inst

-- | Detection (SD-20): stamp a declaring unit's absent engagement marker
--   as PENDING. A unit that does not declare the trigger, or whose marker
--   is already set, is left exactly as it is, which makes a burst of
--   admitted attempts idempotent. It creates no stamina and changes no
--   mode.
markCombatEngaged ∷ UnitDef → UnitInstance → UnitInstance
markCombatEngaged def inst
    | not (declaresEngagement (udPhysiology def)) = inst
    | HM.member combatEngagedKey (uiStats inst)   = inst
    | otherwise = inst { uiStats = HM.insert combatEngagedKey 1.0 (uiStats inst) }
