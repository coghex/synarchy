{-# LANGUAGE Strict #-}
-- | Pure stat-rolling primitives.
--
-- 'rollStat' draws ONE sample from a normal distribution with mean
-- = @base@ and sigma = @range@ / 4, then CLAMPS it to the window
-- [base - range/2, base + range/2] — an out-of-window sample is
-- pinned to the nearest bound, never rejected and redrawn, so each
-- bound carries the ~2.3% tail mass beyond it. A final clamp keeps
-- results at or above zero — strength below zero is meaningless
-- (\"too weak to breathe\"); the upper bound is intentionally soft
-- so creatures like dragons (base ≫ 100) work the same way.
--
-- Range = 0 returns @base@ unchanged (no roll, no RNG consumption).
-- 'standardStat' is the no-draw value a template centres on, for the
-- test-only standard spawn profile (#2756).
module Unit.Stats
    ( rollStat
    , standardStat
    , effectiveStat
    , modifierTotals
    , invertEffectiveStat
    , invertEffectiveStatWhy
    , maxStaminaOf
    , applySkillXP
    , applyItemBuffs
    , applyAccessoryBuffs
    , refreshAccessoryBuffs
    , pickName
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import System.Random (StdGen, randomR)
import Item.Types (ItemBuff(..), ItemInstance(..), ItemManager,
                   lookupItemDef, idDisplayName, idBuffs)
import Unit.Types (StatModifier(..), NamePool(..))

-- | One standard-normal sample via Box-Muller. Returns (z, newGen).
--   Only consumes two uniform draws; the second normal sample of the
--   Box-Muller pair is discarded for simplicity.
boxMuller ∷ StdGen → (Float, StdGen)
boxMuller g0 =
    let (u1, g1) = randomR (0 ∷ Float, 1) g0
        (u2, g2) = randomR (0 ∷ Float, 1) g1
        -- Guard against u1 = 0 producing -Infinity from log.
        r        = sqrt (-2 * log (max u1 1e-9))
        z        = r * cos (2 * pi * u2)
    in (z, g2)

-- | Apply XP to a skill and return the new skill level.
--
--   Formula: @newLevel = level + xp / max (level * level, 1e-4)@.
--
--   This makes growth scale quadratically with the current level:
--   gaining 0.5 XP at level 1 jumps by 0.5; at level 2 by 0.125; at
--   level 3 by ~0.056. The @max ... 1e-4@ guard prevents division by
--   zero if a skill is somehow at 0 (won't happen via normal roll,
--   but possible after @setSkill 0@) — at level 0 the gain rockets
--   to xp*10000, which is fine because real callers don't pump XP
--   into a zeroed skill.
applySkillXP ∷ Float → Float → Float
applySkillXP level xp = level + xp / max (level * level) 1e-4

-- | Effective stat value at time @now@ =
--   @(base + Σ active deltas) × (1 + Σ active percents)@, clamped at
--   zero. A modifier is active when it has no expiry, OR its expiry
--   is in the future (strictly: @now < expiry@, so the modifier ends
--   exactly at its expiry time).
--
--   Pure — caller supplies @now@ so the function is testable without IO.
effectiveStat ∷ Double → Float → [StatModifier] → Float
effectiveStat now base mods =
    let (delta, percent) = modifierTotals now mods
    in max 0 ((base + delta) * (1 + percent))

-- | @(Σ active flat deltas, Σ active percents)@ of a stat's modifiers at
--   game time @now@: the two sums 'effectiveStat' composes, exposed so a
--   writer that solves for a stored base (the stamina commit) uses the
--   very same activity rule (a modifier is active while @now < expiry@).
modifierTotals ∷ Double → [StatModifier] → (Float, Float)
modifierTotals now mods =
    (sum (smDelta <$> active), sum (smPercent <$> active))
  where
    active = filter isActive mods
    isActive m = case smExpiry m of
        Nothing → True
        Just t  → now < t

-- | The BASE that 'effectiveStat' maps to the @target@ effective value at
--   time @now@, under the same active modifiers: @t / (1 + Σp) − Σδ@
--   (physiology transitions seed a Simulated pool so its EFFECTIVE value
--   is the target). 'Nothing' when no representable base exists: the
--   percent multiplier @1 + Σp@ is not positive, or the result is
--   negative or not finite. A zero base is representable.
invertEffectiveStat ∷ Double → Float → [StatModifier] → Maybe Float
invertEffectiveStat now target mods =
    either (const Nothing) Just (invertEffectiveStatWhy now target mods)

-- | 'invertEffectiveStat' with the reason no base exists, as a stable
--   code: @unrepresentable_target@ (the target is negative or not finite),
--   @nonpositive_multiplier@ (@1 + Σp ≤ 0@) or @negative_base@ (the
--   modifiers' flat floor is above the target, or the base is not
--   finite). The code is what the engagement diagnostic debounces on.
invertEffectiveStatWhy ∷ Double → Float → [StatModifier] → Either Text Float
invertEffectiveStatWhy now target mods
    | isNaN target ∨ isInfinite target ∨ target < 0 =
        Left "unrepresentable_target"
    | multiplier ≤ 0 ∨ isNaN multiplier ∨ isInfinite multiplier =
        Left "nonpositive_multiplier"
    | isNaN base ∨ isInfinite base ∨ base < 0 = Left "negative_base"
    | otherwise = Right base
  where
    active     = filter isActive mods
    delta      = sum (smDelta   <$> active)
    multiplier = 1 + sum (smPercent <$> active)
    base       = target / multiplier - delta
    isActive m = case smExpiry m of
        Nothing → True
        Just t  → now < t

-- | The unit's effective maximum stamina at game time @now@ — the ONE
--   rule every consumer shares (#2834, design S8.2): an explicit
--   @max_stamina@ stat wins and resolves through 'effectiveStat' with
--   its own modifiers; otherwise
--   @effectiveStat now (effective(endurance) × 10) (modifiers on
--   max_stamina)@. @fallbackEndurance@ is the endurance a unit with no
--   endurance stat is sized by (combat's 1.0); 'Nothing' leaves such a
--   unit with no maximum, which is what @stats.get@ reports to Lua.
maxStaminaOf ∷ Maybe Float → Double → HM.HashMap T.Text Float
             → HM.HashMap T.Text [StatModifier] → Maybe Float
maxStaminaOf fallbackEndurance now stats mods =
    case HM.lookup "max_stamina" stats of
        Just m  → Just (effectiveStat now m (modsOn "max_stamina"))
        Nothing → do
            e ← maybe fallbackEndurance Just (HM.lookup "endurance" stats)
            pure (effectiveStat now
                      (effectiveStat now e (modsOn "endurance") * 10.0)
                      (modsOn "max_stamina"))
  where
    modsOn name = HM.lookupDefault [] name mods

-- | Fold an item's buffs into a unit's modifier map — the single
--   implementation behind spawn-time accessory buffs, equip-time buff
--   application, and the condition-refresh path (#392). @src@ is the
--   item's display name (the label the stat tooltip shows); same-source
--   modifiers on a stat collapse (replace, not stack), so re-applying
--   at a new condition updates the live modifier instead of duplicating
--   it. A buff with scales_with_condition confers
--   @amount × (condition/100)@ — the same factor scales the percent
--   component, so a worn item's percentage bonus degrades with it.
applyItemBuffs ∷ T.Text → Float → [ItemBuff]
               → HM.HashMap T.Text [StatModifier]
               → HM.HashMap T.Text [StatModifier]
applyItemBuffs src cond buffs mods0 = foldl' applyOne mods0 buffs
  where
    scale b | ibScalesWithCondition b = cond / 100
            | otherwise               = 1
    applyOne acc b =
        let m = StatModifier
                  { smDelta   = ibAmount b * scale b
                  , smSource  = src
                  , smExpiry  = Nothing
                  , smPercent = ibPercent b * scale b
                  }
            existing = HM.lookupDefault [] (ibStat b) acc
            others   = filter (\x → smSource x ≢ src) existing
        in HM.insert (ibStat b) (m : others) acc

-- | Apply one accessory's buffs to a modifier map: def lookup + the
--   shared 'applyItemBuffs'. The modifier source is the item's
--   display_name and same-source modifiers on a stat collapse, so this
--   REPLACES that source's stale modifier rather than stacking. A no-op
--   for items with no buffs (or no def in scope).
applyAccessoryBuffs ∷ ItemManager → ItemInstance
                    → HM.HashMap T.Text [StatModifier]
                    → HM.HashMap T.Text [StatModifier]
applyAccessoryBuffs itemMgr inst mods =
    case lookupItemDef (iiDefName inst) itemMgr of
        Nothing   → mods
        Just iDef → applyItemBuffs (idDisplayName iDef)
                                   (iiCondition inst)
                                   (idBuffs iDef) mods

-- | Re-derive worn-accessory buffs into a unit's `uiModifiers`, so a
--   condition-scaled buff (e.g. technogoggles' perception) tracks
--   whatever changed the worn set. Folds the buffs of EVERY worn
--   accessory over the modifier map IN LIST ORDER, exactly as a fresh
--   sequence of `equipAccessory` calls would: same-source modifiers
--   collapse per stat (dedup by display_name) so the LAST-equipped copy
--   of a duplicated accessory wins, while a non-overlapping stat buffed
--   only by an EARLIER copy survives the fold.
--
--   This is an ADDITIVE fold: it never removes a source absent from
--   @accs@. A caller whose change DROPS an accessory (unequip) must
--   therefore clear that accessory's display-name source from @mods0@
--   first, so the fold rebuilds only what is still worn — re-deriving
--   from the whole list is what keeps a duplicate's remaining copy
--   live (#1209) and keeps repairing an *older* duplicate from
--   silently switching the live buff to its condition (#299).
refreshAccessoryBuffs ∷ ItemManager → [ItemInstance]
                      → HM.HashMap T.Text [StatModifier]
                      → HM.HashMap T.Text [StatModifier]
refreshAccessoryBuffs itemMgr accs mods0 =
    foldl' (\mods inst → applyAccessoryBuffs itemMgr inst mods) mods0 accs

-- | Roll one stat. With range = 0 returns base exactly (no RNG draw).
--   Otherwise ONE normal sample (sigma = range/4) is CLAMPED to
--   [base - range/2, base + range/2]: a sample outside the window is
--   pinned to the nearest bound rather than rejected (about 4.6% of
--   draws land on a bound — 2σ each side). Final clamp to >= 0.
{-# NOINLINE rollStat #-}
rollStat ∷ Float → Float → StdGen → (Float, StdGen)
rollStat base range g
    | range ≤ 0 = (max 0 base, g)
    | otherwise =
        case boxMuller g of
            (z, g') →
                ( max 0
                    (max (base - range / 2)
                        (min (base + range / 2)
                            (base + z * (range / 4))))
                , g'
                )

-- | The value a template takes under the standard spawn profile
--   (#2756): its @base@ \/ @mean@ exactly, with no draw — the centre
--   'rollStat' samples around, under the same non-negative floor.
standardStat ∷ Float → Float
standardStat base = max 0 base

-- | Draw a persistent display name from a name pool (#264). Picks one
--   `given` name and, if the pool has any, one `family` name, joining
--   them with a space. An empty given list yields "" (the unit stays
--   unnamed and falls back to its species label in the UI).
pickName ∷ NamePool → StdGen → (Text, StdGen)
pickName pool g0 =
    case npGiven pool of
        [] → ("", g0)
        givens →
            let (gi, g1)  = randomR (0, length givens - 1) g0
                given     = givens !! gi
            in case npFamily pool of
                [] → (given, g1)
                fams →
                    let (fi, g2) = randomR (0, length fams - 1) g1
                    in (given <> T.pack " " <> (fams !! fi), g2)
