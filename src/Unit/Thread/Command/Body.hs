{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Thread.Command.Body
    ( bloodSeedFromStats
    , injurySpeedMult
    , seedBodyComposition
    , recomputeBodyDerivedStats
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Unit.Types

-- | Spawn-time body composition. Reads rolled height/bulk/bodyfat,
--   computes body_mass / lean_mass (skeletal muscle only) / fat_mass,
--   clamps lean/fat at the viability floors so a tall thin combo can't
--   spawn at or below the death thresholds, then drops bulk/bodyfat
--   from the map (they're spawn-time inputs, not live stats).
--
--   Finally re-runs `recomputeBodyDerivedStats` to fill in strength /
--   strength_body / max_hydration / max_hunger / carrying_capacity /
--   strength_base. No-op if any of height/bulk/bodyfat is missing
--   (e.g. for unit types that don't declare a body block).
-- | Spawn-time blood-volume seed in litres ('bloodMassRatio' of
--   body_mass) — applied via the rolled stats map so units without a
--   body block (no body_mass) start at 0 and won't bleed.
--   The wound-tick code computes max_blood lazily from current
--   body_mass on each read, so wasting/regrowth carries through.
bloodSeedFromStats ∷ HM.HashMap Text Float → Float
bloodSeedFromStats s = case HM.lookup "body_mass" s of
    Just bm → bm * bloodMassRatio
    Nothing → 0.0

-- | Movement-speed multiplier derived from the unit's wounds + blood.
--
--   Three terms compose multiplicatively:
--     * leg/foot wound severity × 1.2 — wounds on parts flagged
--       'bpAffectsLocomotion' hit harder because that's literally
--       what walks.
--     * torso wound severity × 0.3 — parts flagged 'bpAffectsBalance'
--       (concussive / vital-area damage) slow the unit too but less
--       than a leg.
--     * blood fraction → 0.5 + 0.5 × frac — at full blood we're 1.0,
--       at 0 blood we're 0.5 (matches "unconscious bleeding out is
--       still capable of some movement before collapse").
--   Result clamped to [0.1, 1.0] so a unit never literally stops.
--   Applied to UnitMoveTo's speed parameter so all movement gets
--   scaled the same way. Which parts count as leg/torso is
--   DATA-DRIVEN via the unit's own body-part list rather than
--   hardcoded ids, so differently-named skeletons (a robot's
--   "hindquarter", say) work as long as their YAML sets the flags.
injurySpeedMult ∷ [BodyPart] → UnitInstance → Float
injurySpeedMult bodyParts inst =
    let partIdx = HM.fromList [ (bpId p, p) | p ← bodyParts ]
        hasFlag f pid = maybe False f (HM.lookup pid partIdx)
        -- EFFECTIVE severity (heal eases it, necrosis floors it) so a
        -- limping unit regains speed as its leg wound mends, in step
        -- with the bleed/pain/anim consumers that share the same helper.
        legSev = sum [ woundEffSeverity w
                     | w ← uiWounds inst, hasFlag bpAffectsLocomotion (woundPart w) ]
        torsoSev = sum [ woundEffSeverity w
                       | w ← uiWounds inst, hasFlag bpAffectsBalance (woundPart w) ]
        bodyMass = HM.lookupDefault 70.0 "body_mass" (uiStats inst)
        maxBlood = bodyMass * bloodMassRatio
        bloodFrac = if maxBlood > 0
                    then max 0 (min 1 (uiBlood inst / maxBlood))
                    else 1
        -- legCut multiplier 1.2 (was 0.6): a severity-0.5 leg wound
        -- cuts speed 60%, sev-0.8 cuts ~96%, sev-1.0 hits the floor.
        -- Limps are decisively visible — wounded acolytes shuffle.
        -- torsoCut stays gentler — a torso wound hurts but doesn't
        -- mechanically prevent leg movement.
        legCut   = min 1.0 (legSev   * 1.2)
        torsoCut = min 1.0 (torsoSev * 0.3)
        bloodMul = 0.5 + 0.5 * bloodFrac
        raw      = (1 - legCut) * (1 - torsoCut) * bloodMul
    in max 0.1 (min 1.0 raw)

-- Body-composition viability floors, as FRACTIONS of frame mass
-- (frame = 22·h²·bulk), so they scale to ANY creature size — mouse to
-- dragon. Calibrated to reproduce the historical height-only human floors
-- at bulk 1.0: 0.02·(22h²) = 0.44h² and 0.20·(22h²) = 4.4h². The spawn
-- margins seat a fresh roll comfortably above the death floors and are
-- themselves frame-proportional (≈ the old flat +1 kg at human scale, but
-- scaling correctly for tiny/huge bodies instead of swamping them).
minFatFrac, minLeanFrac, spawnFatMargin, spawnLeanMargin ∷ Float
minFatFrac      = 0.02
minLeanFrac     = 0.20
spawnFatMargin  = 1.7
spawnLeanMargin = 1.07

seedBodyComposition ∷ HM.HashMap Text Float → HM.HashMap Text Float
seedBodyComposition rolled =
    case (HM.lookup "height" rolled, HM.lookup "bulk" rolled,
          HM.lookup "bodyfat" rolled) of
        (Just h, Just b, Just f) →
            let bodyMass  = 22 * h * h * b   -- the frame: structural size
                fatMass0  = bodyMass * f
                -- Skeletal muscle is ~50 % of non-fat tissue in real
                -- adult composition; the rest is bones / organs /
                -- water / skin (lumped together as implicit organ
                -- mass, never tracked separately).
                leanMass0 = bodyMass * (1 - f) * 0.5
                -- Frame-proportional viability floors (see the *Frac /
                -- *Margin constants above). These scale with the unit's
                -- own frame mass, so a tiny animal's healthy lean/fat are
                -- never clamped above its own body mass — body composition
                -- stays coherent at ANY size (mouse → dragon), which keeps
                -- every mass-derived quantity (blood = body·bloodMassRatio,
                -- max_salt = body·k, BMR = 22·(body−fat)+…) sane. At human
                -- bulk these reproduce the old 0.44h² / 4.4h² floors.
                minFat    = minFatFrac  * bodyMass
                minLean   = minLeanFrac * bodyMass
                fatMass   = max (minFat  * spawnFatMargin)  fatMass0
                leanMass  = max (minLean * spawnLeanMargin) leanMass0
                -- frame_mass = the stable structural reference. body_mass
                -- can later shrink via catabolism/wasting; frame_mass does
                -- not, so the organ-failure / starvation death thresholds
                -- (unit_resources) read it for a size-correct floor.
                withBody  = HM.insert "frame_mass" bodyMass
                          $ HM.insert "body_mass"  bodyMass
                          $ HM.insert "lean_mass"  leanMass
                          $ HM.insert "fat_mass"   fatMass
                          $ HM.delete "bulk"
                          $ HM.delete "bodyfat" rolled
            in recomputeBodyDerivedStats withBody
        _ → rolled

-- | Recompute body-driven derived stats from live body composition.
--   Reads height + body_mass + lean_mass + strength_base, writes
--   strength / strength_body / max_hydration / max_hunger /
--   max_calories / carrying_capacity.
--
--   strength_base is the un-scaled potential rolled at spawn; on the
--   first call (no strength_base yet) we promote the rolled "strength"
--   into "strength_base" so future re-computes don't compound. This
--   is what makes "a wasted unit physically weakens" emergent —
--   Phase 4 catabolism shrinks lean_mass, this recompute drops the
--   strength derived from it, and carrying_capacity follows.
--
--   strength_body mirrors strength at the moment of this recompute —
--   the untainted, pre-calorie-penalty value. This recompute only
--   runs on a body-composition CHANGE (spawn, catabolism, regrowth),
--   but the calorie-store threshold effect (#806) needs to track the
--   store's fraction every resource tick, far more often than mass
--   changes. scripts/starvation.lua's per-tick refresh reads
--   strength_body (never strength_base) as the anchor it scales by
--   the current starving multiplier and writes back into "strength" —
--   the actual stat every consumer (combat resolution, Lua AI) reads.
--   Keeping strength_body separate from "strength" is what lets that
--   refresh be idempotent (always re-derived from the same untainted
--   source, never compounding) and fully recoverable (a refilled store
--   scales strength_body back to 1×, with strength_base itself never
--   touched).
--
--   avg_skeletal_muscle_at_height(h) = 22 · h² · 0.8 · 0.5 = 8.8 · h²
--   assumes acolyte-like means (bulk=1, bodyfat=0.2). Different
--   species would want a species-specific average; punt until then.
recomputeBodyDerivedStats ∷ HM.HashMap Text Float → HM.HashMap Text Float
recomputeBodyDerivedStats s =
    case (HM.lookup "height" s, HM.lookup "body_mass" s,
          HM.lookup "lean_mass" s) of
        (Just h, Just bm, Just lm) →
            let avgLean    = 8.8 * h * h
                ratio      = if avgLean > 0 then lm / avgLean else 1
                -- First call: no strength_base yet — promote the
                -- rolled "strength" into the base slot, then derive.
                strBase    = case HM.lookup "strength_base" s of
                                 Just b  → b
                                 Nothing → HM.lookupDefault 0 "strength" s
                strength   = strBase * (ratio ** 0.7)
                maxHydration = bm * 0.6
                -- Two-layer food model (#93): max_hunger is STOMACH
                -- capacity (kcal of undigested food — ~713 kcal for a
                -- default acolyte, a few meals); max_calories is the
                -- energy STORE digestion feeds (kept at the old bm*20
                -- pool size, ~1426 kcal ≈ a day of idle burn, so the
                -- starvation timelines carry over). Both are seeded for
                -- any body unit — only units with a LIVE hunger/calories
                -- stat (unit_resources config) actually run the system.
                maxHunger    = bm * 10
                maxCalories  = bm * 20
                -- Carrying capacity from muscle: more lean mass AND
                -- more strength = more capacity, sub-linearly in both
                -- so the product doesn't explode at the tails.
                -- Calibration (acolyte body block): average roll
                -- (lm ≈ 28.5, strength 1.0) → ~23 kg; an exceptional
                -- 2-sigma strongman (strength_base 1.4, lm ≈ 46)
                -- → ~41 kg. The weakest rolls (~11 kg) sit just under
                -- the full starting kit (~12 kg) — by design, the
                -- spawn-time capacity check sheds the pick/shovel
                -- instead of flooring the formula. Extrapolates to
                -- pack species: the technomule body (lm ≈ 87,
                -- strength ≈ 8) → ~163 kg base, before its +50%
                -- percentage modifier.
                carryCap     = 3.2 * ((lm * strength) ** 0.6)
            in HM.insert "strength_base"     strBase
             $ HM.insert "strength"          strength
             $ HM.insert "strength_body"     strength
             $ HM.insert "max_hydration"     maxHydration
             $ HM.insert "max_hunger"        maxHunger
             $ HM.insert "max_calories"      maxCalories
             $ HM.insert "carrying_capacity" carryCap s
        _ → s
