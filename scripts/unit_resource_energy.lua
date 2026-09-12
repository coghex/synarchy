-- Body-energy tick for scripts/unit_resources.lua: surplus regrowth,
-- digestion, and starvation catabolism (Phase 3/4 of the survival math).

local stats  = require("scripts.unit_stats")
local alerts = require("scripts.unit_resource_alerts")

local M = {}

-----------------------------------------------------------
-- Tuning constants — Phase 3+ survival math
-----------------------------------------------------------
-- Real kcal/kg energy densities for body tissue. Used by the regrowth
-- branch here (Phase 3) and the catabolism branch in Phase 4.
local KCAL_PER_KG_FAT  = 7700
local KCAL_PER_KG_LEAN = 1800

-- Surplus regrowth: while the calorie store > 75% of max, divert this
-- much energy per real-second from the store into body mass at the
-- activity-dependent split (idle stores mostly fat, walking builds
-- mostly muscle). Tunable — at 0.1 kcal/sec ≈ 0.017 kg fat/game-day
-- at idle, which is slow; raise if regrowth feels imperceptible in
-- testing.
local REGROWTH_RATE_KCAL_PER_SEC = 0.1

-- Two-layer food model (#93). The STOMACH meter ("hunger", filled by
-- unit.feed) digests into the energy STORE ("calories", spent by
-- metabolism) at this rate. ~3 kcal/s empties a full default-acolyte
-- stomach (~713 kcal) in ~4 game-hours, and outpaces the idle burn
-- (~0.92 kcal/s) ~3:1 so eating genuinely replenishes the store.
-- Digestion PAUSES while the store is full — the stomach acts as a
-- buffer, nothing is wasted (surplus-regrowth then drains the store
-- into body mass, making room).
local DIGESTION_RATE_KCAL_PER_SEC = 3.0
-- kcal credited to the store per stomach-kcal digested. 1.0 = the
-- digestible energy is already what the food's calorie value says.
local DIGESTION_CONVERSION = 1.0

-- Phase 4 catabolism / organ failure constants.
--
-- Starvation eats body in two regimes. While fat reserves are above
-- the unit's fat floor (M.minFatFor below), the deficit is paid mostly
-- from fat with a small muscle toll — MUSCLE_CATABOLISM_FRACTION of the
-- kcal deficit comes from lean tissue, the rest from fat. Once fat
-- reaches that floor, catabolism switches to pure muscle.
local MUSCLE_CATABOLISM_FRACTION = 0.05

-- Tolerance for fat-at-floor comparisons. Engine stores uiStats as
-- Float32; Lua reads them as Float64. The round-trip pushes a
-- clamped fat value slightly ABOVE the Lua-computed min_fat
-- (e.g. 1.21916652 stored vs 1.21916647 recomputed). Without this
-- tolerance, `fat <= min_fat` stays false even when fat is clamped
-- exactly to the floor, and the organ-failure branch never fires.
-- 1e-4 kg = 0.1 g — orders of magnitude above the ~1e-7 Float32
-- noise but biologically negligible. Applied by M.atFatFloor below, so
-- the catabolism regime here and unit_resource_tick's organ-failure
-- check share one tolerance as well as one floor.
M.FAT_FLOOR_TOL = 1e-4

-- The fat floor itself, as fractions of frame mass / height², matching
-- seedBodyComposition's minFatFrac (src/Unit/Thread/Command/Body.hs).
-- frame_mass = 22·h²·bulk is the stable structural size, so the frame
-- form scales correctly for any creature; the height-only form is the
-- legacy fallback for units seeded before frame_mass existed, and the
-- two agree only at bulk 1.0.
local MIN_FAT_FRAC          = 0.02
local LEGACY_MIN_FAT_PER_H2 = 0.44

-----------------------------------------------------------
-- The minimum viable fat mass for a unit, in kg. This is THE fat-floor
-- policy: catabolism's regime split below and unit_resource_tick's
-- organ-failure check both read it, so neither can drift onto a
-- different formula than the one seedBodyComposition seeded against
-- (#2556 — the organ-failure check kept the height-only form after
-- seeding moved to frame_mass, and the two disagree at every bulk
-- except 1.0).
--
-- frame_mass wins whenever it is readable; height is only consulted for
-- units that predate it. Returns nil when NEITHER is readable, which
-- callers must treat as "no floor policy applies" rather than 0.
-----------------------------------------------------------
function M.minFatFor(uid)
    local frame = unit.getStat(uid, "frame_mass")
    if frame then return MIN_FAT_FRAC * frame end
    local h = unit.getStat(uid, "height")
    if h then return LEGACY_MIN_FAT_PER_H2 * h * h end
    return nil
end

-- Whether `fat` has reached this unit's floor, within FAT_FLOOR_TOL.
-- False when no floor input is readable: a unit with no body model is
-- not in organ failure and not in pure-muscle catabolism.
function M.atFatFloor(uid, fat)
    local minFat = M.minFatFor(uid)
    if not minFat then return false end
    return fat <= minFat + M.FAT_FLOOR_TOL
end

-----------------------------------------------------------
-- Surplus regrowth (calorie store > 75 %): divert
-- REGROWTH_RATE_KCAL_PER_SEC from the store into body mass.
--
-- Split varies with activity:
--   idle:    90 % fat / 10 % muscle (stored as reserve)
--   walking: 30 % fat / 70 % muscle (exercise builds muscle)
-- Walking ALSO burns half a regrowth's worth from fat directly — so
-- the net fat delta on a walking surplus tick is negative, modeling
-- "exercise reshapes body composition toward muscle even under
-- caloric surplus".
--
-- After mutating body_mass / lean_mass / fat_mass we MUST call
-- unit.recomputeBody so the engine refreshes strength / max_hydration
-- / max_hunger / carrying_capacity from the new composition.
-- Otherwise a fattening unit would have a stale strength stat.
-----------------------------------------------------------
function M.applyRegrowth(uid, activity, dt)
    local body   = unit.getStat(uid, "body_mass")
    local lean   = unit.getStat(uid, "lean_mass")
    local fat    = unit.getStat(uid, "fat_mass")
    local store  = unit.getStat(uid, "calories")
    if not (body and lean and fat and store) then return end

    local fatFrac, muscleFrac, exerciseBurn
    if activity == "walking" then
        fatFrac, muscleFrac, exerciseBurn = 0.3, 0.7, REGROWTH_RATE_KCAL_PER_SEC * 0.5
    else
        fatFrac, muscleFrac, exerciseBurn = 0.9, 0.1, 0
    end

    local fatKcal  = fatFrac    * REGROWTH_RATE_KCAL_PER_SEC * dt - exerciseBurn * dt
    local leanKcal = muscleFrac * REGROWTH_RATE_KCAL_PER_SEC * dt
    local fatDelta  = fatKcal  / KCAL_PER_KG_FAT
    local leanDelta = leanKcal / KCAL_PER_KG_LEAN

    local newFat    = math.max(0, fat  + fatDelta)
    local newLean   = math.max(0, lean + leanDelta)
    local newBody   = math.max(0, body + fatDelta + leanDelta)
    local newStore  = math.max(0, store - REGROWTH_RATE_KCAL_PER_SEC * dt)

    unit.setStat(uid, "fat_mass",  newFat)
    unit.setStat(uid, "lean_mass", newLean)
    unit.setStat(uid, "body_mass", newBody)
    unit.setStat(uid, "calories",  newStore)
    unit.recomputeBody(uid)
end

-----------------------------------------------------------
-- Digestion tick (#93): move kcal from the stomach meter ("hunger")
-- into the energy store ("calories") at DIGESTION_RATE_KCAL_PER_SEC ×
-- DIGESTION_CONVERSION. Runs AFTER the resource drains (the store has
-- just paid this tick's metabolism) and BEFORE tickStarvation (so a
-- unit still digesting food never catabolizes body mass).
--
-- The transfer is clamped by BOTH ends: what the stomach holds and
-- what the store can absorb. A full store pauses digestion — the
-- stomach is a buffer, nothing is wasted; surplus-regrowth drains the
-- store into body mass, making room again.
-----------------------------------------------------------
function M.tickDigestion(uid, dt)
    local stomach = unit.getStat(uid, "hunger")
    local store   = unit.getStat(uid, "calories")
    local maxCal  = unit.getStat(uid, "max_calories")
    if not (stomach and store and maxCal) then return end
    if stomach <= 0 or store >= maxCal then return end

    local room     = (maxCal - store) / DIGESTION_CONVERSION
    local transfer = math.min(DIGESTION_RATE_KCAL_PER_SEC * dt,
                              stomach, room)
    if transfer <= 0 then return end

    unit.setStat(uid, "hunger",   stomach - transfer)
    unit.setStat(uid, "calories", math.min(maxCal,
        store + transfer * DIGESTION_CONVERSION))
end

-----------------------------------------------------------
-- Starvation tick (Phase 4). Runs once per unit per update, AFTER
-- the calorie store has been drained AND digestion has refilled it,
-- so it sees the post-transfer store value — a unit with food still
-- digesting never catabolizes.
--
-- Three outcomes per tick:
--   1. lean ≤ min_lean → respiratory failure: unit.kill, return.
--      (Lungs and heart are skeletal-or-cardiac muscle; once those
--       waste below the floor, biology ends.)
--   2. calories > 0 → no catabolism (the store has slack).
--   3. calories ≤ 0 → eat body mass to cover the kcal deficit:
--      - fat > min_fat: 95% from fat, 5% from muscle (slow shrink,
--        visibly losing fat first).
--      - fat ≤ min_fat: pure muscle catabolism (sharp wasting).
--
-- After any mass change, call unit.recomputeBody so the engine
-- refreshes strength/max_hydration/max_hunger/carrying_capacity.
-- Without that, a starving unit's strength wouldn't drop.
-----------------------------------------------------------
function M.tickStarvation(uid, dt)
    if unit.getPose(uid) == "dead" then return end

    local body = unit.getStat(uid, "body_mass")
    local lean = unit.getStat(uid, "lean_mass")
    local fat  = unit.getStat(uid, "fat_mass")
    local h    = unit.getStat(uid, "height")
    if not (body and lean and fat and h) then return end

    -- Frame-proportional viability floors (match seedBodyComposition's
    -- minFatFrac/minLeanFrac). frame_mass = the stable structural size,
    -- so these scale correctly for any creature; the lean floor falls
    -- back to the old height-only formula for units seeded before
    -- frame_mass existed. The FAT floor comes from the shared
    -- M.minFatFor, which applies the same preference, so the value this
    -- clamps to is exactly the value the organ-failure check tests.
    local frame   = unit.getStat(uid, "frame_mass")
    local minFat  = M.minFatFor(uid)
    local minLean = frame and (0.20 * frame) or (4.4  * h * h)

    -- Respiratory failure: sharp death when skeletal muscle (which
    -- includes the diaphragm) hits its floor.
    if lean <= minLean then
        alerts.emitDeathAlert(uid, "starvation")
        unit.kill(uid)
        return
    end

    -- No deficit if the calorie store isn't empty yet.
    local store = unit.getStat(uid, "calories")
    if not store or store > 0 then return end

    -- metabolism_rate is already activity-aware (Phase 3 refactor),
    -- so deficit is just rate × dt.
    local rate = stats.get(uid, "metabolism_rate")
    if not rate or rate <= 0 then return end
    local deficit = rate * dt

    local fatEaten, muscleEaten
    if not M.atFatFloor(uid, fat) then
        fatEaten    = deficit * (1 - MUSCLE_CATABOLISM_FRACTION) / KCAL_PER_KG_FAT
        muscleEaten = deficit *      MUSCLE_CATABOLISM_FRACTION  / KCAL_PER_KG_LEAN
    else
        fatEaten    = 0
        muscleEaten = deficit / KCAL_PER_KG_LEAN
    end

    local newFat  = math.max(minFat,  fat  - fatEaten)
    local newLean = math.max(minLean, lean - muscleEaten)
    -- body_mass tracks the actual deltas (not clamped values) so
    -- it stays consistent with fat + lean + organ_mass after the
    -- clamps fire. The lean clamp here is belt-and-suspenders —
    -- the respiratory check above should have killed us first.
    local actualFatDelta  = fat  - newFat
    local actualLeanDelta = lean - newLean
    local newBody = math.max(0, body - actualFatDelta - actualLeanDelta)

    unit.setStat(uid, "fat_mass",  newFat)
    unit.setStat(uid, "lean_mass", newLean)
    unit.setStat(uid, "body_mass", newBody)
    unit.recomputeBody(uid)
end

return M
