-- Nominal-nutrition tissue upkeep (#2831, S4 of epic #2827; design D-9, SD-5,
-- SD-18).
--
-- A NOMINAL nutrition system does not eat, digest, burn calories or starve
-- (scripts/unit_resources.lua gates every one of those on a Simulated
-- nutrition). What it still owes a living, supported unit is the body
-- composition the healthy substitute promises: a unit whose fat or lean mass
-- has been wasted below its spawn margin (a legacy save, a unit switched
-- over after starving) recovers toward the healthy target instead of staying
-- wasted forever with no food system to rebuild it.
--
-- DEPLETION-ONLY. This is recovery, never growth: tissue ABOVE its target is
-- retained exactly, and a healthy unit is left entirely alone.
--
--   * Targets, on the unit's OWN frame_mass (the structural size, which
--     catabolism never shrinks): fat = max(minFat x 1.7, frame x species mean
--     bodyfat), lean = max(minLean x 1.07, frame x (1 - mean) x 0.5) -- the
--     seeding formula of Unit.Thread.Command.Body.seedBodyComposition at the
--     species' mean roll, never below the spawn margins.
--   * Allocation: idle 90 % fat / 10 % lean, walking 30 % fat / 70 % lean, at
--     the surplus-regrowth rate. NOTHING is spent: no calories (there is no
--     calorie store) and no walking fat burn. A tissue that reaches its target
--     stops; its share is NOT redistributed to the other.
--   * Whether it recovers at all is the persisted unowned flag
--     `tissue_recovering` (SD-18), because absence of tissue alone cannot tell
--     "wasted" from "a naturally lean individual spawned below the species
--     mean" (the spawn roll is wide): it is SET when either tissue is below
--     its spawn margin (under either mode) or when Simulated catabolism
--     removes tissue, regrows only while set under Nominal, and is CLEARED
--     once both tissues are at their targets (under either mode). Absent
--     reads 0, so a legacy save needs no migration: a unit below a margin is
--     flagged on its first pass here.
--
-- Mass changes go through unit_resource_regrowth (sub-binary32 residue kept,
-- the clamp applied BEFORE the residue is retained) and are followed by
-- unit.recomputeBody, so strength, the maxima and carrying capacity follow
-- the new composition.

local regrowth    = require("scripts.unit_resource_regrowth")
local starvation  = require("scripts.starvation")
local physiology  = require("scripts.physiology")
local effects = require("scripts.unit_effects")

local M = {}

-- Mirrors seedBodyComposition's constants (src/Unit/Thread/Command/Body.hs)
-- and unit_resource_energy's floors. A pinned test compares them.
local MIN_FAT_FRAC     = 0.02
local MIN_LEAN_FRAC    = 0.20
local FAT_MARGIN       = 1.7
local LEAN_MARGIN      = 1.07
M.MIN_FAT_FRAC, M.MIN_LEAN_FRAC = MIN_FAT_FRAC, MIN_LEAN_FRAC
M.FAT_MARGIN, M.LEAN_MARGIN = FAT_MARGIN, LEAN_MARGIN

-- Recovery runs at surplus regrowth's rate and energy densities, so a unit
-- rebuilds at the pace it would have fattened on a full store.
local RATE_KCAL_PER_SEC = 0.1
local KCAL_PER_KG_FAT   = 7700
local KCAL_PER_KG_LEAN  = 1800

-- The same Float32 tolerance the floors use (unit_resource_energy): a tissue
-- clamped to its target reads back a hair under it.
local TOL = 1e-4

-- The flag's stat name. Unowned by any registry system; 0/1; absent = 0.
local FLAG = "tissue_recovering"
M.FLAG = FLAG

-- The unit's healthy tissue targets and spawn margins, or nil when it has no
-- body composition to judge.
function M.targets(uid)
    local frame = unit.getStat(uid, "frame_mass")
    if not frame or frame ~= frame or frame <= 0 then return nil end
    local info = unit.getInfo(uid)
    local mean = info and info.bodyfatMean
    local minFat, minLean = MIN_FAT_FRAC * frame, MIN_LEAN_FRAC * frame
    local fatMargin  = minFat  * FAT_MARGIN
    local leanMargin = minLean * LEAN_MARGIN
    local fatTarget, leanTarget = fatMargin, leanMargin
    if mean then
        fatTarget  = math.max(fatMargin,  frame * mean)
        leanTarget = math.max(leanMargin, frame * (1 - mean) * 0.5)
    end
    return {
        fatMargin = fatMargin, leanMargin = leanMargin,
        fatTarget = fatTarget, leanTarget = leanTarget,
    }
end

local function flagged(uid)
    return (unit.getStat(uid, FLAG) or 0) > 0
end

local function writeFlag(uid, on)
    local want = on and 1 or 0
    if (unit.getStat(uid, FLAG) or 0) ~= want then
        effects.setStat(uid, FLAG, want)
    end
end

-- Set the flag from the unit's CURRENT tissue: on when either is below its
-- spawn margin, off once both have reached their targets. Under either mode.
-- Returns the targets used (nil for a unit with no composition).
function M.syncFlag(uid)
    local fat  = unit.getStat(uid, "fat_mass")
    local lean = unit.getStat(uid, "lean_mass")
    if not (fat and lean) then return nil end
    local t = M.targets(uid)
    if not t then return nil end
    if fat < t.fatMargin - TOL or lean < t.leanMargin - TOL then
        writeFlag(uid, true)
    elseif fat >= t.fatTarget - TOL and lean >= t.leanTarget - TOL then
        if flagged(uid) then writeFlag(uid, false) end
    end
    return t
end

-- Record that Simulated catabolism has removed tissue (SD-18): the unit now
-- has something to recover should its nutrition ever turn Nominal.
function M.markCatabolism(uid)
    writeFlag(uid, true)
end

-- One Nominal pass for a living unit whose nutrition is Nominal. `activity`
-- selects the allocation; `dt` is the tick in seconds.
local function recover(uid, activity, dt, t)
    local fat  = unit.getStat(uid, "fat_mass")
    local lean = unit.getStat(uid, "lean_mass")
    local fatShare, leanShare = 0.9, 0.1
    if activity == "walking" then fatShare, leanShare = 0.3, 0.7 end
    local fatDelta, leanDelta = 0, 0
    if fat < t.fatTarget - TOL then
        fatDelta = fatShare * RATE_KCAL_PER_SEC * dt / KCAL_PER_KG_FAT
    end
    if lean < t.leanTarget - TOL then
        leanDelta = leanShare * RATE_KCAL_PER_SEC * dt / KCAL_PER_KG_LEAN
    end
    if fatDelta == 0 and leanDelta == 0 then return end
    if not regrowth.commitComposition(uid, fatDelta, leanDelta,
                                      t.fatTarget, t.leanTarget) then
        return
    end
    effects.recomputeBody(uid)
end

-- The Nominal upkeep for one unit. Called every physiology pass for a living
-- unit with a Nominal nutrition, whether or not its definition has a resource
-- config (a species with none -- the exempt nomad -- still gets it).
function M.tick(uid, activity, dt)
    local t = M.syncFlag(uid)
    if t and flagged(uid) then
        recover(uid, activity, dt, t)
        M.syncFlag(uid)            -- a tissue that just arrived clears the flag
    end
    -- Maintain UNDISCOUNTED strength every Nominal pass: there is no calorie
    -- store for the starving band to read, so any discount left over from a
    -- Simulated past is stale and is restored to strength_body.
    starvation.refreshStrength(uid)
end

-- Leaving Simulated nutrition clears its strength discount synchronously,
-- before the transition returns, instead of waiting for the next pass.
physiology.onModeChanged("nutrition_upkeep", "nutrition",
    function(uid, _system, _from, to)
        if to == "nominal" then starvation.refreshStrength(uid) end
    end)

return M
