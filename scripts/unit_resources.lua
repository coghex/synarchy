-- Unit Resources
--
-- Phase D of the stat system: time-varying resources that drain or
-- regenerate per tick and can trigger state transitions when they
-- cross thresholds. Built entirely on top of Phases A/B/C — stamina
-- is just a regular stat (set via setStat), with this module's
-- update loop applying drain/regen each tick and checking for the
-- collapse threshold.
--
-- To add a resource:
--   1. Add an entry under `config[defName].resourceName` with
--      drain/regen/threshold values.
--   2. Ensure `max_resourceName` exists either as a derived formula
--      in scripts/unit_stats.lua or as an attribute in the YAML.
--   3. Add a branch in `tickResource` if the trigger differs from
--      stamina's "collapse when below threshold".
--
-- The first tick a unit is observed, its current value is set to the
-- max (full tank on spawn). After that, the loop just nudges the
-- current value up or down.

local stats = require("scripts.unit_stats")

local unitResources = {}

-----------------------------------------------------------
-- Tuning constants — Phase 3+ survival math
-----------------------------------------------------------
-- Real kcal/kg energy densities for body tissue. Used by the regrowth
-- branch here (Phase 3) and the catabolism branch in Phase 4.
local KCAL_PER_KG_FAT  = 7700
local KCAL_PER_KG_LEAN = 1800

-- Surplus regrowth: while hunger > 75% of max, divert this much
-- energy per real-second from the hunger pool into body mass at the
-- activity-dependent split (idle stores mostly fat, walking builds
-- mostly muscle). Tunable — at 0.1 kcal/sec ≈ 0.017 kg fat/game-day
-- at idle, which is slow; raise if regrowth feels imperceptible in
-- testing.
local REGROWTH_RATE_KCAL_PER_SEC = 0.1

-- Phase 4 catabolism / organ failure constants.
--
-- Starvation eats body in two regimes. While fat reserves are above
-- min_fat(h), the deficit is paid mostly from fat with a small muscle
-- toll — MUSCLE_CATABOLISM_FRACTION of the kcal deficit comes from
-- lean tissue, the rest from fat. Once fat hits min_fat, catabolism
-- switches to pure muscle.
local MUSCLE_CATABOLISM_FRACTION = 0.05

-- When fat ≤ min_fat(h), the organ-failure branch fires in the
-- stamina tick: regen is overridden to 0 and this constant drain
-- runs regardless of activity/pose. With max_stamina ≈ 10 the unit
-- runs out of stamina in 16–20 real-seconds, then Phase 1's universal
-- "stamina == 0 → die" rule fires. The other death path is
-- respiratory failure (lean ≤ min_lean) handled directly in
-- tickStarvation.
local ORGAN_FAILURE_DRAIN_PER_SEC = 0.5

-- Tolerance for fat-at-floor comparisons. Engine stores uiStats as
-- Float32; Lua reads them as Float64. The round-trip pushes a
-- clamped fat value slightly ABOVE the Lua-computed min_fat
-- (e.g. 1.21916652 stored vs 1.21916647 recomputed). Without this
-- tolerance, `fat <= min_fat` stays false even when fat is clamped
-- exactly to the floor, and the organ-failure branch never fires.
-- 1e-4 kg = 0.1 g — orders of magnitude above the ~1e-7 Float32
-- noise but biologically negligible.
local FAT_FLOOR_TOL = 1e-4

-- Per-def resource config. Drain is a per-second constant. Regen
-- factors are multiplied by endurance: a high-endurance unit recovers
-- much faster than a weak one (and may net-regen even while walking).
-- collapse_threshold is the fraction-of-max below which the unit
-- transitions to Collapsed (via unit.collapse). Set to 0 to disable.
local config = {
    acolyte = {
        stamina = {
            max_from               = "max_stamina",
            drain_walking          = 0.1,    -- per second
            -- These four are multiplied by current endurance.
            -- For endurance 1.0: walking net = +0.02/s (regens),
            -- idle = +0.5/s, collapsed = +0.3/s, reviving = +0.3/s.
            -- For endurance 0.5: walking net = -0.04/s (slow drain).
            regen_factor_walking   = 0.12,
            regen_factor_idle      = 0.5,
            regen_factor_collapsed = 0.3,
            -- Stationary at a water source — treat as resting.
            regen_factor_crouching = 0.5,
            collapse_threshold     = 0.1,
            -- When current/max climbs past this, a Collapsed unit
            -- auto-revives. Hysteresis vs collapse_threshold keeps a
            -- borderline unit from flapping between states each tick.
            revive_threshold       = 0.5,
            -- Universal kill rule: any path that drains stamina to 0
            -- ends the unit. Phase 4 reuses this via the organ-failure
            -- stamina drain — exhaustion death falls out of the same
            -- mechanism rather than needing its own kill check.
            kill_on_zero           = true,
            -- Phase 4 organ failure: when fat_mass ≤ min_fat(h), the
            -- body has spent its reserves and biology can't keep up.
            -- tickResource overrides regen to 0 and adds
            -- ORGAN_FAILURE_DRAIN_PER_SEC on top of any other drains,
            -- regardless of activity or pose. The unit ends via the
            -- kill_on_zero rule above ~16-20 real-sec later.
            organ_failure_check    = true,
        },
        hydration = {
            max_from        = "max_hydration",
            -- When on all fours at a water source (pose == "crawling"),
            -- regen tops up hydration fast — ~5 L/s × endurance ≈ 5 L/s
            -- for a typical unit. Other poses/activities don't regen
            -- hydration; canteen-drinking is a separate (Lua-side)
            -- bolus.
            regen_factor_crawling = 5.0,
            -- Constant per-second drain in any activity. Tuned so an
            -- average pool (~43 L) empties in 3 game-days. At
            -- timeScale 1.0 (1 game-minute per real-second), 3 days =
            -- 4320 real-seconds → 43 / 4320 ≈ 0.01 L/s. Real-world
            -- baseline metabolism + sweat is ~2.5 L/day; here we run
            -- a bit higher to make the gameplay loop visible without
            -- accelerating into the "constantly drinking" zone.
            drain_constant  = 0.01,
            -- No regen: hydration only restored by drinking events
            -- (separate API, not yet wired).
            collapse_threshold = 0.2,
            -- Revive requires drinking to bring hydration back up to
            -- 50% — phase 3 work. Until drinking exists, a collapsed-
            -- from-thirst unit stays collapsed.
            revive_threshold   = 0.5,
            -- Snap-kill below 5 % — total dehydration is fatal long
            -- before stamina would catch it. Direct path, no stamina
            -- mediation. The collapse trigger at 20 % fires first;
            -- this is the floor below which the unit doesn't recover.
            death_threshold    = 0.05,
        },
        hunger = {
            max_from         = "max_hunger",
            -- Drain at metabolism_rate × activity_multiplier rather
            -- than a fixed constant. Walking burns ~1.5× idle BMR.
            -- The hunger meter alone doesn't collapse the unit —
            -- fasting is energizing; weakness only kicks in after
            -- Phase 4 catabolism exhausts the fat reserves and the
            -- organ-failure stamina drain takes over.
            drain_metabolic  = true,
            -- Surplus storage. While hunger > 75 % of max, divert
            -- REGROWTH_RATE_KCAL_PER_SEC from the food pool into
            -- body mass at the activity-dependent split. Both the
            -- divert AND the metabolic drain run in the same tick,
            -- so a well-fed unit empties its hunger pool a bit
            -- faster than the BMR alone — that's the food being
            -- stored.
            surplus_regrowth = true,
        },
    },
    -- Bears need stamina so combat drain (Combat.Resolution applies
    -- 5% per quick, 25% per heavy of max_stamina) lands somewhere.
    -- Without this entry the engine writes "stamina" to uiStats but
    -- nothing reads or regenerates it. Bears have no hunger/hydration
    -- yet — wildlife survival is Phase 5 work.
    bear_brown = {
        stamina = {
            max_from               = "max_stamina",
            drain_walking          = 0.1,
            regen_factor_walking   = 0.12,
            regen_factor_idle      = 0.5,
            regen_factor_collapsed = 0.3,
            regen_factor_crouching = 0.5,
            collapse_threshold     = 0.1,
            revive_threshold       = 0.5,
            kill_on_zero           = true,
            -- No organ_failure_check — wildlife doesn't (yet) have
            -- the body-composition catabolism layer that drives it.
        },
    },
}

-----------------------------------------------------------
-- Player-events alert tracking
-----------------------------------------------------------
--
-- Survival emits three player-events flavours:
--   * survival_critical "X died of <cause>" on every unit.kill site
--     (no debouncing — each unit dies once).
--   * survival_warning "X is starving"   when hunger hits 0
--   * survival_warning "X is dehydrated" when hydration < 25%
--
-- Warnings are debounced with hysteresis to avoid spamming the popup
-- when a unit oscillates around the threshold (e.g. drinks a sip,
-- drains back below 25%, drinks again …). Each unit carries a
-- per-alert "active" flag; the alert fires only on the OFF→ON
-- transition, and the flag clears only when the resource recovers
-- above a higher "rearm" threshold (well clear of the trigger).
--
-- Entries are keyed by unit id. They linger after a unit is
-- destroyed via unit.destroy, which is fine — they're tiny (two
-- booleans) and bounded by total units ever spawned. Death
-- explicitly clears the entry.

local unitAlertState = {}  -- uid → { starvation = bool, dehydration = bool }

-- Rearm thresholds (as fractions of max). Trigger thresholds are
-- "hunger ≤ 0" (the floor) and "hydration < 25%" per the design.
local STARVATION_REARM_FRAC   = 0.25  -- clear flag when hunger > 25%
local DEHYDRATION_TRIGGER_FRAC = 0.25  -- fire flag when hydration < 25%
local DEHYDRATION_REARM_FRAC   = 0.50  -- clear flag when hydration > 50%

-- "acolyte" → "Acolyte". Unit defNames are all lower-snake_case
-- today; this is enough to make alert text presentable. If
-- individual unit names land later this becomes <name>.
local function unitLabel(info)
    local n = (info and info.defName) or "Unit"
    return n:sub(1, 1):upper() .. n:sub(2)
end

local function unitCoords(info)
    if not info then return nil, nil end
    return math.floor(info.gridX or 0), math.floor(info.gridY or 0)
end

local function emitDeathAlert(uid, cause)
    local info = unit.getInfo(uid)
    if not info then return end
    local gx, gy = unitCoords(info)
    local msg = unitLabel(info) .. " died of " .. cause
    if gx and gy then
        engine.emitEventAt("survival_critical", msg, gx, gy)
    else
        engine.emitEvent("survival_critical", msg)
    end
    -- Death clears any pending warning flags so a future re-use of
    -- the same uid (engine reassigns ids on destroy/spawn) doesn't
    -- inherit stale state.
    unitAlertState[uid] = nil
end

local function emitWarningAlert(uid, info, msg)
    if not info then info = unit.getInfo(uid) end
    if not info then return end
    local gx, gy = unitCoords(info)
    local fullMsg = unitLabel(info) .. " " .. msg
    if gx and gy then
        engine.emitEventAt("survival_warning", fullMsg, gx, gy)
    else
        engine.emitEvent("survival_warning", fullMsg)
    end
end

-- Called after a resource tick has written the new value. Looks at
-- the current value (post-write), compares against this resource's
-- alert thresholds, and fires/clears the per-unit flag accordingly.
-- pose check skips dead units — they don't get warnings.
local function checkSurvivalAlerts(uid, resourceName, value, maxVal, pose, info)
    if pose == "dead" or not maxVal or maxVal <= 0 then return end
    local state = unitAlertState[uid] or {}

    if resourceName == "hunger" then
        local rearm = maxVal * STARVATION_REARM_FRAC
        if not state.starvation then
            if value <= 0 then
                emitWarningAlert(uid, info, "is starving")
                state.starvation = true
            end
        else
            if value > rearm then
                state.starvation = false
            end
        end
    elseif resourceName == "hydration" then
        local trigger = maxVal * DEHYDRATION_TRIGGER_FRAC
        local rearm   = maxVal * DEHYDRATION_REARM_FRAC
        if not state.dehydration then
            if value < trigger then
                emitWarningAlert(uid, info, "is dehydrated")
                state.dehydration = true
            end
        else
            if value > rearm then
                state.dehydration = false
            end
        end
    end

    unitAlertState[uid] = state
end

-- Translate the resourceName that triggered a kill into a
-- player-visible cause. The two real kill paths in tickResource are
-- hydration (death_threshold) and stamina (kill_on_zero from organ
-- failure when fat reserves hit min_fat). tickStarvation's
-- respiratory-failure path passes "starvation" explicitly.
local function deathCauseFor(resourceName)
    if resourceName == "hydration" then return "dehydration"  end
    if resourceName == "stamina"   then return "starvation"   end
    return "exhaustion"
end

-----------------------------------------------------------
-- Init
-----------------------------------------------------------
function unitResources.init(scriptId)
    engine.logInfo("Unit resources tick initializing...")
end

-----------------------------------------------------------
-- Surplus regrowth (hunger > 75 %): divert REGROWTH_RATE_KCAL_PER_SEC
-- from the hunger pool into body mass.
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
local function applyRegrowth(uid, activity, dt)
    local body   = unit.getStat(uid, "body_mass")
    local lean   = unit.getStat(uid, "lean_mass")
    local fat    = unit.getStat(uid, "fat_mass")
    local hunger = unit.getStat(uid, "hunger")
    if not (body and lean and fat and hunger) then return end

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
    local newHunger = math.max(0, hunger - REGROWTH_RATE_KCAL_PER_SEC * dt)

    unit.setStat(uid, "fat_mass",  newFat)
    unit.setStat(uid, "lean_mass", newLean)
    unit.setStat(uid, "body_mass", newBody)
    unit.setStat(uid, "hunger",    newHunger)
    unit.recomputeBody(uid)
end

-----------------------------------------------------------
-- Starvation tick (Phase 4). Runs once per unit per update, AFTER
-- the hunger resource has been drained, so it sees the post-drain
-- hunger value.
--
-- Three outcomes per tick:
--   1. lean ≤ min_lean → respiratory failure: unit.kill, return.
--      (Lungs and heart are skeletal-or-cardiac muscle; once those
--       waste below the floor, biology ends.)
--   2. hunger > 0 → no catabolism (the meter has slack).
--   3. hunger ≤ 0 → eat body mass to cover the kcal deficit:
--      - fat > min_fat: 95% from fat, 5% from muscle (slow shrink,
--        visibly losing fat first).
--      - fat ≤ min_fat: pure muscle catabolism (sharp wasting).
--
-- After any mass change, call unit.recomputeBody so the engine
-- refreshes strength/max_hydration/max_hunger/carrying_capacity.
-- Without that, a starving unit's strength wouldn't drop.
-----------------------------------------------------------
local function tickStarvation(uid, dt)
    if unit.getPose(uid) == "dead" then return end

    local body = unit.getStat(uid, "body_mass")
    local lean = unit.getStat(uid, "lean_mass")
    local fat  = unit.getStat(uid, "fat_mass")
    local h    = unit.getStat(uid, "height")
    if not (body and lean and fat and h) then return end

    local minFat  = 0.44 * h * h
    local minLean = 4.4  * h * h

    -- Respiratory failure: sharp death when skeletal muscle (which
    -- includes the diaphragm) hits its floor.
    if lean <= minLean then
        emitDeathAlert(uid, "starvation")
        unit.kill(uid)
        return
    end

    -- No deficit if hunger pool isn't empty yet.
    local hunger = unit.getStat(uid, "hunger")
    if not hunger or hunger > 0 then return end

    -- metabolism_rate is already activity-aware (Phase 3 refactor),
    -- so deficit is just rate × dt.
    local rate = stats.get(uid, "metabolism_rate")
    if not rate or rate <= 0 then return end
    local deficit = rate * dt

    local fatEaten, muscleEaten
    if fat > minFat + FAT_FLOOR_TOL then
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

-----------------------------------------------------------
-- Per-resource tick. Returns nothing; side-effects on the unit.
-----------------------------------------------------------
local function tickResource(uid, defName, resourceName, params, activity, pose, dt)
    local maxVal = stats.get(uid, params.max_from)
    if not maxVal or maxVal <= 0 then return end

    -- First-tick init: if the unit has no value yet, fill it.
    local current = unit.getStat(uid, resourceName)
    if current == nil then
        unit.setStat(uid, resourceName, maxVal)
        return
    end

    -- Organ failure (Phase 4): when a unit's fat reserves have run
    -- out, biology can't sustain itself. Bypass all the usual
    -- pose/activity regen logic and let the fixed organ-failure
    -- drain run unopposed. Only stamina opts into this via the
    -- organ_failure_check flag.
    local inOrganFailure = false
    if params.organ_failure_check then
        local fat = unit.getStat(uid, "fat_mass")
        local h   = unit.getStat(uid, "height")
        if fat and h then
            inOrganFailure = fat <= (0.44 * h * h) + FAT_FLOOR_TOL
        end
    end

    -- Pose-keyed factors override activity-keyed ones. Collapsed +
    -- crouching are pose states (orthogonal to whatever activity the
    -- unit is doing inside that pose).
    local regenFactor
    if inOrganFailure then
        regenFactor = 0
    elseif pose     == "collapsed" then regenFactor = params.regen_factor_collapsed
    elseif pose     == "crawling"  then regenFactor = params.regen_factor_crawling
    elseif pose     == "crouching" then regenFactor = params.regen_factor_crouching
    elseif activity == "walking"   then regenFactor = params.regen_factor_walking
    elseif activity == "idle"      then regenFactor = params.regen_factor_idle
    end
    regenFactor = regenFactor or 0

    -- Regen scales with endurance — a strong unit recovers faster.
    -- If endurance is undefined for this unit type, we just skip regen.
    local endurance = unit.getStat(uid, "endurance") or 0
    local regen = regenFactor * endurance

    -- Drain has three parts: an always-on constant (drain_constant), an
    -- activity-specific drain (currently only drain_walking), and a
    -- body-driven metabolic drain (drain_metabolic = true, used by
    -- hunger). All three are additive, so a future resource can mix
    -- them — e.g. a "fatigue" stat could combine drain_constant with
    -- drain_metabolic.
    --
    -- metabolism_rate is already activity-aware (Lua-derived applies
    -- the walking multiplier internally), so the drain code just
    -- reads the authoritative burn rate.
    local drainActivity = (activity == "walking" and params.drain_walking) or 0
    local drainConstant = params.drain_constant or 0
    local drainMetabolic = 0
    if params.drain_metabolic then
        drainMetabolic = stats.get(uid, "metabolism_rate") or 0
    end
    local drainOrganFailure = inOrganFailure and ORGAN_FAILURE_DRAIN_PER_SEC or 0
    local drain = drainActivity + drainConstant + drainMetabolic + drainOrganFailure

    local next = current + (regen - drain) * dt

    if next < 0     then next = 0     end
    if next > maxVal then next = maxVal end

    -- Only write if it actually changed by a meaningful amount.
    -- Avoids hammering the unit manager IORef for sub-pixel updates.
    if math.abs(next - current) > 1e-4 then
        unit.setStat(uid, resourceName, next)
    end

    -- Survival warnings (player events). Debounced per-unit with
    -- hysteresis so a unit drifting around the threshold doesn't
    -- spam popups. See checkSurvivalAlerts for the trigger /
    -- rearm contract. This runs against `next` (post-write) so
    -- the threshold check matches what the engine just stored.
    if resourceName == "hunger" or resourceName == "hydration" then
        checkSurvivalAlerts(uid, resourceName, next, maxVal, pose, nil)
    end

    -- Death triggers run BEFORE collapse so a unit that crosses the
    -- death threshold this tick doesn't also get a collapse queued
    -- behind the kill (the engine processes commands in order; the
    -- kill snap to Dead would be clobbered by the collapse).
    --
    -- We compare against BOTH current (pre-regen) and next (post-regen).
    -- Without the current check, a debug-forced setStat(0) on stamina
    -- regenerates above zero on the same tick and never fires the kill
    -- — making the "force stamina to 0" playtest impossible.
    if pose ~= "dead" then
        if params.death_threshold and params.death_threshold > 0
           and (current / maxVal < params.death_threshold
                or next / maxVal < params.death_threshold) then
            emitDeathAlert(uid, deathCauseFor(resourceName))
            unit.kill(uid)
            return
        end
        if params.kill_on_zero and (current <= 0 or next <= 0) then
            emitDeathAlert(uid, deathCauseFor(resourceName))
            unit.kill(uid)
            return
        end
    end

    -- Collapse trigger. Only fires on non-collapsed units (a unit
    -- regenerating slowly through the threshold would otherwise
    -- re-stamp the collapse every tick). Dead units are excluded so
    -- a corpse never gets re-collapsed by ongoing drain.
    if pose ~= "collapsed" and pose ~= "dead"
       and params.collapse_threshold and params.collapse_threshold > 0
       and next / maxVal < params.collapse_threshold then
        unit.collapse(uid)
    end

    -- Surplus regrowth (hunger only via params.surplus_regrowth).
    -- Runs AFTER the death/collapse triggers so a unit on the edge
    -- of those doesn't also try to regrow body mass in the same
    -- tick. Dead units don't regrow.
    if params.surplus_regrowth and pose ~= "dead"
       and next > 0.75 * maxVal then
        applyRegrowth(uid, activity, dt)
    end

    -- NOTE: auto-revive is checked once per unit in checkRevive after
    -- all resources for that unit have ticked, not here. The per-
    -- resource version would race-revive a unit collapsed from thirst
    -- as soon as stamina recovered, even if hydration was still low.
end

-----------------------------------------------------------
-- Cross-resource revive check (called once per unit per tick).
-- Only revives if EVERY resource with a revive_threshold > 0 is
-- at-or-above its own threshold. Resources with revive_threshold = 0
-- don't gate revive.
-----------------------------------------------------------
local function checkRevive(uid, defConfig)
    -- Re-read pose here: a tickResource earlier in this update pass
    -- may have just called unit.collapse, so the pose snapshot taken
    -- before the per-resource loop is stale.
    local pose = unit.getPose(uid)
    if pose ~= "collapsed" then return end

    -- Blood-loss gate. The combat wound subsystem
    -- (Combat.Wounds.tickOneUnit) collapses a unit when blood drops
    -- below 30% of max. If we revive purely on stamina/hydration,
    -- a bleeding unit pops back up, the next 10 Hz wound tick sees
    -- blood still < 30%, fires UnconsciousNow again, and the pose
    -- flaps standing↔collapsed at the wound-tick rate. Visually
    -- the unit flickers between its injured idle anim and the
    -- collapsed pose (T-pose if no collapsed-idle is registered).
    --
    -- Hysteresis: collapse fires at 30%, revive needs ≥ 50%. Since
    -- blood doesn't passively regen — only wound closure refills
    -- it indirectly — a bleeding-out unit stays down until the
    -- wounds heal or first-aid lands.
    local blood = unit.getBlood(uid)
    if blood and blood.max > 0 and blood.current / blood.max < 0.5 then
        return
    end

    for resourceName, params in pairs(defConfig) do
        local rt = params.revive_threshold
        if rt and rt > 0 then
            local maxVal = stats.get(uid, params.max_from)
            local cur    = unit.getStat(uid, resourceName)
            if not maxVal or maxVal <= 0 or not cur then
                return    -- can't evaluate; play it safe, no revive
            end
            if cur / maxVal < rt then
                return    -- at least one resource still below threshold
            end
        end
    end
    unit.revive(uid)
end

-----------------------------------------------------------
-- Stance: combat readiness, 0..1. Spent by attacking and by taking
-- hits (both engine-side in Combat.Resolution); recovers here toward
-- 1.0 — quickly, and faster for agile/dextrous units (the time it
-- takes to set your feet and ready your guard). Absent ⇒ treated as
-- 1.0 everywhere, so no explicit spawn init is needed.
-----------------------------------------------------------
local STANCE_RECOVER_BASE     = 0.35   -- per second, floor
local STANCE_RECOVER_PER_STAT = 0.12   -- per second per (dex+agi) point
local function tickStance(uid, dt)
    local cur = unit.getStat(uid, "stance")
    if cur == nil or cur >= 1.0 then return end
    local dex  = unit.getStat(uid, "dexterity") or 1.0
    local agi  = unit.getStat(uid, "agility") or 1.0
    local rate = STANCE_RECOVER_BASE + STANCE_RECOVER_PER_STAT * (dex + agi)
    local newv = cur + rate * dt
    if newv > 1.0 then newv = 1.0 end
    unit.setStat(uid, "stance", newv)
end

-----------------------------------------------------------
-- Update (called at tick interval by engine.loadScript)
-----------------------------------------------------------
function unitResources.update(dt)
    if require("scripts.pause").isPaused() then return end
    local ids = unit.getAllIds()
    if not ids or #ids == 0 then return end

    for _, uid in ipairs(ids) do
        local info = unit.getInfo(uid)
        if info and info.defName then
            tickStance(uid, dt)
            local defConfig = config[info.defName]
            if defConfig then
                local activity = unit.getActivity(uid) or "idle"
                local pose     = unit.getPose(uid) or "standing"
                for resourceName, params in pairs(defConfig) do
                    tickResource(uid, info.defName, resourceName,
                                 params, activity, pose, dt)
                end
                -- After resources have ticked (and hunger has drained),
                -- catabolism eats body mass if hunger is empty.
                -- Respiratory failure (lean ≤ min_lean) is the direct
                -- kill path here; the gradual organ-failure path lives
                -- in the stamina branch of tickResource above.
                tickStarvation(uid, dt)
                checkRevive(uid, defConfig)
            end
        end
    end
end

-----------------------------------------------------------
-- Shutdown
-----------------------------------------------------------
function unitResources.shutdown()
    engine.logInfo("Unit resources tick shut down")
end

-- Exposed for debug console: inspect what a unit's drain/regen
-- numbers are. Returns nil if the def isn't configured.
function unitResources.getConfig(defName)
    return config[defName]
end

return unitResources
