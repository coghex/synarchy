-- Per-resource tick + cross-resource revive check for
-- scripts/unit_resources.lua. This is the drain/regen engine that
-- config entries (scripts/unit_resource_config.lua) drive.

local stats          = require("scripts.unit_stats")
local movementSpeed   = require("scripts.movement_speed")
local injuries        = require("scripts.injuries")
local brain           = require("scripts.brain")
local alerts          = require("scripts.unit_resource_alerts")
local energy          = require("scripts.unit_resource_energy")

local M = {}

-- Phase 4 organ failure: when a unit's fat reserves have run out, the
-- organ-failure branch fires in the stamina tick — regen is overridden
-- to 0 and this constant drain runs regardless of activity/pose. With
-- max_stamina ≈ 10 the unit runs out of stamina in 16–20 real-seconds,
-- then Phase 1's universal "stamina == 0 → die" rule fires. The other
-- death path is respiratory failure (lean ≤ min_lean), handled directly
-- in unit_resource_energy's tickStarvation.
local ORGAN_FAILURE_DRAIN_PER_SEC = 0.5

-- Uphill exertion (#375). The engine reports the signed slope grade the
-- unit is walking (getInfo.moveGrade: 1.0 = straight up a ramp's fall
-- line, negative = downhill). Climbing multiplies the EFFORT the
-- speed-drain models: the drain ratio uses speed × (1 + K·grade) in
-- place of speed, so a unit holding its commanded pace up a full grade
-- burns like it's moving (1 + K)× faster. At K = 0.5 a comfort-pace
-- ascent burns (1.5)² = 2.25× the aerobic supply (close to the ~2.4×
-- stair-climbing-vs-walking energy multiple), a net drain that leaves
-- a baseline acolyte's pool nearly spent after ~5 z of continuous
-- full-grade ascent — instead of the flat-ground equilibrium. Downhill
-- and flat leave the model untouched (grade ≤ 0 clamps to 0):
-- descending is easier on the legs, not free stamina. Endurance and
-- encumbrance already shape comfort itself, so they scale this the
-- same way they scale all locomotion drain.
local UPHILL_EXERTION_PER_GRADE = 0.5

-----------------------------------------------------------
-- Per-resource tick. Returns nothing; side-effects on the unit.
-----------------------------------------------------------
function M.tickResource(uid, defName, resourceName, params, activity, pose, dt)
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
            inOrganFailure = fat <= (0.44 * h * h) + energy.FAT_FLOOR_TOL
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
    local isMoving = (activity == "walking" or activity == "running")
    -- Constant drain: an absolute term (drain_constant) plus a
    -- mass-proportional term (drain_constant_frac × this resource's max).
    -- The proportional form keeps depletion TIME size-invariant for pools
    -- that scale with body mass (hydration), so tiny and giant creatures
    -- run dry over the same game-time instead of small ones dying instantly.
    local drainConstant = (params.drain_constant or 0)
                        + (params.drain_constant_frac or 0) * maxVal
    -- Exertion scaling: a working/fighting unit burns its baseline pool
    -- faster than an idle one. Opt-in (hydration) so calories — which
    -- already folds the activity multiplier into metabolism_rate — isn't
    -- double-scaled.
    if params.drain_activity_scaled then
        drainConstant = drainConstant * stats.activityMultiplier(uid)
    end
    local drainMetabolic = 0
    if params.drain_metabolic then
        drainMetabolic = stats.get(uid, "metabolism_rate") or 0
    end
    local drainOrganFailure = inOrganFailure and ORGAN_FAILURE_DRAIN_PER_SEC or 0

    local drainActivity
    if params.speed_drain and isMoving and not inOrganFailure then
        -- Speed-dependent locomotion: recover at a fixed aerobic supply,
        -- drain as (speed/comfort)². Comfort speed is the equilibrium
        -- (net 0); slower regenerates, sprinting drains hard. Overrides
        -- both the activity regen factor and the flat drain_walking.
        -- Uphill grade (#375) multiplies the effort side: sustained
        -- ascent drains even at comfort pace (see UPHILL_EXERTION_PER_
        -- GRADE); flat and downhill leave the model unchanged.
        local supply  = (params.move_regen_factor or 0.5) * endurance
        local info    = unit.getInfo(uid) or {}
        local v       = info.moveSpeed or 0
        local grade   = math.max(0, info.moveGrade or 0)
        local effort  = v * (1 + UPHILL_EXERTION_PER_GRADE * grade)
        local comfort = movementSpeed.comfort(uid)
        local ratio   = (comfort > 0) and (effort / comfort) or 1
        regen         = supply
        drainActivity = supply * ratio * ratio
    else
        drainActivity = (activity == "walking" and params.drain_walking) or 0
    end

    -- Caffeine fatigue-offset (#347): an opt-in stimulant regen bonus,
    -- additive on top of whatever pose/activity/speed regen this
    -- resource already resolved to above. Skipped during organ failure
    -- — the body has nothing left for a stimulant to help with.
    if params.caffeine_regen_bonus and not inOrganFailure then
        local caffeine = unit.getStat(uid, "caffeine") or 0
        regen = regen + caffeine * params.caffeine_regen_bonus
    end

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
    -- spam popups. See unit_resource_alerts.checkSurvivalAlerts for
    -- the trigger / rearm contract. This runs against `next`
    -- (post-write) so the threshold check matches what the engine
    -- just stored.
    if resourceName == "calories" or resourceName == "hydration" then
        alerts.checkSurvivalAlerts(uid, resourceName, next, maxVal, pose, nil)
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
            alerts.emitDeathAlert(uid, alerts.deathCauseFor(resourceName))
            unit.kill(uid)
            return
        end
        if params.kill_on_zero and (current <= 0 or next <= 0) then
            alerts.emitDeathAlert(uid, alerts.deathCauseFor(resourceName))
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
        energy.applyRegrowth(uid, activity, dt)
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
function M.checkRevive(uid, defConfig)
    -- Re-read pose here: a tickResource earlier in this update pass
    -- may have just called unit.collapse, so the pose snapshot taken
    -- before the per-resource loop is stale.
    local pose = unit.getPose(uid)
    if pose ~= "collapsed" then return end

    -- Fall-knockdown gate. A unit knocked down by a fall recovers on its
    -- own self-timed clock (engine-side, in the movement tick), NOT via
    -- this resource-revive path. Standing it up here would (a) cut the
    -- knockdown stun short whenever resources happen to be fine, and (b)
    -- conflate "winded by a fall" with "recovered from exhaustion". Leave
    -- knockdowns alone; the engine stands them up when the timer expires,
    -- and if the unit is ALSO resource-collapsed it re-collapses cleanly
    -- as a survival collapse (no getup timer) right after.
    local info = unit.getInfo(uid)
    if info and info.knockedDown then return end

    -- Injury gate. A unit incapacitated by injury stays down until it
    -- clears the SAME rise band the collapse↔crawl machine uses (#304),
    -- not the lower threshold it collapsed at — otherwise a concussion
    -- healing through 0.25..0.35 (or legs that mend before the concussion)
    -- would stand the unit up while tickInjuries still wants it down,
    -- flapping the pose. Concussion must drop below CONCUSSION_RISE
    -- (injuries.concussionCanRise); a unit that still can't walk stays down
    -- (it crawls via tickInjuries, it doesn't stand).
    if not injuries.concussionCanRise(uid) or injuries.cannotWalk(uid) then
        return
    end

    -- Consciousness gate. A unit knocked out by heat stroke / hypoxia / salt
    -- imbalance stays down until consciousness recovers (hysteresis: collapse
    -- at <0.15, rise at ≥0.40 — see brain.lua).
    if not brain.canRise(uid) then return end

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

return M
