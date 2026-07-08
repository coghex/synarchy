-- Per-def resource config for scripts/unit_resources.lua.
--
-- Drain is a per-second constant. Regen factors are multiplied by
-- endurance: a high-endurance unit recovers much faster than a weak
-- one (and may net-regen even while walking). collapse_threshold is
-- the fraction-of-max below which the unit transitions to Collapsed
-- (via unit.collapse). Set to 0 to disable.
--
-- To add a resource:
--   1. Add an entry under `config[defName].resourceName` with
--      drain/regen/threshold values.
--   2. Ensure `max_resourceName` exists either as a derived formula
--      in scripts/unit_stats.lua or as an attribute in the YAML.
--   3. Add a branch in unit_resource_tick.lua's tickResource if the
--      trigger differs from stamina's "collapse when below threshold".
local config = {
    acolyte = {
        stamina = {
            max_from               = "max_stamina",
            -- Speed-dependent locomotion drain. When the unit is moving
            -- (walking/running), stamina drains as
            --   drain = (move_regen_factor·endurance) · (speed/comfort)²
            -- and regens at move_regen_factor·endurance — so the net is
            -- 0 exactly at the unit's comfort speed, positive below it,
            -- and sharply negative when sprinting. comfort/sprint come
            -- from scripts/movement_speed.lua. Falls back to drain_walking
            -- only if speed_drain is unset.
            speed_drain            = true,
            move_regen_factor      = 0.5,
            drain_walking          = 0.1,    -- per second (legacy fallback)
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
            -- Caffeine fatigue-offset (#347): at a fully-dosed caffeine
            -- meter (brain.lua), +0.3 stamina/sec on top of the normal
            -- pose/activity regen — comparable to regen_factor_idle
            -- (0.5) at endurance 1.0, i.e. a real but not
            -- stamina-trivializing pick-me-up.
            caffeine_regen_bonus   = 0.3,
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
            -- Proportional to max_hydration so the depletion TIME is the
            -- same for any body size. Calibrated to the old absolute
            -- 0.01 L/s at the human max_hydration of ~42 L (0.01/42).
            drain_constant_frac = 0.01 / 42,
            -- Scale the baseline drain by the unit's exertion (the same
            -- activity multiplier the hunger burn uses) — a unit fighting
            -- or mining dries out faster than one standing idle. The sweat
            -- drain in thermo.lua is a SEPARATE, climate-driven loss; this
            -- is the metabolic baseline reacting to activity.
            drain_activity_scaled = true,
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
        -- Two-layer food model (#93). "hunger" is the STOMACH meter:
        -- filled by eating (unit.feed), drained ONLY by digestion
        -- (tickDigestion — a coupled transfer, not a tickResource
        -- drain, so this entry declares no drain at all; it exists so
        -- the first-tick init seeds a live pool). The eat AI triggers
        -- on this meter — the unit feels hungry, not depleted.
        hunger = {
            max_from = "max_hunger",
        },
        -- "calories" is the energy STORE digestion feeds. Everything
        -- the old single-layer "hunger" pool drove now keys on this:
        -- metabolic burn, surplus regrowth, catabolism (tickStarvation),
        -- the starving alert, thermo heat gating and wound-heal rate.
        calories = {
            max_from         = "max_calories",
            -- Drain at metabolism_rate × activity_multiplier rather
            -- than a fixed constant. Walking burns ~1.5× idle BMR.
            -- An empty store alone doesn't collapse the unit —
            -- fasting is energizing; weakness only kicks in after
            -- Phase 4 catabolism exhausts the fat reserves and the
            -- organ-failure stamina drain takes over.
            drain_metabolic  = true,
            -- Surplus storage. While calories > 75 % of max, divert
            -- REGROWTH_RATE_KCAL_PER_SEC from the store into body
            -- mass at the activity-dependent split. Both the divert
            -- AND the metabolic drain run in the same tick, so a
            -- well-fed unit empties its store a bit faster than the
            -- BMR alone — that's the food being stored.
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
            speed_drain            = true,
            move_regen_factor      = 0.5,
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
    -- Red squirrel: same minimal stamina model as the bear so flight
    -- sprints and the (rare) combat drain land somewhere. Tiny prey —
    -- no hunger/hydration/organ-failure layer.
    red_squirrel = {
        stamina = {
            max_from               = "max_stamina",
            speed_drain            = true,
            move_regen_factor      = 0.5,
            drain_walking          = 0.1,
            regen_factor_walking   = 0.12,
            regen_factor_idle      = 0.5,
            regen_factor_collapsed = 0.3,
            regen_factor_crouching = 0.5,
            collapse_threshold     = 0.1,
            revive_threshold       = 0.5,
            kill_on_zero           = true,
        },
    },
}

return config
