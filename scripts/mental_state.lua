-- Mental states (#352) — the threshold state-machine over the unified
-- state_of_mind (#350), extending the physiological alert → confused →
-- delirious → unconscious ladder with psychological states:
--
--   stable    — nothing notable. The default.
--   stressed  — state_of_mind has sunk below STRESSED_BELOW. A label
--               with hysteresis (recovers at STRESS_RECOVER_AT, same
--               band shape as brain.lua's RISE_AT), not an episode:
--               it has no behavioural effect of its own, but while it
--               holds, break rolls run.
--   break     — a mental-break EPISODE: rolled while stressed (chance
--               per roll scales with how far below the threshold the
--               state of mind sits), fixed duration drawn at entry,
--               behaviour (aimless wander or fleeing other units)
--               drawn once at entry and held throughout. The unit AI
--               short-circuits on it exactly like delirium — no goals,
--               no work, no combat (scripts/unit_ai_mental.lua).
--   euphoric  — the high-end mirror: sustained near-content state of
--               mind can tip into a euphoria episode. Mostly narrative
--               (thoughts + event log) plus a small concentration
--               bonus — same instantaneous philosophy as caffeine.
--
-- Flicker-safety (#304's collapse↔crawl lesson) is structural, not
-- tuned: the stressed label carries a hysteresis band, and episodes
-- are fixed-duration + cooldown (temporal hysteresis) — a noisy
-- state_of_mind can't toggle an episode on/off tick-to-tick.
--
-- All machine state lives in unit stats (persists in saves for free,
-- same pattern as mood / thought_next_at; deadlines are absolute
-- engine.gameTime() values, which also persist):
--   mental_state           — 0 stable / 1 stressed / 2 break / 3 euphoric
--   mental_since           — when the current state was entered
--   mental_until           — episode end deadline (break/euphoria)
--   mental_cooldown_until  — no new episode rolls until this passes
--   mental_next_roll_at    — episode-roll cadence deadline
--   mental_high_since      — how long state_of_mind has held the
--                            euphoria band (0 = not in band)
--   mental_break_behavior  — 0 wander / 1 flee, rolled at break entry
--
-- Ticked from unit_resources.update right after thoughts.tick, so it
-- reads the tick's freshly-computed state_of_mind. Deferred to a
-- follow-up (per the #352 design session): mania/psychosis states,
-- catatonia/lash-out behaviours, environmental one-shot triggers.

local brain = require("scripts.brain")

local mental = package.loaded["scripts.mental_state"] or {}
package.loaded["scripts.mental_state"] = mental

-- State codes (the "mental_state" stat's values).
mental.STABLE, mental.STRESSED, mental.BREAK, mental.EUPHORIC = 0, 1, 2, 3
local STATE_NAMES = { [0] = "stable", [1] = "stressed",
                      [2] = "break",  [3] = "euphoric" }

-- Break-behaviour codes (the "mental_break_behavior" stat's values).
mental.WANDER, mental.FLEE = 0, 1

-- Exposed (not local) so the probe / debug console can pin the rolls
-- deterministic (BREAK_CHANCE_MAX = 1.0) or shrink the timers — same
-- pattern as thoughts.TRIGGERS. Durations are game-seconds.
mental.TUNE = {
    STRESSED_BELOW    = 0.35,  -- stable → stressed entry
    STRESS_RECOVER_AT = 0.45,  -- stressed → stable exit (hysteresis)
    EUPHORIC_ABOVE    = 0.90,  -- euphoria-band entry
    EUPHORIC_EXIT     = 0.80,  -- euphoria episode early-exit (hysteresis)
    SUSTAIN           = 20.0,  -- in-band this long before rolls begin
    CHECK_INTERVAL    = 8.0,   -- between episode rolls
    BREAK_CHANCE_MAX  = 0.35,  -- per-roll break chance at max severity
    EUPHORIA_CHANCE   = 0.10,  -- per-roll euphoria chance
    EPISODE_MIN       = 60.0,  -- episode duration draw
    EPISODE_MAX       = 120.0,
    COOLDOWN          = 240.0, -- after an episode ends
    EUPHORIA_CONCENTRATION_BONUS = 0.10,
}

local function clamp(x, lo, hi) return math.max(lo, math.min(hi, x)) end

local function stateCode(uid)
    return unit.getStat(uid, "mental_state") or mental.STABLE
end

function mental.state(uid)
    return STATE_NAMES[stateCode(uid)] or "stable"
end

function mental.isBreaking(uid) return stateCode(uid) == mental.BREAK end
function mental.isEuphoric(uid) return stateCode(uid) == mental.EUPHORIC end
function mental.isStressed(uid) return stateCode(uid) == mental.STRESSED end

function mental.breakBehavior(uid)
    local b = unit.getStat(uid, "mental_break_behavior") or mental.WANDER
    return b == mental.FLEE and "flee" or "wander"
end

local function setState(uid, st, now)
    unit.setStat(uid, "mental_state", st)
    unit.setStat(uid, "mental_since", now)
    -- Fresh roll cadence per state; SUSTAIN dominates the first roll
    -- anyway, this just keeps the deadline from leaking across states.
    unit.setStat(uid, "mental_next_roll_at", now + mental.TUNE.CHECK_INTERVAL)
end

local function unitLabel(uid)
    local info = unit.getInfo(uid)
    if info and info.name and info.name ~= "" then return info.name, info end
    return "Unit " .. tostring(uid), info
end

local function emitFor(uid, category, msg, info)
    if info and info.gridX then
        engine.emitEventForUnit(category, msg, uid, info.gridX, info.gridY)
    else
        engine.emitEventForUnit(category, msg, uid)
    end
end

local function rollDuration()
    return mental.TUNE.EPISODE_MIN
         + math.random() * (mental.TUNE.EPISODE_MAX - mental.TUNE.EPISODE_MIN)
end

local function enterBreak(uid, now, behavior)
    if behavior == nil then
        behavior = math.random() < 0.5 and mental.WANDER or mental.FLEE
    end
    unit.setStat(uid, "mental_break_behavior", behavior)
    unit.setStat(uid, "mental_until", now + rollDuration())
    setState(uid, mental.BREAK, now)
    -- Drop whatever walk is in flight so the break behaviour takes
    -- over immediately rather than after the current leg.
    unit.stop(uid)
    local who, info = unitLabel(uid)
    thought.emit(uid, "Something snaps. It is all too much.", "state")
    emitFor(uid, "unit_warning", who .. " is having a mental break.", info)
end

local function enterEuphoria(uid, now)
    unit.setStat(uid, "mental_until", now + rollDuration())
    setState(uid, mental.EUPHORIC, now)
    local who, info = unitLabel(uid)
    thought.emit(uid, "A sudden lightness — everything feels possible.", "state")
    emitFor(uid, "unit_event", who .. " is overcome with euphoria.", info)
end

local function endEpisode(uid, now, som)
    unit.setStat(uid, "mental_cooldown_until", now + mental.TUNE.COOLDOWN)
    local next_ = som < mental.TUNE.STRESSED_BELOW and mental.STRESSED
                                                    or mental.STABLE
    setState(uid, next_, now)
end

-- Probe / debug-console hook: start a break episode right now,
-- bypassing the stress ladder and the rolls. behavior is optional
-- ("wander" | "flee"; absent = random, like a rolled break).
function mental.forceBreak(uid, behavior)
    local b = nil
    if behavior == "flee" then b = mental.FLEE
    elseif behavior == "wander" then b = mental.WANDER end
    enterBreak(uid, engine.gameTime(), b)
end

-- Advance one unit's mental-state machine (called each physiology
-- tick, after brain.tick + thoughts.tick).
function mental.tick(uid, dt)
    local now = engine.gameTime()
    local st  = unit.getStat(uid, "mental_state")
    if st == nil then
        setState(uid, mental.STABLE, now)
        return
    end
    local som = brain.stateOfMind(uid)

    -- Ongoing episodes run their clock; nothing else fires meanwhile.
    if st == mental.BREAK then
        if now >= (unit.getStat(uid, "mental_until") or 0) then
            endEpisode(uid, now, som)
            local who, info = unitLabel(uid)
            emitFor(uid, "unit_event",
                    who .. " has come back to their senses.", info)
        end
        return
    end
    if st == mental.EUPHORIC then
        if now >= (unit.getStat(uid, "mental_until") or 0)
           or som < mental.TUNE.EUPHORIC_EXIT then
            endEpisode(uid, now, som)
        else
            -- Mild effect: a small focus lift on top of what brain.tick
            -- just recomputed (concentration is instantaneous — no
            -- integration — so bumping the stored stat each tick is
            -- exactly how caffeine's bonus behaves too).
            local c = unit.getStat(uid, "concentration") or 1.0
            unit.setStat(uid, "concentration",
                clamp(c + mental.TUNE.EUPHORIA_CONCENTRATION_BONUS, 0, 1))
        end
        return
    end

    -- Track how long the state of mind has held the euphoria band —
    -- entry needs SUSTAIN in-band, not one lucky tick.
    local highSince = unit.getStat(uid, "mental_high_since") or 0
    if som >= mental.TUNE.EUPHORIC_ABOVE then
        if highSince == 0 then
            highSince = now
            unit.setStat(uid, "mental_high_since", now)
        end
    elseif highSince ~= 0 then
        highSince = 0
        unit.setStat(uid, "mental_high_since", 0)
    end

    -- The stressed label, with its hysteresis band.
    if st == mental.STABLE and som < mental.TUNE.STRESSED_BELOW then
        setState(uid, mental.STRESSED, now)
        st = mental.STRESSED
    elseif st == mental.STRESSED and som >= mental.TUNE.STRESS_RECOVER_AT then
        setState(uid, mental.STABLE, now)
        st = mental.STABLE
    end

    -- Episode rolls: cooled down, physiologically lucid (below the
    -- delirious band the collapse/delirium machinery owns behaviour —
    -- no double-driving), sustained in-state, and on the roll cadence.
    if now < (unit.getStat(uid, "mental_cooldown_until") or 0) then return end
    if brain.isDelirious(uid) or brain.isUnconscious(uid) then return end
    if now < (unit.getStat(uid, "mental_next_roll_at") or 0) then return end
    unit.setStat(uid, "mental_next_roll_at", now + mental.TUNE.CHECK_INTERVAL)

    if st == mental.STRESSED then
        if now - (unit.getStat(uid, "mental_since") or now)
           < mental.TUNE.SUSTAIN then return end
        local severity = clamp((mental.TUNE.STRESSED_BELOW - som)
                               / mental.TUNE.STRESSED_BELOW, 0, 1)
        if math.random() < mental.TUNE.BREAK_CHANCE_MAX * severity then
            enterBreak(uid, now, nil)
        end
    elseif st == mental.STABLE and highSince ~= 0
           and now - highSince >= mental.TUNE.SUSTAIN then
        if math.random() < mental.TUNE.EUPHORIA_CHANCE then
            enterEuphoria(uid, now)
        end
    end
end

-- Everything in one table for the probe / debug console (auto-JSON).
function mental.summary(uid)
    local s = {
        state         = mental.state(uid),
        since         = unit.getStat(uid, "mental_since"),
        endsAt        = unit.getStat(uid, "mental_until"),
        cooldownUntil = unit.getStat(uid, "mental_cooldown_until"),
        stateOfMind   = brain.stateOfMind(uid),
    }
    if mental.isBreaking(uid) then s.behavior = mental.breakBehavior(uid) end
    return s
end

return mental
