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
--               behaviour (aimless wander, fleeing other units,
--               catatonia, or lashing out at whoever's nearby) drawn
--               once at entry and held throughout. The unit AI
--               short-circuits on it exactly like delirium — no goals,
--               no work, no combat outside the episode's own behaviour
--               (scripts/unit_ai_mental.lua).
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
--   mental_break_behavior  — 0 wander / 1 flee / 2 catatonia /
--                            3 lash-out, rolled at break entry
--
-- Ticked from unit_resources.update right after thoughts.tick, so it
-- reads the tick's freshly-computed state_of_mind. Catatonia and
-- lash-out shipped in #717; still deferred to a follow-up (per the
-- #352 design session): mania/psychosis states, environmental
-- one-shot triggers.

local brain = require("scripts.brain")

local mental = package.loaded["scripts.mental_state"] or {}
package.loaded["scripts.mental_state"] = mental

-- State codes (the "mental_state" stat's values).
mental.STABLE, mental.STRESSED, mental.BREAK, mental.EUPHORIC = 0, 1, 2, 3
local STATE_NAMES = { [0] = "stable", [1] = "stressed",
                      [2] = "break",  [3] = "euphoric" }

-- Break-behaviour codes (the "mental_break_behavior" stat's values).
-- Append-only (#717): existing 0/1 meanings never change.
mental.WANDER, mental.FLEE, mental.CATATONIA, mental.LASHOUT = 0, 1, 2, 3
local BEHAVIOR_NAMES = { [0] = "wander", [1] = "flee",
                         [2] = "catatonia", [3] = "lash_out" }

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
    -- Break-behaviour roll weights (#717). Sum to 1.0; wander/flee stay
    -- dominant so existing saved units' expectations hold.
    WANDER_WEIGHT     = 0.35,
    FLEE_WEIGHT       = 0.35,
    CATATONIA_WEIGHT  = 0.15,
    LASHOUT_WEIGHT    = 0.15,
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
    return BEHAVIOR_NAMES[b] or "wander"
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

-- Weighted 4-way break-behaviour draw (#717). Exposed (not local) with an
-- optional `draw` (0..1) so the probe can pin exact boundary values
-- deterministically instead of relying on statistical sampling of
-- math.random(); omitted, it rolls for real.
function mental.rollBehavior(draw)
    local r = draw or math.random()
    local t = mental.TUNE
    if r < t.WANDER_WEIGHT then return mental.WANDER end
    r = r - t.WANDER_WEIGHT
    if r < t.FLEE_WEIGHT then return mental.FLEE end
    r = r - t.FLEE_WEIGHT
    if r < t.CATATONIA_WEIGHT then return mental.CATATONIA end
    return mental.LASHOUT
end

-- Per-behaviour narration (#717): the thought and event-log line both
-- default to the original generic text (wander/flee keep it — no
-- change to their established narration), overridden for catatonia and
-- lash-out so they read as distinguishable episodes. Every event-log
-- variant keeps the "mental break" substring so log/UI code (and the
-- probe) that greps for it keeps matching regardless of which
-- behaviour rolled.
local BREAK_THOUGHT = {
    [mental.CATATONIA] = "Everything goes distant and still. Moving feels impossible.",
    [mental.LASHOUT]   = "Something in me wants to hurt someone.",
}
local BREAK_EVENT_SUFFIX = {
    [mental.CATATONIA] = " is having a catatonic mental break — frozen in place.",
    [mental.LASHOUT]   = " is having a violent mental break — lashing out at anyone nearby.",
}

local function enterBreak(uid, now, behavior)
    if behavior == nil then
        behavior = mental.rollBehavior()
    end
    unit.setStat(uid, "mental_break_behavior", behavior)
    unit.setStat(uid, "mental_until", now + rollDuration())
    setState(uid, mental.BREAK, now)
    -- Drop whatever walk is in flight so the break behaviour takes
    -- over immediately rather than after the current leg.
    unit.stop(uid)
    local who, info = unitLabel(uid)
    thought.emit(uid, BREAK_THOUGHT[behavior]
                     or "Something snaps. It is all too much.", "state")
    emitFor(uid, "unit_warning",
            who .. (BREAK_EVENT_SUFFIX[behavior]
                    or " is having a mental break."), info)
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
-- ("wander" | "flee" | "catatonia" | "lash_out"; absent = random,
-- like a rolled break).
function mental.forceBreak(uid, behavior)
    local b = nil
    if behavior == "flee" then b = mental.FLEE
    elseif behavior == "wander" then b = mental.WANDER
    elseif behavior == "catatonia" then b = mental.CATATONIA
    elseif behavior == "lash_out" then b = mental.LASHOUT end
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
