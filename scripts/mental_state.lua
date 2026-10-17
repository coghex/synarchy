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
-- All machine state lives in the MIND system's owned keys (persists in saves
-- for free, same pattern as mood / thought_next_at; deadlines are absolute
-- engine.gameTime() values, which also persist). Since #2838 (S11a) each
-- event commits its WHOLE key set as one atomic batch (physiology.commit), so
-- the combat and craft threads, which read mental_state while this worker
-- runs, only ever see a complete state: never a new state with the old timers
-- or behavior. A unit whose mind is not Simulated has no such state (Nominal
-- reads a stable, episode-free unit, Unsupported nothing): this machine makes
-- no attempt, no random draw, no thought record and no event for it.
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
-- Ticked from unit_resources.update right after thoughts.tick. It reads the
-- DERIVED state_of_mind when it runs, so a thought's mood nudge from this same
-- pass is already in it, and an euphoric episode's concentration lift (a rule
-- of the derived concentration, not a stored bump) exists exactly while the
-- episode does. Catatonia and
-- lash-out shipped in #717; still deferred to a follow-up (per the
-- #352 design session): mania/psychosis states, environmental
-- one-shot triggers.

local brain      = require("scripts.brain")
local physiology = require("scripts.physiology")

local mental = package.loaded["scripts.mental_state"] or {}
package.loaded["scripts.mental_state"] = mental

-- State codes (the "mental_state" stat's values). EUPHORIC's value (3)
-- is Unit.Physiology.Mind.euphoricState, which the derived concentration rule
-- and Combat.Resolution.Common.mentalEffectiveness (#353, the euphoria bonus
-- gate) both read — change all three in lockstep.
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
    -- Break-behaviour roll weights (#717). Sum to 1.0; wander/flee stay
    -- dominant so existing saved units' expectations hold.
    WANDER_WEIGHT     = 0.35,
    FLEE_WEIGHT       = 0.35,
    CATATONIA_WEIGHT  = 0.15,
    LASHOUT_WEIGHT    = 0.15,
}

local clamp = require("scripts.lib.numeric").clamp

local function stateCode(uid)
    return physiology.get(uid, "mental_state") or mental.STABLE
end

function mental.state(uid)
    return STATE_NAMES[stateCode(uid)] or "stable"
end

function mental.isBreaking(uid) return stateCode(uid) == mental.BREAK end
function mental.isEuphoric(uid) return stateCode(uid) == mental.EUPHORIC end
function mental.isStressed(uid) return stateCode(uid) == mental.STRESSED end

function mental.breakBehavior(uid)
    local b = physiology.get(uid, "mental_break_behavior") or mental.WANDER
    return BEHAVIOR_NAMES[b] or "wander"
end

-- The keys a state change writes: the state, when it was entered and a fresh
-- roll cadence (SUSTAIN dominates the first roll anyway, this just keeps the
-- deadline from leaking across states). Every event adds its own keys to the
-- SAME table and commits once.
local function stateKeys(batch, st, now)
    batch.mental_state        = st
    batch.mental_since        = now
    batch.mental_next_roll_at = now + mental.TUNE.CHECK_INTERVAL
    return batch
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
    -- ONE batch: behavior, episode deadline, state, since and next-roll
    -- deadline together.
    local batch = stateKeys({ mental_break_behavior = behavior,
                              mental_until = now + rollDuration() },
                            mental.BREAK, now)
    if not physiology.commit(uid, batch) then return end
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
    local batch = stateKeys({ mental_until = now + rollDuration() },
                            mental.EUPHORIC, now)
    if not physiology.commit(uid, batch) then return end
    local who, info = unitLabel(uid)
    thought.emit(uid, "A sudden lightness — everything feels possible.", "state")
    emitFor(uid, "unit_event", who .. " is overcome with euphoria.", info)
end

-- Returns whether the episode ended (the batch committed): the cooldown and the
-- next state with its timing, together.
local function endEpisode(uid, now, som)
    local next_ = som < mental.TUNE.STRESSED_BELOW and mental.STRESSED
                                                    or mental.STABLE
    return physiology.commit(uid, stateKeys(
        { mental_cooldown_until = now + mental.TUNE.COOLDOWN }, next_, now))
end

-- Probe / debug-console hook: start a break episode right now,
-- bypassing the stress ladder and the rolls. behavior is optional
-- ("wander" | "flee" | "catatonia" | "lash_out"; absent = random,
-- like a rolled break).
function mental.forceBreak(uid, behavior)
    if not physiology.isSimulated(uid, "mind") then return end
    local b = nil
    if behavior == "flee" then b = mental.FLEE
    elseif behavior == "wander" then b = mental.WANDER
    elseif behavior == "catatonia" then b = mental.CATATONIA
    elseif behavior == "lash_out" then b = mental.LASHOUT end
    enterBreak(uid, engine.gameTime(), b)
end

-- Advance one unit's mental-state machine (called each physiology
-- tick, after brain.tick + thoughts.tick). Every event is ONE atomic batch;
-- an event whose batch is refused publishes none of its consequences.
function mental.tick(uid, dt)
    -- Mode first: a Nominal or Unsupported mind has no machine to advance.
    if not physiology.isSimulated(uid, "mind") then return end
    local now = engine.gameTime()
    local st  = physiology.get(uid, "mental_state")
    local som = brain.stateOfMind(uid)

    -- Ongoing episodes run their clock; nothing else fires meanwhile.
    if st == mental.BREAK then
        if now >= physiology.get(uid, "mental_until") then
            if endEpisode(uid, now, som) then
                local who, info = unitLabel(uid)
                emitFor(uid, "unit_event",
                        who .. " has come back to their senses.", info)
            end
        end
        return
    end
    if st == mental.EUPHORIC then
        if now >= physiology.get(uid, "mental_until")
           or som < mental.TUNE.EUPHORIC_EXIT then
            endEpisode(uid, now, som)
        end
        -- The mild focus lift euphoria gives is a rule of the DERIVED
        -- concentration (Unit.Physiology.Mind.concentrationRule): it exists
        -- exactly while mental_state is EUPHORIC, applied once, with no
        -- stored bump to leak.
        return
    end

    -- Track how long the state of mind has held the euphoria band —
    -- entry needs SUSTAIN in-band, not one lucky tick.
    local highSince = physiology.get(uid, "mental_high_since")
    if som >= mental.TUNE.EUPHORIC_ABOVE then
        if highSince == 0 then
            highSince = now
            physiology.commit(uid, { mental_high_since = now })
        end
    elseif highSince ~= 0 then
        highSince = 0
        physiology.commit(uid, { mental_high_since = 0 })
    end

    -- The stressed label, with its hysteresis band.
    if st == mental.STABLE and som < mental.TUNE.STRESSED_BELOW then
        if physiology.commit(uid, stateKeys({}, mental.STRESSED, now)) then
            st = mental.STRESSED
        end
    elseif st == mental.STRESSED and som >= mental.TUNE.STRESS_RECOVER_AT then
        if physiology.commit(uid, stateKeys({}, mental.STABLE, now)) then
            st = mental.STABLE
        end
    end

    -- Episode rolls: cooled down, physiologically lucid (below the
    -- delirious band the collapse/delirium machinery owns behaviour —
    -- no double-driving), sustained in-state, and on the roll cadence.
    if now < physiology.get(uid, "mental_cooldown_until") then return end
    if brain.isDelirious(uid) or brain.isUnconscious(uid) then return end
    if now < physiology.get(uid, "mental_next_roll_at") then return end
    physiology.commit(uid, { mental_next_roll_at = now + mental.TUNE.CHECK_INTERVAL })

    if st == mental.STRESSED then
        if now - physiology.get(uid, "mental_since") < mental.TUNE.SUSTAIN then
            return
        end
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
        since         = physiology.get(uid, "mental_since"),
        endsAt        = physiology.get(uid, "mental_until"),
        cooldownUntil = physiology.get(uid, "mental_cooldown_until"),
        stateOfMind   = brain.stateOfMind(uid),
    }
    if mental.isBreaking(uid) then s.behavior = mental.breakBehavior(uid) end
    return s
end

return mental
