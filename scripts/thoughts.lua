-- Thought system (#351) — periodic, data-driven per-unit thoughts, the
-- second half of the State-of-mind epic #349/#350 set up: brain.lua gave
-- every unit a mood/emotional_pain substrate; this module reads it (and
-- the physiology/environment underneath it) to pick a thought, then
-- nudges mood back — the two-way loop the issue calls for.
--
-- Catalogue: data/thoughts.yaml (engine.loadYaml, loaded lazily on first
-- tick — same pattern as scripts/structures.lua's pack cache). Each entry:
-- id, category ("state"|"environmental"|"random", a display tag only),
-- trigger (a TRIGGERS predicate name below; absent = always eligible —
-- every "random" entry), valence ("positive"|"negative"|"neutral", biases
-- pick weight by current mood), weight, mood_delta, text.
--
-- Cadence: each unit rolls at most one thought every THOUGHT_INTERVAL_MIN
-- .. THOUGHT_INTERVAL_MAX game-seconds, tracked via a "thought_next_at"
-- uiStat (an absolute engine.gameTime() deadline — same style as
-- combat_log's battle-quiescence check, not a decrementing countdown).
--
-- Ticked from unit_resources.update, right after brain.tick (so a thought
-- reads this tick's freshly-drifted mood/pain, and its mood_delta lands
-- in time for next tick's brain.tick to treat it as "prev" and drift it
-- back toward the physiological target — the fade-over-time a moodlet
-- wants, for free, with no separate stacking/decay system).
--
-- A thought is ONE atomic event (#2838, S11a): the new deadline and the mood
-- nudge are committed as a single batch (a thought with no nudge commits the
-- deadline alone), and its log entry is emitted only after the batch
-- committed. Only a unit whose MIND is Simulated thinks: a Nominal or
-- Unsupported mind makes no attempt, draws no random number and records no
-- thought. The thought clock is seeded to `now` at spawn, at the Nominal to
-- Simulated transition and by absent-only load repair (so a freshly seeded
-- unit's first thought is eligible on its first pass), and the mind
-- mode-change hook below re-rolls it to `now + random * THOUGHT_INTERVAL_MIN`
-- on the gameplay stream, which keeps the population desynchronized.

local brain      = require("scripts.brain")
local mental     = require("scripts.mental_state")
local physiology = require("scripts.physiology")

local thoughts = package.loaded["scripts.thoughts"] or {}
package.loaded["scripts.thoughts"] = thoughts

local THOUGHT_INTERVAL_MIN = 30.0   -- game-seconds
local THOUGHT_INTERVAL_MAX = 90.0

-- How far a valenced entry's weight can swing with mood. Never fully
-- zeroes a category out (a miserable unit can still have a bright
-- thought occasionally, just rarely).
local MOOD_BIAS_MIN = 0.25
local MOOD_BIAS_MAX = 2.0

thoughts.catalogue = thoughts.catalogue or nil

local clamp = require("scripts.lib.numeric").clamp

-- Trigger predicates, referenced by name from data/thoughts.yaml's
-- `trigger` field. `ctx` is built fresh per unit per tick by
-- buildContext() below. Exposed (not local) so the probe / debug
-- console can exercise a predicate in isolation without a live extreme
-- climate tile.
thoughts.TRIGGERS = {
    pain_high = function(ctx) return ctx.pain > 0.5 end,
    hungry    = function(ctx) return ctx.hungerFrac < 0.3 end,
    exhausted = function(ctx) return ctx.staminaFrac < 0.3 end,
    content   = function(ctx) return ctx.pain < 0.1 and ctx.hungerFrac > 0.7
                                   and ctx.staminaFrac > 0.7 end,
    cold      = function(ctx) return ctx.ambient ~= nil and ctx.ambient < 0.0 end,
    hot       = function(ctx) return ctx.ambient ~= nil and ctx.ambient > 32.0 end,
    -- sunAngle: 0.0/1.0 = midnight, 0.25 = dawn, 0.5 = noon, 0.75 = dusk
    -- (World.Time.Types.worldTimeToSunAngle) — night wraps around 0.
    night     = function(ctx) return ctx.sunAngle ~= nil
                                   and (ctx.sunAngle < 0.25 or ctx.sunAngle > 0.75) end,
    day       = function(ctx) return ctx.sunAngle ~= nil
                                   and (ctx.sunAngle >= 0.25 and ctx.sunAngle <= 0.75) end,
    -- Mental states (#352, scripts/mental_state.lua). A break counts
    -- as stressed — the misery narration keeps running through it.
    stressed  = function(ctx) return ctx.mentalState == "stressed"
                                   or ctx.mentalState == "break" end,
    euphoric  = function(ctx) return ctx.mentalState == "euphoric" end,
}

function thoughts.loadCatalogue(path)
    path = path or "data/thoughts.yaml"
    local doc = engine.loadYaml(path)
    thoughts.catalogue = (doc and doc.thoughts) or {}
    return #thoughts.catalogue
end

-- Everything a trigger predicate might read, snapshotted once per unit
-- per tick. `info` is unit.getInfo(uid) (for gridX/gridY); ambient/
-- sunAngle stay nil off-world or with no active climate, so triggers
-- that need them simply can't fire (not an error).
local function buildContext(uid, info)
    local ctx = {
        pain        = brain.painFrac(uid),
        hungerFrac  = brain.hungerFrac(uid),
        staminaFrac = brain.staminaFrac(uid),
        mood        = brain.mood(uid),
        mentalState = mental.state(uid),
    }
    if info and info.gridX and info.gridY then
        ctx.ambient  = world.getAmbientAt  and world.getAmbientAt(info.gridX, info.gridY)
        ctx.sunAngle = world.getSunAngleAt and world.getSunAngleAt(info.gridX, info.gridY)
    end
    return ctx
end

-- valence → mood-bias factor. Negative entries get heavier as mood
-- drops below 0.5; positive entries get heavier as it rises above 0.5;
-- neutral is unaffected. This is the "state of mind biases which
-- thoughts arise" half of the two-way loop.
local function moodBiasFactor(valence, mood)
    if valence == "negative" then
        return clamp(1.0 + (0.5 - mood) * 2.0, MOOD_BIAS_MIN, MOOD_BIAS_MAX)
    elseif valence == "positive" then
        return clamp(1.0 + (mood - 0.5) * 2.0, MOOD_BIAS_MIN, MOOD_BIAS_MAX)
    end
    return 1.0
end

local function eligibleEntries(ctx)
    local out = {}
    for _, t in ipairs(thoughts.catalogue or {}) do
        local trig = t.trigger
        if trig == nil or (thoughts.TRIGGERS[trig] and thoughts.TRIGGERS[trig](ctx)) then
            out[#out + 1] = t
        end
    end
    return out
end

local function pickWeighted(entries, mood)
    local total, weights = 0, {}
    for i, t in ipairs(entries) do
        local w = (t.weight or 1) * moodBiasFactor(t.valence, mood)
        weights[i] = w
        total = total + w
    end
    if total <= 0 then return nil end
    local roll, acc = math.random() * total, 0
    for i, t in ipairs(entries) do
        acc = acc + weights[i]
        if roll <= acc then return t end
    end
    return entries[#entries]
end

-- Advance one unit's thought clock; fires + applies a thought when due.
-- No-op (cheaply) for species/units with nothing eligible.
function thoughts.tick(uid, info, dt)
    -- Mode first: no catalogue load, read or random draw for a unit with no
    -- Simulated mind.
    if not physiology.isSimulated(uid, "mind") then return end
    if not thoughts.catalogue then thoughts.loadCatalogue() end
    if not thoughts.catalogue or #thoughts.catalogue == 0 then return end

    local now = engine.gameTime()
    if now < physiology.get(uid, "thought_next_at") then return end
    local nextAt = now + THOUGHT_INTERVAL_MIN
                 + math.random() * (THOUGHT_INTERVAL_MAX - THOUGHT_INTERVAL_MIN)

    local ctx = buildContext(uid, info)
    local entry = pickWeighted(eligibleEntries(ctx), ctx.mood)

    -- ONE event: the deadline, with the mood nudge when the thought has one.
    local batch = { thought_next_at = nextAt }
    if entry and entry.mood_delta and entry.mood_delta ~= 0 then
        batch.mood = clamp(ctx.mood + entry.mood_delta, 0, 1)
    end
    if not physiology.commit(uid, batch) then return end
    if entry then
        thought.emit(uid, entry.text, entry.category or "random")
    end
end

-- A mind that BECOMES Simulated has its thought clock seeded to `now`; this
-- re-rolls it to `now + random * THOUGHT_INTERVAL_MIN` on the gameplay stream
-- so a population switched together does not think in lockstep. Only an
-- EFFECTIVE change runs it (the facade's hook contract); a unit that turns
-- Nominal or Unsupported holds no clock to re-roll.
physiology.onModeChanged("thought_clock", "mind", function(uid, _system, _from, to)
    if to ~= "simulated" then return end
    physiology.commit(uid, {
        thought_next_at = engine.gameTime() + math.random() * THOUGHT_INTERVAL_MIN,
    })
end)

return thoughts
