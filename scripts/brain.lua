-- State of mind — the unified consciousness + mood model. The sixth
-- homeostasis layer, now covering both axes of "what's going on in this
-- unit's head":
--
--   PHYSIOLOGICAL — the original CONSCIOUSNESS value (0..1, 1 = fully
--   alert) = the WORST of the body's derangements: core-temperature
--   extremes (hypothermic confusion / heat-stroke delirium), low blood
--   oxygen (drowsy → out), and salt imbalance (hypo/hypernatremic
--   confusion). Unchanged from the original brain.lua — every existing
--   consumer (the locomotor collapse↔crawl machine, the AI's delirious/
--   confused checks, the unit-info panel) keeps reading this exact value
--   with the exact same bands, so none of them regress:
--     ≥ 0.70  alert      — normal
--     ≥ 0.40  confused   — addled (Status condition; light)
--     ≥ 0.15  delirious  — can't act purposefully → stumbles/wanders (unit_ai)
--     <  0.15 unconscious → collapses (unit_resources), recovers at ≥ 0.40
--
--   PSYCHOLOGICAL — new mental values layered on top, persistent (they
--   drift rather than snap, so a single bad moment doesn't instantly
--   sour a unit's whole outlook):
--     mood            — 0..1 (1 = content), drifts toward a target set by
--                        current pain / hunger / exhaustion / consciousness
--                        / awareness (a unit that can barely perceive its
--                        surroundings feels less secure).
--     emotional_pain  — 0..1 (0 = none), rises quickly with physical pain
--                        but decays slowly — the ache that outlasts the
--                        wound.
--     concentration   — 0..1, instantaneous (no integration, same
--                        philosophy as consciousness): how well the unit
--                        can focus right now, gated by consciousness and
--                        eroded by pain/exhaustion, boosted by caffeine.
--     caffeine        — 0..1 (0 = none), a stimulant meter (#347):
--                        spiked by drinking a caffeinated consumable
--                        (scripts/consumable.lua), otherwise only
--                        decaying — there's no natural regen source.
--                        Feeds concentration ("alertness"); deliberately
--                        NOT folded into consciousness (its bands are
--                        load-bearing for collapse/delirious/confused).
--
--   state_of_mind — the UNIFIED aggregate: min(consciousness, wellbeing),
--   the same "your worst problem wins" philosophy the original
--   consciousness formula used, extended one level up. It does NOT drive
--   the collapse/delirious/confused machinery (that stays physiological-
--   only, on purpose — a heartbroken-but-healthy unit shouldn't pass
--   out) — but it is not inert either: #352's mental-state threshold
--   machine (scripts/mental_state.lua) reads it as a live behavioural
--   input — the stressed label and its hysteresis band, euphoria entry/
--   exit, and the break roll's severity are all thresholds on it.
--
--   DERIVED, NEVER STORED (#2838, S11a): consciousness, concentration and
--   state_of_mind are computed on demand by the shared pure Haskell rules
--   (Unit.Physiology.Mind), exposed through physiology.get, so this file
--   no longer holds an equation for them and no longer writes them. The
--   combat and craft threads evaluate the SAME rules against the same unit
--   snapshot, so a reader can never observe a value that lags the state it
--   was derived from. What this file still owns is the durable DRIFT: mood,
--   emotional_pain and caffeine, committed as ONE batch per pass.
--
--   A break episode has AI consequences: it preempts ordinary AI scoring
--   through the same short-circuit delirium uses (scripts/unit_ai_mental.lua),
--   then runs its own break behaviour — wander, flee, catatonia, or
--   lash-out — so the two branches short-circuit alike without acting alike.
--
--   Ordering (scripts/unit_resources.lua): brain.tick → thoughts.tick →
--   mental.tick. Each commits one atomic batch, and every read is on
--   demand: a thought's mood nudge reaches the state_of_mind mental.tick
--   reads in the SAME pass (there is no next-pass lag any more), and a
--   euphoric episode's concentration lift appears from the moment it begins
--   to the moment it ends.
--
--   brain.summary / mental.summary also expose the value, but only for
--   the debug console and the probes — nothing about logs or UI gates
--   on it. And the neighbouring systems read the SUBSTRATE rather than
--   the aggregate: #351's thoughts (scripts/thoughts.lua) read mood
--   plus the normalized pain / hunger / stamina fractions, the ambient
--   environment, and the mental state, and may write mood back;
--   #353's canonical multiplier
--   (Combat.Resolution.Common.mentalEffectiveness) reads the derived
--   concentration plus mental_state == 3 (EUPHORIC). Neither one reads
--   state_of_mind.
--
-- This is what makes a unit PASS OUT before a temperature/hypoxia/salt
-- death meter finishes — the "cooks in the desert, passes out, then
-- dies" arc — and, on the psychological side, what gives #351's thought
-- system a mood substrate to read and move.

local stats = require("scripts.unit_stats")
local physiology = require("scripts.physiology")

local brain = {}

-- ---- Consciousness bands (unchanged) ----
local CONFUSED_BELOW    = 0.70
local DELIRIOUS_BELOW   = 0.40
local UNCONSCIOUS_BELOW = 0.15   -- collapse trigger
local RISE_AT           = 0.40   -- revive gate (hysteresis vs collapse)

-- ---- Pain normalization ----
-- Mirrors Combat.Resolution.painCeiling (5.0) / unit.getPain's raw sum —
-- keep in lockstep if either changes.
local PAIN_CEILING = 5.0

-- ---- Mood ----
-- Target mood is 1.0 (content) minus weighted deficits; mood DRIFTS toward
-- that target rather than snapping to it, so mood reads as a persistent
-- background state rather than a jumpy instant. Slow on purpose — much
-- slower than the physiological drivers (cardio/thermo respond in
-- seconds), since a mood swing over seconds would read as manic, not
-- as a mental state.
local MOOD_DRIFT       = 0.02   -- per second, toward target
local MOOD_W_PAIN      = 0.45
local MOOD_W_HUNGER    = 0.20
local MOOD_W_EXHAUSTION = 0.20
local MOOD_W_CONSCIOUS = 0.35
local MOOD_W_AWARENESS = 0.15

-- ---- Awareness ----
-- Read from the same 'perception' stat unitAwareness (Unit.LineOfSight)
-- and vision (unitVisibleTiles) key off — the base sensory keenness that
-- both combat dodge-gating and FOV scale by. 1.0 is the reference every
-- species defaults to when perception is unset (see LineOfSight.hs); a
-- unit AT OR ABOVE that baseline reads as fully aware (no mood penalty —
-- keen senses don't inflate mood past content), only a below-baseline
-- unit (poor perception, or anything that suppresses it) reads as less
-- secure. unitAwareness's own bilateral (defender-vs-attacker) LOS/facing/
-- night-time terms are a per-encounter combat quantity, not a per-unit
-- background state, so they're deliberately NOT folded in here.
local AWARENESS_BASELINE = 1.0

-- ---- Emotional pain ----
-- Asymmetric drift, same "fill fast / recover slow" shape the failure
-- meters use: a fresh wound spikes emotional pain almost as fast as the
-- physical pain itself, but it lingers well after the wound is treated.
local EMOTIONAL_PAIN_RISE  = 0.5    -- per second, toward current pain
local EMOTIONAL_PAIN_DECAY = 0.02   -- per second, absent fresh pain

-- ---- Consciousness, concentration, state of mind ----
-- Derived on demand by the shared Haskell rules (Unit.Physiology.Mind:
-- consciousnessRule, concentrationRule, stateOfMindRule) and read through
-- physiology.get. Their constants live there, with the equations.

-- ---- Caffeine ----
-- Simple decay, no target/drift — coffee (scripts/consumable.lua) is
-- the only source, there's nothing to drift toward. ~1800s (30 real
-- minutes at the default time scale) to clear a fully-dosed meter.
local CAFFEINE_DECAY = 1.0 / 1800

local clamp = require("scripts.lib.numeric").clamp

-- current/max fraction via stats.get (so a Lua-derived max, e.g.
-- max_stamina = endurance*10, resolves the same way tickResource does).
-- A unit with no such resource at all (bears have no hunger) reads as
-- neutral (1.0) rather than penalized.
local function fracOf(uid, curName, maxFromName)
    local f = physiology.fraction(uid, curName, maxFromName)
    if not f then return 1.0 end
    return clamp(f, 0, 1)
end

-- 0..1, 0 = no pain. Mirrors Combat.Resolution.painFor's normalization
-- (unit.getPain returns the same raw, unclamped sum).
local function painFrac(uid)
    return clamp((unit.getPain(uid) or 0) / PAIN_CEILING, 0, 1)
end

-- 0..1, 1 = fully aware (perception at or above the species baseline),
-- 0 = perception has dropped to nothing. Deliberately a floor-only
-- read (no bonus above 1.0) — see the ---- Awareness ---- block above.
local function awareness(uid)
    local perception = unit.getStat(uid, "perception") or AWARENESS_BASELINE
    return clamp(perception / AWARENESS_BASELINE, 0, 1)
end

-- The mood the drift moves toward right now. A pure function of the
-- already-read inputs; the Haskell seed (Unit.Physiology.Mind.moodTargetRule)
-- uses the same weights, and a test pins the two equal.
local function moodTarget(consciousness, pain, hungerFrac, staminaFrac,
                          awarenessFrac)
    return clamp(1.0 - (MOOD_W_PAIN * pain
                       + MOOD_W_HUNGER * (1.0 - hungerFrac)
                       + MOOD_W_EXHAUSTION * (1.0 - staminaFrac)
                       + MOOD_W_CONSCIOUS * (1.0 - consciousness)
                       + MOOD_W_AWARENESS * (1.0 - awarenessFrac)),
                 0, 1)
end

local function driftMood(prev, dt, target)
    return prev + (target - prev) * clamp(MOOD_DRIFT * dt, 0, 1)
end

local function driftEmotionalPain(prev, dt, pain)
    if pain > prev then
        return prev + (pain - prev) * clamp(EMOTIONAL_PAIN_RISE * dt, 0, 1)
    else
        return math.max(0.0, prev - EMOTIONAL_PAIN_DECAY * dt)
    end
end

-- 0..1, only decaying — set by consumable.drink on a caffeinated sip.
local function driftCaffeine(prev, dt)
    return math.max(0.0, prev - CAFFEINE_DECAY * dt)
end

-- Drift the durable mental values one pass: mood, emotional_pain and caffeine,
-- committed as ONE atomic batch (#2838, S11a). A unit whose mind is not
-- Simulated has no such values (Nominal reads the healthy ones, Unsupported
-- nothing): it makes no write attempt and draws nothing. A refused batch is
-- reported by the facade and never raises, so it cannot skip the rest of the
-- unit's pass.
function brain.tick(uid, dt)
    if not physiology.isSimulated(uid, "mind") then return end

    local consciousness = brain.consciousness(uid)
    local pain          = painFrac(uid)
    local hungerFrac    = fracOf(uid, "hunger", "max_hunger")
    local staminaFrac   = fracOf(uid, "stamina", "max_stamina")
    local awarenessFrac = awareness(uid)

    local target = moodTarget(consciousness, pain, hungerFrac, staminaFrac,
                              awarenessFrac)
    physiology.commit(uid, {
        mood           = driftMood(physiology.get(uid, "mood"), dt, target),
        emotional_pain = driftEmotionalPain(physiology.get(uid, "emotional_pain"),
                                            dt, pain),
        caffeine       = driftCaffeine(physiology.get(uid, "caffeine"), dt),
    })
end

-- The mood-drift target for one unit right now, from the same live reads
-- brain.tick feeds the drift. Exposed for the probes and the Haskell/Lua
-- equality test; nothing stored.
function brain.moodTarget(uid)
    return moodTarget(brain.consciousness(uid), painFrac(uid),
                      fracOf(uid, "hunger", "max_hunger"),
                      fracOf(uid, "stamina", "max_stamina"), awareness(uid))
end

-- The three derived values, on demand (#2838): the mode-aware physiology read,
-- 1.0 for a unit whose system is Unsupported (a bodyless unit — the absent-key
-- neutral every consumer had). A violated precondition raises through the
-- read, like every other owned read.
function brain.consciousness(uid)  return physiology.get(uid, "consciousness") or 1.0 end
function brain.concentration(uid)  return physiology.get(uid, "concentration") or 1.0 end
function brain.stateOfMind(uid)    return physiology.get(uid, "state_of_mind") or 1.0 end

function brain.mood(uid)           return physiology.get(uid, "mood") or 1.0 end
function brain.emotionalPain(uid)  return physiology.get(uid, "emotional_pain") or 0.0 end
function brain.caffeine(uid)       return physiology.get(uid, "caffeine") or 0.0 end

-- Live reads of the same normalized inputs the mood target is built
-- from (0 = none/empty .. 1 = full/none, see painFrac/fracOf above) —
-- exposed so #351's thought triggers read the identical numbers mood
-- drifts toward, rather than re-deriving PAIN_CEILING etc. themselves.
function brain.painFrac(uid)       return painFrac(uid) end
function brain.hungerFrac(uid)     return fracOf(uid, "hunger", "max_hunger") end
function brain.staminaFrac(uid)    return fracOf(uid, "stamina", "max_stamina") end

-- Live read (not persisted — perception is already a stat, this is just
-- the same 0..1 normalization brain.tick feeds into the mood target).
function brain.awareness(uid)      return awareness(uid) end

-- Consciousness-gated behaviour — UNCHANGED semantics, still keyed on
-- consciousness alone (not the blended state_of_mind): the locomotor
-- collapse machine and the AI's delirious/confused checks must keep
-- firing exactly as before.
function brain.isUnconscious(uid)  return brain.consciousness(uid) < UNCONSCIOUS_BELOW end
function brain.canRise(uid)        return brain.consciousness(uid) >= RISE_AT end
function brain.isDelirious(uid)
    local c = brain.consciousness(uid)
    return c < DELIRIOUS_BELOW and c >= UNCONSCIOUS_BELOW
end
function brain.isConfused(uid)
    local c = brain.consciousness(uid)
    return c < CONFUSED_BELOW and c >= DELIRIOUS_BELOW
end

function brain.state(uid)
    local c = brain.consciousness(uid)
    if c < UNCONSCIOUS_BELOW then return "unconscious"
    elseif c < DELIRIOUS_BELOW then return "delirious"
    elseif c < CONFUSED_BELOW then return "confused"
    else return "alert" end
end

-- Everything in one table — the log, tests, and the debug console (table
-- returns auto-serialize to JSON) want this rather than five round trips.
function brain.summary(uid)
    return {
        consciousness = brain.consciousness(uid),
        mood          = brain.mood(uid),
        emotionalPain = brain.emotionalPain(uid),
        concentration = brain.concentration(uid),
        awareness     = brain.awareness(uid),
        stateOfMind   = brain.stateOfMind(uid),
        state         = brain.state(uid),
        caffeine      = brain.caffeine(uid),
    }
end

return brain
