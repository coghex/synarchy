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
--                        eroded by pain/exhaustion.
--
--   state_of_mind — the UNIFIED aggregate: min(consciousness, wellbeing),
--   the same "your worst problem wins" philosophy the original
--   consciousness formula used, extended one level up. It does NOT drive
--   the collapse/delirious/confused machinery (that stays physiological-
--   only, on purpose — a heartbroken-but-healthy unit shouldn't pass
--   out); it's the read-only summary value for logs, UI, and future
--   tie-ins (thoughts #351, mood-gated efficiency #353).
--
-- This is what makes a unit PASS OUT before a temperature/hypoxia/salt
-- death meter finishes — the "cooks in the desert, passes out, then
-- dies" arc — and, on the psychological side, what gives #351's thought
-- system a mood/emotional_pain substrate to read and move.

local stats = require("scripts.unit_stats")

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

-- ---- Concentration ----
-- Fully derived, no integration (same "no memory" philosophy as
-- consciousness) — consciousness gates it hard, pain and exhaustion
-- erode it but never zero it out on their own.
local CONCENTRATION_PAIN_WEIGHT  = 0.6
local CONCENTRATION_STAMINA_FLOOR = 0.4  -- factor at zero stamina

local function clamp(x, lo, hi) return math.max(lo, math.min(hi, x)) end

-- current/max fraction via stats.get (so a Lua-derived max, e.g.
-- max_stamina = endurance*10, resolves the same way tickResource does).
-- A unit with no such resource at all (bears have no hunger) reads as
-- neutral (1.0) rather than penalized.
local function fracOf(uid, curName, maxFromName)
    local maxVal = stats.get(uid, maxFromName)
    if not maxVal or maxVal <= 0 then return 1.0 end
    local cur = unit.getStat(uid, curName)
    if not cur then return 1.0 end
    return clamp(cur / maxVal, 0, 1)
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

-- Instantaneous consciousness from the physiological drivers (no
-- integration — the underlying stats already drift). min() = your worst
-- problem knocks you out. UNCHANGED from the original brain.lua.
local function computeConsciousness(uid)
    local core = unit.getStat(uid, "core_temp")   or 37.0
    local o2   = unit.getStat(uid, "blood_oxygen") or 1.0
    local conc = unit.getStat(uid, "salt_conc")    or 1.0

    -- Core temp: confusion when hypothermic (<34°C) or hyperthermic (>39.5°C).
    local tf
    if core < 34.0 then tf = clamp((core - 30.0) / 4.0, 0, 1)        -- 34→1, 30→0
    elseif core > 39.5 then tf = clamp((42.0 - core) / 2.5, 0, 1)    -- 39.5→1, 42→0
    else tf = 1.0 end

    -- Blood oxygen: <0.8 impairs, <0.5 unconscious.
    local of = clamp((o2 - 0.5) / 0.3, 0, 1)                         -- 0.8→1, 0.5→0

    -- Salt: deviation from ideal in either direction.
    local dev = math.abs(conc - 1.0)
    local sf  = clamp(1.0 - (dev - 0.25) / 0.35, 0, 1)               -- dev≤0.25→1, 0.6→0

    return math.min(tf, of, sf)
end

local function driftMood(uid, dt, consciousness, pain, hungerFrac, staminaFrac,
                          awarenessFrac)
    local target = clamp(1.0 - (MOOD_W_PAIN * pain
                               + MOOD_W_HUNGER * (1.0 - hungerFrac)
                               + MOOD_W_EXHAUSTION * (1.0 - staminaFrac)
                               + MOOD_W_CONSCIOUS * (1.0 - consciousness)
                               + MOOD_W_AWARENESS * (1.0 - awarenessFrac)),
                          0, 1)
    local prev = unit.getStat(uid, "mood")
    if prev == nil then return target end   -- first tick: seed at target, no fake drift-in
    return prev + (target - prev) * clamp(MOOD_DRIFT * dt, 0, 1)
end

local function driftEmotionalPain(uid, dt, pain)
    local prev = unit.getStat(uid, "emotional_pain") or 0.0
    if pain > prev then
        return prev + (pain - prev) * clamp(EMOTIONAL_PAIN_RISE * dt, 0, 1)
    else
        return math.max(0.0, prev - EMOTIONAL_PAIN_DECAY * dt)
    end
end

local function computeConcentration(consciousness, pain, staminaFrac)
    local staminaFactor = CONCENTRATION_STAMINA_FLOOR
                        + (1.0 - CONCENTRATION_STAMINA_FLOOR) * staminaFrac
    return clamp(consciousness * (1.0 - CONCENTRATION_PAIN_WEIGHT * pain)
                               * staminaFactor, 0, 1)
end

-- Unified aggregate: the worst of the physiological floor (consciousness)
-- and the psychological one (mood, dragged down by lingering emotional
-- pain). Read-only summary — nothing gates on this directly.
local function computeStateOfMind(consciousness, mood, emotionalPain)
    local wellbeing = clamp(mood - 0.5 * emotionalPain, 0, 1)
    return math.min(consciousness, wellbeing)
end

-- Recompute + store consciousness and the mental values (called each
-- physiology tick).
function brain.tick(uid, dt)
    local consciousness = computeConsciousness(uid)
    unit.setStat(uid, "consciousness", consciousness)

    local pain        = painFrac(uid)
    local hungerFrac   = fracOf(uid, "hunger", "max_hunger")
    local staminaFrac  = fracOf(uid, "stamina", "max_stamina")
    local awarenessFrac = awareness(uid)

    local mood = driftMood(uid, dt, consciousness, pain, hungerFrac, staminaFrac,
                            awarenessFrac)
    unit.setStat(uid, "mood", mood)

    local emotionalPain = driftEmotionalPain(uid, dt, pain)
    unit.setStat(uid, "emotional_pain", emotionalPain)

    local concentration = computeConcentration(consciousness, pain, staminaFrac)
    unit.setStat(uid, "concentration", concentration)

    unit.setStat(uid, "state_of_mind",
                 computeStateOfMind(consciousness, mood, emotionalPain))
end

function brain.consciousness(uid)
    return unit.getStat(uid, "consciousness") or 1.0
end

function brain.mood(uid)           return unit.getStat(uid, "mood") or 1.0 end
function brain.emotionalPain(uid)  return unit.getStat(uid, "emotional_pain") or 0.0 end
function brain.concentration(uid)  return unit.getStat(uid, "concentration") or 1.0 end
function brain.stateOfMind(uid)    return unit.getStat(uid, "state_of_mind") or 1.0 end

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
    }
end

return brain
