-- Bear AI
--
-- Bear-specific behaviour layer. The acolyte's `unit_ai.lua` uses a
-- utility AI (each action scores itself, highest wins). Bears don't
-- need that complexity yet — they cycle through a small set of macro
-- states and pick the next one via weighted random transitions when a
-- timer expires. This file owns nothing the acolyte AI needs, and
-- vice versa.
--
-- Dispatch:
--   `unit_ai.update` walks every unit and calls `bearAi.tickOne(uid)`
--   for bears (and its own `tickOne` for acolytes). See the dispatch
--   block at the bottom of `scripts/unit_ai.lua`.
--
-- Singleton:
--   Per-uid AI state lives on the module table so it survives
--   `dofile`-style reloads (same `package.loaded` trick as
--   `unit_ai.lua` — see gotcha-dofile-module-state).
--
-- Save/load:
--   bear state is in-progress behaviour, not durable design — same
--   policy as unit_ai's per-uid state. Not registered with
--   saveModules; a fresh-session load reseeds bears to "standing-idle"
--   on the next tick.

local bearAi = package.loaded["scripts.bear_ai"] or {}
package.loaded["scripts.bear_ai"] = bearAi

bearAi.state = bearAi.state or {}

-----------------------------------------------------------
-- Tuning
-----------------------------------------------------------
local config = {
    -- Wander geometry
    wander_radius = 8.0,       -- tiles from anchor
    wander_speed  = 1.0,       -- tiles / second
    anchor_drift_chance = 0.25,-- chance the anchor moves to the bear's
                               -- current position on each wander pick.
                               -- Keeps bears from being rooted forever
                               -- to their spawn point.

    -- Duration of each activity, in seconds. Randomised in [min, max].
    wander_dur_min = 5,  wander_dur_max = 14,
    idle_dur_min   = 3,  idle_dur_max   = 8,
    alert_dur_min  = 3,  alert_dur_max  = 7,
    sit_dur_min    = 8,  sit_dur_max    = 20,
    lie_dur_min    = 15, lie_dur_max    = 35,
    sleep_dur_min  = 30, sleep_dur_max  = 90,

    -- One-shot transition durations. Must roughly match the animation
    -- length (frames / fps) — too short and the engine cuts the anim
    -- short; too long and the bear holds the last frame. Bear anims
    -- run at 8-12 fps with 9 frames → ~0.75-1.1s; round to 1.0.
    transition_dur = 1.0,
}

-- Weighted random transitions out of each posture/activity. Higher
-- weights = more frequent.
--
--   From standing-active states (wandering / idle), the bear may:
--     wander, idle, alert (stand_on_hind), sit (drop posture).
--   From posed states it can only stay or step toward the next pose
--   in the chain — sit → lie → sleep — or step back up.
--
-- Weights need not sum to 1; we normalise at pick time.
local transitions = {
    standing_active = {
        { name = "wander",     w = 5.0 },
        { name = "idle",       w = 2.0 },
        { name = "alert",      w = 1.5 },  -- stand_on_hind look-around
        { name = "sit",        w = 1.0 },
    },
    alert = {
        -- Always drop back to all fours when alert timer expires
        { name = "stand_down", w = 1.0 },
    },
    sitting = {
        { name = "stand_up",   w = 1.5 },
        { name = "sit_idle",   w = 2.0 },  -- keep sitting longer
        { name = "lie",        w = 2.0 },
    },
    lying = {
        { name = "sit_up",     w = 1.0 },
        { name = "lie_idle",   w = 2.0 },
        { name = "sleep",      w = 3.0 },
    },
    sleeping = {
        -- Always wake to lying first; the next tick may go sit/stand.
        { name = "wake",       w = 1.0 },
    },
}

local function weightedPick(table_of_options)
    local total = 0
    for _, opt in ipairs(table_of_options) do total = total + opt.w end
    local r = math.random() * total
    local acc = 0
    for _, opt in ipairs(table_of_options) do
        acc = acc + opt.w
        if r <= acc then return opt.name end
    end
    return table_of_options[#table_of_options].name
end

local function randRange(lo, hi) return lo + math.random() * (hi - lo) end

-----------------------------------------------------------
-- Per-bear state
-----------------------------------------------------------
-- s.posture  ∈ "standing" | "sitting" | "lying" | "sleeping" | "stand_on_hind"
-- s.activity ∈ "wander" | "idle" | "alert" | "rest" | "transitioning"
-- s.stateUntil :: gameTime to switch activity / finish transition
-- s.afterTransition :: function or nil — fires when the current
--                       transition completes
-- s.anchor :: {x, y} — wander focal point
-- s.wanderTarget :: {x, y} or nil — current wander destination

local function ensureState(uid)
    local s = bearAi.state[uid]
    if s then return s end
    -- unit.getPos returns x, y, z as three numbers (Lua-multireturn),
    -- not a table — failing to unpack used to error with "attempt to
    -- index a number value".
    local px, py = unit.getPos(uid)
    s = {
        posture     = "standing",
        activity    = "idle",
        stateUntil  = engine.gameTime() + 0.5,
        anchor      = { x = px or 0, y = py or 0 },
        wanderTarget = nil,
    }
    -- Make sure the bear starts in the idle animation rather than the
    -- engine's T-pose fallback.
    unit.setAnim(uid, "idle")
    bearAi.state[uid] = s
    return s
end

local function pickWanderTarget(s)
    local angle = math.random() * 2 * math.pi
    local r     = math.sqrt(math.random()) * config.wander_radius
    return {
        x = s.anchor.x + math.cos(angle) * r,
        y = s.anchor.y + math.sin(angle) * r,
    }
end

-----------------------------------------------------------
-- Activity starters. Each sets the animation, the new activity /
-- posture, the timer, and optionally an after-transition hook to run
-- once the transition animation has finished.
-----------------------------------------------------------
local startActivity  -- forward declaration

local function holdFor(s, secs, afterFn)
    s.stateUntil = engine.gameTime() + secs
    s.afterTransition = afterFn
end

startActivity = function(uid, s, name)
    local now = engine.gameTime()
    if name == "wander" then
        local t = pickWanderTarget(s)
        s.wanderTarget = t
        unit.moveTo(uid, t.x, t.y, config.wander_speed)
        s.activity = "wander"
        s.stateUntil = now + randRange(config.wander_dur_min,
                                        config.wander_dur_max)
        -- Each new wander leg has a chance to drift the anchor to the
        -- bear's current position, letting it roam over time.
        if math.random() < config.anchor_drift_chance then
            local px, py = unit.getPos(uid)
            if px then s.anchor = { x = px, y = py } end
        end

    elseif name == "idle" then
        unit.setAnim(uid, "idle")
        s.activity = "idle"
        s.stateUntil = now + randRange(config.idle_dur_min,
                                        config.idle_dur_max)

    elseif name == "alert" then
        unit.setAnim(uid, "stand_on_hind")  -- transition: rise up
        s.activity = "transitioning"
        holdFor(s, config.transition_dur, function()
            unit.setAnim(uid, "stand_on_hind_idle")
            s.posture  = "stand_on_hind"
            s.activity = "alert"
            s.stateUntil = engine.gameTime() + randRange(
                config.alert_dur_min, config.alert_dur_max)
        end)

    elseif name == "stand_down" then
        -- stand_on_hind → standing. No dedicated reverse asset; snap
        -- straight to idle for now. Polish later with a reverse-play
        -- when we wire engine support for it.
        unit.setAnim(uid, "idle")
        s.posture  = "standing"
        s.activity = "idle"
        s.stateUntil = now + 0.5

    elseif name == "sit" then
        unit.setAnim(uid, "standing_to_sitting")
        s.activity = "transitioning"
        holdFor(s, config.transition_dur, function()
            unit.setAnim(uid, "sitting_idle")
            s.posture  = "sitting"
            s.activity = "sit_idle"
            s.stateUntil = engine.gameTime() + randRange(
                config.sit_dur_min, config.sit_dur_max)
        end)

    elseif name == "stand_up" then
        -- sitting → standing. Same snap-to-idle policy as stand_down.
        unit.setAnim(uid, "idle")
        s.posture  = "standing"
        s.activity = "idle"
        s.stateUntil = now + 0.5

    elseif name == "sit_idle" then
        unit.setAnim(uid, "sitting_idle")
        s.activity = "sit_idle"
        s.stateUntil = now + randRange(config.sit_dur_min,
                                        config.sit_dur_max)

    elseif name == "lie" then
        unit.setAnim(uid, "sitting_to_lying_down")
        s.activity = "transitioning"
        holdFor(s, config.transition_dur, function()
            unit.setAnim(uid, "lying_down_idle")
            s.posture  = "lying"
            s.activity = "lie_idle"
            s.stateUntil = engine.gameTime() + randRange(
                config.lie_dur_min, config.lie_dur_max)
        end)

    elseif name == "sit_up" then
        -- lying → sitting. Snap.
        unit.setAnim(uid, "sitting_idle")
        s.posture  = "sitting"
        s.activity = "sit_idle"
        s.stateUntil = now + 1.0

    elseif name == "lie_idle" then
        unit.setAnim(uid, "lying_down_idle")
        s.activity = "lie_idle"
        s.stateUntil = now + randRange(config.lie_dur_min,
                                        config.lie_dur_max)

    elseif name == "sleep" then
        unit.setAnim(uid, "lying_down_to_sleeping")
        s.activity = "transitioning"
        holdFor(s, config.transition_dur, function()
            unit.setAnim(uid, "sleeping_idle")
            s.posture  = "sleeping"
            s.activity = "sleep"
            s.stateUntil = engine.gameTime() + randRange(
                config.sleep_dur_min, config.sleep_dur_max)
        end)

    elseif name == "wake" then
        -- sleeping → lying. Snap.
        unit.setAnim(uid, "lying_down_idle")
        s.posture  = "lying"
        s.activity = "lie_idle"
        s.stateUntil = now + 1.0

    else
        -- Unknown — fall back to idle so the bear doesn't get stuck.
        unit.setAnim(uid, "idle")
        s.activity = "idle"
        s.stateUntil = now + 1.0
    end
end

-- Resolve which transitions table applies to the bear's current state.
local function transitionsFor(s)
    if s.posture == "standing" then return transitions.standing_active end
    if s.posture == "stand_on_hind" then return transitions.alert end
    if s.posture == "sitting" then return transitions.sitting end
    if s.posture == "lying"   then return transitions.lying end
    if s.posture == "sleeping" then return transitions.sleeping end
    return transitions.standing_active
end

-----------------------------------------------------------
-- Per-tick: called by `unit_ai.update` for each bear.
-----------------------------------------------------------
function bearAi.tickOne(uid)
    local s = ensureState(uid)
    local now = engine.gameTime()
    if now < s.stateUntil then return end

    -- Activity / transition timer expired. If a transition-complete
    -- hook is pending, run it now; that's what installs the next
    -- posture and timer.
    if s.afterTransition then
        local cb = s.afterTransition
        s.afterTransition = nil
        cb()
        return
    end

    -- Otherwise pick a new activity from the current posture's table.
    local next_activity = weightedPick(transitionsFor(s))
    startActivity(uid, s, next_activity)
end

-----------------------------------------------------------
-- Hooks called by the engine lifecycle
-----------------------------------------------------------
function bearAi.shutdown()
    for k in pairs(bearAi.state) do bearAi.state[k] = nil end
    engine.logInfo("Bear AI shut down")
end

return bearAi
