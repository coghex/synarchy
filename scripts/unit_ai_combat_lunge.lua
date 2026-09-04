-- Unit AI lunge state machine (#1713, split out of
-- unit_ai_combat_attack.lua).
--
-- A lunge is a short-reach attacker's committed leap: it jumps to land
-- ADJACENT to a target it cannot otherwise reach, then strikes on
-- arrival with a reach BONUS equal to the leap's strike-reach — so the
-- engine's height-gated part picker can select the now-reachable high
-- parts (neck/throat) — plus the leap's impact speed, which folds the
-- full-body momentum into the damage.
--
-- WHY THIS IS ITS OWN MODULE. The sequence spans several ticks and, for
-- most of them, the unit is AIRBORNE — `TransitioningTo Falling`, which
-- `activityLabel` (src/Unit/Thread.hs) reports to Lua as
-- `activity == "transitioning"`. `unit_ai.lua`'s dispatcher returns from
-- `tickOne` on exactly that activity so an AI command cannot clobber a
-- pose transition, which means `attackTargetExecute` — and therefore
-- `tryLunge` — never runs while the leap is in the air. Before #1713 the
-- phase-2 gate was consequently unreachable: `lungeSawAir` could never
-- become true, no landing strike ever fired, and the phase cleared only
-- through its own timeout.
--
-- The fix is `observeTick` below, which the dispatcher calls on the
-- suppressed paths BEFORE it returns. It is deliberately a pure
-- BOOKKEEPING observer: it reads pose/activity and writes this module's
-- own `lunge*` fields on the AI state, and it does nothing else. It
-- scores no actions, issues no unit commands, and never calls
-- `combat.attack`. The strike itself still fires only from `tryLunge`
-- phase 2, on an ordinary grounded, non-transitioning execute tick — so
-- the dispatcher's clobber protection is untouched for every behaviour
-- including this one.
--
-- BOOKKEEPING IS SEVEN FIELDS, cleared together. `lungePhase`,
-- `lungeSawAir`, `lungeStartAt`, `lungeTarget`, `lungeMode`,
-- `lungeReach` and `lungeImpactSpeed` are set as a set by a launch and
-- cleared as a set by `clear` on EVERY terminal path — strike, timeout,
-- invalidated or replaced target, abandoned goal, collapse/death,
-- retreat, mental-episode end. That is not tidiness: `lungeTarget` is a
-- PERSISTED typed reference (scripts/unit_ai_ref_schema.lua), so a save
-- taken between a dead lunge and the next attempt would otherwise carry
-- a stale unit reference into the integrity graph.

local core = require("scripts.unit_ai_core")
local combatCore = require("scripts.unit_ai_combat")
local computeAttackCooldown = combatCore.computeAttackCooldown

local M = {}

-- Only very-short-reach attackers lunge (slice 2a: the squirrel, reach
-- ~0.11). Normal melee units (reach ≥ this) just close and swing. The
-- general "skilled/unintelligent fighters also lunge" gating is later (2e).
-- Abort a lunge that never resolves (e.g. interrupted mid-air) after this.
local LUNGE_TIMEOUT_SEC = 3.0

-- ----- Lunge decision (2e: rarity, split by intelligence + skill) -----
-- A lunge is a deliberate, committed move — not something a unit does every
-- time it's out of reach. WHO lunges, and how readily, depends on the mind:
--   * Unintelligent creatures (intelligence < LUNGE_INSTINCT_INTEL — squirrels,
--     bears, mules) lunge on INSTINCT. For a short-reach predator/prey it's
--     often the ONLY way to reach a tall target, so the propensity is high;
--     the leap's REACH is still naturally bounded by the jumping skill (the
--     engine's getJumpReach), so a clumsy animal simply can't leap far.
--   * Intelligent fighters (acolytes) treat the lunge as a trained TECHNIQUE,
--     gated by the `jumping` skill — a novice (skill 10) almost never lunges;
--     a skilled one occasionally does to close a gap. Otherwise they walk in
--     and fight normally.
-- Either way it costs commitment, so it's gated on stamina.
local LUNGE_INSTINCT_INTEL    = 0.7   -- below = instinct regime (animals)
local LUNGE_INSTINCT_P        = 0.85  -- animal propensity when out of reach
local LUNGE_TECH_MAX_P        = 0.5   -- ceiling for a trained lunger
local LUNGE_TECH_SKILL_K      = 0.6   -- jumping/100 × this = technique chance
local LUNGE_MIN_STAMINA_FRAC  = 0.25  -- too winded to commit below this

M.LUNGE_TIMEOUT_SEC = LUNGE_TIMEOUT_SEC

-- The complete bookkeeping set, named once so `clear` and every consumer
-- that checks "is anything left over?" agree by construction.
local LUNGE_FIELDS = {
    "lungePhase", "lungeSawAir", "lungeStartAt",
    "lungeTarget", "lungeMode", "lungeReach", "lungeImpactSpeed",
}
M.LUNGE_FIELDS = LUNGE_FIELDS

-- Drop every lunge field. Safe on a state with no lunge in flight, so a
-- terminal path can call it unconditionally rather than testing first.
function M.clear(s)
    if not s then return end
    for _, field in ipairs(LUNGE_FIELDS) do s[field] = nil end
end

-- Is a lunge in flight for this state?
local function inFlight(s)
    return s ~= nil and s.lungePhase == "air"
end

local function timedOut(s, now)
    return (now - (s.lungeStartAt or now)) > LUNGE_TIMEOUT_SEC
end

-- Record that the leap is genuinely airborne.
--
-- This is the ONLY writer of lungeSawAir, and it demands an observed
-- `falling` pose. `unit.jump` returning true proves only that the
-- command was ENQUEUED (Engine.Scripting.Lua.API.Units.Spawn documents
-- this; Unit.Thread.Command.Motion can still reject it), so a launch
-- that never leaves the ground never sets this flag and therefore never
-- reaches the strike — it ages out through the timeout instead.
local function observeAir(uid, s)
    if unit.getPose(uid) == "falling" then s.lungeSawAir = true end
end

-- Dispatcher hook. Called from unit_ai.lua's tickOne on EVERY tick,
-- before the short-circuit returns — the whole point being that a leap
-- spends its airborne life on one of the paths those returns take.
-- STRICTLY bookkeeping: it reads pose/activity, writes this module's own
-- lunge fields, returns nothing and commands nothing, so it widens no
-- execution window. Cheap on the overwhelming majority of ticks: one
-- table lookup and an early return when no lunge is in flight.
--
-- Three jobs, in order:
--   * a lunge whose actor died or collapsed is over — clear it, since
--     no execute tick will ever run again to notice;
--   * a lunge that has outlived LUNGE_TIMEOUT_SEC is over — clear it
--     here too, so an interruption that parks the unit on a suppressed
--     activity (drinking/eating/pickup, or a transition that never ends
--     in a landing) cannot strand the persisted lungeTarget reference;
--   * otherwise, if the unit is airborne, remember it.
function M.observeTick(uid, pose, activity)
    local s = core.aiState[uid]
    if not inFlight(s) then return end
    if pose == "dead" or pose == "collapsed" then
        M.clear(s)
        return
    end
    if timedOut(s, engine.gameTime()) then
        M.clear(s)
        return
    end
    if activity == "transitioning" then observeAir(uid, s) end
end

-- Decide whether an out-of-reach unit commits to a lunge THIS attempt.
-- Rolled only when already eligible (out of reach, off cooldown), so the
-- attack cooldown spaces the rolls — a "no" just means the unit pursues on
-- foot this cycle and may roll again next time it's off cooldown.
local function shouldLunge(uid, s)
    -- Stamina gate — a leap is a big spend.
    local stam = unit.getStat(uid, "stamina")
    if stam then
        local maxStam = require("scripts.unit_stats").get(uid, "max_stamina")
        if maxStam and maxStam > 0 and stam / maxStam < LUNGE_MIN_STAMINA_FRAC then
            return false
        end
    end
    local intel = unit.getStat(uid, "intelligence") or 1.0
    local p
    if intel < LUNGE_INSTINCT_INTEL then
        p = LUNGE_INSTINCT_P                       -- instinct: readily
    else
        local jumping = unit.getSkill(uid, "jumping") or 0.0
        p = math.min(LUNGE_TECH_MAX_P, (jumping / 100.0) * LUNGE_TECH_SKILL_K)
    end
    return math.random() < p
end

-- Multi-tick: issue the leap, wait for the airborne→land transition,
-- then strike. Returns true if it handled this tick (caller skips normal
-- attack/pursue).
--
-- The reach this passes to `combat.attack` is ADVISORY at admission and
-- LOAD-BEARING at commit (#2328): Combat.Resolution.Admission validates
-- the landing strike's horizontal separation against the attacker's
-- `unit.getAttackRange` PLUS this stored reach bonus, so a lunge that
-- declares its extended strike still lands while a stale one does not.
-- A lunge that stopped declaring its reach would be refused as out of
-- reach — the short-reach species this path exists for are far inside
-- their base range only after the leap's bonus is counted.
function M.tryLunge(uid, s, target, me, you, chebyshev)
    local range = unit.getAttackRange(uid) or 1.0
    local now = engine.gameTime()

    -- Phase 2: airborne — wait until we've actually left the ground and
    -- come back down, then deliver the strike.
    if s.lungePhase == "air" then
        if timedOut(s, now) then
            M.clear(s); return false                -- bail; resume normal logic
        end
        -- The strike belongs to the unit this lunge LAUNCHED at, and to
        -- no other. `target` is the caller's mutable local: the mid-fight
        -- retaliation swap can replace it, and the goal can be handed a
        -- different unit outright. Applying this leap's stored reach and
        -- impact speed to a substitute would be a free momentum strike on
        -- a unit we never jumped at, so a replaced, vanished or dead
        -- launch target cancels the lunge instead.
        local launched = s.lungeTarget
        if launched ~= target or not launched or not unit.exists(launched)
           or unit.getPose(launched) == "dead" then
            M.clear(s); return false
        end
        observeAir(uid, s)
        if s.lungeSawAir and unit.getPose(uid) == "standing"
           and unit.getActivity(uid) ~= "transitioning" then
            unit.setAnimOverride(uid, "attack_quick")
            -- reach bonus lets the strike hit a high part; impact speed
            -- folds the leap's full-body momentum into the damage.
            combat.attack(uid, launched, s.lungeMode or "quick",
                          s.lungeReach or 0, s.lungeImpactSpeed or 0)
            s.attackSwingUntil  = now + (unit.getAnimDuration(uid, "attack_quick") or 0.4)
            s.attackLastSwingAt = now
            s.attackLastMode    = s.lungeMode or "quick"
            M.clear(s)
        end
        return true   -- consume the tick while leaping / landing
    end

    -- Phase 1: decide to leap. Must be out of melee reach, off cooldown,
    -- and the mind/skill check (2e) must elect to commit — otherwise fall
    -- through to normal pursue (walk closer and fight).
    if chebyshev <= range then return false end
    local last = s.attackLastSwingAt or 0
    if now - last < computeAttackCooldown(uid, s.attackLastMode or "quick") then
        return false
    end
    if not shouldLunge(uid, s) then return false end
    local jr = unit.getJumpReach(uid)
    if not jr or not jr.dist or jr.dist <= 0 then return false end

    -- Land one tile from the target, on our side. getInfo gridX/Y are
    -- CONTINUOUS positions, so floor to integer TILE coords (unit.jump
    -- needs integers, or its tointeger silently rejects the command).
    local mtx, mty = math.floor(me.gridX),  math.floor(me.gridY)
    local ttx, tty = math.floor(you.gridX), math.floor(you.gridY)
    local sgx = (mtx > ttx and 1) or (mtx < ttx and -1) or 0
    local sgy = (mty > tty and 1) or (mty < tty and -1) or 0
    local landX, landY = ttx + sgx, tty + sgy
    local ldx, ldy = landX - mtx, landY - mty
    local d = math.sqrt(ldx * ldx + ldy * ldy)

    if d < 0.5 then
        -- Already adjacent: a vertical pounce in place — full strike-reach,
        -- no horizontal leap (which the engine would refuse at d≈0).
        unit.setAnimOverride(uid, "attack_quick")
        combat.attack(uid, target, "quick", jr.height or 0,
                      unit.lungeImpactSpeed(uid, 0))
        s.attackSwingUntil  = now + (unit.getAnimDuration(uid, "attack_quick") or 0.4)
        s.attackLastSwingAt = now
        s.attackLastMode    = "quick"
        return true
    end
    if d > jr.dist then return false end

    -- Strike-reach envelope at this leap distance.
    local frac  = d / jr.dist
    local reach = (jr.height or 0) * (1 - frac * frac)
    if unit.jump(uid, landX, landY) then
        s.lungePhase        = "air"
        s.lungeSawAir       = false
        s.lungeStartAt      = now
        s.lungeTarget       = target
        s.lungeMode         = "quick"
        s.lungeReach        = reach
        s.lungeImpactSpeed  = unit.lungeImpactSpeed(uid, d)
    end
    return true
end

return M
