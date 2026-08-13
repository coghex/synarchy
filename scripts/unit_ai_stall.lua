-- Order stall accounting for player-issued orders (#1291).
--
-- Both order timers -- TASK_TIMEOUT_SEC for a commanded move (below)
-- and unit_ai_pickup.lua's pickup_timeout for a commanded pickup --
-- are STALL timers, not total-trip budgets (#920): they answer "is
-- this unit getting anywhere?", never "how long has this trip taken".
--
-- What they measure is ELIGIBLE time. The #306 ladder deliberately
-- outranks a commanded move with eating, drinking, a dry-canteen
-- refill, treat_ally and combat, and forage/refill_canteen walk the
-- unit AWAY from its target so it can never record a closer approach
-- while they run. Charging those intervals dropped orders the ladder
-- had only meant to postpone, contradicting unit_ai_combat.lua's own
-- contract ("commandedTask persists until maintainTask clears it on
-- arrival/timeout, so the unit resumes the move once the higher-
-- priority action finishes"). So an interruption of any length now
-- costs the order nothing.
--
-- The budget still accumulates ACROSS interruptions rather than being
-- restarted by them: time charged before an interruption stays
-- charged, the interruption itself adds nothing, and what is left of
-- the budget is what expires afterwards. Only a new closest approach
-- starts it over (M.reset), exactly as before -- so an order the unit
-- IS free to pursue and makes no headway on still gives up on
-- schedule, and no order becomes immortal.
--
-- An interval is eligible when the order's own action is the one
-- arbitration left in control of the unit. Callers pass that as
-- `s.currentAction`, read BEFORE this tick re-scores, so it describes
-- the interval that just elapsed rather than the one about to start:
-- ordinary scheduling cadence is eligible, an interval another action
-- won is not.
--
-- Intervals in which the AI could not run at all -- unit_ai.lua's
-- collapsed-pose and engine-animation short-circuit, the mental-break
-- / delirium short-circuit, a save/load boundary -- produce no sample
-- at all, so they arrive here as a GAP between two samples rather
-- than as an ineligible interval. Any interval longer than
-- MAX_CHARGED_INTERVAL is taken to be such a gap and is charged
-- nothing. That bound sits well above the slowest cadence either
-- timer samples at (pickup scores on the ~1 s thought tick, jitter
-- included) and far below either budget (30 s / 60 s).
--
-- Deliberately dependency-free: no requires at all (unit_ai_core
-- requires THIS module, so requiring it back would be a load cycle),
-- and the accounting itself takes `now` from its caller rather than
-- reading the clock -- which is also what makes it, and maintainTask
-- with a stubbed `engine`/`unit`, drivable in a bare Lua VM
-- (Test.Headless.Lua.UnitAiStall).

local M = {}

-- Constants for arrival detection on commanded tasks.
local TASK_ARRIVAL_TILES = 0.6
-- TASK_TIMEOUT_SEC is a STALL budget of eligible time (see above);
-- TASK_PROGRESS_TILES is the closer-than-before step that resets it.
local TASK_TIMEOUT_SEC   = 60.0
local TASK_PROGRESS_TILES = 0.5

-- The longest interval between two samples that can still be one
-- uninterrupted stretch of AI ticking. See the header.
local MAX_CHARGED_INTERVAL = 5.0

-- unit_ai_core's own distance, duplicated rather than required: core
-- requires this module, so requiring core back would be a cycle.
local function distance(ax, ay, bx, by)
    local dx = ax - bx
    local dy = ay - by
    return math.sqrt(dx * dx + dy * dy)
end

-- Charge the interval ending at `now` against `order`'s stall budget
-- and return the eligible time it has accumulated since its last
-- closest approach. `eligible` is false for an interval another
-- action controlled the unit through.
--
-- An order carrying no accounting record is one restored from a
-- `lua.unit_ai` payload written before #1291. The old rule measured
-- from an absolute `progressAt`, which it stamped on the FIRST
-- evaluation of every order (alongside bestDist), so that field is
-- present iff the order has ever been evaluated: reconstruct from it
-- and the order carries forward exactly the charge it had accrued,
-- expiring when it would have. Absent, the order has never been
-- evaluated at all and nothing has been charged yet -- which is also
-- the fresh-command case, though maintainTask/pickupUtility reach
-- that one through M.reset's own first-approach branch.
function M.charge(order, eligible, now)
    local acc = order.stalledFor
    if acc == nil then
        acc = 0
        if order.progressAt then
            acc = now - order.progressAt
            if acc < 0 then acc = 0 end
            order.progressAt = nil
        end
    elseif eligible then
        local dt = now - (order.stallSeenAt or now)
        if dt > 0 and dt <= MAX_CHARGED_INTERVAL then acc = acc + dt end
    end
    order.stalledFor  = acc
    order.stallSeenAt = now
    return acc
end

-- A new closest approach: the whole budget is available again.
function M.reset(order, now)
    order.stalledFor  = 0
    order.stallSeenAt = now
    order.progressAt  = nil
end

-----------------------------------------------------------
-- Task arrival / timeout housekeeping
-----------------------------------------------------------
local function maintainTask(uid, s)
    local task = s.commandedTask
    if not task then return end

    local info = unit.getInfo(uid)
    if not info then
        -- Unit gone; drop the task.
        s.commandedTask = nil
        return
    end

    -- Arrival check.
    local d = distance(info.gridX, info.gridY, task.x, task.y)
    if d <= TASK_ARRIVAL_TILES then
        s.commandedTask = nil
        return
    end

    -- Timeout. The deadline resets ONLY on a new closest approach --
    -- circling an unreachable target never refreshes it, and still
    -- gives up -- and only eligible time is charged against it (see
    -- the header). Expiry stays silent, as it has always been: the
    -- player-visible report on an unreachable commanded move is the
    -- stuck-walk watchdog's (unit_ai.lua), not this one's.
    local now = engine.gameTime()
    if not task.bestDist or d < task.bestDist - TASK_PROGRESS_TILES then
        task.bestDist = d
        M.reset(task, now)
    end
    if M.charge(task, s.currentAction == "follow_command", now) > TASK_TIMEOUT_SEC then
        s.commandedTask = nil
    end
end

M.TASK_ARRIVAL_TILES     = TASK_ARRIVAL_TILES
M.TASK_TIMEOUT_SEC       = TASK_TIMEOUT_SEC
M.TASK_PROGRESS_TILES    = TASK_PROGRESS_TILES
M.MAX_CHARGED_INTERVAL   = MAX_CHARGED_INTERVAL
M.maintainTask           = maintainTask

return M
