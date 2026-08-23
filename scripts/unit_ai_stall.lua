-- Order stall accounting for player-issued orders (#1291).
--
-- All three order timers -- TASK_TIMEOUT_SEC for a commanded move
-- (below), unit_ai_pickup.lua's pickup_timeout for a commanded pickup,
-- and unit_ai_transfer.lua's transfer_order_timeout for a queued
-- transfer order (#1247) -- are STALL timers, not total-trip budgets
-- (#920): they answer "is this unit getting anywhere?", never "how
-- long has this trip taken". scripts/unit_ai_hold.lua's walk back to a
-- position anchor (#1216) is charged by the same rules through the
-- same M.charge/M.reset, for the same reason.
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
-- That reset is the ORDER'S OWN action's to make, so it is gated on
-- the same eligibility the charging is: an interruption that happens
-- to carry the unit closer (a `treat_ally` walk, a combat chase in the
-- same direction) must not refund a budget the order had already
-- spent, which is the accumulation rule read in the other direction.
-- Nothing is lost by waiting: the approach is a fact about where the
-- unit now IS, so the first eligible sample after the interruption
-- records it and starts the budget over then.
--
-- An interval is eligible when the order's own action is the one
-- arbitration left in control of the unit. Callers pass that as
-- `s.currentAction`, read BEFORE this tick re-scores, so it describes
-- the interval that just elapsed rather than the one about to start:
-- ordinary scheduling cadence is eligible, an interval another action
-- won is not.
--
-- Intervals in which the AI could not run at all produce no sample
-- here at all, so they cannot be recognised as ineligible after the
-- fact -- however short they are. Every such path therefore RECORDS
-- the boundary as it happens, through M.suspendOrders: unit_ai.lua's
-- collapsed-pose / engine-animation short-circuit and
-- unit_ai_mental.lua's delirium / mental-break preemption both call
-- it on every tick they swallow, and the next sample then charges
-- nothing for the interval that spans them. A one-second get-up stun
-- costs a pending order exactly as little as a five-minute collapse.
--
-- MAX_CHARGED_INTERVAL is the backstop for the gaps no such path can
-- announce -- a save/load boundary, a unit that stopped being ticked
-- at all: an interval longer than it is not one uninterrupted stretch
-- of AI ticking and is charged nothing. The bound sits well above the
-- slowest cadence either timer samples at (pickup scores on the ~1 s
-- thought tick, jitter included) and far below either budget
-- (30 s / 60 s), so a genuinely stalled order still expires.
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

-- Record an interruption boundary on whatever orders -- and whatever
-- elapsed-time work accumulator -- `s` is carrying: the AI could not
-- pursue them over the interval that just elapsed, so the next sample
-- must charge nothing for it. Dropping the last-sample stamp is exactly
-- that -- M.charge reads a missing stamp as a zero-length interval and
-- picks the accounting back up from `now`, leaving everything already
-- charged charged.
--
-- Takes the state table (nil-tolerant, since a short-circuited tick
-- may run for a unit that has no AI state yet) rather than a uid, and
-- is called on EVERY swallowed tick rather than only at entry: an
-- interruption is a span, not an event, and only the last boundary
-- inside it bounds the interval the next sample sees.
function M.suspendOrders(s)
    if not s then return end
    if s.commandedTask  then s.commandedTask.stallSeenAt  = nil end
    if s.pickupOrder    then s.pickupOrder.stallSeenAt    = nil end
    if s.transferOrder  then s.transferOrder.stallSeenAt  = nil end
    -- A position hold's walk back to its anchor (#1216) is charged
    -- against the same budget by the same rules, so it takes the same
    -- boundary.
    if s.holdAnchor     then s.holdAnchor.stallSeenAt     = nil end
    -- Auto-harvest's picking clock (#1582) takes the boundary for the
    -- same reason and by the same mechanism: it is a last-sample stamp
    -- whose next reading would otherwise charge the swallowed interval
    -- as picking. scripts/unit_ai.lua's collapsed-pose and
    -- mid-animation returns are the paths that need it -- they swallow
    -- the tick WITHOUT firing the outgoing action's onExit, which is
    -- how every other preemption clears the same stamp
    -- (unit_ai_mental.lua's preempt documents that hazard for
    -- construct/craft). Only the stamp is dropped: the work already
    -- accumulated on the plant survives the interruption, exactly as a
    -- partially spent stall budget does.
    s.lastHarvestAt = nil
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

    -- Arrival check. A PLAYER move order that actually completed
    -- leaves the unit holding its destination (#1216, SURV-4): the
    -- anchor is written here because this is the one branch that
    -- distinguishes arrival from the timeout below, and requirement 5
    -- turns on exactly that distinction -- an order that gave up
    -- creates no hold. `task.player` is set by unit_ai_core.lua's
    -- commandMove and deliberately absent on an internal one
    -- (scripts/building_spawn.lua's portal walk-out), so a freshly
    -- spawned acolyte is never pinned where the roster put it.
    --
    -- Written as a plain field rather than a call into
    -- scripts/unit_ai_hold.lua, which owns everything else about the
    -- hold: that module requires THIS one (for the arrival radius and
    -- this accounting), so requiring it back would be a load cycle --
    -- the same reason `distance` is duplicated above.
    local d = distance(info.gridX, info.gridY, task.x, task.y)
    if d <= TASK_ARRIVAL_TILES then
        if task.player then s.holdAnchor = { x = task.x, y = task.y } end
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
    local eligible = s.currentAction == "follow_command"
    if eligible and (not task.bestDist
                     or d < task.bestDist - TASK_PROGRESS_TILES) then
        task.bestDist = d
        M.reset(task, now)
    end
    if M.charge(task, eligible, now) > TASK_TIMEOUT_SEC then
        s.commandedTask = nil
    end
end

M.TASK_ARRIVAL_TILES     = TASK_ARRIVAL_TILES
M.TASK_TIMEOUT_SEC       = TASK_TIMEOUT_SEC
M.TASK_PROGRESS_TILES    = TASK_PROGRESS_TILES
M.MAX_CHARGED_INTERVAL   = MAX_CHARGED_INTERVAL
M.maintainTask           = maintainTask

return M
