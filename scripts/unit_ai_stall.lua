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
-- The module carries a SECOND, smaller job for the same two reasons:
-- the elapsed-time WORK clocks (auto-harvest's picking, and since
-- #2332 craft's and construct's pours) are last-sample stamps with
-- exactly this hazard, so M.suspendOrders takes their boundary
-- alongside the orders' and M.workInterval applies the same bound to
-- the interval they charge. Both are documented at their definitions.
--
-- MAX_CHARGED_INTERVAL is the backstop for the gaps no such path can
-- announce -- a save/load boundary, a unit that stopped being ticked
-- at all: an interval longer than it is not one uninterrupted stretch
-- of AI ticking and is charged nothing. The bound sits well above the
-- slowest cadence either timer samples at (pickup scores on the ~1 s
-- thought tick, jitter included) and far below either budget
-- (30 s / 60 s), so a genuinely stalled order still expires.
--
-- All three of those order timers REPORT the abandonment they decide
-- on (#1769). pickup_timeout and transfer_order_timeout always did,
-- in their own modules and unchanged by that issue; TASK_TIMEOUT_SEC's
-- expiry did not, and the report maintainTask now emits below closes
-- the one gap -- see that branch for the case it covers and the
-- duplicate it avoids. The position-hold walk home is deliberately NOT
-- among them: it is the residue of an order that already ARRIVED, not
-- an order of its own, so releasing an unreachable anchor abandons
-- nothing the player is waiting on.
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

-- unit_ai_pickup's own player-facing unit name, duplicated rather
-- than required for the same reason `distance` is: that module
-- requires this one, so requiring it back would be a cycle. A personal
-- name (#264) if the unit has one, else the species label, else a
-- prettified def name.
local function unitLabel(uid)
    local info = unit.getInfo(uid)
    if info and info.name and info.name ~= "" then return info.name end
    if info and info.displayName and info.displayName ~= "" then
        return info.displayName
    end
    local n = (info and info.defName) or "Unit"
    return n:sub(1, 1):upper() .. n:sub(2)
end

-- Per-uid bookkeeping for the stuck-walk watchdog's report, read by
-- maintainTask's timeout branch below. Deliberately module-local and
-- NOT a field on the order or on aiState: it is a within-session fact
-- about a message the player has already seen, not durable player
-- intent, so it must not ride into a save (this change touches no save
-- format). Dropped whole on every branch below that ends the order it
-- describes.
--
--   walking  -- the task the ENGINE was last given a destination for,
--               recorded by M.noteWalk from unit_ai_combat.lua's
--               followCommandExecute. That is the only place a
--               commanded task reaches unit.moveTo, and it is the only
--               honest answer to "whose walk is this?": commandMove
--               replaces `s.commandedTask` alone, and unit_ai.lua's
--               switch-or-idle execute gate does NOT re-run an action
--               that is already running, so a replacement issued mid-
--               walk can stay un-issued for many ticks while the
--               engine is still walking its predecessor's destination.
--   reported -- the task the watchdog has already reported for, held
--               as the TABLE rather than a flag, so a replacement is
--               never covered by a report about its predecessor.
--
-- Identity rather than a timestamp comparison: game time need not
-- advance between a player command and the tick that first sees it, so
-- an order's `startedAt` can tie the watchdog's last-progress stamp and
-- no comparison of the two numbers could separate those orders at all.
local watchdog = {}

-- Called by followCommandExecute when it hands a commanded task to the
-- engine. Until that happens the task is merely PENDING, however long
-- it has been current.
function M.noteWalk(uid, task)
    local w = watchdog[uid]
    if not w then
        w = {}
        watchdog[uid] = w
    end
    w.walking = task
end

-- Called by the stuck-walk watchdog when it reports a unit that has
-- stopped moving. Two things must BOTH hold before that report may
-- silence a later stall expiry, or the wrong order is the one
-- silenced: the commanded move's own action was in control over the
-- interval just judged (so the stuck walk IS a commanded walk rather
-- than some other action's), and the order still current is the one
-- the engine was actually walking. Nothing is lost when the check
-- refuses -- the order then reports for itself if it goes on to stall,
-- which is the direction this issue exists to fix.
function M.noteStuckReport(uid, s)
    if not s or s.currentAction ~= "follow_command" then return end
    local w = watchdog[uid]
    if not w or not s.commandedTask then return end
    if s.commandedTask ~= w.walking then return end
    w.reported = s.commandedTask
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
-- may run for a unit that has no AI state yet), and is called on EVERY
-- swallowed tick rather than only at entry: an interruption is a span,
-- not an event, and only the last boundary inside it bounds the
-- interval the next sample sees. `uid` is the unit that state belongs
-- to, needed by the craft boundary below and optional everywhere else
-- -- a caller with only a state table still gets every stamp dropped.
function M.suspendOrders(s, uid)
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
    -- Craft's and construct's work clocks (#2332) are the same kind of
    -- last-sample stamp and take the same boundary. Without it a
    -- crafter or builder knocked down mid-pour keeps its stamp, and the
    -- first tick after it stands charges the WHOLE collapse as instant
    -- progress -- the hazard unit_ai_mental.lua's preempt already
    -- documents, on the two paths that reach neither preempt nor an
    -- onExit.
    s.lastCraftAt     = nil
    s.lastConstructAt = nil
    -- Dropping the stamp alone is not enough for either job: both pour
    -- from a PHASE, and re-entering that phase is what re-stamps the
    -- clock and (for craft) re-arms #590's working flag. So take the
    -- boundary exactly as each module's own onExit does -- demote the
    -- pouring phase to `walking`, and clear the bill's working flag,
    -- which craft.setBillWorking(uid, id, true) re-arms only on the
    -- walking->working transition. A crafter collapsed at its station
    -- must not keep that bill's power draw registered
    -- (Craft.Bills.cbWorking) for the length of the collapse.
    --
    -- Persistent state is untouched, same as any preemption: the job,
    -- its claim, its consumed materials and its banked progress all
    -- survive: only the stamps, the re-entry phase and the working flag
    -- change. Idempotent across the span, since the second and later
    -- swallowed ticks find the phase already demoted.
    if s.constructJob and s.constructJob.phase == "building" then
        s.constructJob.phase = "walking"
    end
    local cj = s.craftJob
    if cj and cj.phase == "working" then
        cj.phase = "walking"
        -- Guarded rather than assumed: this module is deliberately
        -- dependency-free and drivable in a bare Lua VM, where `craft`
        -- is simply absent (Test.Headless.Lua.UnitAiStall).
        if uid and craft and craft.setBillWorking then
            craft.setBillWorking(uid, cj.billId, false)
        end
    end
end

-- The interval ending at `now` that may be charged as WORK, given the
-- last sample stamp `last`: the elapsed time when it is one
-- uninterrupted stretch of AI ticking, and zero when it is not.
--
-- Two rules, the ones #1291/#1582 settled for auto-harvest and #2332
-- extends to craft and construct:
--
--   * A missing stamp is a zero-length interval. Every path that
--     swallows a tick drops the stamp through M.suspendOrders above as
--     it happens, so the next reading charges nothing for the interval
--     spanning it.
--   * MAX_CHARGED_INTERVAL is the backstop for the gaps no path can
--     announce -- a save/load boundary, a page hidden while another
--     page keeps the session clock running, a unit that stopped being
--     ticked at all. An interval longer than it is charged ZERO rather
--     than being clamped down to the bound: the clamp would still
--     credit seconds of work that never happened, and the bound is
--     already far above the slowest cadence any of these clocks
--     samples at. An interval exactly equal to it is still one
--     uninterrupted stretch, and charges in full.
--
-- The work already banked on the plant, the bill or the designation
-- survives all of that; only the clock restarts.
function M.workInterval(last, now)
    local dt = now - (last or now)
    if dt > 0 and dt <= MAX_CHARGED_INTERVAL then return dt end
    return 0
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
    if not task then
        watchdog[uid] = nil
        return
    end

    -- This unit's watchdog record, if its commanded move has reached
    -- the engine at all yet -- M.noteWalk is what creates one.
    local w = watchdog[uid]

    local info = unit.getInfo(uid)
    if not info then
        -- Unit gone; drop the task.
        watchdog[uid] = nil
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
        watchdog[uid] = nil
        s.commandedTask = nil
        -- Return the completed task for the dispatcher's post-arrival
        -- handoff. This module stays dependency-free: the caller decides
        -- whether task-specific state (such as a combat withdrawal) also
        -- needs to be retired. Timeout and disappearance still return nil,
        -- so neither can masquerade as a successful arrival.
        return task
    end

    -- Timeout. The deadline resets ONLY on a new closest approach --
    -- circling an unreachable target never refreshes it, and still
    -- gives up -- and only eligible time is charged against it (see
    -- the header).
    --
    -- Expiry REPORTS (#1769). It used to stay silent, on the reasoning
    -- that the player-visible report on an unreachable commanded move
    -- was the stuck-walk watchdog's (unit_ai.lua). It is not, because
    -- the two ask different questions: the watchdog asks whether the
    -- unit is MOVING (it refreshes on more than 0.1 tiles of
    -- displacement in any direction from its mark), while this budget
    -- asks whether the unit is CLOSING (it refreshes only on more than
    -- TASK_PROGRESS_TILES of improvement on the best distance yet).
    -- A unit that circles, oscillates, or crawls toward the target
    -- without netting that improvement over the whole eligible budget
    -- keeps the watchdog fresh the entire time -- so it never reported,
    -- and the order was dropped with nothing said at all.
    --
    -- The two reports are one event when the watchdog's stuck walk IS
    -- this order's walk, and M.noteStuckReport records exactly that
    -- case; the order then expires silently rather than saying twice
    -- what the watchdog already said once. A later genuine closest
    -- approach retires that record along with the budget it refreshes.
    --
    -- Only a PLAYER order reports. `task.player` is unit_ai_core's
    -- commandMove marker, absent on an internal move
    -- (scripts/building_spawn.lua's portal walk-out) exactly as it is
    -- on the arrival branch above: nobody asked for that walk, so its
    -- abandonment is not news. An order that ARRIVES, is superseded by
    -- a replacement, or is cancelled never reaches here at all.
    local now = engine.gameTime()
    local eligible = s.currentAction == "follow_command"
    if eligible and (not task.bestDist
                     or d < task.bestDist - TASK_PROGRESS_TILES) then
        task.bestDist = d
        if w then w.reported = nil end
        M.reset(task, now)
    end
    if M.charge(task, eligible, now) > TASK_TIMEOUT_SEC then
        local alreadyReported = w ~= nil and w.reported == task
        watchdog[uid] = nil
        if task.player and not alreadyReported then
            engine.emitEventForUnit("unit_warning", string.format(
                "%s gave up on its move order — no progress toward (%.0f, %.0f)",
                unitLabel(uid), task.x, task.y), uid, info.gridX, info.gridY)
        end
        s.commandedTask = nil
    end
end

M.TASK_ARRIVAL_TILES     = TASK_ARRIVAL_TILES
M.TASK_TIMEOUT_SEC       = TASK_TIMEOUT_SEC
M.TASK_PROGRESS_TILES    = TASK_PROGRESS_TILES
M.MAX_CHARGED_INTERVAL   = MAX_CHARGED_INTERVAL
M.maintainTask           = maintainTask

return M
