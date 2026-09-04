-- Unit AI auto-harvest action (#1582 split from unit_ai_farm.lua).
--
-- auto_harvest (#336) — the farming epic's third tile-work action,
-- lifted out of scripts/unit_ai_farm.lua when the skill-scaled work
-- accumulator below pushed that file past tools/lua_module_budget.py's
-- 500-line cap. The till / plant / harvest seam is the cohesive one:
-- till and plant are player-DESIGNATED jobs sharing a claim registry,
-- lock-in and equip phases, while auto-harvest is autonomous
-- farm-tending that claims nothing.
--
-- Loaded by scripts/unit_ai_farm.lua rather than by unit_ai.lua
-- directly, so the bootstrap contract is unchanged: requiring either
-- scripts.unit_ai or scripts.unit_ai_farm still attaches all three of
-- unitAi.till / unitAi.plant / unitAi.harvest.

local unitAi = package.loaded["scripts.unit_ai"]
local core = require("scripts.unit_ai_core")
local grantWorkXP = core.grantWorkXP

local mv = require("scripts.movement_speed")
local roles = require("scripts.unit_roles")
-- Eligible-time accounting (#1291): the picking clock below charges an
-- interval by the same two rules the commanded-order stall budget uses,
-- and shares its constant rather than inventing a second one. Requiring
-- it here is cycle-free -- unit_ai_stall requires nothing.
local stall = require("scripts.unit_ai_stall")

-----------------------------------------------------------
-- Action: auto_harvest (#336, skill-scaled since #1582)
--
-- Skill-gated colony farm-tending: pick up any ripe harvestable flora
-- in range — planted crops AND wild flora alike (world.
-- findHarvestableFlora / world.harvestFlora, #94, don't distinguish the
-- two; #334's crop species carry worldGen.density 0.0, so any crop
-- instance found here was deliberately planted, never a wild spawn).
-- Both calls stay UNTAGGED: the tag argument belongs to callers with a
-- specific material in mind (chop asks for "wood"), while farm-tending
-- takes whatever the plant yields.
--
-- Harvest is a WORK ACTION, not an instant one (#1582): picking
-- accumulates s.harvestProgress toward WORK_TOTAL at
-- params.harvest_rate scaled by the farming skill, the same
-- 0.5 + farming/100 factor unitAi.till/unitAi.plant use — which is what
-- data/units/acolyte.yaml's farming entry and #336 ("skill-gated
-- auto-harvest") have promised all along. A level-0 picker takes three
-- times as long as a level-100 one; nothing else about the action
-- changed. NOT hunger-gated like forage: this is routine work,
-- weighted by the farming role (#265) in arbitration and scaled by the
-- farming SKILL here in execution — requirement 5 keeps those two
-- distinct.
--
-- Grouped under unitAi.harvest (the unitAi.till convention, #333) —
-- kept as-is by the split.
-----------------------------------------------------------
unitAi.harvest = {}

-- Work needed to pick one plant, in the same progress units till and
-- plant use (a tile is 1.0 there too). params.harvest_rate is progress
-- per game-second at farming 50, so the duration a given skill buys is
-- WORK_TOTAL / (harvest_rate * (0.5 + farming/100)).
unitAi.harvest.WORK_TOTAL = 1.0

-- TRANSIENCE (#1582). s.harvestProgress, s.harvestProgressAt and
-- s.lastHarvestAt are stripped from the lua.unit_ai save payload
-- (scripts/unit_ai_save.lua's TRANSIENT_WORK_FIELDS), so a load starts
-- every picker on a fresh plant. That is the honest post-load state,
-- not a loss, for three reasons:
--
--   * Under four game-seconds of work is re-earned immediately.
--     Persisting it buys nothing a player could notice, and the same
--     trade-off already classified repairPriority transient rather
--     than spend a lua.unit_ai version bump plus a save-compat fixture
--     on it (docs/persistence_state_inventory.md, the unit_ai_claims
--     row).
--   * lastHarvestAt is a raw game-time stamp, and #1291's rule for an
--     interval the AI could not tick through -- a save/load boundary
--     being its named example -- is that it charges nothing. Dropping
--     the stamp IS that answer, applied at the boundary rather than
--     after it.
--   * harvestProgressAt names a TILE, and the progress is only ever
--     valid for the instance standing on it. A load replaces the whole
--     session, so nothing promises the same plant is still there.
--
-- Nothing has to re-populate them: bindProgress below seeds a fresh
-- accumulator on the first adjacent tick. s.harvestTarget is NOT in
-- that set -- it persisted before #1582 and still does.

-- Progress belongs to ONE flora instance, identified by its tile.
-- Dropping it here is what stops partial work on a plant that vanished,
-- was picked by someone else, or stopped being the nearest candidate
-- from being spent on the next plant instead.
function unitAi.harvest.resetProgress(s)
    s.harvestProgress   = nil
    s.harvestProgressAt = nil
    s.lastHarvestAt     = nil
end

-- Bind the accumulator to (tx, ty), restarting it whenever the target
-- moved. lastHarvestAt is cleared alongside so the first tick on a new
-- plant charges no elapsed time.
function unitAi.harvest.bindProgress(s, tx, ty)
    local at = s.harvestProgressAt
    if not at or at.x ~= tx or at.y ~= ty then
        s.harvestProgress   = 0
        s.harvestProgressAt = { x = tx, y = ty }
        s.lastHarvestAt     = nil
    end
end

function unitAi.harvest.utility(uid, s, params)
    -- Pending collection is eligible on its own (#1743). A completed
    -- harvest leaves its yields lying on the ground and only execute's
    -- collecting branch below can pull them in, so the action has to be
    -- SELECTABLE with no living plant left anywhere in range -- which
    -- is exactly the state one ripe plant produces. Scoring -math.huge
    -- here stranded the phase indefinitely, because arbitration
    -- (scripts/unit_ai.lua) selects on `u > bestScore` from a
    -- -math.huge seed and so can never pick it.
    --
    -- Gated on the PHASE alone, never on harvestLoot being non-empty:
    -- the terminal tick that clears the phase is the one where
    -- table.remove has already returned nil, so gating on the list
    -- would strand precisely that cleanup.
    --
    -- No scan runs and no target is set on this path (requirement 5):
    -- execute takes the collecting branch before it reads
    -- s.harvestTarget, so finishing a collection needs no second plant
    -- found, preselected, or searched for. The score is the ordinary
    -- role-weighted band at full proximity -- the yields are underfoot
    -- -- which beats idle's registered 0 while staying finite, so
    -- every higher-priority need, order and combat response still
    -- preempts it.
    if s.harvestPhase == "collecting" then
        return params.harvest_base_utility
             * roles.weight(s, "auto_harvest")
    end
    if not world.findHarvestableFlora then return -math.huge end
    local info = unit.getInfo(uid)
    if not info then return -math.huge end
    local ux = math.floor(info.gridX)
    local uy = math.floor(info.gridY)
    local spot = world.findHarvestableFlora(ux, uy, params.harvest_scan_range)
    if not spot then
        s.harvestTarget = nil
        unitAi.harvest.resetProgress(s)
        return -math.huge
    end
    s.harvestTarget = { x = spot.gx, y = spot.gy }
    local distFactor = math.max(0, 1 - spot.dist / params.harvest_scan_range)
    return params.harvest_base_utility * distFactor
         * roles.weight(s, "auto_harvest")
end

-- Capacity admission for one queued yield (#2293), the same
-- last-moment check unit_ai_fetch.lua and unit_ai_repair_target.lua
-- make immediately before their own item.pickupGround. The verb itself
-- reads no weight and no carrying_capacity -- a missing unit, a unit
-- with no live page, a malformed argument or an absent gid are the only
-- ways it returns false (src/Engine/Scripting/Lua/API/Items/Ground.hs)
-- -- so admission is the CALLER's, deliberately, and an autonomous
-- picker working a field is the one caller that can repeat the pickup
-- indefinitely and walk a unit arbitrarily far over its capacity.
--
-- The row is re-resolved through item.getGroundForUnit on the picker's
-- OWN page, which is the same page resolution item.pickupGround commits
-- through, so the weight gated on is the live total mass of the exact
-- instance that would move -- fill and nested contents included -- and
-- not a static def weight.
--
-- Returns whether the yield may be taken. An UNRESOLVABLE gid is not a
-- capacity refusal: the row raced away, or this picker has no live
-- page, and there is no weight to compare, so it ends the collection
-- silently. Only carried + live > capacity warns, and it warns exactly
-- ONCE because the caller clears the phase in the same tick rather than
-- re-offering the same yield next tick.
local function admitYield(uid, gid)
    local owned = item.getGroundForUnit(uid, gid)
    if not owned then return false end
    local carried = unit.getCarryingWeight(uid) or 0
    local maxW    = unit.getStat(uid, "carrying_capacity") or math.huge
    local w       = owned.weight or 0
    if carried + w > maxW then
        engine.logWarn("harvest: unit " .. tostring(uid)
            .. " at capacity (" .. string.format("%.1f", carried + w)
            .. " > " .. string.format("%.1f", maxW)
            .. " kg) — leaving ground " .. tostring(owned.defName))
        return false
    end
    return true
end

function unitAi.harvest.execute(uid, s, params)
    -- Collecting: pull the harvested yield off the ground, one item
    -- per tick (mirrors forageExecute's collecting phase). No work
    -- accrues here — the plant is already picked. Reached under
    -- ordinary arbitration since #1743: utility above scores this same
    -- phase rather than returning -math.huge, so the branch no longer
    -- depends on some other ripe plant keeping auto_harvest alive.
    --
    -- Every exit that is not a completed pickup ends the phase in THIS
    -- tick (#2293): an exhausted list, an unresolvable row, a refused
    -- weight, or a pickup that lost its race. The yields were
    -- materialized as ordinary ground items before collection began
    -- (World.Forage.Harvest), so whatever is left simply stays where it
    -- lies -- collectable by another worker, or by this one once it has
    -- unloaded -- and nothing is deleted or half-moved.
    if s.harvestPhase == "collecting" then
        s.lastHarvestAt = nil
        local loot = s.harvestLoot or {}
        local nextGid = table.remove(loot)
        if nextGid and admitYield(uid, nextGid)
                   and item.pickupGround(uid, nextGid) then
            return
        end
        s.harvestPhase = nil
        s.harvestLoot  = nil
        return
    end

    local tgt = s.harvestTarget
    if not tgt then
        unitAi.harvest.resetProgress(s)
        return
    end
    local info = unit.getInfo(uid)
    if not info then return end
    local utx = math.floor(info.gridX)
    local uty = math.floor(info.gridY)
    local cheb = math.max(math.abs(utx - tgt.x), math.abs(uty - tgt.y))

    if cheb <= 1 then
        -- A unit that is adjacent but still MOVING is not picking yet.
        -- Dispatch executes an action the moment it wins arbitration,
        -- walking or not (scripts/unit_ai.lua's `switching or activity
        -- == "idle"`), so a switch into auto_harvest can land here
        -- mid-stride with the previous action's walk still under way.
        -- Stop first and start no clock: the walk that follows is
        -- travel, and the next tick would otherwise charge it as
        -- picking. till/plant take the same unit.stop on arrival, and
        -- the progress already banked on this plant is untouched.
        local activity = unit.getActivity(uid)
        if activity == "walking" or activity == "running" then
            unit.stop(uid)
            s.lastHarvestAt = nil
            return
        end

        unitAi.harvest.bindProgress(s, tgt.x, tgt.y)
        -- Elapsed time is charged only between two consecutive
        -- ADJACENT, STATIONARY, executing ticks, by #1291's two rules:
        --
        --   * Every path that swallows a tick DROPS the stamp as it
        --     happens, so the next reading charges nothing for the
        --     interval spanning it. lastHarvestAt is nil on arrival,
        --     on a new target, while collecting, on onExit (the
        --     arbitration switch), and on unit_ai.lua's collapsed-pose
        --     / mid-animation returns and unit_ai_mental.lua's
        --     preemption, which both funnel through
        --     unit_ai_stall.suspendOrders.
        --   * MAX_CHARGED_INTERVAL is the backstop for the gap no path
        --     can announce — a save/load boundary, a unit that stopped
        --     being ticked. An interval longer than it is not one
        --     uninterrupted stretch of picking, so it charges ZERO
        --     rather than being clamped down to the bound.
        --
        -- The work already accumulated on the plant survives all of
        -- them; only the clock restarts.
        local now = engine.gameTime()
        local elapsed = now - (s.lastHarvestAt or now)
        s.lastHarvestAt = now
        local dt = 0
        if elapsed > 0 and elapsed <= stall.MAX_CHARGED_INTERVAL then
            dt = elapsed
        end
        -- Farming skill (#265/#336) scales the pick exactly as it
        -- scales tilling and planting: level 50 ≈ baseline, level 0
        -- half rate. Legacy-save units without the key pick at the
        -- yaml novice base, same fallback as till/plant.
        local fSkill = unit.getSkill(uid, "farming") or 25.0
        s.harvestProgress = (s.harvestProgress or 0)
                          + params.harvest_rate * (0.5 + fSkill / 100.0) * dt
        if s.harvestProgress < unitAi.harvest.WORK_TOTAL then return end

        local yields = world.harvestFlora(tgt.x, tgt.y)
        if yields and #yields > 0 then
            unit.pickup(uid)   -- bend-down anim over the plant
            local gids = {}
            for _, yi in ipairs(yields) do gids[#gids + 1] = yi.gid end
            s.harvestLoot  = gids
            s.harvestPhase = "collecting"
            grantWorkXP(uid, "farming", params.harvest_xp_per_harvest or 0)
        end
        -- Raced / regrowing after all, or a completed harvest either
        -- way: forget the target and its accumulator; the next
        -- decision re-finds and starts fresh.
        s.harvestTarget = nil
        unitAi.harvest.resetProgress(s)
        return
    end

    -- Out of reach: the walk itself is not harvesting work.
    s.lastHarvestAt = nil
    unit.moveTo(uid, tgt.x + 0.5, tgt.y + 0.5, mv.comfort(uid))
end

-- Preemption (thirst, combat, player order): the accumulated work on
-- this plant survives so the pick resumes, but its work clock does
-- not — the interruption itself must not be charged as picking.
function unitAi.harvest.onExit(uid, s, params)
    s.lastHarvestAt = nil
end
