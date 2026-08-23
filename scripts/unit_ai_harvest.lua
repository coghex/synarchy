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

function unitAi.harvest.execute(uid, s, params)
    -- Collecting: pull the harvested yield off the ground, one item
    -- per tick (mirrors forageExecute's collecting phase). No work
    -- accrues here — the plant is already picked.
    if s.harvestPhase == "collecting" then
        s.lastHarvestAt = nil
        local loot = s.harvestLoot or {}
        local nextGid = table.remove(loot)
        if not nextGid or not item.pickupGround(uid, nextGid) then
            s.harvestPhase = nil
            s.harvestLoot  = nil
        end
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
        unitAi.harvest.bindProgress(s, tgt.x, tgt.y)
        -- Elapsed time is charged only between two consecutive
        -- ADJACENT, executing ticks, by #1291's two rules:
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
