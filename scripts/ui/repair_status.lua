-- Shared repair-status display + designation helper (#303).
--
-- Item rows across panels (unit inventory / equip slots / accessories)
-- want the same three things: a name-suffix badge, a tooltip hint
-- line, and a "Prioritize Repair" context-menu toggle. All three read
-- the repair state scripts/unit_ai.lua owns (repairClaims /
-- repairPriority, #302/#303) through its public accessors, so this is
-- the one place that turns that into UI — mirrors quality_tier.lua.
--
-- Only items the repair AI can ever consider (own inventory / equipped
-- / accessories, or the nearest technomule's inventory — see the
-- repair_job comment in unit_ai.lua) carry both an instanceId and a
-- condition field; that is also exactly the set eligible for a
-- priority flag. Ground items and generic cargo-building storage are
-- deliberately excluded (#302: item.listGround() exposes neither
-- instanceId nor sharpness, and repair only ever scans a unit's own
-- held items or a technomule's — never a cargo building's storage —
-- so flagging an item there would be silently inert).

local repairStatus = {}

function repairStatus.isEligible(it)
    return it ~= nil and it.instanceId ~= nil and it.condition ~= nil
end

-- " [Repairing]" / " [Priority]" / "" appended to an item's name.
function repairStatus.suffix(it)
    if not repairStatus.isEligible(it) then return "" end
    local unitAi = require("scripts.unit_ai")
    if unitAi.getRepairClaimant(it.instanceId) then return " [Repairing]" end
    if unitAi.isRepairPriority(it.instanceId) then return " [Priority]" end
    return ""
end

function repairStatus.withSuffix(baseName, it)
    return (baseName or "") .. repairStatus.suffix(it)
end

-- Extra tooltip hint line, or nil when there's nothing to add.
function repairStatus.hintLine(it)
    if not repairStatus.isEligible(it) then return nil end
    local unitAi = require("scripts.unit_ai")
    if unitAi.getRepairClaimant(it.instanceId) then
        return "repair: in progress"
    end
    if unitAi.isRepairPriority(it.instanceId) then
        return "repair: queued (priority)"
    end
    return nil
end

-- A context-menu entry toggling the priority flag, or nil when the
-- item isn't repair-eligible or is already mid-job (can't reprioritize
-- while claimed — the claimant already committed to this instance).
function repairStatus.menuItem(it)
    if not repairStatus.isEligible(it) then return nil end
    local unitAi = require("scripts.unit_ai")
    local iid = it.instanceId
    if unitAi.getRepairClaimant(iid) then return nil end
    local pri = unitAi.isRepairPriority(iid)
    return {
        label    = pri and "Un-prioritize Repair" or "Prioritize Repair",
        callback = function() unitAi.setRepairPriority(iid, not pri) end,
    }
end

return repairStatus
