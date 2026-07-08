-- Stance recovery + injury consequences for scripts/unit_resources.lua:
-- combat-readiness regen, disabled-hand weapon drops, and the
-- death-by-injury / disabling-collapse locomotor state machine.

local injuries = require("scripts.injuries")
local brain    = require("scripts.brain")
local alerts   = require("scripts.unit_resource_alerts")

local M = {}

-----------------------------------------------------------
-- Stance: combat readiness, 0..1. Spent by attacking and by taking
-- hits (both engine-side in Combat.Resolution); recovers here toward
-- 1.0 — quickly, and faster for agile/dextrous units (the time it
-- takes to set your feet and ready your guard). Absent ⇒ treated as
-- 1.0 everywhere, so no explicit spawn init is needed.
-----------------------------------------------------------
local STANCE_RECOVER_BASE     = 0.35   -- per second, floor
local STANCE_RECOVER_PER_STAT = 0.12   -- per second per (dex+agi) point
function M.tickStance(uid, dt)
    local cur = unit.getStat(uid, "stance")
    if cur == nil or cur >= 1.0 then return end
    local dex  = unit.getStat(uid, "dexterity") or 1.0
    local agi  = unit.getStat(uid, "agility") or 1.0
    local rate = STANCE_RECOVER_BASE + STANCE_RECOVER_PER_STAT * (dex + agi)
    local newv = cur + rate * dt
    if newv > 1.0 then newv = 1.0 end
    unit.setStat(uid, "stance", newv)
end

-----------------------------------------------------------
-- Functional consequence: a severed or shattered hand/arm can no longer
-- grip — the unit DROPS whatever weapon it held in that hand onto the
-- ground (instance-preserving, so the dropped dagger keeps its condition /
-- sharpness). A lost finger or a hurt bicep doesn't disarm; only the
-- grip-bearing structures do.
-----------------------------------------------------------
local GRIP_DISABLERS = { hand = true, palm = true, forearm = true, arm = true }
local HAND_SLOT = { ["l_"] = "left_hand", ["r_"] = "right_hand" }

-- Drop the weapon held in any grip slot that a disabling wound has
-- maimed. This runs every injury tick and is self-correcting: the drop
-- only fires when a weapon is actually present (dropEquipmentToGround
-- returns false on an empty slot, and reads/writes the same unit
-- instance the loadout query does), so an emptied hand no-ops on
-- subsequent ticks. A weapon re-equipped into the still-disabled slot
-- later is dropped again — there is no one-shot guard to suppress it
-- (issue #193: a stale per-slot flag used to let a re-equipped weapon
-- stay in a severed hand forever).
local function dropDisabledHandWeapons(uid)
    local ws = unit.getWounds(uid)
    if type(ws) ~= "table" then return end
    for _, w in ipairs(ws) do
        local p    = w.part or ""
        local slot = HAND_SLOT[p:sub(1, 2)]          -- "l_"/"r_" → slot
        if slot then
            local tokn = p:gsub("^[lr]_", "")
            local disabling = GRIP_DISABLERS[tokn]
                and (w.kind == "severed"
                     or (w.kind == "fracture" and (w.severity or 0) >= 0.85))
            if disabling then
                local lo   = equipment.getLoadout(uid)
                local held = lo and lo[slot]
                if held and unit.dropEquipmentToGround(uid, slot) then
                    local info = unit.getInfo(uid)
                    if info then
                        local gx, gy = alerts.unitCoords(info)
                        local msg = alerts.unitLabel(info) .. " drops "
                            .. (held.displayName or held.defName or "a weapon")
                        if gx and gy then
                            engine.emitEventForUnit("unit_event", msg, uid, gx, gy)
                        else
                            engine.emitEventForUnit("unit_event", msg, uid)
                        end
                    end
                end
            end
        end
    end
end

-----------------------------------------------------------
-- Injury tick: death-by-injury + disabling collapse.
--
-- Falls (and combat) stamp wounds via the engine; this is where those
-- injuries TRIGGER consequences:
--   * Lethal: any vital body part whose wounds sum to >= 1.0 kills the
--     unit, with a player alert. A tall fall thus kills by breaking the
--     body — the death emerges from the injuries, not a height check.
--   * Disabling (a concussion or a shattered leg): puts/keeps the unit
--     on the ground (a normal Collapsed). It can't get up — checkRevive
--     gates on the same condition — until the injury heals enough.
--
-- Returns true if the unit died this tick (caller skips the rest).
function M.tickInjuries(uid, info, pose)
    if pose == "dead" then return false end

    local cause = injuries.lethalCause(uid)
    if cause then
        alerts.emitDeathAlert(uid, cause, "injury")
        unit.kill(uid)
        return true
    end

    -- A maimed hand drops its weapon (once).
    dropDisabledHandWeapons(uid)

    -- Locomotor state machine (don't fight a fall knockdown — that's the
    -- engine's own self-timed collapse):
    --   * unconscious (concussion)        → Collapsed (can't even crawl)
    --   * conscious but legs gone         → Crawling (drags itself along)
    --   * recovered enough to walk        → stand back up
    if not (info and info.knockedDown) then
        -- Collapse decision with hysteresis on the collapse↔crawl boundary.
        -- ENTERING collapse uses the bare knockout triggers; STAYING collapsed
        -- holds the unit down until BOTH inputs clear their rise band
        -- (consciousness ≥ RISE_AT via brain.canRise, concussion below
        -- CONCUSSION_RISE via injuries.concussionCanRise). Without this both
        -- directions pivot on the same threshold (0.15 / 0.35), so a unit
        -- jittering across it flaps collapsed↔crawling every tick (#304).
        local knockedOut = injuries.isUnconscious(uid) or brain.isUnconscious(uid)
        local canRise    = brain.canRise(uid) and injuries.concussionCanRise(uid)
        local collapsed  = injuries.collapseWithHysteresis(pose, knockedOut, canRise)
        if collapsed then
            -- Out cold: from a concussion OR from physiological derangement
            -- (heat stroke / hypoxia / salt imbalance dropping consciousness).
            if pose ~= "collapsed" then unit.collapse(uid) end
        elseif injuries.cannotWalk(uid) then
            -- Conscious + can't walk: crawl. Covers both dropping from
            -- standing AND rising out of a collapse once consciousness
            -- returns but the legs are still broken.
            if pose ~= "crawling" then unit.crawl(uid) end
        elseif pose == "crawling" then
            -- Legs healed enough to walk — stand up (revive handles the
            -- Crawling→Standing snap; checkRevive handles Collapsed).
            -- Exception (#612): the sleep goal's lie-down/wake-up chain
            -- passes through Crawling as a deliberate WAYPOINT (stand ->
            -- crouch -> crawl -> sleep), not an injury symptom — reviving
            -- mid-chain would snap a healthy, about-to-sleep unit back to
            -- Standing and the AI would just re-descend forever, never
            -- reaching Sleeping. unit_ai_sleep.lua drives the unit back up
            -- itself once the chain completes.
            local s = require("scripts.unit_ai").getState(uid)
            if not (s and s.sleepPhase) then
                unit.revive(uid)
            end
        end
    end
    return false
end

return M
