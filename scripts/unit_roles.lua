-- Derived unit roles (#265).
--
-- A role is a LABEL derived from a unit's work skills — never assigned,
-- never stored engine-side. The highest work skill at or above
-- THRESHOLD names the role; a unit with no work skill that high is a
-- generalist "laborer", and a unit with no work skills at all (wildlife,
-- technomule) has no role. Because skills grow with use (dig grants
-- mining XP, chop grants woodcutting XP, …) and the role in turn
-- weights the matching work actions' utilities, units gradually
-- specialise into whatever they're naturally good at — emergent
-- professions, no player bookkeeping.
--
-- The derived role lives on the unit's aiState (`s.role`) and is
-- recomputed every thought tick with hysteresis (SWITCH_MARGIN) so two
-- close skills can't flap the label back and forth. Nothing here needs
-- save support: skills already persist in uiSkills, and re-deriving on
-- load lands on the same answer.
--
-- Weighting contract (see the utility ladder in unit_ai.lua): weights
-- apply ONLY to work-action ENTRY utilities, never to in-progress lock
-- values (6.0). Boosting a lock would climb past player orders (7.0);
-- dampening one would let another work entry steal an in-progress job.
-- Entry utilities top out at deliver's 4.0, so ON_ROLE must stay below
-- 6.0/4.0 = 1.5 to keep every weighted entry inside the routine band.

local M = package.loaded["scripts.unit_roles"] or {}
package.loaded["scripts.unit_roles"] = M

-- Derivation order is the tie-break: earlier entries win an exact
-- skill tie, so listing is deterministic (pairs() order is not).
--
-- `family` groups the work actions a role prefers. smith has no work
-- actions yet — the craft AI is #329 — so its family ("craft") appears
-- in no ACTION_FAMILY entry and the role stays weight-neutral: a pure
-- label until crafting gives it work to favor.
M.ROLES = {
    { name = "miner",      display = "Miner",      skill = "mining",       family = "mine"  },
    { name = "woodcutter", display = "Woodcutter", skill = "woodcutting",  family = "wood"  },
    { name = "builder",    display = "Builder",    skill = "construction", family = "build" },
    { name = "smith",      display = "Smith",      skill = "smithing",     family = "craft" },
}

-- Work action → family. deliver/store/build_nearby serve build sites,
-- so they ride with the Builder. Need- and order-driven actions
-- (forage, pickup_ground, follow_command, survival, combat) are
-- deliberately absent: roles only steer ROUTINE work preference.
M.ACTION_FAMILY = {
    dig_designation       = "mine",
    chop_designation      = "wood",
    construct_job         = "build",
    build_nearby          = "build",
    deliver_to_build_site = "build",
    store_materials       = "build",
}

M.THRESHOLD     = 30.0   -- min skill to claim a specialist role
M.SWITCH_MARGIN = 5.0    -- hysteresis: challenger must beat the
                         -- incumbent (or the threshold, on demotion
                         -- to laborer) by this much
M.ON_ROLE       = 1.4    -- entry-utility boost for the role's family
M.OFF_ROLE      = 0.7    -- entry-utility damp for other work families

local byName = {}
for _, r in ipairs(M.ROLES) do byName[r.name] = r end

-- Families that actually map to work actions (smith's "craft" won't
-- appear here until #329 lands crafting work actions).
local familyHasActions = {}
for _, fam in pairs(M.ACTION_FAMILY) do familyHasActions[fam] = true end

-- Display string for a role name ("laborer" is the generalist default
-- and has no ROLES entry). nil → nil so callers can render "—".
function M.display(role)
    if not role then return nil end
    if role == "laborer" then return "Laborer" end
    local r = byName[role]
    return r and r.display or role
end

-- Re-derive s.role from the unit's current EFFECTIVE work skills
-- (getSkill includes modifiers). Called once per thought tick.
function M.update(uid, s)
    local best, bestLvl, any = nil, -1, false
    for _, r in ipairs(M.ROLES) do
        local lvl = unit.getSkill(uid, r.skill)
        if lvl then
            any = true
            if lvl > bestLvl then best, bestLvl = r.name, lvl end
        end
    end
    if not any then
        -- No work skills at all: not a worker (wildlife, technomule).
        s.role = nil
        return
    end

    local candidate = (bestLvl >= M.THRESHOLD) and best or "laborer"
    local cur = s.role
    if cur == candidate or cur == nil then
        s.role = candidate
        return
    end

    if cur == "laborer" then
        -- Promotion out of laborer: the threshold is the gate.
        s.role = candidate
        return
    end

    -- cur is a specialist role. Hysteresis:
    local curDef = byName[cur]
    local curLvl = curDef and unit.getSkill(uid, curDef.skill) or 0
    if candidate == "laborer" then
        -- Demote only when the incumbent skill has clearly sunk below
        -- the threshold (an expiring modifier hovering at 30 must not
        -- flap Miner ↔ Laborer).
        if curLvl < M.THRESHOLD - M.SWITCH_MARGIN then
            s.role = "laborer"
        end
        return
    end
    -- Specialist → different specialist: the challenger must beat the
    -- incumbent's skill by the margin, not just edge past it.
    if bestLvl >= curLvl + M.SWITCH_MARGIN then
        s.role = candidate
    end
end

-- Entry-utility multiplier for `actionName` under the unit's current
-- role. 1.0 whenever the role has no opinion: non-work actions,
-- role-less units, laborers, and roles whose family has no actions
-- yet (smith).
function M.weight(s, actionName)
    local family = M.ACTION_FAMILY[actionName]
    if not family then return 1.0 end
    local role = s and s.role
    if not role or role == "laborer" then return 1.0 end
    local r = byName[role]
    if not r then return 1.0 end
    if r.family == family then return M.ON_ROLE end
    -- Off-role damp only for roles that HAVE a work family to favor —
    -- a smith dampening dig/chop/build while owning no work actions
    -- would just idle more.
    if not familyHasActions[r.family] then return 1.0 end
    return M.OFF_ROLE
end

return M
