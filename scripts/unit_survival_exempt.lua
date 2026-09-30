-- Survival-physiology exemption for enemy unit definitions (#2754).
--
-- Enemy units ignore survival requirements: a nomad holding a ruin
-- never eats, drinks, balances salt or regulates its temperature, so it
-- can neither be delirious from nor die of any of them — and an
-- encounter it guards (death-only clearance, #916) can only clear
-- through combat. Whether enemies should later live under survival
-- mechanics is a separate design question.
--
-- What stays ACTIVE is everything combat drives: wounds, bleeding,
-- pain, blood oxygen (cardio), injury collapse, and the injury failure
-- meters (hypoxia, neuro, shock, organ, sepsis). Circulation is kept
-- live for the same reason: its blood-volume, fitness and sepsis
-- factors are injury physiology; only its cold-vasoconstriction factor
-- is thermal, and that one reads the neutral core below.
--
-- Mechanism: in place of thermo.tick and salts.tick, scripts/
-- unit_resources.lua calls M.neutralize, which pins the survival stats
-- every consumer reads (brain consciousness, cramps, sweat, the status
-- panel, the survival failure meters) at their homeostatic values.
-- Resource pools (hydration, hunger, calories), digestion and
-- starvation already only run for definitions with a
-- unit_resource_config.lua entry, and exempt definitions have none.
--
-- A loaded save can carry an occupant whose stored values had already
-- drifted before this exemption existed; unit_resources.onSaveLoaded
-- neutralizes every exempt survivor then, so it reads healthy while the
-- load is still paused, before its first physiology tick.

local thermo      = require("scripts.thermo")
local salts       = require("scripts.salts")
local circulation = require("scripts.circulation")

local M = {}

-- Per-definition: the unit definitions exempt from survival physiology.
local EXEMPT = {
    nomad_primitive = true,
}

-- The failure meters whose drivers are survival physiology
-- (scripts/unit_resource_failure.lua). Injury meters are not listed.
local SURVIVAL_METERS = { "hypothermia", "hyperthermia", "salt_imbalance" }

function M.isExempt(defName)
    return EXEMPT[defName] == true
end

-- Pin one unit's survival stats at homeostasis.
function M.neutralize(uid)
    unit.setStat(uid, "core_temp", thermo.BASELINE)
    -- Circulation is recomputed at the neutral core, so blood loss,
    -- fitness and sepsis still reach cardio's perfusion.
    unit.setStat(uid, "circulation", circulation.compute(uid, thermo.BASELINE))
    unit.setStat(uid, "salt", salts.maxSalt(uid))
    unit.setStat(uid, "salt_conc", 1.0)
    for _, stat in ipairs(SURVIVAL_METERS) do
        unit.setStat(uid, stat, 0)
    end
end

-- Neutralize every exempt unit among `uids`, then refresh the derived
-- mental values that read those stats (consciousness, concentration,
-- state of mind), so a paused session reads the state its first
-- physiology tick would produce. brain.tick at dt = 0 recomputes the
-- derived values and leaves every drifting one (mood, emotional pain,
-- caffeine) exactly where it was.
function M.neutralizeLoaded(uids)
    local brain = require("scripts.brain")
    for _, uid in ipairs(uids or {}) do
        local info = unit.getInfo(uid)
        if info and M.isExempt(info.defName)
           and unit.getPose(uid) ~= "dead" then
            M.neutralize(uid)
            brain.tick(uid, 0)
        end
    end
end

return M
