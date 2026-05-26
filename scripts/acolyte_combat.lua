-- Acolyte combat — animation resolver + state cache.
--
-- This module sits between game logic ("attack this target") and the
-- engine's animation pipeline ("play this animation name"). It owns
-- the lookup that decides which animation an action should use given
-- the unit's current weapon class, injury state, and combat state.
--
-- It does NOT implement the combat sim itself — no damage, no hit
-- resolution, no AI targeting. Those live in a separate (future) slice.
-- What this provides is a clean surface to TRIGGER the right animation
-- once the combat sim decides something should happen.
--
-- Usage examples (from Lua console or other scripts):
--
--   local c = require("scripts.acolyte_combat")
--   local uid = unit.getSelected()[1]
--
--   c.setInCombat(uid, true)        -- drew weapon, fists up, etc.
--   c.play(uid, "attack_quick")     -- one-shot bite/jab/slash
--   c.play(uid, "attack_heavy")     -- haymaker/overhead
--   c.play(uid, "hit_react")        -- took a hit
--   c.setInjured(uid, true)         -- crosses the wounded threshold
--   c.play(uid, "attack_quick")     -- now uses injured_attack_quick_*
--   c.play(uid, "death")            -- violent kill
--   c.play(uid, "collapse")         -- bleed-out / poison
--
-- The state cache is in-memory only. Save/load is the combat sim's
-- problem when it lands — this module currently has no durable state.

local acolyteCombat = package.loaded["scripts.acolyte_combat"] or {}
package.loaded["scripts.acolyte_combat"] = acolyteCombat

acolyteCombat.state = acolyteCombat.state or {}

-----------------------------------------------------------
-- Weapon classification
-----------------------------------------------------------
-- Decide which animation set the unit's right_hand maps to. Today:
-- steel_dagger → "RH_dagger"; anything else (incl. empty hand) →
-- "unarmed". Extend this table as new weapon classes land.
local WEAPON_CLASS = {
    steel_dagger = "RH_dagger",
}

local function classOf(uid)
    local loadout = equipment.getLoadout(uid)
    local rh      = loadout and loadout.right_hand
    if rh and rh.defName and WEAPON_CLASS[rh.defName] then
        return WEAPON_CLASS[rh.defName]
    end
    return "unarmed"
end

-----------------------------------------------------------
-- State cache (per-uid)
-----------------------------------------------------------
local function stateOf(uid)
    local s = acolyteCombat.state[uid]
    if not s then
        s = { inCombat = false, injured = false }
        acolyteCombat.state[uid] = s
    end
    return s
end

function acolyteCombat.setInCombat(uid, on)
    stateOf(uid).inCombat = on and true or false
end

function acolyteCombat.setInjured(uid, on)
    stateOf(uid).injured = on and true or false
end

function acolyteCombat.isInCombat(uid)  return stateOf(uid).inCombat end
function acolyteCombat.isInjured(uid)   return stateOf(uid).injured end

-----------------------------------------------------------
-- Animation-name resolver
-----------------------------------------------------------
-- Given a high-level action and the unit's state, return the exact
-- animation name to play. `nil` if the action / state combination
-- isn't defined (e.g. "death" isn't class-keyed, so a class lookup
-- there would be a bug).
--
-- Action set the resolver knows:
--   "idle"          — base or combat idle, depending on state
--   "attack_quick"  — bite/jab/slash, class-specific
--   "attack_heavy"  — haymaker/overhead, class-specific
--   "hit_react"     — flinch; class-specific only when in combat
--   "death"         — violent (any cause); not class-specific
--   "collapse"      — slow death (bleed/poison); not class-specific
function acolyteCombat.animFor(uid, action)
    local s     = stateOf(uid)
    local class = classOf(uid)

    if action == "idle" then
        if not s.inCombat then
            return s.injured and "injured_idle" or "idle"
        end
        local prefix = s.injured and "injured_combat_idle_" or "combat_idle_"
        return prefix .. class

    elseif action == "attack_quick" or action == "attack_heavy" then
        -- Attacks always reach for the class-specific anim. No
        -- distinct "out-of-combat attack" — if you're attacking,
        -- you're in combat (the caller should ensure setInCombat).
        local prefix = s.injured and ("injured_" .. action .. "_")
                                  or  (action .. "_")
        return prefix .. class

    elseif action == "hit_react" then
        -- Out of combat: weapon-agnostic anim. In combat: class-keyed
        -- so the hit reaction matches the stance (fists up vs dagger
        -- guard). Injured variants exist for all four buckets.
        if not s.inCombat then
            return s.injured and "injured_hit_react" or "hit_react"
        end
        local prefix = s.injured and "injured_combat_hit_react_"
                                  or  "combat_hit_react_"
        return prefix .. class

    elseif action == "death" then
        return s.injured and "injured_death" or "death"

    elseif action == "collapse" then
        return s.injured and "injured_collapse" or "collapse"
    end

    return nil
end

-----------------------------------------------------------
-- Convenience: resolve + play
-----------------------------------------------------------
function acolyteCombat.play(uid, action)
    local name = acolyteCombat.animFor(uid, action)
    if name then unit.setAnim(uid, name) end
    return name
end

return acolyteCombat
