-- Survival alerts + unit labeling for scripts/unit_resources.lua.
--
-- Survival emits three player-events flavours:
--   * survival_critical "X died of <cause>" on every unit.kill site
--     (no debouncing — each unit dies once).
--   * survival_warning "X is starving"   when the calorie store hits 0
--   * survival_warning "X is dehydrated" when hydration < 25%
--
-- Warnings are debounced with hysteresis to avoid spamming the popup
-- when a unit oscillates around the threshold (e.g. drinks a sip,
-- drains back below 25%, drinks again …). Each unit carries a
-- per-alert "active" flag; the alert fires only on the OFF→ON
-- transition, and the flag clears only when the resource recovers
-- above a higher "rearm" threshold (well clear of the trigger).
--
-- Entries are keyed by unit id. They linger after a unit is
-- destroyed via unit.destroy, which is fine — they're tiny (two
-- booleans) and bounded by total units ever spawned. Death
-- explicitly clears the entry.

local M = {}

local unitAlertState = {}  -- uid → { starvation = bool, dehydration = bool }

-- Rearm thresholds (as fractions of max). Trigger thresholds are
-- "calories ≤ 0" (the floor) and "hydration < 25%" per the design.
local STARVATION_REARM_FRAC   = 0.25  -- clear flag when calories > 25%
local DEHYDRATION_TRIGGER_FRAC = 0.25  -- fire flag when hydration < 25%
local DEHYDRATION_REARM_FRAC   = 0.50  -- clear flag when hydration > 50%

-- "acolyte" → "Acolyte". Unit defNames are all lower-snake_case
-- today; this is enough to make alert text presentable. If
-- individual unit names land later this becomes <name>.
function M.unitLabel(info)
    -- A named unit (acolyte) reads as its personal name (#264); otherwise
    -- the species label (display_name / prettified def name) so survival
    -- alerts say "Brown Bear", not "Bear_brown".
    if info and info.name and info.name ~= "" then return info.name end
    if info and info.displayName and info.displayName ~= "" then
        return info.displayName
    end
    local n = (info and info.defName) or "Unit"
    return n:sub(1, 1):upper() .. n:sub(2)
end

function M.unitCoords(info)
    if not info then return nil, nil end
    return math.floor(info.gridX or 0), math.floor(info.gridY or 0)
end

-- `channel` is optional: pass "injury" for a WOUND-caused death (it also
-- lands in the injury/medical log). Pure survival deaths (starvation /
-- thirst) omit it and stay event-only — they aren't injuries.
function M.emitDeathAlert(uid, cause, channel)
    local info = unit.getInfo(uid)
    if not info then return end
    local gx, gy = M.unitCoords(info)
    local msg = M.unitLabel(info) .. " died of " .. cause
    -- Player-facing alert (event feed / popup / pause), tagged with the
    -- unit so the per-unit log panel can filter it.
    if gx and gy then
        engine.emitEventForUnit("survival_critical", msg, uid, gx, gy)
    else
        engine.emitEventForUnit("survival_critical", msg, uid)
    end
    -- Wound-caused deaths also narrate in the injury log (the medical
    -- record). The injury-log script refines the cause from the corpse's
    -- wounds (injuries.deathCause); `cause` here is the fallback.
    if channel == "injury" and injury and injury.emit then
        injury.emit(uid, "death", cause)
    end
    -- Death clears any pending warning flags so a future re-use of
    -- the same uid (engine reassigns ids on destroy/spawn) doesn't
    -- inherit stale state.
    unitAlertState[uid] = nil
end

local function emitWarningAlert(uid, info, msg)
    if not info then info = unit.getInfo(uid) end
    if not info then return end
    local gx, gy = M.unitCoords(info)
    local fullMsg = M.unitLabel(info) .. " " .. msg
    if gx and gy then
        engine.emitEventForUnit("survival_warning", fullMsg, uid, gx, gy)
    else
        engine.emitEventForUnit("survival_warning", fullMsg, uid)
    end
end

-- Called after a resource tick has written the new value. Looks at
-- the current value (post-write), compares against this resource's
-- alert thresholds, and fires/clears the per-unit flag accordingly.
-- pose check skips dead units — they don't get warnings.
function M.checkSurvivalAlerts(uid, resourceName, value, maxVal, pose, info)
    if pose == "dead" or not maxVal or maxVal <= 0 then return end
    local state = unitAlertState[uid] or {}

    if resourceName == "calories" then
        local rearm = maxVal * STARVATION_REARM_FRAC
        if not state.starvation then
            if value <= 0 then
                emitWarningAlert(uid, info, "is starving")
                state.starvation = true
            end
        else
            if value > rearm then
                state.starvation = false
            end
        end
    elseif resourceName == "hydration" then
        local trigger = maxVal * DEHYDRATION_TRIGGER_FRAC
        local rearm   = maxVal * DEHYDRATION_REARM_FRAC
        if not state.dehydration then
            if value < trigger then
                emitWarningAlert(uid, info, "is dehydrated")
                state.dehydration = true
            end
        else
            if value > rearm then
                state.dehydration = false
            end
        end
    end

    unitAlertState[uid] = state
end

-- Translate the resourceName that triggered a kill into a
-- player-visible cause. The two real kill paths in tickResource are
-- hydration (death_threshold) and stamina (kill_on_zero from organ
-- failure when fat reserves hit min_fat). tickStarvation's
-- respiratory-failure path passes "starvation" explicitly.
function M.deathCauseFor(resourceName)
    if resourceName == "hydration" then return "dehydration"  end
    if resourceName == "stamina"   then return "starvation"   end
    return "exhaustion"
end

-- Save hook (called from unit_resources.init's deserializer): clears the
-- transient per-uid alert-debounce cache on load without writing a
-- persistent blob. Cleared IN-PLACE because the table is an upvalue
-- captured by checkSurvivalAlerts / emitDeathAlert; reassigning would
-- orphan those closures.
function M.resetOnLoad()
    for k in pairs(unitAlertState) do unitAlertState[k] = nil end
end

return M
