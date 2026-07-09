-- Thought log data store (#351) — sibling to combat_log.lua /
-- injury_log_panel.lua, minus their standalone-panel chrome: thoughts
-- surface only via the per-unit scripts/unit_log.lua Thought tab, so this
-- module owns just the drain + per-unit ring, mirroring their DATA layer
-- (a dedicated all-units browser panel is left for a future pass if
-- wanted — not needed for a "readable per-unit log").
--
-- Loaded via engine.loadScript (see scripts/init_loader.lua) so it keeps
-- draining thought.drainEvents() in the background even while no
-- unit-log panel is open, exactly like combat_log/injury_log_panel.

local thoughtLog = package.loaded["scripts.thought_log"] or {}
package.loaded["scripts.thought_log"] = thoughtLog

local UNIT_RING_CAP = 100   -- per-unit cap; oldest entries drop first

thoughtLog.byUnit = thoughtLog.byUnit or {}   -- uid -> {ts, text}, newest-first

local THOUGHT_COLOR = { 0.80, 0.72, 1.0, 1.0 }   -- pastel violet, distinct from event/combat/injury tints

local function formatGameTimeHMS(t)
    local secs = math.floor(t or 0)
    if secs < 0 then secs = 0 end
    local hh = math.floor(secs / 3600)
    local mm = math.floor((secs % 3600) / 60)
    local ss = secs % 60
    return string.format("%02d:%02d:%02d", hh, mm, ss)
end

local function processEvent(ev)
    local uid = ev.target   -- thought.emit's ceTarget = the thinking unit
    if not uid then return end
    local list = thoughtLog.byUnit[uid]
    if not list then
        list = {}
        thoughtLog.byUnit[uid] = list
    end
    table.insert(list, 1, { ts = ev.ts or 0, text = (ev.payload and ev.payload.text) or "" })
    while #list > UNIT_RING_CAP do
        table.remove(list)
    end
end

-- Drain new thought events into the per-unit rings. Called at the
-- script's tick interval regardless of whether any unit-log panel is
-- open (see module comment above).
function thoughtLog.update(dt)
    local events = thought.drainEvents() or {}
    for _, ev in ipairs(events) do
        processEvent(ev)
    end
end

-- Rendered thought-log entries for `uid`, newest-first — the shape
-- scripts/unit_log.lua's Thought tab (and All-tab merge) expects, same
-- as combat_log.unitEntries / injury_log_panel.unitEntries.
function thoughtLog.unitEntries(uid)
    local out = {}
    if not uid then return out end
    local list = thoughtLog.byUnit[uid]
    if not list then return out end
    for _, ev in ipairs(list) do
        out[#out + 1] = {
            ts    = ev.ts,
            text  = string.format("[%s] %s", formatGameTimeHMS(ev.ts), ev.text),
            color = THOUGHT_COLOR,
        }
    end
    return out
end

return thoughtLog
