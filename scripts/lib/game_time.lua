-- Shared game-time formatting (issue #1158).
--
-- `formatGameTimeHMS` was copy-pasted byte-identically into the four
-- modules that stamp player-visible log entries -- scripts/combat_log.lua,
-- scripts/injury_log_panel.lua, scripts/thought_log.lua and
-- scripts/unit_log.lua. Those four feed the same reader (thought_log has
-- no panel of its own; its entries surface through unit_log's Thought
-- tab), so a drift in one copy would show the same clock reading two
-- different ways depending on which log was open.
--
-- NOT the only game-time format in the tree, and consolidating the
-- others would be a behaviour change, not a cleanup:
--   * `formatGameTimeHM` (combat_log, injury_log_panel) is the HH:MM tab
--     title -- a deliberately coarser contract.
--   * `scripts/event_log.lua`'s `formatGameTime` emits MM:SS below one
--     hour and an UNPADDED hour above it -- a different display contract
--     again, left alone on purpose.
--
-- Pure Lua standard library only (math + string): no consumer module, no
-- engine global, so any of the four can require it at any load point.

local gameTime = {}

-- Seconds -> "HH:MM:SS". Preserves the copies' guards exactly: a nil `t`
-- reads as 0, a negative time floors at 0, a fractional time truncates
-- toward zero, and hours past 24 keep counting rather than wrapping (a
-- three-day session reads "72:00:00", not "00:00:00").
function gameTime.formatHMS(t)
    local secs = math.floor(t or 0)
    if secs < 0 then secs = 0 end
    local hh = math.floor(secs / 3600)
    local mm = math.floor((secs % 3600) / 60)
    local ss = secs % 60
    return string.format("%02d:%02d:%02d", hh, mm, ss)
end

return gameTime
