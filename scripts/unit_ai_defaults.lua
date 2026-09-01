-- Transient runtime defaults for one unit's aiState row (#2055).
--
-- TWO paths install a row into scripts.unit_ai_core's aiState table,
-- and before this module only one of them established the runtime
-- fields a thought tick reads before it has decided anything:
--
--   * ensureState (unit_ai_core.lua) builds the row for a unit the AI
--     is seeing for the first time, and it set all three.
--   * the save-restore apply (unit_ai_save.lua) installs each decoded
--     row VERBATIM -- saveModules.applyEntityRows deliberately knows
--     nothing about what any component's rows MEAN -- and it set none.
--
-- A row decoded from an accepted schema version need not carry them.
-- unit_ai_save_validate.lua accepts a free-form state row on purpose
-- (the AI's own fields are not a wire contract), and the tracked
-- b3-lua-versioned-session-v1 fixture's v1 payload is exactly one such
-- sparse row: `{[1] = {buildTarget = 1}}`. Installed verbatim, its
-- first live tick reached `engine.gameTime() < s.nextActionAt`
-- (unit_ai.lua) and errored on the nil -- and survived decode,
-- canonical comparison, resave, restart and reload on the way there,
-- because no step on that path ever supplies a value the writer
-- omitted. Fresh saves were never affected: snapshotUnitState persists
-- all three fields, so only an accepted LEGACY payload -- or a
-- current-format resave of one -- can be sparse.
--
-- So the defaults are declared ONCE, here, and both paths normalize
-- against them. That is what makes the fix version-independent:
-- normalization runs after the whole decode ladder, so every accepted
-- inputVersion converges on this one stage rather than each migration
-- branch needing its own back-fill.
--
-- FILL ONLY, NEVER OVERWRITE. A restored row that carries a value
-- keeps it, whatever it is -- including a `nextActionAt` in the past
-- or an unrecognized `currentAction`. This module exists to make a
-- sparse row tickable, not to reset a complete one, and a save's own
-- scheduling is the save's to state.
--
-- Deliberately NOT a module the rest of the AI requires: it is the
-- declaration two installers share, not a general accessor. Everything
-- else reads aiState rows directly, exactly as before.

local M = {}

-- The enumerated set (#2055 requirement 2), and why each member is in
-- it. "Transient runtime" here means: read by the thought tick BEFORE
-- it has scored anything, on a path with no `or` guard of its own.
--
--   * nextActionAt    -- unit_ai.lua's `engine.gameTime() <
--                        s.nextActionAt` gate, the reported failure.
--                        0 = decide on first sight, so a restored unit
--                        thinks on its next tick rather than waiting
--                        out an interval it never scheduled.
--   * actionStartedAt -- unit_ai_needs.lua's wanderUtility computes
--                        `engine.gameTime() - s.actionStartedAt`
--                        whenever the row says `currentAction ==
--                        "wander"`, so a sparse row that only got
--                        nextActionAt would move from one nil error to
--                        the next as soon as it scored actions.
--   * currentAction   -- every individual read of it is a comparison
--                        and nil-safe on its own, but it is the field
--                        that gates actionStartedAt's arithmetic above
--                        and that the switch/onExit walk compares. It
--                        is also what ensureState sets, and the point
--                        of this module is that the two installers
--                        cannot disagree about a fresh row's shape.
--
-- `commandedTask` is deliberately ABSENT: nil IS its correct value
-- (maintainTask returns early on it, and a row that carries no order
-- has none), so defaulting it would invent a command nobody issued.
-- Everything else an aiState row may hold is either optional by
-- design, guarded at its own read site, or established by the action
-- that owns it.
--
-- Values are thunks because one of them reads the clock: the default
-- must be the moment the row goes live, not module-load time.
M.FIELDS = {
    { name = "currentAction",   value = function() return "idle" end },
    { name = "actionStartedAt", value = function() return engine.gameTime() end },
    { name = "nextActionAt",    value = function() return 0 end },
}

-- Fill every missing runtime default into ONE row, in place, leaving
-- every value the row already carries untouched.
--
-- Returns the row FIRST, so a caller building a fresh one can write
-- `normalize({})` and ignore the rest; the second value says whether
-- anything was actually filled, which is what normalizeAll counts with
-- rather than scanning each row twice.
function M.normalize(s)
    local filled = false
    for _, f in ipairs(M.FIELDS) do
        if s[f.name] == nil then
            s[f.name] = f.value()
            filled = true
        end
    end
    return s, filled
end

-- normalize() over every row of a keyed table, in place. The ROWS are
-- mutated; the table itself is never rebound (consumers across the
-- script graph hold direct references to aiState, and the restore
-- path's whole contract is that its identity survives, #900). Returns
-- how many rows were missing at least one default, which is what lets
-- a legacy load say so once in the log instead of silently.
--
-- Every row here is already known to be a table: unit_ai's own
-- validator rejects a non-table state row before prepareLoad accepts
-- the payload, and the rollback path's rows come from
-- snapshotUnitState. So this deliberately carries no type guard whose
-- branch nothing could reach.
function M.normalizeAll(rows)
    local filled = 0
    for _, s in pairs(rows) do
        local _, changed = M.normalize(s)
        if changed then filled = filled + 1 end
    end
    return filled
end

return M
