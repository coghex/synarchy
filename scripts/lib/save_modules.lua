-- Lua-side save COMPONENT registry (issue #761, save-overhaul B3).
--
-- Replaces the old opaque `name -> blob` map (v2 Phase 5 / #757-#760's
-- transitional `"lua-state"` envelope component) with one independently
-- versioned, scoped, fail-fast component PER registered persistent Lua
-- module, mirroring the Haskell registry's own contract
-- ("World.Save.Component.Types.ComponentCodec"/"RegisteredComponent"):
--
--   saveModules.register("unit_ai", {
--       version       = 1,        -- current schema version
--       inputVersions = {1},      -- versions this reader can decode
--       required      = true,     -- required/optional (requirement 7)
--       scope         = "global", -- documented scope tag (diagnostic only)
--       deps          = {},       -- ids this component depends on -- a
--                                  -- Lua id in THIS registry, or a known
--                                  -- Haskell component id (requirement 2's
--                                  -- "dependencies on Haskell or Lua
--                                  -- components" -- see the deps
--                                  -- validation below)
--       snapshot  = function() return dataOnlyTable end,
--       decode    = function(version, data) return canonicalTable end,
--       validate  = function(data) return errorStringsOrNil end,
--       apply     = function(data, entities) ... end,
--                                              -- run only after ALL
--                                              -- required components
--                                              -- validate (requirement 11).
--                                              -- `entities` (issue #900) is
--                                              -- the restored-session entity
--                                              -- context, or nil meaning
--                                              -- "apply every row, no
--                                              -- ownership filtering" --
--                                              -- see applyEntityRows below
--       default   = function() return dataOnlyTable end, -- required
--                                              -- iff required == false
--       references = function(data) return {{kind=.., id=..}, ...} end,
--                                              -- optional, documents/
--                                              -- traverses entity refs
--   })
--
-- The module NAME stays the first positional argument (not folded into
-- the spec table) so the static persistence-inventory audit
-- (tools/persistence_inventory_audit.py) keeps tracing registrations
-- exactly as it always has -- only the SECOND argument's shape changed.
--
-- A module with no durable state at all never calls `register` --
-- it uses `saveModules.registerResetHook(id, resetFn)` instead, which
-- is NOT a save component (no version, no envelope entry) and gets a
-- fresh call to `resetFn()` on every load, mirroring "session-
-- replacement", never save-time. Post-publication lifecycle behavior
-- (recomputing derived stats once entities exist, etc.) is a SEPARATE,
-- pre-existing mechanism -- the engine broadcasts `onSaveLoaded(...)`
-- to every loaded module directly (see
-- "Engine.Scripting.Lua.Thread.Dispatch"), independent of this
-- registry entirely; this file has no opinion on it.
--
-- Haskell drives this registry through four entry points, called from
-- "Engine.Scripting.Lua.API.Save":
--   describeAll()          -- {id,version,required} for every registered
--                              persistent component, BEFORE encode/decode
--                              (used to build the envelope's known/
--                              required id sets, requirement 12/13)
--   snapshotAll()           -- {ok=true, components={{id,version,payload},..},
--                              references={{component=,kind=,id=,owner=,
--                              path=,page=},..}}
--                              or {ok=false, error=...} -- a REQUIRED
--                              component's snapshot/validate/encode
--                              failure aborts the WHOLE save (requirement
--                              6); an OPTIONAL one is omitted with a
--                              logged warning. validate() (round-6
--                              review, issue #764) runs here too, on the
--                              SAME just-snapshotted data prepareLoad's
--                              validate() call checks on the load side --
--                              a malformed live state (a mutated
--                              reference field, say) is now caught before
--                              it can ever be written to disk, not only
--                              discovered as a dropped/malformed edge on
--                              a later load.
--                              references (issue #764, save-overhaul C3)
--                              is every reference edge collected the SAME
--                              way prepareLoad's are -- the caller cross-
--                              validates these against the just-captured
--                              live snapshot.
--                              (requirement 7)
--   prepareLoad(components, requestId, isMigratingLegacyBaseline,
--               restoredEntities) --
--                              {ok=true, references={...}} or
--                              {ok=false, errors={...}} -- decode +
--                              migrate + component-local-validate EVERY
--                              component with NO live mutation
--                              (requirement 11); all-or-nothing.
--                              requestId is stashed alongside the
--                              prepared data so a later
--                              abortPreparedLoad(requestId) can tell a
--                              stale cleanup for an OLD request apart
--                              isMigratingLegacyBaseline (issue #766,
--                              save-overhaul C4) defaults every
--                              component absent from `components`
--                              instead of hard-failing on "missing" --
--                              set only for a recognized pre-#760
--                              compatibility migration, which predates
--                              every currently-registered component.
--                              from state a NEWER request just prepared.
--                              references (issue #764, save-overhaul C3)
--                              is every {component=,kind=,id=,owner=,
--                              path=,page=} edge every registered
--                              component's references() hook reported,
--                              flattened --
--                              the caller cross-validates these against
--                              the loaded session's real entity sets.
--                              restoredEntities (issue #900) is that same
--                              entity set, handed IN so each component's
--                              apply() can resolve per-entity ownership --
--                              stashed alongside the prepared data and
--                              consumed by the applyAll() below, which is
--                              why the prepare->apply gap needs no carrier
--                              of its own. Optional: absent means "no
--                              ownership filtering" (see applyEntityRows).
--   applyAll()              -- apply the prepared, already-validated
--                              data (only reachable after prepareLoad
--                              returned ok=true), then run every
--                              registered reset hook
--
-- Payloads are canonical, data-only bytes from "scripts.lib.data_codec"
-- (requirement 8) -- never a Lua expression compiled via load().
--
-- Singleton via package.loaded so the engine.loadScript update tick and
-- any test harness requiring this module see the same registry.

local dataCodec = require("scripts.lib.data_codec")

local saveModules = package.loaded["scripts.lib.save_modules"] or {}
package.loaded["scripts.lib.save_modules"] = saveModules

saveModules.registry    = saveModules.registry or {}     -- id -> spec
saveModules.resetHooks  = saveModules.resetHooks or {}    -- id -> fn
saveModules._captureActive = false
saveModules._loadActive     = false
saveModules._pendingApply   = nil
saveModules._pendingRequestId = nil
-- Issue #900: the restored session's entity context, stashed by
-- prepareLoad and consumed by applyAll. nil (no context supplied) means
-- "apply every row" -- deliberately DISTINCT from a present-but-empty
-- context, which means "the restored session has no entities of that
-- kind, so drop every row".
saveModules._pendingEntities = nil

local VALID_ID_PATTERN = "^[a-z][a-z0-9_]*$"

-- Every version travels to Haskell as a Word32 (issue #761 round-4
-- review): `math.huge` passes Lua's own `n == math.floor(n)` check
-- (floor(inf) is inf) and is `>= 1`, so it slipped past the original
-- "positive integer" check below undetected -- then HsLua's
-- `tointeger` silently fails to convert it, and the whole component
-- record was DROPPED from the array read (see
-- Engine.Scripting.Lua.API.Save's readLuaArrayAt), which could make a
-- REQUIRED component vanish from a save instead of failing it. Reject
-- any version outside Word32's representable range here, at
-- registration, before it can ever reach that bridge.
local WORD32_MAX = 4294967295

local function isFn(f) return type(f) == "function" end

local function isValidComponentVersion(v)
    return type(v) == "number" and v == math.floor(v)
        and v >= 1 and v <= WORD32_MAX
end

-- Requirement 2's `deps` is "dependencies on Haskell OR Lua components" --
-- a Lua component genuinely can depend on a Haskell-owned one (round-6
-- review wrongly assumed this registry only needed Lua-to-Lua deps).
-- Haskell's own registry ("World.Save.Component.Types"'s
-- *ComponentId values, World.Save.Component.saveComponentRegistry) has
-- no Lua-visible introspection, so this is a deliberately hand-kept
-- mirror of that FIXED set of top-level component ids -- add an entry
-- here whenever a new one is added there. A Lua component depending on
-- one of these never affects saveModules.dependencyOrder's Lua-internal
-- apply ordering (a non-Lua-registered id is never treated as a
-- blocker there, since every Lua apply() already runs strictly before
-- the Haskell-side restore, unconditionally -- see the register()
-- comment on deps' scope) -- it exists purely so the dependency is
-- DECLARED and its id validated, per requirement 2/3.
local HASKELL_COMPONENT_IDS = {
    ["metadata"] = true, ["core-session"] = true, ["texture-palette"] = true,
    ["world-pages"] = true, ["world-edits"] = true, ["world-activity"] = true,
    ["buildings"] = true, ["units"] = true, ["unit-sim"] = true,
    ["craft-bills"] = true, ["power-nodes"] = true,
}

-- A genuine dense array: integer keys 1..n with no gaps and no other
-- key types -- rejects an associative/sparse table a caller may have
-- intended as an array but mistyped (e.g. `{hibernate = 'x'}`), which
-- `ipairs` would otherwise silently skip everywhere deps is consumed.
local function isDenseArray(t)
    local n = #t
    local count = 0
    for k in pairs(t) do
        count = count + 1
        if type(k) ~= "number" or k ~= math.floor(k) or k < 1 or k > n then
            return false
        end
    end
    return count == n
end

local function sortedIds(t)
    local ids = {}
    for id in pairs(t) do ids[#ids + 1] = id end
    table.sort(ids)
    return ids
end

-- Topologically order the registry by `deps` (Kahn-style, tie-broken by
-- id so the result is deterministic regardless of registration order --
-- requirement 3's "canonical" ordering). Returns (order) on success, or
-- (nil, remainingIds) naming every id that never became ready -- a
-- genuine dependency cycle.
function saveModules.dependencyOrder()
    local ids = sortedIds(saveModules.registry)
    local idSet = {}
    for _, id in ipairs(ids) do idSet[id] = true end
    local emitted = {}
    local order = {}
    local remaining = ids
    while #remaining > 0 do
        local nextRemaining = {}
        local progressed = false
        for _, id in ipairs(remaining) do
            local reg = saveModules.registry[id]
            local ready = true
            for _, d in ipairs(reg.deps) do
                if idSet[d] and not emitted[d] then ready = false end
            end
            if ready then
                order[#order + 1] = id
                emitted[id] = true
                progressed = true
            else
                nextRemaining[#nextRemaining + 1] = id
            end
        end
        if not progressed then
            return nil, nextRemaining
        end
        remaining = nextRemaining
    end
    return order, nil
end

-- Build-time-equivalent registry invariants (requirement 3), re-checked
-- on every save/load since Lua registration happens incrementally as
-- scripts load: every declared dependency resolves to a REGISTERED Lua
-- persistent component or a known Haskell one (requirement 2's
-- "dependencies on Haskell or Lua components" -- see register()'s
-- deps comment), and the Lua-to-Lua dependency graph has no cycle.
-- Empty list = well-formed.
function saveModules.registryStaticErrors()
    local errs = {}
    local ids = sortedIds(saveModules.registry)
    local idSet = {}
    for _, id in ipairs(ids) do idSet[id] = true end
    for _, id in ipairs(ids) do
        local reg = saveModules.registry[id]
        for _, d in ipairs(reg.deps) do
            if not idSet[d] and not HASKELL_COMPONENT_IDS[d] then
                errs[#errs + 1] = "component '" .. id
                    .. "' depends on unregistered '" .. tostring(d) .. "'"
            end
        end
    end
    local _, cyc = saveModules.dependencyOrder()
    if cyc then
        errs[#errs + 1] = "dependency cycle among: " .. table.concat(cyc, ", ")
    end
    return errs
end

-- Register a persistent Lua save component. `id` is the stable
-- identifier (first positional argument, string); `spec` declares its
-- full contract. Fails registration outright (via error()) rather than
-- silently overwriting or defaulting on any invalid input -- a
-- duplicate id, an invalid id/version, or a missing required callback
-- (requirement 3).
function saveModules.register(id, spec)
    if saveModules._captureActive or saveModules._loadActive then
        error("saveModules.register: cannot register '" .. tostring(id)
            .. "' while a save/load is in progress")
    end
    if type(id) ~= "string" or not id:match(VALID_ID_PATTERN) then
        error("saveModules.register: invalid component id "
            .. tostring(id) .. " (must match " .. VALID_ID_PATTERN .. ")")
    end
    if saveModules.registry[id] or saveModules.resetHooks[id] then
        error("saveModules.register: duplicate component id '" .. id .. "'")
    end
    if type(spec) ~= "table" then
        error("saveModules.register: spec for '" .. id .. "' must be a table")
    end

    local version = spec.version
    if not isValidComponentVersion(version) then
        error("saveModules.register: '" .. id
            .. "' version must be a positive integer representable as a "
            .. "32-bit unsigned value (1.." .. WORD32_MAX .. ")")
    end

    -- Requirement 2 (round-5 review): inputVersions must be an EXPLICIT
    -- declaration of every schema version this component's decode() can
    -- still read, not silently defaulted to "just the current version" --
    -- a defaulted registration would look identical to one that
    -- deliberately dropped support for reading its own prior saves, with
    -- no signal at registration time that support was never declared.
    -- Round-8 review: also must be a genuine dense array (same
    -- isDenseArray check as deps below) -- a sparse/associative table
    -- like {1, [3] = 2} previously registered successfully and then
    -- silently dropped version 2 everywhere inputVersions is consumed
    -- via ipairs (isVersionSupported, hasCurrentVersion below), exactly
    -- the deps bug fixed in round 7.
    local inputVersions = spec.inputVersions
    if type(inputVersions) ~= "table" or not isDenseArray(inputVersions)
            or #inputVersions == 0 then
        error("saveModules.register: '" .. id
            .. "' must declare inputVersions as a non-empty dense array "
            .. "(no default, no associative/sparse table) -- list every "
            .. "schema version this component's decode() can still read")
    end
    local hasCurrentVersion = false
    for _, v in ipairs(inputVersions) do
        if not isValidComponentVersion(v) then
            error("saveModules.register: '" .. id
                .. "' inputVersions must all be positive integers "
                .. "representable as a 32-bit unsigned value (1.."
                .. WORD32_MAX .. ")")
        end
        if v == version then hasCurrentVersion = true end
    end
    if not hasCurrentVersion then
        error("saveModules.register: '" .. id
            .. "' inputVersions must include its own current version")
    end

    if type(spec.required) ~= "boolean" then
        error("saveModules.register: '" .. id .. "' required must be a boolean")
    end

    if not isFn(spec.snapshot) or not isFn(spec.decode)
            or not isFn(spec.validate) or not isFn(spec.apply) then
        error("saveModules.register: '" .. id
            .. "' must supply snapshot/decode/validate/apply functions")
    end

    if not spec.required and not isFn(spec.default) then
        error("saveModules.register: optional component '" .. id
            .. "' must supply a default() function")
    end

    if spec.references ~= nil and not isFn(spec.references) then
        error("saveModules.register: '" .. id
            .. "' references must be a function when supplied")
    end

    -- Requirement 2: every registration DECLARES its scope explicitly
    -- (global / per-page / per-entity / other documented) -- no silent
    -- default. Deliberately not restricted to a fixed enum (the
    -- requirement's own wording allows "or other documented scope"),
    -- but it must be a genuine, non-empty declaration.
    if type(spec.scope) ~= "string" or spec.scope == "" then
        error("saveModules.register: '" .. id
            .. "' must declare a non-empty scope string (e.g. 'global', "
            .. "'per-page', 'per-entity')")
    end

    -- Requirement 2: like scope/inputVersions, deps is an EXPLICIT
    -- declaration -- no silent "omitted means no dependencies" default,
    -- so a component that genuinely has none still says so (deps = {})
    -- rather than leaving the question unanswered. Requirement 2 is
    -- explicit that this covers "dependencies on Haskell or Lua
    -- components" (round-7 review correction -- an earlier version of
    -- this comment wrongly claimed deps was Lua-registry-local only):
    -- each entry must be either another id in THIS registry (checked
    -- against the live Lua registry by registryStaticErrors below,
    -- since Lua registration is incremental and a same-run sibling may
    -- not exist yet) or one of HASKELL_COMPONENT_IDS' fixed top-level
    -- ids (checked immediately -- that set never changes mid-run). A
    -- Haskell-id dependency never participates in
    -- saveModules.dependencyOrder's Lua-internal Kahn ordering (a dep
    -- outside the live Lua id set is never treated as a blocker there)
    -- because ordering between the two registries is itself a
    -- structural invariant, not something a topological sort needs to
    -- enforce: every Lua component's apply() (saveModules.applyAll(),
    -- via Engine.Scripting.Lua.API.Save's applyLuaLoad) always runs
    -- strictly BEFORE the Haskell-side live session replacement is ever
    -- queued (issue #763's WorldLoadPublish -- staging, WorldLoadTransaction,
    -- touches no live state and may run before OR after this, since it
    -- doesn't observe or mutate anything either side could disagree on),
    -- for every load, with no exception. The declaration still matters --
    -- documenting a real cross-language coupling, and rejecting a
    -- typo'd/nonexistent id outright, same as a bad Lua-to-Lua dep.
    local deps = spec.deps
    if type(deps) ~= "table" or not isDenseArray(deps) then
        error("saveModules.register: '" .. id
            .. "' must declare deps as a dense array of component id "
            .. "strings (possibly empty -- no default, and no "
            .. "associative/sparse table)")
    end
    for _, d in ipairs(deps) do
        if type(d) ~= "string" or d == "" then
            error("saveModules.register: '" .. id
                .. "' deps entries must be non-empty component id strings")
        end
    end

    saveModules.registry[id] = {
        id             = id,
        version        = version,
        inputVersions  = inputVersions,
        required       = spec.required,
        scope          = spec.scope,
        deps           = deps,
        snapshot       = spec.snapshot,
        decode         = spec.decode,
        validate       = spec.validate,
        apply          = spec.apply,
        default        = spec.default,
        references     = spec.references,
    }
end

-- Register a module with NO durable state (requirement 4): `resetFn`
-- runs once per load (session replacement), never produces a save
-- component, and is never optional-with-default -- there is nothing to
-- default, only a cache to clear.
function saveModules.registerResetHook(id, resetFn)
    if saveModules._captureActive or saveModules._loadActive then
        error("saveModules.registerResetHook: cannot register '" .. tostring(id)
            .. "' while a save/load is in progress")
    end
    if type(id) ~= "string" or not id:match(VALID_ID_PATTERN) then
        error("saveModules.registerResetHook: invalid id " .. tostring(id)
            .. " (must match " .. VALID_ID_PATTERN .. ")")
    end
    if saveModules.registry[id] or saveModules.resetHooks[id] then
        error("saveModules.registerResetHook: duplicate id '" .. id .. "'")
    end
    if not isFn(resetFn) then
        error("saveModules.registerResetHook: '" .. id
            .. "' resetFn must be a function")
    end
    saveModules.resetHooks[id] = resetFn
end

-- {id, version, required} for every registered persistent component, in
-- canonical (id-ascending) order -- used by Haskell to build the
-- envelope's dynamic known/required id sets before encode/decode.
function saveModules.describeAll()
    local out = {}
    for _, id in ipairs(sortedIds(saveModules.registry)) do
        local reg = saveModules.registry[id]
        out[#out + 1] = { id = reg.id, version = reg.version, required = reg.required }
    end
    return out
end

local function snapshotAllImpl()
    local structErrs = saveModules.registryStaticErrors()
    if #structErrs > 0 then
        return { ok = false, error = "registry error: "
            .. table.concat(structErrs, "; ") }
    end
    local components = {}
    -- Issue #764 (save-overhaul C3): collected the SAME way
    -- prepareLoadImpl collects them on the load side -- save and load
    -- cross-validate Lua references against one shared graph, not two
    -- independently-decided ones (Engine.Scripting.Lua.API.Save /
    -- World.Save.Integrity consume this on the Haskell side).
    local referenceEdges = {}
    for _, id in ipairs(sortedIds(saveModules.registry)) do
        local reg = saveModules.registry[id]
        local ok, dataOrErr = pcall(reg.snapshot)
        local validateErr = nil
        if ok then
            -- Round-6 review (issue #764): validate() used to run ONLY on
            -- the load side (prepareLoadImpl below) -- a save never
            -- checked its OWN freshly-snapshotted data against the same
            -- rule the load path would reject it under. A live state
            -- mutated into a malformed shape (e.g. attackTargetUid set to
            -- a non-numeric value by some other bug) would snapshot,
            -- encode, and WRITE to disk untouched -- only surfacing as a
            -- silently-dropped reference edge on a LATER load, never as a
            -- save-time failure. Runs on the SAME already-wrapped shape
            -- validate() already expects (reg.snapshot() for unit_ai/
            -- building_spawn returns wrapAiState(...)/wrapAllLastUid(...)
            -- output, identical in shape to what decode() produces on the
            -- load side), so no shape mismatch.
            local vok, verrs = pcall(reg.validate, dataOrErr)
            if not vok then
                validateErr = "'" .. id .. "': validate crashed: " .. tostring(verrs)
            elseif verrs ~= nil and #verrs > 0 then
                validateErr = "'" .. id .. "' failed validation: "
                    .. table.concat(verrs, "; ")
            end
        end
        if not ok then
            if reg.required then
                return { ok = false, error = "'" .. id
                    .. "' snapshot failed: " .. tostring(dataOrErr) }
            end
            engine.logWarn("saveModules: optional component '" .. id
                .. "' snapshot failed, omitting from save: "
                .. tostring(dataOrErr))
        elseif validateErr ~= nil then
            if reg.required then
                return { ok = false, error = validateErr }
            end
            engine.logWarn("saveModules: optional component '" .. id
                .. "' snapshot failed validation, omitting from save: "
                .. validateErr)
        else
            local payload, encErr = dataCodec.encode(dataOrErr)
            if payload == nil then
                if reg.required then
                    return { ok = false, error = "'" .. id
                        .. "' failed to encode: " .. tostring(encErr) }
                end
                engine.logWarn("saveModules: optional component '" .. id
                    .. "' failed to encode, omitting from save: "
                    .. tostring(encErr))
            else
                components[#components + 1] = { id = id, version = reg.version,
                    required = reg.required, payload = payload }
                if reg.references then
                    local refsOk, refsOrErr = pcall(reg.references, dataOrErr)
                    if not refsOk then
                        if reg.required then
                            return { ok = false, error = "'" .. id
                                .. "': references() crashed: "
                                .. tostring(refsOrErr) }
                        end
                        engine.logWarn("saveModules: optional component '"
                            .. id .. "' references() crashed, omitting its "
                            .. "edges: " .. tostring(refsOrErr))
                    elseif type(refsOrErr) == "table" then
                        for _, r in ipairs(refsOrErr) do
                            if type(r) == "table" and r.kind ~= nil
                                    and r.id ~= nil then
                                referenceEdges[#referenceEdges + 1] =
                                    { component = id, kind = r.kind,
                                      id = r.id, owner = r.owner,
                                      path = r.path, page = r.page }
                            end
                        end
                    end
                end
            end
        end
    end
    return { ok = true, components = components, references = referenceEdges }
end

-- Snapshot every registered persistent component (requirement 6/10):
-- a REQUIRED component's snapshot/encode failure aborts the whole save
-- (the caller must treat {ok=false} as "write nothing"); an OPTIONAL
-- one is omitted from the result with a logged warning.
function saveModules.snapshotAll()
    saveModules._captureActive = true
    local ok, result = pcall(snapshotAllImpl)
    saveModules._captureActive = false
    if not ok then
        return { ok = false, error = tostring(result) }
    end
    return result
end

local function isVersionSupported(reg, version)
    for _, v in ipairs(reg.inputVersions) do
        if v == version then return true end
    end
    return false
end

local function prepareLoadImpl(componentsList, isMigratingLegacyBaseline)
    local structErrs = saveModules.registryStaticErrors()
    if #structErrs > 0 then
        return { ok = false, errors = structErrs }
    end
    local byId = {}
    for _, c in ipairs(componentsList or {}) do
        byId[c.id] = c
    end
    local errors = {}
    local prepared = {}
    -- Issue #764 (save-overhaul C3): every reference a component's
    -- references() hook reports is collected here (component id +
    -- kind + id, flattened across every registered component) and
    -- handed back to the caller, which cross-validates them against
    -- the loaded session's real entity sets
    -- (Engine.Scripting.Lua.API.Save / World.Save.Integrity) --
    -- #761 only ever CALLED references() to catch a crash; the
    -- returned list itself was discarded until now.
    local referenceEdges = {}
    for _, id in ipairs(sortedIds(saveModules.registry)) do
        local reg = saveModules.registry[id]
        local entry = byId[id]
        if entry == nil and isMigratingLegacyBaseline then
            -- Issue #766 (save-overhaul C4) requirement 5: a recognized
            -- pre-#760 compatibility migration predates EVERY currently-
            -- registered persistent Lua component, so "missing" here
            -- means "this baseline is honestly older than this
            -- component", never a corrupt/incomplete save -- reuse the
            -- component's own decode() with no data (every registered
            -- decode() already tolerates a nil/absent payload, the same
            -- contract an ordinary v1-payload-with-no-optional-fields
            -- already relies on) rather than the hard-required error
            -- below, giving an honest empty-state default without
            -- requiring every component to ALSO define its own
            -- optional-only default().
            local dok, decoded = pcall(reg.decode, reg.version, nil)
            if not dok then
                errors[#errors + 1] = "'" .. id
                    .. "': decode(version, nil) failed while defaulting a "
                    .. "pre-existing compatibility baseline: " .. tostring(decoded)
            else
                local vok, verrs = pcall(reg.validate, decoded)
                if not vok then
                    errors[#errors + 1] = "'" .. id .. "': validate crashed: "
                        .. tostring(verrs)
                elseif verrs ~= nil and #verrs > 0 then
                    for _, e in ipairs(verrs) do
                        errors[#errors + 1] = "'" .. id .. "': " .. tostring(e)
                    end
                else
                    prepared[id] = decoded
                end
            end
        elseif entry == nil then
            if reg.required then
                errors[#errors + 1] = "'" .. id
                    .. "': required component missing from save"
            else
                local ok, def = pcall(reg.default)
                if ok then
                    prepared[id] = def
                else
                    errors[#errors + 1] = "'" .. id
                        .. "': default() failed: " .. tostring(def)
                end
            end
        elseif not isVersionSupported(reg, entry.version) then
            errors[#errors + 1] = "'" .. id .. "': unsupported schema version "
                .. tostring(entry.version) .. " (reader supports "
                .. table.concat(reg.inputVersions, ",") .. ")"
        else
            local data, derr = dataCodec.decode(entry.payload)
            if data == nil and derr ~= nil then
                errors[#errors + 1] = "'" .. id .. "': malformed payload: "
                    .. tostring(derr)
            else
                local dok, decoded = pcall(reg.decode, entry.version, data)
                if not dok then
                    errors[#errors + 1] = "'" .. id .. "': decode failed: "
                        .. tostring(decoded)
                else
                    local vok, verrs = pcall(reg.validate, decoded)
                    if not vok then
                        errors[#errors + 1] = "'" .. id .. "': validate crashed: "
                            .. tostring(verrs)
                    elseif verrs ~= nil and #verrs > 0 then
                        for _, e in ipairs(verrs) do
                            errors[#errors + 1] = "'" .. id .. "': " .. tostring(e)
                        end
                    else
                        -- Requirement 11/12: a declared reference schema is
                        -- actually TRAVERSED here, not merely stored and left
                        -- dead -- a crash in references() itself (a real bug
                        -- in the traversal, e.g. indexing a nil claim table)
                        -- is a validate-phase failure. The traversal result
                        -- is not cross-checked against anything here: a
                        -- dangling entry is tolerated by design (#761
                        -- issue-review clarification) and cleared at
                        -- apply/reconcile time instead.
                        local refsOk, refsErr = true, nil
                        if reg.references then
                            refsOk, refsErr = pcall(reg.references, decoded)
                        end
                        if not refsOk then
                            errors[#errors + 1] = "'" .. id
                                .. "': references() crashed: " .. tostring(refsErr)
                        else
                            prepared[id] = decoded
                            if reg.references and type(refsErr) == "table" then
                                for _, r in ipairs(refsErr) do
                                    if type(r) == "table" and r.kind ~= nil
                                            and r.id ~= nil then
                                        referenceEdges[#referenceEdges + 1] =
                                            { component = id, kind = r.kind,
                                              id = r.id, owner = r.owner,
                                              path = r.path, page = r.page }
                                    end
                                end
                            end
                        end
                    end
                end
            end
        end
    end
    if #errors > 0 then
        return { ok = false, errors = errors }
    end
    return { ok = true, prepared = prepared, references = referenceEdges }
end

-- Decode + migrate + component-locally-validate EVERY registered
-- component from `componentsList` ({id,version,payload}, as reported by
-- the envelope decode) with NO live mutation (requirement 11):
-- all-or-nothing. On success, stashes the prepared data for the
-- following `applyAll()` call and returns {ok=true}; on any failure,
-- returns {ok=false, errors={...}} and stashes nothing, so a caller
-- that aborts the load can never accidentally apply a partial result.
-- `isMigratingLegacyBaseline` (issue #766, save-overhaul C4) defaults
-- every component absent from `componentsList` instead of hard-failing
-- on "missing" -- see `prepareLoadImpl`.
-- `restoredEntities` (issue #900) is the entity context of the session
-- being loaded, stashed here for the following applyAll() -- the same
-- prepare->apply gap `_pendingRequestId` already spans, which is why the
-- caller needs no carrier of its own across the world-thread staging
-- round trip that separates the two calls.
function saveModules.prepareLoad(componentsList, requestId,
                                 isMigratingLegacyBaseline, restoredEntities)
    saveModules._loadActive = true
    saveModules._pendingApply = nil
    saveModules._pendingRequestId = nil
    saveModules._pendingEntities = nil
    local ok, result = pcall(prepareLoadImpl, componentsList, isMigratingLegacyBaseline)
    if not ok then
        saveModules._loadActive = false
        return { ok = false, errors = { tostring(result) } }
    end
    if result.ok then
        saveModules._pendingApply = result.prepared
        saveModules._pendingRequestId = requestId
        saveModules._pendingEntities = restoredEntities
    else
        saveModules._loadActive = false
    end
    return { ok = result.ok, errors = result.errors, references = result.references }
end

-- Round 6 review: a successful `prepareLoad` leaves `_loadActive` true
-- (by design -- it stays active until `applyAll` commits it), which is
-- exactly what makes `saveModules.register`/`registerResetHook` refuse
-- to run mid-load. But `applyAll` is the ONLY other thing that ever
-- clears it, and staging (World.Load.Stage, on the world thread) runs
-- AFTER a successful prepareLoad and BEFORE applyAll ever gets called --
-- a staging failure (a worldgen exception, an internal StageError) or a
-- publish-barrier failure (Engine.Save.Barrier timing out waiting for
-- other owners) previously left NEITHER called, wedging `_loadActive`
-- true forever: every later save/load's own prepareLoad, and any
-- ordinary saveModules.register call (e.g. a hot-reloaded script),
-- would fail from that point on for the rest of the session. Call this
-- from every such failure path to abort the prepared-but-never-applied
-- load cleanly -- a no-op (but still safe to call) if nothing is
-- pending.
--
-- Round 9 review: a staging failure is reported to the Lua thread as a
-- QUEUED message (LuaLoadStagingFailed), not a direct call -- it can sit
-- in the queue for a while after the failing request has already been
-- made terminal on the world/engine side (Engine.Load.Status.failLoad).
-- Terminal means the mutual-exclusion gate is open again, so a BRAND
-- NEW request can be accepted and successfully run its own prepareLoad
-- before that stale queued message is ever processed. If this function
-- cleared unconditionally, the stale cleanup for the OLD request would
-- wipe out the NEW request's already-prepared `_pendingApply`. Passing
-- the requestId the caller believes it's aborting -- compared against
-- whatever prepareLoad most recently stashed -- makes a stale abort a
-- no-op instead: it only ever clears state that actually belongs to it.
-- A nil requestId (a caller with no request in play, e.g. tests) always
-- clears, matching the pre-#763-round-9 unconditional behavior.
function saveModules.abortPreparedLoad(requestId)
    if requestId ~= nil and saveModules._pendingRequestId ~= nil
        and requestId ~= saveModules._pendingRequestId then
        return
    end
    saveModules._pendingApply = nil
    saveModules._pendingRequestId = nil
    saveModules._pendingEntities = nil
    saveModules._loadActive = false
end

-- Apply one component's per-entity ROWS into its live singleton, issue
-- #900's replacement for the whole-singleton clobber every per-entity
-- component used to open its apply() with.
--
-- `live` is the module's live table, mutated IN PLACE (never rebound --
-- consumers all over the script graph hold direct references to it, and
-- a fresh table would silently orphan every one of them). `rows` is the
-- decoded payload keyed by the SESSION-GLOBAL entity id, already in
-- live shape. `entities` is applyAll's restored-entity context, or nil.
-- `opts.kind` names which id set to resolve against ("unit"/"building" --
-- the same reference-kind vocabulary the references() hooks and
-- World.Save.Integrity already speak); `opts.component` is the id used
-- in diagnostics.
--
-- `live` is CLEARED first, so the module ends up holding EXACTLY the
-- applicable rows. This is not incidental: ids are session-global
-- allocators that restart per session, so merging rows into a live table
-- would let the new session's unit 7 silently inherit the OLD session's
-- unit 7 state whenever the payload has no row of its own for it -- and
-- the survivor-set rebuild in onSaveLoaded could not catch it either,
-- because 7 IS a survivor. The old wholesale clobber got this right by
-- construction; a per-entity rewrite has to keep it deliberately.
--
-- A row whose owner is absent from the restored session is DROPPED with
-- a warning rather than applied (the persistence contract's tolerated-
-- dangling rule -- never a load failure). Note this is a DEFENSIVE path,
-- not a fix for an observable bug: World.Load.Stage aborts a load whose
-- entities can't be reconstructed, and every registered snapshot()
-- already stores live entities only, so an ownerless row does not arise
-- from a real save/load. Returns the number of rows dropped.
function saveModules.applyEntityRows(live, rows, entities, opts)
    opts = opts or {}
    local label = opts.component or "<component>"
    local kind  = opts.kind
    -- nil context => no filtering (see _pendingEntities). An EMPTY set
    -- for this kind is a real answer and does filter.
    local known = nil
    if entities ~= nil and kind ~= nil then
        known = entities[kind]
    end

    for k in pairs(live) do live[k] = nil end

    local dropped = 0
    for id, row in pairs(rows or {}) do
        if known ~= nil and not known[id] then
            dropped = dropped + 1
            engine.logWarn("saveModules: '" .. label .. "' dropped saved state for "
                .. tostring(kind) .. " " .. tostring(id)
                .. " -- absent from the restored session")
        else
            live[id] = row
        end
    end
    return dropped
end

-- Issue #1200: the ONE place a load-abort diagnostic is worded, so the
-- "the old session was restored" and "the live Lua session may be
-- MIXED" dispositions can never drift apart between `applyAll`'s abort
-- paths. `failures` is `rollbackApplied`'s aggregate -- an EMPTY list
-- means every restore actually completed, which is the only condition
-- under which `restoredClaim` (the historical complete-rollback wording,
-- preserved verbatim per requirement 1) may be asserted at all.
--
-- The `ROLLBACK FAILED` token is a deliberate, stable contract with the
-- Haskell load path, not incidental prose: `applyLuaLoad` propagates
-- this whole message into `engine.getLoadStatus()`'s failure text and
-- the `CatWorld` warning, and that token is what tells an operator (and
-- the gate in Test.Headless.Lua.SaveModules) that the process is now
-- running an OLD Haskell session against partly-NEW Lua singletons
-- rather than a cleanly aborted load. Don't reword it casually.
--
-- Every value is stringified HERE rather than as `rollbackApplied`
-- collects them, so nothing can throw mid-unwind and strand the registry
-- before the caller has cleared its transaction bookkeeping (issue
-- #864's recovery contract) -- which is why that function aggregates raw
-- error VALUES. Deferring the rendering is only half of it though:
-- `abortMessage` builds the argument to `error(...)`, so a render that
-- threw HERE would propagate in place of this whole diagnostic and lose
-- exactly what #1200 exists to report. `safeText` below is what makes
-- that impossible.
--
-- `error()` accepts ANY Lua value, so an error object can be a table or
-- userdata whose `__tostring` metamethod itself throws (or returns a
-- non-string, which Lua then raises on). Rendering one must therefore
-- never be able to fail: an unrenderable value degrades to a placeholder
-- so the surrounding aggregate -- the ROLLBACK FAILED tag, the
-- mixed-session statement, every other component's id, and the original
-- forward failure -- still reaches the load path intact.
local function safeText(value)
    local rendered, text = pcall(tostring, value)
    if rendered and type(text) == "string" then
        return text
    end
    return "<unrenderable error value>"
end

local function abortMessage(prefix, restoredClaim, failures, forwardErr)
    if #failures == 0 then
        return prefix .. ", " .. restoredClaim .. ": " .. safeText(forwardErr)
    end
    local named = {}
    for i = 1, #failures do
        named[#named + 1] = safeText(failures[i].id)
            .. " (" .. safeText(failures[i].err) .. ")"
    end
    return prefix .. " and ROLLBACK FAILED -- the live Lua session may be "
        .. "MIXED (the old Haskell session paired with partly-new Lua "
        .. "state); restore failed for " .. #failures .. " component(s): "
        .. table.concat(named, "; ")
        .. " -- original failure: " .. safeText(forwardErr)
end

-- Apply the load prepared by the most recent successful `prepareLoad`,
-- in dependency order, then run every registered reset hook (session-
-- replacement for modules with no durable state). Only reachable after
-- `prepareLoad` returned {ok=true} -- errors loudly otherwise, since
-- that is a caller bug, not a data problem.
--
-- Round 2 review: `apply()` mutates its module's live singleton with no
-- rollback of its own. Left unguarded, a LATER
-- component's apply() throwing would abort the transaction with some
-- earlier components already migrated to the new session and the rest
-- still holding the old one -- a half-migrated Lua state paired with
-- the OLD Haskell session, since WorldLoadPublish is only ever queued
-- after this whole function returns successfully (see
-- Engine.Scripting.Lua.Thread.Dispatch.handleLoadStaged). Every
-- registered component's PRE-load live state is captured via its own
-- `snapshot()` (the SAME function saveWorld uses, so this is exactly
-- "what would be written if a save happened right now") before
-- anything is mutated; a later failure restores every
-- already-applied component from that capture, in reverse order,
-- before re-raising -- the caller sees the same hard load-abort as
-- before, but the live Lua session is left exactly as it was.
--
-- Issue #1200 bounds that guarantee honestly, in two ways. It covers
-- registered COMPONENTS only: a failing reset hook's own non-durable
-- effects are never compensated, because `registerResetHook` accepts
-- no compensation function to do it with. And a restore is ordinary
-- Lua that can throw on its own, so it is no longer assumed to
-- succeed -- when one does fail, the raised diagnostic says so and
-- names every component left unrestored (`abortMessage`) instead of
-- claiming a clean abort over a live session that is really MIXED.
--
-- Issue #900: the forward pass hands each apply() the restored-entity
-- context prepareLoad stashed, so a per-entity component can resolve
-- ownership. The ROLLBACK pass deliberately does not: `rollback[id]` is
-- the OLD session's own snapshot, whose entities are by definition
-- absent from the RESTORED context, so filtering it would erase exactly
-- the state the rollback exists to restore. A contextless apply() means
-- "apply every row" precisely so that unwinding stays verbatim.
--
-- Issue #1279 rebuilds that context per component from an immutable
-- encoded snapshot taken before the first apply() runs, with the raw
-- source dropped at that point, so what a component observes no longer
-- depends on apply order, on what the components before it did with the
-- value they were handed, or on what they did to `_pendingEntities`
-- behind its back -- every one of them sees exactly the membership and
-- owner pages Haskell supplied. See the loop below for why neither a
-- read-only proxy nor a plain per-component copy was sufficient.
function saveModules.applyAll()
    local prepared = saveModules._pendingApply
    if prepared == nil then
        saveModules._loadActive = false
        error("saveModules.applyAll: no prepared load (call prepareLoad first and check its ok field)")
    end
    local entities = saveModules._pendingEntities

    local rollback = {}
    for _, id in ipairs(sortedIds(saveModules.registry)) do
        local ok, snapOrErr = pcall(saveModules.registry[id].snapshot)
        if not ok then
            saveModules._pendingApply = nil
            saveModules._pendingRequestId = nil
            saveModules._pendingEntities = nil
            saveModules._loadActive = false
            error("saveModules.applyAll: could not capture a rollback "
                .. "point for '" .. id .. "' -- aborting before any "
                .. "state changed: " .. tostring(snapOrErr))
        end
        rollback[id] = snapOrErr
    end

    local order = select(1, saveModules.dependencyOrder())
    local applyOrder = {}
    local orderSeen = {}
    if order then
        for _, id in ipairs(order) do
            applyOrder[#applyOrder + 1] = id
            orderSeen[id] = true
        end
    end
    -- Defensive: apply anything dependencyOrder() didn't cover (should
    -- be unreachable -- prepareLoad already re-checked the registry is
    -- cycle-free -- but never silently drop a prepared component).
    for _, id in ipairs(sortedIds(saveModules.registry)) do
        if not orderSeen[id] then
            applyOrder[#applyOrder + 1] = id
        end
    end

    -- Round 3 review: a reset-hook failure used to report an error
    -- without rolling back the persistent components applied just
    -- above -- leaving the OLD Haskell session paired with the NEW
    -- Lua singleton state for every one of them, exactly the
    -- half-migrated outcome the apply-loop rollback exists to prevent.
    -- A reset hook itself owns no durable state to roll back TO (that
    -- is what makes it a reset hook rather than a component), but the
    -- persistent components it runs after certainly do, via the SAME
    -- `rollback` captures taken above -- so a reset-hook failure now
    -- unwinds those too: either every persistent component AND every
    -- reset hook completes, or every persistent COMPONENT is left
    -- exactly as it was found. That claim stops at components on
    -- purpose (issue #1200): `registerResetHook` takes a callback and
    -- no compensation function, so a hook that mutated its own
    -- non-durable state before throwing is not (and cannot be) undone
    -- here, and nothing below ever claims otherwise.
    --
    -- Issue #1200: a rollback restore is ordinary Lua too and can throw
    -- exactly like the forward apply that got us here. Attempt EVERY
    -- remaining restore rather than short-circuiting (reverse
    -- application order preserved), and aggregate the ones that failed
    -- so the surfaced diagnostic can name them. Before this, each
    -- restore was an unchecked `pcall` whose result was discarded, and
    -- the abort still claimed a complete rollback, leaving the process
    -- running an old Haskell session against partly-new Lua state while
    -- reporting a cleanly aborted load. Raw error VALUES are collected,
    -- never formatted here: see `abortMessage`.
    local function rollbackApplied(applied)
        local failures = {}
        for i = #applied, 1, -1 do
            local rid = applied[i]
            local restored, restoreErr =
                pcall(saveModules.registry[rid].apply, rollback[rid])
            if not restored then
                failures[#failures + 1] = { id = rid, err = restoreErr }
            end
        end
        return failures
    end

    -- Issue #1279: #900 promised every apply() "a read-only restored-
    -- entity context" and handed out the single mutable table Haskell
    -- pushed, shared by every component in dependency order.
    -- `applyEntityRows` TRUSTS the per-kind subtable for its absent-owner
    -- filtering, so an earlier component that deleted or rewrote an entry
    -- changed which rows every LATER component kept, dropped, or
    -- misassigned -- apply order became load-correctness-relevant,
    -- contradicting Haskell's authoritative `KnownEntities`.
    --
    -- What crosses the apply loop now is an ENCODED context -- an
    -- immutable Lua string -- and each component's own table is rebuilt
    -- from it immediately before that component runs.
    --
    -- A read-only metatable PROXY was tried first and rejected, for two
    -- reasons worth recording so it isn't reintroduced as a cheaper
    -- alternative. It wasn't isolated: a proxy has to close over the
    -- shared source to read through to it, and `debug.getmetatable` sees
    -- past `__metatable` while `debug.getupvalue` lifts the source
    -- straight out of the `__index` closure. And it wasn't a transparent
    -- table: `next` is a primitive that consults no metatable at all -- no
    -- `__pairs`, no `__index` -- so an ordinary `next(entities)` or
    -- `for k in next, entities.unit` walk saw an EMPTY context and would
    -- silently conclude the restored session has no units.
    --
    -- Copies alone were not enough either, twice over. Copying per iteration
    -- still re-read the live `_pendingEntities`, which is public on the
    -- module table, so an earlier apply could reach it as
    -- `require('scripts.lib.save_modules')._pendingEntities.unit[7] = nil`
    -- and every later copy would carry the deletion. Precomputing all of
    -- them up front instead left every SIBLING's context alive in this
    -- frame at once, and the stock `debug` library is loaded here (see
    -- Engine.Scripting.Lua.Thread / API.Register.Debug -- the engine adds
    -- to the stdlib table rather than replacing it), so `debug.getlocal`
    -- could walk this frame and reach a later component's table directly.
    --
    -- Encoding closes both: the only thing shared across the loop is a
    -- string, and Lua strings cannot be mutated -- there is no longer any
    -- shared MUTABLE state for a callback to find, by any route. No
    -- sibling's context exists while another component runs, because each
    -- is materialized on demand and dropped with the iteration.
    --
    -- What this does NOT claim is a sandbox. A component that drives the
    -- debug library adversarially can rewrite this frame's locals, replace
    -- `applyEntityRows`, or mutate another module's live singleton
    -- outright; no mechanism at this layer survives that, and it is not
    -- what this issue is about (a BUGGY or future component silently
    -- corrupting its siblings). The invariant is "no shared mutable
    -- state", which is the one that makes ordinary code correct.
    --
    -- `dataCodec` is the same data-only codec the components' own payloads
    -- already ride through: plain booleans, integers and strings, decoded
    -- by walking, never by evaluating. The context is strictly smaller
    -- than the per-entity payload it accompanies (one boolean per unit
    -- against a whole aiState per unit), so a session whose payloads
    -- encode has a context that encodes too.
    local encodedContext = nil
    if entities ~= nil then
        local encoded, encErr = dataCodec.encode(entities)
        if encoded == nil then
            saveModules._pendingApply = nil
            saveModules._pendingRequestId = nil
            saveModules._pendingEntities = nil
            saveModules._loadActive = false
            error("saveModules.applyAll: could not capture the restored-"
                .. "entity context -- aborting before any state changed: "
                .. tostring(encErr))
        end
        encodedContext = encoded
    end
    -- Nothing reads the raw source past this point (the rollback pass is
    -- contextless by design), so drop it here rather than at the end: the
    -- one table every component's view derives from stops being reachable
    -- the moment it stops being needed.
    entities = nil
    saveModules._pendingEntities = nil

    local applied = {}
    for _, id in ipairs(applyOrder) do
        if prepared[id] ~= nil then
            -- The rebuild happens INSIDE the pcall so that a failure to
            -- reconstruct the context is that component's apply failure,
            -- with the ordinary rollback below -- never a nil context
            -- silently downgrading it to "apply every row".
            local ok, err = pcall(function()
                local context = nil
                if encodedContext ~= nil then
                    local decoded, decErr = dataCodec.decode(encodedContext)
                    if decoded == nil then
                        error("could not rebuild the restored-entity "
                            .. "context: " .. tostring(decErr))
                    end
                    context = decoded
                end
                return saveModules.registry[id].apply(prepared[id], context)
            end)
            if not ok then
                -- Round 5 review: `apply` is ordinary Lua code, not
                -- guaranteed all-or-nothing -- it may have mutated
                -- PART of its own singleton before throwing, so `id`
                -- itself is not yet in `applied` and rollbackApplied
                -- alone would skip it, leaving that partial mutation
                -- live. Restore its own pre-load snapshot first, then
                -- unwind every component applied before it.
                --
                -- Issue #1200: that own-restore is checked like every
                -- other one and joins the SAME aggregate -- it is a
                -- restore of a component that had already started
                -- mutating itself, so it is if anything the likeliest
                -- of them to fail.
                local failures = {}
                local ownRestored, ownErr =
                    pcall(saveModules.registry[id].apply, rollback[id])
                if not ownRestored then
                    failures[#failures + 1] = { id = id, err = ownErr }
                end
                for _, failed in ipairs(rollbackApplied(applied)) do
                    failures[#failures + 1] = failed
                end
                saveModules._pendingApply = nil
                saveModules._pendingRequestId = nil
                saveModules._pendingEntities = nil
                saveModules._loadActive = false
                error(abortMessage(
                    "saveModules.applyAll: '" .. id .. "'.apply() failed",
                    "rolled back every already-applied component "
                        .. "(including its own partial mutation)",
                    failures, err))
            end
            applied[#applied + 1] = id
        end
    end

    -- Reset hooks run only once every real component has committed.
    -- Re-running an already-fired reset hook after a rollback is safe
    -- by construction (a "no durable state" module's reset is
    -- idempotent), so unwinding the components here and re-raising is
    -- sufficient -- there's nothing hook-side left to compensate for.
    for _, id in ipairs(sortedIds(saveModules.resetHooks)) do
        local ok, err = pcall(saveModules.resetHooks[id])
        if not ok then
            local failures = rollbackApplied(applied)
            saveModules._pendingApply = nil
            saveModules._pendingRequestId = nil
            saveModules._pendingEntities = nil
            saveModules._loadActive = false
            error(abortMessage(
                "saveModules.applyAll: reset hook '" .. id
                    .. "' failed after every component committed",
                "rolled back every applied component", failures, err))
        end
    end

    saveModules._pendingApply = nil
    saveModules._pendingRequestId = nil
    saveModules._pendingEntities = nil
    saveModules._loadActive = false
end

return saveModules
