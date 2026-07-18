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
--       apply     = function(data) ... end,   -- run only after ALL
--                                              -- required components
--                                              -- validate (requirement 11)
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
--   snapshotAll()           -- {ok=true, components={{id,version,payload},..}}
--                              or {ok=false, error=...} -- a REQUIRED
--                              component's snapshot/encode failure aborts
--                              the WHOLE save (requirement 6); an OPTIONAL
--                              one is omitted with a logged warning
--                              (requirement 7)
--   prepareLoad(components) -- {ok=true} or {ok=false, errors={...}} --
--                              decode + migrate + component-local-
--                              validate EVERY component with NO live
--                              mutation (requirement 11); all-or-nothing
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
saveModules._loadActive    = false
saveModules._pendingApply  = nil

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
    local inputVersions = spec.inputVersions
    if type(inputVersions) ~= "table" or #inputVersions == 0 then
        error("saveModules.register: '" .. id
            .. "' must declare a non-empty inputVersions array (no default "
            .. "-- list every schema version this component's decode() "
            .. "can still read)")
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
    -- strictly BEFORE the Haskell-side world-thread restore is ever
    -- queued (WorldLoadSave), for every load, with no exception. The
    -- declaration still matters -- documenting a real cross-language
    -- coupling, and rejecting a typo'd/nonexistent id outright, same as
    -- a bad Lua-to-Lua dep.
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
    for _, id in ipairs(sortedIds(saveModules.registry)) do
        local reg = saveModules.registry[id]
        local ok, dataOrErr = pcall(reg.snapshot)
        if not ok then
            if reg.required then
                return { ok = false, error = "'" .. id
                    .. "' snapshot failed: " .. tostring(dataOrErr) }
            end
            engine.logWarn("saveModules: optional component '" .. id
                .. "' snapshot failed, omitting from save: "
                .. tostring(dataOrErr))
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
            end
        end
    end
    return { ok = true, components = components }
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

local function prepareLoadImpl(componentsList)
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
    for _, id in ipairs(sortedIds(saveModules.registry)) do
        local reg = saveModules.registry[id]
        local entry = byId[id]
        if entry == nil then
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
                        end
                    end
                end
            end
        end
    end
    if #errors > 0 then
        return { ok = false, errors = errors }
    end
    return { ok = true, prepared = prepared }
end

-- Decode + migrate + component-locally-validate EVERY registered
-- component from `componentsList` ({id,version,payload}, as reported by
-- the envelope decode) with NO live mutation (requirement 11):
-- all-or-nothing. On success, stashes the prepared data for the
-- following `applyAll()` call and returns {ok=true}; on any failure,
-- returns {ok=false, errors={...}} and stashes nothing, so a caller
-- that aborts the load can never accidentally apply a partial result.
function saveModules.prepareLoad(componentsList)
    saveModules._loadActive = true
    saveModules._pendingApply = nil
    local ok, result = pcall(prepareLoadImpl, componentsList)
    if not ok then
        saveModules._loadActive = false
        return { ok = false, errors = { tostring(result) } }
    end
    if result.ok then
        saveModules._pendingApply = result.prepared
    else
        saveModules._loadActive = false
    end
    return { ok = result.ok, errors = result.errors }
end

-- Apply the load prepared by the most recent successful `prepareLoad`,
-- in dependency order, then run every registered reset hook (session-
-- replacement for modules with no durable state). Only reachable after
-- `prepareLoad` returned {ok=true} -- errors loudly otherwise, since
-- that is a caller bug, not a data problem.
function saveModules.applyAll()
    local prepared = saveModules._pendingApply
    if prepared == nil then
        saveModules._loadActive = false
        error("saveModules.applyAll: no prepared load (call prepareLoad \
              first and check its ok field)")
    end
    local order = select(1, saveModules.dependencyOrder())
    local applied = {}
    if order then
        for _, id in ipairs(order) do
            if prepared[id] ~= nil then
                applied[id] = true
                saveModules.registry[id].apply(prepared[id])
            end
        end
    end
    -- Defensive: apply anything dependencyOrder() didn't cover (should
    -- be unreachable -- prepareLoad already re-checked the registry is
    -- cycle-free -- but never silently drop a prepared component).
    for _, id in ipairs(sortedIds(saveModules.registry)) do
        if prepared[id] ~= nil and not applied[id] then
            saveModules.registry[id].apply(prepared[id])
        end
    end
    for _, id in ipairs(sortedIds(saveModules.resetHooks)) do
        saveModules.resetHooks[id]()
    end
    saveModules._pendingApply = nil
    saveModules._loadActive = false
end

return saveModules
