-- Tutorial objective PROGRESS (#958, phase 2 of the tutorial epic
-- #956). #957 defined the tree; this module owns what the player has
-- done with it, and nothing else -- no predicates (#959's
-- scripts/tutorial_eval.lua drives the write surface below), no HUD.
--
-- The module exists because the two kinds of progress a tutorial tree
-- carries have OPPOSITE lifetimes, and conflating them is the bug this
-- slice exists to prevent:
--
--   completed  -- FULL objective ids that have latched. Durable and
--                 MONOTONIC: an id enters exactly once and never
--                 leaves, whatever happens to the gameplay state that
--                 caused it (an item is consumed, a unit dies, the
--                 session is saved and loaded). This set IS the save
--                 payload.
--   checked    -- SUBOBJECTIVE ids currently satisfied. Live and
--                 REVERSIBLE: it checks and unchecks as its predicate
--                 changes, is recomputed from scratch every session,
--                 and is never written to a save.
--
-- Deliberately NOT here: HUD open/closed state. Panel visibility is
-- transient presentation state owned by the HUD child -- the panel
-- starts hidden on a new session and, since nothing about it is stored,
-- comes up hidden after a load too, with no reset step to forget.
--
-- A third, smaller lifetime sits between those two, and it is also
-- never persisted: whether a full/composite id's #996 hide suppression
-- is still owed it. It is granted at a first reveal that finds the id
-- already latched, retired by acknowledgePresented once a consumer has
-- actually shown the row, and RECONSTRUCTED -- never restored -- at
-- every setTree/reset/load, under one rule (#1941 requirement 4):
--
--   an id the durable `completed` set already makes structurally
--   reveal-eligible through its ancestor CHAIN is rebuilt as
--   ALREADY PRESENTED, and an id still gated behind an incomplete
--   ancestor is left unjudged so its real first reveal, later, can
--   still collect #996's protection.
--
-- That is what keeps a load of a finished tutorial from returning
-- ancestors the player already watched retire to the active checklist,
-- while a branch that was completed but never revealed before the save
-- is still protected when it finally appears. Because presentation is
-- deliberately not saved, a save taken after an id's structural reveal
-- but before the HUD acknowledged it is indistinguishable from an
-- acknowledged one, and follows the same deterministic rule.
--
-- Singleton via package.loaded, like scripts/pause.lua: the
-- engine.loadScript tick, the save registry's snapshot/apply callbacks,
-- and any HUD consumer must all see ONE progress state. Nothing is
-- required at MODULE scope (scripts.lib.save_modules is pulled in
-- inside register(), the tutorial tree is injected or fetched lazily),
-- so the whole module is reachable from the standalone-Lua-VM hspec
-- harness with only an `engine` stub -- the same property that makes
-- scripts/unit_ai_locations.lua testable.

local tutorialProgress = package.loaded["scripts.tutorial_progress"] or {}
package.loaded["scripts.tutorial_progress"] = tutorialProgress

-- DURABLE, monotonic: objective id -> true.
tutorialProgress.completed = tutorialProgress.completed or {}
-- LIVE, reversible, never persisted: subobjective id -> true.
tutorialProgress.checked   = tutorialProgress.checked   or {}
-- PRESENTATION-ONLY, never persisted (#996 requirement 6): whether a
-- full/composite id's reveal-eligibility has already been judged once
-- (historySeen), and whether that judgement is still SUPPRESSING the
-- hide rule for it (stickyActive) -- see recomputeHistory below for
-- what "judged" means, and acknowledgePresented for what retires a
-- suppression once the id has actually been shown (#1941).
tutorialProgress.historySeen  = tutorialProgress.historySeen  or {}
tutorialProgress.stickyActive = tutorialProgress.stickyActive or {}
-- The active tree and its derived index, or nil until one is available.
-- Both live on the singleton (not as file-locals) so a re-run of this
-- file via engine.loadScript can't leave a stale tree paired with a
-- freshly-nil index.
tutorialProgress.tree  = tutorialProgress.tree  or nil
tutorialProgress.index = tutorialProgress.index or nil

-- The engine globals this module touches are all guarded: it has to run
-- in the standalone Lua VM the hspec gate uses, where `engine` is a
-- two-function stub and there is no tutorial registry at all.
local function warn(msg)
    if type(engine) == "table" and type(engine.logWarn) == "function" then
        engine.logWarn(msg)
    end
end

-- A FULL objective in the requirement-2 sense: one whose completion
-- latches. Both authored kinds that can complete qualify -- "composite"
-- is a full objective whose completion is composed of live component
-- requirements (see Tutorial.Types), not a third completion semantics.
local function isFullKind(kind)
    return kind == "full" or kind == "composite"
end

-----------------------------------------------------------
-- Tree index
-----------------------------------------------------------

-- Flatten a tree into an id-keyed index plus a stable display order.
--
-- The tree arrives ALREADY validated and ALREADY in display order
-- (#957's engine.getTutorialTree contract: `order` within a sibling
-- group, ties broken by id), so this neither re-sorts nor re-checks it
-- -- re-deriving either here would be a second, drifting copy of a rule
-- the loader already owns. It only records each node's parent,
-- relationship, and depth so the state rules below can be answered by
-- id alone.
--
-- `order` is a PRE-ORDER walk, so a parent is always indexed before its
-- children -- which is what lets computeState resolve reveal/hide in one
-- forward pass instead of recursing.
local function buildIndex(tree)
    if type(tree) ~= "table" or type(tree.root) ~= "table" then return nil end
    local byId, order = {}, {}
    local function visit(node, parentId, relation, depth)
        local id = node.id
        local entry = {
            node          = node,
            parent        = parentId,
            relation      = relation,
            depth         = depth,
            children      = {},
            subobjectives = {},
        }
        byId[id] = entry
        order[#order + 1] = id
        for _, child in ipairs(node.children or {}) do
            entry.children[#entry.children + 1] = child.id
            visit(child, id, "child", depth + 1)
        end
        for _, sub in ipairs(node.subobjectives or {}) do
            entry.subobjectives[#entry.subobjectives + 1] = sub.id
            visit(sub, id, "subobjective", depth + 1)
        end
    end
    visit(tree.root, nil, "root", 0)
    return { byId = byId, order = order, rootId = tree.root.id }
end

-----------------------------------------------------------
-- Reveal history (#996): a full/composite branch that latches before
-- it is ever revealed must not vanish the instant it IS revealed.
-----------------------------------------------------------
--
-- historySeen/stickyActive answer one question per full/composite id,
-- decided exactly once: the first time this id's own structural
-- reveal-eligibility (its ancestor CHAIN's durable completion -- never
-- its own hidden/checked state) goes true, was `completed[id]` ALREADY
-- true? If so the id is stickyActive: computeState's hidden rule below
-- is overridden to false for it, which is what lets it (and, for a
-- composite, the subobjectives its active state reveals) actually be
-- seen instead of latching and hiding in the same instant.
--
-- That suppression is EXCEPTIONAL, not permanent (#1941): it exists
-- only to buy the id a presentation it would otherwise never get, and
-- acknowledgePresented below retires it the moment a consumer reports
-- having actually shown the row, after which the ordinary hide rule
-- applies to that id unchanged.
--
-- This has to be judged at the exact moment identified above, not
-- whenever a reader happens to call getViewModel: completeObjective is
-- itself what can make a DIFFERENT id newly reveal-eligible (its own
-- completion is what a "child" relation reveals), so the judgement is
-- driven from there, incrementally, walking the whole index every time
-- since a single completion can cascade through several links of the
-- chain at once. A node whose ancestor completes while the node itself
-- is still incomplete is judged NOT sticky right then -- the ordinary,
-- already-tested hide-on-its-own-later-completion case -- even though
-- the node may go on to complete later.
--
-- The one case with no incremental order to replay is a fresh
-- setTree()/reset()/load, and it passes `reconstructing` here. Both
-- tables are presentation state and are never persisted (requirement
-- 6), so they are rebuilt from scratch against whatever the durable
-- `completed` set holds at that moment, under ONE deterministic rule
-- (#1941 requirement 4):
--
--   * an id that is ALREADY structurally reveal-eligible through its
--     durable ancestor chain is judged AS IF PRESENTED -- historySeen,
--     never sticky. The pre-save session could only have reached that
--     structural state by revealing the id, so restoring it to the
--     active checklist would resurrect ancestors the player already
--     watched retire. Presentation deliberately is not persisted, so a
--     save taken after an id's structural reveal but BEFORE the HUD
--     acknowledged it is indistinguishable from an acknowledged one and
--     follows this same rule;
--   * an id still gated behind an INCOMPLETE ancestor is not judged at
--     all here, exactly as in a live session. Its real first reveal is
--     still ahead of it, and it collects #996's protection then --
--     which is what keeps the original defect fixed across a save.
local function recomputeHistory(reconstructing)
    local index = tutorialProgress.index
    if index == nil then return end
    local structRevealed = {}
    for _, id in ipairs(index.order) do
        local entry = index.byId[id]
        if entry.parent == nil then
            structRevealed[id] = true
        elseif entry.relation == "child" then
            structRevealed[id] = structRevealed[entry.parent] == true
                and tutorialProgress.completed[entry.parent] == true
        else
            -- Subobjectives are leaves with no latch of their own (see
            -- the module header) -- they never need a history judgement,
            -- and nothing below ever reads structRevealed for one.
            structRevealed[id] = false
        end
        if isFullKind(entry.node.kind) and structRevealed[id]
                and not tutorialProgress.historySeen[id] then
            tutorialProgress.historySeen[id] = true
            if reconstructing then
                tutorialProgress.stickyActive[id] = false
            else
                tutorialProgress.stickyActive[id] =
                    tutorialProgress.completed[id] == true
            end
        end
    end
end

-- Retire the #996 hide suppression for ids a consumer has actually put
-- in front of the player (#1941 requirements 1/2).
--
-- `ids` is one id string or an array of them, and the caller is the
-- surface that RENDERED those rows, asserting that they were exposed on
-- a visible, open page. BUILDING a view model is not presentation --
-- which is exactly why getViewModel stays a pure read a console or a
-- test may call repeatedly, and why this is a separate explicit call
-- rather than a side effect of reading.
--
-- Retirement removes ONLY the exceptional suppression. The ordinary
-- #958 hide rule is never bypassed (a retired composite whose
-- subobjective later unchecks returns to the active view like any
-- other), the durable `completed` set is never touched, and
-- `historySeen` stays set so the id is never re-judged.
--
-- Everything else is state-preserving, by construction rather than by
-- special-casing: only an id currently marked sticky is ever written,
-- so an unknown id, a subobjective id, a full objective that was never
-- sticky, and a repeat of an id already retired all change nothing.
--
-- Returns the sorted ids this call actually retired.
function tutorialProgress.acknowledgePresented(ids)
    local list = ids
    if type(list) == "string" then list = { list } end
    if type(list) ~= "table" then return {} end
    local retired = {}
    for _, id in ipairs(list) do
        if tutorialProgress.stickyActive[id] == true then
            tutorialProgress.stickyActive[id] = false
            retired[#retired + 1] = id
        end
    end
    table.sort(retired)
    return retired
end

-- Adopt `tree` (nil clears), rebuild the index, and reconcile the
-- durable set against it. This is the injection point the hspec gate
-- uses in place of a real engine registry.
function tutorialProgress.setTree(tree)
    tutorialProgress.tree  = tree
    tutorialProgress.index = buildIndex(tree)
    -- A new tree means a new reveal history -- #996 requirement 6 never
    -- persists this, so it is always rebuilt fresh against whatever
    -- `completed` holds right now (empty for a brand-new session; a
    -- load's already-restored set when the tree resolves lazily during
    -- one, since ensureTree() runs before apply()'s own explicit
    -- rebuild below). A rebuild has no incremental order to replay, so
    -- it takes recomputeHistory's RECONSTRUCTION rule (#1941).
    tutorialProgress.historySeen  = {}
    tutorialProgress.stickyActive = {}
    tutorialProgress.reconcile()
    recomputeHistory(true)
    return tutorialProgress.tree
end

-- The tree this session is running, fetching it from the engine the
-- first time it becomes available. The tutorial registry is written
-- once at boot (#957's one loadTutorialDir call), so caching it is safe
-- and a nil result just means "no tutorial this session" -- retried
-- cheaply on the next call rather than latched.
function tutorialProgress.ensureTree()
    if tutorialProgress.tree ~= nil then return tutorialProgress.tree end
    if type(engine) == "table" and type(engine.getTutorialTree) == "function" then
        local tree = engine.getTutorialTree()
        if tree ~= nil then tutorialProgress.setTree(tree) end
    end
    return tutorialProgress.tree
end

local function entryOf(id)
    local index = tutorialProgress.index
    if index == nil then return nil end
    return index.byId[id]
end

-----------------------------------------------------------
-- Write surface (#958 requirements 2/3)
-----------------------------------------------------------

-- Latch a full objective completed, by YAML id. Returns true only when
-- this call is what completed it.
--
-- Idempotent (re-latching is a silent no-op) and total: an unknown id,
-- or a subobjective id whose state is live rather than durable, is a
-- diagnostic no-op returning false, never an error -- this is driven by
-- data-authored evaluator wiring, and a stale id there must not take
-- down a save.
function tutorialProgress.completeObjective(id)
    tutorialProgress.ensureTree()
    local entry = entryOf(id)
    if entry == nil then
        warn("tutorial_progress: completeObjective ignored unknown objective id '"
            .. tostring(id) .. "'")
        return false
    end
    if not isFullKind(entry.node.kind) then
        warn("tutorial_progress: completeObjective ignored '" .. tostring(id)
            .. "' -- a subobjective's check state is live, not durable "
            .. "(use setSubobjectiveChecked)")
        return false
    end
    if tutorialProgress.completed[id] then return false end
    tutorialProgress.completed[id] = true
    -- This completion may be exactly what makes a DIFFERENT id (a
    -- child this one gates) newly reveal-eligible for the first time --
    -- see recomputeHistory's header for why the judgement has to happen
    -- here, not lazily whenever a view is next read. INCREMENTAL: this
    -- is a live reveal with a real order, not a reconstruction.
    recomputeHistory(false)
    return true
end

-- Durable latch state. Answers from the set itself, so it stays correct
-- for a payload applied before the tree was available.
function tutorialProgress.isCompleted(id)
    return tutorialProgress.completed[id] == true
end

-- Set or clear a subobjective's LIVE check state, by YAML id. Returns
-- true only when the state actually changed. Same total, diagnostic
-- treatment of a bad id as completeObjective.
function tutorialProgress.setSubobjectiveChecked(id, checked)
    tutorialProgress.ensureTree()
    local entry = entryOf(id)
    if entry == nil then
        warn("tutorial_progress: setSubobjectiveChecked ignored unknown "
            .. "objective id '" .. tostring(id) .. "'")
        return false
    end
    if entry.node.kind ~= "subobjective" then
        warn("tutorial_progress: setSubobjectiveChecked ignored '" .. tostring(id)
            .. "' -- a full objective latches, it does not check "
            .. "(use completeObjective)")
        return false
    end
    local want = checked and true or false
    if (tutorialProgress.checked[id] == true) == want then return false end
    tutorialProgress.checked[id] = want or nil
    return true
end

function tutorialProgress.isSubobjectiveChecked(id)
    return tutorialProgress.checked[id] == true
end

-- The durable payload, as a canonical sorted array -- the ONE spelling
-- of "what this component persists", shared by snapshot() and the view
-- model so they can never disagree.
function tutorialProgress.completedIds()
    local ids = {}
    for id in pairs(tutorialProgress.completed) do ids[#ids + 1] = id end
    table.sort(ids)
    return ids
end

-- Fresh progress: nothing completed, nothing checked. The state a new
-- session and a save with no tutorial component both start from.
function tutorialProgress.reset()
    tutorialProgress.completed = {}
    tutorialProgress.checked   = {}
    tutorialProgress.historySeen  = {}
    tutorialProgress.stickyActive = {}
    recomputeHistory(true)
end

-- Drop every completion whose id is not a full objective in the tree
-- this session actually loaded, returning the sorted ids dropped.
--
-- A tutorial id that vanished or was renamed between saves is a
-- DANGLING reference, and the persistence contract treats those as
-- tolerated, non-blocking diagnostics scrubbed at reconcile time (the
-- same rule #915's per-unit location memory follows) -- never a load
-- failure. Structural malformation is a different class entirely and is
-- rejected by validate() below.
--
-- With no tree available yet this keeps the set untouched: "I cannot
-- judge these ids" is not "these ids are wrong", and setTree re-runs it
-- as soon as a tree arrives.
function tutorialProgress.reconcile()
    local index = tutorialProgress.index
    if index == nil then return {} end
    local dropped = {}
    for id in pairs(tutorialProgress.completed) do
        local entry = index.byId[id]
        if entry == nil or not isFullKind(entry.node.kind) then
            dropped[#dropped + 1] = id
        end
    end
    table.sort(dropped)
    for _, id in ipairs(dropped) do tutorialProgress.completed[id] = nil end
    -- Live checks get the same treatment, minus the diagnostic: they
    -- are recomputed anyway, so a stale one is noise, not history.
    local staleChecks = {}
    for id in pairs(tutorialProgress.checked) do
        local entry = index.byId[id]
        if entry == nil or entry.node.kind ~= "subobjective" then
            staleChecks[#staleChecks + 1] = id
        end
    end
    for _, id in ipairs(staleChecks) do tutorialProgress.checked[id] = nil end
    if #dropped > 0 then
        warn("tutorial_progress: dropped " .. #dropped .. " completed objective "
            .. "id(s) absent from the loaded tutorial tree: "
            .. table.concat(dropped, ", "))
    end
    return dropped
end

-----------------------------------------------------------
-- Tree-state rules (#958 requirement 4)
-----------------------------------------------------------
--
-- Stated once, here, independently of any renderer:
--
--   * a root is display-eligible from the start;
--   * a completed full objective REVEALS its authored `children`;
--   * a composite DISPLAYS its `subobjectives` while it is active;
--   * a node stays available until it is itself completed AND every
--     relevant child/subobjective is done, then HIDES from the default
--     active view -- for a leaf (no children, no subobjectives) that
--     collapses to its own completion, since its own completion is the
--     entire relevant set;
--   * a hidden node is still reported (as a row with active = false),
--     so completed history stays available to later filters and tree
--     views.
--
-- Note what the last two rules mean together for a composite: its hide
-- condition reads LIVE subobjective checks, so a completed composite
-- whose subobjective later unchecks returns to the active view showing
-- that unchecked row. That is the intended consequence of requirement
-- 3's reversibility, and it costs the player nothing durable --
-- `completed` still holds the composite, exactly as requirement 2
-- demands.
--
-- One override on top of all of that (#996): a node that was ALREADY
-- latched the first time it ever became reveal-eligible never gets a
-- chance to be seen "in progress" at all, so hiding it on the same tick
-- it first appears would mean the player never sees it. stickyActive
-- (recomputeHistory, above) marks exactly those ids, and the hide rule
-- below is suppressed for them -- which, for a composite, is also what
-- lets its subobjectives (gated on the composite staying un-hidden)
-- become observable at all.
--
-- The override is a LOAN, not a second hide rule (#1941): it lasts
-- until the id has actually been presented, which the surface that
-- rendered it reports through acknowledgePresented. Once retired, the
-- id falls back onto the rule above with nothing carried over -- so it
-- hides while its relevant set is done, and returns to the active view
-- the moment a live subobjective unchecks, exactly like a node that was
-- never sticky at all.

local function allCompleted(ids)
    for _, id in ipairs(ids) do
        if not tutorialProgress.completed[id] then return false end
    end
    return true
end

local function allChecked(ids)
    for _, id in ipairs(ids) do
        if not tutorialProgress.checked[id] then return false end
    end
    return true
end

-- reveal/hide for every indexed node, in one forward pass over the
-- pre-order id list (a parent is always resolved before its children).
local function computeState()
    local index = tutorialProgress.index
    if index == nil then return nil end
    local revealed, hidden = {}, {}
    for _, id in ipairs(index.order) do
        local entry = index.byId[id]
        if entry.parent == nil then
            revealed[id] = true
        elseif entry.relation == "child" then
            revealed[id] = revealed[entry.parent] == true
                and tutorialProgress.completed[entry.parent] == true
        else
            -- A composite displays its subobjectives while ACTIVE:
            -- revealed and not yet hidden.
            revealed[id] = revealed[entry.parent] == true
                and hidden[entry.parent] ~= true
        end
        local rawHidden = tutorialProgress.completed[id] == true
            and allCompleted(entry.children)
            and allChecked(entry.subobjectives)
        hidden[id] = rawHidden and not tutorialProgress.stickyActive[id]
    end
    return { revealed = revealed, hidden = hidden }
end

-----------------------------------------------------------
-- Read-only view model (#958 requirement 5)
-----------------------------------------------------------

-- A freshly built, deterministic snapshot of tutorial progress for HUD
-- and console consumers. Read-only in the sense that matters: it shares
-- no table with the live state above except the immutable authored node
-- fields it copies out, so mutating the result changes nothing.
--
-- Row order is the tree's own display order. Each row is identified by
-- its YAML id and states its progress in exactly ONE of two fields, so
-- a consumer can never mistake durable completion for a live check:
--
--   completed -- present only on full/composite rows (durable latch)
--   checked   -- present only on subobjective rows (live, reversible)
--
-- `active` is the default-active-view flag: false means "completed
-- history", which is retained rather than dropped so a later filter or
-- tree view has it.
function tutorialProgress.getViewModel()
    tutorialProgress.ensureTree()
    local tree  = tutorialProgress.tree
    local model = {
        treeId       = type(tree) == "table" and tree.id or nil,
        rows         = {},
        completedIds = tutorialProgress.completedIds(),
    }
    local index = tutorialProgress.index
    if index == nil then return model end
    local state = computeState()
    for _, id in ipairs(index.order) do
        if state.revealed[id] then
            local entry = index.byId[id]
            local node  = entry.node
            local row = {
                id        = id,
                kind      = node.kind,
                label     = node.label,
                tooltip   = node.tooltip,
                evaluator = node.evaluator,
                order     = node.order,
                depth     = entry.depth,
                parent    = entry.parent,
                relation  = entry.relation,
                active    = state.hidden[id] ~= true,
            }
            if isFullKind(node.kind) then
                row.completed = tutorialProgress.completed[id] == true
            else
                row.checked = tutorialProgress.checked[id] == true
            end
            model.rows[#model.rows + 1] = row
        end
    end
    return model
end

-----------------------------------------------------------
-- Persistence component (#958 requirements 1/6/7)
-----------------------------------------------------------

-- Register the "tutorial_progress" save component. Split out from
-- init() so the hspec gate can register it against a stubbed engine
-- without booting one; the guard makes a second call (a hot-reloaded
-- script re-running init) a no-op rather than saveModules.register's
-- duplicate-id error.
function tutorialProgress.register()
    if tutorialProgress._registered then return false end
    local saveMods = require("scripts.lib.save_modules")
    -- Both the component id and the schema version are LITERALS here,
    -- not constants: tools/save_compat_audit.py reads this declaration
    -- statically (and tools/persistence_inventory_audit.py reads the
    -- id), so a name or version routed through a local would read as a
    -- shape change and fail the audit.
    saveMods.register("tutorial_progress", {
        version       = 1,
        inputVersions = { 1 },
        -- OPTIONAL, and that is requirement 7's whole mechanism: a
        -- required component missing from a save is a hard load error,
        -- so every supported save written before this component existed
        -- would fail. Optional-with-default() is the only shape under
        -- which such a save loads, and it loads with default()'s fresh
        -- progress. The flip side is accepted deliberately: an optional
        -- component whose snapshot/validate fails is omitted from the
        -- save with a warning rather than aborting it, which is the
        -- right trade for onboarding progress.
        required      = false,
        scope         = "global",
        -- Tutorial progress is session-wide, not per-page or per-entity,
        -- and keys on authored YAML ids rather than runtime entity ids,
        -- so it depends on no other component and declares no references.
        deps          = {},
        snapshot = function()
            -- Requirement 1: ONLY the completed full-objective ids.
            -- Live subobjective checks (requirement 3) are absent by
            -- construction, not filtered out here. The active tree id
            -- is absent too: #957 makes "exactly one tree, named
            -- first_session" a load-time invariant, so storing it would
            -- persist a constant.
            return { completed = tutorialProgress.completedIds() }
        end,
        decode = function(version, data)
            -- v1 is the only schema. A nil payload is the
            -- legacy-baseline default path (save_modules'
            -- isMigratingLegacyBaseline) and means honest fresh
            -- progress. Anything else passes through untouched so
            -- validate() below is the single place a malformed payload
            -- is judged.
            if data == nil then return { completed = {} } end
            return data
        end,
        validate = function(data)
            if type(data) ~= "table" then
                return { "payload must be a table, got " .. type(data) }
            end
            local errs = {}
            local unknown = {}
            for key in pairs(data) do
                if key ~= "completed" then unknown[#unknown + 1] = tostring(key) end
            end
            table.sort(unknown)
            for _, key in ipairs(unknown) do
                errs[#errs + 1] = "unknown payload field '" .. key .. "'"
            end
            local completed = data.completed
            if type(completed) ~= "table" then
                errs[#errs + 1] = "`completed` must be an array of objective id "
                    .. "strings, got " .. type(completed)
            else
                local n, count = #completed, 0
                for key in pairs(completed) do
                    count = count + 1
                    if type(key) ~= "number" or key ~= math.floor(key)
                            or key < 1 or key > n then
                        errs[#errs + 1] = "`completed` must be a dense array of "
                            .. "objective ids (bad key " .. tostring(key) .. ")"
                    end
                end
                if count ~= n then
                    errs[#errs + 1] = "`completed` must be a dense array of "
                        .. "objective ids (has holes)"
                end
                local seen = {}
                for i = 1, n do
                    local id = completed[i]
                    if type(id) ~= "string" or id == "" then
                        errs[#errs + 1] = "`completed`[" .. i
                            .. "] must be a non-empty objective id string"
                    elseif seen[id] then
                        errs[#errs + 1] = "`completed` repeats objective id '"
                            .. id .. "'"
                    else
                        seen[id] = true
                    end
                end
            end
            if #errs > 0 then return errs end
            return nil
        end,
        default = function()
            -- Requirement 7's documented fresh default, reached by every
            -- save written before this component existed.
            return { completed = {} }
        end,
        apply = function(data)
            local completed = {}
            if type(data) == "table" and type(data.completed) == "table" then
                for _, id in ipairs(data.completed) do completed[id] = true end
            end
            tutorialProgress.completed = completed
            -- Requirement 3: live checks are never restored. A load
            -- starts them empty and the evaluation tick recomputes them
            -- from the loaded world. (applyAll's rollback path calls
            -- apply() with a pre-load snapshot, which clears them the
            -- same way -- correct for the same reason.)
            tutorialProgress.checked = {}
            -- Resolve the tree BEFORE reconciling. On a normal load
            -- nothing has asked this module for anything yet -- init()
            -- only registers, and the load path reaches apply()
            -- directly -- so the lazy tree is still unresolved and
            -- `index` is nil.
            -- Reconciling against a nil index is a no-op, which would
            -- silently RETAIN a renamed/removed objective id and let
            -- the very next save write it straight back out.
            tutorialProgress.ensureTree()
            -- An id the current tree no longer has is scrubbed here, as
            -- a diagnostic, not a load failure. With no tree available
            -- at all this still keeps the ids untouched (see
            -- reconcile): "I cannot judge these" is not "these are
            -- wrong", and ensureTree retries on the next call.
            tutorialProgress.reconcile()
            -- Requirement 6 (#996): the reveal history is presentation
            -- state and is never part of this payload, so a load always
            -- rebuilds it fresh against the just-restored, just-scrubbed
            -- durable set -- regardless of whether ensureTree() above
            -- resolved the tree for the first time this call (which
            -- would already have run this once against the pre-scrub
            -- set) or found it already cached from an earlier session
            -- in the same process (which would otherwise leave stale
            -- history behind).
            --
            -- RECONSTRUCTION (#1941 requirement 4): an id the restored
            -- set already makes structurally reveal-eligible is treated
            -- as previously presented, so a load never returns an
            -- ancestor the player watched retire to the checklist,
            -- while a completed id still gated behind an incomplete one
            -- keeps #996's protection for its real first reveal.
            tutorialProgress.historySeen  = {}
            tutorialProgress.stickyActive = {}
            recomputeHistory(true)
        end,
    })
    tutorialProgress._registered = true
    return true
end

-----------------------------------------------------------
-- Engine lifecycle
-----------------------------------------------------------

-- Loaded via engine.loadScript so the save component registers once per
-- session and engine broadcasts reach the module. No update() of its
-- own: this module stores progress and answers queries, and the
-- predicates that DRIVE it live in scripts/tutorial_eval.lua (#959),
-- which ticks and calls the write surface above.
function tutorialProgress.init(scriptId)
    engine.logInfo("Tutorial progress initializing...")
    tutorialProgress.register()
end

return tutorialProgress
