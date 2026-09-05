-- Tutorial objective EVALUATION (#959, phase 3 of the tutorial epic
-- #956).
--
-- #957 authored the tree and the stable `evaluator` key on each
-- objective; #958 owns what the player has DONE with that tree (the
-- durable completed-full-objective set and the live subobjective
-- checks) and shipped deliberately without any predicates. This module
-- is the missing half: it binds each authored evaluator key to a
-- predicate over durable live gameplay state and drives #958's write
-- surface with the answers.
--
-- Three properties shape everything below.
--
--   * It owns NO gameplay state. Every answer is recomputed from live
--     state on each update, so there is nothing to persist, nothing to
--     reconcile after a load, and no per-unit or per-save bookkeeping
--     for the post-load orphan prune to chase. The only durable residue
--     of an evaluation is what #958 already persists. (The one thing
--     the module remembers is which unbound evaluator keys it has
--     already complained about -- a log memo, so a data mistake reports
--     once instead of once per second.)
--   * It observes only DURABLE facts — placed buildings, persisted AI
--     memory, carried inventory. Never unit selection, never an event
--     log, never a transient UI message. Event streams here are DRAINED
--     (engine.getEventLog / combat.drainEvents), so a predicate reading
--     one would race whichever panel drained it first and would answer
--     differently depending on how long the player had the HUD open.
--   * Scope is GLOBAL PER SAVE. An eligible acolyte on any live page can
--     satisfy a predicate, so nothing here may enumerate through an
--     active-page-scoped list (unit.getAllIds, building.getActiveIds).
--     Units come from the page-agnostic AI-state table, the portal from
--     the page-agnostic building.existsWithDef.
--
-- Loaded via engine.loadScript so update() ticks; requires nothing at
-- module scope, so the whole module is reachable from the standalone-
-- Lua-VM hspec harness ("Tutorial evaluation") with stub globals, the
-- same property that makes scripts/unit_ai_locations.lua testable.

local tutorialEval = package.loaded["scripts.tutorial_eval"] or {}
package.loaded["scripts.tutorial_eval"] = tutorialEval

-- Test injection point for the per-unit AI state table (there is no
-- unit_ai in a bare Lua VM). nil in production, where `aiStateTable`
-- resolves the real singleton instead.
tutorialEval.aiState = tutorialEval.aiState or nil

-- Evaluator keys already reported as unbound. Purely a log memo (see the
-- header): never persisted, never read by a predicate, and reset only by
-- a fresh session.
tutorialEval.warnedKeys = tutorialEval.warnedKeys or {}

-----------------------------------------------------------
-- The facts the shipped predicates are written against.
--
-- Spelled as named constants because each is a CONTRACT with data the
-- player can edit, not a tuning knob: renaming the portal def, the
-- acolyte def, the rations item, or the canteen's `holds` kind silently
-- turns a predicate into a permanent false, which reads as a broken
-- tutorial rather than as an error.
-----------------------------------------------------------

local PORTAL_DEF     = "acolyte_portal"
local ACOLYTE_DEF    = "acolyte"
local PLAYER_FACTION = "player"
local RATIONS_DEF    = "rations"
-- The container `holds` kind, NOT an item def name. Carried water is
-- the sum of currentFill over container items declaring this kind, so
-- any future waterskin counts without being listed here. The discrete
-- `water` item is excluded by construction and on purpose: it is a
-- crafting-input abstraction (data/items/water.yaml) that declares no
-- container and carries no fill, so it is not water anyone can drink on
-- the road.
local WATER_HOLDS = "water"

-- A trip out and back. One full canteen_steel_2l (2.0 L) meets it
-- exactly, which is what the acolyte spawn kit grants.
local EXPEDITION_WATER_L = 2.0
local EXPEDITION_RATIONS = 1

local function warn(msg)
    if type(engine) == "table" and type(engine.logWarn) == "function" then
        engine.logWarn(msg)
    end
end

-----------------------------------------------------------
-- Live-state readers
-----------------------------------------------------------

-- The per-unit AI state table, or nil when the unit AI has not loaded.
--
-- Reached through the package.loaded singleton rather than require() on
-- purpose: scripts/unit_ai_core.lua binds `scripts.unit_ai` at MODULE
-- scope, so require()ing it out of order would half-initialise it. Not
-- cached either -- a table lookup per update costs nothing, and caching
-- would take a position on table identity across a load that this
-- module has no reason to hold.
--
-- This table is also the enumeration surface for units, which is what
-- keeps requirement 6 honest: it is keyed by uid globally, so an
-- acolyte on a live but inactive page is still reachable, while
-- unit.getAllIds would silently drop it.
local function aiStateTable()
    if type(tutorialEval.aiState) == "table" then
        return tutorialEval.aiState
    end
    local unitAi = package.loaded["scripts.unit_ai"]
    if type(unitAi) == "table" and type(unitAi.aiState) == "table" then
        return unitAi.aiState
    end
    return nil
end

-- A live, player-faction acolyte. Every clause is required: aiState
-- outlives nothing (the post-load reconcile prunes dead ids), but a
-- technomule and a bear both keep AI state too, and neither is a
-- traveller this tutorial is about.
local function isPlayerAcolyte(uid)
    if type(uid) ~= "number" then return false end
    if type(unit) ~= "table" then return false end
    if not unit.exists(uid) then return false end
    if unit.getFaction(uid) ~= PLAYER_FACTION then return false end
    local info = unit.getInfo(uid)
    return type(info) == "table" and info.defName == ACOLYTE_DEF
end

-- Litres of water and count of rations this unit is CARRYING, read off
-- its own inventory. Water is measured as fill, not as item count: a
-- canteen is provisioning only to the extent it is full, and an empty
-- one weighs the same as a full one to a naive count.
local function carriedSupplies(uid)
    local litres, rations = 0, 0
    if type(unit) ~= "table" or type(unit.getInventory) ~= "function" then
        return litres, rations
    end
    local inv = unit.getInventory(uid)
    if type(inv) ~= "table" then return litres, rations end
    for _, item in ipairs(inv) do
        if type(item) == "table" then
            if item.holds == WATER_HOLDS then
                litres = litres + (tonumber(item.currentFill) or 0)
            end
            if item.defName == RATIONS_DEF then
                rations = rations + 1
            end
        end
    end
    return litres, rations
end

local function portalExists()
    if type(building) ~= "table"
            or type(building.existsWithDef) ~= "function" then
        return false
    end
    return building.existsWithDef(PORTAL_DEF) == true
end

-- Everything the shipped predicates need, gathered in ONE pass over the
-- eligible acolytes.
--
-- One pass rather than a predicate-per-scan is what makes the
-- same-acolyte rule expressible at all: `food` is true only when a
-- SINGLE unit clears both thresholds, so the two supply answers have to
-- be decided while that unit is still in hand. Deciding them from two
-- independent scans is exactly the bug requirement 4 names -- rations on
-- one acolyte and a full canteen on another would read as a provisioned
-- traveller when nobody is.
function tutorialEval.gatherFacts()
    local facts = {
        portal     = portalExists(),
        knownWater = false,
        water      = false,
        food       = false,
    }
    local aiState = aiStateTable()
    if aiState == nil then return facts end
    for uid, s in pairs(aiState) do
        if isPlayerAcolyte(uid) then
            if type(s) == "table" and type(s.knownWaterSources) == "table"
                    and #s.knownWaterSources > 0 then
                -- Freshness holds by construction rather than by a
                -- stored flag: the only writer that ORIGINATES an entry
                -- is unit_ai_water's scan, which admits lake and river
                -- tiles only, and the radio hand-off merely copies
                -- entries between acolytes.
                facts.knownWater = true
            end
            local litres, rations = carriedSupplies(uid)
            local hasWater = litres >= EXPEDITION_WATER_L
            if hasWater then facts.water = true end
            if hasWater and rations >= EXPEDITION_RATIONS then
                facts.food = true
            end
        end
    end
    return facts
end

-----------------------------------------------------------
-- Evaluator bindings
--
-- Keyed by #957's stable `evaluator` string, so the YAML decides which
-- objective each predicate answers for and this table decides only what
-- the predicate MEANS.
--
-- `prepare_expedition` is deliberately absent. A composite's truth is
-- its subobjectives' live checks in the same update (requirement 5), not
-- an independent scan, so binding it to a predicate here would create a
-- second, drifting spelling of a rule the tree already states. The
-- evaluate pass below handles composites structurally, on `kind`.
-----------------------------------------------------------

local PREDICATES = {
    place_portal        = function(facts) return facts.portal end,
    secure_water_source = function(facts) return facts.knownWater end,
    prepare_water       = function(facts) return facts.water end,
    prepare_food        = function(facts) return facts.food end,
}

-----------------------------------------------------------
-- The evaluation pass
-----------------------------------------------------------

-- Evaluate every objective in the loaded tree once and write the results
-- through #958's surface. Returns an id -> boolean map of what each
-- objective evaluated to THIS pass (nil when no tree is available), for
-- tests and console introspection; it is a report, not state.
--
-- Two passes, because a composite reads its subobjectives' results:
--   1. leaves and plain full objectives, from the gathered facts;
--   2. composites, from the pass-1 results of their own subobjectives.
--
-- Pass 2 reads pass 1's RESULTS rather than re-reading the checked table
-- so "both true in the same update" means what it says. Re-reading would
-- be indistinguishable here today and would quietly become wrong the
-- moment a subobjective's write is filtered or deferred.
--
-- Reveal state is not consulted anywhere. An objective latches when its
-- predicate is true, whether or not the player can currently see the row
-- (#958 keeps reveal a display concern) -- the acolyte spawn kit
-- therefore satisfies both prepare subobjectives immediately, and the
-- composite latches before it is ever revealed. That is intended: the
-- panel measures the arc, it does not gate it.
function tutorialEval.evaluate()
    local TP = package.loaded["scripts.tutorial_progress"]
    if type(TP) ~= "table" then
        TP = require("scripts.tutorial_progress")
    end
    if TP.ensureTree() == nil then return nil end
    local index = TP.index
    if index == nil then return nil end

    local facts   = tutorialEval.gatherFacts()
    local results = {}

    for _, id in ipairs(index.order) do
        local node = index.byId[id].node
        if node.kind ~= "composite" then
            local pred = PREDICATES[node.evaluator]
            if pred == nil then
                -- Data-authored wiring: an evaluator key with no binding
                -- is a diagnostic no-op, never an error, the same way
                -- #958 treats an unknown objective id. Reported once per
                -- session rather than once per tick -- this pass runs
                -- forever, and a data mistake should read as one loud
                -- line, not as a flooded log.
                local key = tostring(node.evaluator)
                if not tutorialEval.warnedKeys[key] then
                    tutorialEval.warnedKeys[key] = true
                    warn("tutorial_eval: no predicate bound for evaluator key '"
                        .. key .. "' (objective '" .. tostring(id) .. "')")
                end
            else
                local ok = pred(facts) and true or false
                results[id] = ok
                if node.kind == "subobjective" then
                    TP.setSubobjectiveChecked(id, ok)
                elseif ok then
                    TP.completeObjective(id)
                end
            end
        end
    end

    for _, id in ipairs(index.order) do
        local entry = index.byId[id]
        if entry.node.kind == "composite" then
            -- An authored composite always has subobjectives (#957
            -- rejects one that does not at load), so the empty case can
            -- only arise from a malformed injected tree -- treat it as
            -- unsatisfied rather than vacuously true, which would latch
            -- a composite nobody earned.
            local all = #entry.subobjectives > 0
            for _, sub in ipairs(entry.subobjectives) do
                if results[sub] ~= true then all = false end
            end
            results[id] = all
            if all then TP.completeObjective(id) end
        end
    end

    return results
end

-----------------------------------------------------------
-- Engine lifecycle
-----------------------------------------------------------

-- Deliberately NOT pause-gated. Evaluating is observation, not
-- simulation: it mutates only tutorial progress, never the world, and a
-- player who opens their inventory while paused should see the panel
-- agree with what they are looking at. This mirrors the pause
-- independence #915's awareness ingestion and the location-discovery
-- tick already have, for the same reason.
function tutorialEval.update(dt)
    tutorialEval.evaluate()
end

function tutorialEval.init(scriptId)
    engine.logDebug("Tutorial evaluation initializing...")
end

return tutorialEval
