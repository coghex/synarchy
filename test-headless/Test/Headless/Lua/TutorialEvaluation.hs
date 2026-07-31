{-# LANGUAGE TypeApplications #-}
-- | The "Tutorial evaluation" gate (#959, phase 3 of the tutorial epic
--   #956): @scripts/tutorial_eval.lua@'s bindings from #957's authored
--   evaluator keys to predicates over live gameplay state, and the
--   writes they drive through #958's progress surface.
--
--   Two halves, for two different risks:
--
--   * 'spec' — the PREDICATES, in the standalone-Lua-VM style
--     "Test.Headless.Lua.TutorialProgress" and
--     "Test.Headless.Lua.UnitAiLocations" use: each 'it' runs one
--     self-contained chunk via 'Lua.dostring' in a fresh interpreter,
--     asserting inside Lua. The world the predicates read is a stub
--     built from the REAL shapes the engine hands Lua — a
--     @unit.getInventory@ row carrying @defName@\/@holds@\/
--     @currentFill@ exactly as @Units.Inventory@ pushes it, an
--     @aiState@ entry carrying @knownWaterSources@ exactly as
--     @unit_ai_core@ writes it, and a page-agnostic
--     @building.existsWithDef@. That is what makes the negative cases
--     (food on one acolyte, water on another) expressible at all: they
--     are inventory ARRANGEMENTS, and arranging them through a live
--     engine would cost a boot per case to assert nothing the stub
--     cannot.
--   * 'luaSpec' — the BINDING, ridden on the shared headless engine.
--     It runs the same real module against the SHIPPED
--     @data/tutorials/first_session.yaml@ tree, loaded through the real
--     loader and handed to Lua by the real @engine.getTutorialTree@
--     projection. This is the half that catches drift: rename an
--     evaluator key in the YAML, or an objective id, and the fixture
--     above would happily keep passing while the shipped tutorial went
--     inert.
--
--   Two properties are asserted by CONSTRUCTION rather than by an
--   example, because they are absences and an example cannot see an
--   absence: the stub wires every drained event stream
--   (@engine.getEventLog@, @combat.drainEvents@, @injury.drainEvents@)
--   and every active-page enumerator (@unit.getAllIds@,
--   @building.getActiveIds@) to raise. A predicate that reaches for one
--   fails its example with that message instead of silently passing —
--   which is the only way to gate "no event dependency" (requirement 8)
--   and "global per save" (requirement 6).
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Tutorial evaluation"'@.
module Test.Headless.Lua.TutorialEvaluation (spec, luaSpec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (writeIORef)
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Core (CoreCapability, toCoreCapability)
import Engine.Core.Capability.ContentRegistries
  (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Scripting.Lua.API.Tutorial
  (loadTutorialDirFn, getTutorialTreeFn)
import Tutorial.Types (emptyTutorialRegistry)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | The @engine@ global, plus the streams this module must never reach
--   for. Everything here is deliberately hostile except the two loggers:
--   the drained event streams raise on contact, so "no event
--   dependency" is enforced rather than reviewed.
engineStub ∷ Text
engineStub = lns
    [ "WARNINGS = {}"
    , "engine = {"
    , "    logInfo = function(...) end,"
    , "    logWarn = function(m) WARNINGS[#WARNINGS + 1] = tostring(m) end,"
    , "    getEventLog = function()"
    , "        error('evaluation must not read the drained event log')"
    , "    end,"
    , "}"
    , "combat = { drainEvents = function()"
    , "    error('evaluation must not drain combat events') end }"
    , "injury = { drainEvents = function()"
    , "    error('evaluation must not drain injury events') end }"
    ]

runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        _ ← Lua.dostring (TE.encodeUtf8 engineStub)
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

-- | The shipped @first_session@ tree as @engine.getTutorialTree()@ hands
--   it to Lua — the real objective ids and the real evaluator keys, not
--   invented ones, since those keys are precisely what this gate binds.
--   'luaSpec' runs the same module against the actual YAML, so a fixture
--   that drifts from the file is caught there.
treeHelpers ∷ Text
treeHelpers = lns
    [ "local function node(id, kind, evaluator, order, children, subs)"
    , "    return { id = id, kind = kind, label = id .. ' label',"
    , "             tooltip = id .. ' tooltip', evaluator = evaluator,"
    , "             order = order, children = children or {},"
    , "             subobjectives = subs or {} }"
    , "end"
    , "function fixtureTree()"
    , "    local water = node('first_session_prepare_water', 'subobjective',"
    , "                       'prepare_water', 1)"
    , "    local food  = node('first_session_prepare_food',  'subobjective',"
    , "                       'prepare_food', 2)"
    , "    local prep  = node('first_session_prepare_expedition', 'composite',"
    , "                       'prepare_expedition', 1, nil, { water, food })"
    , "    local secure = node('first_session_secure_water', 'full',"
    , "                        'secure_water_source', 1, { prep })"
    , "    return { id = 'first_session',"
    , "             root = node('first_session_place_portal', 'full',"
    , "                         'place_portal', 1, { secure }) }"
    , "end"
    -- Short handles for the ids every case below asserts on.
    , "PORTAL = 'first_session_place_portal'"
    , "SECURE = 'first_session_secure_water'"
    , "PREP   = 'first_session_prepare_expedition'"
    , "WATER  = 'first_session_prepare_water'"
    , "FOOD   = 'first_session_prepare_food'"
    ]

-- | Item rows in the exact shape @unit.getInventory@ pushes: a container
--   declares @capacity@\/@holds@\/@currentFill@, and a non-container
--   declares none of them.
itemHelpers ∷ Text
itemHelpers = lns
    [ "function canteen(fill)"
    , "    return { defName = 'canteen_steel_2l', instanceId = 0,"
    , "             displayName = 'Steel Canteen (2L)', weight = 0.2 + fill,"
    , "             capacity = 2.0, holds = 'water', currentFill = fill }"
    , "end"
    , "function rationPack()"
    , "    return { defName = 'rations', instanceId = 0,"
    , "             displayName = 'Rations', weight = 0.35, currentFill = 0 }"
    , "end"
    -- data/items/water.yaml: a nominal 1 L crafting-input lump. It
    -- declares no container, so it has no `holds` and no fill -- the
    -- reason it must not read as water anyone can drink on the road.
    , "function waterLump()"
    , "    return { defName = 'water', instanceId = 0, displayName = 'Water',"
    , "             weight = 1.0, currentFill = 0 }"
    , "end"
    -- A canteen that is real but empty: the count-vs-fill distinction.
    , "function emptyCanteen() return canteen(0) end"
    ]

-- | Build the stub world. @spec@ is
--
--   > { portal = <bool>, units = { [uid] = { faction, def, inv, ai } } }
--
--   Every unit's AI-state entry is derived here rather than declared
--   twice, mirroring production: the AI ticks each live unit, so a unit
--   the evaluator can see has an entry.
worldHelpers ∷ Text
worldHelpers = lns
    [ "function setWorld(spec)"
    , "    WORLD = spec or {}"
    , "    WORLD.units = WORLD.units or {}"
    , "    building = {"
    , "        existsWithDef = function(defName)"
    , "            PORTAL_QUERIES[#PORTAL_QUERIES + 1] = defName"
    , "            return WORLD.portal == true and defName == 'acolyte_portal'"
    , "        end,"
    -- Requirement 6: progress is global per save, so nothing here may
    -- reach an ACTIVE-page-scoped list.
    , "        getActiveIds = function()"
    , "            error('evaluation must not enumerate the active page')"
    , "        end,"
    , "    }"
    , "    unit = {"
    , "        exists = function(uid) return WORLD.units[uid] ~= nil end,"
    , "        getFaction = function(uid)"
    , "            local u = WORLD.units[uid]"
    , "            return u and u.faction or nil"
    , "        end,"
    , "        getInfo = function(uid)"
    , "            local u = WORLD.units[uid]"
    , "            if u == nil then return nil end"
    , "            return { defName = u.def, gridX = 0, gridY = 0 }"
    , "        end,"
    , "        getInventory = function(uid)"
    , "            local u = WORLD.units[uid]"
    , "            if u == nil then return nil end"
    , "            return u.inv or {}"
    , "        end,"
    , "        getAllIds = function()"
    , "            error('evaluation must not enumerate the active page')"
    , "        end,"
    , "    }"
    , "    local ai = {}"
    , "    for uid, u in pairs(WORLD.units) do ai[uid] = u.ai or {} end"
    , "    EV.aiState = ai"
    , "end"
    , "PORTAL_QUERIES = {}"
    -- A live player acolyte carrying `inv`, with optional AI state.
    , "function acolyte(inv, ai)"
    , "    return { faction = 'player', def = 'acolyte', inv = inv, ai = ai }"
    , "end"
    , "function knowsWater() return { knownWaterSources = { { x = 9, y = 4 } } } end"
    -- The fully provisioned acolyte the spawn kit produces.
    , "function provisioned()"
    , "    return acolyte({ canteen(2.0), rationPack(), rationPack() })"
    , "end"
    ]

-- | Read a whole evaluation pass into assertions.
viewHelpers ∷ Text
viewHelpers = lns
    [ "function ids(list) return table.concat(list, ',') end"
    , "function rowById(m, id)"
    , "    for _, r in ipairs(m.rows) do if r.id == id then return r end end"
    , "    return nil"
    , "end"
    -- One update: what the tutorial believes after evaluating once.
    , "function tick()"
    , "    local results = EV.evaluate()"
    , "    assert(results ~= nil, 'evaluate() found no tree')"
    , "    return results"
    , "end"
    , "function state()"
    , "    return {"
    , "        portal = TP.isCompleted(PORTAL),"
    , "        secure = TP.isCompleted(SECURE),"
    , "        prep   = TP.isCompleted(PREP),"
    , "        water  = TP.isSubobjectiveChecked(WATER),"
    , "        food   = TP.isSubobjectiveChecked(FOOD),"
    , "    }"
    , "end"
    -- Assert the whole five-field picture at once, so a case can never
    -- pass by asserting only the field it cares about.
    , "function expect(portal, secure, prep, water, food)"
    , "    local s = state()"
    , "    local got = tostring(s.portal) .. ',' .. tostring(s.secure) .. ','"
    , "        .. tostring(s.prep) .. ',' .. tostring(s.water) .. ','"
    , "        .. tostring(s.food)"
    , "    local want = tostring(portal) .. ',' .. tostring(secure) .. ','"
    , "        .. tostring(prep) .. ',' .. tostring(water) .. ','"
    , "        .. tostring(food)"
    , "    assert(got == want, 'expected ' .. want .. ', got ' .. got)"
    , "end"
    ]

-- | Both modules loaded, the fixture tree injected, and an empty world.
--   A case then calls @setWorld@ with whatever arrangement it is about.
prelude ∷ Text
prelude = lns
    [ treeHelpers
    , itemHelpers
    , "local TP = require('scripts.tutorial_progress')"
    , "local EV = require('scripts.tutorial_eval')"
    , "TP.reset()"
    , "TP.setTree(fixtureTree())"
    -- Everything below is top level in the SAME chunk, so the helper
    -- functions close over these two locals rather than being handed
    -- either module through an argument.
    , worldHelpers
    , viewHelpers
    , "setWorld({})"
    ]

-- | 'prelude' plus the save registry, for the persistence cases.
savePrelude ∷ Text
savePrelude = lns
    [ prelude
    , "local saveModules = require('scripts.lib.save_modules')"
    , "assert(TP.register(), 'register() should register the component')"
    ]

withEV ∷ Text → [Text] → Text
withEV pre body = lns (pre : body)

spec ∷ Spec
spec = describe "Tutorial evaluation" $ do

    describe "place portal (requirement 2)" $ do
        it "does not latch while no portal has been placed" $
            runsOk $ withEV prelude
            [ "setWorld({ portal = false, units = { [1] = provisioned() } })"
            , "tick()"
            , "expect(false, false, true, true, true)"
            ]

        it "latches on a portal instance existing anywhere in the \
           \session, asked page-agnostically by def name" $
            runsOk $ withEV prelude
            [ "setWorld({ portal = true })"
            , "tick()"
            , "expect(true, false, false, false, false)"
            , "assert(PORTAL_QUERIES[1] == 'acolyte_portal',"
            , "       PORTAL_QUERIES[1] or 'never asked')"
            ]

        it "stays latched after the portal is gone — completion is \
           \durable, not a live reading" $ runsOk $ withEV prelude
            [ "setWorld({ portal = true })"
            , "tick()"
            , "assert(TP.isCompleted(PORTAL))"
            , "setWorld({ portal = false })"
            , "tick()"
            , "expect(true, false, false, false, false)"
            ]

    describe "secure water source (requirement 3)" $ do
        it "latches on a player acolyte's persisted water memory, with \
           \no event anywhere in the loop" $ runsOk $ withEV prelude
            [ "setWorld({ units = { [7] = acolyte({}, knowsWater()) } })"
            , "tick()"
            , "expect(false, true, false, false, false)"
            ]

        it "does not latch on an acolyte whose memory is empty or \
           \absent" $ runsOk $ withEV prelude
            [ "setWorld({ units = {"
            , "    [1] = acolyte({}, { knownWaterSources = {} }),"
            , "    [2] = acolyte({}, {}),"
            , "} })"
            , "tick()"
            , "expect(false, false, false, false, false)"
            ]

        it "ignores water known to a non-player or non-acolyte unit" $
            runsOk $ withEV prelude
            [ "setWorld({ units = {"
            , "    [1] = { faction = 'wildlife', def = 'bear_brown',"
            , "            inv = {}, ai = knowsWater() },"
            , "    [2] = { faction = 'player', def = 'technomule',"
            , "            inv = {}, ai = knowsWater() },"
            , "} })"
            , "tick()"
            , "expect(false, false, false, false, false)"
            ]

        it "ignores AI state left behind by a unit that no longer \
           \exists" $ runsOk $ withEV prelude
            [ "setWorld({ units = {} })"
            -- The world says there is no unit 3; a stale entry alone
            -- must not answer for one.
            , "EV.aiState = { [3] = knowsWater() }"
            , "tick()"
            , "expect(false, false, false, false, false)"
            ]

    describe "prepare an expedition (requirements 4 and 5)" $ do
        it "checks water only, and never food, for a carried canteen \
           \with no rations" $ runsOk $ withEV prelude
            [ "setWorld({ units = { [1] = acolyte({ canteen(2.0) }) } })"
            , "tick()"
            , "expect(false, false, false, true, false)"
            ]

        it "checks neither for rations carried without enough water" $
            runsOk $ withEV prelude
            [ "setWorld({ units = { [1] ="
            , "    acolyte({ canteen(1.5), rationPack() }) } })"
            , "tick()"
            , "expect(false, false, false, false, false)"
            ]

        it "does not let food on one acolyte and water on another \
           \satisfy prepare-food or the composite" $
            runsOk $ withEV prelude
            [ "setWorld({ units = {"
            , "    [1] = acolyte({ canteen(2.0) }),"
            , "    [2] = acolyte({ rationPack(), rationPack() }),"
            , "} })"
            , "tick()"
            -- Water is satisfied -- unit 1 really is carrying it -- but
            -- nobody is provisioned, so food and the composite are not.
            , "expect(false, false, false, true, false)"
            ]

        it "latches the composite once ONE acolyte carries both" $
            runsOk $ withEV prelude
            [ "setWorld({ units = {"
            , "    [1] = acolyte({ canteen(2.0) }),"
            , "    [2] = acolyte({ rationPack() }),"
            , "} })"
            , "tick()"
            , "expect(false, false, false, true, false)"
            -- Hand the rations to the one carrying the canteen.
            , "setWorld({ units = {"
            , "    [1] = acolyte({ canteen(2.0), rationPack() }),"
            , "    [2] = acolyte({}),"
            , "} })"
            , "tick()"
            , "expect(false, false, true, true, true)"
            ]

        it "sums fill across containers and ignores an empty canteen" $
            runsOk $ withEV prelude
            [ "setWorld({ units = { [1] ="
            , "    acolyte({ emptyCanteen(), rationPack() }) } })"
            , "tick()"
            , "expect(false, false, false, false, false)"
            -- Two half-full canteens are the same 2 L as one full one.
            , "setWorld({ units = { [1] ="
            , "    acolyte({ canteen(1.0), canteen(1.0), rationPack() }) } })"
            , "tick()"
            , "expect(false, false, true, true, true)"
            ]

        it "does not count the discrete `water` crafting item as \
           \carried water" $ runsOk $ withEV prelude
            [ "setWorld({ units = { [1] = acolyte("
            , "    { waterLump(), waterLump(), rationPack() }) } })"
            , "tick()"
            , "expect(false, false, false, false, false)"
            ]

        it "keeps the composite latched after the supplies are gone, \
           \while the live subobjectives uncheck" $
            runsOk $ withEV prelude
            [ "setWorld({ units = { [1] = provisioned() } })"
            , "tick()"
            , "expect(false, false, true, true, true)"
            -- The traveller drinks the canteen dry and eats.
            , "setWorld({ units = { [1] = acolyte({ emptyCanteen() }) } })"
            , "tick()"
            , "expect(false, false, true, false, false)"
            -- And re-check cleanly when it is provisioned again.
            , "setWorld({ units = { [1] = provisioned() } })"
            , "tick()"
            , "expect(false, false, true, true, true)"
            ]

        it "leaves the subobjectives live and reversible before the \
           \composite latches" $ runsOk $ withEV prelude
            [ "setWorld({ units = { [1] = acolyte({ canteen(2.0) }) } })"
            , "tick()"
            , "expect(false, false, false, true, false)"
            , "setWorld({ units = { [1] = acolyte({ canteen(0.5) }) } })"
            , "tick()"
            , "expect(false, false, false, false, false)"
            ]

    describe "the whole first_session arc" $ do
        it "advances portal, then water, then the composite, and the \
           \view model reveals each in turn" $ runsOk $ withEV prelude
            [ "setWorld({ portal = true, units = {"
            , "    [1] = acolyte({ emptyCanteen() }) } })"
            , "tick()"
            , "expect(true, false, false, false, false)"
            , "local m = TP.getViewModel()"
            , "assert(rowById(m, SECURE) ~= nil, 'portal must reveal water')"
            , "assert(rowById(m, PREP) == nil, 'composite revealed too early')"
            , "setWorld({ portal = true, units = {"
            , "    [1] = acolyte({ emptyCanteen() }, knowsWater()) } })"
            , "tick()"
            , "expect(true, true, false, false, false)"
            , "m = TP.getViewModel()"
            , "assert(rowById(m, PREP) ~= nil, 'water must reveal the composite')"
            , "assert(rowById(m, WATER).checked == false)"
            , "setWorld({ portal = true, units = {"
            , "    [1] = acolyte({ canteen(2.0), rationPack() }, knowsWater()) } })"
            , "tick()"
            , "expect(true, true, true, true, true)"
            , "m = TP.getViewModel()"
            , "assert(ids(m.completedIds) == PORTAL .. ',' .. PREP .. ',' .. SECURE,"
            , "       ids(m.completedIds))"
            ]

        it "latches the composite before it is revealed — reveal is \
           \display, not a gate (requirement 5)" $
            runsOk $ withEV prelude
            -- The shipped acolyte spawn kit is a full canteen and two
            -- rations, so a fresh colony satisfies both prepare
            -- subobjectives before secure-water ever completes. That is
            -- accepted design, not a bug to gate away.
            [ "setWorld({ units = { [1] = provisioned() } })"
            , "tick()"
            , "assert(TP.isCompleted(PREP), 'composite must latch unrevealed')"
            , "assert(rowById(TP.getViewModel(), PREP) == nil,"
            , "       'composite should not be revealed yet')"
            ]

        it "is idempotent — repeated updates over an unchanged world \
           \change nothing" $ runsOk $ withEV prelude
            [ "setWorld({ portal = true, units = {"
            , "    [1] = acolyte({ canteen(2.0), rationPack() }, knowsWater()) } })"
            , "tick(); tick(); tick()"
            , "expect(true, true, true, true, true)"
            , "assert(#TP.completedIds() == 3, ids(TP.completedIds()))"
            , "assert(#WARNINGS == 0, WARNINGS[1] or '')"
            ]

    describe "unbound and malformed wiring" $ do
        it "warns and skips an evaluator key with no predicate, without \
           \failing the pass" $ runsOk $ withEV prelude
            [ "local t = fixtureTree()"
            , "t.root.evaluator = 'not_a_real_key'"
            , "TP.setTree(t)"
            , "setWorld({ portal = true, units = {"
            , "    [1] = acolyte({}, knowsWater()) } })"
            , "tick()"
            , "expect(false, true, false, false, false)"
            , "assert(#WARNINGS == 1, tostring(#WARNINGS))"
            , "assert(WARNINGS[1]:find('not_a_real_key', 1, true), WARNINGS[1])"
            ]

        it "reports nothing, and does not error, when no tree is \
           \available" $ runsOk $ withEV prelude
            [ "TP.setTree(nil)"
            , "setWorld({ portal = true, units = { [1] = provisioned() } })"
            , "assert(EV.evaluate() == nil)"
            , "assert(#TP.completedIds() == 0)"
            ]

        it "evaluates to nothing, rather than erroring, before the unit \
           \AI has published its state" $ runsOk $ withEV prelude
            [ "setWorld({ portal = true, units = { [1] = provisioned() } })"
            -- No AI state at all: the module resolves it lazily and a
            -- boot that has not reached the unit AI yet must not crash
            -- the tutorial tick.
            , "EV.aiState = nil"
            , "tick()"
            , "expect(true, false, false, false, false)"
            ]

    describe "save and load" $ do
        it "persists latched objectives and recomputes live checks from \
           \the loaded world (requirement 8)" $ runsOk $ withEV savePrelude
            [ "setWorld({ portal = true, units = {"
            , "    [1] = acolyte({ canteen(2.0), rationPack() }, knowsWater()) } })"
            , "tick()"
            , "expect(true, true, true, true, true)"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, snap.error)"
            -- Come back to a session whose acolyte has since drunk the
            -- canteen dry: the latches must survive, the checks must not.
            , "TP.reset()"
            , "local prep = saveModules.prepareLoad(snap.components)"
            , "assert(prep.ok, prep.errors and table.concat(prep.errors, '; '))"
            , "saveModules.applyAll()"
            , "expect(true, true, true, false, false)"
            , "setWorld({ portal = false, units = {"
            , "    [1] = acolyte({ emptyCanteen() }) } })"
            , "tick()"
            -- Durable completions are untouched by a world that no
            -- longer satisfies them; the live checks stay off.
            , "expect(true, true, true, false, false)"
            , "assert(ids(TP.completedIds()) == PORTAL .. ',' .. PREP .. ',' .. SECURE,"
            , "       ids(TP.completedIds()))"
            ]

        it "re-checks the subobjectives on the first update after a \
           \load whose world still satisfies them" $
            runsOk $ withEV savePrelude
            [ "setWorld({ portal = true, units = { [1] = provisioned() } })"
            , "tick()"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, snap.error)"
            , "TP.reset()"
            , "local prep = saveModules.prepareLoad(snap.components)"
            , "assert(prep.ok, prep.errors and table.concat(prep.errors, '; '))"
            , "saveModules.applyAll()"
            , "assert(TP.isSubobjectiveChecked(WATER) == false,"
            , "       'a load must not restore live checks')"
            , "tick()"
            , "expect(true, false, true, true, true)"
            -- #996: the composite is still unrevealed here (secure_water
            -- has not completed), so this alone does not yet exercise
            -- the display bug. Let the SAME acolyte's water discovery
            -- complete secure_water now -- prepare_expedition's ancestor
            -- chain completes for the first time here, with the
            -- composite (and both subobjectives) already latched from
            -- before the save. A load must not have lost that history:
            -- the checklist must stay non-empty rather than latching and
            -- immediately hiding on this very tick.
            , "setWorld({ portal = true, units = {"
            , "    [1] = acolyte({ canteen(2.0), rationPack(), rationPack() },"
            , "                  knowsWater()) } })"
            , "tick()"
            , "expect(true, true, true, true, true)"
            , "local m = TP.getViewModel()"
            , "assert(rowById(m, PREP) ~= nil and rowById(m, PREP).active == true,"
            , "       'prepare_expedition must stay observable (#996)')"
            , "assert(rowById(m, WATER) ~= nil and rowById(m, WATER).active == true)"
            , "assert(rowById(m, FOOD) ~= nil and rowById(m, FOOD).active == true)"
            ]

-- | The shipped tutorial directory, as boot loads it.
shippedTutorialDir ∷ FilePath
shippedTutorialDir = "data/tutorials"

-- | The same evaluator module, bound to the REAL
--   @data/tutorials/first_session.yaml@ tree rather than a fixture:
--   loaded through the real loader verb and handed to Lua by the real
--   @engine.getTutorialTree@ projection, so every authored evaluator key
--   and objective id in the shipped file is the one under test.
--
--   Nothing here re-asserts predicate behavior — that is 'spec'. What it
--   asserts is that the shipped keys are all BOUND (no diagnostic
--   warning is emitted for any of them) and that a world satisfying
--   every predicate completes the shipped ids.
luaSpec ∷ SpecWith EngineEnv
luaSpec = describe "Tutorial evaluation (shipped tree)" $
    it "binds every evaluator key the shipped first_session tree \
       \declares, and completes its real objective ids" $ \env → do
        let core = toCoreCapability env
            regs = toContentRegistriesCapability env
        -- Leave the shared engine as we found it: this ref is ours
        -- alone, and no other spec reads it.
        writeIORef (crTutorialRegistryRef regs) emptyTutorialRegistry
        result ← Lua.run @Lua.Exception $ do
            Lua.openlibs
            _ ← Lua.dostring (TE.encodeUtf8 engineStub)
            loaded ← loadDir core regs shippedTutorialDir
            -- Stash the real tree in a global and let the module reach
            -- it the way production does, through engine.getTutorialTree.
            _ ← getTutorialTreeFn regs
            Lua.setglobal (Lua.Name "SHIPPED_TREE")
            status ← Lua.dostring (TE.encodeUtf8 shippedTreeChunk)
            msg ← case status of
                Lua.OK → return Nothing
                _ → do
                    err ← Lua.tostring (-1)
                    return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
            return (loaded, msg)
        let (loaded, msg) = result
        loaded `shouldBe` Just 1
        case msg of
            Nothing → pure ()
            Just m  → expectationFailure (T.unpack m)
        writeIORef (crTutorialRegistryRef regs) emptyTutorialRegistry
  where
    -- One engine.loadTutorialDir(dir) call, leaving the stack exactly
    -- as it found it. 'loadTutorialDirFn' reads its argument at the
    -- ABSOLUTE index 1, so the stack must be restored around it.
    loadDir ∷ CoreCapability → ContentRegistriesCapability → FilePath
            → Lua.LuaE Lua.Exception (Maybe Lua.Integer)
    loadDir core regs path = do
        top ← Lua.gettop
        Lua.pushstring (TE.encodeUtf8 (T.pack path))
        _ ← loadTutorialDirFn core regs
        r ← Lua.tointeger (-1)
        Lua.settop top
        return r

-- | The Lua half of 'luaSpec': the real modules, the real tree, and a
--   world that satisfies every shipped predicate at once.
shippedTreeChunk ∷ Text
shippedTreeChunk = lns
    [ "engine.getTutorialTree = function() return SHIPPED_TREE end"
    , itemHelpers
    , "local TP = require('scripts.tutorial_progress')"
    , "local EV = require('scripts.tutorial_eval')"
    , worldHelpers
    , "setWorld({ portal = true, units = { [1] = {"
    , "    faction = 'player', def = 'acolyte',"
    , "    inv = { canteen(2.0), rationPack() },"
    , "    ai = { knownWaterSources = { { x = 3, y = 8 } } } } } })"
    , "local results = EV.evaluate()"
    , "assert(results ~= nil, 'the shipped tree did not reach the evaluator')"
    -- Every authored key must have had a predicate: an unbound one is
    -- the only thing that warns during a clean pass.
    , "assert(#WARNINGS == 0, WARNINGS[1] or '')"
    -- Walk the tree the evaluator walked and require an answer for
    -- every node, so a NEW objective added to the YAML without a
    -- binding fails here rather than shipping inert.
    , "local index = TP.index"
    , "assert(index ~= nil, 'no tree index')"
    , "assert(#index.order == 5, tostring(#index.order))"
    , "for _, id in ipairs(index.order) do"
    , "    assert(results[id] == true, 'unsatisfied objective: ' .. id)"
    , "end"
    , "assert(TP.isCompleted('first_session_place_portal'))"
    , "assert(TP.isCompleted('first_session_secure_water'))"
    , "assert(TP.isCompleted('first_session_prepare_expedition'))"
    , "assert(TP.isSubobjectiveChecked('first_session_prepare_water'))"
    , "assert(TP.isSubobjectiveChecked('first_session_prepare_food'))"
    ]
