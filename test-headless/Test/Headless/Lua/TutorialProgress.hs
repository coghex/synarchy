{-# LANGUAGE UnicodeSyntax, OverloadedStrings, TypeApplications #-}
-- | The "Tutorial progress" gate (#958, phase 2 of the tutorial epic
--   #956): @scripts/tutorial_progress.lua@'s two-lifetime progress
--   state — the durable, monotonic completed-full-objective set and the
--   live, reversible subobjective checks — the HUD-independent
--   tree-state rules over them, the read-only view model, and the
--   @lua.tutorial_progress@ save component that persists exactly the
--   first of those.
--
--   Same standalone-Lua-VM pattern as "Test.Headless.Lua.SaveModules"
--   and "Test.Headless.Lua.UnitAiLocations": each 'it' runs one
--   self-contained chunk via 'Lua.dostring' in a fresh interpreter
--   (stdlib + a minimal @engine@ stub), asserting inside Lua via
--   @assert()@/@error()@, with a non-OK 'Lua.Status' surfaced as an
--   hspec failure carrying the Lua message. The persistence half runs
--   the PRODUCTION path — @saveModules.snapshotAll@ →
--   @prepareLoad@ → @applyAll@ against the real
--   @scripts/lib/save_modules.lua@ registry, with payloads encoded by
--   the real @scripts/lib/data_codec.lua@ — not a hand-rolled stand-in.
--
--   The tutorial TREE is injected via @tutorialProgress.setTree@ rather
--   than loaded: there is no engine here to hold #957's registry, and
--   the tree's own parsing/validation/ordering is that issue's gate
--   ("Tutorial definitions"). The fixture below is the shipped
--   @first_session@ SHAPE — a two-link full chain ending in a composite
--   with two subobjectives — written directly as the table
--   @engine.getTutorialTree()@ hands Lua.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Tutorial progress"'@.
module Test.Headless.Lua.TutorialProgress (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | A minimal @engine@ global: the diagnostics this module and
--   @save_modules.snapshotAll@ emit are the only things either reaches
--   outside a real boot. @engine.getTutorialTree@ is deliberately
--   ABSENT here — most cases inject their tree, and the module must
--   fall back cleanly when the engine offers none. 'lazyTreePrelude'
--   installs one, to cover the production path where the tree is
--   resolved lazily through the engine and nothing else.
engineStub ∷ Text
engineStub =
    "engine = { logWarn = function(...) end, logInfo = function(...) end }"

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

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | The fixture trees, as Lua source. Shared by the injected-tree
--   prelude below and by 'lazyTreePrelude', which reaches the SAME
--   tree the way production does — through the engine.
treeHelpers ∷ Text
treeHelpers = lns
    [ "local function node(id, kind, order, children, subs)"
    , "    return { id = id, kind = kind, label = id .. ' label',"
    , "             tooltip = id .. ' tooltip', evaluator = id .. '_eval',"
    , "             order = order, children = children or {},"
    , "             subobjectives = subs or {} }"
    , "end"
    -- The shipped first_session shape: place_portal -> secure_water ->
    -- prepare_expedition{prepare_water, prepare_food}.
    , "function fixtureTree()"
    , "    local water = node('prepare_water', 'subobjective', 1)"
    , "    local food  = node('prepare_food',  'subobjective', 2)"
    , "    local prep  = node('prepare_expedition', 'composite', 1, nil,"
    , "                       { water, food })"
    , "    local secure = node('secure_water', 'full', 1, { prep })"
    , "    return { id = 'first_session', root = node('place_portal', 'full', 1,"
    , "                                               { secure }) }"
    , "end"
    -- A single full objective with no children and no subobjectives --
    -- the leaf hide rule's own fixture.
    , "function leafTree()"
    , "    return { id = 'first_session', root = node('only', 'full', 1) }"
    , "end"
    ]

-- | The view-model readers every case shares.
viewHelpers ∷ Text
viewHelpers = lns
    [ "function rowIds(m)"
    , "    local out = {}"
    , "    for _, r in ipairs(m.rows) do out[#out + 1] = r.id end"
    , "    return table.concat(out, ',')"
    , "end"
    , "function activeIds(m)"
    , "    local out = {}"
    , "    for _, r in ipairs(m.rows) do"
    , "        if r.active then out[#out + 1] = r.id end"
    , "    end"
    , "    return table.concat(out, ',')"
    , "end"
    , "function rowById(m, id)"
    , "    for _, r in ipairs(m.rows) do if r.id == id then return r end end"
    , "    return nil"
    , "end"
    , "function ids(list) return table.concat(list, ',') end"
    ]

-- | Load the module and INJECT the fixture tree, plus the shared
--   readers. What most cases want: the tree resolved up front so a case
--   is about the rules, not about tree resolution.
prelude ∷ Text
prelude = lns
    [ treeHelpers
    , "local TP = require('scripts.tutorial_progress')"
    , "TP.reset()"
    , "TP.setTree(fixtureTree())"
    , viewHelpers
    ]

-- | The save-registry helpers, as Lua source. Appended to whichever
--   prelude a persistence case needs.
saveHelpers ∷ Text
saveHelpers = lns
    [ "local saveModules = require('scripts.lib.save_modules')"
    , "local codec = require('scripts.lib.data_codec')"
    , "assert(TP.register(), 'register() should register the component')"
    -- Find one component by id in a snapshotAll()/prepareLoad() list.
    , "function componentNamed(list, id)"
    , "    for _, c in ipairs(list) do if c.id == id then return c end end"
    , "    return nil"
    , "end"
    -- The components list a save would hand back, with `payload` a
    -- real data_codec encoding of `data`.
    , "function componentsFor(data, version)"
    , "    local payload, err = codec.encode(data)"
    , "    assert(payload ~= nil, tostring(err))"
    , "    return { { id = 'tutorial_progress', version = version or 1,"
    , "               payload = payload } }"
    , "end"
    ]

-- | 'prelude' plus a registered save component and the two registry
--   modules the persistence cases drive directly.
savePrelude ∷ Text
savePrelude = lns [prelude, saveHelpers]

-- | The production shape: the tree is reachable ONLY through a stubbed
--   @engine.getTutorialTree@, and nothing resolves it. A real boot looks
--   exactly like this at the moment a load applies — @init()@ only
--   registers the component, so no view model has ever been built and
--   the module's lazy tree is still unresolved.
lazyTreePrelude ∷ Text
lazyTreePrelude = lns
    [ treeHelpers
    , "engine.getTutorialTree = function() return fixtureTree() end"
    , "local TP = require('scripts.tutorial_progress')"
    , saveHelpers
    , viewHelpers
    ]

-- | One chunk: a prelude followed by the case body. Both halves are
--   top level in the SAME chunk, so the prelude's @local TP@ (and
--   @saveModules@\/@codec@ in 'savePrelude') stay in scope throughout.
withTP ∷ Text → [Text] → Text
withTP pre body = lns (pre : body)

spec ∷ Spec
spec = describe "Tutorial progress" $ do

    describe "fresh state" $ do
        it "starts with nothing completed and only the root \
           \display-eligible" $ runsOk $ withTP prelude
            [ "local m = TP.getViewModel()"
            , "assert(m.treeId == 'first_session')"
            , "assert(rowIds(m) == 'place_portal', rowIds(m))"
            , "assert(activeIds(m) == 'place_portal', activeIds(m))"
            , "assert(rowById(m, 'place_portal').completed == false)"
            , "assert(#m.completedIds == 0)"
            ]

        it "reports an empty model, not an error, when no tree is \
           \available" $ runsOk $ withTP prelude
            [ "TP.setTree(nil)"
            , "local m = TP.getViewModel()"
            , "assert(m.treeId == nil)"
            , "assert(#m.rows == 0)"
            , "assert(#m.completedIds == 0)"
            ]

    describe "monotonic full-objective completion (requirement 2)" $ do
        it "latches exactly once — a re-latch is a silent no-op" $
            runsOk $ withTP prelude
            [ "assert(TP.completeObjective('place_portal') == true)"
            , "assert(TP.completeObjective('place_portal') == false)"
            , "assert(TP.isCompleted('place_portal'))"
            , "assert(ids(TP.completedIds()) == 'place_portal')"
            ]

        it "stays completed when the live state that satisfied it \
           \reverses" $ runsOk $ withTP prelude
            [ "TP.completeObjective('place_portal')"
            , "TP.completeObjective('secure_water')"
            , "TP.setSubobjectiveChecked('prepare_water', true)"
            , "TP.setSubobjectiveChecked('prepare_food', true)"
            , "assert(TP.completeObjective('prepare_expedition') == true)"
            -- The acolyte drinks the travel water back down: the live
            -- check flips, the latch does not.
            , "TP.setSubobjectiveChecked('prepare_water', false)"
            , "assert(TP.isCompleted('prepare_expedition'))"
            , "assert(TP.isSubobjectiveChecked('prepare_water') == false)"
            ]

        it "refuses an unknown objective id as a diagnostic no-op" $
            runsOk $ withTP prelude
            [ "assert(TP.completeObjective('no_such_objective') == false)"
            , "assert(TP.completeObjective(nil) == false)"
            , "assert(#TP.completedIds() == 0)"
            ]

        it "refuses a SUBOBJECTIVE id — its state is live, not durable" $
            runsOk $ withTP prelude
            [ "assert(TP.completeObjective('prepare_water') == false)"
            , "assert(TP.isCompleted('prepare_water') == false)"
            , "assert(#TP.completedIds() == 0)"
            ]

    describe "live subobjective checks (requirement 3)" $ do
        it "checks and unchecks, reporting only real changes" $
            runsOk $ withTP prelude
            [ "assert(TP.setSubobjectiveChecked('prepare_water', true) == true)"
            , "assert(TP.setSubobjectiveChecked('prepare_water', true) == false)"
            , "assert(TP.isSubobjectiveChecked('prepare_water'))"
            , "assert(TP.setSubobjectiveChecked('prepare_water', false) == true)"
            , "assert(TP.isSubobjectiveChecked('prepare_water') == false)"
            ]

        it "refuses a FULL objective id — a full objective latches" $
            runsOk $ withTP prelude
            [ "assert(TP.setSubobjectiveChecked('place_portal', true) == false)"
            , "assert(TP.setSubobjectiveChecked('nope', true) == false)"
            , "assert(TP.isSubobjectiveChecked('place_portal') == false)"
            ]

        it "never reaches the durable progress set" $
            runsOk $ withTP prelude
            [ "TP.setSubobjectiveChecked('prepare_water', true)"
            , "TP.setSubobjectiveChecked('prepare_food', true)"
            , "assert(#TP.completedIds() == 0)"
            ]

    describe "tree-state rules (requirement 4)" $ do
        it "a completed full objective reveals its authored children" $
            runsOk $ withTP prelude
            [ "TP.completeObjective('place_portal')"
            , "local m = TP.getViewModel()"
            , "assert(rowIds(m) == 'place_portal,secure_water', rowIds(m))"
            , "assert(rowById(m, 'secure_water').completed == false)"
            , "assert(rowById(m, 'secure_water').active == true)"
            ]

        it "a parent stays available until its child completes, then \
           \hides but stays as history" $ runsOk $ withTP prelude
            [ "TP.completeObjective('place_portal')"
            , "local m = TP.getViewModel()"
            -- Child still in progress: the completed parent is still on
            -- screen above it.
            , "assert(activeIds(m) == 'place_portal,secure_water', activeIds(m))"
            , "TP.completeObjective('secure_water')"
            , "m = TP.getViewModel()"
            , "assert(rowById(m, 'place_portal') ~= nil, 'history row dropped')"
            , "assert(rowById(m, 'place_portal').active == false)"
            , "assert(rowById(m, 'place_portal').completed == true)"
            , "assert(activeIds(m) == 'secure_water,prepare_expedition,"
              <> "prepare_water,prepare_food', activeIds(m))"
            ]

        it "a composite displays its subobjectives while active" $
            runsOk $ withTP prelude
            [ "TP.completeObjective('place_portal')"
            , "TP.completeObjective('secure_water')"
            , "local m = TP.getViewModel()"
            , "assert(rowById(m, 'prepare_water') ~= nil)"
            , "assert(rowById(m, 'prepare_water').checked == false)"
            , "TP.setSubobjectiveChecked('prepare_water', true)"
            , "m = TP.getViewModel()"
            , "assert(rowById(m, 'prepare_water').checked == true)"
            , "assert(rowById(m, 'prepare_water').active == true)"
            ]

        it "a composite hides once completed with every subobjective \
           \checked, and returns when one unchecks" $
            runsOk $ withTP prelude
            [ "TP.completeObjective('place_portal')"
            , "TP.completeObjective('secure_water')"
            , "TP.setSubobjectiveChecked('prepare_water', true)"
            , "TP.setSubobjectiveChecked('prepare_food', true)"
            , "TP.completeObjective('prepare_expedition')"
            , "local m = TP.getViewModel()"
            , "assert(rowById(m, 'prepare_expedition').active == false)"
            -- Hiding the composite hides the rows it was displaying.
            , "assert(rowById(m, 'prepare_water') == nil)"
            , "assert(activeIds(m) == '', activeIds(m))"
            -- A live check reversing brings the composite back, with
            -- its latch untouched.
            , "TP.setSubobjectiveChecked('prepare_water', false)"
            , "m = TP.getViewModel()"
            , "assert(rowById(m, 'prepare_expedition').active == true)"
            , "assert(rowById(m, 'prepare_expedition').completed == true)"
            , "assert(rowById(m, 'prepare_water').checked == false)"
            ]

        it "a completed leaf hides on its own completion and stays \
           \queryable as history" $ runsOk $ withTP prelude
            [ "TP.setTree(leafTree())"
            , "local m = TP.getViewModel()"
            , "assert(activeIds(m) == 'only', activeIds(m))"
            , "TP.completeObjective('only')"
            , "m = TP.getViewModel()"
            , "assert(activeIds(m) == '', activeIds(m))"
            , "assert(rowById(m, 'only').active == false)"
            , "assert(rowById(m, 'only').completed == true)"
            , "assert(ids(m.completedIds) == 'only')"
            ]

        it "identifies each row by YAML id and states full completion \
           \and live check state in DIFFERENT fields" $
            runsOk $ withTP prelude
            [ "TP.completeObjective('place_portal')"
            , "TP.completeObjective('secure_water')"
            , "local m = TP.getViewModel()"
            , "local full = rowById(m, 'secure_water')"
            , "assert(full.kind == 'full')"
            , "assert(full.completed == true and full.checked == nil)"
            , "assert(full.label == 'secure_water label')"
            , "assert(full.evaluator == 'secure_water_eval')"
            , "assert(full.parent == 'place_portal' and full.depth == 1)"
            , "local comp = rowById(m, 'prepare_expedition')"
            , "assert(comp.kind == 'composite')"
            , "assert(comp.completed == false and comp.checked == nil)"
            , "local sub = rowById(m, 'prepare_food')"
            , "assert(sub.kind == 'subobjective')"
            , "assert(sub.checked == false and sub.completed == nil)"
            , "assert(sub.relation == 'subobjective' and sub.depth == 3)"
            ]

        it "hands back a fresh model — mutating it changes no state" $
            runsOk $ withTP prelude
            [ "local m = TP.getViewModel()"
            , "m.rows[1].completed = true"
            , "m.completedIds[#m.completedIds + 1] = 'place_portal'"
            , "assert(TP.isCompleted('place_portal') == false)"
            , "assert(#TP.getViewModel().completedIds == 0)"
            , "assert(TP.getViewModel().rows[1].completed == false)"
            ]

    describe "save component (requirements 1/6/7)" $ do
        it "registers one optional, global, v1 component, and a second \
           \register() is a no-op rather than a duplicate-id error" $
            runsOk $ withTP savePrelude
            [ "assert(TP.register() == false)"
            , "local d = componentNamed(saveModules.describeAll(),"
            , "                         'tutorial_progress')"
            , "assert(d ~= nil, 'component not registered')"
            , "assert(d.version == 1)"
            , "assert(d.required == false, 'must be optional -- requirement 7')"
            , "local reg = saveModules.registry['tutorial_progress']"
            , "assert(reg.scope == 'global', reg.scope)"
            , "assert(#reg.deps == 0)"
            , "assert(#reg.inputVersions == 1 and reg.inputVersions[1] == 1)"
            ]

        it "snapshots ONLY completed full-objective ids, in canonical \
           \order" $ runsOk $ withTP savePrelude
            [ "TP.completeObjective('place_portal')"
            , "TP.completeObjective('secure_water')"
            , "TP.setSubobjectiveChecked('prepare_water', true)"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, snap.error)"
            , "local c = componentNamed(snap.components, 'tutorial_progress')"
            , "assert(c ~= nil and c.version == 1)"
            , "local data = codec.decode(c.payload)"
            , "assert(ids(data.completed) == 'place_portal,secure_water',"
            , "       ids(data.completed))"
            -- Nothing about the live check, and nothing else at all.
            , "local fields = 0"
            , "for _ in pairs(data) do fields = fields + 1 end"
            , "assert(fields == 1, 'payload carries more than `completed`')"
            ]

        it "round trips: full objectives survive, live subobjectives are \
           \recomputed" $ runsOk $ withTP savePrelude
            [ "TP.completeObjective('place_portal')"
            , "TP.completeObjective('secure_water')"
            , "TP.setSubobjectiveChecked('prepare_water', true)"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, snap.error)"
            -- Diverge the live state so the load has something to undo.
            , "TP.completeObjective('prepare_expedition')"
            , "TP.setSubobjectiveChecked('prepare_food', true)"
            , "local prep = saveModules.prepareLoad(snap.components)"
            , "assert(prep.ok, prep.errors and table.concat(prep.errors, '; '))"
            , "saveModules.applyAll()"
            , "assert(ids(TP.completedIds()) == 'place_portal,secure_water',"
            , "       ids(TP.completedIds()))"
            , "assert(TP.isCompleted('prepare_expedition') == false)"
            -- Requirement 3: the checked subobjective is NOT restored --
            -- it is live state the next evaluation tick recomputes.
            , "assert(TP.isSubobjectiveChecked('prepare_water') == false)"
            , "assert(TP.isSubobjectiveChecked('prepare_food') == false)"
            ]

        it "round trips fresh progress — a session saved before any \
           \objective completes" $ runsOk $ withTP savePrelude
            [ "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, snap.error)"
            , "local c = componentNamed(snap.components, 'tutorial_progress')"
            , "assert(c ~= nil, 'an empty component must still be written')"
            , "assert(#codec.decode(c.payload).completed == 0)"
            , "TP.completeObjective('place_portal')"
            , "local prep = saveModules.prepareLoad(snap.components)"
            , "assert(prep.ok, prep.errors and table.concat(prep.errors, '; '))"
            , "saveModules.applyAll()"
            , "assert(#TP.completedIds() == 0)"
            , "assert(activeIds(TP.getViewModel()) == 'place_portal')"
            ]

        it "a save predating the component loads with fresh progress \
           \(requirement 7)" $ runsOk $ withTP savePrelude
            [ "TP.completeObjective('place_portal')"
            -- A supported save written before this component existed
            -- carries no lua.tutorial_progress entry at all.
            , "local prep = saveModules.prepareLoad({})"
            , "assert(prep.ok, prep.errors and table.concat(prep.errors, '; '))"
            , "saveModules.applyAll()"
            , "assert(#TP.completedIds() == 0, 'expected default() fresh state')"
            , "assert(#TP.getViewModel().rows == 1)"
            ]

        it "tolerates and scrubs a completed id the loaded tree no \
           \longer defines, without failing the load" $
            runsOk $ withTP savePrelude
            [ "local prep = saveModules.prepareLoad(componentsFor("
            , "    { completed = { 'ghost_objective', 'place_portal',"
            , "                    'prepare_water' } }))"
            , "assert(prep.ok, prep.errors and table.concat(prep.errors, '; '))"
            , "saveModules.applyAll()"
            -- 'ghost_objective' is gone from the tree; 'prepare_water'
            -- is real but is a SUBOBJECTIVE, so it can never be durable
            -- progress. Both are dropped, the real one is kept.
            , "assert(ids(TP.completedIds()) == 'place_portal',"
            , "       ids(TP.completedIds()))"
            ]

        -- Round-1 review (PR #962): apply() used to reconcile against a
        -- tree nobody had resolved yet. On a REAL load nothing has asked
        -- this module for anything -- init() only registers the
        -- component -- so `index` was still nil, reconcile was a silent
        -- no-op, and a renamed/removed objective id survived the load
        -- and got written straight back out by the next save. No
        -- setTree, no getViewModel anywhere before the load here: that
        -- absence IS the regression.
        it "scrubs a dangling id on a load that never resolved the tree \
           \first, so the next save cannot write it back" $
            runsOk $ withTP lazyTreePrelude
            [ "local prep = saveModules.prepareLoad(componentsFor("
            , "    { completed = { 'ghost_objective', 'place_portal' } }))"
            , "assert(prep.ok, prep.errors and table.concat(prep.errors, '; '))"
            , "saveModules.applyAll()"
            , "assert(ids(TP.completedIds()) == 'place_portal',"
            , "       ids(TP.completedIds()))"
            -- The point of the scrub: it must not come back on write.
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, snap.error)"
            , "local c = componentNamed(snap.components, 'tutorial_progress')"
            , "assert(ids(codec.decode(c.payload).completed) == 'place_portal',"
            , "       ids(codec.decode(c.payload).completed))"
            -- And the tree really did resolve through the engine, so the
            -- view model works without an injected tree.
            , "assert(TP.getViewModel().treeId == 'first_session')"
            ]

        it "keeps an unjudgeable id when NO tree is available at all — \
           \'cannot judge' is not 'wrong'" $ runsOk $ withTP savePrelude
            [ "TP.setTree(nil)"
            , "local prep = saveModules.prepareLoad(componentsFor("
            , "    { completed = { 'place_portal', 'unknowable' } }))"
            , "assert(prep.ok, prep.errors and table.concat(prep.errors, '; '))"
            , "saveModules.applyAll()"
            , "assert(ids(TP.completedIds()) == 'place_portal,unknowable',"
            , "       ids(TP.completedIds()))"
            -- ...and the scrub happens the moment a tree does arrive.
            , "TP.setTree(fixtureTree())"
            , "assert(ids(TP.completedIds()) == 'place_portal',"
            , "       ids(TP.completedIds()))"
            ]

        it "rejects a structurally malformed payload" $
            runsOk $ withTP savePrelude
            [ "local function rejects(data, why)"
            , "    local prep = saveModules.prepareLoad(componentsFor(data))"
            , "    assert(not prep.ok, 'expected rejection: ' .. why)"
            , "    saveModules.abortPreparedLoad()"
            , "    return table.concat(prep.errors, '; ')"
            , "end"
            , "assert(rejects('nope', 'payload is not a table')"
            , "       :find('must be a table'))"
            , "assert(rejects({}, 'no completed field')"
            , "       :find('`completed` must be an array'))"
            , "assert(rejects({ completed = 'x' }, 'completed is not an array')"
            , "       :find('`completed` must be an array'))"
            , "assert(rejects({ completed = { 'a' }, extra = 1 }, 'unknown field')"
            , "       :find(\"unknown payload field 'extra'\"))"
            , "assert(rejects({ completed = { 'a', 42 } }, 'non-string id')"
            , "       :find('must be a non%-empty objective id string'))"
            , "assert(rejects({ completed = { 'a', '' } }, 'empty id')"
            , "       :find('must be a non%-empty objective id string'))"
            , "assert(rejects({ completed = { 'a', 'a' } }, 'repeated id')"
            , "       :find('repeats objective id'))"
            , "assert(rejects({ completed = { [2] = 'a' } }, 'sparse array')"
            , "       :find('dense array'))"
            -- A rejected load leaves the live state untouched.
            , "assert(#TP.completedIds() == 0)"
            ]

        it "rejects an unsupported schema version" $
            runsOk $ withTP savePrelude
            [ "local prep = saveModules.prepareLoad("
            , "    componentsFor({ completed = {} }, 2))"
            , "assert(not prep.ok, 'a v2 payload must not decode as v1')"
            , "assert(table.concat(prep.errors, '; ')"
            , "       :find('unsupported schema version'))"
            ]

        it "accepts a payload whose ids are all still real, keeping the \
           \whole set" $ runsOk $ withTP savePrelude
            [ "local prep = saveModules.prepareLoad(componentsFor("
            , "    { completed = { 'place_portal', 'prepare_expedition',"
            , "                    'secure_water' } }))"
            , "assert(prep.ok, prep.errors and table.concat(prep.errors, '; '))"
            , "saveModules.applyAll()"
            , "assert(ids(TP.completedIds()) =="
            , "       'place_portal,prepare_expedition,secure_water')"
            -- Everything completed, nothing checked: the whole chain is
            -- history and the composite is back in the active view
            -- waiting on its unchecked subobjectives.
            , "local m = TP.getViewModel()"
            , "assert(activeIds(m) == 'prepare_expedition,prepare_water,"
              <> "prepare_food', activeIds(m))"
            ]
