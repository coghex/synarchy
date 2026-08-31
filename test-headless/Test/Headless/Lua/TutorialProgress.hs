{-# LANGUAGE TypeApplications #-}
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

-- | 'prelude' plus enough of a @world@\/@engine@ stub to run the REAL
--   @scripts/world_manager.lua@. That module has no module-scope
--   requires and reaches only these two globals, so the new-session
--   boundary can be driven for real here rather than re-asserted from a
--   copy of it.
worldManagerPrelude ∷ Text
worldManagerPrelude = lns
    [ prelude
    , "world = { inited = {},"
    , "          init = function(id) world.inited[#world.inited + 1] = id end,"
    , "          setTexture = function() end }"
    , "engine.getTextureHandle = function() return -1 end"
    , "local worldManager = require('scripts.world_manager')"
    , "function newWorld(id)"
    , "    worldManager.createWorld({ worldId = id or 'main_world' })"
    , "end"
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

        -- #996: a full/composite branch that latches BEFORE it is ever
        -- revealed (the shipped acolyte spawn kit satisfies both prepare
        -- subobjectives immediately, so the composite can latch long
        -- before secure_water ever completes) must not vanish the
        -- instant it finally IS revealed. Distinguishing rule: reveal
        -- ORDER, not durable state alone -- a node revealed while still
        -- incomplete keeps the ordinary hide-on-completion behavior
        -- pinned by the tests above; only a node whose first reveal
        -- finds it already latched gets forced active.
        it "keeps an already-latched branch observable the first time \
           \it is revealed, without disturbing the ordinary hide rule \
           \for its ancestors (#996)" $
            runsOk $ withTP prelude
            [ -- Regression step 1: latch the composite and check both
              -- subobjectives before that branch is ever revealed.
              "TP.setSubobjectiveChecked('prepare_water', true)"
            , "TP.setSubobjectiveChecked('prepare_food', true)"
            , "assert(TP.completeObjective('prepare_expedition') == true)"
            , "local before = TP.getViewModel()"
            , "assert(rowById(before, 'prepare_expedition') == nil,"
            , "       'must not be revealed before its ancestors complete')"
              -- Regression step 2: complete place_portal, then secure_water.
            , "TP.completeObjective('place_portal')"
            , "TP.completeObjective('secure_water')"
              -- Regression step 3: the checklist stays non-empty and
              -- exposes the whole prepare branch in authored preorder,
              -- already complete -- while place_portal/secure_water
              -- still leave the active view exactly as before (their
              -- OWN first reveal found them incomplete).
            , "local m = TP.getViewModel()"
            , "assert(rowIds(m) == 'place_portal,secure_water,"
              <> "prepare_expedition,prepare_water,prepare_food', rowIds(m))"
            , "assert(activeIds(m) == 'prepare_expedition,prepare_water,"
              <> "prepare_food', activeIds(m))"
            , "assert(rowById(m, 'prepare_expedition').completed == true)"
            , "assert(rowById(m, 'prepare_water').checked == true)"
            , "assert(rowById(m, 'prepare_food').checked == true)"
              -- Regression step 4: removing the supplies afterwards
              -- still unchecks the live subobjectives and never touches
              -- the durable completion -- and, until the branch has
              -- been presented, the composite was never hidden at all.
            , "TP.setSubobjectiveChecked('prepare_water', false)"
            , "m = TP.getViewModel()"
            , "assert(rowById(m, 'prepare_water').checked == false)"
            , "assert(rowById(m, 'prepare_expedition').active == true)"
            , "assert(rowById(m, 'prepare_expedition').completed == true)"
              -- Regression step 5 (#1941): the suppression is a LOAN.
              -- Re-satisfy the check and acknowledge the presentation --
              -- the branch retires and the ordinary hide rule takes over.
              -- One rule from here on, not two.
            , "TP.setSubobjectiveChecked('prepare_water', true)"
            , "assert(ids(TP.acknowledgePresented("
              <> "{ 'prepare_expedition' })) == 'prepare_expedition')"
            , "m = TP.getViewModel()"
            , "assert(activeIds(m) == '', activeIds(m))"
            ]

    -- #1941: #996's hide suppression is EXCEPTIONAL, not a second hide
    -- rule. It buys an already-latched branch the presentation it would
    -- otherwise never get, and retires the moment the consumer that
    -- rendered the row says so -- after which the node behaves exactly
    -- like one that was never sticky.
    describe "presentation retirement (#1941)" $ do

        -- The pre-latched setup every case below shares: the composite
        -- latches with both subobjectives checked BEFORE its ancestors
        -- complete, then the chain completes and reveals it sticky.
        let preLatched =
                [ "TP.setSubobjectiveChecked('prepare_water', true)"
                , "TP.setSubobjectiveChecked('prepare_food', true)"
                , "TP.completeObjective('prepare_expedition')"
                , "TP.completeObjective('place_portal')"
                , "TP.completeObjective('secure_water')"
                ]

        it "retires a pre-latched branch once it has been presented, \
           \after which the ordinary hide rule applies unchanged" $
            runsOk $ withTP prelude $ preLatched ⧺
            [ "assert(activeIds(TP.getViewModel()) == 'prepare_expedition,"
              <> "prepare_water,prepare_food',"
            , "       activeIds(TP.getViewModel()))"
              -- The acknowledgement names only the composite: its
              -- subobjectives are gated on it staying un-hidden, so
              -- retiring it is what takes the whole branch with it.
            , "assert(ids(TP.acknowledgePresented('prepare_expedition'))"
            , "       == 'prepare_expedition')"
            , "local m = TP.getViewModel()"
            , "assert(activeIds(m) == '', activeIds(m))"
              -- The ORDINARY rule, not a deletion: the composite is
              -- still reported as retained history, still latched.
            , "assert(rowById(m, 'prepare_expedition').active == false)"
            , "assert(rowById(m, 'prepare_expedition').completed == true)"
            , "assert(TP.isCompleted('prepare_expedition'))"
            ]

        it "keeps getViewModel a pure read — repeated calls never \
           \present anything" $ runsOk $ withTP prelude $ preLatched ⧺
            [ "local want = 'prepare_expedition,prepare_water,prepare_food'"
            , "for _ = 1, 5 do"
            , "    assert(activeIds(TP.getViewModel()) == want,"
            , "           activeIds(TP.getViewModel()))"
            , "end"
              -- Only the explicit acknowledgement moves it.
            , "TP.acknowledgePresented('prepare_expedition')"
            , "assert(activeIds(TP.getViewModel()) == '')"
            ]

        it "is state-preserving for a repeat, a non-sticky id, a \
           \subobjective and an unknown id" $
            runsOk $ withTP prelude $ preLatched ⧺
            [ "assert(#TP.acknowledgePresented('prepare_expedition') == 1)"
              -- A second acknowledgement of the same id retires nothing
              -- more and cannot re-hide anything on its own.
            , "assert(#TP.acknowledgePresented('prepare_expedition') == 0)"
              -- place_portal/secure_water were revealed while still
              -- incomplete, so they were never sticky: acknowledging
              -- them is a no-op that must not bypass the ordinary hide
              -- rule they are already subject to.
            , "assert(#TP.acknowledgePresented("
              <> "{ 'place_portal', 'secure_water' }) == 0)"
            , "assert(#TP.acknowledgePresented("
              <> "{ 'prepare_water', 'no_such_objective' }) == 0)"
            , "assert(#TP.acknowledgePresented(nil) == 0)"
            , "assert(#TP.acknowledgePresented(42) == 0)"
              -- Nothing durable moved, and the view is exactly what one
              -- retirement leaves.
            , "assert(ids(TP.completedIds()) == 'place_portal,"
              <> "prepare_expedition,secure_water', ids(TP.completedIds()))"
            , "assert(activeIds(TP.getViewModel()) == '')"
            ]

        it "returns a retired composite to the active view when a live \
           \subobjective unchecks, with its latch untouched" $
            runsOk $ withTP prelude $ preLatched ⧺
            [ "TP.acknowledgePresented('prepare_expedition')"
            , "assert(activeIds(TP.getViewModel()) == '')"
            , "TP.setSubobjectiveChecked('prepare_food', false)"
            , "local m = TP.getViewModel()"
            , "assert(activeIds(m) == 'prepare_expedition,prepare_water,"
              <> "prepare_food', activeIds(m))"
            , "assert(rowById(m, 'prepare_expedition').completed == true)"
            , "assert(rowById(m, 'prepare_food').checked == false)"
              -- ...and re-checking hides it again, with no second
              -- acknowledgement needed. One rule.
            , "TP.setSubobjectiveChecked('prepare_food', true)"
            , "assert(activeIds(TP.getViewModel()) == '')"
            ]

    -- Round-2 review (PR #962): tutorial progress lives on a Lua
    -- singleton that outlives any one world, and generating a new world
    -- runs no part of the save/load path -- so a new game started after
    -- playing or loading in the SAME process inherited the previous
    -- session's completed objectives. Driven through the real
    -- world_manager, not a restatement of it.
    describe "new-session lifecycle" $ do
        it "generating a world clears progress carried over from an \
           \earlier session in the same process" $
            runsOk $ withTP worldManagerPrelude
            [ "TP.completeObjective('place_portal')"
            , "TP.setSubobjectiveChecked('prepare_water', true)"
            , "assert(ids(TP.completedIds()) == 'place_portal')"
            , "newWorld('main_world')"
            , "assert(#world.inited == 1, 'world.init should still run')"
            , "assert(#TP.completedIds() == 0,"
            , "       'a new world must not inherit tutorial progress: '"
            , "       .. ids(TP.completedIds()))"
            , "assert(TP.isSubobjectiveChecked('prepare_water') == false)"
            -- The TREE is session-global content, not per-world state,
            -- so it must survive -- otherwise the fresh session has no
            -- objectives at all.
            , "local m = TP.getViewModel()"
            , "assert(m.treeId == 'first_session')"
            , "assert(activeIds(m) == 'place_portal', activeIds(m))"
            ]

        it "a second new world in the same process starts clean too" $
            runsOk $ withTP worldManagerPrelude
            [ "newWorld('main_world')"
            , "TP.completeObjective('place_portal')"
            , "TP.completeObjective('secure_water')"
            , "newWorld('main_world')"
            , "assert(#TP.completedIds() == 0, ids(TP.completedIds()))"
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
            -- #1941 requirement 4: the reveal history is RECONSTRUCTED,
            -- and every id the restored set already makes structurally
            -- reveal-eligible is rebuilt as already presented -- so no
            -- ancestor the player watched retire comes back. What IS
            -- active is what the ORDINARY hide rule leaves active: a
            -- load never restores live checks, so the composite's
            -- subobjectives read unchecked and the composite is not yet
            -- hideable.
            , "local m = TP.getViewModel()"
            , "assert(activeIds(m) == 'prepare_expedition,prepare_water,"
              <> "prepare_food', activeIds(m))"
            , "assert(rowById(m, 'place_portal').active == false)"
            , "assert(rowById(m, 'secure_water').active == false)"
            ]

        -- #1941 requirement 4: a tutorial that was already FINISHED
        -- when it was saved comes back finished. Every id here was
        -- revealed and hidden in the pre-save session -- reveal order
        -- ran forward through the whole chain, so nothing was ever
        -- sticky -- and the reconstruction rule reproduces that without
        -- persisting a byte of presentation state. The next evaluation
        -- tick re-checking the same live world it was saved from leaves
        -- the checklist empty, rather than resurrecting five rows the
        -- player already watched retire.
        it "does not return already-retired ancestors to the checklist \
           \after loading a completed tutorial (#1941)" $
            runsOk $ withTP savePrelude
            [ "TP.completeObjective('place_portal')"
            , "TP.completeObjective('secure_water')"
            , "TP.setSubobjectiveChecked('prepare_water', true)"
            , "TP.setSubobjectiveChecked('prepare_food', true)"
            , "TP.completeObjective('prepare_expedition')"
            , "assert(activeIds(TP.getViewModel()) == '',"
            , "       'precondition: the pre-save checklist is finished')"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, snap.error)"
            , "local prep = saveModules.prepareLoad(snap.components)"
            , "assert(prep.ok, prep.errors and table.concat(prep.errors, '; '))"
            , "saveModules.applyAll()"
            -- Simulate the very next evaluation tick observing the same
            -- live world it was saved from.
            , "TP.setSubobjectiveChecked('prepare_water', true)"
            , "TP.setSubobjectiveChecked('prepare_food', true)"
            , "local m = TP.getViewModel()"
            , "assert(activeIds(m) == '', activeIds(m))"
            , "assert(rowById(m, 'prepare_expedition').completed == true)"
            , "assert(ids(TP.completedIds()) == 'place_portal,"
              <> "prepare_expedition,secure_water', ids(TP.completedIds()))"
            ]

        -- The other half of that rule, and the one that keeps #996's
        -- original defect fixed ACROSS a save: a branch that had
        -- completed but was still gated behind an incomplete ancestor
        -- when the save was taken was never revealed, so the
        -- reconstruction must leave it unjudged rather than assuming it
        -- was presented. Its real first reveal is still ahead, and it
        -- must collect the suppression there.
        it "keeps a completed-but-unrevealed branch protected at its \
           \first reveal after a load (#1941)" $
            runsOk $ withTP savePrelude
            -- Pre-latched and saved with BOTH ancestors incomplete.
            [ "TP.setSubobjectiveChecked('prepare_water', true)"
            , "TP.setSubobjectiveChecked('prepare_food', true)"
            , "TP.completeObjective('prepare_expedition')"
            , "assert(rowById(TP.getViewModel(), 'prepare_expedition') == nil,"
            , "       'precondition: the branch is not revealed yet')"
            , "local snap = saveModules.snapshotAll()"
            , "assert(snap.ok, snap.error)"
            , "local prep = saveModules.prepareLoad(snap.components)"
            , "assert(prep.ok, prep.errors and table.concat(prep.errors, '; '))"
            , "saveModules.applyAll()"
            , "assert(TP.isCompleted('prepare_expedition'))"
            , "assert(rowById(TP.getViewModel(), 'prepare_expedition') == nil,"
            , "       'still gated behind its incomplete ancestors')"
            -- The evaluation tick re-checks the live world, then the
            -- chain completes for the first time in THIS session.
            , "TP.setSubobjectiveChecked('prepare_water', true)"
            , "TP.setSubobjectiveChecked('prepare_food', true)"
            , "TP.completeObjective('place_portal')"
            , "TP.completeObjective('secure_water')"
            , "local m = TP.getViewModel()"
            , "assert(activeIds(m) == 'prepare_expedition,prepare_water,"
              <> "prepare_food', activeIds(m))"
            -- ...and it is a real suppression, retired by presenting it.
            , "assert(ids(TP.acknowledgePresented('prepare_expedition'))"
            , "       == 'prepare_expedition')"
            , "assert(activeIds(TP.getViewModel()) == '')"
            ]
