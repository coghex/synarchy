-- | #1254 (UIT-5B): every way a session can be interrupted.
--
--   Structured the way the issue's own review asked for, and NOT as a
--   trigger-by-endpoint cross-product: each TRIGGER is proved to
--   delegate to the one canonical teardown, and that teardown is
--   proved separately to release both holds and both panes. The two
--   together are what cover requirement 6 for every trigger, without
--   twelve near-identical cases whose failures would all mean the
--   same thing.
--
--   One group of the aggregate gate in
--   "Test.Headless.UI.TransferSession", composed there under the
--   shared engine and Lua state of
--   "Test.Headless.UI.TransferSession.Fixture".
module Test.Headless.UI.TransferSession.Failure (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.Types (LuaBackendState)
import Unit.Faction (Faction(..))
import Test.Headless.UI.TransferSession.Fixture
    ( carrierUid, matesUid, holdBid
    , placeUnit, setPose, setFaction, removeUnit, demolish
    , resetFixture, evalOk, luaLines, createLua )

spec ∷ SpecWith (EngineEnv, LuaBackendState)
spec =
    describe "session failure handling (#1254)" $ do

        -- The per-tick liveness rule. It is the SESSION's own tick and
        -- not the window's on purpose: the approach has no window.
        it "an endpoint demolished while the panes are OPEN ends the \
           \session on its own tick — panes, hold and identity together" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            open' ← evalOk ls "return _G.__phase()"
            open' `shouldBe` "\"open\""
            demolish env holdBid
            gone ← evalOk ls
                "_G.__session().update(0); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth(), \
                \         held = _G.__session().holdsUnit(1), \
                \         score = tostring(_G.__tick(1)) }"
            gone `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone `shouldSatisfy` T.isInfixOf "\"depth\":0"
            gone `shouldSatisfy` T.isInfixOf "\"held\":false"
            gone `shouldSatisfy` T.isInfixOf "\"score\":\"-inf\""

        it "…and one demolished mid-APPROACH ends it too, which is the \
           \half of a session's life no window can notice" $
           \(env, ls) → do
            resetFixture env ls
            -- Far from the hold: this session never opens a window, so
            -- the container manager's own per-tick `stillThere` hook has
            -- nothing to run against and only this tick can see it.
            _ ← evalOk ls (createLua "building" 7)
            approaching ← evalOk ls "return _G.__phase()"
            approaching `shouldBe` "\"approaching\""
            demolish env holdBid
            gone ← evalOk ls
                "_G.__session().update(0); \
                \return { session = tostring(_G.__session().get()), \
                \         held = _G.__session().holdsUnit(1), \
                \         score = tostring(_G.__tick(1)) }"
            gone `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone `shouldSatisfy` T.isInfixOf "\"held\":false"
            gone `shouldSatisfy` T.isInfixOf "\"score\":\"-inf\""

        -- Requirement 2. The contract cannot answer this one:
        -- `Unit.Transfer.endpointEligible` is `uevCommandable` alone, so
        -- a corpse is still a perfectly eligible endpoint by its lights
        -- and the panes would happily keep rendering its inventory.
        it "a DEAD held unit ends the session, even though the transfer \
           \contract still calls it an eligible endpoint" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            -- The contract's own verdict first, so this case cannot
            -- quietly become a test of a rule that moved elsewhere.
            setPose env carrierUid "dead"
            contract ← evalOk ls
                "local i = unit.transferEndpointInfo({kind='unit', id=1}); \
                \return tostring(i ~= nil and i.eligible)"
            contract `shouldBe` "\"true\""
            gone ← evalOk ls
                "_G.__session().update(0); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            gone `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone `shouldSatisfy` T.isInfixOf "\"depth\":0"

        it "an UNCONSCIOUS held target ends it, and a merely crawling one \
           \does not — the session sits through a recoverable pose" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            setPose env matesUid "crawling"
            survives ← evalOk ls
                "_G.__session().update(0); \
                \return { phase = _G.__phase(), \
                \         src = _G.__session().holdsUnit(1), \
                \         dst = _G.__session().holdsUnit(2) }"
            survives `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            survives `shouldSatisfy` T.isInfixOf "\"src\":true"
            survives `shouldSatisfy` T.isInfixOf "\"dst\":true"
            setPose env matesUid "collapsed"
            gone ← evalOk ls
                "_G.__session().update(0); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth(), \
                \         src = tostring(_G.__tick(1)), \
                \         dst = tostring(_G.__tick(2)) }"
            gone `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone `shouldSatisfy` T.isInfixOf "\"depth\":0"
            gone `shouldSatisfy` T.isInfixOf "\"src\":\"-inf\""
            gone `shouldSatisfy` T.isInfixOf "\"dst\":\"-inf\""

        it "a held unit that leaves the player's factions ends it" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            setFaction env matesUid FactionWildlife
            gone ← evalOk ls
                "_G.__session().update(0); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            gone `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone `shouldSatisfy` T.isInfixOf "\"depth\":0"

        -- The review's "a missing FIRST endpoint must not prevent
        -- cleanup of the other endpoint or either panel": the source is
        -- checked first and is the one that vanished, so if the teardown
        -- were not step-isolated the TARGET would stay pinned forever
        -- with no window left to release it.
        it "a source that stopped existing still releases the OTHER end \
           \and still closes both panes" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            removeUnit env carrierUid
            gone ← evalOk ls
                "_G.__session().update(0); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth(), \
                \         dst = _G.__session().holdsUnit(2), \
                \         dstScore = tostring(_G.__tick(2)) }"
            gone `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            gone `shouldSatisfy` T.isInfixOf "\"depth\":0"
            gone `shouldSatisfy` T.isInfixOf "\"dst\":false"
            gone `shouldSatisfy` T.isInfixOf "\"dstScore\":\"-inf\""

        -- The negative control the six above need: the tick must not be
        -- a session's own worst enemy.
        it "a healthy session survives its tick indefinitely, in either \
           \phase" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (createLua "building" 7)
            walking ← evalOk ls
                "_G.__session().update(0); _G.__session().update(0); \
                \return { phase = _G.__phase(), \
                \         stale = tostring(_G.__session().staleReason()), \
                \         held = _G.__session().holdsUnit(1) }"
            walking `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            walking `shouldSatisfy` T.isInfixOf "\"stale\":\"nil\""
            walking `shouldSatisfy` T.isInfixOf "\"held\":true"
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls "return _G.__tick(1)"
            standing ← evalOk ls
                "_G.__session().update(0); _G.__session().update(0); \
                \return { phase = _G.__phase(), \
                \         depth = require('scripts.cargo_inventory_panel').depth(), \
                \         held = _G.__session().holdsUnit(1) }"
            standing `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            standing `shouldSatisfy` T.isInfixOf "\"depth\":1"
            standing `shouldSatisfy` T.isInfixOf "\"held\":true"

        -- Requirement 3, signed off 2026-08-11. The boundary itself;
        -- the real player INGRESS that calls it is pinned in
        -- 'Test.Headless.UI.TransferContextMenu'.
        it "a new player order to a held unit ends the session and the \
           \order proceeds — from EITHER side of a two-sided hold" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            -- Ordering the TARGET, not the escort: being an endpoint of
            -- somebody else's session must not make a unit unorderable.
            ordered ← evalOk ls
                "local core = require('scripts.unit_ai_core'); \
                \local ended = _G.__session().notePlayerOrder(2); \
                \package.loaded['scripts.unit_ai'].commandMove(2, 30, 30); \
                \local task = core.ensureState(2).commandedTask; \
                \return { ended = ended, \
                \         session = tostring(_G.__session().get()), \
                \         src = tostring(_G.__tick(1)), \
                \         dst = tostring(_G.__tick(2)), \
                \         depth = require('scripts.cargo_inventory_panel').depth(), \
                \         ordered = tostring(task ~= nil and task.x) }"
            ordered `shouldSatisfy` T.isInfixOf "\"ended\":true"
            ordered `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            ordered `shouldSatisfy` T.isInfixOf "\"src\":\"-inf\""
            ordered `shouldSatisfy` T.isInfixOf "\"dst\":\"-inf\""
            ordered `shouldSatisfy` T.isInfixOf "\"depth\":0"
            -- The command really did land, and survived the teardown
            -- that ran before it.
            ordered `shouldSatisfy` T.isInfixOf "\"ordered\":\"30\""

        it "the boundary ignores a unit no session holds, so an ordinary \
           \order costs an unrelated session nothing" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            untouched ← evalOk ls
                "local ended = _G.__session().notePlayerOrder(3); \
                \return { ended = ended, phase = _G.__phase(), \
                \         src = _G.__session().holdsUnit(1), \
                \         dst = _G.__session().holdsUnit(2), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            untouched `shouldSatisfy` T.isInfixOf "\"ended\":false"
            untouched `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            untouched `shouldSatisfy` T.isInfixOf "\"src\":true"
            untouched `shouldSatisfy` T.isInfixOf "\"dst\":true"
            untouched `shouldSatisfy` T.isInfixOf "\"depth\":1"

        -- The other half of the review's "not move-only" pair: the
        -- session's OWN movement is not a player order and must not
        -- cancel the thing that issued it. The escort's approach is a
        -- real `unit.moveTo` through the AI action, so this drives that
        -- action rather than asserting about a call that never happened.
        it "the escort's own approach does NOT self-cancel: an internal \
           \move is not a player order" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (createLua "building" 7)
            walked ← evalOk ls
                "_G.__moves = 0; _G.__tick(1); _G.__tick(1); \
                \return { moves = _G.__moves, phase = _G.__phase(), \
                \         held = _G.__session().holdsUnit(1) }"
            walked `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            walked `shouldSatisfy` T.isInfixOf "\"held\":true"
            walked `shouldNotSatisfy` T.isInfixOf "\"moves\":0"

        -- Requirement 4's registry half (#156): the container-window
        -- entry beside it covers an OPEN session by closing its window,
        -- and this one is why a second entry exists at all.
        it "a HUD hide ends a session still APPROACHING, through the \
           \view-teardown registry rather than a one-off call" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (createLua "building" 7)
            hidden ← evalOk ls
                "require('scripts.ui.view_teardown').run('hudHide'); \
                \return { session = tostring(_G.__session().get()), \
                \         held = _G.__session().holdsUnit(1), \
                \         score = tostring(_G.__tick(1)) }"
            hidden `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            hidden `shouldSatisfy` T.isInfixOf "\"held\":false"
            hidden `shouldSatisfy` T.isInfixOf "\"score\":\"-inf\""

        it "…and a zoom-band change does, while a layout-only resize \
           \still leaves an approaching session alone" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (createLua "building" 7)
            resized ← evalOk ls
                "require('scripts.ui.view_teardown').run('resize'); \
                \return { phase = _G.__phase(), \
                \         held = _G.__session().holdsUnit(1) }"
            resized `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            resized `shouldSatisfy` T.isInfixOf "\"held\":true"
            banded ← evalOk ls
                "require('scripts.ui.view_teardown').run('zoomBand', \
                \    { worldId = 'transfer_session_page', newView = 'zoomed_out' }); \
                \return { session = tostring(_G.__session().get()), \
                \         held = _G.__session().holdsUnit(1) }"
            banded `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            banded `shouldSatisfy` T.isInfixOf "\"held\":false"

        -- The load reset is the ONE path whose recorded uids no longer
        -- name the units they did: saveModules.applyAll fires reset
        -- hooks only after every component has applied, and entity ids
        -- are reused across sessions. Panels and identity still go.
        it "the successful-load reset drops the session without stopping \
           \whatever the load restored onto its stale uids" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls
                "require('scripts.transfer_session').init('transfer_session'); \
                \return 'ok'"
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            after ← evalOk ls
                "local sm = require('scripts.lib.save_modules'); \
                \_G.__stops = 0; sm.resetHooks['transfer_session'](); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth(), \
                \         stops = _G.__stops, \
                \         src = tostring(_G.__tick(1)), \
                \         dst = tostring(_G.__tick(2)) }"
            after `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            after `shouldSatisfy` T.isInfixOf "\"depth\":0"
            after `shouldSatisfy` T.isInfixOf "\"src\":\"-inf\""
            after `shouldSatisfy` T.isInfixOf "\"dst\":\"-inf\""
            -- Every OTHER teardown stops what it held; this one must
            -- not, because uid 1 and uid 2 are now somebody else.
            after `shouldSatisfy` T.isInfixOf "\"stops\":0"

        -- Requirement 5 (D-9). An abnormal close is not a special kind
        -- of close: the stack it leaves behind has to be the same empty
        -- stack an ordinary dismissal leaves.
        it "after an abnormal close the stack is empty and the next \
           \container window opens and renders normally" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            setPose env carrierUid "dead"
            _ ← evalOk ls "_G.__session().update(0); return 'ok'"
            reopened ← evalOk ls (luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "local before = cip.depth();"
                , "cip.openFor('building', 7, 10, 10);"
                , "local d = cip.dump();"
                , "local il = require('scripts.ui.item_list');"
                , "local pane = cip.getPane(cip.getLevel(1), 'main');"
                , "return { before = before, depth = d.depth,"
                , "         kind = d.levels[1].kind,"
                , "         rows = #il.getRows(pane.listId),"
                , "         session = tostring(_G.__session().get()) }" ])
            reopened `shouldSatisfy` T.isInfixOf "\"before\":0"
            reopened `shouldSatisfy` T.isInfixOf "\"depth\":1"
            reopened `shouldSatisfy` T.isInfixOf "\"kind\":\"endpoint\""
            reopened `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            reopened `shouldNotSatisfy` T.isInfixOf "\"rows\":0"

        -- Requirement 7, and exactly as far as it goes: per-REQUEST
        -- atomicity. A session owns no transaction, so ending one can
        -- neither half-move an item nor undo a move that completed.
        it "an abnormal close leaves an already-committed transfer \
           \committed — no rollback, no half-moved item" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            moved ← evalOk ls
                "local m = _G.__rowMenu('source', 'ration'); \
                \return tostring(_G.__fire(m, 'Store all'))"
            moved `shouldBe` "\"true\""
            before ← evalOk ls "return _G.__ids('building', 7)"
            before `shouldSatisfy` T.isInfixOf "ration#101"
            demolish env holdBid
            _ ← evalOk ls "_G.__session().update(0); return 'ok'"
            -- The building is gone, so the surviving record of the
            -- commit is the CARRIER's own inventory: the three rations
            -- left it and did not come back.
            after ← evalOk ls
                "return { session = tostring(_G.__session().get()), \
                \         carrier = _G.__ids('unit', 1) }"
            after `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            after `shouldNotSatisfy` T.isInfixOf "ration#101"
            after `shouldSatisfy` T.isInfixOf "rope#110"
