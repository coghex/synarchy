-- | Mode A's escort session lifecycle (#1250, epic #1013 slice
--   UIT-3B): creation, the hold it takes, the approach, the two
--   flanking panes the arrival opens, every way a session is replaced
--   or torn down deliberately, and the responsive layout the pair
--   keeps at every scale.
--
--   One group of the aggregate gate in
--   "Test.Headless.UI.TransferSession", composed there under the
--   shared engine and Lua state of
--   "Test.Headless.UI.TransferSession.Fixture".
module Test.Headless.UI.TransferSession.Lifecycle (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Data.IORef (writeIORef)
import Engine.Core.State (EngineEnv, framebufferSizeRef)
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.UI.TransferSession.Fixture
    ( carrierUid, matesUid
    , placeUnit, stockTall
    , resetFixture, evalOk, luaLines, createLua )

spec ∷ SpecWith (EngineEnv, LuaBackendState)
spec =
    describe "the escort lifecycle" $ do
        it "a fresh session is APPROACHING, holds its source unit, and has \
           \opened no window yet — the panels are an arrival event, not a \
           \creation one" $ \(env, ls) → do
            resetFixture env ls
            created ← evalOk ls (createLua "building" 7)
            created `shouldBe` "\"true\""
            phase ← evalOk ls "return _G.__phase()"
            phase `shouldBe` "\"approaching\""
            held ← evalOk ls
                "return tostring(_G.__session().holdsUnit(1))"
            held `shouldBe` "\"true\""
            other ← evalOk ls
                "return tostring(_G.__session().holdsUnit(2))"
            other `shouldBe` "\"false\""
            depth ← evalOk ls
                "return require('scripts.cargo_inventory_panel').depth()"
            depth `shouldBe` "0"
            snaps ← evalOk ls "return _G.__snaps"
            snaps `shouldBe` "0"

        it "the hold is a real in-progress LOCK: it outscores a fully rested \
           \acolyte's wander and idle, it does not vary with churn, and it \
           \is -inf for a unit no session names" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (createLua "building" 7)
            -- The REAL wander/idle utilities, with the REAL acolyte
            -- tunables and a stat stub that puts wander at its own
            -- ceiling — the strongest form of "the wander tick cannot
            -- steal it" this fixture can state.
            scores ← evalOk ls (luaLines
                [ "unit.getStat = function() return 100 end;"
                , "package.loaded['scripts.unit_stats'] ="
                , "  { get = function() return 100 end };"
                , "local needs = require('scripts.unit_ai_needs');"
                , "local params = require('scripts.unit_ai_tunables').acolyte;"
                , "local s = { currentAction = 'wander',"
                , "            actionStartedAt = engine.gameTime() };"
                , "local escort = _G.__tick(1);"
                , "return { escort = escort,"
                , "         wander = needs.wanderUtility(1, s, params),"
                , "         idle   = needs.idleUtility(1, s, params) }" ])
            scores `shouldSatisfy` T.isInfixOf "\"escort\":7.5"
            -- Repeated scoring never moves it: an in-progress lock that
            -- decayed would be stealable by definition.
            steady ← evalOk ls
                "local a = _G.__tick(1); local b = _G.__tick(1); \
                \return tostring(a == b and a == 7.5)"
            steady `shouldBe` "\"true\""
            -- ...and it sits above every routine-work lock (6.0) and
            -- above follow_command (7.0), which is what puts it in the
            -- player-order band beside pickup and a queued order.
            band ← evalOk ls "return tostring(_G.__tick(1) > 7.0)"
            band `shouldBe` "\"true\""
            unheld ← evalOk ls "return tostring(_G.__tick(2))"
            unheld `shouldBe` "\"-inf\""

        it "a distant carrier WALKS toward the endpoint's footprint and does \
           \not open anything; standing adjacent it stops, reveals, opens \
           \both panes and snaps the camera — each exactly once" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls (createLua "building" 7)
            walked ← evalOk ls "return _G.__tick(1)"
            walked `shouldBe` "7.5"
            moves ← evalOk ls "return _G.__moves"
            moves `shouldBe` "1"
            stillApproaching ← evalOk ls "return _G.__phase()"
            stillApproaching `shouldBe` "\"approaching\""
            noWindow ← evalOk ls
                "return require('scripts.cargo_inventory_panel').depth()"
            noWindow `shouldBe` "0"

            -- Adjacent by the contract's own footprint rule: the hold
            -- occupies (40,40)..(41,41), so (42,41) is Chebyshev 1 from
            -- its far edge and a whole world away from its anchor.
            placeUnit env carrierUid (42, 41)
            arrived ← evalOk ls "_G.__tick(1); return _G.__phase()"
            arrived `shouldBe` "\"open\""
            reveals ← evalOk ls "return _G.__reveals"
            reveals `shouldBe` "1"
            snaps ← evalOk ls "return _G.__snaps"
            snaps `shouldBe` "1"
            -- ONE level, TWO panes: D-9's stated exception.
            shape ← evalOk ls
                "local d = require('scripts.cargo_inventory_panel').dump(); \
                \return { depth = d.depth, kind = d.levels[1].kind, \
                \         panes = d.levels[1].paneCount, \
                \         modal = d.levels[1].modal }"
            shape `shouldSatisfy` T.isInfixOf "\"depth\":1"
            shape `shouldSatisfy` T.isInfixOf "\"panes\":2"
            shape `shouldSatisfy` T.isInfixOf "\"kind\":\"escort\""
            shape `shouldSatisfy` T.isInfixOf "\"modal\":false"
            -- Every later tick holds: no second snap, no second reveal,
            -- no second window.
            again ← evalOk ls
                "_G.__tick(1); _G.__tick(1); \
                \return { snaps = _G.__snaps, reveals = _G.__reveals, \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            again `shouldSatisfy` T.isInfixOf "\"snaps\":1"
            again `shouldSatisfy` T.isInfixOf "\"reveals\":1"
            again `shouldSatisfy` T.isInfixOf "\"depth\":1"

        it "a UNIT destination is held from CREATION — before any tick, \
           \before the source has walked anywhere, and with no reveal" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            created ← evalOk ls (createLua "unit" 2)
            created `shouldBe` "\"true\""
            -- #1251 requirement 1: the target's hold begins HERE, while
            -- the session is still approaching, which is what gives the
            -- source's walk a fixed destination. Read before a single
            -- tick has run.
            atCreation ← evalOk ls
                "return { phase = _G.__phase(), \
                \         src = _G.__session().roleOf(1), \
                \         dst = _G.__session().roleOf(2), \
                \         held = _G.__session().holdsUnit(2) }"
            atCreation `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            atCreation `shouldSatisfy` T.isInfixOf "\"src\":\"source\""
            atCreation `shouldSatisfy` T.isInfixOf "\"dst\":\"target\""
            atCreation `shouldSatisfy` T.isInfixOf "\"held\":true"
            -- ...and the lock it scores is the SAME 7.5 the escort's is,
            -- so neither end can outscore the other and the target's
            -- routine work (<=6.0) and even a move order (7.0) lose to
            -- it, exactly like any player order.
            targetScore ← evalOk ls "return tostring(_G.__tick(2))"
            targetScore `shouldBe` "\"7.5\""
            -- The TARGET side is what won, not the escort's: the two are
            -- mutually exclusive, which is what lets them be registered
            -- separately without either end being scored twice.
            side ← evalOk ls "return _G.__lastAction"
            side `shouldBe` "\"escort_hold\""
            -- Holding the target is STANDING, never approaching: it
            -- issues no walk of its own in either phase.
            heldStill ← evalOk ls
                "return { moves = _G.__moves, stopped = _G.__lastStop }"
            heldStill `shouldSatisfy` T.isInfixOf "\"moves\":0"
            heldStill `shouldSatisfy` T.isInfixOf "\"stopped\":2"
            opened ← evalOk ls "_G.__tick(1); return _G.__phase()"
            opened `shouldBe` "\"open\""
            -- A unit endpoint has no remembered snapshot, so the reveal
            -- verb is never called for one.
            reveals ← evalOk ls "return _G.__reveals"
            reveals `shouldBe` "0"
            holds ← evalOk ls
                "return { src = _G.__session().holdsUnit(1), \
                \         dst = _G.__session().holdsUnit(2) }"
            holds `shouldSatisfy` T.isInfixOf "\"src\":true"
            holds `shouldSatisfy` T.isInfixOf "\"dst\":true"
            -- Still standing once the window is open, and still no walk.
            openMoves ← evalOk ls "_G.__tick(2); return _G.__moves"
            openMoves `shouldBe` "0"

        -- #1251 review round 1. `roleOf` makes EVERY unit destination
        -- the held target, but being a session's SOURCE is a per-species
        -- capability — so scoping the hold to the species that register
        -- `escort_transfer` would leave a legal target (endpoint
        -- eligibility is player-commandability and nothing else) whose
        -- AI never evaluated it: it would keep walking while an escort
        -- approached where it used to be. The hold is therefore
        -- registered universally, and the two questions stay separate.
        it "a commandable species that never registered the ESCORT is still \
           \held as a TARGET, while the SOURCE gate still refuses it" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            -- A REAL registration through the public API, as the #1250
            -- source-gate case does: uid 2's species can run neither the
            -- escort nor anything else this fixture drives.
            _ ← evalOk ls (luaLines
                [ "local a = require('scripts.unit_ai_actions');"
                , "a.byDef = {};"
                , "a.record('acolyte', { {name = 'wander'},"
                , "                      {name = 'escort_transfer'} });"
                , "a.record('bear', { {name = 'wander'} });"
                , "local um = unit.getInfo;"
                , "unit.getInfo = function(uid)"
                , "  local i = um(uid); if not i then return nil end;"
                , "  if uid == 2 then i.defName = 'bear' end;"
                , "  return i end;"
                , "return 'ok'" ])
            -- It is still an eligible ENDPOINT — that rule reads the
            -- live faction, never the action registry — so the session
            -- is created and it becomes the target.
            created ← evalOk ls (createLua "unit" 2)
            created `shouldBe` "\"true\""
            held ← evalOk ls
                "return { role = _G.__session().roleOf(2), \
                \         score = tostring(_G.__tick(2)), \
                \         won = _G.__lastAction, \
                \         stopped = _G.__lastStop, moves = _G.__moves }"
            held `shouldSatisfy` T.isInfixOf "\"role\":\"target\""
            held `shouldSatisfy` T.isInfixOf "\"score\":\"7.5\""
            held `shouldSatisfy` T.isInfixOf "\"won\":\"escort_hold\""
            held `shouldSatisfy` T.isInfixOf "\"stopped\":2"
            held `shouldSatisfy` T.isInfixOf "\"moves\":0"
            -- ...and the SOURCE question is untouched: the same species
            -- still cannot be made an escort, by either gate.
            asSource ← evalOk ls (luaLines
                [ "local s = require('scripts.transfer_session');"
                , "local ep = unit.transferEndpointInfo("
                , "  {kind = 'building', id = 7});"
                , "local made, reason = s.create(2, 'building', 7);"
                , "return { resolved = tostring(s.resolveSource({2}, nil, ep,"
                , "                               s.ESCORT_ACTION)),"
                , "         made = tostring(made),"
                , "         reason = tostring(reason) }" ])
            asSource `shouldSatisfy` T.isInfixOf "\"resolved\":\"nil\""
            asSource `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            asSource `shouldSatisfy`
                T.isInfixOf "\"reason\":\"source_not_escortable\""
            -- The refused creation left the running session alone, so
            -- the bear is STILL held as its target.
            intact ← evalOk ls
                "return { role = _G.__session().roleOf(2), \
                \         src = _G.__session().roleOf(1) }"
            intact `shouldSatisfy` T.isInfixOf "\"role\":\"target\""
            intact `shouldSatisfy` T.isInfixOf "\"src\":\"source\""

        it "a BUILDING destination still holds its source alone — there is \
           \no second endpoint to hold" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            holds ← evalOk ls
                "return { src = _G.__session().roleOf(1), \
                \         mate = tostring(_G.__session().roleOf(2)), \
                \         phase = _G.__phase() }"
            holds `shouldSatisfy` T.isInfixOf "\"src\":\"source\""
            holds `shouldSatisfy` T.isInfixOf "\"mate\":\"nil\""
            holds `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            bystander ← evalOk ls "return tostring(_G.__tick(2))"
            bystander `shouldBe` "\"-inf\""

        -- #1250 post-merge review: unit_ai's execute gate re-runs an
        -- action only on a SWITCH or when the unit is idle, and this
        -- action is deliberately not forceExecute — so a replacement
        -- session on the SAME unit kept walking to the OLD endpoint
        -- until that path ran out before it ever looked at the new one.
        it "replacing a session mid-APPROACH interrupts the walk it was on, \
           \so the new one routes to its own destination immediately" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (60, 60)
            _ ← evalOk ls (createLua "building" 7)
            first ← evalOk ls
                "_G.__tick(1); return { moves = _G.__moves, \
                \ stops = _G.__stops, x = _G.__lastMove.x, \
                \ y = _G.__lastMove.y }"
            -- Walking toward the hold at (40,40)..(41,41), and not yet
            -- stopped: this is the in-flight approach the replacement
            -- has to interrupt.
            first `shouldSatisfy` T.isInfixOf "\"moves\":1"
            first `shouldSatisfy` T.isInfixOf "\"stops\":0"
            first `shouldSatisfy` T.isInfixOf "\"x\":39.5"
            -- Replace it with a session on the MATE, far away in the
            -- other direction.
            replaced ← evalOk ls
                "_G.__session().create(1, 'unit', 2); \
                \return { stops = _G.__stops, stopped = _G.__lastStop, \
                \         phase = _G.__phase() }"
            replaced `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            -- The release STOPPED the escort rather than merely letting
            -- go of it, which is what makes the unit idle and the next
            -- tick re-decide.
            replaced `shouldSatisfy` T.isInfixOf "\"stops\":1"
            replaced `shouldSatisfy` T.isInfixOf "\"stopped\":1"
            -- ...and the very next tick walks toward the MATE at
            -- (60,60), not back to the hold.
            rerouted ← evalOk ls
                "_G.__tick(1); return { x = _G.__lastMove.x, \
                \ y = _G.__lastMove.y, moves = _G.__moves }"
            rerouted `shouldSatisfy` T.isInfixOf "\"moves\":2"
            rerouted `shouldSatisfy` T.isInfixOf "\"x\":59.5"
            rerouted `shouldSatisfy` T.isInfixOf "\"y\":59.5"

        it "closing the window closes BOTH panes, ends the session and \
           \releases the unit — and doing it again changes nothing" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            closed ← evalOk ls
                "local cip = require('scripts.cargo_inventory_panel'); \
                \cip.popLevel(); \
                \return { depth = cip.depth(), \
                \         session = tostring(_G.__session().get()), \
                \         held = _G.__session().holdsUnit(1) }"
            closed `shouldSatisfy` T.isInfixOf "\"depth\":0"
            closed `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            closed `shouldSatisfy` T.isInfixOf "\"held\":false"
            -- Idempotent from both ends.
            again ← evalOk ls
                "local cip = require('scripts.cargo_inventory_panel'); \
                \cip.popLevel(); cip.closeIfOpen(); _G.__session().clear(); \
                \return { depth = cip.depth(), \
                \         session = tostring(_G.__session().get()) }"
            again `shouldSatisfy` T.isInfixOf "\"depth\":0"
            again `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            -- The released unit scores nothing, which IS the release.
            score ← evalOk ls "return tostring(_G.__tick(1))"
            score `shouldBe` "\"-inf\""

        -- #1251 requirement 2: every path that ends a unit-to-unit
        -- session releases BOTH ends. The four below are the whole set —
        -- the coupled panel close, a successful replacement, Exit to
        -- Menu, and the successful-load reset — and each asserts release
        -- as the AI sees it (a -inf score, which is what actually lets
        -- the unit be steered again) rather than as a cleared table.
        it "closing the window releases BOTH units of a unit-to-unit \
           \session, and both can be steered again" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            open' ← evalOk ls "return _G.__phase()"
            open' `shouldBe` "\"open\""
            closed ← evalOk ls
                "local cip = require('scripts.cargo_inventory_panel'); \
                \_G.__stops = 0; cip.popLevel(); \
                \return { depth = cip.depth(), \
                \         session = tostring(_G.__session().get()), \
                \         src = _G.__session().holdsUnit(1), \
                \         dst = _G.__session().holdsUnit(2), \
                \         stops = _G.__stops }"
            closed `shouldSatisfy` T.isInfixOf "\"depth\":0"
            closed `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            closed `shouldSatisfy` T.isInfixOf "\"src\":false"
            closed `shouldSatisfy` T.isInfixOf "\"dst\":false"
            -- The teardown STOPPED each of them rather than merely
            -- letting go, which is what makes both idle and so what
            -- makes the next tick re-decide instead of running out an
            -- approach nobody wants any more.
            closed `shouldSatisfy` T.isInfixOf "\"stops\":2"
            -- Neither is held by anything now, so ordinary AI is free to
            -- steer both: the escort action itself concedes.
            scores ← evalOk ls
                "return { a = tostring(_G.__tick(1)), \
                \         b = tostring(_G.__tick(2)) }"
            scores `shouldSatisfy` T.isInfixOf "\"a\":\"-inf\""
            scores `shouldSatisfy` T.isInfixOf "\"b\":\"-inf\""
            -- ...and neither release issued a walk of its own, so a unit
            -- carrying its own orders is handed back where it stands.
            noWalks ← evalOk ls "return _G.__moves"
            noWalks `shouldBe` "0"

        it "a SUCCESSFUL replacement releases the PRIOR unit target, not \
           \just the source it reuses" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            heldBefore ← evalOk ls
                "return tostring(_G.__session().holdsUnit(2))"
            heldBefore `shouldBe` "\"true\""
            -- The same source, a DIFFERENT destination: the mate stops
            -- being an endpoint of anything and must be let go, or a
            -- unit the player never mentioned again stands pinned
            -- forever with no window left to close.
            _ ← evalOk ls (createLua "building" 7)
            after ← evalOk ls
                "return { phase = _G.__phase(), \
                \         src = _G.__session().holdsUnit(1), \
                \         mate = _G.__session().holdsUnit(2), \
                \         mateScore = tostring(_G.__tick(2)) }"
            after `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            after `shouldSatisfy` T.isInfixOf "\"src\":true"
            after `shouldSatisfy` T.isInfixOf "\"mate\":false"
            after `shouldSatisfy` T.isInfixOf "\"mateScore\":\"-inf\""

        it "Exit to Menu releases BOTH ends of a unit-to-unit session" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            _ ← evalOk ls "world.destroyAll = function() end; return 'ok'"
            exited ← evalOk ls
                "require('scripts.pause_menu').onExitToMenu(); \
                \return { session = tostring(_G.__session().get()), \
                \         src = tostring(_G.__tick(1)), \
                \         dst = tostring(_G.__tick(2)), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            exited `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            exited `shouldSatisfy` T.isInfixOf "\"src\":\"-inf\""
            exited `shouldSatisfy` T.isInfixOf "\"dst\":\"-inf\""
            exited `shouldSatisfy` T.isInfixOf "\"depth\":0"

        -- The reset hook is registered through `registerResetHook`, and
        -- saveModules.applyAll runs those only after every component has
        -- applied successfully — so "the load reset releases both" is a
        -- statement about a SUCCESSFUL, session-replacing load. A failed
        -- load never reaches this hook at all and therefore leaves the
        -- running session and both its holds exactly as they were; that
        -- half is 'Test.Headless.Lua.SaveModules''s rollback coverage,
        -- which this must not duplicate by asserting on a stub.
        it "the successful-load reset releases BOTH units and leaves a \
           \restored durable Mode B order on a reused uid alone" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls
                "require('scripts.transfer_session').init('transfer_session'); \
                \return 'ok'"
            -- A REAL durable order on the very unit the session holds,
            -- created through the engine's own verb against the live
            -- per-page store — the D-3 hazard is that the transient
            -- session's teardown reaches into durable state a load just
            -- restored onto the same uid.
            queued ← evalOk ls
                "return #unit.getTransferOrders(1) .. '/' .. tostring(\
                \unit.createTransferOrder(1, { source = { kind = 'unit', \
                \  id = 1 }, destination = { kind = 'building', id = 7 }, \
                \  items = { { instanceId = 110, defName = 'rope' } } }) \
                \  ~= nil)"
            queued `shouldBe` "\"0/true\""
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "_G.__tick(1); return _G.__tick(2)"
            after ← evalOk ls
                "local sm = require('scripts.lib.save_modules'); \
                \sm.resetHooks['transfer_session'](); \
                \return { session = tostring(_G.__session().get()), \
                \         src = tostring(_G.__tick(1)), \
                \         dst = tostring(_G.__tick(2)), \
                \         orders = #unit.getTransferOrders(1), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            after `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            after `shouldSatisfy` T.isInfixOf "\"src\":\"-inf\""
            after `shouldSatisfy` T.isInfixOf "\"dst\":\"-inf\""
            after `shouldSatisfy` T.isInfixOf "\"depth\":0"
            -- The order is still there and still the carrier's: the
            -- release stops a unit, it never cancels or prunes its work.
            after `shouldSatisfy` T.isInfixOf "\"orders\":1"

        it "the save-load reset hook ends the session and releases the unit, \
           \and the session still contributes NO save component (D-3)" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls
                "require('scripts.transfer_session').init('transfer_session'); \
                \return 'ok'"
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            kinds ← evalOk ls
                "local sm = require('scripts.lib.save_modules'); \
                \return type(sm.resetHooks['transfer_session']) .. '/' \
                \    .. type(sm.registry['transfer_session'])"
            kinds `shouldBe` "\"function/nil\""
            after ← evalOk ls
                "local sm = require('scripts.lib.save_modules'); \
                \sm.resetHooks['transfer_session'](); \
                \return { session = tostring(_G.__session().get()), \
                \         held = _G.__session().holdsUnit(1), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            after `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            after `shouldSatisfy` T.isInfixOf "\"held\":false"
            after `shouldSatisfy` T.isInfixOf "\"depth\":0"

        it "Exit to Menu ends the session the same way" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            _ ← evalOk ls "world.destroyAll = function() end; return 'ok'"
            exited ← evalOk ls
                "require('scripts.pause_menu').onExitToMenu(); \
                \return { session = tostring(_G.__session().get()), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            exited `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""
            exited `shouldSatisfy` T.isInfixOf "\"depth\":0"

        it "a REFUSED replacement leaves the running session — its phase, its \
           \hold and its open panes — exactly as it found it" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            -- Building 999 does not exist, so create refuses at its
            -- receiver-missing branch, before it may disturb anything.
            refused ← evalOk ls
                "local s, reason = _G.__session().create(1, 'building', 999); \
                \return { made = tostring(s), reason = tostring(reason) }"
            refused `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            refused `shouldSatisfy` T.isInfixOf "\"reason\":\"receiver_missing\""
            intact ← evalOk ls
                "local cip = require('scripts.cargo_inventory_panel'); \
                \return { phase = _G.__phase(), held = _G.__session().holdsUnit(1), \
                \         depth = cip.depth(), \
                \         panes = cip.dump().levels[1].paneCount }"
            intact `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            intact `shouldSatisfy` T.isInfixOf "\"held\":true"
            intact `shouldSatisfy` T.isInfixOf "\"depth\":1"
            intact `shouldSatisfy` T.isInfixOf "\"panes\":2"

        it "a SUCCESSFUL replacement closes the old pair and re-arms the \
           \escort, and opening an ordinary container window instead ends \
           \the session outright" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            placeUnit env matesUid (43, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            firstId ← evalOk ls "return _G.__session().get().id"
            -- Replacing with a session on the MATE: one window, one
            -- session, and the new one starts over at approaching.
            replaced ← evalOk ls (luaLines
                [ "_G.__session().create(1, 'unit', 2);"
                , "local cip = require('scripts.cargo_inventory_panel');"
                , "return { depth = cip.depth(), phase = _G.__phase(),"
                , "         id = _G.__session().get().id }" ])
            replaced `shouldSatisfy` T.isInfixOf "\"depth\":0"
            replaced `shouldSatisfy` T.isInfixOf "\"phase\":\"approaching\""
            replaced `shouldNotSatisfy` T.isInfixOf ("\"id\":" <> firstId)
            _ ← evalOk ls "return _G.__tick(1)"
            -- ...and an ordinary container window at the same level
            -- replaces the pair, which ends the session (requirement 7).
            ended ← evalOk ls
                "local cip = require('scripts.cargo_inventory_panel'); \
                \cip.openFor('building', 7, 10, 10); \
                \return { depth = cip.depth(), kind = cip.dump().levels[1].kind, \
                \         session = tostring(_G.__session().get()) }"
            ended `shouldSatisfy` T.isInfixOf "\"depth\":1"
            ended `shouldSatisfy` T.isInfixOf "\"kind\":\"endpoint\""
            ended `shouldSatisfy` T.isInfixOf "\"session\":\"nil\""

        it "a resize preserves the session, the hold, both panes and each \
           \pane's own tab and scroll — and repeats neither the snap nor \
           \the reveal (requirement 8)" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            -- Drive the REAL resize path: snapshot, the "resize"
            -- teardown sweep, restore — exactly what hud.createUI runs.
            restored ← evalOk ls (luaLines
                [ "local cip = require('scripts.cargo_inventory_panel');"
                , "local snap = cip.snapshotStack();"
                , "require('scripts.ui.view_teardown').run('resize');"
                , "local mid = { depth = cip.depth(),"
                , "              session = tostring(_G.__session().get()),"
                , "              held = _G.__session().holdsUnit(1) };"
                , "cip.restoreStack(snap);"
                , "local d = cip.dump();"
                , "return { midDepth = mid.depth, midSession = mid.session,"
                , "         midHeld = mid.held, depth = d.depth,"
                , "         kind = d.levels[1].kind,"
                , "         panes = d.levels[1].paneCount,"
                , "         phase = _G.__phase(),"
                , "         held = _G.__session().holdsUnit(1),"
                , "         snaps = _G.__snaps, reveals = _G.__reveals }" ])
            -- The teardown really did destroy the window...
            restored `shouldSatisfy` T.isInfixOf "\"midDepth\":0"
            -- ...and the session survived it, still holding its unit.
            restored `shouldSatisfy` T.isInfixOf "\"midSession\":\"table"
            restored `shouldSatisfy` T.isInfixOf "\"midHeld\":true"
            restored `shouldSatisfy` T.isInfixOf "\"depth\":1"
            restored `shouldSatisfy` T.isInfixOf "\"kind\":\"escort\""
            restored `shouldSatisfy` T.isInfixOf "\"panes\":2"
            restored `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            restored `shouldSatisfy` T.isInfixOf "\"held\":true"
            restored `shouldSatisfy` T.isInfixOf "\"snaps\":1"
            restored `shouldSatisfy` T.isInfixOf "\"reveals\":1"

        -- #1250 review round 1: a species the escort action was never
        -- registered for could be made a session's source, and the
        -- session then sat in `approaching` forever -- no walk, no
        -- panels, and a "hold" holding nothing.
        it "a source whose species cannot run the escort is skipped by the \
           \shared rule and refused by create, so no stuck session exists" $
           \(env, ls) → do
            resetFixture env ls
            -- A REAL registration, through the same public API every
            -- satellite AI script plugs itself in with: the bear is
            -- player-commandable here and registers no escort action.
            _ ← evalOk ls (luaLines
                [ "local a = require('scripts.unit_ai_actions');"
                , "a.byDef = {};"
                , "a.record('acolyte', { {name = 'wander'},"
                , "                      {name = 'escort_transfer'} });"
                , "a.record('bear', { {name = 'wander'} });"
                , "return 'ok'" ])
            -- uid 2 is the ONLY candidate and is a bear, so the rule has
            -- to answer nil rather than pick the only unit it has.
            _ ← evalOk ls (luaLines
                [ "local um = unit.getInfo;"
                , "unit.getInfo = function(uid)"
                , "  local i = um(uid); if not i then return nil end;"
                , "  if uid == 2 then i.defName = 'bear' end;"
                , "  return i end;"
                , "return 'ok'" ])
            resolved ← evalOk ls (luaLines
                [ "local s = require('scripts.transfer_session');"
                , "local ep = unit.transferEndpointInfo("
                , "  {kind = 'building', id = 7});"
                , "return { unfiltered = s.resolveSource({2}, nil, ep),"
                , "         filtered = tostring(s.resolveSource({2}, nil, ep,"
                , "                                s.ESCORT_ACTION)) }" ])
            resolved `shouldSatisfy` T.isInfixOf "\"unfiltered\":2"
            resolved `shouldSatisfy` T.isInfixOf "\"filtered\":\"nil\""
            -- ...and the ONE creation path refuses it too, so a surface
            -- that never ran the rule cannot mint the stuck session
            -- either.
            refused ← evalOk ls
                "local s, reason = require('scripts.transfer_session')\
                \.create(2, 'building', 7); \
                \return { made = tostring(s), reason = tostring(reason) }"
            refused `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            refused `shouldSatisfy`
                T.isInfixOf "\"reason\":\"source_not_escortable\""
            open' ← evalOk ls
                "return require('scripts.cargo_inventory_panel').depth()"
            open' `shouldBe` "0"
            -- The acolyte, which DID register it, still resolves and
            -- still creates — the filter must not refuse everything.
            ok ← evalOk ls (luaLines
                [ "local s = require('scripts.transfer_session');"
                , "local ep = unit.transferEndpointInfo("
                , "  {kind = 'building', id = 7});"
                , "return s.resolveSource({1}, nil, ep, s.ESCORT_ACTION)" ])
            ok `shouldBe` "1"

        -- #1250 review round 3: both panes named their widgets
        -- "cargo_inv", so their tab controls got identical element
        -- names — and control focus survives a rebuild by NAME,
        -- restoring the FIRST visible match. Focus parked on the
        -- destination pane came back on the SOURCE pane's
        -- corresponding tab: a silently wrong control, not a missing
        -- one.
        it "keyboard control focus on the DESTINATION pane comes back on \
           \the destination pane across a resize, not its source-pane twin" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            -- The two panes' controls must be distinguishable by name
            -- at all: that is the property by-name restore relies on.
            names ← evalOk ls (luaLines
                [ "local c = require('scripts.cargo_inventory_panel');"
                , "local il = require('scripts.ui.item_list');"
                , "local lvl = c.getLevel(1);"
                , "local function firstTab(key)"
                , "  local p = c.getPane(lvl, key);"
                , "  local t = il.getTabs(p.listId)[1];"
                , "  local i = t and UI.getElementInfo(t.boxId);"
                , "  return t and t.boxId, i and i.name end;"
                , "local sh, sn = firstTab('source');"
                , "local dh, dn = firstTab('destination');"
                , "_G.__srcTab, _G.__dstTab = sh, dh;"
                , "return { src = sn, dst = dn, distinct = sn ~= dn }" ])
            names `shouldSatisfy` T.isInfixOf "\"distinct\":true"
            names `shouldSatisfy` T.isInfixOf "cargo_inv_destination"
            -- Now the real round trip, through the SAME snapshot/restore
            -- pair hud.createUI wraps its rebuild in.
            restored ← evalOk ls (luaLines
                [ "local c = require('scripts.cargo_inventory_panel');"
                , "local r = require('scripts.ui.responsive');"
                , "local il = require('scripts.ui.item_list');"
                , "UI.setControlFocus(_G.__dstTab);"
                , "local want = r.snapshotControlFocusName();"
                , "local snap = c.snapshotStack();"
                , "require('scripts.ui.view_teardown').run('resize');"
                , "c.restoreStack(snap);"
                , "r.restoreControlFocusName(want);"
                , "local lvl = c.getLevel(1);"
                , "local focus = UI.getControlFocus();"
                , "local function ownsFocus(key)"
                , "  local p = c.getPane(lvl, key);"
                , "  for _, t in ipairs(il.getTabs(p.listId)) do"
                , "    if t.boxId == focus then return true end end;"
                , "  return false end;"
                , "return { want = want, focused = focus ~= nil,"
                , "         onDestination = ownsFocus('destination'),"
                , "         onSource = ownsFocus('source') }" ])
            restored `shouldSatisfy` T.isInfixOf "\"focused\":true"
            restored `shouldSatisfy` T.isInfixOf "\"onDestination\":true"
            restored `shouldSatisfy` T.isInfixOf "\"onSource\":false"

        it "both panes are framebuffer-clamped and do not overlap each other" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            geom ← evalOk ls (luaLines
                [ "local d = require('scripts.cargo_inventory_panel').dump();"
                , "local p = d.levels[1].panes;"
                , "local a, b = p[1], p[2];"
                , "local function inFrame(r)"
                , "  return r.x >= 0 and r.y >= 0"
                , "     and r.x + r.width <= 1280 and r.y + r.height <= 720 end;"
                , "local overlap = not (a.x + a.width <= b.x or b.x + b.width <= a.x"
                , "                     or a.y + a.height <= b.y"
                , "                     or b.y + b.height <= a.y);"
                , "return { inFrame = inFrame(a) and inFrame(b),"
                , "         overlap = overlap, leftFirst = a.x < b.x,"
                , "         keyA = a.paneKey, keyB = b.paneKey }" ])
            geom `shouldSatisfy` T.isInfixOf "\"inFrame\":true"
            geom `shouldSatisfy` T.isInfixOf "\"overlap\":false"
            geom `shouldSatisfy` T.isInfixOf "\"leftFirst\":true"
            geom `shouldSatisfy` T.isInfixOf "\"keyA\":\"source\""
            geom `shouldSatisfy` T.isInfixOf "\"keyB\":\"destination\""
            -- With room to spare nothing is fitted, so the headers
            -- rasterise at their configured sizes — the other half of
            -- the minimum-viewport case below, which is what stops a
            -- fix there from simply shrinking every header everywhere.
            fonts ← evalOk ls "return _G.__headerFonts('source')"
            fonts `shouldSatisfy` T.isInfixOf "\"title\":16"
            fonts `shouldSatisfy` T.isInfixOf "\"subtitle\":13"

        -- #1250 review round 1: at the envelope's FORMAL MINIMUM the
        -- pair's natural width (2x440 + gap = 904) exceeds the
        -- framebuffer, and clamping each panel on its own — all
        -- measurePane (scripts/cargo_inventory_render.lua since #2155)
        -- and UI.placePopup can do — lands them on top of
        -- each other. The PAIR is fitted first now.
        it "at the supported minimum 800x600 @ 1x the pair still flanks: \
           \fitted to width, in frame, source left, no overlap" $
           \(env, ls) → do
            resetFixture env ls
            writeIORef (framebufferSizeRef env) (800, 600)
            _ ← evalOk ls "require('scripts.cargo_inventory_panel')\
                          \.setup({page = _G.__page, fbW = 800, fbH = 600, \
                          \        boxTexSet = 1, menuFont = 1}); return 'ok'"
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            tight ← evalOk ls (luaLines
                [ "local d = require('scripts.cargo_inventory_panel').dump();"
                , "local p = d.levels[1].panes;"
                , "local a, b = p[1], p[2];"
                , "local function inFrame(r)"
                , "  return r.x >= 0 and r.y >= 0"
                , "     and r.x + r.width <= 800 and r.y + r.height <= 600 end;"
                , "local overlap = not (a.x + a.width <= b.x"
                , "                     or b.x + b.width <= a.x"
                , "                     or a.y + a.height <= b.y"
                , "                     or b.y + b.height <= a.y);"
                , "return { inFrame = inFrame(a) and inFrame(b),"
                , "         overlap = overlap, leftFirst = a.x < b.x,"
                , "         keyA = a.paneKey, keyB = b.paneKey,"
                -- `shrunk` proves this is a FIT and not merely a clamp:
                -- an unfitted pane would still measure its full 440.
                -- (A Lua comment cannot ride inside these chunks — the
                -- debug console is single-line, so `--` would swallow
                -- the rest of the statement.)
                , "         shrunk = a.width < 440 and b.width < 440,"
                , "         panes = d.levels[1].paneCount }" ])
            tight `shouldSatisfy` T.isInfixOf "\"inFrame\":true"
            tight `shouldSatisfy` T.isInfixOf "\"overlap\":false"
            tight `shouldSatisfy` T.isInfixOf "\"leftFirst\":true"
            tight `shouldSatisfy` T.isInfixOf "\"keyA\":\"source\""
            tight `shouldSatisfy` T.isInfixOf "\"keyB\":\"destination\""
            tight `shouldSatisfy` T.isInfixOf "\"shrunk\":true"
            tight `shouldSatisfy` T.isInfixOf "\"panes\":2"
            -- ...and the rows inside shrank WITH the box rather than
            -- overflowing it, which is what makes this a fit and not a
            -- crop.
            fitted ← evalOk ls
                "local c = require('scripts.cargo_inventory_panel'); \
                \local il = require('scripts.ui.item_list'); \
                \local lvl = c.getLevel(1); \
                \return { list = il.getScale(c.getPane(lvl, 'source').listId), \
                \         cfg = require('scripts.ui.scale').get() }"
            fitted `shouldSatisfy` T.isInfixOf "\"cfg\":1"
            fitted `shouldNotSatisfy` T.isInfixOf "\"list\":1"
            -- ...and so did the HEADERS (#1250 review round 2). Their
            -- three bands are reserved at the pane's fitted scale, so a
            -- title still rasterised at the configured 16 would reach
            -- down into rows the panel never sized for it.
            headers ← evalOk ls "return { src = _G.__headerFonts('source'), \
                                \         dst = _G.__headerFonts('destination') }"
            headers `shouldNotSatisfy` T.isInfixOf "\"title\":16"
            headers `shouldNotSatisfy` T.isInfixOf "\"subtitle\":13"
            smaller ← evalOk ls (luaLines
                [ "local f = _G.__headerFonts('source');"
                , "local g = _G.__headerFonts('destination');"
                , "return tostring(f.title > 0 and f.title < 16"
                , "                and f.subtitle > 0 and f.subtitle < 13"
                , "                and g.title == f.title"
                , "                and g.subtitle == f.subtitle)" ])
            smaller `shouldBe` "\"true\""

        -- #1250 review round 4: the pair was fitted to the FULL
        -- framebuffer and each panel then placed independently, so at
        -- the envelope's minimum with the real toolbar reserving a
        -- band, the first panel's own avoidance consumed the space the
        -- second needed and avoidReserved's best-effort fallback landed
        -- them on top of each other.
        it "at 800x600 @ 1x WITH the toolbar reserved and both lists at \
           \their row cap, the pair still fits beside it and still flanks" $
           \(env, ls) → do
            resetFixture env ls
            writeIORef (framebufferSizeRef env) (800, 600)
            stockTall env
            -- Supplied through the same lazy require the placement
            -- reads it with, so this is the production path and not a
            -- parallel one. A FULL-HEIGHT left rail deliberately: a
            -- short bottom-left cluster leaves the pair a vertical
            -- escape, and it is the reservation with no way around it
            -- that forces the pair to be narrow enough to sit BESIDE
            -- it — which is the half `availableWidth` buys and a
            -- fit-to-whole-framebuffer cannot.
            _ ← evalOk ls (luaLines
                [ "package.loaded['scripts.hud'] = {"
                , "  getToolbarRects = function()"
                , "    return { {name = 'tool_rail', x = 16, y = 0,"
                , "              w = 64, h = 600} } end };"
                , "require('scripts.cargo_inventory_panel').setup("
                , "  {page = _G.__page, fbW = 800, fbH = 600,"
                , "   boxTexSet = 1, menuFont = 1});"
                , "return 'ok'" ])
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            crowded ← evalOk ls (luaLines
                [ "local d = require('scripts.cargo_inventory_panel').dump();"
                , "local p = d.levels[1].panes;"
                , "local a, b = p[1], p[2];"
                , "local bar = {x = 16, y = 0, w = 64, h = 600};"
                , "local function hits(r, o)"
                , "  return not (r.x + r.width <= o.x or o.x + o.w <= r.x"
                , "              or r.y + r.height <= o.y"
                , "              or o.y + o.h <= r.y) end;"
                , "local function inFrame(r)"
                , "  return r.x >= 0 and r.y >= 0"
                , "     and r.x + r.width <= 800 and r.y + r.height <= 600 end;"
                , "local overlap = not (a.x + a.width <= b.x"
                , "                     or b.x + b.width <= a.x"
                , "                     or a.y + a.height <= b.y"
                , "                     or b.y + b.height <= a.y);"
                , "return { inFrame = inFrame(a) and inFrame(b),"
                , "         overlap = overlap, leftFirst = a.x < b.x,"
                , "         onToolbar = hits(a, bar) or hits(b, bar),"
                , "         gap = b.x - (a.x + a.width),"
                , "         rows = a.rowCount .. '/' .. b.rowCount,"
                , "         panes = d.levels[1].paneCount }" ])
            crowded `shouldSatisfy` T.isInfixOf "\"panes\":2"
            -- Both lists really are at the cap, so these are the
            -- TALLEST panels this window can produce.
            crowded `shouldSatisfy` T.isInfixOf "\"rows\":\"10/10\""
            crowded `shouldSatisfy` T.isInfixOf "\"inFrame\":true"
            crowded `shouldSatisfy` T.isInfixOf "\"overlap\":false"
            crowded `shouldSatisfy` T.isInfixOf "\"leftFirst\":true"
            crowded `shouldSatisfy` T.isInfixOf "\"onToolbar\":false"
            -- The pair is laid out as ONE rect and then split, so the
            -- two panes sit exactly one scaled gap apart WHEREVER
            -- arbitration puts them. Placing each panel on its own and
            -- nudging the second clear of the first leaves that spacing
            -- to whatever the nudge produced, so this is the assertion
            -- that pins the structure rather than the outcome. The base
            -- gap is 24 at uiscale 1 and the pair is fitted below that
            -- here, so a positive gap no larger than 24 is the fitted
            -- one and nothing else.
            gapText ← evalOk ls (luaLines
                [ "local d = require('scripts.cargo_inventory_panel').dump();"
                , "local p = d.levels[1].panes;"
                , "local g = p[2].x - (p[1].x + p[1].width);"
                , "return tostring(g > 0 and g <= 24)" ])
            gapText `shouldBe` "\"true\""
