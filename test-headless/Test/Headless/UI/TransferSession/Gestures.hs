-- | The immediate row gestures an OPEN Mode A session offers
--   (#1250, #1251): Store and Retrieve on each pane, 1-and-all on a
--   merged row, the partial batch and its report, the proximity
--   refusal, and the accessory that is never a session row at all.
--
--   One group of the aggregate gate in
--   "Test.Headless.UI.TransferSession", composed there under the
--   shared engine and Lua state of
--   "Test.Headless.UI.TransferSession.Fixture".
module Test.Headless.UI.TransferSession.Gestures (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.Types (LuaBackendState)
import Unit.Types (UnitId(unUnitId))
import Test.Headless.UI.TransferSession.Fixture
    ( carrierUid, matesUid, muleUid
    , carrierInventory, muleInventory, mateInventory, carrierWorn
    , placeUnit, stockUnit
    , resetFixture, evalOk, createLua )

spec ∷ SpecWith (EngineEnv, LuaBackendState)
spec =
    describe "the session's row gestures" $ do
        let openOnHold env ls = do
                resetFixture env ls
                placeUnit env carrierUid (42, 41)
                _ ← evalOk ls (createLua "building" 7)
                _ ← evalOk ls "return _G.__tick(1)"
                pure ()
            -- #1251: a unit-to-unit session, already arrived, with both
            -- endpoints carrying something. `destUid` is the ONLY thing
            -- that differs between the two species pairings below, which
            -- is the point — an endpoint is an endpoint.
            openPair env ls destUid destStock = do
                resetFixture env ls
                stockUnit env carrierUid carrierInventory []
                stockUnit env destUid destStock []
                placeUnit env carrierUid (10, 10)
                placeUnit env destUid (11, 10)
                let destId = fromIntegral (unUnitId destUid) ∷ Int
                created ← evalOk ls (createLua "unit" destId)
                created `shouldBe` "\"true\""
                _ ← evalOk ls ("_G.__tick(1); return _G.__tick("
                                <> tshow destId <> ")")
                phase ← evalOk ls "return _G.__phase()"
                phase `shouldBe` "\"open\""
                both ← evalOk ls ("return { src = _G.__session().roleOf(1), \
                                  \         dst = _G.__session().roleOf("
                                  <> tshow destId <> ") }")
                both `shouldSatisfy` T.isInfixOf "\"src\":\"source\""
                both `shouldSatisfy` T.isInfixOf "\"dst\":\"target\""
                pure destId

        it "the source pane offers Store and the destination pane Retrieve, \
           \each 1-and-all on a merged row and 1 alone on a single instance" $
           \(env, ls) → do
            openOnHold env ls
            store ← evalOk ls
                "return _G.__labels(_G.__rowMenu('source', 'ration'))"
            store `shouldBe` "\"Store 1|Store all\""
            lone ← evalOk ls
                "return _G.__labels(_G.__rowMenu('source', 'rope'))"
            lone `shouldBe` "\"Store 1\""
            retrieve ← evalOk ls
                "return _G.__labels(_G.__rowMenu('destination', 'steel_bar'))"
            retrieve `shouldBe` "\"Retrieve 1|Retrieve all\""
            loneOther ← evalOk ls
                "return _G.__labels(_G.__rowMenu('destination', 'crowbar'))"
            loneOther `shouldBe` "\"Retrieve 1\""

        it "Store all moves EVERY exact instance the merged row stands for, \
           \immediately, and Retrieve 1 moves the representative alone" $
           \(env, ls) → do
            openOnHold env ls
            before ← evalOk ls "return _G.__ids('building', 7)"
            before `shouldBe` "\"crowbar#210,steel_bar#201,steel_bar#202\""
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'ration'), \
                \                          'Store all'))"
            afterStore ← evalOk ls "return _G.__ids('building', 7)"
            afterStore `shouldBe`
                "\"crowbar#210,ration#101,ration#102,ration#103,\
                \steel_bar#201,steel_bar#202\""
            carrierLeft ← evalOk ls "return _G.__ids('unit', 1)"
            carrierLeft `shouldBe` "\"rope#110\""
            -- ...and back the other way, one instance only.
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('destination', 'steel_bar'), \
                \                          'Retrieve 1'))"
            carrierNow ← evalOk ls "return _G.__ids('unit', 1)"
            carrierNow `shouldSatisfy` T.isInfixOf "steel_bar#201"
            carrierNow `shouldSatisfy` T.isInfixOf "rope#110"
            holdNow ← evalOk ls "return _G.__ids('building', 7)"
            holdNow `shouldNotSatisfy` T.isInfixOf "steel_bar#201"
            holdNow `shouldSatisfy` T.isInfixOf "steel_bar#202"
            -- The session is untouched by a commit: repeatable while
            -- adjacent is the whole point.
            still ← evalOk ls
                "return { phase = _G.__phase(), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            still `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            still `shouldSatisfy` T.isInfixOf "\"depth\":1"

        it "both panes refresh in the same gesture: the header weights move \
           \without waiting for a tick and without reopening anything" $
           \(env, ls) → do
            openOnHold env ls
            subs ← evalOk ls
                "local d = require('scripts.cargo_inventory_panel').dump(); \
                \return { a = d.levels[1].panes[1].subtitle, \
                \         b = d.levels[1].panes[2].subtitle }"
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'ration'), \
                \                          'Store all'))"
            after ← evalOk ls
                "local d = require('scripts.cargo_inventory_panel').dump(); \
                \return { a = d.levels[1].panes[1].subtitle, \
                \         b = d.levels[1].panes[2].subtitle, \
                \         depth = d.depth }"
            after `shouldNotBe` subs
            after `shouldSatisfy` T.isInfixOf "\"depth\":1"
            -- The carrier's own pane now reads the lighter load, and
            -- the hold's the heavier one — both LIVE, not the values
            -- either had when the window opened.
            after `shouldSatisfy` T.isInfixOf "Carrying: 2.00"
            after `shouldSatisfy` T.isInfixOf "Storage: 8.50"

        it "a partial batch commits what fits and reports the remainder by \
           \count and by the contract's own reason (D-1)" $ \(env, ls) → do
            resetFixture env ls
            -- The tiny hold takes 1.2 kg: exactly two of three rations.
            placeUnit env carrierUid (81, 80)
            _ ← evalOk ls (createLua "building" 8)
            _ ← evalOk ls "return _G.__tick(1)"
            phase ← evalOk ls "return _G.__phase()"
            phase `shouldBe` "\"open\""
            _ ← evalOk ls "_G.__clearEvents(); return 'ok'"
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'ration'), \
                \                          'Store all'))"
            stored ← evalOk ls "return _G.__ids('building', 8)"
            stored `shouldBe` "\"ration#101,ration#102\""
            kept ← evalOk ls "return _G.__ids('unit', 1)"
            kept `shouldSatisfy` T.isInfixOf "ration#103"
            reported ← evalOk ls "return _G.__eventText()"
            reported `shouldSatisfy` T.isInfixOf "unit_warning"
            reported `shouldSatisfy` T.isInfixOf "couldn't Store 1 x"
            reported `shouldSatisfy` T.isInfixOf "receiver_full"
            -- No item half-moved: every instance is on exactly one side.
            open' ← evalOk ls
                "return { phase = _G.__phase(), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            open' `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            open' `shouldSatisfy` T.isInfixOf "\"depth\":1"

        it "a target that drifts out of reach refuses by PROXIMITY, moves \
           \nothing, and leaves the session open (requirement 6)" $
           \(env, ls) → do
            resetFixture env ls
            placeUnit env matesUid (11, 10)
            _ ← evalOk ls (createLua "unit" 2)
            _ ← evalOk ls "return _G.__tick(1)"
            opened ← evalOk ls "return _G.__phase()"
            opened `shouldBe` "\"open\""
            -- The mate is TELEPORTED away — a direct fixture write, not
            -- a walk, so #1251's hold on it is not what is under test
            -- here and could not have prevented this. What is under test
            -- is that the COMMIT is authoritative about reach however
            -- the drift happened. Nothing re-approaches, and UIT-5B
            -- (#1254) deliberately left that so: a live, commandable
            -- endpoint that merely drifted is not a session failure.
            placeUnit env matesUid (60, 60)
            _ ← evalOk ls "_G.__clearEvents(); return 'ok'"
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'ration'), \
                \                          'Store all'))"
            mate ← evalOk ls "return _G.__ids('unit', 2)"
            mate `shouldBe` "\"\""
            carrier ← evalOk ls "return _G.__ids('unit', 1)"
            carrier `shouldSatisfy` T.isInfixOf "ration#101"
            reported ← evalOk ls "return _G.__eventText()"
            reported `shouldSatisfy` T.isInfixOf "unit_warning"
            reported `shouldSatisfy` T.isInfixOf "out_of_range"
            survives ← evalOk ls
                "return { phase = _G.__phase(), held = _G.__session().holdsUnit(1), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            survives `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            survives `shouldSatisfy` T.isInfixOf "\"held\":true"
            survives `shouldSatisfy` T.isInfixOf "\"depth\":1"

        -- #1251 requirement 3 / D-10: acolyte↔acolyte and mule↔acolyte
        -- are the SAME path. Store and Retrieve are exercised in one
        -- session each, because "both directions" is a property of the
        -- open window rather than of two separate gestures — the pane
        -- the player clicked is what picks the direction.
        it "acolyte to acolyte: Store and Retrieve both commit exact \
           \instances between two held units, in one session" $
           \(env, ls) → do
            _ ← openPair env ls matesUid mateInventory
            labels ← evalOk ls
                "return { store = _G.__labels(_G.__rowMenu('source', 'ration')), \
                \         take  = _G.__labels(_G.__rowMenu('destination', 'bandage')) }"
            labels `shouldSatisfy` T.isInfixOf "Store 1|Store all"
            labels `shouldSatisfy` T.isInfixOf "Retrieve 1|Retrieve all"
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'ration'), \
                \                          'Store all'))"
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('destination', 'bandage'), \
                \                          'Retrieve all'))"
            mate ← evalOk ls "return _G.__ids('unit', 2)"
            mate `shouldBe` "\"canteen#310,ration#101,ration#102,ration#103\""
            carrier ← evalOk ls "return _G.__ids('unit', 1)"
            carrier `shouldBe` "\"bandage#301,bandage#302,rope#110\""
            -- Repeatable while adjacent, exactly as against a building:
            -- neither commit ended the session or moved the window.
            still ← evalOk ls
                "return { phase = _G.__phase(), \
                \         src = _G.__session().holdsUnit(1), \
                \         dst = _G.__session().holdsUnit(2), \
                \         depth = require('scripts.cargo_inventory_panel').depth() }"
            still `shouldSatisfy` T.isInfixOf "\"phase\":\"open\""
            still `shouldSatisfy` T.isInfixOf "\"src\":true"
            still `shouldSatisfy` T.isInfixOf "\"dst\":true"
            still `shouldSatisfy` T.isInfixOf "\"depth\":1"

        it "acolyte to technomule commits identically — a DIFFERENT \
           \species on the far end changes nothing" $ \(env, ls) → do
            _ ← openPair env ls muleUid muleInventory
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'rope'), \
                \                          'Store 1'))"
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('destination', 'steel_plate'), \
                \                          'Retrieve all'))"
            mule ← evalOk ls "return _G.__ids('unit', 3)"
            mule `shouldBe` "\"rope#110\""
            carrier ← evalOk ls "return _G.__ids('unit', 1)"
            carrier `shouldBe`
                "\"ration#101,ration#102,ration#103,\
                \steel_plate#401,steel_plate#402\""

        it "a technomule may be the ESCORT too: the source side is no more \
           \species-specific than the target side" $ \(env, ls) → do
            resetFixture env ls
            stockUnit env muleUid muleInventory []
            placeUnit env muleUid (10, 10)
            placeUnit env matesUid (11, 10)
            created ← evalOk ls
                "return tostring(_G.__session().create(3, 'unit', 2) ~= nil)"
            created `shouldBe` "\"true\""
            opened ← evalOk ls "_G.__tick(3); _G.__tick(2); return _G.__phase()"
            opened `shouldBe` "\"open\""
            roles ← evalOk ls
                "return { src = _G.__session().roleOf(3), \
                \         dst = _G.__session().roleOf(2), \
                \         carrier = tostring(_G.__session().roleOf(1)) }"
            roles `shouldSatisfy` T.isInfixOf "\"src\":\"source\""
            roles `shouldSatisfy` T.isInfixOf "\"dst\":\"target\""
            roles `shouldSatisfy` T.isInfixOf "\"carrier\":\"nil\""
            _ ← evalOk ls
                "return tostring(_G.__fire(_G.__rowMenu('source', 'steel_plate'), \
                \                          'Store all'))"
            mate ← evalOk ls "return _G.__ids('unit', 2)"
            mate `shouldBe` "\"steel_plate#401,steel_plate#402\""

        -- #1251 requirement 4 (D-6, contract-enforced — not
        -- reimplemented here). 'Test.Headless.Unit.Transfer' owns the
        -- policy proof; what this adds is that BOTH of the session's own
        -- surfaces agree with it, which is the only place a unit-to-unit
        -- session could have introduced a way around it.
        it "a worn accessory is not a session row at all, and the contract \
           \still refuses it by name if anything names it anyway" $
           \(env, ls) → do
            _ ← openPair env ls matesUid mateInventory
            -- Put the robe on the carrier with the window already open,
            -- then refresh, so the pane is re-derived from the endpoint
            -- as it stands rather than from an opening snapshot.
            stockUnit env carrierUid carrierInventory carrierWorn
            _ ← evalOk ls
                "local c = require('scripts.cargo_inventory_panel'); \
                \c.refreshLevel(c.getLevel(1)); return 'ok'"
            -- It weighs into the endpoint's load — 3 x 0.5 + 2.0 loose,
            -- plus the 1.5 robe — which is the whole reason the contract
            -- keeps worn gear visible to the capacity gate...
            weighed ← evalOk ls
                "local i = unit.transferEndpointInfo({kind='unit', id=1}); \
                \return string.format('%.2f', i.storedWeight)"
            weighed `shouldBe` "\"5.00\""
            -- ...but it is not among the rows, so no menu can offer it.
            rows ← evalOk ls "return _G.__ids('unit', 1)"
            rows `shouldBe` "\"ration#101,ration#102,ration#103,rope#110\""
            noMenu ← evalOk ls
                "return tostring(_G.__rowMenu('source', 'acolyte_robe'))"
            noMenu `shouldBe` "\"nil\""
            -- And naming it directly on the session's OWN endpoint pair
            -- — the identical request `commitNow` builds — is refused by
            -- the contract's own reason, with nothing moved.
            refused ← evalOk ls
                "local s = _G.__session().get(); \
                \local r = unit.commitTransfer({ source = s.source, \
                \  destination = s.destination, \
                \  items = { { instanceId = 120, defName = 'acolyte_robe' } } }); \
                \local o = r and (r.outcomes or {})[1]; \
                \return { accepted = r and r.accepted, \
                \         state = o and o.state, reason = o and o.reason }"
            refused `shouldSatisfy` T.isInfixOf "\"accepted\":true"
            refused `shouldSatisfy` T.isInfixOf "\"state\":\"failed\""
            refused `shouldSatisfy` T.isInfixOf "\"reason\":\"item_not_transferable\""
            intact ← evalOk ls
                "return { mate = _G.__ids('unit', 2), \
                \         carrier = _G.__ids('unit', 1) }"
            intact `shouldSatisfy`
                T.isInfixOf "\"mate\":\"bandage#301,bandage#302,canteen#310\""
            intact `shouldSatisfy`
                T.isInfixOf "\"carrier\":\"ration#101,ration#102,ration#103,rope#110\""
