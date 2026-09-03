-- | #1415. 'M.create' is the ONE place a session is built and is
--   deliberately reusable by a surface that never ran
--   'M.resolveSource', so every rule that resolution enforces has to
--   hold HERE too. The two shipped menus screen all of them before
--   they call, which is why nothing player-facing changes; these
--   cases therefore call 'create' DIRECTLY, with no menu, no
--   resolution and no click, and read every answer back out of the
--   real engine rather than a stub.
--
--   One group of the aggregate gate in
--   "Test.Headless.UI.TransferSession", composed there under the
--   shared engine and Lua state of
--   "Test.Headless.UI.TransferSession.Fixture".
module Test.Headless.UI.TransferSession.CreationBoundary (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.Types (LuaBackendState)
import Unit.Faction (Faction(..))
import Test.Headless.UI.TransferSession.Fixture
    ( carrierUid, matesUid, muleUid, otherPage
    , placeUnit, setFaction, setPage
    , resetFixture, evalOk, createLua )

spec ∷ SpecWith (EngineEnv, LuaBackendState)
spec =
    describe "the creation boundary's own source rules (#1415)" $ do

        it "an uncommandable source creates no session, reports \
           \source_ineligible, and warns that unit exactly once" $
           \(env, ls) → do
            resetFixture env ls
            -- Faction loss is exactly what the engine's endpoint rule
            -- means: Unit.Transfer.endpointEligible is `uevCommandable`
            -- and nothing else, so this is the contract's own refusal
            -- read through the projection rather than a rule invented
            -- for the session.
            setFaction env carrierUid FactionWildlife
            refused ← evalOk ls
                "local s, reason = _G.__session().create(1, 'building', 7); \
                \return { made = tostring(s), reason = tostring(reason), \
                \         warns = _G.__warnCount(1) }"
            refused `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            refused `shouldSatisfy`
                T.isInfixOf "\"reason\":\"source_ineligible\""
            refused `shouldSatisfy` T.isInfixOf "\"warns\":1"
            after ← evalOk ls
                "return tostring(_G.__session().get())"
            after `shouldBe` "\"nil\""

        it "a self-transfer creates no session and reports \
           \receiver_ineligible — the id the ENGINE produces for \
           \from == to" $ \(env, ls) → do
            resetFixture env ls
            -- Nothing downstream ever caught this: 'endpointFailure'
            -- validates each side independently, so a session on one
            -- unit twice passed the liveness tick forever while every
            -- commit was refused by planItemWith's `from == to`.
            refused ← evalOk ls
                "local s, reason = _G.__session().create(1, 'unit', 1); \
                \return { made = tostring(s), reason = tostring(reason), \
                \         warns = _G.__warnCount(1) }"
            refused `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            refused `shouldSatisfy`
                T.isInfixOf "\"reason\":\"receiver_ineligible\""
            refused `shouldSatisfy` T.isInfixOf "\"warns\":1"
            after ← evalOk ls "return tostring(_G.__session().get())"
            after `shouldBe` "\"nil\""

        it "a cross-page pair creates no session and reports \
           \out_of_range, at IDENTICAL coordinates so only the page \
           \differs" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (10, 10)
            placeUnit env matesUid  (10, 10)
            setPage env matesUid otherPage
            -- Distance 0 and still refused: 'reachable' fails on page
            -- identity before it ever measures, and this asserts the
            -- session agrees with it. Left unchecked the escort simply
            -- never arrived, so the session sat in `approaching` with
            -- nothing ever going wrong enough for the liveness tick to
            -- close it.
            refused ← evalOk ls
                "local s, reason = _G.__session().create(1, 'unit', 2); \
                \return { made = tostring(s), reason = tostring(reason), \
                \         warns = _G.__warnCount(1) }"
            refused `shouldSatisfy` T.isInfixOf "\"made\":\"nil\""
            refused `shouldSatisfy` T.isInfixOf "\"reason\":\"out_of_range\""
            refused `shouldSatisfy` T.isInfixOf "\"warns\":1"
            after ← evalOk ls "return tostring(_G.__session().get())"
            after `shouldBe` "\"nil\""

        it "a DESTINATION failure still wins over a source one, so a call \
           \wrong on both sides reports the destination" $
           \(env, ls) → do
            resetFixture env ls
            setFaction env carrierUid FactionWildlife
            -- Building 999 does not exist AND the source is
            -- uncommandable. The placement rule the escort-capability
            -- check already documented says the destination answers.
            refused ← evalOk ls
                "local s, reason = _G.__session().create(1, 'building', 999); \
                \return tostring(reason)"
            refused `shouldBe` "\"receiver_missing\""

        it "each of the three new refusals leaves a RUNNING session — its \
           \identity, phase, hold, panes, both inventories and its stop \
           \count — exactly as it found them" $ \(env, ls) → do
            resetFixture env ls
            placeUnit env carrierUid (42, 41)
            _ ← evalOk ls (createLua "building" 7)
            _ ← evalOk ls "return _G.__tick(1)"
            -- One joined STRING rather than a table, so the comparison
            -- below is a real equality and not a bet on key order.
            let snapshot = "local cip = require('scripts.cargo_inventory_panel'); \
                           \return table.concat({ _G.__session().get().id, \
                           \  _G.__phase(), _G.__session().roleOf(1), \
                           \  cip.depth(), cip.dump().levels[1].paneCount, \
                           \  _G.__ids('unit', 1), _G.__ids('building', 7), \
                           \  _G.__stops }, '|')"
            before ← evalOk ls snapshot
            -- Three refusals in a row, one per NEW branch, and none of
            -- them may disturb what is open. Two of them are not even
            -- ABOUT this session: a refusal naming other units has to
            -- leave it alone just as surely as one naming its own.
            setPage env muleUid otherPage
            setFaction env matesUid FactionWildlife
            reasons ← evalOk ls
                "local s = _G.__session(); \
                \local _, a = s.create(1, 'unit', 1); \
                \local _, b = s.create(1, 'unit', 3); \
                \local _, c = s.create(2, 'building', 8); \
                \return { a = tostring(a), b = tostring(b), \
                \         c = tostring(c) }"
            reasons `shouldSatisfy` T.isInfixOf "\"a\":\"receiver_ineligible\""
            reasons `shouldSatisfy` T.isInfixOf "\"b\":\"out_of_range\""
            reasons `shouldSatisfy` T.isInfixOf "\"c\":\"source_ineligible\""
            after ← evalOk ls snapshot
            after `shouldBe` before
