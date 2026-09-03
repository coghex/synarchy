-- | Mode A's own source guards over @scripts\/unit_ai.lua@'s action
--   lists.
--
--   These two examples read the script off disk and take no fixture,
--   so — alone among the aggregate gate's groups — they perform no
--   per-case reset. That is the shape they have always had, not
--   something the split introduced.
--
--   One group of the aggregate gate in
--   "Test.Headless.UI.TransferSession", composed there alongside the
--   behavioural groups that share
--   "Test.Headless.UI.TransferSession.Fixture".
module Test.Headless.UI.TransferSession.Registration (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.Types (LuaBackendState)

spec ∷ SpecWith (EngineEnv, LuaBackendState)
spec =
    describe "registration" $ do
        -- Source guards beside the behavioural cases of the sibling
        -- groups, following the "random stream ownership" precedent:
        -- neither side of the hold is reachable unless it is in the
        -- real action lists, and nothing the shared fixture can drive
        -- would notice its absence.
        it "the escort action is registered for every player-commandable \
           \species the source rule can resolve" $ \_ → do
            src ← TIO.readFile "scripts/unit_ai.lua"
            let regs = length (T.breakOnAll "transfer.escortAction," src)
            regs `shouldBe` 2

        -- #1251 review round 1: the TARGET side must reach every
        -- species, so it lives in the auto-prepended universal list and
        -- NOWHERE else. Two per-species mentions would be the same
        -- allowlist this exists to avoid, one species at a time.
        it "the target-side hold is registered UNIVERSALLY — once, in the \
           \list every species is given, never per species" $ \_ → do
            src ← TIO.readFile "scripts/unit_ai.lua"
            -- Named exactly once, and that once is inside the universal
            -- list: a second mention would be the per-species allowlist
            -- this exists to avoid, one species at a time.
            let named = length (T.breakOnAll "transfer.escortHoldAction" src)
            named `shouldBe` 1
            let universal = snd (T.breakOn "local UNIVERSAL_ACTIONS = {" src)
                listBody  = fst (T.breakOn "\n}" universal)
            listBody `shouldSatisfy` T.isInfixOf "transfer.escortHoldAction"
            -- ...and registerActions really prepends that list, so a
            -- species registering through the public API gets it
            -- without asking.
            src `shouldSatisfy`
                T.isInfixOf "for _, a in ipairs(UNIVERSAL_ACTIONS) do"
