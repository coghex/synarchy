{-# LANGUAGE Strict #-}
-- | The live half of the "Faction relation store" gate (#2548
--   requirement 3): the relation authority reads the STORE at runtime.
--
--   Everything goes through the production verbs and the real unit AI
--   ('Test.Headless.Lua.DirectedRelationsFixture' builds the Lua
--   session), and a cause reaches them only by being written into
--   'Engine.Core.State.relationCauseStoreRef' — there is no
--   registration-time overlay to pass, so a verb that cached the store
--   at registration would answer from a stale copy and fail here.
--
--   Own engine: every case rewrites the unit manager ref and the store.
module Test.Headless.Faction.RelationStoreLive (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (atomicModifyIORef', readIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Faction.RelationStore.Fixture
import Test.Headless.Lua.DirectedRelationsFixture
import Unit.Faction (FactionRelation(..))
import Unit.Faction.Profile (relationCauseId)
import Unit.Faction.RelationStore
import World.Thread.Command.Basic (handleWorldDestroyAllCommand)

modifyStore ∷ EngineEnv → (RelationCauseStore → RelationCauseStore) → IO ()
modifyStore env f =
    atomicModifyIORef' (relationCauseStoreRef env) (\s → (f s, ()))

-- | Every ordered pair's three answers, in one comparable list.
answers ∷ LuaBackendState → [Int] → IO [Text]
answers ls uids = forM [ (a, b) | a ← uids, b ← uids ] $ \(a, b) →
    let pair = tshow a <> ", " <> tshow b
    in val ls ("unit.relationFromTo(" <> pair <> ") .. ',' .. tostring(unit.canAttack("
               <> pair <> ")) .. ',' .. tostring(unit.isFriendlyToward("
               <> pair <> "))")

spec ∷ SpecWith EngineEnv
spec = describe "Faction relation store" $ describe "live relation answers" $ do
    let fightRoster =
            [ (1, prof False ["fight_a"] [], (0, 0))
            , (2, prof False ["fight_b"] [], (0, 0))
            , (3, prof False ["fight_c"] [], (0, 0))
            , (4, prof False ["acolyte"] [], (0, 0))
            , (5, prof False ["nomad"] [], (0, 0)) ]
        uids = [1 .. 5]

    it "an EMPTY store reproduces every answer FTS-5 shipped, and a cause \
       \matching no unit's tags leaves every answer unchanged" $ \env → do
        resetWorld env fightRoster
        ls ← session env
        baseline ← answers ls uids
        -- Non-vacuity: the shipped acolyte/nomad hostility is in there,
        -- and every unknown-tag pair is neutral.
        baseline `shouldSatisfy` elem "\"hostile,true,false\""
        baseline `shouldSatisfy` elem "\"neutral,false,false\""
        modifyStore env (const sampleStore)
        answers ls uids `shouldReturn` baseline
        modifyStore env (const emptyRelationCauseStore)
        answers ls uids `shouldReturn` baseline

    it "a hostile cause fight_a → fight_b flips the live relation and the \
       \attack permission for that ORDERED pair only — the reverse \
       \direction and every other pair keep their answers" $ \env → do
        resetWorld env fightRoster
        ls ← session env
        baseline ← answers ls uids
        modifyStore env
            (addStoredCause (cause "fixture_fight" "fight_a" "fight_b" RelHostile))
        after ← answers ls uids
        let changed = [ ((a, b), x, y)
                      | ((a, b), x, y) ←
                            zip3 [ (a, b) | a ← uids, b ← uids ] baseline after
                      , x ≢ y ]
        map (\(p, _, y) → (p, y)) changed `shouldBe`
            [ ((1, 2), "\"hostile,true,false\"") ]
        -- The reverse direction is exactly what it was.
        val ls "unit.relationFromTo(2, 1) .. ',' .. tostring(unit.canAttack(2, 1))"
            `shouldReturn` "\"neutral,false\""

    it "reaches the engagement seam with no consumer change: the struck \
       \unit keeps fighting a HOSTILE attacker past the ten-second \
       \self-defense window, and nobody else does" $ \env → do
        resetWorld env fightRoster
        ls ← session env
        let heldAt30 victim attacker = do
                ok ls "AI.aiState[1] = nil; AI.aiState[2] = nil; CLAIMS.resetAll(); return true"
                hit env victim attacker 0
                ok ls "NOW = 1; return true"
                -- Acquired either way: local self-defense is permission-free.
                _ ← val ls ("engage(" <> tshow victim <> ")")
                ok ls "NOW = 30; return true"
                val ls ("tick(" <> tshow victim <> ")")
        -- Empty store: self-defense expired long ago, nobody holds on.
        heldAt30 1 2 `shouldReturn` "false"
        modifyStore env
            (addStoredCause (cause "fixture_fight" "fight_a" "fight_b" RelHostile))
        -- fight_a regards fight_b as hostile: unit 1 is still fighting 2.
        heldAt30 1 2 `shouldReturn` "2"
        -- One direction only: unit 2 does not hold on to attacker 1,
        -- and unit 1 does not hold on to an attacker (3) it has no
        -- cause against.
        heldAt30 2 1 `shouldReturn` "false"
        heldAt30 1 3 `shouldReturn` "false"

    it "reads the store afresh on EVERY call: removing the cause by owner \
       \flips the very next answer back, with no re-registration" $ \env → do
        resetWorld env fightRoster
        ls ← session env
        let rel = val ls "unit.relationFromTo(1, 2)"
        rel `shouldReturn` "\"neutral\""
        modifyStore env
            (addStoredCause (cause "o_1" "fight_a" "fight_b" RelHostile))
        rel `shouldReturn` "\"hostile\""
        modifyStore env (removeCausesByOwner (relationCauseId "o_1"))
        rel `shouldReturn` "\"neutral\""
        readIORef (relationCauseStoreRef env)
            `shouldReturn` emptyRelationCauseStore

    it "two owners on one pair hold independently: removing one keeps the \
       \relation, a softer cause never softens a hostile one, and \
       \removing both restores the base" $ \env → do
        resetWorld env fightRoster
        ls ← session env
        let rel = val ls "unit.relationFromTo(1, 2)"
        modifyStore env
            ( addStoredCause (cause "owner_hostile" "fight_a" "fight_b" RelHostile)
            ∘ addStoredCause (cause "owner_ally" "fight_a" "fight_b" RelAlly) )
        rel `shouldReturn` "\"hostile\""
        modifyStore env (removeCausesByOwner (relationCauseId "owner_ally"))
        rel `shouldReturn` "\"hostile\""
        modifyStore env
            (addStoredCause (cause "owner_ally" "fight_a" "fight_b" RelAlly))
        modifyStore env (removeCausesByOwner (relationCauseId "owner_hostile"))
        -- Only the ally cause remains: it outranks the neutral default
        -- tier's absence, so the pair is allied until it too goes.
        rel `shouldReturn` "\"ally\""
        modifyStore env (removeCausesByOwner (relationCauseId "owner_ally"))
        rel `shouldReturn` "\"neutral\""

    it "does not outlive its session: after Exit to Menu a NEW roster that \
       \reuses the same tags answers from the base, never from the old \
       \cause" $ \env → do
        resetWorld env fightRoster
        ls ← session env
        modifyStore env
            (addStoredCause (cause "old_session" "fight_a" "fight_b" RelHostile))
        val ls "unit.relationFromTo(1, 2)" `shouldReturn` "\"hostile\""
        logger ← readIORef (loggerRef env)
        handleWorldDestroyAllCommand env logger
        readIORef (relationCauseStoreRef env)
            `shouldReturn` emptyRelationCauseStore
        -- The same-process next session reuses tag AND unit ids.
        -- The SAME Lua state, not a fresh 'session' (which would reset
        -- the store itself and hide whether the teardown did).
        resetWorld env fightRoster
        val ls "unit.relationFromTo(1, 2)" `shouldReturn` "\"neutral\""
        val ls "tostring(unit.canAttack(1, 2))" `shouldReturn` "\"false\""
