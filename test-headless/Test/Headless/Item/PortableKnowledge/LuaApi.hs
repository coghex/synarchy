-- | The four registered @item.*@ verbs (#2512 requirement 5), read
--   through the REAL Lua backend with the whole engine API registered —
--   so what is asserted is the projection a script actually receives,
--   including which fields are ABSENT.
--
--   Absence is the point of most of these examples. A consumer that
--   read a missing @storedWeight@ as @0@ would turn "never learned" into
--   "measured as nothing", which is exactly the conflation the four
--   states exist to prevent; only a real table read can tell @nil@ from
--   @0@.
--
--   The engine is PRIVATE (its own 'initializeEngineHeadlessQuiet' plus
--   its own Lua backend), because these examples install content
--   definitions and a whole synthetic page set into it.
module Test.Headless.Item.PortableKnowledge.LuaApi (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, writeIORef)
import Building.Types
    (BuildingId(..), BuildingInstance(..), BuildingManager(..)
    , emptyBuildingManager)
import Engine.Core.State
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Knowledge
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Item.PortableKnowledge.Fixture
import Unit.Types (UnitId(..), UnitManager(..), emptyUnitManager)
import World.Thread (worldTickWith)
import World.State.Types
import World.Thread.Command.Basic (handleWorldDestroyAllCommand)
import Engine.Core.Init (EngineInitResult(..))

-- | A private engine with a real Lua backend, a two-page session, and
--   the crate sitting in a building's storage on the HIDDEN page — so
--   nothing here can pass by reaching the visible page's ground.
data Bindings = Bindings
    { bnEnv ∷ EngineEnv
    , bnLua ∷ LuaBackendState
    }

withBindings ∷ (Bindings → IO α) → IO α
withBindings act = withIsolatedResourceRoot $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    writeIORef (itemManagerRef env) testItems
    wsA ← emptyWorldState
    wsB ← emptyWorldState
    writeIORef (wsGroundItemsRef wsA) (groundWith [loose])
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(pkPageA, wsA), (pkPageB, wsB)]
        , wmVisible = [pkPageA] }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.singleton "cargo_hold_S" storageDef
        , bmInstances = HM.singleton (BuildingId 1)
              (mkBuilding pkPageB "cargo_hold_S" HM.empty [crate])
        , bmNextId = 2 }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umInstances = HM.singleton (UnitId 1)
              (mkUnit pkPageA [] HM.empty [])
        , umNextId = 2 }
    writeIORef (gameTimeRef env) 500
    act (Bindings env ls)

-- | Run ONE world-thread tick, draining the world queue.
--
--   The observe and forget verbs ENQUEUE (#2512): the world thread owns
--   'World.State.Types.wmPortableKnowledge', so a Lua verb measures the
--   observation synchronously and hands the merge to its owner. Every
--   example that asserts on the resulting record therefore drives the
--   real drain rather than expecting the write to have already
--   happened — and the examples below that assert a record is ABSENT
--   drain first too, so "absent" means the command never existed rather
--   than merely that it had not run yet.
drainWorld ∷ Bindings → IO ()
drainWorld b = do
    lastRef ← newIORef 0
    _ ← worldTickWith (pure 0) (bnEnv b) lastRef
    pure ()

-- | Evaluate a Lua chunk and return its result as text. Chunks here
--   return BOOLEANS and NUMBERS, never bare strings: the console
--   quotes a string result, so a string assertion would be comparing
--   against its own quoting rather than against the value.
luaEval ∷ Bindings → Text → IO Text
luaEval b code =
    T.strip ∘ T.filter (≢ '"') <$> executeDebugLua (lbsLuaState (bnLua b)) code

-- | @true@/@false@ from a predicate chunk.
luaBool ∷ Bindings → Text → IO Bool
luaBool b code = (≡ "true") <$> luaEval b ("return " <> code)

-- | The knowledge table for the crate, bound as @k@ in the chunk.
onCrate ∷ Bindings → Text → IO Bool
onCrate b expr = luaBool b
    ("(function() local k = item.getContainerKnowledge(" <> tshow crateId
     <> ") return " <> expr <> " end)()")

-- | Call one observe/forget verb and let its queued command run, so
--   the example can assert on the record it produced.
verb ∷ Bindings → Text → IO Bool
verb b call = do
    ok ← luaBool b call
    drainWorld b
    pure ok

atTime ∷ Bindings → Double → IO ()
atTime b t = writeIORef (gameTimeRef (bnEnv b)) t

knowledgeOf ∷ Bindings → IO PortableKnowledge
knowledgeOf b = wmPortableKnowledge <$> readIORef (worldManagerRef (bnEnv b))

spec ∷ Spec
spec = around withBindings $ do

    describe "item.getContainerKnowledge -- absence is a value" $ do
        it "answers a table with state 'unknown' and NO learned fields \
           \for a crate nobody has touched" $ \b → do
            onCrate b "k ~= nil" `shouldReturn` True
            onCrate b "k.state == 'unknown'" `shouldReturn` True
            onCrate b "k.items == nil" `shouldReturn` True
            onCrate b "k.storedWeight == nil" `shouldReturn` True
            onCrate b "k.weighedAt == nil" `shouldReturn` True
            onCrate b "k.revealedAt == nil" `shouldReturn` True

        it "reports LIVE capacity even for an unknown crate, read from \
           \the located instance's own storage rather than remembered" $
            \b → onCrate b "k.capacity == 60" `shouldReturn` True

        it "omits capacity entirely for an instance that cannot be \
           \located -- never a fabricated 0, which the ownership rules \
           \read as a real accepts-nothing capacity" $ \b → do
            has ← luaBool b ("(function() local k = \
                             \item.getContainerKnowledge("
                             <> tshow unlocatableId
                             <> ") return k.capacity == nil end)()")
            has `shouldBe` True

        it "omits capacity for a locatable item that declares no \
           \internal storage at all" $ \b → do
            ok ← luaBool b ("(function() local k = \
                            \item.getContainerKnowledge(" <> tshow looseId
                            <> ") return k.capacity == nil and \
                            \k.state == 'unknown' end)()")
            ok `shouldBe` True

        it "answers nil for a non-numeric argument -- tointeger COERCES, \
           \so the string '41' must not act on crate #41" $ \b → do
            luaBool b ("item.getContainerKnowledge('" <> tshow crateId
                       <> "') == nil") `shouldReturn` True
            luaBool b "item.getContainerKnowledge(nil) == nil"
                `shouldReturn` True
            luaBool b "item.getContainerKnowledge(0) == nil"
                `shouldReturn` True

    describe "item.observeContainerWeight" $ do
        it "records the weight and its stamp, and NOTHING about the \
           \contents" $ \b → do
            atTime b 700
            verb b ("item.observeContainerWeight(" <> tshow crateId <> ")")
                `shouldReturn` True
            onCrate b "k.state == 'weight-only'" `shouldReturn` True
            onCrate b "k.weighedAt == 700" `shouldReturn` True
            onCrate b "k.storedWeight > 0" `shouldReturn` True
            onCrate b "k.items == nil" `shouldReturn` True
            onCrate b "k.revealedAt == nil" `shouldReturn` True

        it "answers false and ENQUEUES NOTHING for an id nothing live \
           \carries -- the observation is OF a physical item, and the \
           \drain proves no command was left behind to land later" $
            \b → do
            verb b ("item.observeContainerWeight(" <> tshow unlocatableId
                       <> ")") `shouldReturn` False
            drainWorld b
            k ← knowledgeOf b
            knownPortableIds k `shouldBe` []

        it "answers false for a string argument, and enqueues nothing" $
            \b → do
            verb b ("item.observeContainerWeight('" <> tshow crateId
                       <> "')") `shouldReturn` False
            drainWorld b
            knownPortableIds <$> knowledgeOf b `shouldReturn` []

    describe "item.observeContainerContents" $ do
        it "records the contents, the weight and BOTH stamps at the \
           \current game time" $ \b → do
            atTime b 800
            verb b ("item.observeContainerContents(" <> tshow crateId
                       <> ")") `shouldReturn` True
            onCrate b "k.state == 'known'"   `shouldReturn` True
            onCrate b "#k.items == 1"        `shouldReturn` True
            onCrate b "k.revealedAt == 800"  `shouldReturn` True
            onCrate b "k.weighedAt == 800"   `shouldReturn` True

        it "then a LATER weighing moves only weighedAt, leaving \
           \revealedAt and the remembered contents where the open left \
           \them" $ \b → do
            atTime b 800
            _ ← verb b ("item.observeContainerContents(" <> tshow crateId
                           <> ")")
            atTime b 900
            _ ← verb b ("item.observeContainerWeight(" <> tshow crateId
                           <> ")")
            onCrate b "k.weighedAt == 900"  `shouldReturn` True
            onCrate b "k.revealedAt == 800" `shouldReturn` True
            onCrate b "k.state == 'known'"  `shouldReturn` True
            onCrate b "#k.items == 1"       `shouldReturn` True

        it "an EMPTY crate projects an EMPTY items table, not a missing \
           \one: 'observed and found empty' is a fact a consumer must be \
           \able to render" $ \b → do
            writeIORef (buildingManagerRef (bnEnv b)) emptyBuildingManager
                { bmDefs = HM.singleton "cargo_hold_S" storageDef
                , bmInstances = HM.singleton (BuildingId 1)
                      (mkBuilding pkPageB "cargo_hold_S" HM.empty [crateEmpty])
                , bmNextId = 2 }
            atTime b 850
            _ ← verb b ("item.observeContainerContents(" <> tshow crateId
                           <> ")")
            onCrate b "k.state == 'empty'" `shouldReturn` True
            onCrate b "k.items ~= nil"     `shouldReturn` True
            onCrate b "#k.items == 0"      `shouldReturn` True

        it "gives a NESTED container no record of its own until it is \
           \itself observed -- and the kit IS observable, because the \
           \locator descends to it" $ \b → do
            atTime b 800
            _ ← verb b ("item.observeContainerContents(" <> tshow crateId
                           <> ")")
            kitState ← luaEval b
                ("return item.getContainerKnowledge(" <> tshow kitId
                 <> ").state == 'unknown'")
            kitState `shouldBe` "true"
            atTime b 810
            verb b ("item.observeContainerContents(" <> tshow kitId <> ")")
                `shouldReturn` True
            luaBool b ("item.getContainerKnowledge(" <> tshow kitId
                       <> ").state == 'known'") `shouldReturn` True

        it "does not mutate the live crate: after the observation the \
           \real building still holds exactly what it held before" $
            \b → do
            before' ← readIORef (buildingManagerRef (bnEnv b))
            atTime b 800
            _ ← verb b ("item.observeContainerContents(" <> tshow crateId
                           <> ")")
            after' ← readIORef (buildingManagerRef (bnEnv b))
            (biStorage <$> HM.lookup (BuildingId 1) (bmInstances after'))
                `shouldBe` (biStorage <$> HM.lookup (BuildingId 1)
                                (bmInstances before'))

    describe "item.forgetContainerKnowledge" $ do
        it "drops the record and reports that it did, then reports false \
           \for the second call" $ \b → do
            atTime b 800
            _ ← verb b ("item.observeContainerWeight(" <> tshow crateId
                           <> ")")
            verb b ("item.forgetContainerKnowledge(" <> tshow crateId
                       <> ")") `shouldReturn` True
            onCrate b "k.state == 'unknown'" `shouldReturn` True
            verb b ("item.forgetContainerKnowledge(" <> tshow crateId
                       <> ")") `shouldReturn` False

        it "clears the memory of a crate that has since been DESTROYED \
           \-- which is exactly the record a caller most needs to be \
           \able to drop, so it must not require locating anything" $
            \b → do
            atTime b 800
            _ ← verb b ("item.observeContainerWeight(" <> tshow crateId
                           <> ")")
            -- The crate is gone from the session entirely.
            writeIORef (buildingManagerRef (bnEnv b)) emptyBuildingManager
                { bmDefs = HM.singleton "cargo_hold_S" storageDef
                , bmNextId = 2 }
            verb b ("item.observeContainerWeight(" <> tshow crateId <> ")")
                `shouldReturn` False
            verb b ("item.forgetContainerKnowledge(" <> tshow crateId
                       <> ")") `shouldReturn` True
            knownPortableIds <$> knowledgeOf b `shouldReturn` []

    describe "the world thread owns the merge (#2512)" $ do
        it "the verb only ENQUEUES: nothing has changed by the time it \
           \returns, and the record appears once the world thread \
           \drains" $ \b → do
            atTime b 800
            luaBool b ("item.observeContainerWeight(" <> tshow crateId
                       <> ")") `shouldReturn` True
            -- Deliberately NOT drained yet. A Lua-thread write would
            -- already be visible here, and this is what separates the
            -- two designs.
            knownPortableIds <$> knowledgeOf b `shouldReturn` []
            drainWorld b
            knownPortableIds <$> knowledgeOf b `shouldReturn` [crateId]

        it "an Exit-to-Menu teardown queued AFTER the observation still \
           \wins, because both run on the one owner in FIFO order -- a \
           \departed session's crate memory cannot survive into the \
           \next one" $ \b → do
            atTime b 800
            _ ← verb b ("item.observeContainerContents(" <> tshow crateId
                        <> ")")
            knownPortableIds <$> knowledgeOf b `shouldReturn` [crateId]
            logger ← readIORef (loggerRef (bnEnv b))
            handleWorldDestroyAllCommand (bnEnv b) logger
            knownPortableIds <$> knowledgeOf b `shouldReturn` []

        it "a weigh MERGES against the map as it is when the world \
           \thread runs it, preserving a contents observation recorded \
           \in between rather than one the Lua thread happened to see" $
            \b → do
            atTime b 800
            luaBool b ("item.observeContainerWeight(" <> tshow crateId
                       <> ")") `shouldReturn` True
            -- The open is measured and queued BEHIND the weigh, so the
            -- weigh merges into an empty map and the open then replaces
            -- it. Both land in the order they were asked for.
            atTime b 900
            luaBool b ("item.observeContainerContents(" <> tshow crateId
                       <> ")") `shouldReturn` True
            drainWorld b
            onCrate b "k.state == 'known'"   `shouldReturn` True
            onCrate b "k.weighedAt == 900"   `shouldReturn` True
            onCrate b "k.revealedAt == 900"  `shouldReturn` True

    describe "the shared window contract" $
        it "uses the SAME field names the building projection does, so \
           \one renderer consumes either -- plus weighedAt, which a \
           \building has no analogue for" $ \b → do
            atTime b 800
            _ ← verb b ("item.observeContainerContents(" <> tshow crateId
                           <> ")")
            shared ← onCrate b
                "k.state ~= nil and k.items ~= nil and \
                \k.storedWeight ~= nil and k.revealedAt ~= nil and \
                \k.capacity ~= nil"
            shared `shouldBe` True
            onCrate b "k.weighedAt ~= nil" `shouldReturn` True
