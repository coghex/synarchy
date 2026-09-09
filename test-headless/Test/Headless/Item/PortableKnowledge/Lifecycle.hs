-- | The live owner's whole lifecycle (#2512 requirement 2): a session
--   starts with no crate memory, keeps it across page visibility
--   changes, carries it through the REAL save boundary, has it scrubbed
--   and REPLACED by a load, and loses it at Exit to Menu.
--
--   Every step runs through the production owner —
--   'World.Thread.Command.Save.WriteWorld.handleWorldSaveCommand',
--   'World.Save.Serialize.loadWorld', 'World.Load.Stage.stageSession',
--   'World.Load.Publish.publishStagedSession',
--   'World.Thread.Command.Basic.handleWorldDestroyAllCommand' — rather
--   than through a re-implementation of what each is believed to do.
--   A field wired into the DTO but never read off the manager would
--   satisfy a codec test and fail every one of these.
--
--   The engine here is PRIVATE ('initializeEngineHeadlessQuiet'), so
--   publishing a replacement session is safe: nothing else shares it.
--   @engine.saveWorld@ itself is deliberately not used — its owner
--   handshake needs threads this harness never starts — so the entry
--   point is the world-thread command handler it queues.
module Test.Headless.Item.PortableKnowledge.Lifecycle (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Control.Exception (finally)
import Data.IORef (readIORef, writeIORef)
import System.Directory (removePathForcibly)
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.State
import Item.Knowledge
import Item.Ground (GroundItem(..), GroundItems(..))
import Item.Types (ItemInstance(..))
import Test.Headless.Item.PortableKnowledge.Fixture
import Unit.Types (UnitManager(..))
import World.Load.Publish (publishStagedSession)
import World.Load.Stage (renderStageError, stageSession)
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Serialize (loadWorld)
import World.Save.Types (SaveData(..))
import World.State.Types
import World.Thread.Command.Basic (handleWorldDestroyAllCommand)
import World.Thread.Command.Init (handleWorldInitCommand)
import World.Thread.Command.Save.WriteWorld (handleWorldSaveCommand)
import World.Thread.Command.UI (handleWorldHideCommand, handleWorldShowCommand)

pageA ∷ WorldPageId
pageA = WorldPageId "portable_lifecycle_a"

-- | Two remembered crates: one that WILL still be live in the session
--   being loaded, and one that will not.
twoRecords ∷ PortableKnowledge
twoRecords =
    observePortableWeight testItems 700 loose
        (observePortableContents testItems 650 crate emptyPortableKnowledge)

knowledgeIn ∷ EngineEnv → IO PortableKnowledge
knowledgeIn env = wmPortableKnowledge <$> readIORef (worldManagerRef env)

setKnowledge ∷ EngineEnv → PortableKnowledge → IO ()
setKnowledge env k = do
    mgr ← readIORef (worldManagerRef env)
    writeIORef (worldManagerRef env) mgr { wmPortableKnowledge = k }

initEnv ∷ IO EngineEnv
initEnv = do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    pure env

-- | A real (cheap, w8) page plus the crate lying on its ground, and the
--   item defs it names registered — the shape a genuine session has
--   when the player has remembered something about a crate they can
--   still see.
withLiveCrateSession ∷ (EngineEnv → WorldState → IO α) → IO α
withLiveCrateSession act = do
    env ← initEnv
    logger ← readIORef (loggerRef env)
    writeIORef (itemManagerRef env) testItems
    handleWorldInitCommand env logger pageA 42 8 3 Nothing
    mWs ← lookup pageA ∘ wmWorlds <$> readIORef (worldManagerRef env)
    case mWs of
        Nothing → expectationFailure "world init did not register the page"
                    >> error "unreachable"
        Just ws → do
            writeIORef (wsGroundItemsRef ws) (groundWith [crate])
            -- The fixture ids are deliberately high, and the snapshot
            -- boundary refuses a session whose LIVE ids reach the
            -- allocator ('World.Save.Snapshot.itemAllocatorErrors'), so
            -- the cursor has to sit above them exactly as it would in a
            -- session that had really minted them.
            writeIORef (nextItemInstanceIdRef env) 1000
            act env ws

spec ∷ Spec
spec = do
    describe "the live owner" $ do
        it "starts EMPTY: an engine that has remembered nothing reports \
           \nothing, and every crate reads never-inspected" $ do
            env ← initEnv
            k ← knowledgeIn env
            k `shouldBe` emptyPortableKnowledge
            portableState crateId k `shouldBe` NeverInspected
            wmPortableKnowledge emptyWorldManager
                `shouldBe` emptyPortableKnowledge

        it "SURVIVES page visibility changes -- showing and hiding a \
           \page is not a session boundary, and a crate carried onto a \
           \hidden page is still the same crate" $ do
            env ← initEnv
            ws ← emptyWorldState
            logger ← readIORef (loggerRef env)
            writeIORef (worldManagerRef env) emptyWorldManager
                { wmWorlds = [(pageA, ws)], wmVisible = []
                , wmPortableKnowledge = twoRecords }
            handleWorldShowCommand (toWorldSimCapability env) logger pageA
            wmVisible <$> readIORef (worldManagerRef env)
                `shouldReturn` [pageA]
            knowledgeIn env `shouldReturn` twoRecords
            handleWorldHideCommand (toWorldSimCapability env) logger pageA
            wmVisible <$> readIORef (worldManagerRef env) `shouldReturn` []
            knowledgeIn env `shouldReturn` twoRecords

        it "is CLEARED at Exit to Menu, in the same atomic update that \
           \empties the page set -- the next session's crates are \
           \different crates, and an instance id is only unique within \
           \a session" $ do
            env ← initEnv
            ws ← emptyWorldState
            logger ← readIORef (loggerRef env)
            writeIORef (worldManagerRef env) emptyWorldManager
                { wmWorlds = [(pageA, ws)], wmVisible = [pageA]
                , wmPortableKnowledge = twoRecords }
            knowledgeIn env `shouldReturn` twoRecords
            handleWorldDestroyAllCommand env logger
            mgr ← readIORef (worldManagerRef env)
            map fst (wmWorlds mgr) `shouldBe` []
            wmPortableKnowledge mgr `shouldBe` emptyPortableKnowledge

    describe "through the real save/load boundary" $ do
        it "the production save captures what the MANAGER holds, and the \
           \load bridge carries it back with both stamps intact" $
            withLiveCrateSession $ \env _ws → do
            logger ← readIORef (loggerRef env)
            setKnowledge env twoRecords
            let slot = "hspec_portable_knowledge_2512_capture"
                cleanup = removePathForcibly ("saves/" <> slot)
            cleanup
            (`finally` cleanup) $ do
                handleWorldSaveCommand env logger pageA slot
                    "2026-09-08T00:00:00.000000Z" [] [] Nothing
                loaded ← loadWorld logger slot HS.empty HS.empty
                case loaded of
                    Left (_, e) → expectationFailure (T.unpack e)
                    Right (sd, _, _) → do
                        -- The crate's record round-trips exactly; the
                        -- two stamps stay independent.
                        lookupPortable crateId (sdPortableKnowledge sd)
                            `shouldBe` lookupPortable crateId twoRecords
                        (woAt <$> (lookupPortable crateId
                                       (sdPortableKnowledge sd) ⌦ prWeight))
                            `shouldBe` Just 650
                        (coAt <$> (lookupPortable crateId
                                       (sdPortableKnowledge sd) ⌦ prContents))
                            `shouldBe` Just 650

        it "STAGING scrubs a record whose crate is absent from the \
           \REPLACEMENT session and keeps one whose crate is live, and \
           \touches no live ref while doing it -- so a failure before \
           \publication leaves the outgoing session's memory exactly \
           \where it was" $
            withLiveCrateSession $ \env _ws → do
            logger ← readIORef (loggerRef env)
            setKnowledge env twoRecords
            let slot = "hspec_portable_knowledge_2512_stage"
                cleanup = removePathForcibly ("saves/" <> slot)
            cleanup
            (`finally` cleanup) $ do
                handleWorldSaveCommand env logger pageA slot
                    "2026-09-08T00:00:00.000000Z" [] [] Nothing
                matReg ← readIORef (materialRegistryRef env)
                loaded ← loadWorld logger slot HS.empty HS.empty
                case loaded of
                    Left (_, e) → expectationFailure (T.unpack e)
                    Right (sd, _, _) → do
                        -- Both records are in the save; only the crate
                        -- is actually on the page.
                        knownPortableIds (sdPortableKnowledge sd)
                            `shouldMatchList` [crateId, looseId]
                        stagedOrErr ← stageSession env logger sd matReg
                        case stagedOrErr of
                            Left e → expectationFailure
                                (T.unpack (renderStageError e))
                            Right staged → do
                                knownPortableIds (ssPortableKnowledge staged)
                                    `shouldBe` [crateId]
                                map spPageId (ssPages staged)
                                    `shouldBe` [pageA]
                                -- Nothing published yet: the live owner
                                -- is untouched, both records included.
                                knowledgeIn env `shouldReturn` twoRecords

        it "PUBLISHING replaces the live map wholesale rather than \
           \merging: a record the outgoing session held and the incoming \
           \one does not is gone afterwards" $
            withLiveCrateSession $ \env _ws → do
            logger ← readIORef (loggerRef env)
            setKnowledge env twoRecords
            let slot = "hspec_portable_knowledge_2512_publish"
                cleanup = removePathForcibly ("saves/" <> slot)
            cleanup
            (`finally` cleanup) $ do
                handleWorldSaveCommand env logger pageA slot
                    "2026-09-08T00:00:00.000000Z" [] [] Nothing
                -- A memory the SAVE does not contain, installed after
                -- the capture. If publish merged, it would survive.
                setKnowledge env
                    (observePortableWeight testItems 999
                         (loose { iiInstanceId = unlocatableId }) twoRecords)
                matReg ← readIORef (materialRegistryRef env)
                loaded ← loadWorld logger slot HS.empty HS.empty
                case loaded of
                    Left (_, e) → expectationFailure (T.unpack e)
                    Right (sd, _, _) → do
                        stagedOrErr ← stageSession env logger sd matReg
                        case stagedOrErr of
                            Left e → expectationFailure
                                (T.unpack (renderStageError e))
                            Right staged → do
                                publishStagedSession env logger 1 staged
                                k ← knowledgeIn env
                                knownPortableIds k `shouldBe` [crateId]
                                portableState unlocatableId k
                                    `shouldBe` NeverInspected
                                portableState looseId k
                                    `shouldBe` NeverInspected
                                portableState crateId k
                                    `shouldBe` KnownContents

        it "a load whose save carries NO portable knowledge CLEARS what \
           \the outgoing session remembered -- absence is the empty map, \
           \and a load replaces the session rather than adding to it" $
            withLiveCrateSession $ \env _ws → do
            logger ← readIORef (loggerRef env)
            -- Saved with nothing remembered.
            setKnowledge env emptyPortableKnowledge
            let slot = "hspec_portable_knowledge_2512_absent"
                cleanup = removePathForcibly ("saves/" <> slot)
            cleanup
            (`finally` cleanup) $ do
                handleWorldSaveCommand env logger pageA slot
                    "2026-09-08T00:00:00.000000Z" [] [] Nothing
                setKnowledge env twoRecords
                matReg ← readIORef (materialRegistryRef env)
                loaded ← loadWorld logger slot HS.empty HS.empty
                case loaded of
                    Left (_, e) → expectationFailure (T.unpack e)
                    Right (sd, _, _) → do
                        sdPortableKnowledge sd
                            `shouldBe` emptyPortableKnowledge
                        stagedOrErr ← stageSession env logger sd matReg
                        case stagedOrErr of
                            Left e → expectationFailure
                                (T.unpack (renderStageError e))
                            Right staged → do
                                publishStagedSession env logger 1 staged
                                knowledgeIn env
                                    `shouldReturn` emptyPortableKnowledge

    describe "non-vacuity of the session fixtures" $
        it "the crate really is the only live remembered instance -- the \
           \loose item is deliberately nowhere in the session, which is \
           \what makes the scrub cases above mean anything" $
            withLiveCrateSession $ \env ws → do
            ground ← readIORef (wsGroundItemsRef ws)
            um ← readIORef (unitManagerRef env)
            map (iiInstanceId ∘ giInst) (HM.elems (gisItems ground))
                `shouldBe` [crateId]
            HM.keys (umInstances um) `shouldBe` []
            knownPortableIds twoRecords `shouldMatchList` [crateId, looseId]
