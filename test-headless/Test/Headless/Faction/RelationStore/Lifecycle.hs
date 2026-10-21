-- | The live owner's whole lifecycle (#2548 requirements 2, 4 and 7):
--   a session starts with no cause, keeps it across page transitions,
--   carries it through the REAL save boundary, has it REPLACED by a load
--   (never merged), and loses it at Exit to Menu.
--
--   Every step runs through the production owner —
--   'World.Thread.Command.Save.WriteWorld.handleWorldSaveCommand',
--   'World.Save.Serialize.loadWorld', 'World.Load.Stage.stageSession',
--   'World.Load.Publish.publishStagedSession',
--   'World.Thread.Command.Basic.handleWorldDestroyAllCommand' — against
--   the live 'Engine.Core.State.relationCauseStoreRef', rather than
--   through a re-implementation of what each is believed to do. A store
--   wired into the codec but never read off the engine would satisfy a
--   codec test and fail every one of these. Codec equality alone proves
--   nothing about publication.
--
--   The engine here is PRIVATE ('initializeEngineHeadlessQuiet'), so
--   publishing a replacement session is safe: nothing else shares it.
--   No Vulkan and no port: it starts no thread and needs no probe slot.
--   @engine.saveWorld@ itself is not used — its owner handshake needs
--   threads this harness never starts — so the entry point is the
--   world-thread command handler it queues.
module Test.Headless.Faction.RelationStore.Lifecycle (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Control.Exception (finally)
import Data.IORef (readIORef, writeIORef)
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State
import Test.Headless.Faction.RelationStore.Fixture
import Test.Headless.Faction.RelationStore.Persistence (withoutComponent)
import Test.Headless.Harness.Isolation
    (isInsideIsolatedResourceRoot, withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Unit.Faction.RelationStore
import World.Load.Publish (publishStagedSession)
import World.Load.Stage (renderStageError, stageSession)
import World.Load.Types (StagedSession(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Envelope (decodeSessionEnvelope)
import World.Save.Component.Types (relationCausesComponentId)
import World.Save.Serialize (loadWorld)
import World.Save.Snapshot (SessionSnapshot(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import World.Save.Types (SaveData(..), SaveMetadata(..))
import World.State.Types
import World.Thread.Command.Basic
    (handleWorldDestroyAllCommand, handleWorldDestroyCommand)
import World.Thread.Command.Init (handleWorldInitCommand)
import World.Thread.Command.Save.WriteWorld (handleWorldSaveCommand)
import World.Thread.Command.UI (handleWorldHideCommand, handleWorldShowCommand)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)

pageA, pageB ∷ WorldPageId
pageA = WorldPageId "relation_store_a"
pageB = WorldPageId "relation_store_b"

storeOf ∷ EngineEnv → IO RelationCauseStore
storeOf env = readIORef (relationCauseStoreRef env)

setStore ∷ EngineEnv → RelationCauseStore → IO ()
setStore env = writeIORef (relationCauseStoreRef env)

initEnv ∷ IO EngineEnv
initEnv = do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    pure env

-- | A real (cheap, w8) page registered on a private engine: the shape a
--   genuine session has, saveable through the production handler.
withSession ∷ (EngineEnv → IO α) → IO α
withSession act = do
    env ← initEnv
    logger ← readIORef (loggerRef env)
    handleWorldInitCommand env logger pageA 42 8 3 Nothing
    act env

-- | Save the live session into @slot@ through the production handler,
--   run @body@ with the slot's directory, and remove it afterwards.
withSavedSlot ∷ EngineEnv → Text → (FilePath → IO α) → IO α
withSavedSlot env slot body = do
    logger ← readIORef (loggerRef env)
    let dir = "saves/" <> T.unpack slot
        cleanup = removePathForcibly dir
    cleanup
    (`finally` cleanup) $ do
        handleWorldSaveCommand env logger pageA slot
            "2026-10-21T00:00:00.000000Z" [] [] Nothing
        body dir

-- | Load @slot@ and stage it against @env@, publishing nothing.
loadAndStage ∷ EngineEnv → Text → IO (SaveData, StagedSession)
loadAndStage env slot = do
    logger ← readIORef (loggerRef env)
    matReg ← readIORef (materialRegistryRef env)
    loaded ← loadWorld logger slot HS.empty HS.empty
    case loaded of
        Left (_, e) → expectationFailure (T.unpack e) >> error "unreachable"
        Right (sd, _, _) → do
            staged ← stageSession env logger sd matReg
            case staged of
                Left e → expectationFailure (T.unpack (renderStageError e))
                            >> error "unreachable"
                Right ss → pure (sd, ss)

spec ∷ Spec
spec = around_ withIsolatedResourceRoot $ do
    -- The guard that keeps that boundary from being quietly unwired:
    -- every other assertion here passes from the checkout too.
    it "runs inside the scratch resource root, never the checkout" $
        isInsideIsolatedResourceRoot `shouldReturn` True

    describe "the live owner" $ do
        it "starts EMPTY on a fresh engine" $ do
            env ← initEnv
            storeOf env `shouldReturn` emptyRelationCauseStore

        it "SURVIVES ordinary page transitions — showing, hiding and \
           \destroying ONE page are not a session boundary, and a cause \
           \relates tags, which no page owns" $ do
            env ← initEnv
            logger ← readIORef (loggerRef env)
            handleWorldInitCommand env logger pageA 42 8 3 Nothing
            handleWorldInitCommand env logger pageB 43 8 3 Nothing
            setStore env sampleStore
            handleWorldShowCommand (toWorldSimCapability env) logger pageA
            storeOf env `shouldReturn` sampleStore
            handleWorldHideCommand (toWorldSimCapability env) logger pageA
            storeOf env `shouldReturn` sampleStore
            handleWorldDestroyCommand env logger pageB
            -- Non-vacuity: the page really went away.
            (map fst ∘ wmWorlds) <$> readIORef (worldManagerRef env)
                >>= (`shouldSatisfy` notElem pageB)
            storeOf env `shouldReturn` sampleStore

        it "is CLEARED at Exit to Menu, which empties the page set — the \
           \next session's tags are different tags, and a surviving cause \
           \would act on any roster that reuses them" $
            withSession $ \env → do
            logger ← readIORef (loggerRef env)
            setStore env sampleStore
            handleWorldDestroyAllCommand env logger
            (map fst ∘ wmWorlds) <$> readIORef (worldManagerRef env)
                `shouldReturn` []
            storeOf env `shouldReturn` emptyRelationCauseStore
            -- …and a NEW session in the same process starts from it.
            handleWorldInitCommand env logger pageB 44 8 3 Nothing
            storeOf env `shouldReturn` emptyRelationCauseStore

    describe "through the real save/load boundary" $ do
        it "the production save captures what the ENGINE REF holds, and \
           \the load bridge carries every cause back exactly" $
            withSession $ \env → do
            setStore env sampleStore
            withSavedSlot env "hspec_relation_store_2548_capture" $ \_ → do
                logger ← readIORef (loggerRef env)
                loaded ← loadWorld logger "hspec_relation_store_2548_capture"
                              HS.empty HS.empty
                case loaded of
                    Left (_, e) → expectationFailure (T.unpack e)
                    Right (sd, _, _) → do
                        sdRelationCauses sd `shouldBe` sampleStore
                        -- Non-vacuity: a different live value saves
                        -- a different payload.
                        relationCauseStoreNull (sdRelationCauses sd)
                            `shouldBe` False

        it "STAGING touches no live ref — a failure before publication \
           \leaves the outgoing session's store exactly where it was" $
            withSession $ \env → do
            setStore env sampleStore
            withSavedSlot env "hspec_relation_store_2548_stage" $ \_ → do
                setStore env otherStore
                (sd, staged) ← loadAndStage env
                                   "hspec_relation_store_2548_stage"
                ssRelationCauses staged `shouldBe` sampleStore
                sdRelationCauses sd `shouldBe` sampleStore
                -- Staged, not published: the live store is unchanged.
                storeOf env `shouldReturn` otherStore

        it "a load that fails BEFORE publication preserves the live \
           \store — staging is the phase that can still fail, and it \
           \publishes nothing when it does" $
            withSession $ \env → do
            setStore env sampleStore
            withSavedSlot env "hspec_relation_store_2548_stagefail" $ \_ → do
                setStore env otherStore
                logger ← readIORef (loggerRef env)
                matReg ← readIORef (materialRegistryRef env)
                loaded ← loadWorld logger "hspec_relation_store_2548_stagefail"
                              HS.empty HS.empty
                case loaded of
                    Left (_, e) → expectationFailure (T.unpack e)
                    Right (sd, _, _) → do
                        -- A save with no world pages: the first branch
                        -- 'stageSession' refuses on, a real refusal.
                        staged ← stageSession env logger
                                     sd { sdWorlds = [] } matReg
                        case staged of
                            Right _ → expectationFailure
                                "a page-less save staged successfully"
                            Left _ → pure ()
                        storeOf env `shouldReturn` otherStore

        it "PUBLISHING a successful load over a DIFFERENT non-empty live \
           \store replaces it EXACTLY — a cause only the outgoing session \
           \held is gone afterwards, so nothing is merged" $
            withSession $ \env → do
            setStore env sampleStore
            withSavedSlot env "hspec_relation_store_2548_publish" $ \_ → do
                -- A different, non-empty store installed AFTER the
                -- capture. If publish merged, it would survive.
                setStore env otherStore
                (_, staged) ← loadAndStage env
                                  "hspec_relation_store_2548_publish"
                logger ← readIORef (loggerRef env)
                storeOf env `shouldReturn` otherStore
                publishStagedSession env logger 1 staged
                storeOf env `shouldReturn` sampleStore
                -- No leftover of the outgoing store, by identity.
                [ c | c ← relationCauseStoreOverlay sampleStore
                    , c `elem` relationCauseStoreOverlay otherStore ]
                    `shouldBe` []

        it "publishing a REAL envelope with the component REMOVED clears \
           \a live store that holds causes — absence is the empty store, \
           \applied through the live lifecycle and not only the codec" $
            withSession $ \env → do
            setStore env sampleStore
            withSavedSlot env "hspec_relation_store_2548_stripped" $ \dir → do
                bytes ← BS.readFile (dir </> "world.synworld")
                let stripped = withoutComponent relationCausesComponentId bytes
                case ( decodeSessionEnvelope HS.empty HS.empty stripped
                     , decodeSessionEnvelope HS.empty HS.empty bytes ) of
                    (Right (meta, snap, _, _), Right (_, full, _, _)) → do
                        -- Non-vacuity: the unstripped envelope really
                        -- carried the causes.
                        snapRelationCauses full `shouldBe` sampleStore
                        snapRelationCauses snap
                            `shouldBe` emptyRelationCauseStore
                        logger ← readIORef (loggerRef env)
                        matReg ← readIORef (materialRegistryRef env)
                        setStore env otherStore
                        let sd = snapshotToSaveData
                                (SaveRequestMeta (smName meta) "ts" False) snap
                        staged ← stageSession env logger sd matReg
                        case staged of
                            Left e → expectationFailure
                                (T.unpack (renderStageError e))
                            Right ss → do
                                storeOf env `shouldReturn` otherStore
                                publishStagedSession env logger 1 ss
                                storeOf env
                                    `shouldReturn` emptyRelationCauseStore
                    _ → expectationFailure "an envelope did not decode"
