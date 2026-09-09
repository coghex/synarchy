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
import Item.Types (ItemInstance(..))
import Test.Headless.Item.PortableKnowledge.Fixture
import Unit.Types (UnitManager(..))
import Item.Ground (GroundItem(..), GroundItems(..))
import World.Construct.Attempt (firstConstructAttemptId)
import World.Construct.Receipt (ConstructPayment(..), mkMaterialReceipt)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructStatus(..), ConstructTarget(..)
    , StructurePiece(..) )
import World.Load.Publish (publishStagedSession)
import World.Load.Stage (renderStageError, stageSession)
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Page.Types (WorldPageId(..))
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS2
import System.FilePath ((</>))
import World.Save.Component (componentKnownIds)
import World.Save.Component.Types (metadataComponentId, portableKnowledgeComponentId)
import World.Save.Envelope (decodeSessionEnvelope)
import World.Save.Envelope.Codec
    (DecodedEnvelope(..), decodeEnvelope, encodeEnvelope)
import World.Save.Envelope.Types
    ( ComponentDescriptor(..), EnvelopeManifest(..), defaultEnvelopeLimits )
import World.Save.Serialize (loadWorld)
import World.Save.Snapshot (SessionSnapshot(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import World.Save.Types (SaveData(..), SaveMetadata(..))
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

    describe "a load that does not complete, and one that carries \
             \nothing" $ do
        it "a PRE-PUBLICATION failure leaves the outgoing session's \
           \memory exactly where it was -- staging is the phase that \
           \can still fail, and it publishes nothing at all when it \
           \does" $ withLiveCrateSession $ \env _ws → do
            logger ← readIORef (loggerRef env)
            setKnowledge env twoRecords
            let slot = "hspec_portable_knowledge_2512_stagefail"
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
                        -- A save with no world pages: the FIRST branch
                        -- `stageSession` refuses on, and a real refusal
                        -- rather than a stubbed one.
                        stagedOrErr ← stageSession env logger
                            sd { sdWorlds = [] } matReg
                        case stagedOrErr of
                            Right _ → expectationFailure
                                "a page-less save staged successfully"
                            Left _  → pure ()
                        -- Nothing published, so nothing replaced: the
                        -- outgoing map is untouched, both records
                        -- included -- including the one the incoming
                        -- session would have scrubbed.
                        knowledgeIn env `shouldReturn` twoRecords

        it "publishing a REAL envelope with the portable-knowledge \
           \component REMOVED clears a manager that already holds \
           \knowledge -- absence is the empty map, applied through the \
           \live lifecycle rather than only through the codec" $
            withLiveCrateSession $ \env _ws → do
            logger ← readIORef (loggerRef env)
            setKnowledge env twoRecords
            let slot = "hspec_portable_knowledge_2512_stripped"
                cleanup = removePathForcibly ("saves/" <> slot)
            cleanup
            (`finally` cleanup) $ do
                handleWorldSaveCommand env logger pageA slot
                    "2026-09-08T00:00:00.000000Z" [] [] Nothing
                -- The bytes this save really wrote, with one component
                -- dropped -- the shape every pre-#2512 save on disk
                -- genuinely has.
                bytes ← BS.readFile ("saves" </> T.unpack slot
                                            </> "world.synworld")
                let stripped = withoutPortableKnowledge bytes
                case decodeSessionEnvelope HS.empty HS.empty stripped of
                    Left err → expectationFailure (T.unpack err)
                    Right (meta, snap, _, _) → do
                        -- Non-vacuity: the unstripped envelope really
                        -- did carry the records.
                        case decodeSessionEnvelope HS.empty HS.empty bytes of
                            Left err → expectationFailure (T.unpack err)
                            Right (_, full, _, _) →
                                knownPortableIds (snapPortableKnowledge full)
                                    `shouldMatchList` [crateId, looseId]
                        snapPortableKnowledge snap
                            `shouldBe` emptyPortableKnowledge
                        matReg ← readIORef (materialRegistryRef env)
                        let sd = snapshotToSaveData
                                (SaveRequestMeta (smName meta) "ts" False) snap
                        stagedOrErr ← stageSession env logger sd matReg
                        case stagedOrErr of
                            Left e → expectationFailure
                                (T.unpack (renderStageError e))
                            Right staged → do
                                -- The live map is populated right up to
                                -- the publish.
                                knowledgeIn env `shouldReturn` twoRecords
                                publishStagedSession env logger 1 staged
                                knowledgeIn env
                                    `shouldReturn` emptyPortableKnowledge

    describe "the scrub sees what the page actually publishes" $
        it "keeps the record of an item a staging REFUND minted: the \
           \live-id set is read off the page's ground ref AFTER \
           \reconciliation, not off the value written into it before" $
            withLiveCrateSession $ \env ws → do
            logger ← readIORef (loggerRef env)
            -- A paid structure designation whose pack has no registered
            -- art. The private engine's art catalogue is empty, so
            -- staging's revalidation resolves it as missing-art,
            -- self-clears it, and refunds the receipt onto this page's
            -- ground -- AFTER the decoded ground map has already been
            -- written into wsGroundItemsRef.
            writeIORef (wsConstructDesignationsRef ws) $ HM.singleton (3, 3)
                ConstructDesignation
                    { cdZ = 0
                    , cdTarget = CtStructure
                        (StructurePiece "no_such_pack" "floor" Nothing)
                    , cdStatus = CsPending
                    , cdProgress = 0
                    , cdAttempt = firstConstructAttemptId
                    , cdPayment = CpPaid (mkMaterialReceipt [("bandage", 1)])
                    }
            -- The refund draws from the save's own allocator, so the
            -- item it mints carries exactly this id -- and the session
            -- remembers a crate under it.
            let refundedId = 1000 ∷ Word64
            setKnowledge env
                (observePortableWeight testItems 600
                     (loose { iiInstanceId = refundedId })
                     emptyPortableKnowledge)
            let slot = "hspec_portable_knowledge_2512_refund"
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
                        sdNextItemInstanceId sd `shouldBe` refundedId
                        stagedOrErr ← stageSession env logger sd matReg
                        case stagedOrErr of
                            Left e → expectationFailure
                                (T.unpack (renderStageError e))
                            Right staged → case ssPages staged of
                                [sp] → do
                                    -- Non-vacuity: the refund really
                                    -- happened, and it really landed on
                                    -- the staged page under that id.
                                    ground ← readIORef
                                        (wsGroundItemsRef (spWorldState sp))
                                    map (iiInstanceId ∘ giInst)
                                        (HM.elems (gisItems ground))
                                        `shouldSatisfy` elem refundedId
                                    -- …so its memory is live knowledge,
                                    -- not a dangling record.
                                    knownPortableIds
                                        (ssPortableKnowledge staged)
                                        `shouldBe` [refundedId]
                                other → expectationFailure
                                    ("expected one staged page, got "
                                     <> show (length other))

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

-- | The same envelope with its @"portable-knowledge"@ component dropped
--   entirely — the manifest-level ABSENCE every save written before
--   #2512 really has, produced from real bytes rather than described.
withoutPortableKnowledge ∷ BS.ByteString → BS.ByteString
withoutPortableKnowledge bytes =
    case decodeEnvelope defaultEnvelopeLimits 1 allIds HS2.empty bytes of
        Left err → error ("withoutPortableKnowledge: decode: " <> show err)
        Right de →
            let specs = [ (cdId d, cdVersion d, cdRequired d, raw)
                        | d ← emComponents (deManifest de)
                        , cdId d ≢ portableKnowledgeComponentId
                        , Just raw ← [HM.lookup (cdId d) (dePayloads de)] ]
            in case encodeEnvelope defaultEnvelopeLimits 1 specs of
                Left err  → error ("withoutPortableKnowledge: encode: "
                                   <> show err)
                Right out → out
  where allIds = HS2.insert metadataComponentId componentKnownIds
