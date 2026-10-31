{-# LANGUAGE OverloadedStrings #-}
-- | Persisted flora CONDITION records (#2549, EFM-5).
--
--   A death can only be durable as a sparse page-scoped overlay joined
--   back onto the occurrence by its 'FloraInstanceId': wild flora
--   regenerates with its chunk and row crops replay from the edit log.
--   These examples pin that overlay's store operations, its
--   @world-activity@ v7 wire shape, the validation each refusal goes
--   through, and — through the REAL production capture, envelope and
--   staging paths — that a record survives eviction, regeneration and a
--   save/load with its siblings untouched.
--
--   Selected by @--match "flora condition persistence"@.
module Test.Headless.World.FloraCondition (spec, engineSpec) where

import UPrelude
import Test.Hspec
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Log
    (LoggerState, LogConfig(..), LogBackend(..), initLogger, defaultLogConfig)
import Engine.Core.State (EngineEnv(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Item.Knowledge (emptyPortableKnowledge)
import Structure.Palette (emptyTexPalette)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)
import Building.Types (emptyBuildingManager)
import Unit.Types (emptyUnitManager)
import Unit.Sim.Types (emptyUnitThreadState)
import Unit.Faction.RelationStore (emptyRelationCauseStore)

import Item.Ground (emptyGroundItems)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Chunk.Types
    (ChunkCoord, ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Edit.Types (WorldEdit(..), appendEdit, emptyWorldEdits)
import World.Flora.Condition
import World.Flora.Identity
import World.Flora.Reference (FloraRef(..))
import World.Flora.Types
import World.Generate.Coordinates (canonicalTile, globalToChunk, tileAliasStep)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Load.Stage (renderStageError, stageSession)
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Page
import World.Save.Component.Types
    ( ComponentCodec(..), renderComponentError, worldActivityComponentId )
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Integrity (IntegrityError(..), sessionIntegrityErrors)
import World.Save.Snapshot
import World.Save.Snapshot.Adapter
    (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.Save.Types
    ( MissingFloraRef(..), WorldPageSave(..), SaveData(..)
    , missingFloraReferences, renderMissingFloraRef )
import World.State.Types
import World.Tile.Types (WorldTileData(..))
import Structure.Types (emptyChunkStructures)
import World.Thread.Command.Save.WriteWorld (capturePageSnapshot)

-- * Fixture geography

-- | A wrapping page, so a seam alias can be told apart from the
--   canonical name of its tile.
wrapSize ∷ Int
wrapSize = 64

pageA, pageB, arenaPage ∷ WorldPageId
pageA     = WorldPageId "flora_condition_a"
pageB     = WorldPageId "flora_condition_b"
arenaPage = WorldPageId "flora_condition_arena"

-- | Both fixtures' tiles lie well inside the arena sentinel's identity
--   range, so the canonical name of each is itself.
homeTile, otherTile ∷ (Int, Int)
homeTile  = (37, 55)
otherTile = (38, 55)

arenaParams ∷ WorldGenParams
arenaParams = defaultWorldGenParams { wgpSeed = 0, wgpWorldSize = 100000 }

wrapParams ∷ WorldGenParams
wrapParams = defaultWorldGenParams { wgpWorldSize = wrapSize }

-- * Species

oakId, berryId, fernId ∷ FloraId
oakId   = FloraId 1
berryId = FloraId 2
fernId  = FloraId 3

probeCatalog ∷ FloraCatalog
probeCatalog = emptyFloraCatalog
    { fcSpecies = HM.fromList
        [ (1, named "probe_oak"), (2, named "probe_berry")
        , (3, named "probe_fern") ]
    , fcNextId = 4 }
  where named n = newFloraSpecies n (TextureHandle 0)

speciesName ∷ FloraId → Text
speciesName fid = maybe "" fsName (lookupSpecies fid probeCatalog)

-- | The id a generated plant of this species, at this canonical tile
--   and ordinal on the arena page, carries — through the PRODUCTION
--   function, so the fixture cannot drift from the engine's answer.
generatedAt ∷ (Int, Int) → FloraId → Int → FloraInstanceId
generatedAt (gx, gy) fid ordinal =
    let (cgx, cgy) = canonicalTile 100000 gx gy
    in generatedFloraInstanceId (unWorldPageId arenaPage) cgx cgy
           (speciesName fid) ordinal

-- | Three co-tenants on ONE tile, as #1854's identity exists to tell apart.
oakIid, berryIid, fernIid ∷ FloraInstanceId
oakIid   = generatedAt homeTile oakId 0
berryIid = generatedAt homeTile berryId 0
fernIid  = generatedAt homeTile fernId 0

-- | The occurrences a chunk's regeneration yields for the co-tenant tile:
--   built from the SAME provenance every time (the production id
--   function), never copied from the previous residency, so a second call
--   after an eviction is a genuine regeneration.
coTenantInstances ∷ [FloraInstance]
coTenantInstances =
    [ FloraInstance
        { fiSpecies = fid
        , fiTileX = fromIntegral lx, fiTileY = fromIntegral ly
        , fiOffU = 0, fiOffV = 0, fiZ = 4
        , fiAge = 1, fiHealth = 1, fiVariant = 0, fiBaseWidth = 8
        , fiInstanceId = generatedAt homeTile fid 0
        , fiChopDesignated = False
        , fiContext = ContextWild }
    | fid ← [oakId, berryId, fernId]
    , let (_, (lx, ly)) = uncurry globalToChunk homeTile ]

-- | A tile store holding exactly one chunk carrying these occurrences.
chunkStore ∷ [FloraInstance] → WorldTileData
chunkStore insts =
    let area = chunkSize * chunkSize
        col  = ColumnTiles { ctStartZ = 0, ctMats = VU.replicate 20 1
                           , ctSlopes = VU.replicate 20 0
                           , ctVeg = VU.replicate 20 0 }
        coord = chunkOf homeTile
    in WorldTileData
        { wtdChunks = HM.singleton coord LoadedChunk
            { lcCoord = coord, lcTiles = V.replicate area col
            , lcSurfaceMap = VU.replicate area 4
            , lcTerrainSurfaceMap = VU.replicate area 4
            , lcFluidMap = V.replicate area Nothing
            , lcIceMap = emptyIceMap
            , lcFlora = FloraChunkData insts
            , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
            , lcMagma = Nothing, lcStructures = emptyChunkStructures }
        , wtdMaxChunks = 200, wtdBounds = Nothing }

-- | The ids of the occurrences currently resident, per the chunk store.
residentIds ∷ WorldState → IO [FloraInstanceId]
residentIds ws = do
    td ← readIORef (wsTilesRef ws)
    pure [ fiInstanceId fi
         | lc ← HM.elems (wtdChunks td)
         , fi ← fcdInstances (lcFlora lc) ]

-- * Records

-- | A valid dead record: a mature, flowering-stage fire death whose
--   retention window ends at day 180.
deadRecord
    ∷ FloraId → (Int, Int) → FloraContext → FloraConditionRecord
deadRecord fid tile ctx = FloraConditionRecord
    { fcrSpecies   = fid
    , fcrTile      = tile
    , fcrCondition = ConditionDead
    , fcrCause     = CauseFire
    , fcrBeganDay  = 120
    , fcrContext   = ctx
    , fcrPhase     = Just PhaseMatured
    , fcrStage     = Just CycleFlowering
    , fcrRetention = RetentionTransient 180 $ case ctx of
        ContextWild       → RetainReseed
        ContextCultivated → RetainAwaitReplanting
    }

savedOf ∷ FloraConditionRecord → SavedFloraConditionRecord
savedOf r = r { fcrSpecies = FloraByName (speciesName (fcrSpecies r)) }

-- | The independent oracle for which (context, retention) pairs are
--   legal — spelled out from the design record (D-13, D-19) rather than
--   derived from the implementation under test.
legalPair ∷ FloraContext → ConditionRetention → Bool
legalPair _ RetentionPersistent = True
legalPair ContextWild (RetentionTransient _ s) =
    s `elem` [RetainReseed, RetainAbsent]
legalPair ContextCultivated (RetentionTransient _ s) =
    s ≡ RetainAwaitReplanting

-- * Pure examples

spec ∷ Spec
spec = describe "flora condition persistence" $ do

  describe "the store" $ do

    it "marks an occurrence dead and finds it by id" $ do
        let r = deadRecord oakId homeTile ContextWild
        case markFloraDead oakIid r emptyFloraConditions of
            Left e → expectationFailure (show e)
            Right store → do
                lookupFloraCondition oakIid store `shouldBe` Just r
                lookupFloraCondition berryIid store `shouldBe` Nothing

    it "refuses to mark an occurrence that already has a record, \
       \preserving the original EXACTLY, whatever the second record says" $
        forM_ [minBound .. maxBound ∷ FloraDeathCause] $ \cause → do
            let first  = deadRecord oakId homeTile ContextWild
                second = first { fcrCause = cause, fcrBeganDay = 999
                               , fcrRetention = RetentionPersistent }
                store0 = HM.singleton oakIid first
            markFloraDead oakIid second store0
                `shouldBe` Left (AlreadyRecorded oakIid)
            -- The refusal returns no new store; the one the caller holds
            -- is the one it started with.
            lookupFloraCondition oakIid store0 `shouldBe` Just first

    it "clears exactly one record and leaves its siblings" $ do
        let recs = [ (oakIid,   deadRecord oakId   homeTile ContextWild)
                   , (berryIid, deadRecord berryId homeTile ContextWild)
                   , (fernIid,  deadRecord fernId  homeTile ContextWild) ]
            store = HM.fromList recs
            cleared = clearFloraCondition berryIid store
        HM.keys cleared `shouldMatchList` [oakIid, fernIid]
        lookupFloraCondition berryIid cleared `shouldBe` Nothing
        forM_ [oakIid, fernIid] $ \k →
            lookupFloraCondition k cleared `shouldBe` lookupFloraCondition k store
        -- Clearing an occurrence with no record is a no-op.
        clearFloraCondition (generatedAt otherTile oakId 0) store
            `shouldBe` store

    it "accepts a persistent outcome, an absent annual stage, an absent \
       \life phase and a zero-length retention window" $ do
        let base = deadRecord oakId homeTile ContextWild
        forM_ [ base { fcrRetention = RetentionPersistent }
              , base { fcrStage = Nothing }
              , base { fcrPhase = Nothing }
              , base { fcrRetention = RetentionTransient (fcrBeganDay base)
                                                         RetainAbsent } ] $ \r →
            conditionRecordProblems oakIid r `shouldBe` []

    it "accepts exactly the legal (context, retention) pairs, over the \
       \whole outcome space" $
        forM_ [minBound .. maxBound ∷ FloraContext] $ \ctx →
          forM_ ( RetentionPersistent
                : [ RetentionTransient d s
                  | d ← [100, 120, 121], s ← [minBound .. maxBound] ] ) $
            \retention → do
              let r = (deadRecord oakId homeTile ctx)
                          { fcrRetention = retention, fcrBeganDay = 120 }
                  expiryOk = case retention of
                      RetentionTransient d _ → d ≥ 120
                      RetentionPersistent    → True
                  problems = conditionRecordProblems oakIid r
              (null problems) `shouldBe` (legalPair ctx retention ∧ expiryOk)

    it "refuses each invalid record and names why" $ do
        let ok = deadRecord oakId homeTile ContextWild
        conditionRecordProblems floraInstanceIdNone ok `shouldBe` [ReservedKey]
        conditionRecordProblems oakIid (ok { fcrCondition = ConditionAlive })
            `shouldBe` [NotAStoredCondition]
        -- The legacy dead token is not a phase at death (contract §1.1).
        conditionRecordProblems oakIid (ok { fcrPhase = Just PhaseDead })
            `shouldBe` [PhaseDeadAtDeath]
        conditionRecordProblems oakIid
            (ok { fcrRetention = RetentionTransient 119 RetainReseed })
            `shouldBe` [ExpiryBeforeBegin 119 120]
        conditionRecordProblems oakIid
            (ok { fcrRetention = RetentionTransient 150 RetainAwaitReplanting })
            `shouldBe` [WildAwaitsReplanting]
        conditionRecordProblems oakIid
            ((deadRecord berryId homeTile ContextCultivated)
                 { fcrRetention = RetentionTransient 150 RetainReseed })
            `shouldBe` [CultivatedNamesWildSuccessor RetainReseed]
        -- And the store applies the same judgement: nothing is inserted.
        markFloraDead floraInstanceIdNone ok emptyFloraConditions
            `shouldBe` Left (InvalidRecord [ReservedKey])

  describe "the wire vocabulary" $ do

    it "round-trips every value of every vocabulary it carries" $ do
        roundTrip [minBound .. maxBound ∷ FloraCondition]
        roundTrip [minBound .. maxBound ∷ FloraDeathCause]
        roundTrip [minBound .. maxBound ∷ FloraContext]
        roundTrip [minBound .. maxBound ∷ LifePhaseTag]
        roundTrip [minBound .. maxBound ∷ AnnualStageTag]
        roundTrip [minBound .. maxBound ∷ RetentionSuccessor]

    it "accepts exactly the tags below the vocabulary's size, for every \
       \possible byte" $ do
        let accepted ∷ [VocabTag]
            accepted = [ t | t ← [0 .. 255]
                           , isJust (fromVocabTag t ∷ Maybe AnnualStageTag) ]
        accepted `shouldBe` [0 .. fromIntegral (fromEnum (maxBound ∷ AnnualStageTag))]
        [ t | t ← [0 .. 255 ∷ VocabTag]
            , isJust (fromVocabTag t ∷ Maybe RetentionSuccessor) ]
            `shouldBe` [0, 1, 2]

    it "decodes a wire record back to the record it was made from" $ do
        forM_ [ savedOf (deadRecord oakId homeTile ContextWild)
              , savedOf (deadRecord berryId homeTile ContextCultivated)
              , (savedOf (deadRecord fernId homeTile ContextWild))
                    { fcrStage = Nothing, fcrRetention = RetentionPersistent }
              , (savedOf (deadRecord oakId homeTile ContextWild))
                    { fcrPhase = Nothing } ] $
            \r → fromFloraConditionDTO (toFloraConditionDTO r) `shouldBe` Right r

  describe "world-activity versions" $ do

    it "accepts v1 through v8" $
        ccInputVers worldActivityCodec `shouldBe` [1 .. 8]

    it "migrates a v6 payload to an EMPTY condition map, preserving \
       \every other field" $ do
        let slice = (emptyActivityV6 pageA)
                { pad6FloraHarvests = HM.singleton oakIid 42.5
                , pad6ConstructNextAttempt = firstConstructAttemptId }
        case ccDecode worldActivityCodec 6
                 (S.encode (WorldActivityDTOv6 [slice])) of
            Left e → expectationFailure (show e)
            Right (WorldActivityDTO [s]) → do
                padFloraConditions s `shouldBe` HM.empty
                padFloraHarvests s `shouldBe` HM.singleton oakIid 42.5
                padPageId s `shouldBe` pageA
            Right _ → expectationFailure "expected one page slice"

    it "migrates a v5 payload through v6 to an empty condition map" $
        case ccDecode worldActivityCodec 5
                 (S.encode (WorldActivityDTOv5 [emptyActivityV5 pageA])) of
            Left e → expectationFailure (show e)
            Right (WorldActivityDTO [s]) →
                (padFloraConditions s, padPageId s)
                    `shouldBe` (HM.empty, pageA)
            Right _ → expectationFailure "expected one page slice"

    it "round-trips a populated v7 payload through the real codec" $ do
        let dto = WorldActivityDTO
                [ (emptyActivityV7 pageA)
                    { padFloraConditions = HM.fromList
                        [ (oakIid, toFloraConditionDTO
                            (savedOf (deadRecord oakId homeTile ContextWild)))
                        , (plantedFloraInstanceId 3, toFloraConditionDTO
                            (savedOf (deadRecord berryId otherTile
                                         ContextCultivated))) ] } ]
        case ccDecode worldActivityCodec 8 (S.encode dto) of
            Left e → expectationFailure (show e)
            Right back → do
                S.encode back `shouldBe` S.encode dto
                ccValidate worldActivityCodec back `shouldBe` []

  describe "component validation" $ do

    -- Each case is encoded at the CURRENT version and driven through the
    -- real decode and validate path, so a refusal is the one a load
    -- would produce, and it must name the page and the record's key.
    let refusalFor ∷ WorldPageId → FloraInstanceId → FloraConditionDTO
                   → IO [Text]
        refusalFor pid key dto = do
            let payload = WorldActivityDTO
                    [ (emptyActivityV7 pid)
                        { padFloraConditions = HM.singleton key dto } ]
            case ccDecode worldActivityCodec 8 (S.encode payload) of
                Left e → expectationFailure (show e) ≫ pure []
                Right decoded →
                    pure (map renderComponentError
                              (ccValidate worldActivityCodec decoded))
        wireOf = toFloraConditionDTO ∘ savedOf
        base = deadRecord oakId homeTile ContextWild
        namesPageAndKey pid key msgs = do
            msgs `shouldSatisfy` (not ∘ null)
            forM_ msgs $ \m → do
                m `shouldSatisfy` T.isInfixOf (unWorldPageId pid)
                m `shouldSatisfy`
                    T.isInfixOf (tshow (floraInstanceIdToLua key))

    it "refuses the reserved non-identity key, naming the page" $ do
        msgs ← refusalFor pageA floraInstanceIdNone (wireOf base)
        msgs `shouldSatisfy` (not ∘ null)
        msgs `shouldSatisfy` all (T.isInfixOf (unWorldPageId pageA))
        msgs `shouldSatisfy` any (T.isInfixOf "reserved")

    it "refuses an expiry day before the begin day" $ do
        msgs ← refusalFor pageA oakIid
            (wireOf base { fcrRetention = RetentionTransient 119 RetainReseed })
        namesPageAndKey pageA oakIid msgs
        msgs `shouldSatisfy` any (T.isInfixOf "precedes")

    it "refuses a stage outside the vocabulary, naming the page and key, \
       \rather than failing the whole component decode" $ do
        msgs ← refusalFor pageB oakIid
            ((wireOf base) { fcdStage = Just 99 })
        namesPageAndKey pageB oakIid msgs
        msgs `shouldSatisfy` any (T.isInfixOf "annual stage")

    it "refuses a tag outside each other vocabulary, field by field" $
        forM_ [ ("condition", \d → d { fcdCondition = 77 })
              , ("cause",     \d → d { fcdCause = 77 })
              , ("context",   \d → d { fcdContext = 77 })
              , ("life phase", \d → d { fcdPhase = Just 77 })
              , ("retention successor"
                , \d → d { fcdRetention = RetentionTransientD 150 77 }) ] $
            \(field, corrupt) → do
                msgs ← refusalFor pageA oakIid (corrupt (wireOf base))
                namesPageAndKey pageA oakIid msgs
                msgs `shouldSatisfy` any (T.isInfixOf field)

    it "refuses a stored condition that is not dead, and a context \
       \that contradicts the successor" $ do
        alive ← refusalFor pageA oakIid
                    (wireOf base { fcrCondition = ConditionAlive })
        namesPageAndKey pageA oakIid alive
        deadPhase ← refusalFor pageA oakIid
                    (wireOf base { fcrPhase = Just PhaseDead })
        namesPageAndKey pageA oakIid deadPhase
        wildAwait ← refusalFor pageA oakIid
            (wireOf base { fcrRetention = RetentionTransient 150
                                              RetainAwaitReplanting })
        namesPageAndKey pageA oakIid wildAwait
        cultivatedReseed ← refusalFor pageA berryIid
            ((wireOf (deadRecord berryId homeTile ContextCultivated))
                { fcdRetention = RetentionTransientD 150
                                   (toVocabTag RetainReseed) })
        namesPageAndKey pageA berryIid cultivatedReseed

    it "accepts the permitted shapes: equal begin and expiry days, a \
       \persistent outcome, and no annual stage" $
        forM_ [ base { fcrRetention = RetentionTransient 120 RetainAbsent }
              , base { fcrRetention = RetentionPersistent }
              , base { fcrStage = Nothing } ] $ \r →
            refusalFor pageA oakIid (wireOf r) `shouldReturn` []

  describe "page ownership and planting provenance" $ do

    -- Planted ids do not encode a page or tile and repeat across pages,
    -- so these run over assembled snapshots, where each page's own
    -- frame and edit log are both present.
    let -- A SNAPSHOT's planting is already named (#2243).
        planting tile fid iid = WePlaceFloraRef (fst tile) (snd tile)
                                    (FloraByName (speciesName fid)) 10 1.0 iid
        pageWith pid params tile fid iid recs =
            (blankPageSnapshot pid params)
                { pgsEdits = appendEdit (chunkOf tile)
                                 (planting tile fid iid) emptyWorldEdits
                , pgsFloraConditions = HM.map savedOf recs }
        sessionOf pages = SessionSnapshot
            { snapGameTime = 0, snapTexPalette = emptyTexPalette
            , snapNextItemId = 1, snapNextBuildingId = 1, snapNextUnitId = 1
            , snapActivePage = pageA
            , snapVisiblePages = map fst pages
            , snapLiveCamera = LiveCameraSnapshot Nothing 0 0 1 FaceSouth
            , snapPages = HM.fromList pages
            , snapItemTags = mempty, snapItemRefs = mempty
            , snapPortableKnowledge = emptyPortableKnowledge
            , snapRelationCauses = emptyRelationCauseStore }
        problems snap = [ (ieCode e, ieMessage e) | e ← sessionIntegrityErrors snap
                        , ieComponent e ≡ worldActivityComponentId ]
        planted1 = plantedFloraInstanceId 1
        recAt tile = deadRecord berryId tile ContextCultivated
    it "isolates two pages that carry the SAME planted counter at \
       \different tiles" $ do
        let a = pageWith pageA wrapParams homeTile berryId planted1
                    (HM.singleton planted1 (recAt homeTile))
            b = pageWith pageB wrapParams otherTile berryId planted1
                    (HM.singleton planted1 (recAt otherTile))
        problems (sessionOf [(pageA, a), (pageB, b)]) `shouldBe` []

    it "refuses a planted record that contradicts ITS page's planting, \
       \naming that page only" $ do
        let a = pageWith pageA wrapParams homeTile berryId planted1
                    (HM.singleton planted1 (recAt homeTile))
            -- page B's planting is at otherTile, its record at homeTile.
            b = pageWith pageB wrapParams otherTile berryId planted1
                    (HM.singleton planted1 (recAt homeTile))
            errs = problems (sessionOf [(pageA, a), (pageB, b)])
        map fst errs `shouldBe` ["planted-tile-mismatch"]
        forM_ errs $ \(_, m) → do
            m `shouldSatisfy` T.isInfixOf (unWorldPageId pageB)
            m `shouldSatisfy` (not ∘ T.isInfixOf (unWorldPageId pageA))

    it "refuses a planted record naming a different species than its \
       \planting" $ do
        let a = pageWith pageA wrapParams homeTile berryId planted1
                    (HM.singleton planted1
                        ((recAt homeTile) { fcrSpecies = oakId }))
        map fst (problems (sessionOf [(pageA, a)]))
            `shouldBe` ["planted-species-mismatch"]

    it "refuses a tile that is a seam ALIAS rather than the page's \
       \canonical name" $ do
        let (gx, gy) = homeTile
            step = tileAliasStep wrapSize
            alias = (gx + step, gy - step)
        canonicalTile wrapSize (fst alias) (snd alias) `shouldBe` homeTile
        let a = pageWith pageA wrapParams homeTile berryId planted1
                    (HM.singleton planted1 (recAt alias))
            errs = problems (sessionOf [(pageA, a)])
        map fst errs `shouldSatisfy` elem "tile-not-owned"
        forM_ errs $ \(_, m) →
            m `shouldSatisfy` T.isInfixOf (unWorldPageId pageA)

    it "judges tiles in an EXPANDABLE arena by the page's own authority: \
       \a tile far beyond the sentinel's wrap threshold is its own \
       \canonical name, for the record and for its planting" $ do
        let far = (800000, 0)
            iid = plantedFloraInstanceId 1
            -- the sentinel world size would wrap this to (0, 800000)
            a = pageWith pageA arenaParams far berryId iid
                    (HM.singleton iid (recAt far))
        canonicalTile 100000 (fst far) (snd far) `shouldNotBe` far
        problems (sessionOf [(pageA, a)]) `shouldBe` []

    it "tolerates a record whose planting this page's log does not \
       \hold, and a wild record whose chunk is not resident" $ do
        let wild = deadRecord oakId homeTile ContextWild
            a = (blankPageSnapshot pageA wrapParams)
                { pgsFloraConditions = HM.fromList
                    [ (planted1, savedOf (recAt homeTile))   -- no planting
                    , (oakIid, savedOf wild) ] }              -- no chunk
        problems (sessionOf [(pageA, a)]) `shouldBe` []

  describe "species validation" $ do

    it "reports a record naming a species the build lacks, for an \
       \UNLOADED wild occurrence, naming the page, tile and instance" $ do
        let lost = (savedOf (deadRecord oakId homeTile ContextWild))
                       { fcrSpecies = FloraByName "gone_species" }
            snap = (sessionWithPage
                      (blankPageSnapshot arenaPage arenaParams)
                          { pgsFloraConditions = HM.singleton oakIid lost })
            saved = snapshotToSaveData
                (SaveRequestMeta "c" "ts" False) snap
            missing = missingFloraReferences probeCatalog
                [ (wpsPageId w, w) | w ← sdWorlds saved ]
        map mfrSpecies missing `shouldBe` [FloraByName "gone_species"]
        map mfrCoord missing `shouldBe` [homeTile]
        let rendered = map renderMissingFloraRef missing
        rendered `shouldSatisfy` all (T.isInfixOf (unWorldPageId arenaPage))
        rendered `shouldSatisfy`
            all (T.isInfixOf (tshow (floraInstanceIdToLua oakIid)))

    it "accepts a record whose species the catalog holds" $ do
        let snap = sessionWithPage
                (blankPageSnapshot arenaPage arenaParams)
                    { pgsFloraConditions = HM.singleton oakIid
                        (savedOf (deadRecord oakId homeTile ContextWild)) }
            saved = snapshotToSaveData (SaveRequestMeta "c" "ts" False) snap
        missingFloraReferences probeCatalog
            [ (wpsPageId w, w) | w ← sdWorlds saved ] `shouldBe` []

-- * Fixtures shared by the spec and the engine spec

sessionWithPage ∷ PageSnapshot → SessionSnapshot
sessionWithPage page = SessionSnapshot
    { snapGameTime = 0, snapTexPalette = emptyTexPalette
    , snapNextItemId = 1, snapNextBuildingId = 1, snapNextUnitId = 1
    , snapActivePage = pgsPageId page, snapVisiblePages = [pgsPageId page]
    , snapLiveCamera = LiveCameraSnapshot (Just (pgsPageId page)) 0 0 1 FaceSouth
    , snapPages = HM.singleton (pgsPageId page) page
    , snapItemTags = mempty, snapItemRefs = mempty
    , snapPortableKnowledge = emptyPortableKnowledge
            , snapRelationCauses = emptyRelationCauseStore }

roundTrip ∷ (Eq a, Show a, Enum a, Bounded a) ⇒ [a] → Expectation
roundTrip xs = map (fromVocabTag ∘ toVocabTag) xs `shouldBe` map Just xs

-- * Engine-backed examples

quietLog ∷ IO LoggerState
quietLog = initLogger defaultLogConfig { lcBackend = LogToCallback (\_ → pure ()) }

-- | A page carrying the arena params, the probe catalog's species and
--   whatever the example installs, captured through the production
--   'capturePageSnapshot', encoded into a real envelope, decoded again,
--   and staged by the production 'stageSession'.
engineSpec ∷ SpecWith EngineEnv
engineSpec = describe "flora condition persistence" $ do

  describe "capture, envelope and staging" $ do

    it "keeps one of three co-tenants dead across eviction, regeneration, \
       \capture, encode, decode and staging — siblings untouched" $ \env → do
        ws ← emptyWorldState
        writeIORef (wsGenParamsRef ws) (Just arenaParams)
        -- The runtime owner: one occurrence marked dead.
        atomicModifyIORef' (wsFloraConditionsRef ws) $ \m →
            either (error ∘ show) (\m' → (m', ()))
                   (markFloraDead oakIid
                        (deadRecord oakId homeTile ContextWild) m)
        -- The chunk is resident with its three co-tenants, then EVICTED,
        -- then REGENERATED from the same provenance. The overlay belongs
        -- to the page, not the chunk, so nothing here touches it, and each
        -- regenerated occurrence finds (or does not find) its record by id.
        let hasRecord = do
                conds ← readIORef (wsFloraConditionsRef ws)
                ids ← residentIds ws
                pure [ (iid, isJust (lookupFloraCondition iid conds))
                     | iid ← ids ]
        writeIORef (wsTilesRef ws) (chunkStore coTenantInstances)
        hasRecord `shouldReturn`
            [(oakIid, True), (berryIid, False), (fernIid, False)]
        writeIORef (wsTilesRef ws) (chunkStore [])
        hasRecord `shouldReturn` []
        residentIds ws `shouldReturn` []
        writeIORef (wsTilesRef ws) (chunkStore coTenantInstances)
        hasRecord `shouldReturn`
            [(oakIid, True), (berryIid, False), (fernIid, False)]

        (snap, back) ← throughEnvelope probeCatalog ws arenaPage
        -- The production capture reached the snapshot with the species
        -- NAMED, not numbered.
        fmap (fcrSpecies ∘ snd) (HM.toList
            (pgsFloraConditions (snapPages snap HM.! arenaPage)))
          `shouldBe` [FloraByName "probe_oak"]
        -- And the envelope round trip preserved it exactly.
        pgsFloraConditions (snapPages back HM.! arenaPage)
          `shouldBe` pgsFloraConditions (snapPages snap HM.! arenaPage)

        staged ← stageOnly env probeCatalog back
        stagedConds ← readIORef (wsFloraConditionsRef (spWorldState staged))
        stagedConds `shouldBe` HM.singleton oakIid
            (deadRecord oakId homeTile ContextWild)
        -- Same cause, phase, stage and outcome — the record itself — and
        -- both siblings have none.
        lookupFloraCondition berryIid stagedConds `shouldBe` Nothing
        lookupFloraCondition fernIid stagedConds `shouldBe` Nothing
        -- Clearing after a load removes exactly the one record.
        clearFloraCondition oakIid stagedConds `shouldBe` HM.empty

    it "keeps a planted row crop dead across a save/load, alongside its \
       \planting" $ \env → do
        ws ← emptyWorldState
        writeIORef (wsGenParamsRef ws) (Just arenaParams)
        let iid = plantedFloraInstanceId 7
            -- a species with no phases freezes none (contract §1)
            rec' = (deadRecord berryId otherTile ContextCultivated)
                       { fcrPhase = Nothing }
        atomicModifyIORef' (wsEditsRef ws) $ \es →
            ( appendEdit (chunkOf otherTile)
                  (WePlaceFloraWithId (fst otherTile) (snd otherTile) berryId
                       10 1.0 iid) es
            , () )
        atomicModifyIORef' (wsFloraConditionsRef ws) $ \m →
            either (error ∘ show) (\m' → (m', ()))
                   (markFloraDead iid rec' m)
        atomicModifyIORef' (wsPlantedFloraCursorRef ws) $ \_ → (8, ())
        (_, back) ← throughEnvelope probeCatalog ws arenaPage
        staged ← stageOnly env probeCatalog back
        readIORef (wsFloraConditionsRef (spWorldState staged))
            `shouldReturn` HM.singleton iid rec'
        -- The planting it belongs to came back too, under the same id.
        edits ← readIORef (wsEditsRef (spWorldState staged))
        [ i | es ← HM.elems edits, WePlaceFloraWithId _ _ _ _ _ i ← es ]
            `shouldBe` [iid]

    it "refuses to stage a record naming a species the build lacks, \
       \before anything is published" $ \env → do
        ws ← emptyWorldState
        writeIORef (wsGenParamsRef ws) (Just arenaParams)
        atomicModifyIORef' (wsFloraConditionsRef ws) $ \m →
            either (error ∘ show) (\m' → (m', ()))
                   (markFloraDead oakIid
                        (deadRecord oakId homeTile ContextWild) m)
        (_, back) ← throughEnvelope probeCatalog ws arenaPage
        logger ← quietLog
        matReg ← readIORef (materialRegistryRef env)
        -- The loading build has no 'probe_oak'.
        writeIORef (floraCatalogRef env) emptyFloraCatalog
        r ← stageSession env logger (saveDataOf back) matReg
        case r of
            Left e → renderStageError e `shouldSatisfy`
                         T.isInfixOf "flora condition record"
            Right _ → expectationFailure "staged a record with no species"

-- | Capture one page through the production path, build the session
--   snapshot the save barrier builds, encode it into a real envelope and
--   decode it back — the integrity graph and every component's validate
--   and assemble steps all run on the way.
throughEnvelope
    ∷ FloraCatalog → WorldState → WorldPageId
    → IO (SessionSnapshot, SessionSnapshot)
throughEnvelope catalog ws pid = do
    captured ← capturePageSnapshot catalog emptyBuildingManager
                   emptyUnitManager emptyUnitThreadState (pid, ws)
    page ← either (\e → expectationFailure (T.unpack e) ≫ error "unreachable")
                  pure captured
    let globals = SessionGlobals
            { sgGameTime = 0, sgTexPalette = emptyTexPalette
            , sgNextItemId = 1, sgNextBuildingId = 1, sgNextUnitId = 1
            , sgActivePage = pid, sgVisiblePages = [pid]
            , sgLiveCamera = LiveCameraSnapshot (Just pid) 0 0 1 FaceSouth
            , sgItemTags = mempty, sgItemRefs = mempty
            , sgPortableKnowledge = emptyPortableKnowledge
            , sgRelationCauses = emptyRelationCauseStore }
        withId = page { pgsGeneratedId = Just (fixtureGeneratedWorldIdForPage pid) }
    snap ← either (\e → expectationFailure (show e) ≫ error "unreachable")
                  pure (captureSessionSnapshot globals [withId])
    let bytes = encodeSessionSnapshot (snapshotSaveMetadata meta snap) snap []
    case decodeSessionEnvelope HS.empty HS.empty bytes of
        Left e → expectationFailure (T.unpack e) ≫ error "unreachable"
        Right (_, back, _, _) → pure (snap, back)
  where meta = SaveRequestMeta "cond" "2026-10-21T00:00:00.000000Z" False

saveDataOf ∷ SessionSnapshot → SaveData
saveDataOf = snapshotToSaveData
    (SaveRequestMeta "cond" "2026-10-21T00:00:00.000000Z" False)

-- | Stage a decoded session against a catalog and return the one page.
stageOnly ∷ EngineEnv → FloraCatalog → SessionSnapshot → IO StagedPage
stageOnly env catalog snap = do
    logger ← quietLog
    matReg ← readIORef (materialRegistryRef env)
    writeIORef (floraCatalogRef env) catalog
    r ← stageSession env logger (saveDataOf snap) matReg
    case r of
        Left e → expectationFailure (T.unpack (renderStageError e))
                   ≫ error "unreachable"
        Right ss → case L.find ((≡ arenaPage) ∘ spPageId) (ssPages ss) of
            Just p  → pure p
            Nothing → expectationFailure "the staged page is missing"
                        ≫ error "unreachable"

-- * Frozen-slice builders

emptyActivityV7 ∷ WorldPageId → PageActivityDTO
emptyActivityV7 pid = PageActivityDTO pid HM.empty HM.empty HM.empty
    HM.empty HM.empty HM.empty HM.empty
    (toGroundItemsDTO emptyGroundItems) HM.empty HM.empty HM.empty
    firstConstructAttemptId HM.empty

-- | The frozen v6 slice, every field distinct from its default only
--   where an example sets it.
emptyActivityV6 ∷ WorldPageId → PageActivityDTOv6
emptyActivityV6 pid = PageActivityDTOv6
    { pad6PageId = pid, pad6Mine = HM.empty, pad6Construct = HM.empty
    , pad6Chop = HM.empty, pad6Till = HM.empty, pad6Plant = HM.empty
    , pad6FloraHarvests = HM.empty, pad6CropPlots = HM.empty
    , pad6GroundItems = toGroundItemsDTO emptyGroundItems
    , pad6SpoilPiles = HM.empty, pad6PendingChop = HM.empty
    , pad6PendingHarvests = HM.empty
    , pad6ConstructNextAttempt = firstConstructAttemptId }

emptyActivityV5 ∷ WorldPageId → PageActivityDTOv5
emptyActivityV5 pid = PageActivityDTOv5
    { pad5PageId = pid, pad5Mine = HM.empty, pad5Construct = HM.empty
    , pad5Chop = HM.empty, pad5Till = HM.empty, pad5Plant = HM.empty
    , pad5FloraHarvests = HM.empty, pad5CropPlots = HM.empty
    , pad5GroundItems = toGroundItemsDTO emptyGroundItems
    , pad5SpoilPiles = HM.empty, pad5PendingChop = HM.empty
    , pad5PendingHarvests = HM.empty
    , pad5ConstructNextAttempt = firstConstructAttemptId }

chunkOf ∷ (Int, Int) → ChunkCoord
chunkOf (gx, gy) = fst (globalToChunk gx gy)
