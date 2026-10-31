{-# LANGUAGE OverloadedStrings #-}
-- | Persisted groundcover-crop CONDITION records (#2552, EFM-6).
--
--   A groundcover plot has no instance identity: it is a tile-keyed
--   'CropPlotOf' value, so its condition is the SAME shared record #2549
--   gave wild and row flora, EMBEDDED in that value rather than put in the
--   identity-keyed store. These examples pin the plot's pure operations,
--   the one page-qualified query over both owners, the species conversion
--   the record rides through, the @world-activity@ v8 wire shape (and that
--   every retained v1..v7 crop-bearing layout still decodes to an
--   UNCONDITIONED current plot with nothing else lost), the validation each
--   refusal goes through, and — through the REAL production capture,
--   envelope, staging and Lua harvest/plant paths — that a condition
--   survives a save/load and dies with its plot.
--
--   Selected by @--match "crop plot condition persistence"@.
module Test.Headless.World.CropPlotCondition (spec, engineSpec) where

import UPrelude
import Test.Hspec
import Gameplay.Tags.Page (CropPlotStore(..), plotStoreFromPlots)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.ByteString as BS
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
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Ground (GroundItems(..), emptyGroundItems)
import Item.Knowledge (emptyPortableKnowledge)
import Item.Types
    ( ItemDef(..), ItemFood(..), ItemInstance(..), ItemManager(..) )
import Item.Ground (GroundItem(..))
import Structure.Palette (emptyTexPalette)
import Structure.Types (emptyChunkStructures)
import Building.Types (emptyBuildingManager)
import Unit.Types (emptyUnitManager)
import Unit.Sim.Types (emptyUnitThreadState)
import Unit.Faction.RelationStore (emptyRelationCauseStore)
import Test.Headless.Harness.GeneratedIds (fixtureGeneratedWorldIdForPage)

import World.Chunk.Types
    (ChunkCoord, ColumnTiles(..), LoadedChunk(..), chunkSize)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Edit.Types (emptyWorldEdits)
import World.Flora.Condition
import World.Flora.ConditionQuery
import World.Flora.Clock (growthClock)
import World.Flora.CropPlot
import World.Flora.Identity
import World.Flora.Reference (FloraRef(..))
import World.Flora.Types
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (globalToChunk)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Load.Stage (renderStageError, stageSession)
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Page
import World.Save.Component.Types
    ( ComponentCodec(..), renderComponentError )
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Snapshot
import World.Save.Snapshot.Adapter
    (SaveRequestMeta(..), snapshotSaveMetadata, snapshotToSaveData)
import World.Save.Types
    ( MissingFloraRef(..), SaveData(..), WorldPageSave(..)
    , missingFloraReferences, nameFloraReferences, renderMissingFloraRef
    , resolveFloraReferences )
import World.State.Types
import World.Thread.Command.Save.WriteWorld (capturePageSnapshot)
import World.Tile.Types (WorldTileData(..))
import World.Vegetation (vegTilledSoil)

-- * Fixture geography

pageA, pageB, arenaPage ∷ WorldPageId
pageA     = WorldPageId "crop_condition_a"
pageB     = WorldPageId "crop_condition_b"
arenaPage = WorldPageId "crop_condition_arena"

-- | Both tiles are inland and well inside the arena sentinel's identity
--   range, so the canonical name of each is itself.
plotTile, otherTile ∷ (Int, Int)
plotTile  = (37, 55)
otherTile = (38, 55)

arenaParams ∷ WorldGenParams
arenaParams = defaultWorldGenParams { wgpSeed = 0, wgpWorldSize = 100000 }

-- * Species

wheatId, cloverId ∷ FloraId
wheatId  = FloraId 1
cloverId = FloraId 2

probeCatalog ∷ FloraCatalog
probeCatalog = emptyFloraCatalog
    { fcSpecies = HM.fromList
        [ (1, named "probe_wheat"), (2, named "probe_clover") ]
    , fcNextId = 3 }
  where named n = newFloraSpecies n (TextureHandle 0)

speciesName ∷ FloraId → Text
speciesName fid = maybe "" fsName (lookupSpecies fid probeCatalog)

-- * Plots and records

-- | A planted-day-90, health-0.75 plot: every field distinct, so a
--   dropped or swapped field cannot compare equal by coincidence.
livePlot ∷ CropPlot
livePlot = (newCropPlot wheatId 90 0.75)

-- | A valid cultivated death: a mature, fruiting-stage frost death whose
--   corpse awaits replanting until day 180.
deadRecord ∷ FloraId → (Int, Int) → FloraConditionRecord
deadRecord fid tile = FloraConditionRecord
    { fcrSpecies   = fid
    , fcrTile      = tile
    , fcrCondition = ConditionDead
    , fcrCause     = CauseFire
    , fcrBeganDay  = 120
    , fcrContext   = ContextCultivated
    , fcrPhase     = Just PhaseMatured
    , fcrStage     = Just CycleFlowering
    , fcrRetention = RetentionTransient 180 RetainAwaitReplanting
    }

deadPlot ∷ CropPlot
deadPlot = livePlot { cpCondition = Just (deadRecord wheatId plotTile) }

savedOf ∷ FloraConditionRecord → SavedFloraConditionRecord
savedOf r = r { fcrSpecies = FloraByName (speciesName (fcrSpecies r)) }

savedPlot ∷ CropPlot → SavedCropPlot
savedPlot cp = cp
    { cpSpecies   = FloraByName (speciesName (cpSpecies cp))
    , cpCondition = savedOf <$> cpCondition cp }

-- | The independent oracle for which (context, retention) pairs are legal
--   for a CROP plot — spelled out from the design record (D-13, D-19)
--   rather than derived from the implementation under test: a plot is
--   cultivated, so a transient corpse can only await replanting.
legalCropRetention ∷ ConditionRetention → Bool
legalCropRetention RetentionPersistent = True
legalCropRetention (RetentionTransient _ s) = s ≡ RetainAwaitReplanting

-- * Pure examples

spec ∷ Spec
spec = describe "crop plot condition persistence" $ do

  describe "the plot value" $ do

    it "starts a newly planted plot WITHOUT a condition" $ do
        cropPlotCondition (newCropPlot wheatId 90 0.75) `shouldBe` Nothing

    it "reads absence, marks a plot dead, then reads the stored record" $ do
        let r = deadRecord wheatId plotTile
        cropPlotCondition livePlot `shouldBe` Nothing
        case markCropPlotDead plotTile r livePlot of
            Left e → expectationFailure (show e)
            Right marked → do
                cropPlotCondition marked `shouldBe` Just r
                -- Every other plot field is untouched.
                marked { cpCondition = Nothing } `shouldBe` livePlot

    it "refuses to mark an already conditioned plot, preserving the \
       \original EXACTLY whatever the second record says" $
        forM_ [minBound .. maxBound ∷ FloraDeathCause] $ \cause → do
            let first  = deadRecord wheatId plotTile
                second = first { fcrCause = cause, fcrBeganDay = 999
                               , fcrPhase = Just PhaseSeedling
                               , fcrStage = Nothing
                               , fcrRetention = RetentionPersistent }
                plot0  = livePlot { cpCondition = Just first }
            markCropPlotDead plotTile second plot0
                `shouldBe` Left CropAlreadyConditioned
            -- A refusal returns no new plot: the one the caller holds is
            -- the one it started with, every field intact.
            plot0 `shouldBe` deadPlot { cpCondition = Just first }
            cropPlotCondition plot0 `shouldBe` Just first

    it "clears ONLY the condition, and clearing again changes nothing" $ do
        let cleared = clearCropPlotCondition deadPlot
        cropPlotCondition cleared `shouldBe` Nothing
        cleared `shouldBe` livePlot
        clearCropPlotCondition cleared `shouldBe` cleared
        clearCropPlotCondition livePlot `shouldBe` livePlot

    it "marks and clears back to the original plot for every cause and \
       \every legal retention outcome" $
        forM_ [minBound .. maxBound ∷ FloraDeathCause] $ \cause →
          forM_ [ RetentionPersistent
                , RetentionTransient 120 RetainAwaitReplanting
                , RetentionTransient 500 RetainAwaitReplanting ] $ \ret → do
            let r = (deadRecord wheatId plotTile)
                        { fcrCause = cause, fcrRetention = ret }
            legalCropRetention ret `shouldBe` True
            fmap clearCropPlotCondition (markCropPlotDead plotTile r livePlot)
                `shouldBe` Right livePlot

    it "accepts exactly the legal retention outcomes, over the whole \
       \outcome space (the independent oracle decides)" $
        forM_ (RetentionPersistent
               : [ RetentionTransient e s
                 | s ← [minBound .. maxBound ∷ RetentionSuccessor]
                 , e ← [119, 120, 121] ]) $ \ret → do
            let r = (deadRecord wheatId plotTile) { fcrRetention = ret }
                expiryOk = case ret of
                    RetentionPersistent → True
                    RetentionTransient e _ → e ≥ fcrBeganDay r
                legal = legalCropRetention ret ∧ expiryOk
            null (cropConditionProblems plotTile wheatId r)
                `shouldBe` legal
            either (const False) (const True)
                (markCropPlotDead plotTile r livePlot) `shouldBe` legal

    it "refuses each invalid record, naming the problem, and judges \
       \validity BEFORE occupancy" $ do
        let ok = deadRecord wheatId plotTile
            problemsOf r = cropConditionProblems plotTile wheatId r
        problemsOf ok `shouldBe` []
        problemsOf ok { fcrRetention = RetentionTransient 119 RetainAwaitReplanting }
            `shouldBe` [CropConditionShape (ExpiryBeforeBegin 119 120)]
        problemsOf ok { fcrContext = ContextWild
                      , fcrRetention = RetentionPersistent }
            `shouldBe` [CropConditionNotCultivated]
        problemsOf ok { fcrCondition = ConditionAlive }
            `shouldBe` [CropConditionShape NotAStoredCondition]
        problemsOf ok { fcrSpecies = cloverId }
            `shouldBe` [CropConditionSpeciesMismatch]
        problemsOf ok { fcrTile = otherTile }
            `shouldBe` [CropConditionTileMismatch otherTile]
        -- Invalid is judged first, so a bad record on an occupied plot is
        -- reported as invalid rather than as occupied.
        markCropPlotDead plotTile ok { fcrTile = otherTile } deadPlot
            `shouldBe` Left (CropInvalidCondition
                                [CropConditionTileMismatch otherTile])
        -- And a refused mark changes nothing about the plot it was asked
        -- to mark.
        markCropPlotDead plotTile ok { fcrSpecies = cloverId } livePlot
            `shouldBe` Left (CropInvalidCondition [CropConditionSpeciesMismatch])

  describe "the uniform query" $ do

    let rowIid     = plantedFloraInstanceId 7
        wildIid    = generatedFloraInstanceId (unWorldPageId pageA) 40 55
                         "probe_clover" 0
        rowRecord  = (deadRecord cloverId otherTile)
        wildRecord = (deadRecord cloverId (40, 55)) { fcrContext = ContextWild
                         , fcrRetention = RetentionTransient 150 RetainReseed }
        stores = HM.fromList
            [ ( pageA
              , PageConditionStores
                  { pcsOccurrences = HM.fromList
                      [(rowIid, rowRecord), (wildIid, wildRecord)]
                  , pcsPlots = HM.singleton plotTile deadPlot } )
            , ( pageB
              , PageConditionStores
                  { pcsOccurrences = HM.empty
                  , pcsPlots = HM.singleton plotTile livePlot } ) ]
        query pid s = lookupConditionRecord pid s stores

    it "returns the SAME record type for a row-flora identity, a wild \
       \identity and a plot tile" $ do
        let viaRow  = query pageA (SubjectOccurrence rowIid)
            viaWild = query pageA (SubjectOccurrence wildIid)
            viaPlot = query pageA (SubjectPlot plotTile)
        viaRow  `shouldBe` Just rowRecord
        viaWild `shouldBe` Just wildRecord
        viaPlot `shouldBe` Just (deadRecord wheatId plotTile)
        -- One type: the three answers sit in one list.
        let answers ∷ [Maybe FloraConditionRecord]
            answers = [viaRow, viaWild, viaPlot]
        length (catMaybes answers) `shouldBe` 3

    it "answers absence for an unconditioned plot, an unknown tile, an \
       \unknown identity and an unheld page" $ do
        query pageB (SubjectPlot plotTile) `shouldBe` Nothing
        query pageA (SubjectPlot otherTile) `shouldBe` Nothing
        query pageA (SubjectOccurrence (plantedFloraInstanceId 99))
            `shouldBe` Nothing
        query (WorldPageId "elsewhere") (SubjectPlot plotTile) `shouldBe` Nothing

    it "is page-QUALIFIED: the same tile on two pages answers each page's \
       \own plot" $ do
        query pageA (SubjectPlot plotTile) `shouldSatisfy` isJust
        query pageB (SubjectPlot plotTile) `shouldBe` Nothing

    it "never reads the reserved non-identity value, even from a store \
       \that (illegally) holds it" $ do
        let poisoned = HM.singleton pageA PageConditionStores
                { pcsOccurrences = HM.singleton floraInstanceIdNone rowRecord
                , pcsPlots = HM.empty }
        lookupConditionRecord pageA (SubjectOccurrence floraInstanceIdNone)
            poisoned `shouldBe` Nothing
        -- And a plot's growth adapter, which carries that value, never
        -- reaches the occurrence store: the plot is read by its tile.
        fiInstanceId (cropPlotInstance deadPlot) `shouldBe` floraInstanceIdNone
        query pageA (SubjectPlot plotTile) `shouldSatisfy` isJust

  describe "the species conversions" $ do

    it "swaps the plot's species AND its condition's, leaving every other \
       \field alone" $ do
        let f = Just . FloraByName . speciesName
        traverseCropPlotSpecies f deadPlot `shouldBe` Just (savedPlot deadPlot)
        traverseCropPlotSpecies Just deadPlot `shouldBe` Just deadPlot
        traverseCropPlotSpecies f livePlot `shouldBe` Just (savedPlot livePlot)

    it "refuses the conversion when ONLY the condition's species is \
       \unresolvable" $ do
        let f s = if s ≡ cloverId then Nothing else Just s
            odd' = deadPlot { cpCondition =
                       Just (deadRecord cloverId plotTile) }
        traverseCropPlotSpecies f odd' `shouldBe` Nothing

    it "names, then resolves, a conditioned plot back to the identical \
       \live value" $ do
        let crops = HM.fromList [(plotTile, deadPlot), (otherTile, livePlot)]
        case nameFloraReferences probeCatalog arenaPage emptyWorldEdits crops
                 HM.empty HM.empty of
            Left e → expectationFailure (show e)
            Right (_, named, _, _) → do
                named `shouldBe` HM.fromList
                    [ (plotTile, savedPlot deadPlot)
                    , (otherTile, savedPlot livePlot) ]
                case resolveFloraReferences probeCatalog arenaPage
                         emptyWorldEdits named HM.empty HM.empty of
                    Left e → expectationFailure (show e)
                    Right (_, back, _, _) → back `shouldBe` crops

    it "reports a condition species the catalog lacks, naming page, tile \
       \and the crop plot condition" $ do
        let lost = (savedPlot deadPlot)
                { cpCondition = Just (savedOf (deadRecord wheatId plotTile))
                      { fcrSpecies = FloraByName "gone_species" } }
            snap = sessionWithPage
                (blankPageSnapshot arenaPage arenaParams)
                    { pgsCropPlots = HM.singleton plotTile lost }
            saved = snapshotToSaveData (SaveRequestMeta "c" "ts" False) snap
            missing = missingFloraReferences probeCatalog
                [ (wpsPageId w, w) | w ← sdWorlds saved ]
        map mfrSpecies missing `shouldBe` [FloraByName "gone_species"]
        map mfrCoord missing `shouldBe` [plotTile]
        let rendered = map renderMissingFloraRef missing
        rendered `shouldSatisfy` all (T.isInfixOf (unWorldPageId arenaPage))
        rendered `shouldSatisfy` all (T.isInfixOf "crop plot condition")

  describe "world-activity v8 wire shape" $ do

    it "accepts v1 through v8" $
        ccInputVers worldActivityCodec `shouldBe` [1 .. 8]

    it "round-trips a populated v8 payload byte-for-byte, and it validates" $ do
        let dto = WorldActivityDTO
                [ (emptyActivityV8 pageA)
                    { padCropPlots = HM.fromList
                        [ (plotTile, toCropPlotDTO' (savedPlot deadPlot))
                        , (otherTile, toCropPlotDTO' (savedPlot livePlot)) ]
                    , padFloraHarvests = HM.singleton (plantedFloraInstanceId 3) 42.5
                    } ]
        case ccDecode worldActivityCodec 8 (S.encode dto) of
            Left e → expectationFailure (show e)
            Right back → do
                S.encode back `shouldBe` S.encode dto
                ccValidate worldActivityCodec back `shouldBe` []
                case back of
                    WorldActivityDTO [s] →
                        fmap (fmap fromCropPlotDTO' . Just) (HM.lookup plotTile (padCropPlots s))
                            `shouldBe` Just (Just (savedPlot deadPlot))
                    _ → expectationFailure "expected one page slice"

    it "writes the condition through the production encoder" $ do
        let snap = sessionWithPage
                (blankPageSnapshot pageA arenaParams)
                    { pgsCropPlots = HM.singleton plotTile (savedPlot deadPlot) }
            bytes = ccEncode worldActivityCodec snap
        case ccDecode worldActivityCodec 8 bytes of
            Right (WorldActivityDTO [s]) →
                fmap (cpCondition . fromCropPlotDTO')
                     (HM.lookup plotTile (padCropPlots s))
                    `shouldBe` Just (Just (savedOf (deadRecord wheatId plotTile)))
            other → expectationFailure (show (fmap (const ()) other))

  describe "the tracked v8 fixture" $ do

    -- The registered fixture is the proof that the CURRENT format, written
    -- by the production save path, holds a real groundcover plot and
    -- decodes it back (an empty map would prove nothing about the new
    -- crop DTO). No live verb marks a plot dead yet (EFM-7), so the plot
    -- is unconditioned; populated conditions are pinned by this group.
    it "z20 decodes through the real envelope to a NONEMPTY crop-plot map \
       \of unconditioned plots" $ do
        bytes ← BS.readFile
            "test-headless/data/save-compat/z20-crop-plot-condition.bin"
        let luaNames = HS.fromList ["unit_ai", "building_spawn"]
        case decodeSessionEnvelope luaNames luaNames bytes of
            Left err → expectationFailure (T.unpack err)
            Right (_, snap, _, _) → do
                let plots = concatMap (HM.elems ∘ pgsCropPlots)
                                      (HM.elems (snapPages snap))
                length plots `shouldSatisfy` (> 0)
                map cpSpecies plots `shouldSatisfy`
                    all (≡ FloraByName "wheat")
                map cpCondition plots `shouldSatisfy` all isNothing

  describe "migrating every retained crop-bearing layout" $ do

    -- Frozen DTO encoders, never the new DTO: bytes made by the current
    -- layout would prove nothing about legacy compatibility. Each payload
    -- also carries unrelated page data, so a misaligned field or an
    -- accidental loss would show.
    let legacyCrops ∷ HM.HashMap (Int, Int) CropPlotDTOv1
        legacyCrops = HM.fromList
            [ (plotTile,  CropPlotDTOv1 (FloraId 1) 90 0.75)
            , (otherTile, CropPlotDTOv1 (FloraId 2) 12 0.25) ]
        refCrops ∷ HM.HashMap (Int, Int) CropPlotDTOv2
        refCrops = HM.fromList
            [ (plotTile,  CropPlotDTOv2 (FloraByName "probe_wheat") 90 0.75)
            , (otherTile, CropPlotDTOv2 (FloraByLegacyId (FloraId 2)) 12 0.25) ]
        harvests = HM.singleton (plantedFloraInstanceId 3) 42.5
        occurrenceCondition = toFloraConditionDTO
            (savedOf (deadRecord cloverId otherTile))
            -- an unrelated v7 occurrence record, to catch misalignment
        decodedAt ver bytes = case ccDecode worldActivityCodec ver bytes of
            Right (WorldActivityDTO [s]) → pure s
            Right _ → expectationFailure "expected one page slice" ≫ error "unreachable"
            Left e → expectationFailure (show e) ≫ error "unreachable"
        -- What a migrated crop must look like: same species reference,
        -- planted day and health, and NO condition.
        expectUnconditioned expected s = do
            HM.map (\d → (cpiSpecies d, cpiPlantedDay d, cpiHealth d)) (padCropPlots s)
                `shouldBe` expected
            HM.map cpiCondition (padCropPlots s)
                `shouldBe` HM.fromList [(plotTile, Nothing), (otherTile, Nothing)]
        expectedRefs = HM.fromList
            [ (plotTile,  (FloraByName "probe_wheat", 90, 0.75))
            , (otherTile, (FloraByLegacyId (FloraId 2), 12, 0.25)) ]
        expectedLegacy = HM.fromList
            [ (plotTile,  (FloraByLegacyId (FloraId 1), 90, 0.75))
            , (otherTile, (FloraByLegacyId (FloraId 2), 12, 0.25)) ]

    it "migrates a v7 payload: unconditioned plots, the v7 occurrence \
       \condition and every other field intact" $ do
        let slice = (emptyActivityV7 pageA)
                { pad7CropPlots = refCrops
                , pad7FloraHarvests = harvests
                , pad7FloraConditions =
                    HM.singleton (plantedFloraInstanceId 4) occurrenceCondition
                , pad7ConstructNextAttempt = firstConstructAttemptId }
        s ← decodedAt 7 (S.encode (WorldActivityDTOv7 [slice]))
        expectUnconditioned expectedRefs s
        padFloraConditions s `shouldBe`
            HM.singleton (plantedFloraInstanceId 4) occurrenceCondition
        padFloraHarvests s `shouldBe` harvests
        padPageId s `shouldBe` pageA

    it "migrates a v6 payload through the frozen three-field crop shape" $ do
        let slice = (emptyActivityV6 pageA)
                { pad6CropPlots = refCrops
                , pad6FloraHarvests = harvests }
        s ← decodedAt 6 (S.encode (WorldActivityDTOv6 [slice]))
        expectUnconditioned expectedRefs s
        padFloraHarvests s `shouldBe` harvests
        padFloraConditions s `shouldBe` HM.empty

    it "migrates a v5 payload (legacy species ordinals)" $ do
        let slice = (emptyActivityV5 pageA)
                { pad5CropPlots = legacyCrops
                , pad5FloraHarvests = harvests }
        s ← decodedAt 5 (S.encode (WorldActivityDTOv5 [slice]))
        expectUnconditioned expectedLegacy s
        padFloraHarvests s `shouldBe` harvests

    it "migrates a v4 payload (legacy species ordinals)" $ do
        let slice = (emptyActivityV4 pageA)
                { pad4CropPlots = legacyCrops
                , pad4FloraHarvests = harvests }
        s ← decodedAt 4 (S.encode (WorldActivityDTOv4 [slice]))
        expectUnconditioned expectedLegacy s
        padFloraHarvests s `shouldBe` harvests

    it "migrates a v3 payload (legacy species ordinals)" $ do
        let slice = (emptyActivityV3 pageA)
                { pad3CropPlots = legacyCrops }
        s ← decodedAt 3 (S.encode (WorldActivityDTOv3 [slice]))
        expectUnconditioned expectedLegacy s

    it "migrates v2 and v1 payloads, which share one frozen layout" $
        forM_ [2, 1] $ \ver → do
            let slice = (emptyActivityV2 pageA)
                    { pad2CropPlots = legacyCrops }
            s ← decodedAt ver (S.encode (WorldActivityDTOv2 [slice]))
            expectUnconditioned expectedLegacy s

    it "does NOT read v7 bytes as the current four-field crop layout \
       \(the frozen wrapper nests the frozen crop DTO)" $ do
        -- If the v7 wrapper nested the CURRENT crop DTO, these exact
        -- bytes would fail to decode (the trailing Maybe byte is not
        -- there) or misalign every later field.
        let slice = (emptyActivityV7 pageA)
                { pad7CropPlots = refCrops, pad7FloraHarvests = harvests }
            bytes = S.encode (WorldActivityDTOv7 [slice])
        (S.decode bytes ∷ Either String WorldActivityDTOv7)
            `shouldSatisfy` either (const False) (const True)
        s ← decodedAt 7 bytes
        HM.keys (padCropPlots s) `shouldMatchList` [plotTile, otherTile]
        padFloraHarvests s `shouldBe` harvests

  describe "component validation" $ do

    -- Each case is encoded at the CURRENT version and driven through the
    -- real decode and validate path, so a refusal is the one a load
    -- would produce, and it must name the page and the plot's tile.
    let refusalFor ∷ WorldPageId → (Int, Int) → CropPlotDTO → IO [Text]
        refusalFor pid tile dto = do
            let payload = WorldActivityDTO
                    [ (emptyActivityV8 pid)
                        { padCropPlots = HM.singleton tile dto } ]
            case ccDecode worldActivityCodec 8 (S.encode payload) of
                Left e → expectationFailure (show e) ≫ pure []
                Right decoded →
                    pure (map renderComponentError
                              (ccValidate worldActivityCodec decoded))
        planted = savedPlot livePlot
        wireWith r = toCropPlotDTO' planted
            { cpCondition = Just (savedOf r) }
        base = deadRecord wheatId plotTile
        namesPageAndTile pid tile msgs = do
            msgs `shouldSatisfy` (not ∘ null)
            forM_ msgs $ \m → do
                m `shouldSatisfy` T.isInfixOf (unWorldPageId pid)
                m `shouldSatisfy` T.isInfixOf (tshow tile)
                m `shouldSatisfy` T.isInfixOf "crop plot"

    it "accepts a valid condition and an unconditioned plot" $ do
        refusalFor pageA plotTile (wireWith base) `shouldReturn` []
        refusalFor pageA plotTile (toCropPlotDTO' planted) `shouldReturn` []

    it "refuses an expiry day before the begin day" $ do
        msgs ← refusalFor pageA plotTile
            (wireWith base { fcrRetention =
                RetentionTransient 119 RetainAwaitReplanting })
        namesPageAndTile pageA plotTile msgs
        msgs `shouldSatisfy` any (T.isInfixOf "precedes")

    it "refuses a context that is not cultivated" $ do
        msgs ← refusalFor pageB plotTile
            (wireWith base { fcrContext = ContextWild
                           , fcrRetention = RetentionPersistent })
        namesPageAndTile pageB plotTile msgs
        msgs `shouldSatisfy` any (T.isInfixOf "cultivated")

    it "refuses a tag outside each vocabulary, field by field, naming \
       \the page and tile rather than failing the whole decode" $
        forM_ [ ("condition", \d → d { fcdCondition = 77 })
              , ("cause",     \d → d { fcdCause = 77 })
              , ("context",   \d → d { fcdContext = 77 })
              , ("life phase", \d → d { fcdPhase = Just 77 })
              , ("annual stage", \d → d { fcdStage = Just 99 })
              , ("retention successor"
                , \d → d { fcdRetention = RetentionTransientD 150 77 }) ] $
            \(field, corrupt) → do
                let wire = wireWith base
                    bad = wire { cpiCondition = corrupt <$> cpiCondition wire }
                msgs ← refusalFor pageA plotTile bad
                namesPageAndTile pageA plotTile msgs
                msgs `shouldSatisfy` any (T.isInfixOf field)

    it "refuses a record naming a species other than the plot's, or a \
       \tile other than the plot's key" $ do
        spMsgs ← refusalFor pageA plotTile
            (wireWith base { fcrSpecies = cloverId })
        namesPageAndTile pageA plotTile spMsgs
        spMsgs `shouldSatisfy` any (T.isInfixOf "species")
        tileMsgs ← refusalFor pageA plotTile
            (wireWith base { fcrTile = otherTile })
        namesPageAndTile pageA plotTile tileMsgs
        tileMsgs `shouldSatisfy` any (T.isInfixOf "tile")

    it "refuses a stored condition that is not dead, and a dead phase" $ do
        alive ← refusalFor pageA plotTile
                    (wireWith base { fcrCondition = ConditionAlive })
        namesPageAndTile pageA plotTile alive
        deadPhase ← refusalFor pageA plotTile
                    (wireWith base { fcrPhase = Just PhaseDead })
        namesPageAndTile pageA plotTile deadPhase

    it "reports every bad plot, ordered by tile, each naming its own tile" $ do
        let payload = WorldActivityDTO
                [ (emptyActivityV8 pageA)
                    { padCropPlots = HM.fromList
                        [ -- two defects: a tile that is not its key, and a
                          -- species that is not its plot's
                          (otherTile, wireWith base { fcrSpecies = cloverId })
                        , -- one defect: a tile that is not its key
                          (plotTile, wireWith base { fcrTile = otherTile }) ] } ]
        case ccDecode worldActivityCodec 8 (S.encode payload) of
            Left e → expectationFailure (show e)
            Right decoded → do
                let msgs = map renderComponentError
                               (ccValidate worldActivityCodec decoded)
                length msgs `shouldBe` 3
                -- Ordered by the plot's own tile, each message naming it.
                length (filter (T.isInfixOf ("(tile " <> tshow plotTile)) msgs)
                    `shouldBe` 1
                length (filter (T.isInfixOf ("(tile " <> tshow otherTile)) msgs)
                    `shouldBe` 2
                map (T.isInfixOf ("(tile " <> tshow plotTile)) msgs
                    `shouldBe` [True, False, False]

-- * Engine-backed examples

quietLog ∷ IO LoggerState
quietLog = initLogger defaultLogConfig { lcBackend = LogToCallback (\_ → pure ()) }

engineSpec ∷ SpecWith EngineEnv
engineSpec = describe "crop plot condition persistence" $ do

  describe "capture, envelope and staging" $ do

    it "keeps a conditioned plot across capture, encode, decode and \
       \staging — species resolved, every field and its sibling intact" $
        \env → do
          ws ← emptyWorldState
          writeIORef (wsGenParamsRef ws) (Just arenaParams)
          let sibling = newCropPlot cloverId 12 0.25
          plotsBefore ← either (\e → expectationFailure (show e) ≫ error "x")
                               pure (markCropPlotDead plotTile
                                         (deadRecord wheatId plotTile) livePlot)
          writeIORef (wsCropPlotsRef ws) $ plotStoreFromPlots
              (HM.fromList [(plotTile, plotsBefore), (otherTile, sibling)])
          (snap, back) ← throughEnvelope probeCatalog ws arenaPage
          -- The production capture reached the snapshot with BOTH species
          -- spellings NAMED, not numbered.
          let capturedPlot = pgsCropPlots (snapPages snap HM.! arenaPage)
                                 HM.! plotTile
          cpSpecies capturedPlot `shouldBe` FloraByName "probe_wheat"
          fmap fcrSpecies (cpCondition capturedPlot)
              `shouldBe` Just (FloraByName "probe_wheat")
          -- The envelope round trip preserved everything exactly.
          pgsCropPlots (snapPages back HM.! arenaPage)
              `shouldBe` pgsCropPlots (snapPages snap HM.! arenaPage)
          staged ← stageOnly env probeCatalog back
          stagedPlots ← cpsPlots <$> readIORef (wsCropPlotsRef (spWorldState staged))
          stagedPlots `shouldBe`
              HM.fromList [(plotTile, plotsBefore), (otherTile, sibling)]
          -- The record itself, field by field, on the right tile.
          fmap cpCondition (HM.lookup plotTile stagedPlots)
              `shouldBe` Just (Just (deadRecord wheatId plotTile))
          fmap cpCondition (HM.lookup otherTile stagedPlots)
              `shouldBe` Just Nothing

    it "refuses to stage a plot whose CONDITION names a species the build \
       \lacks, before anything is published" $ \env → do
        ws ← emptyWorldState
        writeIORef (wsGenParamsRef ws) (Just arenaParams)
        writeIORef (wsCropPlotsRef ws) $ plotStoreFromPlots
            (HM.singleton plotTile deadPlot)
        (_, back) ← throughEnvelope probeCatalog ws arenaPage
        logger ← quietLog
        matReg ← readIORef (materialRegistryRef env)
        -- The loading build has no 'probe_wheat' at all.
        writeIORef (floraCatalogRef env) emptyFloraCatalog
        r ← stageSession env logger (saveDataOf back) matReg
        case r of
            Left e → renderStageError e `shouldSatisfy` T.isInfixOf "crop plot"
            Right _ → expectationFailure "staged a plot with no species"

  describe "harvest and replanting through the existing Lua verbs" $ do

    it "harvests an otherwise harvest-open CONDITIONED plot; plot and \
       \condition disappear, and planting makes a fresh unconditioned plot" $
        \env → do
          ws ← resetScene env
          putPlot ws plotTile deadPlot
          ls ← newBareLuaBackend env
          -- Occupied: planting over a conditioned plot is refused.
          plantResult ls plotTile `shouldReturn` "false"
          plotAt ws plotTile `shouldReturn` Just deadPlot
          -- The existing harvest path still succeeds (a persistence-only
          -- slice does not close the harvest window)…
          runOk ls (harvestExpr plotTile) `shouldReturn` "1"
          groundNames ws `shouldReturn` ["probe_grain"]
          -- …and deletes the whole plot, condition included.
          plotAt ws plotTile `shouldReturn` Nothing
          -- Replanting on the emptied tile creates a FRESH plot.
          plantResult ls plotTile `shouldReturn` "true"
          replanted ← plotAt ws plotTile
          fmap cpCondition replanted `shouldBe` Just Nothing
          fmap cpSpecies replanted `shouldBe` Just wheatId
          fmap cpHealth replanted `shouldBe` Just 1.0

    it "refuses to plant over an occupied plot whatever its condition" $
        \env → do
          ws ← resetScene env
          putPlot ws plotTile livePlot
          putPlot ws otherTile deadPlot { cpCondition =
              Just (deadRecord wheatId otherTile) }
          ls ← newBareLuaBackend env
          plantResult ls plotTile `shouldReturn` "false"
          plantResult ls otherTile `shouldReturn` "false"
          plotAt ws plotTile `shouldReturn` Just livePlot

    it "still refuses to harvest a growth-derived DEAD plot with nil" $
        \env → do
          ws ← resetScene env
          -- Planted 380 days before NOW: past the 360-day lifespan and
          -- inside the dead window, so the existing growth state is dead,
          -- with or without a stored condition.
          (_, absDay) ← growthClock ws
          let ancient = newCropPlot wheatId (absDay - 380) 1.0
              conditioned = ancient { cpCondition =
                  Just (deadRecord wheatId plotTile) }
          forM_ [ancient, conditioned] $ \plot → do
              putPlot ws plotTile plot
              ls ← newBareLuaBackend env
              runOk ls (harvestExpr plotTile) `shouldReturn` "nil"
              -- Refused: nothing was spawned and the plot is untouched.
              groundNames ws `shouldReturn` []
              plotAt ws plotTile `shouldReturn` Just plot

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
  where meta = SaveRequestMeta "crop" "2026-10-31T00:00:00.000000Z" False

saveDataOf ∷ SessionSnapshot → SaveData
saveDataOf = snapshotToSaveData
    (SaveRequestMeta "crop" "2026-10-31T00:00:00.000000Z" False)

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

-- * Lua scene

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "crop_condition_scene"

homeChunk ∷ ChunkCoord
homeChunk = fst (globalToChunk (fst plotTile) (snd plotTile))

-- | Wheat is an ANNUAL groundcover crop whose harvest opens immediately and
--   yields a registered item; clover exists only so a second species is a
--   catalog member. Wheat declares no lifecycle phases, so its growth
--   state is dead exactly when the plot's age falls in the dead window
--   after its 360-day lifespan.
sceneCatalog ∷ FloraCatalog
sceneCatalog = probeCatalog
    { fcSpecies = HM.insert 1 wheat (fcSpecies probeCatalog)
    , fcWorldGen = HM.singleton 1 groundcoverGen }
  where
    wheat = (newFloraSpecies "probe_wheat" (TextureHandle 0))
        { fsLifecycle = Annual
        , fsHarvest = Just FloraHarvest
            { fhTags = [], fhUngatedTags = []
            , fhYield = [("probe_grain", 1, 1)], fhPhaseYields = HM.empty
            , fhRegrowth = 43200, fhHarvestedTexture = TextureHandle 0 } }
    groundcoverGen = FloraWorldGen
        { fwCategory = "groundcover_crop"
        , fwMinTemp = -100, fwMaxTemp = 100, fwIdealTemp = 15
        , fwMinPrecip = 0, fwMaxPrecip = 100, fwIdealPrecip = 50
        , fwMinAlt = -1000, fwMaxAlt = 1000, fwIdealAlt = 0
        , fwMinHumidity = 0, fwMaxHumidity = 1, fwIdealHumidity = 0.5
        , fwMaxSlope = 255, fwDensity = 0, fwSoils = [], fwFootprint = 1 }

sceneItems ∷ ItemManager
sceneItems = ItemManager $ HM.fromList
    [ ("probe_grain", baseItem { idName = "probe_grain"
                               , idFood = Just (ItemFood 50 0) }) ]
  where
    baseItem = ItemDef
        { idName = "", idDisplayName = "Probe Item"
        , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
        , idWeight = 1, idWeightSpec = Nothing, idBulk = 1
        , idStorage = Nothing, idKind = "misc", idCategory = "Misc"
        , idMake = "", idMaterial = "", idQualitySpec = Nothing
        , idQualityTiers = [], idContainer = Nothing
        , idDefaultContents = [], idFood = Nothing, idWeapon = Nothing
        , idArmor = Nothing, idUnequippable = False, idBuffs = []
        , idInsulation = 0, idSourcePath = "test-fixture" }

-- | A page whose every column is TILLED SOIL, so the production planting
--   verb accepts any tile, holding no plots until the example installs
--   them. No gen params, like every bare-page Lua fixture.
resetScene ∷ EngineEnv → IO WorldState
resetScene env = do
    ws ← emptyWorldState
    let area = chunkSize * chunkSize
        col = ColumnTiles
            { ctStartZ = 0, ctMats = VU.replicate 20 1
            , ctSlopes = VU.replicate 20 0
            , ctVeg = VU.replicate 20 vegTilledSoil }
        chunk = LoadedChunk
            { lcCoord = homeChunk, lcTiles = V.replicate area col
            , lcSurfaceMap = VU.replicate area 0
            , lcTerrainSurfaceMap = VU.replicate area 0
            , lcFluidMap = V.replicate area Nothing
            , lcIceMap = emptyIceMap
            , lcFlora = FloraChunkData []
            , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
            , lcMagma = Nothing, lcStructures = emptyChunkStructures }
    writeIORef (wsTilesRef ws) WorldTileData
        { wtdChunks = HM.singleton homeChunk chunk
        , wtdMaxChunks = 200, wtdBounds = Nothing }
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (floraCatalogRef env) sceneCatalog
    writeIORef (itemManagerRef env) sceneItems
    writeIORef (unitManagerRef env) emptyUnitManager
    pure ws

putPlot ∷ WorldState → (Int, Int) → CropPlot → IO ()
putPlot ws tile plot =
    writeIORef (wsCropPlotsRef ws) . plotStoreFromPlots
        . HM.insert tile plot . cpsPlots =≪ readIORef (wsCropPlotsRef ws)

plotAt ∷ WorldState → (Int, Int) → IO (Maybe CropPlot)
plotAt ws tile = HM.lookup tile . cpsPlots <$> readIORef (wsCropPlotsRef ws)

groundNames ∷ WorldState → IO [Text]
groundNames ws = do
    gis ← readIORef (wsGroundItemsRef ws)
    pure $ map snd $ L.sortOn fst
        [ (gid, iiDefName (giInst gi)) | (gid, gi) ← HM.toList (gisItems gis) ]

harvestExpr ∷ (Int, Int) → Text
harvestExpr (gx, gy) = T.concat
    [ "local r = world.harvestFlora(", tshow gx, ", ", tshow gy, "); "
    , "return r and #r or 'nil'" ]

plantResult ∷ LuaBackendState → (Int, Int) → IO Text
plantResult ls (gx, gy) = runOk ls (T.concat
    [ "return tostring(world.plantCropAt(", tshow gx, ", ", tshow gy
    , ", 'probe_wheat'))" ])

runOk ∷ LuaBackendState → Text → IO Text
runOk ls src = do
    r ← T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState ls) src
    r `shouldNotSatisfy` (\t → "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t)
    pure r

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- * Frozen-slice builders

toCropPlotDTO' ∷ SavedCropPlot → CropPlotDTO
toCropPlotDTO' c = CropPlotDTO
    { cpiSpecies = cpSpecies c, cpiPlantedDay = cpPlantedDay c
    , cpiHealth = cpHealth c
    , cpiCondition = toFloraConditionDTO <$> cpCondition c }

-- | The decoded plot, for comparing against 'savedPlot'. Total on the
--   fixtures here, which are all valid.
fromCropPlotDTO' ∷ CropPlotDTO → SavedCropPlot
fromCropPlotDTO' d = CropPlot
    { cpSpecies = cpiSpecies d, cpPlantedDay = cpiPlantedDay d
    , cpHealth = cpiHealth d
    , cpCondition = cpiCondition d
        ≫= either (const Nothing) Just ∘ fromFloraConditionDTO }

emptyActivityV8 ∷ WorldPageId → PageActivityDTO
emptyActivityV8 pid = PageActivityDTO pid HM.empty HM.empty HM.empty
    HM.empty HM.empty HM.empty HM.empty
    (toGroundItemsDTO emptyGroundItems) HM.empty HM.empty HM.empty
    firstConstructAttemptId HM.empty

emptyActivityV7 ∷ WorldPageId → PageActivityDTOv7
emptyActivityV7 pid = PageActivityDTOv7
    { pad7PageId = pid, pad7Mine = HM.empty, pad7Construct = HM.empty
    , pad7Chop = HM.empty, pad7Till = HM.empty, pad7Plant = HM.empty
    , pad7FloraHarvests = HM.empty, pad7CropPlots = HM.empty
    , pad7GroundItems = toGroundItemsDTO emptyGroundItems
    , pad7SpoilPiles = HM.empty, pad7PendingChop = HM.empty
    , pad7PendingHarvests = HM.empty
    , pad7ConstructNextAttempt = firstConstructAttemptId
    , pad7FloraConditions = HM.empty }

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

emptyActivityV4 ∷ WorldPageId → PageActivityDTOv4
emptyActivityV4 pid = PageActivityDTOv4
    { pad4PageId = pid, pad4Mine = HM.empty, pad4Construct = HM.empty
    , pad4Chop = HM.empty, pad4Till = HM.empty, pad4Plant = HM.empty
    , pad4FloraHarvests = HM.empty, pad4CropPlots = HM.empty
    , pad4GroundItems = toGroundItemsDTO emptyGroundItems
    , pad4SpoilPiles = HM.empty, pad4PendingChop = HM.empty
    , pad4PendingHarvests = HM.empty }

emptyActivityV3 ∷ WorldPageId → PageActivityDTOv3
emptyActivityV3 pid = PageActivityDTOv3
    { pad3PageId = pid, pad3Mine = HM.empty, pad3Construct = HM.empty
    , pad3Chop = HM.empty, pad3Till = HM.empty, pad3Plant = HM.empty
    , pad3FloraHarvests = HM.empty, pad3CropPlots = HM.empty
    , pad3GroundItems = GroundItemsDTO 0 HM.empty
    , pad3SpoilPiles = HM.empty }

emptyActivityV2 ∷ WorldPageId → PageActivityDTOv2
emptyActivityV2 pid = PageActivityDTOv2
    { pad2PageId = pid, pad2Mine = HM.empty, pad2Construct = HM.empty
    , pad2Chop = HM.empty, pad2Till = HM.empty, pad2Plant = HM.empty
    , pad2FloraHarvests = HM.empty, pad2CropPlots = HM.empty
    , pad2GroundItems = GroundItemsDTOv1 0 HM.empty
    , pad2SpoilPiles = HM.empty }
