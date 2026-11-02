{-# LANGUAGE OverloadedStrings #-}
-- | The flora condition mutation/query seam (#2555, EFM-7).
--
--   One world-thread command sets a semantic condition on an occurrence
--   and one clears it; the growth state consults the stored condition
--   through the SAME 'World.Flora.Growth.floraGrowth' entry point every
--   consumer already uses; every render form resolves the condition
--   through the pure resolver; and Lua exposes the seam for scripts and
--   probes only. These examples pin each of those, with synthetic
--   texture handles so an exact and a fallback choice are observable
--   without a GPU:
--
--     * the pure rules — cause vocabulary, corpse-policy selection,
--       retention snapshotting, frozen phase and stage, and the store
--       transitions (refusals change nothing, plot edits preserve every
--       unrelated entry and never resurrect a removed plot);
--     * the growth state — dead, frozen across a calendar-stage change
--       and past the stored expiry, never harvest-open, still chop-
--       eligible where the authored tag is ungated;
--     * the three render forms and the harvest-depletion overlap;
--     * the engine-backed commands and the Lua tables, through the real
--       world-command handlers.
--
--   Selected by @--match "flora condition seam"@.
module Test.Headless.World.FloraConditionSeam
    (spec, engineSpec, corpseRetentionSpec) where

import UPrelude
import Test.Hspec
import Test.QuickCheck
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Engine.ActionOutcome (ActionOutcome(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Engine.Core.Queue as Q
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Data.IORef (newIORef)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)

import Structure.Types (emptyChunkStructures)
import World.Chunk.Types
import World.Chop.Types (newChopDesignation)
import World.Command.Types (WorldCommand)
import World.Flora.Condition
import World.Flora.ConditionSeam
import World.Flora.CropPlot
import World.Flora.Growth
import World.Flora.Identity
import World.Flora.Render
import World.Flora.Types
import World.Flora.Visual
import World.Edit.Apply (applyEdit)
import World.Edit.Types (WorldEdit(..))
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (canonicalTile, tileAliasStep)
import World.Generate.Coordinates (globalToChunk)
import World.Generate.Types
    ( WorldGenParams(..), defaultWorldGenParams )
import World.Page.Types (WorldPageId(..))
import World.Render.FloraDraws (FloraDraw(..), chunkFloraDraws)
import World.State.Types
import World.Thread.Command (handleWorldCommand)
import World.Thread.Command.Flora.Condition (reconcileAdmittedFlora)
import World.Thread.Time (tickWorldTime)
import World.Tile.Types (WorldTileData(..), lookupChunk)
import World.Vegetation (vegTilledSoil)
import World.Time.Types (WorldDate(..))
import Gameplay.Tags.Page (CropPlotStore(..), emptyCropPlotStore)

-- * Synthetic species

-- | A perennial that lives 1000 days (growth rate 1 at full health), with
--   phases sprout/matured/dead, a three-stage annual cycle on a 360-day
--   year, an UNGATED wood harvest, a persistent corpse policy with
--   phase/cause overrides, and one declared dead variant per rung the
--   examples exercise.
speciesTree ∷ FloraSpecies
speciesTree = (newFloraSpecies "seam_tree" (TextureHandle 99))
    { fsLifecycle = Perennial 1000 1000 0
    , fsPhases = HM.fromList
        [ (PhaseSprout,  LifePhase PhaseSprout 0   (TextureHandle 1))
        , (PhaseMatured, LifePhase PhaseMatured 100 (TextureHandle 2))
        , (PhaseDead,    LifePhase PhaseDead 1000  (TextureHandle 3)) ]
    , fsAnnualCycle =
        [ AnnualStage CycleDormant 0 (TextureHandle 4)
        , AnnualStage CycleFlowering 100 (TextureHandle 5)
        , AnnualStage CycleSenescing 200 (TextureHandle 6) ]
    , fsHarvest = Just FloraHarvest
        { fhTags = ["wood", "fruit"], fhUngatedTags = ["wood"]
        , fhYield = [("seam_log", 1, 1)], fhPhaseYields = HM.empty
        , fhRegrowth = 100, fhHarvestedTexture = TextureHandle 88 }
    , fsTextureVariants = HM.fromList
        [ (dead Nothing Nothing, TextureHandle 40)
        , (dead Nothing (Just CauseFire), TextureHandle 41)
        , (dead (Just PhaseSprout) Nothing, TextureHandle 42)
        , ((dead Nothing Nothing) { fvsContext = Just ContextCultivated }
          , TextureHandle 43) ]
    , fsCorpsePolicy = FloraCorpsePolicy
        { fcpOutcome = CorpsePersistent
        , fcpOverrides = HM.fromList
            [ (CorpseOverrideSelector (Just PhaseSprout) Nothing
              , CorpseTransient 60 SuccessorReseed)
            , (CorpseOverrideSelector (Just PhaseSprout) (Just CauseFire)
              , CorpseTransient 10 SuccessorReseed)
            , (CorpseOverrideSelector Nothing (Just CauseDrought)
              , CorpseTransient 5 SuccessorAbsent) ]
        , fcpProvenance = CorpsePolicyAuthored } }
  where dead ph ca = FloraVariantSelector Nothing ph Nothing
                         (Just ConditionDead) ca

-- | A bush: harvest tag gated by the growth window (a fruiting stage), the
--   legacy default corpse policy and only generic dead art.
speciesBush ∷ FloraSpecies
speciesBush = (newFloraSpecies "seam_bush" (TextureHandle 90))
    { fsLifecycle = Perennial 1000 1000 0
    , fsPhases = HM.fromList
        [ (PhaseMatured, LifePhase PhaseMatured 0 (TextureHandle 11))
        , (PhaseDead,    LifePhase PhaseDead 1000 (TextureHandle 12)) ]
    , fsAnnualCycle = [ AnnualStage CycleFruiting 0 (TextureHandle 13) ]
    , fsHarvest = Just FloraHarvest
        { fhTags = ["berries"], fhUngatedTags = []
        , fhYield = [("seam_berry", 1, 1)], fhPhaseYields = HM.empty
        , fhRegrowth = 100, fhHarvestedTexture = TextureHandle 89 } }

-- | A species declaring no phases, no annual cycle and no art but its
--   base: the "base-only fixture" the fallback trace reaches the end for.
speciesBaseOnly ∷ FloraSpecies
speciesBaseOnly = (newFloraSpecies "seam_plain" (TextureHandle 70))
    { fsLifecycle = Evergreen }

treeId, bushId, plainId ∷ FloraId
treeId = FloraId 1
bushId = FloraId 2
plainId = FloraId 3

catalog ∷ FloraCatalog
catalog = emptyFloraCatalog
    { fcSpecies = HM.fromList
        [(1, speciesTree), (2, speciesBush), (3, speciesBaseOnly)]
    , fcNextId = 4 }

-- | A mature, living tile occupant whose clock day is the world day.
instanceOf ∷ FloraId → FloraContext → Int → FloraInstance
instanceOf fid ctx n = FloraInstance
    { fiSpecies = fid, fiTileX = 5, fiTileY = 7, fiOffU = 0, fiOffV = 0
    , fiZ = 4, fiAge = 0, fiHealth = 1, fiVariant = 0, fiBaseWidth = 8
    , fiInstanceId = generatedFloraInstanceId "seam" 5 7 (nameOf fid) n
    , fiChopDesignated = False, fiContext = ctx }

nameOf ∷ FloraId → Text
nameOf fid = maybe "" fsName (lookupSpecies fid catalog)

tree, bush, rowTree ∷ FloraInstance
tree = instanceOf treeId ContextWild 0
bush = instanceOf bushId ContextWild 0
rowTree = instanceOf treeId ContextCultivated 1

daysPerYear ∷ Int
daysPerYear = 360

-- | The record a set builds for @inst@ on @day@.
recordFor ∷ FloraInstance → FloraDeathCause → Int → FloraConditionRecord
recordFor inst cause day =
    buildDeathRecord (fromMaybe (error "species") (lookupSpecies (fiSpecies inst) catalog))
        daysPerYear (5, 7) day day inst cause

gcOf ∷ FloraConditionRecord → Maybe GrowthCondition
gcOf = Just ∘ conditionAtClock 0

-- | A transition the example expects to succeed.
right ∷ Show e ⇒ Either e a → a
right = either (\e → error ("unexpected refusal: " <> show e)) id

-- | The resolver's answer for an occurrence expected to resolve.
resolved ∷ Maybe OccurrenceAppearance → FloraTextureResolution
resolved (Just (AppearanceResolved r)) = r
resolved other = error ("not a resolved appearance: " <> show other)

-- * Pure examples

spec ∷ Spec
spec = describe "flora condition seam" $ do

  describe "the cause vocabulary" $ do
    it "parses exactly the seven tokens, and 'unknown' is a valid cause" $ do
        map (parseDeathCause ∘ deathCauseText) [minBound .. maxBound]
            `shouldBe` map Just [minBound .. maxBound]
        parseDeathCause "unknown" `shouldBe` Just CauseUnknown
        parseDeathCause "arson" `shouldBe` Nothing
        parseDeathCause "" `shouldBe` Nothing
        parseDeathCause "Fire" `shouldBe` Nothing

  describe "retention selection (contract section 7.3)" $ do
    let pick = selectCorpseOutcome (fsCorpsePolicy speciesTree)
    it "prefers phase+cause, then phase, then cause, then the species policy" $ do
        pick (Just PhaseSprout) CauseFire
            `shouldBe` CorpseTransient 10 SuccessorReseed
        pick (Just PhaseSprout) CauseFrost
            `shouldBe` CorpseTransient 60 SuccessorReseed
        pick (Just PhaseMatured) CauseDrought
            `shouldBe` CorpseTransient 5 SuccessorAbsent
        pick (Just PhaseMatured) CauseFire `shouldBe` CorpsePersistent
        pick Nothing CauseFire `shouldBe` CorpsePersistent
        -- Phase beats cause (D-12): a sprout killed by drought takes the
        -- sprout override, not the drought one.
        pick (Just PhaseSprout) CauseDrought
            `shouldBe` CorpseTransient 60 SuccessorReseed
    it "inherits nothing: the legacy default is the 60-day wild reseed" $
        selectCorpseOutcome legacyCorpsePolicy (Just PhaseMatured) CauseFire
            `shouldBe` CorpseTransient 60 SuccessorReseed
    it "keeps an authored whole-day expiry independent of plant health" $ do
        let fullHealth = recordFor (bush { fiHealth = 1 }) CauseFire 5
            lowHealth = recordFor (bush { fiHealth = 0.25 }) CauseFire 5
        fcrRetention fullHealth `shouldBe` RetentionTransient 65 RetainReseed
        fcrRetention lowHealth `shouldBe` RetentionTransient 65 RetainReseed
    it "snapshots an ABSOLUTE expiry, and awaits replanting when cultivated" $ do
        retentionFor ContextWild 150 (CorpseTransient 60 SuccessorAbsent)
            `shouldBe` RetentionTransient 210 RetainAbsent
        retentionFor ContextCultivated 150 (CorpseTransient 60 SuccessorAbsent)
            `shouldBe` RetentionTransient 210 RetainAwaitReplanting
        retentionFor ContextCultivated 150 CorpsePersistent
            `shouldBe` RetentionPersistent

  describe "the death record a set builds" $ do
    it "freezes the phase and annual stage the growth state shows that day" $ do
        let r = recordFor tree CauseFire 150
        fcrPhase r `shouldBe` Just PhaseMatured
        fcrStage r `shouldBe` Just CycleFlowering
        fcrCause r `shouldBe` CauseFire
        fcrBeganDay r `shouldBe` 150
        fcrContext r `shouldBe` ContextWild
        fcrCondition r `shouldBe` ConditionDead
        fcrTile r `shouldBe` (5, 7)
        -- Persistent for a mature tree, whatever killed it.
        fcrRetention r `shouldBe` RetentionPersistent
        fcrRetention (recordFor tree CauseDrought 150)
            `shouldBe` RetentionTransient 155 RetainAbsent
    it "snapshots a cultivated occurrence's successor as awaiting replanting \
       \(D-19)" $
        fcrRetention (recordFor (rowTree { fiAge = 0 }) CauseDrought 150)
            `shouldBe` RetentionTransient 155 RetainAwaitReplanting
    it "freezes the LAST LIVING phase when the occurrence is already inside \
       \its natural dead window" $ do
        let r = recordFor tree CauseNatural 1020
        fcrPhase r `shouldBe` Just PhaseMatured
        conditionRecordProblems (fiInstanceId tree) r `shouldBe` []
    it "always builds a record the shared validity check accepts" $
        property $ forAll (choose (0, 4000)) $ \day →
            forAll (elements [minBound .. maxBound]) $ \cause →
            forAll (elements [tree, rowTree, bush]) $ \inst →
                conditionRecordProblems (fiInstanceId inst)
                    (recordFor inst cause day) ≡ []

  describe "the growth state of a conditioned occurrence" $ do
    let r = recordFor tree CauseFire 150
        living d = floraGrowth Nothing speciesTree d tree
        dead d = floraGrowth (gcOf r) speciesTree d tree
    it "is dead, with the frozen phase and stage, and stops advancing" $ do
        fgDead (dead 150) `shouldBe` True
        growthPhaseTag speciesTree (dead 150) `shouldBe` Just PhaseMatured
        growthReportedStage speciesTree 250 (dead 150) `shouldBe` Just CycleFlowering
        -- A calendar-stage change (flowering -> senescing) and a day far
        -- beyond the natural lifespan wrap change nothing but the
        -- unconditioned control. (This record is PERSISTENT; the stored
        -- transient expiry is exercised by the next example.)
        living 250 `shouldSatisfy` (not ∘ fgDead)
        growthVisualStage speciesTree daysPerYear 250 tree (living 250)
            `shouldBe` Just CycleSenescing
        forM_ [150, 250, 300, 4000, 90000] $ \d → do
            fgDead (dead d) `shouldBe` True
            growthVisualPhase speciesTree tree (dead d) `shouldBe` Just PhaseMatured
            growthVisualStage speciesTree daysPerYear d tree (dead d)
                `shouldBe` Just CycleFlowering
            fgAge (dead d) `shouldBe` fgAge (dead 150)
            fgGeneration (dead d) `shouldBe` fgGeneration (dead 150)
    it "stays frozen past a TRANSIENT record's stored expiry and across a \
       \calendar-stage change" $ do
        let t = recordFor tree CauseDrought 150
            gT d = floraGrowth (gcOf t) speciesTree d tree
        -- The mature tree's drought corpse is transient: expiry 155.
        fcrRetention t `shouldBe` RetentionTransient 155 RetainAbsent
        forM_ [150, 154, 155, 156, 250, 360, 4000, 90000] $ \d → do
            fgDead (gT d) `shouldBe` True
            growthPhaseTag speciesTree (gT d) `shouldBe` Just PhaseMatured
            growthVisualPhase speciesTree tree (gT d) `shouldBe` Just PhaseMatured
            growthVisualStage speciesTree daysPerYear d tree (gT d)
                `shouldBe` Just CycleFlowering
            growthReportedStage speciesTree (d `mod` daysPerYear) (gT d)
                `shouldBe` Just CycleFlowering
            fgAge (gT d) `shouldBe` fgAge (gT 150)
            fgGeneration (gT d) `shouldBe` fgGeneration (gT 150)
        -- Control: the SAME instance, unconditioned, crosses the boundary
        -- (flowering -> senescing) and the stored expiry day.
        growthVisualStage speciesTree daysPerYear 250 tree (living 250)
            `shouldBe` Just CycleSenescing
        growthVisualStage speciesTree daysPerYear 250 tree (gT 250)
            `shouldBe` Just CycleFlowering
    it "freezes a plot on its OWN elapsed clock: across a stage change and \
       \past the absolute expiry day" $ do
        let plot = newCropPlot treeId 100 1.0
            marked = right (setPlotCondition catalog daysPerYear (5, 7) 250
                                CauseDrought (HM.singleton (5, 7) plot))
                         HM.! (5, 7)
            record = fromMaybe (error "plot condition") (cpCondition marked)
            growthAt absDay = floraGrowth (cropPlotGrowthCondition marked)
                speciesTree (cropPlotElapsedDays absDay marked)
                (cropPlotInstance marked)
        -- Died 150 elapsed days after planting (absolute day 250); the
        -- transient cultivated corpse expires on absolute day 255.
        fcrRetention record `shouldBe` RetentionTransient 255 RetainAwaitReplanting
        forM_ [250, 255, 256, 300, 460, 9000, 100000] $ \absDay → do
            let g = growthAt absDay
            fgDead g `shouldBe` True
            growthPhaseTag speciesTree g `shouldBe` Just PhaseMatured
            growthReportedStage speciesTree (absDay `mod` daysPerYear) g
                `shouldBe` Just CycleFlowering
            fgAge g `shouldBe` fgAge (growthAt 250)
            resolveCropPlotTexture catalog daysPerYear absDay marked
                `shouldBe` TextureHandle 43
        -- Control: the unconditioned plot's art moves with ITS clock
        -- (elapsed 150 flowering -> elapsed 360 dormant).
        resolveCropPlotTexture catalog daysPerYear 250 plot
            `shouldBe` TextureHandle 5
        resolveCropPlotTexture catalog daysPerYear 460 plot
            `shouldBe` TextureHandle 4
    it "is the unconditioned lifecycle when there is no condition" $
        property $ forAll (choose (0, 3000)) $ \d →
            floraGrowth Nothing speciesTree d tree
                ≡ (floraGrowth Nothing speciesTree d tree)
                    { fgCondition = Nothing }
    it "freezes for every clock day, not just the sampled ones" $
        property $ forAll (choose (0, 5000)) $ \d →
            let g = dead d
            in fgDead g ∧ growthPhaseTag speciesTree g ≡ Just PhaseMatured
               ∧ growthVisualStage speciesTree daysPerYear d tree g
                    ≡ Just CycleFlowering
    it "closes harvest on a dead bush but keeps the ungated wood tag \
       \chop-eligible on a dead tree" $ do
        let bushDead = floraGrowth (gcOf (recordFor bush CauseFrost 5))
                           speciesBush 5 bush
            treeDead = dead 150
        harvestOpen speciesBush 0 bushDead `shouldBe` False
        floraHarvestAdmits speciesBush (Just "berries") 0 bushDead `shouldBe` False
        floraHarvestAdmits speciesBush Nothing 0 bushDead `shouldBe` False
        -- The living bush is harvest-open in its fruiting window.
        harvestOpen speciesBush 0 (floraGrowth Nothing speciesBush 5 bush)
            `shouldBe` True
        floraHarvestAdmits speciesTree (Just "wood") 0 treeDead `shouldBe` True
        -- The gated tag is refused on the dead tree, like any dead plant.
        floraHarvestAdmits speciesTree (Just "fruit") 0 treeDead `shouldBe` False
    it "uses the occurrence's OWN clock for a plot (rebased onto elapsed days)" $ do
        let plot = (newCropPlot treeId 100 1.0)
            rec' = (recordFor (cropPlotInstance plot) CauseFrost 250)
                       { fcrTile = (5, 7) }
            plotDead = plot { cpCondition = Just rec' }
        -- Died 150 elapsed days after planting.
        fmap gcBeganClockDay (cropPlotGrowthCondition plotDead) `shouldBe` Just 150
        fgDead (floraGrowth (cropPlotGrowthCondition plotDead) speciesTree
                    (cropPlotElapsedDays 9000 plotDead)
                    (cropPlotInstance plotDead)) `shouldBe` True

  describe "the store transitions" $ do
    let setIt inst cause store =
            setOccurrenceCondition catalog daysPerYear (5, 7) 150 inst cause store
    it "sets exactly one occurrence's record and leaves its siblings alone" $ do
        let store = right $ setIt tree CauseFire emptyFloraConditions
        HM.keys store `shouldBe` [fiInstanceId tree]
        lookupFloraCondition (fiInstanceId bush) store `shouldBe` Nothing
    it "refuses a second set without changing the store" $ do
        let store = right $ setIt tree CauseFire emptyFloraConditions
        setIt tree CauseDrought store `shouldBe` Left RefusedAlreadyConditioned
        -- ... and the first record is preserved exactly.
        fmap fcrCause (lookupFloraCondition (fiInstanceId tree) store)
            `shouldBe` Just CauseFire
    it "refuses an occurrence of an unregistered species" $
        setIt (tree { fiSpecies = FloraId 77 }) CauseFire emptyFloraConditions
            `shouldBe` Left RefusedUnknownSpecies
    it "clear removes only the addressed record, idempotently" $ do
        let s1 = right $ setIt tree CauseFire emptyFloraConditions
            s2 = right $ setIt bush CauseFire s1
            s3 = clearOccurrenceCondition (fiInstanceId tree) s2
        HM.keys s3 `shouldBe` [fiInstanceId bush]
        clearOccurrenceCondition (fiInstanceId tree) s3 `shouldBe` s3
    it "temporarily overlays and then restores a wild reseed marker" $ do
        let iid = fiInstanceId tree
            prior = (recordFor tree CauseNatural 800)
                { fcrCondition = ConditionReseeded
                , fcrBeganDay = 780
                , fcrRetention = RetentionTransient 840 RetainReseed }
            initial = HM.singleton iid prior
            explicit = right $ setOccurrenceCondition catalog daysPerYear
                (5, 7) 810 tree CauseFire initial
            under = explicit HM.! iid
        fcrCondition under `shouldBe` ConditionDead
        fcrReseedMarker under `shouldBe` Just FloraReseedMarker
            { frmBeganDay = 780
            , frmPhase = fcrPhase prior
            , frmStage = fcrStage prior
            , frmRetention = RetentionTransient 840 RetainReseed }
        conditionRecordProblems iid under `shouldBe` []
        (clearOccurrenceCondition iid explicit HM.! iid) `shouldBe` prior
    it "round-trips a set/clear on any cause and day to the empty store" $
        property $ forAll (elements [minBound .. maxBound]) $ \cause →
            forAll (choose (0, 4000)) $ \day →
                let s = right $ setOccurrenceCondition catalog daysPerYear
                                  (5, 7) day tree cause emptyFloraConditions
                in clearOccurrenceCondition (fiInstanceId tree) s ≡ emptyFloraConditions

  describe "the plot transitions" $ do
    let plotA = newCropPlot treeId 100 1.0
        plotB = newCropPlot bushId 200 1.0
        tileA = (5, 7)
        tileB = (6, 7)
        plots = HM.fromList [(tileA, plotA), (tileB, plotB)]
    it "marks only the addressed plot and preserves every other entry" $ do
        let after = right $ setPlotCondition catalog daysPerYear tileA 250
                              CauseFire plots
        HM.lookup tileB after `shouldBe` Just plotB
        fmap cpPlantedDay (HM.lookup tileA after) `shouldBe` Just 100
        fmap (fmap fcrCause ∘ cpCondition) (HM.lookup tileA after)
            `shouldBe` Just (Just CauseFire)
        -- The record is a cultivated plot record that validates.
        (HM.lookup tileA after ≫= cpCondition) `shouldSatisfy` \case
            Just c → fcrContext c ≡ ContextCultivated
                   ∧ fcrTile c ≡ tileA ∧ fcrSpecies c ≡ treeId
                   ∧ fcrRetention c ≡ RetentionPersistent
            Nothing → False
    it "builds the record from the plot the GIVEN map holds, so a removed or \
       \replanted plot is never restored from a stale snapshot" $ do
        let live = HM.delete tileA plots
        setPlotCondition catalog daysPerYear tileA 250 CauseFire live
            `shouldBe` Left RefusedUnknownOccurrence
        -- A replacement of the same tile is edited, not the stale plot.
        let replanted = HM.insert tileA (newCropPlot bushId 240 1.0) plots
            after = right $ setPlotCondition catalog daysPerYear tileA 250
                              CauseFire replanted
        fmap cpSpecies (HM.lookup tileA after) `shouldBe` Just bushId
        fmap (fmap fcrSpecies ∘ cpCondition) (HM.lookup tileA after)
            `shouldBe` Just (Just bushId)
    it "refuses a second condition and an unknown tile, changing nothing" $ do
        let once = right $ setPlotCondition catalog daysPerYear tileA 250
                             CauseFire plots
        setPlotCondition catalog daysPerYear tileA 260 CauseFrost once
            `shouldBe` Left RefusedAlreadyConditioned
        setPlotCondition catalog daysPerYear (1, 1) 250 CauseFire plots
            `shouldBe` Left RefusedUnknownOccurrence
    it "clears only the condition, leaving planting day and health" $ do
        let once = right $ setPlotCondition catalog daysPerYear tileA 250
                             CauseFire plots
            cleared = right $ clearPlotCondition tileA once
        cleared `shouldBe` plots
        clearPlotCondition tileA plots `shouldBe` Right plots
        clearPlotCondition (1, 1) plots `shouldBe` Left RefusedUnknownOccurrence

  describe "the three render forms" $ do
    let conds r = HM.singleton (fiInstanceId tree) r
        draw inst conditions harvests day =
            map fdTexture $ chunkFloraDraws catalog daysPerYear day harvests
                conditions (ChunkCoord 0 0) (chunkOf [inst])
        fireRecord = recordFor tree CauseFire 150
    it "draws a wild occurrence's dead art from the record, and living \
       \occurrences exactly as before" $ do
        -- (the legacy annual stage outranks the legacy phase: flowering)
        draw tree HM.empty HM.empty 150 `shouldBe` [TextureHandle 5]
        draw tree (conds fireRecord) HM.empty 150 `shouldBe` [TextureHandle 41]
        -- Frozen: the same dead art long after the calendar stage moved.
        draw tree (conds fireRecord) HM.empty 250 `shouldBe` [TextureHandle 41]
        -- A co-tenant with no record is untouched.
        draw bush (conds fireRecord) HM.empty 150 `shouldBe` [TextureHandle 13]
    it "draws a row occurrence from its CULTIVATED context" $ do
        let r1 = recordFor rowTree CauseDrought 150
            c1 = HM.singleton (fiInstanceId rowTree) r1
        draw rowTree c1 HM.empty 150 `shouldBe` [TextureHandle 43]
        -- Cause outranks context: a fire-killed row crop takes the fire
        -- art, which the cultivated-only art does not outrank.
        draw rowTree (HM.singleton (fiInstanceId rowTree)
                         (recordFor rowTree CauseFire 150)) HM.empty 150
            `shouldBe` [TextureHandle 41]
    it "draws a groundcover plot from its embedded record" $ do
        let plot = newCropPlot treeId 100 1.0
            marked = right $ setPlotCondition catalog daysPerYear (5, 7) 250
                               CauseFire (HM.singleton (5, 7) plot)
            plotDead = marked HM.! (5, 7)
        resolveCropPlotTexture catalog daysPerYear 250 plot
            `shouldBe` TextureHandle 5
        resolveCropPlotTexture catalog daysPerYear 250 plotDead
            `shouldBe` TextureHandle 41
        resolveCropPlotTexture catalog daysPerYear 9000 plotDead
            `shouldBe` TextureHandle 41
    it "falls back to the species' generic dead art, and to its base art" $ do
        let b = recordFor bush CauseFire 5
        draw bush (HM.singleton (fiInstanceId bush) b) HM.empty 5
            `shouldBe` [TextureHandle 12]
        let plain = instanceOf plainId ContextWild 0
            ps = right $ setOccurrenceCondition catalog daysPerYear (5, 7) 5
                           plain CauseUnknown emptyFloraConditions
        draw plain ps HM.empty 5 `shouldBe` [TextureHandle 70]
    it "names the winning candidate and the ordered candidates it tried" $ do
        let res = resolved $ occurrenceAppearance catalog
                daysPerYear 150 (conds fireRecord) HM.empty tree
        ftrTexture res `shouldBe` TextureHandle 41
        ftrWinner res `shouldBe` VariantCandidate
            (FloraVariantSelector Nothing Nothing Nothing
                (Just ConditionDead) (Just CauseFire))
        last (ftrTried res) `shouldBe` ftrWinner res
        length (ftrTried res) `shouldSatisfy` (> 1)
        let plainRes = resolved $ occurrenceAppearance catalog
                daysPerYear 5 HM.empty HM.empty (instanceOf plainId ContextWild 0)
        ftrWinner plainRes `shouldBe` SpeciesBase
    it "lets an explicit condition supersede harvest depletion without \
       \touching the timer, and keeps depletion for the living" $ do
        let harvested = HM.singleton (fiInstanceId tree) 50
        -- Harvest BEFORE the set: stubble while alive, dead art after.
        draw tree HM.empty harvested 150 `shouldBe` [TextureHandle 88]
        draw tree (conds fireRecord) harvested 150 `shouldBe` [TextureHandle 41]
        -- The timer map is an input the seam never writes; a clear brings
        -- the stubble back unchanged.
        draw tree HM.empty harvested 150 `shouldBe` [TextureHandle 88]

-- | One chunk holding the given instances at (5,7).
chunkOf ∷ [FloraInstance] → LoadedChunk
chunkOf insts = LoadedChunk
    { lcCoord = ChunkCoord 0 0
    , lcTiles = V.replicate (chunkSize * chunkSize) col
    , lcSurfaceMap = VU.replicate (chunkSize * chunkSize) 4
    , lcTerrainSurfaceMap = VU.replicate (chunkSize * chunkSize) 4
    , lcFluidMap = V.replicate (chunkSize * chunkSize) Nothing
    , lcIceMap = emptyIceMap, lcFlora = FloraChunkData insts
    , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
    , lcMagma = Nothing, lcStructures = emptyChunkStructures }
  where col = ColumnTiles 0 (VU.replicate 8 1) (VU.replicate 8 0)
                  (VU.replicate 8 0)

-- * Engine-backed examples

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "flora_condition_seam"

worldSize ∷ Int
worldSize = 64

homeChunk ∷ ChunkCoord
homeChunk = ChunkCoord 2 3

homeTile, plotTile ∷ (Int, Int)
homeTile = (2 * chunkSize + 5, 3 * chunkSize + 7)
plotTile = (2 * chunkSize + 9, 3 * chunkSize + 7)

-- | Co-tenants on one tile: a tree, a bush and a plain evergreen.
engineInstance ∷ FloraId → Int → FloraInstance
engineInstance fid n =
    let (_, (lx, ly)) = (homeChunk, (5, 7 ∷ Int))
    in FloraInstance
        { fiSpecies = fid, fiTileX = fromIntegral lx, fiTileY = fromIntegral ly
        , fiOffU = 0, fiOffV = 0, fiZ = 4, fiAge = 0, fiHealth = 1
        , fiVariant = 0, fiBaseWidth = 8
        , fiInstanceId = generatedFloraInstanceId "seamengine"
              (fst homeTile) (snd homeTile) (nameOf fid) n
        , fiChopDesignated = False, fiContext = ContextWild }

engTree, engBush, engPlain ∷ FloraInstance
engTree = engineInstance treeId 0
engBush = engineInstance bushId 0
engPlain = engineInstance plainId 0

-- | The page: resident chunk with the three co-tenants, a registered
--   catalog, the texture-name registry naming the synthetic handles, and
--   a groundcover plot elsewhere.
resetPage ∷ EngineEnv → IO WorldState
resetPage env = do
    ws ← emptyWorldState
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = worldSize })
    -- Mid-year: the annual cycle is in its flowering stage (days 100-199)
    -- and a mature tree is past its sprout phase.
    writeIORef (wsDateRef ws) (WorldDate 1 6 1)
    let ch = (chunkOf [engTree, engBush, engPlain]) { lcCoord = homeChunk }
    writeIORef (wsTilesRef ws) WorldTileData
        { wtdChunks = HM.singleton homeChunk ch, wtdMaxChunks = 200
        , wtdBounds = Nothing }
    writeIORef (wsCropPlotsRef ws) emptyCropPlotStore
        { cpsPlots = HM.singleton plotTile (newCropPlot treeId 0 1.0) }
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (floraCatalogRef env) catalog
    pure ws

-- | Run every command the Lua verbs enqueued, through the real handler.
drainWorldQueue ∷ EngineEnv → IO Int
drainWorldQueue env = do
    logger ← readIORef (loggerRef env)
    let go n = do
            m ← Q.tryReadQueue (wsWorldQueue (toWorldSimCapability env))
            case m of
                Nothing → pure n
                Just (cmd ∷ WorldCommand) →
                    handleWorldCommand env logger cmd ≫ go (n + 1)
    go 0

-- | The action outcomes recorded so far, oldest first, then cleared.
takeOutcomes ∷ EngineEnv → IO [ActionOutcome]
takeOutcomes env = do
    let ref = ucActionOutcomeRef (toUnitCombatCapability env)
    os ← readIORef ref
    writeIORef ref mempty
    pure (toList os)

setLua, clearLua ∷ (Int, Int) → Maybe FloraInstanceId → Text → Text
setLua (gx, gy) mIid cause = T.concat
    [ "return tostring(world.setFloraCondition(", tshow gx, ", ", tshow gy
    , ", ", idArg mIid, ", ", cause, "))" ]
clearLua (gx, gy) mIid _ = T.concat
    [ "return tostring(world.clearFloraCondition(", tshow gx, ", ", tshow gy
    , ", ", idArg mIid, "))" ]

idArg ∷ Maybe FloraInstanceId → Text
idArg = maybe "nil" (tshow ∘ floraInstanceIdToLua)

engineSpec ∷ Spec
engineSpec = beforeAll setup $ describe "flora condition seam" $ do

    it "kills one co-tenant through the Lua verb and the world thread, \
       \leaving its siblings alive and harvestable" $ \(env, ls) → do
        ws ← resetPage env
        evalDebug ls (setLua homeTile (Just (fiInstanceId engTree)) "'fire'")
            `shouldReturn` "true"
        -- Admission is not mutation: nothing has happened yet.
        readIORef (wsFloraConditionsRef ws) `shouldReturn` HM.empty
        drainWorldQueue env `shouldReturn` 1
        conds ← readIORef (wsFloraConditionsRef ws)
        HM.keys conds `shouldBe` [fiInstanceId engTree]
        (fcrCause <$> HM.lookup (fiInstanceId engTree) conds)
            `shouldBe` Just CauseFire
        os ← takeOutcomes env
        map aoOutcome os `shouldBe` ["accepted"]
        map aoKind os `shouldBe` ["flora.condition.set"]
        -- The killed tree reports dead through the growth query; the bush
        -- and the plain evergreen beside it are alive.
        let growthOf name = evalDebug ls (T.concat
              [ "local g = world.getFloraGrowthAt(", tshow (fst homeTile), ", "
              , tshow (snd homeTile), ") for _, e in ipairs(g) do "
              , "if e.id == '", name, "' then return tostring(e.dead) .. '|' "
              , ".. tostring(e.condition) .. '|' .. tostring(e.cause) .. '|' "
              , ".. tostring(e.phase) .. '|' .. tostring(e.phaseAtDeath) .. '|' "
              , ".. tostring(e.stageAtDeath) .. '|' .. tostring(e.context) "
              , ".. '|' .. (e.retention and e.retention.visibility or 'none') "
              , "end end return 'missing'" ])
        growthOf "seam_tree" `shouldReturn`
            "true|dead|fire|matured|matured|flowering|wild|persistent"
        T.isPrefixOf "false|alive|nil" <$> growthOf "seam_bush"
            `shouldReturn` True
        evalDebug ls (T.concat
            [ "local g = world.getFloraGrowthAt(", tshow (fst homeTile), ", "
            , tshow (snd homeTile), ") for _, e in ipairs(g) do "
            , "if e.id == 'seam_bush' then return tostring(e.harvestable) "
            , "end end" ]) `shouldReturn` "true"

    it "refuses harvest on the dead bush and accepts chop on the dead tree" $
        \(env, ls) → do
        _ ← resetPage env
        evalDebug ls (setLua homeTile (Just (fiInstanceId engBush)) "'frost'")
            `shouldReturn` "true"
        evalDebug ls (setLua homeTile (Just (fiInstanceId engTree)) "'fire'")
            `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 2
        -- An ACCEPTED harvest returns a table (empty here: the yield item
        -- is not registered), a REFUSED one returns nil.
        let harvest iid tag = evalDebug ls (T.concat
              [ "local r = world.harvestFloraInstance(", tshow (fst homeTile)
              , ", ", tshow (snd homeTile), ", ", tshow (floraInstanceIdToLua iid)
              , ", '", tag, "') return r == nil and 'nil' or tostring(#r)" ])
        harvest (fiInstanceId engBush) "berries" `shouldReturn` "nil"
        harvest (fiInstanceId engTree) "wood" `shouldReturn` "0"

    it "reports the SELECTED occurrence's dead flag from getFloraAt without \
       \changing its selection" $ \(env, ls) → do
        _ ← resetPage env
        let selected = evalDebug ls (T.concat
              [ "local f = world.getFloraAt(", tshow (fst homeTile), ", "
              , tshow (snd homeTile), ") return f.id .. '|' .. "
              , "tostring(f.dead) .. '|' .. f.condition" ])
        -- Selection prefers a harvest-open harvestable: both are open, so
        -- the first listed (the tree).
        selected `shouldReturn` "seam_tree|false|alive"
        -- Kill the tree: the open bush is now preferred, alive.
        evalDebug ls (setLua homeTile (Just (fiInstanceId engTree)) "'fire'")
            `shouldReturn` "true"
        _ ← drainWorldQueue env
        selected `shouldReturn` "seam_bush|false|alive"
        -- Kill the bush too: nothing is open, so selection falls back to
        -- the first harvestable, the (dead) tree.
        evalDebug ls (setLua homeTile (Just (fiInstanceId engBush)) "'frost'")
            `shouldReturn` "true"
        _ ← drainWorldQueue env
        selected `shouldReturn` "seam_tree|true|dead"

    it "kills a groundcover plot by tile alone and reports it cultivated" $
        \(env, ls) → do
        _ ← resetPage env
        evalDebug ls (setLua plotTile Nothing "'drought'") `shouldReturn` "true"
        _ ← drainWorldQueue env
        evalDebug ls (T.concat
            [ "local p = world.getCropPlotAt(", tshow (fst plotTile), ", "
            , tshow (snd plotTile), ") return tostring(p.dead) .. '|' .. "
            , "p.condition .. '|' .. p.cause .. '|' .. p.context .. '|' .. "
            , "p.retention.visibility .. '|' .. tostring(p.harvestable)" ])
            `shouldReturn` "true|dead|drought|cultivated|transient|false"

    it "canonicalizes tile aliases when addressing" $ \(env, ls) → do
        ws ← resetPage env
        let s = tileAliasStep worldSize
            (gx, gy) = homeTile
        canonicalTile worldSize (gx + s) (gy - s) `shouldBe` homeTile
        evalDebug ls (setLua (gx + s, gy - s) (Just (fiInstanceId engTree))
                          "'natural'") `shouldReturn` "true"
        _ ← drainWorldQueue env
        HM.keys <$> readIORef (wsFloraConditionsRef ws)
            `shouldReturn` [fiInstanceId engTree]

    it "refuses unknown occurrence, repeat, and unknown cause with a named \
       \reason, changing nothing and leaving the cache alone" $ \(env, ls) → do
        ws ← resetPage env
        evalDebug ls (setLua homeTile (Just (fiInstanceId engTree)) "'fire'")
            `shouldReturn` "true"
        _ ← drainWorldQueue env
        _ ← takeOutcomes env
        before ← readIORef (wsFloraConditionsRef ws)
        gen0 ← readIORef (wsQuadCacheGenRef ws)
        let missing = generatedFloraInstanceId "seamengine" 1 1 "seam_tree" 9
        forM_ [ (setLua homeTile (Just (fiInstanceId engTree)) "'frost'"
                , "already has a condition")
              , (setLua homeTile (Just missing) "'fire'", "no such flora")
              , (setLua homeTile (Just (fiInstanceId engBush)) "'arson'"
                , "unknown cause 'arson'")
              , (setLua (3, 3) Nothing "'fire'", "no such flora") ] $
          \(src, reason) → do
            evalDebug ls src `shouldReturn` "true"
            _ ← drainWorldQueue env
            os ← takeOutcomes env
            map aoOutcome os `shouldBe` ["rejected"]
            (aoReason =≪ listToMaybe os) `shouldSatisfy` \case
                Just r → T.isInfixOf reason r
                Nothing → False
        readIORef (wsFloraConditionsRef ws) `shouldReturn` before
        readIORef (wsQuadCacheGenRef ws) `shouldReturn` gen0

    it "returns false, enqueuing nothing, for a malformed request" $
        \(env, ls) → do
        _ ← resetPage env
        mapM (evalDebug ls)
              [ "return tostring(world.setFloraCondition('a', 1, nil, 'fire'))"
              , "return tostring(world.setFloraCondition(1, 1, 0, 'fire'))"
              , "return tostring(world.setFloraCondition(1, 1, '5', 'fire'))"
              , "return tostring(world.setFloraCondition(1, 1, nil, 7))"
              , "return tostring(world.setFloraCondition(1, 1, {}, 'fire'))"
              , "return tostring(world.clearFloraCondition(1, 1, -4))" ]
          `shouldReturn` replicate 6 "false"
        drainWorldQueue env `shouldReturn` 0

    it "clears exactly the addressed record, idempotently, restoring the \
       \clock-derived behaviour" $ \(env, ls) → do
        ws ← resetPage env
        evalDebug ls (setLua homeTile (Just (fiInstanceId engTree)) "'fire'")
            `shouldReturn` "true"
        evalDebug ls (setLua homeTile (Just (fiInstanceId engBush)) "'fire'")
            `shouldReturn` "true"
        _ ← drainWorldQueue env
        _ ← takeOutcomes env
        evalDebug ls (clearLua homeTile (Just (fiInstanceId engTree)) "")
            `shouldReturn` "true"
        evalDebug ls (clearLua homeTile (Just (fiInstanceId engTree)) "")
            `shouldReturn` "true"
        _ ← drainWorldQueue env
        os ← takeOutcomes env
        map aoOutcome os `shouldBe` ["accepted", "noop"]
        HM.keys <$> readIORef (wsFloraConditionsRef ws)
            `shouldReturn` [fiInstanceId engBush]

    it "never touches a regrowth timer: harvest BEFORE the set, then set and \
       \clear, leave the timer exactly as the harvest left it" $
        \(env, ls) → do
        ws ← resetPage env
        let tid = fiInstanceId engTree
            felled = evalDebug ls (T.concat
              [ "local r = world.harvestFloraInstance(", tshow (fst homeTile)
              , ", ", tshow (snd homeTile), ", ", tshow (floraInstanceIdToLua tid)
              , ", 'wood') return r == nil and 'nil' or 'ok'" ])
            timers = readIORef (wsFloraHarvestsRef ws)
        felled `shouldReturn` "ok"
        started ← timers
        HM.member tid started `shouldBe` True
        evalDebug ls (setLua homeTile (Just tid) "'fire'") `shouldReturn` "true"
        _ ← drainWorldQueue env
        timers `shouldReturn` started
        -- Depleted but explicitly dead: the trace says it is the
        -- resolver's dead art, not the stubble, that is drawn.
        appearance ← do
            cat ← readIORef (floraCatalogRef env)
            conds ← readIORef (wsFloraConditionsRef ws)
            pure (occurrenceAppearance cat daysPerYear 151 conds started engTree)
        (appearanceTexture <$> appearance) `shouldBe` Just (TextureHandle 41)
        evalDebug ls (clearLua homeTile (Just tid) "") `shouldReturn` "true"
        _ ← drainWorldQueue env
        timers `shouldReturn` started
        cat ← readIORef (floraCatalogRef env)
        (appearanceTexture <$> occurrenceAppearance cat daysPerYear 151
              HM.empty started engTree) `shouldBe` Just (TextureHandle 88)

    it "traces the winning candidate and the texture name through \
       \world.getFloraVisualAt" $ \(env, ls) → do
        _ ← resetPage env
        let reg = rvTextureNameRegistryRef (toRenderViewCapability env)
        writeIORef reg (HM.fromList
            [ ("flora_variant_seam_tree_condition=dead,cause=fire"
              , TextureHandle 41)
            , ("flora_cycle_seam_tree_flowering", TextureHandle 5) ])
        let visual = evalDebug ls (T.concat
              [ "local v = world.getFloraVisualAt(", tshow (fst homeTile), ", "
              , tshow (snd homeTile), ", "
              , tshow (floraInstanceIdToLua (fiInstanceId engTree))
              , ") return tostring(v.texture) .. '|' .. v.winner .. '|' .. "
              , "tostring(#v.tried > 0)" ])
        visual `shouldReturn` "flora_cycle_seam_tree_flowering|stage:flowering|true"
        evalDebug ls (setLua homeTile (Just (fiInstanceId engTree)) "'fire'")
            `shouldReturn` "true"
        _ ← drainWorldQueue env
        visual `shouldReturn`
            "flora_variant_seam_tree_condition=dead,cause=fire\
            \|variant:condition=dead,cause=fire|true"
        evalDebug ls (T.concat
            [ "return tostring(world.getFloraVisualAt(1, 1, nil))" ])
            `shouldReturn` "nil"
  where
    setup = do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        pure (env, ls)

-- | Production date-command coverage for the EFM-10 calendar boundary.
--   The test deliberately jumps over the death day, the full corpse
--   window and a repeated/backward date assignment.
corpseRetentionSpec ∷ Spec
corpseRetentionSpec = beforeAll setup $ describe "flora corpse retention" $ do
    it "replays exact-ID removal without deleting a same-tile replacement" $
      \_ → do
        let iidA = plantedFloraInstanceId 7
            iidB = plantedFloraInstanceId 8
            x = fst homeTile
            y = snd homeTile
            place iid = WePlaceFloraWithId x y treeId 0 8 iid
            removeA = WeRemoveFlora x y iidA
            replay = foldl' (\chunk edit → applyEdit edit chunk)
                ((chunkOf []) { lcCoord = homeChunk })
                [place iidA, removeA, place iidB, removeA]
        map fiInstanceId (fcdInstances (lcFlora replay)) `shouldBe` [iidB]

    it "keeps the annual's 60-day expiry calendar-based at half health" $ \_ → do
        let annual = speciesBush
                { fsLifecycle = Annual
                , fsCorpsePolicy = FloraCorpsePolicy
                    { fcpOutcome = CorpseTransient 60 SuccessorReseed
                    , fcpOverrides = HM.empty
                    , fcpProvenance = CorpsePolicyAuthored } }
            full = bush { fiHealth = 1 }
            half = bush { fiHealth = 0.5 }
            fullDeath = buildDeathRecord annual daysPerYear (5, 7)
                360 360 full CauseNatural
            halfDeath = buildDeathRecord annual daysPerYear (5, 7)
                576 576 half CauseNatural
        fgDead (floraGrowth Nothing annual 360 full) `shouldBe` True
        fgDead (floraGrowth Nothing annual 576 half) `shouldBe` True
        fcrRetention fullDeath `shouldBe` RetentionTransient 420 RetainReseed
        fcrRetention halfDeath `shouldBe` RetentionTransient 636 RetainReseed

    it "records a half-health row's actual death day after its chunk was evicted" $
      \(env, ls) → do
        ws ← resetPage env
        let annual = speciesBush { fsLifecycle = Annual }
            row = engBush { fiHealth = 0.5, fiContext = ContextCultivated }
        writeIORef (floraCatalogRef env) catalog
            { fcSpecies = HM.insert 2 annual (fcSpecies catalog) }
        td ← readIORef (wsTilesRef ws)
        let chunk = (chunkOf [engTree, row, engPlain]) { lcCoord = homeChunk }
        writeIORef (wsTilesRef ws) td
            { wtdChunks = HM.empty }
        let setDate y m d = do
                evalDebug ls (T.concat
                    [ "world.setDate('flora_condition_seam', ", tshow y, ", "
                    , tshow m, ", ", tshow d, "); return 'queued'" ])
                    `shouldReturn` "queued"
                drainWorldQueue env `shouldReturn` 1
        -- Day 600 is after the half-health annual's natural death on 576,
        -- but its 60-calendar-day expiry on 636 has not arrived.
        setDate 2 9 1
        writeIORef (wsTilesRef ws) td
            { wtdChunks = HM.singleton homeChunk chunk }
        reconcileAdmittedFlora (toWorldSimCapability env) fixturePage ws
            [homeChunk] `shouldReturn` True
        records ← readIORef (wsFloraConditionsRef ws)
        let death = HM.lookup (fiInstanceId row) records
        fmap fcrBeganDay death `shouldBe` Just 576
        fmap fcrRetention death
            `shouldBe` Just (RetentionTransient 636 RetainAwaitReplanting)
        -- The old health-scaled wrap at day 672 cannot replace the
        -- calendar-day expiry or restart the timer.
        setDate 2 11 13
        records' ← readIORef (wsFloraConditionsRef ws)
        HM.lookup (fiInstanceId row) records' `shouldBe` Nothing
        finalTiles ← readIORef (wsTilesRef ws)
        finalChunk ← case lookupChunk homeChunk finalTiles of
            Just ch → pure ch
            Nothing → expectationFailure "the row's chunk should remain loaded"
                        >> pure chunk
        map fiInstanceId (fcdInstances (lcFlora finalChunk))
            `shouldBe` [fiInstanceId engTree, fiInstanceId engPlain]
        edits ← readIORef (wsEditsRef ws)
        any (elem (WeRemoveFlora (fst homeTile) (snd homeTile)
                (fiInstanceId row))) (HM.elems edits) `shouldBe` True

    it "removes an expired row durably while evicted and clears its identity state" $
      \(env, ls) → do
        ws ← resetPage env
        let row = engBush { fiContext = ContextCultivated }
            iid = fiInstanceId row
            setDate y m d = do
                evalDebug ls (T.concat
                    [ "world.setDate('flora_condition_seam', ", tshow y, ", "
                    , tshow m, ", ", tshow d, "); return 'queued'" ])
                    `shouldReturn` "queued"
                drainWorldQueue env `shouldReturn` 1
        tiles ← readIORef (wsTilesRef ws)
        let original = (chunkOf [engTree, row, engPlain]) { lcCoord = homeChunk }
        writeIORef (wsTilesRef ws) tiles
            { wtdChunks = HM.singleton homeChunk original }
        -- The row dies on day 1000 and expires on day 1060.
        setDate 3 10 11
        death ← HM.lookup iid <$> readIORef (wsFloraConditionsRef ws)
        fmap fcrRetention death
            `shouldBe` Just (RetentionTransient 1060 RetainAwaitReplanting)
        writeIORef (wsFloraHarvestsRef ws) (HM.singleton iid 12)
        writeIORef (wsChopDesignationsRef ws)
            (HM.singleton iid (newChopDesignation 1 (fst homeTile) (snd homeTile)))
        writeIORef (wsTilesRef ws) tiles { wtdChunks = HM.empty }
        setDate 3 12 11
        HM.member iid <$> readIORef (wsFloraConditionsRef ws)
            `shouldReturn` False
        HM.member iid <$> readIORef (wsFloraHarvestsRef ws)
            `shouldReturn` False
        HM.member iid <$> readIORef (wsChopDesignationsRef ws)
            `shouldReturn` False
        edits ← readIORef (wsEditsRef ws)
        any (elem (WeRemoveFlora (fst homeTile) (snd homeTile) iid))
            (HM.elems edits) `shouldBe` True
        let replayed = foldl' (flip applyEdit) original
                (HM.lookupDefault [] homeChunk edits)
        map fiInstanceId (fcdInstances (lcFlora replayed))
            `shouldBe` [fiInstanceId engTree, fiInstanceId engPlain]

    it "accepts a seam alias and removes only that addressed identity" $
      \(env, ls) → do
        ws ← resetPage env
        let row = engBush { fiContext = ContextCultivated }
            iid = fiInstanceId row
            alias = ( fst homeTile + tileAliasStep worldSize
                    , snd homeTile - tileAliasStep worldSize )
        tiles ← readIORef (wsTilesRef ws)
        let chunk = (chunkOf [engTree, row, engPlain]) { lcCoord = homeChunk }
        writeIORef (wsTilesRef ws) tiles
            { wtdChunks = HM.singleton homeChunk chunk }
        evalDebug ls (setLua alias (Just iid) "'fire'") `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        set ← HM.lookup iid <$> readIORef (wsFloraConditionsRef ws)
        set `shouldSatisfy` isJust
        evalDebug ls
            "world.setDate('flora_condition_seam', 1, 8, 1); return 'queued'"
            `shouldReturn` "queued"
        drainWorldQueue env `shouldReturn` 1
        edits ← readIORef (wsEditsRef ws)
        HM.lookupDefault [] homeChunk edits
            `shouldContain` [WeRemoveFlora (fst homeTile) (snd homeTile) iid]
        finalTiles ← readIORef (wsTilesRef ws)
        case lookupChunk homeChunk finalTiles of
            Nothing → expectationFailure "resident chunk missing after expiry"
            Just current → map fiInstanceId (fcdInstances (lcFlora current))
                `shouldBe` [fiInstanceId engTree, fiInstanceId engPlain]

    it "applies an explicit wild reseed on the stored expiry boundary" $
      \(env, ls) → do
        ws ← resetPage env
        evalDebug ls
            "world.setDate('flora_condition_seam', 1, 1, 1); return 'queued'"
            `shouldReturn` "queued"
        drainWorldQueue env `shouldReturn` 1
        evalDebug ls (setLua homeTile (Just (fiInstanceId engTree)) "'fire'")
            `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        dead ← HM.lookup (fiInstanceId engTree) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrRetention dead `shouldBe`
            Just (RetentionTransient 10 RetainReseed)
        evalDebug ls
            "world.setDate('flora_condition_seam', 1, 1, 10); return 'queued'"
            `shouldReturn` "queued"
        drainWorldQueue env `shouldReturn` 1
        before ← HM.lookup (fiInstanceId engTree) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrCondition before `shouldBe` Just ConditionDead
        evalDebug ls
            "world.setDate('flora_condition_seam', 1, 1, 11); return 'queued'"
            `shouldReturn` "queued"
        drainWorldQueue env `shouldReturn` 1
        after ← HM.lookup (fiInstanceId engTree) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrCondition after `shouldBe` Just ConditionReseeded
        fmap fcrRetention after `shouldBe`
            Just (RetentionTransient 10 RetainReseed)

    it "keeps an explicit persistent corpse beyond the former growth window" $
      \(env, ls) → do
        ws ← resetPage env
        evalDebug ls (setLua homeTile (Just (fiInstanceId engTree)) "'fire'")
            `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        dead ← HM.lookup (fiInstanceId engTree) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrRetention dead `shouldBe` Just RetentionPersistent
        evalDebug ls
            "world.setDate('flora_condition_seam', 6, 7, 21); return 'queued'"
            `shouldReturn` "queued"
        drainWorldQueue env `shouldReturn` 1
        retained ← HM.lookup (fiInstanceId engTree) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrCondition retained `shouldBe` Just ConditionDead
        case retained of
            Nothing → expectationFailure "persistent condition disappeared"
            Just record → fcrRetention record `shouldBe` RetentionPersistent

    it "replants expired row and plot tiles through their production operations" $
      \(env, ls) → do
        ws ← resetPage env
        let rowId = FloraId 4
            rowSpecies = (newFloraSpecies "seam_retained_row" (TextureHandle 0))
                { fsLifecycle = Annual }
            plotSpecies = (newFloraSpecies "seam_retained_plot" (TextureHandle 0))
                { fsLifecycle = Annual }
            worldGen category = FloraWorldGen
                { fwCategory = category
                , fwMinTemp = -100, fwMaxTemp = 100, fwIdealTemp = 15
                , fwMinPrecip = 0, fwMaxPrecip = 100, fwIdealPrecip = 50
                , fwMinAlt = -1000, fwMaxAlt = 1000, fwIdealAlt = 0
                , fwMinHumidity = 0, fwMaxHumidity = 1, fwIdealHumidity = 0.5
                , fwMaxSlope = 255, fwDensity = 0, fwSoils = [], fwFootprint = 1 }
            setDate = do
                evalDebug ls
                    "world.setDate('flora_condition_seam', 1, 8, 1); return 'queued'"
                    `shouldReturn` "queued"
                drainWorldQueue env `shouldReturn` 1
        writeIORef (floraCatalogRef env) catalog
            { fcSpecies = HM.insert 5 plotSpecies
                        (HM.insert 4 rowSpecies (fcSpecies catalog))
            , fcWorldGen = HM.fromList
                [(4, worldGen "row_crop"), (5, worldGen "groundcover_crop")] }
        writeIORef (wsCropPlotsRef ws) emptyCropPlotStore
        tiles ← readIORef (wsTilesRef ws)
        let chunk0 = (fromMaybe (chunkOf []) (lookupChunk homeChunk tiles))
                { lcFlora = FloraChunkData [] }
            soilTiles = foldl' (\chunk (gx, gy) →
                let (_, (lx, ly)) = globalToChunk gx gy
                    index = ly * chunkSize + lx
                    column = lcTiles chunk V.! index
                    column' = column { ctVeg = VU.replicate
                        (VU.length (ctVeg column)) vegTilledSoil }
                in chunk { lcTiles = lcTiles chunk V.// [(index, column')] })
                chunk0 [homeTile, plotTile]
        writeIORef (wsTilesRef ws) tiles
            { wtdChunks = HM.singleton homeChunk soilTiles }
        let (rowGX, rowGY) = homeTile
            (plotGX, plotGY) = plotTile
        evalDebug ls (T.concat
            [ "return tostring(world.plantRowCropAt('flora_condition_seam', "
            , tshow rowGX, ", ", tshow rowGY, ", 'seam_retained_row'))" ])
            `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        tilesAfterRow ← readIORef (wsTilesRef ws)
        rowChunk ← case lookupChunk homeChunk tilesAfterRow of
            Nothing → expectationFailure "planted row chunk missing"
                        >> pure soilTiles
            Just c → pure c
        let findRow c = listToMaybe (filter ((≡ rowId) ∘ fiSpecies)
                (fcdInstances (lcFlora c)))
        firstRow ← maybe (expectationFailure "row planting did not materialize"
                            >> pure engBush)
            pure (findRow rowChunk)
        evalDebug ls (setLua homeTile (Just (fiInstanceId firstRow)) "'fire'")
            `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        evalDebug ls (T.concat
            [ "return tostring(world.plantCropAt(", tshow plotGX, ", "
            , tshow plotGY, ", 'seam_retained_plot'))" ])
            `shouldReturn` "true"
        evalDebug ls (setLua plotTile Nothing "'fire'") `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        setDate
        HM.member plotTile . cpsPlots <$> readIORef (wsCropPlotsRef ws)
            `shouldReturn` False
        tilesAfterExpiry ← readIORef (wsTilesRef ws)
        let rowAfterExpiry = fromMaybe soilTiles
                (lookupChunk homeChunk tilesAfterExpiry)
        findRow rowAfterExpiry `shouldBe` Nothing
        evalDebug ls (T.concat
            [ "return tostring(world.plantRowCropAt('flora_condition_seam', "
            , tshow rowGX, ", ", tshow rowGY, ", 'seam_retained_row'))" ])
            `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        evalDebug ls (T.concat
            [ "return tostring(world.plantCropAt(", tshow plotGX, ", "
            , tshow plotGY, ", 'seam_retained_plot'))" ])
            `shouldReturn` "true"
        tilesAfterReplant ← readIORef (wsTilesRef ws)
        let secondRowChunk = fromMaybe soilTiles
                (lookupChunk homeChunk tilesAfterReplant)
        secondRow ← maybe (expectationFailure "replanted row missing"
                             >> pure engBush)
            pure (findRow secondRowChunk)
        fiInstanceId secondRow `shouldSatisfy` (> fiInstanceId firstRow)
        secondPlot ← HM.lookup plotTile . cpsPlots <$> readIORef
            (wsCropPlotsRef ws)
        fmap cpCondition secondPlot `shouldBe` Just Nothing

    it "keeps a half-health reseed anchor through explicit clear and its next death" $
      \(env, ls) → do
        ws ← resetPage env
        let annual = speciesBush { fsLifecycle = Annual }
            row = engBush { fiHealth = 0.5 }
            setDate y m d = do
                evalDebug ls (T.concat
                    [ "world.setDate('flora_condition_seam', ", tshow y, ", "
                    , tshow m, ", ", tshow d, "); return 'queued'" ])
                    `shouldReturn` "queued"
                drainWorldQueue env `shouldReturn` 1
            growthAt = do
                growth ← evalDebug ls (T.concat
                    [ "local a = world.getFloraGrowthAt(", tshow (fst homeTile)
                    , ", ", tshow (snd homeTile), ") "
                    , "for _, e in ipairs(a) do if e.id == 'seam_bush' then "
                    , "return tostring(e.dead) .. '|' .. "
                    , "string.format('%.1f', e.age) .. '|' .. "
                    , "tostring(e.generation) end end return 'missing'" ])
                pure growth
        writeIORef (floraCatalogRef env) catalog
            { fcSpecies = HM.insert 2 annual (fcSpecies catalog) }
        tiles ← readIORef (wsTilesRef ws)
        let chunk = (chunkOf [engTree, row, engPlain]) { lcCoord = homeChunk }
        writeIORef (wsTilesRef ws) tiles
            { wtdChunks = HM.insert homeChunk chunk (wtdChunks tiles) }
        -- Half-health annual death is day 576; the fixed 60-day retention
        -- expires at day 636 and establishes generation 1 at age zero.
        setDate 2 8 7
        death ← HM.lookup (fiInstanceId row) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrBeganDay death `shouldBe` Just 576
        setDate 2 10 7
        reseed ← HM.lookup (fiInstanceId row) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrCondition reseed `shouldBe` Just ConditionReseeded
        -- Day 700 has generation 1 at age 40. A temporary explicit record
        -- materializes that anchor before replacing the reseed marker.
        setDate 2 12 11
        growthAt `shouldReturn` "false|40.0|1"
        evalDebug ls (setLua homeTile (Just (fiInstanceId row)) "'fire'")
            `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        evalDebug ls (clearLua homeTile (Just (fiInstanceId row)) "")
            `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        cleared ← HM.lookup (fiInstanceId row) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrCondition cleared `shouldBe` Just ConditionReseeded
        setDate 2 12 12
        growthAt `shouldReturn` "false|40.6|1"
        -- A jump past the old health-scaled dead window must still catch
        -- day 1212, retain through 1272, and anchor generation 2 there.
        setDate 4 8 21
        afterJump ← HM.lookup (fiInstanceId row) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrCondition afterJump `shouldBe` Just ConditionReseeded
        fmap fcrBeganDay afterJump `shouldBe` Just 1212
        fmap fcrRetention afterJump `shouldBe`
            Just (RetentionTransient 1272 RetainReseed)
        growthAt `shouldReturn` "false|23.8|2"
        -- The next death is based on the expiry-day anchor: 1272 + 576.
        setDate 6 2 19
        laterDeath ← HM.lookup (fiInstanceId row) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrBeganDay laterDeath `shouldBe` Just 1848
        fmap fcrRetention laterDeath `shouldBe`
            Just (RetentionTransient 1908 RetainReseed)

    it "catches up a sub-full-health scenario anchor across a long jump" $
      \(env, ls) → do
        ws ← resetPage env
        let annual = speciesBush { fsLifecycle = Annual }
            iid = fiInstanceId engBush
            anchorDay = 100
            row = engBush { fiHealth = 0.5
                          , fiAge = negate (fromIntegral anchorDay
                                               * growthRate 0.5) }
            (gx, gy) = homeTile
            stateEdit = WeSetFloraState gx gy iid (fiZ row) (fiOffU row)
                (fiOffV row) (fiVariant row) (fiHealth row) (fiAge row)
            setDate y m d = do
                evalDebug ls (T.concat
                    [ "world.setDate('flora_condition_seam', ", tshow y, ", "
                    , tshow m, ", ", tshow d, "); return 'queued'" ])
                    `shouldReturn` "queued"
                drainWorldQueue env `shouldReturn` 1
        writeIORef (floraCatalogRef env) catalog
            { fcSpecies = HM.insert 2 annual (fcSpecies catalog) }
        tiles ← readIORef (wsTilesRef ws)
        let chunk = (chunkOf [engTree, row, engPlain]) { lcCoord = homeChunk }
        writeIORef (wsTilesRef ws) tiles
            { wtdChunks = HM.insert homeChunk chunk (wtdChunks tiles) }
        writeIORef (wsEditsRef ws) (HM.singleton homeChunk [stateEdit])
        writeIORef (wsFloraGrowthAnchorsRef ws) (HM.singleton iid anchorDay)
        -- The authored baseline means age zero on day 100 at half health.
        -- Jump from there beyond its day-676 death and day-736 expiry.
        setDate 1 4 11
        setDate 3 7 1
        record ← HM.lookup iid <$> readIORef (wsFloraConditionsRef ws)
        fmap fcrCondition record `shouldBe` Just ConditionReseeded
        fmap fcrBeganDay record `shouldBe` Just 676
        fmap fcrRetention record `shouldBe`
            Just (RetentionTransient 736 RetainReseed)

    it "uses a sub-full-health scenario anchor when an evicted plant returns" $
      \(env, ls) → do
        ws ← resetPage env
        let annual = speciesBush { fsLifecycle = Annual }
            iid = fiInstanceId engBush
            anchorDay = 100
            row = engBush { fiHealth = 0.5
                          , fiAge = negate (fromIntegral anchorDay
                                               * growthRate 0.5) }
            (gx, gy) = homeTile
            stateEdit = WeSetFloraState gx gy iid (fiZ row) (fiOffU row)
                (fiOffV row) (fiVariant row) (fiHealth row) (fiAge row)
            setDate y m d = do
                evalDebug ls (T.concat
                    [ "world.setDate('flora_condition_seam', ", tshow y, ", "
                    , tshow m, ", ", tshow d, "); return 'queued'" ])
                    `shouldReturn` "queued"
                drainWorldQueue env `shouldReturn` 1
        writeIORef (floraCatalogRef env) catalog
            { fcSpecies = HM.insert 2 annual (fcSpecies catalog) }
        tiles ← readIORef (wsTilesRef ws)
        writeIORef (wsTilesRef ws) tiles { wtdChunks = HM.empty }
        writeIORef (wsEditsRef ws) (HM.singleton homeChunk [stateEdit])
        writeIORef (wsFloraGrowthAnchorsRef ws) (HM.singleton iid anchorDay)
        setDate 1 4 11
        setDate 3 7 1
        -- Readmission has no fromDay interval; it must use the saved day
        -- from the flora-condition component to find the earlier cycle.
        let chunk = (chunkOf [engTree, row, engPlain]) { lcCoord = homeChunk }
        latest ← readIORef (wsTilesRef ws)
        writeIORef (wsTilesRef ws) latest
            { wtdChunks = HM.insert homeChunk chunk (wtdChunks latest) }
        reconcileAdmittedFlora (toWorldSimCapability env) fixturePage ws
            [homeChunk] `shouldReturn` True
        record ← HM.lookup iid <$> readIORef (wsFloraConditionsRef ws)
        fmap fcrCondition record `shouldBe` Just ConditionReseeded
        fmap fcrBeganDay record `shouldBe` Just 676
        fmap fcrRetention record `shouldBe`
            Just (RetentionTransient 736 RetainReseed)

    it "reconciles natural death on ordinary time rollover" $ \(env, ls) → do
        ws ← resetPage env
        evalDebug ls
            "world.setDate('flora_condition_seam', 3, 10, 10); return 'queued'"
            `shouldReturn` "queued"
        drainWorldQueue env `shouldReturn` 1
        writeIORef (wsTimeScaleRef ws) 1
        tickWorldTime env 1440
        death ← HM.lookup (fiInstanceId engBush) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrBeganDay death `shouldBe` Just 1000

    it "reconciles a paused page's explicit date assignment" $ \(env, ls) → do
        ws ← resetPage env
        writeIORef (wsEnginePausedRef (toWorldSimCapability env)) True
        evalDebug ls
            "world.setDate('flora_condition_seam', 3, 10, 11); return 'queued'"
            `shouldReturn` "queued"
        drainWorldQueue env `shouldReturn` 1
        death ← HM.lookup (fiInstanceId engBush) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrBeganDay death `shouldBe` Just 1000

    it "naturally expires a cultivated plot, preserves a replacement and other plots" $
      \(env, ls) → do
        ws ← resetPage env
        let transientTree = speciesTree
                { fsCorpsePolicy = FloraCorpsePolicy
                    { fcpOutcome = CorpseTransient 60 SuccessorAbsent
                    , fcpOverrides = HM.empty
                    , fcpProvenance = CorpsePolicyAuthored } }
            unrelatedTile = (fst plotTile + 1, snd plotTile)
            unrelated = newCropPlot plainId 0 1
        writeIORef (floraCatalogRef env) catalog
            { fcSpecies = HM.insert 1 transientTree (fcSpecies catalog) }
        atomicModifyIORef' (wsCropPlotsRef ws) $ \store →
            (store { cpsPlots = HM.insert unrelatedTile unrelated (cpsPlots store) }
            , ())
        let setDate y m d = do
                evalDebug ls (T.concat
                    [ "world.setDate('flora_condition_seam', ", tshow y, ", "
                    , tshow m, ", ", tshow d, "); return 'queued'" ])
                    `shouldReturn` "queued"
                drainWorldQueue env `shouldReturn` 1
        setDate 3 10 11
        plots ← cpsPlots <$> readIORef (wsCropPlotsRef ws)
        let record = cpCondition =<< HM.lookup plotTile plots
        fmap fcrBeganDay record `shouldBe` Just 1000
        fmap fcrRetention record
            `shouldBe` Just (RetentionTransient 1060 RetainAwaitReplanting)
        setDate 3 12 10
        HM.member plotTile . cpsPlots <$> readIORef (wsCropPlotsRef ws)
            `shouldReturn` True
        -- Replant the same tile before the expiry boundary. The stale
        -- corpse must not delete this fresh plot when the date crosses 1060.
        atomicModifyIORef' (wsCropPlotsRef ws) $ \store →
            (store { cpsPlots = HM.insert plotTile (newCropPlot treeId 1000 1)
                    (cpsPlots store) }, ())
        setDate 3 12 11
        plots' ← readIORef (wsCropPlotsRef ws)
        HM.lookup plotTile (cpsPlots plots')
            `shouldBe` Just (newCropPlot treeId 1000 1)
        HM.lookup unrelatedTile (cpsPlots plots') `shouldBe` Just unrelated

    it "matches sequential boundaries across a long date jump" $ \(env, ls) → do
        let setDate y m d = do
                evalDebug ls (T.concat
                    [ "world.setDate('flora_condition_seam', ", tshow y, ", "
                    , tshow m, ", ", tshow d, "); return 'queued'" ])
                    `shouldReturn` "queued"
                drainWorldQueue env `shouldReturn` 1
            growthAt = evalDebug ls (T.concat
                [ "local a = world.getFloraGrowthAt(", tshow (fst homeTile), ", "
                , tshow (snd homeTile), ") for _, e in ipairs(a) do "
                , "if e.id == 'seam_bush' then return tostring(e.dead) .. '|' .. "
                , "string.format('%.1f', e.age) .. '|' .. "
                , "tostring(e.generation) end end return 'missing'" ])
        _ ← resetPage env
        setDate 3 10 11
        setDate 3 12 11
        setDate 6 9 21
        setDate 6 11 21
        setDate 7 12 11
        sequential ← growthAt
        wsJump ← resetPage env
        -- Day 2500 crosses two death/expiry pairs in one command. It must
        -- reach the same current generation and age as the stepped path.
        setDate 7 12 11
        growthAt `shouldReturn` sequential
        sequential `shouldBe` "false|380.0|2"
        edits ← readIORef (wsEditsRef wsJump)
        let anchors =
                [ ()
                | pageEdits ← HM.elems edits
                , WeSetFloraState _ _ iid _ _ _ _ _ _ ← pageEdits
                , iid ≡ fiInstanceId engBush ]
        length anchors `shouldSatisfy` (≤ 1)

    it "keeps a reseed anchor when a temporary explicit condition is cleared" $
      \(env, ls) → do
        ws ← resetPage env
        let setDate y m d = do
                evalDebug ls (T.concat
                    [ "world.setDate('flora_condition_seam', ", tshow y, ", "
                    , tshow m, ", ", tshow d, "); return 'queued'" ])
                    `shouldReturn` "queued"
                drainWorldQueue env `shouldReturn` 1
        setDate 3 10 11
        setDate 3 12 11
        setDate 3 12 12
        let before = do
                growth ← evalDebug ls (T.concat
                    [ "local a = world.getFloraGrowthAt(", tshow (fst homeTile), ", "
                    , tshow (snd homeTile), ") for _, e in ipairs(a) do "
                    , "if e.id == 'seam_bush' then return string.format('%.1f', e.age) .. '|' .. "
                    , "tostring(e.generation) end end return 'missing'" ])
                pure growth
        before `shouldReturn` "1.0|1"
        evalDebug ls (setLua homeTile (Just (fiInstanceId engBush)) "'fire'")
            `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        evalDebug ls (clearLua homeTile (Just (fiInstanceId engBush)) "")
            `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        after ← before
        after `shouldBe` "1.0|1"
        cleared ← HM.lookup (fiInstanceId engBush) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrCondition cleared `shouldBe` Just ConditionReseeded

    it "does not advance a hidden page when another page's date changes" $
      \(env, ls) → do
        visible ← resetPage env
        hidden ← emptyWorldState
        writeIORef (wsGenParamsRef hidden)
            (Just defaultWorldGenParams { wgpWorldSize = worldSize })
        writeIORef (wsDateRef hidden) (WorldDate 1 6 1)
        let hiddenId = WorldPageId "flora_hidden_calendar"
        atomicModifyIORef' (worldManagerRef env) $ \manager →
            (manager { wmWorlds = (hiddenId, hidden) : wmWorlds manager }, ())
        evalDebug ls
            "world.setDate('flora_hidden_calendar', 3, 10, 11); return 'queued'"
            `shouldReturn` "queued"
        drainWorldQueue env `shouldReturn` 1
        readIORef (wsDateRef visible) `shouldReturn` WorldDate 1 6 1
        readIORef (wsDateRef hidden) `shouldReturn` WorldDate 3 10 11

    it "keeps equal planted identities independent on separate pages" $
      \(env, ls) → do
        visible ← resetPage env
        hidden ← emptyWorldState
        let iid = plantedFloraInstanceId 1
            planted = engBush { fiInstanceId = iid
                              , fiContext = ContextCultivated }
            hiddenPage = WorldPageId "flora_equal_planted_id"
            chunk = (chunkOf [planted]) { lcCoord = homeChunk }
        visibleTiles0 ← readIORef (wsTilesRef visible)
        writeIORef (wsTilesRef visible) visibleTiles0
            { wtdChunks = HM.singleton homeChunk chunk }
        writeIORef (wsGenParamsRef hidden)
            (Just defaultWorldGenParams { wgpWorldSize = worldSize })
        writeIORef (wsTilesRef hidden) WorldTileData
            { wtdChunks = HM.singleton homeChunk chunk
            , wtdMaxChunks = 200, wtdBounds = Nothing }
        writeIORef (worldManagerRef env) emptyWorldManager
            { wmWorlds = [(fixturePage, visible), (hiddenPage, hidden)]
            , wmVisible = [fixturePage] }
        visibleTiles ← readIORef (wsTilesRef visible)
        hiddenTiles ← readIORef (wsTilesRef hidden)
        let visibleChunk = fromMaybe chunk (lookupChunk homeChunk visibleTiles)
            hiddenChunk = fromMaybe chunk (lookupChunk homeChunk hiddenTiles)
            visibleIid = map fiInstanceId (fcdInstances (lcFlora visibleChunk))
            hiddenIid = map fiInstanceId (fcdInstances (lcFlora hiddenChunk))
        visibleIid `shouldBe` [iid]
        hiddenIid `shouldBe` visibleIid
        evalDebug ls (setLua homeTile (Just iid) "'fire'")
            `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        HM.member iid <$> readIORef (wsFloraConditionsRef visible)
            `shouldReturn` True
        HM.member iid <$> readIORef (wsFloraConditionsRef hidden)
            `shouldReturn` False

    it "snapshots natural death and expires a wild reseed on calendar days" $
      \(env, ls) → do
        ws ← resetPage env
        let setDate y m d = do
                evalDebug ls (T.concat
                    [ "world.setDate('flora_condition_seam', "
                    , tshow y, ", ", tshow m, ", ", tshow d
                    , "); return 'queued'" ]) `shouldReturn` "queued"
                drained ← drainWorldQueue env
                drained `shouldSatisfy` (≥ 1)
            condition iid = do
                records ← readIORef (wsFloraConditionsRef ws)
                pure (fcrCondition <$> HM.lookup iid records)
            growthAt name = evalDebug ls (T.concat
                [ "local a = world.getFloraGrowthAt(", tshow (fst homeTile)
                , ", ", tshow (snd homeTile), ") "
                , "for _, e in ipairs(a) do if e.id == '", name
                , "' then return tostring(e.dead) .. '|' .. "
                , "string.format('%.1f', e.age) .. '|' .. "
                , "tostring(e.generation) end end "
                , "return 'missing'" ])
        -- Day 999 is before the 1000-day perennial death boundary.
        setDate 3 10 10
        growthAt "seam_bush" `shouldReturn` "false|999.0|0"
        -- The command jumps directly to day 1000. It must record the
        -- natural death and its 60-calendar-day expiry once.
        setDate 3 10 11
        records ← readIORef (wsFloraConditionsRef ws)
        let bushDeath = HM.lookup (fiInstanceId engBush) records
        fmap fcrCondition bushDeath `shouldBe` Just ConditionDead
        fmap fcrRetention bushDeath
            `shouldBe` Just (RetentionTransient 1060 RetainReseed)
        growthAt "seam_bush" `shouldReturn` "true|1000.0|0"
        -- The day before expiry is still the frozen corpse.
        setDate 3 12 10
        condition (fiInstanceId engBush) `shouldReturn` Just ConditionDead
        -- At expiry, it is generation 1 at age zero, based on day 1060.
        setDate 3 12 11
        condition (fiInstanceId engBush) `shouldReturn` Just ConditionReseeded
        growthAt "seam_bush" `shouldReturn` "false|0.0|1"
        -- A backward assignment cannot recreate the corpse or execute a
        -- second successor; replaying the expiry date leaves its anchor.
        setDate 3 12 10
        condition (fiInstanceId engBush) `shouldReturn` Just ConditionReseeded
        setDate 3 12 11
        growthAt "seam_bush" `shouldReturn` "false|0.0|1"
        stored ← HM.lookup (fiInstanceId engBush) <$> readIORef
            (wsFloraConditionsRef ws)
        case stored of
            Nothing → expectationFailure "reseed marker missing before replay"
            Just record → case S.decode (S.encode record) of
                Left err → expectationFailure err
                Right restored → do
                    current ← readIORef (wsFloraConditionsRef ws)
                    writeIORef (wsFloraConditionsRef ws) $ HM.insert
                        (fiInstanceId engBush) restored current
                    tiles ← readIORef (wsTilesRef ws)
                    let regenerated =
                            (chunkOf [engBush, engTree]) { lcCoord = homeChunk }
                    writeIORef (wsTilesRef ws) tiles
                        { wtdChunks = HM.insert homeChunk regenerated
                            (wtdChunks tiles) }
                    reconcileAdmittedFlora (toWorldSimCapability env)
                        fixturePage ws [homeChunk] `shouldReturn` False
                    growthAt "seam_bush" `shouldReturn` "false|0.0|1"
        -- Mature tree policy is persistent, so the same day jump does
        -- not wrap it into another generation.
        condition (fiInstanceId engTree) `shouldReturn` Just ConditionDead
        growthAt "seam_tree" `shouldReturn` "true|1000.0|0"

    it "keeps natural-death policy and frozen phase/stage after content edits" $
      \(env, ls) → do
        ws ← resetPage env
        let setDate y m d = do
                evalDebug ls (T.concat
                    [ "world.setDate('flora_condition_seam', ", tshow y, ", "
                    , tshow m, ", ", tshow d, "); return 'queued'" ])
                    `shouldReturn` "queued"
                drained ← drainWorldQueue env
                drained `shouldSatisfy` (≥ 1)
            iid = fiInstanceId engBush
        setDate 3 10 11
        before ← HM.lookup iid <$> readIORef (wsFloraConditionsRef ws)
        before `shouldSatisfy` isJust
        frozen ← maybe (expectationFailure "natural death record missing"
                            >> pure (recordFor engBush CauseNatural 1000))
                       pure before
        fcrCause frozen `shouldBe` CauseNatural
        fcrRetention frozen `shouldBe` RetentionTransient 1060 RetainReseed
        fcrPhase frozen `shouldBe` Just PhaseMatured
        fcrStage frozen `shouldBe` Just CycleFruiting
        let changed = speciesBush { fsCorpsePolicy = FloraCorpsePolicy
                { fcpOutcome = CorpsePersistent
                , fcpOverrides = HM.empty
                , fcpProvenance = CorpsePolicyAuthored } }
        writeIORef (floraCatalogRef env) catalog
            { fcSpecies = HM.insert 2 changed (fcSpecies catalog) }
        restored ← case S.decode (S.encode frozen) of
            Left err → expectationFailure err >> pure frozen
            Right record → pure record
        writeIORef (wsFloraConditionsRef ws) (HM.singleton iid restored)
        tiles ← readIORef (wsTilesRef ws)
        writeIORef (wsTilesRef ws) tiles
            { wtdChunks = HM.singleton homeChunk
                ((chunkOf [engBush]) { lcCoord = homeChunk }) }
        reconcileAdmittedFlora (toWorldSimCapability env)
            fixturePage ws [homeChunk] `shouldReturn` False
        HM.lookup iid <$> readIORef (wsFloraConditionsRef ws)
            `shouldReturn` Just restored
        setDate 3 12 11
        after ← HM.lookup iid <$> readIORef (wsFloraConditionsRef ws)
        after `shouldBe` Just (frozen { fcrCondition = ConditionReseeded })

    it "applies absent as a durable non-queryable successor" $ \(env, ls) → do
        ws ← resetPage env
        evalDebug ls (setLua homeTile (Just (fiInstanceId engTree)) "'drought'")
            `shouldReturn` "true"
        drained ← drainWorldQueue env
        drained `shouldSatisfy` (≥ 1)
        let setDate = do
                evalDebug ls
                    "world.setDate('flora_condition_seam', 1, 6, 6); return 'queued'"
                    `shouldReturn` "queued"
                drained ← drainWorldQueue env
                drained `shouldSatisfy` (≥ 1)
        -- The tree's authored drought override expires at day 155.
        evalDebug ls
            "world.setDate('flora_condition_seam', 1, 6, 5); return 'queued'"
            `shouldReturn` "queued"
        drainWorldQueue env `shouldReturn` 1
        beforeExpiry ← HM.lookup (fiInstanceId engTree) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrCondition beforeExpiry `shouldBe` Just ConditionDead
        setDate
        records ← readIORef (wsFloraConditionsRef ws)
        fmap fcrCondition (HM.lookup (fiInstanceId engTree) records)
            `shouldBe` Just ConditionAbsent
        evalDebug ls
            "world.setDate('flora_condition_seam', 1, 6, 7); return 'queued'"
            `shouldReturn` "queued"
        drainWorldQueue env `shouldReturn` 1
        afterExpiry ← HM.lookup (fiInstanceId engTree) <$> readIORef
            (wsFloraConditionsRef ws)
        fmap fcrCondition afterExpiry `shouldBe` Just ConditionAbsent
        evalDebug ls (T.concat
            [ "local a = world.getFloraGrowthAt(", tshow (fst homeTile)
            , ", ", tshow (snd homeTile), ") for _, e in ipairs(a) do "
            , "if e.instanceId == ", tshow (floraInstanceIdToLua
                    (fiInstanceId engTree)), " then return 'present' end end "
            , "return 'absent'" ]) `shouldReturn` "absent"
        stored ← HM.lookup (fiInstanceId engTree) <$> readIORef
            (wsFloraConditionsRef ws)
        case stored of
            Nothing → expectationFailure "absent marker missing before replay"
            Just record → case S.decode (S.encode record) of
                Left err → expectationFailure err
                Right restored → do
                    current ← readIORef (wsFloraConditionsRef ws)
                    writeIORef (wsFloraConditionsRef ws) $ HM.insert
                        (fiInstanceId engTree) restored current
                    tiles ← readIORef (wsTilesRef ws)
                    let regenerated = (chunkOf [engTree]) { lcCoord = homeChunk }
                    writeIORef (wsTilesRef ws) tiles
                        { wtdChunks = HM.insert homeChunk regenerated
                            (wtdChunks tiles) }
                    reconcileAdmittedFlora (toWorldSimCapability env)
                        fixturePage ws [homeChunk] `shouldReturn` False
                    evalDebug ls (T.concat
                        [ "local a = world.getFloraGrowthAt("
                        , tshow (fst homeTile), ", ", tshow (snd homeTile)
                        , ") or {}; for _, e in ipairs(a) do if e.instanceId == "
                        , tshow (floraInstanceIdToLua (fiInstanceId engTree))
                        , " then return 'present' end end return 'absent'" ])
                        `shouldReturn` "absent"
        -- Natural and explicit deaths are not clearable after they have
        -- crossed into a durable successor.
        evalDebug ls (clearLua homeTile (Just (fiInstanceId engTree)) "")
            `shouldReturn` "true"
        drainWorldQueue env `shouldReturn` 1
        afterClear ← readIORef (wsFloraConditionsRef ws)
        fmap fcrCondition (HM.lookup (fiInstanceId engTree) afterClear)
            `shouldBe` Just ConditionAbsent
  where
    setup = do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        ls ← newBareLuaBackend env
        pure (env, ls)

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls src = T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState ls) src
