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
module Test.Headless.World.FloraConditionSeam (spec, engineSpec) where

import UPrelude
import Test.Hspec
import Test.QuickCheck
import Data.IORef (readIORef, writeIORef)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
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
import World.Command.Types (WorldCommand)
import World.Flora.Condition
import World.Flora.ConditionSeam
import World.Flora.CropPlot
import World.Flora.Growth
import World.Flora.Identity
import World.Flora.Render
import World.Flora.Types
import World.Flora.Visual
import World.Fluid.Types (emptyIceMap)
import World.Generate.Coordinates (canonicalTile, tileAliasStep)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.Render.FloraDraws (FloraDraw(..), chunkFloraDraws)
import World.State.Types
import World.Thread.Command (handleWorldCommand)
import World.Tile.Types (WorldTileData(..))
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
