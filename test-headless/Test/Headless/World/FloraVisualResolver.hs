module Test.Headless.World.FloraVisualResolver (spec) where

import UPrelude
import Test.Hspec
import Test.QuickCheck
import Data.List (find)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Asset.Handle (TextureHandle(..))
import World.Flora.Types
import World.Flora.Visual
import World.Flora.Growth
import World.Flora.Render
import World.Flora.CropPlot (newCropPlot)
import World.Flora.Identity (generatedFloraInstanceId)
import World.Chunk.Types
import World.Fluid.Types (emptyIceMap)
import Structure.Types (emptyChunkStructures)
import World.Render.FloraDraws (chunkFloraDraws, fdTexture)

base ∷ FloraSpecies
base = newFloraSpecies "synthetic" (TextureHandle 99)

request ∷ FloraVisualSelector
request = floraVisualSelector ContextCultivated (Just PhaseSprout)
    (Just CycleFlowering) ConditionDead (Just CauseFire)

patternFor ∷ Maybe LifePhaseTag → Maybe AnnualStageTag → Maybe FloraDeathCause
           → FloraVariantSelector
patternFor phase stage cause = FloraVariantSelector Nothing phase stage
    (Just ConditionDead) cause

-- The seven semantic dead rungs in the contract's ten-step example.
ladder ∷ [FloraVariantSelector]
ladder =
    [ patternFor (Just PhaseSprout) (Just CycleFlowering) (Just CauseFire)
    , patternFor (Just PhaseSprout) Nothing (Just CauseFire)
    , patternFor (Just PhaseSprout) (Just CycleFlowering) Nothing
    , patternFor (Just PhaseSprout) Nothing Nothing
    , patternFor Nothing Nothing (Just CauseFire)
    , patternFor Nothing (Just CycleFlowering) Nothing
    , patternFor Nothing Nothing Nothing ]

inst ∷ FloraInstance
inst = FloraInstance (FloraId 1) 0 0 0 0 1 0 1 0 1
    (generatedFloraInstanceId "visual-fixture" 0 0 "synthetic" 0) False

catalog ∷ FloraSpecies → FloraCatalog
catalog sp = insertSpecies (FloraId 1) sp emptyFloraCatalog

seasonal ∷ FloraSpecies
seasonal = base
    { fsLifecycle = Perennial 100 100 0
    , fsPhases = HM.fromList
        [ (PhaseSprout, LifePhase PhaseSprout 0 (TextureHandle 1))
        , (PhaseMatured, LifePhase PhaseMatured 80 (TextureHandle 2))
        , (PhaseDead, LifePhase PhaseDead 100 (TextureHandle 3)) ]
    , fsAnnualCycle = [AnnualStage CycleDormant 0 (TextureHandle 4)
                      ,AnnualStage CycleFlowering 100 (TextureHandle 5)]
    , fsCycleOverrides = HM.fromList
        [(AnnualCycleKey PhaseDead CycleDormant, TextureHandle 6)
        ,(AnnualCycleKey PhaseDead CycleFlowering, TextureHandle 7)] }

spec ∷ Spec
spec = do
    it "walks all ten steps, trying cultivated then wild at every semantic rung" $ do
        let keys = [key { fvsContext = Just ctx } | key ← ladder
                   , ctx ← [ContextCultivated, ContextWild]]
            rows = zip keys (map TextureHandle [10..])
            living = base { fsPhases = HM.singleton PhaseSprout
                            (LifePhase PhaseSprout 0 (TextureHandle 50)) }
        forM_ (zip [0..] rows) $ \(n, (expectedKey, expectedTexture)) → do
            let remaining = drop n rows
                sp = living { fsTextureVariants = HM.fromList remaining }
                result = resolveFloraVisual sp request
            ftrTexture result `shouldBe` expectedTexture
            ftrWinner result `shouldBe` VariantCandidate expectedKey
            last (ftrTried result) `shouldBe` ftrWinner result
            -- Every earlier declaration was tried even though removed.
            ftrTried result `shouldSatisfy`
                (\tried → all ((`elem` tried) . VariantCandidate . fst) (take n rows))
        ftrWinner (resolveFloraVisual living request) `shouldBe` LegacyPhase PhaseSprout
        ftrWinner (resolveFloraVisual base request) `shouldBe` SpeciesBase

    it "ranks every permitted mask, including stage plus cause without phase" $ do
        let extra = patternFor Nothing (Just CycleFlowering) (Just CauseFire)
            sp = base { fsTextureVariants = HM.fromList
                [(ladder !! 3, TextureHandle 1), (extra, TextureHandle 2)
                ,(ladder !! 4, TextureHandle 3)] }
        ftrTexture (resolveFloraVisual sp request) `shouldBe` TextureHandle 1
        ftrTexture (resolveFloraVisual (sp { fsTextureVariants =
            HM.delete (ladder !! 3) (fsTextureVariants sp) }) request)
            `shouldBe` TextureHandle 2

    it "uses explicit context before shared context before the wild attempt" $ do
        let key = patternFor (Just PhaseSprout) (Just CycleFlowering) (Just CauseFire)
            row ctx n = (key { fvsContext = ctx }, TextureHandle n)
            sp = base { fsTextureVariants = HM.fromList
                [row (Just ContextWild) 1, row Nothing 2, row (Just ContextCultivated) 3] }
        ftrTexture (resolveFloraVisual sp request) `shouldBe` TextureHandle 3
        ftrTexture (resolveFloraVisual (sp { fsTextureVariants =
            HM.delete (key { fvsContext = Just ContextCultivated }) (fsTextureVariants sp) })
            request) `shouldBe` TextureHandle 2

    it "preserves death priority and declaration-order independence for arbitrary subsets" $
        property $ forAll (sublistOf (zip ladder (map TextureHandle [1..]))) $ \rows →
            forAll (shuffle rows) $ \shuffled →
                let sp rs = seasonal { fsPhases = HM.delete PhaseDead (fsPhases seasonal)
                                     , fsCycleOverrides = HM.empty
                                     , fsTextureVariants = HM.fromList rs }
                    a = resolveFloraVisual (sp rows) request
                    b = resolveFloraVisual (sp shuffled) request
                in a ≡ b ∧ case rows of
                    [] → ftrWinner a ≡ LegacyStage CycleFlowering
                    (_,tex):_ → ftrTexture a ≡ tex

    it "returns the base for every representable selector when nothing is declared" $ do
        let phases = Nothing : map Just [PhaseSprout .. PhaseWithering]
            stages = Nothing : map Just [minBound .. maxBound]
            causes = Nothing : map Just [minBound .. maxBound]
        forM_ [floraVisualSelector ctx phase stage cond cause
              | ctx ← [minBound .. maxBound], phase ← phases, stage ← stages
              , cond ← [minBound .. maxBound]
              , cause ← if cond ≡ ConditionAlive then [Nothing] else causes] $ \sel → do
            let result = resolveFloraVisual base sel
            ftrTexture result `shouldBe` fsBaseTexture base
            ftrWinner result `shouldBe` SpeciesBase
            last (ftrTried result) `shouldBe` SpeciesBase

    it "keeps legacy living precedence override then stage then phase then base" $ do
        let sel = floraVisualSelector ContextWild (Just PhaseSprout)
                    (Just CycleFlowering) ConditionAlive Nothing
            key = AnnualCycleKey PhaseSprout CycleFlowering
            sp = seasonal { fsCycleOverrides = HM.singleton key (TextureHandle 8) }
        ftrTexture (resolveFloraVisual sp sel) `shouldBe` TextureHandle 8
        ftrTexture (resolveFloraVisual (sp { fsCycleOverrides = HM.empty }) sel)
            `shouldBe` TextureHandle 5
        ftrTexture (resolveFloraVisual (sp { fsCycleOverrides = HM.empty
                                         , fsAnnualCycle = [] }) sel)
            `shouldBe` TextureHandle 1
        ftrTexture (resolveFloraVisual base sel) `shouldBe` TextureHandle 99

    it "uses the living variant priority ahead of the legacy stage texture" $ do
        let phase = wildcardVariantSelector { fvsPhase = Just PhaseSprout
                                            , fvsCondition = Just ConditionAlive }
            stage = wildcardVariantSelector { fvsStage = Just CycleFlowering
                                            , fvsCondition = Just ConditionAlive }
            exact = phase { fvsStage = Just CycleFlowering }
            sel = floraVisualSelector ContextWild (Just PhaseSprout)
                    (Just CycleFlowering) ConditionAlive Nothing
            sp = seasonal { fsTextureVariants = HM.fromList
                [(phase, TextureHandle 81), (stage, TextureHandle 82)
                ,(exact, TextureHandle 83)] }
        ftrTexture (resolveFloraVisual sp sel) `shouldBe` TextureHandle 83
        ftrTexture (resolveFloraVisual (sp { fsTextureVariants =
            HM.delete exact (fsTextureVariants sp) }) sel) `shouldBe` TextureHandle 81
        visualPhase (floraVisualSelector ContextWild (Just PhaseDead)
            Nothing ConditionAlive (Just CauseFire)) `shouldBe` Nothing
        visualCause (floraVisualSelector ContextWild (Just PhaseSprout)
            Nothing ConditionAlive (Just CauseFire)) `shouldBe` Nothing

    it "normalizes legacy dead phases and stage overrides to generic dead candidates" $ do
        let dead = floraVisualSelector ContextWild (Just PhaseMatured)
                    (Just CycleFlowering) ConditionDead (Just CauseNatural)
            result = resolveFloraVisual seasonal dead
        ftrTexture result `shouldBe` TextureHandle 7
        ftrWinner result `shouldBe` VariantCandidate
            (patternFor Nothing (Just CycleFlowering) Nothing)
        ftrTexture (resolveFloraVisual (seasonal { fsCycleOverrides = HM.empty }) dead)
            `shouldBe` TextureHandle 3

    it "keeps phase-order semantics even when phase ages are not monotonic" $ do
        let sp = seasonal { fsLifecycle = Evergreen, fsAnnualCycle = []
                          , fsPhases = HM.fromList
                [(PhaseSeedling, LifePhase PhaseSeedling 50 (TextureHandle 8))
                ,(PhaseMatured, LifePhase PhaseMatured 10 (TextureHandle 9))] }
        resolveFloraTexture (catalog sp) 360 60 inst `shouldBe` TextureHandle 9

    it "freezes distinct dead-stage handles across stage and year boundaries" $ do
        forM_ [(1, 100, 99, CycleDormant, 6), (0, 110, 100, CycleFlowering, 7)
              ,(-259, 370, 359, CycleFlowering, 7), (-260, 370, 360, CycleDormant, 6)] $
            \(age, day, death, stage, texture) → do
                let fi = inst { fiAge = age }
                    g = floraGrowth seasonal day fi
                fgDead g `shouldBe` True
                growthDeathDay seasonal fi g `shouldBe` Just death
                growthVisualStage seasonal 360 day fi g `shouldBe` Just stage
                resolveFloraTexture (catalog seasonal) 360 day fi `shouldBe` TextureHandle texture
                growthPhaseTag seasonal g `shouldBe` Just PhaseDead
                growthVisualPhase seasonal fi g `shouldBe` Just PhaseMatured
                harvestOpen seasonal (day `mod` 360) g `shouldBe` False

    it "derives whole death days for fractional thresholds, slow growth, later generations and epoch corpses" $ do
        forM_ [(0.5,1,110,100,0), (0,0,410,400,0)
              ,(0,1,270,260,1), (110,1,0,0,0)] $
            \(age,health,day,death,generation) → do
                let fi = inst { fiAge = age, fiHealth = health }
                    g = floraGrowth seasonal day fi
                fgDead g `shouldBe` True
                fgGeneration g `shouldBe` generation
                growthDeathDay seasonal fi g `shouldBe` Just death
                growthVisualStage seasonal 360 day fi g `shouldBe`
                    activeStageTag seasonal (death `mod` 360)
        let edge = seasonal { fsPhases = HM.insert PhaseWithering
                    (LifePhase PhaseWithering 100 (TextureHandle 55)) (fsPhases seasonal) }
        growthVisualPhase edge inst (floraGrowth edge 100 inst)
            `shouldBe` Just PhaseMatured

    it "derives the first observed death day for arbitrary finite baselines and health" $
        property $ forAll (choose (0, 99)) $ \age →
            forAll (choose (0, 1)) $ \health →
                let fi = inst { fiAge = age, fiHealth = health }
                    firstDead = find (\day → fgDead (floraGrowth seasonal day fi)) [0..400]
                in case firstDead of
                    Nothing → False
                    Just day → growthDeathDay seasonal fi (floraGrowth seasonal day fi)
                                ≡ Just day

    it "uses the actual growth threshold when Float rounding precedes the inverse date" $ do
        let fi = inst { fiAge = 91.12911224365234, fiHealth = 0.2891849875450134 }
            g = floraGrowth seasonal 19 fi
        fgDead (floraGrowth seasonal 18 fi) `shouldBe` False
        fgDead g `shouldBe` True
        growthDeathDay seasonal fi g `shouldBe` Just 19

    it "keeps the CropPlot annual and growth clock elapsed since planting" $ do
        let sp = seasonal { fsLifecycle = Evergreen }
            plot = newCropPlot (FloraId 1) 150 1
        resolveCropPlotTexture (catalog sp) 360 160 plot `shouldBe` TextureHandle 4
        resolveCropPlotTexture (catalog sp) 360 260 plot `shouldBe` TextureHandle 5
        resolveCropPlotTexture (catalog sp) 360 140 plot `shouldBe` TextureHandle 4

    it "preserves the harvested bypass even when dead, and suppresses handle zero" $ do
        let harvest tex = FloraHarvest [] [] [] HM.empty 100 tex
            draws tex day taken = chunkFloraDraws
                (catalog (seasonal { fsHarvest = Just (harvest tex) })) 360 day
                (if taken then HM.singleton (fiInstanceId inst) 1 else HM.empty)
                (ChunkCoord 0 0) (chunkWith inst)
        map fdTexture (draws (TextureHandle 88) 110 True) `shouldBe` [TextureHandle 88]
        map fdTexture (draws (TextureHandle 88) 10 True) `shouldBe` [TextureHandle 88]
        map fdTexture (draws (TextureHandle 0) 110 True) `shouldBe` []
        map fdTexture (draws (TextureHandle 88) 110 False) `shouldBe` [TextureHandle 7]

chunkWith ∷ FloraInstance → LoadedChunk
chunkWith fi = LoadedChunk
    { lcCoord = ChunkCoord 0 0
    , lcTiles = V.replicate 256 (ColumnTiles 0 (VU.singleton 1)
                                (VU.singleton 0) (VU.singleton 0))
    , lcSurfaceMap = VU.replicate 256 0
    , lcTerrainSurfaceMap = VU.replicate 256 0
    , lcFluidMap = V.replicate 256 Nothing
    , lcIceMap = emptyIceMap, lcFlora = FloraChunkData [fi]
    , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
    , lcMagma = Nothing, lcStructures = emptyChunkStructures }
