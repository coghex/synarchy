-- | Frozen pre-EFM-3 growth and lookup chain. Do not replace these oracle
-- helpers with production growth/phase helpers: that would let a regression
-- redefine its own expected result.
module Test.Headless.World.FloraVisualOracle (spec) where

import UPrelude
import Test.Hspec
import Data.List (nub, sort, sortOn)
import qualified Data.HashMap.Strict as HM
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlFlora
import Engine.Core.Log (initLogger, defaultLogConfig, LogConfig(..), LogBackend(..))
import World.Flora.Types
import World.Flora.Identity (generatedFloraInstanceId)
import World.Flora.Render (resolveFloraTexture)
import World.Flora.Growth (floraGrowth, fgDead)

spec ∷ Spec
spec = beforeAll shipped $ do
    it "covers every shipped species and living phase on all 360 days" $ \species → do
        length species `shouldBe` 16
        length (filter ((≡ Evergreen) . fsLifecycle) species) `shouldBe` 4
        forM_ species $ \sp → do
            let phases = filter ((≢ PhaseDead) . lpTag) (HM.elems (fsPhases sp))
            phases `shouldSatisfy` (not . null)
            forM_ phases $ \phase → forM_ [0..359] $ \day → do
                -- Pin each living phase on each calendar date by translating
                -- the placement baseline, independently of production helpers.
                let fi = instanceAt (lpAge phase - fromIntegral day)
                assertSame sp day fi
                fgDead (floraGrowth sp day fi) `shouldBe` False
    it "covers all 12 mortal dead windows and leaves the four evergreens alive" $ \species → do
        length (filter ((≢ Evergreen) . fsLifecycle) species) `shouldBe` 12
        forM_ species $ \sp → forM_ [0..359] $ \day →
            case oldLifespan sp (instanceAt 0) of
                Nothing → do
                    let fi = instanceAt 100000
                    fgDead (floraGrowth sp day fi) `shouldBe` False
                    assertSame sp day fi
                Just lifespan → do
                    let fi = instanceAt (lifespan + 10 - fromIntegral day)
                    fgDead (floraGrowth sp day fi) `shouldBe` True
                    assertSame sp day fi

assertSame ∷ FloraSpecies → Int → FloraInstance → Expectation
assertSame sp day fi = do
    let actual = resolveFloraTexture
            (insertSpecies (FloraId 1) sp emptyFloraCatalog) 360 day fi
        expected = oldTexture sp day fi
    unless (actual ≡ expected) $ expectationFailure $
        show (fsName sp, day, fiAge fi, actual, expected)

instanceAt ∷ Float → FloraInstance
instanceAt age = FloraInstance (FloraId 1) 3 7 0.1 (-0.2) 5 age 1 2 10
    (generatedFloraInstanceId "visual-oracle" 3 7 "oracle" 0) False ContextWild

shipped ∷ IO [FloraSpecies]
shipped = do
    logger ← initLogger defaultLogConfig { lcBackend = LogToCallback (\_ → pure ()) }
    files ← sort . filter ((≡ ".yaml") . takeExtension) <$> listDirectory "data/flora"
    defs ← concat <$> mapM (loadFloraYaml logger . ("data/flora" </>)) files
    pure (map fromDef defs)

-- Same declared asset path gets the same synthetic handle, so reused dead.png
-- across seasonal declarations is the same art just as in the texture cache.
fromDef ∷ FloraYamlDef → FloraSpecies
fromDef def = (newFloraSpecies (fydName def) (handle basePath))
    { fsLifecycle = case fydLifecycle def of
        LifecycleEvergreen → Evergreen
        LifecycleAnnual → Annual
        LifecycleBiennial → Biennial
        LifecyclePerennial → Perennial (fromMaybe 1800 (fydMinLife def))
            (fromMaybe 3600 (fydMaxLife def)) (fromMaybe 0.2 (fydDeathChance def))
    , fsPhases = HM.fromList [(fypTag p, LifePhase (fypTag p) (fypAge p)
                                (handle (fypTexture p))) | p ← fydPhases def]
    , fsAnnualCycle = [AnnualStage (fycsTag s) (fycsStartDay s)
                        (handle (fycsTexture s)) | s ← fydAnnualCycle def]
    , fsCycleOverrides = HM.fromList
        [(AnnualCycleKey (fycoPhase o) (fycoCycle o), handle (fycoTexture o))
        | o ← fydCycleOverrides def]
    , fsTextureVariants = HM.fromList
        [(fytvSelector v, handle (fytvTexture v)) | v ← fydTextureVariants def] }
  where
    basePath = case fydPhases def of
        p:_ → fypTexture p
        [] → "matured.png"
    paths = nub $ basePath : map fypTexture (fydPhases def)
        ⧺ map fycsTexture (fydAnnualCycle def) ⧺ map fycoTexture (fydCycleOverrides def)
        ⧺ map fytvTexture (fydTextureVariants def)
    handles = HM.fromList (zip paths (map TextureHandle [1..]))
    handle p = fromMaybe (TextureHandle 0) (HM.lookup p handles)

oldTexture ∷ FloraSpecies → Int → FloraInstance → TextureHandle
oldTexture sp day fi =
    let age = oldAge sp day fi
        eligible = filter ((≤ age) . lpAge) (HM.elems (fsPhases sp))
        phase = case sortOn (oldPhaseOrder . lpTag) eligible of
            [] → Nothing
            xs → Just (last xs)
        phaseTex = maybe (fsBaseTexture sp) lpTexture phase
        stages = sortOn asStartDay (fsAnnualCycle sp)
        active = filter ((≤ day `mod` 360) . asStartDay) stages
        stage = case stages of
            [] → Nothing
            _ → Just (last (if null active then stages else active))
    in case stage of
        Nothing → phaseTex
        Just st → case phase of
            Nothing → asTexture st
            Just ph → HM.lookupDefault (asTexture st)
                (AnnualCycleKey (lpTag ph) (asTag st)) (fsCycleOverrides sp)

oldPhaseOrder ∷ LifePhaseTag → Int
oldPhaseOrder tag = case tag of
    PhaseSprout → 0
    PhaseSeedling → 1
    PhaseVegetating → 2
    PhaseBudding → 3
    PhaseFlowering → 4
    PhaseRipening → 5
    PhaseMatured → 6
    PhaseWithering → 7
    PhaseDead → 8

oldAge ∷ FloraSpecies → Int → FloraInstance → Float
oldAge sp day fi =
    let total = fiAge fi + fromIntegral (max 0 day)
              * (0.25 + 0.75 * max 0 (min 1 (fiHealth fi)))
    in case oldLifespan sp fi of
        Nothing → total
        Just life →
            let len = life + 60
                gen = floor (total / len) ∷ Int
                r = total - fromIntegral gen * len
            in if r < life then r else
                maybe life (max life . lpAge) (HM.lookup PhaseDead (fsPhases sp))

oldLifespan ∷ FloraSpecies → FloraInstance → Maybe Float
oldLifespan sp fi = case fsLifecycle sp of
    Evergreen → Nothing
    Annual → Just 360
    Biennial → Just 720
    Perennial lo hi _ → Just (lo + fraction * max 0 (hi - lo))
  where
    qu = round ((fiOffU fi + 0.5) * 1023) ∷ Word64
    qv = round ((fiOffV fi + 0.5) * 1023) ∷ Word64
    h0 = qu * 2654435761 `xor` (qv * 2246822519)
         `xor` (fromIntegral (fiTileX fi) * 73856093)
         `xor` (fromIntegral (fiTileY fi) * 19349663)
         `xor` (fromIntegral (fiVariant fi) * 83492791)
    h1 = h0 `xor` (h0 `shiftR` 16)
    h2 = h1 * 1640531527
    h3 = h2 `xor` (h2 `shiftR` 13)
    fraction = fromIntegral (h3 ⌃ 0xFFFF) / 65535
