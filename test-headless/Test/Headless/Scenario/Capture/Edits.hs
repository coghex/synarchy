{-# LANGUAGE Strict #-}
-- | A capture of an arena that was EDITED after it loaded (#2714): the
--   document states the arena as it is, no old override returns, nothing is
--   duplicated, pending content stays pending, and what a scenario does not
--   carry (orders, designations) is neither written nor disturbed.
module Test.Headless.Scenario.Capture.Edits (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (atomicModifyIORef', readIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import System.FilePath ((</>))
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.State (EngineEnv(..))
import qualified Item.Ground as G
import Item.Types (ItemInstance(..))
import Location.Instance (LocationContainerSlot(..), LocationInstance(..), instancesToList)
import Power.Types (PowerNode(..), PowerNodes(..))
import Scenario.Load.Session (arenaPageId)
import Scenario.Placement.Install (requestPlacementInstall)
import Scenario.Capture.Types
import Scenario.Placement.Types
import Scenario.Types
import Scenario.World.Install (requestWorldInstall)
import Scenario.World.Types
import Test.Headless.Scenario.Capture.Support
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Load.Persist (integratedDoc)
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import Test.Headless.Scenario.World.Fixtures (floraEntry, terrainPatch, withPaths)
import Unit.Sim.Types (UnitSimState(..), UnitThreadState(..))
import Unit.Types
import World.Chunk.Admit (pageIncarnation)
import World.Generate.Types (WorldGenParams(..))
import World.State.Types

spec ∷ SpecWith H
spec = describe "an edited arena" $ do

    it "writes the arena as it IS: changed terrain, structures, a wet cell gone dry, current flora, empty contents, changed power" $ \h → do
        path ← writeScenario "capture-world-edits.yaml" integratedDoc
        reloadFrom h path
        let env = fxEnv (hFx h)
            wsc = toWorldSimCapability env
        ws ← liveArena h
        epoch ← pageIncarnation ws
        -- the real owners, as an edit would reach them: the terrain, fluid and
        -- flora baseline replaced (the pit is gone, the pad is another material,
        -- the pond is dry, the oak is older), the structure pieces removed
        let pad = terrainPatch "pad" (RegionRect (-3) (-3) 3 3) "sand" 6 Omitted [tag "pad"]
            ridge = terrainPatch "ridge" (RegionTiles [(10, 10), (11, 10)]) "sand" 4 Omitted []
            oak = (floraEntry "oak" "oak" 12 12) { feAge = Authored 300, feTags = [tag "grove"] }
        outcome ← requestWorldInstall wsc arenaPageId epoch
            (ScenarioWorldRequest (withPaths "terrain" [pad, ridge]) [] (withPaths "flora" [oak]))
        outcome `shouldSatisfy` isInstalled
        placed ← requestPlacementInstall wsc arenaPageId epoch (ScenarioPlacementRequest (Just []) Nothing)
        placed `shouldSatisfy` isPlaced
        -- power drawn down, the location emptied of what lay in it
        atomicModifyIORef' (wsPowerNodesRef ws) $ \ns →
            (ns { pnsNodes = HM.map (\n → n { pnStoredWh = 1234 }) (pnsNodes ns) }, ())
        atomicModifyIORef' (wsGroundItemsRef ws) (const (G.emptyGroundItems, ()))
        (dest, bytes, _) ← captureLive h "world-edits"
        sc ← decodeBytes h bytes
        -- terrain: exactly the current baseline, no old override
        map (\p → (tpMaterial p, tpSurfaceZ p)) (scTerrain sc) `shouldMatchList` [("sand", 4), ("sand", 6)]
        [padNow] ← pure [ p | p ← scTerrain sc, tpSurfaceZ p ≡ 6 ]
        tpTags padNow `shouldBe` [tag "pad"]
        scTerrain sc `shouldSatisfy` all ((≢ -2) ∘ tpSurfaceZ)
        -- the wet cell is dry: no fluid is written
        scFluids sc `shouldBe` []
        -- flora: the one plant, at its current age
        map feSpecies (scFlora sc) `shouldBe` ["oak"]
        map feAge (scFlora sc) `shouldBe` [Authored 300]
        -- structures removed; a building at its current charge; the location
        -- stands with no content owed
        scStructures sc `shouldBe` []
        [bat] ← pure [ b | b ← scBuildings sc, beId b ≡ ExplicitId "bat" ]
        bePowerCharge bat `shouldBe` Authored 1234
        map leIncidental (scLocations sc) `shouldBe` [Authored False]
        scGroundItems sc `shouldBe` []
        -- nothing old comes back, and nothing is duplicated, on loading the capture
        before ← countsOf h
        reloadFrom h dest
        countsOf h `shouldReturn` before
        (_, again, _) ← captureLive h "world-edits-again"
        again `shouldBe` bytes

    it "refuses to write an arena whose content the document cannot state, naming it, and writes nothing" $ \h → do
        path ← writeScenario "capture-unstatable.yaml" integratedDoc
        reloadFrom h path
        let env = fxEnv (hFx h)
            wsc = toWorldSimCapability env
        ws ← liveArena h
        epoch ← pageIncarnation ws
        -- the pad rises over its own tagged cells: a tag on a cell below the
        -- surface is something the terrain vocabulary cannot say
        let higher = terrainPatch "pad" (RegionRect (-3) (-3) 3 3) "granite" 9 Omitted [tag "pad"]
        outcome ← requestWorldInstall wsc arenaPageId epoch
            (ScenarioWorldRequest (withPaths "terrain" [higher]) [] [])
        outcome `shouldSatisfy` isInstalled
        dir ← freshDir "unstatable"
        s ← runCapture h (dir </> "out.yaml") False
        case cstOutcome s of
            Just (CaptureUnsuccessful f) → do
                (cfKind f, cfPublication f) `shouldBe` (FailProjection, NotPublished)
                map sdPath (cfDiagnostics f) `shouldBe` ["terrain.tags"]
            other → expectationFailure ("expected a refusal, got " ⧺ show other)
        listDir dir `shouldReturn` []
        readIORef (enginePausedRef env) `shouldReturn` True

    it "keeps a pending container pending: its descriptor is written, its shell never opened" $ \h → do
        path ← writeScenario "capture-pending.yaml" $ doc
            [ "map: {width: 40, height: 40}"
            , "locations:"
            , "  - {id: crypt, definition: crypt_fx, x: -5, y: -5, significant_items: {}, incidental_content: false"
            , "     , containers: {1: {item: box}}}"
            , "ground_items:"
            , "  - {id: box, definition: field_toolbox, x: -5, y: -5}" ]
        reloadFrom h path
        let pendingOf = do
                ws ← liveArena h
                Just params ← readIORef (wsGenParamsRef ws)
                pure [ (lcsRealized c, lcsInstanceId c) | inst ← instancesToList (wgpLocationInstances params), c ← liContainers inst ]
        livePending ← pendingOf
        -- crypt_fx declares two container slots; both are pending
        map fst livePending `shouldBe` [False, False]
        (dest, bytes, _) ← captureLive h "pending"
        sc ← decodeBytes h bytes
        [loc] ← pure (scLocations sc)
        fmap (map (cbRealized ∘ snd) ∘ HM.toList) (authoredMapOf (leContainers loc)) `shouldBe` Just [Authored False]
        -- the shell is written with exactly the contents it has: nothing is
        -- opened, rolled or realized to write it
        [shell] ← pure (scGroundItems sc)
        liveBox ← liveGroundItems h
        fmap (length ∘ iiContents) liveBox `shouldBe` [length (authoredEntries (ieContents (giItem shell)))]
        -- and after loading the capture the slot is still pending, nothing rolled
        reloadFrom h dest
        map fst <$> pendingOf `shouldReturn` [False, False]

    it "leaves orders and designations out of the document and untouched in the arena" $ \h → do
        path ← writeScenario "capture-orders.yaml" integratedDoc
        reloadFrom h path
        let env = fxEnv (hFx h)
        -- a live order on a unit
        um ← readIORef (unitManagerRef env)
        [uid] ← pure [ u | (u, r) ← Map.toList (umScenarioRefs um), r ≡ "medic" ]
        atomicModifyIORef' (utsRef env) $ \uts →
            ( uts { utsSimStates = HM.adjust (\s → s { usLocalPath = [(5, 5), (6, 6)] }) uid (utsSimStates uts) }, () )
        before ← orderState h uid
        before `shouldBe` Just [(5, 5), (6, 6)]
        (_, bytes, _) ← captureLive h "orders"
        -- nothing of an order reaches the document
        sc ← decodeBytes h bytes
        length (scUnits sc) `shouldBe` 2
        orderState h uid `shouldReturn` before
  where
    isInstalled o = case o of WorldInstalled _ → True; _ → False
    isPlaced o = case o of PlacementInstalled _ → True; _ → False
    authoredMapOf a = case a of Authored m → Just m; Omitted → Nothing

liveGroundItems ∷ H → IO [ItemInstance]
liveGroundItems h = do
    ws ← liveArena h
    ground ← readIORef (wsGroundItemsRef ws)
    pure (map G.giInst (HM.elems (G.gisItems ground)))

authoredEntries ∷ Authored [ItemEntry] → [ItemEntry]
authoredEntries = \case
    Authored xs → xs
    Omitted → []

orderState ∷ H → UnitId → IO (Maybe [(Float, Float)])
orderState h uid = do
    uts ← readIORef (utsRef (fxEnv (hFx h)))
    pure (usLocalPath <$> HM.lookup uid (utsSimStates uts))

countsOf ∷ H → IO (Int, Int, Int, Int)
countsOf h = do
    let env = fxEnv (hFx h)
    um ← readIORef (unitManagerRef env)
    ws ← liveArena h
    ground ← readIORef (wsGroundItemsRef ws)
    pure (HM.size (umInstances um), length (Map.toList (umScenarioRefs um)), HM.size (G.gisItems ground), 0 ∷ Int)
