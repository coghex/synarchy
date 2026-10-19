{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Scenario identity in ordinary saves (#2712): the building and
--   location reference mappings that @buildings@ v4 and @world-pages@ v16
--   carry, as pure codec, integrity and restore rules — no engine. The
--   engine-backed save → stage → recapture round trip is the
--   @Scenario.Placement@ group's own; this is the wire's contract.
module Test.Headless.Scenario.Placement.Persistence (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import qualified Data.Text as T
import Building.Types
    ( BuildingId(..), BuildingManager(..), removeBuildingQuietly, retirePageBuildings )
import Test.Headless.Building.Placement (mkDef)
import Location.Instance
import Test.Headless.World.Save.Components.Fixture
    (minimalBuildingInstance, minimalGlobals, minimalPage, minimalSimState, minimalUnitInstance, significantOwner)
import Unit.Types (UnitId(..))
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Component.EntitySnapshots
    ( BuildingsDTO(..), BuildingsDTOv3(..), PageBuildingsDTO(..), PageBuildingsDTOv3(..)
    , buildingScenarioRefErrors, buildingsCodec )
import World.Save.Component.PageCore (locationScenarioRefErrors)
import World.Save.Component.Types (ComponentCodec(..))
import World.Save.Envelope (decodeSessionEnvelope, encodeSessionSnapshot)
import World.Save.Integrity (IntegrityError(..), sessionIntegrityErrors)
import World.Save.Snapshot
    ( LiveCameraSnapshot(..), PageSnapshot(..), SessionGlobals(..), SessionSnapshot(..)
    , captureSessionSnapshot )
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotSaveMetadata)
import World.Save.Types (BuildingInstanceSnapshot(..), BuildingSnapshot(..), UnitSnapshot(..), fromBuildingSnapshot, toUnitSnapshot)
import Unit.Types (emptyUnitManager)

pageA, pageB ∷ WorldPageId
pageA = WorldPageId "a"
pageB = WorldPageId "b"

globals ∷ [WorldPageId] → SessionGlobals
globals ps = minimalGlobals
    { sgActivePage = pageA, sgVisiblePages = ps, sgNextItemId = 1, sgNextUnitId = 100000
    , sgLiveCamera = (sgLiveCamera minimalGlobals) { lcsOwnerPage = Just pageA } }

-- | A page holding one building (id @bid@) with scenario reference @ref@.
withBuilding ∷ WorldPageId → Word32 → Text → PageSnapshot
withBuilding pid bid ref = (minimalPage pid)
    { pgsBuildings = BuildingSnapshot
        { bsnInstances = HM.singleton (BuildingId bid) (minimalBuildingInstance [])
        , bsnNextId = bid + 1, bsnTags = Map.empty
        , bsnScenarioRefs = Map.singleton (BuildingId bid) ref } }

-- | A page holding one location (the fixture's @ruin_small@, instance id
--   1) with scenario reference @ref@.
withLocation ∷ WorldPageId → Text → PageSnapshot
withLocation pid ref =
    let page = minimalPage pid
        params = pgsGenParams page
        lis = wgpLocationInstances params
        inst = significantOwner []
    in page { pgsGenParams = params { wgpLocationInstances = lis
                { lisById = HM.singleton (liId inst) inst
                , lisNextId = 2
                , lisScenarioRefs = Map.singleton (liId inst) ref } } }

withUnit ∷ WorldPageId → Word32 → Text → PageSnapshot
withUnit pid uid ref =
    let page = minimalPage pid
        snap = (toUnitSnapshot pid emptyUnitManager)
            { usnInstances = HM.singleton (UnitId (fromIntegral uid)) (minimalUnitInstance [])
            , usnScenarioRefs = Map.singleton (UnitId (fromIntegral uid)) ref }
    in page { pgsUnits = snap
            , pgsUnitSimStates = HM.singleton (UnitId (fromIntegral uid)) minimalSimState }

snapshotOf ∷ [PageSnapshot] → IO SessionSnapshot
snapshotOf ps = either (fail ∘ show) pure (captureSessionSnapshot (globals (map pgsPageId ps)) ps)

codes ∷ SessionSnapshot → [Text]
codes snap = [ ieCode e | e ← sessionIntegrityErrors snap ]

spec ∷ Spec
spec = describe "scenario identity in ordinary saves (buildings v4, world-pages v16)" $ do

    it "buildings v3 payloads decode with no references, and v4 round-trips them" $ do
        let v3 = S.encode (BuildingsDTOv3 [PageBuildingsDTOv3 pageA HM.empty []])
        fmap (\(BuildingsDTO ss) → map pbScenarioRefs ss) (ccDecode buildingsCodec 3 v3) `shouldBe` Right [[]]
        let v4 = BuildingsDTO [PageBuildingsDTO pageA HM.empty [] [(BuildingId 3, "furnace"), (BuildingId 7, "buildings[1]")]]
        ccDecode buildingsCodec 4 (S.encode v4) `shouldBe` Right v4
        ccValidate buildingsCodec v4 `shouldBe` []

    it "the wires refuse malformed, duplicate and unordered reference lists" $ do
        let b = [ (BuildingId 1, "a"), (BuildingId 2, "b") ]
            l = [ (LocationInstanceId 1, "a"), (LocationInstanceId 2, "b") ]
        buildingScenarioRefErrors b `shouldBe` []
        locationScenarioRefErrors l `shouldBe` []
        forM_ [ [(BuildingId 1, "")], [(BuildingId 1, T.replicate 513 "a")], [(BuildingId 1, "a b")]
              , [(BuildingId 1, "a"), (BuildingId 1, "b")], [(BuildingId 1, "a"), (BuildingId 2, "a")]
              , [(BuildingId 2, "a"), (BuildingId 1, "b")] ] $ \xs →
            (xs, null (buildingScenarioRefErrors xs)) `shouldBe` (xs, False)
        forM_ [ [(LocationInstanceId 1, "")], [(LocationInstanceId 1, T.replicate 513 "a")], [(LocationInstanceId 1, "a b")]
              , [(LocationInstanceId 1, "a"), (LocationInstanceId 1, "b")], [(LocationInstanceId 1, "a"), (LocationInstanceId 2, "a")]
              , [(LocationInstanceId 2, "a"), (LocationInstanceId 1, "b")] ] $ \xs →
            (xs, null (locationScenarioRefErrors xs)) `shouldBe` (xs, False)

    it "integrity accepts distinct references across pages and families" $ do
        snap ← snapshotOf [ withBuilding pageA 1 "furnace", withLocation pageB "ruin" ]
        codes snap `shouldBe` []

    it "integrity refuses a reference two buildings on different pages hold" $ do
        snap ← snapshotOf [ withBuilding pageA 1 "same", withBuilding pageB 2 "same" ]
        codes snap `shouldSatisfy` elem "duplicate-scenario-reference"

    it "integrity refuses a reference two locations on different pages hold" $ do
        snap ← snapshotOf [ withLocation pageA "same", withLocation pageB "same" ]
        codes snap `shouldSatisfy` elem "duplicate-scenario-reference"

    it "integrity refuses a reference held by a building and a location, or a building and a unit" $ do
        snapBL ← snapshotOf [ withBuilding pageA 1 "same", withLocation pageB "same" ]
        codes snapBL `shouldSatisfy` elem "duplicate-scenario-reference"
        snapBU ← snapshotOf [ withBuilding pageA 1 "same", withUnit pageB 5 "same" ]
        codes snapBU `shouldSatisfy` elem "duplicate-scenario-reference"

    it "integrity reports a location reference naming no location the page holds" $ do
        let page = withLocation pageA "ruin"
            params = pgsGenParams page
            lis = wgpLocationInstances params
            ghost = page { pgsGenParams = params { wgpLocationInstances = lis
                { lisScenarioRefs = Map.insert (LocationInstanceId 9) "ghost" (lisScenarioRefs lis) } } }
        snap ← snapshotOf [ ghost ]
        codes snap `shouldSatisfy` elem "dangling-reference"

    it "a save carrying the references decodes through the real envelope with them intact" $ do
        snap ← snapshotOf [ withBuilding pageA 1 "furnace", withLocation pageB "ruin" ]
        let meta = snapshotSaveMetadata (SaveRequestMeta "slot" "ts" False) snap
        back ← either (fail ∘ T.unpack) (\(_, s, _, _) → pure s)
                   (decodeSessionEnvelope HS.empty HS.empty (encodeSessionSnapshot meta snap []))
        fmap (bsnScenarioRefs ∘ pgsBuildings) (HM.lookup pageA (snapPages back))
            `shouldBe` Just (Map.singleton (BuildingId 1) "furnace")
        fmap (lisScenarioRefs ∘ wgpLocationInstances ∘ pgsGenParams) (HM.lookup pageB (snapPages back))
            `shouldBe` Just (Map.singleton (LocationInstanceId 1) "ruin")

    it "a restore keeps only the references of the buildings it produced" $ do
        let snap = (bsnapOf pageA) { bsnScenarioRefs = Map.fromList
                [ (BuildingId 1, "kept"), (BuildingId 2, "orphaned"), (BuildingId 9, "ghost") ] }
            bsnapOf pid = bsnapFor pid
            (restored, orphans) = fromBuildingSnapshot pageA (HM.singleton "test_building" (mkDef False 1 1)) snap
        orphans `shouldBe` [BuildingId 2]
        bmScenarioRefs restored `shouldBe` Map.singleton (BuildingId 1) "kept"

    it "a retired building takes its reference with it, quietly or with its page" $ do
        let defs = HM.singleton "test_building" (mkDef False 1 1)
            snapOn ids = BuildingSnapshot
                { bsnInstances = HM.fromList [ (BuildingId i, minimalBuildingInstance []) | i ← ids ]
                , bsnNextId = 10, bsnTags = Map.empty
                , bsnScenarioRefs = Map.fromList [ (BuildingId i, T.pack ("ref" ⧺ show i)) | i ← ids ] }
            (onA, _) = fromBuildingSnapshot pageA defs (snapOn [1, 2])
            (onB, _) = fromBuildingSnapshot pageB defs (snapOn [3])
            bm0 = onA { bmInstances = bmInstances onA `HM.union` bmInstances onB
                      , bmScenarioRefs = bmScenarioRefs onA `Map.union` bmScenarioRefs onB }
        Map.keys (bmScenarioRefs (removeBuildingQuietly (BuildingId 1) bm0)) `shouldBe` [BuildingId 2, BuildingId 3]
        -- retiring a page's buildings below the cutoff drops exactly their references
        Map.keys (bmScenarioRefs (retirePageBuildings pageA (BuildingId 3) bm0)) `shouldBe` [BuildingId 3]
  where
    -- two buildings: one of a registered definition, one of a removed one
    bsnapFor _ = BuildingSnapshot
        { bsnInstances = HM.fromList
            [ (BuildingId 1, minimalBuildingInstance [])
            , (BuildingId 2, (minimalBuildingInstance []) { bisDefName = "removed_building" }) ]
        , bsnNextId = 3, bsnTags = Map.empty, bsnScenarioRefs = Map.empty }
