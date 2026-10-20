{-# LANGUAGE Strict #-}
-- | What a capture writes (#2714): the supported baseline as explicit
--   CURRENT values, in the current format, from a loaded scenario, from an
--   edited one and from a manually edited arena — and that loading the
--   captured file gives the same session back.
module Test.Headless.Scenario.Capture.Content (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (atomicModifyIORef', readIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Building.Types
import Engine.Core.State (EngineEnv(..), freshItemInstanceId)
import Location.Instance (LocationInstances(..))
import Scenario.Items.Refs (emptyItemRefs)
import World.Generate.Types (WorldGenParams(..))
import World.State.Types (WorldManager(..), WorldState(..))
import Item.Types (ItemInstance(..))
import Scenario.Capture.Types
import Scenario.Schema (currentScenarioVersion)
import Scenario.Types
import Test.Headless.Scenario.Capture.Support
import Test.Headless.Scenario.Load.Harness
import Test.Headless.Scenario.Load.Persist (integratedDoc)
import Test.Headless.Scenario.Placement.Fixtures (Fx(..))
import Unit.Types

-- | A scenario with the values the "current, not original" examples change.
editableDoc ∷ [String]
editableDoc =
    [ "map: {width: 40, height: 40}"
    , "units:"
    , "  - id: medic"
    , "    definition: acolyte"
    , "    x: 1"
    , "    y: 1"
    , "    stats: {hydration: 4.0, calories: 600}"
    , "    wounds: [{part: torso, kind: slash, severity: 0.2}]"
    , "    equipment: {right_hand: {id: blade, definition: steel_dagger, condition: 80}}"
    , "    inventory:"
    , "      - {id: flask, definition: canteen_steel_2l, fill: 2.0}"
    , "      - id: kit"
    , "        definition: field_toolbox"
    , "        contents: [{id: spare, definition: steel_bar}, {id: spare2, definition: steel_bar}]"
    , "      - id: pack"
    , "        definition: field_toolbox"
    , "        contents: [{id: held, definition: steel_bar}]" ]

-- | Every item of an instance's tree, with the instance first.
treeOf ∷ ItemInstance → [ItemInstance]
treeOf i = i : concatMap treeOf (iiContents i)

findItem ∷ UnitInstance → Text → [ItemInstance]
findItem u def = [ i | root ← uiInventory u ⧺ HM.elems (uiEquipment u), i ← treeOf root, iiDefName i ≡ def ]

spec ∷ SpecWith H
spec = describe "content" $ do

    it "captures a loaded scenario as a complete document in the current format, every family explicit" $ \h → do
        path ← writeScenario "capture-integrated.yaml" integratedDoc
        reloadFrom h path
        (_, bytes, st) ← captureLive h "integrated"
        sc ← decodeBytes h bytes
        scSourceVersion sc `shouldBe` currentScenarioVersion
        scMap sc `shouldBe` Just (MapDimensions 40 40)
        scCamera sc `shouldBe` (3, 4)
        cstStage st `shouldBe` CaptureDone
        -- every family is present with its content
        (length (scTerrain sc), length (scFluids sc), length (scFlora sc), length (scStructures sc))
            `shouldBe` (2, 1, 1, 1)
        (length (scBuildings sc), length (scLocations sc), length (scUnits sc))
            `shouldBe` (2, 1, 2)
        -- the gem the location binds, and the incidental content the load constructed
        -- once, are the document's own ground items now: stated, not owed again
        map (scenarioIdText ∘ ieId ∘ giItem) (scGroundItems sc) `shouldContain` ["gem"]
        map leIncidental (scLocations sc) `shouldBe` [Authored False]
        -- tags across the categories, exactly as they stand
        let tagsOf f = Set.fromList (concat (f sc))
        tagsOf (map tpTags ∘ scTerrain) `shouldBe` Set.fromList [tag "pad"]
        tagsOf (map feTags ∘ scFlora) `shouldBe` Set.fromList [tag "grove"]
        tagsOf (map beTags ∘ scBuildings) `shouldBe` Set.fromList [tag "power"]
        tagsOf (map leTags ∘ scLocations) `shouldBe` Set.fromList [tag "site"]
        tagsOf (map ueTags ∘ scUnits) `shouldBe` Set.fromList [tag "squad"]
        tagsOf (map (ieTags ∘ giItem) ∘ scGroundItems) `shouldBe` Set.fromList [tag "loot"]
        -- the stated values, explicit: stats, blood, wounds, power charge
        [medic] ← pure [ u | u ← scUnits sc, ueId u ≡ ExplicitId "medic" ]
        HM.lookup "hydration" (ueStats medic) `shouldBe` Just 4.0
        HM.lookup "calories" (ueStats medic) `shouldBe` Just 600
        ueBlood medic `shouldBe` Authored 3.2
        fmap (map wsPart) (authoredList (ueWounds medic)) `shouldBe` Just ["torso"]
        [bat] ← pure [ b | b ← scBuildings sc, beId b ≡ ExplicitId "bat" ]
        bePowerCharge bat `shouldBe` Authored 2500

    it "is a fixed point: loading the captured file and capturing again writes the same bytes and adds nothing" $ \h → do
        path ← writeScenario "capture-fixpoint.yaml" integratedDoc
        reloadFrom h path
        (dest1, bytes1, _) ← captureLive h "fix1"
        before ← sessionCounts h
        reloadFrom h dest1
        sessionCounts h `shouldReturn` before
        (dest2, bytes2, _) ← captureLive h "fix2"
        bytes2 `shouldBe` bytes1
        reloadFrom h dest2
        sessionCounts h `shouldReturn` before
        (_, bytes3, _) ← captureLive h "fix3"
        bytes3 `shouldBe` bytes1

    it "writes identical bytes for two captures of the same arena" $ \h → do
        path ← writeScenario "capture-twice.yaml" integratedDoc
        reloadFrom h path
        (_, a, _) ← captureLive h "twice-a"
        (_, b, _) ← captureLive h "twice-b"
        a `shouldBe` b

    it "exports the CURRENT values of an edited scenario, not the original overrides" $ \h → do
        loadDoc h "capture-edited.yaml" editableDoc
        original ← unitNamed h "medic"
        length (uiWounds original) `shouldBe` 1
        -- play: the wound healed, resources moved, the canteen half-emptied,
        -- the dagger nearly broke, a nested container emptied and another
        -- one overloaded far past anything the definition's capacity holds
        spare ← case findItem original "steel_bar" of
            (i : _) → pure i
            [] → fail "no steel bar to copy"
        overload ← forM [1 ∷ Int .. 12] $ \_ → do
            i ← freshItemInstanceId (fxEnv (hFx h))
            pure spare { iiInstanceId = i }
        editUnit h "medic" $ \u → u
            { uiWounds = []
            , uiStats = HM.insert "hydration" 3.0 (HM.insert "calories" 150 (uiStats u))
            , uiEquipment = HM.map (\i → i { iiCondition = 3 }) (uiEquipment u)
            , uiInventory = map (editInventory overload) (uiInventory u) }
        (_, bytes, _) ← captureLive h "edited"
        sc ← decodeBytes h bytes
        [medic] ← pure (scUnits sc)
        -- explicit current values: a healed wound is an EMPTY list (not absent,
        -- not the original wound)
        ueWounds medic `shouldBe` Authored []
        HM.lookup "hydration" (ueStats medic) `shouldBe` Just 3.0
        HM.lookup "calories" (ueStats medic) `shouldBe` Just 150
        -- the half-full canteen, the near-broken blade, the emptied and the overloaded containers
        let items = fromMaybe [] (authoredList (ueInventory medic))
            byDef def = [ i | root ← items, i ← flattenEntry root, ieDefinition i ≡ def ]
        [flask] ← pure (byDef "canteen_steel_2l")
        ieFill flask `shouldBe` Authored 1.0
        [blade] ← pure (HM.elems (authoredMap (ueEquipment medic)))
        ieCondition blade `shouldBe` Authored 3
        let kits = [ i | i ← items, ieDefinition i ≡ "field_toolbox" ]
        map (fmap length ∘ authoredList ∘ ieContents) kits `shouldBe` [Just 0, Just 13]
        -- loading the captured file gives exactly these values back
        (dest', _, _) ← captureLive h "edited-again"
        reloadFrom h dest'
        back ← unitNamed h "medic"
        uiWounds back `shouldBe` []
        HM.lookup "hydration" (uiStats back) `shouldBe` Just 3.0
        map iiCurrentFill (findItem back "canteen_steel_2l") `shouldBe` [1.0]
        map iiCondition (HM.elems (uiEquipment back)) `shouldBe` [3]
        sort' (map (length ∘ iiContents) (findItem back "field_toolbox")) `shouldBe` [0, 13]

    it "captures a manually edited arena: objects with no scenario metadata are named structurally, once" $ \h → do
        path ← writeScenario "capture-manual.yaml" integratedDoc
        reloadFrom h path
        -- as if every object had been created by hand: no scenario identity
        let env = fxEnv (hFx h)
        clearIdentities env
        (_, bytes, _) ← captureLive h "manual"
        sc ← decodeBytes h bytes
        length (scUnits sc) `shouldBe` 2
        length (scBuildings sc) `shouldBe` 2
        let names = map (scenarioIdText ∘ ueId) (scUnits sc) ⧺ map (scenarioIdText ∘ beId) (scBuildings sc)
        -- every name is explicit, unique, and none is an automatic path
        Set.size (Set.fromList names) `shouldBe` length names
        names `shouldSatisfy` all (not ∘ T.isInfixOf "[")
        -- and capturing again names them the same way
        (_, again, _) ← captureLive h "manual-again"
        again `shouldBe` bytes
  where
    sort' = Set.toAscList ∘ Set.fromList
    editInventory overload i = case iiDefName i of
        "canteen_steel_2l" → i { iiCurrentFill = 1.0 }
        "field_toolbox" → case iiContents i of
            [_, _] → i { iiContents = [] }
            _ → i { iiContents = iiContents i ⧺ overload }
        _ → i

-- | The counts a session is judged by across a reload: units, buildings,
--   and referenced objects.
sessionCounts ∷ H → IO (Int, Int, Int, Int)
sessionCounts h = do
    let env = fxEnv (hFx h)
    um ← readIORef (unitManagerRef env)
    bm ← readIORef (buildingManagerRef env)
    pure ( HM.size (umInstances um), HM.size (bmInstances bm)
         , Map.size (umScenarioRefs um), Map.size (bmScenarioRefs bm) )

authoredList ∷ Authored [α] → Maybe [α]
authoredList = \case
    Authored xs → Just xs
    Omitted → Nothing

authoredMap ∷ Authored (HM.HashMap κ α) → HM.HashMap κ α
authoredMap = \case
    Authored m → m
    Omitted → HM.empty

flattenEntry ∷ ItemEntry → [ItemEntry]
flattenEntry e = e : concatMap flattenEntry (fromMaybe [] (authoredList (ieContents e)))

-- | As if every object had been created by hand: no unit, building,
--   location or item carries a scenario identity.
clearIdentities ∷ EngineEnv → IO ()
clearIdentities env = do
    atomicModifyIORef' (unitManagerRef env) $ \m → (m { umScenarioRefs = Map.empty }, ())
    atomicModifyIORef' (buildingManagerRef env) $ \m → (m { bmScenarioRefs = Map.empty }, ())
    atomicModifyIORef' (worldManagerRef env) $ \m → (m { wmItemScenarioRefs = emptyItemRefs }, ())
    wm ← readIORef (worldManagerRef env)
    forM_ (wmWorlds wm) $ \(_, ws) →
        atomicModifyIORef' (wsGenParamsRef ws) $ \mp → (fmap clearLocations mp, ())
  where
    clearLocations p = p { wgpLocationInstances = (wgpLocationInstances p) { lisScenarioRefs = Map.empty } }
