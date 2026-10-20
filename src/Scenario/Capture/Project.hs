{-# LANGUAGE Strict #-}
-- | Projecting one instant of a session into a scenario (#2714, SCN-14 of
--   epic #2698): the composition of every content family's capture adapter
--   (#2709–#2712) over the same 'CaptureInputs'.
--
--   Pure. Nothing here reads or writes engine state, advances the
--   simulation, heals or refills anything, draws gameplay randomness,
--   realizes pending loot or mints an item: it reads values the caller
--   already took at the snapshot boundary.
--
--   == Order
--
--   Scenario identities share ONE namespace across every family, items
--   included, so every referenced object is named together before any
--   entry is captured (the adapters' tier rule), and each later family is
--   named against everything an earlier one took:
--
--   1. document names for the referenced units, buildings and locations
--      ('nameDocumentRefs'), with every item reference reserved up front;
--   2. items, buildings, locations and units, in the order their names
--      depend on one another (the order "Scenario.Load.Construct" installs
--      them in, reversed);
--   3. structures, against everything above;
--   4. terrain, fluids and flora, against everything above.
--
--   == Nothing is dropped silently
--
--   An object an adapter cannot capture — a unit or building whose
--   definition is no longer registered, a location instance of an unknown
--   definition, a structure whose art no registered pack places, a column
--   the terrain vocabulary cannot state — fails the projection with one
--   diagnostic per finding. A dead unit is the single exclusion: a corpse
--   is not a scenario unit.
module Scenario.Capture.Project
    ( Projection(..)
    , ProjectionFailure(..)
    , projectScenario
    , Projected(..)
    , projectParts
    , scenarioIds
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Building.Types
import Gameplay.Tags.Index (categoryTagsOf)
import Gameplay.Tags.Tag (mkItemTarget)
import qualified Item.Ground as G
import Location.Instance
import Power.Types (nodeForBuilding)
import Scenario.Capture.Inputs
import Scenario.Items.Capture
    ( CaptureCtx(..), Captured(..), DocKey(..), captureGroundItems, nameDocumentRefs, newCaptureState )
import Scenario.Items.Refs (lookupItemRef)
import Scenario.Placement.Building
import Scenario.Placement.Location
import Scenario.Placement.Structure
import Scenario.Types
import Scenario.Units.Capture
import Scenario.World.Capture (CapturedWorld(..), WorldSnapshot(..), captureWorldContent, chunkStatesOf)
import Unit.Types
import World.Arena.Bounds (arenaHeight, arenaWidth)

-- | The scenario one instant holds, and how much of each family it has.
data Projection = Projection
    { prScenario ∷ !Scenario
    , prCounts   ∷ ![(Text, Int)]
    } deriving (Show, Eq)

-- | Why an instant cannot be written as a scenario: one summary, and a
--   diagnostic per finding.
data ProjectionFailure = ProjectionFailure
    { pfSummary     ∷ !Text
    , pfDiagnostics ∷ ![ScenarioDiagnostic]
    } deriving (Show, Eq)

-- | Project @ci@. The scenario carries the version the caller names (the
--   current format's, for a capture). Anything that cannot be written as a
--   scenario fails the whole projection: see the module header.
projectScenario ∷ Int → CaptureInputs → Either ProjectionFailure Projection
projectScenario version ci
    | not (null findings) = Left ProjectionFailure
        { pfSummary = tshow (length findings) <> " supported object(s) cannot be captured, so nothing is written"
        , pfDiagnostics = findings }
    | otherwise = Right Projection
        { prScenario = scenario
        , prCounts = [ ("terrain", length (scTerrain scenario)), ("fluids", length (scFluids scenario))
                     , ("flora", length (scFlora scenario)), ("structures", length (scStructures scenario))
                     , ("buildings", length (scBuildings scenario)), ("locations", length (scLocations scenario))
                     , ("units", length (scUnits scenario)), ("ground_items", length (scGroundItems scenario)) ] }
  where
    parts = projectParts version ci
    scenario = pjScenario parts
    findings = pjWorldFindings parts ⧺ pjStructureFindings parts ⧺ pjSkipped parts

-- | The scenario an instant holds, and the findings that make it
--   unrepresentable, kept apart by the family that raised them. A caller that
--   wants the scenario regardless (a test that asserts on one family) reads
--   the parts; the capture service refuses unless all three are empty.
data Projected = Projected
    { pjScenario         ∷ !Scenario
    , pjWorldFindings    ∷ ![ScenarioDiagnostic]
      -- ^ terrain, fluids and flora the vocabulary cannot state
    , pjStructureFindings ∷ ![ScenarioDiagnostic]
      -- ^ placed structure pieces no registered pack places
    , pjSkipped          ∷ ![ScenarioDiagnostic]
      -- ^ live objects whose definition is no longer registered
    }

projectParts ∷ Int → CaptureInputs → Projected
projectParts version ci = Projected
    { pjScenario = scenario
    , pjWorldFindings = cwDiagnostics world
    , pjStructureFindings = scDiagnostics scap
    , pjSkipped = skippedFindings }
  where
    pid = ciPage ci
    um = ciUnits ci
    bm = ciBuildings ci
    lis = ciLocations ci
    itemCtx = CaptureCtx
        { ccRef = \i → lookupItemRef i (ciItemRefs ci)
        , ccTags = \i → maybe Set.empty (\t → categoryTagsOf t (ciItemTags ci)) (mkItemTarget i) }
    pageUnits = sortOn fst [ (u, i) | (u, i) ← HM.toList (umInstances um), uiPage i ≡ pid ]
    pageBuildings = sortOn fst [ (b, i) | (b, i) ← HM.toList (bmInstances bm), biPage i ≡ pid ]
    -- 1. scenario ids share ONE namespace: every referenced unit, building
    --    and location is named together, before anything is captured
    docNames = nameDocumentRefs HS.empty $
        [ (DocUnit u, r) | (u, _) ← pageUnits, Just r ← [Map.lookup u (umScenarioRefs um)] ]
        ⧺ [ (DocBuilding b, r) | (b, _) ← pageBuildings, Just r ← [Map.lookup b (bmScenarioRefs bm)] ]
        ⧺ [ (DocLocation l, r) | (l, r) ← Map.toList (lisScenarioRefs lis) ]
    groundTrees = map G.giInst (HM.elems (G.gisItems (ciGround ci)))
    buildingTrees = concat [ biStorage b ⧺ concat (HM.elems (biMaterialsDelivered b)) | (_, b) ← pageBuildings ]
    unitItemTrees = concatMap (unitTrees ∘ snd) pageUnits
    -- every item reference in the document is named up front too
    stNames = newCaptureState itemCtx (Map.elems docNames) (groundTrees ⧺ buildingTrees ⧺ unitItemTrees)
    lctx = LocationCaptureCtx
        { lccDefs = ciLocationDefs ci, lccItemRef = const Nothing
        , lccNameOf = \l → Map.lookup (DocLocation l) docNames }
    (_, locNameMap) = locationNames lctx stNames lis
    foreignIds = Map.elems docNames ⧺ Map.elems locNameMap
    -- 2. the families that own items
    ucap = captureUnits UnitCaptureCtx
        { uccDefs = umDefs um, uccItems = ciItems ci, uccPhysiology = ciPhysiology ci, uccNow = ciNow ci
        , uccUnitRef = \u → Map.lookup u (umScenarioRefs um)
        , uccUnitTags = \u → categoryTagsOf u (umTags um)
        , uccItemCtx = itemCtx
        , uccOtherTrees = groundTrees ⧺ buildingTrees
        , uccPreNamed = \u → Map.lookup (DocUnit u) docNames
        , uccEncounterOf = \u → lookup u occupantNames }
        foreignIds pageUnits
    occupantNames =
        [ (leoUnitId o, ExplicitId n)
        | inst ← instancesToList lis, Just n ← [Map.lookup (liId inst) locNameMap]
        , Just enc ← [liEncounter inst], o ← leOccupants enc ]
    bcap = captureBuildings BuildingCaptureCtx
        { bccDefs = ciBuildingDefs ci
        , bccNodes = \b → nodeForBuilding b (ciPowerNodes ci)
        , bccNameOf = \b → Map.lookup (DocBuilding b) docNames
        , bccTags = \b → categoryTagsOf b (bmTags bm)
        , bccItemCtx = itemCtx } (ucapState ucap) pageBuildings
    (gst, groundRows, gcap) = captureGroundItems itemCtx (bcapState bcap) (ciGround ci)
    itemRefs = capRefs gcap ⧺ bcapItems bcap ⧺ ucapItems ucap
    lcap = captureLocations lctx { lccItemRef = \i → lookup i itemRefs
                                 , lccNameOf = \l → Map.lookup l locNameMap }
                            gst lis
    -- 3. structures, against every id the families above took
    chunks = chunkStatesOf (ciBounds ci) (ciSeed ci) (snapEdits (ciWorld ci)) (ciChunks ci)
    entryIds = Set.fromList
        ( map (scenarioIdText ∘ beId) (bcapEntries bcap)
        ⧺ map (scenarioIdText ∘ leId) (lcapEntries lcap)
        ⧺ map (scenarioIdText ∘ ueId) (ucapEntries ucap)
        ⧺ concatMap (itemIdsOf ∘ giItem) groundEntries
        ⧺ concatMap itemIdsOfUnit (ucapEntries ucap)
        ⧺ concatMap itemIdsOfBuilding (bcapEntries bcap) )
    scap = captureStructures StructureCaptureCtx
        { sccArt = ciStructureArt ci, sccPalette = ciPalette ci, sccBounds = ciBounds ci } chunks entryIds
    -- 4. terrain, fluids and flora, against all of it
    world = captureWorldContent (Set.union entryIds (scTaken scap)) (ciWorld ci)
    groundEntries = [ GroundItemEntry x y e | (x, y, e) ← groundRows ]
    scenario = Scenario
        { scSourceVersion = version
        , scMap = (\b → MapDimensions (arenaWidth b) (arenaHeight b)) <$> ciBounds ci
        , scCamera = ciCamera ci
        , scTerrain = cwTerrain world, scFluids = cwFluids world, scFlora = cwFlora world
        , scStructures = scPieces scap
        , scBuildings = bcapEntries bcap, scLocations = lcapEntries lcap
        , scUnits = ucapEntries ucap, scGroundItems = groundEntries }
    -- the live objects capture could not name
    skippedFindings =
        [ finding ("units[" <> tshow u <> "]") ("a unit of the unregistered definition '" <> uiDefName i <> "'")
        | (u, i) ← pageUnits, u `elem` ucapSkipped ucap, uiPose i ≢ "dead" ]
        ⧺ [ finding ("buildings[" <> tshow b <> "]") ("a building of the unregistered definition '" <> biDefName i <> "'")
          | (b, i) ← pageBuildings, b `elem` bcapSkipped bcap ]
        ⧺ [ finding ("locations[" <> tshow l <> "]") "a location instance of an unregistered definition"
          | l ← lcapSkipped lcap ]
    finding at why = ScenarioDiagnostic at (InvalidValue why) EntryRejected

-- | Every scenario id a captured document carries, items included.
scenarioIds ∷ Scenario → Set.Set Text
scenarioIds sc = Set.fromList $
    map (scenarioIdText ∘ tpId) (scTerrain sc) ⧺ map (scenarioIdText ∘ fpId) (scFluids sc)
    ⧺ map (scenarioIdText ∘ feId) (scFlora sc) ⧺ map (scenarioIdText ∘ spId) (scStructures sc)
    ⧺ map (scenarioIdText ∘ beId) (scBuildings sc) ⧺ map (scenarioIdText ∘ leId) (scLocations sc)
    ⧺ map (scenarioIdText ∘ ueId) (scUnits sc) ⧺ concatMap itemIdsOfUnit (scUnits sc)
    ⧺ concatMap itemIdsOfBuilding (scBuildings sc) ⧺ concatMap (itemIdsOf ∘ giItem) (scGroundItems sc)

itemIdsOf ∷ ItemEntry → [Text]
itemIdsOf e = scenarioIdText (ieId e) : concatMap itemIdsOf (authoredOr [] (ieContents e))

itemIdsOfUnit ∷ UnitEntry → [Text]
itemIdsOfUnit u = concatMap itemIdsOf
    (authoredOr [] (ueInventory u) ⧺ HM.elems (authoredOr HM.empty (ueEquipment u))
     ⧺ authoredOr [] (ueAccessories u))

itemIdsOfBuilding ∷ BuildingEntry → [Text]
itemIdsOfBuilding b = concatMap itemIdsOf
    (authoredOr [] (beStorage b) ⧺ authoredOr [] (beMaterialsDelivered b))
