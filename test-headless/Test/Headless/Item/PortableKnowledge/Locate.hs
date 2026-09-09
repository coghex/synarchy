-- | The session-wide live-instance locator (#2512 requirement 4):
--   "World.Item.Locate" must find an instance in every container
--   'World.Save.Types.pageItemContainers' enumerates, on every page,
--   at any nesting depth — and must find nothing anywhere else.
--
--   Each example moves ONE crate to a different owner and asserts both
--   halves of the answer, the page and the instance. Asserting only
--   that something was found would pass for a locator that returned the
--   first item it saw.
module Test.Headless.Item.PortableKnowledge.Locate (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef (writeIORef)
import Building.Types (BuildingManager(..), emptyBuildingManager)
import Item.Ground (GroundItems, emptyGroundItems)
import Item.Knowledge
import Item.Types (ItemInstance(..))
import Unit.Types (UnitId(..), UnitManager(..), emptyUnitManager)
import World.Item.Locate
import World.Page.Types (WorldPageId)
import World.State.Types (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import Building.Types (BuildingId(..))
import Test.Headless.Item.PortableKnowledge.Fixture

-- | A live session: two pages, page A visible and page B DELIBERATELY
--   hidden, plus the session-global building and unit managers.
--
--   Page B being hidden is load-bearing in every example that puts the
--   crate there: a locator that walked only the visible list would pass
--   the ground case on page A and fail exactly the cases a crate
--   carried off-screen depends on.
data Scene = Scene
    { scGround    ∷ [(WorldPageId, GroundItems)]
    , scBuildings ∷ BuildingManager
    , scUnits     ∷ UnitManager
    }

emptyScene ∷ Scene
emptyScene = Scene
    { scGround    = [(pkPageA, emptyGroundItems), (pkPageB, emptyGroundItems)]
    , scBuildings = emptyBuildingManager
        { bmDefs = HM.fromList [("cargo_hold_S", storageDef), ("shed", plainDef)]
        , bmNextId = 9 }
    , scUnits = emptyUnitManager { umNextId = 9 }
    }

groundOn ∷ WorldPageId → [ItemInstance] → Scene → Scene
groundOn pid insts sc = sc
    { scGround = [ (p, if p ≡ pid then groundWith insts else g)
                 | (p, g) ← scGround sc ] }

withUnit ∷ UnitId → WorldPageId → [ItemInstance]
         → HM.HashMap Text ItemInstance → [ItemInstance] → Scene → Scene
withUnit uid pid inv eqp acc sc = sc
    { scUnits = (scUnits sc)
        { umInstances = HM.insert uid (mkUnit pid inv eqp acc)
                            (umInstances (scUnits sc)) } }

withBuilding
    ∷ BuildingId → WorldPageId → HM.HashMap Text [ItemInstance]
    → [ItemInstance] → Scene → Scene
withBuilding bid pid delivered storage sc = sc
    { scBuildings = (scBuildings sc)
        { bmInstances = HM.insert bid
              (mkBuilding pid "cargo_hold_S" delivered storage)
              (bmInstances (scBuildings sc)) } }

locate ∷ Scene → Word64 → Maybe LocatedItem
locate sc = locateItemInstanceIn (scGround sc) (scBuildings sc) (scUnits sc)

found ∷ Scene → Word64 → Maybe (WorldPageId, Word64)
found sc iid =
    (\l → (liPage l, iiInstanceId (liInstance l))) <$> locate sc iid

spec ∷ Spec
spec = do
    describe "every container the save system enumerates" $ do
        it "finds a crate lying on the ground of a HIDDEN page, \
           \reporting that page and that instance" $
            found (groundOn pkPageB [crate] emptyScene) crateId
                `shouldBe` Just (pkPageB, crateId)

        it "finds one in a unit's INVENTORY" $
            found (withUnit (UnitId 1) pkPageB [crate] HM.empty []
                       emptyScene) crateId
                `shouldBe` Just (pkPageB, crateId)

        it "finds one in a unit's EQUIPPED slots" $
            found (withUnit (UnitId 1) pkPageB []
                       (HM.singleton "back" crate) [] emptyScene) crateId
                `shouldBe` Just (pkPageB, crateId)

        it "finds one among a unit's ACCESSORIES" $
            found (withUnit (UnitId 1) pkPageB [] HM.empty [crate]
                       emptyScene) crateId
                `shouldBe` Just (pkPageB, crateId)

        it "finds one in a building's DELIVERED MATERIALS" $
            found (withBuilding (BuildingId 1) pkPageB
                       (HM.singleton "supplies" [crate]) [] emptyScene) crateId
                `shouldBe` Just (pkPageB, crateId)

        it "finds one in a building's loose STORAGE" $
            found (withBuilding (BuildingId 1) pkPageB HM.empty [crate]
                       emptyScene) crateId
                `shouldBe` Just (pkPageB, crateId)

        it "descends RECURSIVELY: the kit inside the crate, and the \
           \bandage inside the kit, are each located in their own right" $ do
            let sc = withBuilding (BuildingId 1) pkPageB HM.empty [crate]
                         emptyScene
            found sc kitId     `shouldBe` Just (pkPageB, kitId)
            found sc bandageId `shouldBe` Just (pkPageB, bandageId)

        it "returns the located instance ITSELF, not merely its id -- \
           \which is what lets a caller weigh it and read its capacity" $
            (liInstance <$> locate (groundOn pkPageB [crate] emptyScene) kitId)
                `shouldBe` Just kit

    describe "moving between owners and pages" $ do
        it "follows one crate from page A's ground, to a unit on page B, \
           \to a building on page B, and back to page A -- reporting the \
           \page it is actually on at each step" $ do
            let onGroundA = groundOn pkPageA [crate] emptyScene
                inUnitB   = withUnit (UnitId 1) pkPageB [crate] HM.empty []
                                emptyScene
                inStoreB  = withBuilding (BuildingId 1) pkPageB HM.empty
                                [crate] emptyScene
                backOnA   = onGroundA
            found onGroundA crateId `shouldBe` Just (pkPageA, crateId)
            found inUnitB   crateId `shouldBe` Just (pkPageB, crateId)
            found inStoreB  crateId `shouldBe` Just (pkPageB, crateId)
            found backOnA   crateId `shouldBe` Just (pkPageA, crateId)

        it "the RECORD is untouched by every one of those moves: observe \
           \the crate while it is on page A's ground, move it into a \
           \building on page B, and the memory is bit-identical while \
           \the locator's answer has changed" $ do
            let onGroundA = groundOn pkPageA [crate] emptyScene
                inStoreB  = withBuilding (BuildingId 1) pkPageB HM.empty
                                [crate] emptyScene
            -- Observe through the instance the locator hands back, the
            -- way the real verbs do.
            let k = maybe emptyPortableKnowledge
                        (\l → observePortableContents testItems 7
                                  (liInstance l) emptyPortableKnowledge)
                        (locate onGroundA crateId)
            found onGroundA crateId `shouldBe` Just (pkPageA, crateId)
            found inStoreB  crateId `shouldBe` Just (pkPageB, crateId)
            portableState crateId k `shouldBe` KnownContents
            (coItems <$> (lookupPortable crateId k ⌦ prContents))
                `shouldBe` Just [kit]
            -- And the record is keyed by the CRATE, not by any page:
            -- nothing in it names where the crate was when it was seen.
            HM.keys (pkRecords k) `shouldBe` [crateId]

    describe "what must NOT resolve" $ do
        it "answers Nothing for an id nothing live carries" $
            found (groundOn pkPageA [crate] emptyScene) unlocatableId
                `shouldBe` Nothing

        it "answers Nothing for the never-minted sentinel id 0, even \
           \when an unstamped item is lying around" $
            found (groundOn pkPageA [loose { iiInstanceId = 0 }] emptyScene) 0
                `shouldBe` Nothing

        it "a REMEMBERED-only instance does not resolve: an observation \
           \holds copies, and a copy is not a live entity however \
           \faithfully it records one" $ do
            -- The crate was observed and then destroyed. Its remembered
            -- kit's id is still in the record; nothing live carries it.
            let k  = observePortableContents testItems 7 crate
                         emptyPortableKnowledge
                sc = emptyScene
            (lookupPortable crateId k ⌦ prContents)
                `shouldSatisfy` isJust
            found sc kitId   `shouldBe` Nothing
            found sc crateId `shouldBe` Nothing

    describe "the live-id set the load scrub tests against" $ do
        it "is exactly the union of every page's containers, recursively" $ do
            let sc = withUnit (UnitId 1) pkPageA [loose] HM.empty []
                         (withBuilding (BuildingId 1) pkPageB HM.empty [crate]
                              emptyScene)
                ids = sessionLiveItemIds (scGround sc) (scBuildings sc)
                          (scUnits sc)
            ids `shouldBe` HS.fromList [looseId, crateId, kitId, bandageId]

        it "reads a page's ground items off its own live WorldState ref, \
           \in the manager's page order" $ do
            wsA ← emptyWorldState
            wsB ← emptyWorldState
            writeIORef (wsGroundItemsRef wsA) (groundWith [loose])
            writeIORef (wsGroundItemsRef wsB) (groundWith [crate])
            let mgr = emptyWorldManager
                    { wmWorlds = [(pkPageA, wsA), (pkPageB, wsB)]
                    , wmVisible = [pkPageA] }
            ground ← sessionGroundItems mgr
            map fst ground `shouldBe` [pkPageA, pkPageB]
            let sc = emptyScene { scGround = ground }
            found sc looseId `shouldBe` Just (pkPageA, looseId)
            found sc crateId `shouldBe` Just (pkPageB, crateId)
