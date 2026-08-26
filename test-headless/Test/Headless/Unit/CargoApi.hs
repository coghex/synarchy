-- | The four LAX unit item verbs refuse a cross-page endpoint pair
--   (#1673): @unit.transferItemToBuilding@, @unit.transferItemToUnit@,
--   @unit.depositToCargo@ and @unit.withdrawFromCargo@, driven through
--   the REAL registered production API against REAL manager refs.
--
--   Laxity in these four has always meant "no adjacency check, no
--   receiver-eligibility check, no carrying-capacity check" — the
--   deliberate latitude the fetch / repair / medic ladders depend on.
--   It never meant permission to move an exact 'ItemInstance' between
--   two WORLDS, and until #1673 nothing compared @uiPage@ to @biPage@,
--   so a caller that paired an actor from one page with a target on
--   another teleported the instance across.
--
--   Every scenario therefore comes in two halves, and both halves are
--   load-bearing:
--
--     * the CROSS-PAGE half asserts the call returns false AND that
--       each verb's own ordered item collection is byte-identical to
--       what it was before — @biMaterialsDelivered@ for
--       transferItemToBuilding, BOTH @uiInventory@ lists for
--       transferItemToUnit, @uiInventory@ plus @biStorage@ for
--       deposit/withdraw. Order is gameplay-visible, so "the counts
--       match" would not be the same assertion.
--     * the SAME-PAGE half proves the page check is the ONLY new
--       policy: a NONADJACENT pair still commits, a receiver the strict
--       surface would refuse (a wildlife unit, an under-construction
--       building) is still accepted, and transferItemToUnit still
--       ignores the destination's carrying capacity.
--
--   Container knowledge (#1087) gets its own pair: deposit and withdraw
--   reveal a container's contents on the COMMITTED path only, so a
--   cross-page refusal must leave the remembered view @"unknown"@ while
--   the same-page control turns it @"known"@.
--
--   Two live in-memory pages, no worldgen — the fixture shape
--   'Test.Headless.Building.PageBinding' and
--   'Test.Headless.Unit.TransferOrderApi' both use. The unit /
--   building / item constructors are 'Test.Headless.Unit.TransferApi''s
--   own, re-stamped onto a page, rather than a third copy free to drift
--   from what the projections actually read.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Unit cargo"'@.
module Test.Headless.Unit.CargoApi (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.List (sortOn)
import Data.IORef (readIORef, writeIORef)
import Building.Types
    ( BuildingId(..), BuildingInstance(..), BuildingManager(..)
    , emptyBuildingManager )
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Types (ItemInstance(..), emptyItemManager)
import Unit.Faction (Faction(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldState, emptyWorldManager)
import Test.Headless.Unit.TransferApi
    ( evalDebug, mkBuilding, mkItem, mkUnit, minimalDef, newBareLuaBackend
    , storageDef )

-- * Fixture identities

-- | Page A is where the acting acolyte stands. Page B is live too —
--   the defect is a cross-page COMMIT, not a dead reference, so the
--   counterpart has to be a genuinely live endpoint on a genuinely live
--   page.
pageA, pageB ∷ WorldPageId
pageA = WorldPageId "cargo_api_page_a"
pageB = WorldPageId "cargo_api_page_b"

-- | uid 1, page A, player, at (10, 10) — the actor in every scenario.
acolyteUid ∷ UnitId
acolyteUid = UnitId 1

-- | uid 2, page A, WILDLIFE, adjacent — a receiver the strict surface
--   refuses outright and the lax verbs deliberately accept.
wolfUid ∷ UnitId
wolfUid = UnitId 2

-- | uid 3, page A, player, thirty tiles away — the no-adjacency proof.
farUid ∷ UnitId
farUid = UnitId 3

-- | uid 4, page B, player, standing on the SAME coordinates as the
--   acolyte, so nothing but the page distinguishes it.
offPageUid ∷ UnitId
offPageUid = UnitId 4

-- | bid 7, page A, Built, adjacent. bid 8 is its page-B twin at the
--   same coordinates. bid 9 is page A but UNDER CONSTRUCTION (strict
--   would refuse it); bid 10 is page A, Built and thirty tiles away.
holdA, holdB, siteA, farHoldA ∷ BuildingId
holdA    = BuildingId 7
holdB    = BuildingId 8
siteA    = BuildingId 9
farHoldA = BuildingId 10

-- * Fixture

-- | Reset both live pages and all three manager refs.
--
--   The acolyte carries plate #101 and #102 in that order plus anvil
--   #103; page B's hold and page B's acolyte each carry a ration, so a
--   cross-page WITHDRAW and a cross-page pull-from-a-unit both have a
--   real matching instance to (fail to) move. Nothing here is adjacent
--   to anything by accident: the far unit and far hold sit thirty tiles
--   out precisely so a scenario can show adjacency is still not
--   checked.
resetPages ∷ EngineEnv → IO ()
resetPages env = do
    wsA ← emptyWorldState
    wsB ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(pageA, wsA), (pageB, wsB)], wmVisible = [pageA] }
    writeIORef (itemManagerRef env) emptyItemManager
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList
            [ ("acolyte", minimalDef "acolyte" "Acolyte")
            , ("wolf", minimalDef "wolf" "Wolf") ]
        , umInstances = HM.fromList
            [ (acolyteUid, onPage pageA
                  (mkUnit "acolyte" FactionPlayer (10, 10) 100
                          [ mkItem "plate_steel" 101 5
                          , mkItem "plate_steel" 102 5
                          , mkItem "anvil" 103 500 ] []))
            , (wolfUid, onPage pageA
                  (mkUnit "wolf" FactionWildlife (11, 11) 1 [] []))
            , (farUid, onPage pageA
                  (mkUnit "acolyte" FactionPlayer (40, 40) 100 [] []))
            , (offPageUid, onPage pageB
                  (mkUnit "acolyte" FactionPlayer (10, 10) 100
                          [mkItem "ration" 501 0.5] [])) ]
        }
    writeIORef (buildingManagerRef env) emptyBuildingManager
        { bmDefs = HM.fromList
            [ ("hold", storageDef "hold" "Cargo Hold" (1, 1) 0 200)
            , ("site", storageDef "site" "Build Site" (1, 1) 100 200) ]
        , bmInstances = HM.fromList
            [ (holdA, onPageB pageA
                  (mkBuilding "hold" (11, 10) (1, 1) [mkItem "ration" 601 0.5]))
            , (holdB, onPageB pageB
                  (mkBuilding "hold" (11, 10) (1, 1) [mkItem "ration" 602 0.5]))
            , (siteA, onPageB pageA (mkBuilding "site" (10, 11) (1, 1) []))
            , (farHoldA, onPageB pageA
                  (mkBuilding "hold" (40, 10) (1, 1)
                              [mkItem "ration" 603 0.5])) ]
        }
  where
    onPage  p u = u { uiPage = p }
    onPageB p b = b { biPage = p }

-- * Live-state readers
--
--   Every one of these reports ORDER, because every collection the four
--   verbs touch is order-visible: an assertion that only counted would
--   pass against a rollback that spliced the item back at the wrong
--   index, which is the exact bug the existing vanished-destination
--   path already guards against.

unitLoose ∷ EngineEnv → UnitId → IO [(Word64, Text)]
unitLoose env uid = do
    um ← readIORef (unitManagerRef env)
    pure $ maybe [] (map (\i → (iiInstanceId i, iiDefName i)) ∘ uiInventory)
                    (HM.lookup uid (umInstances um))

buildingStorage ∷ EngineEnv → BuildingId → IO [(Word64, Text)]
buildingStorage env bid = do
    bm ← readIORef (buildingManagerRef env)
    pure $ maybe [] (map (\i → (iiInstanceId i, iiDefName i)) ∘ biStorage)
                    (HM.lookup bid (bmInstances bm))

-- | The collection @unit.transferItemToBuilding@ writes, which is
--   neither of the two above: a per-defName map of ordered delivered
--   instances. Sorted by key so the reading is stable; the LISTS keep
--   their live order.
buildingDelivered ∷ EngineEnv → BuildingId → IO [(Text, [Word64])]
buildingDelivered env bid = do
    bm ← readIORef (buildingManagerRef env)
    pure $ case HM.lookup bid (bmInstances bm) of
        Nothing → []
        Just b  → sortOn fst
            [ (k, map iiInstanceId v)
            | (k, v) ← HM.toList (biMaterialsDelivered b) ]

-- * Lua plumbing

-- | @tostring@ so a boolean comes back as a quoted JSON string rather
--   than a bare literal, matching the sibling specs' discipline.
callBool ∷ LuaBackendState → Text → IO Text
callBool ls expr = evalDebug ls ("return tostring(" <> expr <> ")")

-- | The remembered container state (#1087): @"unknown"@ until something
--   on the committed path reveals it.
knownState ∷ LuaBackendState → BuildingId → IO Text
knownState ls (BuildingId n) = evalDebug ls $ T.concat
    [ "local k = building.getContainerKnowledge(", tshow n, "); "
    , "return tostring(k and k.state)" ]

q ∷ Text → Text
q t = "\"" <> t <> "\""

spec ∷ SpecWith EngineEnv
spec = describe "Unit cargo Lua API (lax verbs, #1673)" $ do

    describe "a cross-page endpoint pair is refused" $ do

        it "transferItemToBuilding leaves both collections untouched" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            before'  ← unitLoose env acolyteUid
            beforeB  ← buildingDelivered env holdB
            callBool ls "unit.transferItemToBuilding(1, 8, 'plate_steel')"
                `shouldReturn` q "false"
            unitLoose env acolyteUid `shouldReturn` before'
            buildingDelivered env holdB `shouldReturn` beforeB
            -- and nothing leaked into the same-page twin either
            buildingDelivered env holdA `shouldReturn` []

        it "transferItemToUnit leaves BOTH inventories untouched, either way" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            beforeA ← unitLoose env acolyteUid
            beforeB ← unitLoose env offPageUid
            callBool ls "unit.transferItemToUnit(1, 4, 'plate_steel')"
                `shouldReturn` q "false"
            unitLoose env acolyteUid `shouldReturn` beforeA
            unitLoose env offPageUid `shouldReturn` beforeB
            -- the reverse direction is the same refusal, not a
            -- source-side special case
            callBool ls "unit.transferItemToUnit(4, 1, 'ration')"
                `shouldReturn` q "false"
            unitLoose env acolyteUid `shouldReturn` beforeA
            unitLoose env offPageUid `shouldReturn` beforeB

        it "depositToCargo leaves the inventory and the storage untouched" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            beforeU ← unitLoose env acolyteUid
            beforeS ← buildingStorage env holdB
            callBool ls "unit.depositToCargo(1, 8, 'plate_steel')"
                `shouldReturn` q "false"
            unitLoose env acolyteUid `shouldReturn` beforeU
            buildingStorage env holdB `shouldReturn` beforeS

        it "withdrawFromCargo leaves the storage and the inventory untouched" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            beforeU ← unitLoose env acolyteUid
            beforeS ← buildingStorage env holdB
            callBool ls "unit.withdrawFromCargo(1, 8, 'ration')"
                `shouldReturn` q "false"
            buildingStorage env holdB `shouldReturn` beforeS
            unitLoose env acolyteUid `shouldReturn` beforeU
            -- the mirror pair — a page-B unit against a page-A
            -- building — is refused by the same rule
            beforeOff ← unitLoose env offPageUid
            beforeHA  ← buildingStorage env holdA
            callBool ls "unit.withdrawFromCargo(4, 7, 'ration')"
                `shouldReturn` q "false"
            buildingStorage env holdA `shouldReturn` beforeHA
            unitLoose env offPageUid `shouldReturn` beforeOff

        it "reveals nothing: a refused deposit or withdrawal is not an observation" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            knownState ls holdB `shouldReturn` q "unknown"
            callBool ls "unit.depositToCargo(1, 8, 'plate_steel')"
                `shouldReturn` q "false"
            knownState ls holdB `shouldReturn` q "unknown"
            callBool ls "unit.withdrawFromCargo(1, 8, 'ration')"
                `shouldReturn` q "false"
            knownState ls holdB `shouldReturn` q "unknown"
            -- Control: the SAME player unit committing on its OWN page
            -- does reveal, so the assertions above are about the
            -- refusal and not about a reveal that never works here.
            knownState ls holdA `shouldReturn` q "unknown"
            callBool ls "unit.depositToCargo(1, 7, 'plate_steel')"
                `shouldReturn` q "true"
            knownState ls holdA `shouldReturn` q "known"

    describe "the page check is the ONLY new policy" $ do

        it "a NONADJACENT same-page pair still commits, in all four verbs" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            -- thirty tiles apart in every case
            callBool ls "unit.transferItemToUnit(1, 3, 'plate_steel')"
                `shouldReturn` q "true"
            unitLoose env farUid `shouldReturn` [(101, "plate_steel")]
            callBool ls "unit.transferItemToBuilding(1, 10, 'plate_steel')"
                `shouldReturn` q "true"
            buildingDelivered env farHoldA `shouldReturn` [("plate_steel", [102])]
            callBool ls "unit.withdrawFromCargo(1, 10, 'ration')"
                `shouldReturn` q "true"
            unitLoose env acolyteUid `shouldReturn` [(103, "anvil"), (603, "ration")]
            callBool ls "unit.depositToCargo(1, 10, 'ration')"
                `shouldReturn` q "true"
            buildingStorage env farHoldA `shouldReturn` [(603, "ration")]

        it "a receiver the strict surface refuses is still accepted" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            -- An UNDER-CONSTRUCTION building: not Built, so the strict
            -- surface refuses it as a destination. Both building verbs
            -- take it. (The deposit goes first, while a light plate is
            -- still in hand: depositToCargo DOES weigh the destination's
            -- capacity, and that check is not what this case is about.)
            callBool ls "unit.depositToCargo(1, 9, 'plate_steel')"
                `shouldReturn` q "true"
            buildingStorage env siteA `shouldReturn` [(101, "plate_steel")]
            callBool ls "unit.transferItemToBuilding(1, 9, 'plate_steel')"
                `shouldReturn` q "true"
            buildingDelivered env siteA `shouldReturn` [("plate_steel", [102])]
            -- A WILDLIFE unit: never player-commandable, so
            -- unit.transferEndpointInfo reports it ineligible.
            callBool ls "unit.transferItemToUnit(1, 2, 'anvil')"
                `shouldReturn` q "true"
            unitLoose env wolfUid `shouldReturn` [(103, "anvil")]
            unitLoose env acolyteUid `shouldReturn` []

        it "transferItemToUnit still ignores the receiver's carrying capacity" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            -- The wolf's carrying_capacity is 1 kg and the anvil is
            -- 500. The lax verb does not weigh it; the Lua caller does.
            callBool ls "unit.transferItemToUnit(1, 2, 'anvil')"
                `shouldReturn` q "true"
            unitLoose env wolfUid `shouldReturn` [(103, "anvil")]
            unitLoose env acolyteUid
                `shouldReturn` [(101, "plate_steel"), (102, "plate_steel")]

        it "a vanished destination still rolls back rather than reading as cross-page" $ \env → do
            resetPages env
            ls ← newBareLuaBackend env
            before' ← unitLoose env acolyteUid
            -- bid 99 is no building at all. The refusal must come from
            -- the pre-existing vanished-destination path, leaving the
            -- source's list in its ORIGINAL order — not from a page
            -- comparison inventing an answer for an absent endpoint.
            callBool ls "unit.transferItemToBuilding(1, 99, 'plate_steel')"
                `shouldReturn` q "false"
            unitLoose env acolyteUid `shouldReturn` before'
            callBool ls "unit.depositToCargo(1, 99, 'plate_steel')"
                `shouldReturn` q "false"
            unitLoose env acolyteUid `shouldReturn` before'
