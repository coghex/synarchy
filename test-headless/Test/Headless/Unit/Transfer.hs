-- | The player-managed transfer contract (#1000 phase A1, generalized
--   to endpoint pairs and ordered instance batches by #1085 phase A2 of
--   epic #1013). Exercises the pure policy in "Unit.Transfer" directly
--   — the same shape 'Test.Headless.Craft.Execute' uses for craft
--   consumption — against the SHIPPED capacity semantics decoded out of
--   @data/buildings/cargo_hold_S.yaml@ and @data/units/technomule.yaml@,
--   so a data change that breaks the contract fails here rather than in
--   a probe.
--
--   Pure policy only. The Lua-integrated verbs
--   (@unit.checkTransfer@ / @unit.commitTransfer@ /
--   @unit.transferEndpointInfo@) wrap exactly these functions against
--   live managers; that wiring has its own live-API gate in
--   'Test.Headless.Unit.TransferApi'.
module Test.Headless.Unit.Transfer (spec) where

import UPrelude
import Test.Hspec
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Building.Types (BuildingId(..))
import Engine.Asset.YamlBuildings
import Engine.Asset.YamlUnits
import Item.Types (ItemInstance(..))
import Unit.Stats (effectiveStat)
import Unit.Transfer
import Unit.Types (StatModifier(..), UnitId(..))
import World.Page.Types (WorldPageId(..))

-- | A bare instance. Only def name, id and weight matter to the
--   policy; the rest carries through untouched, which is the point of
--   moving instances rather than counts.
mkItem ∷ Text → Word64 → Float → ItemInstance
mkItem name iid w = ItemInstance
    { iiDefName     = name
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = w
    , iiSharpness   = 100
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    }

homePage ∷ WorldPageId
homePage = WorldPageId "main_world"

otherPage ∷ WorldPageId
otherPage = WorldPageId "second_world"

sourceUid ∷ UnitId
sourceUid = UnitId 1

muleUid ∷ UnitId
muleUid = UnitId 2

holdBid ∷ BuildingId
holdBid = BuildingId 7

depotBid ∷ BuildingId
depotBid = BuildingId 8

-- | Weigh by the instance's own weight. The engine passes
--   @itemTotalWeight itemMgr@ here; with no container defs in scope
--   that reduces to exactly this for a fill-free, contents-free item.
weighBare ∷ ItemInstance → Float
weighBare = iiWeight

unitAt ∷ (Int, Int) → Bool → Float → [ItemInstance] → [ItemInstance]
       → TransferEndpointView
unitAt tile commandable cap inv worn = UnitEndpointAt UnitEndpointView
    { uevPage          = homePage
    , uevTile          = tile
    , uevCommandable   = commandable
    , uevCapacity      = cap
    , uevInventory     = inv
    , uevEquipped      = worn
    , uevCarriedWeight = sum (map weighBare inv) + sum (map weighBare worn)
    }

buildingAt ∷ (Int, Int) → (Int, Int) → Bool → Float → [ItemInstance]
           → TransferEndpointView
buildingAt anchor size built cap stored = BuildingEndpointAt BuildingEndpointView
    { bevPage         = homePage
    , bevAnchor       = anchor
    , bevTileSize     = size
    , bevBuilt        = built
    , bevCapacity     = cap
    , bevStorage      = stored
    , bevStoredWeight = sum (map weighBare stored)
    }

-- | A player acolyte at tile (10, 10) carrying @inv@, wearing @worn@.
source ∷ [ItemInstance] → [ItemInstance] → TransferEndpointView
source = unitAt (10, 10) True 100

-- | A 1x1 built cargo hold anchored at (11, 10) — Chebyshev 1 from the
--   acolyte, i.e. exactly at the edge of the reach rule.
cargoHold ∷ Float → [ItemInstance] → TransferEndpointView
cargoHold cap stored = buildingAt (11, 10) (1, 1) True cap stored

-- | A technomule standing at (11, 11) — diagonally adjacent.
mule ∷ Float → [ItemInstance] → TransferEndpointView
mule cap inv = unitAt (11, 11) True cap inv []

sceneOf ∷ TransferEndpointView → TransferEndpointView → TransferScene
sceneOf src dst = TransferScene
    { tscSource      = Just src
    , tscDestination = Just dst
    , tscWeigh       = weighBare
    }

acolyteEp ∷ TransferEndpoint
acolyteEp = EndpointUnit sourceUid

muleEp ∷ TransferEndpoint
muleEp = EndpointUnit muleUid

holdEp ∷ TransferEndpoint
holdEp = EndpointBuilding holdBid

depotEp ∷ TransferEndpoint
depotEp = EndpointBuilding depotBid

itemRef ∷ Int64 → Text → TransferItemRef
itemRef iid name = TransferItemRef { tirInstanceId = iid, tirDefName = name }

request ∷ TransferEndpoint → TransferEndpoint → [TransferItemRef]
        → TransferRequest
request from to items = TransferRequest
    { trSource = from, trDestination = to, trItems = items }

toHold ∷ Int64 → Text → TransferRequest
toHold iid name = request acolyteEp holdEp [itemRef iid name]

toMule ∷ Int64 → Text → TransferRequest
toMule iid name = request acolyteEp muleEp [itemRef iid name]

firstItem ∷ TransferRequest → TransferItemRef
firstItem req = case trItems req of
    (i : _) → i
    []      → itemRef 0 ""

-- | The reason a scene/single-item request pair is refused, or Nothing
--   on success.
refusal ∷ TransferScene → TransferRequest → Maybe TransferFailure
refusal scene req = either Just (const Nothing)
    (planItem scene (trSource req) (trDestination req) (firstItem req))

-- | Plan/commit the single item a one-item request names.
planOne ∷ TransferScene → TransferRequest → Either TransferFailure TransferPlan
planOne scene req =
    planItem scene (trSource req) (trDestination req) (firstItem req)

commitOne ∷ TransferScene → TransferRequest
          → Either TransferFailure TransferCommit
commitOne scene req =
    commitItem scene (trSource req) (trDestination req) (firstItem req)

-- | The per-item states of a batch, in request order.
states ∷ TransferBatch → [TransferState]
states = map qtState . tbEntries

-- | The batch a create-time check produces. Fails the example loudly
--   if the WHOLE request was rejected instead — those cases have their
--   own describe, and silently pattern-matching a Right here would turn
--   a contract regression into an incomplete-pattern crash.
checkedBatch ∷ TransferScene → TransferRequest → IO TransferBatch
checkedBatch scene req = case checkBatch scene req of
    Left e  → do
        expectationFailure ("unexpected whole-request error: " <> show e)
        fail "unreachable"
    Right b → pure b

-- | Queue an order and walk it to the point a commit is allowed.
arrived ∷ TransferScene → TransferRequest → IO TransferBatch
arrived scene req =
    markBatchReadyToCommit . markBatchInTransit ⊚ checkedBatch scene req

spec ∷ Spec
spec = describe "Unit transfer contract" $ do

    describe "shipped capacity semantics" $ do
        it "reads the cargo hold's storage capacity from shipped data" $ do
            r ← Yaml.decodeFileEither "data/buildings/cargo_hold_S.yaml"
            case r of
                Left e   → expectationFailure (show e)
                Right bf → do
                    let defs = byfBuildings bf
                    case filter ((≡ "cargo_hold_S") . bydName) defs of
                        [d] → bydStorageCapacity d `shouldBe` 200.0
                        _   → expectationFailure "no cargo_hold_S def"

        it "keeps every shipped unit def decoding after the marker's removal" $ do
            -- A2 deleted `transfer_receiver` from the YAML schema. The
            -- shipped files must still decode unchanged — including the
            -- technomule, whose file the marker used to live in.
            names ← mapM (fmap uydName . shippedUnit)
                [ "technomule", "acolyte", "red_squirrel", "bear_brown" ]
            names `shouldBe` [ "technomule", "acolyte", "red_squirrel"
                             , "bear_brown" ]

        it "applies the technomule's innate +50% capacity modifier" $ do
            -- Eligibility is faction now, but the capacity is still
            -- data. The contract measures a unit endpoint against the
            -- modifier-applied carrying_capacity, which for the shipped
            -- mule is its body-derived base lifted by "cybernetic
            -- enhancements".
            d ← shippedUnit "technomule"
            let mods = [ StatModifier { smDelta   = uymDelta m
                                      , smSource  = uymSource m
                                      , smExpiry  = Nothing
                                      , smPercent = uymPercent m }
                       | m ← uydModifiers d
                       , uymStat m ≡ "carrying_capacity" ]
            length mods `shouldBe` 1
            -- Base ≈ 167 kg from 3.2 × (lean_mass × strength)^0.6.
            effectiveStat 0 167.0 mods `shouldBe` 250.5

    describe "all four endpoint pairs" $ do
        it "moves an exact instance unit → building storage" $ do
            let item  = mkItem "steel_plate" 41 1.2
                scene = sceneOf (source [item] []) (cargoHold 200 [])
            case commitOne scene (toHold 41 "steel_plate") of
                Left f  → expectationFailure (show f)
                Right c → do
                    iiInstanceId (tcItem c) `shouldBe` 41
                    tcSourceItems c `shouldBe` []
                    map iiInstanceId (tcDestinationItems c) `shouldBe` [41]

        it "moves an exact instance unit → unit inventory" $ do
            let item  = mkItem "steel_bar" 55 0.8
                scene = sceneOf (source [item] []) (mule 250.5 [])
            case commitOne scene (toMule 55 "steel_bar") of
                Left f  → expectationFailure (show f)
                Right c → do
                    iiInstanceId (tcItem c) `shouldBe` 55
                    tcSourceItems c `shouldBe` []
                    map iiInstanceId (tcDestinationItems c) `shouldBe` [55]

        it "moves an exact instance building storage → unit" $ do
            -- The direction A1 could not express at all: the source was
            -- always a UnitId.
            let item  = mkItem "steel_plate" 41 1.2
                scene = sceneOf (cargoHold 200 [item]) (source [] [])
                req   = request holdEp acolyteEp [itemRef 41 "steel_plate"]
            case commitOne scene req of
                Left f  → expectationFailure (show f)
                Right c → do
                    iiInstanceId (tcItem c) `shouldBe` 41
                    tcSourceItems c `shouldBe` []
                    -- A unit inventory APPENDS, matching
                    -- transferItemToUnit.
                    map iiInstanceId (tcDestinationItems c) `shouldBe` [41]

        it "moves an exact instance building storage → building storage" $ do
            let item  = mkItem "steel_plate" 41 1.2
                depot = buildingAt (12, 10) (1, 1) True 200 []
                scene = sceneOf (cargoHold 200 [item]) depot
                req   = request holdEp depotEp [itemRef 41 "steel_plate"]
            case commitOne scene req of
                Left f  → expectationFailure (show f)
                Right c → do
                    tcSourceItems c `shouldBe` []
                    -- Building storage PREPENDS, matching
                    -- depositToCargo.
                    map iiInstanceId (tcDestinationItems c) `shouldBe` [41]

        it "keeps building → building expressible with no special refusal" $ do
            -- No gesture in epic #1013 produces this pair yet; the
            -- contract must still accept it rather than reject the
            -- combination outright.
            let item  = mkItem "steel_plate" 41 1.2
                depot = buildingAt (12, 10) (1, 1) True 200 []
                scene = sceneOf (cargoHold 200 [item]) depot
            planOne scene (request holdEp depotEp [itemRef 41 "steel_plate"])
                `shouldSatisfy` isRight

        it "refuses a transfer from an endpoint to itself, either kind" $ do
            let item   = mkItem "steel_bar" 55 0.8
                uScene = sceneOf (source [item] []) (source [item] [])
                bScene = sceneOf (cargoHold 200 [item]) (cargoHold 200 [item])
            refusal uScene (request acolyteEp acolyteEp
                                    [itemRef 55 "steel_bar"])
                `shouldBe` Just (requestFailure ReasonReceiverIneligible)
            refusal bScene (request holdEp holdEp [itemRef 55 "steel_bar"])
                `shouldBe` Just (requestFailure ReasonReceiverIneligible)

    describe "endpoint eligibility" $ do
        it "refuses a non-player-commandable unit SOURCE as source-ineligible" $ do
            -- Faction, not a data marker: a wolf's stash is not the
            -- player's to move.
            let item  = mkItem "steel_bar" 55 0.8
                wolf  = unitAt (10, 10) False 100 [item] []
                scene = sceneOf wolf (cargoHold 200 [])
            refusal scene (toHold 55 "steel_bar")
                `shouldBe` Just (requestFailure ReasonSourceIneligible)

        it "refuses a non-player-commandable unit DESTINATION as receiver-ineligible" $ do
            let item  = mkItem "steel_bar" 55 0.8
                wolf  = unitAt (11, 11) False 250.5 [] []
                scene = sceneOf (source [item] []) wolf
            refusal scene (toMule 55 "steel_bar")
                `shouldBe` Just (requestFailure ReasonReceiverIneligible)

        it "accepts an ordinary player acolyte as a destination" $ do
            -- A2's deliberate widening: with the transfer_receiver
            -- marker gone, every player-commandable unit is a valid
            -- endpoint, not only the technomule.
            let item    = mkItem "steel_bar" 55 0.8
                acolyte = unitAt (11, 11) True 60 [] []
                scene   = sceneOf (source [item] []) acolyte
            planOne scene (toMule 55 "steel_bar") `shouldSatisfy` isRight

        it "refuses an unbuilt building SOURCE as source-ineligible" $ do
            let item  = mkItem "steel_plate" 41 1.2
                ghost = buildingAt (11, 10) (1, 1) False 200 [item]
                scene = sceneOf ghost (source [] [])
            refusal scene (request holdEp acolyteEp [itemRef 41 "steel_plate"])
                `shouldBe` Just (requestFailure ReasonSourceIneligible)

        it "refuses a zero-capacity building SOURCE as source-ineligible" $ do
            let item  = mkItem "steel_plate" 41 1.2
                shed  = buildingAt (11, 10) (1, 1) True 0 [item]
                scene = sceneOf shed (source [] [])
            refusal scene (request holdEp acolyteEp [itemRef 41 "steel_plate"])
                `shouldBe` Just (requestFailure ReasonSourceIneligible)

        it "refuses an unbuilt building DESTINATION as receiver-ineligible" $ do
            let item  = mkItem "steel_plate" 41 1.2
                ghost = buildingAt (11, 10) (1, 1) False 200 []
                scene = sceneOf (source [item] []) ghost
            refusal scene (toHold 41 "steel_plate")
                `shouldBe` Just (requestFailure ReasonReceiverIneligible)

        it "refuses a built building DESTINATION with no storage capacity" $ do
            let item  = mkItem "steel_plate" 41 1.2
                scene = sceneOf (source [item] []) (cargoHold 0 [])
            refusal scene (toHold 41 "steel_plate")
                `shouldBe` Just (requestFailure ReasonReceiverIneligible)

        it "refuses a missing destination" $ do
            let scene = TransferScene
                    { tscSource      = Just (source [mkItem "steel_bar" 61 0.8] [])
                    , tscDestination = Nothing
                    , tscWeigh       = weighBare
                    }
            refusal scene (toHold 61 "steel_bar")
                `shouldBe` Just (requestFailure ReasonReceiverMissing)

        it "refuses a missing source" $ do
            let scene = TransferScene
                    { tscSource      = Nothing
                    , tscDestination = Just (cargoHold 200 [])
                    , tscWeigh       = weighBare
                    }
            refusal scene (toHold 61 "steel_bar")
                `shouldBe` Just (requestFailure ReasonSourceMissing)

    describe "instance identity" $ do
        it "moves exactly one instance exactly once" $ do
            let a     = mkItem "steel_bar" 61 0.8
                b     = mkItem "steel_bar" 62 0.8
                c'    = mkItem "steel_bar" 63 0.8
                scene = sceneOf (source [a, b, c'] []) (cargoHold 200 [])
            case commitOne scene (toHold 62 "steel_bar") of
                Left f  → expectationFailure (show f)
                Right c → do
                    map iiInstanceId (tcSourceItems c) `shouldBe` [61, 63]
                    map iiInstanceId (tcDestinationItems c) `shouldBe` [62]
                    length (filter ((≡ 62) . iiInstanceId)
                                   (tcDestinationItems c)) `shouldBe` 1

        it "distinguishes same-definition instances" $ do
            let full  = (mkItem "canteen" 71 0.5) { iiCurrentFill = 1.0 }
                empty = mkItem "canteen" 72 0.5
                scene = sceneOf (source [full, empty] []) (cargoHold 200 [])
            case commitOne scene (toHold 72 "canteen") of
                Left f  → expectationFailure (show f)
                Right c → do
                    iiInstanceId (tcItem c) `shouldBe` 72
                    iiCurrentFill (tcItem c) `shouldBe` 0
                    map iiInstanceId (tcSourceItems c) `shouldBe` [71]

        it "refuses a missing or stale instance id" $ do
            let scene = sceneOf (source [mkItem "steel_bar" 61 0.8] [])
                                (cargoHold 200 [])
            refusal scene (toHold 999 "steel_bar")
                `shouldBe` Just (requestFailure ReasonInstanceMissing)

        it "refuses an instance id that no longer names the requested def" $ do
            let scene = sceneOf (source [mkItem "steel_bar" 61 0.8] [])
                                (cargoHold 200 [])
            refusal scene (toHold 61 "steel_plate")
                `shouldBe` Just (requestFailure ReasonInstanceMissing)

        it "refuses an equipped item as not transferable" $ do
            let worn  = mkItem "steel_dagger" 81 1.0
                scene = sceneOf (source [] [worn]) (cargoHold 200 [])
            refusal scene (toHold 81 "steel_dagger")
                `shouldBe` Just (requestFailure ReasonItemNotTransferable)

        it "refuses an accessory as not transferable" $ do
            let robe  = mkItem "acolyte_robe" 82 1.5
                scene = sceneOf (source [] [robe]) (cargoHold 200 [])
            refusal scene (toHold 82 "acolyte_robe")
                `shouldBe` Just (requestFailure ReasonItemNotTransferable)

        it "refuses a held instance id that no longer names the requested def" $ do
            -- #1273: the identity contract is validated before the
            -- location-specific reason is chosen, so a stale pair reads
            -- the same wherever its id resolves. "Worn" would assert
            -- the requested item exists, which this reference never
            -- named.
            let worn  = mkItem "steel_dagger" 81 1.0
                robe  = mkItem "acolyte_robe" 82 1.5
                scene = sceneOf (source [] [worn, robe]) (cargoHold 200 [])
            refusal scene (toHold 81 "steel_sword")
                `shouldBe` Just (requestFailure ReasonInstanceMissing)
            refusal scene (toHold 82 "leather_robe")
                `shouldBe` Just (requestFailure ReasonInstanceMissing)

        it "has no equipped list to consult for a building source" $ do
            -- A building endpoint's loose storage is all it has, so an
            -- unknown id there is MISSING, never "worn".
            let scene = sceneOf (cargoHold 200 [mkItem "steel_bar" 61 0.8])
                                (source [] [])
            refusal scene (request holdEp acolyteEp [itemRef 999 "steel_bar"])
                `shouldBe` Just (requestFailure ReasonInstanceMissing)

    describe "whole-request errors" $ do
        it "rejects an empty item list before checking anything" $ do
            let scene = sceneOf (source [] []) (cargoHold 200 [])
            checkBatch scene (request acolyteEp holdEp [])
                `shouldBe` Left ErrEmptyBatch

        it "rejects a duplicate instance id anywhere in the batch" $ do
            let a     = mkItem "steel_bar" 61 0.8
                b     = mkItem "steel_bar" 62 0.8
                scene = sceneOf (source [a, b] []) (cargoHold 200 [])
                req   = request acolyteEp holdEp
                            [ itemRef 61 "steel_bar"
                            , itemRef 62 "steel_bar"
                            , itemRef 61 "steel_bar" ]
            checkBatch scene req `shouldBe` Left ErrDuplicateInstance

        it "treats two entries sharing an INVALID id as one duplicate" $ do
            -- The duplicate scan runs over the parsed identity list
            -- BEFORE any per-item validity check, so two zeros (or the
            -- same negative twice) are one top-level rejection, not two
            -- per-item instance_unspecified outcomes.
            let scene = sceneOf (source [] []) (cargoHold 200 [])
            checkBatch scene (request acolyteEp holdEp
                                [itemRef 0 "steel_bar", itemRef 0 "steel_bar"])
                `shouldBe` Left ErrDuplicateInstance
            checkBatch scene (request acolyteEp holdEp
                                [itemRef (-3) "steel_bar", itemRef (-3) "steel_bar"])
                `shouldBe` Left ErrDuplicateInstance

        it "keeps DIFFERENT invalid ids as separate per-item refusals" $ do
            -- -1 and -2 are not the same id, so they are not duplicates.
            let scene = sceneOf (source [] []) (cargoHold 200 [])
            case checkBatch scene (request acolyteEp holdEp
                                     [itemRef (-1) "a", itemRef (-2) "b"]) of
                Left e  → expectationFailure (show e)
                Right b → states b `shouldBe`
                    replicate 2 (TransferFailed
                        (requestFailure ReasonInstanceUnspecified))

        it "produces no outcomes and no plan for a whole-request error" $ do
            let scene = sceneOf (source [] []) (cargoHold 200 [])
            checkBatch scene (request acolyteEp holdEp []) `shouldSatisfy` isLeft
            -- validateBatch is the same gate, reachable without a scene
            -- at all: nothing was inspected, so nothing can have moved.
            validateBatch (request acolyteEp holdEp []) `shouldBe` Left ErrEmptyBatch

        it "gives both whole-request errors a distinct stable identifier" $ do
            map transferRequestErrorId allTransferRequestErrors
                `shouldBe` ["empty_batch", "duplicate_instance"]

    describe "per-item identity errors" $ do
        it "reports a zero instance id as instance_unspecified, per item" $ do
            let a     = mkItem "steel_bar" 61 0.8
                scene = sceneOf (source [a] []) (cargoHold 200 [])
                req   = request acolyteEp holdEp
                            [itemRef 0 "steel_bar", itemRef 61 "steel_bar"]
            case checkBatch scene req of
                Left e  → expectationFailure (show e)
                Right b → states b `shouldBe`
                    [ TransferFailed (requestFailure ReasonInstanceUnspecified)
                    , TransferQueued ]

        it "reports a NEGATIVE instance id as instance_unspecified, never a wrap" $ do
            let a     = mkItem "steel_bar" 61 0.8
                scene = sceneOf (source [a] []) (cargoHold 200 [])
                req   = request acolyteEp holdEp
                            [itemRef (-1) "steel_bar", itemRef 61 "steel_bar"]
            case checkBatch scene req of
                Left e  → expectationFailure (show e)
                Right b → states b `shouldBe`
                    [ TransferFailed (requestFailure ReasonInstanceUnspecified)
                    , TransferQueued ]

    describe "capacity" $ do
        it "refuses a building deposit that would exceed shipped capacity" $ do
            let stored = [mkItem "ballast" 90 199.5]
                item   = mkItem "steel_plate" 41 1.2
                scene  = sceneOf (source [item] []) (cargoHold 200 stored)
            refusal scene (toHold 41 "steel_plate")
                `shouldBe` Just (requestFailure ReasonReceiverFull)

        it "accepts a building deposit that exactly fills capacity" $ do
            let stored = [mkItem "ballast" 90 198.8]
                item   = mkItem "steel_plate" 41 1.2
                scene  = sceneOf (source [item] []) (cargoHold 200 stored)
            planOne scene (toHold 41 "steel_plate") `shouldSatisfy` isRight

        it "refuses a mule deposit that would exceed carrying capacity" $ do
            let loaded = [mkItem "steel_plate" 91 250.0]
                item   = mkItem "steel_bar" 55 0.8
                scene  = sceneOf (source [item] []) (mule 250.5 loaded)
            refusal scene (toMule 55 "steel_bar")
                `shouldBe` Just (requestFailure ReasonReceiverFull)

        it "refuses rather than over-encumbering a unit destination" $ do
            -- Stricter than the engine's general stance (withdrawals
            -- deliberately let a unit hold above its cap): a
            -- player-managed transfer refuses, like #920's pickup.
            let loaded = [mkItem "steel_plate" 91 250.4]
                item   = mkItem "steel_bar" 55 0.8
                scene  = sceneOf (source [item] []) (mule 250.5 loaded)
            case commitOne scene (toMule 55 "steel_bar") of
                Left f  → tfReason f `shouldBe` ReasonReceiverFull
                Right _ → expectationFailure "over-capacity transfer accepted"

    describe "proximity" $ do
        it "refuses a building further than one tile from a unit source" $ do
            let item  = mkItem "steel_plate" 41 1.2
                far   = buildingAt (13, 10) (1, 1) True 200 []
                scene = sceneOf (source [item] []) far
            refusal scene (toHold 41 "steel_plate")
                `shouldBe` Just (requestFailure ReasonOutOfRange)

        it "refuses a unit destination further than one tile away" $ do
            let item  = mkItem "steel_bar" 55 0.8
                far   = unitAt (12, 12) True 250.5 [] []
                scene = sceneOf (source [item] []) far
            refusal scene (toMule 55 "steel_bar")
                `shouldBe` Just (requestFailure ReasonOutOfRange)

        it "accepts a diagonally adjacent unit destination" $ do
            let item  = mkItem "steel_bar" 55 0.8
                scene = sceneOf (source [item] []) (mule 250.5 [])
            planOne scene (toMule 55 "steel_bar") `shouldSatisfy` isRight

        it "measures building ↔ building rect-to-rect, not anchor-to-anchor" $ do
            -- Unequal multi-tile footprints whose ANCHORS are 3 tiles
            -- apart but whose nearest occupied tiles are adjacent: the
            -- 2x2 hold covers x17..18, the 3x1 depot covers x14..16.
            -- An anchor-only (or one-rect-vs-one-tile) measure would
            -- refuse this.
            let item  = mkItem "steel_plate" 41 1.2
                hold  = buildingAt (17, 10) (2, 2) True 200 [item]
                depot = buildingAt (14, 10) (3, 1) True 200 []
                scene = sceneOf hold depot
            planOne scene (request holdEp depotEp [itemRef 41 "steel_plate"])
                `shouldSatisfy` isRight

        it "refuses two multi-tile buildings whose footprints do not touch" $ do
            let item  = mkItem "steel_plate" 41 1.2
                hold  = buildingAt (17, 10) (2, 2) True 200 [item]
                depot = buildingAt (13, 10) (2, 1) True 200 []
                scene = sceneOf hold depot
            refusal scene (request holdEp depotEp [itemRef 41 "steel_plate"])
                `shouldBe` Just (requestFailure ReasonOutOfRange)

        it "accepts a unit standing beside a multi-tile building's far edge" $ do
            -- The tile-vs-rect case has to fall out of the SAME measure
            -- the rect-vs-rect one uses.
            let item  = mkItem "steel_plate" 41 1.2
                wide  = buildingAt (11, 10) (4, 1) True 200 []
                near  = unitAt (15, 10) True 100 [item] []
                scene = sceneOf near wide
            planOne scene (request acolyteEp holdEp [itemRef 41 "steel_plate"])
                `shouldSatisfy` isRight

        it "refuses a cross-page pair at identical coordinates" $ do
            let item  = mkItem "steel_plate" 41 1.2
                away  = case cargoHold 200 [] of
                    BuildingEndpointAt b →
                        BuildingEndpointAt b { bevPage   = otherPage
                                             , bevAnchor = (10, 10) }
                    r → r
                scene = sceneOf (source [item] []) away
            refusal scene (toHold 41 "steel_plate")
                `shouldBe` Just (requestFailure ReasonOutOfRange)

    -- #1247: creating a durable ORDER validates everything EXCEPT
    -- adjacency, because the endpoints are not adjacent yet by
    -- definition. Everything below is about what that does and,
    -- equally, what it must not touch.
    describe "deferred reach (#1247)" $ do
        it "accepts a distant pair the required-reach policy refuses" $ do
            let item  = mkItem "steel_plate" 41 1.2
                far   = buildingAt (40, 10) (1, 1) True 200 []
                scene = sceneOf (source [item] []) far
                r     = toHold 41 "steel_plate"
            planItemWith ReachRequired scene (trSource r) (trDestination r)
                         (firstItem r)
                `shouldBe` Left (requestFailure ReasonOutOfRange)
            planItemWith (ReachDeferred homePage) scene (trSource r) (trDestination r)
                         (firstItem r)
                `shouldSatisfy` isRight

        it "reaches the CAPACITY verdict a range refusal would have \
           \pre-empted" $ do
            -- planItem checks range BEFORE capacity, so under the
            -- required policy an overweight DISTANT request reports
            -- out_of_range and the create-time capacity gate never
            -- runs at all. This is the exact ordering hazard deferring
            -- reach exists to remove.
            let item  = mkItem "crate" 41 500
                far   = buildingAt (40, 10) (1, 1) True 200 []
                scene = sceneOf (source [item] []) far
                r     = toHold 41 "crate"
            planItemWith (ReachDeferred homePage) scene (trSource r) (trDestination r)
                         (firstItem r)
                `shouldBe` Left (requestFailure ReasonReceiverFull)

        it "still refuses a CROSS-PAGE pair — deferring reach is not \
           \deferring the page" $ do
            let item  = mkItem "steel_plate" 41 1.2
                away  = buildingAt (11, 10) (1, 1) True 200 []
                away' = case away of
                    BuildingEndpointAt b →
                        BuildingEndpointAt b { bevPage = otherPage }
                    x → x
                scene = sceneOf (source [item] []) away'
                r     = toHold 41 "steel_plate"
            planItemWith (ReachDeferred homePage) scene (trSource r) (trDestination r)
                         (firstItem r)
                `shouldBe` Left (requestFailure ReasonOutOfRange)

        it "refuses endpoints that agree with EACH OTHER but not with \
           \the carrier's page" $ do
            -- The acting unit is recorded beside the endpoint pair, not
            -- derived from it, so a THIRD page is expressible: both ends
            -- on main_world, the carrier on second_world. That is not
            -- merely a walk it cannot make — the order would be stored
            -- in the carrier's page's store, where World.Save.Integrity
            -- scopes the acting unit AND both endpoints as blocking
            -- wrong-page errors, so accepting it would poison every
            -- later save of the session. Refuse at creation.
            let item  = mkItem "steel_plate" 41 1.2
                scene = sceneOf (source [item] []) (cargoHold 200 [])
                r     = toHold 41 "steel_plate"
            -- Both endpoints really are on one page…
            planItemWith (ReachDeferred homePage) scene (trSource r)
                         (trDestination r) (firstItem r)
                `shouldSatisfy` isRight
            -- …and it is not the carrier's.
            planItemWith (ReachDeferred otherPage) scene (trSource r)
                         (trDestination r) (firstItem r)
                `shouldBe` Left (requestFailure ReasonOutOfRange)

        it "keeps every other precondition, in the same order" $ do
            let item   = mkItem "steel_plate" 41 1.2
                shut   = buildingAt (40, 10) (1, 1) False 200 []
                scene  = sceneOf (source [item] []) shut
                r      = toHold 41 "steel_plate"
                absent = toHold 99 "steel_plate"
                open   = sceneOf (source [item] [])
                                 (buildingAt (40, 10) (1, 1) True 200 [])
            planItemWith (ReachDeferred homePage) scene (trSource r) (trDestination r)
                         (firstItem r)
                `shouldBe` Left (requestFailure ReasonReceiverIneligible)
            planItemWith (ReachDeferred homePage) open (trSource absent)
                         (trDestination absent) (firstItem absent)
                `shouldBe` Left (requestFailure ReasonInstanceMissing)

        it "leaves the DEFAULT policy required, so every existing \
           \caller is unchanged" $ do
            let item  = mkItem "steel_plate" 41 1.2
                far   = buildingAt (40, 10) (1, 1) True 200 []
                scene = sceneOf (source [item] []) far
                r     = toHold 41 "steel_plate"
            planOne scene r `shouldBe` Left (requestFailure ReasonOutOfRange)
            commitOne scene r `shouldBe` Left (requestFailure ReasonOutOfRange)
            checkBatch scene r `shouldBe`
                Right TransferBatch
                    { tbSource      = trSource r
                    , tbDestination = trDestination r
                    , tbEntries     =
                        [ QueuedTransfer (itemRef 41 "steel_plate")
                            (TransferFailed
                                (requestFailure ReasonOutOfRange)) ] }

        it "queues the first eight of twelve at a distance (D-1)" $ do
            -- The ordered, progressively remeasured check is the SAME
            -- one either way; only the range verdict differs.
            let items = [mkItem "crate" (fromIntegral i) 25 | i ← [41 .. 52 ∷ Int]]
                far   = buildingAt (40, 10) (1, 1) True 200 []
                scene = sceneOf (source items []) far
                r     = request acolyteEp holdEp
                            [itemRef (fromIntegral i) "crate" | i ← [41 .. 52 ∷ Int]]
            case checkBatchWith (ReachDeferred homePage) scene r of
                Left e  → expectationFailure ("unexpected: " <> show e)
                Right b → states b `shouldBe`
                    replicate 8 TransferQueued
                    ⧺ replicate 4 (TransferFailed
                                      (requestFailure ReasonReceiverFull))

    -- #1247: the failure twin of cancelBatch — an order self-terminating
    -- because it provably cannot finish, rather than somebody choosing
    -- to abandon one that still could.
    describe "self-termination (#1247)" $ do
        it "fails every PENDING entry and leaves terminal ones alone" $ do
            let a = QueuedTransfer (itemRef 41 "crate") TransferQueued
                b = QueuedTransfer (itemRef 42 "crate") TransferInTransit
                c = QueuedTransfer (itemRef 43 "crate") TransferReadyToCommit
                d = QueuedTransfer (itemRef 44 "crate") TransferCompleted
                e = QueuedTransfer (itemRef 45 "crate")
                        (TransferFailed (requestFailure ReasonReceiverFull))
                batch = TransferBatch acolyteEp holdEp [a, b, c, d, e]
                out   = requestFailure ReasonOutOfRange
            states (failPendingBatch out batch) `shouldBe`
                [ TransferFailed out, TransferFailed out, TransferFailed out
                , TransferCompleted
                , TransferFailed (requestFailure ReasonReceiverFull) ]

        it "carries a cause through, which is what tells a vanished \
           \counterpart from an unreachable one" $ do
            let batch = TransferBatch acolyteEp holdEp
                            [QueuedTransfer (itemRef 41 "crate") TransferQueued]
                gone  = staleFailure ReasonReceiverMissing
            states (failPendingBatch gone batch) `shouldBe` [TransferFailed gone]
            batchTerminal (failPendingBatch gone batch) `shouldBe` True

    describe "atomicity" $ do
        it "leaves both endpoints unchanged on a refusal" $ do
            let a      = mkItem "steel_bar" 61 0.8
                b      = mkItem "steel_bar" 62 0.8
                stored = [mkItem "ballast" 90 199.9]
                src    = source [a, b] []
                dst    = cargoHold 200 stored
                scene  = sceneOf src dst
            commitOne scene (toHold 62 "steel_bar")
                `shouldBe` Left (requestFailure ReasonReceiverFull)
            -- Nothing the policy returns can be applied, so both sides
            -- are still exactly what went in.
            endpointLooseItems src `shouldBe` [a, b]
            endpointLooseItems dst `shouldBe` stored

        it "preserves source order, not merely the multiset" $ do
            -- A rollback that appended would satisfy a set comparison;
            -- loose-storage order is UI-visible, so the contract reports
            -- the popped instance's ORIGINAL index for the caller to
            -- splice back into.
            let a     = mkItem "steel_bar" 61 0.8
                b     = mkItem "canteen"   62 0.5
                c'    = mkItem "steel_bar" 63 0.8
                scene = sceneOf (source [a, b, c'] []) (cargoHold 200 [])
            case commitOne scene (toHold 62 "canteen") of
                Left f  → expectationFailure (show f)
                Right c → do
                    tcIndex c `shouldBe` 1
                    let restored = take (tcIndex c) (tcSourceItems c)
                                 ⧺ [tcItem c]
                                 ⧺ drop (tcIndex c) (tcSourceItems c)
                    restored `shouldBe` [a, b, c']

        it "reports a building source's original index too" $ do
            let a     = mkItem "steel_bar" 61 0.8
                b     = mkItem "canteen"   62 0.5
                scene = sceneOf (cargoHold 200 [a, b]) (source [] [])
            case commitOne scene (request holdEp acolyteEp
                                          [itemRef 62 "canteen"]) of
                Left f  → expectationFailure (show f)
                Right c → do
                    tcIndex c `shouldBe` 1
                    tcSourceItems c `shouldBe` [a]

    describe "structured failure reasons" $ do
        it "gives every reason a distinct stable identifier" $ do
            let ids = map transferReasonId allTransferReasons
            sort ids `shouldBe` sort
                [ "instance_unspecified"
                , "source_missing", "source_ineligible"
                , "receiver_missing", "receiver_ineligible"
                , "instance_missing", "item_not_transferable"
                , "out_of_range", "receiver_full", "became_stale" ]

        it "no longer carries a quantity or operation-mismatch reason" $ do
            -- A2 removed trQuantity and the independent operation
            -- field; with nothing to disagree with, neither refusal has
            -- a reachable meaning any more.
            let ids = map transferReasonId allTransferReasons
            ids `shouldNotContain` ["quantity_unsupported"]
            ids `shouldNotContain` ["operation_mismatch"]

        it "gives every queued state a stable identifier" $ do
            allTransferStateIds `shouldBe`
                [ "queued", "in_transit", "ready_to_commit"
                , "completed", "cancelled", "failed" ]

        it "names both endpoint kinds without depending on position" $ do
            sort (map transferEndpointKindId allTransferEndpointKinds)
                `shouldBe` ["building", "unit"]

        it "gives every completion category a stable identifier" $ do
            map transferCompletionId
                [CompletionAll, CompletionPartial, CompletionNone]
                `shouldBe` ["all", "partial", "none"]

        it "is deterministic: the same broken scene always names the same reason" $ do
            let item  = mkItem "steel_plate" 41 1.2
                broke = buildingAt (30, 30) (1, 1) False 0 []
                scene = sceneOf (source [item] []) broke
                runs  = map (const (refusal scene (toHold 41 "steel_plate")))
                            [1 .. 5 ∷ Int]
            runs `shouldBe`
                replicate 5 (Just (requestFailure ReasonReceiverIneligible))

    describe "ordered batches" $ do
        it "queues every item of a batch that all fits" $ do
            let items = [mkItem "ration" (fromIntegral i) 0.5 | i ← [101 .. 103 ∷ Int]]
                refs  = [itemRef (fromIntegral i) "ration" | i ← [101 .. 103 ∷ Int]]
                scene = sceneOf (source items []) (cargoHold 200 [])
            case checkBatch scene (request acolyteEp holdEp refs) of
                Left e  → expectationFailure (show e)
                Right b → do
                    states b `shouldBe` replicate 3 TransferQueued
                    checkCompletion b `shouldBe` CompletionAll

        it "keeps the FIRST eight of twelve when only eight fit" $ do
            -- The canonical partial case: capacity is remeasured after
            -- every provisionally accepted item, so the earliest items
            -- win and the rest report receiver_full — in request order.
            let ids   = [101 .. 112 ∷ Int]
                items = [mkItem "ration" (fromIntegral i) 1.0 | i ← ids]
                refs  = [itemRef (fromIntegral i) "ration" | i ← ids]
                scene = sceneOf (source items []) (cargoHold 8 [])
            case checkBatch scene (request acolyteEp holdEp refs) of
                Left e  → expectationFailure (show e)
                Right b → do
                    states b `shouldBe`
                        replicate 8 TransferQueued
                        ⧺ replicate 4 (TransferFailed
                                         (requestFailure ReasonReceiverFull))
                    map (tirInstanceId . qtItem)
                        (filter ((≡ TransferQueued) . qtState) (tbEntries b))
                        `shouldBe` map fromIntegral [101 .. 108 ∷ Int]
                    batchQueuedCount b `shouldBe` 8
                    checkCompletion b `shouldBe` CompletionPartial

        it "does not let one failed item stop later unique items" $ do
            let a     = mkItem "steel_bar" 61 0.8
                c'    = mkItem "steel_bar" 63 0.8
                scene = sceneOf (source [a, c'] []) (cargoHold 200 [])
                req   = request acolyteEp holdEp
                            [ itemRef 61 "steel_bar"
                            , itemRef 62 "steel_bar"   -- never existed
                            , itemRef 63 "steel_bar" ]
            case checkBatch scene req of
                Left e  → expectationFailure (show e)
                Right b → states b `shouldBe`
                    [ TransferQueued
                    , TransferFailed (requestFailure ReasonInstanceMissing)
                    , TransferQueued ]

        it "reports one outcome per requested item, in request order" $ do
            let a     = mkItem "steel_bar" 61 0.8
                scene = sceneOf (source [a] []) (cargoHold 200 [])
                req   = request acolyteEp holdEp
                            [ itemRef 99 "steel_bar"
                            , itemRef 61 "steel_bar"
                            , itemRef 0  "steel_bar" ]
            case checkBatch scene req of
                Left e  → expectationFailure (show e)
                Right b → do
                    map (tirInstanceId . qtItem) (tbEntries b)
                        `shouldBe` [99, 61, 0]
                    length (tbEntries b) `shouldBe` length (trItems req)

        it "starts no movement when every item fails the initial check" $ do
            let scene = sceneOf (source [] []) (cargoHold 200 [])
                req   = request acolyteEp holdEp
                            [itemRef 61 "steel_bar", itemRef 62 "steel_bar"]
            case checkBatch scene req of
                Left e  → expectationFailure (show e)
                Right b → do
                    batchHasQueued b `shouldBe` False
                    checkCompletion b `shouldBe` CompletionNone
                    batchTerminal b `shouldBe` True

        it "repeats an endpoint-level refusal across every item" $ do
            -- Endpoint failures are NOT a third whole-request category:
            -- they surface as one ordinary per-item refusal each.
            let items = [mkItem "ration" (fromIntegral i) 0.5 | i ← [101 .. 103 ∷ Int]]
                refs  = [itemRef (fromIntegral i) "ration" | i ← [101 .. 103 ∷ Int]]
                far   = buildingAt (30, 30) (1, 1) True 200 []
                scene = sceneOf (source items []) far
            case checkBatch scene (request acolyteEp holdEp refs) of
                Left e  → expectationFailure (show e)
                Right b → do
                    states b `shouldBe` replicate 3
                        (TransferFailed (requestFailure ReasonOutOfRange))
                    checkCompletion b `shouldBe` CompletionNone

    describe "per-item queue lifecycle" $ do
        it "advances every queued entry together, leaving failures alone" $ do
            let a     = mkItem "steel_bar" 61 0.8
                scene = sceneOf (source [a] []) (cargoHold 200 [])
                req   = request acolyteEp holdEp
                            [itemRef 61 "steel_bar", itemRef 62 "steel_bar"]
            b0 ← checkedBatch scene req
            let b1 = markBatchInTransit b0
                b2 = markBatchReadyToCommit b1
            states b1 `shouldBe`
                [ TransferInTransit
                , TransferFailed (requestFailure ReasonInstanceMissing) ]
            states b2 `shouldBe`
                [ TransferReadyToCommit
                , TransferFailed (requestFailure ReasonInstanceMissing) ]

        it "walks queued → in transit → ready to commit → completed" $ do
            let a     = mkItem "steel_plate" 41 1.2
                scene = sceneOf (source [a] []) (cargoHold 200 [])
            b0 ← checkedBatch scene (toHold 41 "steel_plate")
            let (b3, commits) = commitBatch scene
                                  (markBatchReadyToCommit (markBatchInTransit b0))
            states b0 `shouldBe` [TransferQueued]
            states b3 `shouldBe` [TransferCompleted]
            map (iiInstanceId . tcItem) commits `shouldBe` [41]
            commitCompletion b3 `shouldBe` CompletionAll

        it "cancels every pending entry and leaves terminal ones alone" $ do
            let a     = mkItem "steel_bar" 61 0.8
                scene = sceneOf (source [a] []) (cargoHold 200 [])
                req   = request acolyteEp holdEp
                            [itemRef 61 "steel_bar", itemRef 62 "steel_bar"]
            b0   ← checkedBatch scene req
            done ← fst . commitBatch scene ⊚ arrived scene req
            states (cancelBatch b0) `shouldBe`
                [ TransferCancelled
                , TransferFailed (requestFailure ReasonInstanceMissing) ]
            states (cancelBatch done) `shouldBe`
                [ TransferCompleted
                , TransferFailed (requestFailure ReasonInstanceMissing) ]

        it "is terminal exactly when every entry is" $ do
            let a     = mkItem "steel_bar" 61 0.8
                scene = sceneOf (source [a] []) (cargoHold 200 [])
                req   = request acolyteEp holdEp
                            [itemRef 61 "steel_bar", itemRef 62 "steel_bar"]
            b0 ← checkedBatch scene req
            batchTerminal b0 `shouldBe` False
            batchTerminal (cancelBatch b0) `shouldBe` True

        it "will not commit before the order has arrived" $ do
            -- Only markBatchReadyToCommit records arrival, so a queued
            -- or in-transit entry is inert: nothing else stops C2 from
            -- handing an item over from across the map.
            let a     = mkItem "steel_plate" 41 1.2
                scene = sceneOf (source [a] []) (cargoHold 200 [])
            b0 ← checkedBatch scene (toHold 41 "steel_plate")
            let b1 = markBatchInTransit b0
            commitBatch scene b0 `shouldBe` (b0, [])
            commitBatch scene b1 `shouldBe` (b1, [])

        it "will not commit a cancelled entry" $ do
            let a     = mkItem "steel_plate" 41 1.2
                scene = sceneOf (source [a] []) (cargoHold 200 [])
            b0 ← checkedBatch scene (toHold 41 "steel_plate")
            let (b1, commits) = commitBatch scene (cancelBatch b0)
            states b1 `shouldBe` [TransferCancelled]
            commits `shouldBe` []

    describe "revalidation immediately before commit" $ do
        it "fails as stale when the destination fills up in transit" $ do
            let item     = mkItem "steel_plate" 41 1.2
                src      = source [item] []
                atQueue  = sceneOf src (cargoHold 200 [])
                atCommit = sceneOf src
                             (cargoHold 200 [mkItem "ballast" 90 199.9])
            ready ← arrived atQueue (toHold 41 "steel_plate")
            let (b1, commits) = commitBatch atCommit ready
            states b1 `shouldBe`
                [TransferFailed (staleFailure ReasonReceiverFull)]
            commits `shouldBe` []
            -- Both sides untouched: no commit was produced to apply.
            endpointLooseItems src `shouldBe` [item]

        it "fails as stale when the source instance is gone by commit" $ do
            let item     = mkItem "steel_plate" 41 1.2
                atQueue  = sceneOf (source [item] []) (cargoHold 200 [])
                atCommit = sceneOf (source [] []) (cargoHold 200 [])
            ready ← arrived atQueue (toHold 41 "steel_plate")
            states (fst (commitBatch atCommit ready)) `shouldBe`
                [TransferFailed (staleFailure ReasonInstanceMissing)]

        it "fails as stale when the destination is destroyed in transit" $ do
            let item     = mkItem "steel_plate" 41 1.2
                src      = source [item] []
                atQueue  = sceneOf src (cargoHold 200 [])
                atCommit = TransferScene { tscSource      = Just src
                                         , tscDestination = Nothing
                                         , tscWeigh       = weighBare }
            ready ← arrived atQueue (toHold 41 "steel_plate")
            states (fst (commitBatch atCommit ready)) `shouldBe`
                [TransferFailed (staleFailure ReasonReceiverMissing)]

        it "fails as stale when the source walked out of reach in transit" $ do
            let item     = mkItem "steel_plate" 41 1.2
                atQueue  = sceneOf (source [item] []) (cargoHold 200 [])
                walked   = unitAt (40, 40) True 100 [item] []
                atCommit = sceneOf walked (cargoHold 200 [])
            ready ← arrived atQueue (toHold 41 "steel_plate")
            states (fst (commitBatch atCommit ready)) `shouldBe`
                [TransferFailed (staleFailure ReasonOutOfRange)]

        it "fails as stale when the source changed world page in transit" $ do
            let item     = mkItem "steel_plate" 41 1.2
                atQueue  = sceneOf (source [item] []) (cargoHold 200 [])
                moved    = case source [item] [] of
                    UnitEndpointAt u → UnitEndpointAt u { uevPage = otherPage }
                    r → r
                atCommit = sceneOf moved (cargoHold 200 [])
            ready ← arrived atQueue (toHold 41 "steel_plate")
            states (fst (commitBatch atCommit ready)) `shouldBe`
                [TransferFailed (staleFailure ReasonOutOfRange)]

        it "reports the stale reason distinctly from a request-time one" $ do
            let item     = mkItem "steel_plate" 41 1.2
                src      = source [item] []
                full     = cargoHold 200 [mkItem "ballast" 90 199.9]
                atQueue  = sceneOf src (cargoHold 200 [])
                atCommit = sceneOf src full
            ready ← arrived atQueue (toHold 41 "steel_plate")
            refusal atCommit (toHold 41 "steel_plate")
                `shouldBe` Just (requestFailure ReasonReceiverFull)
            case states (fst (commitBatch atCommit ready)) of
                [TransferFailed f] → do
                    transferReasonId (tfReason f) `shouldBe` "became_stale"
                    fmap transferReasonId (tfCause f)
                        `shouldBe` Just "receiver_full"
                s → expectationFailure ("expected one failure, got " <> show s)

        it "re-reads capacity after every successful sibling" $ do
            -- All three pass the initial check against an empty hold;
            -- by arrival it holds 8 kg of the 10 kg cap, so the first
            -- two land and the third goes stale — proof the commit loop
            -- re-measures between siblings rather than trusting the
            -- create-time verdict.
            let ids      = [101 .. 103 ∷ Int]
                items    = [mkItem "ration" (fromIntegral i) 1.0 | i ← ids]
                refs     = [itemRef (fromIntegral i) "ration" | i ← ids]
                src      = source items []
                atQueue  = sceneOf src (cargoHold 10 [])
                atCommit = sceneOf src (cargoHold 10 [mkItem "ballast" 90 8.0])
            ready ← arrived atQueue (request acolyteEp holdEp refs)
            let (b1, commits) = commitBatch atCommit ready
            states b1 `shouldBe`
                [ TransferCompleted, TransferCompleted
                , TransferFailed (staleFailure ReasonReceiverFull) ]
            map (iiInstanceId . tcItem) commits `shouldBe` [101, 102]
            commitCompletion b1 `shouldBe` CompletionPartial

        it "does not let one stale sibling roll back or block the others" $ do
            let a        = mkItem "steel_bar" 61 0.8
                b        = mkItem "canteen"   62 0.5
                c'       = mkItem "steel_bar" 63 0.8
                refs     = [ itemRef 61 "steel_bar", itemRef 62 "canteen"
                           , itemRef 63 "steel_bar" ]
                atQueue  = sceneOf (source [a, b, c'] []) (cargoHold 200 [])
                -- The canteen is gone by the time the order arrives.
                atCommit = sceneOf (source [a, c'] []) (cargoHold 200 [])
            ready ← arrived atQueue (request acolyteEp holdEp refs)
            let (b1, commits) = commitBatch atCommit ready
            states b1 `shouldBe`
                [ TransferCompleted
                , TransferFailed (staleFailure ReasonInstanceMissing)
                , TransferCompleted ]
            map (iiInstanceId . tcItem) commits `shouldBe` [61, 63]
            commitCompletion b1 `shouldBe` CompletionPartial

-- | A shipped unit def, decoded straight out of its YAML.
shippedUnit ∷ Text → IO UnitYamlDef
shippedUnit name = do
    r ← Yaml.decodeFileEither
          ("data/units/" <> T.unpack name <> ".yaml")
    case r of
        Left e   → fail (show e)
        Right uf → case filter ((≡ name) . uydName) (uyfUnits uf) of
            (d:_) → pure d
            []    → fail ("no " <> T.unpack name <> " def")
