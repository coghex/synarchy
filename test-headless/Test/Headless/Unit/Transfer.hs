-- | The player-managed unit→container transfer contract (#1000, epic
--   #1013 phase A1). Exercises the pure policy in "Unit.Transfer"
--   directly — the same shape 'Test.Headless.Craft.Execute' uses for
--   craft consumption — against the SHIPPED capacity semantics decoded
--   out of @data/buildings/cargo_hold_S.yaml@ and
--   @data/units/technomule.yaml@, so a data change that breaks the
--   contract fails here rather than in a probe.
--
--   The Lua-integrated verbs (@unit.checkTransfer@ /
--   @unit.commitTransfer@) wrap exactly these functions; what they add
--   is projecting the live managers and applying the returned lists.
module Test.Headless.Unit.Transfer (spec) where

import UPrelude
import Test.Hspec
import Data.Either (isRight)
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

-- | Weigh by the instance's own weight. The engine passes
--   @itemTotalWeight itemMgr@ here; with no container defs in scope
--   that reduces to exactly this for a fill-free, contents-free item.
weighBare ∷ ItemInstance → Float
weighBare = iiWeight

-- | An acolyte at tile (10, 10) carrying @inv@, wearing @worn@.
source ∷ [ItemInstance] → [ItemInstance] → TransferSourceView
source inv worn = TransferSourceView
    { tsvPage      = homePage
    , tsvTile      = (10, 10)
    , tsvInventory = inv
    , tsvEquipped  = worn
    }

-- | A 1x1 built cargo hold anchored at (11, 10) — Chebyshev 1 from the
--   source, i.e. exactly at the edge of the reach rule.
cargoHold ∷ Float → [ItemInstance] → TransferReceiverView
cargoHold cap stored = BuildingReceiverAt BuildingReceiverView
    { brvPage         = homePage
    , brvAnchor       = (11, 10)
    , brvTileSize     = (1, 1)
    , brvBuilt        = True
    , brvCapacity     = cap
    , brvStorage      = stored
    , brvStoredWeight = sum (map weighBare stored)
    }

-- | A technomule standing at (11, 11) — diagonally adjacent.
mule ∷ Float → [ItemInstance] → TransferReceiverView
mule cap inv = UnitReceiverAt UnitReceiverView
    { urvPage          = homePage
    , urvTile          = (11, 11)
    , urvIsReceiver    = True
    , urvCapacity      = cap
    , urvInventory     = inv
    , urvCarriedWeight = sum (map weighBare inv)
    }

sceneOf ∷ TransferSourceView → TransferReceiverView → TransferScene
sceneOf src rcv = TransferScene
    { tscSource   = Just src
    , tscReceiver = Just rcv
    , tscWeigh    = weighBare
    }

toHold ∷ Word64 → Text → TransferRequest
toHold iid name = TransferRequest
    { trSource     = sourceUid
    , trReceiver   = ReceiverBuilding holdBid
    , trInstanceId = iid
    , trDefName    = name
    , trQuantity   = 1
    , trOperation  = ToBuildingStorage
    }

toMule ∷ Word64 → Text → TransferRequest
toMule iid name = TransferRequest
    { trSource     = sourceUid
    , trReceiver   = ReceiverUnit muleUid
    , trInstanceId = iid
    , trDefName    = name
    , trQuantity   = 1
    , trOperation  = ToUnitInventory
    }

-- | The reason a scene/request pair is refused, or Nothing on success.
refusal ∷ TransferScene → TransferRequest → Maybe TransferFailure
refusal scene req = either Just (const Nothing) (planTransfer scene req)

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

        it "reads the technomule's transfer_receiver marker from shipped data" $ do
            d ← shippedMule
            uydTransferReceiver d `shouldBe` True

        it "leaves every other shipped unit def a non-receiver" $ do
            -- The marker is opt-in: a new optional YAML field must not
            -- silently turn acolytes and wildlife into containers, and
            -- their files must still decode unchanged.
            marks ← mapM shippedUnitMarker
                [ "acolyte", "red_squirrel", "bear_brown" ]
            marks `shouldBe` map (const False) marks

        it "applies the technomule's innate +50% capacity modifier" $ do
            -- The eligibility marker is data; so is the capacity. The
            -- contract measures a unit receiver against the
            -- modifier-applied carrying_capacity, which for the shipped
            -- mule is its body-derived base lifted by "cybernetic
            -- enhancements".
            d ← shippedMule
            let mods = [ StatModifier { smDelta   = uymDelta m
                                      , smSource  = uymSource m
                                      , smExpiry  = Nothing
                                      , smPercent = uymPercent m }
                       | m ← uydModifiers d
                       , uymStat m ≡ "carrying_capacity" ]
            length mods `shouldBe` 1
            -- Base ≈ 167 kg from 3.2 × (lean_mass × strength)^0.6.
            effectiveStat 0 167.0 mods `shouldBe` 250.5

    describe "successful transfers" $ do
        it "moves an exact instance into a storage building" $ do
            let item  = mkItem "steel_plate" 41 1.2
                src   = source [item] []
                scene = sceneOf src (cargoHold 200 [])
            case commitTransfer scene (toHold 41 "steel_plate") of
                Left f  → expectationFailure (show f)
                Right c → do
                    iiInstanceId (tcItem c) `shouldBe` 41
                    tcSourceInventory c `shouldBe` []
                    map iiInstanceId (tcReceiverItems c) `shouldBe` [41]

        it "moves an exact instance into the technomule" $ do
            let item  = mkItem "steel_bar" 55 0.8
                src   = source [item] []
                scene = sceneOf src (mule 250.5 [])
            case commitTransfer scene (toMule 55 "steel_bar") of
                Left f  → expectationFailure (show f)
                Right c → do
                    iiInstanceId (tcItem c) `shouldBe` 55
                    tcSourceInventory c `shouldBe` []
                    map iiInstanceId (tcReceiverItems c) `shouldBe` [55]

        it "moves exactly one instance exactly once" $ do
            -- Three identical-def instances, one named: the other two
            -- stay put, in order, and the named one appears in the
            -- receiver exactly once.
            let a     = mkItem "steel_bar" 61 0.8
                b     = mkItem "steel_bar" 62 0.8
                c'    = mkItem "steel_bar" 63 0.8
                src   = source [a, b, c'] []
                scene = sceneOf src (cargoHold 200 [])
            case commitTransfer scene (toHold 62 "steel_bar") of
                Left f  → expectationFailure (show f)
                Right c → do
                    map iiInstanceId (tcSourceInventory c) `shouldBe` [61, 63]
                    map iiInstanceId (tcReceiverItems c) `shouldBe` [62]
                    length (filter ((≡ 62) . iiInstanceId)
                                   (tcReceiverItems c)) `shouldBe` 1

        it "distinguishes same-definition instances" $ do
            -- Same def name, different physical items: the request's
            -- instanceId decides, so a merged UI row can never move the
            -- wrong canteen.
            let full  = (mkItem "canteen" 71 0.5) { iiCurrentFill = 1.0 }
                empty = mkItem "canteen" 72 0.5
                src   = source [full, empty] []
                scene = sceneOf src (cargoHold 200 [])
            case commitTransfer scene (toHold 72 "canteen") of
                Left f  → expectationFailure (show f)
                Right c → do
                    iiInstanceId (tcItem c) `shouldBe` 72
                    iiCurrentFill (tcItem c) `shouldBe` 0
                    map iiInstanceId (tcSourceInventory c) `shouldBe` [71]

    describe "rejections" $ do
        it "rejects a missing or stale instance id" $ do
            let src   = source [mkItem "steel_bar" 61 0.8] []
                scene = sceneOf src (cargoHold 200 [])
            refusal scene (toHold 999 "steel_bar")
                `shouldBe` Just (requestFailure ReasonInstanceMissing)

        it "rejects an instance id that no longer names the requested def" $ do
            let src   = source [mkItem "steel_bar" 61 0.8] []
                scene = sceneOf src (cargoHold 200 [])
            refusal scene (toHold 61 "steel_plate")
                `shouldBe` Just (requestFailure ReasonInstanceMissing)

        it "rejects a def name with no instance id" $ do
            -- A def name alone is insufficient identity for this path,
            -- even though the legacy AI verbs still accept it.
            let src   = source [mkItem "steel_bar" 61 0.8] []
                scene = sceneOf src (cargoHold 200 [])
            refusal scene (toHold 0 "steel_bar")
                `shouldBe` Just (requestFailure ReasonInstanceUnspecified)

        it "rejects an equipped item" $ do
            let worn  = mkItem "steel_dagger" 81 1.0
                src   = source [] [worn]
                scene = sceneOf src (cargoHold 200 [])
            refusal scene (toHold 81 "steel_dagger")
                `shouldBe` Just (requestFailure ReasonItemNotTransferable)

        it "rejects an accessory" $ do
            let robe  = mkItem "acolyte_robe" 82 1.5
                src   = source [] [robe]
                scene = sceneOf src (cargoHold 200 [])
            refusal scene (toHold 82 "acolyte_robe")
                `shouldBe` Just (requestFailure ReasonItemNotTransferable)

        it "rejects an unbuilt storage building" $ do
            let item  = mkItem "steel_plate" 41 1.2
                ghost = case cargoHold 200 [] of
                    BuildingReceiverAt b →
                        BuildingReceiverAt b { brvBuilt = False }
                    r → r
                scene = sceneOf (source [item] []) ghost
            refusal scene (toHold 41 "steel_plate")
                `shouldBe` Just (requestFailure ReasonReceiverIneligible)

        it "rejects a built building with no storage capacity" $ do
            let item  = mkItem "steel_plate" 41 1.2
                scene = sceneOf (source [item] []) (cargoHold 0 [])
            refusal scene (toHold 41 "steel_plate")
                `shouldBe` Just (requestFailure ReasonReceiverIneligible)

        it "rejects a unit that is not marked transfer-capable" $ do
            -- An acolyte is not a container. The marker is the whole
            -- eligibility rule — no def-name comparison anywhere.
            let item    = mkItem "steel_bar" 55 0.8
                acolyte = case mule 40 [] of
                    UnitReceiverAt u →
                        UnitReceiverAt u { urvIsReceiver = False }
                    r → r
                scene   = sceneOf (source [item] []) acolyte
            refusal scene (toMule 55 "steel_bar")
                `shouldBe` Just (requestFailure ReasonReceiverIneligible)

        it "rejects a missing receiver" $ do
            let scene = TransferScene
                    { tscSource   = Just (source [mkItem "steel_bar" 61 0.8] [])
                    , tscReceiver = Nothing
                    , tscWeigh    = weighBare
                    }
            refusal scene (toHold 61 "steel_bar")
                `shouldBe` Just (requestFailure ReasonReceiverMissing)

        it "rejects a missing source unit" $ do
            let scene = TransferScene
                    { tscSource   = Nothing
                    , tscReceiver = Just (cargoHold 200 [])
                    , tscWeigh    = weighBare
                    }
            refusal scene (toHold 61 "steel_bar")
                `shouldBe` Just (requestFailure ReasonSourceMissing)

        it "rejects a receiver kind the operation does not name" $ do
            let item  = mkItem "steel_bar" 55 0.8
                scene = sceneOf (source [item] []) (mule 250.5 [])
            -- A building operation aimed at a unit receiver.
            refusal scene ((toMule 55 "steel_bar")
                             { trOperation = ToBuildingStorage })
                `shouldBe` Just (requestFailure ReasonOperationMismatch)

        it "rejects a unit transferring into itself" $ do
            let item  = mkItem "steel_bar" 55 0.8
                scene = sceneOf (source [item] []) (mule 250.5 [])
            refusal scene ((toMule 55 "steel_bar")
                             { trReceiver = ReceiverUnit sourceUid })
                `shouldBe` Just (requestFailure ReasonReceiverIneligible)

        it "rejects a quantity other than one" $ do
            let item  = mkItem "steel_bar" 61 0.8
                scene = sceneOf (source [item] []) (cargoHold 200 [])
            refusal scene ((toHold 61 "steel_bar") { trQuantity = 2 })
                `shouldBe` Just (requestFailure ReasonQuantityUnsupported)

    describe "capacity" $ do
        it "rejects a building deposit that would exceed shipped capacity" $ do
            -- 199.5 kg already stored in the shipped 200 kg hold: a
            -- 1.2 kg plate does not fit.
            let stored = [mkItem "ballast" 90 199.5]
                item   = mkItem "steel_plate" 41 1.2
                scene  = sceneOf (source [item] []) (cargoHold 200 stored)
            refusal scene (toHold 41 "steel_plate")
                `shouldBe` Just (requestFailure ReasonReceiverFull)

        it "accepts a building deposit that exactly fills capacity" $ do
            let stored = [mkItem "ballast" 90 198.8]
                item   = mkItem "steel_plate" 41 1.2
                scene  = sceneOf (source [item] []) (cargoHold 200 stored)
            planTransfer scene (toHold 41 "steel_plate") `shouldSatisfy` isRight

        it "rejects a mule deposit that would exceed carrying capacity" $ do
            -- The shipped mule spawns already loaded, so an
            -- insufficient-capacity case needs a pre-loaded receiver —
            -- not the default spawn state.
            let loaded = [mkItem "steel_plate" 91 250.0]
                item   = mkItem "steel_bar" 55 0.8
                scene  = sceneOf (source [item] []) (mule 250.5 loaded)
            refusal scene (toMule 55 "steel_bar")
                `shouldBe` Just (requestFailure ReasonReceiverFull)

        it "refuses rather than over-encumbering a unit receiver" $ do
            -- Stricter than the engine's general stance (withdrawals
            -- deliberately let a unit hold above its cap): a
            -- player-managed transfer refuses, like #920's pickup.
            let loaded = [mkItem "steel_plate" 91 250.4]
                item   = mkItem "steel_bar" 55 0.8
                scene  = sceneOf (source [item] []) (mule 250.5 loaded)
            case commitTransfer scene (toMule 55 "steel_bar") of
                Left f  → tfReason f `shouldBe` ReasonReceiverFull
                Right _ → expectationFailure "over-capacity transfer accepted"

    describe "proximity" $ do
        it "rejects a building further than one tile from the source" $ do
            let item  = mkItem "steel_plate" 41 1.2
                far   = case cargoHold 200 [] of
                    BuildingReceiverAt b →
                        BuildingReceiverAt b { brvAnchor = (13, 10) }
                    r → r
                scene = sceneOf (source [item] []) far
            refusal scene (toHold 41 "steel_plate")
                `shouldBe` Just (requestFailure ReasonOutOfRange)

        it "rejects a unit receiver further than one tile away" $ do
            let item  = mkItem "steel_bar" 55 0.8
                far   = case mule 250.5 [] of
                    UnitReceiverAt u → UnitReceiverAt u { urvTile = (12, 12) }
                    r → r
                scene = sceneOf (source [item] []) far
            refusal scene (toMule 55 "steel_bar")
                `shouldBe` Just (requestFailure ReasonOutOfRange)

        it "accepts a diagonally adjacent receiver" $ do
            let item  = mkItem "steel_bar" 55 0.8
                scene = sceneOf (source [item] []) (mule 250.5 [])
            planTransfer scene (toMule 55 "steel_bar") `shouldSatisfy` isRight

        it "rejects a cross-page pair at identical coordinates" $ do
            let item  = mkItem "steel_plate" 41 1.2
                away  = case cargoHold 200 [] of
                    BuildingReceiverAt b →
                        BuildingReceiverAt b { brvPage   = otherPage
                                             , brvAnchor = (10, 10) }
                    r → r
                scene = sceneOf (source [item] []) away
            refusal scene (toHold 41 "steel_plate")
                `shouldBe` Just (requestFailure ReasonOutOfRange)

    describe "atomicity" $ do
        it "leaves both inventories unchanged on a refusal" $ do
            let a      = mkItem "steel_bar" 61 0.8
                b      = mkItem "steel_bar" 62 0.8
                stored = [mkItem "ballast" 90 199.9]
                src    = source [a, b] []
                rcv    = cargoHold 200 stored
                scene  = sceneOf src rcv
            commitTransfer scene (toHold 62 "steel_bar")
                `shouldBe` Left (requestFailure ReasonReceiverFull)
            -- Nothing the policy returns can be applied, so both sides
            -- are still exactly what went in.
            tsvInventory src `shouldBe` [a, b]
            case rcv of
                BuildingReceiverAt bv → brvStorage bv `shouldBe` stored
                _                     → expectationFailure "wrong receiver kind"

        it "preserves source order, not merely the multiset" $ do
            -- A rollback that appended would satisfy a set comparison;
            -- inventory order is UI-visible, so the contract reports the
            -- popped instance's ORIGINAL index for the caller to splice
            -- back into.
            let a     = mkItem "steel_bar" 61 0.8
                b     = mkItem "canteen"   62 0.5
                c'    = mkItem "steel_bar" 63 0.8
                scene = sceneOf (source [a, b, c'] []) (cargoHold 200 [])
            case commitTransfer scene (toHold 62 "canteen") of
                Left f  → expectationFailure (show f)
                Right c → do
                    tcIndex c `shouldBe` 1
                    -- Splicing the item back at tcIndex restores the
                    -- original list exactly, order included.
                    let restored = take (tcIndex c) (tcSourceInventory c)
                                 ++ [tcItem c]
                                 ++ drop (tcIndex c) (tcSourceInventory c)
                    restored `shouldBe` [a, b, c']

    describe "structured failure reasons" $ do
        it "gives every reason a distinct stable identifier" $ do
            let ids = map transferReasonId allTransferReasons
            sort ids `shouldBe` sort
                [ "quantity_unsupported", "instance_unspecified"
                , "source_missing", "receiver_missing"
                , "operation_mismatch", "receiver_ineligible"
                , "instance_missing", "item_not_transferable"
                , "out_of_range", "receiver_full", "became_stale" ]

        it "gives every queued state and operation a stable identifier" $ do
            allTransferStateIds `shouldBe`
                [ "queued", "in_transit", "ready_to_commit"
                , "completed", "cancelled", "failed" ]
            map transferOperationId allTransferOperations `shouldBe`
                [ "unit_to_building_storage", "unit_to_unit_inventory" ]

        it "is deterministic: the same broken scene always names the same reason" $ do
            -- Several preconditions broken at once resolves to ONE
            -- reason, the same one every time, so D1 can present it.
            let item  = mkItem "steel_plate" 41 1.2
                broke = case cargoHold 0 [] of
                    BuildingReceiverAt b →
                        BuildingReceiverAt b { brvBuilt  = False
                                             , brvAnchor = (30, 30) }
                    r → r
                scene = sceneOf (source [item] []) broke
                runs  = map (const (refusal scene (toHold 41 "steel_plate")))
                            [1 .. 5 ∷ Int]
            runs `shouldBe`
                replicate 5 (Just (requestFailure ReasonReceiverIneligible))

    describe "queued transfer state" $ do
        it "queues a request whose preconditions all hold" $ do
            let item  = mkItem "steel_plate" 41 1.2
                scene = sceneOf (source [item] []) (cargoHold 200 [])
            qtState (queueTransfer scene (toHold 41 "steel_plate"))
                `shouldBe` TransferQueued

        it "fails a request at creation when a precondition is broken" $ do
            let item  = mkItem "steel_plate" 41 1.2
                scene = sceneOf (source [item] []) (cargoHold 0 [])
            qtState (queueTransfer scene (toHold 41 "steel_plate"))
                `shouldBe`
                    TransferFailed (requestFailure ReasonReceiverIneligible)

        it "walks queued → in transit → ready to commit → completed" $ do
            let item  = mkItem "steel_plate" 41 1.2
                scene = sceneOf (source [item] []) (cargoHold 200 [])
                q0    = queueTransfer scene (toHold 41 "steel_plate")
                q1    = markInTransit q0
                q2    = markReadyToCommit q1
                (q3, mCommit) = commitQueued scene q2
            qtState q1 `shouldBe` TransferInTransit
            qtState q2 `shouldBe` TransferReadyToCommit
            qtState q3 `shouldBe` TransferCompleted
            fmap (iiInstanceId . tcItem) mCommit `shouldBe` Just 41

        it "cancels a pending request and leaves terminal ones alone" $ do
            let item  = mkItem "steel_plate" 41 1.2
                scene = sceneOf (source [item] []) (cargoHold 200 [])
                q0    = queueTransfer scene (toHold 41 "steel_plate")
                (done, _) = commitQueued scene (markReadyToCommit
                                                  (markInTransit q0))
            qtState (cancelTransfer q0) `shouldBe` TransferCancelled
            qtState (cancelTransfer done) `shouldBe` TransferCompleted

        it "will not commit a cancelled request" $ do
            let item  = mkItem "steel_plate" 41 1.2
                scene = sceneOf (source [item] []) (cargoHold 200 [])
                q0    = cancelTransfer
                          (queueTransfer scene (toHold 41 "steel_plate"))
                (q1, mCommit) = commitQueued scene q0
            qtState q1 `shouldBe` TransferCancelled
            mCommit `shouldBe` Nothing

    describe "revalidation immediately before commit" $ do
        it "fails as stale when the receiver fills up in transit" $ do
            -- Passes every check at request time; the hold is full by
            -- the time the unit arrives.
            let item     = mkItem "steel_plate" 41 1.2
                src      = source [item] []
                atQueue  = sceneOf src (cargoHold 200 [])
                atCommit = sceneOf src
                             (cargoHold 200 [mkItem "ballast" 90 199.9])
                q0       = markReadyToCommit (markInTransit
                             (queueTransfer atQueue (toHold 41 "steel_plate")))
                (q1, mCommit) = commitQueued atCommit q0
            qtState q0 `shouldBe` TransferReadyToCommit
            qtState q1 `shouldBe`
                TransferFailed (staleFailure ReasonReceiverFull)
            mCommit `shouldBe` Nothing
            -- Both sides untouched: no commit was produced to apply.
            tsvInventory src `shouldBe` [item]

        it "fails as stale when the source instance is gone by commit" $ do
            let item     = mkItem "steel_plate" 41 1.2
                atQueue  = sceneOf (source [item] []) (cargoHold 200 [])
                atCommit = sceneOf (source [] []) (cargoHold 200 [])
                q0       = markInTransit
                             (queueTransfer atQueue (toHold 41 "steel_plate"))
                (q1, mCommit) = commitQueued atCommit q0
            qtState q1 `shouldBe`
                TransferFailed (staleFailure ReasonInstanceMissing)
            mCommit `shouldBe` Nothing

        it "fails as stale when the receiver is destroyed in transit" $ do
            let item     = mkItem "steel_plate" 41 1.2
                src      = source [item] []
                atQueue  = sceneOf src (cargoHold 200 [])
                atCommit = TransferScene { tscSource   = Just src
                                         , tscReceiver = Nothing
                                         , tscWeigh    = weighBare }
                q0       = markInTransit
                             (queueTransfer atQueue (toHold 41 "steel_plate"))
                (q1, mCommit) = commitQueued atCommit q0
            qtState q1 `shouldBe`
                TransferFailed (staleFailure ReasonReceiverMissing)
            mCommit `shouldBe` Nothing

        it "reports the stale reason distinctly from a request-time one" $ do
            -- Same broken precondition, two stages: the identifier the
            -- player sees differs, and the stale form still names the
            -- precondition that broke.
            let item     = mkItem "steel_plate" 41 1.2
                src      = source [item] []
                full     = cargoHold 200 [mkItem "ballast" 90 199.9]
                atQueue  = sceneOf src (cargoHold 200 [])
                atCommit = sceneOf src full
                q0       = markInTransit
                             (queueTransfer atQueue (toHold 41 "steel_plate"))
                (q1, _)  = commitQueued atCommit q0
            refusal atCommit (toHold 41 "steel_plate")
                `shouldBe` Just (requestFailure ReasonReceiverFull)
            case qtState q1 of
                TransferFailed f → do
                    transferReasonId (tfReason f) `shouldBe` "became_stale"
                    fmap transferReasonId (tfCause f)
                        `shouldBe` Just "receiver_full"
                s → expectationFailure ("expected failure, got " <> show s)

-- | The shipped technomule def, decoded straight out of its YAML.
shippedMule ∷ IO UnitYamlDef
shippedMule = shippedUnit "technomule"

shippedUnit ∷ Text → IO UnitYamlDef
shippedUnit name = do
    r ← Yaml.decodeFileEither
          ("data/units/" <> T.unpack name <> ".yaml")
    case r of
        Left e   → fail (show e)
        Right uf → case filter ((≡ name) . uydName) (uyfUnits uf) of
            (d:_) → pure d
            []    → fail ("no " <> T.unpack name <> " def")

shippedUnitMarker ∷ Text → IO Bool
shippedUnitMarker name = uydTransferReceiver ⊚ shippedUnit name
