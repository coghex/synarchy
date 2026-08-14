-- | Durable transfer orders through the LIVE engine (#1246, epic #1013
--   slice UIT-2A) — the half of the persistence contract that a pure,
--   derived-'Eq' codec test structurally cannot reach.
--
--   "Test.Headless.World.Save.Contract" builds a 'SessionSnapshot'
--   literal and proves the real envelope codec round-trips every field
--   of it. That says nothing about whether the PRODUCTION capture ever
--   reads @wsTransferOrdersRef@, or whether load staging ever writes it
--   back: a missing @readIORef@ and a missing @writeIORef@ are both
--   invisible to a test whose snapshot never came from, or went back to,
--   live state. So this module drives the real world thread instead —
--   'World.Thread.Command.Save.WriteWorld' capturing a live page's order
--   store into a real @world.synworld@ on disk, and
--   'World.Load.Stage.stageSession' reconstructing it into a staged
--   'World.State.Types.WorldState'.
--
--   Staging only, never publishing (the same rule
--   "Test.Headless.World.Identity" follows, and for the same reason: a
--   real publish REPLACES the whole session and would wipe every other
--   spec's live pages). The publish step itself is a page-map swap that
--   reads nothing component-specific; the genuinely component-specific
--   restore write is the one staging performs, which is what is asserted
--   here. The full fresh-process cycle lives in
--   @tools/persistence_contract_probe.py@.
--
--   Runs under the @persistence contract@ describe (see Spec.hs) so it
--   is covered by that gate's @--match@.
module Test.Headless.World.TransferOrders (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.Int (Int64)
import Data.IORef (readIORef, writeIORef)
import Data.List (find)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import System.Directory (doesFileExist, removePathForcibly)

import Engine.Core.State (EngineEnv(..))
import Test.Headless.Harness (sendWorldCommand, waitForWorldInit)
import World.Types
import World.Load.Stage (stageSession, renderStageError)
import World.Load.Types (StagedSession(..), StagedPage(..))
import World.Save.Serialize (loadWorld)
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))
import Unit.Transfer
    ( TransferBatch(..), TransferEndpoint(..), TransferItemRef(..)
    , TransferReason(..), TransferState(..), QueuedTransfer(..)
    , requestFailure, staleFailure )
import Unit.Transfer.Orders
    ( TransferOrder(..), TransferOrderId(..), TransferOrders(..)
    , addTransferOrder, emptyTransferOrders, transferOrderList )

pageId ∷ WorldPageId
pageId = WorldPageId "tro_live_w8"

slotName ∷ Text
slotName = "tro_live_spec"

entry ∷ Int64 → Text → TransferState → QueuedTransfer
entry iid nm st = QueuedTransfer
    { qtItem  = TransferItemRef { tirInstanceId = iid, tirDefName = nm }
    , qtState = st }

-- | The live store this spec writes into the page. Deliberately built
--   through the real 'addTransferOrder' surface, so the allocator that
--   travels with it is one the store itself issued rather than a number
--   typed into a fixture.
--
--   Every reference DANGLES: the page is freshly generated and holds no
--   units, buildings or items at all. That is the point — a dangling
--   target is tolerated gameplay (a carrier that died, a destination
--   demolished before the save was taken), so a save carrying one must
--   still SUCCEED and a load must restore the order verbatim rather than
--   pruning it. The wrong-page half, which really is fatal, is exercised
--   purely in "Test.Headless.World.Save.Integrity".
liveOrders ∷ TransferOrders
liveOrders =
    let (afterFirst, _)  = addTransferOrder (UnitId 11) unitToBuilding
                               emptyTransferOrders
        (afterSecond, _) = addTransferOrder (UnitId 11) buildingToBuilding
                               afterFirst
    in afterSecond
  where
    unitToBuilding = TransferBatch
        { tbSource      = EndpointUnit (UnitId 11)
        , tbDestination = EndpointBuilding (BuildingId 777)
        , tbEntries =
            [ entry 5001 "bandage"       TransferQueued
            , entry 5002 "first_aid_kit" TransferInTransit
            , entry 5003 "bandage"       TransferReadyToCommit
            , entry 5004 "bandage"       TransferCompleted
            ] }
    -- D-10: both ends buildings, acting unit neither of them.
    buildingToBuilding = TransferBatch
        { tbSource      = EndpointBuilding (BuildingId 777)
        , tbDestination = EndpointBuilding (BuildingId 778)
        , tbEntries =
            [ entry 5005 "bandage" TransferCancelled
            , entry 5006 "bandage"
                (TransferFailed (requestFailure ReasonReceiverFull))
            , entry 5007 "bandage"
                (TransferFailed (staleFailure ReasonInstanceMissing))
            ] }

-- | Poll until the world thread has written the save file (mirrors
--   "Test.Headless.World.Identity"'s helper). Fails after ~30 s.
waitForFile ∷ FilePath → IO ()
waitForFile path = go (300 ∷ Int)
  where
    go 0 = expectationFailure $ "save file never appeared: " ⧺ path
    go n = do
        exists ← doesFileExist path
        if exists then pure () else threadDelay 100000 ≫ go (n - 1)

spec ∷ SpecWith EngineEnv
spec =
    describe "durable transfer orders survive the LIVE save/load path \
             \(#1246)" $
        it "a populated wsTransferOrdersRef is captured by the real \
           \WorldSave transaction and restored, verbatim and \
           \dangling-references-and-all, into the staged WorldState" $
            \env →
        let cleanup = removePathForcibly ("saves/" <> T.unpack slotName)
        in (`finally` cleanup) $ do
            cleanup

            sendWorldCommand env (WorldInit pageId 44 8 3 Nothing)
            ws ← waitForWorldInit env pageId 120

            -- A fresh page starts with an empty store and an allocator
            -- of 1: the same default a save predating this component
            -- decodes to, so the assertions below really do measure what
            -- was written rather than what was already there.
            readIORef (wsTransferOrdersRef ws)
                `shouldReturn` emptyTransferOrders
            writeIORef (wsTransferOrdersRef ws) liveOrders

            sendWorldCommand env
                (WorldSave pageId slotName "2026-08-13T00:00:00.000000Z"
                           [] [] Nothing)
            waitForFile ("saves/" <> T.unpack slotName <> "/world.synworld")

            logger ← readIORef (loggerRef env)
            (sd, _, _) ← loadWorld logger slotName HS.empty HS.empty ⌦ either
                (\(_, e) → expectationFailure (T.unpack e)
                        ≫ error "unreachable")
                pure

            -- Capture read the live ref: the decoded save carries the
            -- orders, not the empty default.
            case find ((≡ pageId) ∘ wpsPageId) (sdWorlds sd) of
                Nothing  → expectationFailure "saved page missing from SaveData"
                Just wps → do
                    wpsTransferOrders wps `shouldBe` liveOrders
                    -- The page really holds nothing for those references
                    -- to resolve against, so everything below is the
                    -- TOLERATED dangling path rather than an
                    -- accidentally-resolving one.
                    bsnInstances (wpsBuildings wps) `shouldBe` HM.empty
                    usnInstances (wpsUnits wps) `shouldBe` HM.empty

            matReg ← readIORef (materialRegistryRef env)
            staged ← stageSession env logger sd matReg ⌦ either
                (\e → expectationFailure (T.unpack (renderStageError e))
                        ≫ error "unreachable")
                pure

            case find ((≡ pageId) ∘ spPageId) (ssPages staged) of
                Nothing → expectationFailure
                    "staged session is missing the saved page"
                Just sp → do
                    restored ← readIORef
                        (wsTransferOrdersRef (spWorldState sp))
                    -- Staging wrote the ref: whole-store equality first
                    -- (ids, allocator, endpoints, states, instance ids),
                    -- then the pieces spelled out, so a break reads as
                    -- what it is rather than as one enormous diff.
                    restored `shouldBe` liveOrders
                    let orders = transferOrderList restored
                    map troId orders
                        `shouldBe` [TransferOrderId 1, TransferOrderId 2]
                    trosNextId restored `shouldBe` 3
                    map troUnit orders `shouldBe` [UnitId 11, UnitId 11]
                    map (\o → (tbSource (troBatch o)
                              , tbDestination (troBatch o))) orders
                        `shouldBe`
                        [ (EndpointUnit (UnitId 11)
                          , EndpointBuilding (BuildingId 777))
                        , (EndpointBuilding (BuildingId 777)
                          , EndpointBuilding (BuildingId 778)) ]
                    concatMap (map qtState ∘ tbEntries ∘ troBatch) orders
                        `shouldBe`
                        [ TransferQueued, TransferInTransit
                        , TransferReadyToCommit, TransferCompleted
                        , TransferCancelled
                        , TransferFailed (requestFailure ReasonReceiverFull)
                        , TransferFailed (staleFailure ReasonInstanceMissing) ]
                    concatMap (map (tirInstanceId ∘ qtItem) ∘ tbEntries
                                   ∘ troBatch) orders
                        `shouldBe` [5001, 5002, 5003, 5004, 5005, 5006, 5007]
