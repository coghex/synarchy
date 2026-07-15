{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Craft-bill backend tests (#329): the pure queue/claim/progress
--   transitions in Craft.Bills that the craft.* bill verbs wrap, plus
--   the save-format roundtrip (bills persist in WorldPageSave, v70;
--   'cbWorking' appended in v80 for #590's job-dependent power draw).
--   The engine-integrated path (Lua verbs → craft AI → executeAt) is
--   gated by tools/craft_bill_probe.py.
module Test.Headless.Craft.Bills (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import Craft.Bills
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))

station1, station2 ∷ BuildingId
station1 = BuildingId 7
station2 = BuildingId 8

worker1, worker2 ∷ UnitId
worker1 = UnitId 100
worker2 = UnitId 200

-- | A queue with one finite bill (count 3) at station1; returns the
--   bills and the new bill's id.
oneBill ∷ (CraftBills, BillId)
oneBill = addBill station1 "smelt_steel_anthracite" 3 emptyCraftBills

everyoneAlive ∷ UnitId → Bool
everyoneAlive = const True

spec ∷ Spec
spec = do
    describe "queue" $ do
        it "addBill starts pending with no progress" $ do
            let (bills, bid) = oneBill
            cbRecipe ⊚ lookupBill bid bills
                `shouldBe` Just "smelt_steel_anthracite"
            cbStation ⊚ lookupBill bid bills `shouldBe` Just station1
            cbRemaining ⊚ lookupBill bid bills `shouldBe` Just 3
            cbClaimant ⊚ lookupBill bid bills `shouldBe` Just Nothing
            cbProgress ⊚ lookupBill bid bills `shouldBe` Just 0

        it "count ≤ 0 normalises to repeat-forever (-1)" $ do
            let (bills, bid) = addBill station1 "r" 0 emptyCraftBills
            cbRemaining ⊚ lookupBill bid bills `shouldBe` Just (-1)

        it "ids are unique and the counter survives removal" $ do
            let (b1, i1) = addBill station1 "a" 1 emptyCraftBills
                (b2, _)  = removeBill i1 b1
                (b3, i2) = addBill station1 "b" 1 b2
            i2 `shouldNotBe` i1
            lookupBill i1 b3 `shouldBe` Nothing

        it "removeBill is False for unknown ids" $ do
            let (bills, _) = oneBill
            snd (removeBill (BillId 999) bills) `shouldBe` False

        it "billsForStation filters and sorts oldest-first" $ do
            let (b1, i1) = addBill station1 "a" 1 emptyCraftBills
                (b2, _)  = addBill station2 "b" 1 b1
                (b3, i3) = addBill station1 "c" 1 b2
            map cbId (billsForStation station1 b3) `shouldBe` [i1, i3]
            map cbRecipe (billsForStation station2 b3) `shouldBe` ["b"]

    describe "claims" $ do
        it "claim, then a rival's fresh claim is refused" $ do
            let (bills, bid) = oneBill
                (b1, ok1) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, ok2) = claimBill 11 30 everyoneAlive bid worker2 b1
            ok1 `shouldBe` True
            ok2 `shouldBe` False
            cbClaimant ⊚ lookupBill bid b2 `shouldBe` Just (Just worker1)

        it "the holder can refresh (claimedAt advances)" $ do
            let (bills, bid) = oneBill
                (b1, _)  = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, ok) = claimBill 25 30 everyoneAlive bid worker1 b1
            ok `shouldBe` True
            cbClaimedAt ⊚ lookupBill bid b2 `shouldBe` Just 25

        it "an expired claim can be taken over" $ do
            let (bills, bid) = oneBill
                (b1, _)  = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, ok) = claimBill 45 30 everyoneAlive bid worker2 b1
            ok `shouldBe` True
            cbClaimant ⊚ lookupBill bid b2 `shouldBe` Just (Just worker2)

        it "a dead claimant's bill is immediately takeable" $ do
            let (bills, bid) = oneBill
                (b1, _)  = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, ok) = claimBill 11 30 (≢ worker1) bid worker2 b1
            ok `shouldBe` True
            cbClaimant ⊚ lookupBill bid b2 `shouldBe` Just (Just worker2)

        it "release keeps progress for the next crafter" $ do
            let (bills, bid) = oneBill
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, p) = addBillProgress bid 0.4 b1
                (b3, ok) = releaseBill bid b2
            p `shouldBe` Just 0.4
            ok `shouldBe` True
            cbClaimant ⊚ lookupBill bid b3 `shouldBe` Just Nothing
            cbProgress ⊚ lookupBill bid b3 `shouldBe` Just 0.4

        it "claiming an unknown bill is False" $ do
            let (bills, _) = oneBill
            snd (claimBill 0 30 everyoneAlive (BillId 999) worker1 bills)
                `shouldBe` False

    describe "progress + cycles" $ do
        it "progress clamps to [0, 1]" $ do
            let (bills, bid) = oneBill
                (b1, p1) = addBillProgress bid 0.7 bills
                (b2, p2) = addBillProgress bid 0.7 b1
                (_,  p3) = addBillProgress bid (-5) b2
            p1 `shouldBe` Just 0.7
            p2 `shouldBe` Just 1.0
            p3 `shouldBe` Just 0.0

        it "completeBillCycle decrements and resets progress" $ do
            let (bills, bid) = oneBill
                (b1, _) = addBillProgress bid 1.5 bills
                (b2, r) = completeBillCycle bid b1
            r `shouldBe` Just 2
            cbRemaining ⊚ lookupBill bid b2 `shouldBe` Just 2
            cbProgress ⊚ lookupBill bid b2 `shouldBe` Just 0

        it "a finite bill is removed when the count runs out" $ do
            let (bills, bid) = addBill station1 "r" 1 emptyCraftBills
                (b1, r) = completeBillCycle bid bills
            r `shouldBe` Just 0
            lookupBill bid b1 `shouldBe` Nothing

        it "a repeat bill stays at -1 forever" $ do
            let (bills, bid) = addBill station1 "r" (-1) emptyCraftBills
                (b1, r1) = completeBillCycle bid bills
                (b2, r2) = completeBillCycle bid b1
            r1 `shouldBe` Just (-1)
            r2 `shouldBe` Just (-1)
            cbRemaining ⊚ lookupBill bid b2 `shouldBe` Just (-1)

        it "completing an unknown bill is Nothing" $ do
            snd (completeBillCycle (BillId 4) emptyCraftBills)
                `shouldBe` Nothing

    describe "pause (#330)" $ do
        it "setBillPaused sets and clears the flag" $ do
            let (bills, bid) = oneBill
                (b1, ok1) = setBillPaused bid True bills
                (b2, ok2) = setBillPaused bid False b1
            ok1 `shouldBe` True
            cbPaused ⊚ lookupBill bid b1 `shouldBe` Just True
            ok2 `shouldBe` True
            cbPaused ⊚ lookupBill bid b2 `shouldBe` Just False

        it "setBillPaused is False for an unknown bill" $ do
            snd (setBillPaused (BillId 999) True emptyCraftBills)
                `shouldBe` False

        it "a paused, unclaimed bill can't be freshly claimed" $ do
            let (bills, bid) = oneBill
                (b1, _)   = setBillPaused bid True bills
                (_, ok)   = claimBill 10 30 everyoneAlive bid worker1 b1
            ok `shouldBe` False

        it "pausing a claimed bill lets the holder keep refreshing" $ do
            let (bills, bid) = oneBill
                (b1, _)  = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _)  = setBillPaused bid True b1
                (_, ok)  = claimBill 20 30 everyoneAlive bid worker1 b2
            ok `shouldBe` True

        it "pausing blocks a dead-claimant takeover by someone else" $ do
            let (bills, bid) = oneBill
                (b1, _)  = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _)  = setBillPaused bid True b1
                (_, ok)  = claimBill 11 30 (≢ worker1) bid worker2 b2
            ok `shouldBe` False

        it "unpausing re-allows a fresh claim" $ do
            let (bills, bid) = oneBill
                (b1, _)  = setBillPaused bid True bills
                (b2, _)  = setBillPaused bid False b1
                (_, ok)  = claimBill 10 30 everyoneAlive bid worker1 b2
            ok `shouldBe` True

    describe "working (#590)" $ do
        it "a freshly added bill starts not working" $ do
            let (bills, bid) = oneBill
            cbWorking ⊚ lookupBill bid bills `shouldBe` Just False

        it "setBillWorking sets and clears the flag" $ do
            let (bills, bid) = oneBill
                (b1, _)  = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, ok1) = setBillWorking bid True b1
                (b3, ok2) = setBillWorking bid False b2
            ok1 `shouldBe` True
            cbWorking ⊚ lookupBill bid b2 `shouldBe` Just True
            ok2 `shouldBe` True
            cbWorking ⊚ lookupBill bid b3 `shouldBe` Just False

        it "setBillWorking is False for an unknown bill" $ do
            snd (setBillWorking (BillId 999) True emptyCraftBills)
                `shouldBe` False

        it "a same-holder refresh (claimBill) preserves cbWorking" $ do
            let (bills, bid) = oneBill
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _) = setBillWorking bid True b1
                (b3, _) = claimBill 20 30 everyoneAlive bid worker1 b2
            cbWorking ⊚ lookupBill bid b3 `shouldBe` Just True

        it "a takeover by a DIFFERENT claimant resets cbWorking" $ do
            let (bills, bid) = oneBill
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _) = setBillWorking bid True b1
                (b3, _) = claimBill 45 30 everyoneAlive bid worker2 b2
                    -- worker1's claim expired (timeout 30) — worker2
                    -- takes over and must start unmarked, not inherit
                    -- worker1's stale "working" state.
            cbWorking ⊚ lookupBill bid b3 `shouldBe` Just False

        it "releaseBill clears cbWorking" $ do
            let (bills, bid) = oneBill
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _) = setBillWorking bid True b1
                (b3, _) = releaseBill bid b2
            cbWorking ⊚ lookupBill bid b3 `shouldBe` Just False

        it "completeBillCycle clears cbWorking on a continuing bill" $ do
            let (bills, bid) = oneBill
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _) = setBillWorking bid True b1
                (b3, _) = completeBillCycle bid b2
            cbRemaining ⊚ lookupBill bid b3 `shouldBe` Just 2
            cbWorking ⊚ lookupBill bid b3 `shouldBe` Just False

        it "completeBillCycle clears cbWorking on a repeat bill" $ do
            let (bills, bid) = addBill station1 "r" (-1) emptyCraftBills
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _) = setBillWorking bid True b1
                (b3, _) = completeBillCycle bid b2
            cbRemaining ⊚ lookupBill bid b3 `shouldBe` Just (-1)
            cbWorking ⊚ lookupBill bid b3 `shouldBe` Just False

        it "pausing a working bill does NOT clear cbWorking (holder keeps drawing)" $ do
            let (bills, bid) = oneBill
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _) = setBillWorking bid True b1
                (b3, _) = setBillPaused bid True b2
            cbWorking ⊚ lookupBill bid b3 `shouldBe` Just True

    describe "pause boundary at cycle completion (#796)" $ do
        it "completeBillCycle keeps the claimant on an UNPAUSED continuing bill" $ do
            let (bills, bid) = oneBill
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _) = setBillWorking bid True b1
                (b3, _) = completeBillCycle bid b2
            cbRemaining ⊚ lookupBill bid b3 `shouldBe` Just 2
            cbClaimant  ⊚ lookupBill bid b3 `shouldBe` Just (Just worker1)

        it "completeBillCycle clears the claimant on a PAUSED finite continuing bill" $ do
            let (bills, bid) = oneBill
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _) = setBillWorking bid True b1
                (b3, _) = setBillPaused bid True b2
                (b4, r) = completeBillCycle bid b3
            r `shouldBe` Just 2
            cbRemaining ⊚ lookupBill bid b4 `shouldBe` Just 2
            cbClaimant  ⊚ lookupBill bid b4 `shouldBe` Just Nothing
            cbWorking   ⊚ lookupBill bid b4 `shouldBe` Just False
            cbPaused    ⊚ lookupBill bid b4 `shouldBe` Just True

        it "completeBillCycle clears the claimant on a PAUSED repeat-forever bill" $ do
            let (bills, bid) = addBill station1 "r" (-1) emptyCraftBills
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _) = setBillWorking bid True b1
                (b3, _) = setBillPaused bid True b2
                (b4, r) = completeBillCycle bid b3
            r `shouldBe` Just (-1)
            cbRemaining ⊚ lookupBill bid b4 `shouldBe` Just (-1)
            cbClaimant  ⊚ lookupBill bid b4 `shouldBe` Just Nothing

        it "the retained, now-unclaimed bill still refuses a fresh claim while paused" $ do
            let (bills, bid) = oneBill
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _) = setBillWorking bid True b1
                (b3, _) = setBillPaused bid True b2
                (b4, _) = completeBillCycle bid b3
                (_, ok) = claimBill 20 30 everyoneAlive bid worker2 b4
            ok `shouldBe` False

        it "unpausing the retained bill lets a fresh claimant resume it" $ do
            let (bills, bid) = oneBill
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _) = setBillWorking bid True b1
                (b3, _) = setBillPaused bid True b2
                (b4, _) = completeBillCycle bid b3
                (b5, _) = setBillPaused bid False b4
                (b6, ok) = claimBill 20 30 everyoneAlive bid worker2 b5
            ok `shouldBe` True
            cbClaimant ⊚ lookupBill bid b6 `shouldBe` Just (Just worker2)
            cbRemaining ⊚ lookupBill bid b6 `shouldBe` Just 2

        it "a finite bill reaching zero is removed regardless of pause" $ do
            let (bills, bid) = addBill station1 "r" 1 emptyCraftBills
                (b1, _) = claimBill 10 30 everyoneAlive bid worker1 bills
                (b2, _) = setBillWorking bid True b1
                (b3, _) = setBillPaused bid True b2
                (b4, r) = completeBillCycle bid b3
            r `shouldBe` Just 0
            lookupBill bid b4 `shouldBe` Nothing

    describe "reorder (#330)" $ do
        it "moving a bill up swaps cbSeq with its predecessor" $ do
            let (b1, i1) = addBill station1 "a" 1 emptyCraftBills
                (b2, i2) = addBill station1 "b" 1 b1
                (b3, ok) = reorderBill MoveUp i2 b2
            ok `shouldBe` True
            map cbId (billsForStation station1 b3) `shouldBe` [i2, i1]

        it "moving a bill down swaps cbSeq with its successor" $ do
            let (b1, i1) = addBill station1 "a" 1 emptyCraftBills
                (b2, i2) = addBill station1 "b" 1 b1
                (b3, ok) = reorderBill MoveDown i1 b2
            ok `shouldBe` True
            map cbId (billsForStation station1 b3) `shouldBe` [i2, i1]

        it "moving the first bill up fails and leaves order untouched" $ do
            let (b1, i1) = addBill station1 "a" 1 emptyCraftBills
                (b2, i2) = addBill station1 "b" 1 b1
                (b3, ok) = reorderBill MoveUp i1 b2
            ok `shouldBe` False
            map cbId (billsForStation station1 b3) `shouldBe` [i1, i2]

        it "moving the last bill down fails" $ do
            let (b1, _)  = addBill station1 "a" 1 emptyCraftBills
                (b2, i2) = addBill station1 "b" 1 b1
                (_, ok)  = reorderBill MoveDown i2 b2
            ok `shouldBe` False

        it "reordering never crosses stations" $ do
            let (b1, i1) = addBill station1 "a" 1 emptyCraftBills
                (b2, _)  = addBill station2 "x" 1 b1
                (_, ok)  = reorderBill MoveDown i1 b2
            ok `shouldBe` False

        it "reordering an unknown bill fails" $ do
            snd (reorderBill MoveUp (BillId 999) emptyCraftBills)
                `shouldBe` False

    describe "persistence" $ do
        it "roundtrips through the save encoding (claims + working included)" $ do
            let (b0, bid) = oneBill
                (b1, _) = claimBill 12.5 30 everyoneAlive bid worker1 b0
                (b2, _) = addBillProgress bid 0.25 b1
                (b3, _) = setBillWorking bid True b2
            S.decode (S.encode b3) `shouldBe` Right b3

        it "pruneToStations drops bills whose station is gone" $ do
            let (b1, i1) = addBill station1 "a" 1 emptyCraftBills
                (b2, i2) = addBill station2 "b" 1 b1
                pruned = pruneToStations (HS.singleton station2) b2
            lookupBill i1 pruned `shouldBe` Nothing
            cbRecipe ⊚ lookupBill i2 pruned `shouldBe` Just "b"
