{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Craft-bill backend tests (#329): the pure queue/claim/progress
--   transitions in Craft.Bills that the craft.* bill verbs wrap, plus
--   the save-format roundtrip (bills persist in WorldPageSave, v70).
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

    describe "persistence" $ do
        it "roundtrips through the save encoding (claims included)" $ do
            let (b0, bid) = oneBill
                (b1, _) = claimBill 12.5 30 everyoneAlive bid worker1 b0
                (b2, _) = addBillProgress bid 0.25 b1
            S.decode (S.encode b2) `shouldBe` Right b2

        it "pruneToStations drops bills whose station is gone" $ do
            let (b1, i1) = addBill station1 "a" 1 emptyCraftBills
                (b2, i2) = addBill station2 "b" 1 b1
                pruned = pruneToStations (HS.singleton station2) b2
            lookupBill i1 pruned `shouldBe` Nothing
            cbRecipe ⊚ lookupBill i2 pruned `shouldBe` Just "b"
