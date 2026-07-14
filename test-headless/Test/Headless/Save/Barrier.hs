{-# LANGUAGE UnicodeSyntax #-}
module Test.Headless.Save.Barrier (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Set as Set
import Engine.Save.Barrier

spec ∷ Spec
spec = describe "save snapshot barrier" $ do
    it "reaches the boundary only after every registered owner acknowledges" $ do
        b ← newSaveBarrier
        Right n ← beginSave b (Set.fromList [SaveLua, SaveWorld, SaveUnit])
        acknowledgeSave b n SaveLua
        acknowledgeSave b n SaveWorld
        early ← waitForOwners 1000 b n
        early `shouldBe` Left "timed out waiting for save state owners"
        acknowledgeSave b n SaveUnit
        waitForOwners 1000 b n `shouldReturn` Right ()
        reachSnapshot b n
        status ← readSaveStatus b
        ssPhase <$> status `shouldBe` Just SaveSnapshotBoundary

    it "fails a non-responsive owner without completing capture" $ do
        b ← newSaveBarrier
        Right n ← beginSave b (Set.singleton SaveWorld)
        failSave b n "world owner did not respond"
        waitForOwners 1000 b n `shouldReturn` Left "world owner did not respond"
        status ← readSaveStatus b
        ssPhase <$> status `shouldBe` Just SaveFailed

    it "serializes back-to-back transactions with distinct ids" $ do
        b ← newSaveBarrier
        Right first ← beginSave b Set.empty
        beginSave b Set.empty `shouldReturn` Left "a save transaction is already active"
        finishSave b first
        Right second ← beginSave b Set.empty
        second `shouldBe` first + 1
