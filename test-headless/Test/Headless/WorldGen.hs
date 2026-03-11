module Test.Headless.WorldGen (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Control.Concurrent (threadDelay)
import Test.Headless.Harness
import World.Types

spec ∷ Spec
spec = around withHeadlessEngine $ do

    describe "Basic terrain generation" $ do

        it "generates a small world with chunks" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "basic") 42 64 3)
            ws ← waitForWorldInit env (WorldPageId "basic") 120
            tiles ← getWorldTileData ws
            let numChunks = HM.size (wtdChunks tiles)
            numChunks `shouldSatisfy` (> 0)

        it "stores generation params after init" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "params") 42 64 3)
            ws ← waitForWorldInit env (WorldPageId "params") 120
            mParams ← getWorldGenParams ws
            mParams `shouldSatisfy` isJust
            case mParams of
                Just params → do
                    wgpSeed params `shouldBe` 42
                    wgpWorldSize params `shouldBe` 64
                Nothing → expectationFailure "params should exist"

        it "generates tectonic plates" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "plates") 99 64 3)
            ws ← waitForWorldInit env (WorldPageId "plates") 120
            mParams ← getWorldGenParams ws
            case mParams of
                Just params →
                    length (wgpPlates params) `shouldSatisfy` (> 0)
                Nothing → expectationFailure "params should exist"

    describe "Determinism" $ do

        it "same seed produces same plate data" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "det1") 123 64 3)
            ws1 ← waitForWorldInit env (WorldPageId "det1") 120

            sendWorldCommand env (WorldInit (WorldPageId "det2") 123 64 3)
            ws2 ← waitForWorldInit env (WorldPageId "det2") 120

            mParams1 ← getWorldGenParams ws1
            mParams2 ← getWorldGenParams ws2
            case (mParams1, mParams2) of
                (Just p1, Just p2) →
                    wgpPlates p1 `shouldBe` wgpPlates p2
                _ → expectationFailure "both worlds should have params"

    describe "World lifecycle" $ do

        it "can destroy a world" $ \env → do
            sendWorldCommand env (WorldInit (WorldPageId "destroy") 42 64 3)
            _ ← waitForWorldInit env (WorldPageId "destroy") 120
            sendWorldCommand env (WorldDestroy (WorldPageId "destroy"))
            threadDelay 500000
            mWs ← getWorldState env (WorldPageId "destroy")
            isNothing mWs `shouldBe` True
