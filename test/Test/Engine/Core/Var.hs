{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Core.Var (spec) where

import UPrelude
import Test.Hspec
import Engine.Core.Var
import Control.Concurrent.Async (concurrently)
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T

spec ∷ Spec
spec = do
  describe "Engine.Core.Var" $ do
    describe "Basic Variable Operations" $ do
      it "can create and read a variable" $ do
        var ← atomically $ newVar "test"
        result ← atomically $ readVar var
        result `shouldBe` "test"

      it "can write to a variable" $ do
        var ← atomically $ newVar "initial"
        atomically $ writeVar var "updated"
        result ← atomically $ readVar var
        result `shouldBe` "updated"

      it "can modify a variable with a function" $ do
        var ← atomically $ newVar 5
        atomically $ modifyVar' var (+1)
        result ← atomically $ readVar var
        result `shouldBe` 6

      it "can modify a variable and return a value" $ do
        var ← atomically $ newVar 5
        result ← atomically $ modifyVar var (\x → (x + 1, x * 2))
        final ← atomically $ readVar var
        final `shouldBe` 6    -- Check the modification
        result `shouldBe` 10  -- Check the returned value

    describe "Concurrent Variable Operations" $ do
      it "handles multiple concurrent modifications safely" $ do
        var ← atomically $ newVar (0 ∷ Int)
        let iterations = 1000
            increment = replicateM_ iterations $
              atomically $ modifyVar' var (+1)
        (_, _) ← concurrently increment increment
        result ← atomically $ readVar var
        result `shouldBe` (iterations * 2)

      it "maintains consistency under concurrent reads and writes" $ do
        var ← atomically $ newVar (0 ∷ Int)
        results ← atomically $ newVar []
        let writer = replicateM_ 100 $ atomically $
              modifyVar' var (+1)
            reader = replicateM_ 100 $ atomically $ do
              val ← readVar var
              modifyVar' results (val:)
        (_, _) ← concurrently writer reader
        finalResults ← atomically $ readVar results
        all (≥ 0) finalResults `shouldBe` True
        all (≤ 100) finalResults `shouldBe` True

    describe "Variable Duplication" $ do
      it "creates independent copies of variables" $ do
        var1 ← atomically $ newVar "original"
        var2 ← dupVar var1
        atomically $ writeVar var1 "changed"
        result1 ← atomically $ readVar var1
        result2 ← atomically $ readVar var2
        result1 `shouldBe` "changed"
        result2 `shouldBe` "original"
