{-# LANGUAGE ExplicitForAll #-}
module Test.UPrelude (spec) where

-- Import the testing framework
import Test.Hspec

-- Import the module under test
import UPrelude

-- We import Data.Bits for comparing bitwise results
import qualified Data.Bits as B

spec :: Spec
spec = do
  describe "UPrelude.flatten" $ do
    it "flattens a list of lists into a single list" $ do
      let nestedList = [[1, 2], [3, 4], [5]]
          expected   = [1, 2, 3, 4, 5]
      flatten nestedList `shouldBe` expected
  
  describe "Bitwise Operators" $ do
    it "(⌃) performs bitwise AND correctly" $ do
      ((6 ∷ Int) ⌃ (3 ∷ Int)) `shouldBe` ((6 ∷ Int) B..&. (3 ∷ Int))
    
    it "(⌄) performs bitwise OR correctly" $ do
      ((6 ∷ Int) ⌄ (3 ∷ Int)) `shouldBe` ((6 ∷ Int) B..|. (3 ∷ Int))

  describe "Functor Operators" $ do
    it "(⚟) replaces functor result with supplied value" $ do
      42 ⚟ Just 999 `shouldBe` Just 42
      42 ⚟ Nothing  `shouldBe` Nothing
    
    it "(⚞) replaces functor result with a new value" $ do
      Just 999 ⚞ 42 `shouldBe` Just 42
      Nothing  ⚞ 42 `shouldBe` Nothing

  describe "Monadic Operators" $ do
    it "(⌦) is an alias for monadic bind (>>=)" $ do
      let exampleM = Just 10
      (exampleM ⌦ (\x -> Just (x + 5))) `shouldBe` Just 15

    it "(⌫) is an alias for (=<<)" $ do
      let f x = Just (x * 2)
      (f ⌫ Just 10) `shouldBe` Just 20
