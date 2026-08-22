{-# LANGUAGE ExplicitForAll #-}
module Test.Headless.UPrelude (spec) where

-- Import the testing framework
import Test.Hspec

-- Import the module under test
import UPrelude

-- We import Data.Bits for comparing bitwise results
import qualified Data.Bits as B

-- The pre-#1099 spelling, kept here as the oracle 'tshow' must match.
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "UPrelude.flatten" $ do
    it "flattens a list of lists into a single list" $ do
      let nestedList = [[1, 2], [3, 4], [5]]
          expected   = [1, 2, 3, 4, 5]
      flatten nestedList `shouldBe` expected

  describe "UPrelude.tshow" $ do
    -- #1099 replaced 709 hand-written `T.pack (show x)` wrappers with this
    -- one helper, so what it must guarantee is that no rendered byte moved.
    it "renders exactly what the hand-written wrapper rendered" $ do
      tshow (42 ∷ Int)            `shouldBe` T.pack (show (42 ∷ Int))
      tshow (-7 ∷ Int)            `shouldBe` T.pack (show (-7 ∷ Int))
      tshow (1.5 ∷ Double)        `shouldBe` T.pack (show (1.5 ∷ Double))
      tshow (Just (3 ∷ Int))      `shouldBe` T.pack (show (Just (3 ∷ Int)))
      tshow [1, 2, 3 ∷ Int]       `shouldBe` T.pack (show [1, 2, 3 ∷ Int])
      tshow True                  `shouldBe` T.pack (show True)
      tshow ('x' ∷ Char)          `shouldBe` T.pack (show ('x' ∷ Char))

    it "shows a String the way 'show' does, quotes and escapes included" $ do
      -- The wrapper is NOT T.pack: several call sites render paths and
      -- other Strings, and those keep their surrounding quotes.
      tshow ("a\"b" ∷ String) `shouldBe` T.pack (show ("a\"b" ∷ String))
      tshow ("héllo" ∷ String) `shouldBe` T.pack (show ("héllo" ∷ String))

    it "round-trips through unpack back to 'show'" $ do
      T.unpack (tshow (1234567890 ∷ Integer))
        `shouldBe` show (1234567890 ∷ Integer)
  
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
