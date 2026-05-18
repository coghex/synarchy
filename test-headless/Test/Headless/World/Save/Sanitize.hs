{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Tests for World.Save.Serialize.sanitizeSaveName — the validator
--   that gates user-supplied save names before they reach the
--   filesystem. The function is pure, so these tests are independent
--   of disk state.
module Test.Headless.World.Save.Sanitize (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import Data.Either (isLeft, isRight)
import World.Save.Serialize (sanitizeSaveName)

spec ∷ Spec
spec = describe "sanitizeSaveName" $ do
    describe "accepts" $ do
        it "simple ascii names"   $ sanitizeSaveName "test"      `shouldBe` Right "test"
        it "underscores"          $ sanitizeSaveName "my_save"   `shouldBe` Right "my_save"
        it "hyphens"              $ sanitizeSaveName "save-1"    `shouldBe` Right "save-1"
        it "spaces"               $ sanitizeSaveName "my save"   `shouldBe` Right "my save"
        it "digits"               $ sanitizeSaveName "save42"    `shouldBe` Right "save42"
        it "mixed case"           $ sanitizeSaveName "MySave"    `shouldBe` Right "MySave"
        it "64 chars exactly"     $
            sanitizeSaveName (T.replicate 64 "a") `shouldSatisfy` isRight
        it "a single dot in mid"  $ sanitizeSaveName "v1.5"      `shouldBe` Right "v1.5"

    describe "rejects" $ do
        it "empty string"         $ sanitizeSaveName ""          `shouldSatisfy` isLeft
        it "traversal"            $ sanitizeSaveName "../foo"    `shouldSatisfy` isLeft
        it "traversal in middle"  $ sanitizeSaveName "foo..bar"  `shouldSatisfy` isLeft
        it "trailing traversal"   $ sanitizeSaveName "foo/.."    `shouldSatisfy` isLeft
        it "absolute unix path"   $ sanitizeSaveName "/etc/passwd" `shouldSatisfy` isLeft
        it "forward slash"        $ sanitizeSaveName "foo/bar"   `shouldSatisfy` isLeft
        it "backslash"            $ sanitizeSaveName "foo\\bar"  `shouldSatisfy` isLeft
        it "leading dot"          $ sanitizeSaveName ".hidden"   `shouldSatisfy` isLeft
        it "65 chars"             $
            sanitizeSaveName (T.replicate 65 "a") `shouldSatisfy` isLeft
        it "newline"              $ sanitizeSaveName "foo\nbar"  `shouldSatisfy` isLeft
        it "tab"                  $ sanitizeSaveName "foo\tbar"  `shouldSatisfy` isLeft
        it "null byte"            $ sanitizeSaveName "foo\0bar"  `shouldSatisfy` isLeft
