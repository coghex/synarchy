{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Key-name vocabulary tests. The contract under test: 'keyToText'
--   is the canonical name set (what Lua's onKeyDown/onKeyUp report),
--   and every name it can produce must round-trip through 'textToKey'
--   and resolve to real GLFW keys via 'parseKeyName' — so anything an
--   event hands to Lua is pollable with isKeyDown and bindable in
--   keybinds.local.yaml. The reachability test is the one that would have
--   caught the dead-Home/End bug (a Key constructor with no
--   fromGLFWKey case).
module Test.Headless.Input.KeyNames (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map as Map
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Types
import Engine.Input.Bindings (parseKeyName, checkKeyDown)

allKeys ∷ [Key]
allKeys = [k | k ← [minBound .. maxBound], k ≢ KeyUnknown]

-- | Input state with exactly the given GLFW keys held down.
heldState ∷ [GLFW.Key] → InputState
heldState ks = defaultInputState
    { inpKeyStates = Map.fromList
        [(k, defaultKeyState { keyPressed = True }) | k ← ks] }

spec ∷ Spec
spec = do
    describe "name round-trip" $ do
        it "textToKey inverts keyToText for every key" $
            [k | k ← allKeys, textToKey (keyToText k) ≢ Just k]
                `shouldBe` []

        it "every canonical name resolves to at least one GLFW key" $
            [k | k ← allKeys, null (parseKeyName (keyToText k))]
                `shouldBe` []

    describe "GLFW reachability" $ do
        it "every Key constructor is reachable from some GLFW key" $
            [k | k ← allKeys, null (keyToGLFW k)] `shouldBe` []

        it "keyToGLFW agrees with fromGLFWKey" $
            [k | k ← allKeys, any (\g → fromGLFWKey g ≢ k) (keyToGLFW k)]
                `shouldBe` []

    describe "merged modifiers" $ do
        it "Shift fans out to both shift keys" $
            parseKeyName "Shift" `shouldMatchList`
                [GLFW.Key'LeftShift, GLFW.Key'RightShift]

        it "side-specific aliases stay single-key" $ do
            parseKeyName "LeftShift"  `shouldBe` [GLFW.Key'LeftShift]
            parseKeyName "RightShift" `shouldBe` [GLFW.Key'RightShift]

        it "isKeyDown(\"Shift\") sees a held right shift" $
            checkKeyDown "Shift" (heldState [GLFW.Key'RightShift])
                `shouldBe` True

        it "isKeyDown(\"Shift\") is false with no shift held" $
            checkKeyDown "Shift" (heldState [GLFW.Key'W])
                `shouldBe` False

    describe "previously dead keys" $ do
        it "Home and End parse and poll" $ do
            checkKeyDown "Home" (heldState [GLFW.Key'Home]) `shouldBe` True
            checkKeyDown "End"  (heldState [GLFW.Key'End])  `shouldBe` True

        it "F-keys parse and poll" $
            checkKeyDown "F5" (heldState [GLFW.Key'F5]) `shouldBe` True

        it "unknown names match nothing" $ do
            parseKeyName "NotAKey" `shouldBe` []
            checkKeyDown "NotAKey" (heldState [GLFW.Key'W]) `shouldBe` False
