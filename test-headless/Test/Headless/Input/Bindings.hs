{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Multi-key binding data-model tests (issue #274). The contract under
--   test: 'KeyBindings' is action → list-of-keys; the YAML parser accepts
--   both the legacy scalar form (@moveUp: W@) and the new array form
--   (@moveUp: [Up, W]@); 'isActionDown' fires when *any* bound key is
--   held; and the writer round-trips through the reader.
module Test.Headless.Input.Bindings (spec) where

import UPrelude
import Test.Hspec
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Types
import Engine.Input.Bindings

-- | Input state with exactly the given GLFW keys held down.
heldState ∷ [GLFW.Key] → InputState
heldState ks = defaultInputState
    { inpKeyStates = Map.fromList
        [(k, defaultKeyState { keyPressed = True }) | k ← ks] }

-- | Parse a keybinds YAML document or fail the test loudly.
parseConfig ∷ ByteString → KeyBindings
parseConfig bs = case Yaml.decodeEither' bs of
    Left err  → error ("decode failed: " ⧺ show err)
    Right cfg → kbcBindings cfg

spec ∷ Spec
spec = do
    describe "config parsing" $ do
        it "accepts the legacy scalar form (one key per action)" $ do
            let b = parseConfig "keybinds:\n  moveUp: W\n  toggleEventLog: L\n"
            Map.lookup "moveUp" b         `shouldBe` Just ["W"]
            Map.lookup "toggleEventLog" b `shouldBe` Just ["L"]

        it "accepts the new list form (multiple keys per action)" $ do
            let b = parseConfig "keybinds:\n  moveUp: [Up, W]\n  moveLeft: [Left, A]\n"
            Map.lookup "moveUp" b   `shouldBe` Just ["Up", "W"]
            Map.lookup "moveLeft" b `shouldBe` Just ["Left", "A"]

        it "accepts a single-element list" $ do
            let b = parseConfig "keybinds:\n  rotateCW: [E]\n"
            Map.lookup "rotateCW" b `shouldBe` Just ["E"]

        it "accepts scalar and list entries mixed in one file" $ do
            let b = parseConfig "keybinds:\n  moveUp: [Up, W]\n  toggleEventLog: L\n"
            Map.lookup "moveUp" b         `shouldBe` Just ["Up", "W"]
            Map.lookup "toggleEventLog" b `shouldBe` Just ["L"]

        it "falls back to defaults when keybinds key is absent" $
            parseConfig "{}" `shouldBe` defaultKeyBindings

        it "merges over defaults so a pre-existing config inherits new actions" $ do
            -- A config written before rotateCW/resetZTracking existed.
            let b = parseConfig "keybinds:\n  moveUp: W\n  toggleEventLog: L\n"
            -- File entries win...
            Map.lookup "moveUp" b         `shouldBe` Just ["W"]
            Map.lookup "toggleEventLog" b `shouldBe` Just ["L"]
            -- ...and absent actions fall back to their defaults rather
            -- than being left unbound.
            Map.lookup "rotateCW" b       `shouldBe` Just ["E"]
            Map.lookup "resetZTracking" b `shouldBe` Just ["Home"]
            Map.lookup "moveLeft" b       `shouldBe` Just ["Left", "A"]

    describe "defaults" $ do
        it "binds movement to arrows and WASD" $ do
            Map.lookup "moveUp" defaultKeyBindings    `shouldBe` Just ["Up", "W"]
            Map.lookup "moveDown" defaultKeyBindings  `shouldBe` Just ["Down", "S"]
            Map.lookup "moveLeft" defaultKeyBindings  `shouldBe` Just ["Left", "A"]
            Map.lookup "moveRight" defaultKeyBindings `shouldBe` Just ["Right", "D"]

        -- world_view.onKeyDown resolves these action names against the live
        -- bindings (issue #275); a rename or default-key change here would
        -- silently break Q/E rotate + Home z-reset.
        it "binds camera rotate + z-reset to Q/E/Home" $ do
            Map.lookup "rotateCCW" defaultKeyBindings      `shouldBe` Just ["Q"]
            Map.lookup "rotateCW" defaultKeyBindings       `shouldBe` Just ["E"]
            Map.lookup "resetZTracking" defaultKeyBindings `shouldBe` Just ["Home"]

    describe "isActionDown (any bound key)" $ do
        let bindings = Map.fromList [("moveUp", ["Up", "W"])]
        it "fires when the first bound key is held" $
            isActionDown "moveUp" bindings (heldState [GLFW.Key'Up]) `shouldBe` True

        it "fires when the second bound key is held" $
            isActionDown "moveUp" bindings (heldState [GLFW.Key'W]) `shouldBe` True

        it "is false when no bound key is held" $
            isActionDown "moveUp" bindings (heldState [GLFW.Key'S]) `shouldBe` False

        it "is false for an unbound action" $
            isActionDown "noSuchAction" bindings (heldState [GLFW.Key'W]) `shouldBe` False

    describe "reserved keys (#275)" $ do
        it "strips Escape/Grave when a hand-edited file binds them to actions" $ do
            let b = parseConfig "keybinds:\n  rotateCW: [Escape]\n  moveUp: [Grave, W]\n"
            -- Reserved keys removed; surviving real keys kept.
            Map.lookup "rotateCW" b `shouldBe` Just []
            Map.lookup "moveUp" b   `shouldBe` Just ["W"]

        it "keeps the reserved-action reference entries intact" $ do
            let b = parseConfig "keybinds:\n  moveUp: [W]\n"
            Map.lookup "escape" b    `shouldBe` Just ["Escape"]
            Map.lookup "openShell" b `shouldBe` Just ["Grave"]

        it "a reserved key bound to an action never drives it" $ do
            let b = parseConfig "keybinds:\n  rotateCW: [Escape]\n"
            isActionDown "rotateCW" b (heldState [GLFW.Key'Escape]) `shouldBe` False

    -- keyMatchesAction takes the *exact* GLFW key the engine recorded for
    -- the press (carried with the key-down event), so it is side-exact and
    -- needs no input-state lookup — a fast tap can't be dropped or
    -- mis-attributed by a race (#275).
    describe "keyMatchesAction (exact-key edge dispatch)" $ do
        let b = Map.fromList [ ("rotateCCW", ["Q"])
                             , ("sideAction", ["LeftShift"])
                             , ("modAction",  ["Shift"]) ]
        it "matches the bound key" $
            keyMatchesAction GLFW.Key'Q "rotateCCW" b `shouldBe` True

        it "does not match a different key" $
            keyMatchesAction GLFW.Key'E "rotateCCW" b `shouldBe` False

        it "is false for an unbound action" $
            keyMatchesAction GLFW.Key'Q "noSuchAction" b `shouldBe` False

        it "side-specific binding matches only that side" $ do
            keyMatchesAction GLFW.Key'LeftShift  "sideAction" b `shouldBe` True
            keyMatchesAction GLFW.Key'RightShift "sideAction" b `shouldBe` False

        it "merged binding matches either side" $ do
            keyMatchesAction GLFW.Key'LeftShift  "modAction" b `shouldBe` True
            keyMatchesAction GLFW.Key'RightShift "modAction" b `shouldBe` True

    describe "save round-trip" $
        it "encodes then decodes back to the same bindings" $ do
            let bs = Yaml.encode (KeyBindingConfig defaultKeyBindings)
            parseConfig bs `shouldBe` defaultKeyBindings
