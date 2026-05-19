{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Input.Bindings where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:), (.!=), FromJSON(..), Value(..))
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Types
import Engine.Core.Log (LoggerState, logWarn, logInfo, LogCategory(..), logDebug)

type KeyBindings = Map.Map T.Text T.Text

defaultKeyBindings ∷ KeyBindings
defaultKeyBindings = Map.fromList
    [ ("moveUp", "W")
    , ("moveDown", "S")
    , ("moveLeft", "A")
    , ("moveRight", "D")
    , ("escape", "Escape")
    , ("shell", "Grave")
    , ("toggleEventLog", "L")
    ]

data KeyBindingConfig = KeyBindingConfig
    { kbcBindings ∷ KeyBindings
    } deriving (Show, Eq)

instance FromJSON KeyBindingConfig where
  parseJSON (Object v) =
    KeyBindingConfig ⊚ v .: "keybinds" .!= defaultKeyBindings
  parseJSON _ = fail "Expected Object for KeyBindingConfig value"

-- | Load keybindings from a YAML file
loadKeyBindings ∷ LoggerState → FilePath → IO KeyBindings
loadKeyBindings logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatInput $ "Failed to load keybindings from "
                                    <> T.pack path <> ": " <> T.pack (show err)
                                    <> ". Using default keybindings."
            return defaultKeyBindings
        Right config → do
            let bindings = kbcBindings config
            logDebug logger CatInput $ "Key bindings loaded: " <> T.pack (show (Map.size bindings)) <> " actions"
            return bindings

isActionDown ∷ Text → KeyBindings → InputState → Bool
isActionDown action bindings state =
    case Map.lookup action bindings of
        Just keyName → checkKeyDown keyName state
        Nothing → False

getKeyForAction ∷ Text → KeyBindings → Maybe Text
getKeyForAction = Map.lookup

parseKeyName ∷ Text → Maybe GLFW.Key
-- Letters
parseKeyName "A" = Just GLFW.Key'A
parseKeyName "B" = Just GLFW.Key'B
parseKeyName "C" = Just GLFW.Key'C
parseKeyName "D" = Just GLFW.Key'D
parseKeyName "E" = Just GLFW.Key'E
parseKeyName "F" = Just GLFW.Key'F
parseKeyName "G" = Just GLFW.Key'G
parseKeyName "H" = Just GLFW.Key'H
parseKeyName "I" = Just GLFW.Key'I
parseKeyName "J" = Just GLFW.Key'J
parseKeyName "K" = Just GLFW.Key'K
parseKeyName "L" = Just GLFW.Key'L
parseKeyName "M" = Just GLFW.Key'M
parseKeyName "N" = Just GLFW.Key'N
parseKeyName "O" = Just GLFW.Key'O
parseKeyName "P" = Just GLFW.Key'P
parseKeyName "Q" = Just GLFW.Key'Q
parseKeyName "R" = Just GLFW.Key'R
parseKeyName "S" = Just GLFW.Key'S
parseKeyName "T" = Just GLFW.Key'T
parseKeyName "U" = Just GLFW.Key'U
parseKeyName "V" = Just GLFW.Key'V
parseKeyName "W" = Just GLFW.Key'W
parseKeyName "X" = Just GLFW.Key'X
parseKeyName "Y" = Just GLFW.Key'Y
parseKeyName "Z" = Just GLFW.Key'Z
-- Digits (top row)
parseKeyName "0" = Just GLFW.Key'0
parseKeyName "1" = Just GLFW.Key'1
parseKeyName "2" = Just GLFW.Key'2
parseKeyName "3" = Just GLFW.Key'3
parseKeyName "4" = Just GLFW.Key'4
parseKeyName "5" = Just GLFW.Key'5
parseKeyName "6" = Just GLFW.Key'6
parseKeyName "7" = Just GLFW.Key'7
parseKeyName "8" = Just GLFW.Key'8
parseKeyName "9" = Just GLFW.Key'9
-- Arrows + named system keys
parseKeyName "Up"        = Just GLFW.Key'Up
parseKeyName "Down"      = Just GLFW.Key'Down
parseKeyName "Left"      = Just GLFW.Key'Left
parseKeyName "Right"     = Just GLFW.Key'Right
parseKeyName "Space"     = Just GLFW.Key'Space
parseKeyName "Enter"     = Just GLFW.Key'Enter
parseKeyName "Escape"    = Just GLFW.Key'Escape
parseKeyName "Tab"       = Just GLFW.Key'Tab
parseKeyName "Backspace" = Just GLFW.Key'Backspace
parseKeyName "Delete"    = Just GLFW.Key'Delete
parseKeyName "Grave"     = Just GLFW.Key'GraveAccent
-- Modifiers (note: KeyShift maps to both Left/Right Shift at the
-- input-state level, so isActionDown reports true for either).
parseKeyName "LeftShift"   = Just GLFW.Key'LeftShift
parseKeyName "RightShift"  = Just GLFW.Key'RightShift
parseKeyName "LeftCtrl"    = Just GLFW.Key'LeftControl
parseKeyName "RightCtrl"   = Just GLFW.Key'RightControl
parseKeyName "LeftAlt"     = Just GLFW.Key'LeftAlt
parseKeyName "RightAlt"    = Just GLFW.Key'RightAlt
parseKeyName _             = Nothing

checkKeyDown ∷ Text → InputState → Bool
checkKeyDown keyName state =
    case parseKeyName keyName of
        Just key →
            case Map.lookup key (inpKeyStates state) of
                Just keyState → keyPressed keyState
                Nothing       → False
        Nothing → False
