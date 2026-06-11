{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Input.Bindings where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:?), (.!=), FromJSON(..), Value(..))
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
    KeyBindingConfig ⊚ v .:? "keybinds" .!= defaultKeyBindings
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

-- | GLFW keys a key name matches. The canonical vocabulary is
--   'keyToText' (what Lua's onKeyDown/onKeyUp report), inverted via
--   'textToKey' — so any name an event hands to Lua is pollable, and
--   merged modifier names ("Shift", "Ctrl", "Alt", "Super") match
--   either side. The side-specific names below are compatibility
--   aliases for existing scripts and keybinds files.
parseKeyName ∷ Text → [GLFW.Key]
parseKeyName "LeftShift"  = [GLFW.Key'LeftShift]
parseKeyName "RightShift" = [GLFW.Key'RightShift]
parseKeyName "LeftCtrl"   = [GLFW.Key'LeftControl]
parseKeyName "RightCtrl"  = [GLFW.Key'RightControl]
parseKeyName "LeftAlt"    = [GLFW.Key'LeftAlt]
parseKeyName "RightAlt"   = [GLFW.Key'RightAlt]
parseKeyName "LeftSuper"  = [GLFW.Key'LeftSuper]
parseKeyName "RightSuper" = [GLFW.Key'RightSuper]
parseKeyName name         = maybe [] keyToGLFW (textToKey name)

checkKeyDown ∷ Text → InputState → Bool
checkKeyDown keyName state = any down (parseKeyName keyName)
  where down k = maybe False keyPressed (Map.lookup k (inpKeyStates state))
