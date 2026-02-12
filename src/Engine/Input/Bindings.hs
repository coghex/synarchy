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

----------- Types -----------------------------------------------------------

type KeyBindings = Map.Map T.Text T.Text

----------- Default Bindings -----------------------------------------------

defaultKeyBindings ∷ KeyBindings
defaultKeyBindings = Map.fromList
    [ ("moveUp", "W")
    , ("moveDown", "S")
    , ("moveLeft", "A")
    , ("moveRight", "D")
    , ("escape", "Escape")
    , ("shell", "Grave")
    ]

----------- YAML Parsing ---------------------------------------------------

data KeyBindingConfig = KeyBindingConfig
    { kbcBindings ∷ KeyBindings
    } deriving (Show, Eq)

instance FromJSON KeyBindingConfig where
  parseJSON (Object v) =
    KeyBindingConfig ⊚ v .: "keybinds" .!= defaultKeyBindings
  parseJSON _ = fail "Expected Object for KeyBindingConfig value"

----------- Loading --------------------------------------------------------

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

----------- Query Functions ------------------------------------------------

isActionDown ∷ Text → KeyBindings → InputState → Bool
isActionDown action bindings state =
    case Map.lookup action bindings of
        Just keyName → checkKeyDown keyName state
        Nothing → False

getKeyForAction ∷ Text → KeyBindings → Maybe Text
getKeyForAction = Map.lookup

----------- Key Parsing ----------------------------------------------------

parseKeyName ∷ Text → Maybe GLFW.Key
parseKeyName "Up"     = Just GLFW.Key'Up
parseKeyName "Down"   = Just GLFW.Key'Down
parseKeyName "Left"   = Just GLFW.Key'Left
parseKeyName "Right"  = Just GLFW.Key'Right
parseKeyName "Escape" = Just GLFW.Key'Escape
parseKeyName "Grave"  = Just GLFW.Key'GraveAccent
parseKeyName _        = Nothing

checkKeyDown ∷ Text → InputState → Bool
checkKeyDown keyName state =
    case parseKeyName keyName of
        Just key →
            case Map.lookup key (inpKeyStates state) of
                Just keyState → keyPressed keyState
                Nothing       → False
        Nothing → False
