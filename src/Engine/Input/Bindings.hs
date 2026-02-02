module Engine.Input.Bindings where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:), (.!=), FromJSON(..), Value(..))
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Types
import Engine.Core.Log (LoggerState, logWarn, LogCategory(..))

-- | maps actino names to key names
type KeyBindings = Map.Map T.Text T.Text

-- | fallback default keybindings
defaultKeyBindings :: KeyBindings
defaultKeyBindings = Map.fromList
    [ ("moveUp", "W")
    , ("moveDown", "S")
    , ("moveLeft", "A")
    , ("moveRight", "D")
    , ("escape", "Escape")
    , ("shell", "Grave")
    ]

-- | YAML structure
data KeyBindingConfig = KeyBindingConfig
    { kbcBindings ∷ KeyBindings
    } deriving (Show, Eq)

instance FromJSON KeyBindingConfig where
  parseJSON (Object v) =
    KeyBindingConfig <$> v .: "keybinds" .!= defaultKeyBindings
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
        Right config → return $ kbcBindings config

-- | check if action is pressed
isActionDown ∷ Text → KeyBindings → InputState → Bool
isActionDown action bindings state =
    case Map.lookup action bindings of
        Just keyName → checkKeyDown keyName state
        Nothing → False

-- | get key name for action
getKeyForAction ∷ Text → KeyBindings → Maybe Text
getKeyForAction = Map.lookup

-- | parse key name to GLFW key
parseKeyName ∷ Text → Maybe GLFW.Key
parseKeyName "W"      = Just GLFW.Key'W
parseKeyName "A"      = Just GLFW.Key'A
parseKeyName "S"      = Just GLFW.Key'S
parseKeyName "D"      = Just GLFW.Key'D
parseKeyName "Escape" = Just GLFW.Key'Escape
parseKeyName "Grave"  = Just GLFW.Key'GraveAccent
parseKeyName _        = Nothing

-- | check if key is down in input state
checkKeyDown ∷ Text → InputState → Bool
checkKeyDown keyName state =
    case parseKeyName keyName of
        Just key →
            case Map.lookup key (inpKeyStates state) of
                Just keyState → keyPressed keyState
                Nothing       → False
        Nothing → False
