{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Input.Bindings where

import UPrelude
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Data.Aeson ((.:?), FromJSON(..), ToJSON(..), Value(..), object, (.=))
import Data.Aeson.Types (Parser)
import qualified Graphics.UI.GLFW as GLFW
import Engine.Input.Types
import Engine.Core.Log (LoggerState, logWarn, logInfo, LogCategory(..), logDebug)

-- | Each action maps to a **list** of key names; the action fires when
--   *any* of them is held. Persisted in @keybinds.yaml@ as YAML arrays.
type KeyBindings = Map.Map T.Text [T.Text]

defaultKeyBindings ∷ KeyBindings
defaultKeyBindings = Map.fromList
    [ ("moveUp",         ["Up", "W"])
    , ("moveDown",       ["Down", "S"])
    , ("moveLeft",       ["Left", "A"])
    , ("moveRight",      ["Right", "D"])
    , ("rotateCCW",      ["Q"])
    , ("rotateCW",       ["E"])
    , ("resetZTracking", ["Home"])
    -- escape/openShell are engine-hardcoded and NOT editable; kept here
    -- and in the config for reference only.
    , ("escape",         ["Escape"])
    , ("openShell",      ["Grave"])
    , ("toggleEventLog", ["L"])
    ]

-- | Actions the engine handles outside the binding table (Escape cascade,
--   Grave shell toggle — see "Engine.Input.Thread"). Their key lists in the
--   config are reference-only and never consulted for behavior, so
--   'sanitizeBindings' leaves them intact while stripping reserved keys from
--   every *editable* action.
reservedActions ∷ [Text]
reservedActions = ["escape", "openShell"]

-- | GLFW keys that must never be reachable through a binding — they are
--   hardcoded (escape cascade, shell toggle) and reassigning them would
--   persist a config the runtime cannot honor.
reservedGLFWKeys ∷ [GLFW.Key]
reservedGLFWKeys = [GLFW.Key'Escape, GLFW.Key'GraveAccent]

-- | True when a key name resolves (via 'parseKeyName') to a reserved key,
--   regardless of which alias spelling was used.
isReservedKeyName ∷ Text → Bool
isReservedKeyName name = any (`elem` reservedGLFWKeys) (parseKeyName name)

-- | Strip reserved key names from every editable action's key list so a
--   hand-edited @keybinds.yaml@ can never make Escape/Grave drive an action
--   (the Lua edit API already refuses them; this closes the file path).
--   Reserved-action reference entries are left untouched.
sanitizeBindings ∷ KeyBindings → KeyBindings
sanitizeBindings = Map.mapWithKey strip
  where strip action keys
          | action `elem` reservedActions = keys
          | otherwise                     = filter (not . isReservedKeyName) keys

data KeyBindingConfig = KeyBindingConfig
    { kbcBindings ∷ KeyBindings
    } deriving (Show, Eq)

-- | Backward-compatible parse: each binding value may be either the
--   legacy scalar form (@moveUp: W@) or the new list form
--   (@moveUp: [Up, W]@). The parsed bindings are merged *over*
--   'defaultKeyBindings' (file entries win), so a config written before
--   a new action existed still picks up that action's default binding
--   rather than leaving it unbound. A missing @keybinds@ key falls back
--   to defaults entirely. Reserved keys (Escape/Grave) are stripped from
--   editable actions via 'sanitizeBindings' so a hand-edited file cannot
--   bind them to gameplay.
instance FromJSON KeyBindingConfig where
  parseJSON (Object v) = do
    mRaw ← v .:? "keybinds" ∷ Parser (Maybe (Map.Map T.Text Value))
    case mRaw of
      Nothing  → pure (KeyBindingConfig defaultKeyBindings)
      Just raw → do
        parsed ← traverse parseKeyList raw
        pure (KeyBindingConfig (sanitizeBindings (Map.union parsed defaultKeyBindings)))
  parseJSON _ = fail "Expected Object for KeyBindingConfig value"

instance ToJSON KeyBindingConfig where
  toJSON (KeyBindingConfig bindings) = object ["keybinds" .= bindings]

-- | Parse one binding value: a bare string becomes a singleton list, a
--   YAML array becomes a list of strings.
parseKeyList ∷ Value → Parser [T.Text]
parseKeyList (String s) = pure [s]
parseKeyList (Array a)  = traverse parseEntry (V.toList a)
  where parseEntry (String s) = pure s
        parseEntry _          = fail "keybind list entries must be strings"
parseKeyList _ = fail "keybind value must be a string or a list of strings"

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

-- | Save keybindings back to a YAML file as @keybinds:@ arrays.
saveKeyBindings ∷ LoggerState → FilePath → KeyBindings → IO ()
saveKeyBindings logger path bindings = do
    Yaml.encodeFile path (KeyBindingConfig bindings)
    logInfo logger CatInput $ "Saved keybindings to " <> T.pack path
                            <> " (" <> T.pack (show (Map.size bindings)) <> " actions)"

-- | True when *any* key bound to the action is currently held.
isActionDown ∷ Text → KeyBindings → InputState → Bool
isActionDown action bindings state =
    case Map.lookup action bindings of
        Just keyNames → any (`checkKeyDown` state) keyNames
        Nothing → False

getKeysForAction ∷ Text → KeyBindings → Maybe [Text]
getKeysForAction = Map.lookup

-- | True when the key that just triggered an @onKeyDown@ event is bound to
--   the action. Used for edge-triggered actions (rotate, z-reset), where the
--   specific pressed key must match rather than polling whether the action
--   is held ('isActionDown').
--
--   Lua's @onKeyDown@ receives the *merged* key name ("Shift" for either
--   side, see 'keyToText'), which alone can't honor a side-specific binding
--   like @LeftShift@. We disambiguate against the live 'InputState': of the
--   GLFW keys the event name covers, keep only those actually held, so the
--   match is side-specific in exactly the way 'isActionDown'/'checkKeyDown'
--   are. If none are recorded as held (a release/timing edge), we fall back
--   to all candidates so a genuine press is never dropped.
keyMatchesAction ∷ Text → Text → KeyBindings → InputState → Bool
keyMatchesAction keyName action bindings st =
    case Map.lookup action bindings of
        Just boundKeys →
            let candidates = parseKeyName keyName
                held       = filter isHeld candidates
                effective  = if null held then candidates else held
            in any (\b → any (`elem` effective) (parseKeyName b)) boundKeys
        Nothing → False
  where isHeld g = maybe False keyPressed (Map.lookup g (inpKeyStates st))

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
