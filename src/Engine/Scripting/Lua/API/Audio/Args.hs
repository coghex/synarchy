-- | Closed, bounded, raw-table audio arguments. No Lua metamethod executes
-- while validating a request; numeric strings and nonfinite values are refused.
module Engine.Scripting.Lua.API.Audio.Args
  ( readText, readValue, parseTrigger, parseLoopUpdate, parseVolumes ) where

import UPrelude
import Data.Bifunctor (first)
import Data.Aeson (Value(..), toJSON, (.:), (.:?), (.!=))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified HsLua as Lua
import Engine.Audio.Config.Parse (strictObject)
import Engine.Audio.Config.Player
import Engine.Audio.Types
import World.Page.Types (WorldPageId(..))

readText ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Either Text Text)
readText index = do
  kind ← Lua.ltype index
  if kind ≢ Lua.TypeString then pure (Left "expected a string") else do
    bytes ← Lua.tostring index
    pure $ case bytes of
      Just value | BS.length value ≤ 4096 → first (const "invalid UTF-8") (Text.decodeUtf8' value)
      _ → Left "audio string is too long"

readValue ∷ Int → Lua.StackIndex → Lua.LuaE Lua.Exception (Either Text Value)
readValue depth index = do
  kind ← Lua.ltype index
  case kind of
    Lua.TypeNone → pure (Right Null)
    Lua.TypeNil → pure (Right Null)
    Lua.TypeString → fmap String <$> readText index
    Lua.TypeNumber → do
      number ← Lua.tonumber index
      pure $ case number of
        Just (Lua.Number n) | not (isNaN n ∨ isInfinite n) → Right (toJSON n)
        _ → Left "expected a finite number"
    Lua.TypeTable | depth > 0 → do
      absolute ← Lua.absindex index
      Lua.pushnil
      pairs absolute 0 KM.empty
    _ → pure (Left "expected an audio options table, string, or finite number")
  where
    pairs absolute count values = do
      more ← Lua.next absolute
      if not more then pure (Right $ Object values)
      else if count ≥ (16 ∷ Int) then Lua.pop 2 >> pure (Left "too many audio fields")
      else do
        key ← readText (-2)
        value ← readValue (depth - 1) (-1)
        Lua.pop 1
        case (key, value) of
          (Right name, Right entry) → pairs absolute (count + 1) (KM.insert (Key.fromText name) entry values)
          (Left err, _) → Lua.pop 1 >> pure (Left err)
          (_, Left err) → Lua.pop 1 >> pure (Left err)

positionPair ∷ Maybe Text → Maybe Value → Parser (Maybe AudioPosition)
positionPair Nothing Nothing = pure Nothing
positionPair (Just page) (Just value) = do
  object ← strictObject "audio position" ["x", "y", "z"] value
  position ← AudioPosition (WorldPageId page) <$> object .: "x" <*> object .: "y" <*> object .: "z"
  unless (validPosition position) $ fail "invalid audio page/position"
  pure (Just position)
positionPair _ _ = fail "pageId and position must be supplied together"

parseTrigger ∷ Value → Parser TriggerOptions
parseTrigger Null = pure defaultTriggerOptions
parseTrigger value = do
  object ← strictObject "audio trigger" ["pageId", "position", "gainDb", "pitchSemitones"] value
  page ← object .:? "pageId"
  rawPosition ← object .:? "position"
  result ← TriggerOptions <$> positionPair page rawPosition
    <*> object .:? "gainDb" .!= 0 <*> object .:? "pitchSemitones" .!= 0
  unless (validTrigger result) $ fail "audio gain/pitch is out of range"
  pure result

parseLoopUpdate ∷ Value → Parser LoopOptions
parseLoopUpdate value = do
  object ← strictObject "audio loop update" ["pageId", "position", "gainDb"] value
  page ← object .:? "pageId"
  rawPosition ← object .:? "position"
  result ← LoopOptions <$> positionPair page rawPosition <*> object .:? "gainDb"
  unless (validLoopOptions result) $ fail "audio loop gain is out of range"
  pure result

parseVolumes ∷ Value → Parser Volumes
parseVolumes value = do
  object ← strictObject "audio volumes" ["master", "world", "ui"] value
  result ← Volumes <$> object .: "master" <*> object .: "world" <*> object .: "ui"
  unless (result ≡ clampVolumes result) $ fail "audio volumes must be integers in 0..100"
  pure result
