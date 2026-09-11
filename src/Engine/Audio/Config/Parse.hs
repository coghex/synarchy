-- | Shared strict object and numeric checks for authored audio configuration.
module Engine.Audio.Config.Parse
  ( strictObject, fieldObject, finiteRange, integerRange, enumValue ) where

import UPrelude
import Data.Aeson (Object, Value, FromJSON, withObject, (.:))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as Text

strictObject ∷ String → [Text] → Value → Parser Object
strictObject label keys = withObject label $ \o → do
  let unknown = filter (`notElem` keys) (map Key.toText $ KM.keys o)
  unless (null unknown) $ fail (label <> ": unknown keys " <> show unknown)
  pure o

fieldObject ∷ Object → Text → [Text] → Parser Object
fieldObject o key keys = o .: Key.fromText key ⌦ strictObject (Text.unpack key) keys

finiteRange ∷ Object → Text → Float → Float → Parser Float
finiteRange o key low high = do
  value ← o .: Key.fromText key
  unless (not (isNaN value ∨ isInfinite value) ∧ value ≥ low ∧ value ≤ high) $
    fail (Text.unpack key <> ": expected finite value in " <> show (low, high))
  pure value

integerRange ∷ (FromJSON α, Integral α, Show α) ⇒ Object → Text → α → α → Parser α
integerRange o key low high = do
  value ← o .: Key.fromText key
  unless (value ≥ low ∧ value ≤ high) $
    fail (Text.unpack key <> ": expected integer in " <> show (low, high))
  pure value

enumValue ∷ Object → Text → [(Text, α)] → Parser α
enumValue o key values = do
  value ← o .: Key.fromText key
  maybe (fail $ Text.unpack key <> ": unsupported value " <> Text.unpack value)
    pure (lookup value values)
