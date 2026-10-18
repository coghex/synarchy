-- | Writing scenario item entries as the document values the decoder
--   reads (#2709, SCN-09 of epic #2698).
--
--   This is the item vocabulary of "Scenario.Decode" run backwards: every
--   field an entry states becomes the key the schema names
--   (@docs/scenario_format.md@, "Items"), and a field it leaves
--   'Scenario.Types.Omitted' is left out, so a decode of an encode is the
--   same entry. The file writer and coherent-capture barrier belong to
--   SCN-14; this module only turns an item tree into a JSON-shaped value
--   its document encoder (YAML included) can serialize.
module Scenario.Items.Encode
    ( itemEntryValue
    , itemEntryFields
    , floatValue
    ) where

import UPrelude
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import Scenario.Types
import Gameplay.Tags.Types (gameplayTagText)
import qualified Data.Scientific as Sci

-- | One entry as a mapping, contents included recursively.
itemEntryValue ∷ ItemEntry → A.Value
itemEntryValue = A.object ∘ itemEntryFields

-- | The key/value pairs of an entry, so an owner can add its own keys
--   beside them (a ground item's @x@ and @y@).
itemEntryFields ∷ ItemEntry → [(A.Key, A.Value)]
itemEntryFields e = concat
    [ [ ("id", A.toJSON (scenarioIdText (ieId e))) | ExplicitId _ ← [ieId e] ]
    , [ ("tags", A.toJSON (map gameplayTagText (ieTags e))) | not (null (ieTags e)) ]
    , [ ("definition", A.toJSON (ieDefinition e)) ]
    , opt "fill" (ieFill e) floatValue
    , opt "quality" (ieQuality e) floatValue
    , opt "condition" (ieCondition e) floatValue
    , opt "sharpness" (ieSharpness e) floatValue
    , opt "weight" (ieWeight e) floatValue
    , opt "bulk" (ieBulk e) (maybe A.Null floatValue)
    , opt "storage_capacity" (ieStorage e) $ \case
        Nothing → A.Null
        Just (StorageCapacity w b) → A.object [("weight", floatValue w), ("bulk", floatValue b)]
    , opt "temperature" (ieTemperature e) $ \case
        AtAmbient → A.String "ambient"
        TrackedTemp t → floatValue t
    , opt "contents" (ieContents e) (A.toJSON ∘ map itemEntryValue)
    ]
  where
    opt ∷ Text → Authored α → (α → A.Value) → [(A.Key, A.Value)]
    opt k a f = case a of
        Omitted → []
        Authored v → [(K.fromText k, f v)]

-- | A float as a number the decoder reads back to the SAME float. The
--   decoder reads a number as a double and narrows it, so the shortest
--   float digits are used when that round trip is exact (nearly always,
--   and they read well), and the exact widening of the float otherwise.
floatValue ∷ Float → A.Value
floatValue f
    | (realToFrac (Sci.toRealFloat short ∷ Double) ∷ Float) ≡ f = A.Number short
    | otherwise = A.Number (Sci.fromFloatDigits (realToFrac f ∷ Double))
  where short = Sci.fromFloatDigits f
