-- | Writing scenario unit entries as the document values the decoder
--   reads (#2710, SCN-10 of epic #2698): the unit vocabulary of
--   "Scenario.Decode" run backwards, so a decode of an encode is the same
--   entry. Item entries are "Scenario.Items.Encode"'s. The file writer is
--   SCN-14's; this module only turns an entry into a JSON-shaped value.
module Scenario.Units.Encode
    ( unitEntryValue
    , directionText
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.HashMap.Strict as HM
import Gameplay.Tags.Types (gameplayTagText)
import Scenario.Items.Encode (itemEntryValue, floatValue)
import Scenario.Types
import Unit.Direction (Direction(..))

-- | The long spelling 'Unit.Direction.parseDirectionName' reads.
directionText ∷ Direction → Text
directionText d = case d of
    DirS → "south"; DirSW → "south-west"; DirW → "west"; DirNW → "north-west"
    DirN → "north"; DirNE → "north-east"; DirE → "east"; DirSE → "south-east"

unitEntryValue ∷ UnitEntry → A.Value
unitEntryValue u = A.object $ concat
    [ [ ("id", A.toJSON (scenarioIdText (ueId u))) | ExplicitId _ ← [ueId u] ]
    , [ ("tags", A.toJSON (map gameplayTagText (ueTags u))) | not (null (ueTags u)) ]
    , [ ("definition", A.toJSON (ueDefinition u))
      , ("x", floatValue (ueX u)), ("y", floatValue (ueY u)) ]
    , opt "z" (ueZ u) A.toJSON
    , opt "name" (ueName u) A.toJSON
    , opt "facing" (ueFacing u) (A.toJSON ∘ directionText)
    , [ ("encounter", A.toJSON (scenarioIdText e)) | Just e ← [ueEncounter u] ]
    , [ ("stats", floatMap (ueStats u)) | not (HM.null (ueStats u)) ]
    , [ ("skills", floatMap (ueSkills u)) | not (HM.null (ueSkills u)) ]
    , opt "knowledge" (ueKnowledge u) floatMap
    , opt "modifiers" (ueModifiers u) $ \m →
        A.object [ (K.fromText k, A.toJSON (map modifierValue ms)) | (k, ms) ← sortOn fst (HM.toList m) ]
    , opt "wounds" (ueWounds u) (A.toJSON ∘ map woundValue)
    , opt "scars" (ueScars u) (A.toJSON ∘ map scarValue)
    , opt "blood" (ueBlood u) floatValue
    , opt "immune_response" (ueImmuneResponse u) floatValue
    , opt "immunities" (ueImmunities u) floatMap
    , opt "inventory" (ueInventory u) (A.toJSON ∘ map itemEntryValue)
    , opt "equipment" (ueEquipment u) $ \m →
        A.object [ (K.fromText k, itemEntryValue e) | (k, e) ← sortOn fst (HM.toList m) ]
    , opt "accessories" (ueAccessories u) (A.toJSON ∘ map itemEntryValue)
    ]

opt ∷ Text → Authored α → (α → A.Value) → [(A.Key, A.Value)]
opt k a f = case a of
    Omitted → []
    Authored v → [(K.fromText k, f v)]

floatMap ∷ HM.HashMap Text Float → A.Value
floatMap m = A.object [ (K.fromText k, floatValue v) | (k, v) ← sortOn fst (HM.toList m) ]

modifierValue ∷ ModifierSpec → A.Value
modifierValue m = A.object $
    [ ("source", A.toJSON (msSource m)), ("delta", floatValue (msDelta m))
    , ("percent", floatValue (msPercent m)) ]
    ⧺ [ ("remaining", floatValue r) | Just r ← [msRemaining m] ]

woundValue ∷ WoundSpec → A.Value
woundValue w = A.object $ concat
    [ [ ("part", A.toJSON (wsPart w)), ("kind", A.toJSON (wsKind w))
      , ("severity", floatValue (wsSeverity w)) ]
    , opt "age" (wsAge w) floatValue
    , opt "bandage" (wsBandage w) floatValue
    , opt "clot" (wsClot w) floatValue
    , opt "heal" (wsHeal w) floatValue
    , opt "dressing" (wsDressing w) A.toJSON
    , opt "infection" (wsInfection w) floatValue
    , opt "clean" (wsClean w) A.toJSON
    , opt "infection_type" (wsInfectionType w) A.toJSON
    , opt "necrosis" (wsNecrosis w) floatValue ]

scarValue ∷ ScarSpec → A.Value
scarValue s = A.object $
    [ ("part", A.toJSON (ssPart s)), ("kind", A.toJSON (ssKind s))
    , ("severity", floatValue (ssSeverity s)) ]
    ⧺ opt "age" (ssAge s) floatValue
