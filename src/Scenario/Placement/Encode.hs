{-# LANGUAGE Strict #-}
-- | Writing scenario structure, building and location entries as the
--   document values the decoder reads (#2712, SCN-12 of epic #2698): the
--   placement vocabulary of "Scenario.Decode" run backwards, so a decode of
--   an encode is the same entry. Item entries are "Scenario.Items.Encode"'s.
--   The file writer is SCN-14's; this module only turns an entry into a
--   JSON-shaped value.
module Scenario.Placement.Encode
    ( structureEntryValue
    , buildingEntryValue
    , locationEntryValue
    ) where

import UPrelude
import Data.List (sortOn)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.HashMap.Strict as HM
import Gameplay.Tags.Types (GameplayTag, gameplayTagText)
import Scenario.Items.Encode (itemEntryValue, floatValue)
import Scenario.Types

structureEntryValue ∷ StructurePiece → A.Value
structureEntryValue p = A.object $ concat
    [ idField (spId p), tagsField (spTags p)
    , [ ("pack", A.toJSON (spPack p)), ("piece", A.toJSON (spPiece p))
      , ("x", A.toJSON (spX p)), ("y", A.toJSON (spY p)) ]
    , opt "z" (spZ p) A.toJSON
    , opt "variant" (spVariant p) A.toJSON ]

buildingEntryValue ∷ BuildingEntry → A.Value
buildingEntryValue b = A.object $ concat
    [ idField (beId b), tagsField (beTags b)
    , [ ("definition", A.toJSON (beDefinition b))
      , ("x", A.toJSON (beX b)), ("y", A.toJSON (beY b)) ]
    , opt "z" (beZ b) A.toJSON
    , opt "storage" (beStorage b) (A.toJSON ∘ map itemEntryValue)
    , opt "build_progress" (beBuildProgress b) floatValue
    , opt "materials_delivered" (beMaterialsDelivered b) (A.toJSON ∘ map itemEntryValue)
    , opt "power_charge" (bePowerCharge b) floatValue ]

locationEntryValue ∷ LocationEntry → A.Value
locationEntryValue l = A.object $ concat
    [ idField (leId l), tagsField (leTags l)
    , [ ("definition", A.toJSON (leDefinition l))
      , ("x", A.toJSON (leX l)), ("y", A.toJSON (leY l)) ]
    , [ ("significant_items"
        , A.object [ (K.fromText (tshow slot), A.toJSON (scenarioIdText r))
                   | (slot, r) ← sortOn fst (HM.toList (leSignificant l)) ])
      | not (HM.null (leSignificant l)) ]
    , [ ("containers"
        , A.object [ (K.fromText (tshow slot), binding b)
                   | (slot, b) ← sortOn fst (HM.toList (leContainers l)) ])
      | not (HM.null (leContainers l)) ] ]
  where
    binding b = A.object $
        [ ("item", A.toJSON (scenarioIdText (cbItem b))) ]
        ⧺ opt "realized" (cbRealized b) A.toJSON
        ⧺ opt "profile" (cbProfile b) A.toJSON

idField ∷ ScenarioId → [(A.Key, A.Value)]
idField sid = [ ("id", A.toJSON (scenarioIdText sid)) | ExplicitId _ ← [sid] ]

tagsField ∷ [GameplayTag] → [(A.Key, A.Value)]
tagsField ts = [ ("tags", A.toJSON (map gameplayTagText ts)) | not (null ts) ]

opt ∷ Text → Authored α → (α → A.Value) → [(A.Key, A.Value)]
opt k a f = case a of
    Omitted → []
    Authored v → [(K.fromText k, f v)]
