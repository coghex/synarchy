{-# LANGUAGE Strict, DeriveGeneric #-}
module Engine.Asset.YamlEquipment
    ( EquipmentYamlSlot(..)
    , EquipmentYamlClass(..)
    , EquipmentYamlFile(..)
    , loadEquipmentYaml
    , loadEquipmentYamlOutcome
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState)
import Engine.Asset.YamlList (loadYamlListOutcome)

-- | One slot entry inside an equipment class YAML.
data EquipmentYamlSlot = EquipmentYamlSlot
    { eysId   ∷ !Text
    , eysName ∷ !Text
    , eysKind ∷ !Text
    , eysX    ∷ !Int
    , eysY    ∷ !Int
    , eysW    ∷ !Int
    , eysH    ∷ !Int
    } deriving (Show, Eq, Generic)

instance FromJSON EquipmentYamlSlot where
    parseJSON = withObject "EquipmentYamlSlot" $ \v → EquipmentYamlSlot
        ⊚ v .:  "id"
        ⊛ v .:? "name" .!= ""
        ⊛ v .:  "kind"
        ⊛ v .:  "x"
        ⊛ v .:  "y"
        ⊛ v .:? "w" .!= 32
        ⊛ v .:? "h" .!= 32

-- | A single equipment class definition (one per YAML file). Silhouette
--   is the texture rendered as the background; slots are placed over
--   it using their (x, y) pixel offsets.
data EquipmentYamlClass = EquipmentYamlClass
    { eycName          ∷ !Text
    , eycSilhouette    ∷ !Text                  -- ^ texture path
    , eycSilhouetteW   ∷ !Int                   -- ^ source-pixel width
    , eycSilhouetteH   ∷ !Int                   -- ^ source-pixel height
    , eycSlots         ∷ ![EquipmentYamlSlot]
    } deriving (Show, Eq, Generic)

instance FromJSON EquipmentYamlClass where
    parseJSON = withObject "EquipmentYamlClass" $ \v → EquipmentYamlClass
        ⊚ v .:  "name"
        ⊛ v .:  "silhouette"
        ⊛ v .:  "silhouette_w"
        ⊛ v .:  "silhouette_h"
        ⊛ v .:? "slots" .!= []

newtype EquipmentYamlFile = EquipmentYamlFile
    { eyfClasses ∷ [EquipmentYamlClass]
    } deriving (Show, Eq, Generic)

instance FromJSON EquipmentYamlFile where
    parseJSON = withObject "EquipmentYamlFile" $ \v → EquipmentYamlFile
        ⊚ v .: "classes"

-- | 'loadEquipmentYaml' with the decode OUTCOME kept (#2203):
--   'Nothing' is a parse failure, @Just xs@ a file that decoded
--   (possibly to an empty list). The startup loader needs the two
--   apart; every other caller reads 'loadEquipmentYaml'.
loadEquipmentYamlOutcome
    ∷ LoggerState → FilePath → IO (Maybe [EquipmentYamlClass])
loadEquipmentYamlOutcome logger =
    loadYamlListOutcome logger "equipment" "equipment classes" eyfClasses

loadEquipmentYaml ∷ LoggerState → FilePath → IO [EquipmentYamlClass]
loadEquipmentYaml logger path =
    fromMaybe [] ⊚ loadEquipmentYamlOutcome logger path
