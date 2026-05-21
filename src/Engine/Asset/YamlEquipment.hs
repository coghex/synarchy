{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlEquipment
    ( EquipmentYamlSlot(..)
    , EquipmentYamlClass(..)
    , EquipmentYamlFile(..)
    , loadEquipmentYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

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

loadEquipmentYaml ∷ LoggerState → FilePath → IO [EquipmentYamlClass]
loadEquipmentYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse equipment YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right f → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (eyfClasses f)))
                <> " equipment classes from " <> T.pack path
            return (eyfClasses f)
