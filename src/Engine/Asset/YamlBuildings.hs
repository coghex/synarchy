{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlBuildings
    ( BuildingYamlDef(..)
    , BuildingYamlAnim(..)
    , BuildingYamlTileSize(..)
    , BuildingYamlFile(..)
    , loadBuildingYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-- | Reuse of the unit anim YAML shape: per-direction frame paths.
--   For buildings we only use the "default" direction key.
data BuildingYamlAnim = BuildingYamlAnim
    { byaFps    ∷ !Float
    , byaLoop   ∷ !Bool
    , byaFrames ∷ !(Map.Map Text [Text])
    } deriving (Show, Eq, Generic)

instance FromJSON BuildingYamlAnim where
    parseJSON = withObject "BuildingYamlAnim" $ \v → BuildingYamlAnim
        ⊚ v .:? "fps"    .!= 8.0
        ⊛ v .:? "loop"   .!= False
        ⊛ v .:? "frames" .!= Map.empty

data BuildingYamlTileSize = BuildingYamlTileSize
    { bytsX ∷ !Int
    , bytsY ∷ !Int
    } deriving (Show, Eq, Generic)

instance FromJSON BuildingYamlTileSize where
    parseJSON = withObject "BuildingYamlTileSize" $ \v → BuildingYamlTileSize
        ⊚ v .:? "x" .!= 1
        ⊛ v .:? "y" .!= 1

data BuildingYamlDef = BuildingYamlDef
    { bydName         ∷ !Text
    , bydDisplayName  ∷ !Text
    , bydCategory     ∷ !Text
    , bydDescription  ∷ !Text
    , bydSprite       ∷ !Text
    , bydTileSize     ∷ !BuildingYamlTileSize
    , bydPlacement    ∷ !Text
      -- ^ "flat_ground" / other constraint kinds in the future
    , bydIsStarting   ∷ !Bool
    , bydRace         ∷ !Text
    , bydSpriteAnchor    ∷ !Text
    , bydBuildWork       ∷ !Float
    , bydMaterials       ∷ !(Map.Map Text Int)
    , bydStorageCapacity ∷ !Float
    , bydOperations      ∷ ![Text]
      -- ^ work-station operations offered when Built (#326); empty =
      --   not a station
    , bydStateAnims      ∷ !(Map.Map Text Text)
    , bydAnimations      ∷ !(Map.Map Text BuildingYamlAnim)
    , bydPowerDrain      ∷ !Float
      -- ^ Watts drawn while Built (#361); 0 (default) = not a power
      --   consumer. See Building.Types.bdPowerDrain.
    } deriving (Show, Eq, Generic)

instance FromJSON BuildingYamlDef where
    parseJSON = withObject "BuildingYamlDef" $ \v → BuildingYamlDef
        ⊚ v .:  "name"
        ⊛ v .:? "display_name"     .!= ""
        ⊛ v .:? "category"         .!= "Misc"
        ⊛ v .:? "description"      .!= ""
        ⊛ v .:  "sprite"
        ⊛ v .:? "tile_size"        .!= BuildingYamlTileSize 1 1
        ⊛ v .:? "placement"        .!= "flat_ground"
        ⊛ v .:? "is_starting"      .!= False
        ⊛ v .:? "race"             .!= ""
        ⊛ v .:? "sprite_anchor"    .!= "diamond_bottom"
        ⊛ v .:? "build_work"       .!= 0.0
        ⊛ v .:? "materials"        .!= Map.empty
        ⊛ v .:? "storage_capacity" .!= 0.0
        ⊛ v .:? "operations"       .!= []
        ⊛ v .:? "state_animations" .!= Map.empty
        ⊛ v .:? "animations"       .!= Map.empty
        ⊛ v .:? "power_drain"      .!= 0.0

newtype BuildingYamlFile = BuildingYamlFile
    { byfBuildings ∷ [BuildingYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON BuildingYamlFile where
    parseJSON = withObject "BuildingYamlFile" $ \v → BuildingYamlFile
        ⊚ v .: "buildings"

loadBuildingYaml ∷ LoggerState → FilePath → IO [BuildingYamlDef]
loadBuildingYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse building YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right uf → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (byfBuildings uf)))
                <> " building definitions from " <> T.pack path
            return (byfBuildings uf)
