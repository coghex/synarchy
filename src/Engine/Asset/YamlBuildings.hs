{-# LANGUAGE Strict, DeriveGeneric #-}
module Engine.Asset.YamlBuildings
    ( BuildingYamlDef(..)
    , BuildingYamlAnim(..)
    , BuildingYamlTileSize(..)
    , BuildingYamlFile(..)
    , loadBuildingYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)
import Engine.Core.Log (LoggerState)
import Engine.Asset.YamlList (loadYamlList)
import Power.Base (PowerNodeSpec, powerNodeSpecFromYaml)

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
    , bydPowerNode       ∷ !(Maybe PowerNodeSpec)
      -- ^ The power NODE this def mints when placed (#1148), decoded
      --   from `power_role` + the one rating that role takes
      --   (`power_peak` watts for a source, `power_capacity` Wh for
      --   storage). Nothing (no `power_role`) = not a power node, which
      --   is every ordinary building. See Building.Types.bdPowerNode.
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
        ⊛ powerNode v

-- | Decode + validate the three optional power-node keys (#1148).
--
--   A malformed declaration is a PARSE failure, not a silently dropped
--   field: the whole file is refused (loadYamlList logs it and yields
--   no defs), so a mistyped role or a missing rating can never leave a
--   half-declared node that the build tool would route through
--   ordinary building placement. Aeson's `.:?` reads an explicit
--   `key: null` as absent, which Power.Base documents and treats as
--   "not declared".
powerNode ∷ Aeson.Object → Aeson.Parser (Maybe PowerNodeSpec)
powerNode v = do
    mName     ← v .:? "name" .!= "<unnamed>"
    mRole     ← v .:? "power_role"
    mPeak     ← v .:? "power_peak"
    mCapacity ← v .:? "power_capacity"
    case powerNodeSpecFromYaml mRole mPeak mCapacity of
        Right spec → pure spec
        Left  err  → fail (T.unpack ("building def " <> mName <> " " <> err))

newtype BuildingYamlFile = BuildingYamlFile
    { byfBuildings ∷ [BuildingYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON BuildingYamlFile where
    parseJSON = withObject "BuildingYamlFile" $ \v → BuildingYamlFile
        ⊚ v .: "buildings"

loadBuildingYaml ∷ LoggerState → FilePath → IO [BuildingYamlDef]
loadBuildingYaml logger =
    loadYamlList logger "building" "building definitions" byfBuildings
