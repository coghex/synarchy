{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlUnits
    ( UnitYamlDef(..)
    , UnitYamlFile(..)
    , loadUnitYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-----------------------------------------------------------
-- YAML Data Types
-----------------------------------------------------------

-- | A single unit definition from YAML.
--   Only 'name' and 'sprite' are mandatory.
data UnitYamlDef = UnitYamlDef
    { uydName              ∷ !Text       -- ^ unique identifier (e.g. "acolyte")
    , uydSprite            ∷ !Text       -- ^ path to default sprite texture
    , uydBaseWidth         ∷ !Float      -- ^ ground contact diameter in pixels (0 = point)
    , uydDirectionalSprites ∷ !(Map.Map Text Text)
      -- ^ optional: direction key ("S","SW",…) → texture path
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlDef where
    parseJSON = withObject "UnitYamlDef" $ \v → UnitYamlDef
        ⊚ v .:  "name"
        ⊛ v .:  "sprite"
        ⊛ v .:? "base_width"          .!= 0.0
        ⊛ v .:? "directional_sprites" .!= Map.empty

-- | Top-level YAML file structure.
newtype UnitYamlFile = UnitYamlFile
    { uyfUnits ∷ [UnitYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlFile where
    parseJSON = withObject "UnitYamlFile" $ \v → UnitYamlFile
        ⊚ v .: "units"

-----------------------------------------------------------
-- YAML Parsing
-----------------------------------------------------------

-- | Parse a single unit YAML file into a list of UnitYamlDefs.
loadUnitYaml ∷ LoggerState → FilePath → IO [UnitYamlDef]
loadUnitYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse unit YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right uf → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (uyfUnits uf)))
                <> " unit definitions from " <> T.pack path
            return (uyfUnits uf)
