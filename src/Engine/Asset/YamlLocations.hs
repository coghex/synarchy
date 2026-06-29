{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlLocations
    ( LocationYamlContent(..)
    , LocationYamlDef(..)
    , LocationYamlFile(..)
    , loadLocationYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-- | One `{kind, id, count}` content entry. `count` defaults to 1.
data LocationYamlContent = LocationYamlContent
    { lycKind  ∷ !Text
    , lycId    ∷ !Text
    , lycCount ∷ !Int
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlContent where
    parseJSON = withObject "LocationYamlContent" $ \v → LocationYamlContent
        ⊚ v .:  "kind"
        ⊛ v .:  "id"
        ⊛ v .:? "count" .!= 1

-- | The YAML shape of a location definition. Converted to
--   'Location.Types.LocationDef' by the API loader.
data LocationYamlDef = LocationYamlDef
    { lydId       ∷ !Text
    , lydLabel    ∷ !Text
    , lydType     ∷ !Text
    , lydBuilder  ∷ !Text
    , lydAnchor   ∷ ![Text]
    , lydContents ∷ ![LocationYamlContent]
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlDef where
    parseJSON = withObject "LocationYamlDef" $ \v → LocationYamlDef
        ⊚ v .:  "id"
        ⊛ v .:? "label"    .!= ""
        ⊛ v .:? "type"     .!= "natural"
        ⊛ v .:  "builder"
        ⊛ v .:? "anchor"   .!= []
        ⊛ v .:? "contents" .!= []

newtype LocationYamlFile = LocationYamlFile
    { lyfLocations ∷ [LocationYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlFile where
    parseJSON = withObject "LocationYamlFile" $ \v → LocationYamlFile
        ⊚ v .: "locations"

loadLocationYaml ∷ LoggerState → FilePath → IO [LocationYamlDef]
loadLocationYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse location YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right lf → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (lyfLocations lf)))
                <> " location definitions from " <> T.pack path
            return (lyfLocations lf)
