{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlLocations
    ( LocationYamlPosition(..)
    , LocationYamlContent(..)
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

-- | A fixed relative tile offset from a location's anchor (#90).
data LocationYamlPosition = LocationYamlPosition
    { lypX ∷ !Int
    , lypY ∷ !Int
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlPosition where
    parseJSON = withObject "LocationYamlPosition" $ \v → LocationYamlPosition
        ⊚ v .:? "x" .!= 0
        ⊛ v .:? "y" .!= 0

-- | One `{kind, id, count, position, faction, rolls}` content entry.
--   `count` defaults to 1; `position`/`faction`/`rolls` are all
--   optional (#90) — see 'Location.Types.LocationContent'.
data LocationYamlContent = LocationYamlContent
    { lycKind     ∷ !Text
    , lycId       ∷ !Text
    , lycCount    ∷ !Int
    , lycPosition ∷ !(Maybe LocationYamlPosition)
    , lycFaction  ∷ !(Maybe Text)
    , lycRolls    ∷ !Int
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlContent where
    parseJSON = withObject "LocationYamlContent" $ \v → LocationYamlContent
        ⊚ v .:  "kind"
        ⊛ v .:  "id"
        ⊛ v .:? "count"    .!= 1
        ⊛ v .:? "position"
        ⊛ v .:? "faction"
        ⊛ v .:? "rolls"    .!= 1

-- | The YAML shape of a location definition. Converted to
--   'Location.Types.LocationDef' by the API loader.
data LocationYamlDef = LocationYamlDef
    { lydId         ∷ !Text
    , lydLabel      ∷ !Text
    , lydType       ∷ !Text
    , lydBuilder    ∷ !Text
    , lydAnchor     ∷ ![Text]
    , lydMaxCount   ∷ !Int   -- ^ max placements (#89); default 4
    , lydMinSpacing ∷ !Int   -- ^ min chunk separation (#89); default 4
    , lydContents   ∷ ![LocationYamlContent]
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlDef where
    parseJSON = withObject "LocationYamlDef" $ \v → LocationYamlDef
        ⊚ v .:  "id"
        ⊛ v .:? "label"       .!= ""
        ⊛ v .:? "type"        .!= "natural"
        ⊛ v .:  "builder"
        ⊛ v .:? "anchor"      .!= []
        ⊛ v .:? "max_count"   .!= 4
        ⊛ v .:? "min_spacing" .!= 4
        ⊛ v .:? "contents"    .!= []

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
