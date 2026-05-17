{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlItems
    ( ItemYamlDef(..)
    , ItemYamlContainer(..)
    , ItemYamlFile(..)
    , loadItemYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-- | Optional container block. Items without this can't hold a fluid.
data ItemYamlContainer = ItemYamlContainer
    { iycCapacity ∷ !Float
    , iycHolds    ∷ !Text
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlContainer where
    parseJSON = withObject "ItemYamlContainer" $ \v → ItemYamlContainer
        ⊚ v .:  "capacity"
        ⊛ v .:? "holds" .!= "water"

data ItemYamlDef = ItemYamlDef
    { iydName        ∷ !Text
    , iydDisplayName ∷ !Text
    , iydSprite      ∷ !Text                       -- ^ texture path
    , iydWeight      ∷ !Float                      -- ^ empty weight (kg)
    , iydContainer   ∷ !(Maybe ItemYamlContainer)
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlDef where
    parseJSON = withObject "ItemYamlDef" $ \v → ItemYamlDef
        ⊚ v .:  "name"
        ⊛ v .:? "display_name" .!= ""
        ⊛ v .:  "sprite"
        ⊛ v .:? "weight"       .!= 0.0
        ⊛ v .:? "container"

newtype ItemYamlFile = ItemYamlFile
    { iyfItems ∷ [ItemYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON ItemYamlFile where
    parseJSON = withObject "ItemYamlFile" $ \v → ItemYamlFile
        ⊚ v .: "items"

loadItemYaml ∷ LoggerState → FilePath → IO [ItemYamlDef]
loadItemYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse item YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right uf → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (iyfItems uf)))
                <> " item definitions from " <> T.pack path
            return (iyfItems uf)
