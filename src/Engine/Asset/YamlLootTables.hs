{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlLootTables
    ( LootTableYamlEntry(..)
    , LootTableYamlDef(..)
    , loadLootTableYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), withObject)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

-- | One `{id, weight}` loot table entry.
data LootTableYamlEntry = LootTableYamlEntry
    { ltyeId     ∷ !Text
    , ltyeWeight ∷ !Float
    } deriving (Show, Eq, Generic)

instance FromJSON LootTableYamlEntry where
    parseJSON = withObject "LootTableYamlEntry" $ \v → LootTableYamlEntry
        ⊚ v .: "id"
        ⊛ v .: "weight"

-- | The YAML shape of a loot table def. Unlike locations/items/units,
--   one file IS one def — no wrapping list — so the top-level document
--   parses directly into this type.
data LootTableYamlDef = LootTableYamlDef
    { ltydId      ∷ !Text
    , ltydEntries ∷ ![LootTableYamlEntry]
    } deriving (Show, Eq, Generic)

instance FromJSON LootTableYamlDef where
    parseJSON = withObject "LootTableYamlDef" $ \v → LootTableYamlDef
        ⊚ v .: "id"
        ⊛ v .: "entries"

-- | Parse one loot table YAML file. Returns 'Nothing' (with a logged
--   warning) on a parse failure — mirrors 'loadLocationYaml', except
--   there is only ever at most one def per file.
loadLootTableYaml ∷ LoggerState → FilePath → IO (Maybe LootTableYamlDef)
loadLootTableYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse loot table YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return Nothing
        Right def → do
            logDebug logger CatAsset $ "Loaded loot table '"
                <> ltydId def <> "' from " <> T.pack path
            return (Just def)
