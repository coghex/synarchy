{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
-- | Loads a unit name pool from YAML. A pool is two flat lists — given
--   names and (optional) family names — referenced from a unit def's
--   `name_pool:` key. At spawn the engine draws one of each to form a
--   unit's persistent display name (#264).
module Engine.Asset.YamlNames
    ( loadNamePool
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:?), (.!=), withObject)
import GHC.Generics (Generic)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))
import Unit.Types (NamePool(..))

-- | YAML shape of a name-pool file. Both lists default to empty so a
--   pool may declare only `given` (single-token names) — or neither,
--   which yields no name.
data NameYamlFile = NameYamlFile
    { nyfGiven  ∷ ![Text]
    , nyfFamily ∷ ![Text]
    } deriving (Show, Eq, Generic)

instance FromJSON NameYamlFile where
    parseJSON = withObject "NameYamlFile" $ \v → NameYamlFile
        ⊚ v .:? "given"  .!= []
        ⊛ v .:? "family" .!= []

-- | Load a name pool from a YAML file. A parse failure or missing file
--   logs a warning and yields an empty pool (so units fall back to their
--   species label rather than crashing the loader).
loadNamePool ∷ LoggerState → FilePath → IO NamePool
loadNamePool logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse name pool "
                <> T.pack path <> ": " <> T.pack (show err)
            return (NamePool [] [])
        Right nf → do
            let pool = NamePool (nyfGiven nf) (nyfFamily nf)
            logDebug logger CatAsset $ "Loaded name pool from "
                <> T.pack path <> " (" <> T.pack (show (length (nyfGiven nf)))
                <> " given, " <> T.pack (show (length (nyfFamily nf)))
                <> " family)"
            return pool
