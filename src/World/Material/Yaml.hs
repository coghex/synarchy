{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
module World.Material.Yaml
    ( MaterialYamlEntry(..)
    , MaterialYamlFile(..)
    , parseMaterialYaml
    , loadMaterialFolder
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=), withObject, decodeEither')
import qualified Data.Yaml as Yaml
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)
import World.Material (MaterialProps(..), MaterialRegistry, registerMaterial, emptyMaterialRegistry)

data MaterialYamlEntry = MaterialYamlEntry
    { myeId       ∷ !Word8
    , myeName     ∷ !Text
    , myeHardness ∷ !Float
    , myeDensity  ∷ !Float
    , myeAlbedo   ∷ !Float
    , myeTile     ∷ !Text
    , myeZoom     ∷ !Text
    , myeBg       ∷ !Text
    } deriving (Show)

instance FromJSON MaterialYamlEntry where
    parseJSON = withObject "MaterialYamlEntry" $ \v → MaterialYamlEntry
        <$> v .: "id"
        <*> v .: "name"
        <*> v .:? "hardness" .!= 0.5
        <*> v .:? "density"  .!= 2.5
        <*> v .:? "albedo"   .!= 0.5
        <*> v .: "tile"
        <*> v .: "zoom"
        <*> v .: "bg"

newtype MaterialYamlFile = MaterialYamlFile
    { myfMaterials ∷ [MaterialYamlEntry]
    } deriving (Show)

instance FromJSON MaterialYamlFile where
    parseJSON = withObject "MaterialYamlFile" $ \v →
        MaterialYamlFile <$> v .: "materials"

-- | Parse a single YAML file, return entries + updated registry.
parseMaterialYaml ∷ FilePath → MaterialRegistry
                  → IO (Either Text ([MaterialYamlEntry], MaterialRegistry))
parseMaterialYaml path registry = do
    raw ← BS.readFile path
    case decodeEither' raw of
        Left err → return $ Left (T.pack (show err))
        Right (MaterialYamlFile entries) → do
            let registry' = foldl' (\reg entry →
                    registerMaterial (myeId entry)
                        (MaterialProps (myeName entry)
                                       (myeHardness entry)
                                       (myeDensity entry)
                                       (myeAlbedo entry))
                        reg
                    ) registry entries
            return $ Right (entries, registry')

-- | Load all .yaml files from a folder, building the full registry.
loadMaterialFolder ∷ FilePath → IO (Either Text ([MaterialYamlEntry], MaterialRegistry))
loadMaterialFolder folder = do
    files ← listDirectory folder
    let yamlFiles = filter (\f → takeExtension f ∈ [".yaml", ".yml"]) files
    go yamlFiles [] emptyMaterialRegistry
  where
    go [] accEntries reg = return $ Right (accEntries, reg)
    go (f:fs) accEntries reg = do
        let fullPath = folder </> f
        exists ← doesFileExist fullPath
        if not exists then go fs accEntries reg
        else do
            result ← parseMaterialYaml fullPath reg
            case result of
                Left err → return $ Left err
                Right (entries, reg') → go fs (accEntries <> entries) reg'
