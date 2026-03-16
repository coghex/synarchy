{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlTextures
    ( -- * YAML types (materials)
      MaterialDef(..)
    , MaterialFile(..)
      -- * YAML types (vegetation)
    , VegetationDef(..)
    , VegetationFile(..)
      -- * Registry
    , TextureNameRegistry
    , emptyTextureNameRegistry
      -- * Loading
    , loadMaterialYaml
    , loadMaterialDirectory
    , loadVegetationYaml
      -- * Lookup
    , lookupTextureName
    , registerTextureName
    , registryToList
      -- * World distribution
    , registryToWorldCommands
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), withObject)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Log (LoggerState, logInfo, logWarn, logDebug, LogCategory(..))

-- * Materials

data MaterialDef = MaterialDef
    { mdId   ∷ Word8
    , mdName ∷ Text
    , mdTile ∷ Text
    , mdZoom ∷ Text
    , mdBg   ∷ Text
    } deriving (Show, Eq, Generic)

instance FromJSON MaterialDef where
    parseJSON = withObject "MaterialDef" $ \v → MaterialDef
        ⊚ v .: "id"
        ⊛ v .: "name"
        ⊛ v .: "tile"
        ⊛ v .: "zoom"
        ⊛ v .: "bg"

data MaterialFile = MaterialFile
    { mfMaterials ∷ [MaterialDef]
    } deriving (Show, Eq, Generic)

instance FromJSON MaterialFile where
    parseJSON = withObject "MaterialFile" $ \v → MaterialFile
        ⊚ v .: "materials"

-- * Vegetation

-- | Variant IDs are @id_start .. id_start + len - 1@
data VegetationDef = VegetationDef
    { vdIdStart  ∷ Word8
    , vdName     ∷ Text
    , vdVariants ∷ [Text]
    } deriving (Show, Eq, Generic)

instance FromJSON VegetationDef where
    parseJSON = withObject "VegetationDef" $ \v → VegetationDef
        ⊚ v .: "id_start"
        ⊛ v .: "name"
        ⊛ v .: "variants"

data VegetationFile = VegetationFile
    { vfVegetation ∷ [VegetationDef]
    } deriving (Show, Eq, Generic)

instance FromJSON VegetationFile where
    parseJSON = withObject "VegetationFile" $ \v → VegetationFile
        ⊚ v .: "vegetation"

-- * Texture name registry

-- | Maps human-readable names to 'TextureHandle's. Populated by the Lua API
--   when textures are loaded.
--
--   Naming convention:
--
--   * @mat_tile_\<name\>@ — e.g. @"mat_tile_loam"@
--   * @mat_zoom_\<name\>@ — e.g. @"mat_zoom_loam"@
--   * @mat_bg_\<name\>@   — e.g. @"mat_bg_loam"@
--   * @veg_tile_\<id\>@   — e.g. @"veg_tile_1"@
type TextureNameRegistry = HM.HashMap Text TextureHandle

emptyTextureNameRegistry ∷ TextureNameRegistry
emptyTextureNameRegistry = HM.empty

lookupTextureName ∷ Text → TextureNameRegistry → Maybe TextureHandle
lookupTextureName = HM.lookup

registryToList ∷ TextureNameRegistry → [(Text, TextureHandle)]
registryToList = HM.toList

-- * YAML parsing

loadMaterialYaml ∷ LoggerState → FilePath → IO [MaterialDef]
loadMaterialYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse material YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right mf → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (mfMaterials mf)))
                <> " materials from " <> T.pack path
            return (mfMaterials mf)

-- | Load and concatenate all @.yaml@\/@.yml@ files in a directory (non-recursive)
loadMaterialDirectory ∷ LoggerState → FilePath → IO [MaterialDef]
loadMaterialDirectory logger dir = do
    entries ← listDirectory dir
    let yamlFiles = filter isYaml entries
    logInfo logger CatAsset $ "Loading materials from "
        <> T.pack dir <> " ("
        <> T.pack (show (length yamlFiles)) <> " files)"
    mats ← concat ⊚ mapM (\f → loadMaterialYaml logger (dir </> f)) yamlFiles
    logInfo logger CatAsset $ "Total materials loaded: "
        <> T.pack (show (length mats))
    return mats
  where
    isYaml f = takeExtension f ∈ [".yaml", ".yml"]

loadVegetationYaml ∷ LoggerState → FilePath → IO [VegetationDef]
loadVegetationYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse vegetation YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right vf → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (vfVegetation vf)))
                <> " vegetation types from " <> T.pack path
            return (vfVegetation vf)

-- * Registry building

registerTextureName ∷ IORef TextureNameRegistry → Text → TextureHandle → IO ()
registerTextureName ref name handle =
    atomicModifyIORef' ref $ \reg → (HM.insert name handle reg, ())

-- * World distribution

-- | Translate name-keyed registry entries into the numeric-ID format
--   expected by @world.setTexture@ (e.g. @"mat_tile_loam"@ → @"mat_tile_56"@)
registryToWorldCommands ∷ [MaterialDef] → TextureNameRegistry
                        → [(Text, TextureHandle)]
registryToWorldCommands defs registry =
    concatMap defToCommands defs
  where
    defToCommands def =
        let name = mdName def
            matId = T.pack (show (mdId def))
            tileName = "mat_tile_" <> name
            zoomName = "mat_zoom_" <> name
            bgName   = "mat_bg_" <> name
        in catMaybes
            [ (\h → ("mat_tile_" <> matId, h)) ⊚ HM.lookup tileName registry
            , (\h → ("mat_zoom_" <> matId, h)) ⊚ HM.lookup zoomName registry
            , (\h → ("mat_bg_"   <> matId, h)) ⊚ HM.lookup bgName   registry
            ]
