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

-----------------------------------------------------------
-- YAML Data Types (Materials)
-----------------------------------------------------------

-- | A single material definition from YAML.
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

-- | Top-level YAML file structure.
data MaterialFile = MaterialFile
    { mfMaterials ∷ [MaterialDef]
    } deriving (Show, Eq, Generic)

instance FromJSON MaterialFile where
    parseJSON = withObject "MaterialFile" $ \v → MaterialFile
        ⊚ v .: "materials"

-----------------------------------------------------------
-- YAML Data Types (Vegetation)
-----------------------------------------------------------

-- | A single vegetation type definition from YAML.
--   Each type has a base id_start and a list of variant
--   texture paths.  Variant IDs are id_start .. id_start+len-1.
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

-- | Top-level vegetation YAML file structure.
data VegetationFile = VegetationFile
    { vfVegetation ∷ [VegetationDef]
    } deriving (Show, Eq, Generic)

instance FromJSON VegetationFile where
    parseJSON = withObject "VegetationFile" $ \v → VegetationFile
        ⊚ v .: "vegetation"

-----------------------------------------------------------
-- Texture Name Registry
--
-- Maps human-readable names to TextureHandles.
-- Populated by the Lua API when textures are loaded.
-- Queried by Lua to get handles by name.
-----------------------------------------------------------

-- | Maps name → TextureHandle.
--   Names follow the convention:
--     mat_tile_<name>   e.g. "mat_tile_loam"
--     mat_zoom_<name>   e.g. "mat_zoom_loam"
--     mat_bg_<name>     e.g. "mat_bg_loam"
--     veg_tile_<id>     e.g. "veg_tile_1"
type TextureNameRegistry = HM.HashMap Text TextureHandle

emptyTextureNameRegistry ∷ TextureNameRegistry
emptyTextureNameRegistry = HM.empty

-- | Look up a texture handle by name.
lookupTextureName ∷ Text → TextureNameRegistry → Maybe TextureHandle
lookupTextureName = HM.lookup

-- | Convert registry to association list (for debugging / iteration).
registryToList ∷ TextureNameRegistry → [(Text, TextureHandle)]
registryToList = HM.toList

-----------------------------------------------------------
-- YAML Parsing (Materials)
-----------------------------------------------------------

-- | Parse a single YAML file into a list of MaterialDefs.
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

-- | Load all .yaml files in a directory (non-recursive).
--   Returns the combined list of MaterialDefs from all files.
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

-----------------------------------------------------------
-- YAML Parsing (Vegetation)
-----------------------------------------------------------

-- | Parse a single vegetation YAML file into a list of VegetationDefs.
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

-----------------------------------------------------------
-- Registry Building
--
-- Called from the Lua API after textures are loaded.
-- For each MaterialDef, inserts three entries:
--   mat_tile_<name> → handle
--   mat_zoom_<name> → handle
--   mat_bg_<name>   → handle
-----------------------------------------------------------

-- | Insert a named handle into the registry IORef.
registerTextureName ∷ IORef TextureNameRegistry → Text → TextureHandle → IO ()
registerTextureName ref name handle =
    atomicModifyIORef' ref $ \reg → (HM.insert name handle reg, ())

-----------------------------------------------------------
-- World Distribution
--
-- Generate the (texType string, TextureHandle) pairs
-- needed by world.setTexture for all materials in the
-- registry that match the mat_ prefix.
--
-- This maps from our name convention back to the existing
-- WorldTextureType string convention:
--   "mat_tile_<name>" → "mat_tile_<id>"
--   "mat_zoom_<name>" → "mat_zoom_<id>"
--   "mat_bg_<name>"   → "mat_bg_<id>"
-----------------------------------------------------------

-- | Given a list of MaterialDefs and the registry, produce
--   (worldTexType, handle) pairs for world.setTexture.
--   The worldTexType uses numeric IDs as the existing
--   parseTextureType expects: "mat_tile_56", "mat_zoom_56", etc.
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
