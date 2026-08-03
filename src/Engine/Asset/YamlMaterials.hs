{-# LANGUAGE Strict, DeriveGeneric #-}
-- | Material definitions loaded from @data/materials/*.yaml@, and the
--   fold of those definitions into a 'World.Material.MaterialRegistry'.
module Engine.Asset.YamlMaterials
    ( -- * YAML types
      MaterialDef(..)
    , MaterialFile(..)
      -- * Loading
    , loadMaterialYaml
    , loadMaterialDirectory
    , loadPopulatedMaterialRegistry
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Engine.Asset.YamlList (loadYamlList)
import Engine.Core.Log (LoggerState, logInfo, LogCategory(..))
import World.Material
    (MaterialRegistry, MaterialProps(..), registerMaterial, emptyMaterialRegistry)

data MaterialDef = MaterialDef
    { mdId       ∷ Word8
    , mdName     ∷ Text
    , mdHardness ∷ Float
    , mdDensity  ∷ Float
    , mdAlbedo   ∷ Float
    , mdDrainage ∷ Float
      -- ^ Hydraulic drainage 0.0–1.0. See @World.Material.MaterialProps@
      --   for semantics. Defaults to 0.4 (neutral) when omitted in YAML.
    , mdPickSpeed   ∷ Float
      -- ^ Dig-rate multiplier with a pick (1.0 = baseline). See
      --   @World.Material.MaterialProps@.
    , mdShovelSpeed ∷ Float
      -- ^ Dig-rate multiplier with a shovel.
    , mdDigSpoil ∷ Maybe Text
      -- ^ Spoil material name produced when digging this material
      --   (e.g. granite → "heavy_gravel"). Absent = no spoil.
    , mdDigBulking ∷ Float
      -- ^ Spoil volume per excavated volume (default 1.0; broken
      --   hard rock bulks above 1).
    , mdDigChunk ∷ Maybe Text
      -- ^ Item def spawned by the chunk-yield accumulator
      --   (granite → "granite_chunk"). Absent = no chunk yields.
    , mdDigGems ∷ Bool
      -- ^ Gem region field applies while digging this material
      --   (default False).
    , mdMoveCost ∷ Float
      -- ^ Surface-traversal cost multiplier for unit pathing (default
      --   1.0). See @World.Material.MaterialProps@ (mpMoveCost) / #312.
    , mdTile     ∷ Text
    , mdZoom     ∷ Text
    , mdBg       ∷ Text
    } deriving (Show, Eq, Generic)

instance FromJSON MaterialDef where
    parseJSON = withObject "MaterialDef" $ \v → MaterialDef
        ⊚ v .: "id"
        ⊛ v .: "name"
        ⊛ v .:? "hardness" .!= 0.5
        ⊛ v .:? "density"  .!= 2.5
        ⊛ v .:? "albedo"   .!= 0.5
        ⊛ v .:? "drainage" .!= 0.4
        ⊛ v .:? "pick_speed"   .!= 0.5
        ⊛ v .:? "shovel_speed" .!= 0.5
        ⊛ v .:? "dig_spoil"
        ⊛ v .:? "dig_bulking"  .!= 1.0
        ⊛ v .:? "dig_chunk"
        ⊛ v .:? "dig_gems"     .!= False
        ⊛ v .:? "move_cost"    .!= 1.0
        ⊛ v .: "tile"
        ⊛ v .: "zoom"
        ⊛ v .: "bg"

data MaterialFile = MaterialFile
    { mfMaterials ∷ [MaterialDef]
    } deriving (Show, Eq, Generic)

instance FromJSON MaterialFile where
    parseJSON = withObject "MaterialFile" $ \v → MaterialFile
        ⊚ v .: "materials"

-- * YAML parsing

loadMaterialYaml ∷ LoggerState → FilePath → IO [MaterialDef]
loadMaterialYaml logger =
    loadYamlList logger "material" "materials" mfMaterials

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

-- | 'loadMaterialDirectory' plus the fold into a real
--   'World.Material.MaterialRegistry' — the exact population pass
--   'World.Thread.Command.Init' runs at "Step 0.5" of every
--   @world.init@, extracted here (issue #763) so
--   'Engine.Scripting.Lua.API.Save' can call the SAME logic on a
--   whole-session LOAD: the registry is otherwise populated only by
--   @world.init@, so a fresh headless boot that goes straight to
--   @engine.loadSave@ with no prior @world.init@ in the SAME process
--   would see an entirely empty registry (every id but air reporting
--   as "unknown") when validating a save's material references.
--   Idempotent, like the population pass it mirrors — safe to call
--   even when a live world has already populated the registry.
loadPopulatedMaterialRegistry ∷ LoggerState → FilePath → IO MaterialRegistry
loadPopulatedMaterialRegistry logger dir = do
    matDefs ← loadMaterialDirectory logger dir
    pure $ foldl' (\r def →
        registerMaterial (mdId def)
            (MaterialProps (mdName def)
                           (mdHardness def)
                           (mdDensity def)
                           (mdAlbedo def)
                           (mdDrainage def)
                           (mdPickSpeed def)
                           (mdShovelSpeed def)
                           (mdDigSpoil def)
                           (mdDigBulking def)
                           (mdDigChunk def)
                           (mdDigGems def)
                           (mdMoveCost def))
            r
        ) emptyMaterialRegistry matDefs
