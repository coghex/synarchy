{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Save.Serialize
    ( saveWorld
    , loadWorld
    , listSaves
    , savesDirectory
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, listDirectory
                        , doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, dropExtension)
import World.Save.Types
import World.Generate.Config (saveWorldGenYaml)

savesDirectory ∷ FilePath
savesDirectory = "saves"

saveExtension ∷ String
saveExtension = ".synworld"

binaryFileName ∷ String
binaryFileName = "world" <> saveExtension

yamlFileName ∷ String
yamlFileName = "world_gen.yaml"

-- | Save a world to disk.
--   Creates saves/{name}/ directory with:
--     - world.synworld  (binary save data)
--     - world_gen.yaml  (human-readable generation params)
saveWorld ∷ Text → SaveData → IO (Either Text ())
saveWorld name saveData = do
    let saveDir = savesDirectory </> T.unpack name
    createDirectoryIfMissing True saveDir
    let binaryPath = saveDir </> binaryFileName
        yamlPath   = saveDir </> yamlFileName
    case S.encode saveData of
        encoded → do
            BS.writeFile binaryPath encoded
            saveWorldGenYaml yamlPath (sdGenParams saveData)
            return (Right ())

-- | Load a world from disk.
--   Tries directory format first (saves/{name}/world.synworld),
--   falls back to legacy flat file (saves/{name}.synworld).
loadWorld ∷ Text → IO (Either Text SaveData)
loadWorld name = do
    let dirPath    = savesDirectory </> T.unpack name
        newPath    = dirPath </> binaryFileName
        legacyPath = savesDirectory </> T.unpack name <> saveExtension
    -- Try new directory format first
    newExists ← doesFileExist newPath
    if newExists
        then decodeFile newPath
        else do
            -- Fall back to legacy flat file
            legacyExists ← doesFileExist legacyPath
            if not legacyExists
                then return (Left $ "Save not found: " <> name)
                else decodeFile legacyPath
  where
    decodeFile path = do
        bytes ← BS.readFile path
        case S.decode bytes of
            Left err → return (Left $ "Failed to decode save: " <> T.pack err)
            Right sd → return (Right sd)

-- | List available saves (returns metadata only).
--   Checks both directory-based saves and legacy flat files.
listSaves ∷ IO [(Text, SaveMetadata)]
listSaves = do
    createDirectoryIfMissing True savesDirectory
    entries ← listDirectory savesDirectory
    results ← mapM tryEntry entries
    return $ concat results
  where
    tryEntry entry = do
        let fullPath = savesDirectory </> entry
        -- Check if it's a directory (new format)
        isDir ← doesDirectoryExist fullPath
        if isDir
            then do
                let binaryPath = fullPath </> binaryFileName
                exists ← doesFileExist binaryPath
                if exists
                    then loadMetadata (T.pack entry) binaryPath
                    else return []
            -- Check if it's a legacy .synworld file
            else if takeExtension entry ≡ saveExtension
                then loadMetadata (T.pack (dropExtension entry)) fullPath
                else return []
    loadMetadata name path = do
        bytes ← BS.readFile path
        case S.decode bytes of
            Left _   → return []
            Right sd → return [(name, sdMetadata sd)]
