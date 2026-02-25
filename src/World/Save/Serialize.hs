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
import System.Directory (createDirectoryIfMissing, listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension, dropExtension)
import World.Save.Types

savesDirectory ∷ FilePath
savesDirectory = "saves"

saveExtension ∷ String
saveExtension = ".synworld"

-- | Save a world to disk
saveWorld ∷ Text → SaveData → IO (Either Text ())
saveWorld name saveData = do
    createDirectoryIfMissing True savesDirectory
    let path = savesDirectory </> T.unpack name <> saveExtension
    case S.encode saveData of
        encoded → do
            BS.writeFile path encoded
            return (Right ())

-- | Load a world from disk
loadWorld ∷ Text → IO (Either Text SaveData)
loadWorld name = do
    let path = savesDirectory </> T.unpack name <> saveExtension
    exists ← doesFileExist path
    if not exists
        then return (Left $ "Save file not found: " <> T.pack path)
        else do
            bytes ← BS.readFile path
            case S.decode bytes of
                Left err → return (Left $ "Failed to decode save: " <> T.pack err)
                Right sd → return (Right sd)

-- | List available saves (returns metadata only)
listSaves ∷ IO [(Text, SaveMetadata)]
listSaves = do
    createDirectoryIfMissing True savesDirectory
    files ← listDirectory savesDirectory
    let saveFiles = filter (\f → takeExtension f ≡ saveExtension) files
    results ← mapM loadMetadata saveFiles
    return $ concat results
  where
    loadMetadata path = do
        let fullPath = savesDirectory </> path
            name = T.pack (dropExtension path)
        bytes ← BS.readFile fullPath
        case S.decode bytes of
            Left _   → return []
            Right sd → return [(name, sdMetadata sd)]
