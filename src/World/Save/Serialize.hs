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
import Control.Monad (when)
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
--     - world.synworld  (binary save data, header + body)
--     - world_gen.yaml  (human-readable generation params)
--
--   File layout: [SaveHeader (magic + version)][SaveData].
--   The header is decoded first on load so we can reject incompatible
--   files before attempting to parse the body — which would fail with
--   a cryptic cereal error if the schema diverges.
saveWorld ∷ Text → SaveData → IO (Either Text ())
saveWorld name saveData = do
    let saveDir = savesDirectory </> T.unpack name
    createDirectoryIfMissing True saveDir
    let binaryPath = saveDir </> binaryFileName
        yamlPath   = saveDir </> yamlFileName
        header     = SaveHeader { shMagic   = saveMagic
                                , shVersion = currentSaveVersion
                                }
        encoded    = S.encode header <> S.encode saveData
    BS.writeFile binaryPath encoded
    saveWorldGenYaml yamlPath (sdGenParams saveData)
    return (Right ())

-- | Load a world from disk.
--   Tries directory format first (saves/{name}/world.synworld),
--   falls back to legacy flat file (saves/{name}.synworld).
--   Rejects pre-v2 saves (no header) and any version mismatch with
--   a clear error — the user must start fresh after a schema bump.
loadWorld ∷ Text → IO (Either Text SaveData)
loadWorld name = do
    let dirPath    = savesDirectory </> T.unpack name
        newPath    = dirPath </> binaryFileName
        legacyPath = savesDirectory </> T.unpack name <> saveExtension
    newExists ← doesFileExist newPath
    if newExists
        then decodeFile newPath
        else do
            legacyExists ← doesFileExist legacyPath
            if not legacyExists
                then return (Left $ "Save not found: " <> name)
                else decodeFile legacyPath
  where
    decodeFile path = do
        bytes ← BS.readFile path
        return $ S.runGet decodeVersioned bytes
            `mapLeft` (\err → "Failed to decode save: " <> T.pack err)
    -- Header first, validate, then body. fail messages bubble up
    -- through cereal's runGet as Left.
    decodeVersioned = do
        h ← S.get
        when (shMagic h ≠ saveMagic) $
            fail "Save file invalid (bad magic — not a synarchy save?)"
        when (shVersion h ≠ currentSaveVersion) $
            fail $ "Save format incompatible: expected v"
                <> show currentSaveVersion
                <> ", got v" <> show (shVersion h)
        S.get

mapLeft ∷ Either a b → (a → c) → Either c b
mapLeft (Left a)  f = Left (f a)
mapLeft (Right b) _ = Right b

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
        -- Header-aware: skip files whose magic/version doesn't match.
        -- Legacy v1 saves (no header) silently disappear from listings.
        case S.runGet decodeListingMeta bytes of
            Left _   → return []
            Right sd → return [(name, sdMetadata sd)]
    decodeListingMeta = do
        h ← S.get
        when (shMagic h ≠ saveMagic) $ fail "bad magic"
        when (shVersion h ≠ currentSaveVersion) $ fail "version mismatch"
        S.get
