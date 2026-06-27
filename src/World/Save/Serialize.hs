{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Save.Serialize
    ( saveWorld
    , loadWorld
    , listSaves
    , savesDirectory
    , sanitizeSaveName
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.Char (isControl)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, formatTime, defaultTimeLocale)
import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, listDirectory
                        , doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, dropExtension)
import World.Save.Types
import World.Generate.Config (saveWorldGenYaml)
import Engine.Core.Log (LoggerState, LogCategory(..), logWarn)

-- | Validate a user-supplied save name before it touches the filesystem.
--   Returns 'Right' with the name unchanged when it's safe to use as a
--   single path component under @saves/@; returns 'Left' with a short
--   reason otherwise.
--
--   Closes:
--     - empty names (would resolve to the saves dir itself)
--     - traversal sequences (@..@)
--     - path separators (@/@, @\\@) — note that @System.FilePath.\</\>@
--       silently discards the left operand when the right is absolute,
--       so a name like @/etc/passwd@ would escape without this guard.
--     - control characters (would break filesystem operations or
--       produce surprising shell behaviour)
--     - leading @.@ (hidden files)
--     - over-long names (filesystem limits vary; 64 is a safe cap)
sanitizeSaveName ∷ Text → Either Text Text
sanitizeSaveName name
    | T.null name           = Left "Save name cannot be empty"
    | T.length name > 64    = Left "Save name too long (max 64 chars)"
    | ".." `T.isInfixOf` name
                            = Left "Save name cannot contain '..'"
    | T.any isPathSep name  = Left "Save name cannot contain '/' or '\\'"
    | T.any isControl name  = Left "Save name cannot contain control characters"
    | T.head name ≡ '.'     = Left "Save name cannot start with '.'"
    | otherwise             = Right name
  where
    isPathSep c = c ≡ '/' ∨ c ≡ '\\'

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
saveWorld rawName saveData = case sanitizeSaveName rawName of
    Left err   → return (Left ("Invalid save name: " <> err))
    Right name → do
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
loadWorld rawName = case sanitizeSaveName rawName of
    Left err   → return (Left ("Invalid save name: " <> err))
    Right name → do
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
--   Results are sorted newest-first by timestamp (ISO 8601
--   lexicographic descending), tiebreak by name ascending.
--   Corrupt / wrong-version saves are logged and dropped from the
--   list — the dev sees the reason; players still need #6's HUD
--   bridge before that message is visible in-game.
listSaves ∷ LoggerState → IO [(Text, SaveMetadata)]
listSaves logger = do
    createDirectoryIfMissing True savesDirectory
    entries ← listDirectory savesDirectory
    results ← mapM tryEntry entries
    let oks = concat results
    pure $ sortBy (comparing (Down . smTimestamp . snd) <> comparing fst) oks
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
        -- Legacy v1 saves (no header) hit "bad magic" and are skipped
        -- with a logged warning so the user has a chance of noticing.
        case S.runGet decodeListingMeta bytes of
            Left err → do
                logWarn logger CatWorld $
                    "listSaves: skipping " <> T.pack path
                    <> ": " <> T.pack err
                return []
            Right sd →
                let meta = sdMetadata sd
                    meta' = meta { smTimestamp =
                                     normalizeTimestamp (smTimestamp meta) }
                in return [(name, meta')]
    decodeListingMeta = do
        h ← S.get
        when (shMagic h ≠ saveMagic) $ fail "bad magic"
        when (shVersion h ≠ currentSaveVersion) $ fail "version mismatch"
        S.get

-- | Canonicalize a save timestamp to the fixed-width microsecond ISO
--   form (@%FT%T%6QZ@) used by the sort in 'listSaves' and by
--   @main_menu.lua@. Both consumers compare timestamps as raw strings,
--   so a legacy save written at second precision (@…32Z@) would sort
--   ahead of a newer fractional one (@…32.5Z@) purely because @'Z' >
--   '.'@. Parsing with @%Q@ (which accepts an optional fraction) and
--   reformatting puts every save — legacy, millisecond, microsecond or
--   picosecond — into one lexicographically comparable shape. Anything
--   that fails to parse is left untouched (#98).
normalizeTimestamp ∷ Text → Text
normalizeTimestamp ts =
    case parseTimeM True defaultTimeLocale "%FT%T%QZ" (T.unpack ts) of
        Just (t ∷ UTCTime) →
            T.pack $ formatTime defaultTimeLocale "%FT%T%6QZ" t
        Nothing → ts
