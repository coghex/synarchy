{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Save.Serialize
    ( encodeSessionSnapshot
    , writeSaveFiles
    , loadWorld
    , listSaves
    , savesDirectory
    , sanitizeSaveName
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Control.Exception (SomeException, try)
import Data.Char (isControl)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, listDirectory
                        , doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, dropExtension)
import World.Save.Types (SaveData(..), SaveMetadata(..))
import World.Save.Envelope
    (encodeSessionSnapshot, decodeSessionEnvelope, decodeSaveEnvelopeMetadata)
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
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

-- | Write already-encoded envelope bytes (see 'encodeSessionSnapshot') to
--   disk. Creates saves/{name}/world.synworld — the SOLE authoritative
--   file for a save generation (issue #759 requirement 5): there is no
--   longer a @world_gen.yaml@ companion; generation params live in the
--   envelope's "session" component like every other gameplay field.
--
--   Every pure computation already ran to produce @encoded@ — the only
--   work left here is genuine, unpredictable-until-attempted I/O
--   (directory creation, disk space, permissions), which is why this
--   is safe to run AFTER the #757 barrier releases (#758): a failure
--   here is a real write failure, not a capture bug. That I/O is
--   wrapped in 'try' (review round 2 follow-up): the caller
--   ('World.Thread.Command.Save.WriteWorld') runs on the world thread
--   AFTER the barrier's capture lock has already released, so an
--   uncaught exception here would escape all the way to
--   "World.Thread"'s top-level crash handler instead of reaching
--   'failSave' — crashing the whole world thread AND leaving the save
--   barrier stuck open (non-terminal) forever, permanently refusing
--   every subsequent save.
writeSaveFiles ∷ Text → BS.ByteString → IO (Either Text ())
writeSaveFiles rawName encoded = case sanitizeSaveName rawName of
    Left err   → return (Left ("Invalid save name: " <> err))
    Right name → do
        outcome ← try (writeSaveFilesUnsafe name encoded)
        case outcome of
            Right () → return (Right ())
            Left (e ∷ SomeException) →
                return (Left ("Failed to write save to disk: " <> T.pack (show e)))

writeSaveFilesUnsafe ∷ Text → BS.ByteString → IO ()
writeSaveFilesUnsafe name encoded = do
    let saveDir = savesDirectory </> T.unpack name
    createDirectoryIfMissing True saveDir
    let binaryPath = saveDir </> binaryFileName
    BS.writeFile binaryPath encoded

-- | Load a world from disk.
--   Tries directory format first (saves/{name}/world.synworld),
--   falls back to legacy flat file (saves/{name}.synworld).
--   Rejects a pre-#759 flat file and any incompatible envelope with a
--   clear error — the user must start fresh after a schema bump.
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
    -- Validate sdWorlds cardinality at DECODE time so the load API
    -- fails cleanly (Left → engine.loadSave returns false) before it
    -- pauses the engine, restores Lua blobs, or marks the head world
    -- loading. Catching it only in the world-thread handler would
    -- wedge the session on the loading screen after those side effects.
    -- Reconstruct the complete, cross-validated 'SessionSnapshot' from
    -- the component envelope (issue #760), then bridge it back into the
    -- transitional 'SaveData' shape the world-thread load path still
    -- consumes — using the AUTHORITATIVE metadata (name/timestamp) the
    -- metadata component carries, so a within-session re-load keys its
    -- provenance under the same save name as before.
    decodeFile path = do
        bytes ← BS.readFile path
        return $ do
            (meta, snap) ← decodeSessionEnvelope bytes
            let req = SaveRequestMeta { srmSlotName  = smName meta
                                      , srmTimestamp = smTimestamp meta }
            checkWorldCount (snapshotToSaveData req snap)

-- | Reject a decoded save with NO world pages (corrupt / truncated). Any
--   non-empty 'sdWorlds' is accepted: the save command snapshots every live
--   page (#216) and the load handler restores all of them (#217/#218), so a
--   multi-page save is fully supported. Only the empty case is rejected, so
--   the rest of the loader can assume at least one page.
--
--   Applied only by 'loadWorld', not by 'listSaves': listing now decodes
--   just the "metadata" component (issue #759 requirement 4), which
--   never carries 'sdWorlds' at all. A save with zero world pages could
--   therefore still appear in a listing, but attempting to load it
--   still fails cleanly here — the far more common corruption case
--   (a truncated/damaged file) is already caught earlier, during the
--   envelope's own structural + checksum validation, which listing
--   performs regardless of which component it goes on to interpret.
checkWorldCount ∷ SaveData → Either Text SaveData
checkWorldCount sd = case sdWorlds sd of
    [] → Left "Save contains no world pages (corrupt or truncated file)"
    _  → Right sd

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
        -- Envelope-aware: skip files whose magic/version/manifest/
        -- checksums don't validate. A pre-#759 flat file fails the
        -- envelope version check and is skipped with a logged warning
        -- so the user has a chance of noticing.
        case decodeSaveEnvelopeMetadata bytes of
            Left err → do
                logWarn logger CatWorld $
                    "listSaves: skipping " <> T.pack path <> ": " <> err
                return []
            Right meta →
                let meta' = meta { smTimestamp =
                                     normalizeTimestamp (smTimestamp meta) }
                in return [(name, meta')]

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
