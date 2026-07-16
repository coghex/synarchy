{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Save.Serialize
    ( encodeSaveData
    , writeSaveFiles
    , loadWorld
    , listSaves
    , savesDirectory
    , sanitizeSaveName
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
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

-- | Serialize a 'SaveData' into on-disk bytes (header + body). Pure —
--   but producing the final 'BS.ByteString' requires 'Data.Serialize'
--   to actually visit and encode every field, so forcing this value to
--   WHNF (a strict 'ByteString' cannot exist half-built) fully forces
--   the ENTIRE 'SaveData', including anything left as an unevaluated
--   thunk by 'World.Save.Snapshot'/'World.Save.Snapshot.Adapter''s
--   partial (WHNF-only) strictness.
--
--   #758 requirement 7 ("capture must be complete before the #757
--   barrier releases") depends on this being forced — via
--   'Control.Exception.evaluate' — BEFORE
--   'Engine.Save.Barrier.releaseCaptureLock' runs, so a bug that only
--   manifests when some deeply-nested captured value is finally
--   touched is caught as a capture failure while the barrier still
--   blocks other owners, not discovered later during 'writeSaveFiles'
--   after they have already resumed. See
--   'World.Thread.Command.Save.WriteWorld'.
encodeSaveData ∷ SaveData → BS.ByteString
encodeSaveData saveData =
    let header = SaveHeader { shMagic = saveMagic, shVersion = currentSaveVersion }
    in S.encode header <> S.encode saveData

-- | Write already-encoded save bytes (see 'encodeSaveData') plus the
--   human-readable companion YAML to disk.
--
--   Creates saves/{name}/ directory with:
--     - world.synworld  (binary save data, header + body)
--     - world_gen.yaml  (human-readable generation params)
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
writeSaveFiles ∷ Text → BS.ByteString → SaveData → IO (Either Text ())
writeSaveFiles rawName encoded saveData = case sanitizeSaveName rawName of
    Left err   → return (Left ("Invalid save name: " <> err))
    Right name → do
        outcome ← try (writeSaveFilesUnsafe name encoded saveData)
        case outcome of
            Right () → return (Right ())
            Left (e ∷ SomeException) →
                return (Left ("Failed to write save to disk: " <> T.pack (show e)))

writeSaveFilesUnsafe ∷ Text → BS.ByteString → SaveData → IO ()
writeSaveFilesUnsafe name encoded saveData = do
    let saveDir = savesDirectory </> T.unpack name
    createDirectoryIfMissing True saveDir
    let binaryPath = saveDir </> binaryFileName
        yamlPath   = saveDir </> yamlFileName
    BS.writeFile binaryPath encoded
    -- The human-readable companion describes the primary (active)
    -- world; per-world gen params now live in sdWorlds (#215). A
    -- well-formed save always has an active page, but tolerate an
    -- empty one by skipping the yaml rather than crashing the save.
    case activeWorldPage saveData of
        Just wps → saveWorldGenYaml yamlPath (wpsGenParams wps)
        Nothing  → return ()

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
        -- Validate sdWorlds cardinality at DECODE time so the load API
        -- fails cleanly (Left → engine.loadSave returns false) before it
        -- pauses the engine, restores Lua blobs, or marks the head world
        -- loading. Catching it only in the world-thread handler would
        -- wedge the session on the loading screen after those side effects.
        sd ← S.get
        checkWorldCount sd

mapLeft ∷ Either a b → (a → c) → Either c b
mapLeft (Left a)  f = Left (f a)
mapLeft (Right b) _ = Right b

-- | Reject a decoded save with NO world pages (corrupt / truncated). Any
--   non-empty 'sdWorlds' is accepted: the save command snapshots every live
--   page (#216) and the load handler restores all of them (#217/#218), so a
--   multi-page save is fully supported. Only the empty case is rejected, so
--   the rest of the loader can assume at least one page.
--
--   Shared by the load decoder ('decodeVersioned') and the listing decoder
--   ('decodeListingMeta') so 'listSaves' never advertises a save that
--   'loadWorld' would refuse.
checkWorldCount ∷ SaveData → S.Get SaveData
checkWorldCount sd = case sdWorlds sd of
    [] → fail "Save contains no world pages (corrupt or truncated file)"
    _  → pure sd

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
        -- Same cardinality gate as loadWorld so listSaves never lists a
        -- save engine.loadSave would refuse (only the empty/corrupt case).
        sd ← S.get
        checkWorldCount sd

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
