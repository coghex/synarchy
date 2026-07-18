{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Save.Serialize
    ( encodeSessionSnapshot
    , writeSaveFiles
    , loadWorld
    , listSaves
    , SaveListing(..)
    , savesDirectory
    , sanitizeSaveName
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Char (isControl)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, listDirectory
                        , doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, dropExtension)
import World.Save.Types (SaveData, SaveMetadata(..), checkWorldCount)
import World.Save.Envelope
    ( encodeSessionSnapshot, decodeSessionEnvelope
    , decodeSaveEnvelopeMetadata, decodeSaveEnvelopeMetadataClassified
    , GenerationFailure(..), renderGenerationFailure )
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import qualified World.Save.Storage as Storage
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

-- | Publish already-encoded envelope bytes (see 'encodeSessionSnapshot')
--   as a new authoritative generation for @saves/{name}/@ (issue #762,
--   persistence-overhaul C1). Delegates the actual write-validate-
--   publish-rotate transaction to 'World.Save.Storage.publishGeneration'
--   — see that module's haddock for the full durability contract; this
--   wrapper only owns save-NAME sanitization and slot-directory-path
--   policy (requirement 12: every candidate/authoritative/previous-
--   generation path stays under 'savesDirectory').
--
--   'World.Save.Storage' never overwrites the existing authoritative
--   @world.synworld@ in place — a crash, power loss, disk-full
--   condition, or write failure can no longer corrupt the ONLY copy of a
--   generation the way the old direct 'BS.writeFile' could. Every pure
--   computation already ran to produce @encoded@ — the only work left
--   here is genuine, unpredictable-until-attempted I/O, which is why
--   this is safe to run AFTER the #757 barrier releases (#758): a
--   failure here is a real write failure, not a capture bug. The whole
--   transaction is internally exception-safe (review round 2 follow-up
--   still applies): the caller ('World.Thread.Command.Save.WriteWorld')
--   runs on the world thread AFTER the barrier's capture lock has
--   already released, so an uncaught exception here would escape all the
--   way to "World.Thread"'s top-level crash handler instead of reaching
--   'failSave' — crashing the whole world thread AND leaving the save
--   barrier stuck open (non-terminal) forever, permanently refusing
--   every subsequent save. On success, the returned list carries any
--   NON-FATAL cleanup warnings ("World.Save.Storage" requirement 10's
--   "cleanup failure" case — the durability boundary has already
--   completed by the time cleanup runs, so this never turns into an
--   overall save failure).
writeSaveFiles ∷ Text → SaveMetadata → BS.ByteString → IO (Either Text [Text])
writeSaveFiles rawName meta encoded = case sanitizeSaveName rawName of
    Left err   → return (Left ("Invalid save name: " <> err))
    Right name → do
        let saveDir = savesDirectory </> T.unpack name
        result ← Storage.publishGeneration saveDir name meta encoded
        case result of
            Right warnings → return (Right warnings)
            Left failure   → return (Left (Storage.renderPublishFailure failure))

-- | Load a world from disk.
--   Tries directory format first (saves/{name}/, selecting between its
--   authoritative and previous generation via
--   'World.Save.Storage.selectLoadGeneration' — issue #762), falls back
--   to a legacy flat file (saves/{name}.synworld) when no slot directory
--   exists at all. Rejects a pre-#759 flat file and any incompatible
--   envelope with a clear error — the user must start fresh after a
--   schema bump.
--
--   A legacy flat file has no slot directory, so none of C1's
--   generation/recovery machinery applies to it: it either decodes
--   cleanly or is rejected outright, exactly as before #762.
loadWorld ∷ LoggerState → Text → IO (Either Text SaveData)
loadWorld logger rawName = case sanitizeSaveName rawName of
    Left err   → return (Left ("Invalid save name: " <> err))
    Right name → do
        let dirPath    = savesDirectory </> T.unpack name
            legacyPath = savesDirectory </> T.unpack name <> saveExtension
        dirExists ← doesDirectoryExist dirPath
        if dirExists
            then loadFromDirectory dirPath name
            else do
                legacyExists ← doesFileExist legacyPath
                if not legacyExists
                    then return (Left $ "Save not found: " <> name)
                    else decodeLegacyFile legacyPath
  where
    -- Validate sdWorlds cardinality (and every other load-bearing check)
    -- at DECODE time so the load API fails cleanly (Left → engine.loadSave
    -- returns false) before it pauses the engine, restores Lua blobs, or
    -- marks the head world loading. Catching it only in the world-thread
    -- handler would wedge the session on the loading screen after those
    -- side effects.
    loadFromDirectory dirPath name = do
        selection ← Storage.selectLoadGeneration dirPath name
        case selection of
            Left err  → return (Left err)
            Right sel → do
                -- Requirement 7: report whether the authoritative or the
                -- previous generation was selected, and why. A recovered
                -- load is loud (logWarn) — it means the authoritative
                -- generation was storage-corrupt, worth a dev's attention
                -- even though the session itself loaded successfully.
                case Storage.lsSource sel of
                    Storage.FromAuthoritative → pure ()
                    Storage.FromPrevious →
                        logWarn logger CatWorld $
                            "loadWorld: '" <> name <> "': "
                                <> Storage.lsDetail sel
                return (Right (Storage.lsSaveData sel))

    -- Reconstruct the complete, cross-validated 'SessionSnapshot' from
    -- the component envelope (issue #760), then bridge it back into the
    -- transitional 'SaveData' shape the world-thread load path still
    -- consumes — using the AUTHORITATIVE metadata (name/timestamp) the
    -- metadata component carries, so a within-session re-load keys its
    -- provenance under the same save name as before.
    decodeLegacyFile path = do
        bytes ← BS.readFile path
        return $ do
            (meta, snap) ← decodeSessionEnvelope bytes
            let req = SaveRequestMeta { srmSlotName  = smName meta
                                      , srmTimestamp = smTimestamp meta }
            checkWorldCount (snapshotToSaveData req snap)

-- | One entry in 'listSaves''s result. 'slRecovered' is 'True' when the
--   listed metadata came from a slot's PREVIOUS generation because its
--   authoritative generation had recoverable storage corruption (issue
--   #762 requirement 8) — a machine-readable recovery status; no
--   save/load UI change is required to consume it.
data SaveListing = SaveListing
    { slName      ∷ !Text
    , slMetadata  ∷ !SaveMetadata
    , slRecovered ∷ !Bool
    } deriving (Show, Eq)

-- | List available saves (returns metadata only).
--   Checks both directory-based saves and legacy flat files.
--   Results are sorted newest-first by timestamp (ISO 8601
--   lexicographic descending), tiebreak by name ascending.
--   Corrupt / wrong-version saves are logged and dropped from the
--   list — the dev sees the reason; players still need #6's HUD
--   bridge before that message is visible in-game.
--
--   A slot whose authoritative generation is missing/truncated/checksum-
--   corrupt (recoverable storage corruption) lists from its valid
--   previous generation instead of disappearing outright ('slRecovered'
--   marks this). A slot whose authoritative generation is present but
--   semantically INCOMPATIBLE (unsupported schema) is still dropped with
--   a logged reason, same as before #762 — requirement 7's "never fall
--   back for an incompatible generation" rule applies to listing too.
--   Neither a previous generation nor a temporary candidate file is ever
--   listed as its own slot.
listSaves ∷ LoggerState → IO [SaveListing]
listSaves logger = do
    createDirectoryIfMissing True savesDirectory
    entries ← listDirectory savesDirectory
    results ← mapM tryEntry entries
    let oks = concat results
    pure $ sortBy (comparing (Down . smTimestamp . slMetadata)
                    <> comparing slName) oks
  where
    tryEntry entry = do
        let fullPath = savesDirectory </> entry
        -- Check if it's a directory (new format)
        isDir ← doesDirectoryExist fullPath
        if isDir
            then loadDirEntry (T.pack entry) fullPath
            -- Check if it's a legacy .synworld file
            else if takeExtension entry ≡ saveExtension
                then loadLegacyEntry (T.pack (dropExtension entry)) fullPath
                else return []

    loadDirEntry name dir = do
        let authPath = dir </> Storage.authoritativeFileName
            prevPath = dir </> Storage.previousGenerationFileName
        authExists ← doesFileExist authPath
        if not authExists
            then tryPreviousListing name prevPath
                    "authoritative save file is missing"
            else do
                bytes ← BS.readFile authPath
                -- Envelope-aware: skip files whose magic/version/manifest/
                -- checksums don't validate. A pre-#759 flat file fails the
                -- envelope version check and is skipped with a logged
                -- warning so the user has a chance of noticing.
                case decodeSaveEnvelopeMetadataClassified bytes of
                    Right meta → return [mkListing name meta False]
                    Left (GenerationIncompatible err) → do
                        logWarn logger CatWorld $
                            "listSaves: skipping " <> name <> ": " <> err
                        return []
                    Left (GenerationCorrupt err) →
                        tryPreviousListing name prevPath err

    tryPreviousListing name prevPath authErr = do
        prevExists ← doesFileExist prevPath
        if not prevExists
            then do
                logWarn logger CatWorld $
                    "listSaves: skipping " <> name
                        <> ": authoritative generation unreadable ("
                        <> authErr <> ") and no previous generation exists"
                return []
            else do
                bytes ← BS.readFile prevPath
                case decodeSaveEnvelopeMetadataClassified bytes of
                    Right meta → do
                        logWarn logger CatWorld $
                            "listSaves: '" <> name
                                <> "': authoritative generation unreadable ("
                                <> authErr
                                <> "), listing from previous generation"
                        return [mkListing name meta True]
                    Left prevErr → do
                        logWarn logger CatWorld $
                            "listSaves: skipping " <> name
                                <> ": authoritative generation unreadable ("
                                <> authErr
                                <> ") and previous generation is also \
                                   \unusable ("
                                <> renderGenerationFailure prevErr <> ")"
                        return []

    loadLegacyEntry name path = do
        bytes ← BS.readFile path
        case decodeSaveEnvelopeMetadata bytes of
            Left err → do
                logWarn logger CatWorld $
                    "listSaves: skipping " <> T.pack path <> ": " <> err
                return []
            Right meta → return [mkListing name meta False]

    mkListing name meta recovered = SaveListing
        { slName      = name
        , slMetadata  = meta { smTimestamp = normalizeTimestamp (smTimestamp meta) }
        , slRecovered = recovered
        }

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
