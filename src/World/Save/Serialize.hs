{-# LANGUAGE Strict #-}
module World.Save.Serialize
    ( encodeSessionSnapshot
    , writeSaveFiles
    , loadWorld
    , listSaves
    , saveListingOrder
    , SaveListing(..)
    , savesDirectory
    , saveExtension
    , sanitizeSaveName
    , loadPhaseFor
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
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
    , GenerationFailure(..), renderGenerationFailure
    , LoadProgress(..)
    , LuaComponentSpec(..) )
import World.Save.Component.Types (ComponentPhase(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import qualified World.Save.Storage as Storage
import Engine.Core.Log (LoggerState, LogCategory(..), logWarn)
import Engine.Load.Status (LoadPhase(..))

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

-- | The extension of a pre-#762 LEGACY FLAT save file
--   (@saves\/\<name\>.synworld@). Nothing writes this shape any more —
--   every save is published as a slot DIRECTORY — but 'listSaves' still
--   lists one and 'loadWorld' still loads one, so any code reasoning
--   about whether a save NAME is occupied has to consider both forms
--   (see "World.Save.Autosave").
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
--   transaction is internally exception-safe: the caller
--   ('World.Thread.Command.Save.WriteWorld')
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
writeSaveFiles
    ∷ Text → SaveMetadata → BS.ByteString → HS.HashSet Text → HS.HashSet Text
    → IO (Either Text [Text])
writeSaveFiles rawName meta encoded luaKnownNames luaRequiredNames =
    case sanitizeSaveName rawName of
    Left err   → return (Left ("Invalid save name: " <> err))
    Right name → do
        let saveDir = savesDirectory </> T.unpack name
        result ← Storage.publishGeneration saveDir name meta encoded
                    luaKnownNames luaRequiredNames
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
--
--   The failure case also names the
--   'LoadPhase' the attempt actually reached before failing, so
--   'engine.getLoadStatus()' can retain real progress instead of every
--   failure collapsing straight from 'LoadPaused' to 'LoadFailed'.
--   'Storage.selectLoadGeneration'/'decodeSessionEnvelope' still perform
--   envelope validation, component decode, migration, and snapshot
--   assembly as one atomic, unobservable-from-outside call (issues
--   #759-#762) — there is no LIVE checkpoint to report progress FROM
--   mid-flight — but a failure now CARRIES how far it got, as the
--   structured 'LoadProgress' 'Storage.lfProgress' transports out of the
--   decode layers (issue #1919); 'loadPhaseFor' below is the one place
--   that turns it into a 'LoadPhase'.
loadWorld
    ∷ LoggerState → Text → HS.HashSet Text → HS.HashSet Text
    → IO (Either (LoadPhase, Text)
                 (SaveData, [(Text, Word32, BS.ByteString)], Bool))
loadWorld logger rawName luaKnownNames luaRequiredNames =
    case sanitizeSaveName rawName of
    Left err   → return (Left (LoadPaused, "Invalid save name: " <> err))
    Right name → do
        let dirPath    = savesDirectory </> T.unpack name
            legacyPath = savesDirectory </> T.unpack name <> saveExtension
        dirExists ← doesDirectoryExist dirPath
        if dirExists
            then loadFromDirectory dirPath name
            else do
                legacyExists ← doesFileExist legacyPath
                if not legacyExists
                    then return (Left (LoadPaused, "Save not found: " <> name))
                    else do
                        -- Requirement 12: the SAME containment check the
                        -- directory-format path gets via
                        -- 'Storage.selectLoadGeneration' — a legacy flat
                        -- file, or 'savesDirectory' itself, being a
                        -- symlink must not be silently followed and read.
                        safety ← Storage.rejectSymlinkedSlotDir legacyPath
                        case safety of
                            Left reason → return (Left (LoadPaused, reason))
                            Right ()    → decodeLegacyFile legacyPath
  where
    -- Validate sdWorlds cardinality (and every other load-bearing check)
    -- at DECODE time so the load API fails cleanly (Left → engine.loadSave
    -- returns false) before it pauses the engine, restores Lua state, or
    -- marks the head world loading. Catching it only in the world-thread
    -- handler would wedge the session on the loading screen after those
    -- side effects.
    loadFromDirectory dirPath name = do
        selection ← Storage.selectLoadGeneration
            luaKnownNames luaRequiredNames dirPath name
        case selection of
            Left failure → return (Left ( loadPhaseFor (Storage.lfProgress failure)
                                        , Storage.lfMessage failure ))
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
                return (Right (Storage.lsSaveData sel, Storage.lsLuaComponents sel
                              , Storage.lsIsMigratedLegacyBaseline sel))

    -- Reconstruct the complete, cross-validated 'SessionSnapshot' from
    -- the component envelope (issue #760), then bridge it back into the
    -- transitional 'SaveData' shape the world-thread load path still
    -- consumes — using the AUTHORITATIVE metadata (name/timestamp) the
    -- metadata component carries, so a within-session re-load keys its
    -- provenance under the same save name as before. A legacy flat file
    -- goes through the UNclassified 'decodeSessionEnvelope', which
    -- produces no 'LoadProgress' at all, so this reports the
    -- conservative 'LoadPaused' — genuinely all that's known without
    -- deeper instrumentation of this rare, deprecated path, and exactly
    -- what it reported before issue #1919.
    --
    --   The read below is deliberately bare: a throwing 'BS.readFile'
    --   (permission error, a special file at the path, the file vanishing
    --   after the existence check) propagates as an 'IOException', and
    --   since #2162 the caller's 'guardAcceptedLoad' terminalizes the
    --   accepted transaction with it, so it can no longer strand the
    --   load status at 'LoadPaused'.
    decodeLegacyFile path = do
        bytes ← BS.readFile path
        let result = do
                (meta, snap, luaComponents, isMigrated) ←
                    decodeSessionEnvelope luaKnownNames luaRequiredNames bytes
                let req = SaveRequestMeta { srmSlotName  = smName meta
                                          , srmTimestamp = smTimestamp meta
                                          , srmAutosave  = smAutosave meta }
                sd ← checkWorldCount (snapshotToSaveData req snap)
                pure (sd, [ (lcsId c, lcsVersion c, lcsPayload c)
                          | c ← luaComponents ]
                     , isMigrated)
        return $ either (\err → Left (LoadPaused, err)) Right result

-- | Turn the structured 'LoadProgress' a failed load carries into the
--   'LoadPhase' @engine.getLoadStatus()@ reports as @failedAtPhase@
--   (issue #1919). This replaced a substring match over the failure's
--   RENDERED text, so a reword anywhere in the save stack can no longer
--   change the reported phase, and a real failure can no longer fall
--   through to 'LoadPaused' merely because no phase word happened to
--   appear.
--
--   The mapping is exactly the one the parser implemented, preserved
--   deliberately:
--
--     * 'ReachedNothing' — no coherent candidate was ever obtained
--       (missing / unreadable / symlink-rejected / storage-corrupt, and
--       the legacy flat-file path, which carries no progress) —
--       'LoadPaused'.
--     * 'ReachedEnvelope' — 'LoadEnvelopeValidated'.
--     * 'ReachedComponents' — the FURTHEST point every component
--       reached, by the parser's own precedence. 'AssemblePhase' and
--       'ValidatePhase' both bottom out at 'LoadComponentsMigrated':
--       there is no 'LoadPhase' constructor between "every component
--       migrated" and "the whole session assembled", and a per-component
--       validate failure or a cross-component assemble failure both mean
--       every component individually got at least that far. A capped
--       failure list mixing phases resolves to the furthest of them, as
--       the substring match did when several phase words appeared in one
--       message.
loadPhaseFor ∷ LoadProgress → LoadPhase
loadPhaseFor ReachedNothing  = LoadPaused
loadPhaseFor ReachedEnvelope = LoadEnvelopeValidated
loadPhaseFor (ReachedComponents phases)
    | any migrated phases        = LoadComponentsMigrated
    | MigratePhase `elem` phases = LoadComponentsDecoded
    -- 'DecodePhase' — and the structurally unreachable empty list, which
    -- still means the envelope itself was coherent — report the same
    -- checkpoint the envelope-level failures do.
    | otherwise                  = LoadEnvelopeValidated
  where
    migrated p = p ≡ AssemblePhase ∨ p ≡ ValidatePhase

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
--   marks this). A slot whose authoritative generation's ENVELOPE
--   structure or @"metadata"@ component is present but semantically
--   INCOMPATIBLE (unsupported version) is still dropped with a logged
--   reason, same as before #762 — requirement 7's "never fall back for
--   an incompatible generation" rule applies to listing too, to
--   whatever depth listing itself validates. Neither a previous
--   generation nor a temporary candidate file is ever listed as its own
--   slot.
--
--   Listing decodes ONLY the envelope structure/checksums and the
--   @"metadata"@ component (issue #759 requirement 4's deliberate,
--   pre-#762 design: @listSaves@\/@engine.listSaves()@ never decodes a
--   save's gameplay payload at all, so populating a save browser never
--   costs a full component-by-component decode). A slot can therefore
--   still be listed as normal even when its authoritative generation
--   would fail to actually LOAD — an incompatible/invalid problem
--   confined to a gameplay component OTHER than @"metadata"@ — exactly
--   as it already could before this issue; #762 does not change what
--   listing validates, only adds the previous-generation fallback path
--   ON TOP of that same pre-existing metadata-only depth. Fully
--   predicting loadability during listing would mean decoding every
--   component (buildings, units, world-pages, ...) for every listed
--   save — the per-save cost #759 explicitly designed listing to avoid.
listSaves ∷ LoggerState → HS.HashSet Text → IO [SaveListing]
listSaves logger luaKnownNames = do
    createDirectoryIfMissing True savesDirectory
    entries ← listDirectory savesDirectory
    results ← mapM tryEntry entries
    let oks = concat results
    pure $ saveListingOrder oks
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
        -- Requirement 12: the same containment check publishGeneration/
        -- selectLoadGeneration apply — a symlinked slot (or a symlinked
        -- saves/ itself) must never have its bytes read and reported on
        -- via listing either, even though listing itself never writes.
        safety ← Storage.rejectSymlinkedSlotDir dir
        case safety of
            Left reason → do
                logWarn logger CatWorld $
                    "listSaves: skipping " <> name <> ": " <> reason
                return []
            Right () → do
                let authPath = dir </> Storage.authoritativeFileName
                    prevPath = dir </> Storage.previousGenerationFileName
                -- requirement 12: the slot-directory check above says
                -- nothing about the GENERATION FILES inside it —
                -- 'publishGeneration' never leaves a symlink at either
                -- (an atomic rename replaces a destination symlink's own
                -- entry rather than writing through it), so finding one
                -- here can only come from outside the transaction. Same
                -- fallback-eligible treatment 'decodeGenerationFile'
                -- (the load-selection path) already gives this exact case.
                authLinkSafe ← Storage.rejectSymlinkedPath authPath
                case authLinkSafe of
                    Left reason → tryPreviousListing name prevPath reason
                    Right () → do
                        authExists ← doesFileExist authPath
                        if not authExists
                            then tryPreviousListing name prevPath
                                    "authoritative save file is missing"
                            else do
                                bytes ← BS.readFile authPath
                                -- Envelope-aware: skip files whose magic/
                                -- version/manifest/checksums don't
                                -- validate. A pre-#759 flat file fails
                                -- the envelope version check and is
                                -- skipped with a logged warning so the
                                -- user has a chance of noticing.
                                case decodeSaveEnvelopeMetadataClassified luaKnownNames bytes of
                                    Right meta → return [mkListing name meta False]
                                    Left (GenerationIncompatible _ err) → do
                                        logWarn logger CatWorld $
                                            "listSaves: skipping " <> name <> ": " <> err
                                        return []
                                    Left (GenerationCorrupt err) →
                                        tryPreviousListing name prevPath err

    tryPreviousListing name prevPath authErr = do
        prevLinkSafe ← Storage.rejectSymlinkedPath prevPath
        case prevLinkSafe of
            Left reason → do
                logWarn logger CatWorld $
                    "listSaves: skipping " <> name
                        <> ": authoritative generation unreadable ("
                        <> authErr <> ") and previous generation is also \
                           \unusable (" <> reason <> ")"
                return []
            Right () → tryPreviousListingUnsafe name prevPath authErr

    tryPreviousListingUnsafe name prevPath authErr = do
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
                case decodeSaveEnvelopeMetadataClassified luaKnownNames bytes of
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
        -- Requirement 12: a legacy flat file's OWN listing path never
        -- routed through 'loadDirEntry''s containment check — apply it
        -- here directly (also covers a symlinked 'savesDirectory' via
        -- 'path''s immediate parent).
        safety ← Storage.rejectSymlinkedSlotDir path
        case safety of
            Left reason → do
                logWarn logger CatWorld $
                    "listSaves: skipping " <> T.pack path <> ": " <> reason
                return []
            Right () → do
                bytes ← BS.readFile path
                case decodeSaveEnvelopeMetadata luaKnownNames bytes of
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

-- | The canonical order 'listSaves' publishes, and the ONLY ordering
--   any consumer of a save listing may apply: newest first by
--   'normalizeTimestamp'-canonicalized timestamp, ties broken by
--   ascending slot name.
--
--   The tiebreak is what makes the order a function of the saves
--   directory's CONTENT rather than of directory-enumeration order,
--   and tied timestamps are reachable: two legacy second-precision
--   saves both normalize to the same @…32.000000Z@, an unparseable
--   timestamp is passed through untouched so two identical malformed
--   strings tie as well, and the per-process monotonic clamp on new
--   save timestamps ('Engine.Scripting.Lua.API.Save') cannot separate
--   saves written by two different processes.
--
--   Exported so a consumer's ordering can be tested against this
--   comparator itself rather than against a re-derived copy of it
--   (#1932).
saveListingOrder ∷ [SaveListing] → [SaveListing]
saveListingOrder = sortBy (comparing (Down ∘ smTimestamp ∘ slMetadata)
                            <> comparing slName)

-- | Canonicalize a save timestamp to the fixed-width microsecond ISO
--   form (@%FT%T%6QZ@) used by 'saveListingOrder' — the one comparison
--   a listed save's timestamp is ever put through. It compares
--   timestamps as raw strings, so a legacy save written at second
--   precision (@…32Z@) would sort ahead of a newer fractional one
--   (@…32.5Z@) purely because @'Z' > '.'@. Parsing with @%Q@ (which
--   accepts an optional fraction) and reformatting puts every save —
--   legacy, millisecond, microsecond or picosecond — into one
--   lexicographically comparable shape. Anything that fails to parse
--   is left untouched (#98).
--
--   @main_menu.lua@ used to be a second consumer, re-sorting the
--   listing on this string alone. It no longer compares timestamps at
--   all: it holds 'saveListingOrder'\'s output verbatim (#1932), so
--   that function's name tiebreak is no longer something a downstream
--   consumer can discard.
normalizeTimestamp ∷ Text → Text
normalizeTimestamp ts =
    case parseTimeM True defaultTimeLocale "%FT%T%QZ" (T.unpack ts) of
        Just (t ∷ UTCTime) →
            T.pack $ formatTime defaultTimeLocale "%FT%T%6QZ" t
        Nothing → ts
