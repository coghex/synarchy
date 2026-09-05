{-# LANGUAGE Strict #-}
module World.Save.Serialize
    ( encodeSessionSnapshot
    , writeSaveFiles
    , loadWorld
    , listSaves
    , listSavesWithSeams
    , ListingSeams(..)
    , productionListingSeams
    , saveListingOrder
    , SaveListing(..)
    , savesDirectory
    , saveExtension
    , saveSlotPath
    , legacySavePath
    , sanitizeSaveName
    , SaveRequestKind(..)
    , checkSaveName
    , loadPhaseFor
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Control.Exception (IOException, evaluate, try)
import Data.Char (isControl)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, listDirectory
                        , doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, dropExtension
                      , addTrailingPathSeparator)
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
--   about whether a save NAME is occupied has to consider both forms:
--   @World.Save.Autosave.ownershipProblem@ for an autosave cycle,
--   'checkSaveName' below for a manual save, and 'listSaves' when it
--   decides which of the two forms a name's single row describes.
saveExtension ∷ String
saveExtension = ".synworld"

-- | The MODERN slot-directory path (@saves\/\<name\>\/@) a sanitized
--   save NAME resolves to — the form 'writeSaveFiles' publishes, and
--   the form 'loadWorld' selects whenever it exists AT ALL.
saveSlotPath ∷ Text → FilePath
saveSlotPath name = savesDirectory </> T.unpack name

-- | The pre-#762 LEGACY FLAT file path (@saves\/\<name\>.synworld@) the
--   same save NAME resolves to — the form 'loadWorld' falls back to only
--   when 'saveSlotPath' does not exist.
legacySavePath ∷ Text → FilePath
legacySavePath name = savesDirectory </> T.unpack name <> saveExtension

-- | Which kind of request is asking to publish under a save name.
--   Only 'checkSaveName''s legacy-occupancy rule distinguishes them
--   (issue #2335): an autosave cycle is refused the same collision one
--   step earlier, by "World.Save.Autosave"'s own ownership check, so
--   applying it twice would only replace that path's specific reason
--   with a less useful one.
--
--   Deliberately NOT 'World.Save.Types.AutosaveRequest': that record is
--   captured at request ACCEPTANCE, after the barrier has opened, and no
--   preflight check has one yet.
data SaveRequestKind
    = ManualSave
      -- ^ A player-initiated @engine.saveWorld@ request.
    | ScheduledAutosave
      -- ^ The interval autosave scheduler's request (#913).
    deriving (Show, Eq)

-- | The complete save-NAME admission check @engine.saveWorld@ applies
--   BEFORE it opens a save transaction: 'sanitizeSaveName' first, then —
--   for a 'ManualSave' only — the legacy-flat-file occupancy rule
--   (issue #2335). 'Right' carries the name it is safe to publish under.
--
--   A save NAME has two physical forms, and only one of them is
--   writable: 'writeSaveFiles' publishes the slot DIRECTORY, while
--   'loadWorld' prefers a directory over a flat namesake. Publishing a
--   directory over an occupied name therefore leaves the player's legacy
--   generation on disk but unreachable BY NAME — the exact shadowing
--   @World.Save.Autosave.ownershipProblem@ has refused since #913, for
--   the same reason. Refusing here rather than at 'writeSaveFiles' is
--   what makes it a refusal rather than a failed save: no barrier is
--   opened, no snapshot is captured, and nothing on disk is touched.
--
--   Occupancy is the PRESENCE of the regular flat file and nothing more.
--   Its bytes are never read, let alone decoded, so a corrupt,
--   incompatible or pre-envelope legacy file shadows a name exactly as a
--   loadable one does — and is preserved exactly as one is. Autosave
--   already draws the line in that same place ('doesFileExist', no
--   decode).
checkSaveName ∷ SaveRequestKind → Text → IO (Either Text Text)
checkSaveName kind rawName =
    case sanitizeSaveName rawName of
    Left err   → pure (Left err)
    Right name → case kind of
        ScheduledAutosave → pure (Right name)
        ManualSave        → maybe (Right name) Left ⊚ legacyOccupancy name

-- | Why a manual save must not publish under this (already sanitized)
--   name, if it must not. See 'checkSaveName'.
legacyOccupancy ∷ Text → IO (Maybe Text)
legacyOccupancy name = do
    let flat = legacySavePath name
    occupied ← doesFileExist flat
    pure $ if not occupied then Nothing else Just $
        "a legacy flat save file '" <> T.pack flat <> "' already occupies \
        \that save name -- saving there would shadow it (a slot directory \
        \is loaded in preference to a flat file, so it could no longer be \
        \loaded by name). Rename or remove it to save under that name."

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
        let dirPath    = saveSlotPath name
            legacyPath = legacySavePath name
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

-- | The two I\/O operations 'listSaves' reaches the filesystem through,
--   bundled so a test can replace one of them (issue #2333).
--
--   Both are expected to signal failure the way the production
--   operations they stand in for do — by throwing an 'IOException' —
--   and 'listSavesWithSeams' contains each one at a different
--   granularity: a generation read is blamed on its own slot, an
--   enumeration failure on the whole survey.
data ListingSeams = ListingSeams
    { lsReadGeneration ∷ FilePath → IO BS.ByteString
      -- ^ read one generation file (authoritative, previous, or legacy
      --   flat). Production: 'BS.readFile'.
    , lsEnumerateSaves ∷ FilePath → IO [FilePath]
      -- ^ enumerate the saves root. Production: 'listDirectory'.
    }

-- | What 'listSaves' itself uses.
productionListingSeams ∷ ListingSeams
productionListingSeams = ListingSeams
    { lsReadGeneration = BS.readFile
    , lsEnumerateSaves = listDirectory
    }

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
--   == Containment (issue #2333)
--
--   Listing is a best-effort, read-only survey of a directory the engine
--   does not own exclusively, so a failure is CONTAINED to the smallest
--   thing it can be blamed on:
--
--   * A generation file that cannot be READ at all — a permission
--     failure, a special file, a file removed between the existence
--     check and the read — is classified exactly as the LOAD path
--     classifies it (@World.Save.Storage.decodeGenerationFile@ returns
--     @GenerationCorrupt \"cannot read: …\"@): recoverable, so the
--     previous generation is tried and the slot lists as 'slRecovered'
--     when that succeeds. Only when neither generation yields metadata
--     is the slot skipped, with one warning naming the failed path and
--     the underlying error. Every OTHER slot is still listed.
--   * A failure to enumerate @saves\/@ ITSELF is a different kind of
--     failure — nothing was surveyed, so \"no saves\" would be a lie —
--     and is reported as 'Left' rather than as an empty listing. The
--     distinction is load-bearing at @World.Save.Autosave.readSlotStates@,
--     which must never read an unenumerable directory as \"those slots
--     are free\". The two public consumers turn it into one logged
--     diagnostic apiece: an empty table at @engine.listSaves()@, and
--     @false, reason@ from the autosave slot verbs.
--
--   Only synchronous 'IOException's are contained. An asynchronous
--   exception, and any other exception type, still propagates.
listSaves ∷ LoggerState → HS.HashSet Text → IO (Either Text [SaveListing])
listSaves = listSavesWithSeams productionListingSeams

-- | 'listSaves' with its two I\/O seams supplied by the caller. Exists
--   so the headless gate can fail one exact generation path, or the
--   enumeration of @saves\/@ itself, deterministically (issue #2333) —
--   the same reason 'World.Save.Storage.publishGenerationWithSeams' has
--   a reader seam (#2227), and for the same reason: filesystem mode bits
--   are ignored by CI's root containers, and a directory standing in for
--   an unreadable file never reaches 'BS.readFile' at all (the
--   'doesFileExist' guard sends it down the missing-generation path
--   instead).
listSavesWithSeams
    ∷ ListingSeams → LoggerState → HS.HashSet Text
    → IO (Either Text [SaveListing])
listSavesWithSeams seams logger luaKnownNames = do
    rootResult ← try $ do
        createDirectoryIfMissing True savesDirectory
        found ← lsEnumerateSaves seams savesDirectory
        -- Force the spine inside the handler's scope: a seam (or a
        -- future enumerator) that produces entries lazily must not
        -- escape containment by throwing at the first 'mapM' step.
        _ ← evaluate (length found)
        pure found
    case rootResult of
        Left (e ∷ IOException) → pure (Left (rootFailure e))
        Right entries → do
            results ← mapM tryEntry entries
            pure (Right (saveListingOrder (concat results)))
  where
    -- Names the directory and carries the underlying error, because
    -- this text IS the reason the autosave verbs return and the sole
    -- diagnostic each public consumer logs.
    rootFailure e =
        "the saves directory '" <> T.pack savesDirectory
            <> "' could not be read: " <> tshow e

    -- The load path's own classification of an unreadable generation,
    -- mirrored: a reason string, never an escaping exception. The path
    -- is part of the reason so every message built from it names the
    -- file that actually failed.
    readGeneration path = do
        result ← try (lsReadGeneration seams path)
        pure $ case result of
            Left (e ∷ IOException) →
                Left ("cannot read " <> T.pack path <> ": " <> tshow e)
            Right bytes → Right bytes

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
                                -- #2333: an unreadable authoritative
                                -- generation is the load path's
                                -- 'GenerationCorrupt "cannot read: …"',
                                -- so it takes the same fallback the
                                -- corrupt-bytes case below takes.
                                readAuth ← readGeneration authPath
                                case readAuth of
                                    Left reason →
                                        tryPreviousListing name prevPath reason
                                    Right bytes → classifyAuth name prevPath bytes

    -- Envelope-aware: skip files whose magic/version/manifest/checksums
    -- don't validate. A pre-#759 flat file fails the envelope version
    -- check and is skipped with a logged warning so the user has a
    -- chance of noticing.
    classifyAuth name prevPath bytes =
        case decodeSaveEnvelopeMetadataClassified luaKnownNames bytes of
            Right meta → return [mkListing name meta False]
            -- Requirement 7: an INCOMPATIBLE authoritative generation is
            -- dropped outright. The previous generation is never even
            -- read, let alone listed.
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
                readPrev ← readGeneration prevPath
                case readPrev of
                    -- #2333: the fallback completes as UNUSABLE — one
                    -- warning, this slot skipped, every other slot still
                    -- listed.
                    Left reason → do
                        logWarn logger CatWorld $
                            "listSaves: skipping " <> name
                                <> ": authoritative generation unreadable ("
                                <> authErr
                                <> ") and previous generation is also \
                                   \unusable (" <> reason <> ")"
                        return []
                    Right bytes →
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
        -- #2335: at most ONE row per logical save NAME, and it is the
        -- form loading that name would actually reach. 'loadWorld'
        -- selects the slot DIRECTORY whenever one exists AT ALL, before
        -- it ever looks at the flat path, so a flat file with a
        -- directory namesake is unreachable by name and must not be
        -- published as a second row that would silently load the
        -- directory instead (every consumer keys a listing row by
        -- 'slName' alone). That holds even when the directory turns out
        -- to be corrupt, symlinked or otherwise unlistable: the flat
        -- file is no more reachable by name then, so NEITHER row is
        -- published and the directory's own path reports why.
        --
        -- The decision is a direct 'doesDirectoryExist' on the namesake
        -- rather than a look at what the other entries produced, so it
        -- is independent of enumeration order.
        shadowed ← doesDirectoryExist (saveSlotPath name)
        if shadowed
          then do
            logWarn logger CatWorld $
                "listSaves: skipping legacy save file '" <> T.pack path
                    <> "': the save name is already held by the slot \
                       \directory '"
                    <> T.pack (addTrailingPathSeparator (saveSlotPath name))
                    <> "', which is what loading '" <> name
                    <> "' reaches. Neither file was modified -- rename \
                       \one of them to list both."
            return []
          else loadLegacyFileEntry name path

    loadLegacyFileEntry name path = do
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
                -- #2333: a legacy flat file has no previous generation
                -- to fall back to, so an unreadable one is skipped the
                -- same way an undecodable one already was.
                readLegacy ← readGeneration path
                case readLegacy of
                    Left reason → do
                        logWarn logger CatWorld $
                            "listSaves: skipping legacy save file: " <> reason
                        return []
                    Right bytes →
                        case decodeSaveEnvelopeMetadata luaKnownNames bytes of
                            Left err → do
                                logWarn logger CatWorld $
                                    "listSaves: skipping " <> T.pack path
                                        <> ": " <> err
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
