{-# LANGUAGE Strict #-}
-- | The atomic save-publication storage transaction (issue #762,
--   persistence-overhaul C1). Prior to this module, saving wrote the
--   already-encoded envelope bytes (issue #759/#760's tagged, checksummed
--   component container) straight to the slot's authoritative
--   @saves/\<slot\>\/world.synworld@ via a single 'Data.ByteString.writeFile'
--   ("World.Save.Serialize"'s old @writeSaveFilesUnsafe@) — a crash, power
--   loss, disk-full condition, or write failure mid-write could truncate
--   or corrupt the ONLY copy of that generation, with no way back.
--
--   This module never reads live gameplay state and never participates in
--   snapshot capture (issue #758's barrier already released by the time
--   'publishGeneration' runs — see "World.Thread.Command.Save.WriteWorld").
--   It receives only already-encoded bytes, the metadata that encode
--   produced, a slot directory, and a slot name for diagnostics, and
--   performs a classic write-validate-publish-rotate transaction:
--
--   1. Refuse to operate at all through a slot directory that is itself a
--      symlink, OR whose immediate parent (in production, @saves/@
--      itself) is (requirement 12) — see 'rejectSymlinkedSlotDir'.
--   2. Write the candidate to a UNIQUELY named temporary file in the SAME
--      slot directory (so the final publish step can use an atomic
--      same-filesystem rename) — never a shared/predictable name two
--      concurrent publishers could collide on
--      ('System.IO.openBinaryTempFile' handles this).
--   3. Flush + durably sync the candidate ('fileSynchronise', POSIX
--      @fsync@) before ever trusting it.
--   4. Re-read the candidate FROM DISK (not the in-memory bytes the
--      encoder produced) and fully decode + deep-force it, applying the
--      same validation bar the load path enforces, PLUS confirming the
--      re-read metadata is EXACTLY the metadata this publish intended —
--      an in-memory encode success is never sufficient on its own to
--      publish.
--   5. Only once validated: classify the slot's CURRENT authoritative
--      file with the loader's own 'decodeGenerationFile'
--      ('classifyAuthoritative'), and rotate ONLY when that
--      classification says a generation is there worth retaining
--      ('AuthoritativeRetained' — a complete generation, or a coherent-
--      but-incompatible one the loader never falls back past). Then:
--      stage the existing previous generation (if any) out of the way
--      under a fresh unique name FIRST (never destroying it outright
--      yet — requirement 5's "older generations are removed only after
--      the new publication is durable"), THEN SYNC THE DIRECTORY so
--      that staging handoff is itself a confirmed-durable recovery
--      point; rotate the CURRENT authoritative generation into the
--      now-free previous-generation slot, THEN SYNC AGAIN. In the two
--      topologies where the previous generation is instead the slot's
--      LIVE load source — no authoritative file at all (a
--      "previous-only" recovery state), or a present-but-STORAGE-CORRUPT
--      one the loader is already falling back past (issue #1203) —
--      staging and rotation are skipped ENTIRELY and the existing
--      previous generation is left completely untouched, never at risk
--      of being destroyed or overwritten before the new candidate is
--      durable; see 'publishValidated'. Either way, finally atomically
--      rename the validated candidate into the authoritative path, THEN
--      SYNC A FINAL TIME. Every rename uses 'System.Directory.renameFile',
--      which is POSIX @rename(2)@ — a single atomic filesystem
--      operation that either fully replaces the destination or doesn't
--      happen at all; there is no filesystem-level "partway through a
--      rename" state to defend against. Syncing after EACH rename (not
--      only the last) means every intermediate handoff is durable in
--      its own right before the next step ever runs.
--   6. Report success only after that FINAL sync (following the publish
--      rename) confirms the new generation's own durability — never
--      before.
--   7. Only now — past the durability boundary — remove the staged old
--      generation and any other recognized stale transaction artifact.
--
--   === Documented durability boundary (requirement 4)
--
--   \"Durable\" here means: the candidate's data + the directory entries
--   for every rename above have been handed to 'fileSynchronise' (@fsync@)
--   and that call returned without error. On Linux (ext4/xfs/btrfs) this
--   is a real durability guarantee against a subsequent crash or power
--   loss. On macOS, the kernel's plain @fsync(2)@ does NOT guarantee the
--   drive's own write cache has been flushed to physical media — only
--   @F_FULLFSYNC@ does, and it is dramatically slower (typically
--   10-100x), so this module deliberately uses the ordinary POSIX @fsync@
--   (via 'System.Posix.Unistd.fileSynchronise') on both platforms rather
--   than pay that cost on every save. This is the same trade-off
--   essentially every desktop application (this one is a game engine, not
--   a database) makes; a save is protected against process crashes,
--   application bugs, and disk-full/permission failures either way — the
--   narrower residual risk this accepts is a genuine OS-level crash or
--   power loss landing in the exact window between a macOS @fsync@
--   returning and the drive's own cache actually reaching platter.
--   Windows is not a supported platform (see CLAUDE.md Platform Notes)
--   and is out of scope.
--
--   === Crash-window recoverability (requirement 6)
--
--   Every phase above is ordered so that an interruption at ANY point —
--   before the temp file exists, mid-write, after write but before
--   validation, after validation but before staging/rotation, mid-
--   staging, mid-rotation, mid-publish, after publish but before the
--   directory syncs, or during the final best-effort cleanup sweep —
--   always leaves AT LEAST ONE fully intact generation that
--   'selectLoadGeneration' actually SELECTS (not merely one that exists
--   on disk somewhere), NEVER a partial/hybrid result. Which phases even
--   run depends on 'classifyAuthoritative''s topology, so the guarantee
--   is stated per topology (issue #1203):
--
--   * 'AuthoritativeRetained' (the ordinary case — a valid, or a
--     coherent-but-incompatible, authoritative generation) runs the full
--     stage → rotate → publish sequence. Anything before the publish
--     rename completes: the authoritative path is untouched (or, if the
--     rotate rename already ran, the OLD generation is sitting at the
--     previous-generation path instead, where the loader's own
--     authoritative-missing fallback reads it) — either way a complete
--     old generation is SELECTED. Staging a previous generation out of
--     the way never destroys it — it only moves to a differently-named
--     file, still fully intact on disk, until cleanup removes it after
--     the durability boundary. It is not selectable while staged, which
--     is exactly why staging is confined to this topology: the
--     generation it displaces is never the one the slot is currently
--     loading from.
--   * 'AuthoritativeAbsent' and 'AuthoritativeRecovering' (the two
--     topologies where 'previousGenerationFileName' IS the slot's live
--     load source) skip staging and rotation altogether, so the ONLY
--     durable state change this transaction makes is the publish rename
--     itself. Before it, the previous generation is still exactly where
--     it was and still what 'selectLoadGeneration' recovers — for
--     'AuthoritativeRecovering' through its 'GenerationCorrupt'
--     fallback, unchanged by anything publication did. There is no
--     intermediate state in between to be interrupted in.
--   * Anything from the publish rename onward, in every topology: the
--     authoritative path already holds the complete NEW generation (a
--     rename is atomic; it cannot be observed half-applied), and the
--     previous generation retained beside it — whether rotated there or
--     never moved — is still a complete generation the loader falls back
--     to if the new authoritative one is ever lost.
--
--   A leftover, unpublished temporary or staged-previous file is always
--   harmless dead weight — genuinely harmless, because per the topology
--   split above a staged file is never the generation the slot was
--   loading from. It is never consulted by 'selectLoadGeneration' or
--   "World.Save.Serialize"'s listing (it lives under a name other than
--   @world.synworld@/@world.synworld.prev@) — and is swept on the NEXT
--   successful publish to this slot, or left for a future explicit
--   cleanup if none ever comes.
module World.Save.Storage
    ( authoritativeFileName
    , previousGenerationFileName
    , rejectSymlinkedSlotDir
    , rejectSymlinkedPath
    , StoragePhase(..)
    , PublishFailure(..)
    , renderPublishFailure
    , publishGeneration
    , publishGenerationWithCandidateCreator
    , publishGenerationWithSeams
    , GenerationSource(..)
    , LoadSelection(..)
    , selectLoadGeneration
    , LoadFailure(..)
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Text as T
import Control.Exception (IOException, SomeException, try, finally)
import System.Directory
    ( createDirectoryIfMissing, doesFileExist, removeFile, listDirectory
    , renameFile )
import System.FilePath ((</>))
import System.IO (Handle, openBinaryTempFile)
import System.IO.Error (isDoesNotExistError)
import World.Save.Storage.Durable
    ( rejectSymlinkedPath, rejectSymlinkedManagedPath, durableFlush
    , syncDirectory, claimUniquePath, isTransientName, closeQuietly
    , removeIfExists )
import World.Save.Types (SaveData, SaveMetadata(..), checkWorldCount)
import World.Save.Envelope
    ( decodeSessionEnvelope, decodeSessionEnvelopeClassified
    , GenerationFailure(..), renderGenerationFailure
    , LoadProgress(..), generationFailureProgress
    , foreignOptionalComponentIds, LuaComponentSpec(..) )
import World.Save.Envelope.Types (ComponentId(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)

-- File naming ----------------------------------------------------------

-- | The slot's authoritative generation — the same name #759/#760 always
--   used; only how it gets WRITTEN changes here.
authoritativeFileName ∷ FilePath
authoritativeFileName = "world.synworld"

-- | The slot's single retained previous generation (requirement 5). Not
--   named @world.synworld@ and not laid out as a sibling slot directory,
--   so it is naturally invisible to ordinary listing/loading unless
--   explicitly consulted as a fallback ('selectLoadGeneration',
--   requirements 2 and 8).
previousGenerationFileName ∷ FilePath
previousGenerationFileName = "world.synworld.prev"

-- | Every candidate this transaction ever creates is opened through
--   'System.IO.openBinaryTempFile' with this template — the RTS mixes in
--   its own per-call unique NUMERIC suffix (retried under an exclusive
--   create on collision), so concurrent publishers to the same slot can
--   never predictably collide on a shared temp name (requirement 2).
--   Deliberately contains no @.@: 'openBinaryTempFile' treats a template's
--   LAST dot as an extension to preserve and inserts its generated suffix
--   BEFORE it (e.g. a @"foo.tmp"@ template yields @"foo1234-0.tmp"@, not
--   @"foo.tmp1234"@) — a dot-free template keeps the generated suffix
--   trailing, which 'isOwnedArtifactName' below relies on to recognise
--   (and only ever recognise) this module's own files.
candidateTemplate ∷ String
candidateTemplate = "world-synworld-tmp"

-- | The unique name a previous generation is staged under while a new
--   candidate is being rotated into place (requirement 5) — see
--   'stageOldPrevious'. Same dot-free-template reasoning as
--   'candidateTemplate'.
staleTemplate ∷ String
staleTemplate = "world-synworld-stale"

-- | The pre-#759 companion file. #759 requirement 5 made it
--   non-authoritative and unused; requirement 13 here lets a successful
--   publication remove a stale leftover copy belonging to THIS slot.
staleGenYamlFileName ∷ FilePath
staleGenYamlFileName = "world_gen.yaml"

-- | True iff @name@ is exactly one of this module's own generated
--   transient names — 'World.Save.Storage.Durable.isTransientName''s
--   digit-anchored recognition of an 'openBinaryTempFile' product, so
--   cleanup can never sweep an unrelated file that merely shares the
--   prefix (e.g. a player's own note file dropped in the slot directory)
--   — requirement 12's "recognized stale transaction artifacts... never
--   unrelated files" and requirement 13's "cleanup must not affect
--   unrelated user files".
isOwnedArtifactName ∷ String → Bool
isOwnedArtifactName name =
    isTransientName candidateTemplate name ∨ isTransientName staleTemplate name

-- Path safety ------------------------------------------------------------

-- | Refuse to operate through a slot directory that is itself a symlink
--   (requirement 12): a pre-existing @saves/\<slot\>@ symlink would
--   otherwise be silently followed by directory creation, the temp
--   candidate, every rename, and cleanup alike — publishing into, and
--   deleting recognized-artifact-named files from, wherever it points,
--   outside the resolved resource root's @saves/@ tree entirely.
--   Exported so 'World.Save.Serialize.listSaves' — the one caller in
--   this slot's family that reads directory entries directly rather
--   than through 'publishGeneration'/'selectLoadGeneration' — can apply
--   the SAME containment check before it ever inspects or reports on a
--   slot's contents; otherwise a symlinked slot 'loadWorld' correctly
--   refuses could still be listed (and its bytes read) by
--   @engine.listSaves()@. Checks
--   both @dir@ itself AND its immediate parent (in production, exactly
--   @savesDirectory@, e.g. @"saves"@ — a symlinked @saves/@ would let
--   every slot beneath it escape the same way): 'pathIsSymbolicLink'
--   only inspects a path's OWN final component, transparently resolving
--   everything before it (ordinary POSIX @lstat@ semantics), so this
--   deliberately does NOT walk any further up than the immediate parent
--   — the resource root itself sitting behind an OS-level symlink (e.g.
--   macOS's @\/tmp@ → @\/private\/tmp@) is a pre-existing, unrelated
--   concern this check must not misfire on. A nonexistent path is not a
--   symlink (nothing to reject — an ordinary new slot); a filesystem
--   error other than "does not exist" while checking is treated the
--   same as "not a symlink" and left for the caller's own next
--   operation to report properly.
--
--   Since #2024 this is the save stack's name for
--   'World.Save.Storage.Durable.rejectSymlinkedManagedPath', the same
--   guard the generated-world library applies to its own root and
--   entries; the two stores cannot drift on what "contained" means.
rejectSymlinkedSlotDir ∷ FilePath → IO (Either Text ())
rejectSymlinkedSlotDir = rejectSymlinkedManagedPath

-- Publication ------------------------------------------------------------

-- | Every phase 'publishGeneration' can fail in, in the order a real
--   publication reaches them (requirement 10: an error names the storage
--   phase, never just "save failed").
data StoragePhase
    = PhaseUnsafePath
    | PhaseExistingGenerationRead
        -- ^ Issue #2227: one of the slot's existing generation files
        --   (@world.synworld@ or @world.synworld.prev@) is PRESENT but
        --   its bytes could not be read during the pre-publication
        --   inspection below — a permission refusal, an I\/O error, a
        --   file locked by something else. Distinct from
        --   'PhaseForeignOptionalData', which reports the opposite
        --   outcome: a generation that WAS read and definitely does
        --   carry an unrecognized optional component. An unreadable
        --   file is not evidence of either answer, and treating it as
        --   \"no foreign data\" let the rest of the transaction rename
        --   over (authoritative) or stage and sweep away (previous) an
        --   intact generation it had never actually inspected. The
        --   publish is refused instead, before any candidate exists —
        --   see 'existingGenerationPreflight'.
    | PhaseForeignOptionalData
        -- ^ Requirement 9 (issue #766, save-overhaul C4): the slot's
        --   existing authoritative generation carries an optional
        --   component this build does not recognize. Overwriting it
        --   would silently discard that data forever, so the whole
        --   publish is refused before any candidate is even written —
        --   see 'foreignOptionalDataCheck'.
    | PhaseDirectoryCreate
    | PhaseCandidateCreate
    | PhaseCandidateWrite
    | PhaseCandidateFlush
    | PhaseCandidateReread
    | PhaseCandidateValidate
    | PhaseStalePrevious
    | PhaseRotatePrevious
    | PhasePublishRename
    | PhaseDirectorySync
    deriving (Show, Eq, Enum, Bounded)

data PublishFailure = PublishFailure
    { pfPhase  ∷ !StoragePhase
    , pfSlot   ∷ !Text
    , pfPath   ∷ !(Maybe FilePath)
    , pfReason ∷ !Text
    } deriving (Show, Eq)

renderPublishFailure ∷ PublishFailure → Text
renderPublishFailure f =
    "save storage failed for slot '" <> pfSlot f <> "' during "
        <> tshow (pfPhase f) <> pathSuffix <> ": " <> pfReason f
  where
    pathSuffix = maybe "" (\p → " (" <> T.pack p <> ")") (pfPath f)

showT ∷ Show a ⇒ a → Text
showT = T.pack . show

-- | The complete storage transaction (requirement 1). Receives ONLY: the
--   slot directory (created if missing), a slot name (diagnostics only —
--   NOT re-sanitized here, the caller already owns that), the metadata
--   this candidate is expected to decode back to, and its complete,
--   already-encoded envelope bytes. Never reads live gameplay state and
--   never participates in snapshot capture.
--
--   On success, returns any NON-FATAL cleanup warnings — by the time
--   cleanup runs the documented durability boundary (the publish rename
--   plus directory sync) has already completed, so a cleanup failure is
--   reported for diagnostics but never turned into an overall save
--   failure (requirement 10's "cleanup failure" case).
publishGeneration
    ∷ FilePath        -- ^ slot directory
    → Text             -- ^ slot name (diagnostics only)
    → SaveMetadata      -- ^ metadata this candidate must decode back to
    → BS.ByteString      -- ^ complete, already-encoded envelope bytes
    → HS.HashSet Text    -- ^ every Lua component NAME this encode included
    → HS.HashSet Text    -- ^ the subset of those marked required
    → IO (Either PublishFailure [Text])
publishGeneration =
    publishGenerationWithSeams openBinaryTempFile BS.readFile

-- | The storage transaction with the candidate-file creation operation
-- supplied by the caller. Production uses 'publishGeneration'; this
-- narrow seam lets the headless gate exercise the candidate-create
-- failure classification deterministically, including in CI containers
-- that run as root and therefore bypass ordinary directory mode bits.
--
-- Reads existing generations with the production 'BS.readFile'; to
-- inject a read failure as well, use 'publishGenerationWithSeams'.
publishGenerationWithCandidateCreator
    ∷ (FilePath → String → IO (FilePath, Handle))
    → FilePath        -- ^ slot directory
    → Text             -- ^ slot name (diagnostics only)
    → SaveMetadata      -- ^ metadata this candidate must decode back to
    → BS.ByteString      -- ^ complete, already-encoded envelope bytes
    → HS.HashSet Text    -- ^ every Lua component NAME this encode included
    → HS.HashSet Text    -- ^ the subset of those marked required
    → IO (Either PublishFailure [Text])
publishGenerationWithCandidateCreator createCandidate =
    publishGenerationWithSeams createCandidate BS.readFile

-- | The storage transaction with BOTH injectable I\/O seams supplied by
-- the caller: the candidate-file creation operation, and the read of an
-- EXISTING generation file during the pre-publication inspection
-- ('existingGenerationPreflight'). Production uses 'publishGeneration',
-- which passes 'openBinaryTempFile' and 'BS.readFile' — the reader seam
-- exists so the headless gate can fail one exact generation path at a
-- time (issue #2227) without depending on filesystem mode bits, which
-- CI's root containers ignore. The reader is handed the full path of
-- the generation being inspected and is expected to throw an
-- 'IOException' the way 'BS.readFile' does; it is never used for the
-- candidate's own re-read (that stays 'BS.readFile' under
-- 'PhaseCandidateReread').
publishGenerationWithSeams
    ∷ (FilePath → String → IO (FilePath, Handle))
    → (FilePath → IO BS.ByteString)
    → FilePath        -- ^ slot directory
    → Text             -- ^ slot name (diagnostics only)
    → SaveMetadata      -- ^ metadata this candidate must decode back to
    → BS.ByteString      -- ^ complete, already-encoded envelope bytes
    → HS.HashSet Text    -- ^ every Lua component NAME this encode included
    → HS.HashSet Text    -- ^ the subset of those marked required
    → IO (Either PublishFailure [Text])
publishGenerationWithSeams createCandidate readGeneration dir slotName
    expectedMeta encoded luaKnownNames luaRequiredNames = do
    safety ← rejectSymlinkedSlotDir dir
    case safety of
        Left reason →
            pure (Left (failure PhaseUnsafePath (Just dir) reason))
        Right () → do
            preflight ← existingGenerationPreflight readGeneration dir
                            luaKnownNames
            case preflight of
                Left (PreflightUnreadable path reason) →
                    pure (Left (failure PhaseExistingGenerationRead
                                    (Just path) reason))
                Left (PreflightForeignData reason) →
                    pure (Left (failure PhaseForeignOptionalData (Just dir) reason))
                Right () → do
                    dirResult ← try (createDirectoryIfMissing True dir)
                    case dirResult of
                        Left (e ∷ IOException) →
                            pure (Left (failure PhaseDirectoryCreate (Just dir) (showT e)))
                        Right () → do
                            created ← try (createCandidate dir candidateTemplate)
                            case created of
                                Left (e ∷ IOException) →
                                    pure (Left (failure PhaseCandidateCreate (Just dir) (showT e)))
                                Right (tempPath, h) →
                                    writeValidateAndPublish dir slotName expectedMeta
                                        encoded luaKnownNames luaRequiredNames
                                        tempPath h
                                        `finally` cleanupLeftoverTemp tempPath
  where
    failure = publishFailureFor slotName

-- | Build a 'PublishFailure' for a given slot. A plain top-level
--   constructor application — kept as a named helper (rather than a
--   local @where@-bound closure duplicated at every call site) so every
--   phase below reads as one line.
publishFailureFor ∷ Text → StoragePhase → Maybe FilePath → Text → PublishFailure
publishFailureFor slotName phase path reason =
    PublishFailure phase slotName path reason

-- | Why 'existingGenerationPreflight' refused this publish. Internal to
--   the two 'StoragePhase' constructors the caller maps these onto —
--   'PhaseExistingGenerationRead' and 'PhaseForeignOptionalData' — kept
--   as a named type rather than a bare 'Text' so the two genuinely
--   different outcomes (issue #2227: \"could not look\" versus issue
--   #766's \"looked, and found unrecognized data\") cannot collapse back
--   into one indistinguishable refusal.
data PreflightRefusal
    = PreflightUnreadable !FilePath !Text
        -- ^ That EXACT generation file is present but unreadable; the
        --   'Text' is the underlying 'IOException'.
    | PreflightForeignData !Text

-- | Requirement 9 (issue #766, save-overhaul C4): refuse to overwrite a
--   slot whose CURRENT authoritative generation carries an optional
--   component this build does not recognize (an id absent from the
--   current Haskell registry AND from @luaKnownNames@, the live Lua
--   registry) — reading the bytes already on disk, never anything this
--   candidate itself is about to write. This satisfies "must not
--   silently discard unknown optional data during load → save" without
--   threading any opaque payload through live session state: the
--   original file simply never gets overwritten, so its bytes — known
--   or not — stay exactly as they were. A generation this build cannot
--   even structurally decode (corrupt, or carrying an unknown REQUIRED
--   component) reports no foreign data here — that failure mode is
--   already reported through the ordinary load path, and this check's
--   job is narrower: protect a generation that WOULD otherwise load
--   cleanly today. No existing authoritative file (first-ever publish,
--   or one already lost to storage corruption) is trivially safe.
--
--   Checks BOTH @world.synworld@ and @world.synworld.prev@ — not
--   merely the authoritative file. If the authoritative
--   generation is corrupt (present but undecodable), a real load
--   actually reads through to 'previousGenerationFileName' instead
--   ('selectLoadGeneration'\'s fallback), and 'publishValidated' then
--   stages THAT file aside and, once the new candidate durably lands,
--   sweeps the staged copy away for good ('cleanupAfterPublish') —
--   discarding whatever it carried without this check ever having
--   looked at it, since it only ever read the (corrupt, foreign-data-
--   free-by-construction) authoritative file. Checking @.prev@ too
--   closes that gap; an ordinary @.prev@ (the routine previous
--   generation from the last publish) never carries foreign data in
--   practice, so this adds no friction to the common case.
--
--   Issue #2227 closes the hole in \"trivially safe\": a generation file
--   that is PRESENT but whose bytes cannot be read is not absent and is
--   not corrupt — an @EACCES@, an @EIO@, or a file something else holds
--   open answers neither question this check asks. Reporting \"no
--   foreign data\" for it handed the rest of the transaction a verdict
--   the check had never earned, and the transaction then destroyed
--   exactly that file: an unreadable AUTHORITATIVE generation is
--   classified 'GenerationCorrupt' by the loader's own decoder, making
--   the topology 'AuthoritativeRecovering', which renames the candidate
--   straight over it (a POSIX rename needs write permission on the
--   DIRECTORY, not read permission on the file, so a read refusal never
--   stopped it); an unreadable @.prev@ beside an intact authoritative
--   file is staged aside by 'stageOldPrevious' and removed by
--   'cleanupAfterPublish'. So an unreadable generation now refuses the
--   whole publish ('PreflightUnreadable' → 'PhaseExistingGenerationRead'),
--   before any candidate file exists, naming the offending path and
--   carrying the underlying I\/O error. Bytes that READ fine but do not
--   structurally decode are unchanged by this: they still report no
--   foreign ids (the paragraph above), and #1203's corrupt-versus-
--   incompatible topology split still owns what happens to them.
existingGenerationPreflight
    ∷ (FilePath → IO BS.ByteString)
    → FilePath
    → HS.HashSet Text
    → IO (Either PreflightRefusal ())
existingGenerationPreflight readGeneration dir luaKnownNames = do
    authIds ← foreignIdsIn (dir </> authoritativeFileName)
    prevIds ← foreignIdsIn (dir </> previousGenerationFileName)
    pure $ do
        auth ← authIds
        prev ← prevIds
        case auth ⧺ prev of
            [] → Right ()
            ids → Left (PreflightForeignData
                ("existing generation carries data this build does not \
                 \recognize (" <> T.intercalate ", " (map cidText (L.nub ids))
                 <> ") -- refusing to overwrite it; save to a different name \
                    \to keep both"))
  where
    cidText (ComponentId t) = t
    foreignIdsIn path = do
        exists ← doesFileExist path
        if not exists then pure (Right []) else do
            readResult ← try (readGeneration path)
            case readResult of
                -- A file that vanished between the existence check above
                -- and this read really IS absent (requirement 4's first
                -- publish / previous-only recovery, losing a race with
                -- an external remover) -- never the \"present but
                -- unreadable\" case this refuses on.
                Left (e ∷ IOException)
                    | isDoesNotExistError e → pure (Right [])
                    | otherwise → pure (Left (PreflightUnreadable path (showT e)))
                Right bytes →
                    pure (Right (foreignOptionalComponentIds luaKnownNames bytes))

writeValidateAndPublish
    ∷ FilePath → Text → SaveMetadata → BS.ByteString
    → HS.HashSet Text → HS.HashSet Text → FilePath → Handle
    → IO (Either PublishFailure [Text])
writeValidateAndPublish dir slotName expectedMeta encoded
    luaKnownNames luaRequiredNames tempPath h = do
    writeResult ← try (BS.hPut h encoded)
    case writeResult of
        Left (e ∷ IOException) → do
            closeQuietly h
            pure (Left (fail' PhaseCandidateWrite (Just tempPath) (showT e)))
        Right () → do
            flushResult ← try (durableFlush h)
            case flushResult of
                Left (e ∷ SomeException) →
                    pure (Left (fail' PhaseCandidateFlush (Just tempPath) (showT e)))
                Right () → do
                    rereadResult ← try (BS.readFile tempPath)
                    case rereadResult of
                        Left (e ∷ IOException) →
                            pure (Left (fail' PhaseCandidateReread (Just tempPath) (showT e)))
                        Right rereadBytes →
                            case validateCandidate expectedMeta
                                    luaKnownNames luaRequiredNames rereadBytes of
                                Left reason →
                                    pure (Left (fail' PhaseCandidateValidate
                                                    (Just tempPath) reason))
                                Right () →
                                    publishValidated dir slotName
                                        luaKnownNames luaRequiredNames tempPath
  where
    fail' = publishFailureFor slotName

-- | Fully decode the re-read candidate bytes and confirm they describe
--   exactly the intended save request (requirement 3): the SAME
--   validation bar the load path enforces — complete envelope structure,
--   checksums, required components, component decode/migrate/validate,
--   and 'checkWorldCount' — PLUS a re-read-metadata-matches-request
--   check the load path has no analogue for (loading never has an
--   "expected" metadata to compare against; publishing does — the
--   COMPLETE 'SaveMetadata', not just the slot name/timestamp, so a
--   caller-side mismatch between @expectedMeta@ and what @encoded@
--   actually contains can never slip through on two matching fields
--   alone). An in-memory encode success alone is never sufficient — only
--   what 'BS.hPut' actually put on disk, read back, counts.
validateCandidate
    ∷ SaveMetadata → HS.HashSet Text → HS.HashSet Text → BS.ByteString
    → Either Text ()
validateCandidate expectedMeta luaKnownNames luaRequiredNames bytes = do
    (meta, snap, _luaComponents, _isMigratedLegacy) ←
        decodeSessionEnvelope luaKnownNames luaRequiredNames bytes
    when (meta ≢ expectedMeta) $
        Left "re-read candidate metadata does not match the intended save \
             \request"
    let req = SaveRequestMeta { srmSlotName  = smName meta
                              , srmTimestamp = smTimestamp meta
                              , srmAutosave  = smAutosave meta }
    sd ← checkWorldCount (snapshotToSaveData req snap)
    -- Deep-force the reconstructed SaveData: decoding alone only forces
    -- each field to WHNF (Strict/HashMap.Strict force the outer shape,
    -- not every nested leaf — the same caveat #758's own capture-side
    -- forcing documents). Walking a derived Show instance's full spine
    -- forces every scalar leaf transitively, since you cannot know a
    -- value's printed length without evaluating it — the same
    -- full-traversal guarantee 'Data.Serialize.encode' gives the write
    -- side, achieved here without adding NFData instances across the
    -- entire save-data tree.
    length (show sd) `seq` Right ()

-- | Stage the current previous generation (if any) out from under
--   'previousGenerationFileName' and onto a freshly claimed unique name,
--   rotate the CURRENT authoritative generation into the now-free
--   previous-generation slot, then atomically publish the validated
--   candidate as the new authoritative generation — syncing the
--   directory after EVERY one of those renames, not only the last, so
--   each intermediate handoff is itself a confirmed-durable recovery
--   point before the next step ever runs (requirement 6's crash-window
--   list names staging, rotation, AND publication as separate
--   interruption phases). Called only after 'validateCandidate' has
--   already accepted the candidate — never a candidate that merely
--   encoded successfully in memory.
--
--   Staging (rather than letting the rotate rename destroy the old
--   previous generation outright) is what satisfies requirement 5:
--   "older generations are removed only after the new publication is
--   durable" — the displaced generation stays fully intact under its
--   staged name until 'cleanupAfterPublish' removes it, which runs only
--   AFTER the FINAL directory sync (following the publish rename)
--   confirms the new generation's own durability, never before.
--
--   Staging and rotation happen ONLY when the slot's CURRENT
--   authoritative file is a generation actually worth retaining — i.e.
--   only when 'classifyAuthoritative' reports 'AuthoritativeRetained'.
--   In the other two topologies 'previousGenerationFileName' is the
--   slot's live load source, so it is left COMPLETELY untouched all the
--   way through the publish rename below (issue #1203):
--
--   * 'AuthoritativeAbsent' — a \"previous-only\" recovery state, e.g.
--     the slot's own authoritative file was already lost to an earlier
--     interruption and 'selectLoadGeneration' has been recovering from
--     'previousGenerationFileName' alone. There is nothing for rotation
--     to displace, and staging the previous generation away regardless
--     (as an early revision of this function did) would itself have
--     destroyed the ONLY known-valid generation before the new
--     candidate became durably selectable.
--   * 'AuthoritativeRecovering' — the authoritative file is PRESENT but
--     storage-corrupt, so 'selectLoadGeneration' is right now serving
--     this slot from 'previousGenerationFileName' via its
--     'GenerationCorrupt' fallback. Rotating the corrupt authoritative
--     file ONTO that path (which is what this function did before
--     #1203, deciding solely from @doesFileExist authPath@) would
--     overwrite the slot's only valid generation with garbage while the
--     valid copy sat under a staged name 'selectLoadGeneration' never
--     consults — an interruption in the window between that rotation's
--     directory sync and the publish rename left NOTHING selectable
--     despite complete save data on disk. Skipping both staging and
--     rotation closes that window: the recovery source never moves, and
--     the corrupt authoritative file is simply replaced outright by the
--     atomic publish rename (its bytes were already worthless).
--
--   Left untouched, that pre-existing previous generation simply
--   continues to serve as the previous generation once the new
--   candidate publishes — no explicit rotation needed, since it was
--   never moved out of that slot at all.
publishValidated
    ∷ FilePath → Text → HS.HashSet Text → HS.HashSet Text → FilePath
    → IO (Either PublishFailure [Text])
publishValidated dir slotName luaKnownNames luaRequiredNames tempPath = do
    let authPath = dir </> authoritativeFileName
        prevPath = dir </> previousGenerationFileName
    topology ← classifyAuthoritative luaKnownNames luaRequiredNames authPath
    case topology of
        AuthoritativeAbsent     → publish authPath Nothing
        AuthoritativeRecovering → publish authPath Nothing
        AuthoritativeRetained   → do
            stageResult ← stageOldPrevious dir prevPath
            case stageResult of
                Left (e ∷ IOException) →
                    pure (Left (fail' PhaseStalePrevious (Just prevPath) (showT e)))
                Right mStaled → afterSync (rotate authPath prevPath mStaled)
  where
    fail' = publishFailureFor slotName
    -- Sync the directory, then run @next@ only on success — every
    -- rename below is followed by this before the transaction proceeds.
    afterSync next = do
        syncResult ← try (syncDirectory dir)
        case syncResult of
            Left (e ∷ SomeException) →
                pure (Left (fail' PhaseDirectorySync (Just dir) (showT e)))
            Right () → next
    rotate authPath prevPath mStaled = do
        rotateResult ← try (renameFile authPath prevPath)
        case rotateResult of
            Left (e ∷ IOException) →
                pure (Left (fail' PhaseRotatePrevious (Just authPath) (showT e)))
            Right () → afterSync (publish authPath mStaled)
    publish authPath mStaled = do
        publishResult ← try (renameFile tempPath authPath)
        case publishResult of
            Left (e ∷ IOException) →
                pure (Left (fail' PhasePublishRename (Just tempPath) (showT e)))
            Right () → afterSync (Right ⊚ cleanupAfterPublish dir mStaled)

-- | What the slot's CURRENT authoritative file means for this
--   publication's rotation topology (issue #1203). Deliberately mirrors
--   the THREE outcomes 'selectLoadGeneration' already distinguishes, so
--   publication's judgement and the loader's can never drift
--   (requirement 4).
data AuthoritativeTopology
    = AuthoritativeAbsent
      -- ^ No authoritative file at all — the slot's first-ever publish,
      --   or an earlier interruption's \"previous-only\" recovery state.
      --   'selectLoadGenerationUnsafe' serves such a slot from
      --   'previousGenerationFileName'; nothing exists to rotate.
    | AuthoritativeRecovering
      -- ^ Present, but STORAGE-corrupt ('GenerationCorrupt': symlinked,
      --   unreadable, truncated, bad framing, or a failed checksum) —
      --   the ONE classification 'selectLoadGenerationUnsafe' falls back
      --   past, so 'previousGenerationFileName' is this slot's LIVE load
      --   source right now and must not be disturbed until the new
      --   candidate is durably authoritative.
    | AuthoritativeRetained
      -- ^ A complete generation worth keeping as the next previous
      --   generation, OR a coherent-but-INCOMPATIBLE one
      --   ('GenerationIncompatible': unsupported schema version, failed
      --   content validation). Incompatible belongs here, NOT with
      --   'AuthoritativeRecovering', precisely because
      --   'selectLoadGenerationUnsafe' never falls back past it
      --   (requirement 3's preserved corrupt-vs-incompatible
      --   distinction): 'previousGenerationFileName' is not serving that
      --   slot, so ordinary staging + rotation applies exactly as before
      --   #1203.
    deriving (Show, Eq)

-- | Classify the slot's current authoritative file for
--   'publishValidated'. Runs the loader's OWN 'decodeGenerationFile' —
--   the same Lua known/required component sets, the same symlink,
--   envelope, component and world-count validation — rather than a
--   cheaper structural approximation, so a file publication treats as
--   retainable is exactly a file 'selectLoadGeneration' would select,
--   and a file publication treats as a live recovery topology is exactly
--   one the loader would fall back past (requirement 4). The added cost
--   is one decode of a file every publish ALREADY reads and inspects
--   ('foreignOptionalDataCheck'), against a candidate this same
--   transaction already decodes and deep-forces in full
--   ('validateCandidate') — and it is skipped entirely when no
--   authoritative file exists.
classifyAuthoritative
    ∷ HS.HashSet Text → HS.HashSet Text → FilePath → IO AuthoritativeTopology
classifyAuthoritative luaKnownNames luaRequiredNames authPath = do
    authExists ← doesFileExist authPath
    if not authExists
        then pure AuthoritativeAbsent
        else do
            decoded ← decodeGenerationFile luaKnownNames luaRequiredNames authPath
            pure $ case decoded of
                Left (GenerationCorrupt _)        → AuthoritativeRecovering
                Left (GenerationIncompatible _ _) → AuthoritativeRetained
                Right _                         → AuthoritativeRetained

-- | If a previous generation exists, move it onto a freshly claimed
--   unique name and return that path; 'Nothing' when there was none to
--   stage (e.g. the slot's first-ever publish). The move is a single
--   atomic rename — the previous generation is never observed missing
--   without also being observed at its new staged name.
stageOldPrevious ∷ FilePath → FilePath → IO (Either IOException (Maybe FilePath))
stageOldPrevious dir prevPath = do
    exists ← doesFileExist prevPath
    if not exists
        then pure (Right Nothing)
        else try $ do
            staledPath ← claimUniquePath dir staleTemplate
            renameFile prevPath staledPath
            pure (Just staledPath)

-- | Runs only AFTER the durability boundary (the caller's directory
--   sync): remove the staged-away previous generation this publish
--   itself displaced (if any), then sweep every other recognized stale
--   transaction artifact belonging to this slot (requirement 13).
cleanupAfterPublish ∷ FilePath → Maybe FilePath → IO [Text]
cleanupAfterPublish dir mStaled = do
    staledWarnings ← maybe (pure []) removeIfExists mStaled
    artifactWarnings ← cleanupStaleArtifacts dir
    pure (staledWarnings ⧺ artifactWarnings)

-- | Best-effort removal of every leftover transaction artifact belonging
--   to this slot (temp candidates AND staged-previous files from THIS or
--   an earlier interrupted publish — see 'isOwnedArtifactName') plus a
--   stale pre-#759 @world_gen.yaml@ companion (requirement 13). Any
--   failure here is reported as a warning, never an overall publish
--   failure (the caller only reaches this after the durability boundary
--   already completed).
cleanupStaleArtifacts ∷ FilePath → IO [Text]
cleanupStaleArtifacts dir = do
    owned ← listOwnedArtifacts dir
    artifactWarnings ← concat ⊚ mapM removeIfExists owned
    yamlWarnings ← removeIfExists (dir </> staleGenYamlFileName)
    pure (artifactWarnings ⧺ yamlWarnings)

listOwnedArtifacts ∷ FilePath → IO [FilePath]
listOwnedArtifacts dir = do
    listed ← try (listDirectory dir)
    case (listed ∷ Either SomeException [FilePath]) of
        Left _        → pure []
        Right entries → pure
            [ dir </> e | e ← entries, isOwnedArtifactName e ]

-- | Best-effort: remove the ORIGINAL temp file if the transaction ended
--   (success or failure) without renaming it away. A successful publish
--   already moved it to the authoritative path, so this is a no-op then;
--   an unpublished candidate is harmless dead weight either way (never
--   consulted by 'selectLoadGeneration' or listing) and gets swept by
--   'cleanupStaleArtifacts' on this slot's next successful publish if
--   this removal itself fails or never runs.
cleanupLeftoverTemp ∷ FilePath → IO ()
cleanupLeftoverTemp tempPath = do
    exists ← doesFileExist tempPath
    when exists $ do
        r ← try (removeFile tempPath)
        case (r ∷ Either SomeException ()) of _ → pure ()

-- Load-source selection ----------------------------------------------

data GenerationSource = FromAuthoritative | FromPrevious
    deriving (Show, Eq)

data LoadSelection = LoadSelection
    { lsSource   ∷ !GenerationSource
    , lsDetail   ∷ !Text          -- ^ human-readable "why" (requirement 7)
    , lsSaveData ∷ !SaveData
    , lsLuaComponents ∷ ![(Text, Word32, BS.ByteString)]
        -- ^ Every Lua-owned component present in the SELECTED generation
        --   (bare name, encoded version, raw payload — issue #761), for
        --   the caller to hand to @saveModules.prepareLoad@. Comes from
        --   the SAME generation as 'lsSaveData' — never merged across
        --   generations, same as every other field here.
    , lsIsMigratedLegacyBaseline ∷ !Bool
        -- ^ Issue #766 (save-overhaul C4): 'True' iff the selected
        --   generation was reconstructed via a recognized pre-#760
        --   compatibility migration rather than decoded directly —
        --   'lsLuaComponents' is then always empty (the baseline predates
        --   every Lua-owned persistent component), and the caller must
        --   supply each currently-required Lua module's own empty-state
        --   default rather than treat it as missing.
    } deriving (Show)

-- | Why load-source selection failed: the human-readable message it has
--   always produced, PLUS how far the attempt structurally got (issue
--   #1919). 'lfProgress' is assembled from the same structured failures
--   the decode layers already carry, never parsed back out of
--   'lfMessage' — so rewording any diagnostic in this module, in
--   'World.Save.Envelope', or in 'World.Save.Component.Types' cannot
--   change what @engine.getLoadStatus()@ reports.
data LoadFailure = LoadFailure
    { lfProgress ∷ !LoadProgress
    , lfMessage  ∷ !Text
    } deriving (Show, Eq)

-- | Load-source selection (requirement 7). A fully valid authoritative
--   generation always wins. A STORAGE-corrupt authoritative (absent,
--   truncated, bad framing, or a checksum failure — see
--   'World.Save.Envelope.GenerationFailure') falls back to a fully valid
--   previous generation. A coherent-but-INCOMPATIBLE authoritative
--   (unsupported schema version, failed content/gameplay validation) is
--   reported directly and NEVER triggers a fallback — silently rolling
--   back to an older generation would hide a real compatibility problem
--   instead of reporting it. Never combines components across
--   generations: exactly one generation's 'SaveData' is ever returned.
selectLoadGeneration
    ∷ HS.HashSet Text → HS.HashSet Text → FilePath → Text
    → IO (Either LoadFailure LoadSelection)
selectLoadGeneration luaKnownNames luaRequiredNames dir slotName = do
    safety ← rejectSymlinkedSlotDir dir
    case safety of
        -- A refused slot directory never yielded a candidate to inspect.
        Left reason → pure (Left (LoadFailure ReachedNothing reason))
        Right ()    →
            selectLoadGenerationUnsafe luaKnownNames luaRequiredNames dir slotName

selectLoadGenerationUnsafe
    ∷ HS.HashSet Text → HS.HashSet Text → FilePath → Text
    → IO (Either LoadFailure LoadSelection)
selectLoadGenerationUnsafe luaKnownNames luaRequiredNames dir slotName = do
    let authPath = dir </> authoritativeFileName
        prevPath = dir </> previousGenerationFileName
    authExists ← doesFileExist authPath
    if not authExists
      then fallbackToPrevious prevPath "authoritative save file is missing"
      else do
        authResult ← decodeGenerationFile luaKnownNames luaRequiredNames authPath
        case authResult of
            Right (sd, luaComponents, isMigrated) →
                pure (Right (LoadSelection FromAuthoritative
                        "authoritative generation" sd luaComponents isMigrated))
            Left (GenerationIncompatible progress reason) →
                pure (Left (LoadFailure progress (incompatibleMessage reason)))
            Left (GenerationCorrupt reason) →
                fallbackToPrevious prevPath reason
  where
    incompatibleMessage reason =
        "save '" <> slotName <> "' authoritative generation is present but \
        \incompatible with this build (not a storage-corruption fallback \
        \case): " <> reason
    fallbackToPrevious prevPath authReason = do
        prevExists ← doesFileExist prevPath
        if not prevExists
          then pure (Left (LoadFailure ReachedNothing ("save '" <> slotName
                <> "' authoritative generation is unreadable (" <> authReason
                <> ") and no previous generation exists to recover from")))
          else do
            prevResult ← decodeGenerationFile luaKnownNames luaRequiredNames prevPath
            case prevResult of
                Right (sd, luaComponents, isMigrated) →
                    pure (Right (LoadSelection FromPrevious
                        ("authoritative generation unreadable (" <> authReason
                         <> "); recovered from the previous generation")
                        sd luaComponents isMigrated))
                -- Issue #1919: the composite message describes BOTH
                -- candidates, but the progress reported is the PREVIOUS
                -- candidate's own — it is the one this selection actually
                -- got furthest with. The authoritative candidate is
                -- 'GenerationCorrupt' here by construction (that is the
                -- only classification that reaches a fallback), so it
                -- contributes 'ReachedNothing' and could only ever lose
                -- information.
                Left prevFail →
                    pure (Left (LoadFailure (generationFailureProgress prevFail)
                        ("save '" <> slotName
                        <> "' authoritative generation is unreadable ("
                        <> authReason
                        <> ") and the previous generation is also unusable ("
                        <> renderGenerationFailure prevFail <> ")")))

-- | Decode + fully validate one candidate generation file, classified
--   per 'GenerationFailure'. 'checkWorldCount' classifies as
--   'GenerationIncompatible', not 'GenerationCorrupt': by the time it
--   runs, the envelope's own structural checksums have already passed
--   (see 'World.Save.Envelope.decodeSessionEnvelopeClassified'), so an
--   empty-pages result reflects a genuine content-validation failure
--   (issue #762 requirement 7's "fails gameplay/content validation" —
--   never a fallback trigger), not routine storage damage. In practice
--   'World.Save.Component.assembleSnapshot''s own cross-component
--   'validateSessionSnapshot' check already rejects an empty page set
--   earlier in this same decode, so this classification is defense in
--   depth rather than a normally-reachable path.
decodeGenerationFile
    ∷ HS.HashSet Text → HS.HashSet Text → FilePath
    → IO (Either GenerationFailure
                 (SaveData, [(Text, Word32, BS.ByteString)], Bool))
decodeGenerationFile luaKnownNames luaRequiredNames path = do
    -- requirement 12: 'publishGeneration' never leaves a symlink at
    -- 'authoritativeFileName'/'previousGenerationFileName' itself
    -- ('System.Directory.renameFile' replaces a destination symlink
    -- entry outright rather than writing through it — see
    -- 'World.Save.Storage.publishValidated'), so finding one here can
    -- only come from something outside this transaction. Classified as
    -- 'GenerationCorrupt' (fallback-eligible) rather than a hard
    -- refusal: the file simply isn't present "in the expected form",
    -- the same bucket @World.Save.Envelope.isRecoverableEnvelopeError@
    -- already puts a missing/truncated/malformed file in — the OTHER
    -- generation still gets its own independent version of this exact
    -- check before it is ever trusted, so a symlink at BOTH paths still
    -- correctly reports a hard failure rather than reading through either.
    linkSafe ← rejectSymlinkedPath path
    case linkSafe of
        Left reason → pure (Left (GenerationCorrupt reason))
        Right () → do
            readResult ← try (BS.readFile path)
            case readResult of
                Left (e ∷ IOException) →
                    pure (Left (GenerationCorrupt ("cannot read: " <> showT e)))
                Right bytes → pure $ do
                    (meta, snap, luaComponents, isMigrated) ←
                        decodeSessionEnvelopeClassified
                            luaKnownNames luaRequiredNames bytes
                    let req = SaveRequestMeta (smName meta) (smTimestamp meta)
                                    (smAutosave meta)
                    -- A content check that runs only AFTER assembly
                    -- succeeded: every component decoded, migrated,
                    -- validated and assembled, so no per-component phase
                    -- describes this failure. 'ReachedEnvelope' is what it
                    -- has always reported.
                    sd ← either (Left . GenerationIncompatible ReachedEnvelope)
                                Right
                           (checkWorldCount (snapshotToSaveData req snap))
                    pure (sd, [ (lcsId c, lcsVersion c, lcsPayload c)
                              | c ← luaComponents ]
                         , isMigrated)
