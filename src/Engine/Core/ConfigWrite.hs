{-# LANGUAGE ScopedTypeVariables #-}
-- | The single durable writer for everything the engine persists under
--   @config/@ (#2202).
--
--   Before this module, five families each called 'Data.Yaml.encodeFile'
--   (or 'Data.ByteString.writeFile', or 'System.Directory.copyFile') on
--   the target path directly. @encodeFile@ streams through libyaml's
--   @yaml_emitter_set_output_file@ onto a directly opened target, so a
--   crash, a kill, or a full disk part way through the emit left a
--   TRUNCATED @*.local.yaml@ behind. The next boot decoded it as
--   malformed, fell back to defaults, and the next save overwrote it:
--   the player's settings were lost once, with only a log line — and a
--   truncated neutrality record (#1937) decoded as 'Nothing' and
--   re-promoted the placeholder that record exists to keep out of local
--   state.
--
--   === The publish sequence
--
--   Every write here is the same four steps, in this order:
--
--     1. create the target's directory if it is absent;
--     2. claim a fresh, uniquely named TEMPORARY in that SAME directory
--        (never @\/tmp@ — a cross-filesystem rename is not atomic and
--        would degrade to a copy);
--     3. write the bytes into it and @fsync@ the file before trusting
--        them;
--     4. @rename(2)@ the temporary onto the target — one atomic
--        filesystem operation that either fully replaces the target or
--        does not happen at all — and then @fsync@ the target's
--        DIRECTORY, because a file's own @fsync@ says nothing about the
--        directory entry naming it.
--
--   The primitives are "World.Save.Storage.Durable"'s, reused rather
--   than reimplemented — that module was extracted for exactly this
--   (#2024 requirement 3) and nothing in it knows what a save is. The
--   world-save transaction itself ('World.Save.Storage.publishGeneration')
--   is NOT reused: it is bound to the save-slot envelope and its
--   @.prev@ rotation, neither of which a config file has.
--
--   The durability stance is the one "World.Save.Storage" already
--   documented and is deliberately not reopened here: plain POSIX
--   @fsync@ on both platforms, never macOS's @F_FULLFSYNC@.
--
--   === What a failure leaves behind
--
--   Failure atomicity is stated BY PHASE, because the phases differ:
--
--     * Every PRE-RENAME failure — the directory, the temporary's name,
--       the write, the flush — leaves the previous target BYTE-IDENTICAL
--       to what it was, because nothing has touched it yet.
--     * A rename failure is the same: the target is untouched.
--     * A failure of the directory sync happens AFTER the rename, so the
--       visible target is the COMPLETE new file, never a partial one.
--       It still returns 'Left', because durability is unconfirmed and
--       claiming otherwise is the lie this module exists to stop.
--
--   In every returned outcome this operation's own temporary is gone.
--
--   === Synchronous versus asynchronous
--
--   A synchronous filesystem failure becomes a descriptive 'Left' naming
--   the target path and the cause, so a caller can log it and a Lua verb
--   can return @false@ instead of raising. An ASYNCHRONOUS exception is
--   never converted: the temporary is removed and the exception is
--   rethrown, because
--   'Engine.Scripting.Lua.API.Internal.registerLuaFunction' re-throws
--   async exceptions on purpose so shutdown's @killThread@ still reaches
--   the Lua thread.
module Engine.Core.ConfigWrite
  ( -- * Writing
    writeConfigBytes
  , writeConfigYaml
  , copyConfigFile
    -- * Removing
  , removeConfigFile
  , removeConfigFileWith
    -- * Injection seam
  , ConfigWriteOps(..)
  , realConfigWriteOps
  , writeConfigBytesWith
  ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Control.Exception (SomeAsyncException, SomeException, fromException
                         , onException, throwIO, try)
import Data.Aeson (ToJSON)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile
                        , renameFile)
import System.FilePath (takeDirectory, takeFileName)
import World.Save.Storage.Durable
  ( WriteStep(..), claimUniquePath, removeIfExists, syncDirectory
  , writeBytesDurably )

-- | The filesystem operations 'writeConfigBytesWith' performs, behind a
--   record so the gate can fail exactly one phase and observe what the
--   other phases left on disk. Production always uses
--   'realConfigWriteOps'; nothing outside the gate substitutes anything
--   else, which is why the seam is a plain record rather than a class.
data ConfigWriteOps = ConfigWriteOps
    { cwoClaimTemp ∷ FilePath → String → IO FilePath
      -- ^ Claim an unused path under the directory, named from the
      --   dot-free template.
    , cwoWrite ∷ FilePath → BS.ByteString
               → IO (Either (WriteStep, SomeException) ())
      -- ^ Write the bytes and @fsync@ the file.
    , cwoRename ∷ FilePath → FilePath → IO ()
      -- ^ Atomically publish the temporary onto the target.
    , cwoSyncDir ∷ FilePath → IO ()
      -- ^ @fsync@ the directory whose entry the rename just replaced.
    , cwoDiscardTemp ∷ FilePath → IO [Text]
      -- ^ Remove a temporary that will never be published, REPORTING a
      --   failure rather than throwing. A leftover is not worth failing
      --   an operation that has already failed for another reason, but
      --   it is worth naming: silently swallowing it would let this
      --   module return a 'Left' while a @tmp-…@ file it made sat in
      --   the player's @config/@ for ever.
    , cwoRemoveTarget ∷ FilePath → IO ()
      -- ^ Unlink an existing target, for 'removeConfigFile'.
    }

realConfigWriteOps ∷ ConfigWriteOps
realConfigWriteOps = ConfigWriteOps
    { cwoClaimTemp = claimUniquePath
    , cwoWrite     = writeBytesDurably
    , cwoRename    = renameFile
    , cwoSyncDir   = syncDirectory
    , cwoDiscardTemp  = removeIfExists
    , cwoRemoveTarget = removeFile
    }

-- | Durably replace @path@ with @bytes@. The one write primitive every
--   @config/@ family goes through; see the module header for the
--   sequence, the per-phase failure guarantees, and the async policy.
writeConfigBytes ∷ FilePath → BS.ByteString → IO (Either Text ())
writeConfigBytes = writeConfigBytesWith realConfigWriteOps

-- | 'writeConfigBytes' over a YAML document. The encode is pure, so a
--   document this cannot render surfaces as the write step's own 'Left'
--   rather than as an exception escaping past the caller.
writeConfigYaml ∷ ToJSON α ⇒ FilePath → α → IO (Either Text ())
writeConfigYaml path = writeConfigBytes path ∘ Yaml.encode

-- | Durably copy one config file onto another — the legacy-to-local
--   migration in 'Engine.Core.Init.migrateLegacyConfig', whose direct
--   'System.Directory.copyFile' had the same truncation hazard AND a
--   worse consequence: migration is gated on the local file's mere
--   EXISTENCE, so one interrupted partial copy suppressed every later
--   migration attempt for good.
copyConfigFile ∷ FilePath → FilePath → IO (Either Text ())
copyConfigFile src dst = do
    loaded ← trySynchronous (BS.readFile src)
    case loaded of
        Left e → pure $ Left $ "could not read " <> T.pack src <> ": "
                                 <> tshow e
        Right bytes → writeConfigBytes dst bytes

-- | 'writeConfigBytes' against injected operations. Exported for the
--   gate; production calls 'writeConfigBytes'.
writeConfigBytesWith ∷ ConfigWriteOps → FilePath → BS.ByteString
                     → IO (Either Text ())
writeConfigBytesWith ops path bytes = do
    prepared ← trySynchronous (createDirectoryIfMissing True dir)
    case prepared of
        Left e   → pure (failed "could not create the directory for" e)
        Right () → do
            claimed ← trySynchronous (cwoClaimTemp ops dir template)
            case claimed of
                Left e    → pure (failed "could not stage a temporary for" e)
                -- Cleanup ownership is held from the moment the name is
                -- claimed until the rename consumes it. 'onException'
                -- covers EVERY exception that escapes 'publish',
                -- asynchronous ones included — those are rethrown rather
                -- than returned, so without this a kill landing in the
                -- rename would leave the temporary behind (#2202 review
                -- round 1).
                Right tmp → publish tmp `onException` bestEffortDiscard tmp
  where
    dir = takeDirectory path

    -- 'claimUniquePath' documents that its template must contain no
    -- '.': a dotted template makes 'openBinaryTempFile' insert its
    -- generated suffix BEFORE the last dot, which would produce a name
    -- ending in the real extension — indistinguishable, to anything
    -- that later scans this directory, from a config file the player
    -- owns.
    template = "tmp-" ⧺ map (\c → if c ≡ '.' then '-' else c)
                             (takeFileName path)

    failed ∷ Text → SomeException → Either Text ()
    failed what e = Left $ what <> " " <> T.pack path <> ": "
                             <> tshow e

    -- A pre-rename failure, with its temporary removed FIRST so the
    -- returned message can also name a leftover the removal could not
    -- clear (#2202 review round 1: a swallowed cleanup failure let this
    -- return 'Left' while its own temporary stayed in config/).
    failedAfterDiscard ∷ FilePath → Text → SomeException
                       → IO (Either Text ())
    failedAfterDiscard tmp what e = do
        leftovers ← cwoDiscardTemp ops tmp
        pure $ case failed what e of
            Left message → Left (message <> renderLeftovers leftovers)
            Right ()     → Right ()

    bestEffortDiscard tmp = void (cwoDiscardTemp ops tmp)

    publish tmp = do
        written ← cwoWrite ops tmp bytes
        case written of
            Left (step, e) → do
                -- Discard BEFORE the rethrow, so an async exception
                -- surfacing HERE (rather than escaping to the
                -- 'onException' above) still leaves nothing behind.
                outcome ← failedAfterDiscard tmp (describeStep step) e
                rethrowIfAsync e
                pure outcome
            Right () → do
                renamed ← trySynchronous (cwoRename ops tmp path)
                case renamed of
                    Left e → failedAfterDiscard tmp "could not publish" e
                    -- Past the rename the temporary no longer exists:
                    -- rename(2) consumed the name. There is nothing left
                    -- to discard on the sync path.
                    Right () → do
                        synced ← trySynchronous (cwoSyncDir ops dir)
                        pure $ case synced of
                            Left e   → failed "could not confirm durable" e
                            Right () → Right ()

    describeStep ∷ WriteStep → Text
    describeStep StepOpen  = "could not open a temporary for"
    describeStep StepWrite = "could not write"
    describeStep StepFlush = "could not flush"

-- | Append a cleanup failure to the message that is already being
--   returned. Empty when the temporary went away, which is the case
--   every real failure takes.
renderLeftovers ∷ [Text] → Text
renderLeftovers [] = ""
renderLeftovers warnings = " (and " <> T.intercalate "; " warnings <> ")"

-- | Durably remove @path@, for the one family whose \"no overrides
--   left\" state is the ABSENCE of the file rather than an empty
--   document ('Engine.Save.Config.clearLocalFile'). @Right True@ when a
--   file was removed and the directory entry's disappearance was
--   confirmed durable, @Right False@ when there was nothing to remove
--   (so nothing changed and nothing needs syncing), @Left@ naming the
--   path and cause otherwise.
--
--   The directory sync is the whole point (#2202 review round 1): an
--   unlink is a directory-entry change exactly like the publish rename,
--   so reporting success before it is confirmed would let a crash
--   restore a stale @config/save.local.yaml@ — and with it autosave
--   settings the player had just reset — over a state this already
--   reported as saved.
removeConfigFile ∷ FilePath → IO (Either Text Bool)
removeConfigFile = removeConfigFileWith realConfigWriteOps

-- | 'removeConfigFile' against injected operations. Exported for the
--   gate; production calls 'removeConfigFile'.
removeConfigFileWith ∷ ConfigWriteOps → FilePath → IO (Either Text Bool)
removeConfigFileWith ops path = do
    present ← trySynchronous (doesFileExist path)
    case present of
        Left e      → pure (failedTo "could not inspect" e)
        Right False → pure (Right False)
        Right True  → do
            removed ← trySynchronous (cwoRemoveTarget ops path)
            case removed of
                Left e   → pure (failedTo "could not remove" e)
                Right () → do
                    synced ← trySynchronous
                                 (cwoSyncDir ops (takeDirectory path))
                    pure $ case synced of
                        -- The unlink itself already happened and is
                        -- visible; only its durability is unconfirmed,
                        -- which is the same phase distinction the write
                        -- path draws after its rename.
                        Left e   → failedTo "could not confirm the removal \
                                            \of" e
                        Right () → Right True
  where
    failedTo ∷ Text → SomeException → Either Text Bool
    failedTo what e = Left $ what <> " " <> T.pack path <> ": " <> tshow e

-- | 'try' that lets an asynchronous exception through. 'writeBytesDurably'
--   catches @SomeException@ (it has to, to name its own failing step), so
--   this module is where the two are told apart again.
trySynchronous ∷ IO α → IO (Either SomeException α)
trySynchronous action = do
    outcome ← try action
    case outcome of
        Left e  → do
            rethrowIfAsync e
            pure outcome
        Right _ → pure outcome

rethrowIfAsync ∷ SomeException → IO ()
rethrowIfAsync e = case fromException e ∷ Maybe SomeAsyncException of
    Just _  → throwIO e
    Nothing → pure ()
