{-# LANGUAGE Strict #-}
-- | The low-level durable-publication primitives the save transaction is
--   built from (issue #762), extracted so a second on-disk store can be
--   built from the SAME pieces rather than from a second implementation
--   of them (issue #2024, requirement 3: the generated-world library
--   "reuses the existing transaction's primitives — a uniquely named
--   temporary in the same filesystem, @fsync@ before trust, an atomic
--   rename to publish — rather than a second implementation of them").
--
--   Nothing here knows what a save, a slot, a generation or a library
--   entry is. Each primitive does exactly one filesystem thing and
--   leaves classifying its failure to the caller — an exception escapes,
--   or 'writeBytesDurably' returns the step it failed in — so each
--   transaction can name its OWN phase; that is why this module is
--   deliberately not a transaction of its own. "World.Save.Storage" re-exports the two containment guards
--   under the names its callers have always used, so extracting them
--   changed no save API, diagnostic, or gate.
--
--   The durability boundary these primitives implement is documented
--   once, on "World.Save.Storage" (its @Documented durability boundary@
--   section): plain POSIX @fsync@, deliberately not macOS's
--   @F_FULLFSYNC@.
module World.Save.Storage.Durable
    ( -- * Containment
      rejectSymlinkedPath
    , rejectSymlinkedManagedPath
      -- * Durability
    , durableFlush
    , WriteStep(..)
    , writeBytesDurably
    , syncDirectory
      -- * Unique names
    , claimUniquePath
    , isTransientName
      -- * Best-effort helpers
    , closeQuietly
    , removeIfExists
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
import Data.Char (isDigit)
import Control.Exception (IOException, SomeException, try, finally)
import System.Directory (doesFileExist, removeFile, pathIsSymbolicLink)
import System.FilePath (takeDirectory)
import System.IO
    ( Handle, IOMode(..), openBinaryFile, openBinaryTempFile, hFlush
    , hClose )
import System.Posix.IO
    (OpenMode(..), closeFd, defaultFileFlags, handleToFd, openFd)
import System.Posix.Unistd (fileSynchronise)

-- Containment -------------------------------------------------------------

-- | Refuse a single path that is itself a symlink. The one primitive
--   check every containment guard in the save stack is built from, so a
--   symlink is recognised the SAME way everywhere: 'rejectSymlinkedManagedPath'
--   calls it for a managed directory and its parent;
--   "World.Save.Storage"'s @decodeGenerationFile@ calls it for one
--   generation file; the generated-world library calls it for its root,
--   every entry directory, every transient directory and every file it
--   reads. A nonexistent path is not a symlink (nothing to reject); a
--   filesystem error other than "does not exist" while checking is
--   treated the same as "not a symlink" and left for the caller's own
--   next operation to report properly.
rejectSymlinkedPath ∷ FilePath → IO (Either Text ())
rejectSymlinkedPath path = do
    result ← try (pathIsSymbolicLink path)
    pure $ case (result ∷ Either IOException Bool) of
        Right True → Left ("path is a symlink, refusing to operate \
                            \through it: " <> T.pack path)
        _          → Right ()

-- | Refuse to operate through a managed directory that is itself a
--   symlink, OR whose immediate parent is (issue #762 requirement 12).
--   In the save stack the managed directory is a slot and the parent is
--   @saves/@; in the generated-world library it is the library root
--   whose parent is the resource root, or an entry whose parent is the
--   library root. A pre-existing symlink at either level would otherwise
--   be silently followed by directory creation, every temporary, every
--   rename and every cleanup alike — publishing into, and deleting
--   recognized-artifact-named files from, wherever it points, outside
--   the resolved resource root entirely.
--
--   Checks the path itself AND its immediate parent, and deliberately
--   NOTHING further up: 'pathIsSymbolicLink' only inspects a path's OWN
--   final component, transparently resolving everything before it
--   (ordinary POSIX @lstat@ semantics), so the resource root itself
--   sitting behind an OS-level symlink (e.g. macOS's @\/tmp@ →
--   @\/private\/tmp@) is a pre-existing, unrelated concern this check
--   must not misfire on.
rejectSymlinkedManagedPath ∷ FilePath → IO (Either Text ())
rejectSymlinkedManagedPath dir = do
    ownSafe ← rejectSymlinkedPath dir
    case ownSafe of
        Left err → pure (Left err)
        Right () → rejectSymlinkedPath (takeDirectory dir)

-- Durability -------------------------------------------------------------

-- | Flush the RTS-level write buffer, then durably sync the underlying
--   file descriptor (POSIX @fsync@) before the written bytes are trusted
--   for anything. 'handleToFd' takes ownership of (and closes) the
--   Haskell-level 'Handle'; the caller must not use @h@ again after this
--   returns, success or failure.
durableFlush ∷ Handle → IO ()
durableFlush h = do
    hFlush h
    fd ← handleToFd h
    fileSynchronise fd `finally` closeFd fd

-- | Which step of 'writeBytesDurably' failed, so a caller can name its
--   own phase for it — the save transaction and the generated-world
--   library both distinguish "bytes never reached the file" from "bytes
--   written but not proven durable".
data WriteStep = StepOpen | StepWrite | StepFlush
    deriving (Show, Eq)

-- | Create-or-truncate @path@, write @bytes@, and 'durableFlush' before
--   returning. The handle is closed on every path out, so a failure
--   never leaks a descriptor, and every failure is returned with the
--   step it happened in rather than thrown. The bytes are NOT re-read
--   here — every transaction built on this re-reads from disk itself,
--   because what it compares the re-read against (an expected metadata,
--   an expected digest) is its own business.
writeBytesDurably ∷ FilePath → BS.ByteString → IO (Either (WriteStep, SomeException) ())
writeBytesDurably path bytes = do
    opened ← try (openBinaryFile path WriteMode)
    case opened of
        Left e  → pure (Left (StepOpen, e))
        Right h → do
            written ← try (BS.hPut h bytes)
            case written of
                Left e → do
                    closeQuietly h
                    pure (Left (StepWrite, e))
                Right () → do
                    flushed ← try (durableFlush h)
                    pure (either (\e → Left (StepFlush, e)) Right flushed)

-- | Durably sync a DIRECTORY, so the entries a preceding rename created
--   or replaced are themselves on disk — a file's own @fsync@ says
--   nothing about the directory entry naming it.
syncDirectory ∷ FilePath → IO ()
syncDirectory dir = do
    fd ← openFd dir ReadOnly defaultFileFlags
    fileSynchronise fd `finally` closeFd fd

-- Unique names -----------------------------------------------------------

-- | Atomically claim a filesystem-unique path under @dir@ named
--   @template@ + a generated numeric suffix, without leaving a file
--   behind at it — 'openBinaryTempFile' is the only portable way this
--   codebase has to generate a collision-free name; the briefly-created
--   placeholder is closed and removed immediately so the caller can
--   rename an EXISTING file onto the now-free name. This leaves a narrow
--   (microsecond) window where another process could claim the identical
--   name first, same as any "reserve a name" dance without a dedicated
--   atomic rename-to-fresh-name primitive; a collision surfaces as an
--   ordinary rename failure in the caller, never silent corruption.
--
--   The template must contain no @.@: 'openBinaryTempFile' treats a
--   template's LAST dot as an extension to preserve and inserts its
--   generated suffix BEFORE it, whereas a dot-free template keeps the
--   generated suffix trailing — which is what lets 'isTransientName'
--   recognise (and only ever recognise) names this primitive produced.
claimUniquePath ∷ FilePath → String → IO FilePath
claimUniquePath dir template = do
    (path, h) ← openBinaryTempFile dir template
    hClose h
    removeFile path
    pure path

-- | True iff @name@ is exactly @template@ immediately followed by at
--   least one digit — 'openBinaryTempFile''s own naming convention for a
--   dot-free template (a numeric suffix, optionally @-N@ on retry, always
--   digit-first). Digit-anchored so a cleanup can never sweep an
--   unrelated file that merely shares the prefix (e.g. a player's own
--   note file dropped in the directory).
isTransientName ∷ String → String → Bool
isTransientName template name = case L.stripPrefix template name of
    Just (c : _) → isDigit c
    _            → False

-- Best-effort helpers ----------------------------------------------------

closeQuietly ∷ Handle → IO ()
closeQuietly h = do
    r ← try (hClose h)
    case (r ∷ Either SomeException ()) of _ → pure ()

-- | Remove a FILE if it exists, reporting (never throwing) a failure —
--   for post-durability-boundary sweeps, where a leftover is dead weight
--   worth a warning but never worth failing the transaction over.
removeIfExists ∷ FilePath → IO [Text]
removeIfExists path = do
    exists ← doesFileExist path
    if not exists then pure [] else do
        r ← try (removeFile path)
        case r of
            Right () → pure []
            Left (e ∷ IOException) →
                pure [ "failed to remove stale artifact " <> T.pack path
                     <> ": " <> T.pack (show e) ]
