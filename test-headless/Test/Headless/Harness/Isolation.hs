-- | Filesystem isolation for headless specs that drive PRODUCTION code
--   paths which WRITE into @config/@ (#1357).
--
--   #1266 requires that running the tests never modifies, truncates or
--   regenerates the developer's real @config/*.local.yaml@ files. Three
--   examples across 'Test.Headless.UI.ResponsiveMenus' and
--   'Test.Headless.UI.ResponsiveGameplay' violated that: they drive the
--   real @scripts/settings_menu.lua@ Defaults action, which by contract
--   is write-through — it calls @engine.loadDefaultKeybinds()@ and then
--   @engine.saveKeybinds()@, and
--   'Engine.Scripting.Lua.API.Keybinds.saveKeybindsFn' persists to the
--   cwd-relative path @config/keybinds.local.yaml@. Every assertion
--   passed while the developer's bindings were silently replaced.
--
--   The fix is a boundary, not a behavior change: those specs run with
--   the process working directory pointed at a scratch RESOURCE ROOT, so
--   every cwd-relative write the production path performs lands in a
--   temporary directory that is thrown away afterwards. The player-facing
--   keybinding contract is untouched — Defaults still resets to factory
--   bindings and still persists them immediately.
--
--   The scratch root mirrors the checkout entry-for-entry:
--
--     * every top-level entry EXCEPT @config@ is a SYMLINK to the real
--       one, so @scripts/@, @assets/@ and @data/@ resolve exactly as they
--       do from the checkout and nothing is copied;
--     * @config/@ is a real, fixture-OWNED directory holding a copy of
--       the checkout's, because that is the one family production code
--       writes into. Symlinking it would defeat the whole point:
--       @saveKeybindsFn@ writes through the link straight back into the
--       checkout.
--
--   Isolation is established BEFORE the engine boots, not after: engine
--   initialization is itself a writer ('Engine.Core.Init.migrateLegacyConfig'
--   can materialize an absent @config/keybinds.local.yaml@ or
--   @config/video.local.yaml@ from the tracked pre-#786 files, and
--   'Engine.Asset.YamlNotifications.loadOverrides' materializes an absent
--   @config/notifications.local.yaml@), so a fixture that only intervened
--   after 'Test.Headless.Harness.withHeadlessEngine' returned would
--   already be too late. Wrap 'withIsolatedResourceRoot' AROUND
--   'Test.Headless.Harness.withHeadlessEngine', never inside it.
--
--   Nothing here ever writes, moves, backs up or restores a
--   checkout-owned file: the checkout is only ever READ, so there is no
--   window in which a crash could leave the developer's state damaged.
--
--   Two properties carry that promise, and both exist because a fixture
--   whose whole job is not to delete the wrong thing has to be built so
--   it cannot (PR #1373 review):
--
--     * The scratch root is created FRESH and EXCLUSIVELY per
--       invocation, under a random name, with 'createDirectory' — which
--       fails if anything already occupies the path, a symlink
--       included. Teardown therefore only ever removes a directory this
--       module itself made. A fixed, predictable path would not be
--       enough: @doesDirectoryExist@ follows symlinks, so a pre-existing
--       symlink sitting at a fixed scratch path would have had its
--       TARGET's children enumerated and recursively deleted.
--     * "Am I isolated?" is answered from fixture-OWNED state
--       ('isInsideIsolatedResourceRoot' — the active root this module
--       recorded, checked against the real working directory), never
--       from a marker file on disk. A file cannot be created anywhere
--       that makes the checkout look like a scratch root, so neither
--       the nesting check nor the suites' own guards can be spoofed
--       into skipping isolation.
module Test.Headless.Harness.Isolation
  ( withIsolatedResourceRoot
  , isInsideIsolatedResourceRoot
  ) where

import UPrelude
import Control.Exception (IOException, bracket, bracket_, try)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Directory
  ( canonicalizePath, copyFile, createDirectory, createDirectoryIfMissing
  , createDirectoryLink, createFileLink, doesDirectoryExist
  , getCurrentDirectory, getTemporaryDirectory, listDirectory
  , pathIsSymbolicLink, removeDirectory, removeDirectoryLink
  , removeDirectoryRecursive, removeFile, withCurrentDirectory )
import System.FilePath ((</>))
import System.IO.Error (isAlreadyExistsError)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random as Random

-- | The one resource family production code WRITES into, and therefore
--   the one the scratch root owns outright instead of symlinking.
configDirName ∷ FilePath
configDirName = "config"

-- | The scratch root currently in effect, or 'Nothing' outside the
--   fixture. This module is its only writer, which is exactly why it is
--   trustworthy: unlike a marker file, nothing a spec or a checkout can
--   create is able to forge it. The suite runs its examples
--   sequentially, so a plain 'IORef' is sufficient.
activeIsolatedRootRef ∷ IORef (Maybe FilePath)
activeIsolatedRootRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE activeIsolatedRootRef #-}

-- | True when the process is running inside a live scratch resource
--   root — the fixture's recorded root AND the real working directory
--   agreeing. Both halves matter: the recorded root alone would still
--   read True if something had since changed directory out of it.
--
--   This is what the two UI suites assert, so that unwiring their
--   'withIsolatedResourceRoot' wrap fails loudly instead of silently
--   restoring the bug (nothing else in either suite notices).
isInsideIsolatedResourceRoot ∷ IO Bool
isInsideIsolatedResourceRoot = do
    mRoot ← readIORef activeIsolatedRootRef
    case mRoot of
        Nothing   → pure False
        Just root → do
            rootReal ← canonicalizePath root
            hereReal ← canonicalizePath =≪ getCurrentDirectory
            pure (hereReal ≡ rootReal)

-- | Run an action with the process working directory pointed at a fresh
--   scratch resource root (see the module header). The root is removed
--   afterwards whether or not the action succeeded. A nested call is a
--   no-op — it runs the action in the root already in effect rather than
--   building, and later tearing down, a second one.
withIsolatedResourceRoot ∷ IO α → IO α
withIsolatedResourceRoot action = do
    alreadyIsolated ← readIORef activeIsolatedRootRef
    case alreadyIsolated of
      Just _  → action
      Nothing → do
        srcRoot ← getCurrentDirectory
        -- Two nested brackets, not one: the outer guarantees a root that
        -- was created is always removed even if 'populateRoot' throws
        -- half way, and the inner guarantees the recorded active root is
        -- cleared before that removal happens.
        bracket createExclusiveScratchRoot discardRoot $ \root → do
            populateRoot srcRoot root
            bracket_ (writeIORef activeIsolatedRootRef (Just root))
                     (writeIORef activeIsolatedRootRef Nothing)
                     (withCurrentDirectory root action)

-- | Create a brand new scratch root under the system temp directory and
--   return its path.
--
--   'createDirectory' — never 'createDirectoryIfMissing' — is the whole
--   point: it fails with an already-exists error if ANYTHING occupies
--   the path, including a symlink, so a successful return means this
--   call created that directory and 'discardRoot' can only ever remove
--   something this module made. The name is randomized so a stale root
--   from an interrupted run is never adopted, reused, or cleaned.
createExclusiveScratchRoot ∷ IO FilePath
createExclusiveScratchRoot = do
    tmp ← getTemporaryDirectory
    go tmp (0 ∷ Int)
  where
    maxAttempts = 64
    go tmp attempt
      | attempt ≥ maxAttempts = ioError $ userError $
          "withIsolatedResourceRoot: could not create a fresh scratch \
          \resource root under " ⧺ tmp ⧺ " after " ⧺ show maxAttempts
            ⧺ " attempts"
      | otherwise = do
          n ← Random.randomRIO (0 ∷ Int, 999999999)
          let path = tmp </> ("synarchy-headless-isolated-root-" ⧺ show n)
          outcome ← try (createDirectory path)
          case outcome ∷ Either IOException () of
              Right ()               → pure path
              Left e
                | isAlreadyExistsError e → go tmp (attempt + 1)
                | otherwise              → ioError e

-- | Mirror @srcRoot@ into @root@: symlinks for everything but
--   'configDirName', which is copied so it is writable fixture state.
populateRoot ∷ FilePath → FilePath → IO ()
populateRoot srcRoot root = do
    entries ← listDirectory srcRoot
    forM_ entries $ \name → unless (name ≡ configDirName) $ do
        let src = srcRoot </> name
        isDir ← doesDirectoryExist src
        if isDir
          then createDirectoryLink src (root </> name)
          else createFileLink src (root </> name)
    hasConfig ← doesDirectoryExist (srcRoot </> configDirName)
    when hasConfig $
        copyTree (srcRoot </> configDirName) (root </> configDirName)

-- | Recursive plain-file copy. @config/@ holds only files today; the
--   recursion is here so a future subdirectory is carried across rather
--   than silently dropped.
copyTree ∷ FilePath → FilePath → IO ()
copyTree src dst = do
    createDirectoryIfMissing True dst
    names ← listDirectory src
    forM_ names $ \name → do
        isDir ← doesDirectoryExist (src </> name)
        if isDir
          then copyTree (src </> name) (dst </> name)
          else copyFile (src </> name) (dst </> name)

-- | Tear the scratch root down.
--
--   The root itself is never TRAVERSED when it is a symbolic link — it
--   is unlinked and nothing more. That cannot happen given
--   'createExclusiveScratchRoot', which is why the check is cheap
--   insurance rather than a code path with a story: enumerating a link's
--   target here would mean deleting somebody else's directory.
--
--   Inside a real root, every symlink is severed EXPLICITLY, one entry
--   at a time, before anything recursive runs: recursive removal is
--   documented not to follow symbolic links, but the cost of that
--   guarantee not holding here is the developer's whole checkout, so the
--   links are gone before the question can be asked. Only fixture-owned
--   real directories (@config/@, plus anything a spec created relative
--   to the root) are then removed recursively.
discardRoot ∷ FilePath → IO ()
discardRoot root = do
    rootIsLink ← pathIsSymbolicLink root
    if rootIsLink
      then severLink root
      else do
        isDir ← doesDirectoryExist root
        when isDir $ do
            entries ← listDirectory root
            forM_ entries $ \name → removeEntry (root </> name)
            removeDirectory root

-- | Remove one direct child of the scratch root: a symlink is severed
--   without touching its target, a real directory goes recursively, and
--   anything else is a plain file.
removeEntry ∷ FilePath → IO ()
removeEntry path = do
    isLink ← pathIsSymbolicLink path
    if isLink
      then severLink path
      else do
        isDir ← doesDirectoryExist path
        if isDir then removeDirectoryRecursive path else removeFile path

-- | Unlink a symbolic link, never its target. 'removeDirectoryLink' is
--   the portable spelling for a link that resolves to a directory
--   (identical to 'removeFile' on POSIX, distinct on Windows); a
--   dangling link resolves to neither and is a plain unlink.
severLink ∷ FilePath → IO ()
severLink path = do
    pointsAtDir ← doesDirectoryExist path
    if pointsAtDir then removeDirectoryLink path else removeFile path
