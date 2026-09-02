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
--     * every top-level entry EXCEPT @config@ and @saves@ is a SYMLINK
--       to the real one, so @scripts/@, @assets/@ and @data/@ resolve
--       exactly as they do from the checkout and nothing is copied;
--     * @config/@ is a real, fixture-OWNED directory holding a copy of
--       the checkout's, because that is the one family production code
--       writes into. Symlinking it would defeat the whole point:
--       @saveKeybindsFn@ writes through the link straight back into the
--       checkout.
--     * @saves/@ is a real, fixture-OWNED directory that starts EMPTY
--       (#2162). It is the other cwd-relative family production code
--       writes into ('World.Save.Serialize.savesDirectory'), and the
--       developer's own is gitignored and may or may not exist — so a
--       spec that plants a save fixture, or drives a production path
--       that publishes one, always lands in a directory this fixture
--       made and never through a link into the developer's saves. It
--       is never copied: a real saves directory can be large, and no
--       spec may depend on what a developer happens to have saved.
--
--   Isolation is established BEFORE the engine boots, not after: engine
--   initialization is itself a writer ('Engine.Core.Init.migrateLegacyConfig'
--   writes @config/keybinds.legacy-neutral.local.yaml@ /
--   @config/video.legacy-neutral.local.yaml@ when it recognizes the tracked
--   pre-#786 files as neutral placeholders, and would materialize
--   @config/keybinds.local.yaml@ / @config/video.local.yaml@ outright
--   from a legacy file carrying real values; and
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
  , withExclusiveTempDirectory
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

-- | The resource family production code WRITES into and READS back, and
--   therefore the one the scratch root owns as a COPY instead of
--   symlinking.
configDirName ∷ FilePath
configDirName = "config"

-- | The other family production code writes into (#2162), owned by the
--   scratch root as a fresh EMPTY directory — see the module header for
--   why it is created rather than copied or linked.
savesDirName ∷ FilePath
savesDirName = "saves"

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
        bracket (createExclusiveTempDirectory scratchRootPrefix)
                discardTempDirectory $ \root → do
            populateRoot srcRoot root
            bracket_ (writeIORef activeIsolatedRootRef (Just root))
                     (writeIORef activeIsolatedRootRef Nothing)
                     (withCurrentDirectory root action)

-- | Run an action with a brand new, exclusively created directory under
--   the system temp directory, removed afterwards.
--
--   This is the one primitive anything in this suite should use for
--   throwaway filesystem state, and 'withIsolatedResourceRoot' is built
--   on it. Its rule — established by the fixture and then applied to the
--   fixture's own tests (PR #1373 review) — is that a cleanup routine
--   may only ever delete a directory the SAME call created. Deleting
--   whatever happens to sit at a predictable path in order to claim it
--   is the bug, not the setup step: that path may hold a stale root from
--   an interrupted run, or somebody else's data entirely.
withExclusiveTempDirectory ∷ String → (FilePath → IO α) → IO α
withExclusiveTempDirectory prefix =
    bracket (createExclusiveTempDirectory prefix) discardTempDirectory

-- | Create a brand new directory named @<prefix>-<random>@ under the
--   system temp directory and return its path.
--
--   'createDirectory' — never 'createDirectoryIfMissing' — is the whole
--   point: it fails with an already-exists error if ANYTHING occupies
--   the path, including a symlink, so a successful return means this
--   call created that directory and 'discardTempDirectory' can only ever
--   remove something this module made. The name is randomized so a stale
--   directory from an interrupted run is never adopted, reused, or
--   cleaned.
createExclusiveTempDirectory ∷ String → IO FilePath
createExclusiveTempDirectory prefix = do
    tmp ← getTemporaryDirectory
    go tmp (0 ∷ Int)
  where
    maxAttempts = 64
    go tmp attempt
      | attempt ≥ maxAttempts = ioError $ userError $
          "withExclusiveTempDirectory: could not create a fresh "
            ⧺ prefix ⧺ " directory under " ⧺ tmp ⧺ " after "
            ⧺ show maxAttempts ⧺ " attempts"
      | otherwise = do
          n ← Random.randomRIO (0 ∷ Int, 999999999)
          let path = tmp </> (prefix ⧺ "-" ⧺ show n)
          outcome ← try (createDirectory path)
          case outcome ∷ Either IOException () of
              Right ()               → pure path
              Left e
                | isAlreadyExistsError e → go tmp (attempt + 1)
                | otherwise              → ioError e

-- | Mirror @srcRoot@ into @root@: symlinks for everything but
--   'configDirName', which is copied so it is writable fixture state,
--   and 'savesDirName', which is created empty so it is writable fixture
--   state that owes nothing to the checkout.
populateRoot ∷ FilePath → FilePath → IO ()
populateRoot srcRoot root = do
    entries ← listDirectory srcRoot
    forM_ entries $ \name →
      unless (name ≡ configDirName ∨ name ≡ savesDirName) $ do
        let src = srcRoot </> name
        isDir ← doesDirectoryExist src
        if isDir
          then createDirectoryLink src (root </> name)
          else createFileLink src (root </> name)
    hasConfig ← doesDirectoryExist (srcRoot </> configDirName)
    when hasConfig $
        copyTree (srcRoot </> configDirName) (root </> configDirName)
    -- 'createDirectory', like the root itself: @root@ was created fresh
    -- and exclusively a moment ago, so anything already at this path
    -- is a bug worth failing on, not something to adopt.
    createDirectory (root </> savesDirName)

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

-- | Name every scratch resource root starts with.
scratchRootPrefix ∷ String
scratchRootPrefix = "synarchy-headless-isolated-root"

-- | Tear one exclusively created temp directory down.
--
--   The directory itself is never TRAVERSED when it is a symbolic link —
--   it is unlinked and nothing more. That cannot happen given
--   'createExclusiveTempDirectory', which is why the check is cheap
--   insurance rather than a code path with a story: enumerating a link's
--   target here would mean deleting somebody else's directory.
--
--   Inside a real directory, every symlink is severed EXPLICITLY, one
--   entry at a time, before anything recursive runs: recursive removal is
--   documented not to follow symbolic links, but the cost of that
--   guarantee not holding here is the developer's whole checkout, so the
--   links are gone before the question can be asked. Only owned real
--   directories (a scratch root's @config/@, plus anything a spec created
--   relative to it) are then removed recursively.
discardTempDirectory ∷ FilePath → IO ()
discardTempDirectory root = do
    rootIsLink ← pathIsSymbolicLink root
    if rootIsLink
      then severLink root
      else do
        isDir ← doesDirectoryExist root
        when isDir $ do
            entries ← listDirectory root
            forM_ entries $ \name → removeEntry (root </> name)
            removeDirectory root

-- | Remove one direct child: a symlink is severed
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
