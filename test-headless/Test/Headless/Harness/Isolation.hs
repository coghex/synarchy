{-# LANGUAGE ScopedTypeVariables #-}
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
module Test.Headless.Harness.Isolation
  ( withIsolatedResourceRoot
  , isolationMarkerName
  ) where

import UPrelude
import Control.Exception (finally)
import System.Directory
  ( copyFile, createDirectoryIfMissing, createDirectoryLink, createFileLink
  , doesDirectoryExist, doesFileExist, getCurrentDirectory
  , getTemporaryDirectory, listDirectory, pathIsSymbolicLink, removeDirectory
  , removeDirectoryLink, removeDirectoryRecursive, removeFile
  , withCurrentDirectory )
import System.FilePath ((</>))

-- | The one resource family production code WRITES into, and therefore
--   the one the scratch root owns outright instead of symlinking.
configDirName ∷ FilePath
configDirName = "config"

-- | Dropped at the scratch root so a nested 'withIsolatedResourceRoot'
--   is a no-op instead of tearing down the root the outer call is
--   standing in. No layout in this suite nests today; the guard is here
--   because the failure mode if one ever does would be silent and
--   confusing rather than a compile error.
isolationMarkerName ∷ FilePath
isolationMarkerName = ".synarchy-isolated-resource-root"

-- | The scratch root's location. A single fixed path under the system
--   temp directory, wiped before and after use — the same convention
--   'Test.Headless.Core.ConfigState' uses, and safe for the same reason:
--   this suite runs its examples sequentially.
isolatedRootPath ∷ IO FilePath
isolatedRootPath = do
    tmp ← getTemporaryDirectory
    pure (tmp </> "synarchy-headless-isolated-root")

-- | Run an action with the process working directory pointed at a fresh
--   scratch resource root (see the module header). The root is removed
--   afterwards whether or not the action succeeded.
withIsolatedResourceRoot ∷ IO α → IO α
withIsolatedResourceRoot action = do
    nested ← doesFileExist isolationMarkerName
    if nested
      then action
      else do
        srcRoot ← getCurrentDirectory
        root    ← isolatedRootPath
        discardRoot root
        populateRoot srcRoot root
        withCurrentDirectory root action `finally` discardRoot root

-- | Mirror @srcRoot@ into @root@: symlinks for everything but
--   'configDirName', which is copied so it is writable fixture state.
populateRoot ∷ FilePath → FilePath → IO ()
populateRoot srcRoot root = do
    createDirectoryIfMissing True root
    writeFile (root </> isolationMarkerName) $
        "Scratch resource root for the headless test suite (#1357).\n\
        \Mirrors " ⧺ srcRoot ⧺ " with a fixture-owned config/.\n"
    entries ← listDirectory srcRoot
    -- 'isolationMarkerName' is skipped alongside @config@ purely so a
    -- checkout that happens to carry that name cannot collide with the
    -- marker just written above.
    forM_ entries $ \name →
      unless (name ≡ configDirName ∨ name ≡ isolationMarkerName) $ do
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

-- | Tear the scratch root down. Every symlink is severed EXPLICITLY,
--   one entry at a time, before anything recursive runs: recursive
--   removal is documented not to follow symbolic links, but the cost of
--   that guarantee not holding here is the developer's whole checkout,
--   so the links are gone before the question can be asked. Only
--   fixture-owned real directories (@config/@, plus anything a spec
--   created relative to the root) are then removed recursively.
discardRoot ∷ FilePath → IO ()
discardRoot root = do
    exists ← doesDirectoryExist root
    when exists $ do
        entries ← listDirectory root
        forM_ entries $ \name → removeEntry (root </> name)
        removeDirectory root
  where
    removeEntry path = do
        isLink ← pathIsSymbolicLink path
        isDir  ← doesDirectoryExist path
        if isLink
          then if isDir then removeDirectoryLink path else removeFile path
          else if isDir then removeDirectoryRecursive path else removeFile path
