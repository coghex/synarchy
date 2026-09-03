-- | Recursive asset-file discovery for the data trees whose contents
--   may be organized into logical subdirectories (#1232).
--
--   Deliberately NOT a replacement for @engine.listFiles@
--   ('Engine.Scripting.Lua.API.Core.listFilesFn'), which stays flat and
--   OS-ordered for every one of its callers: a family whose contents are
--   NOT organized into subdirectories has no tree to walk, and recursing
--   on its behalf would change which files it discovers. A flat family
--   that needs a deterministic ORDER — @data\/flora@, whose sequential
--   @FloraId@s a save's numeric references name — applies
--   @startupLoader.canonicalFileOrder@ at its own call site (#2241)
--   rather than moving here.
--
--   Ordering is NOT this module's business. The walk hands back
--   whatever order the filesystem enumerated, exactly as 'listDirectory'
--   does, so the caller that needs determinism applies its own total
--   order to the returned list — which is what makes that order a
--   testable transformation over an enumerated path list rather than
--   something buried inside an opaque recursion.
module Engine.Asset.Discovery
    ( walkFilesWithExtension
    ) where

import UPrelude
import System.Directory
    (doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath ((</>), takeExtension)

-- | Every file under @root@ whose extension is exactly @ext@ (the same
--   case-sensitive @takeExtension f ≡ ext@ predicate @engine.listFiles@
--   applies — recursion changes the DEPTH of the walk, not which files
--   it accepts), as paths relative to @root@ and joined with @\/@ at
--   every depth so a caller's ordering and diagnostics are
--   platform-independent.
--
--   @[]@ — never an error — when @root@ does not exist, matching
--   @engine.listFiles@'s "nothing here" answer for a missing directory.
--
--   A symlink, file or directory, is skipped entirely the moment it is
--   encountered at any depth: neither returned nor recursed into. Same
--   rule, for the same two reasons, as
--   'Engine.Preview.Discovery.discoverEntries' — nothing under a data
--   tree needs one, and skipping unconditionally (rather than
--   canonicalizing and checking containment) both guarantees the walk
--   terminates on any tree shape and guarantees it never reaches a file
--   outside @root@.
walkFilesWithExtension ∷ FilePath → String → IO [FilePath]
walkFilesWithExtension root ext = do
    exists ← doesDirectoryExist root
    if not exists then pure [] else go []
  where
    go segs = do
        names ← listDirectory (foldl' (</>) root segs)
        fmap concat $ forM names $ \name → do
            let segs' = segs ⧺ [name]
                full  = foldl' (</>) root segs'
            isLink ← pathIsSymbolicLink full
            if isLink then pure [] else do
                isDir ← doesDirectoryExist full
                if isDir
                    then go segs'
                    else pure [ joinSlash segs' | takeExtension name ≡ ext ]
    joinSlash = foldr1 (\a b → a ⧺ "/" ⧺ b)
