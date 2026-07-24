-- | Filesystem discovery + containment logic for the @--preview@ simple-
--   category texture browser (#886, Phase 2 of the browser epic #427).
--   Pure labeling/ordering plus the IO directory walk and focused-item
--   containment check that back both the pre-boot CLI contract
--   (@app/Main.hs@ rejects a bad @--preview cat/item@ before ever
--   creating a window) and the in-engine browser
--   (@scripts/preview_manager.lua@ via @engine.getPreviewBrowse()@).
module Engine.Preview.Discovery
  ( FocusError(..)
  , focusErrorMessage
  , textureCategoryRoot
  , isSupportedTextureFile
  , sortEntries
  , discoverEntries
  , resolveFocusedEntry
  ) where

import UPrelude
import qualified Data.Text as T
import Data.Char (toLower)
import Data.List (sortBy, isPrefixOf)
import Data.Ord (comparing)
import System.Directory
    ( doesDirectoryExist, doesFileExist, listDirectory, canonicalizePath
    , pathIsSymbolicLink )
import System.FilePath
    ( (</>), isAbsolute, splitDirectories, takeExtension, pathSeparator )
import Engine.Core.Types (PreviewEntry(..))

-- | Every reason a requested @--preview \<simple category\>/\<item\>@
--   target can fail to resolve, all rejected BEFORE 'App.Preview.runPreview'
--   ever creates a window (Requirement 4).
data FocusError
  = FocusEscapesRoot
  -- ^ Absolute path, a @..@ component, or (after canonicalization — a
  --   final defensive check, since a real escape should already be
  --   impossible once 'FocusSymlink' rejects every symlink) resolves
  --   outside the category root.
  | FocusSymlink
  -- ^ The item path, or one of its ancestor directories, is a symlink —
  --   rejected unconditionally (not just an escaping one) so a bare
  --   category listing ('discoverEntries', which skips every symlink
  --   the identical way) and a typed-out item target
  --   ('resolveFocusedEntry') can never disagree about the same path.
  | FocusNotFound
  -- ^ No such file (or the category root itself doesn't exist).
  | FocusNotAFile
  -- ^ The requested path names a directory, not a file.
  | FocusUnsupportedExtension
  -- ^ Not a @.png@ — the one supported texture extension (every existing
  --   simple-category asset in this repo already is one; see
  --   'isSupportedTextureFile').
  deriving (Eq, Show)

focusErrorMessage ∷ FocusError → Text
focusErrorMessage FocusEscapesRoot =
    "item path must stay within the category (no absolute paths or \
    \\"..\" components)"
focusErrorMessage FocusSymlink =
    "item path must not pass through a symlink (neither the item itself \
    \nor any of its ancestor directories)"
focusErrorMessage FocusNotFound = "no such texture"
focusErrorMessage FocusNotAFile = "is a directory, not a texture file"
focusErrorMessage FocusUnsupportedExtension =
    "unsupported file type (expected .png)"

-- | @assets/textures/\<category\>@ — the canonical simple-category root.
textureCategoryRoot ∷ String → FilePath
textureCategoryRoot cat = "assets" </> "textures" </> cat

-- | The ONE supported browsable texture extension. Every file under
--   @assets/textures/{icons,items,ui,world}@ is already a @.png@, so
--   discovery and focused-item resolution apply the identical rule and
--   can never disagree about whether a given path is browsable (a
--   displayed entry's label always resolves back to itself as an item
--   target, extension included — Requirement 4's "documented extension
--   rule" amendment).
isSupportedTextureFile ∷ FilePath → Bool
isSupportedTextureFile p = map toLower (takeExtension p) ≡ ".png"

-- | Deterministic ordering (Requirement 3): lexicographic, case-sensitive
--   'Ord' on the category-relative label string.
sortEntries ∷ [PreviewEntry] → [PreviewEntry]
sortEntries = sortBy (comparing peLabel)

-- | Recursively discover every supported texture under 'root' (e.g.
--   'textureCategoryRoot' of a simple category), labeled by the @/@-
--   separated path relative to 'root' (platform-independent regardless
--   of host path-separator conventions), in 'sortEntries' order. Empty
--   (never an error) if 'root' doesn't exist or holds no supported
--   textures.
discoverEntries ∷ FilePath → IO [PreviewEntry]
discoverEntries root = do
    exists ← doesDirectoryExist root
    if not exists
        then pure []
        else do
            paths ← walkFiles root []
            let entries =
                    [ mkEntry segs
                    | segs ← paths
                    , isSupportedTextureFile (last segs)
                    ]
            pure (sortEntries entries)
  where
    mkEntry segs = PreviewEntry
        { peLabel = T.pack (joinSlash segs)
        , pePath  = T.pack (foldl' (</>) root segs)
        }
    joinSlash = foldr1 (\a b → a ⧺ "/" ⧺ b)

-- | Every file under 'root', as the list of path segments (relative to
--   'root') needed to reach it — never mixed with 'System.FilePath'
--   combinators past this point so the eventual label is guaranteed
--   @/@-separated. A symlink (file or directory) is skipped entirely —
--   neither listed as an entry nor recursed into — the moment it's
--   encountered, at whatever depth: no browsable use case needs one,
--   and skipping unconditionally (rather than canonicalizing and
--   checking containment) also rules out a symlinked-directory cycle
--   recursing indefinitely.
walkFiles ∷ FilePath → [String] → IO [[String]]
walkFiles root segs = do
    let dir = foldl' (</>) root segs
    names ← listDirectory dir
    fmap concat $ forM names $ \name → do
        let segs' = segs ⧺ [name]
            full  = foldl' (</>) root segs'
        isLink ← pathIsSymbolicLink full
        if isLink
            then pure []
            else do
                isDir ← doesDirectoryExist full
                if isDir
                    then walkFiles root segs'
                    else pure [segs']

-- | Validate and resolve a requested @item@ path against 'root' (e.g.
--   'textureCategoryRoot' of a simple category): reject an absolute path
--   or a @..@ component before ever touching the filesystem, then
--   require the candidate exist as a regular, supported-extension,
--   non-symlinked file (checked at every path level — see
--   'anySegmentIsSymlink' — so a symlinked ANCESTOR directory is
--   rejected exactly like 'walkFiles' would have made it invisible to
--   discovery, not just a symlinked leaf file) whose canonical path
--   still stays under 'root'\'s own canonical path as a final defensive
--   check.
resolveFocusedEntry ∷ FilePath → String → IO (Either FocusError PreviewEntry)
resolveFocusedEntry root item
    | isAbsolute item ∨ ".." `elem` splitDirectories item =
        pure (Left FocusEscapesRoot)
    | otherwise = do
        let candidate = root </> item
        rootExists ← doesDirectoryExist root
        if not rootExists
            then pure (Left FocusNotFound)
            else do
                isFile ← doesFileExist candidate
                if not isFile
                    then do
                        isDir ← doesDirectoryExist candidate
                        pure (Left (if isDir then FocusNotAFile else FocusNotFound))
                    else do
                        -- Safe to lstat every level now: 'doesFileExist'
                        -- already proved the full chain resolves to a
                        -- real file, so every ancestor along it exists.
                        hasSymlink ← anySegmentIsSymlink root (splitDirectories item)
                        if hasSymlink
                            then pure (Left FocusSymlink)
                            else if not (isSupportedTextureFile candidate)
                                then pure (Left FocusUnsupportedExtension)
                                else do
                                    canonRoot ← canonicalizePath root
                                    canonCandidate ← canonicalizePath candidate
                                    if canonRoot `isPathPrefixOf` canonCandidate
                                        then pure (Right PreviewEntry
                                            { peLabel = T.pack item
                                            , pePath  = T.pack candidate
                                            })
                                        else pure (Left FocusEscapesRoot)

-- | True if 'root' followed by any prefix of 'segs' (checked
--   incrementally, root-outward) is itself a symlink — every ancestor
--   directory as well as the final leaf, so a symlinked directory
--   further up the chain can't smuggle a file discovery would never
--   have reached (walkFiles skips a symlinked directory the moment it's
--   encountered, at whatever depth; this mirrors that one level at a
--   time instead of jumping straight to the final candidate path).
anySegmentIsSymlink ∷ FilePath → [String] → IO Bool
anySegmentIsSymlink root = go root
  where
    go _ [] = pure False
    go acc (s:rest) = do
        let acc' = acc </> s
        isLink ← pathIsSymbolicLink acc'
        if isLink then pure True else go acc' rest

-- | Path-boundary-aware prefix check: 'root' must be either exactly
--   'candidate' or followed immediately by a path separator, so
--   @assets/textures/icons@ doesn't spuriously "contain"
--   @assets/textures/iconsEvil/x.png@.
isPathPrefixOf ∷ FilePath → FilePath → Bool
isPathPrefixOf root candidate =
    root ≡ candidate ∨ (root ⧺ [pathSeparator]) `isPrefixOf` candidate
