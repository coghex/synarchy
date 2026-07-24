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
    ( doesDirectoryExist, doesFileExist, listDirectory, canonicalizePath )
import System.FilePath
    ( (</>), isAbsolute, splitDirectories, takeExtension, pathSeparator )
import Engine.Core.Types (PreviewEntry(..))

-- | Every reason a requested @--preview \<simple category\>/\<item\>@
--   target can fail to resolve, all rejected BEFORE 'App.Preview.runPreview'
--   ever creates a window (Requirement 4).
data FocusError
  = FocusEscapesRoot
  -- ^ Absolute path, a @..@ component, or (after canonicalization, which
  --   also catches a symlink escape) resolves outside the category root.
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
    "item path must stay within the category (no absolute paths, \
    \\"..\" components, or symlink escapes)"
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
--   @/@-separated.
walkFiles ∷ FilePath → [String] → IO [[String]]
walkFiles root segs = do
    let dir = foldl' (</>) root segs
    names ← listDirectory dir
    fmap concat $ forM names $ \name → do
        let segs' = segs ⧺ [name]
        isDir ← doesDirectoryExist (foldl' (</>) root segs')
        if isDir
            then walkFiles root segs'
            else pure [segs']

-- | Validate and resolve a requested @item@ path against 'root' (e.g.
--   'textureCategoryRoot' of a simple category): reject an absolute path
--   or a @..@ component before ever touching the filesystem, then
--   require the candidate exist as a regular, supported-extension file
--   whose canonical path stays under 'root'\'s own canonical path
--   (catches a symlink escape that a plain string-prefix check would
--   miss).
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

-- | Path-boundary-aware prefix check: 'root' must be either exactly
--   'candidate' or followed immediately by a path separator, so
--   @assets/textures/icons@ doesn't spuriously "contain"
--   @assets/textures/iconsEvil/x.png@.
isPathPrefixOf ∷ FilePath → FilePath → Bool
isPathPrefixOf root candidate =
    root ≡ candidate ∨ (root ⧺ [pathSeparator]) `isPrefixOf` candidate
