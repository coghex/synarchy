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
  , ItemDirError(..)
  , itemDirErrorMessage
  , textureCategoryRoot
  , isSupportedTextureFile
  , sortEntries
  , sortFrameFiles
  , discoverEntries
  , resolveFocusedEntry
  , resolveItemDir
  ) where

import UPrelude
import qualified Data.Text as T
import Data.Char (isDigit, toLower)
import Data.List (sortBy, sortOn, isPrefixOf)
import Data.Ord (comparing)
import System.Directory
    ( doesDirectoryExist, doesFileExist, listDirectory, canonicalizePath
    , pathIsSymbolicLink )
import System.FilePath
    ( (</>), isAbsolute, splitDirectories, takeExtension, pathSeparator
    , dropExtension )
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

-- | Every reason a requested GROUPED-category item directory
--   (@--preview \<grouped category\>/\<item\>@ — a unit, building, flora
--   species, or structure pack) can fail to resolve, all rejected
--   BEFORE 'App.Preview.runPreview' ever creates a window (#888
--   Requirement 3). The grouped counterpart of 'FocusError' above: an
--   item there names a FILE within the category, an item here names one
--   contained, non-symlinked direct child DIRECTORY of it.
data ItemDirError
  = ItemDirEscapesRoot
  -- ^ Empty, absolute, carries a path separator, is @.@\/@..@, or (after
  --   canonicalization — a defensive final check) resolves outside the
  --   category root.
  | ItemDirSymlink
  -- ^ The item directory is a symlink — refused unconditionally (not
  --   just an escaping one), the same rule 'walkFiles' applies to every
  --   entry it walks past. 'doesDirectoryExist' follows links, so a
  --   symlinked item would otherwise browse (and load textures from)
  --   another tree entirely, breaking the trimmed-loading contract.
  | ItemDirNotFound
  -- ^ No such entry under the category root (or the root itself is
  --   missing). A dangling symlink reports here too — there is no item
  --   behind it.
  | ItemDirNotADirectory
  -- ^ The name resolves to a regular file, not a browsable item
  --   directory (e.g. @assets\/textures\/flora\/unknown_flora.png@, the
  --   per-category fallback texture sitting beside the real items).
  deriving (Eq, Show)

itemDirErrorMessage ∷ ItemDirError → Text
itemDirErrorMessage ItemDirEscapesRoot =
    "item must be a single directory name under the category root (no \
    \absolute paths, path separators, or \"..\" components)"
itemDirErrorMessage ItemDirSymlink =
    "item directory must not be a symlink"
itemDirErrorMessage ItemDirNotFound = "no such item in this category"
itemDirErrorMessage ItemDirNotADirectory =
    "is a file, not a browsable item directory"

-- | @assets/textures/\<category\>@ — the canonical category root, for
--   simple categories (browsed recursively) and grouped ones
--   (whose direct children are the browsable items) alike.
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

-- | Order a directory's @frame_NNN.png@ files NUMERICALLY, not
--   lexicographically: the shipped names are zero-padded so the two
--   agree today, but an unpadded @frame_10.png@ must not sort before
--   @frame_2.png@. Files whose stem carries no trailing digits sort
--   after the numbered ones, by name, so nothing is silently dropped.
--   Shared by the units viewer ('Engine.Preview.Unit', which re-exports
--   it) and the buildings viewer ('Engine.Preview.Building') so the two
--   can never disagree about frame order.
sortFrameFiles ∷ [FilePath] → [FilePath]
sortFrameFiles = sortOn key
  where
    key f = (maybe (1 ∷ Int, 0 ∷ Integer) ((,) 0) (trailingNumber f), f)
    trailingNumber f =
        let digits = reverse (takeWhile isDigit (reverse (dropExtension f)))
        in if null digits then Nothing else Just (read digits)

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

-- | Validate @item@ as exactly ONE contained, non-symlinked direct
--   child DIRECTORY of @root@ (a grouped category's item folder) and
--   return its path — the shared pre-boot containment rule behind every
--   grouped category: @units/\<name\>@ (via
--   'Engine.Preview.Unit.resolveUnitDir', which layers its own
--   @animations\/@ requirement on top), @buildings/\<name\>@ (via
--   'Engine.Preview.Building.buildPreviewBuilding'), and the
--   @flora/\<name\>@ \/ @structures/\<name\>@ targets @app\/Main.hs@
--   routes straight into 'discoverEntries' (#888 Requirement 2 — the
--   simple-category browser, rooted at the item's own folder).
--
--   Structure is rejected before the filesystem is touched at all
--   (absolute, separators, @.@\/@..@, empty), the directory is
--   lstat-checked for symlink-ness, and a final canonicalization
--   confirms containment defensively — the same layered rule
--   'resolveFocusedEntry' applies to a focused simple-category file.
resolveItemDir ∷ FilePath → String → IO (Either ItemDirError FilePath)
resolveItemDir root item
    | null item ∨ isAbsolute item ∨ length (splitDirectories item) ≢ 1
        ∨ item ≡ "." ∨ item ≡ ".." ∨ pathSeparator `elem` item =
        pure (Left ItemDirEscapesRoot)
    | otherwise = do
        let candidate = root </> item
        rootExists ← doesDirectoryExist root
        if not rootExists
            then pure (Left ItemDirNotFound)
            else do
                -- Existence FIRST: 'pathIsSymbolicLink' throws on a path
                -- that isn't there at all, which is exactly the ordinary
                -- "no such item" case.
                isDir ← doesDirectoryExist candidate
                if not isDir
                    then do
                        isFile ← doesFileExist candidate
                        pure (Left (if isFile then ItemDirNotADirectory
                                              else ItemDirNotFound))
                    else do
                        isLink ← pathIsSymbolicLink candidate
                        if isLink
                            then pure (Left ItemDirSymlink)
                            else do
                                canonRoot ← canonicalizePath root
                                canonCand ← canonicalizePath candidate
                                pure $ if canonRoot `isPathPrefixOf` canonCand
                                    then Right candidate
                                    else Left ItemDirEscapesRoot

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
