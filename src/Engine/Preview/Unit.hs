{-# LANGUAGE OverloadedStrings #-}
-- | Filesystem discovery + playback resolution for the @--preview
--   units/\<name\>@ animation viewer (#887, Phase 3 of the browser epic
--   #427). Companion to 'Engine.Preview.Discovery' (which owns the
--   simple-category texture browser) and, like it, runs entirely
--   BEFORE @App.Preview.runPreview@ ever creates a window: a bad unit
--   name is a pre-boot exit, and the resolved 'PreviewUnit' rides into
--   the engine as 'Engine.Core.Types.PreviewBrowse' for
--   @scripts/preview_manager.lua@ to render.
--
--   Authority split (#887 review amendment): the unit's own asset tree
--   @assets\/textures\/units\/\<name\>\/animations\/@ is authoritative
--   for WHICH animations exist, WHICH directions each has, and the
--   frame ORDER within a direction. @data\/units\/\<name\>.yaml@ only
--   AUGMENTS a matching animation with playback metadata
--   (@fps@\/@loop@\/@flip@). Three shipped animation folders
--   (@acolyte\/pushing_idle@, @bear_brown\/roar@,
--   @technomule\/hit_react@) have no YAML entry at all, three asset
--   units have no YAML file at all, and the bear's @run@\/@walk@
--   folders hold frame 008 files the YAML omits — the viewer shows all
--   of them.
module Engine.Preview.Unit
  ( UnitFocusError(..)
  , unitFocusErrorMessage
  , unitsCategoryRoot
  , unitDataPath
  , resolveUnitDir
  , directionDirName
  , parseDirectionDirName
  , previewDirectionOrder
  , sortFrameFiles
  , defaultAnimationName
  , effectiveFlip
  , resolveAnimDirections
  , frameIndexAt
  , loadUnitAnimMeta
  , discoverUnitAnimations
  , buildPreviewAnims
  , buildPreviewUnit
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Control.Monad (filterM)
import Data.Char (isDigit, toLower)
import Data.List (sortOn, sort, isPrefixOf, find)
import Data.Maybe (mapMaybe)
import System.Directory
    ( doesDirectoryExist, doesFileExist, listDirectory, canonicalizePath
    , pathIsSymbolicLink )
import System.FilePath
    ( (</>), (<.>), isAbsolute, splitDirectories, takeExtension
    , pathSeparator, dropExtension )
import Engine.Asset.YamlUnits (UnitYamlAnim(..))
import Engine.Core.Types (PreviewUnit(..), PreviewAnim(..), PreviewFrameDir(..))
import Unit.Direction (Direction(..), mirrorDir)

-- | Every reason a requested @--preview units/\<name\>@ target can fail
--   to resolve. All of these reject BEFORE a window is ever created,
--   joining @Engine.Preview.Discovery@'s 'FocusError' set at the same
--   pre-boot CLI exit.
data UnitFocusError
  = UnitNameEscapesRoot
  -- ^ Absolute, empty, contains a path separator, or is @.@\/@..@ —
  --   a unit target is exactly ONE direct child of the units root, not
  --   a path, so anything with structure is refused before the
  --   filesystem is touched at all.
  | UnitNameSymlink
  -- ^ The unit directory is a symlink — refused unconditionally (not
  --   just an escaping one), the same rule
  --   'Engine.Preview.Discovery.walkFiles' applies to every entry it
  --   walks past.
  | UnitNotFound
  -- ^ No such directory under @assets\/textures\/units@ (the
  --   @units\/nosuch@ case), or the units root itself is missing.
  | UnitNoAnimations
  -- ^ The unit directory exists but has no @animations\/@ subtree, so
  --   there is nothing for the viewer to show.
  deriving (Eq, Show)

unitFocusErrorMessage ∷ UnitFocusError → Text
unitFocusErrorMessage UnitNameEscapesRoot =
    "unit name must be a single directory name under assets/textures/units \
    \(no absolute paths, path separators, or \"..\" components)"
unitFocusErrorMessage UnitNameSymlink =
    "unit directory must not be a symlink"
unitFocusErrorMessage UnitNotFound = "no such unit"
unitFocusErrorMessage UnitNoAnimations =
    "unit has no animations/ directory"

-- | @assets\/textures\/units@ — the grouped units category root.
unitsCategoryRoot ∷ FilePath
unitsCategoryRoot = "assets" </> "textures" </> "units"

-- | @data\/units\/\<name\>.yaml@ — the ONE gameplay YAML the units
--   viewer is allowed to read (Requirement 6's trimmed-boot contract:
--   never the full unit catalog, never any other @data\/*.yaml@).
unitDataPath ∷ Text → FilePath
unitDataPath name = "data" </> "units" </> T.unpack name <.> "yaml"

-- | The folder-name spelling of each direction, identical to the
--   @unknownUnitDirName@ vocabulary the shipped assets already use.
directionDirName ∷ Direction → Text
directionDirName DirS  = "south"
directionDirName DirSW = "south-west"
directionDirName DirW  = "west"
directionDirName DirNW = "north-west"
directionDirName DirN  = "north"
directionDirName DirNE = "north-east"
directionDirName DirE  = "east"
directionDirName DirSE = "south-east"

-- | Inverse of 'directionDirName', accepting the same short/long
--   spellings @Engine.Scripting.Lua.API.Units.Yaml.parseDirKey@ does so
--   an asset folder can be named either way.
parseDirectionDirName ∷ Text → Maybe Direction
parseDirectionDirName t = case T.toLower t of
    "s"          → Just DirS
    "sw"         → Just DirSW
    "w"          → Just DirW
    "nw"         → Just DirNW
    "n"          → Just DirN
    "ne"         → Just DirNE
    "e"          → Just DirE
    "se"         → Just DirSE
    "south"      → Just DirS
    "south-west" → Just DirSW
    "west"       → Just DirW
    "north-west" → Just DirNW
    "north"      → Just DirN
    "north-east" → Just DirNE
    "east"       → Just DirE
    "south-east" → Just DirSE
    _            → Nothing

-- | The game's own compass order (@Unit.Direction.allDirections@:
--   clockwise from South) — the left-to-right order the viewer's
--   direction row and the introspection dump both use.
previewDirectionOrder ∷ [Direction]
previewDirectionOrder = [minBound .. maxBound]

-- | The canonical five-direction eastern-half layout: exactly what a
--   bilaterally-symmetric animation ships when it relies on mirroring.
canonicalFiveDirections ∷ [Direction]
canonicalFiveDirections = [DirS, DirSE, DirE, DirNE, DirN]

-- | Order a direction's @frame_NNN.png@ files NUMERICALLY, not
--   lexicographically: the shipped names are zero-padded so the two
--   agree today, but an unpadded @frame_10.png@ must not sort before
--   @frame_2.png@. Files whose stem carries no trailing digits sort
--   after the numbered ones, by name, so nothing is silently dropped.
sortFrameFiles ∷ [FilePath] → [FilePath]
sortFrameFiles = sortOn key
  where
    key f = (maybe (1 ∷ Int, 0 ∷ Integer) ((,) 0) (trailingNumber f), f)
    trailingNumber f =
        let digits = reverse (takeWhile isDigit (reverse (dropExtension f)))
        in if null digits then Nothing else Just (read digits)

-- | Default selection (Requirement 2, made deterministic by the review
--   amendment): @idle@ when the unit has one, otherwise the first name
--   in the SAME case-sensitive lexicographic order the list itself
--   uses. 'Nothing' only when there are no animations at all.
defaultAnimationName ∷ [Text] → Maybe Text
defaultAnimationName names
    | "idle" `elem` names = Just "idle"
    | otherwise           = listToMaybe names

-- | Whether western directions may mirror their eastern counterparts.
--
--   A YAML entry is authoritative — its @flip@ field (default 'False',
--   see 'Engine.Asset.YamlUnits.UnitYamlAnim') decides, exactly as it
--   does for the live game renderer.
--
--   With NO YAML entry (a missing @data\/units\/\<name\>.yaml@, or an
--   animation folder the YAML never mentions) the viewer INFERS
--   mirroring, but only for the exact canonical five-direction layout
--   @{S, SE, E, NE, N}@ — the unambiguous signal that the author shipped
--   the eastern half and expected the renderer to supply the rest. Any
--   other stored set (all eight already authored, or a partial set) is
--   left alone: its missing directions stay unavailable rather than
--   being invented.
effectiveFlip ∷ Maybe UnitYamlAnim → Map.Map Direction [Text] → Bool
effectiveFlip (Just anim) _ = uyaFlip anim
effectiveFlip Nothing stored =
    -- 'Map.keys' is ascending-'Ord'; sort the reference set the same
    -- way rather than relying on how it happens to be written above.
    Map.keys stored ≡ sort canonicalFiveDirections

-- | Resolve the eight display cells against the stored per-direction
--   frames, in 'previewDirectionOrder'.
--
--   A directly-authored direction ALWAYS wins (never mirrored even when
--   flipping is enabled) — the same precedence
--   'Unit.Render.pickFrame' applies. Only W\/SW\/NW can mirror, and
--   only from their own eastern counterpart via 'mirrorDir'. Anything
--   still unresolved is omitted: an unavailable direction shows no
--   cell rather than falling back to another unit's textures.
resolveAnimDirections ∷ Bool → Map.Map Direction [Text] → [PreviewFrameDir]
resolveAnimDirections flipOK stored =
    mapMaybe resolve previewDirectionOrder
  where
    resolve d = case nonEmpty =≪ Map.lookup d stored of
        Just frames → Just (cell d d False frames)
        Nothing
          | not flipOK → Nothing
          | otherwise  → do
              src ← mirrorDir d
              frames ← nonEmpty =≪ Map.lookup src stored
              pure (cell d src True frames)
    nonEmpty fs = if null fs then Nothing else Just fs
    cell d src mirrored frames = PreviewFrameDir
        { pfdDirection = directionDirName d
        , pfdSource    = directionDirName src
        , pfdMirrored  = mirrored
        , pfdFrames    = frames
        }

-- | The frame index one direction shows at @elapsed@ seconds into the
--   animation's single shared playback clock.
--
--   Each direction computes against its OWN frame count (the checked-in
--   acolyte animations are not uniformly sized across directions), from
--   the SAME elapsed value, so the enlarged sprite and the direction row
--   stay phase-aligned.
--
--   Non-loop end-of-clip policy (Requirement 5, implementer's
--   discretion): HOLD the last frame — the same clamp
--   'Unit.Render.pickFrame' already applies to a non-looping game
--   animation, so the viewer shows exactly what the game would.
frameIndexAt ∷ Bool → Float → Int → Double → Int
frameIndexAt looping fps frameCount elapsed
    | frameCount ≤ 1 = 0
    | otherwise =
        let rate = max 0 (realToFrac fps ∷ Double)
            raw  = floor (max 0 elapsed * rate) ∷ Int
        in if looping then raw `mod` frameCount
                      else min raw (frameCount - 1)

-- * YAML metadata

-- | A deliberately narrow view of @data\/units\/\<name\>.yaml@: just
--   each def's name and its animation metadata map. Reusing
--   'UnitYamlAnim'\'s own 'FromJSON' instance keeps the viewer's
--   defaults (@fps=8@, @loop=true@, @flip=false@) literally the same
--   code the game uses, while ignoring the rest of the (large, and for
--   this purpose irrelevant) unit schema — so a preview never fails
--   because some unrelated gameplay field moved.
data UnitAnimMetaDef = UnitAnimMetaDef
    { uamdName ∷ !Text
    , uamdAnims ∷ !(Map.Map Text UnitYamlAnim)
    }

instance FromJSON UnitAnimMetaDef where
    parseJSON = withObject "UnitAnimMetaDef" $ \v → UnitAnimMetaDef
        ⊚ v .:  "name"
        ⊛ v .:? "animations" .!= Map.empty

newtype UnitAnimMetaFile = UnitAnimMetaFile { uamfUnits ∷ [UnitAnimMetaDef] }

instance FromJSON UnitAnimMetaFile where
    parseJSON = withObject "UnitAnimMetaFile" $ \v → UnitAnimMetaFile
        ⊚ v .: "units"

-- | Playback metadata for @unitName@, keyed by animation name. Empty
--   for a missing, unreadable, or unparseable YAML file, and empty for
--   a file that holds no def matching @unitName@ — every such case
--   falls back to the documented per-animation defaults rather than
--   failing the preview (three of the seven shipped unit asset trees
--   have no YAML at all).
loadUnitAnimMeta ∷ Text → IO (Map.Map Text UnitYamlAnim)
loadUnitAnimMeta unitName = do
    let path = unitDataPath unitName
    exists ← doesFileExist path
    if not exists
        then pure Map.empty
        else do
            result ← Yaml.decodeFileEither path
            pure $ case result of
                Left _   → Map.empty
                Right uf → maybe Map.empty uamdAnims
                    (find ((≡ unitName) ∘ uamdName) (uamfUnits uf))

-- * Filesystem discovery

-- | Validate @name@ as exactly one contained, non-symlinked direct
--   child of @root@ holding an @animations\/@ subtree, and return that
--   child's path. Structure is rejected before the filesystem is
--   touched (absolute paths, separators, @.@\/@..@, empty), the
--   directory is lstat-checked for symlink-ness, and a final
--   canonicalization confirms containment defensively — the same
--   layered rule 'Engine.Preview.Discovery.resolveFocusedEntry' uses.
resolveUnitDir ∷ FilePath → String → IO (Either UnitFocusError FilePath)
resolveUnitDir root name
    | null name ∨ isAbsolute name ∨ length (splitDirectories name) /= 1
        ∨ name ≡ "." ∨ name ≡ ".." ∨ pathSeparator `elem` name =
        pure (Left UnitNameEscapesRoot)
    | otherwise = do
        let candidate = root </> name
        rootExists ← doesDirectoryExist root
        if not rootExists
            then pure (Left UnitNotFound)
            else do
                -- Existence FIRST: 'pathIsSymbolicLink' throws on a
                -- path that isn't there at all, which is exactly the
                -- ordinary `units/nosuch` case. A dangling symlink
                -- fails this check too and reports as not-found, which
                -- is the honest answer — there's no unit behind it.
                isDir ← doesDirectoryExist candidate
                if not isDir
                    then pure (Left UnitNotFound)
                    else do
                        isLink ← pathIsSymbolicLink candidate
                        if isLink
                            then pure (Left UnitNameSymlink)
                            else do
                                canonRoot ← canonicalizePath root
                                canonCand ← canonicalizePath candidate
                                if not (canonRoot `isPathPrefixOf` canonCand)
                                    then pure (Left UnitNameEscapesRoot)
                                    else do
                                        hasAnims ← doesDirectoryExist
                                            (candidate </> "animations")
                                        pure $ if hasAnims
                                            then Right candidate
                                            else Left UnitNoAnimations

-- | Every animation the unit's asset tree holds, in the case-sensitive
--   lexicographic directory-name order the list displays, each mapped
--   to its stored per-direction frame paths (numerically ordered).
--   Symlinks are skipped at every level, matching
--   'Engine.Preview.Discovery.walkFiles'. Directions whose folder name
--   isn't a recognized compass spelling, and directions holding no
--   @.png@ frames, are dropped.
discoverUnitAnimations ∷ FilePath → IO [(Text, Map.Map Direction [Text])]
discoverUnitAnimations unitDir = do
    let animRoot = unitDir </> "animations"
    exists ← doesDirectoryExist animRoot
    if not exists
        then pure []
        else do
            names ← listDirectory animRoot
            -- Case-sensitive lexicographic by exact directory name —
            -- the SAME 'Ord'-on-the-label rule
            -- 'Engine.Preview.Discovery.sortEntries' uses, so the two
            -- browsers order their lists identically.
            anims ← forM (sort names) $ \animName → do
                let animPath = animRoot </> animName
                skip ← shouldSkip animPath
                if skip
                    then pure Nothing
                    else do
                        dirs ← discoverDirections animPath
                        pure $ if Map.null dirs
                            then Nothing
                            else Just (T.pack animName, dirs)
            pure (catMaybes anims)
  where
    shouldSkip p = do
        isLink ← pathIsSymbolicLink p
        if isLink then pure True else not ⊚ doesDirectoryExist p

discoverDirections ∷ FilePath → IO (Map.Map Direction [Text])
discoverDirections animPath = do
    names ← listDirectory animPath
    entries ← forM names $ \dirName → case parseDirectionDirName (T.pack dirName) of
        Nothing  → pure Nothing
        Just dir → do
            let dirPath = animPath </> dirName
            isLink ← pathIsSymbolicLink dirPath
            isDir  ← doesDirectoryExist dirPath
            if isLink ∨ not isDir
                then pure Nothing
                else do
                    files ← listDirectory dirPath
                    frames ← filterM (isFrameFile dirPath)
                        (sortFrameFiles (filter isPng files))
                    pure $ if null frames
                        then Nothing
                        else Just (dir, map (T.pack ∘ (dirPath </>)) frames)
    pure (Map.fromList (catMaybes entries))
  where
    isPng f = map toLower (takeExtension f) ≡ ".png"
    isFrameFile dirPath f = do
        isLink ← pathIsSymbolicLink (dirPath </> f)
        pure (not isLink)

-- | Pure assembly (Requirement 8's dump payload and the viewer's whole
--   model): join the filesystem-derived animations with whatever
--   metadata the YAML contributed. Exposed separately from
--   'buildPreviewUnit' so the metadata/mirroring/default-selection
--   rules are testable without a fixture on disk.
buildPreviewAnims
    ∷ Map.Map Text UnitYamlAnim
    → [(Text, Map.Map Direction [Text])]
    → [PreviewAnim]
buildPreviewAnims meta = map build
  where
    build (name, stored) =
        let mAnim = Map.lookup name meta
            flipOK = effectiveFlip mAnim stored
        in PreviewAnim
            { paName  = name
            , paFps   = maybe defaultFps uyaFps mAnim
            , paLoop  = maybe defaultLoop uyaLoop mAnim
            , paFlip  = flipOK
            , paThumb = fromMaybe "" (listToMaybe =≪ Map.lookup DirS stored)
            , paDirs  = resolveAnimDirections flipOK stored
            }
    -- The SAME values 'UnitYamlAnim's FromJSON instance defaults to,
    -- restated here for the no-YAML-entry-at-all case.
    defaultFps  = 8.0
    defaultLoop = True

-- | The whole pre-boot pipeline for @--preview units/\<name\>@:
--   validate the target, discover its animations, augment them from
--   the unit's own YAML, and pick the default selection.
buildPreviewUnit ∷ FilePath → String → IO (Either UnitFocusError PreviewUnit)
buildPreviewUnit root name = resolveUnitDir root name ⌦ \case
    Left err → pure (Left err)
    Right unitDir → do
        let unitName = T.pack name
        stored ← discoverUnitAnimations unitDir
        if null stored
            then pure (Left UnitNoAnimations)
            else do
                meta ← loadUnitAnimMeta unitName
                let anims = buildPreviewAnims meta stored
                pure (Right PreviewUnit
                    { puName    = unitName
                    , puAnims   = anims
                    , puDefault = fromMaybe ""
                        (defaultAnimationName (map paName anims))
                    })

-- | Path-boundary-aware prefix check (same rule as
--   'Engine.Preview.Discovery'\'s own): @root@ must be exactly
--   @candidate@ or followed immediately by a path separator.
isPathPrefixOf ∷ FilePath → FilePath → Bool
isPathPrefixOf root candidate =
    root ≡ candidate ∨ (root ⧺ [pathSeparator]) `isPrefixOf` candidate
