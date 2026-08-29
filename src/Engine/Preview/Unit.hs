-- | Target resolution + playback assembly for the @--preview
--   units/\<name\>@ animation viewer (#887, Phase 3 of the browser epic
--   #427). Companion to 'Engine.Preview.Discovery' (which owns the
--   simple-category texture browser) and, like it, runs entirely
--   BEFORE @App.Preview.runPreview@ ever creates a window: a bad unit
--   name is a pre-boot exit, and the resolved 'PreviewUnit' rides into
--   the engine as 'Engine.Core.Types.PreviewBrowse' for
--   @scripts/preview_manager.lua@ to render.
--
--   AUTHORITY IS THE UNIT YAML AND ITS COMPILED INDEX (#1261, TEX-6).
--   @data\/units\/\<name\>.yaml@ declares which animations exist and
--   @atlas\/index.json@ says how each one is stored; the viewer takes
--   its list, its directions, its per-direction frame counts, its cell
--   geometry and its @fps@\/@loop@\/@flip@ from that pair, and samples
--   the compiled atlas — the SAME artifacts, the SAME loader
--   ('Unit.Atlas.Yaml.resolveUnitAtlases'), and the SAME frozen cell
--   arithmetic ('Unit.Atlas.Types.atlasCellUV') the game uses. D-9 asks
--   for exactly that: a viewer running a preview-only decoder would
--   miss the malformed-metadata and sampling regressions it exists to
--   catch. A rejected index is a PRE-BOOT FAILURE, never a quiet fall
--   back to the source frames sitting beside it.
--
--   That REPLACES #887's filesystem-first discovery. The asset tree is
--   still what a unit target must contain — @animations\/@ must exist,
--   and neither it nor the unit directory may be a symlink — but it no
--   longer decides which animations browse, because there is no
--   per-frame path left to render an undeclared folder through: #1261
--   retired it in gameplay and in the viewer alike. An animation folder
--   present on disk and absent from the YAML is therefore EXCLUDED from
--   the browse list rather than rendered from its frames. Nothing
--   committed can be in that state — @tools\/pack_atlas.py
--   --validate-only --strict@ (CI and @make ci@) fails on any animation
--   PNG no declaration owns, which is where an undeclared folder is
--   reported loudly and by path.
--
--   Every shipped tree is declared and compiled. Gameplay @units:@ and
--   preview-only @asset_units:@ share this path deliberately; the top-level
--   YAML key decides runtime registration, not whether the art can preview.
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
  , resolveAnimDirections
  , frameIndexAt
  , loadUnitAnimMeta
  , buildPreviewAnims
  , buildPreviewUnit
  , buildPreviewUnitIn
  ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Data.List (sortOn, find)
import Data.Maybe (mapMaybe)
import System.Directory
    ( doesDirectoryExist, doesFileExist, pathIsSymbolicLink )
import System.FilePath ((</>), (<.>))
import Engine.Asset.YamlUnits (UnitYamlAnim(..))
import Engine.Core.Types
    ( PreviewUnit(..), PreviewAnim(..), PreviewFrameDir(..)
    , PreviewFrame(..) )
import Engine.Preview.Discovery (ItemDirError(..), resolveItemDir, sortFrameFiles)
import Unit.Atlas.Index (renderAtlasLoadError)
import Unit.Atlas.Types
    ( AtlasAnimation(..), AtlasDirectionRow(..), atlasCellUV )
import Unit.Atlas.Yaml (resolveUnitAtlasesIn)
import Unit.Direction (Direction(..), mirrorDir, parseDirectionName)

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
  -- ^ The unit directory, or its @animations\/@ root, is a symlink —
  --   refused unconditionally (not just an escaping one), the same rule
  --   'Engine.Preview.Discovery.walkFiles' applies to every entry it
  --   walks past. Both levels matter: 'doesDirectoryExist' follows
  --   links, so a real unit directory with a symlinked @animations\/@
  --   would otherwise browse another tree's animations entirely.
  | UnitNotFound
  -- ^ No such directory under @assets\/textures\/units@ (the
  --   @units\/nosuch@ case), or the units root itself is missing.
  | UnitNoAnimations
  -- ^ The unit directory exists but has no @animations\/@ subtree, so
  --   there is nothing for the viewer to show.
  | UnitAtlasRejected !Text
  -- ^ The unit ships compiled atlas artifacts and the PRODUCTION loader
  --   refused them (#1260): a missing, stale, unsupported, or malformed
  --   index, or an atlas that no longer holds the source art it was
  --   compiled from. Carries
  --   'Unit.Atlas.Index.renderAtlasLoadError'\'s own diagnostic, which
  --   already names the unit, the animation and the artifact.
  --
  --   This is a REJECTION, not a fallback. The viewer could trivially
  --   draw the source frames sitting beside the broken atlas, and that
  --   is precisely what would make it useless as D-9's acceptance
  --   surface: it would render art the game refuses to load.
  deriving (Eq, Show)

unitFocusErrorMessage ∷ UnitFocusError → Text
unitFocusErrorMessage UnitNameEscapesRoot =
    "unit name must be a single directory name under assets/textures/units \
    \(no absolute paths, path separators, or \"..\" components)"
unitFocusErrorMessage UnitNameSymlink =
    "unit directory, and its animations/ directory, must not be a symlink"
unitFocusErrorMessage UnitNotFound = "no such unit"
unitFocusErrorMessage UnitNoAnimations =
    "unit has no animations/ directory"
unitFocusErrorMessage (UnitAtlasRejected reason) = reason

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
--   an asset folder can be named either way — literally the same table
--   ('Unit.Direction.parseDirectionName') since #1260, so a folder the
--   viewer browses and a YAML key the game loads can never disagree.
parseDirectionDirName ∷ Text → Maybe Direction
parseDirectionDirName = parseDirectionName

-- | The game's own compass order (@Unit.Direction.allDirections@:
--   clockwise from South) — the left-to-right order the viewer's
--   direction row and the introspection dump both use.
previewDirectionOrder ∷ [Direction]
previewDirectionOrder = [minBound .. maxBound]

-- | Default selection (Requirement 2, made deterministic by the review
--   amendment): @idle@ when the unit has one, otherwise the first name
--   in the SAME case-sensitive lexicographic order the list itself
--   uses. 'Nothing' only when there are no animations at all.
defaultAnimationName ∷ [Text] → Maybe Text
defaultAnimationName names
    | "idle" `elem` names = Just "idle"
    | otherwise           = listToMaybe names

-- | Resolve the eight display cells against the stored per-direction
--   frames, in 'previewDirectionOrder'.
--
--   A directly-authored direction ALWAYS wins (never mirrored even when
--   flipping is enabled) — the same precedence
--   'Unit.Render.pickFrame' applies. Only W\/SW\/NW can mirror, and
--   only from their own eastern counterpart via 'mirrorDir'. Anything
--   still unresolved is omitted: an unavailable direction shows no
--   cell rather than falling back to another unit's textures.
resolveAnimDirections ∷ Bool → Map.Map Direction [PreviewFrame] → [PreviewFrameDir]
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

-- | An animation's per-direction frames, addressed through the compiled
--   index (#1260).
--
--   The index is the whole authority here: which directions the
--   animation authors, how many frames each REALLY holds
--   ('adrFrameCount', never the padded column count — D-5), which row
--   each occupies, and the cell geometry. Every frame names the one
--   atlas image; only the sub-rect differs, computed by the game's own
--   'atlasCellUV'.
atlasFrames ∷ AtlasAnimation → Map.Map Direction [PreviewFrame]
atlasFrames aa = frame ⊚ aaDirections aa
  where
    path = T.pack (aaPath aa)
    cell = (aaCellWidth aa, aaCellHeight aa)
    frame row =
        [ PreviewFrame
            { pfPath = path
            , pfUV   = atlasCellUV aa (adrRow row) column
            , pfCell = cell
            }
        | column ← [0 .. adrFrameCount row - 1] ]

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

-- | Both declaration forms, flattened. A gameplay unit lives under
--   @units:@ and an asset-only unit (#1257) under @asset_units:@; the
--   viewer wants the animation metadata either way and has no reason to
--   care which list it came from — the distinction is about
--   REGISTRATION, which the preview never performs. Both keys are
--   optional here (unlike @Engine.Asset.YamlUnits.UnitYamlFile@, which
--   refuses a file with neither): a preview must degrade to defaults
--   rather than fail, so an unrecognised file simply yields no metadata.
newtype UnitAnimMetaFile = UnitAnimMetaFile { uamfUnits ∷ [UnitAnimMetaDef] }

instance FromJSON UnitAnimMetaFile where
    parseJSON = withObject "UnitAnimMetaFile" $ \v → do
        gameplay ← v .:? "units"       .!= []
        assets   ← v .:? "asset_units" .!= []
        pure (UnitAnimMetaFile (gameplay ⧺ assets))

-- | Playback metadata for @unitName@, keyed by animation name. Reads
--   both the @units:@ and @asset_units:@ declaration forms. Empty for a
--   missing, unreadable, or unparseable YAML file, and empty for a file
--   that holds no def matching @unitName@.
--
--   Empty is no longer a set of defaults to fall back on: since #1261
--   these declarations are the animation list itself, so an empty
--   result means the target has nothing to browse and
--   'buildPreviewUnitIn' reports 'UnitNoAnimations'. Returning the
--   empty map rather than an error is still deliberate — the caller
--   distinguishes "declared nothing" from "declared something the
--   compiler has not produced", which are different diagnoses.
loadUnitAnimMeta ∷ Text → IO (Map.Map Text UnitYamlAnim)
loadUnitAnimMeta = loadUnitAnimMetaIn ""

-- | 'loadUnitAnimMeta' against an explicit filesystem ROOT — the same
--   fixture hook 'Unit.Atlas.Load.loadUnitAtlasIndexIn' provides, and
--   for the same reason. Production passes @""@.
loadUnitAnimMetaIn ∷ FilePath → Text → IO (Map.Map Text UnitYamlAnim)
loadUnitAnimMetaIn root unitName = do
    let path = if null root then unitDataPath unitName
                            else root </> unitDataPath unitName
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
--   child's path.
--
--   The containment rule itself is
--   'Engine.Preview.Discovery.resolveItemDir' — the SAME pre-boot check
--   every grouped category applies (#888), so a unit target and a
--   building\/flora\/structure target can never disagree about which
--   names are safe. This function adds only the units-specific
--   @animations\/@ requirement on top, and restates the errors in the
--   units vocabulary ('UnitFocusError') the CLI already reports. A
--   candidate that exists but is a regular file reports as 'UnitNotFound',
--   exactly as it did before the shared helper existed.
resolveUnitDir ∷ FilePath → String → IO (Either UnitFocusError FilePath)
resolveUnitDir root name = resolveItemDir root name ⌦ \case
    Left ItemDirEscapesRoot     → pure (Left UnitNameEscapesRoot)
    Left ItemDirSymlink         → pure (Left UnitNameSymlink)
    Left ItemDirNotFound        → pure (Left UnitNotFound)
    Left ItemDirNotADirectory   → pure (Left UnitNotFound)
    Right candidate → do
        let animRoot = candidate </> "animations"
        hasAnims ← doesDirectoryExist animRoot
        if not hasAnims
            then pure (Left UnitNoAnimations)
            else do
                -- The animations/ root gets the SAME lstat as every
                -- other level: a symlinked one would otherwise let a
                -- non-symlinked unit pull animations and textures from
                -- outside its own tree (doesDirectoryExist follows
                -- links), breaking both the symlink rule and the
                -- requested-unit-only trimmed-loading contract.
                animLink ← pathIsSymbolicLink animRoot
                pure $ if animLink
                    then Left UnitNameSymlink
                    else Right candidate

-- | Pure assembly (Requirement 8's dump payload and the viewer's whole
--   model): turn the PRODUCTION atlas selection into the viewer's
--   animation list. Exposed separately from 'buildPreviewUnit' so the
--   ordering, mirroring and default-selection rules are testable
--   without a fixture on disk.
--
--   @atlases@ is 'Unit.Atlas.Yaml.resolveUnitAtlases'\' result, already
--   validated against the unit's YAML and its source art, and covering
--   exactly the animations that YAML declares. Every animation takes
--   EVERYTHING from its index record — directions, real per-direction
--   frame counts (never the padded column count), cell geometry,
--   @fps@\/@loop@\/@flip@ — because that is what the game does with
--   it. There is no other branch: since #1261 there is no
--   representation an animation could be in that this does not
--   describe.
--
--   Ordering is case-sensitive lexicographic by animation name, the
--   SAME 'Ord'-on-the-label rule 'Engine.Preview.Discovery.sortEntries'
--   uses, so the two browsers order their lists identically. An
--   animation's name IS its directory name — @tools\/pack_atlas.py@
--   requires every declared frame to resolve inside
--   @\<unit\>\/animations\/\<animation\>\/\<direction\>\/@ —
--   so this is the same order the filesystem walk produced before
--   #1261.
buildPreviewAnims ∷ HM.HashMap Text AtlasAnimation → [PreviewAnim]
buildPreviewAnims atlases =
    [ build name aa | (name, aa) ← sortOn fst (HM.toList atlases) ]
  where
    build name aa =
        let frames = atlasFrames aa
            flipOK = aaFlip aa
        in PreviewAnim
            { paName  = name
            , paFps   = aaFps aa
            , paLoop  = aaLoop aa
            , paFlip  = flipOK
            , paAtlas = T.pack (aaPath aa)
            , paThumb = listToMaybe =≪ Map.lookup DirS frames
            , paDirs  = resolveAnimDirections flipOK frames
            }

-- | The whole pre-boot pipeline for @--preview units/\<name\>@:
--   validate the target, discover its animations, resolve its compiled
--   atlases through the PRODUCTION loader, augment the rest from the
--   unit's own YAML, and pick the default selection.
--
--   The atlas resolution runs before anything is assembled and its
--   failure is the whole target's failure (D-9): a unit whose compiled
--   artifacts the game would refuse must not open a window showing the
--   source frames instead.
buildPreviewUnit ∷ FilePath → String → IO (Either UnitFocusError PreviewUnit)
buildPreviewUnit = buildPreviewUnitIn ""

-- | 'buildPreviewUnit' with an explicit RESOURCE root for the two
--   resource-relative reads it makes beyond the unit's own asset
--   directory — @data\/units\/\<name\>.yaml@ and the compiled
--   @atlas\/index.json@. Production passes @""@ (the executable has
--   already chdir'd into the resource root); a fixture tree passes its
--   own, which is what lets the rejected-index path be exercised
--   without writing into the shipped assets.
--
--   The units-category root stays a separate parameter because it is
--   not always @\<resource root\>\/assets\/textures\/units@: the
--   containment specs point it at a bare temp directory.
buildPreviewUnitIn
    ∷ FilePath → FilePath → String → IO (Either UnitFocusError PreviewUnit)
buildPreviewUnitIn resourceRoot root name = resolveUnitDir root name ⌦ \case
    Left err → pure (Left err)
    Right _unitDir → do
        let unitName = T.pack name
        meta ← loadUnitAnimMetaIn resourceRoot unitName
        eAtlases ← resolveUnitAtlasesIn resourceRoot unitName meta
        case eAtlases of
            Left err → pure ∘ Left ∘ UnitAtlasRejected
                           $ renderAtlasLoadError err
            Right atlases
                -- A tree whose YAML declares nothing has nothing to
                -- show, exactly as an empty `animations/` directory
                -- did before #1261 — same pre-boot exit, and the same
                -- one `resolveUnitDir` reports for a missing
                -- `animations/` root.
                | HM.null atlases → pure (Left UnitNoAnimations)
                | otherwise → do
                    let anims = buildPreviewAnims atlases
                    pure (Right PreviewUnit
                        { puName    = unitName
                        , puAnims   = anims
                        , puDefault = fromMaybe ""
                            (defaultAnimationName (map paName anims))
                        })
