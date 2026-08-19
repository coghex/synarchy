-- | Filesystem discovery + default-selection resolution for the
--   @--preview buildings/\<name\>@ viewer (#888, Phase 4 of the browser
--   epic #427). Companion to 'Engine.Preview.Discovery' (the simple
--   texture browser) and 'Engine.Preview.Unit' (the units viewer), and
--   like both it runs entirely BEFORE @App.Preview.runPreview@ ever
--   creates a window: a bad building name is a pre-boot exit, and the
--   resolved 'PreviewBuilding' rides into the engine as
--   'Engine.Core.Types.PreviewBrowse' for
--   @scripts/preview_manager.lua@ to render.
--
--   Authority split (the #888 review amendment, mirroring #887's):
--   @assets\/textures\/buildings\/\<name\>\/@ is authoritative for WHICH
--   entries exist and, within a recognized animation directory, the
--   frame ORDER. @data\/buildings\/\<name\>.yaml@ only AUGMENTS a
--   matching animation with @fps@\/@loop@ and supplies the
--   default-selection hints (@state_animations.built@, @sprite@).
--   Missing, malformed, or unmatched YAML never rejects an otherwise
--   valid asset folder — @dungeon_1@ has no YAML file at all, and both
--   @cargo_hold_S@ and @furnace@ ship a @demolish\/@ folder no YAML
--   mentions.
--
--   Animation association is by CONTENT, never by equal names: the
--   acolyte portal's YAML names its animations @portal-appear@ and
--   @portal-idle@ while the directories are @appear\/@ and @idle\/@, so
--   a directory is matched to a YAML animation through the frame paths
--   that animation actually declares.
module Engine.Preview.Building
  ( BuildingPreviewMeta(..)
  , emptyBuildingPreviewMeta
  , buildingsCategoryRoot
  , buildingDefaultFps
  , buildingDefaultLoop
  , isFrameFileName
  , matchAnimForDir
  , defaultBuildingEntry
  , loadBuildingPreviewMeta
  , discoverBuildingEntries
  , buildPreviewBuilding
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Char (isDigit, toLower)
import Data.List (sortBy, sort, find, stripPrefix)
import Data.Ord (comparing)
import System.Directory
    ( doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink )
import System.FilePath
    ( (</>), (<.>), dropExtension, takeDirectory, takeFileName, pathSeparator )
import Engine.Asset.YamlBuildings (BuildingYamlAnim(..))
import Engine.Core.Types (PreviewBuilding(..), PreviewBuildingEntry(..))
import Engine.Preview.Discovery
    ( ItemDirError, isSupportedTextureFile, resolveItemDir, sortFrameFiles )

-- | @assets\/textures\/buildings@ — the grouped buildings category root.
buildingsCategoryRoot ∷ FilePath
buildingsCategoryRoot = "assets" </> "textures" </> "buildings"

-- | @data\/buildings\/\<name\>.yaml@ — the ONE gameplay YAML the
--   buildings viewer is allowed to read (the trimmed-boot contract's
--   second documented exception, beside the units viewer's own
--   @data\/units\/\<name\>.yaml@): never the full building catalog,
--   never any other @data\/*.yaml@.
buildingDataPath ∷ Text → FilePath
buildingDataPath name = "data" </> "buildings" </> T.unpack name <.> "yaml"

-- | The SAME values 'Engine.Asset.YamlBuildings.BuildingYamlAnim'\'s
--   'FromJSON' instance defaults to, restated here for a directory the
--   YAML never mentions (or a building with no YAML at all). NB
--   buildings default to @loop = False@ — the units viewer's default is
--   'True', and copying it here would silently loop a construction
--   animation the game plays once.
buildingDefaultFps ∷ Float
buildingDefaultFps = 8.0

buildingDefaultLoop ∷ Bool
buildingDefaultLoop = False

-- * YAML metadata

-- | Everything the buildings viewer takes from
--   @data\/buildings\/\<name\>.yaml@: the playback metadata map, the
--   state→animation-name map (for the @built@ default), and the
--   building's own @sprite@ (the second default-selection fallback).
data BuildingPreviewMeta = BuildingPreviewMeta
    { bpmAnims  ∷ !(Map.Map Text BuildingYamlAnim)
    , bpmStates ∷ !(Map.Map Text Text)
    , bpmSprite ∷ !(Maybe Text)
    } deriving (Eq, Show)

emptyBuildingPreviewMeta ∷ BuildingPreviewMeta
emptyBuildingPreviewMeta = BuildingPreviewMeta
    { bpmAnims = Map.empty, bpmStates = Map.empty, bpmSprite = Nothing }

-- | A deliberately narrow view of the building YAML — just the fields
--   above. Reusing 'BuildingYamlAnim'\'s own 'FromJSON' instance keeps
--   the viewer's @fps@\/@loop@ defaults literally the same code the
--   game uses, while ignoring the rest of the (large, and for this
--   purpose irrelevant) building schema — so a preview never fails
--   because some unrelated gameplay field moved. Even @sprite@, which
--   the game's own decoder requires, is optional here.
data BuildingAnimMetaDef = BuildingAnimMetaDef
    { bamdName   ∷ !Text
    , bamdSprite ∷ !(Maybe Text)
    , bamdStates ∷ !(Map.Map Text Text)
    , bamdAnims  ∷ !(Map.Map Text BuildingYamlAnim)
    }

instance FromJSON BuildingAnimMetaDef where
    parseJSON = withObject "BuildingAnimMetaDef" $ \v → BuildingAnimMetaDef
        ⊚ v .:  "name"
        ⊛ v .:? "sprite"
        ⊛ v .:? "state_animations" .!= Map.empty
        ⊛ v .:? "animations"       .!= Map.empty

newtype BuildingAnimMetaFile = BuildingAnimMetaFile
    { bamfBuildings ∷ [BuildingAnimMetaDef] }

instance FromJSON BuildingAnimMetaFile where
    parseJSON = withObject "BuildingAnimMetaFile" $ \v → BuildingAnimMetaFile
        ⊚ v .: "buildings"

-- | Preview metadata for @name@. 'emptyBuildingPreviewMeta' for a
--   missing, unreadable, or unparseable YAML file, and for a file that
--   holds no def matching @name@ — every such case falls back to the
--   documented defaults rather than failing the preview.
loadBuildingPreviewMeta ∷ Text → IO BuildingPreviewMeta
loadBuildingPreviewMeta name = do
    let path = buildingDataPath name
    exists ← doesFileExist path
    if not exists
        then pure emptyBuildingPreviewMeta
        else do
            result ← Yaml.decodeFileEither path
            pure $ case result of
                Left _   → emptyBuildingPreviewMeta
                Right bf → maybe emptyBuildingPreviewMeta fromDef
                    (find ((≡ name) ∘ bamdName) (bamfBuildings bf))
  where
    fromDef d = BuildingPreviewMeta
        { bpmAnims  = bamdAnims d
        , bpmStates = bamdStates d
        , bpmSprite = bamdSprite d
        }

-- * Pure classification / ordering rules

-- | Every frame path a YAML animation declares, across all of its
--   direction keys (buildings only ever use @default@, but nothing in
--   the schema enforces that, and a stray key must not hide frames from
--   the association below).
animFramePaths ∷ BuildingYamlAnim → [Text]
animFramePaths = concat ∘ Map.elems ∘ byaFrames

-- | The YAML animation whose declared frames live in @dir@ — the
--   CONTENT association that makes @portal-idle@ resolve to the
--   @idle\/@ directory even though the two names differ. A directory
--   matches when any declared frame path's own directory is that
--   directory; ties are impossible in practice and resolved by the
--   map's ascending key order.
matchAnimForDir ∷ Map.Map Text BuildingYamlAnim → FilePath
                → Maybe (Text, BuildingYamlAnim)
matchAnimForDir anims dir =
    find (any ((≡ wanted) ∘ normSlashes ∘ takeDirectory ∘ T.unpack)
              ∘ animFramePaths ∘ snd)
         (Map.toAscList anims)
  where wanted = normSlashes dir

-- | Platform-independent path comparison: YAML always spells its frame
--   paths with @\/@, while a discovered path is built with
--   'System.FilePath.</>'.
normSlashes ∷ FilePath → FilePath
normSlashes = map (\c → if c ≡ pathSeparator then '/' else c)

-- | The checked-in numbered-frame convention (@frame_000.png@,
--   @frame_1.png@, …) — the fallback signal that recognizes an
--   animation directory the YAML never mentions (@cargo_hold_S/demolish@
--   and @furnace/demolish@ today).
isFrameFileName ∷ FilePath → Bool
isFrameFileName f =
    case stripPrefix "frame" (map toLower (dropExtension (takeFileName f))) of
        Nothing   → False
        Just rest →
            let digits = dropWhile (`elem` ("-_" ∷ String)) rest
            in not (null digits) ∧ all isDigit digits

-- | Deterministic ordering: lexicographic, case-sensitive 'Ord' on the
--   label — the SAME rule 'Engine.Preview.Discovery.sortEntries' uses,
--   applied across animation and static entries together (the building
--   folder mixes them, so one list, one order).
sortBuildingEntries ∷ [PreviewBuildingEntry] → [PreviewBuildingEntry]
sortBuildingEntries = sortBy (comparing pbeLabel)

-- | The default selection (Requirement 1, made explicit by the review
--   amendment), in strict order:
--
--   1. the animation @state_animations.built@ names, resolved through
--      the frame paths that animation declares (never by equal names);
--   2. the building's own @sprite@, when it names a discovered static
--      entry of this folder;
--   3. a top-level @default.png@;
--   4. the first entry in @sortBuildingEntries@ order.
--
--   Empty only when the folder holds no browsable texture at all.
--   @dungeon_1@ (no YAML, no @default.png@) lands on rule 4.
defaultBuildingEntry ∷ BuildingPreviewMeta → [PreviewBuildingEntry] → Text
defaultBuildingEntry meta entries = fromMaybe "" $
    builtLabel <|> spriteLabel <|> defaultPngLabel <|> firstLabel
  where
    builtLabel = do
        animName ← Map.lookup "built" (bpmStates meta)
        ya       ← Map.lookup animName (bpmAnims meta)
        let wanted = map normText (animFramePaths ya)
        entry ← find (\e → pbeAnimated e
                        ∧ any ((`elem` wanted) ∘ normText) (pbeFrames e))
                     entries
        pure (pbeLabel entry)
    spriteLabel = do
        sprite ← normText ⊚ bpmSprite meta
        entry  ← find (\e → not (pbeAnimated e)
                         ∧ any ((≡ sprite) ∘ normText) (pbeFrames e))
                      entries
        pure (pbeLabel entry)
    defaultPngLabel = pbeLabel ⊚ find ((≡ "default.png") ∘ pbeLabel) entries
    firstLabel      = pbeLabel ⊚ listToMaybe entries
    normText        = T.pack ∘ normSlashes ∘ T.unpack

-- * Filesystem discovery

-- | Every browsable entry of a building's asset folder, in
--   @sortBuildingEntries@ order.
--
--   Walked recursively, classifying at EVERY directory level: a
--   directory recognized as an animation (see 'matchAnimForDir' /
--   'isFrameFileName') becomes ONE entry labeled by its path relative
--   to @root@ (its bare directory name at the top level, which is where
--   every shipped animation lives); any other directory is descended
--   into, so its textures surface as ordinary static entries rather
--   than being played as one clip or silently lost — exactly what
--   @dungeon_1\/damaged\/@ needs. Symlinks are skipped at every level,
--   matching 'Engine.Preview.Discovery.walkFiles'.
discoverBuildingEntries ∷ Map.Map Text BuildingYamlAnim → FilePath
                        → IO [PreviewBuildingEntry]
discoverBuildingEntries anims root = do
    exists ← doesDirectoryExist root
    if not exists then pure [] else sortBuildingEntries ⊚ walk []
  where
    walk segs = do
        let dir = foldl' (</>) root segs
        names ← listDirectory dir
        fmap concat $ forM (sort names) $ \name → do
            let segs' = segs ⧺ [name]
                full  = foldl' (</>) root segs'
            isLink ← pathIsSymbolicLink full
            if isLink
                then pure []
                else do
                    isDir ← doesDirectoryExist full
                    if isDir
                        then classifyDir segs' full ⌦ \case
                            Just entry → pure [entry]
                            Nothing    → walk segs'
                        else pure [ staticEntry segs' full
                                  | isSupportedTextureFile name ]

    -- 'Just' when this directory is a recognized animation.
    classifyDir segs dir = do
        names ← listDirectory dir
        pngs  ← filterM (isPlainFile dir) (filter isSupportedTextureFile names)
        if null pngs
            then pure Nothing
            else do
                let frames = map (T.pack ∘ (dir </>)) (sortFrameFiles pngs)
                    entry fps loop = Just PreviewBuildingEntry
                        { pbeLabel    = label segs
                        , pbeAnimated = True
                        , pbeFps      = fps
                        , pbeLoop     = loop
                        , pbeFrames   = frames
                        }
                pure $ case matchAnimForDir anims dir of
                    Just (_, ya) → entry (byaFps ya) (byaLoop ya)
                    Nothing
                      | all isFrameFileName pngs →
                          entry buildingDefaultFps buildingDefaultLoop
                      | otherwise → Nothing

    isPlainFile dir f = not ⊚ pathIsSymbolicLink (dir </> f)

    staticEntry segs full = PreviewBuildingEntry
        { pbeLabel    = label segs
        , pbeAnimated = False
        , pbeFps      = buildingDefaultFps
        , pbeLoop     = buildingDefaultLoop
        , pbeFrames   = [T.pack full]
        }

    -- Labels are always @/@-separated regardless of host path
    -- conventions, the same guarantee 'discoverEntries' gives.
    label = T.pack ∘ foldr1 (\a b → a ⧺ "/" ⧺ b)

-- | The whole pre-boot pipeline for @--preview buildings/\<name\>@:
--   validate the target (the shared grouped-item containment rule),
--   read the building's own YAML, discover its entries, and pick the
--   default selection. An existing-but-empty folder is NOT an error —
--   it browses as an empty list, exactly like a bare simple category
--   with no textures.
buildPreviewBuilding ∷ FilePath → String → IO (Either ItemDirError PreviewBuilding)
buildPreviewBuilding root name = resolveItemDir root name ⌦ \case
    Left err → pure (Left err)
    Right dir → do
        let buildingName = T.pack name
        meta    ← loadBuildingPreviewMeta buildingName
        entries ← discoverBuildingEntries (bpmAnims meta) dir
        pure (Right PreviewBuilding
            { pbName    = buildingName
            , pbEntries = entries
            , pbDefault = defaultBuildingEntry meta entries
            })
