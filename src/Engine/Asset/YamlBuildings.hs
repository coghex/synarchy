{-# LANGUAGE Strict, DeriveGeneric #-}
module Engine.Asset.YamlBuildings
    ( BuildingYamlDef(..)
    , BuildingYamlAnim(..)
    , BuildingYamlTileSize(..)
    , BuildingYamlFile(..)
    , parseBuildingAnim
    , loadBuildingYaml
    , loadBuildingYamlOutcome
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)
import Building.Schema
import Engine.Core.Log (LoggerState)
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Asset.YamlList (loadYamlListOutcome)
import Power.Base (PowerNodeSpec, powerNodeSpecFromYaml)

-- | One building animation as DECLARED: four ordered frame lists, one
--   per camera facing, plus the provenance of that declaration.
--
--   The canonical form is
--
--   > frames:
--   >   south: [...]
--   >   west:  [...]
--   >   north: [...]
--   >   east:  [...]
--
--   with four non-empty lists of equal length, and no path repeated
--   between two facings at the same stage. The legacy @frames.default@
--   form stays readable during migration (see 'AssetLegacy') and is the
--   only way one path reaches all four views.
data BuildingYamlAnim = BuildingYamlAnim
    { byaFps    ∷ !Float
    , byaLoop   ∷ !Bool
    , byaFrames ∷ !(FacingAssets [Text])
    } deriving (Show, Eq, Generic)

-- | The context-free instance, for consumers that decode an animation
--   without a surrounding definition ('Engine.Preview.Building' reuses
--   it so the viewer's @fps@\/@loop@ defaults stay literally the game's
--   code). Rejection messages name no building or animation here;
--   'parseBuildingAnim' is what the game's decoder calls, and it does.
instance FromJSON BuildingYamlAnim where
    parseJSON = parseBuildingAnim "<building>" "<animation>"

-- | 'BuildingYamlAnim' with the names a rejection message needs.
parseBuildingAnim ∷ Text → Text → Aeson.Value → Aeson.Parser BuildingYamlAnim
parseBuildingAnim building animName =
    withObject "BuildingYamlAnim" $ \v → BuildingYamlAnim
        ⊚ v .:? "fps"  .!= 8.0
        ⊛ v .:? "loop" .!= False
        ⊛ animFrames ctx v
  where
    ctx = "building " <> quoted building <> " animation " <> quoted animName

data BuildingYamlTileSize = BuildingYamlTileSize
    { bytsX ∷ !Int
    , bytsY ∷ !Int
    } deriving (Show, Eq, Generic)

instance FromJSON BuildingYamlTileSize where
    parseJSON = withObject "BuildingYamlTileSize" $ \v → BuildingYamlTileSize
        ⊚ v .:? "x" .!= 1
        ⊛ v .:? "y" .!= 1

data BuildingYamlDef = BuildingYamlDef
    { bydName         ∷ !Text
    , bydDisplayName  ∷ !Text
    , bydCategory     ∷ !Text
    , bydDescription  ∷ !Text
    , bydSprites      ∷ !(FacingAssets Text)
      -- ^ The four static views, from the canonical @sprites@ block or
      --   — during migration only — the legacy singular @sprite@ path.
    , bydVisualClass  ∷ !BuildingVisualClass
      -- ^ Which art family owns this building (#2080 requirement 8).
      --   Mandatory: a missing or unrecognized value refuses the whole
      --   file rather than defaulting to a class the art slices would
      --   then have to guess at.
    , bydTileSize     ∷ !BuildingYamlTileSize
    , bydPlacement    ∷ !Text
      -- ^ "flat_ground" / other constraint kinds in the future
    , bydIsStarting   ∷ !Bool
    , bydRace         ∷ !Text
    , bydSpriteAnchor    ∷ !Text
    , bydBuildWork       ∷ !Float
    , bydMaterials       ∷ !(Map.Map Text Int)
    , bydStorageCapacity ∷ !Float
    , bydOperations      ∷ ![Text]
      -- ^ work-station operations offered when Built (#326); empty =
      --   not a station
    , bydRoleAnims       ∷ !(Map.Map BuildingRole Text)
      -- ^ Lifecycle role → animation name in 'bydAnimations', decoded
      --   from @state_animations@. A legacy @appearing@ key resolves
      --   through 'legacyRoleFor'; mixing it with the canonical role it
      --   resolves to is a parse error rather than a precedence rule.
    , bydAnimations      ∷ !(Map.Map Text BuildingYamlAnim)
    , bydPowerDrain      ∷ !Float
      -- ^ Watts drawn while Built (#361); 0 (default) = not a power
      --   consumer. See Building.Types.bdPowerDrain.
    , bydPowerNode       ∷ !(Maybe PowerNodeSpec)
      -- ^ The power NODE this def mints when placed (#1148), decoded
      --   from `power_role` + the one rating that role takes
      --   (`power_peak` watts for a source, `power_capacity` Wh for
      --   storage). Nothing (no `power_role`) = not a power node, which
      --   is every ordinary building. See Building.Types.bdPowerNode.
    } deriving (Show, Eq, Generic)

-- | Record syntax rather than the usual applicative chain: three of
--   these fields need @name@ (for their rejection messages) and one
--   needs @build_work@ (to resolve a legacy lifecycle key), so the
--   parser is monadic anyway — and naming each field keeps a later
--   record reordering from silently swapping two values of the same
--   type.
instance FromJSON BuildingYamlDef where
    parseJSON = withObject "BuildingYamlDef" $ \v → do
        name ← v .: "name"
        -- `build_work` is read before the lifecycle block because it is
        -- what a legacy `appearing` mapping resolves against.
        buildWork ← v .:? "build_work" .!= 0.0
        sprites ← defSprites name v
        vClass ← defVisualClass name v
        roles ← defRoleAnims name buildWork v
        anims ← defAnimations name v
        displayName ← v .:? "display_name"     .!= ""
        category ← v .:? "category"            .!= "Misc"
        description ← v .:? "description"      .!= ""
        tileSize ← v .:? "tile_size"           .!= BuildingYamlTileSize 1 1
        placement ← v .:? "placement"          .!= "flat_ground"
        isStarting ← v .:? "is_starting"       .!= False
        race ← v .:? "race"                    .!= ""
        spriteAnchor ← v .:? "sprite_anchor"   .!= "diamond_bottom"
        materials ← v .:? "materials"          .!= Map.empty
        storageCapacity ← v .:? "storage_capacity" .!= 0.0
        operations ← v .:? "operations"        .!= []
        powerDrain ← v .:? "power_drain"       .!= 0.0
        node ← powerNode v
        pure BuildingYamlDef
            { bydName            = name
            , bydDisplayName     = displayName
            , bydCategory        = category
            , bydDescription     = description
            , bydSprites         = sprites
            , bydVisualClass     = vClass
            , bydTileSize        = tileSize
            , bydPlacement       = placement
            , bydIsStarting      = isStarting
            , bydRace            = race
            , bydSpriteAnchor    = spriteAnchor
            , bydBuildWork       = buildWork
            , bydMaterials       = materials
            , bydStorageCapacity = storageCapacity
            , bydOperations      = operations
            , bydRoleAnims       = roles
            , bydAnimations      = anims
            , bydPowerDrain      = powerDrain
            , bydPowerNode       = node
            }

-- * Facing-set decoding

-- | Decode a CLOSED four-facing block. Every unknown key and every
--   missing direction is a rejection naming the offending declaration,
--   so a typo can never leave a definition quietly short of a view.
facingBlock ∷ Text                                -- ^ message context
            → Text                                -- ^ the block's own key
            → (CameraFacing → Aeson.Value → Aeson.Parser a)
            → Aeson.Object
            → Aeson.Parser (FacingSet a)
facingBlock ctx blockKey parseView obj = do
    let unknown = [ Key.toText k | k ← KM.keys obj
                  , isNothing (facingFromKey (Key.toText k)) ]
    unless (null unknown) $ failT $
        ctx <> ": `" <> blockKey <> "` declares unknown direction key"
            <> plural unknown <> keyList unknown
            <> "; the key set is exactly " <> keyList facingKeyList
    let view f = case KM.lookup (Key.fromText (facingKey f)) obj of
            Nothing → failT $
                ctx <> ": `" <> blockKey <> "` is missing the `"
                    <> facingKey f <> "` direction; all four of "
                    <> keyList facingKeyList <> " are required"
            Just val → parseView f val
    FacingSet ⊚ view FaceSouth ⊛ view FaceWest
              ⊛ view FaceNorth ⊛ view FaceEast

-- | The static views: canonical @sprites@ block, or the legacy singular
--   @sprite@ path — declaring BOTH is rejected rather than resolved by
--   precedence, and declaring neither is rejected too.
defSprites ∷ Text → Aeson.Object → Aeson.Parser (FacingAssets Text)
defSprites name v = do
    mCanonical ← v .:? "sprites"
    mLegacy ← v .:? "sprite"
    case (mCanonical, mLegacy) of
        (Just _, Just _) → failT $
            ctx <> " declares both the canonical `sprites` block and the "
                <> "legacy `sprite` path; declare exactly one"
        (Just obj, Nothing) → do
            views ← facingBlock ctx "sprites" (const parseJSON) obj
            canonicalStatics ctx views
            pure (canonicalAssets views)
        (Nothing, Just p) → pure (legacyAssets p)
        (Nothing, Nothing) → failT $
            ctx <> " declares no sprite: expected a `sprites` block with "
                <> keyList facingKeyList
                <> ", or the legacy `sprite` path"
  where ctx = "building " <> quoted name

-- | A canonical static declaration never aliases one view into another:
--   four facings, four paths. The legacy branch is the ONE place a path
--   reaches all four views, which is what makes 'AssetLegacy' mean
--   something — a canonical block repeating a path would be
--   indistinguishable from it in the runtime views while claiming to be
--   real four-facing art.
canonicalStatics ∷ Text → FacingSet Text → Aeson.Parser ()
canonicalStatics ctx views =
    case repeatedPath [ (f, facingValue f views) | f ← canonicalFacings ] of
        Nothing → pure ()
        Just (a, b, path) → failT $
            ctx <> ": `sprites` assigns " <> quoted path <> " to both `"
                <> facingKey a <> "` and `" <> facingKey b
                <> "`; each direction needs its own art"

-- | An animation's frame lists: the canonical four-direction block, or
--   the legacy @default@ list. Mixing the two is rejected.
animFrames ∷ Text → Aeson.Object → Aeson.Parser (FacingAssets [Text])
animFrames ctx v = do
    mFrames ← v .:? "frames"
    obj ← case mFrames of
        Nothing → failT $
            ctx <> " declares no `frames`; expected a block with "
                <> keyList facingKeyList
                <> ", or the legacy `default` list"
        Just o → pure o
    let keys = map Key.toText (KM.keys obj)
        directional = filter (isJust ∘ facingFromKey) keys
        hasLegacy = legacyFramesKey `elem` keys
        unknown = [ k | k ← keys
                  , isNothing (facingFromKey k), k ≢ legacyFramesKey ]
    -- Named FIRST, so it holds in every branch below. A block holding
    -- ONLY an unknown key would otherwise fall through to "declares no
    -- direction" — true, but never telling the author which key is wrong.
    unless (null unknown) $ failT $
        ctx <> ": `frames` declares unknown direction key"
            <> plural unknown <> keyList unknown
            <> "; the key set is exactly " <> keyList facingKeyList
            <> " (or the legacy `" <> legacyFramesKey <> "` list)"
    case (null directional, hasLegacy) of
        (False, True) → failT $
            ctx <> " declares both canonical direction key"
                <> plural directional <> keyList directional
                <> " and the legacy `" <> legacyFramesKey
                <> "` list; declare exactly one form"
        (False, False) → do
            views ← facingBlock ctx "frames" (const parseJSON) obj
            canonicalFrames ctx views
            pure (canonicalAssets views)
        (True, True) → do
            paths ← obj .: Key.fromText legacyFramesKey
            when (null paths) $ failT $
                ctx <> ": the legacy `" <> legacyFramesKey
                    <> "` frame list is empty"
            pure (legacyAssets paths)
        (True, False) → failT $
            ctx <> ": `frames` declares no direction; expected "
                <> keyList facingKeyList
                <> ", or the legacy `" <> legacyFramesKey <> "` list"

-- | The three content rules a canonical frame declaration must satisfy:
--   every direction non-empty, all four the same length, and no path
--   shared between two directions at one stage.
canonicalFrames ∷ Text → FacingSet [Text] → Aeson.Parser ()
canonicalFrames ctx views = do
    forM_ canonicalFacings $ \f →
        when (null (facingValue f views)) $ failT $
            ctx <> ": the `" <> facingKey f <> "` frame list is empty; "
                <> "every canonical direction needs at least one frame"
    let southCount = length (fsSouth views)
    forM_ canonicalFacings $ \f → do
        let n = length (facingValue f views)
        when (n ≢ southCount) $ failT $
            ctx <> ": the `" <> facingKey f <> "` frame list declares "
                <> tshow n <> " frames but `south` declares "
                <> tshow southCount
                <> "; all four directions must share one frame count"
    forM_ (zip [(0 ∷ Int) ..] (stages views)) $ \(i, stage) →
        case repeatedPath stage of
            Nothing → pure ()
            Just (a, b, p) → failT $
                ctx <> ": stage " <> tshow i <> " assigns " <> quoted p
                    <> " to both `" <> facingKey a <> "` and `"
                    <> facingKey b <> "`; each direction needs its own art"

-- | The per-stage columns of a canonical declaration: stage @i@ paired
--   with each facing's @i@-th frame. Only ever called once the four
--   lists are known to share a length.
stages ∷ FacingSet [Text] → [[(CameraFacing, Text)]]
stages views =
    [ [ (f, facingValue f views !! i) | f ← canonicalFacings ]
    | i ← [0 .. length (fsSouth views) - 1] ]

-- | The first pair of facings sharing one path at a stage.
repeatedPath ∷ [(CameraFacing, Text)] → Maybe (CameraFacing, CameraFacing, Text)
repeatedPath [] = Nothing
repeatedPath ((f, p) : rest) =
    case [ g | (g, q) ← rest, q ≡ p ] of
        (g : _) → Just (f, g, p)
        []      → repeatedPath rest

legacyFramesKey ∷ Text
legacyFramesKey = "default"

-- * Lifecycle + visual class

-- | @visual_class@ is mandatory: the art slices need to know which
--   family owns a building's textures, and a default would silently
--   answer that question wrongly.
defVisualClass ∷ Text → Aeson.Object → Aeson.Parser BuildingVisualClass
defVisualClass name v = do
    mRaw ← v .:? "visual_class"
    case mRaw of
        Nothing → failT $
            ctx <> " declares no `visual_class`; one of "
                <> keyList visualClassKeyList <> " is required"
        Just raw → case visualClassFromKey raw of
            Just vc → pure vc
            Nothing → failT $
                ctx <> " declares an unknown `visual_class` " <> quoted raw
                    <> "; expected one of " <> keyList visualClassKeyList
  where ctx = "building " <> quoted name

-- | @state_animations@ as a closed role vocabulary. A legacy
--   @appearing@ mapping resolves through 'legacyRoleFor'; declaring it
--   beside the canonical role it resolves to is rejected.
defRoleAnims ∷ Text → Float → Aeson.Object
             → Aeson.Parser (Map.Map BuildingRole Text)
defRoleAnims name buildWork v = do
    raw ← v .:? "state_animations" .!= (Map.empty ∷ Map.Map Text Text)
    let entries = Map.toList raw
        legacyNames = [ a | (k, a) ← entries, k ≡ legacyLifecycleKey ]
    canonical ← Map.fromList ⊚ forM
        [ e | e@(k, _) ← entries, k ≢ legacyLifecycleKey ]
        (\(k, animName) → case roleFromKey k of
            Just r → pure (r, animName)
            Nothing → failT $
                ctx <> " declares an unknown lifecycle key " <> quoted k
                    <> "; expected one of " <> keyList roleKeyList
                    <> " (or the legacy `" <> legacyLifecycleKey <> "`)")
    case legacyNames of
        [] → pure canonical
        (animName : _) → do
            let resolved = legacyRoleFor buildWork
            when (Map.member resolved canonical) $ failT $
                ctx <> " declares the legacy `" <> legacyLifecycleKey
                    <> "` mapping beside the canonical `"
                    <> roleKey resolved <> "` it resolves to; declare "
                    <> "exactly one"
            pure (Map.insert resolved animName canonical)
  where ctx = "building " <> quoted name

-- | Every declared animation, decoded with its own name in scope so a
--   rejection names the building AND the animation.
defAnimations ∷ Text → Aeson.Object
              → Aeson.Parser (Map.Map Text BuildingYamlAnim)
defAnimations name v = do
    raw ← v .:? "animations" .!= (Map.empty ∷ Map.Map Text Aeson.Value)
    Map.fromList ⊚ forM (Map.toList raw)
        (\(animName, val) →
            (,) animName ⊚ parseBuildingAnim name animName val)

-- * Message helpers

failT ∷ Text → Aeson.Parser a
failT = fail ∘ T.unpack

quoted ∷ Text → Text
quoted t = "`" <> t <> "`"

keyList ∷ [Text] → Text
keyList = T.intercalate ", " ∘ map quoted

-- | The pluralising space between "key"\/"keys" and the list itself.
plural ∷ [a] → Text
plural xs = if length xs > 1 then "s " else " "

-- | Decode + validate the three optional power-node keys (#1148).
--
--   A malformed declaration is a PARSE failure, not a silently dropped
--   field: the whole file is refused (loadYamlList logs it and yields
--   no defs), so a mistyped role or a missing rating can never leave a
--   half-declared node that the build tool would route through
--   ordinary building placement. Aeson's `.:?` reads an explicit
--   `key: null` as absent, which Power.Base documents and treats as
--   "not declared".
powerNode ∷ Aeson.Object → Aeson.Parser (Maybe PowerNodeSpec)
powerNode v = do
    mName     ← v .:? "name" .!= "<unnamed>"
    mRole     ← v .:? "power_role"
    mPeak     ← v .:? "power_peak"
    mCapacity ← v .:? "power_capacity"
    case powerNodeSpecFromYaml mRole mPeak mCapacity of
        Right spec → pure spec
        Left  err  → fail (T.unpack ("building def " <> mName <> " " <> err))

newtype BuildingYamlFile = BuildingYamlFile
    { byfBuildings ∷ [BuildingYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON BuildingYamlFile where
    parseJSON = withObject "BuildingYamlFile" $ \v → BuildingYamlFile
        ⊚ v .: "buildings"

-- | 'loadBuildingYaml' with the decode OUTCOME kept (#2203):
--   'Nothing' is a parse failure, @Just xs@ a file that decoded
--   (possibly to an empty list). The startup loader needs the two
--   apart; every other caller reads 'loadBuildingYaml'.
loadBuildingYamlOutcome ∷ LoggerState → FilePath → IO (Maybe [BuildingYamlDef])
loadBuildingYamlOutcome logger =
    loadYamlListOutcome logger "building" "building definitions" byfBuildings

loadBuildingYaml ∷ LoggerState → FilePath → IO [BuildingYamlDef]
loadBuildingYaml logger path =
    fromMaybe [] ⊚ loadBuildingYamlOutcome logger path
