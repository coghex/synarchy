{-# LANGUAGE Strict #-}
-- | The building asset + lifecycle DECLARATION vocabulary (BDA-1,
--   #2080), shared by the YAML decoder ('Engine.Asset.YamlBuildings')
--   and the runtime definition ('Building.Types').
--
--   Buildings are drawn from one of the four camera facings, so a
--   building asset is declared once per 'CameraFacing' — never per
--   'Unit.Direction.Direction', which has eight values and a mirror
--   flag that mean nothing here. The declaration is TOTAL: a
--   'FacingSet' has a value for every facing by construction, so no
--   consumer can ask for a view the definition failed to declare.
--
--   BDA-1 introduces the shape and keeps rendering on the south view;
--   BDA-2 owns selecting a view from the active camera, BDA-3 owns
--   destruction playback, and BDA-13 owns the final audit that rejects
--   'AssetLegacy' declarations from shipped definitions.
--
--   The as-built reference — canonical YAML, every rejection, the two
--   independent migration axes, and the loader's registry naming — is
--   @docs/building_asset_schema.md@.
module Building.Schema
    ( -- * Camera-facing asset sets
      FacingSet(..)
    , facingValue
    , canonicalFacings
    , facingKey
    , facingFromKey
    , facingKeyList
      -- * Declaration provenance
    , AssetSource(..)
    , FacingAssets(..)
    , canonicalAssets
    , legacyAssets
    , facingAsset
    , isLegacyDeclared
      -- * Lifecycle roles
    , BuildingRole(..)
    , roleKey
    , roleFromKey
    , roleKeyList
    , legacyLifecycleKey
    , legacyRoleFor
      -- * Visual class
    , BuildingVisualClass(..)
    , visualClassKey
    , visualClassFromKey
    , visualClassKeyList
    ) where

import UPrelude
import Engine.Graphics.Camera (CameraFacing(..))

-- | One value per camera facing. Total by construction — there is no
--   \"missing direction\" case downstream, because the decoder refuses
--   an incomplete declaration instead.
--
--   Field order IS the canonical declaration order (south, west,
--   north, east), so the derived 'Foldable' enumerates the facings in
--   the order the YAML spells them.
data FacingSet a = FacingSet
    { fsSouth ∷ !a
    , fsWest  ∷ !a
    , fsNorth ∷ !a
    , fsEast  ∷ !a
    } deriving (Show, Eq, Functor, Foldable, Traversable)

facingValue ∷ CameraFacing → FacingSet a → a
facingValue FaceSouth = fsSouth
facingValue FaceWest  = fsWest
facingValue FaceNorth = fsNorth
facingValue FaceEast  = fsEast

-- | The canonical declaration order: south, west, north, east.
canonicalFacings ∷ [CameraFacing]
canonicalFacings = [FaceSouth, FaceWest, FaceNorth, FaceEast]

-- | The YAML key naming a facing. The key set is CLOSED: anything not
--   produced here is a parse error, never a silently ignored block.
facingKey ∷ CameraFacing → Text
facingKey FaceSouth = "south"
facingKey FaceWest  = "west"
facingKey FaceNorth = "north"
facingKey FaceEast  = "east"

facingFromKey ∷ Text → Maybe CameraFacing
facingFromKey "south" = Just FaceSouth
facingFromKey "west"  = Just FaceWest
facingFromKey "north" = Just FaceNorth
facingFromKey "east"  = Just FaceEast
facingFromKey _       = Nothing

-- | Every canonical facing key, in declaration order — the exact set a
--   rejection message quotes back at the author.
facingKeyList ∷ [Text]
facingKeyList = map facingKey canonicalFacings

-- | How a facing set was DECLARED.
--
--   'AssetLegacy' records that the definition supplied ONE pre-#2080
--   path (a singular @sprite@, or a @frames.default@ list) which the
--   loader exposes through all four views. That is deliberately
--   distinguishable from a real four-facing declaration: BDA-13's
--   whole-tree audit rejects it from shipped definitions once the art
--   slices have migrated every building, and it cannot do that if the
--   compatibility branch is indistinguishable from the canonical one.
data AssetSource = AssetCanonical | AssetLegacy
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | A facing set together with the provenance of its declaration.
data FacingAssets a = FacingAssets
    { faSource ∷ !AssetSource
    , faViews  ∷ !(FacingSet a)
    } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Four independently declared views.
canonicalAssets ∷ FacingSet a → FacingAssets a
canonicalAssets = FacingAssets AssetCanonical

-- | One legacy value exposed through all four views. The ONLY
--   construction that repeats a value across facings — a canonical
--   declaration never mirrors, aliases or collapses direction keys.
legacyAssets ∷ a → FacingAssets a
legacyAssets x = FacingAssets AssetLegacy (FacingSet x x x x)

facingAsset ∷ CameraFacing → FacingAssets a → a
facingAsset f = facingValue f ∘ faViews

isLegacyDeclared ∷ FacingAssets a → Bool
isLegacyDeclared = (≡ AssetLegacy) ∘ faSource

-- | The closed lifecycle vocabulary a definition maps to animation
--   names. Construction (worker-driven), timed appearance, the built
--   loop and destruction are separately addressable even when a
--   particular definition declares only some of them.
--
--   'RoleDestruction' is declarable here and deliberately not played
--   yet — BDA-3 owns destruction timing and playback.
data BuildingRole
    = RoleConstruction
    | RoleAppearance
    | RoleBuilt
    | RoleDestruction
    deriving (Show, Eq, Ord, Enum, Bounded)

roleKey ∷ BuildingRole → Text
roleKey RoleConstruction = "construction"
roleKey RoleAppearance   = "appearance"
roleKey RoleBuilt        = "built"
roleKey RoleDestruction  = "destruction"

roleFromKey ∷ Text → Maybe BuildingRole
roleFromKey "construction" = Just RoleConstruction
roleFromKey "appearance"   = Just RoleAppearance
roleFromKey "built"        = Just RoleBuilt
roleFromKey "destruction"  = Just RoleDestruction
roleFromKey _              = Nothing

roleKeyList ∷ [Text]
roleKeyList = map roleKey [minBound .. maxBound]

-- | The pre-#2080 lifecycle key. It overloaded worker-driven
--   construction and timed materialisation onto one name, which is
--   exactly the ambiguity 'BuildingRole' removes.
legacyLifecycleKey ∷ Text
legacyLifecycleKey = "appearing"

-- | Which canonical role a legacy @appearing@ mapping resolves to: a
--   positive @build_work@ definition was always describing worker-driven
--   construction, a zero-work one always described timed appearance.
--   The SAME discriminator picks the role a Built building pins its
--   last frame from, so a migrated definition and an unmigrated one
--   render identically.
legacyRoleFor ∷ Float → BuildingRole
legacyRoleFor buildWork
    | buildWork > 0 = RoleConstruction
    | otherwise     = RoleAppearance

-- | Which art family a building belongs to (#2080 requirement 8).
--   Records asset OWNERSHIP for the art slices; it does not affect
--   placement, gameplay, or rendering.
data BuildingVisualClass
    = IndoorFixture
    | FreestandingInstallation
    | Gateway
    deriving (Show, Eq, Ord, Enum, Bounded)

visualClassKey ∷ BuildingVisualClass → Text
visualClassKey IndoorFixture            = "indoor_fixture"
visualClassKey FreestandingInstallation = "freestanding_installation"
visualClassKey Gateway                  = "gateway"

visualClassFromKey ∷ Text → Maybe BuildingVisualClass
visualClassFromKey "indoor_fixture"            = Just IndoorFixture
visualClassFromKey "freestanding_installation" = Just FreestandingInstallation
visualClassFromKey "gateway"                   = Just Gateway
visualClassFromKey _                           = Nothing

visualClassKeyList ∷ [Text]
visualClassKeyList = map visualClassKey [minBound .. maxBound]
