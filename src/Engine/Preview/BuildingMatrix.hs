-- | The DECLARED lifecycle + facing inspection matrix the buildings
--   viewer layers over its filesystem browser (BDA-4, #2492).
--
--   Two authorities, deliberately kept apart:
--
--   * @assets\/textures\/buildings\/\<name\>\/@ remains authoritative
--     for WHICH raw entries are browsable and, inside a recognized
--     animation directory, their frame order. 'Engine.Preview.Building'
--     owns that half and #2492 does not touch it.
--   * @data\/buildings\/\<name\>.yaml@ is the ONLY source of the
--     declared matrix here. Nothing in this module discovers, filters,
--     reorders or relabels a raw entry; it classifies them and adds rows
--     beside them.
--
--   The declaration is read through the GAME's own field decoders
--   ('Engine.Asset.YamlBuildings.defSprites' \/ 'defRoleAnims' \/
--   'defAnimations'), so the closed lifecycle vocabulary, the legacy
--   @appearing@ resolution through 'Building.Schema.legacyRoleFor', the
--   canonical-vs-legacy sprite and frame forms, and every rejection are
--   literally the same code the game applies. A preview-only restatement
--   would drift, and the whole point of the viewer is to show what the
--   game will do.
--
--   Every rejection lands in ONE fallback (#2492 requirement 4): no
--   declared rows at all, and the complete raw browser with its existing
--   default behavior. There is no precedence rule and no pre-boot
--   rejection — a malformed building YAML must still browse, exactly as
--   it did before this module existed.
module Engine.Preview.BuildingMatrix
  ( -- * The declared matrix, as read from YAML
    BuildingDeclaredMatrix(..)
  , DeclaredRole(..)
  , declaredMatrixOf
    -- * Identities
  , lifecycleIdentity
  , spriteIdentity
  , filesystemIdentity
    -- * Resolution against the asset boundary
  , CellStatus(..)
  , missingReasonKey
  , classifyDeclaredPath
  , resolveDeclaredEntries
    -- * Raw-entry classification and the combined list
  , classifyFsEntries
  , defaultSelectionIdentity
    -- * Playback
  , previewFrameIndexAt
  , cyclePhaseAt
  ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (parseMaybe)
import Data.List (find)
import Data.Foldable (toList)
import System.Directory (doesPathExist, pathIsSymbolicLink)
import System.Posix.Files (getSymbolicLinkStatus, isRegularFile, isDirectory)
import System.FilePath (pathSeparator, splitDirectories, joinPath)
import Building.Schema
    ( AssetSource(..), BuildingRole(..), FacingAssets(..), faViews
    , canonicalFacings, facingKey, facingValue, roleKey )
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Asset.YamlBuildings
    ( BuildingYamlAnim(..), defAnimations, defRoleAnims, defSprites
    , nonNegative, optionalFloat )
import Engine.Core.Types
    ( PreviewBuildingEntry(..), PreviewDeclaredEntry(..)
    , PreviewFacingCell(..), PreviewFsClass(..) )
import Engine.Preview.Discovery (isSupportedTextureFile)

-- * The declared matrix

-- | One @state_animations@ mapping, already resolved through the closed
--   role vocabulary — and its referenced animation, when the definition
--   actually declares one.
--
--   'drAnim' being 'Nothing' is requirement 3's diagnostic case, kept
--   deliberately distinguishable from an UNDECLARED role (which simply
--   has no 'DeclaredRole' at all): the definition names an animation it
--   never defines, which is a real authoring fault the viewer exists to
--   surface rather than hide.
data DeclaredRole = DeclaredRole
    { drRole ∷ !BuildingRole
    , drName ∷ !Text
    , drAnim ∷ !(Maybe BuildingYamlAnim)
    } deriving (Eq, Show)

-- | Everything the declared matrix is: the static sprite's own facing
--   set, and the declared lifecycle roles in the fixed
--   @construction, appearance, built, destruction@ order.
data BuildingDeclaredMatrix = BuildingDeclaredMatrix
    { bdmSprite ∷ !(FacingAssets Text)
    , bdmRoles  ∷ ![DeclaredRole]
    } deriving (Eq, Show)

-- | The declared matrix of one building definition object, or 'Nothing'
--   when it cannot yield a usable one.
--
--   Total by construction ('Aeson.parseMaybe'), which is what keeps
--   requirement 4's fallback a fallback: an unknown lifecycle key, a
--   legacy @appearing@ declared beside the canonical role it resolves
--   to, a mixed @sprites@\/@sprite@ or @frames@ form, a negative or
--   non-finite @build_work@, and a malformed animation all answer
--   'Nothing' here — never a partially-trusted matrix, and never a
--   failure that reaches the raw browser.
declaredMatrixOf ∷ Text → Aeson.Object → Maybe BuildingDeclaredMatrix
declaredMatrixOf name v = Aeson.parseMaybe (const parse) (Aeson.Object v)
  where
    parse = do
        -- The gameplay default and the gameplay domain: 'legacyRoleFor'
        -- reads this to decide whether a legacy `appearing` mapping was
        -- describing worker-driven construction or timed appearance, so
        -- a preview that defaulted it differently would expose the
        -- wrong role for exactly the unmigrated definitions the viewer
        -- is meant to inspect.
        buildWork ← optionalFloat ctx "build_work" nonNegative (≥ 0) 0.0 v
        sprite ← defSprites name v
        roles ← defRoleAnims name buildWork v
        anims ← defAnimations name v
        pure BuildingDeclaredMatrix
            { bdmSprite = sprite
            , bdmRoles =
                [ DeclaredRole
                    { drRole = r
                    , drName = animName
                    , drAnim = Map.lookup animName anims
                    }
                | r ← [minBound .. maxBound]
                , Just animName ← [Map.lookup r roles]
                ]
            }
    ctx = "building `" <> name <> "`"

-- * Identities

-- | @lifecycle:\<role\>@ — a declared lifecycle row.
lifecycleIdentity ∷ BuildingRole → Text
lifecycleIdentity r = "lifecycle:" <> roleKey r

-- | @sprite@ — the declared static row.
spriteIdentity ∷ Text
spriteIdentity = "sprite"

-- | @filesystem:\<label\>@ — a raw browser row. Prefixed so a lifecycle
--   row and the raw row backing the very same files cannot alias each
--   other, which is the whole reason duplicate-looking rows are safe.
filesystemIdentity ∷ Text → Text
filesystemIdentity = ("filesystem:" <>)

-- * Resolution against the asset boundary

-- | Why one declared path cannot be shown. Nothing here corrects a
--   declaration: a diagnostic path keeps its declared spelling and is
--   simply never requested.
data CellStatus
    = CellLoadable
    | CellAbsent
    | CellDirectory
    | CellSymlink
    | CellSpecial
    | CellUnsupportedExtension
    | CellOutsideRoot
      -- ^ The declared path does not resolve under the building's own
      --   asset folder. Requesting it would load a texture from outside
      --   the requested item, which is exactly what the trimmed-loading
      --   contract forbids — so it is a missing cell with its own
      --   reason rather than a load.
    | CellUnresolved
      -- ^ Not a path verdict at all: the lifecycle row's animation
      --   reference itself never resolved, so there is no declared path
      --   to check.
    deriving (Eq, Show)

missingReasonKey ∷ CellStatus → Maybe Text
missingReasonKey CellLoadable             = Nothing
missingReasonKey CellAbsent               = Just "absent"
missingReasonKey CellDirectory            = Just "directory"
missingReasonKey CellSymlink              = Just "symlink"
missingReasonKey CellSpecial              = Just "special"
missingReasonKey CellUnsupportedExtension = Just "unsupported_extension"
missingReasonKey CellOutsideRoot          = Just "outside_root"
missingReasonKey CellUnresolved           = Just "unresolved"

-- | Judge ONE declared path against the building-preview asset
--   boundary, in the order the checks can be answered without trusting
--   the previous one:
--
--   1. containment under @root@ — a purely syntactic test, so it runs
--      before any filesystem call reaches a path that should never be
--      touched;
--   2. the supported-extension NAME test, the same rule
--      'Engine.Preview.Discovery.isSupportedTextureFile' applies to
--      discovery, so a browsable path and a declared one can never
--      disagree about what a texture is;
--   3. existence, then the TYPE from a real @lstat@.
--
--   The symlink test comes before the directory\/regular reading for the
--   same reason 'Engine.Preview.Building.discoverBuildingEntries' keeps
--   its own: @lstat@ not following links is what makes a symlink a
--   non-regular file, but "symlinks are refused" is a stated rule and
--   must not survive only as a side effect of how a type is read.
--   Neither existence predicate answers the type question —
--   @doesDirectoryExist@ misses a FIFO and @doesFileExist@ accepts one.
classifyDeclaredPath ∷ FilePath → Text → IO CellStatus
classifyDeclaredPath root declared
    | not (containedIn root path) = pure CellOutsideRoot
    | not (isSupportedTextureFile path) = pure CellUnsupportedExtension
    | otherwise = do
        exists ← doesPathExist path
        if not exists then pure CellAbsent else do
            anyLink ← anyAncestorIsSymlink root path
            if anyLink then pure CellSymlink else do
                st ← getSymbolicLinkStatus path
                pure $ if isDirectory st then CellDirectory
                       else if isRegularFile st then CellLoadable
                       else CellSpecial
  where path = T.unpack declared

-- | Whether @path@ names something at or under @root@, judged
--   syntactically on normalized components. An absolute path, a @..@
--   component, or a different root all answer 'False'.
containedIn ∷ FilePath → FilePath → Bool
containedIn root path =
    case stripComponents (norm root) (norm path) of
        Nothing   → False
        Just rest → not (null rest) ∧ notElem ".." rest
  where
    norm = filter (≢ ".") ∘ splitDirectories ∘ normSlashes
    stripComponents [] ys = Just ys
    stripComponents _  [] = Nothing
    stripComponents (x : xs) (y : ys)
        | x ≡ y     = stripComponents xs ys
        | otherwise = Nothing

-- | Whether any component of @path@ BELOW @root@ is a symlink. The
--   whole chain matters, not just the leaf: @root/link/frame_000.png@
--   with a symlinked @link/@ would otherwise pass a leaf-only test and
--   load another tree's texture through a contained-looking path.
anyAncestorIsSymlink ∷ FilePath → FilePath → IO Bool
anyAncestorIsSymlink root path = go (norm root) (drop (length (norm root)) (norm path))
  where
    norm = filter (≢ ".") ∘ splitDirectories ∘ normSlashes
    go _ [] = pure False
    go acc (c : cs) = do
        let acc' = acc ⧺ [c]
        isLink ← pathIsSymbolicLink (joinPath acc')
        if isLink then pure True else go acc' cs

-- | Platform-independent path comparison: YAML always spells its paths
--   with @\/@, while a discovered path is built with
--   'System.FilePath.</>'.
normSlashes ∷ FilePath → FilePath
normSlashes = map (\c → if c ≡ pathSeparator then '/' else c)

-- | Build the declared rows of a resolved matrix: the lifecycle roles
--   in their fixed order, then the static sprite.
--
--   @root@ is the building's own asset folder — the containment
--   denominator, never the category root.
resolveDeclaredEntries
    ∷ FilePath                    -- ^ the building's own asset folder
    → [PreviewBuildingEntry]      -- ^ the raw browser, for the projections
    → BuildingDeclaredMatrix
    → IO [PreviewDeclaredEntry]
resolveDeclaredEntries root entries matrix = do
    roleRows ← forM (bdmRoles matrix) lifecycleRow
    spriteRow ← staticRow
    pure (roleRows ⧺ [spriteRow])
  where
    lifecycleRow dr = case drAnim dr of
        -- Requirement 3: RETAINED as a diagnostic row naming its role
        -- and the animation it referenced. Four cells, every one
        -- missing for the one reason that actually applies, so
        -- selecting it terminates in the missing presentation instead
        -- of substituting art from anywhere else.
        Nothing → pure PreviewDeclaredEntry
            { pdeIdentity  = lifecycleIdentity (drRole dr)
            , pdeKind      = "lifecycle"
            , pdeLabel     = lifecycleLabel dr
            , pdeRole      = Just (roleKey (drRole dr))
            , pdeAnimName  = Just (drName dr)
            , pdeResolved  = False
            , pdeFps       = 0
            , pdeLoop      = False
            , pdeSource    = sourceKey AssetCanonical
            , pdeLegacy    = False
            , pdeCells     = [ unresolvedCell f | f ← canonicalFacings ]
            , pdeProjected = Nothing
            }
        Just ya → do
            let views = byaFrames ya
                legacy = faSource views ≡ AssetLegacy
            cells ← forM canonicalFacings $ \f →
                cellFor legacy f (facingValue f (faViews views))
            pure PreviewDeclaredEntry
                { pdeIdentity  = lifecycleIdentity (drRole dr)
                , pdeKind      = "lifecycle"
                , pdeLabel     = lifecycleLabel dr
                , pdeRole      = Just (roleKey (drRole dr))
                , pdeAnimName  = Just (drName dr)
                , pdeResolved  = True
                , pdeFps       = byaFps ya
                , pdeLoop      = byaLoop ya
                , pdeSource    = sourceKey (faSource views)
                , pdeLegacy    = legacy
                , pdeCells     = cells
                , pdeProjected = animProjection views
                }

    staticRow = do
        let views = bdmSprite matrix
            legacy = faSource views ≡ AssetLegacy
        cells ← forM canonicalFacings $ \f →
            cellFor legacy f [facingValue f (faViews views)]
        pure PreviewDeclaredEntry
            { pdeIdentity  = spriteIdentity
            , pdeKind      = "sprite"
            , pdeLabel     = "sprite"
            , pdeRole      = Nothing
            , pdeAnimName  = Nothing
            , pdeResolved  = True
            , pdeFps       = 0
            , pdeLoop      = False
            , pdeSource    = sourceKey (faSource views)
            , pdeLegacy    = legacy
            , pdeCells     = cells
            , pdeProjected = spriteProjection views
            }

    -- The whole ordered list is kept whatever the verdict: the cell IS
    -- the declaration, and a diagnostic that hid what was declared
    -- would tell an art reviewer nothing. The reason reported is the
    -- FIRST failing path's, so a partly-authored facing names the
    -- concrete fault rather than a summary.
    cellFor legacy f paths = do
        statuses ← mapM (classifyDeclaredPath root) paths
        let bad = find (≢ CellLoadable) statuses
        pure PreviewFacingCell
            { pfcFacing        = facingKey f
            , pfcPaths         = paths
            , pfcMissing       = isJust bad ∨ null paths
            , pfcMissingReason = case bad of
                Just st → missingReasonKey st
                Nothing | null paths → missingReasonKey CellAbsent
                        | otherwise  → Nothing
            , pfcLegacy        = legacy
            }

    unresolvedCell f = PreviewFacingCell
        { pfcFacing        = facingKey f
        , pfcPaths         = []
        , pfcMissing       = True
        , pfcMissingReason = missingReasonKey CellUnresolved
        , pfcLegacy        = False
        }

    lifecycleLabel dr = roleKey (drRole dr) <> " \8594 " <> drName dr

    -- Requirement 11's compatibility projection, FACING-INDEPENDENT by
    -- design: it is the pre-#2492 matching rule, unchanged, so
    -- `selected` keeps meaning what every existing consumer reads it as
    -- and turning the camera in the viewer cannot move it.
    animProjection views =
        let wanted = map normText (concat (toList (faViews views)))
        in pbeLabel ⊚ find (\e → pbeAnimated e
                              ∧ any ((`elem` wanted) ∘ normText) (pbeFrames e))
                          entries
    spriteProjection views =
        let south = normText (facingValue FaceSouth (faViews views))
        in pbeLabel ⊚ find (\e → not (pbeAnimated e)
                              ∧ any ((≡ south) ∘ normText) (pbeFrames e))
                          entries

    normText = T.pack ∘ normSlashes ∘ T.unpack

sourceKey ∷ AssetSource → Text
sourceKey AssetCanonical = "canonical"
sourceKey AssetLegacy    = "legacy"

-- * Raw-entry classification

-- | Classify every raw entry against the declared rows WITHOUT
--   filtering, reordering or relabelling one. A raw row whose files
--   also back a declaration reports that overlap; one that backs none
--   reports @undeclared@. Both stay selectable with exactly their
--   existing identity, order and playback.
classifyFsEntries
    ∷ [PreviewDeclaredEntry] → [PreviewBuildingEntry] → [PreviewFsClass]
classifyFsEntries declared = map classify
  where
    classify e =
        let overlaps = [ pdeIdentity d | d ← declared, overlapsWith d e ]
        in PreviewFsClass
            { pfcsLabel      = pbeLabel e
            , pfcsIdentity   = filesystemIdentity (pbeLabel e)
            , pfcsDeclared   = overlaps
            , pfcsUndeclared = null overlaps
            }
    overlapsWith d e =
        let want = [ normText p | c ← pdeCells d, p ← pfcPaths c ]
        in any ((`elem` want) ∘ normText) (pbeFrames e)
    normText = T.pack ∘ normSlashes ∘ T.unpack

-- | Requirement 13's initial selection: the declared @built@ row, else
--   the declared sprite row, else the raw row the UNCHANGED
--   'Engine.Preview.Building.defaultBuildingEntry' ladder named, else
--   empty for an empty browser.
--
--   A declared @built@ row wins even when it is a pure diagnostic —
--   that is the point of exposing it: an art reviewer opening a
--   building must land on the state the game shows most, and see at
--   once that its art is missing.
defaultSelectionIdentity
    ∷ [PreviewDeclaredEntry] → Text → [PreviewBuildingEntry] → Text
defaultSelectionIdentity declared rawDefault entries
    | Just d ← find ((≡ builtId) ∘ pdeIdentity) declared = pdeIdentity d
    | Just d ← find ((≡ spriteIdentity) ∘ pdeIdentity) declared = pdeIdentity d
    | not (T.null rawDefault) = filesystemIdentity rawDefault
    | Just e ← listToMaybe entries = filesystemIdentity (pbeLabel e)
    | otherwise = ""
  where builtId = lifecycleIdentity RoleBuilt

-- * Playback

-- | The preview's frame index at @elapsed@ — the same forced-replay
--   rule 'Engine.Preview.Unit.frameIndexAt' states and
--   @scripts/ui/building_asset_view.lua@ implements: @floor (elapsed *
--   fps)@ modulo the frame count, for EVERY clip whatever its authored
--   @loop@ says (#1833).
previewFrameIndexAt ∷ Float → Int → Double → Int
previewFrameIndexAt fps frameCount elapsed
    | frameCount ≤ 1 = 0
    | otherwise =
        let rate = max 0 (realToFrac fps ∷ Double)
            raw  = floor (max 0 elapsed * rate) ∷ Int
        in raw `mod` frameCount

-- | Where inside its current forced-replay cycle the preview is, as a
--   fraction in @[0, 1)@.
--
--   This is the bridge requirement 15 needs for the CONSTRUCTION role:
--   gameplay indexes construction by build progress, not by a clock, so
--   equality is asserted by mapping this phase onto a build fraction.
--   For the timed roles the cycle-LOCAL elapsed time
--   (@phase * frameCount / fps@) is what gameplay is compared against,
--   because gameplay clamps a non-looping clip and expires a
--   destruction effect while the preview deliberately replays.
cyclePhaseAt ∷ Float → Int → Double → Double
cyclePhaseAt fps frameCount elapsed
    | frameCount ≤ 0 = 0
    | rate ≤ 0       = 0
    | otherwise      =
        let cycleLen = fromIntegral frameCount / rate
            within   = max 0 elapsed - cycleLen * fromIntegral
                          (floor (max 0 elapsed / cycleLen) ∷ Int)
        in within / cycleLen
  where rate = max 0 (realToFrac fps ∷ Double)
