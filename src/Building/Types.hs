{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
module Building.Types
    ( BuildingId(..)
    , BuildingAnimation(..)
    , buildingAnimMaxFrames
    , BuildingActivity(..)
    , buildingActivityLabel
    , BuildingDef(..)
    , bdSouthTexture
    , BuildingInstance(..)
    , BuildingGhost(..)
    , BuildingManager(..)
    , emptyBuildingManager
    , nextBuildingId
    , currentActivity
    , materialsSatisfied
    , footprintDist
    , footprintDistAt
    , footprintDistBetween
    , footprintTiles
    , buildingsOnPage
    , buildingsOnPages
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Building.Schema
import World.Page.Types (WorldPageId(..))
import Item.Types (ItemInstance)
import Power.Base (PowerNodeSpec)

-- | One building animation: a flat per-frame sequence of whole-image
--   textures, held once per camera facing.
--
--   Buildings used to share the unit 'Unit.Types.Def.Animation' record,
--   whose per-frame representation #1261 (TEX-6) retired for units. D-8
--   leaves building animation STORAGE untouched — nothing compiles a
--   building to an atlas and no building index exists — so the
--   representation moved here instead of being deleted.
--
--   Frames are keyed by 'Engine.Graphics.Camera.CameraFacing' (#2080),
--   not by 'Unit.Direction.Direction': a building has four camera views,
--   never eight unit facings, and there is no mirror flag because a
--   canonical declaration never mirrors one view into another. The four
--   views are independently addressable, and 'faSource' records whether
--   they came from four declared frame lists or from one legacy
--   @frames.default@ list exposed through all four (BDA-13 rejects the
--   latter from shipped definitions once the art slices have landed).
data BuildingAnimation = BuildingAnimation
    { banFps    ∷ !Float
    , banLoop   ∷ !Bool
    , banFrames ∷ !(FacingAssets (V.Vector TextureHandle))
    } deriving (Show, Eq)

-- | The longest facing's frame count (0 when the animation has no
--   frames at all) — the clip-LENGTH question 'currentActivity' asks to
--   derive an appear animation's duration. The decoder equalizes the
--   four counts, so this is simply the clip length; it stays a maximum
--   so a hand-built fixture cannot read short.
buildingAnimMaxFrames ∷ BuildingAnimation → Int
buildingAnimMaxFrames anim =
    maximum (V.length <$> [ facingAsset f (banFrames anim)
                          | f ← canonicalFacings ])

newtype BuildingId = BuildingId { unBuildingId ∷ Word32 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, Serialize)

-- | Derived from build progress or elapsed time, NOT stored on the
--   instance (#2080 splits the old overloaded @Appearing@ in two):
--
--   * 'Constructing' — a positive-@build_work@ definition whose
--     'biBuildProgress' has not reached 'bdBuildWork' yet. Worker-driven,
--     and frozen whenever no worker is contributing.
--   * 'Appearing' — a zero-work definition still inside its declared
--     timed appearance. Elapsed-game-time driven; nobody builds it.
--   * 'Built' — both eventually reach this.
data BuildingActivity = Constructing | Appearing | Built
    deriving (Show, Eq)

-- | The string @building.getActivity@ hands Lua. Lives here beside the
--   type so the wire vocabulary and the Haskell one cannot drift, and
--   so the mapping is assertable without a live Lua state.
buildingActivityLabel ∷ BuildingActivity → Text
buildingActivityLabel Constructing = "constructing"
buildingActivityLabel Appearing    = "appearing"
buildingActivityLabel Built        = "built"

-- | Definition loaded from YAML, immutable after load. Mirrors UnitDef
--   in shape but minus the directional-sprite + state-machine concerns
--   that buildings don't have.
data BuildingDef = BuildingDef
    { bdName        ∷ !Text
    , bdDisplayName ∷ !Text               -- ^ shown in build menu + tooltips
    , bdCategory    ∷ !Text               -- ^ build-menu tab key ("Starting", "Cargo", ...)
    , bdDescription ∷ !Text               -- ^ build-menu tooltip hint body
    , bdTextures    ∷ !(FacingAssets TextureHandle)
      -- ^ The four static views (#2080), one independently addressable
      --   handle per camera facing, plus whether they were declared
      --   canonically or came from one legacy @sprite@ path. Only the
      --   south view is drawn in this slice ('bdSouthTexture'); BDA-2
      --   owns selecting a view from the active camera.
    , bdIconTexture ∷ !TextureHandle
      -- ^ The SAME south sprite uploaded under the UI policy (#2075),
      --   for the build menu's `iconTex`. The menu shows one view, and
      --   south is it. A second handle on a second slot,
      --   because a slot's sampler is fixed by the policy that uploaded
      --   it and this art is drawn both in the world and in a panel.
      --   Never used by 'Building.Render'.
    , bdTileW       ∷ !Int                -- ^ footprint width in tiles
    , bdTileH       ∷ !Int                -- ^ footprint height in tiles
    , bdPlacement   ∷ !Text               -- ^ "flat_ground" / future kinds
    , bdIsStarting  ∷ !Bool
    , bdRace        ∷ !Text
    , bdSpriteAnchor∷ !Text
      -- ^ Where the sprite's bottom edge lands relative to the tile:
      --   "diamond_bottom" (default) — south point of the top face;
      --   "tile_bottom"    — bottom of the side face (= cube's lowest
      --   point). Use "tile_bottom" when the texture draws its own
      --   side-face below the diamond, so the side face lines up with
      --   the world tile's side face instead of dangling into the
      --   tile below.
    , bdBuildWork   ∷ !Float
      -- ^ Worker-seconds needed to finish construction at the single-
      --   worker base rate. 0 = instant-built (portal-style: the
      --   building flips to Built as soon as the appearing animation
      --   completes, with no acolyte assignment needed). When > 0,
      --   Constructing→Built and the construction-anim frame are
      --   derived from biBuildProgress / bdBuildWork instead of
      --   elapsed time.
    , bdMaterials   ∷ !(HM.HashMap Text Int)
      -- ^ Materials required to start construction. Item def name →
      --   integer count. Empty (default) = no materials gate: progress
      --   ticks as soon as workers arrive. Non-empty: building shows
      --   as ghost until all required counts have been delivered (see
      --   biMaterialsDelivered), then construction begins.
    , bdStorageCapacity ∷ !Float
      -- ^ Maximum total weight (kg) the building's storage can hold.
      --   0 (default) = no storage; the building doesn't show an
      --   inventory panel. Non-zero = built-state buildings can be
      --   deposited into / withdrawn from. Storage is separate from
      --   biMaterialsDelivered: materials consumed into the build are
      --   locked for future deconstruction recovery; storage items
      --   are free-floating cargo the player and AI move around.
    , bdOperations  ∷ ![Text]
      -- ^ Work-station operations this building offers once Built
      --   (#326): recipe `station` kinds it can run ("smelt", "forge",
      --   "assemble", …) plus "repair_condition"/"repair_sharpness"
      --   for the repair flows (#301) — split per wear axis so
      --   findStation/executeAt route unambiguously to the right
      --   station. Empty (default) = not a work station. craft.executeAt
      --   validates the recipe's rdStation against this list;
      --   building.findStation routes by it.
    , bdAnimations  ∷ !(HM.HashMap Text BuildingAnimation)
    , bdRoleAnims   ∷ !(Map.Map BuildingRole Text)
      -- ^ Lifecycle role → animation name in bdAnimations (#2080).
      --   Construction, timed appearance, the built loop and
      --   destruction are separately addressable; a definition declares
      --   only the roles it has art for. 'RoleDestruction' is
      --   declarable and not yet played — BDA-3 owns that.
    , bdVisualClass ∷ !BuildingVisualClass
      -- ^ Which art family owns this building's textures (#2080).
      --   Records ownership for the art slices; it changes no
      --   placement or gameplay behaviour.
    , bdPowerDrain    ∷ !Float
      -- ^ Watts drawn whenever this building is Built (#361) — flat,
      --   not scaled by whether anything is actively happening at it;
      --   an always-on appliance's constant draw. 0 (default) = an
      --   ordinary building, unaffected by the power grid — a building
      --   is a power CONSUMER iff this is > 0; there's no separate
      --   requires_power flag to fall out of sync with it. Deliberately
      --   not a Power.Types PowerRole/PowerNode — a consumer never
      --   gets a registry entry; Power.Network derives its tile +
      --   drain fresh from BuildingManager + this field every call
      --   (see Power.Network.consumersOn), the same way its position
      --   is already derived rather than duplicated.
      --   #590 SUPERSEDES this for CRAFT STATIONS: a station's actual
      --   electrical load is job-dependent (Craft.Types.rdPowerDraw,
      --   drawn only while a power-tagged recipe is actively being
      --   worked — see Power.Network.activeCraftConsumersOn), not a
      --   flat per-building wattage. This field remains for a
      --   hypothetical future ALWAYS-ON non-crafting device (lights,
      --   etc.); no shipped or crafting building should set it.
    , bdPowerNode     ∷ !(Maybe PowerNodeSpec)
      -- ^ The power-registry NODE this def mints when placed (#1148) —
      --   'Power.Base.PowerNodeSource' with its peak watts, or
      --   'Power.Base.PowerNodeStorage' with its capacity Wh. Nothing
      --   (default) = an ordinary building, not placeable through
      --   @power.placeNode@ at all.
      --
      --   This is the node half of what 'bdPowerDrain' is for
      --   consumers: both now come off the def's own YAML, so a third
      --   power device is a content file rather than a source edit
      --   (it replaced a hardcoded two-name catalogue in
      --   'Power.Types'). It is also the whole placeability registry —
      --   @power.isPlaceable@ answers from exactly this field, which is
      --   what keeps the build tool's item-consuming route and the free
      --   @building.spawn@ route agreeing without a second list in Lua.
      --
      --   Read at PLACEMENT time and snapshotted into the
      --   'Power.Types.PowerNode' the placement mints, so editing a
      --   rating here changes what LATER placements get; nodes already
      --   placed (or loaded from a save) keep the values they were
      --   built with.
    } deriving (Show, Eq)

-- | A placed building. anchor = bottom-left corner of the footprint.
data BuildingInstance = BuildingInstance
    { biDefName    ∷ !Text
    , biPage       ∷ !WorldPageId
      -- ^ which world this building belongs to. Runtime-only (not
      --   serialized — a save holds one world; loaded buildings are
      --   stamped with the load target page). Scopes placement/render so
      --   a building in one world never blocks or draws in another (#76).
    , biTexture    ∷ !TextureHandle      -- ^ copied from def
    , biAnchorX    ∷ !Int                -- ^ tile coords (footprint origin)
    , biAnchorY    ∷ !Int
    , biGridZ      ∷ !Int                -- ^ vertical layer (terrain Z at place time)
    , biSpawnedAt  ∷ !Double             -- ^ game-time seconds when placed
    , biTileW      ∷ !Int                -- ^ cached from def for cheap iteration
    , biTileH      ∷ !Int
    , biSpawnRemaining ∷ !Int
      -- ^ Roster countdown for the spawn sequencer. 0 = no more units to
      --   spawn (or building doesn't spawn anything). Engine doesn't
      --   interpret this — it's set + decremented by Lua's
      --   building_spawn module. Lives here (not in Lua module state)
      --   so it survives save/load and chunk-eviction without a
      --   separate Lua serializer.
    , biBuildProgress  ∷ !Float
      -- ^ Accumulated worker-seconds toward bdBuildWork. Reaches
      --   bdBuildWork → Constructing flips to Built. Driven by Lua's
      --   construction tick via building.addBuildProgress; engine
      --   only reads it via currentActivity / pickBuildingFrame.
    , biMaterialsDelivered ∷ !(HM.HashMap Text [ItemInstance])
      -- ^ Actual items consumed into the build. Keyed by item def
      --   name; each list preserves the full ItemInstance (so the
      --   electric motors that built this cargo hold come back out
      --   with their then-current condition when it's deconstructed).
      --   The construction tick is gated on this satisfying bdMaterials.
    , biStorage          ∷ ![ItemInstance]
      -- ^ Cargo currently stored in this building. Each entry is a
      --   full ItemInstance, so per-item quality / condition / fill
      --   round-trip through deposit→withdraw exactly. Capacity is
      --   bdStorageCapacity (kg); the deposit API enforces it.
    } deriving (Show, Eq)

-- | Singleton ghost preview: one optional def + tile + valid flag.
--   Drawn by the render pass when present, cleared by the build tool
--   when leaving placement mode.
data BuildingGhost = BuildingGhost
    { bgDefName ∷ !Text
    , bgGridX   ∷ !Int
    , bgGridY   ∷ !Int
    , bgGridZ   ∷ !Int
      -- ^ Terrain surface Z at the ghost tile, sampled by setGhost.
      --   Render pass uses this to apply the same height offset placed
      --   buildings get, so the ghost previews where the building will
      --   actually land. Without it the ghost sits at zSlice while the
      --   placed building sits at terrainZ — visible as a vertical
      --   offset between cursor + ghost on non-flat terrain (arena
      --   testing didn't reveal this because every arena tile is at
      --   the same Z).
    , bgValid   ∷ !Bool
    } deriving (Show, Eq)

data BuildingManager = BuildingManager
    { bmDefs      ∷ !(HM.HashMap Text BuildingDef)
    , bmInstances ∷ !(HM.HashMap BuildingId BuildingInstance)
    , bmNextId    ∷ !Word32
    , bmSelected  ∷ !(Maybe BuildingId)
      -- ^ Single-select for now. Units use a HashSet; buildings stay
      --   single until there's a real multi-select use case. Cleared
      --   automatically when the selected building is destroyed.
    } deriving (Show, Eq)

emptyBuildingManager ∷ BuildingManager
emptyBuildingManager = BuildingManager
    { bmDefs      = HM.empty
    , bmInstances = HM.empty
    , bmNextId    = 1
    , bmSelected  = Nothing
    }

nextBuildingId ∷ BuildingManager → (BuildingId, BuildingManager)
nextBuildingId bm =
    let bid = BuildingId (bmNextId bm)
    in (bid, bm { bmNextId = bmNextId bm + 1 })

-- | Buildings belonging to one specific world page (active-world
--   placement / occupancy scoping, #76).
buildingsOnPage ∷ WorldPageId
                → HM.HashMap BuildingId BuildingInstance
                → HM.HashMap BuildingId BuildingInstance
buildingsOnPage pid = HM.filter (\bi → biPage bi ≡ pid)

-- | Buildings belonging to any of the given world pages (the visible set,
--   for rendering).
buildingsOnPages ∷ HS.HashSet WorldPageId
                 → HM.HashMap BuildingId BuildingInstance
                 → HM.HashMap BuildingId BuildingInstance
buildingsOnPages pages = HM.filter (\bi → HS.member (biPage bi) pages)

-- | The south view — the one static handle this slice renders and
--   copies onto a placed instance. BDA-2 owns making that a choice.
bdSouthTexture ∷ BuildingDef → TextureHandle
bdSouthTexture = facingAsset FaceSouth ∘ bdTextures

-- | True iff every entry in bdMaterials has at least the required
--   count delivered. Empty bdMaterials trivially satisfies (the
--   portal and other legacy defs).
materialsSatisfied ∷ BuildingInstance → BuildingDef → Bool
materialsSatisfied inst def =
    all (\(t, n) → length (HM.lookupDefault [] t (biMaterialsDelivered inst)) >= n)
        (HM.toList (bdMaterials def))

-- | The tile rectangle an anchor + tile_size footprint covers:
--   @[ax..ax+w-1] x [ay..ay+h-1]@. The single anchor/tile_size
--   convention shared by placement validation ('Building.Placement.
--   checkFlatGround'), 'building.spawn', and — since #807 — the
--   committed-blueprint render pass ('World.Construct.Types.
--   constructDesignationFootprint'), so none of them can drift apart.
footprintTiles ∷ Int → Int → Int → Int → [(Int, Int)]
footprintTiles ax ay w h =
    [(x, y) | x ← [ax .. ax + w - 1], y ← [ay .. ay + h - 1]]

-- | Chebyshev distance from a tile to the nearest tile of a
--   building's footprint. 0 = standing on it; 1 = adjacent (incl.
--   diagonals) — the "close enough to work here" test shared by
--   building.findStation and craft.executeAt (#326).
footprintDist ∷ BuildingInstance → (Int, Int) → Int
footprintDist inst =
    footprintDistAt (biAnchorX inst, biAnchorY inst)
                    (biTileW inst, biTileH inst)

-- | 'footprintDist' over a bare anchor + tile size, for callers that
--   hold a projection of a building rather than the instance itself
--   (Unit.Transfer's endpoint views). Splitting it out keeps ONE
--   footprint-distance implementation — a second copy could drift from
--   the measure craft.executeAt and the Store menu already use.
--
--   A single tile IS a 1x1 footprint, so this is 'footprintDistBetween'
--   with the caller's tile as the second rectangle rather than a
--   separate measure.
footprintDistAt ∷ (Int, Int) → (Int, Int) → (Int, Int) → Int
footprintDistAt anchor size tile = footprintDistBetween anchor size tile (1, 1)

-- | Minimum Chebyshev distance between two footprint RECTANGLES:
--   0 = overlapping, 1 = adjacent (incl. diagonals). Generalizes
--   'footprintDistAt' for #1085's building↔building transfers, where
--   neither endpoint is a single tile and both may be multi-tile of
--   unequal size.
footprintDistBetween ∷ (Int, Int) → (Int, Int) → (Int, Int) → (Int, Int) → Int
footprintDistBetween (ax, ay) (aw, ah) (bx, by) (bw, bh) =
    let axHi = ax + aw - 1
        ayHi = ay + ah - 1
        bxHi = bx + bw - 1
        byHi = by + bh - 1
        dx = maximum [ax - bxHi, 0, bx - axHi]
        dy = maximum [ay - byHi, 0, by - ayHi]
    in max dx dy

-- | Pure derivation of activity (#2080). Two modes, and they now
--   report DIFFERENT activities rather than sharing one:
--
--   * bdBuildWork > 0 (worker-driven): 'Constructing' while
--     biBuildProgress < bdBuildWork.
--
--   * bdBuildWork == 0 (time-based): 'Appearing' while elapsed
--     game-time is inside the 'RoleAppearance' animation's duration. A
--     definition with no appearance animation is 'Built' from the
--     moment it spawns.
currentActivity ∷ Double → BuildingInstance → BuildingDef → BuildingActivity
currentActivity now inst def
    | bdBuildWork def > 0 =
        if biBuildProgress inst < bdBuildWork def then Constructing else Built
    | otherwise =
        let elapsed = now - biSpawnedAt inst
            appearDuration = case Map.lookup RoleAppearance (bdRoleAnims def) of
                Nothing       → 0
                Just animName → case HM.lookup animName (bdAnimations def) of
                    Nothing  → 0
                    Just a   →
                        let maxN = buildingAnimMaxFrames a
                            fps  = banFps a
                        in if fps > 0 ∧ maxN > 0
                           then fromIntegral maxN / realToFrac fps ∷ Double
                           else 0
        in if elapsed < appearDuration then Appearing else Built
