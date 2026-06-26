{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module Building.Types
    ( BuildingId(..)
    , BuildingActivity(..)
    , BuildingDef(..)
    , BuildingInstance(..)
    , BuildingGhost(..)
    , BuildingManager(..)
    , emptyBuildingManager
    , nextBuildingId
    , currentActivity
    , materialsSatisfied
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
import World.Page.Types (WorldPageId(..))
import Unit.Direction (Direction(..))
import Unit.Types (Animation(..))
import Item.Types (ItemInstance)

-- | Buildings reuse the unit Animation type. They only need one
--   "direction" key — DirS by convention — since they don't rotate.

newtype BuildingId = BuildingId { unBuildingId ∷ Word32 }
    deriving (Show, Eq, Ord, Generic, Hashable, Serialize)

-- | Derived from elapsed time, NOT stored on the instance:
--   elapsed < appear-anim duration → Appearing.
--   otherwise                       → Built.
data BuildingActivity = Appearing | Built
    deriving (Show, Eq)

-- | Definition loaded from YAML, immutable after load. Mirrors UnitDef
--   in shape but minus the directional-sprite + state-machine concerns
--   that buildings don't have.
data BuildingDef = BuildingDef
    { bdName        ∷ !Text
    , bdDisplayName ∷ !Text               -- ^ shown in build menu + tooltips
    , bdCategory    ∷ !Text               -- ^ build-menu tab key ("Starting", "Cargo", ...)
    , bdDescription ∷ !Text               -- ^ build-menu tooltip hint body
    , bdTexture     ∷ !TextureHandle      -- ^ static fallback
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
      --   Appearing→Built and the appearing-anim frame are derived
      --   from biBuildProgress / bdBuildWork instead of elapsed time.
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
    , bdAnimations  ∷ !(HM.HashMap Text Animation)
    , bdStateAnims  ∷ !(HM.HashMap Text Text)
      -- ^ "appearing" / "built" → animation name in bdAnimations.
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
      --   bdBuildWork → Appearing flips to Built. Driven by Lua's
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

-- | Pure derivation of activity. Two modes:
--
--   * bdBuildWork > 0 (worker-driven): Appearing while biBuildProgress
--     < bdBuildWork. The portal and other instant-build defs leave
--     bdBuildWork at 0 and use the time-based fallback.
--
--   * bdBuildWork == 0 (time-based, legacy): Appearing while elapsed
--     game-time < appearing-anim duration. If no appearing anim is
--     defined, the building is Built from the moment it spawns.
-- | True iff every entry in bdMaterials has at least the required
--   count delivered. Empty bdMaterials trivially satisfies (the
--   portal and other legacy defs).
materialsSatisfied ∷ BuildingInstance → BuildingDef → Bool
materialsSatisfied inst def =
    all (\(t, n) → length (HM.lookupDefault [] t (biMaterialsDelivered inst)) >= n)
        (HM.toList (bdMaterials def))

currentActivity ∷ Double → BuildingInstance → BuildingDef → BuildingActivity
currentActivity now inst def
    | bdBuildWork def > 0 =
        if biBuildProgress inst < bdBuildWork def then Appearing else Built
    | otherwise =
        let elapsed = now - biSpawnedAt inst
            appearDuration = case HM.lookup "appearing" (bdStateAnims def) of
                Nothing       → 0
                Just animName → case HM.lookup animName (bdAnimations def) of
                    Nothing  → 0
                    Just a   →
                        let counts = V.length <$> Map.elems (aFrames a)
                            maxN   = if null counts then 0 else maximum counts
                            fps    = aFps a
                        in if fps > 0 ∧ maxN > 0
                           then fromIntegral maxN / realToFrac fps ∷ Double
                           else 0
        in if elapsed < appearDuration then Appearing else Built
