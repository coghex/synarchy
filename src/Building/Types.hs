{-# LANGUAGE Strict, UnicodeSyntax #-}
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
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..))
import Unit.Direction (Direction(..))
import Unit.Types (Animation(..))

-- | Buildings reuse the unit Animation type. They only need one
--   "direction" key — DirS by convention — since they don't rotate.

newtype BuildingId = BuildingId { unBuildingId ∷ Word32 }
    deriving (Show, Eq, Ord, Generic, Hashable)

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
    , bdTexture     ∷ !TextureHandle      -- ^ static fallback
    , bdTileW       ∷ !Int                -- ^ footprint width in tiles
    , bdTileH       ∷ !Int                -- ^ footprint height in tiles
    , bdPlacement   ∷ !Text               -- ^ "flat_ground" / future kinds
    , bdIsStarting  ∷ !Bool
    , bdRace        ∷ !Text
    , bdAnimations  ∷ !(HM.HashMap Text Animation)
    , bdStateAnims  ∷ !(HM.HashMap Text Text)
      -- ^ "appearing" / "built" → animation name in bdAnimations.
    } deriving (Show, Eq)

-- | A placed building. anchor = bottom-left corner of the footprint.
data BuildingInstance = BuildingInstance
    { biDefName    ∷ !Text
    , biTexture    ∷ !TextureHandle      -- ^ copied from def
    , biAnchorX    ∷ !Int                -- ^ tile coords (footprint origin)
    , biAnchorY    ∷ !Int
    , biGridZ      ∷ !Int                -- ^ vertical layer (terrain Z at place time)
    , biSpawnedAt  ∷ !Double             -- ^ POSIX seconds when placed
    , biTileW      ∷ !Int                -- ^ cached from def for cheap iteration
    , biTileH      ∷ !Int
    } deriving (Show, Eq)

-- | Singleton ghost preview: one optional def + tile + valid flag.
--   Drawn by the render pass when present, cleared by the build tool
--   when leaving placement mode.
data BuildingGhost = BuildingGhost
    { bgDefName ∷ !Text
    , bgGridX   ∷ !Int
    , bgGridY   ∷ !Int
    , bgValid   ∷ !Bool
    } deriving (Show, Eq)

data BuildingManager = BuildingManager
    { bmDefs      ∷ !(HM.HashMap Text BuildingDef)
    , bmInstances ∷ !(HM.HashMap BuildingId BuildingInstance)
    , bmNextId    ∷ !Word32
    } deriving (Show, Eq)

emptyBuildingManager ∷ BuildingManager
emptyBuildingManager = BuildingManager
    { bmDefs      = HM.empty
    , bmInstances = HM.empty
    , bmNextId    = 1
    }

nextBuildingId ∷ BuildingManager → (BuildingId, BuildingManager)
nextBuildingId bm =
    let bid = BuildingId (bmNextId bm)
    in (bid, bm { bmNextId = bmNextId bm + 1 })

-- | Pure derivation of activity from elapsed time + the def's appear
--   animation duration. If no appearing anim is defined, the building
--   is Built from the moment it spawns.
currentActivity ∷ Double → BuildingInstance → BuildingDef → BuildingActivity
currentActivity now inst def =
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
