{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.Magma.Types
    ( MagmaSource(..)
    , LavaShape(..)
    , VolcanoCtx(..)
    , MagmaOverlay(..)
    , emptyMagmaOverlay
    , emptyVolcanoCtx
    , msSurfaceRadius
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import World.Base (GeoCoord(..), GeoFeatureId(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Geology.Timeline.Types
    ( VolcanicFeature(..)
    , FeatureActivity(..)
    , EventBBox(..)
    , ShieldParams(..)
    , CinderConeParams(..)
    , LavaDomeParams(..)
    , CalderaParams(..)
    , FissureParams(..)
    , LavaTubeParams(..)
    , SuperVolcanoParams(..)
    , HydrothermalParams(..)
    )
-- Re-exported so existing call sites that import MagmaOverlay from
-- 'World.Magma.Types' keep working; the type itself now lives in
-- 'World.Magma.Overlay' so 'World.Chunk.Types' can reference it
-- without a module-graph cycle through 'World.Geology.Timeline.Types'.
import World.Magma.Overlay (MagmaOverlay(..), emptyMagmaOverlay)

-- | Runtime cache for one active/dormant volcanic feature. Built at
--   world-init from gtFeatures + worldSeed; not persisted.
data MagmaSource = MagmaSource
    { msFeatureId    ∷ !GeoFeatureId
    , msType         ∷ !VolcanicFeature
    , msActivity     ∷ !FeatureActivity
    , msCenter       ∷ !GeoCoord
    , msShapes       ∷ ![LavaShape]
    , msBBox         ∷ !EventBBox
    , msHotspotBoost ∷ !Float
    } deriving (Show, Eq, Generic, NFData)

-- | The six geometric primitives. Chutes (4) + chambers (2). Union
--   of these defines a source's molten volume above the mantle.
data LavaShape
    = Cylindrical
        { lsX        ∷ !Int
        , lsY        ∷ !Int
        , lsZBottom  ∷ !Int
        , lsZTop     ∷ !Int
        , lsRadius   ∷ !Float
        }
    | Conical
        { lsX        ∷ !Int
        , lsY        ∷ !Int
        , lsZBottom  ∷ !Int
        , lsZTop     ∷ !Int
        , lsRBottom  ∷ !Float
        , lsRTop     ∷ !Float
        }
    | Perturbed
        { lsX          ∷ !Int
        , lsY          ∷ !Int
        , lsZBottom    ∷ !Int
        , lsZTop       ∷ !Int
        , lsBaseRadius ∷ !Float
        , lsAmplitude  ∷ !Float
        , lsFreq       ∷ !Float
        , lsPhase      ∷ !Float
        }
    | Slot
        { lsStartX   ∷ !Int
        , lsStartY   ∷ !Int
        , lsEndX     ∷ !Int
        , lsEndY     ∷ !Int
        , lsZBottom  ∷ !Int
        , lsZTop     ∷ !Int
        , lsWidth    ∷ !Float
        }
    | EllipsoidChamber
        { lsX  ∷ !Int
        , lsY  ∷ !Int
        , lsZ  ∷ !Int
        , lsRX ∷ !Float
        , lsRY ∷ !Float
        , lsRZ ∷ !Float
        }
    | IrregularChamber
        { lsX            ∷ !Int
        , lsY            ∷ !Int
        , lsZ            ∷ !Int
        , lsRX           ∷ !Float
        , lsRY           ∷ !Float
        , lsRZ           ∷ !Float
        , lsPerturbAmp   ∷ !Float
          -- ^ Perturbation amplitude in NORMALISED units (a fraction
          --   of the unit radius); 0.2 ≈ ±20 % wobble at the chamber
          --   edge. Was previously absolute tiles relative to a single
          --   3-D radius — saucer-shaped chambers need normalised
          --   perturbation so the relative wobble looks the same on
          --   the wide top + bottom and the narrow rim.
        , lsPerturbFreq  ∷ !Float
        , lsSeed         ∷ !Word64
        }
    deriving (Show, Eq, Generic, NFData)

-- | Runtime context built once after the timeline finishes. Lives
--   inside 'WorldGenParams' as the transient @wgpVolcanoCtx@ field;
--   rebuilt deterministically from @seed + worldSize + plates + features@
--   on save-load.
data VolcanoCtx = VolcanoCtx
    { vcSources    ∷ !(V.Vector MagmaSource)
      -- ^ Vector (not list) so 'sourceContains' indexing is O(1).
    , vcIndex      ∷ !(HM.HashMap ChunkCoord [Int])
      -- ^ Per-chunk candidate-source list. Indices into 'vcSources'.
    , vcHotspotIndex ∷ !(HM.HashMap ChunkCoord [Int])
      -- ^ Wider per-chunk index for 'sumHotspots'. Each source's
      --   bbox is padded by @3σ@ (where @σ = 2× surface radius@) so
      --   any chunk where a Gaussian contribution would be
      --   non-negligible is in the candidate list.
    , vcSeed       ∷ !Word64
    , vcWorldSize  ∷ !Int
    } deriving (Show, Eq, Generic, NFData)

-- | An empty context, suitable for 'defaultWorldGenParams' and any
--   pre-init code path. 'lavaAt' through this context returns False
--   for any z at or above the (noise-only) mantle baseline.
emptyVolcanoCtx ∷ VolcanoCtx
emptyVolcanoCtx = VolcanoCtx
    { vcSources      = V.empty
    , vcIndex        = HM.empty
    , vcHotspotIndex = HM.empty
    , vcSeed         = 0
    , vcWorldSize    = 1
    }

-- | Surface radius of a source — used as σ for hotspot Gaussian and
--   as the basis for kits that derive chamber/chute dimensions.
msSurfaceRadius ∷ MagmaSource → Float
msSurfaceRadius src = case msType src of
    ShieldVolcano    p → fromIntegral (shBaseRadius p)
    CinderCone       p → fromIntegral (ccBaseRadius p)
    LavaDome         p → fromIntegral (ldBaseRadius p)
    Caldera          p → fromIntegral (caOuterRadius p)
    SuperVolcano     p → fromIntegral (svCalderaRadius p)
    HydrothermalVent p → fromIntegral (htRadius p)
    FissureVolcano   p → fromIntegral (fpWidth p)
    LavaTube         p → fromIntegral (ltWidth p)
