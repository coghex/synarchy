{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module World.River.Types
    ( RiverMask
    , RiverTileInfo(..)
    , emptyRiverMask
    , riverMaskAt
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.DeepSeq (NFData)
import qualified Data.Vector as V

-- | Per-tile river ownership info.
data RiverTileInfo = RiverTileInfo
    { rtiRouteIndex  ∷ !Int
    , rtiWaterSurf   ∷ !Int
    , rtiRefElev     ∷ !Int
    , rtiPerpDist    ∷ !Float
    , rtiChannelHalf ∷ !Float
    } deriving (Show, Eq, Generic, Serialize, NFData)

-- | Per-chunk river mask. One entry per tile.
type RiverMask = V.Vector (Maybe RiverTileInfo)

-- | chunkSize is 16 — hardcoded here to avoid circular import.
maskChunkSize ∷ Int
maskChunkSize = 16

emptyRiverMask ∷ RiverMask
emptyRiverMask = V.replicate (maskChunkSize * maskChunkSize) Nothing

riverMaskAt ∷ RiverMask → Int → Int → Maybe RiverTileInfo
riverMaskAt mask lx ly
    | lx < 0 ∨ lx ≥ maskChunkSize ∨ ly < 0 ∨ ly ≥ maskChunkSize = Nothing
    | otherwise = mask V.! (ly * maskChunkSize + lx)
