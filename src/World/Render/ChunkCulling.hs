{-# LANGUAGE Strict #-}
module World.Render.ChunkCulling
    ( isChunkVisibleWrapped
    , isChunkRelevantForSlice
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import Engine.Graphics.Camera (CameraFacing(..))
import World.Generate (chunkWorldBounds)
import World.Grid (gridToScreen, tileWidth, tileHeight, worldWrapPeriod)
import World.Types
import World.Render.ViewBounds (ViewBounds(..))

-- * Chunk-Level Culling

-- | Pick the u-alias of a chunk nearest the camera, as a screen-space
--   @(x, y)@ shift.
--
--   FACING-AWARE since #1176. A u-wrap shifts u by a whole world and
--   preserves v, and 'World.Grid.gridToWorld' swaps which screen axis
--   carries u with the facing — so the displacement to cancel is pure
--   screen X at FaceSouth / FaceNorth and pure screen Y at FaceWest /
--   FaceEast. 'worldWrapPeriod' is the single statement of which; the
--   candidates are offered along that axis and the other component
--   comes back exactly 0 (v does not move, so there is nothing to
--   correct there).
--
--   This USED to return an X-only shift, which is exact at south/north
--   but cannot touch east/west at all: for a 64-chunk world the alias
--   pair (272,-240)/(-240,272) differs by ΔX=±76.8, ΔY=0 at
--   south/north but ΔX=0, ΔY=∓38.4 at west/east. Content resolved
--   across the seam was then culled by the bounds test or placed a
--   half-world off in Y at those two facings — invisible AND
--   unhittable wherever a consumer gates its hit test on this too.
bestWrapOffset ∷ CameraFacing → Int → Float → Float → Float → Float
  → (Float, Float)
bestWrapOffset facing worldSize camX camY chunkScreenX chunkScreenY =
    let (periodX, periodY) = worldWrapPeriod facing worldSize
    in ( nearestAlias periodX camX chunkScreenX
       , nearestAlias periodY camY chunkScreenY )
  where
    -- Ties keep the earliest candidate — 0 over ±period, +period over
    -- -period. That is the pre-#1176 'minimumBy' fold's exact order,
    -- preserved so north/south offsets stay bit-identical (req 4). A
    -- zero period collapses the three candidates onto 0, which is why
    -- the inactive axis needs no special case.
    nearestAlias period cam center =
        let dist off = abs (center + off - cam)
        in foldl' (\best c → if dist c < dist best then c else best)
                  0 [period, -period]

-- | Is a chunk visible through its nearest u-alias, and by what
--   screen-space shift? Visibility is tested against bounds translated
--   by the SAME pair that is returned for placement (req 2), so a chunk
--   can never be judged visible at one position and drawn at another.
isChunkVisibleWrapped ∷ CameraFacing → Int → ViewBounds → Float → Float
  → ChunkCoord → Maybe (Float, Float)
isChunkVisibleWrapped facing worldSize vb camX camY coord =
    let ((minGX, minGY), (maxGX, maxGY)) = chunkWorldBounds coord
        corners = [ gridToScreen facing gx gy
                  | gx ← [minGX, maxGX]
                  , gy ← [minGY, maxGY]
                  ]
        sxs = map fst corners
        sys = map snd corners
        sxMin = minimum sxs
        sxMax = maximum sxs
        syMin = minimum sys
        syMax = maximum sys

        chunkCenterX = (sxMin + sxMax + tileWidth) / 2.0
        chunkCenterY = (syMin + syMax + tileHeight) / 2.0
        (offX, offY) = bestWrapOffset facing worldSize camX camY
                                      chunkCenterX chunkCenterY

        chunkLeft   = sxMin + offX
        chunkRight  = sxMax + tileWidth + offX
        chunkTop    = syMin + offY
        chunkBottom = syMax + tileHeight + offY

        visible = not (chunkRight  < vbLeft vb
                    ∨ chunkLeft   > vbRight vb
                    ∨ chunkBottom < vbTop vb
                    ∨ chunkTop    > vbBottom vb)
    in if visible then Just (offX, offY) else Nothing

isChunkRelevantForSlice ∷ Int → LoadedChunk → Bool
isChunkRelevantForSlice _zSlice lc =
    VU.any (≢ minBound) (lcSurfaceMap lc)
