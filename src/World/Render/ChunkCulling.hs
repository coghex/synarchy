{-# LANGUAGE Strict #-}
module World.Render.ChunkCulling
    ( bestWrapOffset
    , isChunkVisibleWrapped
    , isChunkRelevantForSlice
    ) where

import UPrelude
import qualified Data.Vector.Unboxed as VU
import Engine.Graphics.Camera (CameraFacing(..))
import World.Generate (chunkWorldBounds)
import World.Grid (gridToScreen, tileWidth, tileHeight, worldScreenWidth)
import World.Types
import World.Render.ViewBounds (ViewBounds(..))

-- * Chunk-Level Culling

-- | Pick the u-alias of a chunk nearest the camera, as a screen-X shift.
--
--   KNOWN INCOMPLETE at east/west facings (#1176; surfaced by #1135's
--   audit, recorded here at the source rather than in each of the six
--   consumers). The offset is X-only, which is EXACT at FaceSouth /
--   FaceNorth: there 'gridToWorld' gives sx = ±u·tileHalfWidth and
--   sy = ±v·tileHalfDiamondHeight, and a u-wrap shifts u by a whole
--   world while preserving v — so the displacement is pure X and one of
--   the candidates below cancels it exactly.
--
--   At FaceWest / FaceEast 'applyFacing' swaps the axes
--   (sx = ±v·tileHalfWidth, sy = ∓u·tileHalfDiamondHeight), so the SAME
--   wrap displaces screen Y instead and no X candidate can correct it:
--   for a 64-chunk world the alias pair (272,-240)/(-240,272) differs by
--   ΔX=±76.8, ΔY=0 at south/north but ΔX=0, ΔY=∓38.4 at west/east.
--   Content resolved across the seam is then culled or placed a
--   half-world off in Y at those two facings — invisible AND unhittable
--   where a consumer gates its hit test on this too. #423 met the same
--   degeneracy and skipped it locally in 'structureFrontWallClear';
--   #1176 is the general facing-aware fix.
bestWrapOffset ∷ Int → Float → Float → Float
bestWrapOffset worldSize camX chunkScreenX =
    let wsw = worldScreenWidth worldSize
        candidates = [0, wsw, -wsw]
        dist offset = abs (chunkScreenX + offset - camX)
    in minimumBy (\ac bc → compare (dist ac) (dist bc)) candidates
  where
    minimumBy f (hd:tl) = foldl' (\best c → if f c best ≡ LT then c else best) hd tl
    minimumBy _ []       = 0

isChunkVisibleWrapped ∷ CameraFacing → Int → ViewBounds → Float
  → ChunkCoord → Maybe Float
isChunkVisibleWrapped facing worldSize vb camX coord =
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
        offset = bestWrapOffset worldSize camX chunkCenterX

        chunkLeft   = sxMin + offset
        chunkRight  = sxMax + tileWidth + offset
        chunkTop    = syMin
        chunkBottom = syMax + tileHeight

        visible = not (chunkRight  < vbLeft vb
                    ∨ chunkLeft   > vbRight vb
                    ∨ chunkBottom < vbTop vb
                    ∨ chunkTop    > vbBottom vb)
    in if visible then Just offset else Nothing

isChunkRelevantForSlice ∷ Int → LoadedChunk → Bool
isChunkRelevantForSlice _zSlice lc =
    VU.any (≢ minBound) (lcSurfaceMap lc)
