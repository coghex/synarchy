{-# LANGUAGE Strict, UnicodeSyntax #-}
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

-----------------------------------------------------------
-- Chunk-Level Culling
-----------------------------------------------------------

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
    VU.any (/= minBound) (lcSurfaceMap lc)
