{-# LANGUAGE Strict #-}
module World.Render.SideDecoQuads
    ( waterSideFaceQuads
    , fluidSideIntervals
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vec2(..), Vec4(..), mkVertexWorld
                                           , tileWorldUV)
import qualified Data.HashMap.Strict as HM
import World.Chunk.Types (ChunkCoord(..), chunkSize, columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Fluid.Exact (exactSurfaceOfZ, exactSurfaceCeilZ
                        , exactSurfaceFloorZ, exactSurfaceRenderZ)
import World.Material (matOcean, matLava, unMaterialId)
import World.Generate (chunkToGlobal)
import World.Grid (gridToScreen, tileWidth, tileHeight, tileSideHeight
                  , tileHalfDiamondHeight
                  , worldLayer, applyFacing)
import World.Render.QuadContext (QuadContext(..), WorldX(..), WorldY(..)
                                , ZSlice(..), EffectiveDepth(..))
import World.Render.Textures.Types (WorldTextures(..))
import World.Render.ViewBounds (ViewBounds, isTileVisible)

-- | Additional fluid sides below the slab already drawn by the level mask.
--   Each visible edge ends at its own exact neighbour plane (or dry terrain).
--   The mask owns its top fractional slab; painter order lets front neighbours
--   occlude its hidden pixels. These strips fill only the interval BELOW it.
--   Ocean, River, Lake and Lava share geometry, retaining their materials.
waterSideFaceQuads ∷ QuadContext
                   → ChunkCoord
                   → V.Vector (Maybe FluidCell)  -- ^ this chunk's fluid map
                   → VU.Vector Int               -- ^ this chunk's terrain surface map
                   → (ChunkCoord → Maybe (V.Vector (Maybe FluidCell)))
                                                 -- ^ neighbour-chunk fluid lookup
                   → (ChunkCoord → Maybe (VU.Vector Int))
                                                 -- ^ neighbour-chunk terrain lookup
                   → ViewBounds
                   → [SortableQuad]
waterSideFaceQuads ctx coord
                   fluidMap terrainSurfMap fluidLookup terrLookup vb =
    [ sq
    | lx ← [0 .. chunkSize - 1]
    , ly ← [0 .. chunkSize - 1]
    , let idx = columnIndex lx ly
    , Just fc ← [fluidMap V.! idx]
    , (nx, ny, isLeftFace) ← neighborDirs facing lx ly
    , Just (nFluid, nTerrZ) ← [neighborCell nx ny]
    , let bottom = maybe (exactSurfaceOfZ nTerrZ) fcExactSurface nFluid
    , (lo, hi) ← fluidSideIntervals (fcExactSurface fc) bottom
                     zSlice effDepth
    , let (gx, gy) = chunkToGlobal coord lx ly
    , sq ← maybeToList (waterSideQuad ctx (fcType fc)
                            (WorldX gx) (WorldY gy) lo hi isLeftFace vb)
    ]
  where
    facing   = qcFacing ctx
    zSlice   = unZSlice (qcZSlice ctx)
    effDepth = unEffectiveDepth (qcEffectiveDepth ctx)

    -- Resolve a cardinal neighbor's (fluid cell, terrain surface z),
    -- following a step out of this chunk into the adjacent one. Returns
    -- Nothing only when that neighbor chunk isn't loaded — then the drop
    -- is unknown, so we draw no side face (the conservative default at an
    -- unloaded seam).
    --
    -- That "only" holds because the caller resolves the coord built here
    -- through World.Render.ChunkLookup (#1135): the step below is in THIS
    -- chunk's raw frame, and at the cylindrical U seam it names an alias
    -- of the coord the neighbour is stored under. Handing that alias
    -- straight to HM.lookup used to miss on a LOADED neighbour, and the
    -- silent Nothing read here as "not loaded" — so water side faces
    -- vanished along the seam.
    neighborCell ∷ Int → Int → Maybe (Maybe FluidCell, Int)
    neighborCell nx ny
        | nx ≥ 0 ∧ nx < chunkSize ∧ ny ≥ 0 ∧ ny < chunkSize =
            let nIdx = columnIndex nx ny
            in Just (fluidMap V.! nIdx, terrainSurfMap VU.! nIdx)
        | otherwise =
            let ChunkCoord cx cy = coord
                (cx', lx') = if nx < 0              then (cx - 1, nx + chunkSize)
                             else if nx ≥ chunkSize then (cx + 1, nx - chunkSize)
                             else                        (cx, nx)
                (cy', ly') = if ny < 0              then (cy - 1, ny + chunkSize)
                             else if ny ≥ chunkSize then (cy + 1, ny - chunkSize)
                             else                        (cy, ny)
                ncoord = ChunkCoord cx' cy'
                nIdx   = columnIndex lx' ly'
            in case (fluidLookup ncoord, terrLookup ncoord) of
                   (Just nFM, Just nTM) → Just (nFM V.! nIdx, nTM VU.! nIdx)
                   _                    → Nothing

-- | Cardinal neighbor directions with face orientation.
--   Returns (nx, ny, isLeftFace).
--   Only the two camera-facing directions are visible.
neighborDirs ∷ CameraFacing → Int → Int → [(Int, Int, Bool)]
neighborDirs facing lx ly = case facing of
    FaceSouth → [(lx, ly + 1, True),  (lx + 1, ly, False)]
    FaceEast  → [(lx + 1, ly, True),  (lx, ly - 1, False)]
    FaceNorth → [(lx, ly - 1, True),  (lx - 1, ly, False)]
    FaceWest  → [(lx - 1, ly, True),  (lx, ly + 1, False)]

-- | Exact half-open intervals [lo, hi) below the mask's slab, split on
--   whole-z boundaries. Clip the interval itself to the depth/slice window;
--   a top above the slice can still have a visible side below it.
fluidSideIntervals ∷ Int → Int → Int → Int → [(Int, Int)]
fluidSideIntervals surface neighbour zSlice depth =
    [ (max bottom (exactSurfaceOfZ z), min top (exactSurfaceOfZ (z + 1)))
    | z ← [exactSurfaceFloorZ bottom .. exactSurfaceCeilZ top - 1]
    , bottom < top
    ]
  where
    slabBottom = exactSurfaceOfZ (exactSurfaceCeilZ surface - 1)
    bottom = max neighbour (exactSurfaceOfZ (zSlice - depth))
    top = min slabBottom (exactSurfaceOfZ zSlice)

-- | Affinely compress the WHOLE side-mask canvas about its slanted top
--   edge. Cropping the canvas to an ideal parallelogram clips off the last
--   opaque staircase row and opens a one-pixel crack between full strips.
--   The whole canvas preserves those authored boundary pixels. At full
--   height this is exactly the existing rectangular mask projection.
waterSideQuad ∷ QuadContext → FluidType → WorldX → WorldY
              → Int → Int → Bool → ViewBounds → Maybe SortableQuad
waterSideQuad ctx ftype wx wy lo hi isLeft vb =
    let textures = qcTextures ctx
        facing = qcFacing ctx
        gx = unWorldX wx
        gy = unWorldY wy
        zSlice = unZSlice (qcZSlice ctx)
        relativeZ = exactSurfaceRenderZ hi - fromIntegral zSlice
        segmentHeight = exactSurfaceRenderZ (hi - lo) * tileSideHeight
        (rawX, rawY) = gridToScreen facing gx gy
        (wrapX, wrapY) = qcWrapOffset ctx
        drawX = rawX + wrapX
        drawY = rawY + wrapY - relativeZ * tileSideHeight
        (fa, fb) = applyFacing facing gx gy
        fmHandle = if isLeft then wtSideFaceMapLeft textures
                             else wtSideFaceMapRight textures
        fmSlot = qcLookupFmSlot ctx fmHandle
        sideMat = if ftype ≡ Lava then matLava else matOcean
        texHandle = HM.lookupDefault (wtNoTexture textures)
                        (unMaterialId sideMat) (wtTileTextures textures)
        actualSlot = fromIntegral (qcLookupSlot ctx texHandle)
        tint = Vec4 1 1 1 (qcTileAlpha ctx)
        wuv = tileWorldUV gx gy
        -- Compress relative to the side edge, not the canvas origin:
        -- the edge must keep its isometric slope while thickness changes.
        scale = segmentHeight / tileSideHeight
        edgeAt u = if isLeft
                   then tileHalfDiamondHeight * (1 + 2 * u)
                   else tileHalfDiamondHeight * (3 - 2 * u)
        vertex u v =
            let y = scale * v * tileHeight + (1 - scale) * edgeAt u
            in mkVertexWorld wuv (Vec2 (drawX + u * tileWidth) (drawY + y))
                   (Vec2 u v) tint actualSlot fmSlot
    in if fmSlot ≡ 0 ∨ not (isTileVisible vb drawX drawY)
       then Nothing
       else Just SortableQuad
            { sqSortKey = fromIntegral (fa + fb) + relativeZ * 0.001 + 0.00005
            , sqV0 = vertex 0 0
            , sqV1 = vertex 1 0
            , sqV2 = vertex 1 1
            , sqV3 = vertex 0 1
            , sqTexture = texHandle
            , sqLayer = worldLayer
            }
