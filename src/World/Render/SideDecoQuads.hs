{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.SideDecoQuads
    ( waterSideFaceQuads
    ) where

import UPrelude
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vec2(..), Vec4(..), mkVertexWorld
                                           , packWorldUV)
import qualified Data.HashMap.Strict as HM
import World.Chunk.Types (ChunkCoord(..), chunkSize, columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Material (matOcean, matLava, unMaterialId)
import World.Generate (chunkToGlobal)
import World.Grid (gridToScreen, tileWidth, tileHeight, tileSideHeight
                  , worldLayer, applyFacing)
import World.Render.Textures.Types (WorldTextures(..))
import World.Render.ViewBounds (ViewBounds, isTileVisible)

-- | Generate quads for water side faces where water drops between
--   adjacent tiles. Draws side faces for:
--   1. Water-to-water drops (water neighbor at lower surface)
--   2. Water-to-dry drops (dry neighbor with terrain below water)
--   In case 2, terrain cliff faces cover up to terrain level;
--   water side faces cover terrain level to water surface.
waterSideFaceQuads ∷ (TextureHandle → Int)
                   → (TextureHandle → Float)
                   → WorldTextures
                   → CameraFacing
                   → ChunkCoord
                   → V.Vector (Maybe FluidCell)  -- ^ this chunk's fluid map
                   → VU.Vector Int               -- ^ this chunk's terrain surface map
                   → (ChunkCoord → Maybe (V.Vector (Maybe FluidCell)))
                                                 -- ^ neighbour-chunk fluid lookup
                   → (ChunkCoord → Maybe (VU.Vector Int))
                                                 -- ^ neighbour-chunk terrain lookup
                   → Int → Int                   -- ^ zSlice, effectiveDepth
                   → Float → Float               -- ^ tileAlpha, xOffset
                   → ViewBounds
                   → [SortableQuad]
waterSideFaceQuads lookupSlot lookupFmSlot textures facing coord
                   fluidMap terrainSurfMap fluidLookup terrLookup
                   zSlice effDepth tileAlpha xOffset vb =
    [ sq
    | lx ← [0 .. chunkSize - 1]
    , ly ← [0 .. chunkSize - 1]
    , let idx = columnIndex lx ly
    , Just fc ← [fluidMap V.! idx]
    , fcType fc ≢ Ocean
    , let mySurf = fcSurface fc
    -- Check each camera-visible cardinal neighbor. A neighbor can sit in
    -- the adjacent chunk (a waterfall/cliff right at a seam): resolve it
    -- through the cross-chunk lookup exactly as waterSlopeAt does, so side
    -- faces don't vanish at chunk boundaries.
    , (nx, ny, isLeftFace) ← neighborDirs facing lx ly
    , Just (nFluid, nTerrZ) ← [neighborCell nx ny]
    , let -- Bottom of the side-face stack depends on neighbor type:
          --   Water neighbor: draw from neighbor water surface
          --   Dry neighbor: draw from neighbor terrain surface
          -- A 1-z gap is handled by the sloped water surface tile
          -- (freshwaterTileToQuad + waterSlopeAt), so we only draw
          -- side faces for gaps ≥ 2 (real waterfalls).
          (bottomZ, shouldDraw) = case nFluid of
              Just nfc | fcSurface nfc < mySurf - 1 → (fcSurface nfc, True)
              Just _                                → (mySurf, False)
              Nothing | nTerrZ < mySurf - 1         → (nTerrZ, True)
              Nothing                               → (mySurf, False)
    , shouldDraw
    -- One quad per z-level of gap
    , z ← [bottomZ .. mySurf - 1]
    , z ≥ zSlice - effDepth
    , z ≤ zSlice
    , let (gx, gy) = chunkToGlobal coord lx ly
    , sq ← maybeToList (waterSideQuad lookupSlot lookupFmSlot textures facing
                            (fcType fc) gx gy z isLeftFace
                            zSlice effDepth tileAlpha xOffset vb)
    ]
  where
    -- Resolve a cardinal neighbor's (fluid cell, terrain surface z),
    -- following a step out of this chunk into the adjacent one. Returns
    -- Nothing only when that neighbor chunk isn't loaded — then the drop
    -- is unknown, so we draw no side face (the same conservative default
    -- waterSlopeAt uses at an unloaded seam).
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

-- | Create a single fluid side-face quad at a given z-level.
waterSideQuad ∷ (TextureHandle → Int)
              → (TextureHandle → Float)
              → WorldTextures
              → CameraFacing
              → FluidType       -- ^ owning fluid (texture choice)
              → Int → Int       -- ^ global x, y
              → Int             -- ^ z-level of this side face
              → Bool            -- ^ True = left face, False = right face
              → Int → Int       -- ^ zSlice, effectiveDepth
              → Float → Float   -- ^ tileAlpha, xOffset
              → ViewBounds
              → Maybe SortableQuad
waterSideQuad lookupSlot lookupFmSlot textures facing ftype gx gy z isLeft
              zSlice _effDepth tileAlpha xOffset vb =
    let (rawX, rawY) = gridToScreen facing gx gy
        (fa, fb) = applyFacing facing gx gy
        relativeZ = z - zSlice
        heightOffset = fromIntegral relativeZ * tileSideHeight

        drawX = rawX + xOffset
        drawY = rawY - heightOffset

        -- Check side face map is loaded before rendering
        fmHandle0 = if isLeft
                    then wtSideFaceMapLeft textures
                    else wtSideFaceMapRight textures
        fmSlot0 = lookupFmSlot fmHandle0

    in if not (isTileVisible vb drawX drawY) ∨ fmSlot0 ≡ 0.0
       then Nothing
       else let
            sortKey = fromIntegral (fa + fb)
                    + fromIntegral relativeZ * 0.001
                    + 0.00005

            -- Texture by fluid type: lava side faces are lava, every
            -- water class shares the ocean texture. (Pre-2026-06-06
            -- lava sides rendered as water — bright blue cliffs under
            -- floating pool rims.)
            sideMat = case ftype of
                Lava → matLava
                _    → matOcean
            texHandle = case HM.lookup (unMaterialId sideMat)
                                       (wtTileTextures textures) of
                            Nothing → wtNoTexture textures
                            Just h  → h
            actualSlot = lookupSlot texHandle

            -- Side face map slot (already checked non-zero above)
            fmSlot = fmSlot0

            -- No tinting — color comes from texture
            tint = Vec4 1.0 1.0 1.0 tileAlpha
            wuv = packWorldUV gx gy

            v0 = mkVertexWorld wuv (Vec2 drawX drawY)
                         (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
            v1 = mkVertexWorld wuv (Vec2 (drawX + tileWidth) drawY)
                         (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
            v2 = mkVertexWorld wuv (Vec2 (drawX + tileWidth) (drawY + tileHeight))
                         (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            v3 = mkVertexWorld wuv (Vec2 drawX (drawY + tileHeight))
                         (Vec2 0 1) tint (fromIntegral actualSlot) fmSlot

        in Just SortableQuad
            { sqSortKey = sortKey
            , sqV0      = v0
            , sqV1      = v1
            , sqV2      = v2
            , sqV3      = v3
            , sqTexture = texHandle
            , sqLayer   = worldLayer
            }
