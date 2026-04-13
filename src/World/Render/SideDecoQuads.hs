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
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import qualified Data.HashMap.Strict as HM
import World.Chunk.Types (ChunkCoord(..), chunkSize, columnIndex)
import World.Fluid.Types (FluidCell(..), FluidType(..))
import World.Material (matOcean, unMaterialId)
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
                   → V.Vector (Maybe FluidCell)  -- ^ fluid map
                   → VU.Vector Int               -- ^ terrain surface map
                   → Int → Int                   -- ^ zSlice, effectiveDepth
                   → Float → Float               -- ^ tileAlpha, xOffset
                   → ViewBounds
                   → [SortableQuad]
waterSideFaceQuads lookupSlot lookupFmSlot textures facing coord
                   fluidMap terrainSurfMap zSlice effDepth tileAlpha xOffset vb =
    [ sq
    | lx ← [0 .. chunkSize - 1]
    , ly ← [0 .. chunkSize - 1]
    , let idx = columnIndex lx ly
    , Just fc ← [fluidMap V.! idx]
    , fcType fc ≢ Ocean
    , let mySurf = fcSurface fc
    -- Check each camera-visible cardinal neighbor
    , (nx, ny, isLeftFace) ← neighborDirs facing lx ly
    , nx ≥ 0, nx < chunkSize, ny ≥ 0, ny < chunkSize
    , let nIdx = columnIndex nx ny
          nFluid = fluidMap V.! nIdx
          nTerrZ = terrainSurfMap VU.! nIdx
          -- Bottom of the side-face stack depends on neighbor type:
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
                            gx gy z isLeftFace
                            zSlice effDepth tileAlpha xOffset vb)
    ]

-- | Cardinal neighbor directions with face orientation.
--   Returns (nx, ny, isLeftFace).
--   Only the two camera-facing directions are visible.
neighborDirs ∷ CameraFacing → Int → Int → [(Int, Int, Bool)]
neighborDirs facing lx ly = case facing of
    FaceSouth → [(lx, ly + 1, True),  (lx + 1, ly, False)]
    FaceEast  → [(lx + 1, ly, True),  (lx, ly - 1, False)]
    FaceNorth → [(lx, ly - 1, True),  (lx - 1, ly, False)]
    FaceWest  → [(lx - 1, ly, True),  (lx, ly + 1, False)]

-- | Create a single water side-face quad at a given z-level.
waterSideQuad ∷ (TextureHandle → Int)
              → (TextureHandle → Float)
              → WorldTextures
              → CameraFacing
              → Int → Int       -- ^ global x, y
              → Int             -- ^ z-level of this side face
              → Bool            -- ^ True = left face, False = right face
              → Int → Int       -- ^ zSlice, effectiveDepth
              → Float → Float   -- ^ tileAlpha, xOffset
              → ViewBounds
              → Maybe SortableQuad
waterSideQuad lookupSlot lookupFmSlot textures facing gx gy z isLeft
              zSlice effDepth tileAlpha xOffset vb =
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

            -- All water uses the same material texture (matOcean)
            texHandle = case HM.lookup (unMaterialId matOcean)
                                       (wtTileTextures textures) of
                            Nothing → wtNoTexture textures
                            Just h  → h
            actualSlot = lookupSlot texHandle

            -- Side face map slot (already checked non-zero above)
            fmSlot = fmSlot0

            -- No tinting — color comes from texture
            tint = Vec4 1.0 1.0 1.0 tileAlpha

            v0 = Vertex (Vec2 drawX drawY)
                         (Vec2 0 0) tint (fromIntegral actualSlot) fmSlot
            v1 = Vertex (Vec2 (drawX + tileWidth) drawY)
                         (Vec2 1 0) tint (fromIntegral actualSlot) fmSlot
            v2 = Vertex (Vec2 (drawX + tileWidth) (drawY + tileHeight))
                         (Vec2 1 1) tint (fromIntegral actualSlot) fmSlot
            v3 = Vertex (Vec2 drawX (drawY + tileHeight))
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
