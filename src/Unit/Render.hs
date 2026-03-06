{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Render
    ( renderUnitQuads
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import World.Grid (tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , worldLayer, GridConfig(..), defaultGridConfig)
import Unit.Types

-- | Tile pixel dimensions — must match GridConfig (same as FloraQuads).
baseTileW ∷ Float
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)   -- 96

baseTileH ∷ Float
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)  -- 64

-- | Unit sort nudge — same as flora (0.0003) so units and trees
--   interleave correctly by position in the painter's algorithm.
unitSortNudge ∷ Float
unitSortNudge = 0.0003

-----------------------------------------------------------
-- Top-Level: generate quads for all live unit instances
-----------------------------------------------------------

renderUnitQuads ∷ EngineEnv → CameraFacing → Int → Float → IO (V.Vector SortableQuad)
renderUnitQuads env facing zSlice tileAlpha = do
    um ← readIORef (unitManagerRef env)
    let instances = umInstances um
    if HM.null instances
        then return V.empty
        else do
            texSizes ← readIORef (textureSizeRef env)
            mBts ← readIORef (textureSystemRef env)
            defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)
            case mBts of
                Nothing → return V.empty
                Just bts → do
                    let lookupSlot h = getTextureSlotIndex h bts
                        defFmSlot = fromIntegral defFmSlotWord
                        quads = V.fromList
                            $ HM.foldl' (\acc inst →
                                case unitToQuad lookupSlot defFmSlot facing
                                                zSlice tileAlpha inst texSizes of
                                    Just sq → sq : acc
                                    Nothing → acc
                              ) [] instances

                    return quads

-----------------------------------------------------------
-- Convert a single UnitInstance to a SortableQuad
-----------------------------------------------------------

unitToQuad
    ∷ (TextureHandle → Word32)    -- ^ bindless slot lookup
    → Float                     -- ^ default face map slot
    → CameraFacing
    → Int                       -- ^ current z-slice
    → Float                     -- ^ tileAlpha
    → UnitInstance
    → HM.HashMap TextureHandle (Int, Int)  -- ^ texture sizes
    → Maybe SortableQuad
unitToQuad lookupSlot defFmSlot facing zSlice tileAlpha inst texSizes =
    let gridZ = uiGridZ inst
        relativeZ = gridZ - zSlice
    in if gridZ > zSlice ∨ gridZ < (zSlice - 25)
       then Nothing
       else
        let texHandle = uiTexture inst

            -- Look up actual texture dimensions, default to tile size
            (texW, texH) = case HM.lookup texHandle texSizes of
                Just (w, h) → (fromIntegral w, fromIntegral h)
                Nothing     → (baseTileW, baseTileH)

            -- Scale relative to the base tile pixel size
            scaleX = texW / baseTileW
            scaleY = texH / baseTileH

            quadW = tileWidth  * scaleX
            quadH = tileHeight * scaleY

            -- Fractional grid → isometric screen position.
            -- Replicates gridToScreen math with floats for sub-tile precision.
            gxF = uiGridX inst
            gyF = uiGridY inst

            (faF, fbF) = applyFacingF facing gxF gyF

            rawX = (faF - fbF) * tileHalfWidth - tileHalfWidth
            rawY = (faF + fbF) * tileHalfDiamondHeight

            heightOffset = fromIntegral relativeZ * tileSideHeight

            -- Anchor at ground contact: base_width gives the contact
            -- circle diameter, same logic as FloraQuads.
            baseRadius = uiBaseWidth inst * 0.5 / baseTileH * tileHeight

            -- Center horizontally on the position
            drawX = rawX + (tileWidth - quadW) * 0.5

            -- Anchor: feet at the tile diamond center Y
            drawY = rawY - heightOffset
                  + tileHalfDiamondHeight - quadH + baseRadius

            -- The sprite's feet are at feetRow, but the quad extends
            -- upward visually into rows ahead of it. Tiles at those
            -- rows would sort after the unit and paint over it.
            -- Shift the sort key forward by half the sprite's height
            -- in isometric row-space so the unit sorts after all
            -- tiles its body overlaps.
            spriteRowSpan = quadH / tileHalfDiamondHeight * 0.5
            sortKey = (faF + fbF)
                    + spriteRowSpan
                    + fromIntegral relativeZ * 0.001
                    + 0.0006

            actualSlot = lookupSlot texHandle

            tint = Vec4 1.0 1.0 1.0 tileAlpha

            v0 = Vertex (Vec2 drawX drawY)
                         (Vec2 0 0) tint (fromIntegral actualSlot) defFmSlot
            v1 = Vertex (Vec2 (drawX + quadW) drawY)
                         (Vec2 1 0) tint (fromIntegral actualSlot) defFmSlot
            v2 = Vertex (Vec2 (drawX + quadW) (drawY + quadH))
                         (Vec2 1 1) tint (fromIntegral actualSlot) defFmSlot
            v3 = Vertex (Vec2 drawX (drawY + quadH))
                         (Vec2 0 1) tint (fromIntegral actualSlot) defFmSlot

        in Just SortableQuad
            { sqSortKey = sortKey
            , sqV0      = v0
            , sqV1      = v1
            , sqV2      = v2
            , sqV3      = v3
            , sqTexture = texHandle
            , sqLayer   = worldLayer
            }

-----------------------------------------------------------
-- Float-precision facing transform (mirrors World.Grid.applyFacing)
-----------------------------------------------------------

applyFacingF ∷ CameraFacing → Float → Float → (Float, Float)
applyFacingF FaceSouth gx gy = ( gx,  gy)
applyFacingF FaceWest  gx gy = ( gy, -gx)
applyFacingF FaceNorth gx gy = (-gx, -gy)
applyFacingF FaceEast  gx gy = (-gy,  gx)
