{-# LANGUAGE Strict, UnicodeSyntax #-}
module Unit.Render
    ( renderUnitQuads
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
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
import Unit.Direction (Direction(..), dirIndex, indexToDir)

baseTileW ∷ Float
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)

baseTileH ∷ Float
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)

unitSortNudge ∷ Float
unitSortNudge = 0.0003

-- | How many direction steps (in our 8-dir clockwise ring) the
--   camera rotation shifts.  Each 90 deg CW rotation = 2 steps.
cameraRotSteps ∷ CameraFacing → Int
cameraRotSteps FaceSouth = 0
cameraRotSteps FaceWest  = 2
cameraRotSteps FaceNorth = 4
cameraRotSteps FaceEast  = 6

-- | Pick the correct directional sprite for a unit given its world-space
--   facing and the current camera rotation. Falls back to the default texture
--   if no directional map or no entry for the computed screen direction.
resolveTexture
    ∷ CameraFacing
    → Direction                          -- ^ unit world facing
    → Map.Map Direction TextureHandle    -- ^ directional sprites
    → TextureHandle                      -- ^ fallback default
    → TextureHandle
resolveTexture camFacing unitFacing dirSprites fallback
    | Map.null dirSprites = fallback
    | otherwise =
        let screenIdx = (dirIndex unitFacing - cameraRotSteps camFacing) `mod` 8
            screenDir = indexToDir screenIdx
        in case Map.lookup screenDir dirSprites of
            Just h  → h
            Nothing → fallback

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

unitToQuad
    ∷ (TextureHandle → Word32)
    → Float
    → CameraFacing
    → Int
    → Float
    → UnitInstance
    → HM.HashMap TextureHandle (Int, Int)
    → Maybe SortableQuad
unitToQuad lookupSlot defFmSlot facing zSlice tileAlpha inst texSizes =
    let gridZ = uiGridZ inst
        relativeZ = gridZ - zSlice
    in if gridZ > zSlice ∨ gridZ < (zSlice - 25)
       then Nothing
       else
        let texHandle = resolveTexture facing (uiFacing inst)
                                       (uiDirSprites inst) (uiTexture inst)

            (texW, texH) = case HM.lookup texHandle texSizes of
                Just (w, h) → (fromIntegral w, fromIntegral h)
                Nothing     → (baseTileW, baseTileH)

            scaleX = texW / baseTileW
            scaleY = texH / baseTileH
            quadW = tileWidth  * scaleX
            quadH = tileHeight * scaleY

            gxF = uiGridX inst
            gyF = uiGridY inst
            (faF, fbF) = applyFacingF facing gxF gyF

            rawX = (faF - fbF) * tileHalfWidth - tileHalfWidth
            rawY = (faF + fbF) * tileHalfDiamondHeight

            heightOffset = fromIntegral relativeZ * tileSideHeight
            baseRadius = uiBaseWidth inst * 0.5 / baseTileH * tileHeight

            drawX = rawX + (tileWidth - quadW) * 0.5
            drawY = rawY - heightOffset
                  + tileHalfDiamondHeight - quadH + baseRadius

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

applyFacingF ∷ CameraFacing → Float → Float → (Float, Float)
applyFacingF FaceSouth gx gy = ( gx,  gy)
applyFacingF FaceWest  gx gy = ( gy, -gx)
applyFacingF FaceNorth gx gy = (-gx, -gy)
applyFacingF FaceEast  gx gy = (-gy,  gx)
