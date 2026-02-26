{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Background layer rendering (always-visible terrain beneath the zoom map).
module World.Render.Zoom.Background
    ( generateBackgroundQuads
    , emitQuadBg
    ) where

import UPrelude
import Data.IORef (readIORef, IORef)
import qualified Data.Vector as V
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scene.Base (LayerId(..))
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import World.Types
import World.Constants (seaLevel)
import World.Grid (backgroundMapLayer)

import World.Render.Zoom.Bake (ensureBaked)
import World.Render.Zoom.ViewBounds (computeZoomViewBounds, isChunkInView, bestZoomWrapOffset)
import World.Render.Zoom.Textures (getBgTexture)

generateBackgroundQuads ∷ EngineEnv → Camera2D → Int → Int → IO (V.Vector SortableQuad)
generateBackgroundQuads env camera fbW fbH = do
    worldManager ← readIORef (worldManagerRef env)

    let zSlice = camZSlice camera

    quads ← forM (wmVisible worldManager) $ \pageId →
        case lookup pageId (wmWorlds worldManager) of
            Just worldState →
                renderFromBakedBg env worldState camera
                    fbW fbH 1.0 getBgTexture
                    (wsBakedBgRef worldState) backgroundMapLayer zSlice
            Nothing → return V.empty
    return $ V.concat quads

renderFromBakedBg ∷ EngineEnv → WorldState → Camera2D → Int → Int → Float
                  → (WorldTextures → Word8 → Int → TextureHandle)
                  → IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)
                  → LayerId → Int
                  → IO (V.Vector SortableQuad)
renderFromBakedBg env worldState camera fbW fbH alpha texturePicker bakedRef layer zSlice = do
    mParams  ← readIORef (wsGenParamsRef worldState)
    textures ← readIORef (wsTexturesRef worldState)
    rawCache ← readIORef (wsZoomCacheRef worldState)

    mBindless ← readIORef (textureSystemRef env)
    defFmSlotWord ← readIORef (defaultFaceMapSlotRef env)
    let lookupSlot texHandle = fromIntegral $ case mBindless of
            Just bindless → getTextureSlotIndex texHandle bindless
            Nothing       → 0
        defFmSlot = fromIntegral defFmSlotWord
        facing = camFacing camera
    case mParams of
        Nothing → return V.empty
        Just params → do
            baked ← ensureBaked bakedRef rawCache textures facing
                        texturePicker lookupSlot defFmSlot
            let vb = computeZoomViewBounds camera fbW fbH
                (camX, camY) = camPosition camera
                ws = wgpWorldSize params

                !visibleQuads = V.mapMaybe (\entry →
                    let baseX = bzeDrawX entry
                        baseY = bzeDrawY entry
                        w = bzeWidth entry
                        h = bzeHeight entry
                        centerX = baseX + w / 2.0
                        centerY = baseY + h / 2.0
                        (offX, offY) = bestZoomWrapOffset facing ws camX camY centerX centerY
                        wrappedX = baseX + offX
                        wrappedY = baseY + offY
                    in if isChunkInView vb wrappedX wrappedY w h
                       then Just (emitQuadBg entry wrappedX wrappedY alpha layer zSlice)
                       else Nothing
                    ) baked
            return visibleQuads

emitQuadBg ∷ BakedZoomEntry → Float → Float → Float → LayerId → Int → SortableQuad
emitQuadBg entry dx dy alpha layer zSlice =
    let !baseX = bzeDrawX entry
        !baseY = bzeDrawY entry
        !xShift = dx - baseX
        !yShift = dy - baseY

        (tintR, tintG, tintB) =
            if bzeIsOcean entry ∧ zSlice < seaLevel
            then let waterDepth = max 0 (seaLevel - zSlice)
                     t = clamp01 (fromIntegral waterDepth / 30.0)
                     r = 0.6 - t * 0.4
                     g = 0.7 - t * 0.4
                     b = 0.9 - t * 0.3
                 in (r, g, b)
            else (1.0, 1.0, 1.0)

        shiftV (Vertex (Vec2 px py) uv (Vec4 _ _ _ _) aid fid) =
            Vertex (Vec2 (px + xShift) (py + yShift)) uv (Vec4 tintR tintG tintB alpha) aid fid
        v0 = shiftV (bzeV0 entry)
        v1 = shiftV (bzeV1 entry)
        v2 = shiftV (bzeV2 entry)
        v3 = shiftV (bzeV3 entry)
    in SortableQuad
        { sqSortKey  = bzeSortKey entry
        , sqV0       = v0
        , sqV1       = v1
        , sqV2       = v2
        , sqV3       = v3
        , sqTexture  = bzeTexture entry
        , sqLayer    = layer
        }
