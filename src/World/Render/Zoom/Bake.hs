{-# OPTIONS_GHC -fprof-auto #-}
{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Bake ZoomChunkEntry vectors into render-ready BakedZoomEntry vectors.
module World.Render.Zoom.Bake
    ( bakeEntries
    , ensureBaked
    ) where

import UPrelude
import Data.IORef (readIORef, writeIORef, IORef)
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Graphics.Camera (CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import World.Types
import World.Grid (gridToWorld)

bakeEntries ∷ CameraFacing → V.Vector ZoomChunkEntry
            → (Word8 → Int → TextureHandle)
            → (TextureHandle → Int)
            → Float
            → V.Vector BakedZoomEntry
bakeEntries facing cache texPicker lookupSlot defFmSlot =
    V.map bakeOne cache
  where
    bakeOne entry =
        let texHandle = texPicker (zceTexIndex entry) (zceElev entry)
            actualSlot = fromIntegral (lookupSlot texHandle)
            baseGX = zceBaseGX entry
            baseGY = zceBaseGY entry
            (x0,y0) = gridToWorld facing baseGX baseGY
            (x1,y1) = gridToWorld facing (baseGX + chunkSize) baseGY
            (x2,y2) = gridToWorld facing baseGX (baseGY + chunkSize)
            (x3,y3) = gridToWorld facing (baseGX + chunkSize) (baseGY + chunkSize)
            drawX = min x0 (min x1 (min x2 x3))
            drawY = min y0 (min y1 (min y2 y3))
            w = max x0 (max x1 (max x2 x3)) - drawX
            h = max y0 (max y1 (max y2 y3)) - drawY
            white = Vec4 1.0 1.0 1.0 1.0
        in BakedZoomEntry
            { bzeChunkX  = zceChunkX entry
            , bzeChunkY  = zceChunkY entry
            , bzeDrawX   = drawX
            , bzeDrawY   = drawY
            , bzeWidth   = w
            , bzeHeight  = h
            , bzeSortKey = fromIntegral (zceChunkY entry)
                         + fromIntegral (zceChunkX entry) * 0.0001
            , bzeV0      = Vertex (Vec2 drawX drawY)            (Vec2 0 0) white actualSlot defFmSlot
            , bzeV1      = Vertex (Vec2 (drawX + w) drawY)       (Vec2 1 0) white actualSlot defFmSlot
            , bzeV2      = Vertex (Vec2 (drawX + w) (drawY + h)) (Vec2 1 1) white actualSlot defFmSlot
            , bzeV3      = Vertex (Vec2 drawX (drawY + h))       (Vec2 0 1) white actualSlot defFmSlot
            , bzeTexture = texHandle
            , bzeIsOcean = zceIsOcean entry
            , bzeHasLava = zceHasLava entry
            , bzeElev    = zceElev entry
            }

ensureBaked ∷ IORef (V.Vector BakedZoomEntry, WorldTextures, CameraFacing)
              → V.Vector ZoomChunkEntry → WorldTextures
              → CameraFacing
              → (WorldTextures → Word8 → Int → TextureHandle)
              → (TextureHandle → Int) → Float
              → IO (V.Vector BakedZoomEntry)
ensureBaked bakedRef rawCache textures facing texPicker lookupSlot defFmSlot = do
    (existing, bakedWith, bakedFacing) ← readIORef bakedRef
    let texturesChanged = bakedWith ≢ textures
        facingChanged   = bakedFacing ≢ facing
        needsBake = not (V.null rawCache)
                  ∧ (V.null existing ∨ texturesChanged ∨ facingChanged)
    if needsBake
        then do
            let baked = bakeEntries facing rawCache
                            (\mat elev → texPicker textures mat elev)
                            lookupSlot defFmSlot
            writeIORef bakedRef (baked, textures, facing)
            return baked
        else return existing
