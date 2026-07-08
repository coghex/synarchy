{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Blood decal rendering (#606). Two halves that only meet through
--   'World.State.Types.wsBloodTextureHandlesRef':
--
--   * 'uploadBloodTextures' — GPU-touching, EngineM. Once per frame,
--     diffs each loaded world's blood-texture FIFO ('Blood.Types') against
--     what's already resident on the GPU: uploads pixel data
--     ('Blood.Texture.generateBloodTexture') for anything new, and
--     unregisters + disposes anything the FIFO evicted (issue #606
--     requirement 5). Mirrors the staging-buffer upload
--     'Engine.Scripting.Lua.Message.handleZoomAtlasUpload' already does
--     for the zoom atlas, generalized from "one shared texture" to "a
--     per-world set of independently-lived textures".
--
--   * 'renderBloodDecalQuads' — pure IO, no GPU calls, same shape as
--     'World.Render.GroundItemQuads.renderGroundItemQuads'. Turns each
--     visible decal into a world-space 'SortableQuad' through the SAME
--     bindless/vertex path every other world sprite uses (issue #606
--     requirement 9: no dedicated decal pipeline), sat just above bare
--     terrain and below units/ground items (see 'bloodSortNudge').
--
--   A decal whose texture was never uploaded (GPU upload lags a frame
--   behind a fresh 'blood.spawn', or the bindless system was full) simply
--   has no entry in 'wsBloodTextureHandlesRef' yet and is skipped this
--   frame — it appears once 'uploadBloodTextures' catches up.
module World.Render.BloodQuads
    ( uploadBloodTextures
    , renderBloodDecalQuads
    ) where

import UPrelude
import Control.Monad (foldM)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Maybe (mapMaybe)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Vulkan.Core10 (Device, PhysicalDevice, CommandPool, Queue, deviceWaitIdle)
import Engine.Core.Monad
import Engine.Core.State
import Engine.Asset.Handle (TextureHandle, toInt)
import Engine.Asset.Manager (generateTextureHandle)
import Engine.Graphics.Types (DevQueues(..))
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Vulkan.Texture (createTextureFromRGBABytes)
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (registerTexture, unregisterTexture)
import Engine.Scene.Types (SortableQuad(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..)
                                           , packWorldUV)
import Blood.Types
import Blood.Texture (generateBloodTexture, btiWidth, btiHeight, btiPixels)
import Blood.Render (BloodRenderRecord(..), bloodRenderRecords)
import World.Generate (viewDepth)
import World.Generate.Coordinates (globalToChunk)
import World.Grid (gridToScreen, tileWidth, tileHeight, tileSideHeight
                  , tileHalfWidth, tileHalfDiamondHeight
                  , worldLayer, applyFacing, GridConfig(..)
                  , defaultGridConfig)
import World.Render.ChunkCulling (isChunkVisibleWrapped)
import World.Render.ViewBounds (computeViewBounds)
import World.Types

-- * GPU upload / eviction sync (EngineM, once per frame)

-- | Sync every loaded world's blood-texture FIFO to the GPU. Windows
--   'wmWorlds' rather than just 'wmVisible' — mirrors
--   'Engine.Scripting.Lua.Message.invalidateAllWorldRenderCaches' — so a
--   hidden world's decals are already uploaded the moment it becomes
--   visible instead of popping in a frame late.
uploadBloodTextures ∷ EngineM ε σ ()
uploadBloodTextures = do
    env ← ask
    gs ← gets graphicsState
    mBindless ← liftIO $ readIORef (textureSystemRef env)
    case ( vulkanDevice gs, vulkanPDevice gs, vulkanCmdPool gs
         , deviceQueues gs, mBindless ) of
        (Just dev, Just pdev, Just cmdPool, Just queues, Just bindless0) → do
            mgr ← liftIO $ readIORef (worldManagerRef env)
            finalBindless ← foldM
                (syncWorldBloodTextures dev pdev cmdPool (graphicsQueue queues))
                bindless0 (wmWorlds mgr)
            liftIO $ writeIORef (textureSystemRef env) (Just finalBindless)
        _ → pure ()

-- | One world's diff-and-sync step: unregister anything evicted from its
--   FIFO since last frame, then upload anything new.
syncWorldBloodTextures ∷ Device → PhysicalDevice → CommandPool → Queue
                       → BindlessTextureSystem → (WorldPageId, WorldState)
                       → EngineM ε σ BindlessTextureSystem
syncWorldBloodTextures dev pdev cmdPool queue bindless0 (_pid, ws) = do
    pool  ← bstPool ⊚ liftIO (readIORef (wsBloodStoreRef ws))
    known ← liftIO $ readIORef (wsBloodTextureHandlesRef ws)
    let liveDescs = HM.fromList [ (btdId d, d) | d ← allTextures pool ]
        staleIds  = filter (\tid → not (HM.member tid liveDescs)) (HM.keys known)
        newDescs  = [ d | (tid, d) ← HM.toList liveDescs
                        , not (HM.member tid known) ]
    -- An evicted texture may still be sampled by an already-submitted,
    -- not-yet-presented frame: this runs from processLuaMessages, before
    -- drawFrame, and drawFrame only waits the CURRENT frame's own fence,
    -- not every frame still in flight (same reasoning
    -- Engine.Scripting.Lua.Message.disposeTransientTexture documents for
    -- the preview/zoom-atlas texture). Wait once, only when this pass
    -- actually has something to destroy, rather than stalling every frame.
    when (not (null staleIds)) $ liftIO (deviceWaitIdle dev)
    (bindless1, known1) ← foldM (evictOne dev) (bindless0, known) staleIds
    (bindless2, known2) ← foldM (uploadOne dev pdev cmdPool queue)
                                 (bindless1, known1) newDescs
    liftIO $ writeIORef (wsBloodTextureHandlesRef ws) known2
    pure bindless2

type HandleMap = HM.HashMap BloodTextureId (TextureHandle, IO ())

-- | Unregister + dispose one evicted texture's GPU resources (after the
--   caller has already waited for the device to go idle — see
--   'syncWorldBloodTextures'), and drop its now-stale 'textureSizeRef'
--   entry (issue #606 review: eviction must fully release generated
--   bookkeeping, not just the bindless slot).
evictOne ∷ Device → (BindlessTextureSystem, HandleMap) → BloodTextureId
        → EngineM ε σ (BindlessTextureSystem, HandleMap)
evictOne dev (bl, known) tid = case HM.lookup tid known of
    Nothing            → pure (bl, known)
    Just (h, cleanup)  → do
        bl' ← unregisterTexture dev h bl
        env ← ask
        liftIO $ atomicModifyIORef' (textureSizeRef env) (\m → (HM.delete h m, ()))
        liftIO cleanup
        pure (bl', HM.delete tid known)

uploadOne ∷ Device → PhysicalDevice → CommandPool → Queue
         → (BindlessTextureSystem, HandleMap) → BloodTextureDescriptor
         → EngineM ε σ (BindlessTextureSystem, HandleMap)
uploadOne dev pdev cmdPool queue (bl, known) d = do
    let img = generateBloodTexture d
    env ← ask
    poolRef ← asks assetPoolRef
    ap ← liftIO $ readIORef poolRef
    texHandle ← liftIO $ generateTextureHandle ap
    ((_image, imageView), cleanupImg) ← createTextureFromRGBABytes
        pdev dev cmdPool queue (btiWidth img, btiHeight img) (btiPixels img)
    (mBindlessHandle, bl') ← registerTexture dev texHandle imageView
                                (btsTextureSampler bl) bl
    case mBindlessHandle of
        Just _  → do
            liftIO $ atomicModifyIORef' (textureSizeRef env) $ \m →
                (HM.insert texHandle (btiWidth img, btiHeight img) m, ())
            pure (bl', HM.insert (btdId d) (texHandle, cleanupImg) known)
        Nothing → do
            -- Bindless system full — drop the GPU resource we just made;
            -- this decal's texture simply isn't uploaded yet and its
            -- render pass is skipped until a slot frees up.
            liftIO cleanupImg
            pure (bl', known)

-- * Quad building (pure IO, per frame — same shape as GroundItemQuads)

baseTileW ∷ Float
baseTileW = fromIntegral (gcTilePixelWidth defaultGridConfig)

baseTileH ∷ Float
baseTileH = fromIntegral (gcTilePixelHeight defaultGridConfig)

-- | How large a fresh blood mark reads relative to a tile at
--   'brrScale' == 1 — big enough to be legible, small enough not to
--   swallow the whole tile.
bloodDecalBaseScale ∷ Float
bloodDecalBaseScale = 1.4

-- | Sort-key offset above bare terrain (0.0) and below ground items /
--   units (0.0006, see 'World.Render.GroundItemQuads'/'Unit.Render') —
--   the same convention 'World.Render.SpoilQuads' and
--   'World.Render.FloraQuads' use for "sits on the ground".
bloodSortNudge ∷ Float
bloodSortNudge = 0.0003

-- | Rotate @(x, y)@ by @angle@ radians around @(cx, cy)@.
rotateAround ∷ Float → Float → Float → Float → Float → (Float, Float)
rotateAround cx cy angle x y =
    let dx = x - cx
        dy = y - cy
        ca = cos angle
        sa = sin angle
    in (cx + dx * ca - dy * sa, cy + dx * sa + dy * ca)

renderBloodDecalQuads ∷ EngineEnv → WorldPageId → WorldState → Float
                      → IO (V.Vector SortableQuad)
renderBloodDecalQuads env pageId worldState tileAlpha = do
    store ← readIORef (wsBloodStoreRef worldState)
    if HM.null (bdlDecals (bstDecals store))
      then return V.empty
      else do
        now      ← readIORef (gameTimeRef env)
        camera   ← readIORef (cameraRef env)
        handles  ← readIORef (wsBloodTextureHandlesRef worldState)
        texSizes ← readIORef (textureSizeRef env)
        paramsM  ← readIORef (wsGenParamsRef worldState)
        (fbW, fbH) ← readIORef (framebufferSizeRef env)

        let recs = bloodRenderRecords now pageId store
            lookupSlot texHandle = toInt texHandle
            defFmSlot = -1 ∷ Float
            facing  = camFacing camera
            zoom    = camZoom camera
            zSlice  = camZSlice camera
            (camX, _camY) = camPosition camera
            worldSize = maybe 128 wgpWorldSize paramsM
            effectiveDepth =
                min viewDepth (max 8 (round (zoom * 80.0 + 8.0 ∷ Float)))
            vb = computeViewBounds camera fbW fbH effectiveDepth

            quadForM r = do
                (texHandle, _cleanup) ← HM.lookup (brrTexture r) handles
                let tx = floor (brrX r) ∷ Int
                    ty = floor (brrY r) ∷ Int
                    tz = brrSurfaceZ r
                    (chunkCoord, _) = globalToChunk tx ty
                if tz > zSlice ∨ tz < zSlice - effectiveDepth
                  then Nothing
                  else do
                    xOff ← isChunkVisibleWrapped facing worldSize vb camX
                                                 chunkCoord
                    let (texW, texH) = case HM.lookup texHandle texSizes of
                            Just (w, h) → (fromIntegral w, fromIntegral h)
                            Nothing     → (24.0, 24.0)
                        quadW = tileWidth  * (texW / baseTileW)
                              * bloodDecalBaseScale * brrScale r
                        quadH = tileHeight * (texH / baseTileH)
                              * bloodDecalBaseScale * brrScale r

                        fx = brrX r - fromIntegral tx + brrOffsetX r
                        fy = brrY r - fromIntegral ty + brrOffsetY r
                        offU = fx - 0.5
                        offV = fy - 0.5
                        subX = (offU - offV) * tileHalfWidth
                        subY = (offU + offV) * tileHalfDiamondHeight

                        relativeZ = tz - zSlice
                        (rawX, rawY) = gridToScreen facing tx ty
                        heightOffset = fromIntegral relativeZ * tileSideHeight

                        drawX = rawX + subX + (tileWidth - quadW) * 0.5 + xOff
                        drawY = rawY - heightOffset + subY
                              + tileHalfDiamondHeight - quadH * 0.5

                        cx = drawX + quadW * 0.5
                        cy = drawY + quadH * 0.5
                        rot = brrRotation r
                        corner ox oy = rotateAround cx cy rot
                                          (drawX + ox) (drawY + oy)
                        (x0, y0) = corner 0 0
                        (x1, y1) = corner quadW 0
                        (x2, y2) = corner quadW quadH
                        (x3, y3) = corner 0 quadH

                        (fa, fb) = applyFacing facing tx ty
                        sortKey = fromIntegral (fa + fb)
                                + fromIntegral relativeZ * 0.001
                                + bloodSortNudge
                                + fy * 0.00005

                        tint = Vec4 (brrTintR r) (brrTintG r) (brrTintB r)
                                    (brrAlpha r * tileAlpha)
                        slotF = fromIntegral (lookupSlot texHandle)
                        wuv = packWorldUV tx ty

                        v0 = Vertex (Vec2 x0 y0) (Vec2 0 0) tint slotF defFmSlot 0 wuv
                        v1 = Vertex (Vec2 x1 y1) (Vec2 1 0) tint slotF defFmSlot 0 wuv
                        v2 = Vertex (Vec2 x2 y2) (Vec2 1 1) tint slotF defFmSlot 0 wuv
                        v3 = Vertex (Vec2 x3 y3) (Vec2 0 1) tint slotF defFmSlot 0 wuv
                    Just SortableQuad
                        { sqSortKey = sortKey
                        , sqV0 = v0, sqV1 = v1, sqV2 = v2, sqV3 = v3
                        , sqTexture = texHandle
                        , sqLayer = worldLayer
                        }

        return $ V.fromList (mapMaybe quadForM recs)
