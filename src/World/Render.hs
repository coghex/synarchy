{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render
    ( updateWorldTiles
    , surfaceHeadroom
    ) where

import UPrelude
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Maybe (isJust)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), EngineState(..), GraphicsState(..))
import Engine.Core.Monad (EngineM)
import Engine.Scene.Base (LayerId(..), ObjectId(..))
import Engine.Scene.Types (RenderBatch(..), SortableQuad(..))
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..), Vec4(..))
import Engine.Graphics.Vulkan.Texture.Types (BindlessTextureSystem(..))
import Engine.Graphics.Vulkan.Texture.Bindless (getTextureSlotIndex)
import Engine.Asset.Handle (TextureHandle(..))
import World.Types
import World.Constants (seaLevel)
import World.Slope (slopeToFaceMapIndex)
import World.Fluids (FluidCell(..), FluidType(..))
import World.Generate (chunkToGlobal, chunkWorldBounds, viewDepth, globalToChunk)
import World.Grid (tileWidth, tileHeight, gridToScreen, tileSideHeight, worldLayer,
                   tileHalfWidth, tileHalfDiamondHeight, zoomFadeStart, zoomFadeEnd
                   , worldToGrid, worldScreenWidth, applyFacing)
import World.ZoomMap (generateZoomMapQuads, generateBackgroundQuads)

import World.Render.Camera (cameraChanged, camEpsilon)
import World.Render.Quads (renderWorldQuads, renderWorldCursorQuads)
import World.Render.ViewBounds (computeViewBounds)
import World.Render.ChunkCulling (isChunkRelevantForSlice)
import World.Render.Textures ()
import World.Render.TileQuads ()

-----------------------------------------------------------
-- Surface Headroom
-----------------------------------------------------------

surfaceHeadroom ∷ Int
surfaceHeadroom = 25

-----------------------------------------------------------
-- Top-Level Entry Point
-----------------------------------------------------------

updateWorldTiles ∷ EngineEnv → IO (V.Vector SortableQuad)
updateWorldTiles env = do
    camera ← readIORef (cameraRef env)
    (fbW, fbH) ← readIORef (framebufferSizeRef env)

    let zoom = camZoom camera
        tileAlpha = clamp01 (1.0 - (zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))
        zoomAlpha = clamp01 ((zoom - zoomFadeStart) / (zoomFadeEnd - zoomFadeStart))

    worldManager ← readIORef (worldManagerRef env)

    tileQuads ← if tileAlpha ≤ 0.001
        then return V.empty
        else do
            let currentSnap = WorldCameraSnapshot
                    { wcsPosition = camPosition camera
                    , wcsZoom     = zoom
                    , wcsZSlice   = camZSlice camera
                    , wcsFbSize   = (fbW, fbH)
                    , wcsFacing   = camFacing camera
                    }
            quads ← forM (wmVisible worldManager) $ \pageId →
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState → do
                        cached ← readIORef (wsQuadCacheRef worldState)
                        case cached of
                            Just wqc | not (cameraChanged (wqcCamera wqc) currentSnap) →
                                return (wqcQuads wqc)
                            _ → do
                                result ← renderWorldQuads env worldState tileAlpha currentSnap
                                writeIORef (wsQuadCacheRef worldState) $
                                    Just (WorldQuadCache currentSnap result)
                                return result
                    Nothing → return V.empty
            return $ V.concat quads

    -- Cursor quads are generated every frame (cheap: just 1-2 quads)
    -- so they respond instantly to mouse movement
    worldCursorQuads ← if tileAlpha ≤ 0.001
        then return V.empty
        else do
            cursorResults ← forM (wmVisible worldManager) $ \pageId →
                case lookup pageId (wmWorlds worldManager) of
                    Just worldState →
                        renderWorldCursorQuads env worldState tileAlpha
                    Nothing → return V.empty
            return $ V.concat cursorResults

    zoomQuads ← generateZoomMapQuads env camera fbW fbH

    let shouldTrack = camZTracking camera
                    ∨ (tileAlpha > 0.001 ∧ tileAlpha < 0.999)
    when shouldTrack $ do
        when (not (camZTracking camera)) $
            atomicModifyIORef' (cameraRef env) $ \cam →
                (cam { camZTracking = True }, ())
        worldManager' ← readIORef (worldManagerRef env)
        forM_ (wmVisible worldManager') $ \pageId →
            case lookup pageId (wmWorlds worldManager') of
                Just worldState → do
                    tileData ← readIORef (wsTilesRef worldState)
                    let (camX, camY) = camPosition camera
                        facing = camFacing camera
                        (gx, gy) = worldToGrid facing camX camY
                        (chunkCoord, (lx, ly)) = globalToChunk gx gy
                    case lookupChunk chunkCoord tileData of
                        Just lc → do
                            let surfElev = (lcSurfaceMap lc) VU.! columnIndex lx ly
                                targetZ = surfElev + surfaceHeadroom
                            atomicModifyIORef' (cameraRef env) $ \cam →
                                (cam { camZSlice = targetZ }, ())
                        Nothing → return ()
                Nothing → return ()

    let allQuads = tileQuads <> worldCursorQuads <> zoomQuads
    return allQuads
