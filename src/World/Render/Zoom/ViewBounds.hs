{-# LANGUAGE Strict #-}
-- | View-frustum culling and wrap-around helpers for zoom-level rendering.
module World.Render.Zoom.ViewBounds
    ( ZoomViewBounds(..)
    , computeZoomViewBounds
    , isChunkInView
    , bestZoomWrapOffset
    ) where

import UPrelude
import Engine.Graphics.Camera (Camera2D(..), CameraFacing)
import Engine.Graphics.Viewport (safeAspect)
import World.Grid (worldWrapPeriod,
                   chunkWorldWidth, chunkWorldDiamondHeight)

data ZoomViewBounds = ZoomViewBounds
    { zvLeft   ∷ !Float
    , zvRight  ∷ !Float
    , zvTop    ∷ !Float
    , zvBottom ∷ !Float
    }

computeZoomViewBounds ∷ Camera2D → Int → Int → ZoomViewBounds
computeZoomViewBounds camera fbW fbH =
    let (cx, cy) = camPosition camera
        zoom = camZoom camera
        -- Guard against a zero-size framebuffer (minimize): a raw
        -- fbW/fbH would feed Infinity/NaN into the culling bounds.
        aspect = safeAspect fbW fbH
        halfW = zoom * aspect
        halfH = zoom
        padX = chunkWorldWidth * 2.0
        padY = chunkWorldDiamondHeight * 2.0
    in ZoomViewBounds
        { zvLeft   = cx - halfW - padX
        , zvRight  = cx + halfW + padX
        , zvTop    = cy - halfH - padY
        , zvBottom = cy + halfH + padY
        }

isChunkInView ∷ ZoomViewBounds → Float → Float → Float → Float → Bool
isChunkInView vb drawX drawY w h =
    let right  = drawX + w
        bottom = drawY + h
    in not (right  < zvLeft vb
         ∨ drawX  > zvRight vb
         ∨ bottom < zvTop vb
         ∨ drawY  > zvBottom vb)

-- | Zoom-render twin of @World.Render.ChunkCulling.bestWrapOffset@. The
--   two renderers keep separate bounds types and culling, but the
--   world's own wrap period is one fact: both read it from
--   'World.Grid.worldWrapPeriod' (#1176) rather than restating the
--   tile-geometry product.
bestZoomWrapOffset ∷ CameraFacing → Int → Float → Float → Float → Float → (Float, Float)
bestZoomWrapOffset facing worldSize camX camY centerX centerY =
    let (wswX, wswY) = worldWrapPeriod facing worldSize
    -- The inactive axis has period 0, which collapses pickBest's three
    -- candidates onto 0 — so there is no per-facing case to restate.
    in (pickBest wswX camX centerX, pickBest wswY camY centerY)
  where
    pickBest w cam center =
        let d0 = abs (center - cam)
            d1 = abs (center + w - cam)
            d2 = abs (center - w - cam)
        in if d1 < d0 then (if d2 < d1 then -w else w)
           else (if d2 < d0 then -w else 0)
