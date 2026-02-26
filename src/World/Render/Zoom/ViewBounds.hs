{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | View-frustum culling and wrap-around helpers for zoom-level rendering.
module World.Render.Zoom.ViewBounds
    ( ZoomViewBounds(..)
    , computeZoomViewBounds
    , isChunkInView
    , bestZoomWrapOffset
    ) where

import UPrelude
import Engine.Graphics.Camera (Camera2D(..), CameraFacing(..))
import World.Grid (tileHalfWidth, tileHalfDiamondHeight,
                   chunkWorldWidth, chunkWorldDiamondHeight)
import World.Types (chunkSize)

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
        aspect = fromIntegral fbW / fromIntegral fbH
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

bestZoomWrapOffset ∷ CameraFacing → Int → Float → Float → Float → Float → (Float, Float)
bestZoomWrapOffset facing worldSize camX camY centerX centerY =
    let worldTiles = worldSize * chunkSize
        wswX = fromIntegral worldTiles * tileHalfWidth
        wswY = fromIntegral worldTiles * tileHalfDiamondHeight
    in case facing of
        FaceSouth → (pickBest wswX camX centerX, 0)
        FaceNorth → (pickBest wswX camX centerX, 0)
        FaceWest  → (0, pickBest wswY camY centerY)
        FaceEast  → (0, pickBest wswY camY centerY)
  where
    pickBest w cam center =
        let d0 = abs (center - cam)
            d1 = abs (center + w - cam)
            d2 = abs (center - w - cam)
        in if d1 < d0 then (if d2 < d1 then -w else w)
           else (if d2 < d0 then -w else 0)
