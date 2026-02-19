{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.ViewBounds
    ( ViewBounds(..)
    , computeViewBounds
    , isTileVisible
    ) where

import UPrelude
import Engine.Graphics.Camera (Camera2D(..))
import World.Grid (tileWidth, tileHeight, tileSideHeight)
import World.Render.Camera (camEpsilon)

-----------------------------------------------------------
-- View Bounds
-----------------------------------------------------------

data ViewBounds = ViewBounds
    { vbLeft   ∷ !Float
    , vbRight  ∷ !Float
    , vbTop    ∷ !Float
    , vbBottom ∷ !Float
    } deriving (Show)

computeViewBounds ∷ Camera2D → Int → Int → Int → ViewBounds
computeViewBounds camera fbW fbH effDepth =
    let (cx, cy) = camPosition camera
        zoom     = camZoom camera
        aspect   = fromIntegral fbW / fromIntegral fbH
        halfW    = zoom * aspect
        halfH    = zoom
        maxHeightPad = fromIntegral effDepth * tileSideHeight
        padX     = tileWidth + camEpsilon
        padY     = tileHeight + maxHeightPad + camEpsilon
    in ViewBounds
        { vbLeft   = cx - halfW - padX
        , vbRight  = cx + halfW + padX
        , vbTop    = cy - halfH - padY
        , vbBottom = cy + halfH + padY
        }

isTileVisible ∷ ViewBounds → Float → Float → Bool
isTileVisible vb drawX drawY =
    let tileRight  = drawX + tileWidth
        tileBottom = drawY + tileHeight
    in not (tileRight  < vbLeft vb
         ∨ drawX      > vbRight vb
         ∨ tileBottom < vbTop vb
         ∨ drawY      > vbBottom vb)
