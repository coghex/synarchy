{-# LANGUAGE Strict #-}
module World.Render.ViewBounds
    ( ViewBounds(..)
    , computeViewBounds
    , viewBoundsAt
    , expandViewBounds
    , isTileVisible
    ) where

import UPrelude
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Viewport (safeAspect)
import World.Grid (tileWidth, tileHeight, tileSideHeight)
import World.Render.Camera (camEpsilon)

-- * View Bounds

data ViewBounds = ViewBounds
    { vbLeft   ∷ !Float
    , vbRight  ∷ !Float
    , vbTop    ∷ !Float
    , vbBottom ∷ !Float
    } deriving (Show)

-- | The bounds a viewport covers, taken from the only two camera
--   fields that decide them — position and zoom. 'computeViewBounds'
--   is this over a LIVE 'Camera2D'; the cached tile pass reaches it
--   through the 'World.Render.Camera.Types.WorldCameraSnapshot' its
--   cache entry is stamped with instead (#1720), so the geometry and
--   the stamp can never come from two different reads of a camera
--   other threads are concurrently rewriting.
viewBoundsAt ∷ (Float, Float)   -- ^ camera position
             → Float            -- ^ camera zoom (viewport half-height)
             → Int → Int        -- ^ framebuffer width, height
             → Int              -- ^ effective view depth
             → ViewBounds
viewBoundsAt (cx, cy) zoom fbW fbH effDepth =
    let -- Guard against a zero-size framebuffer (minimize): a raw
        -- fbW/fbH would feed Infinity/NaN into the culling bounds.
        aspect   = safeAspect fbW fbH
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

computeViewBounds ∷ Camera2D → Int → Int → Int → ViewBounds
computeViewBounds camera =
    viewBoundsAt (camPosition camera) (camZoom camera)

-- | Widen bounds by per-axis margins. The cached tile pass uses this
--   with 'World.Render.Camera.quadCacheMargins' so a pan can travel
--   the margin before the quad cache must rebuild (#447). Per-frame
--   (dynamic) passes keep the tight bounds — margin would only make
--   them build more offscreen quads every tick.
expandViewBounds ∷ (Float, Float) → ViewBounds → ViewBounds
expandViewBounds (mX, mY) vb = ViewBounds
    { vbLeft   = vbLeft vb - mX
    , vbRight  = vbRight vb + mX
    , vbTop    = vbTop vb - mY
    , vbBottom = vbBottom vb + mY
    }

isTileVisible ∷ ViewBounds → Float → Float → Bool
isTileVisible vb drawX drawY =
    let tileRight  = drawX + tileWidth
        tileBottom = drawY + tileHeight
    in not (tileRight  < vbLeft vb
         ∨ drawX      > vbRight vb
         ∨ tileBottom < vbTop vb
         ∨ drawY      > vbBottom vb)
