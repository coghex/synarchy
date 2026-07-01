{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Camera
    ( camEpsilon
    , quadCacheMarginFrac
    , quadCacheMargins
    , cameraChanged
    ) where

import UPrelude
import Engine.Graphics.Viewport (safeAspect)
import World.Types
import World.Grid (tileHalfWidth)

-- * Camera Change Detection

camEpsilon ∷ Float
camEpsilon = tileHalfWidth

-- | Fraction of the viewport half-extent baked into the cached tile
--   pass as extra view-bounds margin, and — paired — the extra camera
--   travel allowed before the quad cache invalidates (#447). One
--   constant feeds both consumers of 'quadCacheMargins' (bounds
--   expansion in renderWorldQuads, invalidation below) so coverage and
--   invalidation can't diverge. 0.25 trades ~1.5× more cached quads
--   for ~5× fewer full rebuilds while panning.
quadCacheMarginFrac ∷ Float
quadCacheMarginFrac = 0.25

-- | Per-axis world-unit margins for a snapshot's viewport. camZoom is
--   the viewport HALF-HEIGHT in world units; width scales by the
--   framebuffer aspect (zero-size-safe), so the relative overhead is
--   constant across zoom levels.
quadCacheMargins ∷ WorldCameraSnapshot → (Float, Float)
quadCacheMargins snap =
    let (fbW, fbH) = wcsFbSize snap
        aspect     = safeAspect fbW fbH
        z          = wcsZoom snap
    in (quadCacheMarginFrac * z * aspect, quadCacheMarginFrac * z)

-- | The position threshold pairs exactly with the coverage built at
--   'old': quads were culled to viewport(old) + camEpsilon pad
--   (computeViewBounds) + quadCacheMargins(old) (renderWorldQuads), so
--   the true viewport stays inside the built coverage for any pan up
--   to camEpsilon + margin per axis.
cameraChanged ∷ WorldCameraSnapshot → WorldCameraSnapshot → Bool
cameraChanged old new =
    let (ox, oy) = wcsPosition old
        (nx, ny) = wcsPosition new
        (mX, mY) = quadCacheMargins old
    in abs (ox - nx) > camEpsilon + mX
     ∨ abs (oy - ny) > camEpsilon + mY
     ∨ abs (wcsZoom old - wcsZoom new) > camEpsilon
     ∨ wcsZSlice old ≢ wcsZSlice new
     ∨ wcsFbSize old ≢ wcsFbSize new
     ∨ wcsFacing old ≢ wcsFacing new
