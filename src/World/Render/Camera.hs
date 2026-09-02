{-# LANGUAGE Strict #-}
module World.Render.Camera
    ( camEpsilon
    , quadCacheMargins
    , cameraChanged
    , placementCamera
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
--   ('World.Render.ViewBounds.viewBoundsAt', taken since #1720 from the
--   position and zoom of that same 'old' snapshot rather than from a
--   second live camera read) + quadCacheMargins(old) (renderWorldQuads), so
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

-- * Placement parity

-- | The camera the world quads currently ON SCREEN were built with
--   (#1856).
--
--   Cached tile and flora quads carry WORLD coordinates, and a chunk's
--   wrap alias ('World.Render.ChunkCulling.bestWrapOffset') is baked
--   into them: it is chosen by distance to the camera, so it switches
--   DISCONTINUOUSLY at the cylindrical seam's midpoint. 'cameraChanged'
--   deliberately tolerates a pan of up to 'camEpsilon' plus the cache
--   margins before rebuilding — and that pan can cross the midpoint. In
--   the window between the crossing and the next rebuild, a tree is
--   DRAWN at the old alias while anything reading the live camera
--   computes the new one, a whole world width away.
--
--   So anything that has to agree with where a sprite is drawn —
--   Chop's screen-space selection oracle, and the designation marker
--   anchored to whatever it picked — derives its PLACEMENT from here.
--   No prediction is involved: after every frame 'wsQuadCacheRef' holds
--   the cache whose quads were drawn, and a reuse leaves its snapshot
--   untouched while a rebuild overwrites it with the one it built at,
--   so this is the placement camera whatever the render pass decided
--   and whyever it decided it.
--
--   The LIVE camera still supplies the VIEW transform — the
--   pixel→world unprojection — which is correct and unaffected: cached
--   world coordinates are viewed through the live camera every frame.
--
--   Falls back to the live snapshot when no cache exists yet (the first
--   frame of a page), where the live camera is what the imminent build
--   will use.
placementCamera
    ∷ Maybe WorldQuadCache → WorldCameraSnapshot → WorldCameraSnapshot
placementCamera cached live = maybe live wqcCamera cached
