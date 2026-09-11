-- | Page-space and camera geometry stays in Haskell; C receives only a local
-- orthonormal screen-facing frame, an affine rebase, and range/gain targets.
module Engine.Audio.Spatial
  ( relativePosition, listenerRebase, listenerWrapFrame, zoomTargets, shortest, screenFrame ) where

import UPrelude
import Engine.Audio.Config.Runtime
import Engine.Audio.Native.Command (XYZ, Affine3)
import Engine.Audio.Types
import Engine.Graphics.Camera (CameraFacing)
import World.Grid (applyFacingF)

shortest ∷ Float → Float → Float
shortest period delta
  | period ≤ 0 ∨ not (finite period ∧ finite delta) = delta
  | otherwise = realToFrac $ value - width * fromInteger (floor (value / width + 0.5) ∷ Integer)
  where value = realToFrac delta ∷ Double
        width = realToFrac period ∷ Double

-- The isometric projection's unequal pixel scales must not distort tile
-- distance. A 45-degree orthonormal frame supplies the visible horizontal
-- direction while preserving grid Euclidean length.
screenFrame ∷ CameraFacing → Float → Float → (Float, Float)
screenFrame facing x y =
  let (a, b) = applyFacingF facing x y
      diagonal = sqrt 0.5
  in ((a - b) * diagonal, (a + b) * diagonal)

relativePosition ∷ ListenerSnapshot → AudioPosition → Maybe XYZ
relativePosition listener position
  | not (validListener listener ∧ validPosition position) = Nothing
  | listenerPage listener ≢ apPage position = Nothing
  | otherwise =
      let dx = apX position - listenerX listener
          dy = apY position - listenerY listener
          u = shortest (listenerWrapU listener) (dx - dy)
          v = shortest (listenerWrapV listener) (dx + dy)
          (x, y) = screenFrame (listenerFacing listener) ((u + v) / 2) ((v - u) / 2)
          z = apZ position - listenerZ listener
      in if all finite [x, y, z] then Just (x, y, z) else Nothing

listenerRebase ∷ ListenerSnapshot → ListenerSnapshot → Maybe Affine3
listenerRebase old new
  | listenerPage old ≢ listenerPage new
    ∨ listenerWrapU old ≢ listenerWrapU new ∨ listenerWrapV old ≢ listenerWrapV new = Nothing
  | otherwise = do
      (tx, ty, tz) ← relativePosition new
        (AudioPosition (listenerPage old) (listenerX old) (listenerY old) (listenerZ old))
      -- A shift beyond a quarter circumference (capped at 128 tiles) is a
      -- discontinuity: discard World instead of applying an ambiguous alias.
      let periods = filter (> 0) [listenerWrapU new, listenerWrapV new]
          threshold = minimum (128 : map (/ 4) periods)
      guard (sqrt (tx * tx + ty * ty) ≤ threshold ∧ abs tz ≤ 128)
      let (ox1, oy1) = screenFrame (listenerFacing old) 1 0
          (ox2, oy2) = screenFrame (listenerFacing old) 0 1
          (nx1, ny1) = screenFrame (listenerFacing new) 1 0
          (nx2, ny2) = screenFrame (listenerFacing new) 0 1
          a = nx1 * ox1 + nx2 * ox2
          b = nx1 * oy1 + nx2 * oy2
          c = ny1 * ox1 + ny2 * ox2
          d = ny1 * oy1 + ny2 * oy2
      pure ((a,b,0,tx), (c,d,0,ty), (0,0,1,tz))

-- | Generic orthogonal period vectors in the NEW camera frame. Native applies
-- these after rebase so already-playing voices keep the shortest wrapped image.
listenerWrapFrame ∷ ListenerSnapshot → (XYZ, XYZ)
listenerWrapFrame listener =
  let halfU = listenerWrapU listener / 2
      halfV = listenerWrapV listener / 2
      vector x y = let (sx, sy) = screenFrame (listenerFacing listener) x y in (sx, sy, 0)
  in (vector halfU (-halfU), vector halfV halfV)

zoomTargets ∷ RuntimeConfig → ListenerSnapshot → (Float, Float)
zoomTargets config listener =
  let zoom = listenerZoom listener
      start = listenerFadeStart listener
      end = listenerFadeEnd listener
      detail = clamp 0 1 $ (zoom - listenerZoomFloor listener) / (start - listenerZoomFloor listener)
      fade = clamp 0 1 $ (zoom - start) / (end - start)
      smooth = fade * fade * (3 - 2 * fade)
      interpolate a b = a + (b - a) * detail
      range = interpolate (rcCloseRangeScale config) (rcFarRangeScale config)
      gainDb = interpolate (rcCloseGainDb config) (rcFarGainDb config)
  in (range, (1 - smooth) * 10 ** (gainDb / 20))
