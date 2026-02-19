{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Render.Camera
    ( camEpsilon
    , cameraChanged
    ) where

import UPrelude
import World.Types
import World.Grid (tileHalfWidth)

-----------------------------------------------------------
-- Camera Change Detection
-----------------------------------------------------------

camEpsilon ∷ Float
camEpsilon = tileHalfWidth

cameraChanged ∷ WorldCameraSnapshot → WorldCameraSnapshot → Bool
cameraChanged old new =
    let (ox, oy) = wcsPosition old
        (nx, ny) = wcsPosition new
    in abs (ox - nx) > camEpsilon
     ∨ abs (oy - ny) > camEpsilon
     ∨ abs (wcsZoom old - wcsZoom new) > camEpsilon
     ∨ wcsZSlice old ≢ wcsZSlice new
     ∨ wcsFbSize old ≢ wcsFbSize new
     ∨ wcsFacing old ≢ wcsFacing new
