{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scene.Batch.Visibility
  ( isNodeVisible
  , isUILayer
  ) where

import UPrelude
import Engine.Scene.Base (LayerId(..), Transform2D(..))
import Engine.Scene.Types.Node (SceneNode(..))
import Engine.Graphics.Camera (Camera2D(..))

isNodeVisible ∷ Camera2D → Float → Float → SceneNode → Bool
isNodeVisible camera viewWidth viewHeight node =
    if not (nodeVisible node)
    then False
    else
        let (camX, camY) = camPosition camera
            zoom = camZoom camera
            (nodeX, nodeY) = position (nodeTransform node)
            (sclX, sclY) = scale (nodeTransform node)
            rot = rotation (nodeTransform node)
            (sizeX, sizeY) = nodeSize node
            w = sizeX * sclX
            h = sizeY * sclY

            -- Conservative half-extents: axis-aligned when unrotated;
            -- a rotated quad fits inside its half-diagonal circle.
            (halfW, halfH) = if rot ≡ 0
                             then (w * 0.5, h * 0.5)
                             else let r = 0.5 * sqrt (w * w + h * h)
                                  in (r, r)

            left = camX - (viewWidth * zoom * 0.5)
            right = camX + (viewWidth * zoom * 0.5)
            bottom = camY - (viewHeight * zoom * 0.5)
            top = camY + (viewHeight * zoom * 0.5)

            nodeLeft = nodeX - halfW
            nodeRight = nodeX + halfW
            nodeBottom = nodeY - halfH
            nodeTop = nodeY + halfH

        in not (nodeRight < left ∨ nodeLeft > right ∨
                nodeTop < bottom ∨ nodeBottom > top)

-- | UI layers (>= 10) bypass frustum culling.
isUILayer ∷ LayerId → Bool
isUILayer (LayerId l) = l ≥ 10
