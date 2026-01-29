{-# LANGUAGE Strict #-}
module Engine.Scene.Batch.Visibility
  ( isNodeVisible
  , isUILayer
  ) where

import UPrelude
import Engine.Scene.Base (LayerId(..), Transform2D(..))
import Engine.Scene.Types.Node (SceneNode(..))
import Engine.Graphics.Camera (Camera2D(..))

-- | Check if a node is visible within camera frustum
isNodeVisible ∷ Camera2D → Float → Float → SceneNode → Bool
isNodeVisible camera viewWidth viewHeight node =
    if not (nodeVisible node)
    then False
    else
        let (camX, camY) = camPosition camera
            zoom = camZoom camera
            (nodeX, nodeY) = position (nodeTransform node)
            (sizeX, sizeY) = nodeSize node
            
            left = camX - (viewWidth * zoom * 0.5)
            right = camX + (viewWidth * zoom * 0.5)
            bottom = camY - (viewHeight * zoom * 0.5)
            top = camY + (viewHeight * zoom * 0.5)
            
            nodeLeft = nodeX - sizeX * 0.5
            nodeRight = nodeX + sizeX * 0.5
            nodeBottom = nodeY - sizeY * 0.5
            nodeTop = nodeY + sizeY * 0.5
            
        in not (nodeRight < left || nodeLeft > right || 
                nodeTop < bottom || nodeBottom > top)

-- | Check if a layer is a UI layer (always visible)
isUILayer ∷ LayerId → Bool
isUILayer (LayerId l) = l >= 10
