{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Manager.BoxTexture
  ( registerBoxTextures
  , getBoxTextureSet
  , setBoxTextures
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import UI.Types
import UI.Manager.Core (modifyElement)

-- * Box Textures

registerBoxTextures ∷ BoxTextureSet → UIPageManager → (BoxTextureHandle, UIPageManager)
registerBoxTextures texSet mgr =
    let handle = BoxTextureHandle (upmNextBoxTexId mgr)
    in (handle, mgr
          { upmBoxTextures  = Map.insert handle texSet (upmBoxTextures mgr)
          , upmNextBoxTexId = upmNextBoxTexId mgr + 1
          })

getBoxTextureSet ∷ BoxTextureHandle → UIPageManager → Maybe BoxTextureSet
getBoxTextureSet handle mgr = Map.lookup handle (upmBoxTextures mgr)

setBoxTextures ∷ ElementHandle → BoxTextureHandle → UIPageManager → UIPageManager
setBoxTextures handle texHandle = modifyElement handle `flip` \elem →
    case ueRenderData elem of
        RenderBox style → elem { ueRenderData = RenderBox style { ubsTextures = texHandle } }
        _ → elem
