{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Manager.Element
  ( createElement
  , createBox
  , createText
  , createSprite
  , deleteElement
  , getElement
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..), FontHandle(..))
import UI.Types
import UI.Manager.Core (deleteElementTree, removeElementReference)

-- * Element Creation

createElement ∷ Text → Float → Float → PageHandle → UIPageManager
              → (ElementHandle, UIPageManager)
createElement name width height pageHandle mgr =
    createElementInternal name width height pageHandle RenderNone mgr

createBox ∷ Text → Float → Float → BoxTextureHandle → Float
          → (Float, Float, Float, Float) → Float → PageHandle
          → UIPageManager → (ElementHandle, UIPageManager)
createBox name width height texHandle tileSize color overflow pageHandle mgr =
    let style = UIBoxStyle
          { ubsTextures = texHandle
          , ubsTileSize = tileSize
          , ubsColor    = color
          , ubsOverflow = overflow
          }
    in createElementInternal name width height pageHandle (RenderBox style) mgr

createText ∷ Text → Text → FontHandle → Float → (Float, Float, Float, Float) → PageHandle
           → UIPageManager → (ElementHandle, UIPageManager)
createText name text font size color pageHandle mgr =
    createElementInternal name 0 0 pageHandle
        (RenderText (UITextStyle text font size color)) mgr

createSprite ∷ Text → Float → Float → TextureHandle → (Float, Float, Float, Float)
             → PageHandle → UIPageManager → (ElementHandle, UIPageManager)
createSprite name width height texture color pageHandle mgr =
    createElementInternal name width height pageHandle
        (RenderSprite (UISpriteStyle texture color)) mgr

createElementInternal ∷ Text → Float → Float → PageHandle → UIRenderData
                      → UIPageManager → (ElementHandle, UIPageManager)
createElementInternal name width height pageHandle renderData mgr =
    let handle = ElementHandle (upmNextElemId mgr)
        element = UIElement
          { ueHandle     = handle
          , uePage       = pageHandle
          , ueParent     = Nothing
          , ueName       = name
          , uePosition   = (0, 0)
          , ueSize       = (width, height)
          , ueZIndex     = 0
          , ueVisible    = True
          , ueClickable  = False
          , ueChildren   = []
          , ueRenderData = renderData
          , ueOnClick    = Nothing
          , ueOnRightClick = Nothing
          , ueBlocksPointer = False
          , ueCapturesScroll = False
          , ueDragActivation = False
          , ueRouteEpoch  = 0
          , ueSteppable   = False
          , ueTabIndex    = Nothing
          , ueClipChildren = False
          , ueTextBuffer  = Nothing
          , ueTooltip     = Nothing
          }
    in (handle, mgr
          { upmElements   = Map.insert handle element (upmElements mgr)
          , upmNextElemId = upmNextElemId mgr + 1
          })

deleteElement ∷ ElementHandle → UIPageManager → UIPageManager
deleteElement handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → mgr
        Just element →
            let mgrWithoutRef = removeElementReference handle element mgr
            in deleteElementTree handle mgrWithoutRef

getElement ∷ ElementHandle → UIPageManager → Maybe UIElement
getElement handle mgr = Map.lookup handle (upmElements mgr)
