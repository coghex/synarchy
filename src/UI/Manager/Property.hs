{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Manager.Property
  ( setElementPosition
  , setElementSize
  , setElementVisible
  , setElementClickable
  , setElementZIndex
  , setElementOnClick
  , setElementOnRightClick
  , setBoxColor
  , setText
  , setTextColor
  , setSpriteTexture
  , setSpriteColor
  , setElementTooltip
  , clearElementTooltip
  , getElementTooltip
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Engine.Asset.Handle (TextureHandle(..))
import UI.Types
import UI.Manager.Core (modifyElement)

-- * Property Setters

setElementPosition ∷ ElementHandle → Float → Float → UIPageManager → UIPageManager
setElementPosition handle x y = modifyElement handle `flip`
    (\elem → elem { uePosition = (x, y) })

setElementSize ∷ ElementHandle → Float → Float → UIPageManager → UIPageManager
setElementSize handle w h = modifyElement handle `flip`
    (\elem → elem { ueSize = (w, h) })

setElementVisible ∷ ElementHandle → Bool → UIPageManager → UIPageManager
setElementVisible handle visible = modifyElement handle `flip`
    (\elem → elem { ueVisible = visible })

setElementClickable ∷ ElementHandle → Bool → UIPageManager → UIPageManager
setElementClickable handle clickable = modifyElement handle `flip`
    (\elem → elem { ueClickable = clickable })

setElementOnClick ∷ ElementHandle → Text → UIPageManager → UIPageManager
setElementOnClick handle callbackName = modifyElement handle `flip`
    (\elem → elem { ueOnClick = Just callbackName })

setElementZIndex ∷ ElementHandle → Int → UIPageManager → UIPageManager
setElementZIndex handle z = modifyElement handle `flip`
    (\elem → elem { ueZIndex = z })

setBoxColor ∷ ElementHandle → (Float, Float, Float, Float) → UIPageManager → UIPageManager
setBoxColor handle color = modifyElement handle `flip` \elem →
    case ueRenderData elem of
        RenderBox style → elem { ueRenderData = RenderBox style { ubsColor = color } }
        _ → elem

setText ∷ ElementHandle → Text → UIPageManager → UIPageManager
setText handle text = modifyElement handle `flip` \elem →
    case ueRenderData elem of
        RenderText style → elem { ueRenderData = RenderText style { utsText = text } }
        _ → elem

setTextColor ∷ ElementHandle → (Float, Float, Float, Float) → UIPageManager → UIPageManager
setTextColor handle color = modifyElement handle `flip` \elem →
    case ueRenderData elem of
        RenderText style → elem { ueRenderData = RenderText style { utsColor = color } }
        _ → elem

setSpriteTexture ∷ ElementHandle → TextureHandle → UIPageManager → UIPageManager
setSpriteTexture handle texture = modifyElement handle `flip` \elem →
    case ueRenderData elem of
        RenderSprite style → elem { ueRenderData = RenderSprite style { ussTexture = texture } }
        _ → elem

setSpriteColor ∷ ElementHandle → (Float, Float, Float, Float) → UIPageManager → UIPageManager
setSpriteColor handle color = modifyElement handle `flip` \elem →
    case ueRenderData elem of
        RenderSprite style → elem { ueRenderData = RenderSprite style { ussColor = color } }
        _ → elem

-- | Set the right-click callback on an element
setElementOnRightClick ∷ ElementHandle → Text → UIPageManager → UIPageManager
setElementOnRightClick handle callbackName = modifyElement handle `flip`
    (\elem → elem { ueOnRightClick = Just callbackName })

-- * Tooltips

setElementTooltip ∷ ElementHandle → TooltipContent → UIPageManager → UIPageManager
setElementTooltip handle content = modifyElement handle `flip`
    (\elem → elem { ueTooltip = Just content })

clearElementTooltip ∷ ElementHandle → UIPageManager → UIPageManager
clearElementTooltip handle = modifyElement handle `flip`
    (\elem → elem { ueTooltip = Nothing })

getElementTooltip ∷ ElementHandle → UIPageManager → Maybe TooltipContent
getElementTooltip handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → Nothing
        Just elem → ueTooltip elem
