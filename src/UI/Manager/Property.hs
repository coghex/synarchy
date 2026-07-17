{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Manager.Property
  ( setElementPosition
  , setElementSize
  , setElementVisible
  , setElementClickable
  , setElementBlocksPointer
  , setElementCapturesScroll
  , setElementDragActivation
  , setElementSteppable
  , setElementTabIndex
  , setElementClipChildren
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
import UI.Manager.Core (modifyElement, bumpElementRouteEpoch)

-- * Property Setters

setElementPosition ∷ ElementHandle → Float → Float → UIPageManager → UIPageManager
setElementPosition handle x y = modifyElement handle `flip`
    (\elem → elem { uePosition = (x, y) })

setElementSize ∷ ElementHandle → Float → Float → UIPageManager → UIPageManager
setElementSize handle w h = modifyElement handle `flip`
    (\elem → elem { ueSize = (w, h) })

-- | #745 review round 12: also bumps this element's OWN
--   'UI.Types.ueRouteEpoch' — a pending pointer activation on this
--   element, or on any of its descendants (checked via their ancestor
--   chain), must not survive a visibility flip even if it's reverted
--   before release; see 'bumpElementRouteEpoch'.
setElementVisible ∷ ElementHandle → Bool → UIPageManager → UIPageManager
setElementVisible handle visible mgr =
    bumpElementRouteEpoch handle $
        modifyElement handle mgr (\elem → elem { ueVisible = visible })

-- | #745 review round 12: also bumps this element's OWN
--   'UI.Types.ueRouteEpoch' — see 'setElementVisible'.
setElementClickable ∷ ElementHandle → Bool → UIPageManager → UIPageManager
setElementClickable handle clickable mgr =
    bumpElementRouteEpoch handle $
        modifyElement handle mgr (\elem → elem { ueClickable = clickable })

-- | #743: explicit opt-in that this element blocks pointer input with
--   no click callback of its own — see 'ueBlocksPointer'.
setElementBlocksPointer ∷ ElementHandle → Bool → UIPageManager → UIPageManager
setElementBlocksPointer handle blocks = modifyElement handle `flip`
    (\elem → elem { ueBlocksPointer = blocks })

-- | #743: explicit opt-in that this element captures wheel/scroll
--   input with no click callback of its own — see 'ueCapturesScroll'.
setElementCapturesScroll ∷ ElementHandle → Bool → UIPageManager → UIPageManager
setElementCapturesScroll handle captures = modifyElement handle `flip`
    (\elem → elem { ueCapturesScroll = captures })

-- | #745: opt this control OUT of the discrete release-activation
--   contract — see 'ueDragActivation'. Only slider knobs and
--   scrollbar thumbs should ever set this.
setElementDragActivation ∷ ElementHandle → Bool → UIPageManager → UIPageManager
setElementDragActivation handle dragActivation = modifyElement handle `flip`
    (\elem → elem { ueDragActivation = dragActivation })

-- | #745: opt this control in to arrow-key stepping while it holds
--   keyboard control focus — see 'ueSteppable'.
setElementSteppable ∷ ElementHandle → Bool → UIPageManager → UIPageManager
setElementSteppable handle steppable = modifyElement handle `flip`
    (\elem → elem { ueSteppable = steppable })

-- | #745: explicit Tab-traversal order — see 'ueTabIndex'.
setElementTabIndex ∷ ElementHandle → Int → UIPageManager → UIPageManager
setElementTabIndex handle idx = modifyElement handle `flip`
    (\elem → elem { ueTabIndex = Just idx })

-- | #747: explicit opt-in that this element clips its DESCENDANTS to
--   its own current bounds — see 'ueClipChildren'.
setElementClipChildren ∷ ElementHandle → Bool → UIPageManager → UIPageManager
setElementClipChildren handle clips = modifyElement handle `flip`
    (\elem → elem { ueClipChildren = clips })

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
