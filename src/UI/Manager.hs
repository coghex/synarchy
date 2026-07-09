{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | UI page/element manager: umbrella re-export of the 'UI.Manager.*'
--   submodules (page/element/hierarchy/focus/property/query/text/box
--   texture operations). Split out of a single large module for
--   ownership/review reasons; the public API here is unchanged.
module UI.Manager
  ( -- * Page Operations
    createPage
  , deletePage
  , showPage
  , hidePage
  , getPage
  , getVisiblePages
    -- * Element Creation
  , createBox
  , createText
  , createSprite
  , createElement
  , deleteElement
  , getElement
    -- * Hierarchy
  , addElementToPage
  , addChildElement
  , removeElement
    -- * Focus Operations
  , setElementFocus
  , clearElementFocus
  , getElementFocus
  , getPageFocus
  , clearPageFocus
  , validateFocus
    -- * Text Buffer Operations
  , enableTextInput
  , getTextBuffer
  , setTextBuffer
  , modifyTextBuffer
    -- * Property Setters
  , setElementPosition
  , setElementSize
  , setElementVisible
  , setElementClickable
  , setElementZIndex
  , setElementOnClick
  , setElementOnRightClick
  , isPointInElement
  , findClickableElementAt
  , findRightClickableElementAt
  , findClickableAncestor
  , setBoxColor
  , setText
  , setTextColor
  , setSpriteTexture
  , setSpriteColor
    -- * Queries
  , getElementAbsolutePosition
  , getPageElements
  , removeFromPage
  , getElementChildren
  , findElementAt
  , findElementAtExcept
    -- * Tooltips
  , setElementTooltip
  , clearElementTooltip
  , getElementTooltip
    -- * Box Textures
  , registerBoxTextures
  , getBoxTextureSet
  , setBoxTextures
  ) where

import UI.Manager.Page
import UI.Manager.Element
import UI.Manager.Hierarchy
import UI.Manager.Focus
import UI.Manager.Property
import UI.Manager.Query
import UI.Manager.Text
import UI.Manager.BoxTexture
