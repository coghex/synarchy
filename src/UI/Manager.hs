{-# LANGUAGE Strict #-}
module UI.Manager
  ( -- Page operations
    createPage
  , deletePage
  , showPage
  , hidePage
  , getPage
  , getVisiblePages
    -- Element operations
  , createBox
  , createText
  , createSprite
  , createElement
  , deleteElement
  , getElement
    -- Hierarchy
  , addElementToPage
  , addChildElement
  , removeElement
    -- Property setters
  , setElementPosition
  , setElementSize
  , setElementVisible
  , setElementClickable
  , setElementZIndex
  , setElementOnClick
  , isPointInElement
  , findClickableElementAt
  , setBoxColor
  , setText
  , setTextColor
  , setSpriteTexture
  , setSpriteColor
    -- Queries
  , getElementAbsolutePosition
  , getPageElements
  , getElementChildren
    -- Box textures
  , registerBoxTextures
  , getBoxTextureSet
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, isJust, listToMaybe)
import qualified Data.Set as Set
import Data.List (sortOn)
import Engine.Asset.Handle (TextureHandle(..), FontHandle(..))
import UI.Types

-----------------------------------------------------------
-- Page Operations
-----------------------------------------------------------

createPage :: Text -> UILayer -> UIPageManager -> (PageHandle, UIPageManager)
createPage name layer mgr =
    let handle = PageHandle (upmNextPageId mgr)
        page = UIPage
          { upHandle       = handle
          , upName         = name
          , upLayer        = layer
          , upZIndex       = 0
          , upVisible      = False
          , upRootElements = []
          }
    in (handle, mgr
          { upmPages      = Map.insert handle page (upmPages mgr)
          , upmNextPageId = upmNextPageId mgr + 1
          })

deletePage :: PageHandle -> UIPageManager -> UIPageManager
deletePage handle mgr =
    case Map.lookup handle (upmPages mgr) of
        Nothing -> mgr
        Just page ->
            let mgrWithoutElements = foldr deleteElementTree mgr (upRootElements page)
            in mgrWithoutElements
                { upmPages = Map.delete handle (upmPages mgrWithoutElements)
                , upmVisiblePages = Set.delete handle (upmVisiblePages mgrWithoutElements)
                }

showPage :: PageHandle -> UIPageManager -> UIPageManager
showPage handle mgr =
    case Map.lookup handle (upmPages mgr) of
        Nothing -> mgr
        Just page ->
            mgr { upmPages = Map.insert handle (page { upVisible = True }) (upmPages mgr)
                , upmVisiblePages = Set.insert handle (upmVisiblePages mgr)
                }

hidePage :: PageHandle -> UIPageManager -> UIPageManager
hidePage handle mgr =
    case Map.lookup handle (upmPages mgr) of
        Nothing -> mgr
        Just page ->
            mgr { upmPages = Map.insert handle (page { upVisible = False }) (upmPages mgr)
                , upmVisiblePages = Set.delete handle (upmVisiblePages mgr)
                }

getPage :: PageHandle -> UIPageManager -> Maybe UIPage
getPage handle mgr = Map.lookup handle (upmPages mgr)

getVisiblePages :: UIPageManager -> [UIPage]
getVisiblePages mgr =
    let visibleList = mapMaybe (`Map.lookup` upmPages mgr) 
                              (Set.toList $ upmVisiblePages mgr)
    in sortOn (\p -> (upLayer p, upZIndex p)) visibleList

-----------------------------------------------------------
-- Element Creation
-----------------------------------------------------------

createElement :: Text -> Float -> Float -> PageHandle -> UIPageManager 
              -> (ElementHandle, UIPageManager)
createElement name width height pageHandle mgr =
    createElementInternal name width height pageHandle RenderNone mgr

createBox :: Text -> Float -> Float -> BoxTextureHandle -> Float 
          -> (Float, Float, Float, Float) -> PageHandle 
          -> UIPageManager -> (ElementHandle, UIPageManager)
createBox name width height texHandle tileSize color pageHandle mgr =
    let style = UIBoxStyle
          { ubsTextures = texHandle
          , ubsTileSize = tileSize
          , ubsColor    = color
          }
    in createElementInternal name width height pageHandle (RenderBox style) mgr

createText :: Text -> Text -> FontHandle -> (Float, Float, Float, Float) -> PageHandle 
           -> UIPageManager -> (ElementHandle, UIPageManager)
createText name text font color pageHandle mgr =
    createElementInternal name 0 0 pageHandle 
        (RenderText (UITextStyle text font color)) mgr

createSprite :: Text -> Float -> Float -> TextureHandle -> (Float, Float, Float, Float) 
             -> PageHandle -> UIPageManager -> (ElementHandle, UIPageManager)
createSprite name width height texture color pageHandle mgr =
    createElementInternal name width height pageHandle 
        (RenderSprite (UISpriteStyle texture color)) mgr

createElementInternal :: Text -> Float -> Float -> PageHandle -> UIRenderData 
                      -> UIPageManager -> (ElementHandle, UIPageManager)
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
          }
    in (handle, mgr
          { upmElements   = Map.insert handle element (upmElements mgr)
          , upmNextElemId = upmNextElemId mgr + 1
          })

deleteElement :: ElementHandle -> UIPageManager -> UIPageManager
deleteElement handle mgr = 
    case Map.lookup handle (upmElements mgr) of
        Nothing -> mgr
        Just element ->
            let mgrWithoutRef = removeElementReference handle element mgr
            in deleteElementTree handle mgrWithoutRef

deleteElementTree :: ElementHandle -> UIPageManager -> UIPageManager
deleteElementTree handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing -> mgr
        Just element ->
            let mgrWithoutChildren = foldr deleteElementTree mgr (ueChildren element)
            in mgrWithoutChildren
                { upmElements = Map.delete handle (upmElements mgrWithoutChildren)
                , upmHovered = if upmHovered mgrWithoutChildren == Just handle 
                               then Nothing 
                               else upmHovered mgrWithoutChildren
                }

removeElementReference :: ElementHandle -> UIElement -> UIPageManager -> UIPageManager
removeElementReference handle element mgr =
    case ueParent element of
        Just parentHandle ->
            modifyElement parentHandle mgr $ \parent ->
                parent { ueChildren = filter (/= handle) (ueChildren parent) }
        Nothing ->
            modifyPage (uePage element) mgr $ \page ->
                page { upRootElements = filter (/= handle) (upRootElements page) }

getElement :: ElementHandle -> UIPageManager -> Maybe UIElement
getElement handle mgr = Map.lookup handle (upmElements mgr)

-----------------------------------------------------------
-- Hierarchy
-----------------------------------------------------------

addElementToPage :: PageHandle -> ElementHandle -> Float -> Float 
                 -> UIPageManager -> UIPageManager
addElementToPage pageHandle elemHandle x y mgr =
    let mgr' = modifyElement elemHandle mgr $ \elem ->
            elem { uePosition = (x, y), uePage = pageHandle, ueParent = Nothing }
    in modifyPage pageHandle mgr' $ \page ->
            page { upRootElements = upRootElements page ++ [elemHandle] }

addChildElement :: ElementHandle -> ElementHandle -> Float -> Float 
                -> UIPageManager -> UIPageManager
addChildElement parentHandle childHandle x y mgr =
    case Map.lookup parentHandle (upmElements mgr) of
        Nothing -> mgr
        Just parent ->
            let mgr' = modifyElement childHandle mgr $ \child ->
                    child { uePosition = (x, y)
                          , uePage     = uePage parent
                          , ueParent   = Just parentHandle 
                          }
            in modifyElement parentHandle mgr' $ \p ->
                    p { ueChildren = ueChildren p ++ [childHandle] }

removeElement :: ElementHandle -> UIPageManager -> UIPageManager
removeElement handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing -> mgr
        Just element -> removeElementReference handle element mgr

-----------------------------------------------------------
-- Property Setters
-----------------------------------------------------------

setElementPosition :: ElementHandle -> Float -> Float -> UIPageManager -> UIPageManager
setElementPosition handle x y = modifyElement handle `flip` 
    (\elem -> elem { uePosition = (x, y) })

setElementSize :: ElementHandle -> Float -> Float -> UIPageManager -> UIPageManager
setElementSize handle w h = modifyElement handle `flip` 
    (\elem -> elem { ueSize = (w, h) })

setElementVisible :: ElementHandle -> Bool -> UIPageManager -> UIPageManager
setElementVisible handle visible = modifyElement handle `flip` 
    (\elem -> elem { ueVisible = visible })

setElementClickable :: ElementHandle -> Bool -> UIPageManager -> UIPageManager
setElementClickable handle clickable = modifyElement handle `flip` 
    (\elem -> elem { ueClickable = clickable })

setElementOnClick :: ElementHandle -> Text -> UIPageManager -> UIPageManager
setElementOnClick handle callbackName = modifyElement handle `flip` 
    (\elem -> elem { ueOnClick = Just callbackName })

setElementZIndex :: ElementHandle -> Int -> UIPageManager -> UIPageManager
setElementZIndex handle z = modifyElement handle `flip` 
    (\elem -> elem { ueZIndex = z })

setBoxColor :: ElementHandle -> (Float, Float, Float, Float) -> UIPageManager -> UIPageManager
setBoxColor handle color = modifyElement handle `flip` \elem ->
    case ueRenderData elem of
        RenderBox style -> elem { ueRenderData = RenderBox style { ubsColor = color } }
        _ -> elem

setText :: ElementHandle -> Text -> UIPageManager -> UIPageManager
setText handle text = modifyElement handle `flip` \elem ->
    case ueRenderData elem of
        RenderText style -> elem { ueRenderData = RenderText style { utsText = text } }
        _ -> elem

setTextColor :: ElementHandle -> (Float, Float, Float, Float) -> UIPageManager -> UIPageManager
setTextColor handle color = modifyElement handle `flip` \elem ->
    case ueRenderData elem of
        RenderText style -> elem { ueRenderData = RenderText style { utsColor = color } }
        _ -> elem

setSpriteTexture :: ElementHandle -> TextureHandle -> UIPageManager -> UIPageManager
setSpriteTexture handle texture = modifyElement handle `flip` \elem ->
    case ueRenderData elem of
        RenderSprite style -> elem { ueRenderData = RenderSprite style { ussTexture = texture } }
        _ -> elem

setSpriteColor :: ElementHandle -> (Float, Float, Float, Float) -> UIPageManager -> UIPageManager
setSpriteColor handle color = modifyElement handle `flip` \elem ->
    case ueRenderData elem of
        RenderSprite style -> elem { ueRenderData = RenderSprite style { ussColor = color } }
        _ -> elem

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

getElementAbsolutePosition :: ElementHandle -> UIPageManager -> Maybe (Float, Float)
getElementAbsolutePosition handle mgr = 
    case Map.lookup handle (upmElements mgr) of
        Nothing -> Nothing
        Just elem -> Just $ computeAbsolutePos elem
  where
    computeAbsolutePos element =
        let (ex, ey) = uePosition element
            (px, py) = case ueParent element of
                Nothing -> (0, 0)
                Just parentHandle -> 
                    case Map.lookup parentHandle (upmElements mgr) of
                        Nothing -> (0, 0)
                        Just parent -> computeAbsolutePos parent
        in (px + ex, py + ey)

getPageElements :: PageHandle -> UIPageManager -> [UIElement]
getPageElements pageHandle mgr =
    case Map.lookup pageHandle (upmPages mgr) of
        Nothing -> []
        Just page -> concatMap collectElements (upRootElements page)
  where
    collectElements handle =
        case Map.lookup handle (upmElements mgr) of
            Nothing -> []
            Just elem -> elem : concatMap collectElements (ueChildren elem)

getElementChildren :: ElementHandle -> UIPageManager -> [UIElement]
getElementChildren handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing -> []
        Just elem -> mapMaybe (`Map.lookup` upmElements mgr) (ueChildren elem)

-----------------------------------------------------------
-- Click Detection
-----------------------------------------------------------

-- | Check if a point is inside an element's bounds (in screen coordinates)
isPointInElement :: (Float, Float) -> UIElement -> UIPageManager -> Bool
isPointInElement (px, py) element mgr =
    if not (ueVisible element) then False
    else case getElementAbsolutePosition (ueHandle element) mgr of
        Nothing -> False
        Just (ex, ey) ->
            let (w, h) = ueSize element
            in px >= ex && px <= (ex + w) &&
               py >= ey && py <= (ey + h)

-- Replace the findClickableElementAt function:

findClickableElementAt :: (Float, Float) -> UIPageManager -> Maybe (ElementHandle, Text)
findClickableElementAt pos mgr =
    let visiblePages = Set.toList (upmVisiblePages mgr)
        -- Get only ROOT elements from visible pages (not children)
        allRootHandles = concatMap (\ph -> 
            case Map.lookup ph (upmPages mgr) of
                Just page -> upRootElements page
                Nothing -> []
            ) visiblePages
        
        -- For each root, check if point hits it OR any of its children
        clickableRoots = filter (\rootHandle ->
            case Map.lookup rootHandle (upmElements mgr) of
                Nothing -> False
                Just rootElem -> 
                    ueClickable rootElem && 
                    ueVisible rootElem &&
                    isJust (ueOnClick rootElem) &&
                    pointInElementTree pos rootHandle mgr
            ) allRootHandles
        
        -- Sort by layer and zIndex (highest first)
        -- FIXED: Properly combine page layer, page z-index, and element z-index
        sorted = sortOn (\eh -> 
            case Map.lookup eh (upmElements mgr) of
                Just elem -> 
                    let page = Map.lookup (uePage elem) (upmPages mgr)
                        pageLayerVal = case page of
                            Just p -> fromEnum (upLayer p) * 1000000  -- Layer is most significant
                            Nothing -> 0
                        pageZVal = case page of
                            Just p -> upZIndex p * 1000  -- Page z-index is next
                            Nothing -> 0
                        elemZVal = ueZIndex elem  -- Element z-index is least significant
                        totalVal = pageLayerVal + pageZVal + elemZVal
                    in negate totalVal  -- Negate for descending sort (highest first)
                Nothing -> 0
            ) clickableRoots
    in case sorted of
        (eh:_) -> case Map.lookup eh (upmElements mgr) of
            Just elem -> case ueOnClick elem of
                Just cb -> Just (eh, cb)
                Nothing -> Nothing
            Nothing -> Nothing
        [] -> Nothing
  where
    -- Check if point is in element or any of its children
    pointInElementTree :: (Float, Float) -> ElementHandle -> UIPageManager -> Bool
    pointInElementTree point handle mgr' =
        case Map.lookup handle (upmElements mgr') of
            Nothing -> False
            Just elem ->
                if isPointInElement point elem mgr'
                then True
                else any (\childH -> pointInElementTree point childH mgr') (ueChildren elem)

-----------------------------------------------------------
-- Box Textures
-----------------------------------------------------------

registerBoxTextures :: BoxTextureSet -> UIPageManager -> (BoxTextureHandle, UIPageManager)
registerBoxTextures texSet mgr =
    let handle = BoxTextureHandle (upmNextBoxTexId mgr)
    in (handle, mgr
          { upmBoxTextures  = Map.insert handle texSet (upmBoxTextures mgr)
          , upmNextBoxTexId = upmNextBoxTexId mgr + 1
          })

getBoxTextureSet :: BoxTextureHandle -> UIPageManager -> Maybe BoxTextureSet
getBoxTextureSet handle mgr = Map.lookup handle (upmBoxTextures mgr)

-----------------------------------------------------------
-- Internal Helpers
-----------------------------------------------------------

modifyElement :: ElementHandle -> UIPageManager -> (UIElement -> UIElement) -> UIPageManager
modifyElement handle mgr f =
    mgr { upmElements = Map.adjust f handle (upmElements mgr) }

modifyPage :: PageHandle -> UIPageManager -> (UIPage -> UIPage) -> UIPageManager
modifyPage handle mgr f =
    mgr { upmPages = Map.adjust f handle (upmPages mgr) }
