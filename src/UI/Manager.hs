{-# LANGUAGE Strict, UnicodeSyntax #-}
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

import UPrelude
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, isJust, listToMaybe)
import qualified Data.Set as Set
import Data.List (sortOn)
import Engine.Asset.Handle (TextureHandle(..), FontHandle(..))
import UI.Types

-- * Page Operations

createPage ∷ Text → UILayer → UIPageManager → (PageHandle, UIPageManager)
createPage name layer mgr =
    let handle = PageHandle (upmNextPageId mgr)
        page = UIPage
          { upHandle       = handle
          , upName         = name
          , upLayer        = layer
          , upZIndex       = 0
          , upVisible      = False
          , upRootElements = []
          , upFocusedElement = Nothing
          }
    in (handle, mgr
          { upmPages      = Map.insert handle page (upmPages mgr)
          , upmNextPageId = upmNextPageId mgr + 1
          })

deletePage ∷ PageHandle → UIPageManager → UIPageManager
deletePage handle mgr =
    case Map.lookup handle (upmPages mgr) of
        Nothing → mgr
        Just page →
            let mgrWithoutElements = foldr deleteElementTree mgr (upRootElements page)
            in mgrWithoutElements
                { upmPages = Map.delete handle (upmPages mgrWithoutElements)
                , upmVisiblePages = Set.delete handle (upmVisiblePages mgrWithoutElements)
                }

showPage ∷ PageHandle → UIPageManager → UIPageManager
showPage handle mgr =
    case Map.lookup handle (upmPages mgr) of
        Nothing → mgr
        Just page →
            mgr { upmPages = Map.insert handle (page { upVisible = True }) (upmPages mgr)
                , upmVisiblePages = Set.insert handle (upmVisiblePages mgr)
                }

hidePage ∷ PageHandle → UIPageManager → UIPageManager
hidePage handle mgr =
    case Map.lookup handle (upmPages mgr) of
        Nothing → mgr
        Just page →
            mgr { upmPages = Map.insert handle (page { upVisible = False }) (upmPages mgr)
                , upmVisiblePages = Set.delete handle (upmVisiblePages mgr)
                -- Keyboard focus must not survive on a hidden page —
                -- the input thread routes ALL keys to UI-text mode
                -- while upmGlobalFocus is set, so a focused element on
                -- a hidden page would capture the keyboard. The page's
                -- own upFocusedElement memory is intentionally kept.
                , upmGlobalFocus =
                    case upmGlobalFocus mgr of
                        Just fh | Just el ← Map.lookup fh (upmElements mgr)
                                , uePage el ≡ handle → Nothing
                        other → other
                }

getPage ∷ PageHandle → UIPageManager → Maybe UIPage
getPage handle mgr = Map.lookup handle (upmPages mgr)

getVisiblePages ∷ UIPageManager → [UIPage]
getVisiblePages mgr =
    let visibleList = mapMaybe (`Map.lookup` upmPages mgr) 
                              (Set.toList $ upmVisiblePages mgr)
    in sortOn (\p → (upLayer p, upZIndex p)) visibleList

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

deleteElementTree ∷ ElementHandle → UIPageManager → UIPageManager
deleteElementTree handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → mgr
        Just element →
            let mgrWithoutChildren = foldr deleteElementTree mgr (ueChildren element)
            in mgrWithoutChildren
                { upmElements = Map.delete handle (upmElements mgrWithoutChildren)
                , upmHovered = if upmHovered mgrWithoutChildren ≡ Just handle
                               then Nothing
                               else upmHovered mgrWithoutChildren
                -- Same hygiene as upmHovered: a deleted element must
                -- not keep the global focus, or the input thread keeps
                -- routing the whole keyboard to a dead handle.
                , upmGlobalFocus = if upmGlobalFocus mgrWithoutChildren ≡ Just handle
                                   then Nothing
                                   else upmGlobalFocus mgrWithoutChildren
                }

removeElementReference ∷ ElementHandle → UIElement → UIPageManager → UIPageManager
removeElementReference handle element mgr =
    case ueParent element of
        Just parentHandle →
            modifyElement parentHandle mgr $ \parent →
                parent { ueChildren = filter (/= handle) (ueChildren parent) }
        Nothing →
            modifyPage (uePage element) mgr $ \page →
                page { upRootElements = filter (/= handle) (upRootElements page) }

getElement ∷ ElementHandle → UIPageManager → Maybe UIElement
getElement handle mgr = Map.lookup handle (upmElements mgr)

-- * Hierarchy

addElementToPage ∷ PageHandle → ElementHandle → Float → Float 
                 → UIPageManager → UIPageManager
addElementToPage pageHandle elemHandle x y mgr =
    let mgr' = modifyElement elemHandle mgr $ \elem →
            elem { uePosition = (x, y), uePage = pageHandle, ueParent = Nothing }
    in modifyPage pageHandle mgr' $ \page →
            page { upRootElements = upRootElements page ⧺ [elemHandle] }

addChildElement ∷ ElementHandle → ElementHandle → Float → Float
                → UIPageManager → UIPageManager
addChildElement parentHandle childHandle x y mgr =
    case Map.lookup parentHandle (upmElements mgr) of
        Nothing → mgr
        Just parent
            -- Refuse to create a parent cycle (child already an
            -- ancestor of the parent, or child ≡ parent). A cycle
            -- would hang every parent-chain walk (absolute position,
            -- accumulated z-index, tree recursion) on the render and
            -- input threads forever. This is the only site that sets
            -- a Just parent, so the check here keeps the forest
            -- acyclic globally.
            | wouldCycle → mgr
            | otherwise →
                let mgr' = modifyElement childHandle mgr $ \child →
                        child { uePosition = (x, y)
                              , uePage     = uePage parent
                              , ueParent   = Just parentHandle
                              }
                in modifyElement parentHandle mgr' $ \p →
                        p { ueChildren = ueChildren p ⧺ [childHandle] }
  where
    wouldCycle = walkUp (64 ∷ Int) parentHandle
    walkUp depth h
        | depth ≤ 0 = True          -- pathological depth: refuse too
        | h ≡ childHandle = True
        | otherwise = case Map.lookup h (upmElements mgr) ⌦ ueParent of
            Just p  → walkUp (depth - 1) p
            Nothing → False

removeElement ∷ ElementHandle → UIPageManager → UIPageManager
removeElement handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → mgr
        Just element →
            let mgr' = removeElementReference handle element mgr
            -- A detached element is unreachable for rendering and
            -- hit-testing; it must not keep the keyboard either.
            in if upmGlobalFocus mgr' ≡ Just handle
               then mgr' { upmGlobalFocus = Nothing }
               else mgr'

-- * Focus Operations

-- | Set focus to an element (also updates page's remembered focus)
setElementFocus ∷ ElementHandle → UIPageManager → UIPageManager
setElementFocus handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → mgr
        Just elem →
            let pageHandle = uePage elem
                mgr' = mgr { upmGlobalFocus = Just handle }
            in modifyPage pageHandle mgr' $ \page →
                page { upFocusedElement = Just handle }

-- | Clear global focus (page still remembers its last focused element)
clearElementFocus ∷ UIPageManager → UIPageManager
clearElementFocus mgr = mgr { upmGlobalFocus = Nothing }

-- | Get currently focused element globally
getElementFocus ∷ UIPageManager → Maybe ElementHandle
getElementFocus = upmGlobalFocus

-- | Validate the global focus against the live element tree: the
--   focused element must still exist, be visible, and belong to a
--   visible page — otherwise clear it (repair) and report no focus.
--   The input thread routes through this (atomicModifyIORef') as a
--   belt-and-suspenders guard: the destroy/hide paths above maintain
--   the invariant, but a ghost focus would otherwise capture the
--   entire keyboard (all keys route to UI-text mode and are dropped).
validateFocus ∷ UIPageManager → (UIPageManager, Maybe ElementHandle)
validateFocus mgr = case upmGlobalFocus mgr of
    Nothing → (mgr, Nothing)
    Just h →
        let valid = case Map.lookup h (upmElements mgr) of
                Nothing → False
                Just el → ueVisible el
                        ∧ Set.member (uePage el) (upmVisiblePages mgr)
        in if valid
           then (mgr, Just h)
           else (mgr { upmGlobalFocus = Nothing }, Nothing)

-- | Get a page's remembered focused element
getPageFocus ∷ PageHandle → UIPageManager → Maybe ElementHandle
getPageFocus handle mgr = 
    case Map.lookup handle (upmPages mgr) of
        Nothing → Nothing
        Just page → upFocusedElement page

-- | Clear a page's remembered focus
clearPageFocus ∷ PageHandle → UIPageManager → UIPageManager
clearPageFocus handle = modifyPage handle `flip` \page →
    page { upFocusedElement = Nothing }

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

-- * Queries

getElementAbsolutePosition ∷ ElementHandle → UIPageManager → Maybe (Float, Float)
getElementAbsolutePosition handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → Nothing
        Just elem → Just $ computeAbsolutePos (0 ∷ Int) elem
  where
    -- Depth cap: belt-and-suspenders against parent cycles (which
    -- addChildElement refuses to create) — this runs on the render
    -- and input threads, where an unbounded walk would freeze the
    -- engine.
    computeAbsolutePos depth element =
        let (ex, ey) = uePosition element
            (px, py) = case ueParent element of
                _ | depth ≥ 64 → (0, 0)
                Nothing → (0, 0)
                Just parentHandle →
                    case Map.lookup parentHandle (upmElements mgr) of
                        Nothing → (0, 0)
                        Just parent → computeAbsolutePos (depth + 1) parent
        in (px + ex, py + ey)

getPageElements ∷ PageHandle → UIPageManager → [UIElement]
getPageElements pageHandle mgr =
    case Map.lookup pageHandle (upmPages mgr) of
        Nothing → []
        Just page → concatMap collectElements (upRootElements page)
  where
    collectElements handle =
        case Map.lookup handle (upmElements mgr) of
            Nothing → []
            Just elem → elem : concatMap collectElements (ueChildren elem)

getElementChildren ∷ ElementHandle → UIPageManager → [UIElement]
getElementChildren handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → []
        Just elem → mapMaybe (`Map.lookup` upmElements mgr) (ueChildren elem)

-- * Click Detection

-- | Check if a point is inside an element's bounds (in screen coordinates)
isPointInElement ∷ (Float, Float) → UIElement → UIPageManager → Bool
isPointInElement (px, py) element mgr =
    if not (ueVisible element) then False
    else case getElementAbsolutePosition (ueHandle element) mgr of
        Nothing → False
        Just (ex, ey) →
            let (w, h) = ueSize element
            in px ≥ ex ∧ px ≤ (ex + w) &&
               py ≥ ey ∧ py ≤ (ey + h)

-- | Walk every visible element in paint order, yielding the elements
--   that contain the point together with their paint key (page band +
--   accumulated element zIndex — exactly the key 'UI.Render' draws
--   with, see 'uiLayerBand'). All hit-test queries below share this
--   walk, so the element you SEE on top is the element the cursor
--   interacts with. Deliberately does NOT clip children to parent
--   bounds — the renderer doesn't either (dropdown option lists
--   extend past their display box). An invisible element prunes its
--   whole subtree, matching the renderer.
hitsAtPointBy ∷ (UIPage → Bool) → (UIElement → Bool) → (Float, Float)
              → UIPageManager → [(ElementHandle, Int)]
hitsAtPointBy pageOk elemOk pos mgr =
    concatMap perPage (filter pageOk (getVisiblePages mgr))
  where
    perPage page =
        let band = uiLayerBand (upLayer page) (upZIndex page)
        in concatMap (go band) (upRootElements page)
    go base h = case Map.lookup h (upmElements mgr) of
        Nothing → []
        Just el
            | not (ueVisible el) → []
            | otherwise →
                let key  = base + ueZIndex el
                    self = if elemOk el ∧ isPointInElement pos el mgr
                           then [(h, key)]
                           else []
                    kids = sortOn (childZIndex mgr) (ueChildren el)
                in self ⧺ concatMap (go key) kids

childZIndex ∷ UIPageManager → ElementHandle → Int
childZIndex mgr h = maybe 0 ueZIndex (Map.lookup h (upmElements mgr))

-- | The topmost hit: highest paint key wins; at equal keys the
--   later-painted element wins (a later sibling paints over an
--   earlier one), which the fold's @≥@-replacement encodes since
--   'hitsAtPointBy' yields hits in paint order.
topHitBy ∷ (UIPage → Bool) → (UIElement → Bool) → (Float, Float)
         → UIPageManager → Maybe ElementHandle
topHitBy pageOk elemOk pos mgr =
    fst ⊚ foldl' step Nothing (hitsAtPointBy pageOk elemOk pos mgr)
  where
    step acc (h, k) = case acc of
        Just (_, k') | k' > k → acc
        _                     → Just (h, k)

findClickableElementAt ∷ (Float, Float) → UIPageManager → Maybe (ElementHandle, Text)
findClickableElementAt pos mgr = do
    h  ← topHitBy (const True) clickOk pos mgr
    el ← Map.lookup h (upmElements mgr)
    cb ← ueOnClick el
    pure (h, cb)
  where
    -- Elements WITHOUT a click callback don't block clicks: the click
    -- falls through to the topmost element that has one.
    clickOk el = ueClickable el ∧ isJust (ueOnClick el)

-- | Find the nearest ancestor (or self) that has an onClick callback
findClickableAncestor ∷ ElementHandle → UIPageManager → Maybe (ElementHandle, Text)
findClickableAncestor handle mgr = go handle
  where
    go h = case Map.lookup h (upmElements mgr) of
        Nothing → Nothing
        Just elem → case ueOnClick elem of
            Just cb → Just (h, cb)
            Nothing → case ueParent elem of
                Just parentHandle → go parentHandle
                Nothing → Nothing

-- | Find the topmost visible element at a point (paint order: page
--   band + accumulated zIndex, later-painted wins ties).
findElementAt ∷ (Float, Float) → UIPageManager → Maybe ElementHandle
findElementAt = findElementAtExcept Set.empty

-- | Like 'findElementAt', but skips elements that belong to any page
--   in the given ignore set. Used by the tooltip subsystem to avoid
--   hit-testing its own transient page (which would otherwise hijack
--   the hover and create flicker).
findElementAtExcept ∷ Set.Set PageHandle → (Float, Float) → UIPageManager
                    → Maybe ElementHandle
findElementAtExcept ignored pos mgr =
    topHitBy pageOk sized pos mgr
  where
    pageOk p = upHandle p `Set.notMember` ignored
    -- Zero-size elements (pure containers) are not hover targets.
    sized el = let (w, h) = ueSize el in w > 0 ∧ h > 0

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

-- | Set the right-click callback on an element
setElementOnRightClick ∷ ElementHandle → Text → UIPageManager → UIPageManager
setElementOnRightClick handle callbackName = modifyElement handle `flip`
    (\elem → elem { ueOnRightClick = Just callbackName })

-- | Remove an element from its page's root list (without deleting it).
-- This detaches the element so its sprites disappear, but the handle
-- remains valid for potential re-use or deferred GC.
removeFromPage ∷ PageHandle → ElementHandle → UIPageManager → UIPageManager
removeFromPage pageHandle elemHandle mgr =
    let mgr'  = modifyPage pageHandle mgr $ \page →
            page { upRootElements = filter (/= elemHandle) (upRootElements page) }
        mgr'' = modifyElement elemHandle mgr' $ \elem →
            elem { ueParent = Nothing }
    -- Same focus hygiene as removeElement: detached ⇒ no keyboard.
    in if upmGlobalFocus mgr'' ≡ Just elemHandle
       then mgr'' { upmGlobalFocus = Nothing }
       else mgr''

-- | Like findClickableElementAt but looks at ueOnRightClick instead.
findRightClickableElementAt ∷ (Float, Float) → UIPageManager → Maybe (ElementHandle, Text)
findRightClickableElementAt pos mgr = do
    h  ← topHitBy (const True) clickOk pos mgr
    el ← Map.lookup h (upmElements mgr)
    cb ← ueOnRightClick el
    pure (h, cb)
  where
    clickOk el = ueClickable el ∧ isJust (ueOnRightClick el)

-- * Text Buffer Operations

-- | Enable text input on an element (initializes empty buffer)
enableTextInput ∷ ElementHandle → UIPageManager → UIPageManager
enableTextInput handle = modifyElement handle `flip` \elem →
    elem { ueTextBuffer = Just emptyBuffer }

-- | Get an element's text buffer
getTextBuffer ∷ ElementHandle → UIPageManager → Maybe TextBuffer
getTextBuffer handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → Nothing
        Just elem → ueTextBuffer elem

-- | Set an element's text buffer
setTextBuffer ∷ ElementHandle → TextBuffer → UIPageManager → UIPageManager
setTextBuffer handle buffer = modifyElement handle `flip` \elem →
    elem { ueTextBuffer = Just buffer }

-- | Modify an element's text buffer with a function
modifyTextBuffer ∷ ElementHandle → (TextBuffer → TextBuffer) → UIPageManager → UIPageManager
modifyTextBuffer handle f = modifyElement handle `flip` \elem →
    case ueTextBuffer elem of
        Nothing → elem
        Just buf → elem { ueTextBuffer = Just (f buf) }

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

-- * Internal Helpers

modifyElement ∷ ElementHandle → UIPageManager → (UIElement → UIElement) → UIPageManager
modifyElement handle mgr f =
    mgr { upmElements = Map.adjust f handle (upmElements mgr) }

modifyPage ∷ PageHandle → UIPageManager → (UIPage → UIPage) → UIPageManager
modifyPage handle mgr f =
    mgr { upmPages = Map.adjust f handle (upmPages mgr) }
