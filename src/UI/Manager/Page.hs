{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Manager.Page
  ( createPage
  , deletePage
  , showPage
  , hidePage
  , getPage
  , getVisiblePages
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.List (sortOn)
import UI.Types
import UI.Manager.Core (deleteElementTree)

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
