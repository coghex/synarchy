{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Manager.Page
  ( createPage
  , deletePage
  , showPage
  , hidePage
  , getPage
  , getVisiblePages
  , setPageInputExclusive
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
          -- #742: a modal-layer page is a real input boundary by
          -- default; every other layer defaults pass-through. Callers
          -- that want a modal-layer page to stay pass-through (e.g.
          -- popup.lua's notification stack) opt out explicitly via
          -- 'setPageInputExclusive'.
          , upInputExclusive = layer ≡ LayerModal
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
                -- #745 review round 3: keyboard CONTROL focus needs
                -- the exact same hide-time hygiene as TEXT focus above
                -- — otherwise a control focused before its page is
                -- hidden sits stale in upmControlFocus (unnoticed
                -- until the next keyboard dispatch's lazy validation)
                -- and, if the page is shown again before any key
                -- reaches that validation, reads as still-focused with
                -- no intervening "clear" ever having been observed.
                , upmControlFocus =
                    case upmControlFocus mgr of
                        Just fh | Just el ← Map.lookup fh (upmElements mgr)
                                , uePage el ≡ handle → Nothing
                        other → other
                }

getPage ∷ PageHandle → UIPageManager → Maybe UIPage
getPage handle mgr = Map.lookup handle (upmPages mgr)

-- | Override a page's default input-exclusivity (#742). See
--   'UI.Types.upInputExclusive'.
setPageInputExclusive ∷ PageHandle → Bool → UIPageManager → UIPageManager
setPageInputExclusive handle exclusive mgr =
    case Map.lookup handle (upmPages mgr) of
        Nothing → mgr
        Just page →
            mgr { upmPages = Map.insert handle
                    (page { upInputExclusive = exclusive }) (upmPages mgr) }

getVisiblePages ∷ UIPageManager → [UIPage]
getVisiblePages mgr =
    let visibleList = mapMaybe (`Map.lookup` upmPages mgr)
                              (Set.toList $ upmVisiblePages mgr)
    in sortOn (\p → (upLayer p, upZIndex p)) visibleList
