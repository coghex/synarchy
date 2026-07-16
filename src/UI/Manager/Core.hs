{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Internal state-mutation helpers shared by the 'UI.Manager' submodules.
--   Not part of the public API re-exported by 'UI.Manager'.
module UI.Manager.Core
  ( deleteElementTree
  , removeElementReference
  , modifyElement
  , modifyPage
  , bumpActivationEpoch
  , bumpPageActivationEpoch
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import UI.Types

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
                -- #745 review round 3: same hygiene for keyboard
                -- CONTROL focus.
                , upmControlFocus = if upmControlFocus mgrWithoutChildren ≡ Just handle
                                    then Nothing
                                    else upmControlFocus mgrWithoutChildren
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

-- * Internal Helpers

modifyElement ∷ ElementHandle → UIPageManager → (UIElement → UIElement) → UIPageManager
modifyElement handle mgr f =
    mgr { upmElements = Map.adjust f handle (upmElements mgr) }

modifyPage ∷ PageHandle → UIPageManager → (UIPage → UIPage) → UIPageManager
modifyPage handle mgr f =
    mgr { upmPages = Map.adjust f handle (upmPages mgr) }

-- | #745 review round 9: bump a single element's 'ueActivationEpoch'
--   — called by every route-affecting per-element mutator
--   ('UI.Manager.Property.setElementVisible'/'setElementClickable',
--   'UI.Manager.Hierarchy.removeElement'/'removeFromPage'/
--   'addElementToPage'/'addChildElement') so a
--   'UI.ControlActivation.PendingActivation' captured before the call
--   can no longer match after it.
bumpActivationEpoch ∷ ElementHandle → UIPageManager → UIPageManager
bumpActivationEpoch handle mgr =
    modifyElement handle mgr $ \el → el { ueActivationEpoch = ueActivationEpoch el + 1 }

-- | #745 review round 9: bump 'ueActivationEpoch' for every element
--   belonging to one page — 'UI.Manager.Page.hidePage'/'showPage'
--   call this so a pending activation on any control that page owns
--   cancels safely even if the page comes back before release ("a
--   modal appearing on top" / "changing menus" per the #745 issue
--   text, not just per-element hide/disable/detach).
bumpPageActivationEpoch ∷ PageHandle → UIPageManager → UIPageManager
bumpPageActivationEpoch pageHandle mgr =
    mgr { upmElements = Map.map bump (upmElements mgr) }
  where
    bump el
        | uePage el ≡ pageHandle = el { ueActivationEpoch = ueActivationEpoch el + 1 }
        | otherwise = el
