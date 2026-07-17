{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Internal state-mutation helpers shared by the 'UI.Manager' submodules.
--   Not part of the public API re-exported by 'UI.Manager'.
module UI.Manager.Core
  ( deleteElementTree
  , removeElementReference
  , modifyElement
  , modifyPage
  , bumpRouteEpoch
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

-- | #745 review round 10: bump 'UI.Types.upmRouteEpoch' — called by
--   every route-affecting mutator
--   ('UI.Manager.Property.setElementVisible'/'setElementClickable',
--   'UI.Manager.Hierarchy.removeElement'/'removeFromPage'/
--   'addElementToPage'/'addChildElement', 'UI.Manager.Page.hidePage'/
--   'showPage') so a 'UI.ControlActivation.PendingActivation' captured
--   before the call can no longer match after it — deliberately
--   GLOBAL, not scoped to the mutated element/page: see
--   'UI.Types.upmRouteEpoch' for why a narrower scope misses a
--   separate modal/menu or an ancestor interruption.
bumpRouteEpoch ∷ UIPageManager → UIPageManager
bumpRouteEpoch mgr = mgr { upmRouteEpoch = upmRouteEpoch mgr + 1 }
