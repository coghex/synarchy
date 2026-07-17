{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Internal state-mutation helpers shared by the 'UI.Manager' submodules.
--   Not part of the public API re-exported by 'UI.Manager'.
module UI.Manager.Core
  ( deleteElementTree
  , removeElementReference
  , modifyElement
  , modifyPage
  , bumpElementRouteEpoch
  , bumpPageEpoch
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

-- | #745 review round 12: bump ONE element's 'UI.Types.ueRouteEpoch'
--   — called by 'UI.Manager.Property.setElementVisible'/
--   'setElementClickable' and 'UI.Manager.Hierarchy.removeElement'/
--   'removeFromPage' (detach only — see 'UI.Types.ueRouteEpoch' for
--   why (re)attach must not bump anything). Deliberately scoped to
--   THIS element only, not global: 'UI.ControlActivation.
--   resolveActivation' walks the pressed element's ancestor chain and
--   compares each ancestor's own epoch, so an unrelated element's
--   mutation is invisible to a pending activation it was never on the
--   route of.
bumpElementRouteEpoch ∷ ElementHandle → UIPageManager → UIPageManager
bumpElementRouteEpoch handle mgr =
    modifyElement handle mgr $ \el → el { ueRouteEpoch = ueRouteEpoch el + 1 }

-- | #745 review round 12: bump the manager-wide 'UI.Types.upmPageEpoch'
--   — called by 'UI.Manager.Page.hidePage'/'showPage' for ANY page.
--   Deliberately GLOBAL (unlike 'bumpElementRouteEpoch'): see
--   'UI.Types.upmPageEpoch' for why page-level visibility needs to
--   invalidate every pending activation regardless of which page.
bumpPageEpoch ∷ UIPageManager → UIPageManager
bumpPageEpoch mgr = mgr { upmPageEpoch = upmPageEpoch mgr + 1 }
