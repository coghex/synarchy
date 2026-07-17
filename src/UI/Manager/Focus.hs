{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Manager.Focus
  ( setElementFocus
  , clearElementFocus
  , getElementFocus
  , getPageFocus
  , clearPageFocus
  , validateFocus
    -- * Control focus (#745)
  , setControlFocus
  , clearControlFocus
  , getControlFocus
  , validateControlFocusIn
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import UI.Types
import UI.Manager.Core (modifyPage)
import UI.FocusNavigation (validateControlFocus)

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

-- * Control focus (#745) — keyboard focus for non-text controls,
--   independent of the text-input focus above. See 'UI.FocusNavigation'
--   for the traversal decisions; these are the thin manager-level
--   read/write operations 'Engine.Input.Thread.Keyboard' drives.

-- | Set keyboard control focus to an element, unconditionally (callers
--   — Tab/Shift+Tab traversal — only ever pass an already-eligible
--   handle from 'UI.FocusNavigation.focusableElements').
setControlFocus ∷ ElementHandle → UIPageManager → UIPageManager
setControlFocus handle mgr = mgr { upmControlFocus = Just handle }

clearControlFocus ∷ UIPageManager → UIPageManager
clearControlFocus mgr = mgr { upmControlFocus = Nothing }

getControlFocus ∷ UIPageManager → Maybe ElementHandle
getControlFocus = upmControlFocus

-- | Validate the current control focus against the live element tree
--   — mirrors 'validateFocus' exactly, just for 'upmControlFocus'
--   instead of 'upmGlobalFocus'. The input thread routes every
--   keyboard dispatch through this the same way it already does for
--   text focus, so a hidden/deleted/disabled/detached focused control
--   never keeps capturing Enter/Space/arrow keys.
validateControlFocusIn ∷ UIPageManager → (UIPageManager, Maybe ElementHandle)
validateControlFocusIn mgr =
    let valid = validateControlFocus mgr (upmControlFocus mgr)
    in (mgr { upmControlFocus = valid }, valid)

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
