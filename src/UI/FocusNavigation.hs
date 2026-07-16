{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Keyboard CONTROL focus and Tab/Shift+Tab traversal (#745, Phase B
--   child B1 of #741) — distinct from the pre-existing TEXT-input
--   focus ('UI.Manager.Focus.setElementFocus'/'upmGlobalFocus'), which
--   this module never touches. A non-text control (button, checkbox,
--   toggle, tab, list row, dropdown, scroll arrow, hand-built menu
--   control) can hold keyboard focus so Enter/Space can activate it
--   and, for a steppable control (a slider — 'UI.Types.ueSteppable'),
--   arrow keys can adjust it, all without a pointer.
--
--   This module is pure — no Vulkan, window, or Lua — so traversal is
--   Hspec-testable in isolation; see @Test.Headless.UI.FocusNavigation@.
--   It reuses Phase A machinery for eligibility and scope rather than
--   re-deriving them: 'UI.InputOwnership.pagesInScope' (the modal
--   boundary) so a visible modal traps traversal exactly like it traps
--   pointer routing, while 'LayerDebug' stays reachable above it (that
--   layer always paints above any modal — see 'UI.Types.uiLayerBand');
--   and 'UI.Manager.Query.paintTraversalOrder' (the same walk
--   'UI.Manager.Query.topHitBy' resolves hits with) for the default,
--   stable page-local order that follows visual/read order.
module UI.FocusNavigation
  ( focusableElements
  , nextFocus
  , prevFocus
  , validateControlFocus
  , isEligibleControl
  ) where

import UPrelude
import Data.List (sortOn, elemIndex)
import qualified Data.Map.Strict as Map
import UI.Types
import UI.InputOwnership (isPageInScope)
import UI.Manager.Query (paintTraversalOrder)

-- | True for a non-text, visible, clickable, primary-actionable
--   control on a page currently in scope for input — the same
--   ingredients 'UI.Manager.Query.findClickableElementAt' requires to
--   fire a click, plus the modal-scope and text-exclusion rules B1
--   adds. A text-input field (`ueTextBuffer` set) is deliberately
--   excluded: it keeps using the pre-existing text-focus system, never
--   this one.
eligibleControl ∷ UIPageManager → UIElement → Bool
eligibleControl mgr el =
    ueVisible el
    ∧ ueClickable el
    ∧ isJust (ueOnClick el)
    ∧ isNothing (ueTextBuffer el)
    ∧ isPageInScope (uePage el) mgr

-- | Every keyboard-focusable control, in stable traversal order.
--   'UI.Types.ueTabIndex' (when set) sorts a control by that explicit
--   value first; every control that leaves it unset sorts by its own
--   paint-traversal position, so explicit indices can slot a control
--   anywhere in the natural flow without requiring every sibling to
--   also set one, and two explicit ties (or two unset ones) both fall
--   back to paint order.
focusableElements ∷ UIPageManager → [ElementHandle]
focusableElements mgr =
    map snd $ sortOn fst
        [ ((fromMaybe order (ueTabIndex el), order), h)
        | (order, h) ← zip [0 ∷ Int ..] (paintTraversalOrder mgr)
        , Just el ← [Map.lookup h (upmElements mgr)]
        , eligibleControl mgr el
        ]

-- | Move focus one step through 'focusableElements', wrapping at
--   either end. With no control currently focused, the first Tab
--   focuses the first eligible control (direction ≥ 0) and the first
--   Shift+Tab focuses the last (direction < 0) — the entry defaults
--   the traversal contract requires. A STALE current handle (no longer
--   present in the eligible list — hidden, deleted, disabled, detached,
--   or newly out of scope since it was focused) is treated the same as
--   no focus at all: it repairs to the first/last eligible control
--   rather than propagating a ghost. 'Nothing' only when nothing on
--   screen is focusable at all.
advance ∷ Int → UIPageManager → Maybe ElementHandle → Maybe ElementHandle
advance direction mgr current = case focusableElements mgr of
    [] → Nothing
    xs → Just $ case current ⌦ (`elemIndex` xs) of
        Nothing → if direction ≥ 0 then head xs else last xs
        Just i  → xs !! wrapIndex (i + signum direction) (length xs)
  where
    wrapIndex i n = ((i `mod` n) + n) `mod` n

nextFocus ∷ UIPageManager → Maybe ElementHandle → Maybe ElementHandle
nextFocus = advance 1

prevFocus ∷ UIPageManager → Maybe ElementHandle → Maybe ElementHandle
prevFocus = advance (-1)

-- | Validate a remembered control-focus handle against the live
--   eligible set — mirrors 'UI.Manager.Focus.validateFocus''s repair
--   contract for text focus exactly: still eligible ⇒ kept, otherwise
--   cleared to 'Nothing' (never left pointing at a ghost). Showing a
--   page back up never resurrects stale focus on its own — the next
--   validation pass (every keyboard dispatch consults this, same as
--   text focus) only ever looks at CURRENT eligibility, never at what
--   used to be focused before the page was hidden.
validateControlFocus ∷ UIPageManager → Maybe ElementHandle → Maybe ElementHandle
validateControlFocus mgr current = do
    h  ← current
    el ← Map.lookup h (upmElements mgr)
    if eligibleControl mgr el then Just h else Nothing

-- | Handle-based lookup of 'eligibleControl' — an unknown/deleted
--   handle is never eligible. Lets a pointer click (which only has a
--   handle, not a 'UIElement' in hand) decide whether landing on this
--   control should also move keyboard control focus to it — see
--   'Engine.Input.Thread.Mouse'.
isEligibleControl ∷ ElementHandle → UIPageManager → Bool
isEligibleControl h mgr = maybe False (eligibleControl mgr) (Map.lookup h (upmElements mgr))
