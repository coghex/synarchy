{-# LANGUAGE Strict #-}
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
import UI.Manager.Core (deleteElementTree, bumpPageEpoch)

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

-- | #745: also bumps 'UI.Types.upmPageEpoch' — a
--   pending pointer activation on ANY control (not just one this page
--   owns — a SEPARATE modal/menu page appearing over it counts too)
--   must not restore across a page flickering hidden-then-shown
--   ("changing menus" per the #745 issue text); see 'bumpPageEpoch'.
--   Deliberately GLOBAL, unlike element-level property mutators —
--   page visibility is a genuinely route-affecting event everywhere.
--
--   #745: only bumps when the page was actually
--   hidden — a no-op re-show (already visible) must not poison an
--   in-flight pending activation that was never really interrupted.
showPage ∷ PageHandle → UIPageManager → UIPageManager
showPage handle mgr =
    case Map.lookup handle (upmPages mgr) of
        Nothing → mgr
        Just page →
            (if upVisible page then id else bumpPageEpoch) $
            mgr { upmPages = Map.insert handle (page { upVisible = True }) (upmPages mgr)
                , upmVisiblePages = Set.insert handle (upmVisiblePages mgr)
                }

-- | #745: also bumps 'UI.Types.upmPageEpoch' — see
--   'showPage'.
--
--   #745: only bumps when the page was actually
--   visible — see 'showPage'.
hidePage ∷ PageHandle → UIPageManager → UIPageManager
hidePage handle mgr =
    case Map.lookup handle (upmPages mgr) of
        Nothing → mgr
        Just page →
            (if upVisible page then bumpPageEpoch else id) $
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
                -- #745: keyboard CONTROL focus needs
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
--
--   #1748: also bumps 'UI.Types.upmPageEpoch', but only when the
--   assignment REALLY changes 'upInputExclusive' on a page that is
--   CURRENTLY VISIBLE. Inserting or removing a modal boundary on a
--   visible page is route-affecting at page scope in exactly the way
--   'showPage'/'hidePage' are — 'UI.InputOwnership.inputBoundaryPage'
--   picks the topmost visible exclusive page and
--   'UI.InputOwnership.pagesInScope' drops everything below it — so a
--   @False → True → False@ round trip during one press must cancel the
--   pending activation it interrupted, even though the final route,
--   the exclusivity value and the pressed element's ancestor chain are
--   all restored to their press-time state by release.
--
--   BOTH guards are load-bearing, and neither is stylistic:
--
--     * The no-op guard mirrors 'showPage'/'hidePage''s (#745): an
--       assignment that does not change the value interrupted nothing
--       and must not poison an in-flight activation.
--     * The visibility guard is what keeps this precise. Exclusivity
--       is invisible to routing until the page is shown, because
--       'inputBoundaryPage' filters 'getVisiblePages'. Bumping on
--       every real change regardless of visibility would cancel an
--       unrelated in-flight click every time @scripts/popup.lua@'s
--       @popup.init@ runs, since a 'LayerModal' page defaults
--       exclusive and the notification page opts back out — a genuine
--       @True → False@ transition. All three tracked call sites
--       (@scripts/popup.lua@ x2, @scripts/input_check_fixture.lua@)
--       configure a freshly created, not-yet-shown page, so this
--       stays neutral for each of them; the 'showPage' that follows
--       bumps on its own, as it always did.
--
--   This deliberately remains a generally callable mutation rather
--   than a construction-only one: the Lua binding
--   ('Engine.Scripting.Lua.API.UI.Page') exposes it unrestricted, so
--   the invariant holds by construction here instead of by caller
--   discipline.
setPageInputExclusive ∷ PageHandle → Bool → UIPageManager → UIPageManager
setPageInputExclusive handle exclusive mgr =
    case Map.lookup handle (upmPages mgr) of
        Nothing → mgr
        Just page →
            (if upInputExclusive page ≢ exclusive ∧ upVisible page
                then bumpPageEpoch else id) $
            mgr { upmPages = Map.insert handle
                    (page { upInputExclusive = exclusive }) (upmPages mgr) }

getVisiblePages ∷ UIPageManager → [UIPage]
getVisiblePages mgr =
    let visibleList = mapMaybe (`Map.lookup` upmPages mgr)
                              (Set.toList $ upmVisiblePages mgr)
    in sortOn (\p → (upLayer p, upZIndex p)) visibleList
