{-# LANGUAGE Strict, UnicodeSyntax #-}
module UI.Manager.Hierarchy
  ( addElementToPage
  , addChildElement
  , removeElement
  , removeFromPage
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import UI.Types
import UI.Manager.Core (modifyElement, modifyPage, removeElementReference, bumpActivationEpoch)

-- * Hierarchy

-- | #745 review round 9: also bumps 'ueActivationEpoch' — a pending
--   pointer activation captured before a detach must not survive a
--   re-attach that lands back on the same element; see
--   'bumpActivationEpoch'.
addElementToPage ∷ PageHandle → ElementHandle → Float → Float
                 → UIPageManager → UIPageManager
addElementToPage pageHandle elemHandle x y mgr =
    let mgr' = modifyElement elemHandle mgr $ \elem →
            elem { uePosition = (x, y), uePage = pageHandle, ueParent = Nothing }
        mgr'' = bumpActivationEpoch elemHandle mgr'
    in modifyPage pageHandle mgr'' $ \page →
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
            -- #745 review round 9: also bumps 'ueActivationEpoch' on
            -- childHandle — see 'bumpActivationEpoch'.
            | otherwise →
                let mgr' = modifyElement childHandle mgr $ \child →
                        child { uePosition = (x, y)
                              , uePage     = uePage parent
                              , ueParent   = Just parentHandle
                              }
                    mgr'' = bumpActivationEpoch childHandle mgr'
                in modifyElement parentHandle mgr'' $ \p →
                        p { ueChildren = ueChildren p ⧺ [childHandle] }
  where
    wouldCycle = walkUp (64 ∷ Int) parentHandle
    walkUp depth h
        | depth ≤ 0 = True          -- pathological depth: refuse too
        | h ≡ childHandle = True
        | otherwise = case Map.lookup h (upmElements mgr) ⌦ ueParent of
            Just p  → walkUp (depth - 1) p
            Nothing → False

-- | #745 review round 9: also bumps 'ueActivationEpoch' — a pending
--   pointer activation must not survive detach→re-add on the same
--   handle; see 'bumpActivationEpoch'.
removeElement ∷ ElementHandle → UIPageManager → UIPageManager
removeElement handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → mgr
        Just element →
            let mgr0 = bumpActivationEpoch handle mgr
                mgr' = removeElementReference handle element mgr0
                -- A detached element is unreachable for rendering and
                -- hit-testing; it must not keep the keyboard either.
                mgr'' = if upmGlobalFocus mgr' ≡ Just handle
                        then mgr' { upmGlobalFocus = Nothing }
                        else mgr'
            -- #745 review round 3: same hygiene for CONTROL focus.
            in if upmControlFocus mgr'' ≡ Just handle
               then mgr'' { upmControlFocus = Nothing }
               else mgr''

-- | Remove an element from its page's root list (without deleting it).
-- This detaches the element so its sprites disappear, but the handle
-- remains valid for potential re-use or deferred GC.
--
-- #745 review round 9: also bumps 'ueActivationEpoch' — see
-- 'removeElement'.
removeFromPage ∷ PageHandle → ElementHandle → UIPageManager → UIPageManager
removeFromPage pageHandle elemHandle mgr0 =
    let mgr   = bumpActivationEpoch elemHandle mgr0
        mgr'  = modifyPage pageHandle mgr $ \page →
            page { upRootElements = filter (/= elemHandle) (upRootElements page) }
        mgr'' = modifyElement elemHandle mgr' $ \elem →
            elem { ueParent = Nothing }
        -- Same focus hygiene as removeElement: detached ⇒ no keyboard.
        mgr''' = if upmGlobalFocus mgr'' ≡ Just elemHandle
                 then mgr'' { upmGlobalFocus = Nothing }
                 else mgr''
    -- #745 review round 3: same hygiene for CONTROL focus.
    in if upmControlFocus mgr''' ≡ Just elemHandle
       then mgr''' { upmControlFocus = Nothing }
       else mgr'''
