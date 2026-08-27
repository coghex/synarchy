{-# LANGUAGE Strict #-}
module UI.Manager.Hierarchy
  ( addElementToPage
  , addChildElement
  , removeElement
  , removeFromPage
  ) where

import UPrelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import UI.Types
import UI.Manager.Core (modifyElement, modifyPage, removeElementReference, bumpElementRouteEpoch)

-- * Structural ownership (#1694)

-- | The one container a live element is reachable through: a page's
--   'upRootElements' or another element's 'ueChildren'.
data StructuralOwner
  = OwnedByPage PageHandle
  | OwnedByParent ElementHandle
  deriving (Eq)

-- | Every structural container that CURRENTLY references @handle@.
--
--   #1694: ownership is defined by actual container
--   membership, deliberately NOT by the element's recorded
--   'uePage'/'ueParent' fields — the two can disagree through existing
--   public verbs. 'removeElement' drops the handle from its parent's
--   'ueChildren' while leaving @ueParent = Just p@ recorded, and
--   'removeFromPage' clears 'ueParent' without touching any parent's
--   'ueChildren'. A detach step that trusted the recorded fields would
--   therefore leave behind exactly the unreachable stale reference the
--   single-owner rule exists to eliminate.
--
--   Linear in the manager's page and element counts. Attachment is a
--   UI-build-time operation (never per frame), and the alternative —
--   trusting the recorded fields — is not correct, so the scan is the
--   price of the invariant.
structuralOwners ∷ ElementHandle → UIPageManager → [StructuralOwner]
structuralOwners handle mgr =
    Map.foldrWithKey pageOwner (Map.foldrWithKey parentOwner [] (upmElements mgr))
                     (upmPages mgr)
  where
    pageOwner ph p acc
        | handle `elem` upRootElements p = OwnedByPage ph : acc
        | otherwise                      = acc
    parentOwner eh el acc
        | handle `elem` ueChildren el = OwnedByParent eh : acc
        | otherwise                   = acc

-- | The element whose 'ueChildren' actually contains @handle@, or
--   'Nothing' when it sits directly in a page's root list.
--
--   The recorded 'ueParent' is CHECKED rather than trusted (see
--   'structuralOwners'), and answering "no structural parent" goes
--   through the page root lists rather than a full element scan. Both
--   shortcuts are exact under the single-owner invariant this module
--   maintains, and together they keep the common case — a consistent
--   element whose recorded parent really does hold it, or a genuine
--   page root — off the fallback scan entirely. That matters: this
--   runs per attachment, and a UI build performs thousands.
structuralParentOf ∷ ElementHandle → UIPageManager → Maybe ElementHandle
structuralParentOf handle mgr
    | Just p ← recorded, holds p = Just p
    | isPageRoot                 = Nothing
    | otherwise                  = scanForParent
  where
    recorded = Map.lookup handle (upmElements mgr) ⌦ ueParent
    holds p = maybe False ((handle `elem`) ∘ ueChildren)
                    (Map.lookup p (upmElements mgr))
    isPageRoot = any ((handle `elem`) ∘ upRootElements)
                     (Map.elems (upmPages mgr))
    -- Short-circuits at the first match: 'foldrWithKey' only forces
    -- the accumulator when the guard falls through.
    scanForParent = Map.foldrWithKey pick Nothing (upmElements mgr)
      where
        pick eh el acc
            | handle `elem` ueChildren el = Just eh
            | otherwise                   = acc

-- | Drop every reference to @handle@ from the given containers. A
--   container holding it more than once is cleaned completely, which
--   is what makes repeated attachment idempotent (#1694 requirement 3).
detachFromOwners ∷ ElementHandle → [StructuralOwner] → UIPageManager → UIPageManager
detachFromOwners handle owners mgr0 = foldr step mgr0 owners
  where
    step (OwnedByPage ph) mgr = modifyPage ph mgr $ \page →
        page { upRootElements = filter (≢ handle) (upRootElements page) }
    step (OwnedByParent eh) mgr = modifyElement eh mgr $ \parent →
        parent { ueChildren = filter (≢ handle) (ueChildren parent) }

-- | Assign @pageHandle@ to @root@ and every descendant (#1694). A
--   subtree moved between pages must not leave grandchildren reporting
--   the page they were created under: 'UI.Manager.Page.hidePage'
--   scopes both focus clears by comparing @uePage el ≡ handle@, and
--   'UI.Manager.Query.getPageElements' reaches descendants through the
--   NEW owner, so a stale descendant page is directly observable.
--
--   Termination is by VISITED SET, deliberately not by a depth budget.
--   The root of a moved subtree can itself sit at any depth, so a
--   budget would have to encode where the walk STARTS as well as how
--   tall the subtree is; getting that wrong stops short and leaves the
--   deepest elements reporting a stale page — the exact defect this
--   function exists to prevent. The visited set needs neither number,
--   is exact for every legal shape, and still terminates on a
--   malformed one.
setSubtreePage ∷ PageHandle → ElementHandle → UIPageManager → UIPageManager
setSubtreePage pageHandle root mgr0 = go Set.empty [root] mgr0
  where
    go _ [] mgr = mgr
    go seen (handle : rest) mgr
        | handle `Set.member` seen = go seen rest mgr
        | otherwise =
            let seen' = Set.insert handle seen
            in case Map.lookup handle (upmElements mgr) of
                Nothing → go seen' rest mgr
                Just el → go seen' (ueChildren el ⧺ rest) $
                    modifyElement handle mgr $ \e → e { uePage = pageHandle }

-- | Did this attachment actually change the element's structural
--   owner? The case split is total over membership (#1694 requirement
--   7): no current owner (a fresh element, or one already detached) is
--   epoch-neutral, the same owner is epoch-neutral, and any other
--   current ownership is a real relocation.
ownerChanges ∷ [StructuralOwner] → StructuralOwner → Bool
ownerChanges owners destination =
    not (null owners) ∧ owners ≢ [destination]

-- * Hierarchy

-- | Attach @elemHandle@ to @pageHandle@'s root list at @(x, y)@.
--
--   #1694: every attachment gives the element exactly ONE structural
--   owner.
--
--   * __Validation first.__ Both handles are checked before anything
--     is mutated; an unknown element or page leaves the complete
--     hierarchy unchanged rather than half-updating it (both
--     'modifyElement' and 'modifyPage' are 'Map.adjust'-based and would
--     otherwise silently no-op on one side only).
--   * __Single owner.__ The element is removed from EVERY container
--     that currently references it (see 'structuralOwners') before
--     being appended here, so it appears exactly once under this page
--     and nowhere under a previous page or parent. Re-attaching to the
--     same page is therefore idempotent, not a duplicate reference.
--   * __Recursive page.__ @pageHandle@ is assigned to the element AND
--     every descendant (see 'setSubtreePage'), so a subtree moved
--     between pages never leaves grandchildren reporting the old one.
--   * __Activation epoch.__ Attaching a fresh (or already detached)
--     element, and re-attaching to the same page, are both neutral:
--     nothing bumps. An actual owner change bumps the relocated
--     subtree ROOT's 'UI.Types.ueRouteEpoch' exactly once, which is
--     what cancels a pending activation on that root or on any
--     descendant — 'UI.ControlActivation.resolveActivation' compares
--     the pressed element's whole ancestor chain. A page-root to
--     page-root move needs this: it changes neither
--     'UI.Types.upmPageEpoch' nor the chain's shape, so without the
--     bump a press interrupted by such a move would still activate at
--     release. Nothing here ever touches 'UI.Types.upmPageEpoch'.
--   * __Neutral churn stays neutral.__ Attaching a BRAND-NEW element
--     must not invalidate unrelated pending activations: clicking a
--     control that moves keyboard control focus fires
--     'scripts/ui/focus_indicator.lua'\'s @onUIControlFocusChanged@,
--     which creates and @UI.addChild@s four fresh ring sprites onto
--     the newly focused element — a purely visual side effect of the
--     SAME click, not an interruption of it (#745).
--   * __Focus is preserved.__ Relocation is not a detach, so neither
--     'UI.Types.upmGlobalFocus' (text) nor 'UI.Types.upmControlFocus'
--     is cleared, whether the focused handle is the relocated root or
--     one of its descendants.
addElementToPage ∷ PageHandle → ElementHandle → Float → Float
                 → UIPageManager → UIPageManager
addElementToPage pageHandle elemHandle x y mgr
    | not (Map.member elemHandle (upmElements mgr)) = mgr
    | not (Map.member pageHandle (upmPages mgr))    = mgr
    | otherwise =
        let owners = structuralOwners elemHandle mgr
            mgr1   = detachFromOwners elemHandle owners mgr
            mgr2   = modifyElement elemHandle mgr1 $ \elem →
                        elem { uePosition = (x, y), ueParent = Nothing }
            mgr3   = setSubtreePage pageHandle elemHandle mgr2
            mgr4   = modifyPage pageHandle mgr3 $ \page →
                        page { upRootElements = upRootElements page ⧺ [elemHandle] }
        in if ownerChanges owners (OwnedByPage pageHandle)
           then bumpElementRouteEpoch elemHandle mgr4
           else mgr4

-- | Attach @childHandle@ under @parentHandle@ at @(x, y)@.
--
--   #1694: holds exactly the same single-owner, recursive-page,
--   activation-epoch, focus-retention and atomic-rejection rules as
--   'addElementToPage' — see there. The destination page is the
--   parent's own 'uePage', propagated through the whole moved subtree.
--
--   Rejection stays atomic in both of its forms. An unknown child or
--   parent handle, and a refused cycle, each return the manager
--   untouched: in particular a refused attachment never detaches the
--   child from the owner it already had. The guard covers BOTH edge
--   directions this call adds, and the resulting DEPTH of the whole
--   moved subtree — see 'refuseAttach' below.
addChildElement ∷ ElementHandle → ElementHandle → Float → Float
                → UIPageManager → UIPageManager
addChildElement parentHandle childHandle x y mgr
    | not (Map.member childHandle (upmElements mgr)) = mgr
    | otherwise = case Map.lookup parentHandle (upmElements mgr) of
        Nothing → mgr
        Just parent
            -- Refuse a cycle (child already an ancestor of the
            -- parent, or child ≡ parent) or an over-deep result. A
            -- cycle would hang every tree walk (absolute position,
            -- accumulated z-index, rendering, hit testing, deletion)
            -- on the render and input threads forever; excess depth
            -- truncates those same walks instead. This is the only
            -- site that adds either kind of edge, so the checks here
            -- keep the forest acyclic and bounded globally. Evaluated
            -- against the PRE-detach hierarchy, which is the one the
            -- move has to be legal in.
            | refuseAttach → mgr
            | otherwise →
                let owners = structuralOwners childHandle mgr
                    mgr1   = detachFromOwners childHandle owners mgr
                    mgr2   = modifyElement childHandle mgr1 $ \child →
                                child { uePosition = (x, y)
                                      , ueParent   = Just parentHandle
                                      }
                    mgr3   = setSubtreePage (uePage parent) childHandle mgr2
                    mgr4   = modifyElement parentHandle mgr3 $ \p →
                                p { ueChildren = ueChildren p ⧺ [childHandle] }
                in if ownerChanges owners (OwnedByParent parentHandle)
                   then bumpElementRouteEpoch childHandle mgr4
                   else mgr4
  where
    -- Attaching adds BOTH a @child → parent@ 'ueParent' edge and a
    -- @parent → child@ 'ueChildren' edge, and the two graphs are
    -- walked by different consumers — up by 'getElementAbsolutePosition'
    -- / 'UI.ControlActivation.ancestorChain' /
    -- 'UI.Clipping.effectiveClip', down by 'UI.Render' /
    -- 'UI.Manager.Query.getPageElements' / 'hitsAtPointBy' /
    -- 'paintTraversalOrder' / 'UI.Manager.Core.deleteElementTree'. Each
    -- therefore needs its own guard, and #1694 is
    -- exactly why the second one cannot be inferred from the first:
    -- membership and the recorded 'ueParent' can disagree (see
    -- 'structuralOwners'), so a child whose own structural descendant
    -- had its 'ueParent' cleared by 'removeFromPage' would pass the
    -- up-walk while closing a real 'ueChildren' loop.
    refuseAttach = walkUp maxHierarchyDepth parentHandle
                 ∨ parentInChildSubtree ∨ tooDeep

    -- Is the child already an ancestor of the parent? Keeps the
    -- pre-existing depth budget: it IS the policy 'maxHierarchyDepth'
    -- states, and exceeding it is refused rather than followed.
    walkUp depth h
        | depth ≤ 0 = True          -- pathological depth: refuse too
        | h ≡ childHandle = True
        | otherwise = case Map.lookup h (upmElements mgr) ⌦ ueParent of
            Just p  → walkUp (depth - 1) p
            Nothing → False

    -- Is the parent already inside the child's structural subtree?
    -- Visited-set termination, NOT a depth budget: a subtree standing
    -- exactly at 'UI.Types.maxHierarchyDepth' is a legal shape, and a
    -- budget sized to that would refuse it as if it were a cycle.
    parentInChildSubtree = descend Set.empty [childHandle]
      where
        descend _ [] = False
        descend seen (h : rest)
            | h ≡ parentHandle    = True
            | h `Set.member` seen = descend seen rest
            | otherwise = case Map.lookup h (upmElements mgr) of
                Nothing → descend (Set.insert h seen) rest
                Just el → descend (Set.insert h seen) (ueChildren el ⧺ rest)

    -- Would the move push some element past 'UI.Types.maxHierarchyDepth'
    -- ancestors? Every upward walk in the UI is sized to exactly that
    -- many edges, so a deeper chain does not merely look odd: it
    -- truncates absolute position, accumulated z-index, effective
    -- clip, effective visibility and — worst — the pending-activation
    -- ancestor snapshot, each silently and each on the render or input
    -- thread. The subject is the whole moved SUBTREE, not just the
    -- child: attaching a leaf is bounded by the parent's own depth,
    -- but relocating a tall subtree adds its full height.
    tooDeep = parentDepth + 1 + childHeight > maxHierarchyDepth

    -- Edges above the destination parent, counted over the SAME
    -- membership 'structuralOwners' defines ownership by — NOT over
    -- the recorded 'ueParent'. 'removeFromPage' clears that field on
    -- an element still sitting in its parent's 'ueChildren', which
    -- understates its real depth as zero; a guard reading it would
    -- then admit a fresh child under the deepest element of a
    -- full-depth tree, and repeating the trick extends the structural
    -- chain without bound.
    --
    -- 'walkUp' separately bounds the recorded-'ueParent' chain, so the
    -- two together keep BOTH graphs inside 'maxHierarchyDepth' even
    -- where they disagree.
    parentDepth = climb Set.empty parentHandle 0
      where
        climb seen h acc
            | h `Set.member` seen = acc     -- malformed loop: stop
            | acc > maxHierarchyDepth = acc -- already refusing; stop
            | otherwise = case structuralParentOf h mgr of
                Nothing → acc
                Just p  → climb (Set.insert h seen) p (acc + 1)

    -- Edges below the child, over the same 'ueChildren' membership
    -- graph 'parentInChildSubtree' walks and with the same visited-set
    -- termination.
    childHeight = descend Set.empty [(childHandle, 0 ∷ Int)] 0
      where
        descend _ [] best = best
        descend seen ((h, d) : rest) best
            | h `Set.member` seen = descend seen rest best
            | otherwise =
                let seen' = Set.insert h seen
                    best' = max best d
                in case Map.lookup h (upmElements mgr) of
                    Nothing → descend seen' rest best'
                    Just el → descend seen'
                        ([ (c, d + 1) | c ← ueChildren el ] ⧺ rest) best'

-- | #745: also bumps this element's OWN
--   'UI.Types.ueRouteEpoch' — a pending pointer activation on this
--   handle, or on a descendant that has it as an ancestor, must not
--   survive detach→re-add on the same handle; see
--   'bumpElementRouteEpoch'. #1694: the re-attach side now bumps too,
--   but only on an actual owner CHANGE (see 'addElementToPage'), so a
--   detach→re-attach sequence is still poisoned by this bump alone.
removeElement ∷ ElementHandle → UIPageManager → UIPageManager
removeElement handle mgr =
    case Map.lookup handle (upmElements mgr) of
        Nothing → mgr
        Just element →
            let mgr0 = bumpElementRouteEpoch handle mgr
                mgr' = removeElementReference handle element mgr0
                -- A detached element is unreachable for rendering and
                -- hit-testing; it must not keep the keyboard either.
                mgr'' = if upmGlobalFocus mgr' ≡ Just handle
                        then mgr' { upmGlobalFocus = Nothing }
                        else mgr'
            -- #745: same hygiene for CONTROL focus.
            in if upmControlFocus mgr'' ≡ Just handle
               then mgr'' { upmControlFocus = Nothing }
               else mgr''

-- | Remove an element from its page's root list (without deleting it).
-- This detaches the element so its sprites disappear, but the handle
-- remains valid for potential re-use or deferred GC. #1694 keeps that
-- note accurate: re-attaching a detached handle is a no-current-owner
-- attachment, which adds exactly one reference and stays
-- epoch-neutral, so reuse behaves as documented.
--
-- #745: also bumps this element's OWN
-- 'UI.Types.ueRouteEpoch' — see 'removeElement'.
removeFromPage ∷ PageHandle → ElementHandle → UIPageManager → UIPageManager
removeFromPage pageHandle elemHandle mgr0 =
    let mgr   = bumpElementRouteEpoch elemHandle mgr0
        mgr'  = modifyPage pageHandle mgr $ \page →
            page { upRootElements = filter (≢ elemHandle) (upRootElements page) }
        mgr'' = modifyElement elemHandle mgr' $ \elem →
            elem { ueParent = Nothing }
        -- Same focus hygiene as removeElement: detached ⇒ no keyboard.
        mgr''' = if upmGlobalFocus mgr'' ≡ Just elemHandle
                 then mgr'' { upmGlobalFocus = Nothing }
                 else mgr''
    -- #745: same hygiene for CONTROL focus.
    in if upmControlFocus mgr''' ≡ Just elemHandle
       then mgr''' { upmControlFocus = Nothing }
       else mgr'''
