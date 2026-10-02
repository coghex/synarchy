{-# LANGUAGE Strict #-}
-- | Cross-entry validation of a scenario document (#2699 requirements 5
--   and 6, design D-17/D-47): identities, references and rejection
--   cascades.
--
--   Everything here works on the 'Node's the first decoding pass
--   recorded, and every rule is a function of the SET of nodes, never of
--   their order: a duplicated id rejects all of its holders, references
--   resolve against the whole document (so a forward reference is as
--   good as a backward one), and the required-reference closure runs to
--   a fixpoint.
module Scenario.Validate
    ( Analysis(..)
    , analyseNodes
    , fallbackStatNames
    , fallbackSkillNames
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Scenario.Types
import Scenario.Decode.Monad (Node(..))

data Analysis = Analysis
    { anRejected    ∷ !(HS.HashSet Text)   -- ^ entry paths to exclude
    , anDropped     ∷ !(HS.HashSet Text)   -- ^ optional-reference fields to drop
    , anDiagnostics ∷ ![ScenarioDiagnostic]
    }

analyseNodes ∷ [Node] → Analysis
analyseNodes nodes =
    Analysis rejected dropped (dupDiags ⧺ refDiags ⧺ cascadeDiags ⧺ bindingDiags)
  where
    alive = filter nAlive nodes
    byPath = HM.fromList [ (nPath n, n) | n ← nodes ]
    children = HM.fromListWith (⧺)
        [ (o, [nPath n]) | n ← alive, Just o ← [nOwner n] ]
    -- every id that was declared anywhere, alive or not
    declared = HS.fromList (mapMaybe nId nodes)

    -- 1. duplicate explicit ids: every holder is rejected
    aliveById = HM.fromListWith (⧺) [ (i, [n]) | n ← alive, Just i ← [nId n] ]
    dupNodes = [ (n, i) | (i, ns@(_ : _ : _)) ← HM.toList aliveById, n ← ns ]
    dupDiags = [ ScenarioDiagnostic (nPath n <> ".id") (DuplicateId i) EntryRejected
               | (n, i) ← dupNodes ]
    dupRejected = HS.fromList (map (nPath ∘ fst) dupNodes)

    -- 2. required references, to a fixpoint
    closeOver r = HS.union r (HS.fromList (concatMap descendants (HS.toList r)))
    descendants p = concat [ c : descendants c | c ← HM.findWithDefault [] p children ]
    target r i = [ n | n ← HM.findWithDefault [] i aliveById
                     , not (HS.member (nPath n) r) ]
    step r = HS.union r $ HS.fromList
        [ nPath n | n ← alive, not (HS.member (nPath n) r)
                  , any (refBroken r) (nRequired n) ]
    refBroken r (_, i, kind) = case target r i of
        [t] → nKind t ≢ kind
        _   → True
    fixpoint r = let r' = closeOver (step r) in if r' ≡ r then r else fixpoint r'
    rejected = fixpoint (closeOver dupRejected)

    -- the diagnostic for each node the reference closure rejected
    refDiags =
        [ ScenarioDiagnostic fp (refReason rejected i kind) (refEffect rejected i)
        | n ← alive, HS.member (nPath n) rejected
        , not (HS.member (nPath n) dupRejected)
        , not (ownerRejected n)
        , (fp, i, kind) ← nRequired n, refBroken rejected (fp, i, kind) ]
    refReason r i kind = case target r i of
        [t] | nKind t ≢ kind → WrongReferenceKind i
        _ | HS.member i declared → RejectedReference i
          | otherwise → MissingReference i
    refEffect r i = case target r i of
        [_] → EntryRejected
        _ | HS.member i declared → CascadeRejected
          | otherwise → EntryRejected
    ownerRejected n = maybe False (`HS.member` rejected) (nOwner n)

    -- 3. owned descendants of anything the analysis rejected
    cascadeDiags =
        [ ScenarioDiagnostic (nPath n) (OwnerRejected root) CascadeRejected
        | n ← alive, HS.member (nPath n) rejected, ownerRejected n
        , let root = topRejected n ]
    topRejected n = case nOwner n ≫= (`HM.lookup` byPath) of
        Just o | HS.member (nPath o) rejected → topRejected o
        _ → nPath n

    -- 4. optional bindings of surviving entries
    survivors = [ n | n ← alive, not (HS.member (nPath n) rejected) ]
    survivingIds = HM.fromListWith (⧺) [ (i, [n]) | n ← survivors, Just i ← [nId n] ]
    bindings = [ b | n ← survivors, b ← nOptional n ]
    bindingProblem (_, i, kind, want) = case HM.findWithDefault [] i survivingIds of
        [t] | nKind t ≢ kind → Just (WrongReferenceKind i)
            | Just w ← want, nDefinition t ≢ Just w → Just (DefinitionMismatch w)
            | otherwise → Nothing
        _ | HS.member i declared → Just (RejectedReference i)
          | otherwise → Just (MissingReference i)
    badBindings = [ (fp, why) | b@(fp, _, _, _) ← bindings, Just why ← [bindingProblem b] ]
    goodBindings = [ b | b ← bindings, isNothing (bindingProblem b) ]
    boundTwice = HM.filter ((> 1) ∘ length) $
        HM.fromListWith (⧺) [ (i, [fp]) | (fp, i, _, _) ← goodBindings ]
    ambiguous = [ (fp, AmbiguousBinding i) | (i, fps) ← HM.toList boundTwice, fp ← fps ]
    bindingDiags = [ ScenarioDiagnostic fp why FieldRejected
                   | (fp, why) ← badBindings ⧺ ambiguous ]
    dropped = HS.fromList (map fst (badBindings ⧺ ambiguous))

-- | The authorable stats a unit entry leaves to the definition's
--   deterministic fallback (D-5, D-22): every authorable stat of the
--   CURRENT definition it does not override. A stat the definition
--   gained after the scenario was written lands here, with no warning.
fallbackStatNames ∷ UnitCatalogEntry → UnitEntry → [Text]
fallbackStatNames uc ue =
    sort [ s | (s, AuthorableStat) ← HM.toList (ucStats uc)
             , not (HM.member s (ueStats ue)) ]

fallbackSkillNames ∷ UnitCatalogEntry → UnitEntry → [Text]
fallbackSkillNames uc ue =
    sort [ s | s ← HS.toList (ucSkills uc), not (HM.member s (ueSkills ue)) ]
