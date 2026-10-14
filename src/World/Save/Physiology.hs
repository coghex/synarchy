{-# LANGUAGE Strict #-}
-- | Load-time rejection of invalid saved physiology state (#2829, S3 of
--   the physiology stat-modes epic #2827; design D-6 cases (d) and (h)).
--
--   __Where this runs, and why not in the component.__ The @units@
--   component's decode and @csValidate@ see only the decoded DTO, and
--   whether a system id or a Simulated key is meaningful depends on the
--   LOADED registry and the unit's loaded definition, which a component
--   migration must never read (content-dependent normalization is S4's,
--   and a migration that consulted live content would make an old save
--   decode differently per build). So the content-dependent half lives at
--   the content-aware load boundary, beside the missing-definition checks
--   in "World.Save.Types", which rejects the whole load BEFORE anything is
--   staged: the old live session is never touched. The content-independent
--   half needs no code here: an unknown 'Unit.Physiology.Types.PhysioMode'
--   tag (case (e)) fails cereal's generic sum decode, which the component
--   machinery reports as a malformed payload.
--
--   __Recognition, not migration.__ An override naming a system the
--   registry declares @retired:@ or @renamed_from:@ is neither rejected
--   (that is case (d)'s undeclared system) nor normalized (case (c), S4):
--   it is restored verbatim and round-trips exactly until S4 applies the
--   declared migration.
--
--   __What (h) does and does not look at.__ A non-finite stored value of
--   a SIMULATED system's durable state — its owned keys and its meters —
--   is malformed. A derived key (@salt_conc@) is never examined: a legacy
--   stored one is S4's case (i), scrubbed in any mode, and rejecting it
--   here would reject saves that scrub is meant to repair. Finite
--   out-of-range values are retained exactly, and no unrelated stat is
--   validated.
module World.Save.Physiology
    ( PhysiologyLoadRejection(..)
    , physiologyLoadRejections
    , renderPhysiologyRejection
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Unit.Physiology (resolveMode)
import Unit.Physiology.Types
import Unit.Types (UnitId(..))
import World.Page.Types (WorldPageId(..))
import World.Save.Types (UnitSnapshot(..), UnitInstanceSnapshot(..))

-- | Why a saved unit's physiology state rejects the load.
data PhysiologyLoadRejection
    = UndeclaredOverrideSystem
        { plrPage   ∷ !WorldPageId
        , plrUnit   ∷ !Word32
        , plrDef    ∷ !Text
        , plrSystem ∷ !Text
        }
      -- ^ an override naming a system the registry neither declares nor
      --   recognizes as retired or renamed: an active gameplay reference
      --   to a missing definition.
    | NonFiniteSimulatedValue
        { plrPage   ∷ !WorldPageId
        , plrUnit   ∷ !Word32
        , plrDef    ∷ !Text
        , plrSystem ∷ !Text
        , plrKey    ∷ !Text
        , plrValue  ∷ !Float
        }
      -- ^ a NaN or infinite stored value of a Simulated system's owned
      --   key or meter: a malformed payload.
    deriving (Show, Eq)

renderPhysiologyRejection ∷ PhysiologyLoadRejection → Text
renderPhysiologyRejection r = case r of
    UndeclaredOverrideSystem{} →
        who <> " carries a physiology override for system '"
        <> plrSystem r <> "', which the physiology registry does not \
        \declare (nor as retired or renamed): an active gameplay \
        \definition is missing"
    NonFiniteSimulatedValue{} →
        who <> " holds a non-finite value (" <> tshow (plrValue r)
        <> ") for '" <> plrKey r <> "', which belongs to its Simulated \
        \physiology system '" <> plrSystem r <> "': malformed payload"
  where
    who = "unit #" <> tshow (plrUnit r) <> " (" <> plrDef r
        <> ") on page '" <> unWorldPageId (plrPage r) <> "'"

-- | Every rejection across the saved pages, in page then unit-id order
--   (so the first message is the one an author finds first). Empty ⇒ the
--   saved physiology state may proceed to staging.
--
--   Pure: the registry and the loaded definitions' 'UnitPhysiology' come
--   from the live content at the load boundary, the pages from the
--   decoded save. A unit whose definition is not registered is skipped —
--   the missing-definition check already rejects it, naming the
--   definition, and a second diagnostic for the same unit would only
--   bury that one.
physiologyLoadRejections
    ∷ PhysiologyRegistry
    → HM.HashMap Text UnitPhysiology         -- ^ loaded definitions' physiology
    → [(WorldPageId, UnitSnapshot)]
    → [PhysiologyLoadRejection]
physiologyLoadRejections registry defs pages = concatMap pageRejections pages
  where
    declared = map psId (prSystems registry)
                 ⧺ prRetired registry
                 ⧺ concatMap psRenamedFrom (prSystems registry)

    pageRejections (pid, us) = concat
        [ unitRejections pid (unUnitId uid) u
        | (uid, u) ← sortOn (unUnitId ∘ fst) (HM.toList (usnInstances us)) ]

    unitRejections pid uid u = case HM.lookup (uisDefName u) defs of
        Nothing → []
        Just up →
            [ UndeclaredOverrideSystem pid uid (uisDefName u) sid
            | sid ← sortOn id (HM.keys (uisPhysioOverrides u))
            , sid `notElem` declared ]
            ⧺ [ NonFiniteSimulatedValue pid uid (uisDefName u) (psId sys) key v
              | sys ← prSystems registry
              , resolveMode sys up (HM.lookup (psId sys) (uisPhysioOverrides u))
                    ≡ ModeSimulated
              , key ← map okKey (psOwns sys) ⧺ psMeters sys
              , Just v ← [HM.lookup key (uisStats u)]
              , isNaN v ∨ isInfinite v ]
