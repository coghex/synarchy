-- | The spatial floor the two medical treatment verbs hold (#2297).
--
--   @unit.treatBleeding@ and @unit.treatInfection@ are item-consuming
--   commits: they drop bandage instances out of a first-aid kit and
--   spend antiseptic / antibiotic doses out of it. Until #2297 neither
--   read a page and neither read a position, so a caller could dress a
--   patient anywhere on the map and spend a THIRD unit's supplies from
--   anywhere else — including across world pages, which is the floor
--   #1673 closed for the four lax AI item verbs and these two never
--   had.
--
--   This module is the one authority for that floor, and deliberately
--   pure: the verbs consult it, @unit.canTreat@ exposes the same answer
--   to the context menu so a row cannot be enabled into a refusal, and
--   @unit.treatmentRange@ hands 'treatmentRange' itself to the
--   autonomous medic's arrival check. There is no second copy of the
--   number and no second copy of the predicate.
module Unit.Medical.Reach
  ( treatmentRange
  , TreatReachRefusal(..)
  , reachRefusalMessage
  , checkTreatReach
  , withinTreatmentRange
  ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Unit.Types (UnitId, UnitInstance(..), UnitManager(..))

-- | How far, in tiles, the medic may be from the patient it treats and
--   from the unit whose kit it spends. This is the shipped autonomous
--   constant: @scripts/unit_ai_medic.lua@'s treat-arrival check reads
--   it back through @unit.treatmentRange()@ rather than restating it,
--   so the AI cannot walk to a distance the verb then refuses.
treatmentRange ∷ Float
treatmentRange = 1.5

-- | Why a treatment was refused before anything was read or spent.
--   Each maps to its own message, distinct from the wound-state
--   outcomes (\"no bleeding wound to treat\" \/ \"no infected wound to
--   treat\"), so a caller can tell a spatial refusal from a clinical
--   one even when both would apply.
data TreatReachRefusal
    = ReachNotFound         -- ^ medic, patient or kit owner is not live
    | ReachPatientOffPage   -- ^ patient stands on another world page
    | ReachOwnerOffPage     -- ^ kit owner stands on another world page
    | ReachPatientTooFar    -- ^ patient is beyond 'treatmentRange'
    | ReachOwnerTooFar      -- ^ kit owner is beyond 'treatmentRange'
    deriving (Eq, Show)

reachRefusalMessage ∷ TreatReachRefusal → Text
reachRefusalMessage ReachNotFound
    = "medic, patient, or kit owner not found"
reachRefusalMessage ReachPatientOffPage
    = "patient is on another world page"
reachRefusalMessage ReachOwnerOffPage
    = "kit owner is on another world page"
reachRefusalMessage ReachPatientTooFar
    = "patient is out of treatment range"
reachRefusalMessage ReachOwnerTooFar
    = "kit owner is out of treatment range"

-- | Resolve the three units a treatment involves and hold the floor
--   over them, in the one order the contract fixes: existence, then
--   PAGE identity, then range. Range is only meaningful once both
--   endpoints are known to be in the same world, so a cross-page pair
--   is always reported as a page refusal rather than as a huge
--   distance.
--
--   The medic supplies its own kit unless the caller names another
--   owner, so passing @medic@ for @owner@ (the default) can never fail
--   on the owner clauses.
--
--   Returns the three live instances on success so the caller does not
--   look them up a second time.
checkTreatReach ∷ UnitManager → UnitId → UnitId → UnitId
                → Either TreatReachRefusal
                         (UnitInstance, UnitInstance, UnitInstance)
checkTreatReach um medic patient owner =
    case ( HM.lookup medic   (umInstances um)
         , HM.lookup patient (umInstances um)
         , HM.lookup owner   (umInstances um) ) of
      (Just med, Just pat, Just own)
        | uiPage pat ≢ uiPage med            → Left ReachPatientOffPage
        | uiPage own ≢ uiPage med            → Left ReachOwnerOffPage
        | not (withinTreatmentRange med pat) → Left ReachPatientTooFar
        | not (withinTreatmentRange med own) → Left ReachOwnerTooFar
        | otherwise                          → Right (med, pat, own)
      _ → Left ReachNotFound

-- | Raw two-dimensional Euclidean distance over the CONTINUOUS grid
--   coordinates, the same measure @scripts/unit_ai_core.lua@'s
--   @distance@ applies to @unit.getInfo@'s @gridX@ \/ @gridY@. Exactly
--   'treatmentRange' apart is in reach; anything greater is not. The
--   z coordinate is deliberately not part of it — the autonomous path
--   this mirrors does not consider it either.
withinTreatmentRange ∷ UnitInstance → UnitInstance → Bool
withinTreatmentRange a b =
    let dx = uiGridX a - uiGridX b
        dy = uiGridY a - uiGridY b
    in sqrt (dx * dx + dy * dy) ≤ treatmentRange
