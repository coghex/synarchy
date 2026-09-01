{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Durable per-designation ATTEMPT identity (#1844).
--
--   A construction designation used to be addressed by page and
--   canonical tile alone, and every delayed operation the build AI
--   issues — a claim, a status change, a progress pour, a payment, a
--   cancellation, a completion, a slope cleanup — named only that
--   coordinate. Cancel a job and designate a successor at the same tile
--   and the old job's in-flight operations land on the NEW one: they
--   cannot tell the two apart, because nothing in the message says
--   which designation was observed.
--
--   'StructureStageToken' (#1674) already solved exactly this problem
--   for staged structure PLACEMENT, and this is the same mechanism one
--   layer up: each designation takes an id from its page's own
--   monotonic allocator at admission, every operation carries the id it
--   observed, and a mutation applies only when the stored designation's
--   id matches. A delayed operation for a removed attempt is then a
--   no-op against any successor rather than a silent corruption.
--
--   Two properties are load-bearing and neither is an accident:
--
--     * The allocator only ever ADVANCES. Nothing recycles a retired id
--       — not a cancellation, not a completion, not a whole-page sweep
--       — so an id that named a removed attempt can never come to name
--       a live one while delayed work for the first may still exist.
--     * It is DURABLE. It rides the page's activity slice, so a
--       designation created after a save\/load cannot collide with one
--       the save already holds (requirement 12). A legacy payload with
--       no allocator gets ids assigned deterministically by ascending
--       tile key and the allocator advanced past every one of them.
module World.Construct.Attempt
    ( ConstructAttemptId(..)
    , firstConstructAttemptId
    , takeConstructAttempt
    , takeConstructAttempts
    , advanceConstructAttemptsPast
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)

-- | Identity of ONE construction-designation attempt on ONE page.
--
--   Unique within its page (its "ownership domain" — designations are a
--   per-page map) and never reused within it. Deliberately a newtype
--   over 'Word64' rather than a bare number so a coordinate cannot be
--   passed where an attempt is wanted.
newtype ConstructAttemptId = ConstructAttemptId Word64
    deriving stock (Generic)
    deriving newtype (Show, Eq, Ord, Serialize, NFData)

-- | The first id a fresh page hands out. 1, not 0: every engine
--   allocator but ground items starts at 1 (@docs\/persistence_contract.md@),
--   which also leaves 0 free as an unmistakable "no attempt" sentinel
--   for the Lua boundary, where a missing field reads as nil.
firstConstructAttemptId ∷ ConstructAttemptId
firstConstructAttemptId = ConstructAttemptId 1

-- | Hand out one id, returning it with the advanced allocator.
takeConstructAttempt ∷ ConstructAttemptId
                     → (ConstructAttemptId, ConstructAttemptId)
                        -- ^ (allocated, next)
takeConstructAttempt a@(ConstructAttemptId n) = (a, ConstructAttemptId (n + 1))

-- | Hand out @n@ ids in ascending order, returning them with the
--   advanced allocator. A non-positive count allocates nothing.
--
--   The commit path asks for one id per CANDIDATE and then admits only
--   some of them, so ids are routinely burned. That is deliberate:
--   uniqueness is the contract, density is not.
takeConstructAttempts ∷ Int → ConstructAttemptId
                      → ([ConstructAttemptId], ConstructAttemptId)
takeConstructAttempts n a@(ConstructAttemptId base)
    | n ≤ 0     = ([], a)
    | otherwise = ( [ ConstructAttemptId (base + fromIntegral i)
                    | i ← [0 .. n - 1] ]
                  , ConstructAttemptId (base + fromIntegral n) )

-- | Raise an allocator so it sits strictly above every supplied id, and
--   never below where it already was. The legacy-migration primitive
--   (requirement 12): ids reconstructed for a pre-#1844 payload must be
--   unreachable by any later allocation, and a payload that already
--   carries a sane allocator must not be dragged backwards by it.
advanceConstructAttemptsPast
    ∷ [ConstructAttemptId] → ConstructAttemptId → ConstructAttemptId
advanceConstructAttemptsPast used next =
    foldl' step next used
  where
    step acc@(ConstructAttemptId n) (ConstructAttemptId u)
        | u + 1 > n = ConstructAttemptId (u + 1)
        | otherwise = acc
