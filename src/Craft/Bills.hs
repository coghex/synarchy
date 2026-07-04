{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Craft-bill queue (#329): the data + pure transitions behind the
--   crafting production loop. A bill is a standing order against one
--   work station — "run this recipe N times" (or forever) — that the
--   craft AI claims, works and completes; the player-facing queue UI
--   is #330.
--
--   This is the crafting parallel to 'World.Construct.Types': a
--   per-world job layer, persisted in saves ('wpsCraftBills'), with
--   the claim/progress lifecycle mirrored from the construction
--   designations the build AI (#96) executes. Unlike designations
--   (keyed by tile) bills are keyed by an id of their own and point at
--   their station ('Building.Types.BuildingId' — stable across
--   save/load, so a persisted bill survives with its station).
--
--   Everything here is pure so the transitions can be exercised
--   directly by the headless suite; the Lua verbs
--   (Engine.Scripting.Lua.API.Craft) wrap each one in a single
--   atomicModifyIORef' on the world's 'wsCraftBillsRef', which is what
--   makes a claim race between two crafters resolve atomically.
module Craft.Bills
    ( BillId(..)
    , CraftBill(..)
    , CraftBills(..)
    , ReorderDirection(..)
    , emptyCraftBills
    , addBill
    , removeBill
    , lookupBill
    , billsForStation
    , claimAvailable
    , claimBill
    , releaseBill
    , addBillProgress
    , completeBillCycle
    , setBillPaused
    , reorderBill
    , pruneToStations
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.List (sortOn, findIndex)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Building.Types (BuildingId(..))
import Unit.Types (UnitId(..))

-- | Bill ids start at 1 (see 'emptyCraftBills') so 0 never names a
--   bill — Lua callers can't confuse a real id with a falsy default.
newtype BillId = BillId { unBillId ∷ Word32 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, Serialize)

-- | One standing craft order. Field order is load-bearing (positional
--   Generic Serialize — append, don't reorder).
data CraftBill = CraftBill
    { cbId        ∷ !BillId
    , cbStation   ∷ !BuildingId
      -- ^ The work station this bill runs at. Validated to offer the
      --   recipe's station operation at add time; the AI re-checks the
      --   station is alive + Built each scan (a demolished station's
      --   bills linger, visible + cancellable, but draw no workers).
    , cbRecipe    ∷ !Text
      -- ^ Recipe id into the RecipeManager catalogue (#325).
    , cbRemaining ∷ !Int
      -- ^ Crafts left to run; negative = repeat forever (#330's
      --   "repeat" mode). Finite bills are removed when this hits 0.
    , cbClaimant  ∷ !(Maybe UnitId)
      -- ^ The worker currently on the bill, if any. One crafter per
      --   bill; stale claims (dead / stuck claimant) are recoverable —
      --   see 'claimAvailable'.
    , cbClaimedAt ∷ !Double
      -- ^ Game-time of the last claim or refresh. Meaningless while
      --   'cbClaimant' is Nothing.
    , cbProgress  ∷ !Float
      -- ^ Work poured into the CURRENT craft cycle, 0.0 → 1.0. Kept
      --   across a release so a replacement crafter resumes rather
      --   than restarts (inputs are only consumed at cycle end, so
      --   nothing is lost either way).
    , cbSeq       ∷ !Int
      -- ^ Manual queue-order key (#330's "reorder"), independent of
      --   'cbId' so it can be freely rewritten. Starts equal to the
      --   bill's id; 'reorderBill' swaps it with a neighbour's.
      --   'billsForStation' sorts on this, not 'cbId'.
    , cbPaused    ∷ !Bool
      -- ^ #330's "pause": a paused bill can never be freshly claimed
      --   (not even via a dead/stale-claimant takeover — see
      --   'claimAvailable'), but a claimant already on it when it's
      --   paused keeps working to the end of the current cycle rather
      --   than being ripped off mid-craft.
    } deriving (Show, Eq, Generic, Serialize)

-- | The per-world bill set. The id counter lives inside so it
--   persists with the bills — a loaded save can't mint an id that
--   collides with a saved bill.
data CraftBills = CraftBills
    { cbsBills  ∷ !(HM.HashMap BillId CraftBill)
    , cbsNextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

emptyCraftBills ∷ CraftBills
emptyCraftBills = CraftBills HM.empty 1

-- | Queue a new bill: pending (unclaimed), no progress. @count@ ≤ 0 is
--   normalised to -1 (repeat forever); the Lua verb layer owns
--   validating the station/recipe pairing.
addBill ∷ BuildingId → Text → Int → CraftBills → (CraftBills, BillId)
addBill station recipe count bills =
    let bid   = BillId (cbsNextId bills)
        bill  = CraftBill
            { cbId        = bid
            , cbStation   = station
            , cbRecipe    = recipe
            , cbRemaining = if count ≤ 0 then -1 else count
            , cbClaimant  = Nothing
            , cbClaimedAt = 0
            , cbProgress  = 0
            , cbSeq       = fromIntegral (cbsNextId bills)
            , cbPaused    = False
            }
    in ( bills { cbsBills  = HM.insert bid bill (cbsBills bills)
               , cbsNextId = cbsNextId bills + 1 }
       , bid )

-- | Cancel a bill outright. False when the id names nothing.
removeBill ∷ BillId → CraftBills → (CraftBills, Bool)
removeBill bid bills
    | HM.member bid (cbsBills bills) =
        (bills { cbsBills = HM.delete bid (cbsBills bills) }, True)
    | otherwise = (bills, False)

lookupBill ∷ BillId → CraftBills → Maybe CraftBill
lookupBill bid = HM.lookup bid . cbsBills

-- | All bills queued at one station (the #330 station panel's view).
--   Sorted by 'cbSeq' — oldest-first at add time, but freely
--   rewritable by 'reorderBill' (unlike 'cbId', which is immutable and
--   only used for lookup).
billsForStation ∷ BuildingId → CraftBills → [CraftBill]
billsForStation station =
    sortOn cbSeq . filter ((≡ station) . cbStation) . HM.elems . cbsBills

-- | Can @uid@ take (or keep) this bill? Yes when it's unclaimed, when
--   @uid@ already holds it (a refresh), or when the standing claim has
--   gone stale — the claimant died or hasn't refreshed within
--   @timeout@ game-seconds (stuck worker, orphan from a save/reload).
--   The staleness rules mirror the construct AI's claim sweep, but
--   live engine-side so recovery doesn't depend on any worker
--   scanning.
--
--   A paused bill ('cbPaused') can never be FRESHLY claimed — not even
--   via the dead-claimant or stale-timeout takeover paths — but the
--   worker who already holds it may keep refreshing: pausing stops new
--   work from starting, it doesn't rip an in-progress craft away
--   mid-cycle.
claimAvailable ∷ Double → Double → (UnitId → Bool) → UnitId → CraftBill
               → Bool
claimAvailable now timeout alive uid bill = case cbClaimant bill of
    Nothing → not (cbPaused bill)
    Just c  → c ≡ uid
            ∨ (not (cbPaused bill)
               ∧ (not (alive c) ∨ now - cbClaimedAt bill > timeout))

-- | Claim (or refresh) a bill for @uid@. False when the bill doesn't
--   exist or someone else's claim is still fresh. Run inside one
--   atomicModifyIORef' this is the whole cross-crafter race: exactly
--   one of two simultaneous claimants sees True.
claimBill ∷ Double → Double → (UnitId → Bool) → BillId → UnitId
          → CraftBills → (CraftBills, Bool)
claimBill now timeout alive bid uid bills = case lookupBill bid bills of
    Just bill | claimAvailable now timeout alive uid bill →
        let bill' = bill { cbClaimant = Just uid, cbClaimedAt = now }
        in (bills { cbsBills = HM.insert bid bill' (cbsBills bills) }, True)
    _ → (bills, False)

-- | Hand a bill back to pending (claim released, progress kept — see
--   'cbProgress'). False when the id names nothing.
releaseBill ∷ BillId → CraftBills → (CraftBills, Bool)
releaseBill bid bills = case lookupBill bid bills of
    Nothing → (bills, False)
    Just bill →
        let bill' = bill { cbClaimant = Nothing }
        in (bills { cbsBills = HM.insert bid bill' (cbsBills bills) }, True)

-- | Pour work into the current cycle; progress clamps to [0, 1].
--   Returns the post-add progress, or Nothing for an unknown bill.
--   The craft AI watches the return value hit 1.0 and then fires the
--   actual craft (craft.executeAt) + 'completeBillCycle' itself —
--   consumption stays in the executeAt verb, one authority.
addBillProgress ∷ BillId → Float → CraftBills → (CraftBills, Maybe Float)
addBillProgress bid delta bills = case lookupBill bid bills of
    Nothing → (bills, Nothing)
    Just bill →
        let p' = max 0 (min 1 (cbProgress bill + delta))
            bill' = bill { cbProgress = p' }
        in (bills { cbsBills = HM.insert bid bill' (cbsBills bills) }
           , Just p')

-- | One craft cycle finished: reset progress and decrement the count.
--   A finite bill that reaches 0 is removed; a repeat bill stays at
--   -1 forever. Returns the new remaining count (0 = bill done and
--   gone), or Nothing for an unknown bill. The claim is kept so the
--   crafter rolls straight into the next cycle without re-claiming.
completeBillCycle ∷ BillId → CraftBills → (CraftBills, Maybe Int)
completeBillCycle bid bills = case lookupBill bid bills of
    Nothing → (bills, Nothing)
    Just bill
        | cbRemaining bill < 0 →
            let bill' = bill { cbProgress = 0 }
            in ( bills { cbsBills = HM.insert bid bill' (cbsBills bills) }
               , Just (-1) )
        | cbRemaining bill ≤ 1 →
            (bills { cbsBills = HM.delete bid (cbsBills bills) }, Just 0)
        | otherwise →
            let bill' = bill { cbProgress = 0
                             , cbRemaining = cbRemaining bill - 1 }
            in ( bills { cbsBills = HM.insert bid bill' (cbsBills bills) }
               , Just (cbRemaining bill') )

-- | Toggle a bill's paused flag. Claim/progress are untouched — pause
--   only gates future claims (see 'claimAvailable'). False for an
--   unknown id.
setBillPaused ∷ BillId → Bool → CraftBills → (CraftBills, Bool)
setBillPaused bid paused bills = case lookupBill bid bills of
    Nothing → (bills, False)
    Just bill →
        let bill' = bill { cbPaused = paused }
        in (bills { cbsBills = HM.insert bid bill' (cbsBills bills) }, True)

-- | #330's manual reorder: swap with the immediate neighbour (in
--   current 'billsForStation' order, at the SAME station) one step up
--   or down. False when the bill is unknown or already at that end of
--   its station's queue.
data ReorderDirection = MoveUp | MoveDown
    deriving (Show, Eq)

reorderBill ∷ ReorderDirection → BillId → CraftBills → (CraftBills, Bool)
reorderBill dir bid bills = case lookupBill bid bills of
    Nothing → (bills, False)
    Just bill →
        let queue = billsForStation (cbStation bill) bills
        in case findIndex ((≡ bid) . cbId) queue of
            Nothing → (bills, False)
            Just i →
                let j = case dir of
                        MoveUp   → i - 1
                        MoveDown → i + 1
                in if j < 0 ∨ j ≥ length queue
                   then (bills, False)
                   else
                       let neighbor  = queue !! j
                           bill'     = bill     { cbSeq = cbSeq neighbor }
                           neighbor' = neighbor { cbSeq = cbSeq bill }
                           inserted  = HM.insert bid bill'
                                     $ HM.insert (cbId neighbor) neighbor'
                                     $ cbsBills bills
                       in (bills { cbsBills = inserted }, True)

-- | Drop bills whose station isn't in the given id set — the save-load
--   defense (a bill whose station's def was deregistered between
--   sessions would otherwise point at nothing forever).
pruneToStations ∷ HS.HashSet BuildingId → CraftBills → CraftBills
pruneToStations stations bills = bills
    { cbsBills = HM.filter ((`HS.member` stations) . cbStation)
                           (cbsBills bills) }
