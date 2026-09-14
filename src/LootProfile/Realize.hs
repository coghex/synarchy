{-# LANGUAGE Strict #-}
-- | Deterministic loot-profile REALIZATION (#2502, epic #1231 PLC-13;
--   design authority @docs/portable_loot_containers.md@ D-3, D-6, D-19,
--   D-22, D-23).
--
--   A pending portable container becomes a real cargo tree exactly
--   once, and the result must be the same in any process and any
--   chunk\/location load order. That is what this module is: given a
--   realization CONTEXT — world seed, location-instance id, slot — a
--   'LootProfileDef' and the shell 'ItemInstance' it populates, it
--   answers the same contents every time.
--
--   __No caller-supplied generator.__ Every stream is derived from the
--   context alone through "LootTable.Roll"'s written-out 'mixFold', so
--   the interface exposes no RNG parameter at all: a caller that could
--   hand one in could make the result depend on its own state, which is
--   exactly the property D-3 forbids. Per-lot 'StdGen's are built
--   INSIDE this module from context-derived seeds and thrown away.
--
--   __Deterministic computation with isolated effects, not purity.__
--   'Item.Materialize.materializeItem' is the one mint boundary and it
--   is in 'IO': it needs an 'ItemManager', a 'LoggerState' and an
--   instance-id allocator. This module therefore runs in 'IO' too, but
--   the only effects it performs are the materializer's own — it reads
--   no shared stat RNG, no process entropy, and touches the caller's
--   REAL allocator only while committing an admitted lot.
--
--   __Candidate, then commit__ (requirement 5). PLC-4's admission
--   decision needs a lot's exact tree, quality, weight, fill, nesting,
--   recursive weight and direct-child bulk — all of which the
--   materializer ROLLS. So each lot is materialized twice from the
--   IDENTICAL per-lot seed: once with a local allocator, purely to
--   decide admission, and again with the caller's real allocator only
--   if that decision was "admit". Committed physical values therefore
--   equal the values that were capacity-tested, and a rejected lot
--   consumes no real id and cannot shift what a later lot rolls.
--
--   __Admission goes through "Item.Ownership"__ ('insertInstance'),
--   never through a second copy of its capacity arithmetic. A lot is
--   @quantity_factor@ instances and is atomic: every instance is
--   offered to that boundary in turn against the accumulating shell,
--   and the lot is admitted only if ALL of them are accepted. A partial
--   lot is never committed.
module LootProfile.Realize
    ( -- * The realization context
      RealizeContext(..)
      -- * Refusals and per-lot verdicts
    , RealizeRefusal(..)
    , realizeRefusalId
    , LotVerdict(..)
    , LotReport(..)
    , RealizeReport(..)
    , RealizeResult(..)
    , reportAdmitted
    , reportRejected
      -- * Realization
    , realizeLootProfile
      -- * The derivation, exposed for the simulation and its fixtures
    , RealizeStream(..)
    , realizeGen
    , Lot(..)
    , proposedLots
    , shuffleLots
    , admitCandidates
    , localAllocator
    ) where

import UPrelude
import Control.Monad (foldM)
import Data.IORef (newIORef, atomicModifyIORef')
import System.Random (StdGen, mkStdGen)
import Engine.Core.Log (LoggerState)
import Item.Materialize (materializeItem, pristineItem)
import Item.Ownership
    ( OwnershipRefusal(..), OwnershipScene(..), RootCapacity(..)
    , insertInstance )
import Item.Types
    ( ItemInstance(..), ItemManager, itemTotalWeight, lookupItemDef )
import LootProfile.Types (LootProfileDef(..), LootProfileEntry(..))
import LootTable.Roll (mixFold, unitFromHash)

-- | The stable identity of ONE realization (design D-3). Every
--   component is durable state, so the same shell realizes identically
--   in any process and in any load order:
--
--     * 'rcWorldSeed' — the world page's persisted generation seed
--       (@wgpSeed@, exposed as @world.getSeed()@);
--     * 'rcInstanceId' — the placed location's stable instance id
--       (#911), the @Int@ inside @Location.Instance.LocationInstanceId@;
--     * 'rcSlot' — which container of that location this is. The
--       positional index of the content source, for the same reason
--       'LootTable.Roll.lrcEntryIndex' is positional: one location may
--       hold several crates sharing a profile, and those must not share
--       a stream.
--
--   Where the slot comes from is PLC-14\/PLC-15's (the pending
--   descriptor); this module only requires that it is stable.
data RealizeContext = RealizeContext
    { rcWorldSeed  ∷ !Int
    , rcInstanceId ∷ !Int
    , rcSlot       ∷ !Int
    } deriving (Show, Eq)

-- | Why a realization could not be attempted AT ALL — as distinct from
--   a realization that ran and admitted nothing (requirement 6, design
--   D-23). A refusal changes no contents and allocates no real
--   instance id, and PLC-15 refuses the pickup on one; "the crate came
--   up empty" and "the crate cannot be realized" must never collapse
--   into one outcome.
data RealizeRefusal
    = ShellNotStorage
      -- ^ The shell's 'iiStorage' is 'Nothing': it declares no internal
      --   capacity, so it accepts nothing (the same fail-closed reading
      --   'Item.Ownership.TargetNotStorage' has).
    | UnknownEntryItem !Text
      -- ^ A profile entry names an item definition absent from the
      --   supplied 'ItemManager'. #2499 rejects such a profile at load,
      --   so this is not expected to occur — but it is reported rather
      --   than panicked on, and it refuses the WHOLE realization rather
      --   than silently dropping that entry, because a profile whose
      --   calibration is partly unreachable is not a distribution
      --   anybody authored.
    deriving (Show, Eq)

-- | The stable external spelling of a refusal, in the shape
--   'Item.Ownership.ownershipRefusalId' already established.
realizeRefusalId ∷ RealizeRefusal → Text
realizeRefusalId r = case r of
    ShellNotStorage   → "shell_not_storage"
    UnknownEntryItem _ → "unknown_entry_item"

-- | What happened to one proposed lot.
data LotVerdict
    = LotAdmitted
      -- ^ Every instance in the lot cleared 'insertInstance' and the
      --   whole lot is in the shell.
    | LotRejected !OwnershipRefusal
      -- ^ The lot did not fit, reported with the boundary's own reason
      --   for the first instance that was refused. Nothing of the lot
      --   was committed, and nothing already admitted was evicted.
    | LotUnmintable
      -- ^ The materializer answered 'Nothing' for this lot's item.
      --   Unreachable while the 'ItemManager' handed to this call is
      --   the one the entry check above passed against — it is kept
      --   because a lot that could not be minted must not read as an
      --   admitted empty one.
    deriving (Show, Eq)

-- | One proposed lot and its outcome. Plain values throughout (no
--   'Data.IORef.IORef', no engine handle) so a fixed-vector test can
--   compare a whole report structurally.
data LotReport = LotReport
    { lrEntryIndex ∷ !Int
      -- ^ 1-based index of the profile entry that proposed this lot, in
      --   AUTHORED order.
    , lrItem       ∷ !Text
    , lrLotIndex   ∷ !Int
      -- ^ 1-based index of this lot among the ones that entry proposed
      --   (an entry rolling a multiplier of 3 proposes lots 1, 2, 3).
      --   Together with 'lrEntryIndex' this is the STABLE lot identity
      --   the per-lot seed is derived from, so shuffling reorders which
      --   lot is considered first without changing what any lot holds.
    , lrCandidates ∷ ![ItemInstance]
      -- ^ The candidate tree(s) — @quantity_factor@ of them — exactly as
      --   materialized for the admission decision. Their instance ids
      --   are LOCAL scratch values; an admitted lot's committed trees
      --   carry real ids and are otherwise identical, which is what
      --   requirement 5 promises. Kept in the report because the
      --   simulation's saturation measure must re-test a REJECTED lot
      --   against an empty shell without rerolling it.
    , lrVerdict    ∷ !LotVerdict
    } deriving (Show, Eq)

-- | Every lot the profile proposed, in the SHUFFLED order they were
--   considered in — which is the order that decided them.
newtype RealizeReport = RealizeReport
    { rpLots ∷ [LotReport]
    } deriving (Show, Eq)

-- | The lots that were admitted, in consideration order.
reportAdmitted ∷ RealizeReport → [LotReport]
reportAdmitted rep = [ l | l ← rpLots rep, lrVerdict l ≡ LotAdmitted ]

-- | The lots that were NOT admitted, in consideration order.
reportRejected ∷ RealizeReport → [LotReport]
reportRejected rep = [ l | l ← rpLots rep, lrVerdict l ≢ LotAdmitted ]

-- | A realization either refuses outright or completes with a report.
--   An empty 'rpLots', or one holding only rejections, is a SUCCESS —
--   the crate genuinely came up empty — and is a different answer from
--   'RealizeRefused'.
data RealizeResult
    = RealizeRefused !RealizeRefusal
    | RealizeDone !ItemInstance !RealizeReport
    deriving (Show, Eq)

-- * Stream derivation

-- | Which draw a hash is for. Folded into the context so two streams
--   can never collide: without it, the appearance roll for entry 2 and
--   the multiplier roll for entry 2 would be the same number.
data RealizeStream
    = StreamAppearance
    | StreamMultiplier
    | StreamShuffle
    | StreamLot
    | StreamShell
      -- ^ Not used here: it is the stream the SIMULATION mints its
      --   sample shell from ("LootProfile.Simulate"). It lives in this
      --   enumeration so the one tag space is declared in one place and
      --   cannot be given two meanings.
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Fold the context, the stream tag and that stream's own coordinates
--   into one well-mixed word, through "LootTable.Roll"'s exact
--   finalizer. The tag is absorbed AFTER the three context components
--   and before the coordinates, and that order is part of the pinned
--   contract.
realizeHash ∷ RealizeContext → RealizeStream → [Int] → Word64
realizeHash ctx stream coords = mixFold $
    [ fromIntegral (rcWorldSeed ctx)
    , fromIntegral (rcInstanceId ctx)
    , fromIntegral (rcSlot ctx)
    , fromIntegral (fromEnum stream)
    ] ⧺ map fromIntegral coords

-- | That hash's draw in [0, 1), through the same top-24-bit conversion
--   'LootTable.Roll.lootRollUnit' uses. This is the STABLE appearance,
--   multiplier and shuffle draw; it is deliberately NOT claimed to be
--   the 'StdGen' algorithm 'materializeItem' samples physical values
--   with (see 'realizeGen').
realizeUnit ∷ RealizeContext → RealizeStream → [Int] → Float
realizeUnit ctx stream = unitFromHash ∘ realizeHash ctx stream

-- | A 'StdGen' seeded from that hash. This is the OTHER half of
--   requirement 3's distinction: the context-to-seed mapping is pinned
--   here, and what the generator then does with the seed belongs to
--   @random@ and to "Item.Roll".
realizeGen ∷ RealizeContext → RealizeStream → [Int] → StdGen
realizeGen ctx stream = mkStdGen ∘ fromIntegral ∘ realizeHash ctx stream

-- * Proposal

-- | One atomic generation lot: @lotSize@ instances of @lotItem@, all
--   admitted or all rejected.
data Lot = Lot
    { lotEntryIndex ∷ !Int
    , lotLotIndex   ∷ !Int
    , lotItem       ∷ !Text
    , lotSize       ∷ !Int
    } deriving (Show, Eq)

-- | The lots a profile proposes for one context, in AUTHORED entry
--   order (requirement 3's draw contract):
--
--     1. every entry's appearance is rolled, in authored order;
--     2. every entry that appeared rolls its OWN quantity multiplier,
--        in that same order (design population step 4 — one multiplier
--        per successful entry, not one shared by the profile);
--     3. that entry contributes @multiplier@ lots of
--        @quantity_factor@ instances each, which is exactly the
--        @quantity_factor × multiplier@ proposal expressed as
--        factor-sized atoms.
--
--   The draws are indexed by the entry's authored position, so an entry
--   that does NOT appear still consumes its own multiplier index and
--   cannot shift a later entry's numbers.
proposedLots ∷ RealizeContext → LootProfileDef → [Lot]
proposedLots ctx profile = concatMap forEntry (zip [1 ..] (lpdEntries profile))
  where
    forEntry (ix, e)
        | realizeUnit ctx StreamAppearance [ix] < lpeChance e =
            [ Lot { lotEntryIndex = ix
                  , lotLotIndex   = k
                  , lotItem       = lpeItem e
                  , lotSize       = lpeQuantityFactor e }
            | k ← [1 .. multiplierFor ctx profile ix] ]
        | otherwise = []

-- | The quantity multiplier for the entry at this authored index, drawn
--   from the profile's inclusive @{min, max}@ range. The loader already
--   guarantees @1 ≤ min ≤ max@; the 'max' guard below keeps a
--   hand-built def from dividing by zero rather than restating that
--   rule.
multiplierFor ∷ RealizeContext → LootProfileDef → Int → Int
multiplierFor ctx profile ix =
    lpdMultiplierMin profile + min (width - 1) (floor (u * fromIntegral width))
  where
    width = max 1 (lpdMultiplierMax profile - lpdMultiplierMin profile + 1)
    u     = realizeUnit ctx StreamMultiplier [ix]

-- | The seeded shuffle that decides admission priority, and the ONLY
--   admission priority there is (design D-6): a selection shuffle that
--   draws the next element uniformly from what is left, indexed by
--   step. Authored order, item weight, bulk and rarity are inputs to
--   the shuffle and to nothing else.
shuffleLots ∷ RealizeContext → [Lot] → [Lot]
shuffleLots ctx = go 0
  where
    go step lots = case lots of
        [] → []
        _  →
            let remaining = length lots
                u         = realizeUnit ctx StreamShuffle [step]
                ix        = min (remaining - 1)
                                (floor (u * fromIntegral remaining))
            in case splitAt ix lots of
                (before, picked : after) →
                    picked : go (step + 1) (before ⧺ after)
                -- Unreachable: @ix@ is clamped to @remaining - 1@ and
                -- the empty list is matched above.
                (before, [])             → go (step + 1) before

-- * Realization

-- | Realize one profile into one shell.
--
--   The shell is returned with its own identity, physical values and
--   'iiStorage' untouched and its EXISTING children preserved in order
--   (design D-22): admitted lots are appended to whatever it already
--   held, and the capacity they are admitted against is whatever those
--   authored contents left.
realizeLootProfile
    ∷ ItemManager
    → LoggerState    -- ^ where 'materializeItem' reports a cyclic graph
    → IO Word64      -- ^ the caller's REAL instance-id allocator
    → RealizeContext
    → LootProfileDef
    → ItemInstance   -- ^ the shell
    → IO RealizeResult
realizeLootProfile itemMgr logger allocId ctx profile shell
    | isNothing (iiStorage shell) = pure (RealizeRefused ShellNotStorage)
    | (missing : _) ← unresolvedEntries =
        pure (RealizeRefused (UnknownEntryItem missing))
    | otherwise = do
        (shell', reports) ←
            foldM step (shell, []) (shuffleLots ctx (proposedLots ctx profile))
        pure (RealizeDone shell' (RealizeReport (reverse reports)))
  where
    unresolvedEntries =
        [ lpeItem e
        | e ← lpdEntries profile
        , isNothing (lookupItemDef (lpeItem e) itemMgr) ]

    step (acc, reports) lot = do
        mCands ← mintLot itemMgr logger (localAllocatorFor acc) ctx lot
        case mCands of
            Nothing    → pure (acc, noted lot [] LotUnmintable : reports)
            Just cands → case admitCandidates itemMgr acc cands of
                Left refusal →
                    pure (acc, noted lot cands (LotRejected refusal) : reports)
                Right _ → do
                    -- Admitted on the candidate values, so mint the lot
                    -- AGAIN from the identical per-lot seed, this time
                    -- with the caller's REAL allocator, and commit that
                    -- tree. The two mints differ only in the instance
                    -- ids they allocate.
                    mReal ← mintLot itemMgr logger (pure allocId) ctx lot
                    case mReal of
                        -- Both branches below are unreachable while the
                        -- two mints agree — no capacity bound reads an
                        -- id, and the real ids are fresh. They keep the
                        -- whole lot rolled back rather than partly
                        -- committed if that ever stops being true.
                        Nothing →
                            pure (acc, noted lot cands LotUnmintable : reports)
                        Just real → case admitCandidates itemMgr acc real of
                            Left refusal →
                                pure ( acc
                                     , noted lot cands (LotRejected refusal)
                                         : reports )
                            Right acc' →
                                pure ( acc'
                                     , noted lot cands LotAdmitted : reports )

    noted lot cands verdict = LotReport
        { lrEntryIndex = lotEntryIndex lot
        , lrItem       = lotItem lot
        , lrLotIndex   = lotLotIndex lot
        , lrCandidates = cands
        , lrVerdict    = verdict
        }

-- | Materialize one whole lot from its per-lot seed. The
--   @quantity_factor@ instances are drawn IN SEQUENCE from ONE
--   generator, so the lot's contents are a function of the lot's
--   identity and of nothing else — in particular not of how many lots
--   were considered before it.
--
--   'Nothing' when any instance failed to mint; see 'LotUnmintable'.
mintLot
    ∷ ItemManager → LoggerState → IO (IO Word64) → RealizeContext
    → Lot → IO (Maybe [ItemInstance])
mintLot itemMgr logger mkAlloc ctx lot = do
    rngRef ← newIORef (realizeGen ctx StreamLot
                           [lotEntryIndex lot, lotLotIndex lot])
    alloc  ← mkAlloc
    sequence <$> replicateM (max 0 (lotSize lot))
        (materializeItem itemMgr logger rngRef alloc pristineItem
                         (lotItem lot))

-- | Offer a whole lot to PLC-4's boundary against the shell as it
--   currently stands, one instance at a time, and answer the shell
--   holding ALL of them or the boundary's own refusal for the first one
--   that did not fit.
--
--   Sequential insertion IS the aggregate check: each instance is
--   measured against the shell already holding its predecessors, so a
--   lot only passes when the lot as a whole fits. Nothing is written
--   anywhere until the caller takes the returned shell, so a rejection
--   leaves the real shell untouched by construction rather than by
--   rollback.
admitCandidates
    ∷ ItemManager → ItemInstance → [ItemInstance]
    → Either OwnershipRefusal ItemInstance
admitCandidates itemMgr = foldM one
  where
    one shell candidate = do
        items' ← insertInstance (sceneFor itemMgr shell)
                                (iiInstanceId shell) candidate
        case items' of
            [shell'] → Right shell'
            -- Unreachable: the scene holds exactly one root and
            -- 'insertInstance' rewrites it in place.
            _        → Left NoSuchTarget

-- | The one-root scene a realization measures against: the shell is the
--   whole tree, the owner carries nothing else, and there is no root
--   owner yet — a shell being populated is not in anybody's inventory,
--   so 'RootUnlimited' is the honest reading and the shell's own
--   'iiStorage' is what actually bounds it.
sceneFor ∷ ItemManager → ItemInstance → OwnershipScene
sceneFor itemMgr shell = OwnershipScene
    { oscItems    = [shell]
    , oscOther    = []
    , oscCapacity = RootUnlimited
    , oscWeigh    = itemTotalWeight itemMgr
    }

-- | A scratch instance-id allocator for CANDIDATE materialization,
--   handing out ids above every id already in the given tree.
--
--   Candidate ids never reach the shell, but they are still compared
--   against it: 'insertInstance' refuses a candidate carrying an id the
--   destination tree already holds, so a scratch counter starting at
--   zero would report a spurious 'DuplicateInstanceId' against a shell
--   whose authored contents happen to occupy low ids.
localAllocator ∷ Word64 → IO (IO Word64)
localAllocator start = do
    ref ← newIORef start
    pure (atomicModifyIORef' ref (\n → (n + 1, n)))

-- | 'localAllocator' positioned above this tree.
localAllocatorFor ∷ ItemInstance → IO (IO Word64)
localAllocatorFor shell = localAllocator (1 + maxTreeId shell)

-- | The largest instance id anywhere in a tree, its root included.
maxTreeId ∷ ItemInstance → Word64
maxTreeId i = foldl' (\acc c → max acc (maxTreeId c)) (iiInstanceId i)
                     (iiContents i)
