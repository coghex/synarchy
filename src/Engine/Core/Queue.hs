{-# LANGUAGE Strict #-}
-- | Inter-thread message queue with built-in telemetry (#1910).
--
--   == The telemetry contract
--
--   Every 'Queue' maintains four counters — current depth, cumulative
--   enqueued, cumulative dequeued, and a never-decreasing depth
--   high-water mark — and stamps each element with the monotonic-clock
--   instant it was enqueued at. Both halves are load-bearing:
--
--   * Every counter update happens inside the SAME STM transaction as
--     the enqueue or dequeue that caused it, so an observer can never
--     see a depth that disagrees with the queue's actual contents, and
--     @depth == enqueued - dequeued@ holds at every commit boundary.
--   * 'queueStats' is the ONE read-only accessor. It captures the
--     counters and the oldest undrained element's enqueue instant in a
--     single transaction, so the values in a snapshot cannot disagree
--     with each other the way several independent reads could.
--   * An empty queue reports 'Nothing' for its oldest age, never a zero
--     that a just-enqueued element would be indistinguishable from.
--
--   == What this does NOT do
--
--   Nothing here bounds, drops, coalesces or budgets anything. FIFO
--   ordering, 'readQueue''s blocking, 'readQueueTimeout''s
--   single-transaction dequeue\/timeout race and 'flushQueue''s atomic
--   whole-queue capture are exactly what they were before the counters
--   existed; the counters only observe. Deciding admission policy is
--   the follow-up these measurements exist to inform.
--
--   Instrumentation is unconditional — there is no flag to turn it off,
--   because there is nothing to turn off that would be worth the
--   branch: an enqueue adds one clock read, one machine-word-sized
--   wrapper and one counter update, and nothing on any path logs or
--   builds a label per message.
--
--   == Never force a queued element inside a transaction
--
--   Every binding below that names an element, or a list of them, is
--   explicitly LAZY, and each must stay that way. This module enables
--   @Strict@, so an ordinary binding is a strict pattern; stm, by
--   contrast, deliberately hands back an UNFORCED element — its
--   'readTQueue' takes the read side from @let (z:zs) = reverse ys@,
--   carrying the comment /"NB. lazy: we want the transaction to be
--   short, otherwise it will conflict"/. Forcing such a value inside the
--   transaction runs that @reverse@ there, walking the whole write-side
--   backlog while concurrent producers keep touching the same 'TQueue' —
--   so the transaction can be invalidated repeatedly, or starve,
--   precisely under the overload this telemetry exists to diagnose.
--   Every counter update here is O(1); making one of these bindings
--   strict is what would make a dequeue O(backlog).
module Engine.Core.Queue where

import UPrelude
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM.TQueue
import GHC.Clock (getMonotonicTimeNSec)

-- | One queued element together with the monotonic-clock instant at
--   which 'writeQueue' accepted it.
--
--   The payload field is lazy despite this module's @Strict@ pragma so
--   that the wrapper introduces no forcing of its own. That deliberately
--   changes nothing about WHERE a message is forced: @Strict@ already
--   makes 'writeQueue''s own argument binder strict, exactly as it did
--   before this wrapper existed, so a message still reaches WHNF on its
--   producer's thread for precisely the reason it always did. A strict
--   field would only add a second forcing point behind the first.
data Timestamped α = Timestamped
    { tsEnqueuedAt ∷ Word64
      -- ^ 'getMonotonicTimeNSec', sampled immediately before the write.
    , tsValue      ∷ ~α
    }

-- | A queue's four live counters, held in one 'STM.TVar' so that
--   reading them is a single transactional read.
data QueueCounters = QueueCounters
    { qcDepth     ∷ Int
      -- ^ Elements currently undrained.
    , qcEnqueued  ∷ Word64
      -- ^ Cumulative elements accepted since the queue was created.
    , qcDequeued  ∷ Word64
      -- ^ Cumulative elements removed, by every dequeue path including
      --   'flushQueue' (which removes many at once).
    , qcHighWater ∷ Int
      -- ^ The largest 'qcDepth' ever reached. Monotonically
      --   non-decreasing: draining the queue never lowers it.
    } deriving (Eq, Show)

-- | A queue that has neither accepted nor removed anything.
emptyQueueCounters ∷ QueueCounters
emptyQueueCounters = QueueCounters
    { qcDepth     = 0
    , qcEnqueued  = 0
    , qcDequeued  = 0
    , qcHighWater = 0
    }

-- | Thin wrapper around 'TQueue' for inter-thread communication,
--   carrying its own telemetry.
--
--   'Eq' remains IDENTITY-based, as it was before the counters existed:
--   both fields are mutable cells whose own 'Eq' instances compare
--   identity, and both are created together by 'newQueue'. The
--   capability-aliasing suites compare a projected queue against the
--   live 'Engine.Core.State.EngineEnv' field it came from, which is
--   exactly that comparison.
data Queue α = Queue
    { queueTQueue   ∷ TQueue (Timestamped α)
    , queueCounters ∷ STM.TVar QueueCounters
    } deriving (Eq)

newQueue ∷ IO (Queue α)
newQueue = STM.atomically $ do
    tq       ← newTQueue
    counters ← STM.newTVar emptyQueueCounters
    return (Queue tq counters)

-- | One element accepted: depth rises, and the high-water mark follows
--   it up but never back down.
countEnqueue ∷ QueueCounters → QueueCounters
countEnqueue c = c { qcDepth     = depth'
                   , qcEnqueued  = qcEnqueued c + 1
                   , qcHighWater = max (qcHighWater c) depth'
                   }
  where depth' = qcDepth c + 1

-- | @n@ elements removed. Cumulative counts and the high-water mark are
--   deliberately untouched apart from @dequeued@ — draining is not
--   supposed to erase the evidence that a backlog happened.
countDequeue ∷ Int → QueueCounters → QueueCounters
countDequeue n c = c { qcDepth    = qcDepth c - n
                     , qcDequeued = qcDequeued c + fromIntegral n
                     }

writeQueue ∷ Queue α → α → IO ()
writeQueue q val = do
    -- Sampled just BEFORE the atomic write rather than inside it: the
    -- monotonic clock is IO, and reaching it from STM would need
    -- 'unsafeIOToSTM', whose effect a retry would re-run. The gap is
    -- one uncontended STM commit, and it can only ever make a reported
    -- age very slightly large.
    now ← getMonotonicTimeNSec
    STM.atomically $ do
        writeTQueue (queueTQueue q) (Timestamped now val)
        STM.modifyTVar' (queueCounters q) countEnqueue

-- | Lazy binding: see the module header. The element is forced by
--   whoever consumes the result, outside the transaction — which is
--   where it was forced before the counters existed.
readQueue ∷ Queue α → IO α
readQueue q = STM.atomically $ do
    ~ts ← readTQueue (queueTQueue q)
    STM.modifyTVar' (queueCounters q) (countDequeue 1)
    return (tsValue ts)

-- | An unsuccessful read leaves every counter untouched: the empty
--   branch commits no write at all.
--
--   Deciding WHICH branch to take costs nothing — 'tryReadTQueue' wraps
--   the element in a 'Just' without forcing it — so only the element
--   binder needs the module header's lazy treatment.
tryReadQueue ∷ Queue α → IO (Maybe α)
tryReadQueue q = STM.atomically $ do
    mts ← tryReadTQueue (queueTQueue q)
    case mts of
      Nothing  → return Nothing
      Just ~ts → do
          STM.modifyTVar' (queueCounters q) (countDequeue 1)
          return (Just (tsValue ts))

-- | 'tryReadQueue', handing back the element still WRAPPED in its
--   enqueue stamp so 'unreadQueue' can put it back unchanged.
--
--   The pair exists for one consumer-side pattern: a drain that cannot
--   decide whether it may run a message until it has looked at it, and
--   must leave a message it declines exactly where it was
--   ("World.Thread"'s Exit-to-Menu fence, #2291). Deciding INSIDE the
--   transaction is not an option — the predicate would force the
--   element there, which is the O(backlog) hazard the module header
--   forbids — so the decision happens between two transactions and
--   'unreadQueue' repairs the queue afterwards.
--
--   __Single-consumer only.__ Another consumer dequeuing between the two
--   calls would see the queue without the withheld element. Every
--   'Queue' here is drained by exactly one thread, and both calls must
--   be made by it.
tryReadQueueStamped ∷ Queue α → IO (Maybe (Timestamped α))
tryReadQueueStamped q = STM.atomically $ do
    mts ← tryReadTQueue (queueTQueue q)
    case mts of
      Nothing  → return Nothing
      Just ~ts → do
          STM.modifyTVar' (queueCounters q) (countDequeue 1)
          return (Just ts)

-- | Put an element taken by 'tryReadQueueStamped' back at the FRONT of
--   the queue, carrying the enqueue instant it already had.
--
--   The front, not the tail, is what makes this order-preserving: a
--   producer that appended while the element was withheld lands BEHIND
--   it, which is where it belonged all along — it was accepted later.
--   Re-queuing through 'writeQueue' instead would let that producer
--   overtake, and flushing-and-rewriting the whole queue would do the
--   same in a wider window.
--
--   The stamp is carried rather than resampled so a withheld message's
--   reported age keeps counting from when the queue actually accepted
--   it; a deferral must not make a backlog look younger than it is.
unreadQueue ∷ Queue α → Timestamped α → IO ()
unreadQueue q ts = STM.atomically $ do
    unGetTQueue (queueTQueue q) ts
    STM.modifyTVar' (queueCounters q) countEnqueue

-- | Read with a timeout (microseconds). Don't wrap 'readQueue' in
--   'System.Timeout.timeout' instead: its exception can arrive after
--   the STM dequeue commits, silently dropping the message. Here the
--   dequeue and the timeout race inside a single transaction.
--
--   The counter update rides in that same transaction, on the dequeue
--   side of the race only, so a timed-out call cannot record a
--   dequeue that never happened: 'STM.orElse' discards the first
--   branch's writes entirely when it retries.
readQueueTimeout ∷ Int → Queue α → IO (Maybe α)
readQueueTimeout micros q = do
    delayVar ← STM.registerDelay micros
    STM.atomically $ STM.orElse
        (do ~ts ← readTQueue (queueTQueue q)
            STM.modifyTVar' (queueCounters q) (countDequeue 1)
            return (Just (tsValue ts)))
        (do timedOut ← STM.readTVar delayVar
            STM.check timedOut
            return Nothing)

-- | Atomically remove and return the whole queue. Counts every removed
--   element as dequeued, which is what keeps @depth == enqueued -
--   dequeued@ true for the five queues "World.Load.Publish" flushes
--   when it replaces a session. An empty flush commits no counter
--   write.
--
--   The number removed is taken from 'qcDepth' rather than by measuring
--   the drained list, and that is a correctness AND a cost decision.
--   Correctness: every enqueue and every dequeue moves 'qcDepth' inside
--   the same transaction as the queue mutation itself, so within THIS
--   transaction the counter is exactly the number of elements
--   'flushTQueue' just captured. Cost: the whole point is that the
--   transaction stays O(1) in the backlog, exactly as it was before the
--   counters existed.
--
--   Which is why the flushed list is bound LAZILY. Under this module's
--   @Strict@ pragma a plain @items ←@ is a strict pattern, and stm
--   builds 'flushTQueue''s result as @xs ++ reverse ys@ — so on the
--   ordinary shape for a queue that has only been written to (an empty
--   read side), forcing even its WHNF walks the entire write-side
--   backlog inside the transaction. Concurrent 'writeQueue's touch that
--   same 'TQueue', so a transaction that long can be invalidated
--   repeatedly, or starve, precisely under the producer overload this
--   telemetry exists to diagnose. With the lazy binding the transaction
--   does one TVar read and at most one TVar write of its own, and every
--   list cell is built outside STM by whoever consumes the result —
--   which is where it was built before, too.
flushQueue ∷ Queue α → IO [α]
flushQueue q = STM.atomically $ do
    ~items   ← flushTQueue (queueTQueue q)
    counters ← STM.readTVar (queueCounters q)
    when (qcDepth counters > 0) $
        STM.writeTVar (queueCounters q)
                      (countDequeue (qcDepth counters) counters)
    return (map tsValue items)

-- | Everything one queue reports about itself, as of a single atomic
--   observation.
data QueueStats = QueueStats
    { qsDepth       ∷ Int
    , qsEnqueued    ∷ Word64
    , qsDequeued    ∷ Word64
    , qsHighWater   ∷ Int
    , qsOldestAgeNs ∷ Maybe Word64
      -- ^ How long the oldest undrained element has been waiting, in
      --   nanoseconds. 'Nothing' — never @Just 0@ — when the queue is
      --   empty, so "nothing is waiting" stays distinguishable from
      --   "something was enqueued a moment ago".
    } deriving (Eq, Show)

-- | The atomic half of a snapshot: the counters and the oldest
--   undrained element's enqueue instant, read in ONE transaction so
--   they cannot disagree with each other or with the queue's contents.
--
--   No element enters or leaves: 'tryPeekTQueue' puts back exactly what
--   it took, unforced. The head is handed BACK rather than reduced to
--   its timestamp here, because reading that field would force the
--   element — see the module header. 'queueStats' extracts it once the
--   transaction has committed.
readQueueSampleSTM ∷ Queue α → STM.STM (QueueCounters, Maybe (Timestamped α))
readQueueSampleSTM q = do
    counters ← STM.readTVar (queueCounters q)
    oldest   ← tryPeekTQueue (queueTQueue q)
    return (counters, oldest)

-- | The head's enqueue instant, keeping no reference to its payload.
--   Forces the element, so it belongs OUTSIDE any transaction.
oldestEnqueuedAt ∷ Maybe (Timestamped α) → Maybe Word64
oldestEnqueuedAt Nothing   = Nothing
oldestEnqueuedAt (Just ts) = let at = tsEnqueuedAt ts in Just at

-- | The one read-only accessor: a whole 'QueueStats' from a single STM
--   transaction, with the age derived afterwards.
--
--   Splitting it that way is deliberate. The transaction captures the
--   counters and the oldest enqueue instant together — that is the part
--   that has to be consistent — and the monotonic clock, which is IO
--   and would need 'unsafeIOToSTM' from inside, is sampled immediately
--   after the commit. Because the clock is read AFTER the observation,
--   'qsOldestAgeNs' is the age as of the moment this call returned and
--   can never be negative.
queueStats ∷ Queue α → IO QueueStats
queueStats q = do
    (counters, oldest) ← STM.atomically (readQueueSampleSTM q)
    now ← getMonotonicTimeNSec
    let oldestAt = oldestEnqueuedAt oldest
    return QueueStats
        { qsDepth       = qcDepth counters
        , qsEnqueued    = qcEnqueued counters
        , qsDequeued    = qcDequeued counters
        , qsHighWater   = qcHighWater counters
        , qsOldestAgeNs = ageSince now ⊚ oldestAt
        }

-- | Monotonic-clock difference, floored at zero. 'getMonotonicTimeNSec'
--   never goes backwards, so the floor is purely a guard against a
--   'Word64' underflow turning a hypothetical inversion into a nonsense
--   age of some six hundred years.
ageSince ∷ Word64 → Word64 → Word64
ageSince now enqueuedAt
    | now ≥ enqueuedAt = now - enqueuedAt
    | otherwise        = 0

-- | A type-erased, READ-ONLY handle to one queue's telemetry: enough to
--   snapshot the queue, and nothing that could enqueue, dequeue, or so
--   much as look at an element. That erasure is what lets a single
--   inventory list carry queues whose element types all differ.
newtype QueueStatsSource = QueueStatsSource
    { runQueueStatsSource ∷ IO QueueStats }

queueStatsSource ∷ Queue α → QueueStatsSource
queueStatsSource q = QueueStatsSource (queueStats q)

-- | One entry in a telemetry inventory: a queue paired with a stable,
--   low-cardinality identifier for the QUEUE itself. A name identifies
--   the declaration, never anything that passed through it — no element
--   contents, arguments or entity ids ever appear in one.
data NamedQueue = NamedQueue
    { nqName   ∷ Text
    , nqSource ∷ QueueStatsSource
    }

namedQueue ∷ Text → Queue α → NamedQueue
namedQueue name q = NamedQueue name (queueStatsSource q)

-- | Snapshot a whole inventory, in the order given.
--
--   Each queue's own snapshot is atomic; the inventory as a whole is
--   deliberately NOT one transaction. Reading every queue in a single
--   transaction would make it retry whenever any of them changed —
--   which on a live engine is continuously — and the consistency that
--   matters is per queue.
queueInventoryStats ∷ [NamedQueue] → IO [(Text, QueueStats)]
queueInventoryStats = mapM $ \nq → do
    stats ← runQueueStatsSource (nqSource nq)
    return (nqName nq, stats)
