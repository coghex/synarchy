-- | @debug.getQueueStats()@ — the read-only queue-telemetry query
--   (#1910), registered on the @debug@ table beside the other
--   diagnostic verbs.
--
--   It reaches the queues through 'engineQueueInventory', a narrow
--   read-only projection: this module never sees the wider
--   'Engine.Core.State.EngineEnv' record, and the inventory hands back
--   type-erased 'Q.QueueStatsSource' handles that can snapshot a queue
--   and do nothing else — not enqueue, not dequeue, not read an
--   element. Observing the queues cannot disturb them.
--
--   The query needs no world, no GPU and no gameplay thread: it reads
--   the queue objects the environment was built with, so it answers on
--   a bare @--headless@ boot before anything has been loaded, which is
--   exactly when a backlog diagnostic is most wanted.
module Engine.Scripting.Lua.API.QueueStats
  ( getQueueStatsFn
  ) where

import UPrelude
import Engine.Core.State (EngineEnv, engineQueueInventory)
import qualified Engine.Core.Queue as Q
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua

-- | @debug.getQueueStats() → array of@
--   @{name, depth, enqueued, dequeued, highWater[, oldestAgeSeconds]}@
--
--   One dense-array row per named engine queue, in
--   'engineQueueInventory' order. Each row's four counters come from a
--   single atomic observation of that queue, so @depth == enqueued -
--   dequeued@ holds within a row; rows are independent snapshots of
--   independent queues and are not claimed to be simultaneous with each
--   other.
--
--   @oldestAgeSeconds@ is a non-negative monotonic-clock duration in
--   seconds and is ABSENT for an empty queue rather than zero — a
--   just-enqueued element legitimately reports an age near zero, so a
--   zero sentinel would be ambiguous exactly where it mattered.
getQueueStatsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
getQueueStatsFn env = do
    rows ← Lua.liftIO $ Q.queueInventoryStats (engineQueueInventory env)
    Lua.newtable
    forM_ (zip [1 ..] rows) $ \(i, (name, stats)) → do
        pushQueueStatsRow name stats
        Lua.rawseti (-2) i
    return 1

pushQueueStatsRow ∷ Text → Q.QueueStats → Lua.LuaE Lua.Exception ()
pushQueueStatsRow name stats = do
    Lua.newtable
    Lua.pushstring (TE.encodeUtf8 name)
    Lua.setfield (-2) "name"
    Lua.pushinteger (fromIntegral (Q.qsDepth stats))
    Lua.setfield (-2) "depth"
    Lua.pushinteger (fromIntegral (Q.qsEnqueued stats))
    Lua.setfield (-2) "enqueued"
    Lua.pushinteger (fromIntegral (Q.qsDequeued stats))
    Lua.setfield (-2) "dequeued"
    Lua.pushinteger (fromIntegral (Q.qsHighWater stats))
    Lua.setfield (-2) "highWater"
    forM_ (Q.qsOldestAgeNs stats) $ \ageNs → do
        Lua.pushnumber (Lua.Number (realToFrac (ageSeconds ageNs)))
        Lua.setfield (-2) "oldestAgeSeconds"

-- | A nanosecond age as the seconds the Lua row reports. Non-negative
--   by construction: 'Q.qsOldestAgeNs' is an unsigned monotonic-clock
--   difference that 'Q.ageSince' has already floored at zero.
ageSeconds ∷ Word64 → Double
ageSeconds ageNs = fromIntegral ageNs / 1.0e9
