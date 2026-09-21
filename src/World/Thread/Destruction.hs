{-# LANGUAGE Strict #-}
-- | Expiry of the structure teardown presentations #2491 captures.
--
--   Its own step on the world tick, deliberately, and not part of any
--   render pass: requirement 7 is that an effect expires whether or not
--   anything ever drew it. A page can be hidden, its chunk can have been
--   evicted since the capture, the camera can be looking elsewhere, and
--   a headless session has no texture system at all — in every one of
--   those states the clip still runs out on the game clock and the
--   effect still goes.
--
--   It is also why this walks @wmWorlds@ rather than @wmVisible@. The
--   world clock tick ('World.Thread.Time.tickWorldTime') advances only
--   VISIBLE pages, which is correct for time-of-day and wrong for this:
--   a hidden page's effects were started against a clock that is now
--   frozen, so they neither advance nor expire while it is hidden, and
--   the moment it is shown again they resume at their own phase. The
--   pruning has to see every page for that to hold — a page that is
--   hidden and then destroyed must not be the only reason its effects
--   were never collected.
module World.Thread.Destruction
    ( pruneStructureDestructions
    ) where

import UPrelude
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Structure.Destruction
    (anyDestructionExpired, pruneExpiredDestructionEffects)
import World.Types

-- | Drop every page's out-of-time teardown effects.
--
--   Read-then-write on purpose, per page: the render thread reads these
--   refs every frame, and the overwhelmingly common state is a page with
--   no effects at all, so a tick must not pay for a write it has no
--   reason to make. The read and the write are separate, which is safe
--   because the world thread is the only writer of expiry — a capture
--   landing in between is this thread's own, and an effect captured
--   after the read is simply collected on the next tick rather than
--   dropped early.
pruneStructureDestructions ∷ EngineEnv → IO ()
pruneStructureDestructions env = do
    let worldSim = toWorldSimCapability env
    manager ← readIORef (wsWorldManagerRef worldSim)
    now     ← readIORef (wsGameTimeRef worldSim)
    forM_ (wmWorlds manager) $ \(_, worldState) → do
        effects ← readIORef (wsStructureDestructionsRef worldState)
        when (anyDestructionExpired now effects) $
            atomicModifyIORef' (wsStructureDestructionsRef worldState) $ \es →
                (pruneExpiredDestructionEffects now es, ())
