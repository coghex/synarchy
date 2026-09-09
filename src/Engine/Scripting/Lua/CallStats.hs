-- | Runtime-local, inclusive Lua-to-Haskell action telemetry (#2483).
-- The Lua thread owns this ref; registration closures retain it until the
-- runtime closes. Neither arguments nor entity ids enter the key space.
module Engine.Scripting.Lua.CallStats
  ( LuaCallStats
  , CallStat(..)
  , CallSnapshot(..)
  , newLuaCallStats
  , readLuaCallStats
  , resetLuaCallStats
  , withLuaCallStats
  ) where

import UPrelude
import qualified Control.Monad.Catch as Catch
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import GHC.Clock (getMonotonicTimeNSec)
import qualified HsLua as Lua

data CallStat = CallStat
    { callCount ∷ !Integer
    , callTotalDurationNs ∷ !Integer
    , callMaxDurationNs ∷ !Integer
    } deriving (Eq, Show)

data CallSnapshot = CallSnapshot
    { callSequence ∷ !Integer
    , callVerbs ∷ !(Map.Map BS.ByteString CallStat)
    } deriving (Eq, Show)

-- The reset epoch differs from sequence: nested completions advance sequence
-- without invalidating the outer call, whereas reset invalidates all old calls.
data CallWindow = CallWindow !Integer !CallSnapshot
newtype LuaCallStats = LuaCallStats (IORef CallWindow)

newLuaCallStats ∷ IO LuaCallStats
newLuaCallStats = LuaCallStats <$> newIORef (CallWindow 0 (CallSnapshot 0 Map.empty))

readLuaCallStats ∷ LuaCallStats → IO CallSnapshot
readLuaCallStats (LuaCallStats ref) = do
    CallWindow _ snapshot ← readIORef ref
    pure snapshot

resetLuaCallStats ∷ LuaCallStats → IO ()
resetLuaCallStats (LuaCallStats ref) = modifyIORef' ref $
    \(CallWindow epoch snapshot) → CallWindow (epoch + 1)
        (CallSnapshot (callSequence snapshot + 1) Map.empty)

-- | Record exactly once on return or unwind. Mask only bookkeeping; the
-- action keeps its incoming masking state and cancellation responsiveness.
-- Time includes nested callbacks and waits, so nested totals may overlap;
-- it is neither isolated crossing overhead nor downstream queued work.
withLuaCallStats ∷ LuaCallStats → BS.ByteString → Lua.LuaE Lua.Exception α
                 → Lua.LuaE Lua.Exception α
withLuaCallStats stats@(LuaCallStats ref) name action
    | name ≡ "debug.getLuaCallStats" ∨ name ≡ "debug.resetLuaCallStats" = action
    | otherwise = Catch.mask $ \restore → do
        CallWindow epoch _ ← Lua.liftIO (readIORef ref)
        start ← Lua.liftIO getMonotonicTimeNSec
        restore action `Catch.finally` Lua.liftIO (do
            end ← getMonotonicTimeNSec
            recordCall stats epoch name (max 0 (toInteger end - toInteger start)))

recordCall ∷ LuaCallStats → Integer → BS.ByteString → Integer → IO ()
recordCall (LuaCallStats ref) startedEpoch name duration = modifyIORef' ref $
    \window@(CallWindow epoch snapshot) →
        if epoch ≢ startedEpoch then window else
            let add Nothing = Just (CallStat 1 duration duration)
                add (Just previous) = Just (CallStat
                    (callCount previous + 1)
                    (callTotalDurationNs previous + duration)
                    (max (callMaxDurationNs previous) duration))
            in CallWindow epoch (CallSnapshot (callSequence snapshot + 1)
                (Map.alter add name (callVerbs snapshot)))
