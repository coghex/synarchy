{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Lua surface for the F4 (#646) action-outcome oracle tap.
--
--   * `debug.recordOutcome{ kind=, outcome=, where={x=,y=}, target=,
--     requested=, applied=, dropped=, reason=, handler= }` — Lua
--     producer for action handlers to report an outcome. Only `kind`
--     and `outcome` are required; every other field is the caller's
--     to fill in as applicable.
--   * `debug.drainActionOutcomes()` — atomically read + clear the
--     engine's action-outcome buffer, returning the records as a Lua
--     array. Mirrors combat.drainEvents' read-then-clear contract:
--     records pushed during a drain stay buffered for the next call.
--
--   Engine-side producers (World.Thread.Command.Cursor's designation
--   handlers, which know partial-drop counts the Lua call site can't
--   see) push directly via 'Engine.ActionOutcome.pushActionOutcome'
--   rather than round-tripping through recordOutcome.
module Engine.Scripting.Lua.API.ActionOutcome
    ( debugRecordOutcomeFn
    , debugDrainActionOutcomesFn
    ) where

import UPrelude
import qualified Data.Sequence as Seq
import qualified Data.Text.Encoding as TE
import Data.IORef (atomicModifyIORef', readIORef)
import qualified HsLua as Lua
import Engine.ActionOutcome (ActionOutcome(..))
import Engine.Core.State (EngineEnv(..))

-- | debug.recordOutcome(table) → bool. False (no record pushed) if the
--   required `kind`/`outcome` fields are missing.
debugRecordOutcomeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
debugRecordOutcomeFn env = do
    -- The record table is argument 1, so 'Lua.nth 1' addresses it at the
    -- start of every helper below (each helper pops back to that
    -- baseline) — same top-relative convention as
    -- Engine.Scripting.Lua.API.World.GenConfig's getIntField/getSubInt.
    let getStr ∷ Lua.Name → Lua.LuaE Lua.Exception (Maybe Text)
        getStr name = do
            _ ← Lua.getfield (Lua.nth 1) name
            ms ← Lua.tostring Lua.top
            Lua.pop 1
            pure (TE.decodeUtf8 <$> ms)
        getInt ∷ Lua.Name → Lua.LuaE Lua.Exception (Maybe Int)
        getInt name = do
            _ ← Lua.getfield (Lua.nth 1) name
            mi ← Lua.tointeger Lua.top
            Lua.pop 1
            pure (fromIntegral <$> mi)
        getSubInt ∷ Lua.Name → Lua.Name → Lua.LuaE Lua.Exception (Maybe Int)
        getSubInt tbl name = do
            _ ← Lua.getfield (Lua.nth 1) tbl
            isT ← Lua.istable Lua.top
            r ← if isT
                then do
                    _ ← Lua.getfield (Lua.nth 1) name
                    mi ← Lua.tointeger Lua.top
                    Lua.pop 1
                    pure (fromIntegral <$> mi)
                else pure Nothing
            Lua.pop 1
            pure r

    mKind     ← getStr "kind"
    mOutcome  ← getStr "outcome"
    whereX    ← getSubInt "where" "x"
    whereY    ← getSubInt "where" "y"
    mTarget   ← getInt "target"
    requested ← getInt "requested"
    applied   ← getInt "applied"
    dropped   ← getInt "dropped"
    reason    ← getStr "reason"
    handler   ← getStr "handler"
    case (mKind, mOutcome) of
        (Just kind, Just outcome) → do
            gt ← Lua.liftIO $ readIORef (gameTimeRef env)
            Lua.liftIO $ atomicModifyIORef' (actionOutcomeRef env) $ \buf →
                ( buf Seq.|> ActionOutcome
                    { aoTs        = gt
                    , aoKind      = kind
                    , aoOutcome   = outcome
                    , aoWhereX    = whereX
                    , aoWhereY    = whereY
                    , aoTarget    = fromIntegral <$> mTarget
                    , aoRequested = requested
                    , aoApplied   = applied
                    , aoDropped   = dropped
                    , aoReason    = reason
                    , aoHandler   = handler
                    }
                , () )
            Lua.pushboolean True
            return 1
        _ → Lua.pushboolean False >> return 1

-- | Push one ActionOutcome as a Lua table onto the stack. Field shape:
--     { ts, kind, outcome, where = {x,y} | nil, target, requested,
--       applied, dropped, reason, handler }
pushActionOutcomeLua ∷ ActionOutcome → Lua.LuaE Lua.Exception ()
pushActionOutcomeLua ev = do
    Lua.newtable
    Lua.pushnumber (Lua.Number (realToFrac (aoTs ev)))
    Lua.setfield (-2) "ts"
    Lua.pushstring (TE.encodeUtf8 (aoKind ev))
    Lua.setfield (-2) "kind"
    Lua.pushstring (TE.encodeUtf8 (aoOutcome ev))
    Lua.setfield (-2) "outcome"
    case (aoWhereX ev, aoWhereY ev) of
        (Just wx, Just wy) → do
            Lua.newtable
            Lua.pushinteger (fromIntegral wx)
            Lua.setfield (-2) "x"
            Lua.pushinteger (fromIntegral wy)
            Lua.setfield (-2) "y"
        _ → Lua.pushnil
    Lua.setfield (-2) "where"
    case aoTarget ev of
        Just t  → Lua.pushinteger (fromIntegral t)
        Nothing → Lua.pushnil
    Lua.setfield (-2) "target"
    case aoRequested ev of
        Just n  → Lua.pushinteger (fromIntegral n)
        Nothing → Lua.pushnil
    Lua.setfield (-2) "requested"
    case aoApplied ev of
        Just n  → Lua.pushinteger (fromIntegral n)
        Nothing → Lua.pushnil
    Lua.setfield (-2) "applied"
    case aoDropped ev of
        Just n  → Lua.pushinteger (fromIntegral n)
        Nothing → Lua.pushnil
    Lua.setfield (-2) "dropped"
    case aoReason ev of
        Just r  → Lua.pushstring (TE.encodeUtf8 r)
        Nothing → Lua.pushnil
    Lua.setfield (-2) "reason"
    case aoHandler ev of
        Just h  → Lua.pushstring (TE.encodeUtf8 h)
        Nothing → Lua.pushnil
    Lua.setfield (-2) "handler"

-- | debug.drainActionOutcomes() → array of outcome tables (oldest-first).
--   Atomically swaps in an empty buffer, so records pushed mid-drain are
--   preserved for the next call (same contract as combat.drainEvents).
debugDrainActionOutcomesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
debugDrainActionOutcomesFn env = do
    drained ← Lua.liftIO $ atomicModifyIORef' (actionOutcomeRef env) $
        \buf → (Seq.empty, buf)
    Lua.newtable
    forM_ (zip [1..] (foldr (:) [] drained)) $ \(i, ev) → do
        pushActionOutcomeLua ev
        Lua.rawseti (-2) i
    return 1
