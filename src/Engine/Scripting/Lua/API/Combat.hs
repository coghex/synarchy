{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}

-- | Lua surface for the combat system.
--
-- Two functions:
--   * `combat.attack(attackerUid, targetUid)` — push a CombatAttack
--     command onto the queue the combat thread drains. Skeleton phase:
--     the thread just logs and discards.
--   * `combat.drainEvents()` — atomically read + clear the engine's
--     combat-event buffer, returning the events as a Lua array. Skeleton
--     phase: always empty.
--
-- The drain pattern (read-then-clear in a single `atomicModifyIORef'`)
-- prevents the combat thread and Lua from racing: events that fire
-- during a drain stay in the buffer until next call.
module Engine.Scripting.Lua.API.Combat
    ( combatAttackFn
    , combatDrainEventsFn
    , combatEmitDeathFn
    , injuryEmitFn
    , injuryDrainEventsFn
    ) where

import UPrelude
import Control.Monad (forM_, forM)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import qualified HsLua as Lua
import Combat.Types (CombatCommand(..), CombatEvent(..), AttackMode(..))
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q

-- | combat.attack(attackerUid, targetUid [, mode]) → bool
--
--   mode is "quick" (default) or "heavy". Unknown strings fall back
--   to Quick so old call sites that pass two args still work.
combatAttackFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
combatAttackFn env = do
    aArg    ← Lua.tointeger 1
    tArg    ← Lua.tointeger 2
    modeArg ← Lua.tostring 3
    reachArg ← Lua.tonumber 4
    speedArg ← Lua.tonumber 5
    let mode = case modeArg of
            Just bs | TE.decodeUtf8 bs == ("heavy" ∷ Text) → Heavy
            _                                              → Quick
        reachBonus = case reachArg of
            Just (Lua.Number v) → realToFrac v   -- lunge strike-reach (m)
            _                   → 0.0
        impactSpeed = case speedArg of
            Just (Lua.Number v) → realToFrac v   -- lunge body speed (m/s)
            _                   → 0.0
    case (aArg, tArg) of
        (Just a, Just t) → do
            Lua.liftIO $ Q.writeQueue (combatQueue env) $
                CombatAttack (fromIntegral a) (fromIntegral t) mode reachBonus impactSpeed
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | combat.emitDeath(targetUid [, cause]) → bool
--
-- Pushes a synthetic "death" combat event so that NON-combat / delayed
-- deaths (suffocation, organ failure, starvation, fall) narrate in the
-- combat log alongside engine-resolved combat deaths. There is no
-- attacker. The combat-log script refines the cause from the corpse's
-- own wounds (injuries.deathCause); the optional `cause` string is the
-- fallback used for woundless deaths (e.g. "starvation").
combatEmitDeathFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
combatEmitDeathFn env = do
    tArg     ← Lua.tointeger 1
    causeArg ← Lua.tostring 2
    case tArg of
        Just t → do
            gt ← Lua.liftIO $ readIORef (gameTimeRef env)
            let cause = maybe "their wounds" TE.decodeUtf8 causeArg
                ev = CombatEvent
                    { ceTs       = gt
                    , ceKind     = "death"
                    , ceAttacker = Nothing
                    , ceTarget   = Just (fromIntegral t)
                    , cePayload  = HM.singleton "cause" cause
                    }
            Lua.liftIO $ atomicModifyIORef' (combatEventsRef env) $ \buf →
                (buf Seq.|> ev, ())
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | combat.drainEvents() → array of event tables.
--
-- Each event table has:
--   { ts       = number,
--     kind     = string,
--     attacker = integer | nil,
--     target   = integer | nil,
--     payload  = { key1 = value1, ... } }
--
-- Atomically swaps in an empty Seq and returns the previously-held
-- contents — events emitted by the combat thread during the drain are
-- preserved for the next call.
combatDrainEventsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
combatDrainEventsFn env = drainEventStream (combatEventsRef env)

-- | Push one CombatEvent as a Lua table onto the stack (shared by the
--   combat and injury drains — the injury stream reuses the CombatEvent
--   shape: `target` = victim, `kind` = "fall"|"injure"|"death", and the
--   payload carries part / woundKind / severity / cause).
pushCombatEvent ∷ CombatEvent → Lua.LuaE Lua.Exception ()
pushCombatEvent ev = do
    Lua.newtable
    Lua.pushnumber (Lua.Number (realToFrac (ceTs ev)))
    Lua.setfield (-2) "ts"
    Lua.pushstring (TE.encodeUtf8 (ceKind ev))
    Lua.setfield (-2) "kind"
    case ceAttacker ev of
        Just a  → Lua.pushinteger (fromIntegral a)
        Nothing → Lua.pushnil
    Lua.setfield (-2) "attacker"
    case ceTarget ev of
        Just t  → Lua.pushinteger (fromIntegral t)
        Nothing → Lua.pushnil
    Lua.setfield (-2) "target"
    Lua.newtable
    forM_ (HM.toList (cePayload ev)) $ \(k, v) → do
        Lua.pushstring (TE.encodeUtf8 v)
        Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 k))
    Lua.setfield (-2) "payload"

-- | Atomically read+clear an event buffer, returning the events as a
--   Lua array (oldest-first).
drainEventStream
    ∷ IORef (Seq.Seq CombatEvent)
    → Lua.LuaE Lua.Exception Lua.NumResults
drainEventStream ref = do
    drained ← Lua.liftIO $ atomicModifyIORef' ref $ \buf → (Seq.empty, buf)
    Lua.newtable
    forM_ (zip [1..] (foldr (:) [] drained)) $ \(i, ev) → do
        pushCombatEvent ev
        Lua.rawseti (-2) i
    return 1

-- | injury.drainEvents() → array of event tables (same shape as
--   combat.drainEvents). Drives the injury-log UI.
injuryDrainEventsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
injuryDrainEventsFn env = drainEventStream (injuryEventsRef env)

-- | injury.emit(victimUid, kind [, cause [, part [, woundKind [, severity]]]])
--
--   Push a NON-COMBAT injury event for the injury log: a fall, a hazard,
--   or a wound-caused death. `kind` is the event class
--   ("fall" | "injure" | "death"); the rest fill the payload the
--   injury-log prose reads. Engine-side injury producers (Fall, unit.injure)
--   push directly; this is the Lua entry point (e.g. failure-meter deaths
--   in unit_resources). Returns true if the victim id was supplied.
injuryEmitFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
injuryEmitFn env = do
    vArg     ← Lua.tointeger 1
    kindArg  ← Lua.tostring 2
    causeArg ← Lua.tostring 3
    partArg  ← Lua.tostring 4
    wkArg    ← Lua.tostring 5
    sevArg   ← Lua.tonumber 6
    case (vArg, kindArg) of
        (Just v, Just kindBS) → do
            gt ← Lua.liftIO $ readIORef (gameTimeRef env)
            let opt key = maybe [] (\bs → [(key, TE.decodeUtf8 bs)])
                payload = HM.fromList $ concat
                    [ opt "cause"     causeArg
                    , opt "part"      partArg
                    , opt "woundKind" wkArg
                    , maybe [] (\(Lua.Number n) →
                        [("severity", T.pack (show (realToFrac n ∷ Double)))])
                        sevArg
                    ]
                ev = CombatEvent
                    { ceTs       = gt
                    , ceKind     = TE.decodeUtf8 kindBS
                    , ceAttacker = Nothing
                    , ceTarget   = Just (fromIntegral v)
                    , cePayload  = payload
                    }
            Lua.liftIO $ atomicModifyIORef' (injuryEventsRef env) $ \buf →
                (buf Seq.|> ev, ())
            Lua.pushboolean True
            return 1
        _ → Lua.pushboolean False >> return 1
