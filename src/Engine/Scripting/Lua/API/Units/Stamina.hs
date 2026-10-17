{-# LANGUAGE Strict #-}
-- | Stamina commits (#2470) — the ONE engine-owned relative write to a
--   unit's exertion pool, and the only authority on what the physiology
--   tick's exhaustion checks are allowed to read.
--
--   Stamina is spent engine-side, inside the single atomic unit-manager
--   commit #2328 built ('Combat.Resolution.Admission.commitIfAdmitted'
--   applying 'Combat.Resolution.Wear.spendStrikeCost'). Drain and
--   recovery are Lua's, on the physiology tick. Before this module the
--   tick READ the stat, computed @current + (regen − drain) × dt@ in
--   script, clamped it there, and republished the sum with
--   @unit.setStat@ — an absolute write derived from a value read
--   outside the transaction. The combat worker and the Lua worker are
--   separate threads, so a strike committing between that read and that
--   write was silently refunded: from stamina 6.0 a heavy strike (2.5
--   of a 10 pool) leaves 3.5, and an idle recovery of +0.05 computed
--   against the stale 6.0 republishes 6.05 — 2.5 above the 3.55 that
--   preserves both effects.
--
--   The second half of the same defect is worse than an arithmetic
--   slip. @scripts\/unit_resource_tick.lua@'s death and collapse rules
--   then fired against that same stale @current@ and the script's own
--   @next@, neither of which is what storage holds. A unit whose pool a
--   strike drove to exactly zero inside the window read as @2.0@ and
--   @2.05@, and @kill_on_zero@ — the universal exhaustion-death rule —
--   never fired.
--
--   The fix is not a lock but a RELATIVE verb that also REPORTS.
--   @unit.commitStamina@ takes the intended net amount and performs the
--   lookup, the maximum resolution, the read, the addition, the clamp
--   and the publication inside ONE 'atomicModifyIORef'' on the same
--   unit-manager reference combat commits against; then it hands back
--   the @before@, @after@ and @maximum@ that transaction actually saw,
--   so the caller's thresholds consume committed values rather than
--   script-side estimates. Whatever a debit did to the stored value is
--   by construction already visible to both the addition and the
--   checks.
--
--   Deliberate properties, each one a case an earlier path got wrong:
--
--   * The BOUND is resolved from the committing unit record, through
--     'Combat.Resolution.Common.maxStaminaFor' — the ONE maximum rule
--     ('Unit.Stats.maxStaminaOf', #2834) that the registry's healthy
--     value, @unit.getMaxStamina@ and combat's swing cost share. One
--     captured game-time sample resolves every expiry, so the bound
--     cannot shift mid-transaction.
--
--   * The amount and every report are EFFECTIVE stamina (#2834): the
--     commit runs 'Combat.Resolution.Stamina.staminaCore', which solves
--     for the stored BASE whose effective value is nearest the target
--     and reports the effective read of what it stored. A retained
--     modifier on @stamina@ therefore never compounds into the base and
--     a report never differs from an effective read of storage.
--
--   * An ABSENT pool on a Simulated receiver is a broken invariant, not
--     a cue to refill (the old first-observation path is gone): spawn,
--     load and transitions seed the pool, so the commit refuses with
--     @missing_pool@ and mutates nothing. A Nominal receiver refuses
--     with @nominal@ and an Unsupported one with @unsupported@, so a
--     caller can tell "no pool by design" from "no body".
--
--   * A refusal mutates NOTHING and is named. The pool is the input to
--     a death rule; silently treating a malformed call as a no-op write
--     would leave the caller evaluating @kill_on_zero@ against numbers
--     no transaction produced.
--
--   Registered directly from "Engine.Scripting.Lua.API.Register.Unit"
--   rather than through the 'Engine.Scripting.Lua.API.Units' facade,
--   whose export list is frozen at the pre-split surface — the same
--   route the transfer (#1000), medical (#2297) and stance (#2468)
--   verbs take.
module Engine.Scripting.Lua.API.Units.Stamina
  ( unitCommitStaminaFn
  , unitGetMaxStaminaFn
  , StaminaCommit(..)
  , commitStaminaIn
  , staminaStatName
  ) where

import UPrelude
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef (atomicModifyIORef', readIORef)
import Combat.Resolution.Stamina
    (StaminaReport(..), StaminaStatus, staminaCommit, staminaStatusText)
import Unit.Physiology.Types (PhysiologyRegistry)
import Unit.Stats (maxStaminaOf)
import Unit.Types

-- | The @uiStats@ key combat spends and this verb adjusts. Named once
--   so the debit and the physiology update cannot drift onto different
--   entries.
staminaStatName ∷ Text
staminaStatName = "stamina"

-- | What one commit actually did, as the transaction saw it: EFFECTIVE
--   values (#2834). The numbers describe the SAME update: 'scAfter' is
--   the effective read of what storage now holds and 'scMaximum' the
--   bound it was clamped against, both resolved from the committing
--   record rather than from anything the caller read earlier.
data StaminaCommit = StaminaCommit
    { scBefore  ∷ Float
    , scAfter   ∷ Float
    , scMaximum ∷ Float
    , scStatus  ∷ StaminaStatus
    } deriving (Show, Eq)

-- | The whole transaction, as a pure 'UnitManager' step so
--   'atomicModifyIORef'' can apply it in one go.
--
--   @Left reason@ is a refusal that mutated NOTHING — the manager is
--   returned unchanged and the caller reports @nil, reason@ to Lua.
--
--   @now@ is the caller's single captured game-time sample, used for
--   nothing but modifier expiry. The arithmetic is
--   'Combat.Resolution.Stamina.staminaCommit''s.
commitStaminaIn ∷ PhysiologyRegistry → Double → UnitId → Double → UnitManager
                → (UnitManager, Either Text StaminaCommit)
commitStaminaIn reg now uid amount um =
    case staminaCommit reg now um uid amount of
        Left reason → (um, Left reason)
        Right (um', r) → ( um'
                         , Right (StaminaCommit (srBefore r) (srAfter r)
                                                (srMaximum r) (srStatus r)) )

-- | The @uid@ argument: a Lua NUMBER (checked with 'Lua.ltype' before
--   any conversion, so a numeric STRING such as @"47"@ is refused
--   rather than silently coerced onto unit 47), integral, and inside
--   the 'Word32' range 'UnitId' is. Mirrors
--   'Engine.Scripting.Lua.API.Units.Stance''s reader so a negative or
--   oversized Lua integer can never wrap onto a live unit.
readUnitIdArg ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe UnitId)
readUnitIdArg idx = do
    ty ← Lua.ltype idx
    if ty ≢ Lua.TypeNumber then pure Nothing else do
        mN ← Lua.tointeger idx
        pure $ do
            n ← mN
            unless (n ≥ 0 ∧ n ≤ 4294967295) Nothing
            pure (UnitId (fromIntegral n))

-- | The @amount@ argument: a Lua NUMBER and finite. This is the FULL
--   intended NET change, @(regen − drain) × dt@ BEFORE any script-side
--   clamp — never an absolute target and never an offset derived from
--   one, which is the whole point of the verb. Unlike stance recovery
--   it is SIGNED: physiology drains stamina as readily as it restores
--   it, so a negative amount is ordinary input rather than a refusal.
readAmountArg ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe Double)
readAmountArg idx = do
    ty ← Lua.ltype idx
    if ty ≢ Lua.TypeNumber then pure Nothing else do
        mV ← Lua.tonumber idx
        pure $ case mV of
            Nothing → Nothing
            Just (Lua.Number v)
                | isNaN v ∨ isInfinite v → Nothing
                | otherwise              → Just v

-- | @nil, "<reason>"@ — the refusal shape
--   'Engine.Scripting.Lua.API.Units.Stance' already uses. The shipped
--   caller turns it into a Lua @error@, which is what makes it
--   diagnosable: 'Engine.Scripting.Lua.Script.callModuleFunctionReportingError'
--   reports RAISED errors and discards a callback's normal returns.
pushRefusal ∷ Text → Lua.LuaE Lua.Exception Lua.NumResults
pushRefusal reason = do
    Lua.pushnil
    Lua.pushstring (TE.encodeUtf8 reason)
    return 2

-- | The success shape: one table carrying the whole commit, every value
--   EFFECTIVE.
pushCommit ∷ StaminaCommit → Lua.LuaE Lua.Exception Lua.NumResults
pushCommit sc = do
    Lua.newtable
    Lua.pushnumber (Lua.Number (realToFrac (scBefore sc)))
    Lua.setfield (-2) "before"
    Lua.pushnumber (Lua.Number (realToFrac (scAfter sc)))
    Lua.setfield (-2) "after"
    Lua.pushnumber (Lua.Number (realToFrac (scMaximum sc)))
    Lua.setfield (-2) "maximum"
    Lua.pushstring (TE.encodeUtf8 (staminaStatusText (scStatus sc)))
    Lua.setfield (-2) "status"
    return 1

-- | @unit.commitStamina(uid, amount) → table | nil, reason@
--
--   Adds the EFFECTIVE @amount@ to @uid@'s stamina and returns what the
--   commit did: @{ before, after, maximum, status }@, all effective,
--   @status@ one of @"ok"@, @"floored"@, @"zeroMultiplier"@.
--   Argument checks run BEFORE the unit lookup, so a malformed call is
--   refused identically whether or not the unit exists and whether or
--   not its pool is already full.
--
--   Refusal reasons: @invalid_unit_id@, @invalid_amount@,
--   @no_such_unit@, @dead@, @nominal@ (a Nominal receiver holds no
--   pool), @unsupported@ (a bodyless unit), @missing_pool@ (a Simulated
--   receiver whose pool is absent — never refilled), @invalid_stamina@
--   (the stored value is not finite) and @invalid_maximum@ (the resolved
--   bound is not a positive finite number). Every one of them leaves the
--   unit manager untouched.
unitCommitStaminaFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCommitStaminaFn env = do
    mUid ← readUnitIdArg 1
    mAmt ← readAmountArg 2
    case mUid of
        Nothing  → pushRefusal "invalid_unit_id"
        Just uid → case mAmt of
            Nothing     → pushRefusal "invalid_amount"
            Just amount → do
                res ← Lua.liftIO $ do
                    -- ONE game-time sample, read before the
                    -- transaction, so every modifier expiry inside the
                    -- commit resolves against the same instant.
                    now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
                    reg ← readIORef
                        (crPhysiologyRegistryRef
                            (toContentRegistriesCapability env))
                    atomicModifyIORef'
                        (ucUnitManagerRef (toUnitCombatCapability env))
                        (commitStaminaIn reg now uid amount)
                case res of
                    Left reason → pushRefusal reason
                    Right sc    → pushCommit sc

-- | @unit.getMaxStamina(uid) → number | nil@. A PURE read of the unit's
--   effective maximum stamina through the ONE maximum rule
--   ('Unit.Stats.maxStaminaOf', #2834) — what @stats.get(uid,
--   "max_stamina")@ answers, so Lua reads, combat cost, commit bounds and
--   seed targets cannot disagree. @nil@ for a missing unit and for a unit
--   with neither an explicit @max_stamina@ nor an @endurance@ stat (only
--   the statless bodyless definitions): combat's internal endurance-1.0
--   fallback is not a maximum Lua may see. Any caller may use it: it
--   writes nothing.
unitGetMaxStaminaFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetMaxStaminaFn env = do
    mUid ← readUnitIdArg 1
    mMax ← case mUid of
        Nothing  → pure Nothing
        Just uid → Lua.liftIO $ do
            now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
            um  ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
            pure $ do
                inst ← HM.lookup uid (umInstances um)
                maxStaminaOf Nothing now (uiStats inst) (uiModifiers inst)
    case mMax of
        Nothing → Lua.pushnil
        Just v  → Lua.pushnumber (Lua.Number (realToFrac v))
    return 1
