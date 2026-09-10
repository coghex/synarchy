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
--   Four deliberate properties, each one a case the old path got wrong:
--
--   * The BOUND is resolved from the committing unit record, through
--     'Combat.Resolution.Common.maxStaminaFor' — the same mirror of
--     Lua's @unit_stats@ derivation combat sizes a swing against
--     (#1735: explicit @max_stamina@ wins, else effective
--     @endurance × 10@, both through 'Unit.Stats.effectiveStat'). A
--     modifier that expired, or an accessory that was equipped, during
--     the Lua rate calculation moves the clamp here rather than a tick
--     later. One captured game-time sample resolves every expiry, so
--     the bound cannot shift mid-transaction.
--
--   * It works on the STORED BASE, exactly the value combat spends.
--     @unit.getStat@ returns the modifier-adjusted EFFECTIVE value, so
--     the old read-modify-write also baked an active stamina modifier
--     into the base, compounding it once per tick. Nothing here
--     consults 'uiModifiers' for the pool itself — only
--     'maxStaminaFor' does, for the bound.
--
--   * An ABSENT pool is initialised at COMMIT, not on the strength of
--     an earlier script-side read. @initialized@ says which path ran,
--     and the shipped caller skips that pass's consequences exactly as
--     the old first-tick @return@ did. A pool combat CREATED after the
--     script last looked therefore takes the ordinary delta path — the
--     old code refilled it to full, handing back every point the strike
--     had just spent.
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
  , StaminaCommit(..)
  , commitStaminaIn
  , staminaStatName
  ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef (atomicModifyIORef', readIORef)
import Combat.Resolution.Common (maxStaminaFor)
import Unit.Types

-- | The @uiStats@ key combat spends and this verb adjusts. Named once
--   so the debit and the physiology update cannot drift onto different
--   entries.
staminaStatName ∷ Text
staminaStatName = "stamina"

-- | What one commit actually did, as the transaction saw it.
--
--   'scBefore' is 'Nothing' only on the initialisation path — the pool
--   was absent when the transaction ran. The three numbers describe the
--   SAME update: 'scAfter' is what storage now holds and 'scMaximum' is
--   the bound it was clamped against, both resolved from the committing
--   record rather than from anything the caller read earlier.
data StaminaCommit = StaminaCommit
    { scBefore      ∷ Maybe Float
    , scAfter       ∷ Float
    , scMaximum     ∷ Float
    , scInitialized ∷ Bool
    } deriving (Show, Eq)

-- | The whole transaction, as a pure 'UnitManager' step so
--   'atomicModifyIORef'' can apply it in one go.
--
--   @Left reason@ is a refusal that mutated NOTHING — the manager is
--   returned unchanged and the caller reports @nil, reason@ to Lua.
--
--   @now@ is the caller's single captured game-time sample, used for
--   nothing but modifier expiry inside 'maxStaminaFor'.
--
--   The addition and the clamp happen in 'Double' and only the clamped
--   result narrows to the 'Float' 'uiStats' holds, so an amount that is
--   finite in Lua but outside 'Float' range (1e40, say) saturates at
--   the bound instead of publishing an infinity.
--
--   The one write elision — @new ≡ cur@ leaves the manager untouched —
--   is INSIDE the transaction and does not change what is reported:
--   'scAfter' is the value storage holds either way. It replaces the
--   script-side @abs(next − current) > 1e-4@ guard, which used to skip
--   the write on a stale comparison and take the thresholds with it.
commitStaminaIn ∷ Double → UnitId → Double → UnitManager
                → (UnitManager, Either Text StaminaCommit)
commitStaminaIn now uid amount um =
    case HM.lookup uid (umInstances um) of
        Nothing → (um, Left "no_such_unit")
        Just inst →
            let maxStam = maxStaminaFor now inst
            in if isNaN maxStam ∨ isInfinite maxStam ∨ maxStam ≤ 0
                 then (um, Left "invalid_maximum")
                 else case HM.lookup staminaStatName (uiStats inst) of
                    -- Absent AT COMMIT ⇒ first observation. Fill to the
                    -- bound this transaction resolved and say so; the
                    -- caller skips this pass's consequences.
                    Nothing → ( publish inst maxStam
                              , Right (StaminaCommit Nothing maxStam maxStam True) )
                    Just cur
                        | isNaN cur ∨ isInfinite cur → (um, Left "invalid_stamina")
                        | otherwise →
                            let new = clampStamina maxStam
                                          (realToFrac cur + amount)
                                um' | new ≡ cur = um
                                    | otherwise = publish inst new
                            in ( um'
                               , Right (StaminaCommit (Just cur) new maxStam False) )
  where
    publish inst v =
        let inst' = inst { uiStats = HM.insert staminaStatName v (uiStats inst) }
        in um { umInstances = HM.insert uid inst' (umInstances um) }

-- | Add-then-clamp into @[0, maximum]@, narrowing only after the clamp.
--   Applied unconditionally, so a stored value some other writer left
--   outside the band — or one a SHRUNK maximum has just put outside it
--   — is brought back into range even by a zero delta.
clampStamina ∷ Float → Double → Float
clampStamina maxStam =
    realToFrac ∘ min (realToFrac maxStam) ∘ max 0

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

-- | The success shape: one table carrying the whole commit. @before@ is
--   OMITTED (so Lua reads @nil@) on the initialisation path, which is
--   the only way absence can be distinguished from a stored 0.
pushCommit ∷ StaminaCommit → Lua.LuaE Lua.Exception Lua.NumResults
pushCommit sc = do
    Lua.newtable
    forM_ (scBefore sc) $ \b → do
        Lua.pushnumber (Lua.Number (realToFrac b))
        Lua.setfield (-2) "before"
    Lua.pushnumber (Lua.Number (realToFrac (scAfter sc)))
    Lua.setfield (-2) "after"
    Lua.pushnumber (Lua.Number (realToFrac (scMaximum sc)))
    Lua.setfield (-2) "maximum"
    Lua.pushboolean (scInitialized sc)
    Lua.setfield (-2) "initialized"
    return 1

-- | @unit.commitStamina(uid, amount) → table | nil, reason@
--
--   Adds @amount@ to @uid@'s STORED stamina and returns what the commit
--   did: @{ before, after, maximum, initialized }@, with @before@ nil
--   exactly when the pool was absent and this call filled it.
--   Argument checks run BEFORE the unit lookup, so a malformed call is
--   refused identically whether or not the unit exists and whether or
--   not its pool is already full.
--
--   Refusal reasons: @invalid_unit_id@, @invalid_amount@,
--   @no_such_unit@ (absent at commit — never recreated),
--   @invalid_stamina@ (the stored value is not finite) and
--   @invalid_maximum@ (the resolved bound is not a positive finite
--   number). Every one of them leaves the unit manager untouched.
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
                    -- transaction, so every modifier expiry inside
                    -- 'maxStaminaFor' resolves against the same instant.
                    now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
                    atomicModifyIORef'
                        (ucUnitManagerRef (toUnitCombatCapability env))
                        (commitStaminaIn now uid amount)
                case res of
                    Left reason → pushRefusal reason
                    Right sc    → pushCommit sc
