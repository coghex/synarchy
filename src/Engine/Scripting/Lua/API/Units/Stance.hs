{-# LANGUAGE Strict #-}
-- | Stance recovery (#2468) — the ONE engine-owned relative write to a
--   unit's combat-readiness resource.
--
--   Stance is a 0..1 pool that combat spends engine-side, inside the
--   single atomic unit-manager commit #2328 built
--   ('Combat.Resolution.Admission.commitIfAdmitted' applying
--   'Combat.Resolution.Wear.spendStrikeCost'). Recovery is Lua's, on
--   the physiology tick. Before this module Lua recovered by READING
--   the stat, adding @rate × dt@ in script, and writing the sum back
--   with @unit.setStat@ — an absolute write derived from a value read
--   outside the transaction. The combat worker and the Lua worker are
--   separate threads, so a strike committing between that read and
--   that write was silently erased: from stance 0.600, a quick strike
--   (0.25) leaves 0.350, and a recovery of +0.059 computed against the
--   stale 0.600 republishes 0.659 — a value neither serial order can
--   produce, and 0.25 above the 0.409 that preserves both effects.
--
--   The fix is not a lock but a RELATIVE verb: @unit.recoverStance@
--   takes the amount to add and performs the lookup, the read, the
--   addition, the clamp and the publication inside ONE
--   'atomicModifyIORef'' on the same unit-manager reference combat
--   commits against. Whatever the debit did to the stored value is by
--   construction already visible to the addition.
--
--   Two deliberate asymmetries with @unit.setStat@:
--
--   * It works on the STORED BASE, exactly the value combat spends.
--     @unit.getStat@ returns the modifier-adjusted EFFECTIVE value, so
--     the old read/modify/write could also bake an active stance
--     modifier into the base. Nothing here consults 'uiModifiers'.
--
--   * An ABSENT stance entry stays absent. Absence means implicitly
--     full stance everywhere ('Combat.Resolution.Wear.staminaDrainStats'
--     reads @HM.lookupDefault 1.0@), so recovering an absent entry
--     reports 1 and writes nothing — no entry is created and no lazy
--     stat roll is triggered, which keeps the verb free of the RNG the
--     'Engine.Scripting.Lua.API.Units.Stats' getters may consume.
--
--   Registered directly from "Engine.Scripting.Lua.API.Register.Unit"
--   rather than through the 'Engine.Scripting.Lua.API.Units' facade,
--   whose export list is frozen at the pre-split surface — the same
--   route the transfer (#1000) and medical (#2297) verbs take.
module Engine.Scripting.Lua.API.Units.Stance
  ( unitRecoverStanceFn
  , recoverStanceIn
  , stanceStatName
  ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.State (EngineEnv)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef (atomicModifyIORef')
import Unit.Types

-- | The @uiStats@ key combat spends and this verb restores. Named once
--   so the debit and the credit cannot drift onto different entries.
stanceStatName ∷ Text
stanceStatName = "stance"

-- | The whole transaction, as a pure 'UnitManager' step so
--   'atomicModifyIORef'' can apply it in one go.
--
--   @Left reason@ is a refusal that mutated NOTHING — the manager is
--   returned unchanged and the caller reports @nil, reason@ to Lua.
--   @Right v@ is the committed stored value, which for an absent entry
--   is the implicit 1.0 with no entry written.
--
--   The addition and the clamp happen in 'Double' and only the clamped
--   result narrows to the 'Float' 'uiStats' holds, so an amount that is
--   finite in Lua but outside 'Float' range (1e100, say) saturates at 1
--   instead of publishing an infinity.
recoverStanceIn ∷ UnitId → Double → UnitManager → (UnitManager, Either Text Float)
recoverStanceIn uid amount um =
    case HM.lookup uid (umInstances um) of
        Nothing → (um, Left "no_such_unit")
        Just inst → case HM.lookup stanceStatName (uiStats inst) of
            -- Absent ⇒ implicitly full. Reported as 1, never materialised.
            Nothing → (um, Right 1.0)
            Just cur
                | isNaN cur ∨ isInfinite cur → (um, Left "invalid_stance")
                | otherwise →
                    let new   = clampStance (realToFrac cur + amount)
                        inst' = inst { uiStats =
                                    HM.insert stanceStatName new (uiStats inst) }
                    in ( um { umInstances = HM.insert uid inst' (umInstances um) }
                       , Right new )

-- | Add-then-clamp into @[0, 1]@, narrowing only after the clamp.
--   Applied unconditionally, so a stored value that some other writer
--   left outside the band is brought back into it even by a zero
--   recovery.
clampStance ∷ Double → Float
clampStance = realToFrac ∘ min 1 ∘ max 0

-- | The @uid@ argument: a Lua NUMBER (checked with 'Lua.ltype' before
--   any conversion, so a numeric STRING such as @"47"@ is refused
--   rather than silently coerced onto unit 47), integral, and inside
--   the 'Word32' range 'UnitId' is. Mirrors
--   'Engine.Scripting.Lua.API.Units.TransferOrder''s @readId@ so a
--   negative or oversized Lua integer can never wrap onto a live unit.
readUnitIdArg ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe UnitId)
readUnitIdArg idx = do
    ty ← Lua.ltype idx
    if ty ≢ Lua.TypeNumber then pure Nothing else do
        mN ← Lua.tointeger idx
        pure $ do
            n ← mN
            unless (n ≥ 0 ∧ n ≤ 4294967295) Nothing
            pure (UnitId (fromIntegral n))

-- | The @amount@ argument: a Lua NUMBER, non-negative and finite. This
--   is the FULL intended recovery (@rate × dt@), never an absolute
--   target — the whole point of the verb.
readAmountArg ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe Double)
readAmountArg idx = do
    ty ← Lua.ltype idx
    if ty ≢ Lua.TypeNumber then pure Nothing else do
        mV ← Lua.tonumber idx
        pure $ case mV of
            Nothing → Nothing
            Just (Lua.Number v)
                | isNaN v ∨ isInfinite v → Nothing
                | v < 0                  → Nothing
                | otherwise              → Just v

-- | @nil, "<reason>"@ — the refusal shape
--   'Engine.Scripting.Lua.API.Units.TransferOrder' already uses. The
--   shipped caller turns it into a Lua @error@, which is what makes it
--   diagnosable: 'Engine.Scripting.Lua.Script.callModuleFunctionReportingError'
--   reports RAISED errors and discards a callback's normal returns.
pushRefusal ∷ Text → Lua.LuaE Lua.Exception Lua.NumResults
pushRefusal reason = do
    Lua.pushnil
    Lua.pushstring (TE.encodeUtf8 reason)
    return 2

-- | @unit.recoverStance(uid, amount) → number | nil, reason@
--
--   Adds @amount@ to @uid@'s STORED stance and returns what was
--   committed, clamped into @[0, 1]@. Argument checks run BEFORE the
--   unit lookup, so a malformed call is refused identically whether or
--   not the unit exists and whether or not its stance is already full.
--
--   Refusal reasons: @invalid_unit_id@, @invalid_amount@,
--   @no_such_unit@ (absent at commit), @invalid_stance@ (the stored
--   value is not finite). Every one of them leaves the unit manager
--   untouched.
unitRecoverStanceFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitRecoverStanceFn env = do
    mUid ← readUnitIdArg 1
    mAmt ← readAmountArg 2
    case mUid of
        Nothing  → pushRefusal "invalid_unit_id"
        Just uid → case mAmt of
            Nothing     → pushRefusal "invalid_amount"
            Just amount → do
                res ← Lua.liftIO $ atomicModifyIORef'
                          (ucUnitManagerRef (toUnitCombatCapability env))
                          (recoverStanceIn uid amount)
                case res of
                    Left reason → pushRefusal reason
                    Right v     → do
                        Lua.pushnumber (Lua.Number (realToFrac v))
                        return 1
