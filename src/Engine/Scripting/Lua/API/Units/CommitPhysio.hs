{-# LANGUAGE Strict #-}
-- | @unit.commitPhysio(uid, { key = value, ... })@ (#2838, S11a; design
--   @docs\/designs\/physiology_stat_modes_design.md@ S11.2 "Atomic writes"):
--   ONE validated batch of stored-base writes, applied in a single
--   unit-manager update or not at all.
--
--   It exists because the mental state is several keys that only make sense
--   together (a break's behavior, deadline, state and roll cadence), and the
--   combat and craft threads read the unit while the Lua worker is writing
--   it. Separate @unit.setStat@ calls expose every intermediate combination
--   (S11-F1); one batch exposes only complete ones.
--
--   Values are stored BASES with @unit.setStat@'s existing semantics (a
--   clamp at zero), so every writer's arithmetic is unchanged. The write gate
--   ('rawWriteAllowed') is read INSIDE the same atomic update that stores the
--   batch, for every key, so a concurrent mode transition cannot slip between
--   the check and the write; one refused key refuses the whole batch.
module Engine.Scripting.Lua.API.Units.CommitPhysio
    ( unitCommitPhysioFn
    , commitPhysioIn
    ) where

import UPrelude
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv)
import Unit.Physiology (engagementApplied, rawWriteAllowed)
import Unit.Physiology.Types (PhysiologyRegistry)
import Unit.Types

-- | The whole transaction as a pure 'UnitManager' step. @Left reason@ mutated
--   NOTHING: the manager is returned unchanged.
commitPhysioIn ∷ PhysiologyRegistry → UnitId → [(Text, Float)] → UnitManager
               → (UnitManager, Either Text ())
commitPhysioIn reg uid batch um = case HM.lookup uid (umInstances um) of
    Nothing → (um, Left "no_such_unit")
    Just inst
        | (k : _) ← [ k | (k, _) ← batch, not (allowed inst k) ] →
            (um, Left ("forbidden_key:" <> k))
        | otherwise →
            let stats' = foldl' (\m (k, v) → HM.insert k (max 0 v) m)
                                (uiStats inst) batch
                inst'  = inst { uiStats = stats' }
            in (um { umInstances = HM.insert uid inst' (umInstances um) }, Right ())
  where
    allowed inst k = rawWriteAllowed reg
        (maybe noUnitPhysiology udPhysiology (HM.lookup (uiDefName inst) (umDefs um)))
        (engagementApplied (uiStats inst)) (uiPhysioOverrides inst) k

-- | @nil, "<reason>"@: the refusal shape @unit.commitStamina@ uses.
pushRefusal ∷ Text → Lua.LuaE Lua.Exception Lua.NumResults
pushRefusal reason = do
    Lua.pushnil
    Lua.pushstring (TE.encodeUtf8 reason)
    pure 2

-- | The @uid@ argument: a Lua NUMBER (a numeric string is refused, not
--   coerced), integral and inside the 'Word32' range 'UnitId' is.
readUid ∷ Lua.LuaE Lua.Exception (Maybe UnitId)
readUid = do
    ty ← Lua.ltype 1
    if ty ≢ Lua.TypeNumber then pure Nothing else do
        mN ← Lua.tointeger 1
        pure $ do
            n ← mN
            unless (n ≥ 0 ∧ n ≤ 4294967295) Nothing
            pure (UnitId (fromIntegral n))

-- | The batch: a non-empty table of string keys to finite numbers. The
--   finiteness test is on the STORED 'Float', after the conversion, so a
--   large Lua double that overflows to Infinity on the way in is refused like
--   an explicit NaN (#1733).
readBatch ∷ Lua.LuaE Lua.Exception (Either Text [(Text, Float)])
readBatch = do
    ty ← Lua.ltype 2
    if ty ≢ Lua.TypeTable then pure (Left "invalid_batch") else do
        Lua.pushvalue 2
        Lua.pushnil
        r ← loop []
        pure (r ⌦ \xs → if null xs then Left "invalid_batch" else Right xs)
  where
    loop acc = do
        more ← Lua.next (-2)
        if not more
            then Lua.pop 1 ≫ pure (Right (reverse acc))
            else do
                kTy ← Lua.ltype (-2)
                vTy ← Lua.ltype (-1)
                if kTy ≢ Lua.TypeString then bail "invalid_key"
                else if vTy ≢ Lua.TypeNumber then bail "invalid_value"
                else do
                    mk ← Lua.tostring (-2)
                    mv ← Lua.tonumber (-1)
                    case (mk, mv) of
                        (Just k, Just (Lua.Number v))
                            | let f = realToFrac v ∷ Float
                            , not (isNaN f ∨ isInfinite f) → do
                                Lua.pop 1
                                loop ((TE.decodeUtf8Lenient k, f) : acc)
                        _ → bail "invalid_value"
    -- leave the stack as found: the key and the table copy
    bail why = Lua.pop 2 ≫ pure (Left why)

-- | @unit.commitPhysio(uid, batch) → true | nil, reason@. Refusal reasons:
--   @invalid_unit_id@, @invalid_batch@ (not a table, or empty),
--   @invalid_key@ (a non-string key), @invalid_value@ (a non-number or
--   non-finite value), @no_such_unit@ and @forbidden_key:<key>@ (the write
--   gate refused the key: a derived key, a key a Nominal or Unsupported
--   system owns, a field-backed key). Every one leaves the unit untouched.
--   Argument checks run before the unit lookup, so a malformed call is refused
--   identically whether or not the unit exists.
unitCommitPhysioFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCommitPhysioFn env = do
    mUid ← readUid
    case mUid of
        Nothing → pushRefusal "invalid_unit_id"
        Just uid → do
            parsed ← readBatch
            case parsed of
                Left why → pushRefusal why
                Right batch → do
                    res ← Lua.liftIO $ do
                        reg ← readIORef (crPhysiologyRegistryRef
                                  (toContentRegistriesCapability env))
                        atomicModifyIORef'
                            (ucUnitManagerRef (toUnitCombatCapability env))
                            (commitPhysioIn reg uid batch)
                    case res of
                        Left why → pushRefusal why
                        Right () → Lua.pushboolean True ≫ pure 1
