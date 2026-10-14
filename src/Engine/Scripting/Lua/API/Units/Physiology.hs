{-# LANGUAGE Strict #-}
-- | The physiology READ verbs (#2828, S2 of epic #2827; design D-2, D-7,
--   SD-19) and the Haskell 'physioRead' they share.
--
--   __Five read verbs, no mutating one.__ @unit.getPhysioMode@,
--   @getPhysioOverride@, @getPhysioDefault@, @readPhysio@ and
--   @readPhysioBase@ are an explicit exception to the facade-only rule
--   the design (D-7) puts on physiology access: any code may call them,
--   including the console Lua the probes send, because they never change
--   physiology state. The mutating pair (@setPhysioMode@,
--   @clearPhysioMode@) is S4's and is NOT installed here, and nothing in
--   this module can write a mode, an override or a stat.
--
--   __Owned versus unowned.__ A key a registry system owns, derives or
--   meters is read purely by "Unit.Physiology": it never initializes,
--   rolls, writes, advances a generator or changes mode state. Any other
--   key delegates to today's 'getEffectiveStat' \/ @getOrRollStat@ and so
--   keeps @unit.getStat@'s semantics exactly, a lazy roll and its cache
--   included (SD-19). Note what that means for @max_stamina@,
--   @max_exhaustion@, @max_sleep_pressure@ and @metabolism_rate@: this
--   engine verb applies NO @scripts\/unit_stats.lua@ formula, because
--   @getStat@ does not either (the formulas run in the Lua @stats.get@).
--   Applying them through a physiology read is the S5 facade's job.
module Engine.Scripting.Lua.API.Units.Physiology
    ( PhysioReadResult(..)
    , physioRead
    , physioReadBase
    , unitGetPhysioModeFn
    , unitGetPhysioOverrideFn
    , unitGetPhysioDefaultFn
    , unitReadPhysioFn
    , unitReadPhysioBaseFn
    ) where

import UPrelude
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..), toContentRegistriesCapability)
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv)
import Engine.Scripting.Lua.API.Units.Stats (getEffectiveStat, getOrRollStat)
import Unit.Physiology
import Unit.Physiology.Types
import Unit.Types

-- | The outcome of a physiology read, before any Lua marshalling.
data PhysioReadResult
    = ReadNoUnit
      -- ^ the unit does not exist.
    | ReadUnsupported
      -- ^ the key's system needs a capability the definition lacks.
    | ReadOwned !ResolvedMode !Float
      -- ^ a registry-owned key: its mode (never Unsupported) and value.
    | ReadUnowned !(Maybe Float)
      -- ^ an unowned key, answered by today's stat read; 'Nothing' when
      --   it is undefined.
    | ReadInvariant !Text
      -- ^ a violated precondition (a missing Simulated value, a missing
      --   healthy-rule input), described.
    deriving (Show, Eq)

-- | Read a key's EFFECTIVE value. The Haskell counterpart of
--   @unit.readPhysio@: pure for owned keys, today's 'getEffectiveStat'
--   for the rest.
physioRead ∷ EngineEnv → UnitId → Text → IO PhysioReadResult
physioRead = physioReadWith ReadEffective

-- | Read a key's stored BASE (@unit.readPhysioBase@).
physioReadBase ∷ EngineEnv → UnitId → Text → IO PhysioReadResult
physioReadBase = physioReadWith ReadBase

physioReadWith ∷ ReadKind → EngineEnv → UnitId → Text → IO PhysioReadResult
physioReadWith kind env uid key = do
    snapshot ← unitSnapshot env uid
    case snapshot of
        Nothing → pure ReadNoUnit
        Just ctx → case physioReadPure kind ctx key of
            OutcomeUnsupported       → pure ReadUnsupported
            OutcomeValue mode v      → pure (ReadOwned mode v)
            OutcomeInvariant why     → pure (ReadInvariant why)
            OutcomeUnowned           → ReadUnowned <$> case kind of
                ReadEffective → getEffectiveStat env uid key
                ReadBase      → getOrRollStat env uid key

-- | The unit's read context, or 'Nothing' for a unit that does not
--   exist. The override map is the unit's STORED intent
--   ('uiPhysioOverrides', S3, #2829): empty for every unit this build
--   creates, nonempty only when a save (or a test) carried one.
unitSnapshot ∷ EngineEnv → UnitId → IO (Maybe PhysioContext)
unitSnapshot env uid = do
    um  ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    reg ← readIORef (crPhysiologyRegistryRef (toContentRegistriesCapability env))
    now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    pure $ case HM.lookup uid (umInstances um) of
        Nothing   → Nothing
        Just inst → Just PhysioContext
            { pcRegistry   = reg
            , pcPhysiology = definitionPhysiology um inst
            , pcOverrides  = uiPhysioOverrides inst
            , pcNow        = now
            , pcStats      = uiStats inst
            , pcModifiers  = uiModifiers inst
            }

-- | What the unit's definition says; a unit whose definition is no
--   longer registered has no capabilities and so resolves Unsupported.
definitionPhysiology ∷ UnitManager → UnitInstance → UnitPhysiology
definitionPhysiology um inst =
    maybe noUnitPhysiology udPhysiology (HM.lookup (uiDefName inst) (umDefs um))

-- * Lua verbs

pushNilPair ∷ Lua.LuaE Lua.Exception Lua.NumResults
pushNilPair = Lua.pushnil >> Lua.pushnil >> return 2

readVerb ∷ ReadKind → EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
readVerb kind env = do
    idArg  ← Lua.tointeger 1
    keyArg ← Lua.tostring 2
    case (idArg, keyArg) of
        (Just n, Just keyBS) → do
            result ← Lua.liftIO $ physioReadWith kind env
                         (UnitId (fromIntegral n)) (TE.decodeUtf8Lenient keyBS)
            case result of
                ReadNoUnit       → pushNilPair
                ReadUnsupported  → pushNilPair
                ReadOwned mode v → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    maybe Lua.pushnil (Lua.pushstring ∘ TE.encodeUtf8)
                          (resolvedModeText mode)
                    return 2
                ReadUnowned (Just v) → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    Lua.pushnil
                    return 2
                ReadUnowned Nothing → pushNilPair
                ReadInvariant why → do
                    -- An invariant violation is a bug in the caller's
                    -- state, never a value: raised so it cannot be read
                    -- as "healthy" or "absent".
                    Lua.pushstring (TE.encodeUtf8 why)
                    Lua.error
        _ → pushNilPair

-- | @unit.readPhysio(uid, key)@ → @value, mode@ | @nil, nil@. EFFECTIVE:
--   for a Simulated owned key the stored base plus active modifiers; for
--   a Nominal one the healthy rule against effective inputs (modifiers on
--   the key itself ignored); meters of a Nominal system read 0. @mode@ is
--   @"nominal"@ \/ @"simulated"@, or @nil@ for an unowned key, whose
--   value is exactly what @unit.getStat@ returns. @nil, nil@ for an
--   Unsupported key, an undefined key or a missing unit.
unitReadPhysioFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitReadPhysioFn = readVerb ReadEffective

-- | @unit.readPhysioBase(uid, key)@ → @value, mode@ | @nil, nil@. As
--   'unitReadPhysioFn' but the stored BASE for a Simulated owned key
--   (what @unit.getStatBase@ returns for an unowned one). A Nominal owned
--   key has no base distinct from its healthy value, so it returns that;
--   a derived key has no stored base, so it returns the same on-demand
--   value as @readPhysio@.
unitReadPhysioBaseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitReadPhysioBaseFn = readVerb ReadBase

-- | @unit.getPhysioMode(uid, system)@ → @"nominal"@ | @"simulated"@ |
--   @nil@. @nil@ for an Unsupported system, an unknown system id or a
--   missing unit.
unitGetPhysioModeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPhysioModeFn env = do
    idArg  ← Lua.tointeger 1
    sysArg ← Lua.tostring 2
    mText ← case (idArg, sysArg) of
        (Just n, Just sysBS) → Lua.liftIO $ do
            snapshot ← unitSnapshot env (UnitId (fromIntegral n))
            pure $ do
                ctx ← snapshot
                sys ← registrySystem (pcRegistry ctx)
                           (TE.decodeUtf8Lenient sysBS)
                resolvedModeText (modeOf ctx sys)
        _ → pure Nothing
    maybe Lua.pushnil (Lua.pushstring ∘ TE.encodeUtf8) mText
    return 1

-- | @unit.getPhysioOverride(uid, system)@ → @"nominal"@ | @"simulated"@ |
--   @nil@: the unit's STORED override for the system, exactly as held
--   (S3, #2829) — including one equal to the species default, which is
--   still explicit intent. @nil@ when the unit has none, for an unknown
--   system id, or for a missing unit. Always @nil@ for a unit this
--   build created: no setter exists until S4.
unitGetPhysioOverrideFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPhysioOverrideFn env = do
    idArg  ← Lua.tointeger 1
    sysArg ← Lua.tostring 2
    mText ← case (idArg, sysArg) of
        (Just n, Just sysBS) → Lua.liftIO $ do
            snapshot ← unitSnapshot env (UnitId (fromIntegral n))
            pure $ do
                ctx ← snapshot
                physioModeText <$> HM.lookup (TE.decodeUtf8Lenient sysBS)
                                             (pcOverrides ctx)
        _ → pure Nothing
    maybe Lua.pushnil (Lua.pushstring ∘ TE.encodeUtf8) mText
    return 1

-- | @unit.getPhysioDefault(uid, system)@ → @"nominal"@ | @"simulated"@ |
--   @nil@: the definition's AUTHORED default for the system after
--   profile expansion, or @nil@ when it authors none. The resolver's
--   final Nominal fallback is not a default and is not reported as one.
unitGetPhysioDefaultFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPhysioDefaultFn env = do
    idArg  ← Lua.tointeger 1
    sysArg ← Lua.tostring 2
    mText ← case (idArg, sysArg) of
        (Just n, Just sysBS) → Lua.liftIO $ do
            snapshot ← unitSnapshot env (UnitId (fromIntegral n))
            pure $ do
                ctx ← snapshot
                physioModeText <$> HM.lookup (TE.decodeUtf8Lenient sysBS)
                                             (upDefaults (pcPhysiology ctx))
        _ → pure Nothing
    maybe Lua.pushnil (Lua.pushstring ∘ TE.encodeUtf8) mText
    return 1
