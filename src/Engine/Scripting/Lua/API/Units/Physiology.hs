{-# LANGUAGE Strict #-}
-- | The physiology verbs (#2828 S2 reads, #2831 S4 transitions; design
--   D-2, D-5, D-7, SD-19) and the Haskell 'physioRead' they share.
--
--   __Five read verbs.__ @unit.getPhysioMode@, @getPhysioOverride@,
--   @getPhysioDefault@, @readPhysio@ and @readPhysioBase@ are an explicit
--   exception to the facade-only rule the design (D-7) puts on
--   physiology access: any code may call them, including the console Lua
--   the probes send, because they never change physiology state.
--
--   __Two mutating verbs, facade-only.__ @unit.setPhysioMode@ and
--   @unit.clearPhysioMode@ are the engine half of
--   @scripts\/physiology.lua@'s @setMode@ \/ @clearMode@, and
--   @tools\/physiology_access_audit.py@ refuses any other caller. Each is
--   ONE unit-manager update ('applyTransition'): the override, the seeded
--   or scrubbed keys and the changed values are published together or not
--   at all, and a refusal leaves the unit exactly as it was. Planning is
--   "Unit.Physiology.Transition"'s; this module only applies it.
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
    , unitReadBloodViewFn
    , unitSetPhysioModeFn
    , unitClearPhysioModeFn
    , unitApplyCombatEngagementFn
    , applyTransition
    , applyEngagement
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
import Data.IORef (atomicModifyIORef', readIORef)
import Engine.Core.State (EngineEnv)
import Combat.Resolution.Common (isAlreadyDead)
import Engine.Scripting.Lua.API.Units.Stats (getEffectiveStat, getOrRollStat)
import Unit.Blood (BloodView(..), bloodView, bloodViewOf)
import Unit.Physiology
import Unit.Physiology.Transition
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
--   ('uiPhysioOverrides', S3, #2829).
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
            , pcBlood      = bloodOf um inst
            }

-- | The unit's blood through "Unit.Blood"; a unit whose definition is no
--   longer registered has no wounds that can be priced, so no bleed.
bloodOf ∷ UnitManager → UnitInstance → BloodView
bloodOf um inst = case HM.lookup (uiDefName inst) (umDefs um) of
    Just def → bloodView def inst
    Nothing  → bloodViewOf (uiStats inst) (uiBlood inst) 0

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

-- | @unit.readBloodView(uid)@ → @{ volume, max, fraction, bleedRate }@ |
--   @nil@ for a missing unit (#2836, S9). The pure Lua face of
--   "Unit.Blood".'Unit.Blood.bloodView': the unit's stored blood volume, its
--   maximum from BASE @body_mass@, the fraction clamped to @0..1@ and the
--   live wound bleed in litres per second. It is how cardio reaches its
--   blood input without changing @unit.getBlood@, whose record and current
--   behaviour stay as they are until S10 migrates it. Like the five read
--   verbs it changes nothing, so any code may call it.
unitReadBloodViewFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitReadBloodViewFn env = do
    idTy  ← Lua.ltype 1
    idArg ← Lua.tointeger 1
    mView ← case (idTy ≡ Lua.TypeNumber, idArg) of
        (True, Just n) → Lua.liftIO $ do
            um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
            pure (bloodOf um <$> HM.lookup (UnitId (fromIntegral n))
                                           (umInstances um))
        _ → pure Nothing
    case mView of
        Nothing → Lua.pushnil
        Just v → do
            Lua.newtable
            forM_ [ ("volume", bvVolume v), ("max", bvMax v)
                  , ("fraction", bvFraction v), ("bleedRate", bvBleedRate v) ] $
                \(k, x) → do
                    Lua.pushnumber (Lua.Number (realToFrac x))
                    Lua.setfield (-2) k
    return 1

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
--   unit nothing has set an override on.
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

-- * Transitions (S4, #2831)

-- | Plan a transition against the unit's CURRENT state and publish the
--   result, all inside ONE 'atomicModifyIORef'' on the unit manager: the
--   plan reads the state it replaces, so nothing can land between the
--   read and the write. 'Left' (an unknown unit or system, a dead unit,
--   an unplannable seed) writes nothing.
--
--   The registry and the game clock are read BEFORE the update: the
--   registry is immutable content and the clock only decides which
--   modifiers are active, so neither is part of the unit's own state.
applyTransition ∷ EngineEnv → UnitId → (PhysioContext → Either Text Transition)
                → IO (Either Text Transition)
applyTransition env uid plan = do
    reg ← readIORef (crPhysiologyRegistryRef (toContentRegistriesCapability env))
    now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
        case HM.lookup uid (umInstances um) of
            Nothing → (um, Left "no such unit")
            Just inst
                | uiPose inst ≡ "dead" → (um, Left "the unit is dead")
                | otherwise →
                    let ctx = PhysioContext
                            { pcRegistry   = reg
                            , pcPhysiology = definitionPhysiology um inst
                            , pcOverrides  = uiPhysioOverrides inst
                            , pcNow        = now
                            , pcStats      = uiStats inst
                            , pcModifiers  = uiModifiers inst
                            , pcBlood      = bloodOf um inst }
                    in case plan ctx of
                        Left why → (um, Left why)
                        Right t →
                            let inst' = inst { uiStats = trStats t
                                             , uiPhysioOverrides = trOverrides t }
                            in ( um { umInstances =
                                          HM.insert uid inst' (umInstances um) }
                               , Right t )

-- | Marshal a transition's outcome: @true, changed, from, to@ or
--   @nil, reason@. @from@ \/ @to@ are the resolved mode spellings.
pushOutcome ∷ Either Text Transition → Lua.LuaE Lua.Exception Lua.NumResults
pushOutcome (Left why) = do
    Lua.pushnil
    Lua.pushstring (TE.encodeUtf8 why)
    return 2
pushOutcome (Right t) = do
    Lua.pushboolean True
    Lua.pushboolean (transitionChangesMode t)
    mode (trFrom t)
    mode (trTo t)
    return 4
  where
    mode = maybe Lua.pushnil (Lua.pushstring ∘ TE.encodeUtf8) ∘ resolvedModeText

-- | A @{ key = number }@ table of EFFECTIVE target values. Every key must
--   be a string and every value a real number: a numeric string is NOT a
--   number here (@Lua.tointeger@ and @tonumber@ coerce one), so a
--   mistyped caller is refused rather than quietly converted.
readValuesTable ∷ Lua.StackIndex
                → Lua.LuaE Lua.Exception (Either Text [(Text, Float)])
readValuesTable idx = do
    Lua.pushvalue idx
    Lua.pushnil
    loop []
  where
    loop acc = do
        more ← Lua.next (-2)
        if not more
            then Lua.pop 1 ≫ pure (Right (reverse acc))
            else do
                kTy ← Lua.ltype (-2)
                vTy ← Lua.ltype (-1)
                case (kTy, vTy) of
                    (Lua.TypeString, Lua.TypeNumber) → do
                        mk ← Lua.tostring (-2)
                        mv ← Lua.tonumber (-1)
                        Lua.pop 1
                        case (mk, mv) of
                            (Just k, Just (Lua.Number v)) →
                                loop ((TE.decodeUtf8Lenient k, realToFrac v) : acc)
                            _ → bail
                    _ → bail
    bail = do
        -- Leave the stack as found: the key and the table copy.
        Lua.pop 2
        pure (Left "values must be a table of string keys to numbers")

-- | @unit.setPhysioMode(uid, system, mode [, values])@ →
--   @true, changed, from, to@ | @nil, reason@. FACADE-ONLY (the audit
--   enforces it): scripts call @physiology.setMode@, which also runs the
--   cache refresh and the change hooks. See 'applyTransition' for the
--   atomicity, and "Unit.Physiology.Transition" for what is seeded.
unitSetPhysioModeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetPhysioModeFn env = do
    idTy   ← Lua.ltype 1
    sysTy  ← Lua.ltype 2
    modeTy ← Lua.ltype 3
    valTy  ← Lua.ltype 4
    idArg  ← Lua.tointeger 1
    sysArg ← Lua.tostring 2
    modeArg ← Lua.tostring 3
    let wellTyped = idTy ≡ Lua.TypeNumber ∧ sysTy ≡ Lua.TypeString
                  ∧ modeTy ≡ Lua.TypeString
    case (wellTyped, idArg, sysArg, modeArg ⌦ parsePhysioMode ∘ TE.decodeUtf8Lenient) of
        (True, Just n, Just sysBS, Just mode)
            | valTy ≡ Lua.TypeNil ∨ valTy ≡ Lua.TypeNone →
                run n sysBS mode Nothing
            | valTy ≡ Lua.TypeTable → do
                parsed ← readValuesTable 4
                case parsed of
                    Left why → pushOutcome (Left why)
                    Right vs → run n sysBS mode (Just vs)
            | otherwise → pushOutcome (Left "values must be a table")
        _ → pushOutcome (Left "usage: setPhysioMode(uid, system, \"nominal\"|\"simulated\"[, values])")
  where
    run n sysBS mode mValues = do
        outcome ← Lua.liftIO $ applyTransition env (UnitId (fromIntegral n)) $
            \ctx → planSetMode ctx (TE.decodeUtf8Lenient sysBS) mode mValues
        pushOutcome outcome

-- | @unit.clearPhysioMode(uid, system)@ → @true, changed, from, to@ |
--   @nil, reason@. Facade-only; removes the override.
unitClearPhysioModeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitClearPhysioModeFn env = do
    idTy   ← Lua.ltype 1
    sysTy  ← Lua.ltype 2
    idArg  ← Lua.tointeger 1
    sysArg ← Lua.tostring 2
    case (idTy ≡ Lua.TypeNumber ∧ sysTy ≡ Lua.TypeString, idArg, sysArg) of
        (True, Just n, Just sysBS) → do
            outcome ← Lua.liftIO $ applyTransition env (UnitId (fromIntegral n)) $
                \ctx → planClearMode ctx (TE.decodeUtf8Lenient sysBS)
            pushOutcome outcome
        _ → pushOutcome (Left "usage: clearPhysioMode(uid, system)")

-- * Combat engagement (SD-20, #2834)

-- | Apply a unit's PENDING engagement in ONE unit-manager update
--   (design S8.3): the dead check (the combat lethality rule, not the
--   pose mirror alone), the explicit-override latch or the Nominal →
--   Simulated seed, the pinned override and marker 2 are published
--   together or not at all, so a strike committing on the combat worker
--   is entirely before or entirely after. 'Left' is "no such unit" or
--   "nothing pending to apply"; a refusal mutates nothing.
applyEngagement ∷ EngineEnv → UnitId → IO (Either Text EngagementOutcome)
applyEngagement env uid = do
    reg ← readIORef (crPhysiologyRegistryRef (toContentRegistriesCapability env))
    now ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
        case HM.lookup uid (umInstances um) of
            Nothing → (um, Left "no such unit")
            Just inst →
                let mDef = HM.lookup (uiDefName inst) (umDefs um)
                    dead = uiPose inst ≡ "dead" ∨ maybe False (isAlreadyDead inst) mDef
                    ctx = PhysioContext
                        { pcRegistry   = reg
                        , pcPhysiology = definitionPhysiology um inst
                        , pcOverrides  = uiPhysioOverrides inst
                        , pcNow        = now
                        , pcStats      = uiStats inst
                        , pcModifiers  = uiModifiers inst
                        , pcBlood      = bloodOf um inst }
                in case planEngagement ctx of
                    Left EngagementNotPending → (um, Left "not pending")
                    Left (EngagementSeedRefused code) →
                        if dead then (um, Right EngagementRefusedDead)
                                else (um, Right (EngagementSeedUnrepresentable code))
                    Right plan
                        | dead → (um, Right EngagementRefusedDead)
                        | otherwise →
                            let inst' = inst { uiStats = epStats plan
                                             , uiPhysioOverrides = epOverrides plan }
                            in ( um { umInstances = HM.insert uid inst' (umInstances um) }
                               , Right (epOutcome plan) )

-- | @unit.applyCombatEngagement(uid)@ → @"transitioned"@ | @"latched"@ |
--   @"refusedDead"@ | @"seedUnrepresentable", code@ | @nil, reason@.
--   FACADE-ONLY (the audit enforces it): @scripts\/physiology.lua@ calls
--   it for each pending unit at the start of an eligible pass and, on
--   @transitioned@, runs the hooks and the refresh. See 'applyEngagement'.
unitApplyCombatEngagementFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitApplyCombatEngagementFn env = do
    idTy  ← Lua.ltype 1
    idArg ← Lua.tointeger 1
    case (idTy ≡ Lua.TypeNumber, idArg) of
        (True, Just n) | n ≥ 0 ∧ n ≤ 4294967295 → do
            outcome ← Lua.liftIO $ applyEngagement env (UnitId (fromIntegral n))
            case outcome of
                Left why → do
                    Lua.pushnil
                    Lua.pushstring (TE.encodeUtf8 why)
                    return 2
                Right o → do
                    Lua.pushstring (TE.encodeUtf8 (engagementOutcomeText o))
                    case o of
                        EngagementSeedUnrepresentable code → do
                            Lua.pushstring (TE.encodeUtf8 code)
                            return 2
                        _ → return 1
        _ → do
            Lua.pushnil
            Lua.pushstring "invalid_unit_id"
            return 2
