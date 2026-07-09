{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units.Stats
  ( unitGetSkillFn
  , unitSetSkillFn
  , unitGetKnowledgeFn
  , unitSetKnowledgeFn
  , unitGetKnowledgeListFn
  , unitAddXPFn
  , unitGetAllSkillsFn
  , unitGetStatFn
  , unitGetStatBaseFn
  , unitSetStatFn
  , unitGetAllStatsFn
  , unitAddModifierFn
  , unitRemoveModifierFn
  , unitGetModifiersFn
  , unitClearModifiersFn
  , getEffectiveStat
  )
    where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Unit.Types
import Unit.Stats (rollStat, effectiveStat, applySkillXP)


-- * Skills

-- | unit.getSkill(uid, name) — EFFECTIVE skill value (base + active
--   modifier deltas, clamped at 0). Phase F: skills now share the
--   modifier pipeline with stats, so a modifier on "balance" shifts
--   getSkill's result too. nil if undefined.
unitGetSkillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetSkillFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8Lenient nameBS
            mVal ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → pure Nothing
                    Just inst → case HM.lookup name (uiSkills inst) of
                        Nothing → pure Nothing
                        Just base → do
                            now ← readIORef (gameTimeRef env)
                            let mods = HM.lookupDefault [] name
                                          (uiModifiers inst)
                            pure (Just (effectiveStat now base mods))
            case mVal of
                Just v  → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.setSkill(uid, name, value) — debug override. Sets the skill
--   level directly. Clamps at 0 (negative skill is meaningless under
--   the XP formula). Returns true if the unit exists, false otherwise.
unitSetSkillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetSkillFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    valArg  ← Lua.tonumber 3
    case (idArg, nameArg, valArg) of
        (Just n, Just nameBS, Just (Lua.Number v)) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8Lenient nameBS
                lvl  = max 0 (realToFrac v)
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiSkills =
                                HM.insert name lvl (uiSkills inst) }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.getKnowledge(uid, name) → level | nil. nil means the unit does
--   NOT know it (presence in uiKnowledge = known). Raw stored level —
--   callers wanting effective capability multiply by the relevant stat
--   (e.g. intelligence) themselves.
unitGetKnowledgeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetKnowledgeFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8Lenient nameBS
            mVal ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure $ HM.lookup uid (umInstances um)
                         ⌦ (HM.lookup name . uiKnowledge)
            case mVal of
                Just v  → Lua.pushnumber (Lua.Number (realToFrac v)) >> return 1
                Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | unit.setKnowledge(uid, name, value) — grant/set a knowledge at `value`
--   (its mere presence marks it KNOWN). Clamps at 0. Used to seed knowledge
--   from a source (book/teacher) or for debug; spawn-known knowledge comes
--   from the unit def's `knowledge:` block. Returns true if the unit exists.
unitSetKnowledgeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetKnowledgeFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    valArg  ← Lua.tonumber 3
    case (idArg, nameArg, valArg) of
        (Just n, Just nameBS, Just (Lua.Number v)) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8Lenient nameBS
                lvl  = max 0 (realToFrac v)
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiKnowledge =
                                HM.insert name lvl (uiKnowledge inst) }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → Lua.pushboolean False >> return 1

-- | unit.getKnowledgeList(uid) → array of { name, level } for everything the
--   unit KNOWS. Empty table if it knows nothing / doesn't exist.
unitGetKnowledgeListFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetKnowledgeListFn env = do
    idArg ← Lua.tointeger 1
    Lua.newtable
    case idArg of
        Nothing → return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mInst ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (HM.lookup uid (umInstances um))
            case mInst of
                Nothing → return 1
                Just inst → do
                    forM_ (zip [1 ∷ Int ..] (HM.toList (uiKnowledge inst)))
                        $ \(i, (k, v)) → do
                            Lua.newtable
                            Lua.pushstring (TE.encodeUtf8 k)
                            Lua.setfield (-2) "name"
                            Lua.pushnumber (Lua.Number (realToFrac v))
                            Lua.setfield (-2) "level"
                            Lua.rawseti (-2) (fromIntegral i)
                    return 1

-- | unit.addXP(uid, name, amount) — apply XP to a stat OR skill via
--   @newValue = value + amount / max (value^2, 1e-4)@. The lookup
--   tries skills first (typical case), then stats — whichever the
--   unit has under that name gets nudged. There's no separate XP
--   accumulator. Returns the new value, or nil if the unit doesn't
--   have a stat or skill by that name.
--
--   Phase F: this used to be addSkillXP and required the name to be
--   in skills. Now stats can grow this way too — a unit performing
--   manual labour can @unit.addXP(uid, \"strength\", 0.01)@ to slowly
--   build their strength stat over time.
unitAddXPFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitAddXPFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    amtArg  ← Lua.tonumber 3
    case (idArg, nameArg, amtArg) of
        (Just n, Just nameBS, Just (Lua.Number amt)) → do
            let uid    = UnitId (fromIntegral n)
                name   = TE.decodeUtf8Lenient nameBS
                amount = realToFrac amt
            mVal ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, Nothing)
                        Just inst →
                            -- Try skills first (typical case), then
                            -- stats. Whichever map has the name owns
                            -- the value and gets updated.
                            case HM.lookup name (uiSkills inst) of
                                Just lvl →
                                    let lvl'  = applySkillXP lvl amount
                                        inst' = inst { uiSkills =
                                            HM.insert name lvl' (uiSkills inst) }
                                    in (um { umInstances = HM.insert uid inst'
                                                             (umInstances um) }
                                       , Just lvl')
                                Nothing → case HM.lookup name (uiStats inst) of
                                    Just v →
                                        let v'    = applySkillXP v amount
                                            inst' = inst { uiStats =
                                                HM.insert name v' (uiStats inst) }
                                        in (um { umInstances = HM.insert uid inst'
                                                                 (umInstances um) }
                                           , Just v')
                                    Nothing → (um, Nothing)
            case mVal of
                Just v → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.getAllSkills(uid) — returns a Lua table keyed by skill name
--   with @{ level }@ subtables. Phase F: @level@ is the EFFECTIVE
--   value (base + active modifiers, clamped) — matches unit.getSkill.
--   nil if the unit is missing.
unitGetAllSkillsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAllSkillsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mEntries ← Lua.liftIO $ do
                um  ← readIORef (unitManagerRef env)
                now ← readIORef (gameTimeRef env)
                pure $ do
                    inst ← HM.lookup uid (umInstances um)
                    def  ← HM.lookup (uiDefName inst) (umDefs um)
                    let tmpls = udSkillTemplates def
                        mods  = uiModifiers inst
                    pure
                        [ (name, eff)
                        | (name, _) ← HM.toList tmpls
                        , let base = HM.lookupDefault 0 name (uiSkills inst)
                              eff  = effectiveStat now base
                                       (HM.lookupDefault [] name mods)
                        ]
            case mEntries of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just entries → do
                    Lua.newtable
                    forM_ entries $ \(name, lvl) → do
                        Lua.newtable
                        Lua.pushnumber (Lua.Number (realToFrac lvl))
                        Lua.setfield (-2) "level"
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.insert (-2)
                        Lua.rawset (-3)
                    return 1

-- * Stats

-- | unit.getStat(id, name) — read the EFFECTIVE stat value (base +
--   active modifier deltas, clamped at 0). Lazy-rolls the base if the
--   unit type defines the stat but the value hasn't been rolled yet.
--   Returns nil if the unit doesn't exist or doesn't define this stat.
unitGetStatFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetStatFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8Lenient nameBS
            mVal ← Lua.liftIO $ getEffectiveStat env uid name
            case mVal of
                Just v  → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.getStatBase(id, name) — read the RAW rolled value with no
--   modifiers applied. Use this when you want the underlying base —
--   e.g. to compute how much a debuff is shifting the effective value.
--   Lazy-rolls if needed; returns nil if undefined or unit missing.
unitGetStatBaseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetStatBaseFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8Lenient nameBS
            mVal ← Lua.liftIO $ getOrRollStat env uid name
            case mVal of
                Just v  → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.setStat(id, name, value) — overwrite a stat. Clamps at >= 0.
--   Returns true if the unit exists, false otherwise. No definition
--   check: setting a stat the unit type doesn't declare is allowed
--   (the value becomes accessible via getStat from then on).
unitSetStatFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetStatFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    valArg  ← Lua.tonumber 3
    case (idArg, nameArg, valArg) of
        (Just n, Just nameBS, Just (Lua.Number v)) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8Lenient nameBS
                clamped = max 0 (realToFrac v)
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiStats =
                                HM.insert name clamped (uiStats inst) }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.getAllStats(id) — return a Lua table with every stat declared
--   by the unit's type. Values are EFFECTIVE (base + active modifiers,
--   clamped). Lazy-rolls any that haven't been rolled. nil if the unit
--   doesn't exist.
unitGetAllStatsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAllStatsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPairs ← Lua.liftIO $ effectiveAllStats env uid
            case mPairs of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just pairs → do
                    Lua.newtable
                    forM_ pairs $ \(name, v) → do
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.pushnumber (Lua.Number (realToFrac v))
                        Lua.rawset (-3)
                    return 1

-- | unit.addModifier(id, name, delta, source, durationSec, percent) —
--   add or replace a modifier on a stat. Same @source@ on
--   the same @name@ overwrites the previous entry; different sources
--   stack. @durationSec@ is optional (nil = permanent); when given it
--   is added to the current gameTimeRef value to produce smExpiry,
--   so modifier expiries survive save/load (gameTimeRef is restored
--   on load; POSIX wall-clock isn't). @percent@ is an optional
--   fractional multiplier contribution (0.5 = +50%, applied as
--   (base + Σdelta) × (1 + Σpercent)); nil/absent = 0 (additive only).
--   Returns true on success, false if the unit doesn't exist.
unitAddModifierFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitAddModifierFn env = do
    idArg     ← Lua.tointeger 1
    nameArg   ← Lua.tostring 2
    deltaArg  ← Lua.tonumber 3
    srcArg    ← Lua.tostring 4
    -- 5 may be nil (permanent) or a number (duration seconds).
    durMaybe  ← Lua.tonumber 5
    -- 6 may be nil (purely additive) or a fractional percent.
    pctMaybe  ← Lua.tonumber 6
    case (idArg, nameArg, deltaArg, srcArg) of
        (Just n, Just nameBS, Just (Lua.Number d), Just srcBS) → do
            let uid   = UnitId (fromIntegral n)
                name  = TE.decodeUtf8Lenient nameBS
                src   = TE.decodeUtf8Lenient srcBS
                delta = realToFrac d
                pct   = case pctMaybe of
                            Just (Lua.Number p) → realToFrac p
                            _                   → 0
            ok ← Lua.liftIO $ do
                expiry ← case durMaybe of
                    Just (Lua.Number dur) → do
                        now ← readIORef (gameTimeRef env)
                        pure (Just (now + realToFrac dur))
                    _ → pure Nothing
                let mod' = StatModifier
                        { smDelta  = delta
                        , smSource = src
                        , smExpiry = expiry
                        , smPercent = pct
                        }
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let existing = HM.lookupDefault []
                                              name (uiModifiers inst)
                                -- Drop any prior entry from the same source.
                                others = filter (\m → smSource m ≠ src)
                                                existing
                                newList = mod' : others
                                inst' = inst { uiModifiers =
                                    HM.insert name newList
                                        (uiModifiers inst) }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }
                               , True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.removeModifier(id, source) — remove every modifier owned
--   by @source@ across all stats. Returns the count of removed entries
--   (0 if unit missing or no matches).
unitRemoveModifierFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitRemoveModifierFn env = do
    idArg  ← Lua.tointeger 1
    srcArg ← Lua.tostring 2
    case (idArg, srcArg) of
        (Just n, Just srcBS) → do
            let uid = UnitId (fromIntegral n)
                src = TE.decodeUtf8Lenient srcBS
            removed ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, 0 ∷ Int)
                        Just inst →
                            let mods = uiModifiers inst
                                -- Count before, then filter each list.
                                cnt = sum [ length (filter
                                              (\m → smSource m ≡ src) ms)
                                          | ms ← HM.elems mods ]
                                pruned = HM.map (filter
                                            (\m → smSource m ≠ src)) mods
                                -- Drop now-empty entries to keep the
                                -- map tidy.
                                pruned' = HM.filter (not . null) pruned
                                inst' = inst { uiModifiers = pruned' }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }
                               , cnt)
            Lua.pushinteger (fromIntegral removed)
            return 1
        _ → do
            Lua.pushinteger 0
            return 1

-- | unit.getModifiers(id, name) — list every modifier on the named
--   stat as a Lua array of @{delta, percent, source, expiry}@ tables. Expired
--   entries are NOT filtered — caller can compare expiry to the game
--   clock (e.g. `engine.gameTime()`). NB: @expiry@ is in game-clock
--   seconds (stamped from `gameTimeRef` by `unit.addModifier`), NOT
--   POSIX epoch, so comparing it to `os.time()` is wrong. @expiry@ is
--   absent on permanent modifiers.
--   nil if the unit doesn't exist; empty array if no modifiers.
unitGetModifiersFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetModifiersFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8Lenient nameBS
            mMods ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing   → pure Nothing
                    Just inst → pure (Just
                        (HM.lookupDefault [] name (uiModifiers inst)))
            case mMods of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just mods → do
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] mods) $ \(i, m) → do
                        Lua.newtable
                        Lua.pushnumber (Lua.Number (realToFrac (smDelta m)))
                        Lua.setfield (-2) "delta"
                        Lua.pushnumber (Lua.Number (realToFrac (smPercent m)))
                        Lua.setfield (-2) "percent"
                        Lua.pushstring (TE.encodeUtf8 (smSource m))
                        Lua.setfield (-2) "source"
                        case smExpiry m of
                            Just t  → do
                                Lua.pushnumber (Lua.Number (realToFrac t))
                                Lua.setfield (-2) "expiry"
                            Nothing → pure ()
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.clearModifiers(id) — drop every modifier on every name
--   for this unit. Returns the count of removed entries. Mainly for
--   tests / debug; production code should usually use
--   removeModifier per-source.
unitClearModifiersFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitClearModifiersFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushinteger 0
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            cnt ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, 0 ∷ Int)
                        Just inst →
                            let c = sum (length <$>
                                         HM.elems (uiModifiers inst))
                                inst' = inst { uiModifiers = HM.empty }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }
                               , c)
            Lua.pushinteger (fromIntegral cnt)
            return 1

-- | Read a stat or skill's base value. Phase F: lookup is unified —
--   stats first, then skills as a fallback. Lazy-rolls a stat
--   template if the value isn't cached and eager_stats is false.
--   Returns Nothing if undefined under either category.
getOrRollStat ∷ EngineEnv → UnitId → Text → IO (Maybe Float)
getOrRollStat env uid name = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup uid (umInstances um) of
        Nothing → return Nothing
        Just inst →
            case HM.lookup name (uiStats inst) of
                Just v  → return (Just v)
                Nothing → case HM.lookup name (uiSkills inst) of
                    -- Skills are always eager-rolled; if it's in
                    -- uiSkills the value is final.
                    Just v  → return (Just v)
                    Nothing → case HM.lookup (uiDefName inst) (umDefs um) of
                        Nothing  → return Nothing
                        Just def → case HM.lookup name (udStatTemplates def) of
                            Nothing     → return Nothing
                            Just (b, r) → do
                                v ← atomicModifyIORef' (statRNGRef env) $ \g0 →
                                    let (val, g') = rollStat b r g0
                                    in (g', val)
                                -- Cache the rolled value. If the unit was
                                -- destroyed mid-roll, the lookup inside the
                                -- atomic modify finds nothing and we silently
                                -- drop — never resurrects a zombie unit.
                                atomicModifyIORef' (unitManagerRef env) $ \um' →
                                    case HM.lookup uid (umInstances um') of
                                        Nothing → (um', ())
                                        Just i  →
                                            let i' = i { uiStats =
                                                    HM.insert name v (uiStats i) }
                                            in (um' { umInstances =
                                                    HM.insert uid i'
                                                        (umInstances um') }, ())
                                return (Just v)

-- | Roll every stat declared by the unit's def that hasn't been rolled
--   yet, then return the full set as (name, value) pairs. Returns
--   Nothing if the unit doesn't exist.
rollAllDefinedStats ∷ EngineEnv → UnitId → IO (Maybe [(Text, Float)])
rollAllDefinedStats env uid = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup uid (umInstances um) of
        Nothing → return Nothing
        Just inst → case HM.lookup (uiDefName inst) (umDefs um) of
            Nothing → return (Just (HM.toList (uiStats inst)))
            Just def → do
                let templates = HM.toList (udStatTemplates def)
                -- Roll any missing stats (one getOrRollStat each).
                mapM_ (\(n, _) → getOrRollStat env uid n) templates
                -- Re-read for final values (including any pre-existing).
                um' ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um') of
                    Nothing    → return (Just [])
                    Just inst' → return (Just (HM.toList (uiStats inst')))

-- | Read a stat through the full Phase-C pipeline: lazy-roll the base
--   if needed, then apply this unit's active modifiers at the current
--   game time (gameTimeRef). Returns Nothing if undefined.
getEffectiveStat ∷ EngineEnv → UnitId → Text → IO (Maybe Float)
getEffectiveStat env uid name = do
    mBase ← getOrRollStat env uid name
    case mBase of
        Nothing   → pure Nothing
        Just base → do
            now ← readIORef (gameTimeRef env)
            um ← readIORef (unitManagerRef env)
            case HM.lookup uid (umInstances um) of
                Nothing   → pure (Just base)   -- destroyed mid-call
                Just inst →
                    let mods = HM.lookupDefault [] name
                                  (uiModifiers inst)
                    in pure (Just (effectiveStat now base mods))

-- | Like rollAllDefinedStats, but returns effective values (post-modifier).
--   Reads now once and applies it to every modifier list for consistency.
effectiveAllStats ∷ EngineEnv → UnitId → IO (Maybe [(Text, Float)])
effectiveAllStats env uid = do
    mBases ← rollAllDefinedStats env uid
    case mBases of
        Nothing    → pure Nothing
        Just bases → do
            now ← readIORef (gameTimeRef env)
            um ← readIORef (unitManagerRef env)
            case HM.lookup uid (umInstances um) of
                Nothing   → pure (Just bases)  -- destroyed mid-call
                Just inst →
                    let mods = uiModifiers inst
                        eff (n, b) =
                            (n, effectiveStat now b
                                  (HM.lookupDefault [] n mods))
                    in pure (Just (map eff bases))
