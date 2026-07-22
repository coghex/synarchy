{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units.Combat
  ( unitGetWoundSeverityOnFn
  , unitGetWoundsFn
  , unitGetScarsFn
  , unitGetImmunitiesFn
  , unitGetInsulationFn
  , unitGetBloodFn
  , unitGetLastAttackerFn
  , unitGetPainFn
  , unitGetMentalEffectivenessFn
  , unitInjureFn
  )
    where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import qualified Data.List as L
import Engine.Core.State (EngineEnv(..))
import Infection.Types (InfectionDef(..), lookupInfection)
import Unit.Types
import Combat.Wounds (bleedRateFor)
import Combat.Types (pushInjuryEvent)
import Combat.Resolution.Common (mentalEffectiveness)
import Blood.Impact (spawnImpactBlood, impactFallbackAngle)
import Item.Types (ItemInstance(..), ItemDef(..), lookupItemDef)


-- | unit.getWoundSeverityOn(uid, partId) → float | nil
--
--   Sum of EFFECTIVE severity (heal eases it, necrosis floors it) for
--   all current wounds on the given body part — so the gate eases as the
--   wound mends, matching the bleed/pain consumers. Used by the AI's
--   attack-mode picker and cooldown formula to gate heavy attacks when
--   the weapon arm is hurt. Returns 0 (not nil) for
--   a part with no wounds — only returns nil if the unit itself
--   doesn't exist.
unitGetWoundSeverityOnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetWoundSeverityOnFn env = do
    idArg   ← Lua.tointeger 1
    partArg ← Lua.tostring 2
    case (idArg, partArg) of
        (Just n, Just pbs) → do
            let uid     = UnitId (fromIntegral n)
                partId  = TE.decodeUtf8Lenient pbs
            mTotal ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst →
                        let s = sum [ woundEffSeverity w
                                    | w ← uiWounds inst
                                    , woundPart w ≡ partId ]
                        in return (Just s)
            case mTotal of
                Just s  → Lua.pushnumber (Lua.Number (realToFrac s))
                            >> return 1
                Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | unit.getWounds(uid) → array of { part, kind, severity, at } | nil
--
--   Newest-first. Sub-cleanup-threshold wounds are removed by the
--   per-tick wound subsystem so this only returns currently-active
--   wounds. Returns nil if unit doesn't exist.
unitGetWoundsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetWoundsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            infMgr ← Lua.liftIO $ readIORef (infectionManagerRef env)
            mWounds ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure $ do
                    inst ← HM.lookup uid (umInstances um)
                    let parts = maybe [] udBodyParts
                                  (HM.lookup (uiDefName inst) (umDefs um))
                    pure (uiWounds inst, parts)
            case mWounds of
                Nothing → Lua.pushnil >> return 1
                Just (ws, parts) → do
                    -- Index for the subpart → macro-part rollup + vital
                    -- lookup, so Lua impairment/death logic can work on
                    -- subpart wounds without knowing the body tree.
                    let idx = HM.fromList [(bpId p, p) | p ← parts]
                        vitalOf pid = maybe False bpVital (HM.lookup pid idx)
                        -- Walk parents until a targetable macro-part (the
                        -- limb the combat log names); bounded by part count.
                        macroOf pid = climb (length parts) pid
                        climb 0 pid = pid
                        climb k pid = case HM.lookup pid idx of
                            Just p | bpTargetable p → pid
                                   | otherwise → case bpParent p of
                                       Just par → climb (k - 1) par
                                       Nothing  → pid
                            Nothing → pid
                    Lua.newtable
                    forM_ (zip [1..] ws) $ \(i, w) → do
                        Lua.newtable
                        Lua.pushstring (TE.encodeUtf8 (woundPart w))
                        Lua.setfield (-2) "part"
                        Lua.pushstring (TE.encodeUtf8 (macroOf (woundPart w)))
                        Lua.setfield (-2) "macro"
                        Lua.pushboolean (vitalOf (woundPart w))
                        Lua.setfield (-2) "vital"
                        Lua.pushstring (TE.encodeUtf8 (woundKind w))
                        Lua.setfield (-2) "kind"
                        -- `severity` is the ACUTE/mechanical effective
                        -- severity (inflicted × (1 − heal)) — the trauma
                        -- itself, easing as the wound mends. It deliberately
                        -- does NOT fold in the necrosis floor: rot is a
                        -- separate failure mode (gangrene/sepsis, with its
                        -- own death path) reported separately as `necrosis`,
                        -- so the acute organ-failure meters (suffocation,
                        -- neuro, shock, visceral) read this without a rotting
                        -- wound spuriously driving an acute-trauma meter.
                        -- Consumers that drive IMPAIRMENT/bleed (limp/crawl,
                        -- injured-anim, bleeding) read `severityEffective`
                        -- below instead. `severityInflicted` is the original,
                        -- `heal` the 0..1 healing progress.
                        Lua.pushnumber (Lua.Number (realToFrac
                            (woundSeverity w * (1 - woundHeal w))))
                        Lua.setfield (-2) "severity"
                        -- `severityEffective` is the engine's full effective
                        -- severity — max(acute, necrosis) via woundEffSeverity,
                        -- the SAME value the Haskell bleed/pain/impairment
                        -- paths (bleedRateFor, painFor, injurySpeedMult, …)
                        -- use. Lua consumers that must stay in lockstep with
                        -- those (locomotion limp/crawl, injured-anim, bleeding
                        -- badge) read this so a healed-but-necrotic wound still
                        -- impairs, matching the engine.
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundEffSeverity w)))
                        Lua.setfield (-2) "severityEffective"
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundSeverity w)))
                        Lua.setfield (-2) "severityInflicted"
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundHeal w)))
                        Lua.setfield (-2) "heal"
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundAt w)))
                        Lua.setfield (-2) "at"
                        -- First-aid dressing: fraction of natural bleed
                        -- that still seeps (1.0 untreated, < 1.0 = bandaged).
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundBandage w)))
                        Lua.setfield (-2) "bandage"
                        -- Clotting progress 0..1 (bleed × (1−clot); 1 = stopped).
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundClot w)))
                        Lua.setfield (-2) "clot"
                        -- Dressing type: "" / "bandage" / "tourniquet".
                        Lua.pushstring (TE.encodeUtf8 (woundDressing w))
                        Lua.setfield (-2) "dressing"
                        -- Infection level 0..1 (drives the sepsis meter;
                        -- antibiotics cure it) and the clean/disinfected flag
                        -- (antiseptic prevention — a clean wound won't infect).
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundInfection w)))
                        Lua.setfield (-2) "infection"
                        Lua.pushboolean (woundClean w)
                        Lua.setfield (-2) "clean"
                        -- Which infection (id) + its display name / icon /
                        -- category, resolved from the catalogue. Empty id =
                        -- not yet typed (generic "infected" naming in Lua).
                        Lua.pushstring (TE.encodeUtf8 (woundInfectionType w))
                        Lua.setfield (-2) "infectionType"
                        let mInf = if woundInfectionType w ≡ ""
                                     then Nothing
                                     else lookupInfection (woundInfectionType w) infMgr
                        Lua.pushstring (TE.encodeUtf8
                            (maybe "" infName mInf))
                        Lua.setfield (-2) "infectionName"
                        Lua.pushstring (TE.encodeUtf8
                            (maybe "" infIcon mInf))
                        Lua.setfield (-2) "infectionIcon"
                        Lua.pushstring (TE.encodeUtf8
                            (maybe "" infCategory mInf))
                        Lua.setfield (-2) "infectionCategory"
                        -- Necrosis (dead tissue) 0..1: drives the rot display
                        -- + the gangrene death cause.
                        Lua.pushnumber (Lua.Number
                            (realToFrac (woundNecrosis w)))
                        Lua.setfield (-2) "necrosis"
                        Lua.rawseti (-2) i
                    return 1

-- | unit.getScars(uid) → array of { part, kind, severity, at } | nil
--   Permanent marks left by severe wounds that finished healing.
--   `severity` is the wound's inflicted severity (how bad it was);
--   `at` is the game-time it healed. Empty table if none; nil if the
--   unit doesn't exist.
unitGetScarsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetScarsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mScars ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiScars <$> HM.lookup uid (umInstances um))
            case mScars of
                Nothing → Lua.pushnil >> return 1
                Just scars → do
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] scars) $ \(i, sc) → do
                        Lua.newtable
                        Lua.pushstring (TE.encodeUtf8 (scarPart sc))
                        Lua.setfield (-2) "part"
                        Lua.pushstring (TE.encodeUtf8 (scarKind sc))
                        Lua.setfield (-2) "kind"
                        Lua.pushnumber (Lua.Number (realToFrac (scarSeverity sc)))
                        Lua.setfield (-2) "severity"
                        Lua.pushnumber (Lua.Number (realToFrac (scarAt sc)))
                        Lua.setfield (-2) "at"
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1

-- | unit.getImmunities(uid) → array of { type, name, icon, level } | nil.
--   Acquired per-type immunity (level 0..1), resolved to display name + icon
--   from the infection catalogue (falls back to the id + immunity.png).
--   Sorted strongest-first. Empty table if none.
unitGetImmunitiesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetImmunitiesFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            infMgr ← Lua.liftIO $ readIORef (infectionManagerRef env)
            mImm ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiImmunities <$> HM.lookup uid (umInstances um))
            case mImm of
                Nothing → Lua.pushnil >> return 1
                Just immMap → do
                    let rows = L.sortBy (\a b → compare (snd b) (snd a))
                                        (HM.toList immMap)
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] rows) $ \(i, (tid, lvl)) → do
                        let mInf = lookupInfection tid infMgr
                        Lua.newtable
                        Lua.pushstring (TE.encodeUtf8 tid)
                        Lua.setfield (-2) "type"
                        Lua.pushstring (TE.encodeUtf8 (maybe tid infName mInf))
                        Lua.setfield (-2) "name"
                        Lua.pushstring (TE.encodeUtf8 "immunity")
                        Lua.setfield (-2) "icon"
                        Lua.pushnumber (Lua.Number (realToFrac lvl))
                        Lua.setfield (-2) "level"
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1

-- | unit.getInsulation(uid) → total thermal insulation (Float) of the unit's
--   worn gear — summed `idInsulation` over equipped items + accessories. Read
--   by scripts/thermo.lua to slow heat loss (dress for the climate). 0 if the
--   unit doesn't exist or wears nothing insulating.
unitGetInsulationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetInsulationFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnumber 0 >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            total ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                im ← readIORef (itemManagerRef env)
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing → 0
                    Just inst →
                        let worn = HM.elems (uiEquipment inst)
                                 ++ uiAccessories inst
                            insOf it = maybe 0 idInsulation
                                         (lookupItemDef (iiDefName it) im)
                        in sum (map insOf worn)
            Lua.pushnumber (Lua.Number (realToFrac (total ∷ Float)))
            return 1

-- | unit.getBlood(uid) → { current, max } | nil
--
--   max is body_mass × 'bloodMassRatio'. current is the
--   spawn-time-seeded value minus per-tick bleed loss. Both in
--   litres.
unitGetBloodFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetBloodFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPair ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst →
                        let bm = HM.lookupDefault 70.0 "body_mass"
                                                 (uiStats inst)
                            rate = maybe 0.0 (\d → bleedRateFor d inst)
                                     (HM.lookup (uiDefName inst) (umDefs um))
                        in return $ Just (uiBlood inst, bm * bloodMassRatio, rate)
            case mPair of
                Nothing → Lua.pushnil >> return 1
                Just (cur, mx, rate) → do
                    Lua.newtable
                    Lua.pushnumber (Lua.Number (realToFrac cur))
                    Lua.setfield (-2) "current"
                    Lua.pushnumber (Lua.Number (realToFrac mx))
                    Lua.setfield (-2) "max"
                    Lua.pushnumber (Lua.Number (realToFrac rate))
                    Lua.setfield (-2) "bleedRate"
                    return 1

-- | unit.getLastAttacker(uid) → { uid, at } | nil
--
--   Reads the runtime-only last-attacker memory written by
--   Combat.Resolution. Returns nil if no incoming hit has landed
--   yet on this unit. The AI is responsible for deciding whether
--   the memory is fresh enough to act on (compare `at` against
--   `engine.gameTime()`).
unitGetLastAttackerFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetLastAttackerFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mAtt ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing   → return Nothing
                    Just inst → case uiLastAttackerUid inst of
                        Nothing     → return Nothing
                        Just attRaw → return $ Just
                                          (attRaw, uiLastAttackerAt inst)
            case mAtt of
                Nothing → Lua.pushnil >> return 1
                Just (attRaw, at) → do
                    Lua.newtable
                    Lua.pushinteger (fromIntegral attRaw)
                    Lua.setfield (-2) "uid"
                    Lua.pushnumber (Lua.Number (realToFrac at))
                    Lua.setfield (-2) "at"
                    return 1

-- | unit.getPain(uid) → number | nil
--
--   Pain accumulator derived live from the wound list:
--     pain = sum(effective_severity × kind_pain_factor)
--   (effective severity eases as the wound heals, floored by necrosis).
--   Used by the AI (future retreat candidate), the unit-info UI,
--   and the combat hit-roll's pain penalty (computed Haskell-side
--   in Combat.Resolution; this getter mirrors the same formula
--   for Lua reads). Unclamped (the resolution code clamps via
--   painCeiling internally).
unitGetPainFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPainFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPain ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst →
                        let pain = sum
                              [ woundEffSeverity w * kindPainFactor
                                                      (woundKind w)
                              | w ← uiWounds inst ]
                        in return $ Just pain
            case mPain of
                Just p  → Lua.pushnumber (Lua.Number (realToFrac p))
                                >> return 1
                Nothing → Lua.pushnil >> return 1
  where
    -- Mirror Combat.Resolution.kindPainFactor. Kept in lockstep
    -- manually; if either changes, update both.
    kindPainFactor ∷ Text → Float
    kindPainFactor "slash" = 1.0
    kindPainFactor "stab"  = 1.2
    kindPainFactor "blunt" = 1.5
    kindPainFactor _       = 1.0

-- | unit.getMentalEffectiveness(uid) → number | nil
--
--   The #353 canonical mental-effectiveness multiplier (0.75..1.10),
--   read straight off 'Combat.Resolution.Common.mentalEffectiveness' —
--   the SAME function combat's hit/active-dodge rolls and craft-quality
--   consume, so this getter can never drift from either. Used by the
--   craft-bill AI (scripts/unit_ai_craft.lua) to scale its per-tick
--   progress pour.
unitGetMentalEffectivenessFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetMentalEffectivenessFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mEff ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                return $ mentalEffectiveness ⊚ HM.lookup uid (umInstances um)
            case mEff of
                Just e  → Lua.pushnumber (Lua.Number (realToFrac e))
                                >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.injure(uid, part, kind, severity [, bandage]) → bool
--   Stamp an arbitrary wound onto a unit — the Lua hook into the
--   injury system (debug, arena testing, and future gameplay events
--   like traps/hazards). `part` is a body-part id (e.g. "l_thigh",
--   "neck", "torso"), `kind` a wound kind ("slash"/"stab"/"blunt"/
--   "arterial"/"severed"/"internal"/"fracture"/"concussion"),
--   `severity` 0..1, optional `bandage` 0..1 (default 1 = untreated).
--   Prepended so it reads newest-first like a combat wound. Returns
--   false if the unit is missing or required args are absent.
unitInjureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitInjureFn env = do
    idArg   ← Lua.tointeger 1
    partArg ← Lua.tostring 2
    kindArg ← Lua.tostring 3
    sevArg  ← Lua.tonumber 4
    bandArg ← Lua.tonumber 5
    case (idArg, partArg, kindArg, sevArg) of
        (Just n, Just partBS, Just kindBS, Just (Lua.Number sev)) → do
            now ← Lua.liftIO $ readIORef (gameTimeRef env)
            let uid = UnitId (fromIntegral n)
                bandage = case bandArg of
                    Just (Lua.Number b) → max 0 (min 1 (realToFrac b))
                    _                   → 1.0
                w = Wound
                    { woundPart     = TE.decodeUtf8Lenient partBS
                    , woundKind     = TE.decodeUtf8Lenient kindBS
                    , woundSeverity = max 0 (min 1 (realToFrac sev))
                    , woundAt       = now
                    , woundBandage  = bandage
                    , woundClot     = 0.0
                    , woundHeal     = 0.0
                    , woundDressing = ""
                    , woundInfection = 0.0
                    , woundClean    = False
                    , woundInfectionType = ""
                    , woundNecrosis = 0.0 }
            mPos ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing   → (um, Nothing)
                    Just inst →
                        let inst' = inst { uiWounds = w : uiWounds inst }
                        in ( um { umInstances =
                                    HM.insert uid inst' (umInstances um) }
                           , Just (uiPage inst, uiGridX inst, uiGridY inst, uiGridZ inst) )
            let ok = mPos ≢ Nothing
            -- A successful (non-combat) wound feeds the injury log.
            Lua.liftIO $ when ok $
                pushInjuryEvent (injuryEventsRef env) now (fromIntegral n)
                    "injure"
                    [ ("part",      TE.decodeUtf8Lenient partBS)
                    , ("woundKind", TE.decodeUtf8Lenient kindBS)
                    , ("severity",  T.pack (show (woundSeverity w))) ]
            -- Impact blood (#607): unit.injure has no attacker, so
            -- direction always falls back to a deterministic seeded
            -- angle (requirement 7) — this debug path exists precisely
            -- so headless probes can exercise every wound kind without
            -- staging a real attack (requirement 2).
            Lua.liftIO $ case mPos of
                Nothing → pure ()
                Just (page, gx, gy, gz) → do
                    let seed  = round (now * 1000.0) + fromIntegral n
                        angle = impactFallbackAngle seed
                    spawnImpactBlood env page gx gy gz
                        (TE.decodeUtf8Lenient kindBS) (woundSeverity w)
                        angle seed (Just uid) now
            Lua.pushboolean ok
            return 1
        _ → Lua.pushboolean False >> return 1
