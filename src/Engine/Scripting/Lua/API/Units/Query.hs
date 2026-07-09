{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units.Query
  ( unitGetAttackRangeFn
  , unitGetAttackCooldownFn
  , unitGetAnimDurationFn
  , unitGetMaxSpeedFn
  , unitGetEquippedWeaponWeightFn
  , unitGetWeaponWieldedFromFn
  , unitGetWeaponClassFn
  , unitGetJumpReachFn
  , unitLungeImpactSpeedFn
  , unitGetActivityFn
  , unitGetCurrentAnimFn
  , unitGetVisibleTilesFn
  )
    where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import Unit.Types
import Unit.Thread.Movement (jumpMaxTiles, maxJumpHeight, lungeImpactSpeed)
import Unit.LineOfSight (unitVisibleTiles)
import Item.Types (ItemInstance(..), ItemDef(..), ItemWeapon(..), ItemManager(..), lookupItemDef)


-- | unit.getAttackRange(uid) → float (tiles) | nil
--
--   Computes melee reach as `(height / 2.4) + (blade_length / 100)`
--   where height is in metres (read from `uiStats["height"]`) and
--   blade_length is in centimetres. Looks up the blade length from
--   (in priority order):
--     1. equipped right_hand weapon
--     2. equipped left_hand weapon
--     3. unit-def's natural_weapon.effective_blade_length
--     4. 0 (bare arm reach only)
--   Returns nil if the unit doesn't exist OR has no rolled height
--   stat.
unitGetAttackRangeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAttackRangeFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mRange ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                im ← readIORef (itemManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → case HM.lookup "height" (uiStats inst) of
                        Nothing → return Nothing
                        Just h  →
                            let armReach = h / 2.4
                                blade    = resolveBladeLength im um inst
                            in return (Just (armReach + blade / 100.0))
            case mRange of
                Just r  → Lua.pushnumber (Lua.Number (realToFrac r))
                                >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.getAttackCooldown(uid) → float seconds | nil
--
--   Cooldown between swings. Read from the equipped weapon
--   (right_hand → left_hand) or natural_weapon, with a 1.5s fallback.
unitGetAttackCooldownFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAttackCooldownFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mCD ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                im ← readIORef (itemManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → return $ Just (resolveCooldown im um inst)
            case mCD of
                Just c  → Lua.pushnumber (Lua.Number (realToFrac c))
                                >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.getAnimDuration(uid, animName) → float seconds | nil
--
--   Total play time of one animation: frame count / fps (frames are the
--   longest per-direction track). Used by combat to hold a one-shot
--   swing override on screen for its real length before the AI reverts
--   to the combat-idle stance — otherwise the next AI tick overwrites
--   the swing before a single frame shows.
unitGetAnimDurationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAnimDurationFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBs) → do
            let uid      = UnitId (fromIntegral n)
                animName = TE.decodeUtf8Lenient nameBs
            mDur ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → case HM.lookup (uiDefName inst) (umDefs um) of
                        Nothing  → return Nothing
                        Just def → case HM.lookup animName (udAnimations def) of
                            Nothing → return Nothing
                            Just an →
                                let fps     = aFps an
                                    nFrames = maximum
                                        (0 : map V.length
                                                 (Map.elems (aFrames an)))
                                in if fps ≤ 0 ∨ nFrames ≡ 0
                                   then return Nothing
                                   else return
                                       (Just (fromIntegral nFrames / fps))
            case mDur of
                Just (d ∷ Float) → Lua.pushnumber (Lua.Number (realToFrac d))
                                       >> return 1
                Nothing          → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | unit.getMaxSpeed(uid) → float (tiles/sec) | nil
--
--   Per-species top speed at a full sprint, read from the unit-def's
--   udMaxSpeed. AI candidates derive their per-action speed as a
--   fraction of this. Stat-based modulation (agility, injuries) lives
--   elsewhere — this returns the raw def-level cap.
unitGetMaxSpeedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetMaxSpeedFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mSpeed ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → case HM.lookup (uiDefName inst)
                                              (umDefs um) of
                        Just d  → return (Just (udMaxSpeed d))
                        Nothing → return Nothing
            case mSpeed of
                Just s  → Lua.pushnumber (Lua.Number (realToFrac s))
                                >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.getEquippedWeaponWeight(uid) → float kg | nil
--
--   The "weight" of whatever weapon would swing if this unit attacked
--   right now. For equipped weapons: the item's idWeight. For natural
--   weapons (bear paws, fists): derived as body_mass × 0.04 — a paw is
--   ~4% of the animal's mass, the same fraction across species so we
--   never need a per-creature knob. Returns nil if the unit doesn't
--   exist.
unitGetEquippedWeaponWeightFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetEquippedWeaponWeightFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mW ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                im ← readIORef (itemManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → return $ Just
                        (resolveWeaponWeight im inst)
            case mW of
                Just w  → Lua.pushnumber (Lua.Number (realToFrac w))
                            >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.getWeaponWieldedFrom(uid) → string body-part id | nil
--
--   The body part the swing originates from. Used by the cooldown
--   formula to look up the wound severity on that part — injured
--   arms slow you down. For equipped weapons: derived from the slot
--   ("right_hand" → "right_arm", "left_hand" → "left_arm"). For
--   natural weapons: a species-default. Stage 4 will add a YAML
--   override (`wielded_from:` on weapon / natural_weapon blocks) for
--   creatures whose claws hang off a non-default body part.
unitGetWeaponWieldedFromFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetWeaponWieldedFromFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPart ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → return $ Just
                        (resolveWieldedFrom (uiEquipment inst)
                                            (uiDefName inst))
            case mPart of
                Just p  → Lua.pushstring (TE.encodeUtf8 p) >> return 1
                Nothing → Lua.pushnil >> return 1

-- | Walk slot priority for an equipped weapon's blade length; fall
--   back to the unit-def's natural_weapon; finally 0.
resolveBladeLength
    ∷ ItemManager → UnitManager → UnitInstance → Float
resolveBladeLength im um inst =
    case firstEquippedWeapon im (uiEquipment inst) of
        Just w  → iwBladeLength w
        Nothing → case HM.lookup (uiDefName inst) (umDefs um) of
            Just d  → maybe 0.0 nwEffectiveBladeLength (udNaturalWeapon d)
            Nothing → 0.0

-- | Weapon weight for the currently-wielded weapon. Equipped item:
--   idWeight (kg). Natural weapon: body_mass × 0.04 — a small fraction
--   of body mass that scales correctly across species (a bear paw at
--   ~12 kg, a small predator's claw at <1 kg) without per-species
--   tuning. Returns 0 if no weapon and no body_mass stat exists.
resolveWeaponWeight ∷ ItemManager → UnitInstance → Float
resolveWeaponWeight im inst =
    case firstEquippedItemDef im (uiEquipment inst) of
        Just d  → idWeight d
        Nothing →
            let bodyMass = HM.lookupDefault 0.0 "body_mass" (uiStats inst)
            in  bodyMass * 0.04

-- | Body part the wielded weapon swings from. Slot-driven for
--   equipped weapons; species-defaulted for natural weapons. Returns
--   a body-part id that unit.getWoundSeverityOn can look up — must
--   match the `id` declared in the unit YAML's body_parts list.
--
--   Future: prefer a YAML `wielded_from:` field on the weapon /
--   natural_weapon block once that's parsed, so creatures whose
--   claws hang off non-default body parts (tail strikes, head butts)
--   can declare it in data.
resolveWieldedFrom
    ∷ HM.HashMap Text ItemInstance → Text → Text
resolveWieldedFrom eq defName
    | HM.member "right_hand" eq = "r_arm"
    | HM.member "left_hand"  eq = "l_arm"
    | otherwise                  = case defName of
        "bear_brown" → "r_fore_leg"
        "acolyte"    → "r_arm"
        _            → "head"

-- | Same priority chain for cooldown; default 1.5 s if nothing
--   useful is found.
resolveCooldown
    ∷ ItemManager → UnitManager → UnitInstance → Float
resolveCooldown im um inst =
    case firstEquippedWeapon im (uiEquipment inst) of
        Just w  → iwAttackCooldown w
        Nothing → case HM.lookup (uiDefName inst) (umDefs um) of
            Just d  → maybe 1.5 nwAttackCooldown (udNaturalWeapon d)
            Nothing → 1.5

firstEquippedWeapon
    ∷ ItemManager → HM.HashMap Text ItemInstance → Maybe ItemWeapon
firstEquippedWeapon im eq = go ["right_hand", "left_hand"]
  where
    go [] = Nothing
    go (slot:rest) = case HM.lookup slot eq of
        Nothing → go rest
        Just it → case lookupItemDef (iiDefName it) im of
            Just d | Just w ← idWeapon d → Just w
            _ → go rest

-- | Same slot priority as firstEquippedWeapon, but returns the full
--   ItemDef so callers can read non-weapon fields (idWeight, idMaterial).
--   Only matches slots whose item has a weapon block — i.e. the unit
--   is actually wielding a weapon, not a torch.
firstEquippedItemDef
    ∷ ItemManager → HM.HashMap Text ItemInstance → Maybe ItemDef
firstEquippedItemDef im eq = go ["right_hand", "left_hand"]
  where
    go [] = Nothing
    go (slot:rest) = case HM.lookup slot eq of
        Nothing → go rest
        Just it → case lookupItemDef (iiDefName it) im of
            Just d | Just _ ← idWeapon d → Just d
            _ → go rest

-- | unit.getWeaponClass(uid) → string | nil
--
--   The active weapon-class string ("dagger", "unarmed", …) the
--   unit would actually use to swing right now. Resolution chain:
--   equipped right_hand → equipped left_hand → unit-def's
--   natural_weapon → "unarmed" fallback. Used by the AI's combat
--   effectiveness helper to read the relevant skill stat off the
--   unit's skills map.
unitGetWeaponClassFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetWeaponClassFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mClass ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                im ← readIORef (itemManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst → case firstEquippedWeapon im
                                       (uiEquipment inst) of
                        Just w  → return $ Just (iwWeaponClass w)
                        Nothing →
                            case HM.lookup (uiDefName inst) (umDefs um) of
                                Just d → case udNaturalWeapon d of
                                    Just nw → return $ Just
                                                  (nwWeaponClass nw)
                                    Nothing → return $ Just "unarmed"
                                Nothing → return $ Just "unarmed"
            case mClass of
                Just c  → Lua.pushstring (TE.encodeUtf8 c)
                                >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.getJumpReach(uid) → { dist = maxTiles, height = maxMetres } or nil.
--   The unit's leap envelope: max horizontal distance (tiles) and max
--   vertical strike reach (metres), from jumping skill + agility + strength,
--   penalised by body-fat. The AI reads these to decide whether to lunge and
--   to compute the strike-reach it passes to combat.attack.
unitGetJumpReachFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetJumpReachFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mReach ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst →
                        let jmp = HM.lookupDefault 0.0 "jumping"  (uiSkills inst)
                            agi = HM.lookupDefault 1.0 "agility"   (uiStats inst)
                            str = HM.lookupDefault 1.0 "strength"  (uiStats inst)
                            bm  = HM.lookupDefault 1.0 "body_mass" (uiStats inst)
                            fm  = HM.lookupDefault 0.0 "fat_mass"  (uiStats inst)
                            ff  = if bm > 0 then fm / bm else 0
                        in return (Just ( jumpMaxTiles jmp agi str ff
                                        , maxJumpHeight jmp agi str ff ))
            case mReach of
                Just (d, h) → do
                    Lua.newtable
                    Lua.pushnumber (Lua.Number (realToFrac d))
                    Lua.setfield (-2) "dist"
                    Lua.pushnumber (Lua.Number (realToFrac h))
                    Lua.setfield (-2) "height"
                    return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.lungeImpactSpeed(uid, distTiles) → m/s. How fast the unit's body
--   is moving at the end of a leap of `distTiles` tiles (0 = an in-place
--   vertical pounce). The AI passes this to combat.attack so the lunge's
--   full-body momentum folds into the strike. 0 / nil unit → 0.
unitLungeImpactSpeedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitLungeImpactSpeedFn env = do
    idArg ← Lua.tointeger 1
    dArg  ← Lua.tonumber 2
    case idArg of
        Nothing → Lua.pushnumber (Lua.Number 0) >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                d   = case dArg of
                          Just (Lua.Number v) → realToFrac v
                          _                   → 0.0
            mh ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → return Nothing
                    Just inst →
                        let jmp = HM.lookupDefault 0.0 "jumping"  (uiSkills inst)
                            agi = HM.lookupDefault 1.0 "agility"   (uiStats inst)
                            str = HM.lookupDefault 1.0 "strength"  (uiStats inst)
                            bm  = HM.lookupDefault 1.0 "body_mass" (uiStats inst)
                            fm  = HM.lookupDefault 0.0 "fat_mass"  (uiStats inst)
                            ff  = if bm > 0 then fm / bm else 0
                        in return (Just (maxJumpHeight jmp agi str ff))
            case mh of
                Just h  → Lua.pushnumber (Lua.Number (realToFrac (lungeImpactSpeed d h)))
                                >> return 1
                Nothing → Lua.pushnumber (Lua.Number 0) >> return 1

-- | unit.getActivity(uid) — returns the unit's current sim-thread
--   activity as a string, one of: "idle", "walking", "running",
--   "drinking", "eating", "pickup", "transitioning" (see
--   `Unit.Thread.activityLabel`). nil if the unit doesn't exist. This
--   is the activity, NOT the pose — collapse/death are poses, read via
--   `unit.getPose`, and never appear here. Reads `uiActivity`, which is
--   mirrored from `usState` by Unit.Thread.publishToRender every tick.
unitGetActivityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetActivityFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mAct ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiActivity <$> HM.lookup uid (umInstances um))
            case mAct of
                Just label → do
                    Lua.pushstring (TE.encodeUtf8 label)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | unit.getCurrentAnim(uid) — the unit's resolved animation name
--   (uiCurrentAnim) as a string, "" for a T-pose. nil if the unit
--   doesn't exist. Lets scripts classify exertion that isn't a distinct
--   UnitActivity — mining (pickaxe/shovel work anims) and combat
--   (attack_/combat_ overrides) — for the metabolic burn / hydration drain.
unitGetCurrentAnimFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetCurrentAnimFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mAnim ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiCurrentAnim <$> HM.lookup uid (umInstances um))
            case mAnim of
                Just anim → do
                    Lua.pushstring (TE.encodeUtf8 anim)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | unit.getVisibleTiles(uid) → array of {x, y} tables, or nil if the
--   unit doesn't exist. Includes the unit's own tile.
unitGetVisibleTilesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetVisibleTilesFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            tiles ← Lua.liftIO $ unitVisibleTiles env uid
            -- Return nil specifically when the unit is missing
            -- (distinct from "exists but sees nothing", which is []).
            um ← Lua.liftIO $ readIORef (unitManagerRef env)
            if not (HM.member uid (umInstances um))
                then do
                    Lua.pushnil
                    return 1
                else do
                    Lua.newtable
                    forM_ (zip [1 ..] tiles) $ \(i, (gx, gy)) → do
                        Lua.newtable
                        Lua.pushinteger (fromIntegral gx)
                        Lua.setfield (-2) "x"
                        Lua.pushinteger (fromIntegral gy)
                        Lua.setfield (-2) "y"
                        Lua.rawseti (-2) (fromIntegral (i ∷ Int))
                    return 1
