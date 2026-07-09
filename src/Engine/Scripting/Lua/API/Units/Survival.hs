{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units.Survival
  ( unitDrinkFn
  , unitEatFn
  , unitFeedFn
  , unitGetCaloriesFn
  , unitPickupFn
  )
    where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Command.Types (UnitCommand(..))
import Item.Types (ItemInstance(..), ItemDef(..), ItemFood(..), lookupItemDef)
import Engine.Scripting.Lua.API.Units.Inventory (removeFirstByName)
import Engine.Scripting.Lua.API.Units.Stats (getEffectiveStat)


-- | unit.drink(uid) — start the drinking animation on an Idle unit.
--   Engine handles the auto-revert to Idle when the anim finishes.
--   Stat / canteen effects are applied Lua-side BEFORE calling this
--   (see scripts/unit_ai drink action); the engine doesn't know about
--   sip amounts.
unitDrinkFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDrinkFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitDrink uid
            Lua.pushboolean True
            return 1

-- | unit.eat(uid) — start the eating animation on an Idle unit.
--   Mirror of unit.drink. Nutrition + inventory mutation happen
--   Lua-side BEFORE this call (see scripts/unit_ai's eat_from_inventory
--   action) — the engine only owns the state/anim/timer.
unitEatFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitEatFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitEat uid
            Lua.pushboolean True
            return 1

-- | unit.feed(uid, itemId) — eat from the unit's inventory and credit
--   the kcal to the hunger meter (STOMACH fullness, clamped to
--   max_hunger; digestion later moves it into the "calories" energy
--   store). Two shapes, keyed off the item def's nutrition:
--
--   * DISCRETE food (ifCalories): consume one whole item, credit its
--     kcal (overflow past a full stomach is wasted, the item is still
--     consumed). Plays the eat animation.
--   * BULK food (ifCaloriesPerKg): draw just enough fill (kg) from the
--     first non-empty instance to top the stomach up; the item persists
--     with reduced fill, and is removed once eaten dry. A full stomach
--     draws nothing and the call fails (nil) without consuming.
--
--   Returns the kcal actually credited on success, or nil if the unit
--   doesn't carry the item / the item has no food data (the codebase
--   signals failure with nil rather than raising). The authoritative
--   consume-and-credit primitive; the auto-eat AI routes through it.
unitFeedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitFeedFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid     = UnitId (fromIntegral n)
                defName = TE.decodeUtf8Lenient nameBS
            mCredited ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                case lookupItemDef defName itemMgr >>= idFood of
                    Nothing   → pure Nothing   -- no food data → can't feed
                    Just food → atomicModifyIORef' (unitManagerRef env) $ \um →
                        case HM.lookup uid (umInstances um) of
                            Nothing → (um, Nothing)
                            -- Require a LIVE hunger pool: max_hunger is seeded
                            -- for any body unit (incl. wildlife), but only a
                            -- present "hunger" stat means the unit actually
                            -- runs the calorie system. Feeding one that
                            -- doesn't would conjure a permanent, never-
                            -- draining pool. Reject without consuming the item.
                            Just u  → case HM.lookup "hunger" (uiStats u) of
                                Nothing  → (um, Nothing)  -- no hunger system
                                Just cur
                                  | ifCaloriesPerKg food > 0 →
                                    -- BULK: eat kg of fill, stomach-deficit
                                    -- driven. maxH falls back to cur (deficit
                                    -- 0 → reject) if max_hunger is missing.
                                    let stats0  = uiStats u
                                        maxH    = HM.lookupDefault cur
                                                      "max_hunger" stats0
                                        kgWant  = max 0 (maxH - cur)
                                                    / ifCaloriesPerKg food
                                        drawFrom [] = Nothing
                                        drawFrom (it:rest)
                                          | iiDefName it ≡ defName
                                            ∧ iiCurrentFill it > 0 =
                                              let kgEat   = min kgWant
                                                                (iiCurrentFill it)
                                                  newFill = iiCurrentFill it - kgEat
                                                  -- An eaten-dry sack is spent
                                                  -- packaging — drop it.
                                                  rest'   = if newFill ≤ 1e-6
                                                            then rest
                                                            else it { iiCurrentFill
                                                                        = newFill }
                                                                 : rest
                                              in if kgEat ≤ 0
                                                 then Nothing
                                                 else Just (kgEat, rest')
                                          | otherwise =
                                              (\(k, xs) → (k, it:xs))
                                                  ⊚ drawFrom rest
                                    in case drawFrom (uiInventory u) of
                                        Nothing → (um, Nothing)
                                        Just (kgEat, newInv) →
                                            let newH = min maxH
                                                         (cur + kgEat
                                                            * ifCaloriesPerKg food)
                                                u'   = u { uiInventory = newInv
                                                         , uiStats = HM.insert
                                                             "hunger" newH stats0 }
                                            in ( um { umInstances = HM.insert uid u'
                                                        (umInstances um) }
                                               , Just (newH - cur) )
                                  | otherwise →
                                    case removeFirstByName defName (uiInventory u) of
                                        Nothing     → (um, Nothing)  -- not carried
                                        Just newInv →
                                            let stats0 = uiStats u
                                                maxH   = HM.lookupDefault (cur + ifCalories food)
                                                             "max_hunger" stats0
                                                newH   = min maxH (cur + ifCalories food)
                                                u'     = u { uiInventory = newInv
                                                           , uiStats = HM.insert "hunger" newH stats0 }
                                            in ( um { umInstances = HM.insert uid u' (umInstances um) }
                                               , Just (newH - cur) )
            case mCredited of
                Just credited → do
                    Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitEat uid
                    Lua.pushnumber (Lua.Number (realToFrac credited))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.getCalories(uid) — the unit's current energy store (the
--   "calories" stat: kcal available to spend, fed by digesting the
--   stomach's "hunger" meter). nil if the unit / stat is absent.
--   Pairs with max_calories for the fraction.
unitGetCaloriesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetCaloriesFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Just n → do
            let uid = UnitId (fromIntegral n)
            mVal ← Lua.liftIO $ getEffectiveStat env uid "calories"
            case mVal of
                Just v  → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        Nothing → do
            Lua.pushnil
            return 1

-- | unit.pickup(uid) — start the "picking up" animation on an Idle
--   unit. Same shape as drink; used for canteen refilling.
unitPickupFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitPickupFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitPickup uid
            Lua.pushboolean True
            return 1
