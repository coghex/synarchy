{-# LANGUAGE OverloadedStrings #-}
-- | The "autonomous canteen drink credit" gate (#2631):
--   @scripts/unit_ai_needs.lua@'s @drinkExecute@ must credit hydration
--   and queue its drink animation from the delta
--   @unit.modifyItemFillById@ ACTUALLY applied, never from the sip its
--   inventory snapshot asked for.
--
--   The defect this pins: the snapshot is a COPY
--   (@unit.getInventory@ is a plain read) and the drain is a separate
--   transaction, so a canteen that emptied, shrank, or vanished between
--   the two still paid a full sip of hydration and a full drink anim.
--   The player coffee path was fixed for this class in #1744; this one
--   was excluded there and kept crediting the request.
--
--   What this fixture is, and why:
--
--   * The module under test is the REAL @scripts.unit_ai_needs@, driven
--     with the SHIPPED @scripts.unit_ai_tunables@ acolyte block — so the
--     0.5 L sip and the 11 hydration-per-litre the arithmetic below is
--     written against are production's numbers, not the fixture's.
--
--   * Hydration is REAL storage: @withHeadlessEngine@'s own
--     'EngineEnv', a real 'UnitInstance', and the registered
--     @unit.getStat@ / @unit.setStat@ verbs. A stubbed stat table would
--     prove only that an expression was evaluated; what is under test is
--     which number gets committed.
--
--   * Exactly three verbs are intercepted, and only because the engine
--     cannot be made to answer them adversarially from real state: the
--     inventory READ (the snapshot), the drain's RETURN VALUE (the case
--     parameter), and the drink ANIMATION request (counted, so "no
--     animation queued" is asserted against a number). The stub is held
--     to 'Engine.Scripting.Lua.API.Units.Equipment''s own contract — a
--     SIGNED applied delta, negative for a real drain, @nil@ when the
--     unit or the instance is gone — and it records the @(uid,
--     instanceId, delta)@ it was asked for, so requirement 3's
--     exact-instance targeting and both request limits are asserted on
--     the call rather than inferred from the result.
--
--   * The snapshot always holds an EMPTY canteen AHEAD of the live one,
--     the ordering #1220 exists for. A first-match drain would take the
--     empty peer's id, which the recorded arguments would catch.
--
--   * The POSITIVE drain result is its own case (the player-path
--     equivalent is 'Test.Headless.UI.ConsumableGesture'): an
--     @abs()@-based implementation passes every negative case above and
--     fails only here.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "autonomous canteen drink credit"'@.
module Test.Headless.Lua.UnitAiCanteenDrain (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (newIORef, writeIORef)

import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Unit.TransferApi (minimalDef, mkUnit)
import Unit.Faction (Faction(..))
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldState, emptyWorldManager)

-- * Fixture

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "canteen_drain_page"

drinkerUid ∷ UnitId
drinkerUid = UnitId 1

-- | The shipped acolyte canteen def, so @findCanteenWithWater@ matches
--   on the same string production does.
canteenDef ∷ Text
canteenDef = "canteen_steel_2l"

-- | Hydration 10 of 40: a 30-point deficit, which at 11 per litre wants
--   2.727 L — comfortably more than the 0.5 L sip, so the DEFAULT cases
--   are sip-limited and neither limit can mask the other.
resetUnit ∷ EngineEnv → Float → IO ()
resetUnit env hyd = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" (minimalDef "acolyte" "Acolyte")
        , umInstances = HM.singleton drinkerUid
            (mkUnit "acolyte" FactionPlayer (10, 10) 100 [] [])
                { uiPage  = fixturePage
                , uiStats = HM.fromList
                    [ ("carrying_capacity", 100)
                    , ("hydration", hyd), ("max_hydration", 40) ]
                }
        }

-- * Lua plumbing

withSharedFixture ∷ ((EngineEnv, LuaBackendState) → IO ()) → IO ()
withSharedFixture action = withHeadlessEngine $ \env → do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    action (env, ls)

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls src = do
    r ← executeDebugLua (lbsLuaState ls) src
    r `shouldNotSatisfy` isLuaError
    pure r

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

-- | Restore the engine's own verbs, drop every loaded module so the real
--   ones reload against the fresh state, and install the three
--   interceptions. @fill@ is the live canteen's snapshot fill;
--   @result@ is what the drain answers.
sceneLua ∷ Text
sceneLua = luaLines
    [ "if _G.__origDrain then"
    , "  unit.modifyItemFillById = _G.__origDrain; _G.__origDrain = nil end;"
    , "if _G.__origInv then"
    , "  unit.getInventory = _G.__origInv; _G.__origInv = nil end;"
    , "if _G.__origDrink then"
    , "  unit.drink = _G.__origDrink; _G.__origDrink = nil end;"
    , "for k, _ in pairs(package.loaded) do package.loaded[k] = nil end;"
    -- The snapshot: an EMPTY canteen of the same def FIRST (the #1220
    -- ordering), then the live one the selection must pick.
    , "_G.__stage = function(fill, result)"
    , "  _G.__origInv = _G.__origInv or unit.getInventory;"
    , "  _G.__origDrain = _G.__origDrain or unit.modifyItemFillById;"
    , "  _G.__origDrink = _G.__origDrink or unit.drink;"
    , "  _G.__drainArgs = 'none'; _G.__drains = 0; _G.__drinks = 0;"
    , "  unit.getInventory = function()"
    , "    return { { defName = '" <> canteenDef <> "', instanceId = 701,"
    , "               currentFill = 0.0, capacity = 2.0 },"
    , "             { defName = '" <> canteenDef <> "', instanceId = 702,"
    , "               currentFill = fill, capacity = 2.0 } } end;"
    , "  unit.modifyItemFillById = function(uid, iid, delta)"
    , "    _G.__drains = _G.__drains + 1;"
    , "    _G.__drainArgs = string.format('%d|%d|%.4f', uid, iid, delta);"
    , "    return result end;"
    , "  unit.drink = function() _G.__drinks = _G.__drinks + 1 end end;"
    -- Run the real action with the shipped acolyte tunables, then report
    -- committed hydration, the drink count, and the drain's arguments.
    , "_G.__drink = function()"
    , "  require('scripts.unit_ai_needs').drinkExecute(1, {},"
    , "    require('scripts.unit_ai_tunables').acolyte);"
    , "  return string.format('%.4f|%d|%d|%s',"
    , "    unit.getStat(1, 'hydration') or -1,"
    , "    _G.__drinks, _G.__drains, _G.__drainArgs) end;"
    , "return true"
    ]

-- | Stage one case and run it: snapshot fill, drain answer.
runCase ∷ EngineEnv → LuaBackendState → Float → Text → Text → IO Text
runCase env ls hyd fill result = do
    resetUnit env hyd
    _ ← evalOk ls sceneLua
    _ ← evalOk ls ("_G.__stage(" <> fill <> ", " <> result
                     <> "); return true")
    evalOk ls "return _G.__drink()"

-- * Spec

spec ∷ Spec
spec = aroundAll withSharedFixture $
  describe "autonomous canteen drink credit (#2631)" $ do

    describe "the credit follows the drain the engine applied" $ do

        it "a FULL drain credits the whole sip and queues one drink, \
           \draining the selected instance by the requested delta \
           \(requirement 3)" $ \(env, ls) → do
            r ← runCase env ls 10 "2.0" "-0.5"
            -- 10 + 0.5*11 = 15.5; the drain was asked of 702 — the live
            -- canteen — for the full -0.5 sip, not of the empty 701.
            r `shouldBe` "\"15.5000|1|1|1|702|-0.5000\""

        it "a SHORT drain credits the magnitude the engine returned, \
           \never the sip the snapshot requested (requirement 1)" $
           \(env, ls) → do
            r ← runCase env ls 10 "2.0" "-0.1"
            -- 10 + 0.1*11 = 11.1. The request was still -0.5, so the
            -- credit is derived from the answer and not from the ask.
            r `shouldBe` "\"11.1000|1|1|1|702|-0.5000\""

        it "a nil drain result — the unit or that instance is gone — \
           \credits nothing and queues no animation (requirement 2)" $
           \(env, ls) → do
            r ← runCase env ls 10 "2.0" "nil"
            r `shouldBe` "\"10.0000|0|1|1|702|-0.5000\""

        it "a ZERO drain result — nothing left to remove — credits \
           \nothing and queues no animation (requirement 2)" $
           \(env, ls) → do
            r ← runCase env ls 10 "2.0" "0"
            r `shouldBe` "\"10.0000|0|1|1|702|-0.5000\""

        it "a POSITIVE applied delta is a fill INCREASE, not \
           \consumption: it refuses rather than crediting its \
           \magnitude (requirement 2)" $ \(env, ls) → do
            r ← runCase env ls 10 "2.0" "0.1"
            -- An abs()-based credit would read 11.1000|1 here.
            r `shouldBe` "\"10.0000|0|1|1|702|-0.5000\""

    describe "the request limits are unchanged (requirement 3)" $ do

        it "a snapshot fill below the per-action sip caps the REQUEST \
           \at the fill, and the credit follows that drain" $
           \(env, ls) → do
            r ← runCase env ls 10 "0.2" "-0.2"
            -- min(0.5 sip, 0.2 fill, 30/11 deficit) = 0.2 L requested;
            -- 10 + 0.2*11 = 12.2 credited.
            r `shouldBe` "\"12.2000|1|1|1|702|-0.2000\""

        it "a shallow deficit caps the REQUEST at deficit/k, so a full \
           \drain lands exactly on max_hydration" $ \(env, ls) → do
            r ← runCase env ls 38 "2.0" "-0.181818"
            -- min(0.5, 2.0, 2/11 = 0.1818) = 0.1818 L requested;
            -- 38 + 0.181818*11 = 40.0, the maximum, not past it.
            r `shouldBe` "\"40.0000|1|1|1|702|-0.1818\""
