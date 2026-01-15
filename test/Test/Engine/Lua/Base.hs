{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Lua.Base (spec) where

import UPrelude
import Test.Hspec
import Engine.Lua.Base
import Engine.Lua.Types
import qualified Engine.Core.Queue as Q
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Concurrent.STM (newTVarIO, atomically, readTVar)

spec ∷ Spec
spec = do
  describe "Engine.Lua.Base" $ do
    describe "calculateLCM" $ do
      it "calculates LCM of tick intervals" $ do
        let script1 = LuaScript "test1" "/path/1.lua" 2 0 True False
            script2 = LuaScript "test2" "/path/2.lua" 3 0 True False
            scripts = Map.fromList [("test1", script1), ("test2", script2)]
        calculateLCM scripts `shouldBe` 6

      it "returns 1 for empty scripts" $ do
        let scripts ∷ LuaScripts = Map.empty
        calculateLCM scripts `shouldBe` 1

      it "returns 1 for scripts with only 0 intervals" $ do
        let script = LuaScript "test" "/path.lua" 0 0 True False
            scripts = Map.singleton "test" script
        calculateLCM scripts `shouldBe` 1

    describe "script management" $ do
      it "can add and retrieve a script" $ do
        scriptsVar ← newTVarIO Map.empty
        let script = LuaScript "test" "/path.lua" 1 0 True False
        addScript scriptsVar script
        result ← getScript scriptsVar "test"
        result `shouldBe` Just script

      it "can remove a script" $ do
        let script = LuaScript "test" "/path.lua" 1 0 True False
        scriptsVar ← newTVarIO $ Map.singleton "test" script
        removeScript scriptsVar "test"
        result ← getScript scriptsVar "test"
        result `shouldBe` Nothing

      it "can update a script" $ do
        let script = LuaScript "test" "/path.lua" 1 0 True False
            updatedScript = script { lsTickInterval = 5 }
        scriptsVar ← newTVarIO $ Map.singleton "test" script
        updateScript scriptsVar updatedScript
        result ← getScript scriptsVar "test"
        result `shouldBe` Just updatedScript
        case result of
          Just s → lsTickInterval s `shouldBe` 5
          Nothing → expectationFailure "Script not found"

    describe "logging" $ do
      it "can log errors" $ do
        queue ← Q.newQueue
        let err = LuaLoadError "test error" "/path.lua"
        logLuaError queue err
        result ← Q.tryReadQueue queue
        case result of
          Just msg → T.isInfixOf "Lua Error" msg `shouldBe` True
          Nothing → expectationFailure "No log message found"

      it "can log info" $ do
        queue ← Q.newQueue
        logLuaInfo queue "test info"
        result ← Q.tryReadQueue queue
        case result of
          Just msg → T.isInfixOf "Lua Info" msg `shouldBe` True
          Nothing → expectationFailure "No log message found"
