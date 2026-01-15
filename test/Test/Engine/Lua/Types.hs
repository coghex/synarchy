{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Engine.Lua.Types (spec) where

import UPrelude
import Test.Hspec
import Engine.Lua.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

spec ∷ Spec
spec = do
  describe "Engine.Lua.Types" $ do
    describe "LuaScript" $ do
      it "can create a LuaScript" $ do
        let script = LuaScript
              { lsName = "test"
              , lsFilePath = "/path/to/script.lua"
              , lsTickInterval = 1
              , lsNextTick = 0
              , lsEnabled = True
              , lsInitialized = False
              }
        lsName script `shouldBe` "test"
        lsTickInterval script `shouldBe` 1
        lsEnabled script `shouldBe` True

      it "can create a LuaScripts map" $ do
        let script = LuaScript
              { lsName = "test"
              , lsFilePath = "/path/to/script.lua"
              , lsTickInterval = 1
              , lsNextTick = 0
              , lsEnabled = True
              , lsInitialized = False
              }
        let scripts ∷ LuaScripts = Map.singleton "test" script
        Map.size scripts `shouldBe` 1
        Map.lookup "test" scripts `shouldBe` Just script
