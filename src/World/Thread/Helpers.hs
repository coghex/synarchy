{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Helpers
    ( sendGenLog
    , sendHudInfo
    , sendHudWeatherInfo
    , unWorldPageId
    ) where

import UPrelude
import Engine.Core.State (EngineEnv(..))
import qualified Engine.Core.Queue as Q
import Engine.Scripting.Lua.Types (LuaMsg(..))
import World.Types (WorldPageId(..))

-- | Send a progress message to Lua
sendGenLog ∷ EngineEnv → Text → IO ()
sendGenLog env msg = Q.writeQueue (luaQueue env) (LuaWorldGenLog msg)

-- | Info message to lua's HUD
sendHudInfo ∷ EngineEnv → Text → Text → IO ()
sendHudInfo env msgbas msgadv = Q.writeQueue (luaQueue env)
                                  (LuaHudLogInfo msgbas msgadv)

-- | Send weather info to lua's HUD
sendHudWeatherInfo ∷ EngineEnv → Text → IO ()
sendHudWeatherInfo env weatherText = Q.writeQueue (luaQueue env)
                                            (LuaHudLogWeatherInfo weatherText)

unWorldPageId ∷ WorldPageId → Text
unWorldPageId (WorldPageId t) = t
