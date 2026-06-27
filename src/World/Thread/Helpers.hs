{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Helpers
    ( sendGenLog
    , sendSaveLoaded
    , sendHudInfo
    , sendHudWeatherInfo
    , sendHudResourcesInfo
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

-- | Signal Lua that a save finished loading, so per-id modules can
--   reconcile their state against the entities that survived the load
--   (#195). Emit only after units + buildings have been written back.
sendSaveLoaded ∷ EngineEnv → IO ()
sendSaveLoaded env = Q.writeQueue (luaQueue env) LuaSaveLoaded

-- | Info message to lua's HUD
sendHudInfo ∷ EngineEnv → Text → Text → IO ()
sendHudInfo env msgbas msgadv = Q.writeQueue (luaQueue env)
                                  (LuaHudLogInfo msgbas msgadv)

-- | Send weather info to lua's HUD
sendHudWeatherInfo ∷ EngineEnv → Text → IO ()
sendHudWeatherInfo env weatherText = Q.writeQueue (luaQueue env)
                                            (LuaHudLogWeatherInfo weatherText)

-- | Send resources info (zoom-chunk ore survey) to lua's HUD.
--   Empty text removes the Resources tab.
sendHudResourcesInfo ∷ EngineEnv → Text → IO ()
sendHudResourcesInfo env resText = Q.writeQueue (luaQueue env)
                                            (LuaHudLogResourcesInfo resText)

unWorldPageId ∷ WorldPageId → Text
unWorldPageId (WorldPageId t) = t
