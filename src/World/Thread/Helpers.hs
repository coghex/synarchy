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
--   reconcile their global singleton state (#195). Carries the loaded
--   page's surviving unit + building ids. The Lua side rebuilds each
--   table as "survivors restored from the save blob + every other
--   still-live (off-page) entity's pre-load state", so a load replaces
--   only loaded-page state and other live pages are untouched. Emit only
--   after units + buildings have been written back.
sendSaveLoaded ∷ EngineEnv → [Int] → [Int] → IO ()
sendSaveLoaded env survivingUnitIds survivingBuildingIds =
    Q.writeQueue (luaQueue env)
        (LuaSaveLoaded survivingUnitIds survivingBuildingIds)

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
