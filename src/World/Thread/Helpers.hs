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
--   reconcile their global singleton state (#195). Carries, for the
--   loaded page, the unit + building ids that SURVIVED (successfully
--   restored) and the ORPHANS (claimed by the snapshot but dropped). The
--   Lua side keeps survivors, force-prunes orphans (collision-safe), and
--   keeps any other id only if it's a still-live off-page entity — so
--   other live pages' state is preserved. Emit only after units +
--   buildings have been written back.
sendSaveLoaded ∷ EngineEnv → [Int] → [Int] → [Int] → [Int] → IO ()
sendSaveLoaded env survivingUnitIds survivingBuildingIds
                   orphanUnitIds orphanBuildingIds =
    Q.writeQueue (luaQueue env)
        (LuaSaveLoaded survivingUnitIds survivingBuildingIds
                       orphanUnitIds orphanBuildingIds)

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
