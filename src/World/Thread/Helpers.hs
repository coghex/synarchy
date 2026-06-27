{-# LANGUAGE Strict, UnicodeSyntax #-}
module World.Thread.Helpers
    ( sendGenLog
    , sendHudInfo
    , sendHudChunkInfo
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

-- | Info message to lua's HUD, tagged with its SOURCE kind so the
--   entity-info watchers can tell a zoomed-in tile selection ("tile")
--   apart from a zoom-map chunk selection ("chunk") — both ride this
--   one broadcast (issue #133).
sendHudInfoKind ∷ EngineEnv → Text → Text → Text → IO ()
sendHudInfoKind env kind msgbas msgadv = Q.writeQueue (luaQueue env)
                                  (LuaHudLogInfo msgbas msgadv kind)

-- | Tile (zoomed-in) info push. Also used for the blank-payload panel
--   clear; a blank carries no selection so its kind is immaterial.
sendHudInfo ∷ EngineEnv → Text → Text → IO ()
sendHudInfo env = sendHudInfoKind env "tile"

-- | Chunk (zoom-map) info push. Tagged "chunk" so unit/building/item
--   watchers don't mistake it for a tile click and drop their
--   zoomed-in selection (issue #133).
sendHudChunkInfo ∷ EngineEnv → Text → Text → IO ()
sendHudChunkInfo env = sendHudInfoKind env "chunk"

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
