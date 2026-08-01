{-# LANGUAGE Strict #-}
-- | Visible-page resolution for the save-request path. Split out of
--   "Engine.Scripting.Lua.API.Save" by issue #985: nothing here takes an
--   @EngineEnv@, so this module stays outside the save\/load path's
--   permanent full-access exception
--   (@docs\/engineenv_capability_inventory.md@ §6.1).
module Engine.Scripting.Lua.API.Save.Page
    ( visiblePageState
    ) where

import UPrelude
import World.Types
    ( WorldManager(wmWorlds, wmVisible), WorldState )

-- | The page whose clock a save request pauses and captures: the head of
--   @wmVisible@ that is still a live page — the SAME resolution
--   "World.Thread.Command.Save.WriteWorld" performs, so the scale
--   captured at acceptance and the one the world thread zeroes are
--   always the same page's. 'Nothing' when there is no visible page at
--   all, which is unreachable through the autosave scheduler (it only
--   fires in a gameplay view) and defensive only.
visiblePageState ∷ WorldManager → Maybe WorldState
visiblePageState mgr = case wmVisible mgr of
    (vid:_) → lookup vid (wmWorlds mgr)
    _       → Nothing
