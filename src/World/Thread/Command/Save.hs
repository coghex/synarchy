{-# LANGUAGE UnicodeSyntax #-}

-- | Save/load command handlers, split (issue #561) into focused
--   submodules under "World.Thread.Command.Save.*":
--
--     * "World.Thread.Command.Save.RestoreIds" — collision-free
--       restore-id assignment for saved pages.
--     * "World.Thread.Command.Save.WriteWorld" — the save path
--       (snapshot every live page, write to disk).
--     * "World.Thread.Command.Save.LoadPage" — per-page load
--       restoration (zoom cache, center chunk, chunk queue, arena
--       special-case), called once per saved page.
--     * "World.Thread.Command.Save.LoadWorld" — the load path
--       orchestrator (manager merge, stale-page teardown, visibility
--       restore, Lua signal).
--
--   This module re-exports the public API unchanged.
module World.Thread.Command.Save
    ( handleWorldSaveCommand
    , handleWorldLoadSaveCommand
    ) where

import World.Thread.Command.Save.WriteWorld (handleWorldSaveCommand)
import World.Thread.Command.Save.LoadWorld (handleWorldLoadSaveCommand)
