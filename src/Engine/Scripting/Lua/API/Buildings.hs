{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Top-level building API module. Re-exports every Lua-facing
--   function from the sub-modules so existing call-sites keep
--   working: Buildings.Yaml loads building YAML defs
--   (engine.loadBuildingYaml), Buildings.Spawn covers spawn/destroy/
--   placement/ghost preview, Buildings.Progress covers the spawn-
--   roster countdown and construction progress/activity, Buildings.
--   Materials covers material need/delivery and storage, Buildings.
--   Query covers info/operations/station lookup/listing, and
--   Buildings.Selection covers hit-testing and selection.
module Engine.Scripting.Lua.API.Buildings
    ( -- * YAML loading
      module Engine.Scripting.Lua.API.Buildings.Yaml
      -- * Spawn / destroy / placement / ghost
    , module Engine.Scripting.Lua.API.Buildings.Spawn
      -- * Spawn-roster + construction progress
    , module Engine.Scripting.Lua.API.Buildings.Progress
      -- * Materials + storage
    , module Engine.Scripting.Lua.API.Buildings.Materials
      -- * Info / operations / station / listing queries
    , module Engine.Scripting.Lua.API.Buildings.Query
      -- * Hit-test + selection
    , module Engine.Scripting.Lua.API.Buildings.Selection
    ) where

import Engine.Scripting.Lua.API.Buildings.Yaml
import Engine.Scripting.Lua.API.Buildings.Spawn
import Engine.Scripting.Lua.API.Buildings.Progress
import Engine.Scripting.Lua.API.Buildings.Materials
import Engine.Scripting.Lua.API.Buildings.Query
import Engine.Scripting.Lua.API.Buildings.Selection
