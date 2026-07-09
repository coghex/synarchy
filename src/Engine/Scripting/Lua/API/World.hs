-- | Top-level world API module. Re-exports every Lua-facing function
--   from the sub-modules so existing call-sites keep working:
--   World.GenConfig is the world-gen config get/set pair,
--   World.Lifecycle covers init/arena/show/hide/destroy plus init-
--   progress polling, World.Clock covers texture/camera/sun/time/
--   date/timescale/map-mode, World.Cursor covers zoom- and world-
--   cursor hover/select state and the direct tile-select primitive,
--   World.Designation is the mine-designation tool (anchor/commit/
--   query), World.Query covers read-only debug/validation lookups
--   (dig info, gem info, spoil info, material list, tile-quad
--   introspection), World.Tools is the tool-mode get/set pair, and
--   World.Edit covers tile/cell/slope/vegetation/fluid edits plus
--   the one-time location-content/stamp flags.
module Engine.Scripting.Lua.API.World
    ( -- * World-gen config
      module Engine.Scripting.Lua.API.World.GenConfig
      -- * Lifecycle (init/arena/show/hide/destroy/progress)
    , module Engine.Scripting.Lua.API.World.Lifecycle
      -- * Texture/camera/sun/time/date/timescale/map-mode
    , module Engine.Scripting.Lua.API.World.Clock
      -- * Zoom/world cursor hover/select + tile select
    , module Engine.Scripting.Lua.API.World.Cursor
      -- * Mine designation tool
    , module Engine.Scripting.Lua.API.World.Designation
      -- * Read-only debug/validation queries
    , module Engine.Scripting.Lua.API.World.Query
      -- * Tool mode get/set
    , module Engine.Scripting.Lua.API.World.Tools
      -- * Tile/cell/slope/vegetation/fluid edits + location flags
    , module Engine.Scripting.Lua.API.World.Edit
    ) where

import Engine.Scripting.Lua.API.World.GenConfig
import Engine.Scripting.Lua.API.World.Lifecycle
import Engine.Scripting.Lua.API.World.Clock
import Engine.Scripting.Lua.API.World.Cursor
import Engine.Scripting.Lua.API.World.Designation
import Engine.Scripting.Lua.API.World.Query
import Engine.Scripting.Lua.API.World.Tools
import Engine.Scripting.Lua.API.World.Edit
