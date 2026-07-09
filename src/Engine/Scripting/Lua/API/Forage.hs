{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Top-level interactive-flora Lua API module (#94): re-exports every
--   Lua-facing function from the sub-modules so existing call-sites
--   keep working. Forage.Query is the read-only surface (getFloraAt,
--   getFloraGrowthAt, findHarvestableFlora, getFood, getCropPlotAt),
--   Forage.Harvest is the reap verb (harvestFlora — rolls + spawns
--   yields), and Forage.Crop is the groundcover planting verb
--   (plantCropAt). Forage.Lookup holds the flora-instance/growth-clock
--   helpers all three share.
--
--   Harvest STATE lives in the world-level 'wsFloraHarvestsRef' map
--   (tile → regrowth game-seconds), not in the chunk — see
--   World.Flora.Harvest for why. All functions run directly on the Lua
--   thread against the active world's refs, the same pattern as the
--   WorldQuery reads and item.spawnGround.
module Engine.Scripting.Lua.API.Forage
    ( -- * Read-only queries
      module Engine.Scripting.Lua.API.Forage.Query
      -- * Harvest verb
    , module Engine.Scripting.Lua.API.Forage.Harvest
      -- * Crop-plot planting verb
    , module Engine.Scripting.Lua.API.Forage.Crop
    ) where

import Engine.Scripting.Lua.API.Forage.Query
import Engine.Scripting.Lua.API.Forage.Harvest
import Engine.Scripting.Lua.API.Forage.Crop
