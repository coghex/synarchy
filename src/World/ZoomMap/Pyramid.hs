-- | The world map's spatial page pyramid (issue #2298, WML-5).
--
--   A pure, deterministic map representation generated from
--   'World.Generate.Types.WorldGenParams' alone:
--
--     * "World.ZoomMap.Pyramid.Address" — the canonical
--       parity-compressed cylindrical level/page addressing.
--     * "World.ZoomMap.Pyramid.Reduce" — D-16's premultiplied 2x2 box
--       reduction and the rasters it operates on.
--     * "World.ZoomMap.Pyramid.Inventory" — the streamable level
--       inventory, priced through "World.Map.ImagePlan".
--     * "World.ZoomMap.Pyramid.Page" — page generation, gutters
--       included, over an injected cell source.
--     * "World.ZoomMap.Pyramid.Cells" — the world-generation cell
--       source, which reuses the shipping builder's own two passes.
--
--   Nothing here is activated. Fresh world creation, save loading,
--   upload and rendering keep their current behaviour and their current
--   bytes; this pyramid has no production caller in this slice.
module World.ZoomMap.Pyramid
    ( module World.ZoomMap.Pyramid.Address
    , module World.ZoomMap.Pyramid.Reduce
    , module World.ZoomMap.Pyramid.Inventory
    , module World.ZoomMap.Pyramid.Page
    , module World.ZoomMap.Pyramid.Cells
    ) where

import World.ZoomMap.Pyramid.Address
import World.ZoomMap.Pyramid.Reduce
import World.ZoomMap.Pyramid.Inventory
import World.ZoomMap.Pyramid.Page
import World.ZoomMap.Pyramid.Cells
