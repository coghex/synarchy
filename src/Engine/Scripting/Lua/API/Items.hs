{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Top-level item API module (#577). Re-exports every Lua-facing
--   function from the sub-modules so existing call-sites keep working:
--   Items.Defs is the item def catalogue (item.loadYaml, item.listDefs),
--   Items.Ground is ground-item CRUD (spawn/list/remove/count/temp/
--   pickup), and Items.Render is the render-facing queries (hit test,
--   world-view selection, debugQuads). The export list is explicit (not
--   a blanket @module@ re-export) so sub-module-internal helpers (e.g.
--   Items.Ground's resolveItemPage) don't leak onto this top-level
--   module's public API.
module Engine.Scripting.Lua.API.Items
    ( loadItemYamlFn
    , itemListDefsFn
    , itemSpawnGroundFn
    , itemListGroundFn
    , itemRemoveGroundFn
    , itemGroundCountFn
    , itemGetGroundTempFn
    , itemSetGroundTempFn
    , itemHitTestAtFn
    , itemSelectFn
    , itemDeselectFn
    , itemGetSelectedFn
    , itemPickupGroundFn
    , itemDebugQuadsFn
    ) where

import Engine.Scripting.Lua.API.Items.Defs
import Engine.Scripting.Lua.API.Items.Ground
import Engine.Scripting.Lua.API.Items.Render
