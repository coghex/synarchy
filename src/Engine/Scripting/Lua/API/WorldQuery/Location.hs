{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Placed-location queries: world.listPlacedLocations,
--   world.getLocationInstance, world.hasSpawnedLocationContents,
--   world.hasStampedLocation.
--
--   This module was narrowed to the @content-registries@ capability by
--   #890 (epic #537) so the location-def registry was reached only
--   through 'ContentRegistriesCapability'. #911 removed even that: every
--   value these queries report — definition id, anchor, bounds, margin,
--   display name, lifecycle, content-spawn flag — is stored on the
--   placed-location INSTANCE, so nothing here consults the registry at
--   all any more. The remaining 'EngineEnv' parameter is purely the
--   opaque token the not-yet-narrowed @world-sim-render-handoff@
--   page-lookup services ('activeWorldState', 'worldStateByPage')
--   demand — this module dereferences no 'EngineEnv' field itself, and
--   that parameter goes away when @world-sim-render-handoff@ migrates
--   (SS7.4).
module Engine.Scripting.Lua.API.WorldQuery.Location
    ( worldListPlacedLocationsFn
    , worldGetLocationInstanceFn
    , worldHasSpawnedLocationContentsFn
    , worldHasStampedLocationFn
    ) where

import UPrelude
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified HsLua as Lua
import Data.ByteString (ByteString)
import qualified Data.HashSet as HS
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv, activeWorldStateFrom)
import World.Types
import Location.Instance
    ( LocationInstance(..), LocationInstanceId(..), LocationInstances
    , instancesToList, instancesInChunk, lookupLocationInstance
    , isDiscoveredLifecycle, lifecycleName, emptyLocationInstances )
import Location.Bounds (AbsBounds(..))
import World.Generate.Coordinates (globalToChunk)
import Engine.Scripting.Lua.API.WorldQuery.Lookup (worldStateByPage)

-- | The page's live gen params: the named page when a page-id string is
--   given (the location stamper needs a specific world's placements even
--   before it becomes the active page), the active world otherwise.
--   'Nothing' when no such world exists or its params aren't live yet.
paramsForPage
    ∷ EngineEnv → Maybe ByteString → IO (Maybe WorldGenParams)
paramsForPage env mPage = do
    mWs ← case mPage of
        Just pidBS → worldStateByPage (toWorldSimCapability env)
                        (TE.decodeUtf8Lenient pidBS)
        Nothing    → activeWorldStateFrom
                        (wsWorldManagerRef (toWorldSimCapability env))
    case mWs of
        Just ws → readIORef (wsGenParamsRef ws)
        Nothing → pure Nothing

-- | The page's placed-location instance table, empty when the page or
--   its params aren't live.
instancesForPage ∷ EngineEnv → Maybe ByteString → IO LocationInstances
instancesForPage env mPage =
    maybe emptyLocationInstances wgpLocationInstances <$> paramsForPage env mPage

-- | world.listPlacedLocations([pageId]) → array of placed-location
--   tables, each:
--     { cx, cy,    -- chunk coordinate hosting the location
--       gx, gy,    -- the location's anchor tile
--       id,        -- the LocationDef id (#88) placed there
--       bounds,    -- { min_x, min_y, max_x, max_y } — absolute,
--                  --   inclusive tile bounds (#777)
--       discovery_margin,  -- the discovery margin (#777)
--       discovered,        -- has a player-faction unit entered the
--                          --   discovery-margin halo yet (#780)?
--       instance_id,       -- the stable per-page instance id (#911)
--       lifecycle,         -- "unknown" | "hinted" | "discovered"
--                          --   | "active" | "cleared" | "depleted"
--       name,              -- display name (placeholder from the def's
--                          --   label; #708 wiring is future work)
--       contents_spawned } -- one-time content-spawn flag (#90)
--
--   #911 EXTENDED this table; it did not repurpose anything. `id` still
--   means the DEFINITION id (that is what @scripts/locations.lua@ joins
--   against @locations.getDef@) — the instance identity is the separate
--   `instance_id`. `discovered` is now derived as "lifecycle at or
--   beyond discovered", which is exactly what the flag it replaced meant.
--   `bounds` / `discovery_margin` come from the INSTANCE, which stored
--   them when it was placed, so they no longer depend on the def still
--   being registered this session (and an unregistered def can no longer
--   silently omit them).
--
--   With a page-id string argument the named page is read (the location
--   stamper needs a specific world's placements even before it becomes
--   the active page); with no argument the active world is used.
--   Ordered by instance id. Returns an empty table when no such world
--   exists or none were placed.
worldListPlacedLocationsFn ∷ EngineEnv
                           → Lua.LuaE Lua.Exception Lua.NumResults
worldListPlacedLocationsFn env = do
    mPage ← Lua.tostring 1
    instances ← Lua.liftIO (instancesForPage env mPage)
    Lua.newtable
    forM_ (zip [1 ..] (instancesToList instances)) $ \(i, inst) → do
        pushInstanceTable inst
        Lua.rawseti (-2) i
    return 1

-- | world.getLocationInstance(instanceId [, pageId]) → the same table
--   'worldListPlacedLocationsFn' pushes, or nil when that page carries
--   no instance under that id (#911 — the page-scoped by-id lookup).
worldGetLocationInstanceFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetLocationInstanceFn env = do
    idArg ← Lua.tointeger 1
    mPage ← Lua.tostring 2
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just rawId → do
            instances ← Lua.liftIO (instancesForPage env mPage)
            case lookupLocationInstance
                    (LocationInstanceId (fromIntegral rawId)) instances of
                Nothing   → Lua.pushnil >> return 1
                Just inst → pushInstanceTable inst >> return 1

-- | One instance as a Lua table (the shape both queries above return).
pushInstanceTable ∷ LocationInstance → Lua.LuaE Lua.Exception ()
pushInstanceTable inst = do
    let ChunkCoord cx cy = liChunk inst
        (gx, gy)         = liAnchor inst
        ab               = liBounds inst
    Lua.newtable
    pushIntField "cx" cx
    pushIntField "cy" cy
    pushIntField "gx" gx
    pushIntField "gy" gy
    Lua.pushstring (TE.encodeUtf8 (liDefId inst))
    Lua.setfield (-2) "id"
    pushIntField "instance_id" (unLocationInstanceId (liId inst))
    Lua.pushstring (TE.encodeUtf8 (lifecycleName (liLifecycle inst)))
    Lua.setfield (-2) "lifecycle"
    Lua.pushstring (TE.encodeUtf8 (liDisplayName inst))
    Lua.setfield (-2) "name"
    Lua.pushboolean (isDiscoveredLifecycle (liLifecycle inst))
    Lua.setfield (-2) "discovered"
    Lua.pushboolean (liContentsSpawned inst)
    Lua.setfield (-2) "contents_spawned"
    Lua.newtable
    pushIntField "min_x" (abMinX ab)
    pushIntField "min_y" (abMinY ab)
    pushIntField "max_x" (abMaxX ab)
    pushIntField "max_y" (abMaxY ab)
    Lua.setfield (-2) "bounds"
    pushIntField "discovery_margin" (liDiscoveryMargin inst)
  where
    pushIntField name v = do
        Lua.pushinteger (fromIntegral v)
        Lua.setfield (-2) name

-- | world.hasSpawnedLocationContents(gx, gy [, pageId]) → bool.
--   One-time content-spawn flag (#90). COORDINATE-ADDRESSED
--   compatibility wrapper (#911): it resolves the chunk containing
--   (gx, gy) and reports the FIRST (lowest-id) instance anchored there,
--   which is what @scripts/locations.lua@ and
--   @scripts/location_stamper.lua@ have always meant by it — placement
--   never puts two locations in one chunk
--   ('Location.Overlay.placeDef' rejects that). A caller that needs to
--   address a specific instance uses @world.getLocationInstance@'s
--   `contents_spawned` instead. With no page argument the active world
--   is read; false when no such world, no live params, or no instance
--   in that chunk.
worldHasSpawnedLocationContentsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldHasSpawnedLocationContentsFn env = do
    gxA ← Lua.tointeger 1
    gyA ← Lua.tointeger 2
    pageA ← Lua.tostring 3
    case (gxA, gyA) of
        (Just gx, Just gy) → do
            spawned ← Lua.liftIO $ do
                instances ← instancesForPage env pageA
                let (coord, _) = globalToChunk (fromIntegral gx) (fromIntegral gy)
                pure $ case instancesInChunk coord instances of
                    (inst:_) → liContentsSpawned inst
                    []       → False
            Lua.pushboolean spawned
            return 1
        _ → Lua.pushboolean False >> return 1

-- | world.hasStampedLocation(gx, gy [, pageId]) → bool. One-time
--   geometry-stamp flag (#424), deliberately still CHUNK-keyed (#911
--   left it alone): true once the chunk containing (gx, gy) has had its
--   placed location's builder run. This is the idempotency check
--   'scripts/location_stamper.lua' consults instead of
--   @structure.hasAt gx gy "floor"@ — a check that a player clearing the
--   anchor floor tile would otherwise defeat. With no page argument the
--   active world is read; false when no such world or its gen params
--   aren't live.
worldHasStampedLocationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldHasStampedLocationFn env = do
    gxA ← Lua.tointeger 1
    gyA ← Lua.tointeger 2
    pageA ← Lua.tostring 3
    case (gxA, gyA) of
        (Just gx, Just gy) → do
            stamped ← Lua.liftIO $ do
                mParams ← paramsForPage env pageA
                pure $ case mParams of
                    Nothing → False
                    Just params →
                        let (coord, _) =
                                globalToChunk (fromIntegral gx) (fromIntegral gy)
                        in HS.member coord (wgpLocationStamped params)
            Lua.pushboolean stamped
            return 1
        _ → Lua.pushboolean False >> return 1
