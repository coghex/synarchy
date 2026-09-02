{-# LANGUAGE Strict #-}
-- | Placed-location queries: world.listPlacedLocations,
--   world.getLocationInstance, world.hasSpawnedLocationContents,
--   world.hasStampedLocation.
--
--   This module was narrowed to the @content-registries@ capability by
--   #890 (epic #537) so the location-def registry was reached only
--   through 'ContentRegistriesCapability'. #911 removed even that: every
--   value these queries report — definition id, anchor, bounds,
--   display name and gloss, lifecycle, content-spawn flag — is stored
--   on the placed-location INSTANCE, so nothing here consults the
--   registry at all any more. The remaining 'EngineEnv' parameter is purely the
--   opaque token the not-yet-narrowed @world-sim-render-handoff@
--   page-lookup services ('activeWorldState', 'worldStateByPage')
--   demand — this module dereferences no 'EngineEnv' field itself
--   (#915's awareness query reaches the unit manager through
--   'UnitCombatCapability', the same projection
--   "World.Thread.Discovery" uses), and that parameter goes away when
--   @world-sim-render-handoff@ migrates (SS7.4).
module Engine.Scripting.Lua.API.WorldQuery.Location
    ( worldListPlacedLocationsFn
    , worldGetLocationInstanceFn
    , worldGetLocationAwarenessFn
    , worldHasSpawnedLocationContentsFn
    , worldHasStampedLocationFn
    ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified HsLua as Lua
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (sortOn)
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv, activeWorldStateFrom)
import World.Types
import Location.Discovery (AwarenessHit(..), UnitSight(..), findAwareness)
import Location.Instance
    ( LocationEncounter(..), LocationEncounterOccupant(..)
    , LocationInstance(..), LocationInstanceId(..), LocationInstances
    , LocationSignificantItem(..)
    , instancesToList, instancesInChunk, lookupLocationInstance
    , isDiscoveredLifecycle, lifecycleName, emptyLocationInstances
    , locationAuthorsClearance, locationClearanceSatisfied )
import Location.Bounds (AbsBounds(..))
import Unit.Faction (isPlayerOwned)
import Unit.LineOfSight (visibleTilesOnPage)
import Unit.Types (UnitInstance(..), UnitManager(..), UnitId(..))
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
--       discovered,        -- has a player-owned unit SEEN it yet
--                          --   (#780 trigger, sight-based since #1230)?
--       instance_id,       -- the stable per-page instance id (#911)
--       lifecycle,         -- "unknown" | "hinted" | "discovered"
--                          --   | "active" | "cleared" | "depleted"
--       name,              -- display name: rendered in the page's own
--                          --   generated language (#1101) when it has
--                          --   one, else the def's label
--       gloss,             -- OMITTED unless `name` is generated: its
--                          --   English reading (#1101), from the same
--                          --   name expression. Mirrors
--                          --   world.getIdentity's optional gloss.
--       contents_spawned,  -- one-time content-spawn flag (#90)
--       significant,       -- #917: array of this instance's guaranteed
--                          --   significant-item obligations, in slot
--                          --   order: { slot, item, taken } plus
--                          --   `item_instance_id` once one is spawned
--                          --   (OMITTED before that). Always present,
--                          --   empty when the location owes none.
--       authors_clearance, -- does it author ANY clearance condition?
--       clearance_satisfied, -- is the compound predicate satisfied?
--       clear_event_emitted } -- has its one clearance notice been spent?
--
--   #911 EXTENDED this table; it did not repurpose anything. `id` still
--   means the DEFINITION id (that is what @scripts/locations.lua@ joins
--   against @locations.getDef@) — the instance identity is the separate
--   `instance_id`. `discovered` is now derived as "lifecycle at or
--   beyond discovered", which is exactly what the flag it replaced meant.
--   `bounds` comes from the INSTANCE, which stored it when it was
--   placed, so it no longer depends on the def still being registered
--   this session (and an unregistered def can no longer silently omit
--   it). #1230 removed the `discovery_margin` field entirely: reveal is
--   sight-based, and `bounds` is the only location footprint left.
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

-- | world.getLocationAwareness() → array of
--   @{ uid, page, instance_id, gx, gy }@ rows: every PLAYER-OWNED unit
--   (#912) that can currently SEE a placed location, on EVERY loaded
--   page — the acquisition surface for #915's per-unit location memory
--   (@scripts\/unit_ai_locations.lua@).
--
--   This is deliberately a stateless, idempotent QUERY rather than a
--   drained event stream: the caller re-asks and re-records, and its own
--   identity dedup makes a repeat row a no-op. What matters is that it
--   preserves all three properties of the player-wide discovery tick it
--   shares its predicate with ("World.Thread.Discovery"):
--
--   * the same 'Unit.Faction.isPlayerOwned' filter and the same
--     seam-aware bounds containment over the same sight calculation,
--     because both come from the SAME pure enumeration
--     ('Location.Discovery.findAwareness' /
--     'Location.Discovery.findDiscoveries' share @sightContactsWhere@);
--   * every LOADED page, not only the active/visible one — hence
--     @page@ on every row, since a location's durable identity is
--     @(page, instance_id)@ and bare instance ids alias across pages;
--   * pause independence — nothing here reads the pause flag, and
--     @scripts\/unit_ai.lua@ calls it BEFORE its own pause guard.
--
--   Reports awareness regardless of lifecycle: a unit that later sees a
--   ruin the player mapped long ago still learns where it is, which is
--   exactly the difference between the experiential and cartographic
--   layers.
worldGetLocationAwarenessFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
worldGetLocationAwarenessFn env = do
    rows ← Lua.liftIO (locationAwarenessRows env)
    Lua.newtable
    forM_ (zip [1 ..] rows) $ \(i, (pageText, hit)) → do
        Lua.newtable
        pushIntField' "uid" (unUnitId (ahUnit hit))
        Lua.pushstring (TE.encodeUtf8 pageText)
        Lua.setfield (-2) "page"
        pushIntField' "instance_id" (unLocationInstanceId (ahInstance hit))
        pushIntField' "gx" (fst (ahAnchor hit))
        pushIntField' "gy" (snd (ahAnchor hit))
        Lua.rawseti (-2) i
    return 1
  where
    pushIntField' name v = do
        Lua.pushinteger (fromIntegral v)
        Lua.setfield (-2) name

-- | Every loaded page's awareness hits, page-id text carried alongside.
--   Units are partitioned by 'uiPage' and passed in unit-id order, the
--   same way 'World.Thread.Discovery.tickLocationDiscovery' does, so the
--   row order is deterministic across runs.
--
--   Sight comes from 'Unit.LineOfSight.visibleTilesOnPage' against each
--   page's OWN 'WorldState' (#1230) — the same calculation the discovery
--   tick and the public @unit.getVisibleTiles@ query run, so the two
--   knowledge layers cannot drift — and deliberately not through the
--   public 'Unit.LineOfSight.unitVisibleTiles' wrapper, whose
--   @wmVisible@ gate would silently blind every unit on a loaded but
--   hidden page. As in the discovery tick, sight is computed ONCE per
--   unit and only for units the shared 'isPlayerOwned' filter could
--   accept; 'findAwareness' still applies that filter itself.
locationAwarenessRows ∷ EngineEnv → IO [(Text, AwarenessHit UnitId)]
locationAwarenessRows env = do
    um  ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    let allUnits = sortOn fst (HM.toList (umInstances um))
    fmap concat $ forM (wmWorlds mgr) $ \(pageId@(WorldPageId pageText), ws) → do
        mParams ← readIORef (wsGenParamsRef ws)
        case mParams of
            Nothing → pure []
            Just p
                | null (instancesToList (wgpLocationInstances p)) → pure []
                | otherwise → do
                    let pageUnits =
                            [ (uid, inst)
                            | (uid, inst) ← allUnits
                            , uiPage inst ≡ pageId
                            , isPlayerOwned (uiFactionId inst) ]
                    sights ← forM pageUnits $ \(uid, inst) → do
                        tiles ← visibleTilesOnPage ws inst
                        pure UnitSight { usUnit    = uid
                                       , usFaction = uiFactionId inst
                                       , usTiles   = tiles }
                    pure [ (pageText, hit)
                         | hit ← findAwareness (wgpWorldSize p)
                                     (wgpLocationInstances p) sights ]

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
    -- #1101: the English gloss of a name generated in the page's own
    -- language. OMITTED (not a Lua nil value) when absent, mirroring
    -- world.getIdentity's optional `gloss` — its absence means the name
    -- is a definition label, which has no meaning to explain.
    forM_ (liGloss inst) $ \g → do
        Lua.pushstring (TE.encodeUtf8 g)
        Lua.setfield (-2) "gloss"
    Lua.pushboolean (isDiscoveredLifecycle (liLifecycle inst))
    Lua.setfield (-2) "discovered"
    Lua.pushboolean (liContentsSpawned inst)
    Lua.setfield (-2) "contents_spawned"
    forM_ (liEncounter inst) $ \encounter → do
        Lua.newtable
        pushIntField "rolled_count" (leRolledCount encounter)
        Lua.pushboolean (leRosterComplete encounter)
        Lua.setfield (-2) "roster_complete"
        Lua.pushboolean (leDeathOnlyClearance encounter)
        Lua.setfield (-2) "death_only_clearance"
        Lua.pushboolean (leActivated encounter)
        Lua.setfield (-2) "activated"
        Lua.pushboolean (leEpisodeActive encounter)
        Lua.setfield (-2) "episode_active"
        Lua.pushboolean (leAggressionAnnounced encounter)
        Lua.setfield (-2) "aggression_announced"
        Lua.pushboolean (leDisengageAnnounced encounter)
        Lua.setfield (-2) "disengage_announced"
        Lua.pushboolean (leCleared encounter)
        Lua.setfield (-2) "cleared"
        Lua.newtable
        forM_ (zip [1 ..] (leOccupants encounter)) $ \(index, occupant) → do
            Lua.newtable
            pushIntField "uid" (unUnitId (leoUnitId occupant))
            let (homeX, homeY) = leoHome occupant
            Lua.pushnumber (realToFrac homeX)
            Lua.setfield (-2) "home_x"
            Lua.pushnumber (realToFrac homeY)
            Lua.setfield (-2) "home_y"
            Lua.pushboolean (leoEngaged occupant)
            Lua.setfield (-2) "engaged"
            Lua.pushboolean (leoReturning occupant)
            Lua.setfield (-2) "returning"
            Lua.rawseti (-2) index
        Lua.setfield (-2) "occupants"
        Lua.setfield (-2) "encounter"
    -- #917: the guaranteed significant items this instance owes, in
    -- authored slot order. ALWAYS an array (empty for a location that
    -- authors none), so a caller can iterate without a nil check and
    -- can tell "owes nothing" from "owes one, not yet spawned" — which
    -- an omitted field could not express.
    Lua.newtable
    forM_ (zip [1 ..] (sortOn lsiSlot (liSignificant inst)))
        $ \(index, entry) → do
            Lua.newtable
            pushIntField "slot" (lsiSlot entry)
            Lua.pushstring (TE.encodeUtf8 (lsiItemDefName entry))
            Lua.setfield (-2) "item"
            -- OMITTED until the content spawn binds one, mirroring
            -- `gloss` above: absence means "not spawned yet", which is
            -- exactly what keeps the loot condition incomplete.
            forM_ (lsiInstanceId entry) $ \iid → do
                Lua.pushinteger (fromIntegral iid)
                Lua.setfield (-2) "item_instance_id"
            Lua.pushboolean (lsiTaken entry)
            Lua.setfield (-2) "taken"
            Lua.rawseti (-2) index
    Lua.setfield (-2) "significant"
    -- The compound clearance predicate, reported rather than left for
    -- every caller to re-derive from `encounter` and `significant` — a
    -- second implementation is what would drift. `authors_clearance`
    -- distinguishes a location with no clearance condition at all
    -- (which never clears) from one whose conditions are outstanding.
    Lua.pushboolean (locationAuthorsClearance inst)
    Lua.setfield (-2) "authors_clearance"
    Lua.pushboolean (locationClearanceSatisfied inst)
    Lua.setfield (-2) "clearance_satisfied"
    Lua.pushboolean (liClearEventEmitted inst)
    Lua.setfield (-2) "clear_event_emitted"
    Lua.newtable
    pushIntField "min_x" (abMinX ab)
    pushIntField "min_y" (abMinY ab)
    pushIntField "max_x" (abMaxX ab)
    pushIntField "max_y" (abMaxY ab)
    Lua.setfield (-2) "bounds"
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
--   placed location's builder complete with every placement it attempted
--   successful (#1719) — a partial stamp leaves this false so the
--   every-load dispatch retries it. This is the idempotency check
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
