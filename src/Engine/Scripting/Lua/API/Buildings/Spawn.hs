{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Buildings.Spawn
    ( buildingSpawnFn
    , buildingDestroyFn
    , buildingCanPlaceAtFn
    , buildingRemoteCheckFn
    , buildingSetGhostFn
    , buildingClearGhostFn
    ) where

import UPrelude
import Engine.Core.Capability.Building
    (BuildingCapability(..), toBuildingCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, writeIORef)
import Engine.Core.State (EngineEnv, activeWorldPageFrom, resolveActiveWorld)
import World.Page.Types (WorldPageId(..))
import qualified Engine.Core.Queue as Q
import Building.Types
import Building.Command.Types (BuildingCommand(..))
import World.Command.Types (WorldCommand(..))
import Building.Placement
    ( buildingAnchorZ, canPlaceAt, PlacementResult(..), RemoteCheck(..)
    , remoteCheck, isRemote
    )
import Building.Reservation (reserveFootprint)
import Location.Bounds (remotePortalThresholdTiles)
import World.Types
    ( WorldManager(..), WorldState(..), WorldGenParams(..) )
import Engine.Scripting.Lua.API.PageBinding
    (bindingStale, pageBindingStaleReason)
import World.Generate.Coordinates (canonicalTile)
import World.Tile.Types (WorldTileData)
import Location.Instance (emptyLocationInstances)

-- * Spawn / destroy

-- | building.spawn(defName, gx, gy [, pageId [, bindGen]]) — returns the
--   new building id on success, or @(nil, reason)@ otherwise (unknown
--   def, placement invalid, footprint already claimed, stale page
--   binding). Placement is validated
--   server-side too so Lua scripts can't accidentally place into water
--   etc. An explicit pageId (slot 4) pins the spawn — AND the
--   occupancy/terrain-Z check — to that live page (even hidden) instead
--   of the active world: location content-spawning (#90) passes its own
--   page so a building lands (and validates) on the page its location is
--   on, not whichever happens to be visible. Omitted → the active world,
--   as before (#76).
--
--   @bindGen@ (slot 5, #1602) is the page-selection generation
--   @world.pickTile@ reported for the click this spawn is committing.
--   When present it is compared against 'wmSelectionGen' inside the SAME
--   manager read that resolves the target page, so page selection cannot
--   move between the check and the resolution: a mismatch spawns
--   nothing and answers @(nil, "page binding stale")@, distinct from
--   every ordinary placement refusal.
--
--   #2326: the placement check above is ADVISORY — it reads a manager
--   snapshot, and the insertion happens later on another thread. What
--   is authoritative is the 'Building.Reservation.reserveFootprint'
--   transaction that follows it: the footprint claim and the
--   'BuildingId' allocation are taken together, so a second request
--   admitted against that same snapshot is refused here, synchronously,
--   with @(nil, "tile already occupied")@ and no id consumed. The
--   commit re-verifies that claim before it inserts.
--
--   That is the SYNCHRONOUS half, and it is what this call owes its
--   caller — but it is not the commit, and this thread cannot make it
--   one: 'wmSelectionGen' belongs to the world thread. A bound spawn is
--   therefore routed to that thread as 'WorldSpawnBoundBuilding', which
--   re-checks the binding and performs the insertion itself, in the one
--   place a selection change cannot be interleaved between the two.
--   Omitted → the command goes straight to the building queue, exactly
--   as before.
buildingSpawnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingSpawnFn env = do
    nameArg ← Lua.tostring 1
    xArg    ← Lua.tointeger 2
    yArg    ← Lua.tointeger 3
    pageArg ← Lua.tostring 4
    bindArg ← Lua.tointeger 5
    case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → do
            let defName = TE.decodeUtf8Lenient nameBS
                gx      = fromIntegral x
                gy      = fromIntegral y
            result ← Lua.liftIO $ do
                bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
                -- ONE manager read serves both the binding check and the
                -- target resolution (#1602): re-reading for the second
                -- would reopen the very window the binding closes.
                wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
                let mTarget = case pageArg of
                        Just pidBS →
                            let pid = WorldPageId (TE.decodeUtf8Lenient pidBS)
                            in (\ws → (pid, ws)) <$> lookup pid (wmWorlds wm)
                        Nothing → resolveActiveWorld wm
                case (bindingStale bindArg wm, HM.lookup defName (bmDefs bm), mTarget) of
                    (True, _, _) → pure (Left pageBindingStaleReason)
                    (_, Just def, Just (pid, ws)) → do
                        wtd ← readIORef (wsTilesRef ws)
                        mParams ← readIORef (wsGenParamsRef ws)
                        let locInstances = maybe emptyLocationInstances
                                                 wgpLocationInstances mParams
                            worldSizeChunks = maybe 0 wgpWorldSize mParams
                            -- #1175: resolve the anchor into the stored
                            -- frame BEFORE validating, reading terrain z, or
                            -- recording the spawn. A CtBuilding construct job
                            -- restored from a pre-#1175 save can hold an
                            -- alias, and validating that raw reports the
                            -- (loaded) canonical chunk as missing — the AI
                            -- then cancels a perfectly good designation.
                            -- Identity away from the seam.
                            (cgx, cgy) = canonicalTile worldSizeChunks gx gy
                        case canPlaceAt
                                (bm { bmInstances =
                                        buildingsOnPage pid (bmInstances bm) })
                                wtd locInstances worldSizeChunks def cgx cgy of
                            NotPlaceable reason → pure (Left reason)
                            Placeable → do
                                let gz = floorZAt worldSizeChunks wtd cgx cgy
                                -- #2326: the id allocation and the
                                -- footprint claim are ONE transaction.
                                -- The check above ran against a snapshot
                                -- and is advisory; this is what makes
                                -- the tiles unavailable to the next
                                -- request reading that same snapshot,
                                -- and what refuses this one — with no id
                                -- consumed — when it lost the race.
                                eBid ← reserveFootprint
                                    (bcBuildingManagerRef
                                        (toBuildingCapability env))
                                    worldSizeChunks pid def cgx cgy
                                case eBid of
                                    Left reason → pure (Left reason)
                                    Right bid → do
                                        enqueueSpawn env bindArg
                                            bid defName cgx cgy gz pid
                                        pure (Right bid)
                    (_, Nothing, _) → pure (Left "unknown building")
                    (_, _, Nothing)  → pure (Left "no active world")
            case result of
                Right (BuildingId n) → do
                    Lua.pushinteger (fromIntegral n)
                    return 1
                Left reason → do
                    Lua.pushnil
                    Lua.pushstring (TE.encodeUtf8 reason)
                    return 2
        _ → do
            Lua.pushnil
            Lua.pushstring "bad arguments"
            return 2

buildingDestroyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingDestroyFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (bcBuildingQueue (toBuildingCapability env)) $ BuildingDestroy bid
            Lua.pushboolean True
            return 1

-- * Placement check

-- | building.canPlaceAt(defName, gx, gy [, bindPage, bindGen]) — returns
--   @(true, nil, false)@ on success or @(false, reason, stale)@ on
--   rejection. Cheap to call every frame from the build tool's ghost
--   preview update.
--
--   ADVISORY, and deliberately so (#2326 requirement 4). It is an early
--   diagnostic and the page-binding probe, not the authority on whether
--   the tiles are free: it reports on a manager snapshot any concurrent
--   admission can move, and it deliberately ignores 'bmReservations' so
--   a ghost is never greyed out by a placement that has not landed.
--   @building.spawn@'s own reservation transaction is what actually
--   decides, and a true answer here can still be refused there.
--
--   #1602: ONE 'wsWorldManagerRef' read now answers the whole call. The
--   page identity, the page-scoped occupancy filter, the location
--   instances, the u-wrap world size, the canonical coordinates and the
--   terrain all come from that single resolution, so the check can no
--   longer combine one page's metadata with another page's terrain (it
--   previously resolved the active page and then re-read the manager for
--   the visible page's tiles).
--
--   The optional binding (slots 4-5) is @world.pickTile@'s page id and
--   page-selection generation for the click being validated. The call
--   answers @(false, "page binding stale", true)@ WITHOUT consulting any
--   page when either half no longer holds — the generation has moved, or
--   the visible page is no longer the one named. The generation alone
--   already implies the page (it moves on every visible-list change),
--   but checking the id too means a caller that supplies one is never
--   silently answered about a different page. The third result is what
--   lets a caller tell a moved page apart from an ordinary refusal
--   without matching on the reason text; existing three-argument callers
--   see their previous two-result behaviour unchanged.
--
--   Empty-visible behaviour is deliberately preserved exactly (#1602
--   requirement 10), including its two DISTINCT reasons: @"no active
--   world"@ when no page is registered at all, and @"no world loaded"@
--   when a page is registered but none is visible. A registered-but-
--   hidden page is never silently used.
buildingCanPlaceAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingCanPlaceAtFn env = do
    nameArg ← Lua.tostring 1
    xArg    ← Lua.tointeger 2
    yArg    ← Lua.tointeger 3
    pageArg ← Lua.tostring 4
    bindArg ← Lua.tointeger 5
    case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → do
            let defName  = TE.decodeUtf8Lenient nameBS
                gx       = fromIntegral x
                gy       = fromIntegral y
                bindPage = WorldPageId . TE.decodeUtf8Lenient <$> pageArg
            (result, stale) ← Lua.liftIO $ do
                bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
                wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
                -- Occupancy is checked only against the ACTIVE world's
                -- buildings — a building in another world must not block
                -- placement here (#76).
                if bindingStale bindArg wm ∨ boundPageMoved bindPage wm
                then pure (NotPlaceable pageBindingStaleReason, True)
                else fmap (\r → (r, False)) $
                  case (HM.lookup defName (bmDefs bm), resolveActiveWorld wm) of
                    (Nothing, _) → pure (NotPlaceable "unknown building")
                    (_, Nothing) → pure (NotPlaceable "no active world")
                    (Just def, Just (pid, ws))
                        -- resolveActiveWorld falls back to the wmWorlds
                        -- head when nothing is visible; placement must
                        -- never answer for a hidden page, so that case
                        -- keeps rejecting exactly as the separate
                        -- visible-page terrain read used to.
                        | null (wmVisible wm) → pure (NotPlaceable "no world loaded")
                        | otherwise → do
                            wtd ← readIORef (wsTilesRef ws)
                            mParams ← readIORef (wsGenParamsRef ws)
                            let locInstances = maybe emptyLocationInstances
                                                     wgpLocationInstances mParams
                                worldSizeChunks = maybe 0 wgpWorldSize mParams
                                -- #1175: the ghost preview must answer
                                -- for the tile building.spawn will
                                -- actually use, or the tool says
                                -- "placeable" and the spawn refuses.
                                (cgx, cgy) = canonicalTile worldSizeChunks gx gy
                            pure (canPlaceAt
                                (bm { bmInstances =
                                        buildingsOnPage pid (bmInstances bm) })
                                wtd locInstances worldSizeChunks def cgx cgy)
            case result of
                Placeable → do
                    Lua.pushboolean True
                    Lua.pushnil
                    Lua.pushboolean stale
                    return 3
                NotPlaceable reason → do
                    Lua.pushboolean False
                    Lua.pushstring (TE.encodeUtf8 reason)
                    Lua.pushboolean stale
                    return 3
        _ → do
            Lua.pushboolean False
            Lua.pushstring "bad arguments"
            Lua.pushboolean False
            return 3

-- * Remote-settlement check (#779)

-- | building.remoteCheck(defName, gx, gy) — (remote, distance,
--   thresholdTiles). For a starting building, `distance` is the
--   seam-aware nearest footprint→placed-location distance among every
--   location placed on the ACTIVE world page, or nil when that page
--   has none at all (still `remote = true` in that case — see
--   'Building.Placement.RemoteCheck'). Always @(false, nil,
--   thresholdTiles)@ for a non-starting def, an unknown def, or when
--   there's no active world — mirrors 'canPlaceAt's own #778 gate on
--   `bdIsStarting`. Cheap enough to call once per click, unlike
--   'canPlaceAt' which runs every ghost-preview frame.
buildingRemoteCheckFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingRemoteCheckFn env = do
    nameArg ← Lua.tostring 1
    xArg    ← Lua.tointeger 2
    yArg    ← Lua.tointeger 3
    check ← case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → Lua.liftIO $ do
            let defName = TE.decodeUtf8Lenient nameBS
                gx      = fromIntegral x
                gy      = fromIntegral y
            bm ← readIORef (bcBuildingManagerRef (toBuildingCapability env))
            mActive ← activeWorldPageFrom (wsWorldManagerRef (toWorldSimCapability env))
            case (HM.lookup defName (bmDefs bm), mActive) of
                (Just def, Just (_pid, ws)) → do
                    mParams ← readIORef (wsGenParamsRef ws)
                    let locInstances = maybe emptyLocationInstances
                                             wgpLocationInstances mParams
                        worldSizeChunks = maybe 0 wgpWorldSize mParams
                        -- #1175: measured against the same anchor
                        -- canPlaceAt/spawn resolve, so the remoteness
                        -- answer describes the tile that gets built on.
                        (cgx, cgy) = canonicalTile worldSizeChunks gx gy
                    pure (remoteCheck locInstances worldSizeChunks def cgx cgy)
                _ → pure NotStartingBuilding
        _ → pure NotStartingBuilding
    Lua.pushboolean (isRemote check)
    case check of
        RemoteDistance (Just d) → Lua.pushinteger (fromIntegral d)
        _                       → Lua.pushnil
    Lua.pushinteger (fromIntegral remotePortalThresholdTiles)
    return 3

-- * Ghost preview

-- | building.setGhost(defName, gx, gy, valid) — install or update the
--   single ghost preview slot. Cleared via clearGhost or by passing
--   an empty string as defName.
buildingSetGhostFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingSetGhostFn env = do
    nameArg  ← Lua.tostring 1
    xArg     ← Lua.tointeger 2
    yArg     ← Lua.tointeger 3
    validArg ← Lua.toboolean 4
    case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → do
            let name = TE.decodeUtf8Lenient nameBS
                gx   = fromIntegral x
                gy   = fromIntegral y
            Lua.liftIO $ do
                -- Sample the terrain Z at the ghost tile so the
                -- render pass can elevate the preview to where the
                -- building will actually land. Matches the spawn
                -- path's `floorZAt`; falls back to 0 if the chunk
                -- isn't loaded.
                --
                -- #1175: the ghost is drawn at the CANONICAL tile the
                -- spawn will use, so the preview and the building that
                -- follows it never sit a world apart at the seam.
                --
                -- #1602: ONE manager read supplies BOTH the u-wrap world
                -- size that canonicalizes the tile and the terrain the
                -- elevation is sampled from. The pair of independent
                -- reads this replaced could size the ghost by one page
                -- and elevate it by another. Empty wmVisible keeps its
                -- documented fallback: unwrapped input coordinates
                -- (world size 0) and elevation 0.
                mVisible ← visiblePageStateFrom env
                (worldSize, mWtd) ← case mVisible of
                    Nothing → pure (0, Nothing)
                    Just ws → do
                        sz ← maybe 0 wgpWorldSize <$> readIORef (wsGenParamsRef ws)
                        wtd ← readIORef (wsTilesRef ws)
                        pure (sz, Just wtd)
                let (cgx, cgy) = canonicalTile worldSize gx gy
                    gz = case mWtd of
                        Just wtd → floorZAt worldSize wtd cgx cgy
                        Nothing  → 0
                writeIORef (bcBuildingGhostRef (toBuildingCapability env)) $ Just BuildingGhost
                    { bgDefName = name
                    , bgGridX   = cgx
                    , bgGridY   = cgy
                    , bgGridZ   = gz
                    , bgValid   = validArg
                    }
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

buildingClearGhostFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingClearGhostFn env = do
    Lua.liftIO $ writeIORef (bcBuildingGhostRef (toBuildingCapability env)) Nothing
    Lua.pushboolean True
    return 1

-- * Helpers

-- | The VISIBLE page's state (head of wmVisible) from ONE manager read
--   (#1602) — the single resolution the ghost preview derives BOTH its
--   u-wrap world size (#1175) and its terrain elevation from. 'Nothing'
--   when nothing is visible, which is the documented fallback to
--   unwrapped coordinates and elevation 0; a registered-but-hidden page
--   is never substituted.
visiblePageStateFrom ∷ EngineEnv → IO (Maybe WorldState)
visiblePageStateFrom env = do
    wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    pure $ case wmVisible wm of
        []         → Nothing
        (pageId:_) → lookup pageId (wmWorlds wm)

-- | Route a validated spawn to the queue that can actually commit it
--   (#1602). An UNBOUND spawn — location content-spawning, the AI's
--   blueprint staking, anything with no click behind it — goes straight
--   to the building queue exactly as it always has. A BOUND one goes to
--   the world thread instead, because that is the only thread page
--   selection cannot move underneath: it discharges the binding and
--   forwards the same 'BuildingSpawn' from there
--   ("World.Thread.Command.BoundSpawn").
enqueueSpawn ∷ EngineEnv → Maybe Lua.Integer
             → BuildingId → Text → Int → Int → Int → WorldPageId → IO ()
enqueueSpawn env mBind bid defName gx gy gz pid = case mBind of
    Nothing   → Q.writeQueue (bcBuildingQueue (toBuildingCapability env)) $
        BuildingSpawn bid defName gx gy gz pid
    Just want → Q.writeQueue (wsWorldQueue (toWorldSimCapability env)) $
        WorldSpawnBoundBuilding bid defName gx gy gz pid (fromIntegral want)

-- | Is the page a binding names no longer the visible one (#1602)? The
--   generation check above already covers this — it moves on every
--   change to 'wmVisible' — so this is the redundant half that keeps a
--   supplied page id from being accepted and then quietly ignored.
--   'Nothing' (no page supplied) never rejects.
boundPageMoved ∷ Maybe WorldPageId → WorldManager → Bool
boundPageMoved Nothing    _  = False
boundPageMoved (Just pid) wm = case wmVisible wm of
    (visible:_) → visible ≢ pid
    []          → True

-- | Terrain Z at the anchor tile. Falls back to 0 if the chunk isn't
--   loaded — shouldn't happen since canPlaceAt already verified, but
--   defensive.
--
--   'Building.Placement.buildingAnchorZ' is the read itself, shared with
--   the committed-designation ghost since #1845 so a planned building is
--   never drawn at a z its stake will not land on.
floorZAt ∷ Int → WorldTileData → Int → Int → Int
floorZAt worldSize wtd gx gy = fromMaybe 0 (buildingAnchorZ worldSize wtd gx gy)
