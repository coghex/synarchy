{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.World.Edit
    ( worldAddTileFn
    , worldDigTileFn
    , worldDeleteTileFn
    , worldSetFluidTileFn
    , worldSetSlopeFn
    , worldSetVegFn
    , worldPlantRowCropAtFn
    , worldSetCellFn
    , worldMarkLocationContentsSpawnedFn
    , worldMarkLocationContentsSpawnedByIdFn
    , worldRegisterLocationEncounterOccupantsFn
    , worldSetLocationEncounterOccupantStateFn
    , worldSetLocationEncounterEpisodeStateFn
    , worldSetLocationLifecycleFn
    , worldMarkLocationStampedFn
    ) where

import UPrelude
import qualified HsLua as Lua
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as TE
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Core.State (activeWorldPageFrom)
import Location.Instance
    ( LocationInstance(..), LocationInstanceId(..)
    , instancesInChunk, lifecycleFromName )
import Structure.Types
    (StructureCommitWindow(..), StructureStageToken(..))
import World.Generate.Coordinates (globalToChunk)
import World.Types hiding (activeWorldPage)
import World.Material (MaterialId(..), materialIdByName)
import Unit.Types (UnitId(..))

-- | The live page this call targets: the named one when a pageId string
--   is given (even hidden), the active world otherwise. 'Nothing' when
--   neither resolves.
targetPage ∷ WorldSimCapability → Maybe ByteString → IO (Maybe WorldPageId)
targetPage wsc pageArg = case pageArg of
    Just pidBS → pure (Just (WorldPageId (TE.decodeUtf8Lenient pidBS)))
    Nothing    → fmap fst <$> activeWorldPageFrom (wsWorldManagerRef wsc)

-- | That page's live gen params, when it has any.
pageParams ∷ WorldSimCapability → WorldPageId → IO (Maybe WorldGenParams)
pageParams wsc pid = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    case lookup pid (wmWorlds mgr) of
        Nothing → pure Nothing
        Just ws → readIORef (wsGenParamsRef ws)

-- | world.markLocationContentsSpawned(gx, gy [, pageId]) — one-time
--   content-spawn flag (#90). COORDINATE-ADDRESSED compatibility
--   wrapper (#911): it resolves the chunk containing (gx, gy) and marks
--   the FIRST (lowest-id) instance anchored there, matching what
--   @world.hasSpawnedLocationContents@ reports and what
--   @scripts/locations.lua@ has always meant — placement never puts two
--   locations in one chunk. A caller that must address a specific
--   instance uses @world.markLocationContentsSpawnedById@. An explicit
--   pageId targets that live page (even hidden); omitted defaults to
--   the active world. No-op (queues nothing) when neither resolves to a
--   live page, or when no instance is anchored in that chunk.
worldMarkLocationContentsSpawnedFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldMarkLocationContentsSpawnedFn wsc = do
    gxArg   ← Lua.tointeger 1
    gyArg   ← Lua.tointeger 2
    pageArg ← Lua.tostring 3
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            Lua.liftIO $ do
                mPid ← targetPage wsc pageArg
                forM_ mPid $ \pid → do
                    mParams ← pageParams wsc pid
                    let (coord, _) =
                            globalToChunk (fromIntegral gx) (fromIntegral gy)
                        insts = maybe [] (instancesInChunk coord
                                           . wgpLocationInstances) mParams
                    case insts of
                        (inst:_) → Q.writeQueue (wsWorldQueue wsc) $
                            WorldMarkLocationContentsSpawned pid (liId inst)
                        [] → pure ()
            return 0
        _ → return 0

-- | world.markLocationContentsSpawnedById(instanceId [, pageId]) — the
--   instance-addressed one-time content-spawn flag (#90/#911). Marking
--   one instance never touches another, including a second instance
--   anchored in the same chunk. No-op when the page doesn't resolve; an
--   unknown instance id is a no-op on the world thread.
worldMarkLocationContentsSpawnedByIdFn
    ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldMarkLocationContentsSpawnedByIdFn wsc = do
    idArg   ← Lua.tointeger 1
    pageArg ← Lua.tostring 2
    case idArg of
        Just rawId → do
            Lua.liftIO $ do
                mPid ← targetPage wsc pageArg
                forM_ mPid $ \pid → Q.writeQueue (wsWorldQueue wsc) $
                    WorldMarkLocationContentsSpawned pid
                        (LocationInstanceId (fromIntegral rawId))
            return 0
        Nothing → return 0

-- | world.registerLocationEncounterOccupants(instanceId, occupants
--   [, pageId]) → bool. Each occupant is @{ uid, home_x, home_y }@. The
--   whole dense roster is parsed before queueing, so malformed input can
--   never persist a partial encounter membership.
worldRegisterLocationEncounterOccupantsFn
    ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldRegisterLocationEncounterOccupantsFn wsc = do
    idArg ← Lua.tointeger 1
    isRoster ← Lua.istable 2
    pageArg ← Lua.tostring 3
    mRoster ← if isRoster then readRoster else pure Nothing
    queued ← case (idArg, mRoster) of
        (Just rawId, Just roster) → Lua.liftIO $ do
            mPid ← targetPage wsc pageArg
            case mPid of
                Nothing → pure False
                Just pid → do
                    Q.writeQueue (wsWorldQueue wsc) $
                        WorldRegisterLocationEncounterOccupants pid
                            (LocationInstanceId (fromIntegral rawId)) roster
                    pure True
        _ → pure False
    Lua.pushboolean queued
    return 1
  where
    readRoster = do
        n ← Lua.rawlen 2
        go 1 (fromIntegral n) []
    go i n acc
        | i > n = pure (Just (reverse acc))
        | otherwise = do
            ty ← Lua.rawgeti 2 (fromIntegral i)
            mEntry ← if ty ≢ Lua.TypeTable then pure Nothing else do
                uid ← integerField "uid"
                hx ← numberField "home_x"
                hy ← numberField "home_y"
                pure $ case (uid, hx, hy) of
                    (Just rawUid, Just x, Just y) | rawUid ≥ 0 →
                        Just (UnitId (fromIntegral rawUid), (x, y))
                    _ → Nothing
            Lua.pop 1
            maybe (pure Nothing) (\entry → go (i + 1) n (entry : acc)) mEntry
    integerField name = do
        _ ← Lua.getfield (-1) name
        value ← Lua.tointeger (-1)
        Lua.pop 1
        pure value
    numberField name = do
        _ ← Lua.getfield (-1) name
        value ← Lua.tonumber (-1)
        Lua.pop 1
        pure ((\(Lua.Number v) → realToFrac v) ⊚ value)

-- | world.setLocationEncounterOccupantState(instanceId, uid, engaged,
--   returning [, pageId]) → bool.
--   The whole state row is replaced atomically on the world thread.
worldSetLocationEncounterOccupantStateFn
    ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetLocationEncounterOccupantStateFn wsc = do
    idArg ← Lua.tointeger 1
    uidArg ← Lua.tointeger 2
    engaged ← Lua.toboolean 3
    returning ← Lua.toboolean 4
    pageArg ← Lua.tostring 5
    queued ← case (idArg, uidArg) of
        (Just rawId, Just rawUid) | rawId ≥ 0 ∧ rawUid ≥ 0 → Lua.liftIO $ do
            mPid ← targetPage wsc pageArg
            case mPid of
                Nothing → pure False
                Just pid → do
                    Q.writeQueue (wsWorldQueue wsc) $
                        WorldSetLocationEncounterOccupantState pid
                            (LocationInstanceId (fromIntegral rawId))
                            (UnitId (fromIntegral rawUid)) engaged returning
                    pure True
        _ → pure False
    Lua.pushboolean queued
    return 1

-- | world.setLocationEncounterEpisodeState(instanceId, active,
--   aggressionAnnounced, disengageAnnounced [, pageId]) → bool. Episode
--   feedback belongs to the encounter, not to each participating occupant.
worldSetLocationEncounterEpisodeStateFn
    ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetLocationEncounterEpisodeStateFn wsc = do
    idArg ← Lua.tointeger 1
    active ← Lua.toboolean 2
    aggressionAnnounced ← Lua.toboolean 3
    disengageAnnounced ← Lua.toboolean 4
    pageArg ← Lua.tostring 5
    queued ← case idArg of
        Just rawId | rawId ≥ 0 → Lua.liftIO $ do
            mPid ← targetPage wsc pageArg
            case mPid of
                Nothing → pure False
                Just pid → do
                    Q.writeQueue (wsWorldQueue wsc) $
                        WorldSetLocationEncounterEpisodeState pid
                            (LocationInstanceId (fromIntegral rawId)) active
                            aggressionAnnounced disengageAnnounced
                    pure True
        _ → pure False
    Lua.pushboolean queued
    return 1

-- | world.setLocationLifecycle(instanceId, lifecycle [, pageId]) → bool
--   (#911). @lifecycle@ is one of "unknown" / "hinted" / "discovered" /
--   "active" / "cleared" / "depleted". Returns whether the request was
--   ACCEPTED for queueing (a known state name and a resolvable page) —
--   the world thread then applies it only if it moves the instance
--   strictly forward, so a backward or same-state request changes
--   nothing. Poll @world.getLocationInstance@ for the settled state.
--   Encounters now drive @active@/@cleared@ themselves; this explicit
--   editor remains for debug and later reward/retrieval lifecycle work.
worldSetLocationLifecycleFn
    ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetLocationLifecycleFn wsc = do
    idArg    ← Lua.tointeger 1
    stateArg ← Lua.tostring 2
    pageArg  ← Lua.tostring 3
    let mLifecycle =
            lifecycleFromName . TE.decodeUtf8Lenient =<< stateArg
    case (idArg, mLifecycle) of
        (Just rawId, Just lifecycle) → do
            queued ← Lua.liftIO $ do
                mPid ← targetPage wsc pageArg
                case mPid of
                    Nothing → pure False
                    Just pid → do
                        Q.writeQueue (wsWorldQueue wsc) $
                            WorldSetLocationLifecycle pid
                                (LocationInstanceId (fromIntegral rawId))
                                lifecycle
                        pure True
            Lua.pushboolean queued
            return 1
        _ → Lua.pushboolean False >> return 1

-- | world.markLocationStamped(gx, gy [, pageId [, fromToken, toToken]])
--   — one-time geometry-stamp flag (#424). An explicit pageId targets
--   that live page (even hidden); omitted defaults to the active world.
--   No-op (queues nothing) when neither resolves to a live page.
--
--   @fromToken@/@toToken@ are a pair of @structure.stageWatermark@ reads
--   taken either side of the builder run this marker is completing
--   (#2051). They name the placement attempts that run ACCEPTED, so the
--   world thread can withhold the marker when one of them was later
--   declined — @structure.place@ returning true means staged and queued,
--   not committed. Supplying only one of the two, or a pair that is not
--   a forward range, carries NO window: the pair is an all-or-nothing
--   claim, and a half-stated one must not silently read as "nothing to
--   check" when the caller believed it had asked for the check. An
--   EMPTY range (@fromToken == toToken@) is a real, well-formed window
--   — a builder that accepted no placements has nothing that can have
--   been declined — so it is carried, not discarded.
worldMarkLocationStampedFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldMarkLocationStampedFn wsc = do
    gxArg   ← Lua.tointeger 1
    gyArg   ← Lua.tointeger 2
    pageArg ← Lua.tostring 3
    fromArg ← Lua.tointeger 4
    toArg   ← Lua.tointeger 5
    let mWindow = case (fromArg, toArg) of
            (Just lo, Just hi) | lo ≥ 0, hi ≥ lo →
                Just (StructureCommitWindow
                        (StructureStageToken (fromIntegral lo))
                        (StructureStageToken (fromIntegral hi)))
            _ → Nothing
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            Lua.liftIO $ do
                mPid ← case pageArg of
                    Just pidBS → pure (Just (WorldPageId (TE.decodeUtf8Lenient pidBS)))
                    Nothing    → (fmap fst) <$> activeWorldPageFrom (wsWorldManagerRef wsc)
                case mPid of
                    Just pid → Q.writeQueue (wsWorldQueue wsc) $
                        WorldMarkLocationStamped pid
                            (fromIntegral gx) (fromIntegral gy) mWindow
                    Nothing  → pure ()
            return 0
        _ → return 0

-- | world.addTile(pageId, gx, gy, material) → bool
--   Raise the column at (gx, gy) one z of the named material (string
--   name or numeric id). Queued onto the world thread; lands in the
--   edit log via WeAddTile, so it persists like any player edit.
--   Debug terrain placement. Returns false when the material can't
--   be resolved.
worldAddTileFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldAddTileFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    -- 4: material name (string) or id (number).
    matName ← Lua.tostring 4
    matNum  ← Lua.tonumber 4
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → do
            registry ← Lua.liftIO $ readIORef (wsMaterialRegistryRef wsc)
            let mMat = case (matNum, matName) of
                    (Just (Lua.Number n), _) | n ≥ 1 ∧ n ≤ 255 →
                        Just (MaterialId (round n))
                    (_, Just nameBS) →
                        materialIdByName registry
                            (TE.decodeUtf8Lenient nameBS)
                    _ → Nothing
            case mMat of
                Nothing → Lua.pushboolean False >> return 1
                Just mat → do
                    let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                    Lua.liftIO $ Q.writeQueue (wsWorldQueue wsc) $
                        WorldAddTile pageId (round gx) (round gy) mat
                    Lua.pushboolean True
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | world.digTile(pageId, gx, gy, ux, uy, amount, minerSkill,
--   perception) —
--   apply dig progress to the designated tile. (ux, uy) is the
--   digger's tile-space position (drain order); amount is pre-scaled
--   by tool × material speed (see getDigInfoAt). minerSkill (the
--   current digger's mining skill; optional, defaults 0) scales the
--   per-tick chunk-yield fill — pass it every tick so a mid-dig
--   handoff uses the new digger's rate.
worldDigTileFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldDigTileFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    uxArg ← Lua.tonumber 4
    uyArg ← Lua.tonumber 5
    amtArg ← Lua.tonumber 6
    skillArg ← Lua.tonumber 7
    percepArg ← Lua.tonumber 8
    case (pageIdArg, gxArg, gyArg, uxArg, uyArg, amtArg) of
        (Just pageIdBS, Just gx, Just gy, Just ux, Just uy, Just amt) →
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                    skill = case skillArg of
                        Just (Lua.Number s) → realToFrac s
                        _                   → 0
                    percep = case percepArg of
                        Just (Lua.Number s) → realToFrac s
                        _                   → 1.0
                Q.writeQueue (wsWorldQueue wsc) $
                    WorldDigTile pageId (round gx) (round gy)
                                 (realToFrac ux) (realToFrac uy)
                                 (realToFrac amt) skill percep
        _ → pure ()
    return 0

-- | world.deleteTile(pageId, gx, gy) → bool
-- Enqueues a dig-1-Z-down edit at the given tile. The actual mutation
-- happens on the next world-thread tick, so this returns true once
-- enqueued (not once applied).
worldDeleteTileFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldDeleteTileFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → do
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                Q.writeQueue (wsWorldQueue wsc)
                    (WorldDeleteTile pageId (fromIntegral gx) (fromIntegral gy))
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | world.setFluidTile(pageId, gx, gy, kind) → bool
-- Places one tile of fluid on top of the column at (gx, gy). `kind` is
-- one of "water" (Lake) / "lava" (Lava) / "river" (River) / "ocean"
-- (Ocean); unknown values fall back to "water". Debug-tool affordance:
-- lets the arena have water sources without waiting for procedural
-- generation.
worldSetFluidTileFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetFluidTileFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    kindArg   ← Lua.tostring 4
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → do
            let fluidType = case kindArg of
                    Just kBS → case TE.decodeUtf8Lenient kBS of
                        "lava"  → Lava
                        "river" → River
                        "ocean" → Ocean
                        _       → Lake     -- "water" / default
                    Nothing → Lake
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                Q.writeQueue (wsWorldQueue wsc) $
                    WorldSetFluidTile pageId
                        (fromIntegral gx) (fromIntegral gy) fluidType
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | world.setCell(pageId, gx, gy, z, material) → bool
--   Set the single 3D cell at (gx,gy,z) to a material — the locations
--   primitive for carving interior air, walls, ceilings, staircases.
--   `material` is a string name, a numeric id, or "air"/0 to clear the
--   cell. Queued onto the world thread; lands in the edit log via
--   WeSetCell so it persists like any player edit. Grows the column up
--   to reach z; a z below the column floor is dropped (warns). Returns
--   false when the material can't be resolved.
worldSetCellFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetCellFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    zArg      ← Lua.tointeger 4
    -- 5: material name (string), numeric id, or "air"/0 for air.
    matName ← Lua.tostring 5
    matNum  ← Lua.tonumber 5
    case (pageIdArg, gxArg, gyArg, zArg) of
        (Just pageIdBS, Just gx, Just gy, Just z) → do
            registry ← Lua.liftIO $ readIORef (wsMaterialRegistryRef wsc)
            let mMat = case (matNum, matName) of
                    (Just (Lua.Number n), _) | n ≥ 0 ∧ n ≤ 255 →
                        Just (MaterialId (round n))
                    (_, Just nameBS) → case TE.decodeUtf8Lenient nameBS of
                        "air" → Just (MaterialId 0)
                        name  → materialIdByName registry name
                    _ → Nothing
            case mMat of
                Nothing → Lua.pushboolean False >> return 1
                Just mat → do
                    let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                    Lua.liftIO $ Q.writeQueue (wsWorldQueue wsc) $
                        WorldSetCell pageId
                            (fromIntegral gx) (fromIntegral gy)
                            (fromIntegral z) mat
                    Lua.pushboolean True
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | world.setSlope(pageId, gx, gy, z, bits) → bool
--   Set the walkable-ramp slope bitmask of the tile at (gx,gy,z).
--   Bits: 0=N 1=E 2=S 3=W; a set bit marks that cardinal neighbour as a
--   1-z ramp down (so a unit can walk up it instead of climbing). addTile
--   only ever makes flat tops (slope 0 = cliff), so this is the only way
--   to author a walkable ramp — exists for the movement test harness.
worldSetSlopeFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetSlopeFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    zArg      ← Lua.tointeger 4
    bitsArg   ← Lua.tointeger 5
    case (pageIdArg, gxArg, gyArg, zArg, bitsArg) of
        (Just pageIdBS, Just gx, Just gy, Just z, Just bits) → do
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                Q.writeQueue (wsWorldQueue wsc) $
                    WorldSetSlope pageId
                        (fromIntegral gx) (fromIntegral gy)
                        (fromIntegral z)
                        (fromIntegral bits)  -- → Word8 truncates to low 8 bits
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | world.plantRowCropAt(pageId, gx, gy, cropName) → bool
--   Plant a single row-crop FloraInstance at (gx,gy) via the WePlaceFlora
--   edit path (queued, same fire-and-forget shape as world.setVegAt) —
--   the farm AI's (#336) row-crop planting completion, the FloraInstance
--   counterpart to world.plantCropAt's CropPlot for groundcover crops.
--   Refused world-thread-side unless the tile is tilled soil and
--   cropName names a registered row_crop species; poll
--   world.getFloraGrowthAt afterward to confirm it landed.
worldPlantRowCropAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldPlantRowCropAtFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    cropArg   ← Lua.tostring 4
    case (pageIdArg, gxArg, gyArg, cropArg) of
        (Just pageIdBS, Just gx, Just gy, Just cropBS) → do
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                Q.writeQueue (wsWorldQueue wsc) $
                    WorldPlantRowCropAt pageId
                        (fromIntegral gx) (fromIntegral gy)
                        (TE.decodeUtf8Lenient cropBS)
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | world.setVegAt(pageId, gx, gy, z, vegId) → bool
--   Set the vegetation id of the tile at (gx,gy,z). Mirrors
--   world.setSlope's shape and edit-log routing — the till AI's (#333)
--   completion primitive: flips a tilled tile's ground cover to
--   'World.Vegetation.vegTilledSoil' so it survives chunk eviction +
--   save/load like every other edit. No generator path emits arbitrary
--   ids here (computeChunkVegetation owns natural placement).
worldSetVegFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
worldSetVegFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tointeger 2
    gyArg     ← Lua.tointeger 3
    zArg      ← Lua.tointeger 4
    vegIdArg  ← Lua.tointeger 5
    case (pageIdArg, gxArg, gyArg, zArg, vegIdArg) of
        (Just pageIdBS, Just gx, Just gy, Just z, Just vegId) → do
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                Q.writeQueue (wsWorldQueue wsc) $
                    WorldSetVeg pageId
                        (fromIntegral gx) (fromIntegral gy)
                        (fromIntegral z)
                        (fromIntegral vegId)  -- → Word8 truncates to low 8 bits
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1
