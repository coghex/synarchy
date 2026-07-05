{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Buildings
    ( loadBuildingYamlFn
    , buildingSpawnFn
    , buildingDestroyFn
    , buildingCanPlaceAtFn
    , buildingSetGhostFn
    , buildingClearGhostFn
    , buildingGetStartingBuildingsFn
    , buildingGetInfoFn
    , buildingGetActivityFn
    , buildingListFn
    , buildingGetActiveIdsFn
    , buildingListDefsFn
    , buildingHitTestAtFn
    , buildingSelectFn
    , buildingDeselectFn
    , buildingGetSelectedFn
    , buildingSetSpawnRemainingFn
    , buildingGetSpawnRemainingFn
    , buildingConsumeSpawnFn
    , buildingGetBuildProgressFn
    , buildingGetBuildRequiredFn
    , buildingAddBuildProgressFn
    , buildingGetMaterialNeedFn
    , buildingGetMaterialDeliveredFn
    , buildingAreMaterialsSatisfiedFn
    , buildingGetStorageFn
    , buildingGetStorageCapacityFn
    , buildingGetStorageWeightFn
    , buildingGetOperationsFn
    , buildingFindStationFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import World.Page.Types (WorldPageId(..))
import Engine.Core.Log (LogCategory(..), logInfo, logDebug)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister, resolveTexturePath)
import Engine.Asset.YamlBuildings (BuildingYamlDef(..), BuildingYamlAnim(..),
                                   BuildingYamlTileSize(..), loadBuildingYaml)
import qualified Engine.Core.Queue as Q
import Building.Types
import Building.Command.Types (BuildingCommand(..))
import Engine.Asset.Handle (TextureHandle(..))
import Item.Types (itemTotalWeight)
import Engine.Scripting.Lua.API.Equipment (pushItemInstance)
import Building.Placement (canPlaceAt, PlacementResult(..))
import Building.HitTest (hitTestBuildingAt)
import Unit.Direction (Direction(..))
import Unit.Types (Animation(..))
import Unit.Pathing.Cost (lookupTerrainZ)
import World.Types (WorldManager(..), WorldState(..))
import World.Tile.Types (WorldTileData)

-- * YAML loading

loadBuildingYamlFn ∷ EngineEnv → LuaBackendState
                   → Lua.LuaE Lua.Exception Lua.NumResults
loadBuildingYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadBuildingYaml logger filePath

                let (lteq, _) = lbsMsgQueues backendState

                total ← foldM (\acc def → do
                    let name      = bydName def
                        spritePath = T.unpack (bydSprite def)
                        unknownBuilding = "assets/textures/buildings/unknown_building.png"

                    resolvedSprite ← resolveTexturePath env "Building sprite"
                                          unknownBuilding spritePath
                    handle ← loadAndRegister env backendState lteq
                                 ("building_" <> name) resolvedSprite

                    -- Build animations: frame textures are loaded via
                    -- the same loader. We only key by the single
                    -- direction "default" (mapped to DirS internally).
                    animMap ← foldM (\accA (animName, animDef) → do
                        frameMap ← foldM (\accF (_dirKey, framePaths) → do
                            handles ← mapM (\(i, p) → do
                                resolved ← resolveTexturePath env "Building animation frame"
                                               unknownBuilding (T.unpack p)
                                loadAndRegister env backendState lteq
                                    ("building_" <> name
                                     <> "_" <> animName
                                     <> "_" <> T.pack (show i))
                                    resolved
                                ) (zip [(0 ∷ Int)..] framePaths)
                            return (Map.insert DirS
                                      (V.fromList handles) accF)
                            ) Map.empty (Map.toList (byaFrames animDef))
                        let anim = Animation
                                { aFps    = byaFps animDef
                                , aLoop   = byaLoop animDef
                                , aFlip   = False  -- buildings have a
                                                   -- single direction
                                                   -- (DirS); the mirror
                                                   -- path never fires
                                                   -- for them.
                                , aFrames = frameMap
                                }
                        return (HM.insert animName anim accA)
                        ) HM.empty (Map.toList (bydAnimations def))

                    let stateAnims = HM.fromList (Map.toList (bydStateAnims def))

                    -- Default display_name to the raw name if YAML
                    -- didn't supply one — keeps older defs renderable
                    -- in the build menu without forcing a YAML edit.
                    let displayName = if T.null (bydDisplayName def)
                                      then name
                                      else bydDisplayName def
                    let bdef = BuildingDef
                            { bdName            = name
                            , bdDisplayName     = displayName
                            , bdCategory        = bydCategory def
                            , bdDescription     = bydDescription def
                            , bdTexture         = handle
                            , bdTileW           = bytsX (bydTileSize def)
                            , bdTileH           = bytsY (bydTileSize def)
                            , bdPlacement       = bydPlacement def
                            , bdIsStarting      = bydIsStarting def
                            , bdRace            = bydRace def
                            , bdSpriteAnchor    = bydSpriteAnchor def
                            , bdBuildWork       = bydBuildWork def
                            , bdMaterials       = HM.fromList (Map.toList (bydMaterials def))
                            , bdStorageCapacity = bydStorageCapacity def
                            , bdOperations      = bydOperations def
                            , bdAnimations      = animMap
                            , bdStateAnims      = stateAnims
                            , bdRequiresPower   = bydRequiresPower def
                            , bdPowerDrain      = bydPowerDrain def
                            }
                    atomicModifyIORef' (buildingManagerRef env) $ \bm →
                        (bm { bmDefs = HM.insert name bdef (bmDefs bm) }, ())

                    logDebug logger CatAsset $
                        "Registered building def: " <> name
                        <> " (" <> T.pack (show (HM.size animMap))
                        <> " animations, " <> T.pack (show (bytsX (bydTileSize def)))
                        <> "x" <> T.pack (show (bytsY (bydTileSize def))) <> ")"

                    return (acc + 1)
                    ) (0 ∷ Int) defs

                logInfo logger CatAsset $
                    "loadBuildingYaml: loaded " <> T.pack (show total)
                    <> " building definitions from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

-- * Spawn / destroy

-- | building.spawn(defName, gx, gy [, pageId]) — returns the new
--   building id on success, nil otherwise (unknown def, placement
--   invalid). Placement is validated server-side too so Lua scripts
--   can't accidentally place into water etc. An explicit pageId (slot
--   4) pins the spawn — AND the occupancy/terrain-Z check — to that
--   live page (even hidden) instead of the active world: location
--   content-spawning (#90) passes its own page so a building lands (and
--   validates) on the page its location is on, not whichever happens to
--   be visible. Omitted → the active world, as before (#76).
buildingSpawnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingSpawnFn env = do
    nameArg ← Lua.tostring 1
    xArg    ← Lua.tointeger 2
    yArg    ← Lua.tointeger 3
    pageArg ← Lua.tostring 4
    case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → do
            let defName = TE.decodeUtf8 nameBS
                gx      = fromIntegral x
                gy      = fromIntegral y
            mBid ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                mTarget ← case pageArg of
                    Just pidBS → do
                        let pid = WorldPageId (TE.decodeUtf8 pidBS)
                        wm ← readIORef (worldManagerRef env)
                        pure $ (\ws → (pid, ws)) <$> lookup pid (wmWorlds wm)
                    Nothing → activeWorldPage env
                case (HM.lookup defName (bmDefs bm), mTarget) of
                    (Just def, Just (pid, ws)) → do
                        wtd ← readIORef (wsTilesRef ws)
                        case canPlaceAt
                                (bm { bmInstances =
                                        buildingsOnPage pid (bmInstances bm) })
                                wtd def gx gy of
                            NotPlaceable _ → pure Nothing
                            Placeable → do
                                let gz = floorZAt wtd gx gy
                                bid ← atomicModifyIORef'
                                        (buildingManagerRef env) $ \bm' →
                                            let (bid', bm'') = nextBuildingId bm'
                                            in (bm'', bid')
                                Q.writeQueue (buildingQueue env) $
                                    BuildingSpawn bid defName gx gy gz pid
                                pure (Just bid)
                    _ → pure Nothing
            case mBid of
                Just (BuildingId n) → do
                    Lua.pushinteger (fromIntegral n)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

buildingDestroyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingDestroyFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (buildingQueue env) $ BuildingDestroy bid
            Lua.pushboolean True
            return 1

-- * Placement check

-- | building.canPlaceAt(defName, gx, gy) — returns @(true, nil)@ on
--   success or @(false, reason)@ on rejection. Cheap to call every
--   frame from the build tool's ghost preview update.
buildingCanPlaceAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingCanPlaceAtFn env = do
    nameArg ← Lua.tostring 1
    xArg    ← Lua.tointeger 2
    yArg    ← Lua.tointeger 3
    case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → do
            let defName = TE.decodeUtf8 nameBS
                gx      = fromIntegral x
                gy      = fromIntegral y
            result ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                mActive ← activeWorldPage env
                -- Occupancy is checked only against the ACTIVE world's
                -- buildings — a building in another world must not block
                -- placement here (#76).
                case (HM.lookup defName (bmDefs bm), mActive) of
                    (Nothing, _) → pure (NotPlaceable "unknown building")
                    (_, Nothing) → pure (NotPlaceable "no active world")
                    (Just def, Just (pid, _)) → do
                        mWtd ← snapshotVisibleWorldTiles env
                        case mWtd of
                            Nothing  → pure (NotPlaceable "no world loaded")
                            Just wtd → pure (canPlaceAt
                                (bm { bmInstances =
                                        buildingsOnPage pid (bmInstances bm) })
                                wtd def gx gy)
            case result of
                Placeable → do
                    Lua.pushboolean True
                    Lua.pushnil
                    return 2
                NotPlaceable reason → do
                    Lua.pushboolean False
                    Lua.pushstring (TE.encodeUtf8 reason)
                    return 2
        _ → do
            Lua.pushboolean False
            Lua.pushstring "bad arguments"
            return 2

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
            let name = TE.decodeUtf8 nameBS
                gx   = fromIntegral x
                gy   = fromIntegral y
            Lua.liftIO $ do
                -- Sample the terrain Z at the ghost tile so the
                -- render pass can elevate the preview to where the
                -- building will actually land. Matches the spawn
                -- path's `floorZAt`; falls back to 0 if the chunk
                -- isn't loaded.
                gz ← do
                    mWtd ← snapshotVisibleWorldTiles env
                    case mWtd of
                        Just wtd → pure (floorZAt wtd gx gy)
                        Nothing  → pure 0
                writeIORef (buildingGhostRef env) $ Just BuildingGhost
                    { bgDefName = name
                    , bgGridX   = gx
                    , bgGridY   = gy
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
    Lua.liftIO $ writeIORef (buildingGhostRef env) Nothing
    Lua.pushboolean True
    return 1

-- * Queries

-- | building.getStartingBuildings() — Lua array of def names where
--   is_starting is true. Used by the build tool's popup.
buildingGetStartingBuildingsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetStartingBuildingsFn env = do
    names ← Lua.liftIO $ do
        bm ← readIORef (buildingManagerRef env)
        pure [ n | (n, d) ← HM.toList (bmDefs bm), bdIsStarting d ]
    Lua.newtable
    forM_ (zip [1..] names) $ \(i, name) → do
        Lua.pushstring (TE.encodeUtf8 name)
        Lua.rawseti (-2) i
    return 1

-- | building.getInfo(id) — returns a Lua table with the building's
--   attributes, or nil if missing.
buildingGetInfoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetInfoFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mInst ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure (HM.lookup bid (bmInstances bm))
            case mInst of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just inst → do
                    -- Resolve def once for display fields that come
                    -- from the def rather than the instance.
                    mDef ← Lua.liftIO $ do
                        bm ← readIORef (buildingManagerRef env)
                        pure (HM.lookup (biDefName inst) (bmDefs bm))
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (biDefName inst))
                    Lua.setfield (-2) "defName"
                    Lua.pushstring (TE.encodeUtf8 $ case mDef of
                        Just d  → bdDisplayName d
                        Nothing → biDefName inst)
                    Lua.setfield (-2) "displayName"
                    Lua.pushinteger (fromIntegral (biAnchorX inst))
                    Lua.setfield (-2) "gridX"
                    Lua.pushinteger (fromIntegral (biAnchorY inst))
                    Lua.setfield (-2) "gridY"
                    Lua.pushinteger (fromIntegral (biGridZ inst))
                    Lua.setfield (-2) "gridZ"
                    Lua.pushinteger (fromIntegral (biTileW inst))
                    Lua.setfield (-2) "tileW"
                    Lua.pushinteger (fromIntegral (biTileH inst))
                    Lua.setfield (-2) "tileH"
                    Lua.pushnumber (Lua.Number (realToFrac (biSpawnedAt inst)))
                    Lua.setfield (-2) "spawnedAt"
                    -- The world page this building lives on (#76). Lets a
                    -- caller bind follow-up actions (e.g. unit.spawn) to the
                    -- building's own page instead of the active page (#196).
                    Lua.pushstring (TE.encodeUtf8 (case biPage inst of
                        WorldPageId p → p))
                    Lua.setfield (-2) "page"
                    return 1

-- | building.setSpawnRemaining(bid, n) — initialize the spawn-roster
--   countdown on a building. Called once by Lua spawn sequencers when
--   they first see a built building. Engine-owned because the value
--   needs to survive save/load and chunk eviction without a Lua
--   serializer (Phase 5 work).
buildingSetSpawnRemainingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingSetSpawnRemainingFn env = do
    idArg ← Lua.tointeger 1
    nArg  ← Lua.tointeger 2
    case (idArg, nArg) of
        (Just n, Just count) → do
            let bid = BuildingId (fromIntegral n)
                rem = max 0 (fromIntegral count)
            Lua.liftIO $ atomicModifyIORef' (buildingManagerRef env) $ \bm →
                case HM.lookup bid (bmInstances bm) of
                    Nothing → (bm, ())
                    Just inst →
                        let inst' = inst { biSpawnRemaining = rem }
                        in (bm { bmInstances = HM.insert bid inst' (bmInstances bm) }, ())
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | building.getSpawnRemaining(bid) → Int. Number of units still to
--   spawn from this building's roster. 0 = done (or building has no
--   spawn sequencer attached). nil if the bid doesn't exist.
buildingGetSpawnRemainingFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetSpawnRemainingFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mRem ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure (biSpawnRemaining <$> HM.lookup bid (bmInstances bm))
            case mRem of
                Just r → do
                    Lua.pushinteger (fromIntegral r)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.consumeSpawn(bid) → Int (new remaining). Decrement by 1,
--   clamped at 0. Returns the value AFTER the decrement so the caller
--   can branch on "still more to spawn" cheaply.
buildingConsumeSpawnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingConsumeSpawnFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mRem ← Lua.liftIO $ atomicModifyIORef' (buildingManagerRef env) $ \bm →
                case HM.lookup bid (bmInstances bm) of
                    Nothing → (bm, Nothing)
                    Just inst →
                        let newRem = max 0 (biSpawnRemaining inst - 1)
                            inst'  = inst { biSpawnRemaining = newRem }
                        in (bm { bmInstances = HM.insert bid inst' (bmInstances bm) }
                           , Just newRem)
            case mRem of
                Just r → do
                    Lua.pushinteger (fromIntegral r)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.getBuildProgress(bid) → Float (accumulated worker-seconds).
--   nil if the bid doesn't exist.
buildingGetBuildProgressFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetBuildProgressFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mProg ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure (biBuildProgress <$> HM.lookup bid (bmInstances bm))
            case mProg of
                Just p → do
                    Lua.pushnumber (Lua.Number (realToFrac p))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.getBuildRequired(bid) → Float (bdBuildWork from the def).
--   0 means instant-build (legacy time-based, no worker assignment).
--   nil if the bid doesn't exist or its def is missing.
buildingGetBuildRequiredFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetBuildRequiredFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mReq ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure $ do
                    inst ← HM.lookup bid (bmInstances bm)
                    def  ← HM.lookup (biDefName inst) (bmDefs bm)
                    pure (bdBuildWork def)
            case mReq of
                Just w → do
                    Lua.pushnumber (Lua.Number (realToFrac w))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.addBuildProgress(bid, delta) → Float (new progress) | nil.
--   Clamps the result at [0, ∞). Currently called by the Lua
--   construction tick once per frame with delta = R(workers) * dt.
buildingAddBuildProgressFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingAddBuildProgressFn env = do
    idArg    ← Lua.tointeger 1
    deltaArg ← Lua.tonumber   2
    case (idArg, deltaArg) of
        (Just n, Just (Lua.Number d)) → do
            let bid   = BuildingId (fromIntegral n)
                delta = realToFrac d ∷ Float
            mNew ← Lua.liftIO $ atomicModifyIORef' (buildingManagerRef env) $ \bm →
                case HM.lookup bid (bmInstances bm) of
                    Nothing → (bm, Nothing)
                    Just inst →
                        let newProg = max 0 (biBuildProgress inst + delta)
                            inst'   = inst { biBuildProgress = newProg }
                        in (bm { bmInstances = HM.insert bid inst' (bmInstances bm) }
                           , Just newProg)
            case mNew of
                Just p → do
                    Lua.pushnumber (Lua.Number (realToFrac p))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | building.getMaterialNeed(bid) → table {[defName] = count}. The
--   def-declared requirement map. nil if the bid or its def is gone.
buildingGetMaterialNeedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetMaterialNeedFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mMat ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure $ do
                    inst ← HM.lookup bid (bmInstances bm)
                    def  ← HM.lookup (biDefName inst) (bmDefs bm)
                    pure (bdMaterials def)
            case mMat of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just mats → do
                    Lua.newtable
                    forM_ (HM.toList mats) $ \(name, count) → do
                        Lua.pushinteger (fromIntegral count)
                        Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 name))
                    return 1

-- | building.getMaterialDelivered(bid) → table {[defName] = count}.
--   Just the count per type — the actual ItemInstances stay engine-
--   side until deconstruction recovery surfaces them. nil if the bid
--   doesn't exist.
buildingGetMaterialDeliveredFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetMaterialDeliveredFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mMap ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure (biMaterialsDelivered <$> HM.lookup bid (bmInstances bm))
            case mMap of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just mats → do
                    Lua.newtable
                    forM_ (HM.toList mats) $ \(name, items) → do
                        Lua.pushinteger (fromIntegral (length items))
                        Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 name))
                    return 1

-- | building.areMaterialsSatisfied(bid) → bool. True iff for every
--   material type in bdMaterials the delivered count meets the need.
--   Empty bdMaterials (default for legacy defs like the portal)
--   trivially satisfies. nil if the bid is gone.
buildingAreMaterialsSatisfiedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingAreMaterialsSatisfiedFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mOk ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure $ do
                    inst ← HM.lookup bid (bmInstances bm)
                    def  ← HM.lookup (biDefName inst) (bmDefs bm)
                    let need = bdMaterials def
                        delivered = biMaterialsDelivered inst
                    pure $ all (\(t, n') →
                                  length (HM.lookupDefault [] t delivered) >= n')
                               (HM.toList need)
            case mOk of
                Just ok → do
                    Lua.pushboolean ok
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.getStorage(bid) → array of item-instance tables (same
--   shape as unit.getInventory entries: defName, displayName, weight,
--   quality / condition when applicable, iconTex, category, …). nil
--   if the bid is unknown. Empty array for buildings with storage
--   capacity but nothing deposited yet.
buildingGetStorageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetStorageFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mInst ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure (HM.lookup bid (bmInstances bm))
            case mInst of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just inst → do
                    itemMgr ← Lua.liftIO $ readIORef (itemManagerRef env)
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] (biStorage inst)) $ \(i, item) → do
                        Lua.newtable
                        pushItemInstance item itemMgr
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1

-- | building.getStorageCapacity(bid) → Float kg (def-declared cap).
--   0 means "no storage". nil if the bid is gone.
buildingGetStorageCapacityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetStorageCapacityFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mCap ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure $ do
                    inst ← HM.lookup bid (bmInstances bm)
                    def  ← HM.lookup (biDefName inst) (bmDefs bm)
                    pure (bdStorageCapacity def)
            case mCap of
                Just c → do
                    Lua.pushnumber (Lua.Number (realToFrac c))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.getStorageWeight(bid) → Float kg currently stored. Sums
--   the full weight of each ItemInstance in biStorage via
--   'itemTotalWeight' (instance weight + container fill + nested
--   contents) — so a stored canteen counts at its filled weight and a
--   first-aid kit counts its contents, matching the unit-side
--   'unit.getCarryingWeight' convention and the depositToCargo capacity
--   gate. nil if the bid is gone.
buildingGetStorageWeightFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetStorageWeightFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mW ← Lua.liftIO $ do
                bm      ← readIORef (buildingManagerRef env)
                itemMgr ← readIORef (itemManagerRef env)
                pure $ do
                    inst ← HM.lookup bid (bmInstances bm)
                    pure $ sum (map (itemTotalWeight itemMgr) (biStorage inst))
            case mW of
                Just w → do
                    Lua.pushnumber (Lua.Number (realToFrac w))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | building.getOperations(bid) → array of operation tags from the
--   def ("smelt", "forge", "assemble", "repair_condition",
--   "repair_sharpness", …). Empty array for
--   buildings that aren't work stations. nil if the bid or its def is
--   gone. Def-level data (#326) — what the station CAN do; whether it
--   currently can (Built) is getActivity's business.
buildingGetOperationsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetOperationsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mOps ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure $ do
                    inst ← HM.lookup bid (bmInstances bm)
                    def  ← HM.lookup (biDefName inst) (bmDefs bm)
                    pure (bdOperations def)
            case mOps of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just ops → do
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] ops) $ \(i, op) → do
                        Lua.pushstring (TE.encodeUtf8 op)
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1

-- | building.findStation(op [, gx, gy]) → bid, gridX, gridY | nil.
--   Nearest BUILT building on the ACTIVE world page whose def offers
--   the operation (#326). Distance is Chebyshev from (gx, gy) to the
--   closest footprint tile; omit the coords to get the lowest-id
--   match. Ties break to the lowest id so results are deterministic
--   (bmInstances is a HashMap). Ghost/under-construction stations
--   never match — an unbuilt furnace can't smelt.
buildingFindStationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingFindStationFn env = do
    opArg ← Lua.tostring 1
    gxArg ← Lua.tointeger 2
    gyArg ← Lua.tointeger 3
    case opArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just opBS → do
            let op = TE.decodeUtf8 opBS
                mFrom = case (gxArg, gyArg) of
                    (Just x, Just y) → Just (fromIntegral x ∷ Int,
                                             fromIntegral y ∷ Int)
                    _                → Nothing
            mBest ← Lua.liftIO $ do
                bm      ← readIORef (buildingManagerRef env)
                now     ← readIORef (gameTimeRef env)
                mActive ← activeWorldPage env
                pure $ case mActive of
                    Nothing → Nothing
                    Just (pid, _) →
                        let candidates =
                              [ (bid, inst)
                              | (bid, inst) ← HM.toList
                                    (buildingsOnPage pid (bmInstances bm))
                              , Just def ← [HM.lookup (biDefName inst)
                                                      (bmDefs bm)]
                              , op `elem` bdOperations def
                              , currentActivity now inst def ≡ Built ]
                            rank (bid, inst) =
                              ( maybe 0 (footprintDist inst) mFrom
                              , unBuildingId bid )
                        in case candidates of
                            [] → Nothing
                            cs → Just (minimumBy
                                    (comparing rank) cs)
            case mBest of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just (BuildingId n, inst) → do
                    Lua.pushinteger (fromIntegral n)
                    Lua.pushinteger (fromIntegral (biAnchorX inst))
                    Lua.pushinteger (fromIntegral (biAnchorY inst))
                    return 3

-- | building.getActivity(id) — returns "appearing" while the appear
--   animation is still playing, "built" afterwards. nil if the
--   building doesn't exist. Computed from elapsed time vs the def's
--   appearing anim duration (no stored state to query).
buildingGetActivityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetActivityFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mLabel ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                case HM.lookup bid (bmInstances bm) of
                    Nothing → pure Nothing
                    Just inst → case HM.lookup (biDefName inst) (bmDefs bm) of
                        Nothing  → pure Nothing
                        Just def → do
                            -- Game-clock matches biSpawnedAt, so the
                            -- Appearing→Built transition freezes on
                            -- pause and doesn't drift against POSIX.
                            now ← readIORef (gameTimeRef env)
                            pure $ Just $ case currentActivity now inst def of
                                Appearing → "appearing" ∷ Text
                                Built     → "built"
            case mLabel of
                Just lbl → do
                    Lua.pushstring (TE.encodeUtf8 lbl)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

buildingListFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingListFn env = do
    result ← Lua.liftIO $ do
        bm ← readIORef (buildingManagerRef env)
        let entries = HM.toList (bmInstances bm)
        if null entries
        then return "No buildings placed"
        else return $ T.unpack $ T.intercalate "\n" $
            map (\(bid, inst) →
                "id=" <> T.pack (show (unBuildingId bid))
                <> " " <> biDefName inst
                <> " (" <> T.pack (show (biAnchorX inst))
                <> ", " <> T.pack (show (biAnchorY inst))
                <> ", " <> T.pack (show (biGridZ inst)) <> ")"
            ) entries
    Lua.pushstring (TE.encodeUtf8 (T.pack result))
    return 1

-- | building.getActiveIds() — Lua array of every live building's integer
--   id, scoped to the ACTIVE world page. The world-scoping boundary for
--   per-tick iteration so a building in another live world never leaks
--   into the current one (#198), mirroring unit.getAllIds (#78). Empty
--   when no world is active. Scripts should prefer this over parsing the
--   global, page-agnostic building.list when they iterate gameplay.
buildingGetActiveIdsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetActiveIdsFn env = do
    ids ← Lua.liftIO $ do
        bm ← readIORef (buildingManagerRef env)
        mActive ← activeWorldPage env
        pure $ case mActive of
            Just (pid, _) → HM.keys (buildingsOnPage pid (bmInstances bm))
            Nothing       → []
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] ids) $ \(i, bid) → do
        Lua.pushinteger (fromIntegral (unBuildingId bid))
        Lua.rawseti (-2) (fromIntegral i)
    return 1

-- | building.listDefs() — Lua array of tables, one per registered
--   building def. Each entry: {name, displayName, category, description,
--   iconTex, isStarting}. Used by the build menu to populate icons +
--   tooltips with categorisation.
buildingListDefsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingListDefsFn env = do
    defs ← Lua.liftIO $ do
        bm ← readIORef (buildingManagerRef env)
        return $ HM.elems (bmDefs bm)
    Lua.newtable
    forM_ (zip [1..] defs) $ \(i, d) → do
        Lua.newtable
        Lua.pushstring (TE.encodeUtf8 (bdName d))
        Lua.setfield (-2) "name"
        Lua.pushstring (TE.encodeUtf8 (bdDisplayName d))
        Lua.setfield (-2) "displayName"
        Lua.pushstring (TE.encodeUtf8 (bdCategory d))
        Lua.setfield (-2) "category"
        Lua.pushstring (TE.encodeUtf8 (bdDescription d))
        Lua.setfield (-2) "description"
        let TextureHandle texInt = bdTexture d
        Lua.pushinteger (fromIntegral texInt)
        Lua.setfield (-2) "iconTex"
        Lua.pushboolean (bdIsStarting d)
        Lua.setfield (-2) "isStarting"
        Lua.pushnumber (Lua.Number (realToFrac (bdBuildWork d)))
        Lua.setfield (-2) "buildWork"
        Lua.rawseti (-2) i
    return 1

-- * Helpers

snapshotVisibleWorldTiles ∷ EngineEnv → IO (Maybe WorldTileData)
snapshotVisibleWorldTiles env = do
    wm ← readIORef (worldManagerRef env)
    case wmVisible wm of
        []          → pure Nothing
        (pageId:_)  → case lookup pageId (wmWorlds wm) of
            Nothing → pure Nothing
            Just ws → Just <$> readIORef (wsTilesRef ws)

-- | Terrain Z at the anchor tile. Falls back to 0 if the chunk isn't
--   loaded — shouldn't happen since canPlaceAt already verified, but
--   defensive.
floorZAt ∷ WorldTileData → Int → Int → Int
floorZAt wtd gx gy = case lookupTerrainZ wtd gx gy of
    Just z  → z
    Nothing → 0

-- * Selection

-- | building.hitTestAt(pixelX, pixelY) → id|nil
--   Topmost (highest gridZ) building whose sprite quad contains the
--   click. Pixel coords are framebuffer pixels.
buildingHitTestAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingHitTestAtFn env = do
    xArg ← Lua.tonumber 1
    yArg ← Lua.tonumber 2
    case (xArg, yArg) of
        (Just (Lua.Number x), Just (Lua.Number y)) → do
            mBid ← Lua.liftIO $ hitTestBuildingAt env (realToFrac x) (realToFrac y)
            case mBid of
                Just (BuildingId n) → do
                    Lua.pushinteger (fromIntegral n)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | building.select(id) — single-select; replaces any prior selection.
buildingSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingSelectFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mActive ← Lua.liftIO $ activeWorldPage env
            Lua.liftIO $ atomicModifyIORef' (buildingManagerRef env) $ \bm →
                -- Only select a building of the ACTIVE world (#76) that
                -- still exists; otherwise leave the previous selection.
                case (mActive, HM.lookup bid (bmInstances bm)) of
                    (Just (pid, _), Just bi) | biPage bi ≡ pid →
                        (bm { bmSelected = Just bid }, ())
                    _ → (bm, ())
        Nothing → pure ()
    return 0

-- | building.deselect() — clear any selection.
buildingDeselectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingDeselectFn env = do
    Lua.liftIO $ atomicModifyIORef' (buildingManagerRef env) $ \bm →
        (bm { bmSelected = Nothing }, ())
    return 0

-- | building.getSelected() → id|nil. Validates the selection still
--   exists; returns nil if the building was destroyed.
buildingGetSelectedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetSelectedFn env = do
    mBid ← Lua.liftIO $ do
        bm ← readIORef (buildingManagerRef env)
        mActive ← activeWorldPage env
        pure $ case bmSelected bm of
            Just bid
                | Just bi      ← HM.lookup bid (bmInstances bm)
                , Just (pid,_) ← mActive
                , biPage bi ≡ pid → Just bid
            _ → Nothing
    case mBid of
        Just (BuildingId n) → do
            Lua.pushinteger (fromIntegral n)
            return 1
        Nothing → do
            Lua.pushnil
            return 1
