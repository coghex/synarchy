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
    , buildingListDefsFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import Control.Monad (foldM, forM_)
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo, logDebug, logWarn)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister)
import Engine.Asset.YamlBuildings (BuildingYamlDef(..), BuildingYamlAnim(..),
                                   BuildingYamlTileSize(..), loadBuildingYaml)
import qualified Engine.Core.Queue as Q
import Building.Types
import Building.Command.Types (BuildingCommand(..))
import Building.Placement (canPlaceAt, PlacementResult(..))
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

                    handle ← loadAndRegister env backendState lteq
                                 ("building_" <> name) spritePath

                    -- Build animations: frame textures are loaded via
                    -- the same loader. We only key by the single
                    -- direction "default" (mapped to DirS internally).
                    animMap ← foldM (\accA (animName, animDef) → do
                        frameMap ← foldM (\accF (_dirKey, framePaths) → do
                            handles ← mapM (\(i, p) →
                                loadAndRegister env backendState lteq
                                    ("building_" <> name
                                     <> "_" <> animName
                                     <> "_" <> T.pack (show i))
                                    (T.unpack p)
                                ) (zip [(0 ∷ Int)..] framePaths)
                            return (Map.insert DirS
                                      (V.fromList handles) accF)
                            ) Map.empty (Map.toList (byaFrames animDef))
                        let anim = Animation
                                { aFps    = byaFps animDef
                                , aLoop   = byaLoop animDef
                                , aFrames = frameMap
                                }
                        return (HM.insert animName anim accA)
                        ) HM.empty (Map.toList (bydAnimations def))

                    let stateAnims = HM.fromList (Map.toList (bydStateAnims def))

                    let bdef = BuildingDef
                            { bdName       = name
                            , bdTexture    = handle
                            , bdTileW      = bytsX (bydTileSize def)
                            , bdTileH      = bytsY (bydTileSize def)
                            , bdPlacement  = bydPlacement def
                            , bdIsStarting = bydIsStarting def
                            , bdRace       = bydRace def
                            , bdAnimations = animMap
                            , bdStateAnims = stateAnims
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

-- | building.spawn(defName, gx, gy) — returns the new building id on
--   success, nil otherwise (unknown def, placement invalid). Placement
--   is validated server-side too so Lua scripts can't accidentally
--   place into water etc.
buildingSpawnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingSpawnFn env = do
    nameArg ← Lua.tostring 1
    xArg    ← Lua.tointeger 2
    yArg    ← Lua.tointeger 3
    case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → do
            let defName = TE.decodeUtf8 nameBS
                gx      = fromIntegral x
                gy      = fromIntegral y
            mBid ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                case HM.lookup defName (bmDefs bm) of
                    Nothing  → pure Nothing
                    Just def → do
                        mWtd ← snapshotVisibleWorldTiles env
                        case mWtd of
                            Nothing  → pure Nothing
                            Just wtd → case canPlaceAt bm wtd def gx gy of
                                NotPlaceable _ → pure Nothing
                                Placeable → do
                                    let gz = floorZAt wtd gx gy
                                    bid ← atomicModifyIORef'
                                            (buildingManagerRef env) $ \bm' →
                                                let (bid', bm'') = nextBuildingId bm'
                                                in (bm'', bid')
                                    Q.writeQueue (buildingQueue env) $
                                        BuildingSpawn bid defName gx gy gz
                                    pure (Just bid)
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
                case HM.lookup defName (bmDefs bm) of
                    Nothing  → pure (NotPlaceable "unknown building")
                    Just def → do
                        mWtd ← snapshotVisibleWorldTiles env
                        case mWtd of
                            Nothing  → pure (NotPlaceable "no world loaded")
                            Just wtd → pure (canPlaceAt bm wtd def gx gy)
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
            Lua.liftIO $ writeIORef (buildingGhostRef env) $ Just BuildingGhost
                { bgDefName = name
                , bgGridX   = fromIntegral x
                , bgGridY   = fromIntegral y
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
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (biDefName inst))
                    Lua.setfield (-2) "defName"
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
                    return 1

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
                            now ← realToFrac <$> getPOSIXTime
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

buildingListDefsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingListDefsFn env = do
    names ← Lua.liftIO $ do
        bm ← readIORef (buildingManagerRef env)
        return $ HM.keys (bmDefs bm)
    Lua.newtable
    forM_ (zip [1..] names) $ \(i, name) → do
        Lua.pushstring (TE.encodeUtf8 name)
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
