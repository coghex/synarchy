{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units
    ( loadUnitYamlFn
    , unitSpawnFn
    , unitDestroyFn
    , unitSetPosFn
    , unitMoveToFn
    , unitStopFn
    , unitGetPosFn
    , unitListFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo, logDebug, logWarn)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister)
import Engine.Asset.YamlUnits (UnitYamlDef(..), loadUnitYaml)
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Command.Types (UnitCommand(..))
import World.Types (WorldManager(..), WorldState(..), WorldTileData(..),
                    LoadedChunk(..), ChunkCoord(..), columnIndex, lookupChunk)
import World.Generate (globalToChunk)

-----------------------------------------------------------
-- engine.loadUnitYaml(filePath)
-----------------------------------------------------------

loadUnitYamlFn ∷ EngineEnv → LuaBackendState
               → Lua.LuaE Lua.Exception Lua.NumResults
loadUnitYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadUnitYaml logger filePath

                let (lteq, _) = lbsMsgQueues backendState

                total ← foldM (\acc def → do
                    let name = uydName def
                        spritePath = T.unpack (uydSprite def)

                    handle ← loadAndRegister env backendState lteq
                                 ("unit_" <> name) spritePath

                    let unitDef = UnitDef
                            { udName      = name
                            , udTexture   = handle
                            , udBaseWidth = uydBaseWidth def
                            }
                    atomicModifyIORef' (unitManagerRef env) $ \um →
                        (um { umDefs = HM.insert name unitDef (umDefs um) }, ())

                    logDebug logger CatAsset $
                        "Registered unit def: " <> name
                        <> " (handle " <> T.pack (show handle) <> ")"

                    return (acc + 1)
                    ) (0 ∷ Int) defs

                logInfo logger CatAsset $
                    "loadUnitYaml: loaded " <> T.pack (show total)
                    <> " unit definitions from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

-----------------------------------------------------------
-- Surface elevation lookup
-----------------------------------------------------------

lookupSurfaceZ ∷ EngineEnv → Int → Int → IO (Maybe Int)
lookupSurfaceZ env gx gy = do
    wm ← readIORef (worldManagerRef env)
    go (wmVisible wm) (wmWorlds wm)
  where
    (chunkCoord, (lx, ly)) = globalToChunk gx gy
    go [] _ = return Nothing
    go (pageId:rest) worlds =
        case lookup pageId worlds of
            Nothing → go rest worlds
            Just ws → do
                td ← readIORef (wsTilesRef ws)
                case lookupChunk chunkCoord td of
                    Just lc → return $ Just ((lcSurfaceMap lc) VU.! columnIndex lx ly)
                    Nothing → go rest worlds

-----------------------------------------------------------
-- unit.spawn(defName, gridX, gridY [, gridZ])
--
-- Spawns a unit instance.  If gridZ is omitted, looks up the
-- surface elevation at (gridX, gridY) from loaded terrain.
-- Falls back to Z=0 if the chunk isn't loaded.
-- Returns the unit ID (integer), or -1 on failure.
-----------------------------------------------------------

unitSpawnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSpawnFn env = do
    nameArg ← Lua.tostring 1
    xArg    ← Lua.tonumber 2
    yArg    ← Lua.tonumber 3
    zArg    ← Lua.tointeger 4   -- optional

    case nameArg of
        Nothing → do
            Lua.pushnumber (-1)
            return 1
        Just nameBS → do
            let name = TE.decodeUtf8 nameBS
                gx = case xArg of
                         Just (Lua.Number n) → realToFrac n
                         _                   → 0.0
                gy = case yArg of
                         Just (Lua.Number n) → realToFrac n
                         _                   → 0.0

            result ← Lua.liftIO $ do
                -- Check def exists
                um ← readIORef (unitManagerRef env)
                case HM.lookup name (umDefs um) of
                    Nothing → return (-1)
                    Just _ → do
                        -- Resolve Z
                        gz ← case zArg of
                            Just n  → return (fromIntegral n)
                            Nothing → do
                                let gxi = floor gx ∷ Int
                                    gyi = floor gy ∷ Int
                                mSurf ← lookupSurfaceZ env gxi gyi
                                case mSurf of
                                    Just z  → return z
                                    Nothing → do
                                        logger ← readIORef (loggerRef env)
                                        logWarn logger CatAsset $
                                            "unit.spawn: chunk not loaded at ("
                                            <> T.pack (show gxi) <> ", "
                                            <> T.pack (show gyi)
                                            <> "), defaulting Z=0"
                                        return 0

                        -- Allocate ID
                        uid ← atomicModifyIORef' (unitManagerRef env) $ \um' →
                            let (uid', um'') = nextUnitId um'
                            in (um'', uid')

                        -- Enqueue spawn command
                        Q.writeQueue (unitQueue env) $
                            UnitSpawn uid name gx gy gz

                        return (fromIntegral (unUnitId uid) ∷ Int)

            Lua.pushnumber (Lua.Number (fromIntegral result))
            return 1

-----------------------------------------------------------
-- unit.destroy(unitId)
--
-- Removes a unit instance by ID. Returns true.
-----------------------------------------------------------

unitDestroyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDestroyFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitDestroy uid
            Lua.pushboolean True
            return 1

-----------------------------------------------------------
-- unit.setPos(unitId, gridX, gridY [, gridZ])
--
-- Teleports a unit instance to a new position.
-- If gridZ is omitted, looks up surface elevation.
-----------------------------------------------------------

unitSetPosFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetPosFn env = do
    idArg ← Lua.tointeger 1
    xArg  ← Lua.tonumber 2
    yArg  ← Lua.tonumber 3
    zArg  ← Lua.tointeger 4

    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                gx = case xArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                gy = case yArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                mGz = case zArg of
                         Just z  → Just (fromIntegral z)
                         Nothing → Nothing
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitTeleport uid gx gy mGz
            Lua.pushboolean True
            return 1

-----------------------------------------------------------
-- unit.moveTo(unitId, targetX, targetY [, speed])
--
-- Orders a unit to walk to (targetX, targetY).
-- Speed is in tiles per second, defaults to 2.0.
-----------------------------------------------------------

unitMoveToFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitMoveToFn env = do
    idArg    ← Lua.tointeger 1
    xArg     ← Lua.tonumber 2
    yArg     ← Lua.tonumber 3
    speedArg ← Lua.tonumber 4

    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                tx = case xArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                ty = case yArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                speed = case speedArg of
                            Just (Lua.Number v) → realToFrac v
                            _                   → 2.0
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitMoveTo uid tx ty speed
            Lua.pushboolean True
            return 1

-----------------------------------------------------------
-- unit.stop(unitId)
--
-- Cancels any movement order on a unit.
-----------------------------------------------------------

unitStopFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitStopFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitStop uid
            Lua.pushboolean True
            return 1

-----------------------------------------------------------
-- unit.getPos(unitId)
--
-- Returns gridX, gridY, gridZ or nil on failure.
-----------------------------------------------------------

unitGetPosFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPosFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            um ← Lua.liftIO $ readIORef (unitManagerRef env)
            case HM.lookup uid (umInstances um) of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just inst → do
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridX inst)))
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridY inst)))
                    Lua.pushnumber (Lua.Number (fromIntegral (uiGridZ inst)))
                    return 3

-----------------------------------------------------------
-- unit.list()
--
-- Returns a string listing all live unit instances.
-- Useful for shell debugging.
-----------------------------------------------------------

unitListFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitListFn env = do
    result ← Lua.liftIO $ do
        um ← readIORef (unitManagerRef env)
        let entries = HM.toList (umInstances um)
        if null entries
        then return "No units spawned"
        else return $ T.unpack $ T.intercalate "\n" $
            map (\(uid, inst) →
                "id=" <> T.pack (show (unUnitId uid))
                <> " " <> uiDefName inst
                <> " (" <> T.pack (show (uiGridX inst))
                <> ", " <> T.pack (show (uiGridY inst))
                <> ", " <> T.pack (show (uiGridZ inst)) <> ")"
            ) entries
    Lua.pushstring (TE.encodeUtf8 (T.pack result))
    return 1
