{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units
    ( loadUnitYamlFn
    , unitSpawnFn
    , unitDestroyFn
    , unitSetPosFn
    , unitListFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
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
-- unit.spawn(defName, gridX, gridY [, gridZ])
--
-- Spawns a unit instance.  If gridZ is omitted, defaults to 0.
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
                gz = case zArg of
                         Just n  → fromIntegral n
                         Nothing → 0

            result ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup name (umDefs um) of
                    Nothing → do
                        logger ← readIORef (loggerRef env)
                        logWarn logger CatAsset $
                            "unit.spawn: unknown unit def '" <> name <> "'"
                        return (-1)
                    Just def → do
                        let (uid, um') = nextUnitId um
                            inst = UnitInstance
                                { uiDefName   = name
                                , uiTexture   = udTexture def
                                , uiBaseWidth = udBaseWidth def
                                , uiGridX     = gx
                                , uiGridY     = gy
                                , uiGridZ     = gz
                                }
                            um'' = um' { umInstances =
                                HM.insert uid inst (umInstances um') }
                        atomicModifyIORef' (unitManagerRef env) $
                            const (um'', ())
                        logger ← readIORef (loggerRef env)
                        logInfo logger CatAsset $
                            "Spawned unit '" <> name <> "' id="
                            <> T.pack (show (unUnitId uid))
                            <> " at (" <> T.pack (show gx)
                            <> ", " <> T.pack (show gy)
                            <> ", " <> T.pack (show gz) <> ")"
                        return (fromIntegral (unUnitId uid) ∷ Int)

            Lua.pushnumber (Lua.Number (fromIntegral result))
            return 1

-----------------------------------------------------------
-- unit.destroy(unitId)
--
-- Removes a unit instance by ID. Returns true/false.
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
            found ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                if HM.member uid (umInstances um)
                then (um { umInstances = HM.delete uid (umInstances um) }, True)
                else (um, False)
            Lua.pushboolean found
            return 1

-----------------------------------------------------------
-- unit.setPos(unitId, gridX, gridY [, gridZ])
--
-- Moves a unit instance to a new position.
-- If gridZ is omitted, keeps current Z.
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
            found ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let gz = case zArg of
                                     Just z  → fromIntegral z
                                     Nothing → uiGridZ inst
                            inst' = inst { uiGridX = gx
                                         , uiGridY = gy
                                         , uiGridZ = gz }
                        in (um { umInstances = HM.insert uid inst' (umInstances um) }, True)
            Lua.pushboolean found
            return 1

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
