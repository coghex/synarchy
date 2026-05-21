{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
module Engine.Scripting.Lua.API.Equipment
    ( loadEquipmentYamlFn
    , equipmentGetClassFn
    , equipmentGetClassNamesFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM, forM_)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister)
import Engine.Asset.YamlEquipment
import Equipment.Types

-- | equipment.loadYaml(path) — parses a YAML file describing one or
--   more equipment classes, loads each class's silhouette texture, and
--   registers the classes into the EquipmentClassManager. Returns the
--   number of classes loaded.
loadEquipmentYamlFn ∷ EngineEnv → LuaBackendState
                    → Lua.LuaE Lua.Exception Lua.NumResults
loadEquipmentYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                classes ← loadEquipmentYaml logger filePath
                let (lteq, _) = lbsMsgQueues backendState

                total ← foldM (\acc c → do
                    let regName = "equipment_" <> eycName c
                    handle ← loadAndRegister env backendState lteq
                                regName (T.unpack (eycSilhouette c))

                    let slots = map
                            (\s → EquipmentSlot
                                { esId   = eysId s
                                , esName = if T.null (eysName s)
                                           then eysId s
                                           else eysName s
                                , esKind = eysKind s
                                , esX    = eysX s
                                , esY    = eysY s
                                , esW    = eysW s
                                , esH    = eysH s
                                })
                            (eycSlots c)
                        ecDef = EquipmentClass
                            { ecName          = eycName c
                            , ecSilhouetteTex = handle
                            , ecSilhouetteW   = eycSilhouetteW c
                            , ecSilhouetteH   = eycSilhouetteH c
                            , ecSlots         = slots
                            }

                    atomicModifyIORef' (equipmentClassManagerRef env) $ \m →
                        (EquipmentClassManager
                            { ecmDefs = HM.insert (eycName c) ecDef
                                                  (ecmDefs m) }, ())

                    return (acc + 1)
                    ) (0 ∷ Int) classes

                logInfo logger CatAsset $
                    "loadEquipmentYaml: loaded " <> T.pack (show total)
                    <> " equipment classes from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

-- | equipment.getClass(name) → table or nil. The returned table is the
--   render-side view of an EquipmentClass:
--   { name, silhouette = <textureHandle int>, silhouetteW, silhouetteH,
--     slots = { { id, name, kind, x, y, w, h }, … } }
equipmentGetClassFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentGetClassFn env = do
    nameArg ← Lua.tostring 1
    case nameArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just nameBS → do
            let name = TE.decodeUtf8 nameBS
            mClass ← Lua.liftIO $ do
                mgr ← readIORef (equipmentClassManagerRef env)
                pure (lookupEquipmentClass name mgr)
            case mClass of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just c → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (ecName c))
                    Lua.setfield (-2) "name"
                    let TextureHandle texInt = ecSilhouetteTex c
                    Lua.pushinteger (fromIntegral texInt)
                    Lua.setfield (-2) "silhouette"
                    Lua.pushinteger (fromIntegral (ecSilhouetteW c))
                    Lua.setfield (-2) "silhouetteW"
                    Lua.pushinteger (fromIntegral (ecSilhouetteH c))
                    Lua.setfield (-2) "silhouetteH"
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] (ecSlots c)) $ \(i, s) → do
                        Lua.newtable
                        Lua.pushstring (TE.encodeUtf8 (esId s))
                        Lua.setfield (-2) "id"
                        Lua.pushstring (TE.encodeUtf8 (esName s))
                        Lua.setfield (-2) "name"
                        Lua.pushstring (TE.encodeUtf8 (esKind s))
                        Lua.setfield (-2) "kind"
                        Lua.pushinteger (fromIntegral (esX s))
                        Lua.setfield (-2) "x"
                        Lua.pushinteger (fromIntegral (esY s))
                        Lua.setfield (-2) "y"
                        Lua.pushinteger (fromIntegral (esW s))
                        Lua.setfield (-2) "w"
                        Lua.pushinteger (fromIntegral (esH s))
                        Lua.setfield (-2) "h"
                        Lua.rawseti (-2) (fromIntegral i)
                    Lua.setfield (-2) "slots"
                    return 1

-- | equipment.getClassNames() → array of strings (sorted by HashMap
--   iteration order, i.e. arbitrary). Returns every registered class.
equipmentGetClassNamesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentGetClassNamesFn env = do
    names ← Lua.liftIO $ do
        mgr ← readIORef (equipmentClassManagerRef env)
        pure (HM.keys (ecmDefs mgr))
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] names) $ \(i, n) → do
        Lua.pushstring (TE.encodeUtf8 n)
        Lua.rawseti (-2) (fromIntegral i)
    return 1
