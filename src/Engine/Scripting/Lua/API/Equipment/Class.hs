{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Equipment class catalogue (#478 silhouette fallback).
--   engine.loadEquipmentYaml parses data/equipment/*.yaml, loads each
--   class's silhouette texture, and registers the classes into the
--   EquipmentClassManager; equipment.getClass / getClassNames are the
--   read-only queries. The slot/accessory equip verbs and the
--   render-field pushers that consume these classes live in the
--   sibling Slot/Accessory/Render sub-modules.
module Engine.Scripting.Lua.API.Equipment.Class
    ( loadEquipmentYamlFn
    , equipmentGetClassFn
    , equipmentGetClassNamesFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister, resolveTexturePath)
import Engine.Asset.YamlEquipment
import Equipment.Types

-- | Canonical equipment-class fallback (#478): the pre-existing blank
--   humanoid silhouette (no slot artwork baked in), same dimensions as
--   an ordinary class silhouette, so a bad `silhouette:` path in
--   data/equipment/*.yaml substitutes here instead of reaching the
--   Vulkan texture loader with a nonexistent file.
missingEquipmentSilhouette ∷ FilePath
missingEquipmentSilhouette =
    "assets/textures/ui/placeholders/humanoid_silhouette_blank.png"

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
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                classes ← loadEquipmentYaml logger filePath
                let (lteq, _) = lbsMsgQueues backendState

                total ← foldM (\acc c → do
                    let regName = "equipment_" <> eycName c
                    silhouettePath ← resolveTexturePath env
                        "Equipment silhouette" missingEquipmentSilhouette
                        (T.unpack (eycSilhouette c))
                    handle ← loadAndRegister env backendState lteq
                                regName silhouettePath

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
            let name = TE.decodeUtf8Lenient nameBS
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
