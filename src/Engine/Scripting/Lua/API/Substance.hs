{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
module Engine.Scripting.Lua.API.Substance
    ( loadSubstanceYamlFn
    , substanceGetFn
    , substanceGetNamesFn
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
import Engine.Asset.YamlSubstance
import Substance.Types

-- | engine.loadSubstanceYaml(path) — parses a YAML file of substance
--   defs, registers each into the SubstanceManager, returns the count.
--   Mirrors engine.loadItemYaml / engine.loadEquipmentYaml.
loadSubstanceYamlFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadSubstanceYamlFn env = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadSubstanceYaml logger filePath
                total ← foldM (\acc d → do
                    let sbsDef = SubstanceDef
                            { sbsName              = syName d
                            , sbsDensity           = syDensity d
                            , sbsTensileStrength   = syTensileStrength d
                            , sbsYieldStrength     = syYieldStrength d
                            , sbsShearStrength     = syShearStrength d
                            , sbsFractureToughness = syFractureToughness d
                            , sbsHardness          = syHardness d
                            , sbsStabResistance    = syStabResistance d
                            , sbsSlashResistance   = sySlashResistance d
                            , sbsBluntResistance   = syBluntResistance d
                            }
                    atomicModifyIORef' (substanceManagerRef env) $ \m →
                        (SubstanceManager
                            { sbmDefs = HM.insert (syName d) sbsDef
                                                  (sbmDefs m) }, ())
                    return (acc + 1)
                    ) (0 ∷ Int) defs
                logInfo logger CatAsset $
                    "loadSubstanceYaml: loaded " <> T.pack (show total)
                    <> " substances from " <> T.pack filePath
                return total
            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

-- | substance.get(name) → table or nil. Returns the substance's full
--   property set so Lua-side combat math (when it lands) can read
--   density, tensile, fracture toughness, etc. directly.
substanceGetFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
substanceGetFn env = do
    nameArg ← Lua.tostring 1
    case nameArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just nameBS → do
            let name = TE.decodeUtf8 nameBS
            mDef ← Lua.liftIO $ do
                mgr ← readIORef (substanceManagerRef env)
                pure (lookupSubstance name mgr)
            case mDef of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just d → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (sbsName d))
                    Lua.setfield (-2) "name"
                    Lua.pushnumber (Lua.Number (realToFrac (sbsDensity d)))
                    Lua.setfield (-2) "density"
                    Lua.pushnumber
                        (Lua.Number (realToFrac (sbsTensileStrength d)))
                    Lua.setfield (-2) "tensileStrength"
                    Lua.pushnumber
                        (Lua.Number (realToFrac (sbsYieldStrength d)))
                    Lua.setfield (-2) "yieldStrength"
                    Lua.pushnumber
                        (Lua.Number (realToFrac (sbsShearStrength d)))
                    Lua.setfield (-2) "shearStrength"
                    Lua.pushnumber
                        (Lua.Number (realToFrac (sbsFractureToughness d)))
                    Lua.setfield (-2) "fractureToughness"
                    Lua.pushnumber (Lua.Number (realToFrac (sbsHardness d)))
                    Lua.setfield (-2) "hardness"
                    Lua.pushnumber
                        (Lua.Number (realToFrac (sbsStabResistance d)))
                    Lua.setfield (-2) "stabResistance"
                    Lua.pushnumber
                        (Lua.Number (realToFrac (sbsSlashResistance d)))
                    Lua.setfield (-2) "slashResistance"
                    Lua.pushnumber
                        (Lua.Number (realToFrac (sbsBluntResistance d)))
                    Lua.setfield (-2) "bluntResistance"
                    return 1

substanceGetNamesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
substanceGetNamesFn env = do
    names ← Lua.liftIO $ do
        mgr ← readIORef (substanceManagerRef env)
        pure (HM.keys (sbmDefs mgr))
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] names) $ \(i, n) → do
        Lua.pushstring (TE.encodeUtf8 n)
        Lua.rawseti (-2) (fromIntegral i)
    return 1
