{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units
    ( loadUnitYamlFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo, logDebug)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister)
import Engine.Asset.YamlUnits (UnitYamlDef(..), loadUnitYaml)
import qualified Engine.Core.Queue as Q
import Unit.Types

-----------------------------------------------------------
-- engine.loadUnitYaml(filePath)
--
-- Parses a single unit .yaml file.  For each unit definition:
--   1. Loads the default sprite texture
--   2. Registers the TextureHandle by name ("unit_<name>")
--   3. Creates a UnitDef and inserts it into the UnitManager
--
-- Returns: number of textures queued for loading.
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

                    -- Reuse the existing texture loading helper
                    handle ← loadAndRegister env backendState lteq
                                 ("unit_" <> name) spritePath

                    -- Build UnitDef and insert into UnitManager
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
