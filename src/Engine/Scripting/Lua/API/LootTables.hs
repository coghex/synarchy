{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
module Engine.Scripting.Lua.API.LootTables
    ( loadLootTableYamlFn
    , lootRollFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Asset.YamlLootTables
import LootTable.Types
import LootTable.Roll (rollLootTable)

-- | engine.loadLootTableYaml(path) — parses one loot table YAML file
--   and registers it, returns 1 on success, 0 on failure (unlike the
--   other engine.loadXYaml functions, a loot table file holds exactly
--   one def, not a list).
loadLootTableYamlFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadLootTableYamlFn env = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                mDef ← loadLootTableYaml logger filePath
                case mDef of
                    Nothing → return (0 ∷ Int)
                    Just d → do
                        let def = LootTableDef
                                { ltdId      = ltydId d
                                , ltdEntries = map toEntry (ltydEntries d)
                                }
                        atomicModifyIORef' (lootTableRegistryRef env) $ \reg →
                            (registerLootTable def reg, ())
                        logInfo logger CatAsset $
                            "loadLootTableYaml: loaded '" <> ltdId def
                            <> "' from " <> T.pack filePath
                        return 1
            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1
  where
    toEntry e = LootTableEntry
        { lteId     = ltyeId e
        , lteWeight = ltyeWeight e
        }

-- | loot.roll(tableId) → item def name (string) | nil. A single
--   weighted draw from the named table using the same RNG source as
--   item weight rolls ('Item.Roll.rollItemWeight'). Unknown table id
--   (or an empty one) returns nil — the location content-spawn
--   dispatcher (scripts/locations.lua) logs the warning.
lootRollFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
lootRollFn env = do
    idArg ← Lua.tostring 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just idBS → do
            let tid = TE.decodeUtf8Lenient idBS
            mPick ← Lua.liftIO $ do
                reg ← readIORef (lootTableRegistryRef env)
                case lookupLootTable tid reg of
                    Nothing  → pure Nothing
                    Just def → rollLootTable def (statRNGRef env)
            case mPick of
                Just pickedId → do
                    Lua.pushstring (TE.encodeUtf8 pickedId)
                    return 1
                Nothing → Lua.pushnil >> return 1
