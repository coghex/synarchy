{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua surface for the loot-table catalogue (#90).
--
--   Narrowed to the @content-registries@ capability (#890, epic #537):
--   the loot-table registry is reached only through
--   'ContentRegistriesCapability', the logger only through
--   'CoreCapability', and the one field outside both groups (the shared
--   stat RNG, @units-buildings-combat@ — see
--   'docs/engineenv_capability_inventory.md' SS7.5) is passed in as the
--   bare 'IORef' it is, so this module never touches an 'EngineEnv'.
module Engine.Scripting.Lua.API.LootTables
    ( loadLootTableYamlFn
    , lootRollFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import System.Random (StdGen)
import Engine.Core.Capability.Core (CoreCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..))
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Core.Log.Monad (getLoggerFor)
import Engine.Asset.YamlLootTables
import LootTable.Types
import LootTable.Roll (rollLootTable)

-- | engine.loadLootTableYaml(path) — parses one loot table YAML file
--   and registers it, returns 1 on success, 0 on failure (unlike the
--   other engine.loadXYaml functions, a loot table file holds exactly
--   one def, not a list). Callable repeatedly; each call
--   inserts/replaces by table id.
loadLootTableYamlFn ∷ CoreCapability → ContentRegistriesCapability
                    → Lua.LuaE Lua.Exception Lua.NumResults
loadLootTableYamlFn core regs = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            count ← Lua.liftIO $ do
                logger ← getLoggerFor core
                mDef ← loadLootTableYaml logger filePath
                case mDef of
                    Nothing → return (0 ∷ Int)
                    Just d → do
                        let def = LootTableDef
                                { ltdId      = ltydId d
                                , ltdEntries = map toEntry (ltydEntries d)
                                }
                        atomicModifyIORef' (crLootTableRegistryRef regs) $ \reg →
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
lootRollFn ∷ ContentRegistriesCapability → IORef StdGen
           → Lua.LuaE Lua.Exception Lua.NumResults
lootRollFn regs rngRef = do
    idArg ← Lua.tostring 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just idBS → do
            let tid = TE.decodeUtf8Lenient idBS
            mPick ← Lua.liftIO $ do
                reg ← readIORef (crLootTableRegistryRef regs)
                case lookupLootTable tid reg of
                    Nothing  → pure Nothing
                    Just def → rollLootTable def rngRef
            case mPick of
                Just pickedId → do
                    Lua.pushstring (TE.encodeUtf8 pickedId)
                    return 1
                Nothing → Lua.pushnil >> return 1
