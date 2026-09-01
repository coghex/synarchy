{-# LANGUAGE Strict #-}
-- | Lua surface for the loot-table catalogue (#90).
--
--   Narrowed to the @content-registries@ capability (#890, epic #537):
--   the loot-table registry is reached only through
--   'ContentRegistriesCapability', the logger only through
--   'CoreCapability', and the one field outside both groups (the shared
--   stat RNG, @units-buildings-combat@ — see
--   'docs/engineenv_capability_inventory.md' SS7.5) is passed in as the
--   bare 'IORef' it is, so this module never touches an 'EngineEnv'.
--   'lootRollForFn' (#948) needs the registry alone: its draw is a pure
--   function of the caller's stable context, so it takes no RNG handle
--   at all and the module's 'EngineEnv' field set is unchanged.
module Engine.Scripting.Lua.API.LootTables
    ( loadLootTableYamlFn
    , lootRollForFn
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
import Engine.Core.Log (LogCategory(..), logDebug)
import Engine.Core.Log.Monad (getLoggerFor)
import Engine.Asset.YamlLootTables
import LootTable.Types
import LootTable.Roll (LootRollContext(..), rollLootTable, rollLootTableFor)

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
                    Nothing → do
                        -- The parse failure itself already warned in
                        -- 'Engine.Asset.YamlLootTables'; this is the same
                        -- per-file Debug detail the successful branch
                        -- carries, so the value handed back to Lua is
                        -- recoverable for BOTH outcomes (#1930).
                        logDebug logger CatAsset $
                            "loadLootTableYaml: loaded 0 loot tables from "
                            <> T.pack filePath
                        return (0 ∷ Int)
                    Just d → do
                        let def = LootTableDef
                                { ltdId      = ltydId d
                                , ltdEntries = map toEntry (ltydEntries d)
                                }
                        atomicModifyIORef' (crLootTableRegistryRef regs) $ \reg →
                            (registerLootTable def reg, ())
                        -- Debug, not Info (#1930): the aggregate is
                        -- scripts/startup_loader.lua's. Unlike its eleven
                        -- siblings this line named the table id but no
                        -- count, so the 1 it returns to Lua is spelled out
                        -- here — a loot table file holds exactly one def.
                        logDebug logger CatAsset $
                            "loadLootTableYaml: loaded 1 loot table '"
                            <> ltdId def <> "' from " <> T.pack filePath
                        return 1
            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1
  where
    toEntry e = LootTableEntry
        { lteId     = ltyeId e
        , lteWeight = ltyeWeight e
        }

-- | loot.roll(tableId) → item def name (string) | nil. A single
--   weighted draw from the named table using the engine's shared,
--   entropy-seeded stat RNG — the same generator item weight rolls
--   ('Item.Roll.rollItemWeight') draw from. This is the UNCONTEXTUAL
--   compatibility surface (#948): its result depends on process entropy
--   and on every other consumer of that generator, so repeated runs do
--   NOT agree. Placed-location content spawning does not use it — that
--   path calls 'lootRollForFn' below. Unknown table id (or an empty one)
--   returns nil — the location content-spawn dispatcher
--   (scripts/locations.lua) logs the warning.
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

-- | loot.rollFor(tableId, worldSeed, instanceId, entryIndex, rollIndex)
--   → item def name (string) | nil. The SEED-STABLE draw placed-location
--   content spawning uses (#948): a pure function of the named table and
--   the caller's stable context ('LootRollContext'), reading no
--   generator and consuming no process entropy. Two fresh processes
--   generating the same world therefore assign the same item to the same
--   ruin, whatever order chunks and locations load in.
--
--   All four context arguments are REQUIRED and must be integers —
--   a missing or non-integer one returns nil rather than silently
--   falling back to the entropy path, which would reintroduce exactly
--   the non-determinism this exists to remove. Unknown table id (or an
--   empty one) returns nil, same as 'lootRollFn'.
lootRollForFn ∷ ContentRegistriesCapability
              → Lua.LuaE Lua.Exception Lua.NumResults
lootRollForFn regs = do
    idArg    ← Lua.tostring 1
    seedArg  ← Lua.tointeger 2
    instArg  ← Lua.tointeger 3
    entryArg ← Lua.tointeger 4
    rollArg  ← Lua.tointeger 5
    case (idArg, seedArg, instArg, entryArg, rollArg) of
        (Just idBS, Just sd, Just inst, Just entry, Just roll) → do
            let tid = TE.decodeUtf8Lenient idBS
                ctx = LootRollContext
                        { lrcWorldSeed  = fromIntegral sd
                        , lrcInstanceId = fromIntegral inst
                        , lrcEntryIndex = fromIntegral entry
                        , lrcRollIndex  = fromIntegral roll
                        }
            reg ← Lua.liftIO $ readIORef (crLootTableRegistryRef regs)
            case lookupLootTable tid reg ⌦ \def → rollLootTableFor def ctx of
                Just pickedId → do
                    Lua.pushstring (TE.encodeUtf8 pickedId)
                    return 1
                Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1
