{-# LANGUAGE Strict #-}
-- | Lua surface for the loot-profile catalogue (#2499, epic #1231
--   PLC-12): the @engine.loadLootProfileYaml@ populator and the two
--   read-only queries under the existing @loot@ namespace (D-20 —
--   the namespace list stays closed).
--
--   __Two capability records, deliberately.__ The profile registry is
--   this module's to WRITE, so it arrives through
--   'ContentRegistriesCapability' like its loot-table sibling. The ITEM
--   registry is only READ here, to resolve each entry's item id, and it
--   is one of the four registries #1896 narrowed: it arrives through
--   'ContentRegistriesViewCapability' as a
--   'Engine.Core.ReadOnlyRef.ReadOnlyRef', so this module cannot write
--   it even by accident. Copying the raw item handle out of
--   'ContentRegistriesCapability' — which is what
--   'Engine.Scripting.Lua.API.Locations' still does for #917's check —
--   would hand a new module write authority over items that it has no
--   claim to (@docs/engineenv_capability_inventory.md@ §2.1).
--
--   The logger comes through 'CoreCapability'. This module never
--   touches an 'EngineEnv'.
module Engine.Scripting.Lua.API.LootProfiles
    ( loadLootProfileYamlFn
    , lootProfileFn
    , lootListProfilesFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Capability.Core (CoreCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..))
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..))
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.Log (LogCategory(..), logDebug, logWarn)
import Engine.Core.Log.Monad (getLoggerFor)
import Engine.Scripting.Lua.API.YamlResult (pushYamlResult)
import Engine.Asset.YamlLootProfiles
import Item.Types (ItemManager(..))
import LootProfile.Types

-- | @engine.loadLootProfileYaml(path)@ — parse one loot profile YAML
--   file and register it. Answers @1@ on success and @0@ otherwise;
--   like a loot table and unlike the list-shaped families, a profile
--   file holds exactly one def. Callable repeatedly; each call
--   inserts/replaces by profile id.
--
--   __The #2203 outcome contract is preserved exactly.__ A bare call
--   still answers ONE number; a truthy SECOND argument opts in to
--   @(count, parsed)@, where @parsed@ is about the DECODE alone:
--
--     * a decode failure (including a repeated key) answers
--       @(0, false)@ — @scripts/startup_loader.lua@ turns that into the
--       terminal startup failure it turns every family's parse failure
--       into;
--     * a successful registration answers @(1, true)@;
--     * an unknown item id answers @(0, true)@ and registers NOTHING.
--       The file decoded, so it is not a parse failure and must not be
--       reported as one — this is the same zero-count success
--       'Engine.Scripting.Lua.API.Locations' has always reported for a
--       file its own post-decode schema validation rejects (#917,
--       #1101). It is a loud WARNING per offending entry rather than a
--       silent zero.
--
--   There is deliberately no third value: 'pushYamlRefusal' exists for
--   the #2241 duplicate-NAME collision within a list-shaped family, and
--   nothing here is that.
loadLootProfileYamlFn ∷ CoreCapability → ContentRegistriesCapability
                      → ContentRegistriesViewCapability
                      → Lua.LuaE Lua.Exception Lua.NumResults
loadLootProfileYamlFn core regs regsView = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → pushYamlResult False 0
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            (parsed, count) ← Lua.liftIO $ do
                logger ← getLoggerFor core
                mDef ← loadLootProfileYaml logger filePath
                case mDef of
                    Nothing → do
                        -- The parse failure itself already warned in
                        -- 'Engine.Asset.YamlLootProfiles'; this is the
                        -- same per-file Debug detail the successful
                        -- branch carries, so the value handed back to
                        -- Lua is recoverable for BOTH outcomes (#1930).
                        logDebug logger CatAsset $
                            "loadLootProfileYaml: loaded 0 loot profiles from "
                            <> T.pack filePath
                        return (False, 0 ∷ Int)
                    Just d → do
                        -- The ONE read of the item registry, through the
                        -- read-only view. Items load before profiles, so
                        -- this is the complete registry (D-20).
                        im ← readReadOnlyRef (crvItemManagerRef regsView)
                        let itemErrs = lootProfileItemErrors
                                           (HM.keysSet (imDefs im)) d
                        case itemErrs of
                          (_:_) → do
                            forM_ itemErrs $ \e →
                                logWarn logger CatAsset $
                                    "loadLootProfileYaml: rejected "
                                    <> T.pack filePath <> ": " <> e
                            logDebug logger CatAsset $
                                "loadLootProfileYaml: loaded 0 loot profiles \
                                \from " <> T.pack filePath
                            return (True, 0)
                          [] → do
                            let def = LootProfileDef
                                    { lpdId            = lpydId d
                                    , lpdMultiplierMin = lpydMultiplierMin d
                                    , lpdMultiplierMax = lpydMultiplierMax d
                                    , lpdEntries       = map toEntry
                                                             (lpydEntries d)
                                    }
                            -- Insert/replace by id, atomically, and say
                            -- so when it REPLACED something: two files
                            -- claiming one profile id is an authoring
                            -- mistake whose only symptom is otherwise
                            -- that whichever file the directory listing
                            -- happened to yield second wins.
                            replaced ← atomicModifyIORef'
                                (crLootProfileRegistryRef regs) $ \reg →
                                    ( registerLootProfile def reg
                                    , lookupLootProfile (lpdId def) reg )
                            forM_ replaced $ \_ →
                                logWarn logger CatAsset $
                                    "loadLootProfileYaml: " <> T.pack filePath
                                    <> " replaces the already-registered loot \
                                       \profile '" <> lpdId def <> "'"
                            -- Debug, not Info (#1930): the aggregate is
                            -- scripts/startup_loader.lua's. The count is
                            -- spelled out beside the id, exactly as
                            -- 'loadLootTableYamlFn' spells its own.
                            logDebug logger CatAsset $
                                "loadLootProfileYaml: loaded 1 loot profile '"
                                <> lpdId def <> "' from " <> T.pack filePath
                            return (True, 1)
            pushYamlResult parsed count
  where
    toEntry e = LootProfileEntry
        { lpeItem           = lpyeItem e
        , lpeChance         = lpyeChance e
        , lpeQuantityFactor = lpyeQuantityFactor e
        }

-- | @loot.profile(id)@ → profile table | nil. READ-ONLY: the table is
--   built fresh from the registry on every call, so a caller that edits
--   what it got back has edited its own copy and the next call answers
--   the registry again.
--
--   Shape, with dense 1-based @entries@ in AUTHORED order:
--
--   > { id = "ruin_industrial_salvage",
--   >   quantity_multiplier = { min = 1, max = 4 },
--   >   entries = { { item = "steel_bar", chance = 0.3,
--   >                 quantity_factor = 5 }, … } }
--
--   Unknown (or empty) profile id returns nil, the same answer
--   'Engine.Scripting.Lua.API.LootTables.lootRollFn' gives for an
--   unknown table.
lootProfileFn ∷ ContentRegistriesCapability
              → Lua.LuaE Lua.Exception Lua.NumResults
lootProfileFn regs = do
    idArg ← Lua.tostring 1
    case idArg of
        Nothing → Lua.pushnil ≫ return 1
        Just idBS → do
            let pid = TE.decodeUtf8Lenient idBS
            mDef ← Lua.liftIO $
                lookupLootProfile pid <$> readIORef (crLootProfileRegistryRef regs)
            case mDef of
                Nothing  → Lua.pushnil ≫ return 1
                Just def → pushProfile def ≫ return 1

pushProfile ∷ LootProfileDef → Lua.LuaE Lua.Exception ()
pushProfile def = do
    Lua.newtable
    Lua.pushstring (TE.encodeUtf8 (lpdId def))
    Lua.setfield (-2) "id"
    Lua.newtable
    Lua.pushinteger (fromIntegral (lpdMultiplierMin def))
    Lua.setfield (-2) "min"
    Lua.pushinteger (fromIntegral (lpdMultiplierMax def))
    Lua.setfield (-2) "max"
    Lua.setfield (-2) "quantity_multiplier"
    Lua.newtable
    forM_ (zip [1..] (lpdEntries def)) $ \(i, e) → do
        Lua.newtable
        Lua.pushstring (TE.encodeUtf8 (lpeItem e))
        Lua.setfield (-2) "item"
        Lua.pushnumber (Lua.Number (realToFrac (lpeChance e)))
        Lua.setfield (-2) "chance"
        Lua.pushinteger (fromIntegral (lpeQuantityFactor e))
        Lua.setfield (-2) "quantity_factor"
        Lua.rawseti (-2) i
    Lua.setfield (-2) "entries"

-- | @loot.listProfiles()@ → dense 1-based array of every registered
--   profile id, ASCENDING. READ-ONLY, and sorted rather than left in
--   'HM.HashMap' traversal order so the answer is the same in two
--   processes that loaded the same files.
lootListProfilesFn ∷ ContentRegistriesCapability
                   → Lua.LuaE Lua.Exception Lua.NumResults
lootListProfilesFn regs = do
    ids ← Lua.liftIO $
        lootProfileIds <$> readIORef (crLootProfileRegistryRef regs)
    Lua.newtable
    forM_ (zip [1..] ids) $ \(i, pid) → do
        Lua.pushstring (TE.encodeUtf8 pid)
        Lua.rawseti (-2) i
    return 1
