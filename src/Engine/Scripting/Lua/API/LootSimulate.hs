{-# LANGUAGE Strict #-}
-- | @loot.simulate@ (#2502, epic #1231 PLC-13; design D-21) — the
--   distribution diagnostic for one loot profile against one container
--   definition.
--
--   Deliberately its own module rather than a fifth function in
--   "Engine.Scripting.Lua.API.LootProfiles", whose own haddock records
--   why it takes exactly two capability records. This verb needs four,
--   and each for one reason:
--
--   * 'ContentRegistriesCapability' — the loot-profile registry, READ
--     to resolve the profile id. (That record is the only handle on
--     that registry; nothing here writes it.)
--   * 'ContentRegistriesViewCapability' — the ITEM registry, as the
--     read-only ref #1896 narrowed it to, for the container definition
--     and every entry's item.
--   * 'WorldSimCapability' — the active world page, for its generation
--     seed. The simulation is only meaningful against the seed the
--     realizations it predicts would actually use.
--   * 'CoreCapability' — the logger 'materializeItem' reports a cyclic
--     definition graph through.
--
--   Nothing here touches an 'Engine.Core.State.EngineEnv', advances the
--   engine's instance-id counter, or reads a shared generator: every
--   allocator and generator the simulation uses is local to the call
--   (see "LootProfile.Simulate").
module Engine.Scripting.Lua.API.LootSimulate
    ( lootSimulateFn
    ) where

import UPrelude
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Engine.Core.Capability.Core (CoreCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..))
import Engine.Core.Capability.ContentRegistriesView
    (ContentRegistriesViewCapability(..))
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import Engine.Core.Log.Monad (getLoggerFor)
import Engine.Core.ReadOnlyRef (readReadOnlyRef)
import Engine.Core.State (activeWorldStateFrom)
import LootProfile.Simulate (SimSummary(..), simulateLootProfile)
import LootProfile.Types (lookupLootProfile)
import World.Types (WorldState(..), WorldGenParams(..))

-- | @loot.simulate(profileId, containerDefName, sampleCount)@ → report
--   table | nil.
--
--   Sample @i@ (one-based) realizes the profile into a freshly minted
--   @containerDefName@ using the ACTIVE world page's generation seed,
--   location-instance id @i@ and slot @0@, through the same realization
--   and admission a placed crate would use. The report:
--
--   > { samples          = 200,
--   >   naturally_empty  = 0.145,        -- fraction of samples
--   >   saturated        = 0.310,        -- fraction of samples
--   >   refused          = 0,            -- count (fail-closed; see below)
--   >   weight_histogram = { …10 counts… },   -- [0,10) … [90,100]
--   >   bulk_histogram   = { …10 counts… },
--   >   rejected_by_item = { steel_bar = 41, … } }
--
--   @nil@, with nothing measured, for: a missing or non-string profile
--   id or container name; a sample count that is not a Lua @number@;
--   a non-positive count; an unknown profile id; an item definition
--   that is unknown or declares no @storage:@; and no active world
--   page, or one with no generation parameters yet — the same @nil@
--   @world.getSeed()@ answers there, for the same reason.
--
--   __Every argument is type-checked before it is converted.__ All
--   three of Lua's conversions coerce across the number\/string line:
--   'Lua.tointeger' accepts @"200"@ and 'Lua.tostring' turns @123@
--   into @"123"@. Either coercion would make a typo'd payload run a
--   simulation the caller did not ask for — and for the two names it
--   would do so silently, since a registry may legitimately hold a
--   profile or an item definition whose id is all digits. So each
--   argument's 'Lua.ltype' decides before its value is read, and a
--   wrong type is the same @nil@ a missing one is.
--
--   Deterministic and side-effect-free in the sense that matters to a
--   caller: same seed and same arguments, same table, and the engine's
--   allocator and stat RNG are exactly where they were.
lootSimulateFn ∷ CoreCapability → ContentRegistriesCapability
               → ContentRegistriesViewCapability → WorldSimCapability
               → Lua.LuaE Lua.Exception Lua.NumResults
lootSimulateFn core regs regsView wsc = do
    profileArg   ← stringArg 1
    containerArg ← stringArg 2
    countTy      ← Lua.ltype 3
    countArg     ← case countTy of
        Lua.TypeNumber → Lua.tointeger 3
        _              → pure Nothing
    case (profileArg, containerArg, countArg) of
        (Just profileBS, Just containerBS, Just count) → do
            mSummary ← Lua.liftIO $ do
                reg ← readIORef (crLootProfileRegistryRef regs)
                let pid = TE.decodeUtf8Lenient profileBS
                case lookupLootProfile pid reg of
                    Nothing      → pure Nothing
                    Just profile → do
                        mSeed ← activeWorldSeed wsc
                        case mSeed of
                            Nothing   → pure Nothing
                            Just seed → do
                                logger ← getLoggerFor core
                                im ← readReadOnlyRef
                                         (crvItemManagerRef regsView)
                                simulateLootProfile im logger seed profile
                                    (TE.decodeUtf8Lenient containerBS)
                                    (fromIntegral count)
            case mSummary of
                Nothing      → Lua.pushnil ≫ return 1
                Just summary → pushSummary summary ≫ return 1
        _ → Lua.pushnil ≫ return 1

-- | One argument that must be an actual Lua @string@. 'Lua.tostring'
--   would otherwise coerce a @number@ (and, for the error-object type,
--   consult a metamethod), so the type is checked first and nothing
--   else is.
stringArg ∷ Lua.StackIndex → Lua.LuaE Lua.Exception (Maybe ByteString)
stringArg ix = do
    ty ← Lua.ltype ix
    case ty of
        Lua.TypeString → Lua.tostring ix
        _              → pure Nothing

-- | The generation seed of the ACTIVE world page, with exactly
--   'Engine.Scripting.Lua.API.World.Clock.worldGetSeedFn''s no-argument
--   semantics: 'Nothing' while no world, or no generation parameters,
--   exist.
activeWorldSeed ∷ WorldSimCapability → IO (Maybe Int)
activeWorldSeed wsc = do
    mWs ← activeWorldStateFrom (wsWorldManagerRef wsc)
    case mWs of
        Nothing → pure Nothing
        Just ws → fmap (fromIntegral ∘ wgpSeed)
                      <$> readIORef (wsGenParamsRef ws)

pushSummary ∷ SimSummary → Lua.LuaE Lua.Exception ()
pushSummary summary = do
    Lua.newtable
    Lua.pushinteger (fromIntegral (ssSamples summary))
    Lua.setfield (-2) "samples"
    pushFraction (ssNaturallyEmpty summary) (ssSamples summary)
    Lua.setfield (-2) "naturally_empty"
    pushFraction (ssSaturated summary) (ssSamples summary)
    Lua.setfield (-2) "saturated"
    Lua.pushinteger (fromIntegral (ssRefused summary))
    Lua.setfield (-2) "refused"
    pushCounts (ssWeightBins summary)
    Lua.setfield (-2) "weight_histogram"
    pushCounts (ssBulkBins summary)
    Lua.setfield (-2) "bulk_histogram"
    Lua.newtable
    forM_ (ssRejectedByItem summary) $ \(item, n) → do
        Lua.pushinteger (fromIntegral n)
        Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 item))
    Lua.setfield (-2) "rejected_by_item"

-- | A count as a fraction of the sample size. The denominator is
--   'ssSamples', which the caller-side guard has already established is
--   positive.
pushFraction ∷ Int → Int → Lua.LuaE Lua.Exception ()
pushFraction n total = Lua.pushnumber ∘ Lua.Number $
    if total ≤ 0 then 0 else fromIntegral n / fromIntegral total

-- | A dense 1-based array of counts.
pushCounts ∷ [Int] → Lua.LuaE Lua.Exception ()
pushCounts counts = do
    Lua.newtable
    forM_ (zip [1 ..] counts) $ \(i, n) → do
        Lua.pushinteger (fromIntegral n)
        Lua.rawseti (-2) i
