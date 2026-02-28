{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Flora
    ( floraRegisterFn
    , floraSetLifecycleFn
    , floraAddPhaseFn
    , floraAddCycleStageFn
    , floraAddCycleOverrideFn
    , floraRegisterForWorldGenFn
    ) where

import UPrelude
import qualified HsLua as Lua
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef', IORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import World.Types
import World.Flora.Types

-----------------------------------------------------------
-- Helper: get wsFloraCatalogRef from the first world
-----------------------------------------------------------

withFloraCatalog ∷ EngineEnv
                 → (IORef FloraCatalog → Lua.LuaE Lua.Exception Lua.NumResults)
                 → Lua.LuaE Lua.Exception Lua.NumResults
withFloraCatalog env action = do
    manager ← Lua.liftIO $ readIORef (worldManagerRef env)
    case wmWorlds manager of
        ((_, worldState):_) → action (wsFloraCatalogRef worldState)
        [] → do
            Lua.pushnil
            return 1

-----------------------------------------------------------
-- flora.register(name, baseTextureHandle) → floraId
--
-- Lua:
--   local tex = engine.loadTexture("assets/.../default.png")
--   local oak = flora.register("oak", tex)
-----------------------------------------------------------

floraRegisterFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
floraRegisterFn env = do
    nameArg ← Lua.tostring 1
    texArg  ← Lua.tointeger 2

    case (nameArg, texArg) of
        (Just nameBS, Just texInt) → withFloraCatalog env $ \catRef → do
            let name = TE.decodeUtf8 nameBS
                baseTex = TextureHandle (fromIntegral texInt)

            fid ← Lua.liftIO $ atomicModifyIORef' catRef $ \cat →
                let (newId, cat') = nextFloraId cat
                    species = newFloraSpecies name baseTex
                    cat'' = insertSpecies newId species cat'
                in (cat'', newId)

            Lua.pushinteger (fromIntegral (unFloraId fid))
            return 1
        _ → do
            Lua.pushnil
            return 1

-----------------------------------------------------------
-- flora.setLifecycle(floraId, type)
-- flora.setLifecycle(floraId, "perennial", minLifespan, maxLifespan, deathChance)
--
-- type: "evergreen" (default), "perennial", "annual", "biennial"
--
-- For perennial:
--   minLifespan = game-days before death is possible
--   maxLifespan = game-days, guaranteed dead by here
--   deathChance = per-year probability once past min (0.0-1.0)
-----------------------------------------------------------

floraSetLifecycleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
floraSetLifecycleFn env = do
    fidArg   ← Lua.tointeger 1
    typeArg  ← Lua.tostring 2
    arg3     ← Lua.tonumber 3
    arg4     ← Lua.tonumber 4
    arg5     ← Lua.tonumber 5

    case (fidArg, typeArg) of
        (Just fidInt, Just typeBS) → withFloraCatalog env $ \catRef → do
            let fid = FloraId (fromIntegral fidInt)
                typeText = TE.decodeUtf8 typeBS
                lifecycle = case typeText of
                    "perennial" →
                        let minL = luaNum arg3 1800.0   -- ~5 years default
                            maxL = luaNum arg4 3600.0   -- ~10 years default
                            dc   = luaNum arg5 0.2
                        in Perennial minL maxL dc
                    "annual"    → Annual
                    "biennial"  → Biennial
                    _           → Evergreen

            Lua.liftIO $ atomicModifyIORef' catRef $ \cat →
                case lookupSpecies fid cat of
                    Just species →
                        let species' = species { fsLifecycle = lifecycle }
                        in (insertSpecies fid species' cat, ())
                    Nothing → (cat, ())
            return 0
        _ → return 0

-----------------------------------------------------------
-- flora.addPhase(floraId, phaseTag, textureHandle, age)
--
-- phaseTag: "sprout", "seedling", "vegetating", "budding",
--   "flowering", "ripening", "matured", "withering", "dead"
-- age: game-days to enter this phase (default 0)
-----------------------------------------------------------

floraAddPhaseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
floraAddPhaseFn env = do
    fidArg ← Lua.tointeger 1
    tagArg ← Lua.tostring 2
    texArg ← Lua.tointeger 3
    ageArg ← Lua.tonumber 4

    case (fidArg, tagArg, texArg) of
        (Just fidInt, Just tagBS, Just texInt) → withFloraCatalog env $ \catRef → do
            let fid = FloraId (fromIntegral fidInt)
                tagText = TE.decodeUtf8 tagBS
                tex = TextureHandle (fromIntegral texInt)
                age = case ageArg of
                    Just (Lua.Number a) → realToFrac a
                    _                   → 0.0

            case parsePhaseTag tagText of
                Just tag → Lua.liftIO $ atomicModifyIORef' catRef $ \cat →
                    case lookupSpecies fid cat of
                        Just species →
                            let phase = LifePhase
                                    { lpTag     = tag
                                    , lpAge     = age
                                    , lpTexture = tex
                                    }
                                species' = species
                                    { fsPhases = HM.insert tag phase
                                                   (fsPhases species) }
                            in (insertSpecies fid species' cat, ())
                        Nothing → (cat, ())
                Nothing → pure ()
            return 0
        _ → return 0

-----------------------------------------------------------
-- flora.addCycleStage(floraId, stageTag, startDay, textureHandle)
--
-- Defines one stage of the annual cycle.
-- stageTag: "dormant", "budding", "flowering",
--           "fruiting", "senescing"
-- startDay: day-of-year (0-359) when this stage begins.
--           Stages run until the next stage's startDay.
--
-- Lua:
--   flora.addCycleStage(dandelion, "dormant",   0,   texDormant)
--   flora.addCycleStage(dandelion, "budding",   60,  texBud)
--   flora.addCycleStage(dandelion, "flowering",  90, texFlower)
--   flora.addCycleStage(dandelion, "senescing", 150, texWilt)
-----------------------------------------------------------

floraAddCycleStageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
floraAddCycleStageFn env = do
    fidArg   ← Lua.tointeger 1
    tagArg   ← Lua.tostring 2
    dayArg   ← Lua.tointeger 3
    texArg   ← Lua.tointeger 4

    case (fidArg, tagArg, dayArg, texArg) of
        (Just fidInt, Just tagBS, Just dayInt, Just texInt) →
            withFloraCatalog env $ \catRef → do
                let fid = FloraId (fromIntegral fidInt)
                    tagText = TE.decodeUtf8 tagBS
                    tex = TextureHandle (fromIntegral texInt)
                    day = fromIntegral dayInt

                case parseCycleTag tagText of
                    Just tag → Lua.liftIO $ atomicModifyIORef' catRef $ \cat →
                        case lookupSpecies fid cat of
                            Just species →
                                let stage = AnnualStage
                                        { asTag      = tag
                                        , asStartDay = day
                                        , asTexture  = tex
                                        }
                                    species' = species
                                        { fsAnnualCycle =
                                            fsAnnualCycle species ++ [stage] }
                                in (insertSpecies fid species' cat, ())
                            Nothing → (cat, ())
                    Nothing → pure ()
                return 0
        _ → return 0

-----------------------------------------------------------
-- flora.addCycleOverride(floraId, phaseTag, cycleTag, textureHandle)
--
-- Override the annual cycle texture for a specific life phase.
-- E.g. an oak's "matured + flowering" looks different from
-- its "vegetating + flowering".
--
-- Lua:
--   flora.addCycleOverride(oak, "matured", "budding", texOakMatureBud)
-----------------------------------------------------------

floraAddCycleOverrideFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
floraAddCycleOverrideFn env = do
    fidArg   ← Lua.tointeger 1
    phaseArg ← Lua.tostring 2
    cycleArg ← Lua.tostring 3
    texArg   ← Lua.tointeger 4

    case (fidArg, phaseArg, cycleArg, texArg) of
        (Just fidInt, Just phaseBS, Just cycleBS, Just texInt) →
            withFloraCatalog env $ \catRef → do
                let fid = FloraId (fromIntegral fidInt)
                    phaseText = TE.decodeUtf8 phaseBS
                    cycleText = TE.decodeUtf8 cycleBS
                    tex = TextureHandle (fromIntegral texInt)

                case (parsePhaseTag phaseText, parseCycleTag cycleText) of
                    (Just pTag, Just cTag) →
                        Lua.liftIO $ atomicModifyIORef' catRef $ \cat →
                            case lookupSpecies fid cat of
                                Just species →
                                    let key = AnnualCycleKey pTag cTag
                                        species' = species
                                            { fsCycleOverrides =
                                                HM.insert key tex
                                                    (fsCycleOverrides species) }
                                    in (insertSpecies fid species' cat, ())
                                Nothing → (cat, ())
                    _ → pure ()
                return 0
        _ → return 0

-----------------------------------------------------------
-- flora.registerForWorldGen(floraId, category,
--     minTemp, maxTemp, minPrecip, maxPrecip,
--     maxSlope, density)
-----------------------------------------------------------

floraRegisterForWorldGenFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
floraRegisterForWorldGenFn env = do
    fidArg      ← Lua.tointeger 1
    catArg      ← Lua.tostring 2
    minTempArg  ← Lua.tonumber 3
    maxTempArg  ← Lua.tonumber 4
    minPrecArg  ← Lua.tonumber 5
    maxPrecArg  ← Lua.tonumber 6
    maxSlopeArg ← Lua.tointeger 7
    densityArg  ← Lua.tonumber 8

    case (fidArg, catArg) of
        (Just fidInt, Just catBS) → withFloraCatalog env $ \catRef → do
            let fid = FloraId (fromIntegral fidInt)
                category = TE.decodeUtf8 catBS
                minTemp  = luaNum minTempArg (-40.0)
                maxTemp  = luaNum maxTempArg 50.0
                minPrec  = luaNum minPrecArg 0.0
                maxPrec  = luaNum maxPrecArg 1.0
                maxSlope = maybe 15 fromIntegral maxSlopeArg
                density  = luaNum densityArg 0.1

                wg = FloraWorldGen
                    { fwCategory  = category
                    , fwMinTemp   = minTemp
                    , fwMaxTemp   = maxTemp
                    , fwMinPrecip = minPrec
                    , fwMaxPrecip = maxPrec
                    , fwMaxSlope  = maxSlope
                    , fwDensity   = density
                    , fwSoils     = []
                    }

            Lua.liftIO $ atomicModifyIORef' catRef $ \cat →
                (insertWorldGen fid wg cat, ())
            return 0
        _ → return 0

-----------------------------------------------------------
-- Parsers
-----------------------------------------------------------

parsePhaseTag ∷ Text → Maybe LifePhaseTag
parsePhaseTag "sprout"     = Just PhaseSprout
parsePhaseTag "seedling"   = Just PhaseSeedling
parsePhaseTag "vegetating" = Just PhaseVegetating
parsePhaseTag "budding"    = Just PhaseBudding
parsePhaseTag "flowering"  = Just PhaseFlowering
parsePhaseTag "ripening"   = Just PhaseRipening
parsePhaseTag "matured"    = Just PhaseMatured
parsePhaseTag "withering"  = Just PhaseWithering
parsePhaseTag "dead"       = Just PhaseDead
parsePhaseTag _            = Nothing

parseCycleTag ∷ Text → Maybe AnnualStageTag
parseCycleTag "dormant"   = Just CycleDormant
parseCycleTag "budding"   = Just CycleBudding
parseCycleTag "flowering" = Just CycleFlowering
parseCycleTag "fruiting"  = Just CycleFruiting
parseCycleTag "senescing" = Just CycleSenescing
parseCycleTag _           = Nothing

luaNum ∷ Maybe Lua.Number → Float → Float
luaNum (Just (Lua.Number n)) _ = realToFrac n
luaNum _                     d = d
