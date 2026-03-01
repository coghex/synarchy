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
import World.Flora.Types

-----------------------------------------------------------
-- flora.register(name, baseTextureHandle) → floraId
-----------------------------------------------------------

floraRegisterFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
floraRegisterFn env = do
    nameArg ← Lua.tostring 1
    texArg  ← Lua.tointeger 2

    case (nameArg, texArg) of
        (Just nameBS, Just texInt) → do
            let name = TE.decodeUtf8 nameBS
                baseTex = TextureHandle (fromIntegral texInt)
                catRef = floraCatalogRef env

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
-----------------------------------------------------------

floraSetLifecycleFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
floraSetLifecycleFn env = do
    fidArg   ← Lua.tointeger 1
    typeArg  ← Lua.tostring 2
    arg3     ← Lua.tonumber 3
    arg4     ← Lua.tonumber 4
    arg5     ← Lua.tonumber 5

    case (fidArg, typeArg) of
        (Just fidInt, Just typeBS) → do
            let fid = FloraId (fromIntegral fidInt)
                typeText = TE.decodeUtf8 typeBS
                catRef = floraCatalogRef env
                lifecycle = case typeText of
                    "perennial" →
                        let minL = luaNum arg3 1800.0
                            maxL = luaNum arg4 3600.0
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
-----------------------------------------------------------

floraAddPhaseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
floraAddPhaseFn env = do
    fidArg ← Lua.tointeger 1
    tagArg ← Lua.tostring 2
    texArg ← Lua.tointeger 3
    ageArg ← Lua.tonumber 4

    case (fidArg, tagArg, texArg) of
        (Just fidInt, Just tagBS, Just texInt) → do
            let fid = FloraId (fromIntegral fidInt)
                tagText = TE.decodeUtf8 tagBS
                tex = TextureHandle (fromIntegral texInt)
                catRef = floraCatalogRef env
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
-----------------------------------------------------------

floraAddCycleStageFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
floraAddCycleStageFn env = do
    fidArg   ← Lua.tointeger 1
    tagArg   ← Lua.tostring 2
    dayArg   ← Lua.tointeger 3
    texArg   ← Lua.tointeger 4

    case (fidArg, tagArg, dayArg, texArg) of
        (Just fidInt, Just tagBS, Just dayInt, Just texInt) → do
            let fid = FloraId (fromIntegral fidInt)
                tagText = TE.decodeUtf8 tagBS
                tex = TextureHandle (fromIntegral texInt)
                day = fromIntegral dayInt
                catRef = floraCatalogRef env

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
-----------------------------------------------------------

floraAddCycleOverrideFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
floraAddCycleOverrideFn env = do
    fidArg   ← Lua.tointeger 1
    phaseArg ← Lua.tostring 2
    cycleArg ← Lua.tostring 3
    texArg   ← Lua.tointeger 4

    case (fidArg, phaseArg, cycleArg, texArg) of
        (Just fidInt, Just phaseBS, Just cycleBS, Just texInt) → do
            let fid = FloraId (fromIntegral fidInt)
                phaseText = TE.decodeUtf8 phaseBS
                cycleText = TE.decodeUtf8 cycleBS
                tex = TextureHandle (fromIntegral texInt)
                catRef = floraCatalogRef env

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
        (Just fidInt, Just catBS) → do
            let fid = FloraId (fromIntegral fidInt)
                category = TE.decodeUtf8 catBS
                catRef = floraCatalogRef env
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
