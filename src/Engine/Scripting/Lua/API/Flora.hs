{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Flora
    ( floraRegisterFn
    , floraAddPhaseFn
    , floraAddSeasonFn
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
-- Helper: get the wsFloraCatalogRef from the first world
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
-- Lua usage:
--   local tex = engine.loadTexture("assets/.../oak/default.png")
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
-- flora.addPhase(floraId, phaseTag, textureHandle, age)
--
-- Lua usage:
--   local tex = engine.loadTexture("assets/.../oak/sapling.png")
--   flora.addPhase(oak, "sprout", tex, 0)
--   flora.addPhase(oak, "matured", tex2, 30.0)
--
-- phaseTag: "sprout", "seedling", "vegetating", "budding",
--   "flowering", "ripening", "matured", "withering", "dead"
-- age: game-days (float), minimum age to enter this phase.
--   Optional, defaults to 0.
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
-- flora.addSeason(floraId, phaseTag, season, textureHandle)
--
-- Lua usage:
--   local tex = engine.loadTexture("assets/.../oak/budding_spring.png")
--   flora.addSeason(oak, "budding", "spring", tex)
--
-- season: "spring", "summer", "autumn"/"fall", "winter"
-- The phase must already have been added via addPhase.
-----------------------------------------------------------

floraAddSeasonFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
floraAddSeasonFn env = do
    fidArg    ← Lua.tointeger 1
    tagArg    ← Lua.tostring 2
    seasonArg ← Lua.tostring 3
    texArg    ← Lua.tointeger 4

    case (fidArg, tagArg, seasonArg, texArg) of
        (Just fidInt, Just tagBS, Just seasonBS, Just texInt) →
            withFloraCatalog env $ \catRef → do
                let fid = FloraId (fromIntegral fidInt)
                    tagText = TE.decodeUtf8 tagBS
                    seasonText = TE.decodeUtf8 seasonBS
                    tex = TextureHandle (fromIntegral texInt)

                case (parsePhaseTag tagText, parseSeason seasonText) of
                    (Just tag, Just season) →
                        Lua.liftIO $ atomicModifyIORef' catRef $ \cat →
                            case lookupSpecies fid cat of
                                Just species →
                                    let key = SeasonKey tag season
                                        species' = species
                                            { fsSeasonOverride =
                                                HM.insert key tex
                                                    (fsSeasonOverride species) }
                                    in (insertSpecies fid species' cat, ())
                                Nothing → (cat, ())
                    _ → pure ()
                return 0
        _ → return 0

-----------------------------------------------------------
-- flora.registerForWorldGen(floraId, category,
--     minTemp, maxTemp, minPrecip, maxPrecip,
--     maxSlope, density)
--
-- Lua usage:
--   flora.registerForWorldGen(oak, "tree",
--       2, 30, 0.3, 1.0, 2, 0.3)
--
-- All biome parameters are positional.
-- soils list not yet supported (empty = all non-barren).
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
-- Helpers
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

parseSeason ∷ Text → Maybe Season
parseSeason "spring" = Just Spring
parseSeason "summer" = Just Summer
parseSeason "autumn" = Just Autumn
parseSeason "fall"   = Just Autumn
parseSeason "winter" = Just Winter
parseSeason _        = Nothing

luaNum ∷ Maybe Lua.Number → Float → Float
luaNum (Just (Lua.Number n)) _ = realToFrac n
luaNum _                     d = d
