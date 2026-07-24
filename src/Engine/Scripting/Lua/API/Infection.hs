{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua surface for the infection catalogue. engine.loadInfectionYaml loads
--   data/infections/*.yaml into the InfectionManager (mirrors
--   engine.loadSubstanceYaml); infection.get / infection.getNames give
--   read-only access for debug + the probe.
--
--   Narrowed to the @content-registries@ capability (#890, epic #537):
--   the infection catalogue is reached only through
--   'ContentRegistriesCapability' and the logger only through
--   'CoreCapability', so this module never touches an 'EngineEnv'.
module Engine.Scripting.Lua.API.Infection
    ( loadInfectionYamlFn
    , infectionGetFn
    , infectionGetNamesFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.Capability.Core (CoreCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..))
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Core.Log.Monad (getLoggerFor)
import Engine.Asset.YamlInfection
import Infection.Types

-- | engine.loadInfectionYaml(path) — parse a YAML file of infection defs,
--   register each into the InfectionManager, return the count.
--   Callable repeatedly; each call inserts/replaces by def id.
loadInfectionYamlFn ∷ CoreCapability → ContentRegistriesCapability
                    → Lua.LuaE Lua.Exception Lua.NumResults
loadInfectionYamlFn core regs = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → Lua.pushnumber 0 >> return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            count ← Lua.liftIO $ do
                logger ← getLoggerFor core
                defs ← loadInfectionYaml logger filePath
                total ← foldM (\acc d → do
                    let inf = InfectionDef
                            { infId             = iyId d
                            , infName           = iyName d
                            , infIcon           = iyIcon d
                            , infCategory       = iyCategory d
                            , infSites          = iySites d
                            , infBaseWeight     = iyBaseWeight d
                            , infTempMin        = iyTempMin d
                            , infTempMax        = iyTempMax d
                            , infMoistMin       = iyMoistMin d
                            , infMoistMax       = iyMoistMax d
                            , infAggressiveness = iyAggressiveness d
                            , infInfectability  = iyInfectability d
                            , infCurableBy      = iyCurableBy d
                            , infCureRate       = iyCureRate d
                            , infWoundInfectable = iyWoundInfectable d
                            , infEffects        = iyEffects d
                            , infTransmissibility = iyTransmissibility d
                            , infTransmission   = iyTransmission d
                            }
                    atomicModifyIORef' (crInfectionManagerRef regs) $ \m →
                        (InfectionManager
                            { infmDefs = HM.insert (iyId d) inf
                                                   (infmDefs m) }, ())
                    return (acc + 1)
                    ) (0 ∷ Int) defs
                logInfo logger CatAsset $
                    "loadInfectionYaml: loaded " <> T.pack (show total)
                    <> " infections from " <> T.pack filePath
                return total
            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

-- | infection.get(id) → table | nil. Read-only access to one def.
infectionGetFn ∷ ContentRegistriesCapability
               → Lua.LuaE Lua.Exception Lua.NumResults
infectionGetFn regs = do
    idArg ← Lua.tostring 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just idBS → do
            let key = TE.decodeUtf8Lenient idBS
            mDef ← Lua.liftIO $ do
                m ← readIORef (crInfectionManagerRef regs)
                pure (lookupInfection key m)
            case mDef of
                Nothing → Lua.pushnil >> return 1
                Just d → do
                    Lua.newtable
                    let putS k v = Lua.pushstring (TE.encodeUtf8 v)
                                   >> Lua.setfield (-2) k
                        putN k v = Lua.pushnumber (Lua.Number (realToFrac v))
                                   >> Lua.setfield (-2) k
                        putB k v = Lua.pushboolean v >> Lua.setfield (-2) k
                    putS "id"            (infId d)
                    putS "name"          (infName d)
                    putS "icon"          (infIcon d)
                    putS "category"      (infCategory d)
                    putN "baseWeight"    (infBaseWeight d)
                    putN "aggressiveness" (infAggressiveness d)
                    putN "infectability" (infInfectability d)
                    putN "cureRate"      (infCureRate d)
                    putB "woundInfectable" (infWoundInfectable d)
                    putN "transmissibility" (infTransmissibility d)
                    return 1

-- | infection.getNames() → array of all loaded def ids.
infectionGetNamesFn ∷ ContentRegistriesCapability
                    → Lua.LuaE Lua.Exception Lua.NumResults
infectionGetNamesFn regs = do
    m ← Lua.liftIO $ readIORef (crInfectionManagerRef regs)
    Lua.newtable
    forM_ (zip [1..] (HM.keys (infmDefs m))) $ \(i, k) → do
        Lua.pushstring (TE.encodeUtf8 k)
        Lua.rawseti (-2) i
    return 1
