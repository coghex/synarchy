{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
module Engine.Scripting.Lua.API.Locations
    ( loadLocationYamlFn
    , locationListDefsFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Control.Monad (foldM, forM_)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Asset.YamlLocations
import Location.Types

-- | engine.loadLocationYaml(path) — parses a YAML file of location
--   defs, registers each into the LocationRegistry, returns the count.
--   Mirrors engine.loadBuildingYaml / engine.loadSubstanceYaml.
--   Locations load LAST at boot (after items / units / buildings) so a
--   future cross-registry validation pass (#90) can resolve content ids.
loadLocationYamlFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
loadLocationYamlFn env = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadLocationYaml logger filePath
                total ← foldM (\acc d → do
                    let def = LocationDef
                            { ldId       = lydId d
                            , ldLabel    = if T.null (lydLabel d)
                                           then lydId d else lydLabel d
                            , ldType     = lydType d
                            , ldBuilder  = lydBuilder d
                            , ldAnchor   = lydAnchor d
                            , ldContents = map toContent (lydContents d)
                            }
                    atomicModifyIORef' (locationDefsRef env) $ \reg →
                        (registerLocation def reg, ())
                    return (acc + 1)
                    ) (0 ∷ Int) defs
                logInfo logger CatAsset $
                    "loadLocationYaml: loaded " <> T.pack (show total)
                    <> " locations from " <> T.pack filePath
                return total
            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1
  where
    toContent c = LocationContent
        { lconKind  = lycKind c
        , lconId    = lycId c
        , lconCount = lycCount c
        }

-- | engine.listLocationDefs() → array of location def tables, in
--   registration order. Each entry:
--     { id, label, type, builder,
--       anchor   = { tag, … },
--       contents = { { kind, id, count }, … } }
--   The Lua `locations` module wraps this as locations.listDefs().
locationListDefsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
locationListDefsFn env = do
    defs ← Lua.liftIO $ allLocations <$> readIORef (locationDefsRef env)
    Lua.newtable
    forM_ (zip [1..] defs) $ \(i, d) → do
        Lua.newtable
        Lua.pushstring (TE.encodeUtf8 (ldId d))
        Lua.setfield (-2) "id"
        Lua.pushstring (TE.encodeUtf8 (ldLabel d))
        Lua.setfield (-2) "label"
        Lua.pushstring (TE.encodeUtf8 (ldType d))
        Lua.setfield (-2) "type"
        Lua.pushstring (TE.encodeUtf8 (ldBuilder d))
        Lua.setfield (-2) "builder"
        -- anchor: array of tag strings
        Lua.newtable
        forM_ (zip [1..] (ldAnchor d)) $ \(j, tag) → do
            Lua.pushstring (TE.encodeUtf8 tag)
            Lua.rawseti (-2) j
        Lua.setfield (-2) "anchor"
        -- contents: array of {kind, id, count}
        Lua.newtable
        forM_ (zip [1..] (ldContents d)) $ \(j, c) → do
            Lua.newtable
            Lua.pushstring (TE.encodeUtf8 (lconKind c))
            Lua.setfield (-2) "kind"
            Lua.pushstring (TE.encodeUtf8 (lconId c))
            Lua.setfield (-2) "id"
            Lua.pushinteger (fromIntegral (lconCount c))
            Lua.setfield (-2) "count"
            Lua.rawseti (-2) j
        Lua.setfield (-2) "contents"
        Lua.rawseti (-2) i
    return 1
