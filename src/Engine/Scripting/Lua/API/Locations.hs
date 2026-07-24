{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Lua surface for the location-def catalogue (#88/#90).
--
--   Narrowed to the @content-registries@ capability (#890, epic #537):
--   the location registry is reached only through
--   'ContentRegistriesCapability' and the logger only through
--   'CoreCapability'. 'loadLocationYamlFn' still takes an 'EngineEnv',
--   but purely as the opaque token the not-yet-narrowed
--   @render-gpu-asset@ texture helpers ('resolveTexturePath',
--   'loadAndRegister') demand — this module dereferences no 'EngineEnv'
--   field itself, and that parameter goes away when @render-gpu-asset@
--   migrates (SS7.2).
module Engine.Scripting.Lua.API.Locations
    ( loadLocationYamlFn
    , locationListDefsFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Core (CoreCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..))
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Core.Log.Monad (getLoggerFor)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister, resolveTexturePath)
import Engine.Asset.YamlLocations
import Location.Types
import Location.Bounds (RelBounds(..))

-- | Fallback texture substituted when a location def's declared
--   @map_icons@ path doesn't exist on disk (#781) — the same generic
--   undefined-texture placeholder 'Engine.Scripting.Lua.API.YamlTextures'
--   already substitutes for a missing material texture, logged via
--   'resolveTexturePath' rather than failing the whole YAML load.
missingLocationIconTexture ∷ FilePath
missingLocationIconTexture = "assets/textures/utility/notexture.png"

-- | engine.loadLocationYaml(path) — parses a YAML file of location
--   defs, registers each into the LocationRegistry, returns the count.
--   Mirrors engine.loadBuildingYaml / engine.loadSubstanceYaml.
--   Locations load LAST at boot (after items / units / buildings) so a
--   future cross-registry validation pass (#90) can resolve content ids.
--   Callable repeatedly; each call inserts/replaces by def id.
loadLocationYamlFn ∷ CoreCapability → ContentRegistriesCapability
                   → EngineEnv → LuaBackendState
                   → Lua.LuaE Lua.Exception Lua.NumResults
loadLocationYamlFn core regs env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
            count ← Lua.liftIO $ do
                logger ← getLoggerFor core
                defs ← loadLocationYaml logger filePath
                let (lteq, _) = lbsMsgQueues backendState
                total ← foldM (\acc d → do
                    -- Register + queue the def's paired zoom-map icon
                    -- textures (#781), if it declares any. Named via
                    -- 'locationIconTextureName' so 'World.Render.Zoom.
                    -- Icons' can look them back up by the same
                    -- convention at render time.
                    forM_ (lydMapIcons d) $ \(undiscPath, discPath) → do
                        undiscResolved ← resolveTexturePath env
                            "Location map icon" missingLocationIconTexture
                            (T.unpack undiscPath)
                        discResolved ← resolveTexturePath env
                            "Location map icon" missingLocationIconTexture
                            (T.unpack discPath)
                        void $ loadAndRegister env backendState lteq
                            (locationIconTextureName (lydId d) False)
                            undiscResolved
                        void $ loadAndRegister env backendState lteq
                            (locationIconTextureName (lydId d) True)
                            discResolved
                    let def = LocationDef
                            { ldId         = lydId d
                            , ldLabel      = if T.null (lydLabel d)
                                             then lydId d else lydLabel d
                            , ldType       = lydType d
                            , ldBuilder    = lydBuilder d
                            , ldAnchor     = lydAnchor d
                            , ldMaxCount   = lydMaxCount d
                            , ldMinSpacing = lydMinSpacing d
                            , ldContents   = map toContent (lydContents d)
                            , ldBounds     = toBounds (lydBounds d)
                            , ldDiscoveryMargin = lydDiscoveryMargin d
                            , ldMapIcons   = lydMapIcons d
                            }
                    atomicModifyIORef' (crLocationDefsRef regs) $ \reg →
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
        { lconKind     = lycKind c
        , lconId       = lycId c
        , lconCount    = lycCount c
        , lconPosition = (\p → (lypX p, lypY p)) ⊚ lycPosition c
        , lconFaction  = lycFaction c
        , lconRolls    = lycRolls c
        }
    toBounds b = RelBounds
        { rbMinX = lybMinX b, rbMinY = lybMinY b
        , rbMaxX = lybMaxX b, rbMaxY = lybMaxY b
        }

-- | engine.listLocationDefs() → array of location def tables, in
--   registration order. Each entry:
--     { id, label, type, builder,
--       anchor   = { tag, … },
--       bounds   = { min_x, min_y, max_x, max_y },  -- relative to anchor (#777)
--       discovery_margin = number,
--       contents = { { kind, id, count, rolls,
--                      position = {x,y} | nil,
--                      faction  = string | nil }, … } }
--   `position` / `faction` fields are OMITTED (not set to a Lua nil
--   value) when absent, so `entry.position` reads as nil either way.
--   `bounds` / `discovery_margin` are always present — every def loads
--   with a required, validated spatial contract (#777).
--   The Lua `locations` module wraps this as locations.listDefs().
locationListDefsFn ∷ ContentRegistriesCapability
                   → Lua.LuaE Lua.Exception Lua.NumResults
locationListDefsFn regs = do
    defs ← Lua.liftIO $ allLocations <$> readIORef (crLocationDefsRef regs)
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
        -- bounds: relative inclusive tile box (#777)
        Lua.newtable
        Lua.pushinteger (fromIntegral (rbMinX (ldBounds d)))
        Lua.setfield (-2) "min_x"
        Lua.pushinteger (fromIntegral (rbMinY (ldBounds d)))
        Lua.setfield (-2) "min_y"
        Lua.pushinteger (fromIntegral (rbMaxX (ldBounds d)))
        Lua.setfield (-2) "max_x"
        Lua.pushinteger (fromIntegral (rbMaxY (ldBounds d)))
        Lua.setfield (-2) "max_y"
        Lua.setfield (-2) "bounds"
        Lua.pushinteger (fromIntegral (ldDiscoveryMargin d))
        Lua.setfield (-2) "discovery_margin"
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
            Lua.pushinteger (fromIntegral (lconRolls c))
            Lua.setfield (-2) "rolls"
            case lconPosition c of
                Just (px, py) → do
                    Lua.newtable
                    Lua.pushinteger (fromIntegral px)
                    Lua.setfield (-2) "x"
                    Lua.pushinteger (fromIntegral py)
                    Lua.setfield (-2) "y"
                    Lua.setfield (-2) "position"
                Nothing → return ()
            case lconFaction c of
                Just fac → do
                    Lua.pushstring (TE.encodeUtf8 fac)
                    Lua.setfield (-2) "faction"
                Nothing → return ()
            Lua.rawseti (-2) j
        Lua.setfield (-2) "contents"
        Lua.rawseti (-2) i
    return 1
