{-# LANGUAGE Strict #-}
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
import Engine.Core.Log (LogCategory(..), logDebug, logWarn)
import Engine.Core.Log.Monad (getLoggerFor)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Graphics.Vulkan.Texture.Policy (UploadSampler(..))
import Engine.Scripting.Lua.API.YamlTextures
    (isTextureNameRegistered, loadAndRegister, resolveTexturePath)
import Engine.Asset.YamlLocations
import Language.Semantic.Types (ConceptId(..), catalogueErrorText)
import Language.Semantic.Catalogue ( conceptCataloguePath
                                   , conceptOrdinalPath, loadCatalogue )
import Location.Anchor (locationAnchorText)
import Location.Naming (locationNamingErrors)
import Location.Types
import Location.Bounds (RelBounds(..))

-- | Fallback texture substituted when a location def's declared
--   @map_icon@ path doesn't exist on disk (#781) — the same generic
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
                -- #1101: every definition's authored naming scheme is
                -- checked against the concept catalogue BEFORE anything
                -- is registered, so a bad scheme rejects the whole file
                -- (the same all-or-nothing outcome 'loadYamlList' gives
                -- a parse failure) instead of registering a location
                -- whose names would silently fall back to its label in
                -- every world forever.
                namingErrs ← if null defs then pure [] else do
                    eCat ← loadCatalogue conceptCataloguePath
                                         conceptOrdinalPath
                    pure $ case eCat of
                        Left err → [ "concept catalogue "
                                     <> T.pack conceptCataloguePath
                                     <> " could not be loaded, so location "
                                     <> "naming schemes cannot be validated: "
                                     <> catalogueErrorText err ]
                        Right cat → concatMap (locationNamingErrors cat . toDef)
                                              defs
                case namingErrs of
                  (_:_) → do
                    forM_ namingErrs $ \e → logWarn logger CatAsset $
                        "loadLocationYaml: rejected " <> T.pack filePath
                        <> ": " <> e
                    return (0 ∷ Int)
                  [] → do
                    let (lteq, _) = lbsMsgQueues backendState
                    -- The ONE shared unknown-location marker (#1230),
                    -- registered ONCE per session and independently of
                    -- every definition: it belongs to no location type,
                    -- so it is loaded whether or not any def in this
                    -- file (or any file) declares a type icon. Guarded
                    -- by the registry itself so a second location YAML
                    -- doesn't queue a second GPU upload of it.
                    alreadyLoaded ← isTextureNameRegistered env
                                        locationUnknownIconTextureName
                    unless alreadyLoaded $ do
                        unknownResolved ← resolveTexturePath env
                            "Location unknown map icon"
                            missingLocationIconTexture
                            locationUnknownIconPath
                        void $ loadAndRegister env backendState lteq UploadGlobalSampler
                            locationUnknownIconTextureName unknownResolved
                    total ← foldM (\acc d → do
                        -- Register + queue the def's own zoom-map TYPE
                        -- icon (#781), if it declares one. Named via
                        -- 'locationIconTextureName' so 'World.Render.Zoom.
                        -- Icons' can look it back up by the same
                        -- convention at render time. There is no
                        -- per-definition unknown icon any more (#1230).
                        forM_ (lydMapIcon d) $ \iconPath → do
                            resolved ← resolveTexturePath env
                                "Location map icon" missingLocationIconTexture
                                (T.unpack iconPath)
                            void $ loadAndRegister env backendState lteq UploadGlobalSampler
                                (locationIconTextureName (lydId d)) resolved
                        atomicModifyIORef' (crLocationDefsRef regs) $ \reg →
                            (registerLocation (toDef d) reg, ())
                        return (acc + 1)
                        ) (0 ∷ Int) defs
                    logDebug logger CatAsset $
                        "loadLocationYaml: loaded " <> tshow total
                        <> " locations from " <> T.pack filePath
                    return total
            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1
  where
    toDef d = LocationDef
        { ldId         = lydId d
        , ldLabel      = if T.null (lydLabel d) then lydId d else lydLabel d
        , ldType       = lydType d
        , ldBuilder    = lydBuilder d
        , ldAnchor     = lydAnchor d
        , ldMaxCount   = lydMaxCount d
        , ldMinSpacing = lydMinSpacing d
        , ldContents   = map toContent (lydContents d)
        , ldBounds     = toBounds (lydBounds d)
        , ldMapIcon    = lydMapIcon d
        , ldNaming     = toNaming (lydNaming d)
        }
    toNaming n = LocationNaming
        { lnHeads     = map ConceptId (lynHeads n)
        , lnModifiers = map ConceptId (lynModifiers n)
        }
    toContent c = LocationContent
        { lconKind     = lycKind c
        , lconId       = lycId c
        , lconCount    = lycCount c
        , lconPosition = (\p → (lypX p, lypY p)) ⊚ lycPosition c
        , lconFaction  = lycFaction c
        , lconRolls    = lycRolls c
        , lconCountRange = (\r → (lycrMin r, lycrMax r)) ⊚ lycCountRange c
        , lconClearance = lycClearance c
        , lconSignificant = lycSignificant c
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
--       max_count = number, min_spacing = number,   -- placement knobs (#997)
--       contents = { { kind, id, count, rolls,
--                      position = {x,y} | nil,
--                      faction  = string | nil }, … } }
--   `position` / `faction` fields are OMITTED (not set to a Lua nil
--   value) when absent, so `entry.position` reads as nil either way.
--   `bounds` is always present — every def loads with a required,
--   validated spatial contract (#777). #1230 removed
--   `discovery_margin`: reveal is sight-based, and `bounds` is the only
--   location footprint left.
--   `max_count` is reported RAW, exactly as authored: a value of 0 (or
--   below) is an explicit "do not place", and it is the only thing that
--   lets a caller tell an empty placed-location list caused by an
--   authored no-placement content set apart from one caused by a world
--   with no land — the distinction Create World reports on (#997).
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
        -- placement knobs (#997), raw as authored
        Lua.pushinteger (fromIntegral (ldMaxCount d))
        Lua.setfield (-2) "max_count"
        Lua.pushinteger (fromIntegral (ldMinSpacing d))
        Lua.setfield (-2) "min_spacing"
        -- anchor: array of tag strings, in the def's authored order.
        -- The values are the closed #1681 vocabulary, rendered back
        -- through the type's own total spelling map, so Lua still sees
        -- exactly the strings the YAML authored.
        Lua.newtable
        forM_ (zip [1..] (ldAnchor d)) $ \(j, tag) → do
            Lua.pushstring (TE.encodeUtf8 (locationAnchorText tag))
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
            case lconCountRange c of
                Just (lo, hi) → do
                    Lua.newtable
                    Lua.pushinteger (fromIntegral lo)
                    Lua.setfield (-2) "min"
                    Lua.pushinteger (fromIntegral hi)
                    Lua.setfield (-2) "max"
                    Lua.setfield (-2) "count_range"
                Nothing → return ()
            forM_ (lconClearance c) $ \policy → do
                Lua.pushstring (TE.encodeUtf8 policy)
                Lua.setfield (-2) "clearance"
            -- #917: always present, never omitted — a content entry is
            -- either a guaranteed significant item the location's
            -- clearance predicate waits on, or it is not, and
            -- scripts/locations.lua reads this to decide which spawn
            -- path an entry takes.
            Lua.pushboolean (lconSignificant c)
            Lua.setfield (-2) "significant"
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
