{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units
    ( loadUnitYamlFn
    , unitSpawnFn
    , unitDestroyFn
    , unitSetPosFn
    , unitMoveToFn
    , unitStopFn
    , unitGetPosFn
    , unitGetInfoFn
    , unitListFn
    , unitListDefsFn
    , unitSelectFn
    , unitDeselectAllFn
    , unitGetSelectedFn
    , unitIsSelectedFn
    , unitHitTestAtFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import Control.Monad (foldM, forM_)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo, logDebug, logWarn)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister)
import Engine.Asset.YamlUnits (UnitYamlDef(..), loadUnitYaml)
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Command.Types (UnitCommand(..))
import Unit.Direction (Direction(..))
import qualified Unit.Selection as Sel
import qualified Unit.HitTest as HitTest
import World.Types (WorldManager(..), WorldState(..), WorldTileData(..),
                    LoadedChunk(..), ChunkCoord(..), columnIndex, lookupChunk)
import World.Generate (globalToChunk)

-- * YAML loading

loadUnitYamlFn ∷ EngineEnv → LuaBackendState
               → Lua.LuaE Lua.Exception Lua.NumResults
loadUnitYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadUnitYaml logger filePath

                let (lteq, _) = lbsMsgQueues backendState

                total ← foldM (\acc def → do
                    let name      = uydName def
                        spritePath = T.unpack (uydSprite def)

                    handle ← loadAndRegister env backendState lteq
                                 ("unit_" <> name) spritePath

                    -- Load directional sprites (if any)
                    dirMap ← foldM (\acc (dirKey, texPath) →
                        case parseDirKey dirKey of
                            Nothing → do
                                logWarn logger CatAsset $
                                    "Unknown direction key '" <> dirKey
                                    <> "' in unit " <> name <> ", skipping"
                                return acc
                            Just dir → do
                                h ← loadAndRegister env backendState lteq
                                        ("unit_" <> name <> "_" <> dirKey)
                                        (T.unpack texPath)
                                return (Map.insert dir h acc)
                        ) Map.empty (Map.toList (uydDirectionalSprites def))

                    let unitDef = UnitDef
                            { udName       = name
                            , udTexture    = handle
                            , udDirSprites = dirMap
                            , udBaseWidth  = uydBaseWidth def
                            }
                    atomicModifyIORef' (unitManagerRef env) $ \um →
                        (um { umDefs = HM.insert name unitDef (umDefs um) }, ())

                    logDebug logger CatAsset $
                        "Registered unit def: " <> name
                        <> " (handle " <> T.pack (show handle) <> ")"
                        <> " (" <> T.pack (show (Map.size dirMap))
                        <> " directional sprites)"

                    return (acc + 1)
                    ) (0 ∷ Int) defs

                logInfo logger CatAsset $
                    "loadUnitYaml: loaded " <> T.pack (show total)
                    <> " unit definitions from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

lookupSurfaceZ ∷ EngineEnv → Int → Int → IO (Maybe Int)
lookupSurfaceZ env gx gy = do
    wm ← readIORef (worldManagerRef env)
    go (wmVisible wm) (wmWorlds wm)
  where
    (chunkCoord, (lx, ly)) = globalToChunk gx gy
    go [] _ = return Nothing
    go (pageId:rest) worlds =
        case lookup pageId worlds of
            Nothing → go rest worlds
            Just ws → do
                td ← readIORef (wsTilesRef ws)
                case lookupChunk chunkCoord td of
                    Just lc → return $ Just ((lcSurfaceMap lc) VU.! columnIndex lx ly)
                    Nothing → go rest worlds

-- | Spawn a unit. If gridZ is omitted, looks up surface elevation.
--   Falls back to Z=0 if chunk isn't loaded. Returns unit ID or -1.
unitSpawnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSpawnFn env = do
    nameArg ← Lua.tostring 1
    xArg    ← Lua.tonumber 2
    yArg    ← Lua.tonumber 3
    zArg    ← Lua.tointeger 4   -- optional

    case nameArg of
        Nothing → do
            Lua.pushnumber (-1)
            return 1
        Just nameBS → do
            let name = TE.decodeUtf8 nameBS
                gx = case xArg of
                         Just (Lua.Number n) → realToFrac n
                         _                   → 0.0
                gy = case yArg of
                         Just (Lua.Number n) → realToFrac n
                         _                   → 0.0

            result ← Lua.liftIO $ do
                -- Check def exists
                um ← readIORef (unitManagerRef env)
                case HM.lookup name (umDefs um) of
                    Nothing → return (-1)
                    Just _ → do
                        -- Resolve Z
                        gz ← case zArg of
                            Just n  → return (fromIntegral n)
                            Nothing → do
                                let gxi = floor gx ∷ Int
                                    gyi = floor gy ∷ Int
                                mSurf ← lookupSurfaceZ env gxi gyi
                                case mSurf of
                                    Just z  → return z
                                    Nothing → do
                                        logger ← readIORef (loggerRef env)
                                        logWarn logger CatAsset $
                                            "unit.spawn: chunk not loaded at ("
                                            <> T.pack (show gxi) <> ", "
                                            <> T.pack (show gyi)
                                            <> "), defaulting Z=0"
                                        return 0

                        -- Allocate ID
                        uid ← atomicModifyIORef' (unitManagerRef env) $ \um' →
                            let (uid', um'') = nextUnitId um'
                            in (um'', uid')

                        -- Enqueue spawn command
                        Q.writeQueue (unitQueue env) $
                            UnitSpawn uid name gx gy gz

                        return (fromIntegral (unUnitId uid) ∷ Int)

            Lua.pushnumber (Lua.Number (fromIntegral result))
            return 1

unitDestroyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDestroyFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitDestroy uid
            Lua.pushboolean True
            return 1

-- | Teleport a unit. If gridZ is omitted, looks up surface elevation.
unitSetPosFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetPosFn env = do
    idArg ← Lua.tointeger 1
    xArg  ← Lua.tonumber 2
    yArg  ← Lua.tonumber 3
    zArg  ← Lua.tointeger 4

    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                gx = case xArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                gy = case yArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                mGz = case zArg of
                         Just z  → Just (fromIntegral z)
                         Nothing → Nothing
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitTeleport uid gx gy mGz
            Lua.pushboolean True
            return 1

-- | Order a unit to walk to a target. Speed defaults to 2.0 tiles/sec.
unitMoveToFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitMoveToFn env = do
    idArg    ← Lua.tointeger 1
    xArg     ← Lua.tonumber 2
    yArg     ← Lua.tonumber 3
    speedArg ← Lua.tonumber 4

    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                tx = case xArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                ty = case yArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                speed = case speedArg of
                            Just (Lua.Number v) → realToFrac v
                            _                   → 2.0
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitMoveTo uid tx ty speed
            Lua.pushboolean True
            return 1

unitStopFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitStopFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitStop uid
            Lua.pushboolean True
            return 1

unitGetPosFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPosFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            um ← Lua.liftIO $ readIORef (unitManagerRef env)
            case HM.lookup uid (umInstances um) of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just inst → do
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridX inst)))
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridY inst)))
                    Lua.pushnumber (Lua.Number (fromIntegral (uiGridZ inst)))
                    return 3

unitListFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitListFn env = do
    result ← Lua.liftIO $ do
        um ← readIORef (unitManagerRef env)
        let entries = HM.toList (umInstances um)
        if null entries
        then return "No units spawned"
        else return $ T.unpack $ T.intercalate "\n" $
            map (\(uid, inst) →
                "id=" <> T.pack (show (unUnitId uid))
                <> " " <> uiDefName inst
                <> " (" <> T.pack (show (uiGridX inst))
                <> ", " <> T.pack (show (uiGridY inst))
                <> ", " <> T.pack (show (uiGridZ inst)) <> ")"
            ) entries
    Lua.pushstring (TE.encodeUtf8 (T.pack result))
    return 1

-- | unit.listDefs() — Lua array of available unit definition names.
--   These are the keys loadable into `unit.spawn(name, ...)`.
unitListDefsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitListDefsFn env = do
    names ← Lua.liftIO $ do
        um ← readIORef (unitManagerRef env)
        return $ HM.keys (umDefs um)
    Lua.newtable
    forM_ (zip [1..] names) $ \(i, name) → do
        Lua.pushstring (TE.encodeUtf8 name)
        Lua.rawseti (-2) i
    return 1

-- | unit.getInfo(id) — returns a Lua table with the unit's render-visible
--   attributes, or nil if the unit doesn't exist. Used by the info panel.
unitGetInfoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetInfoFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mInst ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (HM.lookup uid (umInstances um))
            case mInst of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just inst → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (uiDefName inst))
                    Lua.setfield (-2) "defName"
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridX inst)))
                    Lua.setfield (-2) "gridX"
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridY inst)))
                    Lua.setfield (-2) "gridY"
                    Lua.pushinteger (fromIntegral (uiGridZ inst))
                    Lua.setfield (-2) "gridZ"
                    Lua.pushstring (TE.encodeUtf8 (dirToText (uiFacing inst)))
                    Lua.setfield (-2) "facing"
                    Lua.pushnumber (Lua.Number (realToFrac (uiBaseWidth inst)))
                    Lua.setfield (-2) "baseWidth"
                    return 1

dirToText ∷ Direction → Text
dirToText DirS  = "S"
dirToText DirSW = "SW"
dirToText DirW  = "W"
dirToText DirNW = "NW"
dirToText DirN  = "N"
dirToText DirNE = "NE"
dirToText DirE  = "E"
dirToText DirSE = "SE"

-- * Selection

-- | unit.select(id) — replace the selection with a single unit.
--   Returns true if the unit exists, false if not.
unitSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSelectFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            ok ← Lua.liftIO $ Sel.selectUnit env uid
            Lua.pushboolean ok
            return 1

-- | unit.deselectAll() — empty the selection. Always returns true.
unitDeselectAllFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDeselectAllFn env = do
    Lua.liftIO $ Sel.clearSelection env
    Lua.pushboolean True
    return 1

-- | unit.getSelected() — returns a Lua array of integer unit IDs.
--   Filtered to only live units.
unitGetSelectedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetSelectedFn env = do
    selected ← Lua.liftIO $ Sel.getSelected env
    let ids = HS.toList selected
    Lua.newtable
    forM_ (zip [1..] ids) $ \(i, uid) → do
        Lua.pushinteger (fromIntegral (unUnitId uid))
        Lua.rawseti (-2) i
    return 1

-- | unit.isSelected(id) — bool.
unitIsSelectedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitIsSelectedFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            sel ← Lua.liftIO $ Sel.isSelected env uid
            Lua.pushboolean sel
            return 1

-- | unit.hitTestAt(screenX, screenY) — returns the unit ID under the
--   given framebuffer-pixel coordinates, or nil if no unit is hit.
--
--   Lua side passes the raw GLFW mouse position (window-space pixels,
--   pre-DPI-scaling). `Unit.HitTest.hitTestUnitAt` does the projection
--   and per-unit AABB test against the sprite quad.
unitHitTestAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitHitTestAtFn env = do
    xArg ← Lua.tonumber 1
    yArg ← Lua.tonumber 2
    case (xArg, yArg) of
        (Just (Lua.Number x), Just (Lua.Number y)) → do
            mUid ← Lua.liftIO $ HitTest.hitTestUnitAt env
                                  (realToFrac x) (realToFrac y)
            case mUid of
                Just uid → do
                    Lua.pushinteger (fromIntegral (unUnitId uid))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- * Helpers

parseDirKey ∷ Text → Maybe Direction
parseDirKey "S"  = Just DirS
parseDirKey "SW" = Just DirSW
parseDirKey "W"  = Just DirW
parseDirKey "NW" = Just DirNW
parseDirKey "N"  = Just DirN
parseDirKey "NE" = Just DirNE
parseDirKey "E"  = Just DirE
parseDirKey "SE" = Just DirSE
parseDirKey _    = Nothing
