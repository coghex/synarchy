{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Buildings.Query
    ( buildingGetStartingBuildingsFn
    , buildingGetInfoFn
    , buildingGetOperationsFn
    , buildingFindStationFn
    , buildingListFn
    , buildingGetActiveIdsFn
    , buildingListDefsFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import World.Page.Types (WorldPageId(..))
import Building.Types
import Engine.Asset.Handle (TextureHandle(..))

-- | building.getStartingBuildings() — Lua array of def names where
--   is_starting is true. Used by the build tool's popup.
buildingGetStartingBuildingsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetStartingBuildingsFn env = do
    names ← Lua.liftIO $ do
        bm ← readIORef (buildingManagerRef env)
        pure [ n | (n, d) ← HM.toList (bmDefs bm), bdIsStarting d ]
    Lua.newtable
    forM_ (zip [1..] names) $ \(i, name) → do
        Lua.pushstring (TE.encodeUtf8 name)
        Lua.rawseti (-2) i
    return 1

-- | building.getInfo(id) — returns a Lua table with the building's
--   attributes, or nil if missing.
buildingGetInfoFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetInfoFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mInst ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure (HM.lookup bid (bmInstances bm))
            case mInst of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just inst → do
                    -- Resolve def once for display fields that come
                    -- from the def rather than the instance.
                    mDef ← Lua.liftIO $ do
                        bm ← readIORef (buildingManagerRef env)
                        pure (HM.lookup (biDefName inst) (bmDefs bm))
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (biDefName inst))
                    Lua.setfield (-2) "defName"
                    Lua.pushstring (TE.encodeUtf8 $ case mDef of
                        Just d  → bdDisplayName d
                        Nothing → biDefName inst)
                    Lua.setfield (-2) "displayName"
                    Lua.pushinteger (fromIntegral (biAnchorX inst))
                    Lua.setfield (-2) "gridX"
                    Lua.pushinteger (fromIntegral (biAnchorY inst))
                    Lua.setfield (-2) "gridY"
                    Lua.pushinteger (fromIntegral (biGridZ inst))
                    Lua.setfield (-2) "gridZ"
                    Lua.pushinteger (fromIntegral (biTileW inst))
                    Lua.setfield (-2) "tileW"
                    Lua.pushinteger (fromIntegral (biTileH inst))
                    Lua.setfield (-2) "tileH"
                    Lua.pushnumber (Lua.Number (realToFrac (biSpawnedAt inst)))
                    Lua.setfield (-2) "spawnedAt"
                    -- The world page this building lives on (#76). Lets a
                    -- caller bind follow-up actions (e.g. unit.spawn) to the
                    -- building's own page instead of the active page (#196).
                    Lua.pushstring (TE.encodeUtf8 (case biPage inst of
                        WorldPageId p → p))
                    Lua.setfield (-2) "page"
                    return 1

-- | building.getOperations(bid) → array of operation tags from the
--   def ("smelt", "forge", "assemble", "repair_condition",
--   "repair_sharpness", …). Empty array for
--   buildings that aren't work stations. nil if the bid or its def is
--   gone. Def-level data (#326) — what the station CAN do; whether it
--   currently can (Built) is getActivity's business.
buildingGetOperationsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetOperationsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let bid = BuildingId (fromIntegral n)
            mOps ← Lua.liftIO $ do
                bm ← readIORef (buildingManagerRef env)
                pure $ do
                    inst ← HM.lookup bid (bmInstances bm)
                    def  ← HM.lookup (biDefName inst) (bmDefs bm)
                    pure (bdOperations def)
            case mOps of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just ops → do
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] ops) $ \(i, op) → do
                        Lua.pushstring (TE.encodeUtf8 op)
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1

-- | building.findStation(op [, gx, gy]) → bid, gridX, gridY | nil.
--   Nearest BUILT building on the ACTIVE world page whose def offers
--   the operation (#326). Distance is Chebyshev from (gx, gy) to the
--   closest footprint tile; omit the coords to get the lowest-id
--   match. Ties break to the lowest id so results are deterministic
--   (bmInstances is a HashMap). Ghost/under-construction stations
--   never match — an unbuilt furnace can't smelt.
buildingFindStationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingFindStationFn env = do
    opArg ← Lua.tostring 1
    gxArg ← Lua.tointeger 2
    gyArg ← Lua.tointeger 3
    case opArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just opBS → do
            let op = TE.decodeUtf8Lenient opBS
                mFrom = case (gxArg, gyArg) of
                    (Just x, Just y) → Just (fromIntegral x ∷ Int,
                                             fromIntegral y ∷ Int)
                    _                → Nothing
            mBest ← Lua.liftIO $ do
                bm      ← readIORef (buildingManagerRef env)
                now     ← readIORef (gameTimeRef env)
                mActive ← activeWorldPage env
                pure $ case mActive of
                    Nothing → Nothing
                    Just (pid, _) →
                        let candidates =
                              [ (bid, inst)
                              | (bid, inst) ← HM.toList
                                    (buildingsOnPage pid (bmInstances bm))
                              , Just def ← [HM.lookup (biDefName inst)
                                                      (bmDefs bm)]
                              , op `elem` bdOperations def
                              , currentActivity now inst def ≡ Built ]
                            rank (bid, inst) =
                              ( maybe 0 (footprintDist inst) mFrom
                              , unBuildingId bid )
                        in case candidates of
                            [] → Nothing
                            cs → Just (minimumBy
                                    (comparing rank) cs)
            case mBest of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just (BuildingId n, inst) → do
                    Lua.pushinteger (fromIntegral n)
                    Lua.pushinteger (fromIntegral (biAnchorX inst))
                    Lua.pushinteger (fromIntegral (biAnchorY inst))
                    return 3

buildingListFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingListFn env = do
    result ← Lua.liftIO $ do
        bm ← readIORef (buildingManagerRef env)
        let entries = HM.toList (bmInstances bm)
        if null entries
        then return "No buildings placed"
        else return $ T.unpack $ T.intercalate "\n" $
            map (\(bid, inst) →
                "id=" <> T.pack (show (unBuildingId bid))
                <> " " <> biDefName inst
                <> " (" <> T.pack (show (biAnchorX inst))
                <> ", " <> T.pack (show (biAnchorY inst))
                <> ", " <> T.pack (show (biGridZ inst)) <> ")"
            ) entries
    Lua.pushstring (TE.encodeUtf8 (T.pack result))
    return 1

-- | building.getActiveIds() — Lua array of every live building's integer
--   id, scoped to the ACTIVE world page. The world-scoping boundary for
--   per-tick iteration so a building in another live world never leaks
--   into the current one (#198), mirroring unit.getAllIds (#78). Empty
--   when no world is active. Scripts should prefer this over parsing the
--   global, page-agnostic building.list when they iterate gameplay.
buildingGetActiveIdsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingGetActiveIdsFn env = do
    ids ← Lua.liftIO $ do
        bm ← readIORef (buildingManagerRef env)
        mActive ← activeWorldPage env
        pure $ case mActive of
            Just (pid, _) → HM.keys (buildingsOnPage pid (bmInstances bm))
            Nothing       → []
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] ids) $ \(i, bid) → do
        Lua.pushinteger (fromIntegral (unBuildingId bid))
        Lua.rawseti (-2) (fromIntegral i)
    return 1

-- | building.listDefs() — Lua array of tables, one per registered
--   building def. Each entry: {name, displayName, category, description,
--   iconTex, isStarting}. Used by the build menu to populate icons +
--   tooltips with categorisation.
buildingListDefsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
buildingListDefsFn env = do
    defs ← Lua.liftIO $ do
        bm ← readIORef (buildingManagerRef env)
        return $ HM.elems (bmDefs bm)
    Lua.newtable
    forM_ (zip [1..] defs) $ \(i, d) → do
        Lua.newtable
        Lua.pushstring (TE.encodeUtf8 (bdName d))
        Lua.setfield (-2) "name"
        Lua.pushstring (TE.encodeUtf8 (bdDisplayName d))
        Lua.setfield (-2) "displayName"
        Lua.pushstring (TE.encodeUtf8 (bdCategory d))
        Lua.setfield (-2) "category"
        Lua.pushstring (TE.encodeUtf8 (bdDescription d))
        Lua.setfield (-2) "description"
        let TextureHandle texInt = bdTexture d
        Lua.pushinteger (fromIntegral texInt)
        Lua.setfield (-2) "iconTex"
        Lua.pushboolean (bdIsStarting d)
        Lua.setfield (-2) "isStarting"
        Lua.pushnumber (Lua.Number (realToFrac (bdBuildWork d)))
        Lua.setfield (-2) "buildWork"
        Lua.rawseti (-2) i
    return 1
