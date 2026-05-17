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
    , unitHitTestInRectFn
    , unitSetSelectionFn
    , unitSetAnimFn
    , unitCollapseFn
    , unitReviveFn
    , unitGetStatFn
    , unitGetStatBaseFn
    , unitSetStatFn
    , unitGetAllStatsFn
    , unitAddModifierFn
    , unitRemoveModifierFn
    , unitGetModifiersFn
    , unitClearModifiersFn
    , unitGetAllIdsFn
    , unitGetActivityFn
    , unitGetSkillFn
    , unitSetSkillFn
    , unitAddXPFn
    , unitGetAllSkillsFn
    , unitGetInventoryFn
    , unitDrinkFn
    , unitPickupFn
    , unitTransitionToFn
    , unitGetPoseFn
    , unitModifyItemFillFn
    , unitAddItemFn
    , unitGetVisibleTilesFn
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map.Strict as Map
import qualified HsLua as Lua
import Control.Monad (foldM, forM_)
import Data.IORef (readIORef, atomicModifyIORef')
import Data.Time.Clock.POSIX (getPOSIXTime)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo, logDebug, logWarn)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister)
import Engine.Asset.YamlUnits (UnitYamlDef(..), UnitYamlAnim(..),
                               UnitYamlStat(..), UnitYamlSkill(..),
                               UnitYamlBody(..), UnitYamlBodyAttr(..),
                               UnitYamlInventoryEntry(..),
                               loadUnitYaml)
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Command.Types (UnitCommand(..))
import Unit.Direction (Direction(..))
import Unit.Sim.Types (Pose(..))
import Item.Types (ItemInstance(..), ItemDef(..), ItemContainer(..)
                  , ItemManager(..), lookupItemDef)
import Unit.LineOfSight (unitVisibleTiles)
import Unit.Stats (rollStat, effectiveStat, applySkillXP)
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

                    -- Load animations (if any)
                    animMap ← foldM (\accA (animName, animDef) → do
                        frameMap ← foldM (\accF (dirKey, framePaths) →
                            case parseDirKey dirKey of
                                Nothing → do
                                    logWarn logger CatAsset $
                                        "Unknown direction '" <> dirKey
                                        <> "' in anim '" <> animName
                                        <> "' of unit " <> name <> ", skipping"
                                    return accF
                                Just dir → do
                                    handles ← mapM (\(i, p) →
                                        loadAndRegister env backendState lteq
                                            ("unit_" <> name
                                             <> "_" <> animName
                                             <> "_" <> dirKey
                                             <> "_" <> T.pack (show i))
                                            (T.unpack p)
                                        ) (zip [(0 ∷ Int)..] framePaths)
                                    return (Map.insert dir
                                              (V.fromList handles) accF)
                            ) Map.empty (Map.toList (uyaFrames animDef))
                        let anim = Animation
                                { aFps    = uyaFps animDef
                                , aLoop   = uyaLoop animDef
                                , aFrames = frameMap
                                }
                        return (HM.insert animName anim accA)
                        ) HM.empty (Map.toList (uydAnimations def))

                    let stateAnims = HM.fromList (Map.toList (uydStateAnimations def))
                        body = uydBody def
                        bodyEntries =
                            [ ("height",  (uybaMean (uybHeight body),
                                           uybaRange (uybHeight body)))
                            , ("bulk",    (uybaMean (uybBulk body),
                                           uybaRange (uybBulk body)))
                            , ("bodyfat", (uybaMean (uybBodyfat body),
                                           uybaRange (uybBodyfat body)))
                            ]
                        statTemplates = HM.fromList $
                            bodyEntries ++
                            [ (sname, (uysBase s, uysRange s))
                            | (sname, s) ← Map.toList (uydStats def)
                            ]
                        skillTemplates = HM.fromList
                            [ (sname, (uyskBase s, uyskRange s))
                            | (sname, s) ← Map.toList (uydSkills def)
                            ]

                    let startingInv =
                            [ (uyieItem e, uyieFill e)
                            | e ← uydStartingInventory def
                            ]
                        unitDef = UnitDef
                            { udName          = name
                            , udTexture       = handle
                            , udDirSprites    = dirMap
                            , udBaseWidth     = uydBaseWidth def
                            , udAnimations    = animMap
                            , udStateAnims    = stateAnims
                            , udEagerStats    = uydEagerStats def
                            , udStatTemplates = statTemplates
                            , udSkillTemplates = skillTemplates
                            , udStartingInventory = startingInv
                            }
                    atomicModifyIORef' (unitManagerRef env) $ \um →
                        (um { umDefs = HM.insert name unitDef (umDefs um) }, ())

                    logDebug logger CatAsset $
                        "Registered unit def: " <> name
                        <> " (handle " <> T.pack (show handle) <> ")"
                        <> " (" <> T.pack (show (Map.size dirMap))
                        <> " directional sprites, "
                        <> T.pack (show (HM.size animMap))
                        <> " animations)"

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

-- | unit.collapse(id) — transition the unit into the Collapsed state.
--   The state's anim is resolved via udStateAnims ("collapsed" → name);
--   a non-looping anim plays once and pickFrame holds the last frame.
--   Collapsed units ignore subsequent UnitMoveTo commands.
unitCollapseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCollapseFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitCollapse uid
            Lua.pushboolean True
            return 1

-- | unit.revive(id) — transition a Collapsed unit through the
--   Reviving state and back to Idle. The reviving-state anim plays
--   (typically the collapse anim in reverse via uiAnimReverse). No-op
--   if the unit isn't currently Collapsed.
unitReviveFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitReviveFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitRevive uid
            Lua.pushboolean True
            return 1

-- | unit.drink(uid) — start the drinking animation on an Idle unit.
--   Engine handles the auto-revert to Idle when the anim finishes.
--   Stat / canteen effects are applied Lua-side BEFORE calling this
--   (see scripts/unit_ai drink action); the engine doesn't know about
--   sip amounts.
unitDrinkFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDrinkFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitDrink uid
            Lua.pushboolean True
            return 1

-- | unit.pickup(uid) — start the "picking up" animation on an Idle
--   unit. Same shape as drink; used for canteen refilling.
unitPickupFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitPickupFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitPickup uid
            Lua.pushboolean True
            return 1

-- | unit.transitionTo(uid, poseName, stride?) — initiate a pose
--   transition. poseName is one of "standing", "crouching", "crawling",
--   "collapsed". Optional stride defaults to 1; pass 2 (or higher) to
--   skip frames when chaining transitions back-to-back.
--   No-op if the unit is already in that pose or mid-transition.
unitTransitionToFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTransitionToFn env = do
    idArg     ← Lua.tointeger 1
    mPoseBS   ← Lua.tostring 2
    mStrideArg ← Lua.tointeger 3
    let stride = case mStrideArg of
            Just s | s ≥ 1 → fromIntegral s
            _              → 1
    case (idArg, mPoseBS >>= parsePose . TE.decodeUtf8) of
        (Just n, Just target) → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitTransitionTo uid target stride
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

parsePose ∷ Text → Maybe Pose
parsePose "standing"  = Just Standing
parsePose "crouching" = Just Crouching
parsePose "crawling"  = Just Crawling
parsePose "collapsed" = Just Collapsed
parsePose _           = Nothing

-- | unit.getPose(uid) — returns the unit's current pose as a string:
--   "standing" / "crouching" / "crawling" / "collapsed". nil if the
--   unit doesn't exist. Reads `uiPose`, mirrored from `usPose` by
--   Unit.Thread.publishToRender every tick.
unitGetPoseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPoseFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPose ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiPose <$> HM.lookup uid (umInstances um))
            case mPose of
                Just label → do
                    Lua.pushstring (TE.encodeUtf8 label)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | unit.modifyItemFill(uid, defName, delta) → actual applied delta
--   (after clamp to [0, capacity]). Adjusts the fill of the FIRST item
--   matching defName. Returns 0 if no such item / no container / unit
--   missing.
unitModifyItemFillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitModifyItemFillFn env = do
    idArg    ← Lua.tointeger 1
    nameArg  ← Lua.tostring 2
    deltaArg ← Lua.tonumber 3
    case (idArg, nameArg, deltaArg) of
        (Just n, Just nameBS, Just (Lua.Number d)) → do
            let uid     = UnitId (fromIntegral n)
                defName = TE.decodeUtf8 nameBS
                delta   = realToFrac d ∷ Float
            applied ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, 0 ∷ Float)
                        Just inst →
                            let (newInv, app) =
                                    adjustFirstFill itemMgr defName delta
                                        (uiInventory inst)
                                inst' = inst { uiInventory = newInv }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }, app)
            Lua.pushnumber (Lua.Number (realToFrac applied))
            return 1
        _ → do
            Lua.pushnumber 0
            return 1

-- | unit.addItem(uid, defName, fill) → bool. Adds a new ItemInstance
--   to the unit's inventory. Fill is clamped to the def's container
--   capacity (or zeroed for non-containers). Returns false if the
--   unit or item def doesn't exist.
unitAddItemFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitAddItemFn env = do
    idArg    ← Lua.tointeger 1
    nameArg  ← Lua.tostring 2
    fillArg  ← Lua.tonumber 3
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid     = UnitId (fromIntegral n)
                defName = TE.decodeUtf8 nameBS
                fillIn  = case fillArg of
                    Just (Lua.Number d) → realToFrac d ∷ Float
                    _ → 0
            ok ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                case lookupItemDef defName itemMgr of
                    Nothing → return False
                    Just def → do
                        let clampedFill = case idContainer def of
                                Just c  → max 0 (min fillIn (icCapacity c))
                                Nothing → 0
                            inst' = ItemInstance
                                { iiDefName     = defName
                                , iiCurrentFill = clampedFill
                                }
                        atomicModifyIORef' (unitManagerRef env) $ \um →
                            case HM.lookup uid (umInstances um) of
                                Nothing → (um, False)
                                Just u  →
                                    let u' = u { uiInventory =
                                                  uiInventory u ++ [inst'] }
                                    in (um { umInstances = HM.insert uid u'
                                                            (umInstances um) },
                                        True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | Helper: adjust the fill of the first ItemInstance matching defName.
--   Clamps to [0, capacity] looked up via the ItemManager. Returns the
--   resulting inventory and the actual applied delta (post-clamp).
adjustFirstFill
    ∷ ItemManager → Text → Float → [ItemInstance] → ([ItemInstance], Float)
adjustFirstFill itemMgr defName delta = go
  where
    go [] = ([], 0)
    go (x : xs)
      | iiDefName x ≡ defName =
          let cap = case lookupItemDef defName itemMgr of
                  Just d  → case idContainer d of
                      Just c  → icCapacity c
                      Nothing → iiCurrentFill x   -- no container → no headroom
                  Nothing → iiCurrentFill x
              newFill = max 0 (min cap (iiCurrentFill x + delta))
              applied = newFill - iiCurrentFill x
              x'      = x { iiCurrentFill = newFill }
          in (x' : xs, applied)
      | otherwise =
          let (xs', applied) = go xs
          in (x : xs', applied)

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

-- * Skills

-- | unit.getSkill(uid, name) — EFFECTIVE skill value (base + active
--   modifier deltas, clamped at 0). Phase F: skills now share the
--   modifier pipeline with stats, so a modifier on "balance" shifts
--   getSkill's result too. nil if undefined.
unitGetSkillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetSkillFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
            mVal ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing → pure Nothing
                    Just inst → case HM.lookup name (uiSkills inst) of
                        Nothing → pure Nothing
                        Just base → do
                            now ← realToFrac <$> getPOSIXTime
                            let mods = HM.lookupDefault [] name
                                          (uiModifiers inst)
                            pure (Just (effectiveStat now base mods))
            case mVal of
                Just v  → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.setSkill(uid, name, value) — debug override. Sets the skill
--   level directly. Clamps at 0 (negative skill is meaningless under
--   the XP formula). Returns true if the unit exists, false otherwise.
unitSetSkillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetSkillFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    valArg  ← Lua.tonumber 3
    case (idArg, nameArg, valArg) of
        (Just n, Just nameBS, Just (Lua.Number v)) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
                lvl  = max 0 (realToFrac v)
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiSkills =
                                HM.insert name lvl (uiSkills inst) }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.addXP(uid, name, amount) — apply XP to a stat OR skill via
--   @newValue = value + amount / max (value^2, 1e-4)@. The lookup
--   tries skills first (typical case), then stats — whichever the
--   unit has under that name gets nudged. There's no separate XP
--   accumulator. Returns the new value, or nil if the unit doesn't
--   have a stat or skill by that name.
--
--   Phase F: this used to be addSkillXP and required the name to be
--   in skills. Now stats can grow this way too — a unit performing
--   manual labour can @unit.addXP(uid, \"strength\", 0.01)@ to slowly
--   build their strength stat over time.
unitAddXPFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitAddXPFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    amtArg  ← Lua.tonumber 3
    case (idArg, nameArg, amtArg) of
        (Just n, Just nameBS, Just (Lua.Number amt)) → do
            let uid    = UnitId (fromIntegral n)
                name   = TE.decodeUtf8 nameBS
                amount = realToFrac amt
            mVal ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, Nothing)
                        Just inst →
                            -- Try skills first (typical case), then
                            -- stats. Whichever map has the name owns
                            -- the value and gets updated.
                            case HM.lookup name (uiSkills inst) of
                                Just lvl →
                                    let lvl'  = applySkillXP lvl amount
                                        inst' = inst { uiSkills =
                                            HM.insert name lvl' (uiSkills inst) }
                                    in (um { umInstances = HM.insert uid inst'
                                                             (umInstances um) }
                                       , Just lvl')
                                Nothing → case HM.lookup name (uiStats inst) of
                                    Just v →
                                        let v'    = applySkillXP v amount
                                            inst' = inst { uiStats =
                                                HM.insert name v' (uiStats inst) }
                                        in (um { umInstances = HM.insert uid inst'
                                                                 (umInstances um) }
                                           , Just v')
                                    Nothing → (um, Nothing)
            case mVal of
                Just v → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.getAllSkills(uid) — returns a Lua table keyed by skill name
--   with @{ level }@ subtables. Phase F: @level@ is the EFFECTIVE
--   value (base + active modifiers, clamped) — matches unit.getSkill.
--   nil if the unit is missing.
unitGetAllSkillsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAllSkillsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mEntries ← Lua.liftIO $ do
                um  ← readIORef (unitManagerRef env)
                now ← realToFrac <$> getPOSIXTime
                pure $ do
                    inst ← HM.lookup uid (umInstances um)
                    def  ← HM.lookup (uiDefName inst) (umDefs um)
                    let tmpls = udSkillTemplates def
                        mods  = uiModifiers inst
                    pure
                        [ (name, eff)
                        | (name, _) ← HM.toList tmpls
                        , let base = HM.lookupDefault 0 name (uiSkills inst)
                              eff  = effectiveStat now base
                                       (HM.lookupDefault [] name mods)
                        ]
            case mEntries of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just entries → do
                    Lua.newtable
                    forM_ entries $ \(name, lvl) → do
                        Lua.newtable
                        Lua.pushnumber (Lua.Number (realToFrac lvl))
                        Lua.setfield (-2) "level"
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.insert (-2)
                        Lua.rawset (-3)
                    return 1

-- | unit.getActivity(uid) — returns the unit's current sim-thread
--   activity as a string: "idle", "walking", or "collapsed". nil if
--   the unit doesn't exist. Reads `uiActivity`, which is mirrored
--   from `usState` by Unit.Thread.publishToRender every tick.
unitGetActivityFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetActivityFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mAct ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiActivity <$> HM.lookup uid (umInstances um))
            case mAct of
                Just label → do
                    Lua.pushstring (TE.encodeUtf8 label)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

-- | unit.getAllIds() — return a Lua array of every live unit's
--   integer id. Useful for per-tick iteration in scripts that don't
--   want to parse the human-readable string from unit.list.
unitGetAllIdsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAllIdsFn env = do
    ids ← Lua.liftIO $ do
        um ← readIORef (unitManagerRef env)
        pure (HM.keys (umInstances um))
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] ids) $ \(i, uid) → do
        Lua.pushinteger (fromIntegral (unUnitId uid))
        Lua.rawseti (-2) (fromIntegral i)
    return 1

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
                    Lua.pushstring (TE.encodeUtf8 (uiCurrentAnim inst))
                    Lua.setfield (-2) "currentAnim"
                    Lua.pushnumber (Lua.Number (realToFrac (uiAnimStart inst)))
                    Lua.setfield (-2) "animStart"
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

-- | unit.hitTestInRect(x1, y1, x2, y2) — returns a Lua array of unit
--   IDs whose sprite-quad center falls inside the screen rect (window
--   pixels). Used by drag-box selection.
unitHitTestInRectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitHitTestInRectFn env = do
    x1A ← Lua.tonumber 1
    y1A ← Lua.tonumber 2
    x2A ← Lua.tonumber 3
    y2A ← Lua.tonumber 4
    case (x1A, y1A, x2A, y2A) of
        (Just (Lua.Number a), Just (Lua.Number b),
         Just (Lua.Number c), Just (Lua.Number d)) → do
            ids ← Lua.liftIO $ HitTest.hitTestUnitsInRect env
                                  (realToFrac a) (realToFrac b)
                                  (realToFrac c) (realToFrac d)
            Lua.newtable
            forM_ (zip [1..] ids) $ \(i, uid) → do
                Lua.pushinteger (fromIntegral (unUnitId uid))
                Lua.rawseti (-2) i
            return 1
        _ → do
            Lua.newtable
            return 1

-- | unit.setSelection(idTable) — replace the selection with the given
--   array of unit IDs. IDs not corresponding to live units are filtered
--   out by the underlying setSelection.
unitSetSelectionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetSelectionFn env = do
    n ← Lua.rawlen 1   -- 0 if arg 1 isn't a table; loop body skips
    let go i acc
            | i > fromIntegral n = return acc
            | otherwise = do
                _ ← Lua.rawgeti 1 i
                m ← Lua.tointeger (-1)
                Lua.pop 1
                case m of
                    Just k  → go (i + 1) (UnitId (fromIntegral k) : acc)
                    Nothing → go (i + 1) acc
    ids ← go 1 []
    Lua.liftIO $ Sel.setSelection env (HS.fromList ids)
    Lua.pushboolean True
    return 1

-- | unit.setAnim(id, name) — sets the raw animation name on a unit and
--   resets its start time to now. Empty string clears the animation back
--   to T-pose. No state-name resolution; that's Phase 3's `setUnitAnim`.
--   Returns true on success, false if the unit doesn't exist.
unitSetAnimFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetAnimFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
            ok ← Lua.liftIO $ do
                now ← realToFrac <$> getPOSIXTime
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let inst' = inst { uiCurrentAnim = name
                                             , uiAnimStart   = now
                                             }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- * Stats

-- | unit.getStat(id, name) — read the EFFECTIVE stat value (base +
--   active modifier deltas, clamped at 0). Lazy-rolls the base if the
--   unit type defines the stat but the value hasn't been rolled yet.
--   Returns nil if the unit doesn't exist or doesn't define this stat.
unitGetStatFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetStatFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
            mVal ← Lua.liftIO $ getEffectiveStat env uid name
            case mVal of
                Just v  → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.getStatBase(id, name) — read the RAW rolled value with no
--   modifiers applied. Use this when you want the underlying base —
--   e.g. to compute how much a debuff is shifting the effective value.
--   Lazy-rolls if needed; returns nil if undefined or unit missing.
unitGetStatBaseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetStatBaseFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
            mVal ← Lua.liftIO $ getOrRollStat env uid name
            case mVal of
                Just v  → do
                    Lua.pushnumber (Lua.Number (realToFrac v))
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.setStat(id, name, value) — overwrite a stat. Clamps at >= 0.
--   Returns true if the unit exists, false otherwise. No definition
--   check: setting a stat the unit type doesn't declare is allowed
--   (the value becomes accessible via getStat from then on).
unitSetStatFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetStatFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    valArg  ← Lua.tonumber 3
    case (idArg, nameArg, valArg) of
        (Just n, Just nameBS, Just (Lua.Number v)) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
                clamped = max 0 (realToFrac v)
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiStats =
                                HM.insert name clamped (uiStats inst) }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.getAllStats(id) — return a Lua table with every stat declared
--   by the unit's type. Values are EFFECTIVE (base + active modifiers,
--   clamped). Lazy-rolls any that haven't been rolled. nil if the unit
--   doesn't exist.
unitGetAllStatsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetAllStatsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPairs ← Lua.liftIO $ effectiveAllStats env uid
            case mPairs of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just pairs → do
                    Lua.newtable
                    forM_ pairs $ \(name, v) → do
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.pushnumber (Lua.Number (realToFrac v))
                        Lua.rawset (-3)
                    return 1

-- | unit.addModifier(id, name, delta, source, durationSec) —
--   add or replace an additive modifier on a stat. Same @source@ on
--   the same @name@ overwrites the previous entry; different sources
--   stack. @durationSec@ is optional (nil = permanent); when given it
--   is added to the current POSIX time to produce smExpiry.
--   Returns true on success, false if the unit doesn't exist.
unitAddModifierFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitAddModifierFn env = do
    idArg     ← Lua.tointeger 1
    nameArg   ← Lua.tostring 2
    deltaArg  ← Lua.tonumber 3
    srcArg    ← Lua.tostring 4
    -- 5 may be nil (permanent) or a number (duration seconds).
    durMaybe  ← Lua.tonumber 5
    case (idArg, nameArg, deltaArg, srcArg) of
        (Just n, Just nameBS, Just (Lua.Number d), Just srcBS) → do
            let uid   = UnitId (fromIntegral n)
                name  = TE.decodeUtf8 nameBS
                src   = TE.decodeUtf8 srcBS
                delta = realToFrac d
            ok ← Lua.liftIO $ do
                expiry ← case durMaybe of
                    Just (Lua.Number dur) → do
                        now ← realToFrac <$> getPOSIXTime
                        pure (Just (now + realToFrac dur))
                    _ → pure Nothing
                let mod' = StatModifier
                        { smDelta  = delta
                        , smSource = src
                        , smExpiry = expiry
                        }
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let existing = HM.lookupDefault []
                                              name (uiModifiers inst)
                                -- Drop any prior entry from the same source.
                                others = filter (\m → smSource m ≠ src)
                                                existing
                                newList = mod' : others
                                inst' = inst { uiModifiers =
                                    HM.insert name newList
                                        (uiModifiers inst) }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }
                               , True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | unit.removeModifier(id, source) — remove every modifier owned
--   by @source@ across all stats. Returns the count of removed entries
--   (0 if unit missing or no matches).
unitRemoveModifierFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitRemoveModifierFn env = do
    idArg  ← Lua.tointeger 1
    srcArg ← Lua.tostring 2
    case (idArg, srcArg) of
        (Just n, Just srcBS) → do
            let uid = UnitId (fromIntegral n)
                src = TE.decodeUtf8 srcBS
            removed ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, 0 ∷ Int)
                        Just inst →
                            let mods = uiModifiers inst
                                -- Count before, then filter each list.
                                cnt = sum [ length (filter
                                              (\m → smSource m ≡ src) ms)
                                          | ms ← HM.elems mods ]
                                pruned = HM.map (filter
                                            (\m → smSource m ≠ src)) mods
                                -- Drop now-empty entries to keep the
                                -- map tidy.
                                pruned' = HM.filter (not . null) pruned
                                inst' = inst { uiModifiers = pruned' }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }
                               , cnt)
            Lua.pushinteger (fromIntegral removed)
            return 1
        _ → do
            Lua.pushinteger 0
            return 1

-- | unit.getModifiers(id, name) — list every modifier on the named
--   stat as a Lua array of @{delta, source, expiry}@ tables. Expired
--   entries are NOT filtered — caller can compare expiry to os.time().
--   nil if the unit doesn't exist; empty array if no modifiers.
unitGetModifiersFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetModifiersFn env = do
    idArg   ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (idArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid  = UnitId (fromIntegral n)
                name = TE.decodeUtf8 nameBS
            mMods ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um) of
                    Nothing   → pure Nothing
                    Just inst → pure (Just
                        (HM.lookupDefault [] name (uiModifiers inst)))
            case mMods of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just mods → do
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] mods) $ \(i, m) → do
                        Lua.newtable
                        Lua.pushnumber (Lua.Number (realToFrac (smDelta m)))
                        Lua.setfield (-2) "delta"
                        Lua.pushstring (TE.encodeUtf8 (smSource m))
                        Lua.setfield (-2) "source"
                        case smExpiry m of
                            Just t  → do
                                Lua.pushnumber (Lua.Number (realToFrac t))
                                Lua.setfield (-2) "expiry"
                            Nothing → pure ()
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1
        _ → do
            Lua.pushnil
            return 1

-- | unit.clearModifiers(id) — drop every modifier on every name
--   for this unit. Returns the count of removed entries. Mainly for
--   tests / debug; production code should usually use
--   removeModifier per-source.
unitClearModifiersFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitClearModifiersFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushinteger 0
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            cnt ← Lua.liftIO $
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, 0 ∷ Int)
                        Just inst →
                            let c = sum (length <$>
                                         HM.elems (uiModifiers inst))
                                inst' = inst { uiModifiers = HM.empty }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }
                               , c)
            Lua.pushinteger (fromIntegral cnt)
            return 1

-- | Read a stat or skill's base value. Phase F: lookup is unified —
--   stats first, then skills as a fallback. Lazy-rolls a stat
--   template if the value isn't cached and eager_stats is false.
--   Returns Nothing if undefined under either category.
getOrRollStat ∷ EngineEnv → UnitId → Text → IO (Maybe Float)
getOrRollStat env uid name = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup uid (umInstances um) of
        Nothing → return Nothing
        Just inst →
            case HM.lookup name (uiStats inst) of
                Just v  → return (Just v)
                Nothing → case HM.lookup name (uiSkills inst) of
                    -- Skills are always eager-rolled; if it's in
                    -- uiSkills the value is final.
                    Just v  → return (Just v)
                    Nothing → case HM.lookup (uiDefName inst) (umDefs um) of
                        Nothing  → return Nothing
                        Just def → case HM.lookup name (udStatTemplates def) of
                            Nothing     → return Nothing
                            Just (b, r) → do
                                v ← atomicModifyIORef' (statRNGRef env) $ \g0 →
                                    let (val, g') = rollStat b r g0
                                    in (g', val)
                                -- Cache the rolled value. If the unit was
                                -- destroyed mid-roll, the lookup inside the
                                -- atomic modify finds nothing and we silently
                                -- drop — never resurrects a zombie unit.
                                atomicModifyIORef' (unitManagerRef env) $ \um' →
                                    case HM.lookup uid (umInstances um') of
                                        Nothing → (um', ())
                                        Just i  →
                                            let i' = i { uiStats =
                                                    HM.insert name v (uiStats i) }
                                            in (um' { umInstances =
                                                    HM.insert uid i'
                                                        (umInstances um') }, ())
                                return (Just v)

-- | Roll every stat declared by the unit's def that hasn't been rolled
--   yet, then return the full set as (name, value) pairs. Returns
--   Nothing if the unit doesn't exist.
rollAllDefinedStats ∷ EngineEnv → UnitId → IO (Maybe [(Text, Float)])
rollAllDefinedStats env uid = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup uid (umInstances um) of
        Nothing → return Nothing
        Just inst → case HM.lookup (uiDefName inst) (umDefs um) of
            Nothing → return (Just (HM.toList (uiStats inst)))
            Just def → do
                let templates = HM.toList (udStatTemplates def)
                -- Roll any missing stats (one getOrRollStat each).
                mapM_ (\(n, _) → getOrRollStat env uid n) templates
                -- Re-read for final values (including any pre-existing).
                um' ← readIORef (unitManagerRef env)
                case HM.lookup uid (umInstances um') of
                    Nothing    → return (Just [])
                    Just inst' → return (Just (HM.toList (uiStats inst')))

-- | Read a stat through the full Phase-C pipeline: lazy-roll the base
--   if needed, then apply this unit's active modifiers at the current
--   POSIX time. Returns Nothing if undefined.
getEffectiveStat ∷ EngineEnv → UnitId → Text → IO (Maybe Float)
getEffectiveStat env uid name = do
    mBase ← getOrRollStat env uid name
    case mBase of
        Nothing   → pure Nothing
        Just base → do
            now ← realToFrac <$> getPOSIXTime
            um ← readIORef (unitManagerRef env)
            case HM.lookup uid (umInstances um) of
                Nothing   → pure (Just base)   -- destroyed mid-call
                Just inst →
                    let mods = HM.lookupDefault [] name
                                  (uiModifiers inst)
                    in pure (Just (effectiveStat now base mods))

-- | Like rollAllDefinedStats, but returns effective values (post-modifier).
--   Reads now once and applies it to every modifier list for consistency.
effectiveAllStats ∷ EngineEnv → UnitId → IO (Maybe [(Text, Float)])
effectiveAllStats env uid = do
    mBases ← rollAllDefinedStats env uid
    case mBases of
        Nothing    → pure Nothing
        Just bases → do
            now ← realToFrac <$> getPOSIXTime
            um ← readIORef (unitManagerRef env)
            case HM.lookup uid (umInstances um) of
                Nothing   → pure (Just bases)  -- destroyed mid-call
                Just inst →
                    let mods = uiModifiers inst
                        eff (n, b) =
                            (n, effectiveStat now b
                                  (HM.lookupDefault [] n mods))
                    in pure (Just (map eff bases))

-- * Helpers

-- | Accept short uppercase ("S","SW") or long lowercase ("south","south-east").
parseDirKey ∷ Text → Maybe Direction
parseDirKey t = case T.toLower t of
    "s"          → Just DirS
    "sw"         → Just DirSW
    "w"          → Just DirW
    "nw"         → Just DirNW
    "n"          → Just DirN
    "ne"         → Just DirNE
    "e"          → Just DirE
    "se"         → Just DirSE
    "south"      → Just DirS
    "south-west" → Just DirSW
    "west"       → Just DirW
    "north-west" → Just DirNW
    "north"      → Just DirN
    "north-east" → Just DirNE
    "east"       → Just DirE
    "south-east" → Just DirSE
    _            → Nothing

-- | unit.getInventory(uid) → array of item tables, or nil if the unit
--   doesn't exist. Each table has:
--     defName        — ItemDef key (e.g. "canteen_steel_2l")
--     displayName    — UI-facing name from YAML
--     weight         — empty kg
--     currentFill    — litres held (0 for non-containers)
--     capacity       — litres max (nil for non-containers)
--     holds          — fluid kind (nil for non-containers)
unitGetInventoryFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetInventoryFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mInv ← Lua.liftIO $ do
                um      ← readIORef (unitManagerRef env)
                itemMgr ← readIORef (itemManagerRef env)
                pure $ case HM.lookup uid (umInstances um) of
                    Nothing   → Nothing
                    Just inst → Just (uiInventory inst, itemMgr)
            case mInv of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just (insts, itemMgr) → do
                    Lua.newtable
                    -- Use 1-based Lua indexing for a real array.
                    forM_ (zip [1 ..] insts) $ \(i, inst) → do
                        Lua.newtable
                        let name = iiDefName inst
                            mDef = lookupItemDef name itemMgr
                            displayName = case mDef of
                                Just d  → idDisplayName d
                                Nothing → name
                            weight = case mDef of
                                Just d  → idWeight d
                                Nothing → 0
                            mContainer = mDef >>= idContainer
                        Lua.pushstring (TE.encodeUtf8 name)
                        Lua.setfield (-2) "defName"
                        Lua.pushstring (TE.encodeUtf8 displayName)
                        Lua.setfield (-2) "displayName"
                        Lua.pushnumber (Lua.Number (realToFrac weight))
                        Lua.setfield (-2) "weight"
                        Lua.pushnumber (Lua.Number (realToFrac (iiCurrentFill inst)))
                        Lua.setfield (-2) "currentFill"
                        case mContainer of
                            Just c → do
                                Lua.pushnumber (Lua.Number (realToFrac (icCapacity c)))
                                Lua.setfield (-2) "capacity"
                                Lua.pushstring (TE.encodeUtf8 (icHolds c))
                                Lua.setfield (-2) "holds"
                            Nothing → pure ()
                        Lua.rawseti (-2) (fromIntegral (i ∷ Int))
                    return 1

-- | unit.getVisibleTiles(uid) → array of {x, y} tables, or nil if the
--   unit doesn't exist. Includes the unit's own tile.
unitGetVisibleTilesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetVisibleTilesFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            tiles ← Lua.liftIO $ unitVisibleTiles env uid
            -- Return nil specifically when the unit is missing
            -- (distinct from "exists but sees nothing", which is []).
            um ← Lua.liftIO $ readIORef (unitManagerRef env)
            if not (HM.member uid (umInstances um))
                then do
                    Lua.pushnil
                    return 1
                else do
                    Lua.newtable
                    forM_ (zip [1 ..] tiles) $ \(i, (gx, gy)) → do
                        Lua.newtable
                        Lua.pushinteger (fromIntegral gx)
                        Lua.setfield (-2) "x"
                        Lua.pushinteger (fromIntegral gy)
                        Lua.setfield (-2) "y"
                        Lua.rawseti (-2) (fromIntegral (i ∷ Int))
                    return 1
