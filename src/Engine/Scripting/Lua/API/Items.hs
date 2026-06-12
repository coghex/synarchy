{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
module Engine.Scripting.Lua.API.Items
    ( loadItemYamlFn
    , itemListDefsFn
    , itemSpawnGroundFn
    , itemListGroundFn
    , itemRemoveGroundFn
    , itemGroundCountFn
    , itemHitTestAtFn
    , itemSelectFn
    , itemDeselectFn
    , itemGetSelectedFn
    , itemPickupGroundFn
    ) where

import UPrelude
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import System.Directory (doesFileExist)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo, logWarn)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister)
import Engine.Asset.YamlItems
import Item.Ground (GroundItem(..), GroundItems(..), spawnGroundItem
                   , removeGroundItem)
import Item.Types
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import World.Cursor.Types (CursorState(..))
import World.Render.GroundItemQuads (hitTestGroundItemAt)
import World.Types (WorldManager(..), WorldState(..))

-- | If the preferred path doesn't exist on disk, swap in the equipment
--   missing-texture placeholder so loadAndRegister has *something* to
--   queue. Logged so missing assets are visible during iteration.
--   The fallback path itself isn't checked — if you delete it too,
--   you'll get the usual broken-texture behaviour at draw time.
missingEquipmentTexture ∷ FilePath
missingEquipmentTexture = "assets/textures/equipment/missing_equipment.png"

resolveSpritePath ∷ EngineEnv → FilePath → IO FilePath
resolveSpritePath env preferred = do
    exists ← doesFileExist preferred
    if exists then return preferred else do
        logger ← readIORef (loggerRef env)
        logWarn logger CatAsset $
            "Item sprite missing: " <> T.pack preferred
            <> " — substituting " <> T.pack missingEquipmentTexture
        return missingEquipmentTexture

-- | item.loadYaml(path) — parses a YAML file of item defs, loads each
--   item's sprite, and registers the defs into the ItemManager.
--   Returns the number of defs loaded.
loadItemYamlFn ∷ EngineEnv → LuaBackendState
               → Lua.LuaE Lua.Exception Lua.NumResults
loadItemYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                defs ← loadItemYaml logger filePath
                let (lteq, _) = lbsMsgQueues backendState

                total ← foldM (\acc def → do
                    -- Load the sprite texture so it's ready for any
                    -- future inventory grid UI. Register under
                    -- "item_<defName>" so other systems can fetch it.
                    let regName = "item_" <> iydName def
                    spritePath ← resolveSpritePath env (T.unpack (iydSprite def))
                    handle ← loadAndRegister env backendState lteq
                                regName spritePath

                    let container = fmap
                            (\c → ItemContainer
                                { icCapacity = iycCapacity c
                                , icHolds    = iycHolds c
                                })
                            (iydContainer def)
                        food = fmap
                            (\f → ItemFood { ifNutrition = iyfNutrition f })
                            (iydFood def)
                        weapon = fmap
                            (\w → ItemWeapon
                                { iwBladeLength    = iywBladeLength w
                                , iwBaseSharpness  = iywBaseSharpness w
                                , iwStabEff        = iywStabEff w
                                , iwSlashEff       = iywSlashEff w
                                , iwBluntEff       = iywBluntEff w
                                , iwWeaponClass    = iywWeaponClass w
                                , iwAttackCooldown = iywAttackCooldown w
                                })
                            (iydWeapon def)
                        itemDef = ItemDef
                            { idName        = iydName def
                            , idDisplayName = if T.null (iydDisplayName def)
                                              then iydName def
                                              else iydDisplayName def
                            , idTexture     = handle
                            , idWeight      = iydWeight def
                            , idKind        = iydKind def
                            , idCategory    = iydCategory def
                            , idMake        = iydMake def
                            , idMaterial    = iydMaterial def
                            , idQualitySpec   = (\r → (iyrsMin r, iyrsMax r))
                                              <$> iydQuality def
                            , idConditionSpec = (\r → (iyrsMin r, iyrsMax r))
                                              <$> iydCondition def
                            , idContainer   = container
                            , idFood        = food
                            , idWeapon      = weapon
                            , idUnequippable = iydUnequippable def
                            , idBuffs       = map
                                (\b → ItemBuff
                                    { ibStat = iybStat b
                                    , ibAmount = iybAmount b
                                    , ibScalesWithCondition =
                                        iybScalesWithCondition b
                                    })
                                (iydBuffs def)
                            }

                    atomicModifyIORef' (itemManagerRef env) $ \im →
                        (ItemManager
                            { imDefs = HM.insert (iydName def) itemDef
                                                (imDefs im) }, ())

                    return (acc + 1)
                    ) (0 ∷ Int) defs

                logInfo logger CatAsset $
                    "loadItemYaml: loaded " <> T.pack (show total)
                    <> " item definitions from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

-- * Ground items (items lying in the world — see Item.Ground)

-- | Resolve the active (first visible) world's state — ground items
--   live on the world page being played.
activeWorld ∷ EngineEnv → IO (Maybe WorldState)
activeWorld env = do
    mgr ← readIORef (worldManagerRef env)
    pure $ case wmVisible mgr of
        (pid:_) → lookup pid (wmWorlds mgr)
        []      → case wmWorlds mgr of
            ((_, ws):_) → Just ws
            []          → Nothing

-- | item.listDefs() → array of {name, displayName, category, weight}
--   Sorted by name for a stable debug-overlay listing.
itemListDefsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemListDefsFn env = do
    im ← Lua.liftIO $ readIORef (itemManagerRef env)
    let defs = L.sortOn idName (HM.elems (imDefs im))
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] defs) $ \(i, d) → do
        Lua.newtable
        Lua.pushstring (TE.encodeUtf8 (idName d))
        Lua.setfield (Lua.nth 2) "name"
        Lua.pushstring (TE.encodeUtf8 (idDisplayName d))
        Lua.setfield (Lua.nth 2) "displayName"
        Lua.pushstring (TE.encodeUtf8 (idCategory d))
        Lua.setfield (Lua.nth 2) "category"
        Lua.pushnumber (Lua.Number (realToFrac (idWeight d)))
        Lua.setfield (Lua.nth 2) "weight"
        Lua.rawseti (Lua.nth 2) (fromIntegral i)
    return 1

-- | item.spawnGround(defName, x, y [, props]) → gid | nil
--   Spawns an item into the world at float tile coords. Optional
--   props table: fill, quality, condition (defaults 0/100/100).
--   Resting height derives from terrain at render time, so items on
--   slopes sit on the incline and items over dug tiles drop with
--   the terrain.
itemSpawnGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemSpawnGroundFn env = do
    nameArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    propsTy ← Lua.ltype 4
    let getProp ∷ Lua.Name → Float → Lua.LuaE Lua.Exception Float
        getProp key def = case propsTy of
            Lua.TypeTable → do
                _ ← Lua.getfield 4 key
                mv ← Lua.tonumber Lua.top
                Lua.pop 1
                pure $ case mv of
                    Just (Lua.Number n) → realToFrac n
                    _ → def
            _ → pure def
    fill ← getProp "fill" 0.0
    quality ← getProp "quality" 100.0
    condition ← getProp "condition" 100.0
    case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → do
            let name = TE.decodeUtf8 nameBS
            im ← Lua.liftIO $ readIORef (itemManagerRef env)
            mWs ← Lua.liftIO $ activeWorld env
            case (HM.member name (imDefs im), mWs) of
                (True, Just ws) → do
                    let inst = ItemInstance
                            { iiDefName = name
                            , iiCurrentFill = fill
                            , iiQuality = quality
                            , iiCondition = condition
                            }
                    gid ← Lua.liftIO $
                        atomicModifyIORef' (wsGroundItemsRef ws) $
                            spawnGroundItem inst (realToFrac x) (realToFrac y)
                    Lua.pushinteger (fromIntegral gid)
                    return 1
                _ → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | item.listGround() → array of {id, defName, x, y, fill, quality,
--   condition}
itemListGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemListGroundFn env = do
    mWs ← Lua.liftIO $ activeWorld env
    case mWs of
        Nothing → Lua.pushnil >> return 1
        Just ws → do
            gis ← Lua.liftIO $ readIORef (wsGroundItemsRef ws)
            Lua.newtable
            forM_ (zip [1 ∷ Int ..] (HM.toList (gisItems gis))) $
                \(i, (gid, gi)) → do
                    Lua.newtable
                    Lua.pushinteger (fromIntegral gid)
                    Lua.setfield (Lua.nth 2) "id"
                    Lua.pushstring (TE.encodeUtf8 (iiDefName (giInst gi)))
                    Lua.setfield (Lua.nth 2) "defName"
                    Lua.pushnumber (Lua.Number (realToFrac (giX gi)))
                    Lua.setfield (Lua.nth 2) "x"
                    Lua.pushnumber (Lua.Number (realToFrac (giY gi)))
                    Lua.setfield (Lua.nth 2) "y"
                    Lua.pushnumber (Lua.Number (realToFrac
                        (iiCurrentFill (giInst gi))))
                    Lua.setfield (Lua.nth 2) "fill"
                    Lua.pushnumber (Lua.Number (realToFrac
                        (iiQuality (giInst gi))))
                    Lua.setfield (Lua.nth 2) "quality"
                    Lua.pushnumber (Lua.Number (realToFrac
                        (iiCondition (giInst gi))))
                    Lua.setfield (Lua.nth 2) "condition"
                    Lua.rawseti (Lua.nth 2) (fromIntegral i)
            return 1

-- | item.removeGround(gid) → true | false
itemRemoveGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemRemoveGroundFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Just n → do
            mWs ← Lua.liftIO $ activeWorld env
            case mWs of
                Nothing → Lua.pushboolean False >> return 1
                Just ws → do
                    mGi ← Lua.liftIO $
                        atomicModifyIORef' (wsGroundItemsRef ws) $
                            removeGroundItem (fromIntegral n)
                    Lua.pushboolean (isJust mGi)
                    return 1
        _ → Lua.pushboolean False >> return 1

-- | item.groundCount() → n (headless tests / HUD readouts)
itemGroundCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemGroundCountFn env = do
    mWs ← Lua.liftIO $ activeWorld env
    case mWs of
        Nothing → Lua.pushinteger 0 >> return 1
        Just ws → do
            gis ← Lua.liftIO $ readIORef (wsGroundItemsRef ws)
            Lua.pushinteger (fromIntegral (HM.size (gisItems gis)))
            return 1

-- | item.hitTestAt(px, py) → gid | nil — topmost ground item whose
--   sprite contains the window-pixel point (unit.hitTestAt analog).
itemHitTestAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemHitTestAtFn env = do
    xArg ← Lua.tonumber 1
    yArg ← Lua.tonumber 2
    case (xArg, yArg) of
        (Just (Lua.Number x), Just (Lua.Number y)) → do
            mWs ← Lua.liftIO $ activeWorld env
            case mWs of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    mGid ← Lua.liftIO $
                        hitTestGroundItemAt env ws (realToFrac x)
                                                   (realToFrac y)
                    case mGid of
                        Just gid → Lua.pushinteger (fromIntegral gid)
                        Nothing  → Lua.pushnil
                    return 1
        _ → Lua.pushnil >> return 1

-- | item.select(gid) / item.deselect() / item.getSelected() — the
--   world-view ground-item selection (white outline + info panel).
itemSelectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemSelectFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Just n → do
            mWs ← Lua.liftIO $ activeWorld env
            case mWs of
                Just ws → Lua.liftIO $
                    atomicModifyIORef' (wsCursorRef ws) $ \cs →
                        (cs { selectedGroundItem =
                                Just (fromIntegral n) }, ())
                Nothing → pure ()
        _ → pure ()
    return 0

itemDeselectFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemDeselectFn env = do
    mWs ← Lua.liftIO $ activeWorld env
    case mWs of
        Just ws → Lua.liftIO $
            atomicModifyIORef' (wsCursorRef ws) $ \cs →
                (cs { selectedGroundItem = Nothing }, ())
        Nothing → pure ()
    return 0

itemGetSelectedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemGetSelectedFn env = do
    mWs ← Lua.liftIO $ activeWorld env
    case mWs of
        Nothing → Lua.pushnil >> return 1
        Just ws → do
            cs ← Lua.liftIO $ readIORef (wsCursorRef ws)
            case selectedGroundItem cs of
                Just gid → Lua.pushinteger (fromIntegral gid)
                Nothing  → Lua.pushnil
            return 1

-- | item.pickupGround(uid, gid) → true | false — atomically move a
--   ground item into a unit's inventory, PRESERVING the instance
--   (fill / quality / condition), unlike unit.addItem which rolls
--   fresh values. Remove-first ordering means two racing pickups
--   can't duplicate the item: the loser's remove returns Nothing.
--   If the unit vanished between remove and insert, the item is
--   re-spawned at its old position (new id).
itemPickupGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemPickupGroundFn env = do
    uidArg ← Lua.tointeger 1
    gidArg ← Lua.tointeger 2
    case (uidArg, gidArg) of
        (Just u, Just g) → do
            mWs ← Lua.liftIO $ activeWorld env
            case mWs of
                Nothing → Lua.pushboolean False >> return 1
                Just ws → do
                    ok ← Lua.liftIO $ do
                        mGi ← atomicModifyIORef' (wsGroundItemsRef ws) $
                            removeGroundItem (fromIntegral g)
                        case mGi of
                            Nothing → return False
                            Just gi → do
                                let uid = UnitId (fromIntegral u)
                                inserted ← atomicModifyIORef'
                                    (unitManagerRef env) $ \um →
                                        case HM.lookup uid
                                                 (umInstances um) of
                                            Nothing → (um, False)
                                            Just inst →
                                                let inst' = inst
                                                      { uiInventory =
                                                          uiInventory inst
                                                          ++ [giInst gi] }
                                                in (um { umInstances =
                                                       HM.insert uid inst'
                                                         (umInstances um) }
                                                   , True)
                                if inserted
                                    then return True
                                    else do
                                        -- Unit gone: put it back.
                                        _ ← atomicModifyIORef'
                                            (wsGroundItemsRef ws) $
                                            spawnGroundItem (giInst gi)
                                                (giX gi) (giY gi)
                                        return False
                    -- Deselect if the picked item was selected.
                    Lua.liftIO $
                        atomicModifyIORef' (wsCursorRef ws) $ \cs →
                            ( if selectedGroundItem cs
                                   ≡ Just (fromIntegral g)
                              then cs { selectedGroundItem = Nothing }
                              else cs
                            , () )
                    Lua.pushboolean ok
                    return 1
        _ → Lua.pushboolean False >> return 1
