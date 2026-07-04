{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
module Engine.Scripting.Lua.API.Items
    ( loadItemYamlFn
    , itemListDefsFn
    , itemSpawnGroundFn
    , itemListGroundFn
    , itemRemoveGroundFn
    , itemGroundCountFn
    , itemGetGroundTempFn
    , itemSetGroundTempFn
    , itemHitTestAtFn
    , itemSelectFn
    , itemDeselectFn
    , itemGetSelectedFn
    , itemPickupGroundFn
    , itemDebugQuadsFn
    ) where

import UPrelude
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), activeWorldState, freshItemInstanceId)
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister, resolveTexturePath)
import Engine.Asset.YamlTextures (lookupTextureName)
import Engine.Asset.YamlItems
import Item.Ground (GroundItem(..), GroundItems(..), spawnGroundItem
                   , removeGroundItem)
import Item.Roll (rollItemWeight)
import Item.Temperature (effectiveItemTemp)
import Item.Types
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import World.Cursor.Types (CursorState(..))
import qualified Data.Vector as V
import Engine.Graphics.Camera (Camera2D(..))
import Engine.Graphics.Vulkan.Types.Vertex (Vertex(..), Vec2(..))
import Engine.Scene.Types (SortableQuad(..))
import World.Render.GroundItemQuads (hitTestGroundItemAt
                                    , renderGroundItemQuads)
import World.Types (WorldManager(..), WorldState(..), WorldPageId(..)
                   , WorldGenParams(..), wmWorlds)
import World.Weather.Ambient (ambientTempAt)

-- | If the preferred path doesn't exist on disk, swap in the equipment
--   missing-texture placeholder so loadAndRegister has *something* to
--   queue. Logged so missing assets are visible during iteration.
--   The fallback path itself isn't checked — if you delete it too,
--   you'll get the usual broken-texture behaviour at draw time.
missingEquipmentTexture ∷ FilePath
missingEquipmentTexture = "assets/textures/ui/placeholders/missing_equipment.png"

-- | Overlay drawn over a broken item's sprite. Loaded once (lazily,
--   alongside item sprites) and registered under this name; the ground-
--   item renderer looks it up by name via the texture-name registry.
brokenEquipmentTexture ∷ FilePath
brokenEquipmentTexture = "assets/textures/ui/placeholders/broken_equipment.png"

brokenEquipmentTexName ∷ Text
brokenEquipmentTexName = "broken_equipment"

resolveSpritePath ∷ EngineEnv → FilePath → IO FilePath
resolveSpritePath env = resolveTexturePath env "Item sprite" missingEquipmentTexture

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

                -- Register the broken-weapon overlay once (same flow as
                -- item sprites). The ground-item renderer fetches it by
                -- name from the texture-name registry.
                reg0 ← readIORef (textureNameRegistryRef env)
                when (isNothing (lookupTextureName brokenEquipmentTexName reg0)) $
                    void $ loadAndRegister env backendState lteq
                               brokenEquipmentTexName brokenEquipmentTexture

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
                                { icCapacity    = iycCapacity c
                                , icHolds       = iycHolds c
                                , icFillWeight  = iycFillWeight c
                                , icDefaultFill = iycDefaultFill c
                                })
                            (iydContainer def)
                        food = fmap
                            (\f → ItemFood
                                { ifCalories      = iyfCalories f
                                , ifCaloriesPerKg = iyfCaloriesPerKg f
                                })
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
                                , iwLength         = if iywLength w > 0
                                                     then iywLength w
                                                     else iywBladeLength w
                                , iwCenterOfMass   = iywCenterOfMass w
                                })
                            (iydWeapon def)
                        armor = fmap
                            (\a → ItemArmor
                                { iaThickness = iyaThickness a
                                , iaCovers    = iyaCovers a
                                })
                            (iydArmor def)
                        (wMean, wSpec) = case iydWeight def of
                            WeightFixed w   → (w, Nothing)
                            WeightSpec m r  → (m, Just (m, r))
                        itemDef = ItemDef
                            { idName        = iydName def
                            , idDisplayName = if T.null (iydDisplayName def)
                                              then iydName def
                                              else iydDisplayName def
                            , idTexture     = handle
                            , idWeight      = wMean
                            , idWeightSpec  = wSpec
                            , idKind        = iydKind def
                            , idCategory    = iydCategory def
                            , idMake        = iydMake def
                            , idMaterial    = iydMaterial def
                            , idQualitySpec   = (\r → (iyrsMin r, iyrsMax r))
                                              <$> iydQuality def
                            , idConditionSpec = (\r → (iyrsMin r, iyrsMax r))
                                              <$> iydCondition def
                            , idQualityTiers = map
                                (\t → QualityTier (iyqtMin t) (iyqtLabel t))
                                (iydQualityTiers def)
                            , idContainer   = container
                            , idDefaultContents =
                                [ (iycoItem c, iycoCount c, iycoFill c)
                                | c ← iydContents def ]
                            , idFood        = food
                            , idWeapon      = weapon
                            , idArmor       = armor
                            , idUnequippable = iydUnequippable def
                            , idBuffs       = map
                                (\b → ItemBuff
                                    { ibStat = iybStat b
                                    , ibAmount = iybAmount b
                                    , ibPercent = iybPercent b
                                    , ibScalesWithCondition =
                                        iybScalesWithCondition b
                                    })
                                (iydBuffs def)
                            , idInsulation  = iydInsulation def
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
activeWorld = activeWorldState

-- | Resolve which world page a ground-item op targets: a named page
--   (any in wmWorlds, even hidden / non-active) when a page-id is
--   given, else the active world. Location content-spawning (#90)
--   passes the page id so an item spawned into a hidden secondary
--   page's location lands on THAT page — mirrors
--   'Engine.Scripting.Lua.API.Structure.resolveStructurePage'.
resolveItemPage ∷ EngineEnv → Maybe Text → IO (Maybe WorldState)
resolveItemPage env (Just pid) = do
    mgr ← readIORef (worldManagerRef env)
    pure $ lookup (WorldPageId pid) (wmWorlds mgr)
resolveItemPage env Nothing = activeWorld env

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

-- | item.spawnGround(defName, x, y [, props] [, pageId]) → gid | nil
--   Spawns an item into the world at float tile coords. Optional
--   props table: fill, quality, condition (defaults 0/100/100) and
--   temp (°C — spawns the item hot/cold; omitted = at ambient, #344).
--   Resting height derives from terrain at render time, so items on
--   slopes sit on the incline and items over dug tiles drop with
--   the terrain. An explicit pageId (slot 5) pins the spawn to that
--   live page (even hidden) instead of the active world — location
--   content-spawning (#90) passes its own page so an item lands on
--   the page its location is on.
itemSpawnGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemSpawnGroundFn env = do
    nameArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    propsTy ← Lua.ltype 4
    pageArg ← Lua.tostring 5
    let getMaybeProp ∷ Lua.Name → Lua.LuaE Lua.Exception (Maybe Float)
        getMaybeProp key = case propsTy of
            Lua.TypeTable → do
                _ ← Lua.getfield 4 key
                mv ← Lua.tonumber Lua.top
                Lua.pop 1
                pure $ case mv of
                    Just (Lua.Number n) → Just (realToFrac n)
                    _ → Nothing
            _ → pure Nothing
        getProp ∷ Lua.Name → Float → Lua.LuaE Lua.Exception Float
        getProp key def = fromMaybe def ⊚ getMaybeProp key
    mFill ← getMaybeProp "fill"
    quality ← getProp "quality" 100.0
    condition ← getProp "condition" 100.0
    mTemp ← getMaybeProp "temp"
    case (nameArg, xArg, yArg) of
        (Just nameBS, Just x, Just y) → do
            let name = TE.decodeUtf8 nameBS
            im ← Lua.liftIO $ readIORef (itemManagerRef env)
            mWs ← Lua.liftIO $ resolveItemPage env (TE.decodeUtf8 <$> pageArg)
            case (HM.lookup name (imDefs im), mWs) of
                (Just iDef, Just ws) → do
                    wght ← Lua.liftIO $
                        rollItemWeight iDef (statRNGRef env)
                    iid ← Lua.liftIO $ freshItemInstanceId env
                    -- No explicit fill from the caller → the def's
                    -- default_fill (so a loot-rolled quinoa sack spawns
                    -- holding quinoa). Explicit fill always wins; both
                    -- clamp to capacity, non-containers stay 0.
                    let fill = case idContainer iDef of
                            Just c  → max 0 (min (icCapacity c)
                                        (fromMaybe (icDefaultFill c) mFill))
                            Nothing → 0
                        inst = ItemInstance
                            { iiDefName = name
                            , iiCurrentFill = fill
                            , iiQuality = quality
                            , iiCondition = condition
                            , iiWeight = wght
                            , iiSharpness = 100.0
                            , iiContents = []
                            , iiInstanceId = iid
                            , iiTemp = mTemp
                            }
                    gid ← Lua.liftIO $
                        atomicModifyIORef' (wsGroundItemsRef ws) $
                            spawnGroundItem inst (realToFrac x) (realToFrac y)
                    Lua.pushinteger (fromIntegral gid)
                    return 1
                _ → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | item.listGround() → array of {id, defName, x, y, fill, quality,
--   qualityTier, condition, weight}. `weight` is the live total mass
--   (itemTotalWeight: empty weight + fill + nested contents), not the
--   static def weight. `qualityTier` (#345) is present only when the
--   def declares a quality spec.
itemListGroundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemListGroundFn env = do
    mWs ← Lua.liftIO $ activeWorld env
    im  ← Lua.liftIO $ readIORef (itemManagerRef env)
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
                    -- Tier label only when the def actually declares a
                    -- quality spec (mirrors unit.getInventory / the
                    -- equipment queries — #345).
                    case lookupItemDef (iiDefName (giInst gi)) im of
                        Just d | Just _ ← idQualitySpec d →
                            case qualityTierLabel d
                                     (iiQuality (giInst gi)) of
                                Just tier → do
                                    Lua.pushstring (TE.encodeUtf8 tier)
                                    Lua.setfield (Lua.nth 2) "qualityTier"
                                Nothing → pure ()
                        _ → pure ()
                    Lua.pushnumber (Lua.Number (realToFrac
                        (iiCondition (giInst gi))))
                    Lua.setfield (Lua.nth 2) "condition"
                    -- True live mass: empty weight + fill (at the
                    -- container's per-unit fill weight) + everything
                    -- nested in iiContents, computed recursively. A
                    -- stocked first-aid kit weighs its contents, not
                    -- just its empty case.
                    Lua.pushnumber (Lua.Number (realToFrac
                        (itemTotalWeight im (giInst gi))))
                    Lua.setfield (Lua.nth 2) "weight"
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

-- | item.getGroundTemp(gid) → °C | nil. The ground item's effective
--   temperature (#344): its tracked iiTemp when it's hotter/colder
--   than its surroundings, else the ambient air at its own tile
--   (elevation-corrected — World.Weather.Ambient). nil if the id
--   doesn't exist (or the item is untracked and the page has no gen
--   params to read an ambient from).
itemGetGroundTempFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemGetGroundTempFn env = do
    idArg ← Lua.tointeger 1
    mT ← case idArg of
        Nothing → pure Nothing
        Just n → Lua.liftIO $ do
            mWs ← activeWorld env
            case mWs of
                Nothing → pure Nothing
                Just ws → do
                    gis ← readIORef (wsGroundItemsRef ws)
                    case HM.lookup (fromIntegral n) (gisItems gis) of
                        Nothing → pure Nothing
                        Just gi → do
                            mp ← readIORef (wsGenParamsRef ws)
                            let mAmb = fmap (\p → ambientTempAt
                                    (wgpSeed p) (wgpPlates p)
                                    (wgpClimateState p) (wgpWorldSize p)
                                    (floor (giX gi)) (floor (giY gi))) mp
                            pure $ case mAmb of
                                Just amb → Just (effectiveItemTemp amb
                                                     (giInst gi))
                                Nothing  → iiTemp (giInst gi)
    case mT of
        Just t  → do
            Lua.pushnumber (Lua.Number (realToFrac t))
            return 1
        Nothing → Lua.pushnil >> return 1

-- | item.setGroundTemp(gid [, temp]) → bool. Sets a ground item's
--   tracked temperature (°C) — the "this item was made hot/cold" hook
--   (#344); the per-page tick then relaxes it toward the tile's
--   ambient. Omitting temp (or passing nil) clears the tracked value —
--   the item reads as "at ambient" again. False if the id doesn't
--   exist.
itemSetGroundTempFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemSetGroundTempFn env = do
    idArg ← Lua.tointeger 1
    tArg  ← Lua.tonumber 2
    case idArg of
        Nothing → Lua.pushboolean False >> return 1
        Just n → do
            let mT = case tArg of
                    Just (Lua.Number d) → Just (realToFrac d ∷ Float)
                    _                   → Nothing
            mWs ← Lua.liftIO $ activeWorld env
            case mWs of
                Nothing → Lua.pushboolean False >> return 1
                Just ws → do
                    ok ← Lua.liftIO $
                        atomicModifyIORef' (wsGroundItemsRef ws) $ \gis →
                            case HM.lookup (fromIntegral n) (gisItems gis) of
                                Nothing → (gis, False)
                                Just gi →
                                    let gi' = gi { giInst = (giInst gi)
                                                     { iiTemp = mT } }
                                    in ( gis { gisItems = HM.insert
                                                 (fromIntegral n) gi'
                                                 (gisItems gis) }
                                       , True )
                    Lua.pushboolean ok
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

-- | item.debugQuads() — run the actual ground-item render pass and
--   report what it produced: how many ground items exist, how many
--   quads survived geometry + culling, the camera snapshot the pass
--   saw, and the first few quads' screen rects. Splits "items aren't
--   spawning" from "geometry/culling drops them" from "they reach the
--   GPU but the shader hides them" without needing a debugger on the
--   render loop. Headless-safe (no GPU touched — slot lookups just
--   return 0 without a texture system).
itemDebugQuadsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
itemDebugQuadsFn env = do
    mWs ← Lua.liftIO $ activeWorld env
    case mWs of
        Nothing → Lua.pushnil >> return 1
        Just ws → do
            (quads, gis, cam) ← Lua.liftIO $ do
                q ← renderGroundItemQuads env ws 1.0
                g ← readIORef (wsGroundItemsRef ws)
                c ← readIORef (cameraRef env)
                pure (q, g, c)
            let (camX, camY) = camPosition cam
            Lua.newtable
            Lua.pushinteger (fromIntegral (HM.size (gisItems gis)))
            Lua.setfield (Lua.nth 2) "groundItems"
            Lua.pushinteger (fromIntegral (V.length quads))
            Lua.setfield (Lua.nth 2) "quads"
            Lua.pushnumber (Lua.Number (realToFrac camX))
            Lua.setfield (Lua.nth 2) "camX"
            Lua.pushnumber (Lua.Number (realToFrac camY))
            Lua.setfield (Lua.nth 2) "camY"
            Lua.pushinteger (fromIntegral (camZSlice cam))
            Lua.setfield (Lua.nth 2) "zSlice"
            Lua.pushnumber (Lua.Number (realToFrac (camZoom cam)))
            Lua.setfield (Lua.nth 2) "zoom"
            Lua.newtable
            forM_ (zip [1 ∷ Int ..]
                       (take 8 (V.toList quads))) $ \(i, sq) → do
                let Vec2 qx qy = pos (sqV0 sq)
                    Vec2 q2x q2y = pos (sqV2 sq)
                Lua.newtable
                Lua.pushnumber (Lua.Number (realToFrac qx))
                Lua.setfield (Lua.nth 2) "x0"
                Lua.pushnumber (Lua.Number (realToFrac qy))
                Lua.setfield (Lua.nth 2) "y0"
                Lua.pushnumber (Lua.Number (realToFrac q2x))
                Lua.setfield (Lua.nth 2) "x1"
                Lua.pushnumber (Lua.Number (realToFrac q2y))
                Lua.setfield (Lua.nth 2) "y1"
                Lua.pushnumber (Lua.Number
                    (realToFrac (atlasId (sqV0 sq))))
                Lua.setfield (Lua.nth 2) "texSlot"
                Lua.pushnumber (Lua.Number
                    (realToFrac (faceMapId (sqV0 sq))))
                Lua.setfield (Lua.nth 2) "fmSlot"
                Lua.pushnumber (Lua.Number
                    (realToFrac (sqSortKey sq)))
                Lua.setfield (Lua.nth 2) "sortKey"
                Lua.rawseti (Lua.nth 2) (fromIntegral i)
            Lua.setfield (Lua.nth 2) "sample"
            return 1
