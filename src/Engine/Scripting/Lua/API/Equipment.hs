{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
module Engine.Scripting.Lua.API.Equipment
    ( loadEquipmentYamlFn
    , equipmentGetClassFn
    , equipmentGetClassNamesFn
    , equipmentEquipFn
    , equipmentUnequipFn
    , equipmentGetLoadoutFn
    , equipmentEquipAccessoryFn
    , equipmentUnequipAccessoryFn
    , equipmentGetAccessoriesFn
    , pushItemInstance
    ) where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM, forM_, unless)
import Data.List (foldl')
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister)
import Engine.Asset.YamlEquipment
import Equipment.Types
import Item.Types (ItemInstance(..), ItemDef(..), ItemWeapon(..),
                   ItemBuff(..), ItemContainer(..),
                   ItemManager(..), lookupItemDef)
import Unit.Types (UnitInstance(..), UnitManager(..), UnitId(..),
                   UnitDef(..), StatModifier(..))

-- | equipment.loadYaml(path) — parses a YAML file describing one or
--   more equipment classes, loads each class's silhouette texture, and
--   registers the classes into the EquipmentClassManager. Returns the
--   number of classes loaded.
loadEquipmentYamlFn ∷ EngineEnv → LuaBackendState
                    → Lua.LuaE Lua.Exception Lua.NumResults
loadEquipmentYamlFn env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → do
            Lua.pushnumber 0
            return 1
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8 pathBS)
            count ← Lua.liftIO $ do
                logger ← readIORef (loggerRef env)
                classes ← loadEquipmentYaml logger filePath
                let (lteq, _) = lbsMsgQueues backendState

                total ← foldM (\acc c → do
                    let regName = "equipment_" <> eycName c
                    handle ← loadAndRegister env backendState lteq
                                regName (T.unpack (eycSilhouette c))

                    let slots = map
                            (\s → EquipmentSlot
                                { esId   = eysId s
                                , esName = if T.null (eysName s)
                                           then eysId s
                                           else eysName s
                                , esKind = eysKind s
                                , esX    = eysX s
                                , esY    = eysY s
                                , esW    = eysW s
                                , esH    = eysH s
                                })
                            (eycSlots c)
                        ecDef = EquipmentClass
                            { ecName          = eycName c
                            , ecSilhouetteTex = handle
                            , ecSilhouetteW   = eycSilhouetteW c
                            , ecSilhouetteH   = eycSilhouetteH c
                            , ecSlots         = slots
                            }

                    atomicModifyIORef' (equipmentClassManagerRef env) $ \m →
                        (EquipmentClassManager
                            { ecmDefs = HM.insert (eycName c) ecDef
                                                  (ecmDefs m) }, ())

                    return (acc + 1)
                    ) (0 ∷ Int) classes

                logInfo logger CatAsset $
                    "loadEquipmentYaml: loaded " <> T.pack (show total)
                    <> " equipment classes from " <> T.pack filePath
                return total

            Lua.pushnumber (Lua.Number (fromIntegral count))
            return 1

-- | equipment.getClass(name) → table or nil. The returned table is the
--   render-side view of an EquipmentClass:
--   { name, silhouette = <textureHandle int>, silhouetteW, silhouetteH,
--     slots = { { id, name, kind, x, y, w, h }, … } }
equipmentGetClassFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentGetClassFn env = do
    nameArg ← Lua.tostring 1
    case nameArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just nameBS → do
            let name = TE.decodeUtf8 nameBS
            mClass ← Lua.liftIO $ do
                mgr ← readIORef (equipmentClassManagerRef env)
                pure (lookupEquipmentClass name mgr)
            case mClass of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just c → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (ecName c))
                    Lua.setfield (-2) "name"
                    let TextureHandle texInt = ecSilhouetteTex c
                    Lua.pushinteger (fromIntegral texInt)
                    Lua.setfield (-2) "silhouette"
                    Lua.pushinteger (fromIntegral (ecSilhouetteW c))
                    Lua.setfield (-2) "silhouetteW"
                    Lua.pushinteger (fromIntegral (ecSilhouetteH c))
                    Lua.setfield (-2) "silhouetteH"
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] (ecSlots c)) $ \(i, s) → do
                        Lua.newtable
                        Lua.pushstring (TE.encodeUtf8 (esId s))
                        Lua.setfield (-2) "id"
                        Lua.pushstring (TE.encodeUtf8 (esName s))
                        Lua.setfield (-2) "name"
                        Lua.pushstring (TE.encodeUtf8 (esKind s))
                        Lua.setfield (-2) "kind"
                        Lua.pushinteger (fromIntegral (esX s))
                        Lua.setfield (-2) "x"
                        Lua.pushinteger (fromIntegral (esY s))
                        Lua.setfield (-2) "y"
                        Lua.pushinteger (fromIntegral (esW s))
                        Lua.setfield (-2) "w"
                        Lua.pushinteger (fromIntegral (esH s))
                        Lua.setfield (-2) "h"
                        Lua.rawseti (-2) (fromIntegral i)
                    Lua.setfield (-2) "slots"
                    return 1

-- | equipment.getClassNames() → array of strings (sorted by HashMap
--   iteration order, i.e. arbitrary). Returns every registered class.
equipmentGetClassNamesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentGetClassNamesFn env = do
    names ← Lua.liftIO $ do
        mgr ← readIORef (equipmentClassManagerRef env)
        pure (HM.keys (ecmDefs mgr))
    Lua.newtable
    forM_ (zip [1 ∷ Int ..] names) $ \(i, n) → do
        Lua.pushstring (TE.encodeUtf8 n)
        Lua.rawseti (-2) (fromIntegral i)
    return 1

-- | Drop the first inventory entry matching @defName@. Returns
--   @(remainingInventory, Just removedInstance)@ if found, else
--   @(originalInventory, Nothing)@.
removeFirstFromInventory ∷ Text → [ItemInstance]
                         → ([ItemInstance], Maybe ItemInstance)
removeFirstFromInventory defName = go []
  where
    go acc [] = (reverse acc, Nothing)
    go acc (x:xs)
        | iiDefName x ≡ defName = (reverse acc ++ xs, Just x)
        | otherwise             = go (x : acc) xs

-- | Effective buff delta after applying condition scaling.
buffEffectiveDelta ∷ ItemBuff → Float → Float
buffEffectiveDelta b cond
    | ibScalesWithCondition b = ibAmount b * (cond / 100)
    | otherwise               = ibAmount b

-- | Apply every buff on an item to the unit's `uiModifiers`. Source
--   string is the item's display_name; identical sources on the same
--   stat collapse via the existing dedup-by-source rule.
applyItemBuffs ∷ ItemInstance → ItemDef
               → HM.HashMap Text [StatModifier]
               → HM.HashMap Text [StatModifier]
applyItemBuffs inst iDef mods = foldl' applyOne mods (idBuffs iDef)
  where
    src   = idDisplayName iDef
    cond  = iiCondition inst
    applyOne acc b =
        let delta = buffEffectiveDelta b cond
            m     = StatModifier
                      { smDelta  = delta
                      , smSource = src
                      , smExpiry = Nothing
                      , smPercent = 0
                      }
            existing = HM.lookupDefault [] (ibStat b) acc
            others   = filter (\x → smSource x ≢ src) existing
        in HM.insert (ibStat b) (m : others) acc

-- | Remove every modifier with @source@ across all stats. Mirrors
--   `unit.removeModifier` exactly so equip/unequip stay symmetric.
removeModifiersBySource ∷ Text
                        → HM.HashMap Text [StatModifier]
                        → HM.HashMap Text [StatModifier]
removeModifiersBySource src =
    HM.map (filter (\m → smSource m ≢ src))

-- | equipment.equip(uid, slotId, itemDefName) → bool. Moves the first
--   inventory item matching @itemDefName@ into the named slot,
--   validating that its kind matches the slot's accepted kind. If the
--   slot already has something equipped, that item goes back to the
--   inventory. Returns false on: unknown unit, unknown slot for the
--   unit's class, kind mismatch, or no matching item in inventory.
equipmentEquipFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentEquipFn env = do
    uidArg   ← Lua.tointeger 1
    slotArg  ← Lua.tostring 2
    itemArg  ← Lua.tostring 3
    case (uidArg, slotArg, itemArg) of
        (Just n, Just slotBS, Just itemBS) → do
            let uid    = UnitId (fromIntegral n)
                slotId = TE.decodeUtf8 slotBS
                itemNm = TE.decodeUtf8 itemBS
            ok ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                ecMgr   ← readIORef (equipmentClassManagerRef env)
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            case HM.lookup (uiDefName inst) (umDefs um) of
                                Nothing → (um, False)
                                Just def → case udEquipmentClass def of
                                    Nothing → (um, False)
                                    Just clsNm →
                                        case lookupEquipmentClass clsNm ecMgr of
                                            Nothing → (um, False)
                                            Just cls →
                                                tryEquip um inst itemMgr cls
                                                  slotId itemNm uid
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1
  where
    tryEquip um inst itemMgr cls slotId itemNm uid =
        case [s | s ← ecSlots cls, esId s ≡ slotId] of
            []       → (um, False)
            (slot:_) → case lookupItemDef itemNm itemMgr of
                Nothing → (um, False)
                Just iDef
                    | idKind iDef ≢ esKind slot → (um, False)
                    | otherwise →
                        case removeFirstFromInventory itemNm
                                                       (uiInventory inst) of
                            (_, Nothing)        → (um, False)
                            (newInv, Just newI) →
                                let -- if something is already in the slot,
                                    -- bump it back to the inventory tail
                                    -- so the swap is atomic from Lua's
                                    -- perspective.
                                    existing = HM.lookup slotId
                                                 (uiEquipment inst)
                                    invAfter = case existing of
                                        Nothing → newInv
                                        Just e  → newInv ++ [e]
                                    eqAfter  = HM.insert slotId newI
                                                 (uiEquipment inst)
                                    inst'    = inst
                                        { uiInventory = invAfter
                                        , uiEquipment = eqAfter
                                        }
                                in (um { umInstances = HM.insert uid inst'
                                                         (umInstances um) },
                                    True)

-- | equipment.unequip(uid, slotId) → bool. Pops the item out of the
--   named slot and appends it to the unit's inventory. Returns false
--   if the unit doesn't exist or the slot was already empty.
equipmentUnequipFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentUnequipFn env = do
    uidArg  ← Lua.tointeger 1
    slotArg ← Lua.tostring 2
    case (uidArg, slotArg) of
        (Just n, Just slotBS) → do
            let uid    = UnitId (fromIntegral n)
                slotId = TE.decodeUtf8 slotBS
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst → case HM.lookup slotId (uiEquipment inst) of
                        Nothing → (um, False)
                        Just it →
                            let inst' = inst
                                  { uiInventory = uiInventory inst ++ [it]
                                  , uiEquipment = HM.delete slotId
                                                    (uiEquipment inst)
                                  }
                            in (um { umInstances = HM.insert uid inst'
                                                     (umInstances um) }, True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | equipment.getLoadout(uid) → table or nil. Each present slot maps
--   to a sub-table with the item def's render-visible fields plus its
--   current fill (so containers like an equipped flask show usage).
--   Absent slots are omitted; the renderer should treat missing keys
--   as empty. Returns nil if the unit doesn't exist.
--
--   Shape: { slotId = { defName, displayName, kind, weight,
--                       iconTex, currentFill }, … }
equipmentGetLoadoutFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentGetLoadoutFn env = do
    uidArg ← Lua.tointeger 1
    case uidArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPair ← Lua.liftIO $ do
                um      ← readIORef (unitManagerRef env)
                itemMgr ← readIORef (itemManagerRef env)
                pure $ do
                    inst ← HM.lookup uid (umInstances um)
                    pure (uiEquipment inst, itemMgr)
            case mPair of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just (eq, itemMgr) → do
                    Lua.newtable
                    forM_ (HM.toList eq) $ \(slotId, inst) → do
                        Lua.newtable
                        Lua.pushstring (TE.encodeUtf8 (iiDefName inst))
                        Lua.setfield (-2) "defName"
                        Lua.pushnumber
                            (Lua.Number (realToFrac (iiCurrentFill inst)))
                        Lua.setfield (-2) "currentFill"
                        -- Gate quality / condition on def specs so
                        -- canteens / rations don't show "100%" they
                        -- never had.
                        let mDef = lookupItemDef (iiDefName inst) itemMgr
                        case mDef >>= idQualitySpec of
                            Just _ → do
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iiQuality inst)))
                                Lua.setfield (-2) "quality"
                            Nothing → pure ()
                        case mDef >>= idConditionSpec of
                            Just _ → do
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iiCondition inst)))
                                Lua.setfield (-2) "condition"
                            Nothing → pure ()
                        case mDef of
                            Nothing → pure ()
                            Just iDef → do
                                Lua.pushstring
                                    (TE.encodeUtf8 (idDisplayName iDef))
                                Lua.setfield (-2) "displayName"
                                Lua.pushstring (TE.encodeUtf8 (idKind iDef))
                                Lua.setfield (-2) "kind"
                                Lua.pushstring
                                    (TE.encodeUtf8 (idCategory iDef))
                                Lua.setfield (-2) "category"
                                Lua.pushstring (TE.encodeUtf8 (idMake iDef))
                                Lua.setfield (-2) "make"
                                Lua.pushstring
                                    (TE.encodeUtf8 (idMaterial iDef))
                                Lua.setfield (-2) "material"
                                -- Instance weight, not the def mean —
                                -- gems vary per find (matches
                                -- getCarryingWeight + pushItemInstance).
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iiWeight inst)))
                                Lua.setfield (-2) "weight"
                                let TextureHandle texInt = idTexture iDef
                                Lua.pushinteger (fromIntegral texInt)
                                Lua.setfield (-2) "iconTex"
                                case idWeapon iDef of
                                    Just w → do
                                        Lua.newtable
                                        Lua.pushnumber
                                            (Lua.Number
                                                (realToFrac (iwBladeLength w)))
                                        Lua.setfield (-2) "bladeLength"
                                        Lua.pushnumber
                                            (Lua.Number
                                                (realToFrac (iwBaseSharpness w)))
                                        Lua.setfield (-2) "baseSharpness"
                                        Lua.pushnumber
                                            (Lua.Number
                                                (realToFrac (iwStabEff w)))
                                        Lua.setfield (-2) "stabEffectiveness"
                                        Lua.pushnumber
                                            (Lua.Number
                                                (realToFrac (iwSlashEff w)))
                                        Lua.setfield (-2) "slashEffectiveness"
                                        Lua.pushnumber
                                            (Lua.Number
                                                (realToFrac (iwBluntEff w)))
                                        Lua.setfield (-2) "bluntEffectiveness"
                                        Lua.setfield (-2) "weapon"
                                    Nothing → pure ()
                        Lua.setfield (-2) (Lua.Name (TE.encodeUtf8 slotId))
                    return 1

-- | Push an item-instance's render-side fields onto a freshly-created
--   Lua table at the top of the stack. Used by both getLoadout's slot
--   tables and getAccessories' list entries — same shape, so the Lua
--   side's hint-builder doesn't have to branch.
pushItemInstance ∷ ItemInstance → ItemManager → Lua.LuaE Lua.Exception ()
pushItemInstance inst itemMgr = do
    Lua.pushstring (TE.encodeUtf8 (iiDefName inst))
    Lua.setfield (-2) "defName"
    Lua.pushnumber (Lua.Number (realToFrac (iiCurrentFill inst)))
    Lua.setfield (-2) "currentFill"
    -- Quality / condition only surface when the def declares them —
    -- items like canteens / rations don't have these qualities and
    -- shouldn't show "100%" in tooltips.
    let mDef = lookupItemDef (iiDefName inst) itemMgr
    case mDef >>= idQualitySpec of
        Just _ → do
            Lua.pushnumber (Lua.Number (realToFrac (iiQuality inst)))
            Lua.setfield (-2) "quality"
        Nothing → pure ()
    case mDef >>= idConditionSpec of
        Just _ → do
            Lua.pushnumber (Lua.Number (realToFrac (iiCondition inst)))
            Lua.setfield (-2) "condition"
        Nothing → pure ()
    case mDef of
        Nothing → pure ()
        Just iDef → do
            Lua.pushstring (TE.encodeUtf8 (idDisplayName iDef))
            Lua.setfield (-2) "displayName"
            Lua.pushstring (TE.encodeUtf8 (idKind iDef))
            Lua.setfield (-2) "kind"
            Lua.pushstring (TE.encodeUtf8 (idCategory iDef))
            Lua.setfield (-2) "category"
            Lua.pushstring (TE.encodeUtf8 (idMake iDef))
            Lua.setfield (-2) "make"
            Lua.pushstring (TE.encodeUtf8 (idMaterial iDef))
            Lua.setfield (-2) "material"
            -- Instance weight, not the def mean — gems vary per find.
            Lua.pushnumber (Lua.Number (realToFrac (iiWeight inst)))
            Lua.setfield (-2) "weight"
            let TextureHandle texInt = idTexture iDef
            Lua.pushinteger (fromIntegral texInt)
            Lua.setfield (-2) "iconTex"
            Lua.pushboolean (idUnequippable iDef)
            Lua.setfield (-2) "unequippable"
            case idContainer iDef of
                Just c → do
                    Lua.pushnumber (Lua.Number (realToFrac (icCapacity c)))
                    Lua.setfield (-2) "capacity"
                    Lua.pushstring (TE.encodeUtf8 (icHolds c))
                    Lua.setfield (-2) "holds"
                Nothing → pure ()
            case idWeapon iDef of
                Just w → do
                    Lua.newtable
                    Lua.pushnumber (Lua.Number (realToFrac (iwBladeLength w)))
                    Lua.setfield (-2) "bladeLength"
                    Lua.pushnumber (Lua.Number (realToFrac (iwBaseSharpness w)))
                    Lua.setfield (-2) "baseSharpness"
                    Lua.pushnumber (Lua.Number (realToFrac (iwStabEff w)))
                    Lua.setfield (-2) "stabEffectiveness"
                    Lua.pushnumber (Lua.Number (realToFrac (iwSlashEff w)))
                    Lua.setfield (-2) "slashEffectiveness"
                    Lua.pushnumber (Lua.Number (realToFrac (iwBluntEff w)))
                    Lua.setfield (-2) "bluntEffectiveness"
                    Lua.setfield (-2) "weapon"
                Nothing → pure ()
            -- Buffs: pushed as an array of { stat, amount, scalesWithCondition }
            unless (null (idBuffs iDef)) $ do
                Lua.newtable
                forM_ (zip [1 ∷ Int ..] (idBuffs iDef)) $ \(i, b) → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (ibStat b))
                    Lua.setfield (-2) "stat"
                    Lua.pushnumber (Lua.Number (realToFrac (ibAmount b)))
                    Lua.setfield (-2) "amount"
                    Lua.pushboolean (ibScalesWithCondition b)
                    Lua.setfield (-2) "scalesWithCondition"
                    Lua.rawseti (-2) (fromIntegral i)
                Lua.setfield (-2) "buffs"

-- | equipment.equipAccessory(uid, itemDefName) → bool. Moves the
--   first matching inventory item to the end of the unit's accessory
--   list. Returns false if the unit or item doesn't exist.
equipmentEquipAccessoryFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentEquipAccessoryFn env = do
    uidArg  ← Lua.tointeger 1
    nameArg ← Lua.tostring 2
    case (uidArg, nameArg) of
        (Just n, Just nameBS) → do
            let uid    = UnitId (fromIntegral n)
                defName = TE.decodeUtf8 nameBS
            ok ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            case removeFirstFromInventory defName
                                                           (uiInventory inst) of
                                (_, Nothing) → (um, False)
                                (newInv, Just newI) →
                                    -- Apply the accessory's buffs to the
                                    -- unit's modifier list so combat /
                                    -- stat display sees them immediately.
                                    let mods' = case lookupItemDef
                                                  (iiDefName newI) itemMgr of
                                          Just d  → applyItemBuffs newI d
                                                       (uiModifiers inst)
                                          Nothing → uiModifiers inst
                                        inst' = inst
                                          { uiInventory   = newInv
                                          , uiAccessories =
                                              uiAccessories inst ++ [newI]
                                          , uiModifiers   = mods'
                                          }
                                    in (um { umInstances = HM.insert uid inst'
                                                             (umInstances um) },
                                        True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | equipment.unequipAccessory(uid, index) → bool. Pops the accessory
--   at the given 1-based index and appends it to the unit's
--   inventory. Returns false on missing unit, out-of-range index, or
--   if the accessory's def is flagged `unequippable`.
equipmentUnequipAccessoryFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentUnequipAccessoryFn env = do
    uidArg ← Lua.tointeger 1
    idxArg ← Lua.tointeger 2
    case (uidArg, idxArg) of
        (Just n, Just i) → do
            let uid    = UnitId (fromIntegral n)
                idx0   = fromIntegral i - 1
            ok ← Lua.liftIO $ do
                itemMgr ← readIORef (itemManagerRef env)
                atomicModifyIORef' (unitManagerRef env) $ \um →
                    case HM.lookup uid (umInstances um) of
                        Nothing → (um, False)
                        Just inst →
                            let xs = uiAccessories inst
                            in if idx0 < 0 ∨ idx0 >= length xs
                                 then (um, False)
                                 else
                                   let target = xs !! idx0
                                       mDef = lookupItemDef
                                                (iiDefName target) itemMgr
                                       defLocked = case mDef of
                                           Just d  → idUnequippable d
                                           Nothing → False
                                   in if defLocked
                                       then (um, False)
                                       else
                                         let xs' = take idx0 xs
                                                 ++ drop (idx0 + 1) xs
                                             -- Remove the accessory's
                                             -- buffs from the unit's
                                             -- modifier list (by source =
                                             -- item display_name).
                                             mods' = case mDef of
                                                Just d  → removeModifiersBySource
                                                            (idDisplayName d)
                                                            (uiModifiers inst)
                                                Nothing → uiModifiers inst
                                             inst' = inst
                                               { uiAccessories = xs'
                                               , uiInventory   =
                                                   uiInventory inst ++ [target]
                                               , uiModifiers   = mods'
                                               }
                                         in (um { umInstances =
                                                    HM.insert uid inst'
                                                      (umInstances um) },
                                             True)
            Lua.pushboolean ok
            return 1
        _ → do
            Lua.pushboolean False
            return 1

-- | equipment.getAccessories(uid) → array of item-instance tables (in
--   wear order) or nil if the unit doesn't exist.
equipmentGetAccessoriesFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
equipmentGetAccessoriesFn env = do
    uidArg ← Lua.tointeger 1
    case uidArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPair ← Lua.liftIO $ do
                um      ← readIORef (unitManagerRef env)
                itemMgr ← readIORef (itemManagerRef env)
                pure $ do
                    inst ← HM.lookup uid (umInstances um)
                    pure (uiAccessories inst, itemMgr)
            case mPair of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just (acc, itemMgr) → do
                    Lua.newtable
                    forM_ (zip [1 ∷ Int ..] acc) $ \(i, inst) → do
                        Lua.newtable
                        pushItemInstance inst itemMgr
                        Lua.rawseti (-2) (fromIntegral i)
                    return 1
