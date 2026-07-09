{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Render-side item-instance field pushing, shared by the equipped
--   loadout and accessory-list queries (and, via pushItemInstance,
--   by Engine.Scripting.Lua.API.Buildings.Materials' ground-item
--   listing) — the Lua-table shape a tooltip needs to draw an item.
module Engine.Scripting.Lua.API.Equipment.Render
    ( pushItemInstance
    , equipmentGetLoadoutFn
    , equipmentGetAccessoriesFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv(..))
import Engine.Asset.Handle (TextureHandle(..))
import Item.Types (ItemInstance(..), ItemDef(..), ItemWeapon(..),
                   ItemBuff(..), ItemContainer(..),
                   ItemManager(..), lookupItemDef,
                   itemContentsSig, itemTotalWeight, qualityTierLabel)
import Unit.Types (UnitInstance(..), UnitManager(..), UnitId(..))

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
                        -- Process-unique identity (#67), same as
                        -- unit.getInventory / pushItemInstance.
                        Lua.pushinteger (fromIntegral (iiInstanceId inst))
                        Lua.setfield (-2) "instanceId"
                        Lua.pushnumber
                            (Lua.Number (realToFrac (iiCurrentFill inst)))
                        Lua.setfield (-2) "currentFill"
                        Lua.pushstring (TE.encodeUtf8 (itemContentsSig inst))
                        Lua.setfield (-2) "contentsKey"
                        -- Instance sharpness, not the def's base —
                        -- combat wear dulls the equipped weapon
                        -- (iiSharpness); the tooltip must show the live
                        -- value (matches unit.getInventory).
                        Lua.pushnumber
                            (Lua.Number (realToFrac (iiSharpness inst)))
                        Lua.setfield (-2) "sharpness"
                        -- Gate quality / condition on def specs so
                        -- canteens / rations don't show "100%" they
                        -- never had.
                        let mDef = lookupItemDef (iiDefName inst) itemMgr
                        case mDef of
                            Just d | Just _ ← idQualitySpec d → do
                                Lua.pushnumber
                                    (Lua.Number (realToFrac (iiQuality inst)))
                                Lua.setfield (-2) "quality"
                                -- Named tier (#345), e.g. "excellent".
                                case qualityTierLabel d (iiQuality inst) of
                                    Just tier → do
                                        Lua.pushstring (TE.encodeUtf8 tier)
                                        Lua.setfield (-2) "qualityTier"
                                    Nothing → pure ()
                            _ → pure ()
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
                                -- True carried mass (empty + fill + nested
                                -- contents) — matches getCarryingWeight +
                                -- pushItemInstance + unit.getInventory.
                                Lua.pushnumber
                                    (Lua.Number (realToFrac
                                        (itemTotalWeight itemMgr inst)))
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
    -- Process-unique identity so the UI can target THIS instance instead
    -- of the first inventory entry matching defName (#67).
    Lua.pushinteger (fromIntegral (iiInstanceId inst))
    Lua.setfield (-2) "instanceId"
    Lua.pushnumber (Lua.Number (realToFrac (iiCurrentFill inst)))
    Lua.setfield (-2) "currentFill"
    -- Signature of nested contents so item-containers (kits) split by
    -- internal state in the row key (#67A).
    Lua.pushstring (TE.encodeUtf8 (itemContentsSig inst))
    Lua.setfield (-2) "contentsKey"
    -- Instance sharpness, not the def's base — combat wear dulls the
    -- worn weapon (iiSharpness), and the inventory/loadout tooltip
    -- must show the live value, mirroring unit.getInventory.
    Lua.pushnumber (Lua.Number (realToFrac (iiSharpness inst)))
    Lua.setfield (-2) "sharpness"
    -- Tracked temperature (°C) — present only while the item is
    -- hotter/colder than its surroundings (#344); absent = at ambient
    -- (unit.getItemTemp resolves the ambient-aware effective value).
    case iiTemp inst of
        Just t → do
            Lua.pushnumber (Lua.Number (realToFrac t))
            Lua.setfield (-2) "temp"
        Nothing → pure ()
    -- Quality / condition only surface when the def declares them —
    -- items like canteens / rations don't have these qualities and
    -- shouldn't show "100%" in tooltips.
    let mDef = lookupItemDef (iiDefName inst) itemMgr
    case mDef of
        Just d | Just _ ← idQualitySpec d → do
            Lua.pushnumber (Lua.Number (realToFrac (iiQuality inst)))
            Lua.setfield (-2) "quality"
            -- Named tier (#345), e.g. "excellent" at 95%.
            case qualityTierLabel d (iiQuality inst) of
                Just tier → do
                    Lua.pushstring (TE.encodeUtf8 tier)
                    Lua.setfield (-2) "qualityTier"
                Nothing → pure ()
        _ → pure ()
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
            -- True carried mass (empty case + fill + nested contents) so a
            -- stocked kit / filled canteen reads its real weight and two
            -- kits with diverged contents differ visibly (#67A).
            Lua.pushnumber (Lua.Number (realToFrac (itemTotalWeight itemMgr inst)))
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
            -- Buffs: pushed as an array of
            -- { stat, amount, percent, scalesWithCondition }
            unless (null (idBuffs iDef)) $ do
                Lua.newtable
                forM_ (zip [1 ∷ Int ..] (idBuffs iDef)) $ \(i, b) → do
                    Lua.newtable
                    Lua.pushstring (TE.encodeUtf8 (ibStat b))
                    Lua.setfield (-2) "stat"
                    Lua.pushnumber (Lua.Number (realToFrac (ibAmount b)))
                    Lua.setfield (-2) "amount"
                    Lua.pushnumber (Lua.Number (realToFrac (ibPercent b)))
                    Lua.setfield (-2) "percent"
                    Lua.pushboolean (ibScalesWithCondition b)
                    Lua.setfield (-2) "scalesWithCondition"
                    Lua.rawseti (-2) (fromIntegral i)
                Lua.setfield (-2) "buffs"

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
