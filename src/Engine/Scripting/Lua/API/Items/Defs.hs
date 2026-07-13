{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Item definition catalogue: loading item YAML (registering sprites
--   and defs into the ItemManager) and listing the loaded defs.
--   Split from Engine.Scripting.Lua.API.Items (#577) — ground-item
--   world state lives in Items.Ground, selection/render-introspection
--   in Items.Render.
module Engine.Scripting.Lua.API.Items.Defs
    ( loadItemYamlFn
    , itemListDefsFn
    ) where

import UPrelude
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Log (LogCategory(..), logInfo)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegister, resolveTexturePath)
import Engine.Asset.YamlTextures (lookupTextureName)
import Engine.Asset.YamlItems
import Item.Types

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
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
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
