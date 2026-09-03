{-# LANGUAGE Strict #-}
-- | Item definition catalogue: loading item YAML (registering sprites
--   and defs into the ItemManager) and listing the loaded defs.
--   Split from Engine.Scripting.Lua.API.Items (#577) — ground-item
--   world state lives in Items.Ground, selection/render-introspection
--   in Items.Render.
--
--   Narrowed to the @content-registries@ capability (#890, epic #537):
--   the item catalogue is reached only through
--   'ContentRegistriesCapability' and the logger only through
--   'CoreCapability'. 'loadItemYamlFn' still takes an 'EngineEnv', but
--   purely as the opaque token the not-yet-narrowed @render-gpu-asset@
--   texture helpers ('resolveTexturePath', 'loadAndRegisterWithPool',
--   'isTextureNameRegistered') demand — this module dereferences no
--   'EngineEnv' field itself, and that parameter goes away when
--   @render-gpu-asset@ migrates (SS7.2).
module Engine.Scripting.Lua.API.Items.Defs
    ( loadItemYamlFn
    , registerItemDefs
    , itemDuplicateMessage
    , itemListDefsFn
      -- * The pure YAML → registry mapping
      --
      -- Exported for the tests that materialize SHIPPED definitions
      -- (#1418): building an 'ItemDef' by hand there would prove the
      -- fixture, not the data.
    , itemDefFromYaml
    ) where

import UPrelude
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Control.Monad (foldM)
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv)
import Engine.Core.Capability.Core (CoreCapability)
import Engine.Core.Capability.ContentRegistries
    (ContentRegistriesCapability(..))
import Engine.Core.Log (LogCategory(..), LoggerState, logDebug, logWarn)
import Engine.Core.Log.Monad (getLoggerFor)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaToEngineMsg)
import Engine.Graphics.Vulkan.Texture.Policy (UploadSampler(..))
import Engine.Scripting.Lua.API.YamlResult (pushYamlResult)
import Engine.Scripting.Lua.API.YamlTextures (loadAndRegisterWithPool,
                                              isTextureNameRegistered,
                                              resolveTexturePath)
import Engine.Asset.Handle (TextureHandle)
import Engine.Asset.Types (AssetPool)
import qualified Engine.Core.Queue as Q
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

-- | The same badge under the UI upload policy (#2075), for the overlay
--   @scripts\/ui\/broken_overlay.lua@ draws over an inventory icon. A
--   separate NAME because a texture-name registry entry resolves to one
--   handle, and the two policies are two handles on two slots.
brokenEquipmentUiTexName ∷ Text
brokenEquipmentUiTexName = "broken_equipment_ui"

resolveSpritePath ∷ EngineEnv → FilePath → IO FilePath
resolveSpritePath env = resolveTexturePath env "Item sprite" missingEquipmentTexture

-- | item.loadYaml(path) — parses a YAML file of item defs, loads each
--   item's sprite, and registers the defs into the ItemManager.
--   Returns the number of defs loaded. Callable repeatedly; each call
--   inserts/replaces by def name (#1232 requirement 8: a repeat load of
--   a valid file stays a legal no-op-shaped reload, never a
--   duplicate-id failure).
--
--   Argument marshalling only: every effect lives in 'registerItemDefs',
--   so a test can drive the identical registration path without a Lua
--   state.
loadItemYamlFn ∷ CoreCapability → ContentRegistriesCapability → EngineEnv
               → LuaBackendState → Lua.LuaE Lua.Exception Lua.NumResults
loadItemYamlFn core regs env backendState = do
    pathArg ← Lua.tostring 1
    case pathArg of
        Nothing → pushYamlResult False 0
        Just pathBS → do
            let filePath = T.unpack (TE.decodeUtf8Lenient pathBS)
                (lteq, _) = lbsMsgQueues backendState
            (parsed, count) ← Lua.liftIO $ do
                logger ← getLoggerFor core
                mDefs ← loadItemYamlOutcome logger filePath
                let defs = fromMaybe [] mDefs
                total ← registerItemDefs env logger
                            (lbsAssetPool backendState) lteq
                            (crItemManagerRef regs) filePath defs
                logDebug logger CatAsset $
                    "loadItemYaml: loaded " <> tshow total
                    <> " item definitions from " <> T.pack filePath
                return (isJust mDefs, total)

            pushYamlResult parsed count

-- | #1232 requirement 7's collision diagnostic, in one place so both
--   the emitter and its gate spell it the same way.
--
--   Informational by contract: it neither fails the load, reduces the
--   count 'loadItemYamlFn' returns, nor prevents the replacement. The
--   two paths are legitimately EQUAL twice over — an intra-file
--   duplicate, and a repeated load of the same valid file — so the
--   message reads correctly when they are.
itemDuplicateMessage ∷ Text → Text → Text → Text
itemDuplicateMessage itemId replacedPath replacingPath =
    "duplicate item definition id \"" <> itemId <> "\": the definition \
    \from " <> replacedPath <> " is replaced by the one from "
    <> replacingPath <> " (last write wins)"

-- | Register every item definition a parsed YAML file declared,
--   returning how many were published.
--
--   Registration is last-write-wins in the caller's order (#1232
--   requirement 6): within this list, the later authored entry wins;
--   across files, whichever file the caller loads later wins. Every
--   replacement emits 'itemDuplicateMessage'.
registerItemDefs
    ∷ EngineEnv                    -- ^ opaque token for the not-yet-narrowed
                                   --   @render-gpu-asset@ texture helpers
    → LoggerState
    → IORef AssetPool
    → Q.Queue LuaToEngineMsg
    → IORef ItemManager
    → FilePath                     -- ^ the YAML's own path (provenance)
    → [ItemYamlDef]
    → IO Int
registerItemDefs env logger poolRef lteq managerRef filePath defs = do
    -- Register the broken-weapon overlay once (same flow as item
    -- sprites). The ground-item renderer fetches it by name from the
    -- texture-name registry.
    alreadyRegistered ← isTextureNameRegistered env brokenEquipmentTexName
    unless alreadyRegistered $ do
        void $ loadAndRegisterWithPool env poolRef lteq UploadGlobalSampler
                   brokenEquipmentTexName brokenEquipmentTexture
        -- Dual-use (#2075): the ground-item renderer draws the badge in
        -- the world, and scripts/ui/broken_overlay.lua draws it over an
        -- inventory icon. Two slots, one per policy, each reachable by
        -- its own registered name.
        void $ loadAndRegisterWithPool env poolRef lteq UploadPinnedNearest
                   brokenEquipmentUiTexName brokenEquipmentTexture

    foldM (\acc def → do
        -- Load the sprite texture so it's ready for any future
        -- inventory grid UI. Register under "item_<defName>" so other
        -- systems can fetch it.
        let regName = "item_" <> iydName def
        spritePath ← resolveSpritePath env (T.unpack (iydSprite def))
        -- An item sprite is genuinely dual-use (#2075): the ground-item
        -- renderer draws it in the world at world scale, and the
        -- inventory / equipment / container panels draw it as a UI
        -- icon. So it is loaded under BOTH policies -- two slots for one
        -- file, D-4's accepted cost -- and the def carries both handles
        -- so each consumer reads the one with its own sampler.
        handle ← loadAndRegisterWithPool env poolRef lteq UploadGlobalSampler
                     regName spritePath
        iconHandle ← loadAndRegisterWithPool env poolRef lteq UploadPinnedNearest
                         (regName <> "_ui") spritePath

        let itemDef = itemDefFromYaml filePath handle iconHandle def
        replaced ← atomicModifyIORef' managerRef $ \im →
            ( ItemManager { imDefs = HM.insert (idName itemDef) itemDef
                                               (imDefs im) }
            , HM.lookup (idName itemDef) (imDefs im) )
        forM_ replaced $ \old →
            logWarn logger CatAsset $
                itemDuplicateMessage (idName itemDef) (idSourcePath old)
                                     (idSourcePath itemDef)

        return (acc + 1)
        ) (0 ∷ Int) defs

-- | One authored default-content entry, YAML → registry (#1418).
--   Recursive: a nested @contents:@ list carries the same shape all the
--   way down, and the omitted / empty / replaced distinction the decoder
--   preserves survives verbatim into 'ItemContentEntry'.
contentEntry ∷ ItemYamlContent → ItemContentEntry
contentEntry c = ItemContentEntry
    { iceItem     = iycoItem c
    , iceCount    = iycoCount c
    , iceFill     = iycoFill c
    , iceContents = map contentEntry <$> iycoContents c
    }

-- | The authored YAML definition, as the registry holds it. Pure: the
--   only things it cannot derive are the already-loaded sprite handle
--   and the file the definition came from, both passed in.
-- | Build an 'ItemDef' from its YAML, given the sprite's two handles:
--   the SCENE one the ground renderer draws and the UI one the panels
--   draw (#2075). A caller with no GPU may pass the same handle twice.
itemDefFromYaml ∷ FilePath → TextureHandle → TextureHandle → ItemYamlDef
                → ItemDef
itemDefFromYaml filePath handle iconHandle def = ItemDef
    { idName        = iydName def
    , idDisplayName = if T.null (iydDisplayName def)
                      then iydName def
                      else iydDisplayName def
    , idTexture     = handle
    , idIconTexture = iconHandle
    , idWeight      = wMean
    , idWeightSpec  = wSpec
    , idBulk        = iydBulk def
    , idKind        = iydKind def
    , idCategory    = iydCategory def
    , idMake        = iydMake def
    , idMaterial    = iydMaterial def
    , idQualitySpec   = (\r → (iyrsMin r, iyrsMax r)) <$> iydQuality def
    , idQualityTiers = map (\t → QualityTier (iyqtMin t) (iyqtLabel t))
                           (iydQualityTiers def)
    , idContainer   = container
    , idDefaultContents = map contentEntry (iydContents def)
    , idStorage     = storage
    , idFood        = food
    , idWeapon      = weapon
    , idArmor       = armor
    , idUnequippable = iydUnequippable def
    , idBuffs       = map
        (\b → ItemBuff
            { ibStat = iybStat b
            , ibAmount = iybAmount b
            , ibPercent = iybPercent b
            , ibScalesWithCondition = iybScalesWithCondition b
            })
        (iydBuffs def)
    , idInsulation  = iydInsulation def
    , idSourcePath  = T.pack filePath
    }
  where
    container = fmap
        (\c → ItemContainer
            { icCapacity    = iycCapacity c
            , icHolds       = iycHolds c
            , icFillWeight  = iycFillWeight c
            , icDefaultFill = iycDefaultFill c
            })
        (iydContainer def)
    -- #1233: the optional portable ITEM-storage capacities, carried
    -- across independently of `container` above — neither block
    -- supplies the other's values (D-12).
    storage = fmap
        (\s → ItemStorage
            { isWeightCapacity = iysWeightCapacity s
            , isBulkCapacity   = iysBulkCapacity s
            })
        (iydStorage def)
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

-- | item.listDefs() → array of {name, displayName, category, weight}
--   Sorted by name for a stable debug-overlay listing.
itemListDefsFn ∷ ContentRegistriesCapability
               → Lua.LuaE Lua.Exception Lua.NumResults
itemListDefsFn regs = do
    im ← Lua.liftIO $ readIORef (crItemManagerRef regs)
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
