{-# LANGUAGE Strict #-}
-- | Reconciling a SAVED equipment map against the equipment class the
--   unit's definition declares TODAY (#2307).
--
--   'Unit.Types.Instance.uiEquipment' is keyed by slot id, and both
--   axes of that key are content that can be edited between the save
--   and the load: the class may retire the slot entirely, or keep it
--   and change the item kind it accepts. Equipping at runtime checks
--   both ('Engine.Scripting.Lua.API.Equipment.Slot'); until this,
--   LOADING checked neither, because the save gate walks the map's
--   VALUES ('World.Save.Types.unitItemContainers') and never its keys.
--
--   A drifted key is invisible and mechanically live at once. The
--   inventory UI enumerates the CURRENT class's slots and looks up only
--   those ids (@scripts/unit_info_v2_inventory_data.lua@), so an entry
--   under a retired id never renders — while
--   'Combat.Resolution.Damage.defenderArmor',
--   @unit.getInsulation@ and @unit.getCarryingWeight@ all read the
--   whole map and keep honouring it. And @equipment.unequip@ pops only
--   the slot its caller names, whose ids likewise come from the current
--   class, so nothing the player can reach takes the item back out.
--
--   This is the same shape as #1087's container-knowledge scrub and
--   #2305's acquired-immunity scrub, and takes the same footing: a
--   tolerated, non-blocking DIAGNOSTIC applied while the session is
--   still being staged, never a load failure. It differs from both in
--   one way that matters — the orphan is a real 'ItemInstance', so it
--   is MOVED to the unit's loose inventory rather than dropped. The
--   exact instance survives (id, quality, condition, sharpness, fill
--   and nested contents alike), and because
--   'Engine.Scripting.Lua.API.Units.Cargo' weighs loose inventory and
--   equipped slots identically, the unit's carried mass is unchanged by
--   the move and no capacity gate can newly trip.
module Equipment.Reconcile
    ( EquipmentOrphanCause(..)
    , EquipmentOrphan(..)
    , reconcileUnitEquipment
    , renderEquipmentOrphan
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Equipment.Types
    (EquipmentClass(..), EquipmentClassManager, EquipmentSlot(..)
    , lookupEquipmentClass)
import Item.Types (ItemDef(..), ItemInstance(..), ItemManager, lookupItemDef)
import Unit.Types.Manager (UnitId(..))
import World.Page.Types (WorldPageId(..))

-- | Why one saved equipment entry could not be restored into its own
--   slot. The two cases are exactly the two 'equipment.equip' refuses
--   at runtime, checked here against the same content.
data EquipmentOrphanCause
    = EquipmentSlotRetired
      -- ^ The unit's current equipment class declares no such slot id —
      --   the class dropped it, was renamed away, no longer resolves in
      --   the 'EquipmentClassManager', or the unit's definition declares
      --   no equipment class at all.
    | EquipmentKindMismatch !Text !Text
      -- ^ The slot survives but no longer accepts this item: the item's
      --   current @idKind@, then the slot's declared @esKind@.
    deriving (Show, Eq)

-- | One migrated entry, carrying everything the diagnostic names:
--   which unit, which slot the item came out of, which item it is
--   (definition name AND physical instance id, so two identical items
--   are still distinguishable), and which of the two causes applied.
data EquipmentOrphan = EquipmentOrphan
    { eqoUnit    ∷ !UnitId
    , eqoSlot    ∷ !Text
    , eqoItemDef ∷ !Text
    , eqoItemId  ∷ !Word64
    , eqoCause   ∷ !EquipmentOrphanCause
    } deriving (Show, Eq)

-- | Split one saved equipment map into the entries that still belong in
--   their own slots and the ones that do not, appending every orphaned
--   instance to the unit's loose inventory.
--
--   @mClassName@ is the unit definition's own 'Unit.Types.Def.udEquipmentClass'.
--   'Nothing', or a name that no longer resolves in the manager,
--   invalidates EVERY entry: such a unit exposes no slots in the
--   inventory UI and @equipment.equip@ refuses it outright, so anything
--   still keyed there is exactly the hidden-but-live state this
--   reconciliation exists to end.
--
--   Ordering is deterministic and preserves what the save stored. The
--   existing loose inventory keeps its order and its position, and the
--   migrated entries are appended in SLOT-ID order — the order
--   'Engine.Scripting.Lua.API.Units.Inventory.unitHeldItems' already
--   presents equipped slots in — so a direct 'HM.HashMap' traversal
--   never decides what the next save writes.
--
--   An entry whose item DEFINITION does not resolve is deliberately
--   left where it is rather than treated as a kind mismatch. That
--   reference is a hard load rejection
--   ('World.Save.Types.missingItemDefReferences', run by
--   'Engine.Scripting.Lua.API.Save.continueLoad' before staging is ever
--   queued), and silently migrating it here would be this non-blocking
--   repair quietly absorbing a case that must stay fatal. A retired
--   SLOT is decided without the definition, so such an entry still
--   moves on that axis.
reconcileUnitEquipment
    ∷ EquipmentClassManager
    → ItemManager
    → Maybe Text
      -- ^ the unit definition's declared equipment-class name
    → UnitId
    → HM.HashMap Text ItemInstance
      -- ^ the saved equipment map
    → [ItemInstance]
      -- ^ the saved loose inventory
    → (HM.HashMap Text ItemInstance, [ItemInstance], [EquipmentOrphan])
reconcileUnitEquipment ecm im mClassName uid equipped inventory =
    ( HM.fromList [ (slotId, it) | (slotId, it, Nothing) ← classified ]
    , inventory ⧺ [ it | (_, it, Just _) ← classified ]
    , [ EquipmentOrphan uid slotId (iiDefName it) (iiInstanceId it) cause
      | (slotId, it, Just cause) ← classified ] )
  where
    -- Slot id → the kind that slot accepts today. Empty when the unit
    -- declares no class or its class is gone, which is what makes every
    -- entry a retired-slot orphan.
    slotKinds = case mClassName ⌦ (`lookupEquipmentClass` ecm) of
        Nothing  → HM.empty
        Just cls → HM.fromList [ (esId s, esKind s) | s ← ecSlots cls ]
    classified =
        [ (slotId, it, verdict slotId it)
        | (slotId, it) ← L.sortOn fst (HM.toList equipped) ]
    verdict slotId it = case HM.lookup slotId slotKinds of
        Nothing       → Just EquipmentSlotRetired
        Just slotKind → case lookupItemDef (iiDefName it) im of
            Nothing → Nothing
            Just iDef
                | idKind iDef ≢ slotKind →
                    Just (EquipmentKindMismatch (idKind iDef) slotKind)
                | otherwise → Nothing

-- | One migrated entry, as the load says it. Names the unit, the page,
--   the affected slot, the item's definition and physical instance id,
--   and which of the two causes applied — enough to identify the exact
--   content edit that stranded it.
renderEquipmentOrphan ∷ WorldPageId → EquipmentOrphan → Text
renderEquipmentOrphan pid o =
    "unit #" <> tshow (unUnitId (eqoUnit o)) <> " on page '"
      <> unWorldPageId pid <> "': equipped item '" <> eqoItemDef o
      <> "' (instance " <> tshow (eqoItemId o) <> ") in slot '"
      <> eqoSlot o <> "' " <> reason
      <> "; moved to the unit's inventory"
  where
    reason = case eqoCause o of
        EquipmentSlotRetired →
            "is in a slot the unit's equipment class no longer declares"
        EquipmentKindMismatch itemKind slotKind →
            "is kind '" <> itemKind <> "' but that slot now accepts '"
              <> slotKind <> "'"
