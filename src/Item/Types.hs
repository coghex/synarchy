{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
module Item.Types
    ( ItemDef(..)
    , ItemContainer(..)
    , ItemFood(..)
    , ItemWeapon(..)
    , ItemArmor(..)
    , ItemBuff(..)
    , ItemInstance(..)
    , itemMatches
    , itemContentsSig
    , itemTotalWeight
    , ItemManager(..)
    , emptyItemManager
    , lookupItemDef
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.List (sort)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Engine.Asset.Handle (TextureHandle(..))

-- | Container properties — items with a Just here can hold a fluid
--   (water, future: lava/beer/etc). Non-containers (a hammer, food)
--   have Nothing.
data ItemContainer = ItemContainer
    { icCapacity   ∷ !Float   -- ^ max volume: litres for fluids, COUNT for
                              --   discrete contents (e.g. 60 pills).
    , icHolds      ∷ !Text    -- ^ what it holds: "water" / "antiseptic" /
                              --   "antibiotics" / etc.
    , icFillWeight ∷ !Float   -- ^ kilograms per fill unit. 1.0 for fluids
                              --   (1 L = 1 kg, the default); tiny for
                              --   discrete solids (a 0.5 mg pill =
                              --   5.0e-7). Carried weight counts
                              --   iiCurrentFill × this, so a bottle sheds
                              --   mass as each unit is drawn out and the
                              --   empty case (iiWeight) is what remains.
    , icDefaultFill ∷ !Float  -- ^ fill a fresh instance spawns holding when
                              --   the creation site doesn't say otherwise
                              --   (loot rolls, bare item.spawnGround /
                              --   unit.addItem). 0 for refillable vessels
                              --   (canteens, bottles) — an explicit fill at
                              --   the spawn site still wins. A quinoa sack
                              --   sets this to its capacity so loot is
                              --   never an empty bag.
    } deriving (Show, Eq, Generic, Serialize)

-- | A single stat-modifier conferred by wearing/holding this item.
--   The combat system applies these as engine-side StatModifiers when
--   the item is equipped, and removes them on unequip. Source is the
--   item's display name so the player can trace the bonus.
data ItemBuff = ItemBuff
    { ibStat                ∷ !Text
      -- ^ stat name, e.g. "perception", "strength".
    , ibAmount              ∷ !Float
      -- ^ base bonus amount. Positive for buff, negative for debuff.
    , ibScalesWithCondition ∷ !Bool
      -- ^ when True, the applied bonus = amount × (condition/100). A
      --   100%-condition technogoggles confers the full +1; a 50%
      --   pair confers +0.5. When False the bonus is flat.
    } deriving (Show, Eq)

-- | Weapon-specific stats. Combined with the item's `idMaterial`
--   substance properties at combat-time. Material lives at the top
--   level on ItemDef now (armor needs it too); this block only
--   carries weapon-shape stats and per-attack-type effectiveness.
data ItemWeapon = ItemWeapon
    { iwBladeLength    ∷ !Float   -- ^ cm
    , iwBaseSharpness  ∷ !Float   -- ^ engineering scale: lower = sharper.
                                  --   Effective sharpness is modulated
                                  --   by per-instance condition.
    , iwStabEff        ∷ !Float   -- ^ 0..1; how well this weapon shape
                                  --   delivers stabbing attacks.
    , iwSlashEff       ∷ !Float   -- ^ 0..1; slashing effectiveness.
    , iwBluntEff       ∷ !Float   -- ^ 0..1; blunt effectiveness.
    , iwWeaponClass    ∷ !Text    -- ^ skill name the wielder uses
                                  --   ("dagger", "unarmed", "sword"…).
                                  --   Combat resolution reads
                                  --   uiSkills[iwWeaponClass] for the
                                  --   hit-roll skill contribution.
    , iwAttackCooldown ∷ !Float   -- ^ seconds between swings. Read by
                                  --   the AI's attack candidate to
                                  --   gate continuous attacks.
    , iwLength         ∷ !Float   -- ^ cm; TOTAL length (handle + blade),
                                  --   the lever arm of the swing. 0 ⇒
                                  --   fall back to iwBladeLength.
    , iwCenterOfMass   ∷ !Float   -- ^ 0..1 along the length from the
                                  --   grip. A head-heavy weapon (~0.8)
                                  --   swings with more inertia than a
                                  --   balanced one (0.5).
    } deriving (Show, Eq)

-- | Armour properties — worn protective gear. Combat prepends this as
--   an outer tissue layer over each body part it covers, and wears it
--   (condition ↓, may break) when struck. The protective material lives
--   in the item's top-level idMaterial (resolved against the
--   SubstanceManager), same as weapons.
data ItemArmor = ItemArmor
    { iaThickness ∷ !Float    -- ^ mm of material (the outer layer's depth)
    , iaCovers    ∷ ![Text]   -- ^ body-part ids this piece protects
    } deriving (Show, Eq)

-- | Food properties — items with a Just here restore hunger (stomach
--   kcal) when eaten. Two mutually exclusive shapes:
--
--   * DISCRETE food (rations): ifCalories > 0 — kcal per item consumed,
--     the whole item is removed on eat.
--   * BULK food (a quinoa sack): ifCaloriesPerKg > 0 — kcal per kg of the
--     item's FILL; eating draws just enough fill (kg) to top up the
--     eater's stomach and the item persists until its fill runs dry.
--
--   Credited kcal are clamped against the eater's max_hunger. Parsed from
--   a `nutrition:` sub-object so future macronutrient fields slot in
--   beside these without a schema/save change.
data ItemFood = ItemFood
    { ifCalories      ∷ !Float   -- ^ kcal per item consumed (0 = not
                                 --   discrete food)
    , ifCaloriesPerKg ∷ !Float   -- ^ kcal per kg of fill consumed (0 =
                                 --   not bulk food)
    } deriving (Show, Eq, Generic, Serialize)

-- | Immutable item definition — one per type loaded from YAML.
data ItemDef = ItemDef
    { idName        ∷ !Text             -- ^ unique key, e.g. "canteen_steel_2l"
    , idDisplayName ∷ !Text             -- ^ shown in UI
    , idTexture     ∷ !TextureHandle    -- ^ inventory sprite (UI use)
    , idWeight      ∷ !Float            -- ^ empty weight in kg (the
                                        --   mean, when a spec exists)
    , idWeightSpec  ∷ !(Maybe (Float, Float))
                                        -- ^ optional (mean, range) for
                                        --   per-instance weight rolls
                                        --   (truncated normal, like
                                        --   stats). Nothing = every
                                        --   instance weighs idWeight.
    , idKind        ∷ !Text             -- ^ equipment-slot kind ("weapon",
                                        --   "headwear", …). "misc" for
                                        --   non-equippable items. Matched
                                        --   against EquipmentSlot.esKind
                                        --   when equipping.
    , idCategory    ∷ !Text             -- ^ inventory-tab category
                                        --   ("Weapons", "Armor",
                                        --   "Supplies", "Misc"). Drives
                                        --   which dynamic tab the item
                                        --   appears under in the unit
                                        --   info inventory list. Default
                                        --   "Misc" — distinct from kind
                                        --   so a category can contain
                                        --   multiple kinds (Armor =
                                        --   helmet + gauntlets + …).
    , idMake        ∷ !Text             -- ^ design / crafting tradition
                                        --   the item descends from (e.g.
                                        --   "acolyte"). Empty for items
                                        --   without a known maker.
                                        --   Shown in the inventory
                                        --   tooltip's hint line.
    , idMaterial    ∷ !Text             -- ^ substance name (e.g. "steel",
                                        --   "leather"). Resolved against
                                        --   SubstanceManager when combat
                                        --   needs physical / resistance
                                        --   properties. Empty for items
                                        --   that don't have a single
                                        --   primary material (or that
                                        --   don't participate in combat).
    , idQualitySpec   ∷ !(Maybe (Float, Float))
      -- ^ (min, max) % range for quality rolls at spawn. Interpreted
      --   as a normal distribution centered at (min+max)/2 with
      --   stddev (max-min)/6, clamped to [min, max]. Nothing ⇒ spawn
      --   at 100%.
    , idConditionSpec ∷ !(Maybe (Float, Float))
      -- ^ (min, max) % range for condition rolls at spawn. Same
      --   distribution shape as quality. Condition degrades with use;
      --   quality is fixed for the item's lifetime.
    , idContainer   ∷ !(Maybe ItemContainer)
    , idDefaultContents ∷ ![(Text, Int, Maybe Float)]
      -- ^ For ITEM-containers (a first-aid kit, a toolbox): the contents a
      --   fresh instance spawns holding — (item def name, count, optional
      --   fill for fillable contents like a pill/fluid bottle). Each entry
      --   is materialised into `iiContents` (rolled like any item) at
      --   creation. Empty for everything that doesn't hold items.
    , idFood        ∷ !(Maybe ItemFood)
    , idWeapon      ∷ !(Maybe ItemWeapon)
    , idArmor       ∷ !(Maybe ItemArmor)
      -- ^ Weapon stats for items with kind="weapon". Nothing for
      --   everything else.
    , idUnequippable ∷ !Bool
      -- ^ When True, the unequip API refuses to remove this item once
      --   it's been put on. Used for ritual gear (acolyte's habit)
      --   that the player chose to commit to. Has no effect on items
      --   sitting in the inventory.
    , idBuffs       ∷ ![ItemBuff]
      -- ^ Stat modifiers conferred while the item is equipped.
      --   Applied via the existing uiModifiers system at equip time;
      --   removed on unequip. Empty list = no buffs.
    , idInsulation  ∷ !Float
      -- ^ Thermal insulation added while worn — slows the body's heat loss
      --   (scripts/thermo.lua sums it over equipped+accessory items via
      --   unit.getInsulation). 0 for non-clothing. Dress for the climate.
    } deriving (Show, Eq)

-- | Per-unit instance. References its def by name; currentFill is for
--   containers, quality is rolled once at spawn (immutable), condition
--   is rolled at spawn and degrades with use.
data ItemInstance = ItemInstance
    { iiDefName     ∷ !Text
    , iiCurrentFill ∷ !Float    -- ^ litres held; 0 for non-containers
    , iiQuality     ∷ !Float    -- ^ 0..100; how well-made this instance
                                --   is. Multiplicative on effective
                                --   sharpness / damage / armor value.
    , iiCondition   ∷ !Float    -- ^ 0..100; current wear. Degrades with
                                --   use. 0 = broken.
    , iiWeight      ∷ !Float    -- ^ THIS instance's empty weight (kg),
                                --   rolled at creation from the def's
                                --   weight spec when one is declared
                                --   (raw gems vary per find); equals
                                --   idWeight otherwise. Carried weight
                                --   = iiWeight + iiCurrentFill (1 L =
                                --   1 kg). Field order is load-bearing
                                --   (positional Generic Serialize) —
                                --   appended for save v36.
    , iiSharpness   ∷ !Float    -- ^ 0..100; edge keenness as a % of the
                                --   def's base_sharpness (100 = factory
                                --   edge). DISTINCT from iiCondition:
                                --   sharpness gates penetration and is
                                --   honed on a whetstone; condition is
                                --   structural fractures, gates breakage,
                                --   and is restored at a furnace. Both
                                --   drop with use. Appended for save v(+1).
    , iiContents    ∷ ![ItemInstance]
                                -- ^ For ITEM-containers (first-aid kit /
                                --   toolbox): the items it holds. Units draw
                                --   tools + supplies from here and return
                                --   reusable tools. Empty for ordinary
                                --   items. Recursive (a kit could hold a
                                --   kit); serialised via the same instance.
                                --   Appended for save v42.
    , iiInstanceId  ∷ !Word64
                                -- ^ Process-unique identity for THIS physical
                                --   item, stamped from a monotonic counter at
                                --   genuine creation (rolls, spawns) and
                                --   PRESERVED verbatim through every move
                                --   (equip / store / withdraw / transfer /
                                --   drop). Lets the UI target the exact
                                --   instance the player clicked instead of the
                                --   first inventory entry matching a defName,
                                --   so same-def items with different fill /
                                --   sharpness never act on the wrong one
                                --   (#67). 0 = unassigned (never minted; only a
                                --   legacy/default sentinel). The counter is
                                --   persisted as sdNextItemInstanceId so ids
                                --   stay unique across save/load. Field order
                                --   is load-bearing (positional Generic
                                --   Serialize) — appended for save v56.
    } deriving (Show, Eq, Generic, Serialize)

-- | A stable, order-independent signature of an item's nested contents
--   (#67A). Two ITEM-containers (a first-aid kit, a toolbox) are
--   interchangeable only if they hold the same things in the same state,
--   so the inventory / cargo UIs fold this into the row key: kits whose
--   contents have diverged (one drew a bandage) stop merging and become
--   individually inspectable / withdrawable rather than collapsing onto a
--   single representative instance. Empty for ordinary items (no nested
--   contents), so non-containers and fluid containers stack exactly as
--   before. Recurses so a kit-in-a-kit is captured too.
itemContentsSig ∷ ItemInstance → Text
itemContentsSig inst
    | null (iiContents inst) = T.empty
    | otherwise = T.intercalate ";" $ sort
        [ T.intercalate ":"
            [ iiDefName c
            , T.pack (show (iiCurrentFill c))
            , T.pack (show (iiCondition c))
            , T.pack (show (iiSharpness c))
            , itemContentsSig c ]
        | c ← iiContents inst ]

-- | Target predicate for an inventory action (#67). When the caller
--   supplies a unique instance id (>0) it wins — the action hits exactly
--   that physical item, so two same-def instances with different fill /
--   sharpness never get confused. Id 0 means "no id given" (legacy / AI
--   callers) and falls back to the historical first-match-by-defName.
itemMatches ∷ Word64 → Text → ItemInstance → Bool
itemMatches iid name it
    | iid > 0   = iiInstanceId it ≡ iid
    | otherwise = iiDefName it ≡ name

-- | Total carried mass of an item (kg), INCLUDING its container
--   contents, computed recursively. Empty weight (iiWeight) + the mass
--   of its fill (iiCurrentFill × the container's per-unit fill weight)
--   + the full weight of everything nested inside it. So a stocked
--   first-aid kit weighs its bandages, bottles, and tools — not just
--   its empty case — and a pill bottle sheds mass as pills are drawn.
--
--   Fill weight is looked up per item from its container def
--   (icFillWeight): 1 kg/L for fluids, ~5e-7 kg for a 0.5 mg pill. A
--   non-container (fill 0) contributes nothing from fill regardless.
itemTotalWeight ∷ ItemManager → ItemInstance → Float
itemTotalWeight im it =
    iiWeight it
      + iiCurrentFill it * fillUnitWeight
      + sum (map (itemTotalWeight im) (iiContents it))
  where
    fillUnitWeight = case idContainer =<< lookupItemDef (iiDefName it) im of
        Just c  → icFillWeight c
        Nothing → 1.0   -- no container def in scope → assume litres (1 kg/L)

-- | Engine-wide registry of all loaded item defs.
newtype ItemManager = ItemManager
    { imDefs ∷ HM.HashMap Text ItemDef
    } deriving (Show, Eq)

emptyItemManager ∷ ItemManager
emptyItemManager = ItemManager HM.empty

lookupItemDef ∷ Text → ItemManager → Maybe ItemDef
lookupItemDef name (ItemManager m) = HM.lookup name m
