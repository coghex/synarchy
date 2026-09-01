{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
module Item.Types
    ( ItemDef(..)
    , ItemContentEntry(..)
    , ItemContainer(..)
    , ItemStorage(..)
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
    , QualityTier(..)
    , qualityTierLabel
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.List (sort, sortBy, find)
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

-- | One authored default-content entry of an ITEM-container (#1418):
--   which item, how many, an optional fill, and — recursively — what
--   THAT child spawns holding.
--
--   'iceContents' is the whole reason this is a record rather than the
--   flat tuple it replaced, and its three states are DISTINCT:
--
--   * @Nothing@ (the key omitted, or authored as an explicit @null@ —
--     aeson reads both the same way) delegates to the referenced child
--     definition's own 'idDefaultContents'. A kit inside a crate still
--     arrives stocked.
--   * @Just []@ deliberately materialises that child EMPTY, overriding
--     the child definition's defaults. An empty kit is a thing a
--     designer can author.
--   * @Just entries@ REPLACES the child definition's defaults with
--     exactly these, in authored order.
--
--   Every positive 'iceCount' occurrence gets its own independently
--   materialised subtree — two kits in a crate are two distinct trees of
--   distinct instances, never one tree shared twice.
data ItemContentEntry = ItemContentEntry
    { iceItem     ∷ !Text          -- ^ referenced item definition name
    , iceCount    ∷ !Int           -- ^ how many; ≤ 0 materialises none
    , iceFill     ∷ !(Maybe Float) -- ^ explicit fill for a fillable child
                                   --   (a pill bottle's count, a fluid
                                   --   bottle's litres); Nothing takes the
                                   --   child definition's own default_fill
    , iceContents ∷ !(Maybe [ItemContentEntry])
                                   -- ^ see above: omitted / empty / replaced
    } deriving (Show, Eq)

-- | Portable ITEM-storage capacity (#1233, epic #1231) — the optional
--   @storage:@ component. Deliberately SEPARATE from 'ItemContainer'
--   above, which means something physically different (a homogeneous
--   fluid/pill FILL, tracked by 'iiCurrentFill'): a crate stores exact
--   nested 'ItemInstance' values, a canteen holds litres of water, and
--   an item may eventually carry both without either capacity
--   inheriting the other's defaults or validation
--   (@docs\/portable_loot_containers.md@ D-12).
--
--   Both capacities are INTERNAL — they bound the CONTENTS, never the
--   empty container itself — and both are independently authored: not
--   derived from each other, not derived from the item's own external
--   'idBulk', and with no default (see
--   "Engine.Asset.YamlItems"'s @storage:@ parser, which rejects a
--   missing / non-positive / non-finite capacity by definition name).
--
--   NOTHING enforces either capacity yet (#1233 is data only; PLC-4,
--   the epic's former PLC-3B, owns capacity-safe ownership moves), so a
--   value here is authored, materialized and persisted, but never
--   consulted on insert, transfer, pickup or drop.
data ItemStorage = ItemStorage
    { isWeightCapacity ∷ !Float  -- ^ kilograms of CONTENTS this item can
                                 --   structurally support. Independent of
                                 --   the item's own empty weight.
    , isBulkCapacity   ∷ !Float  -- ^ litres of usable INTERNAL packing
                                 --   space. Direct children consume their
                                 --   own external 'idBulk' against this;
                                 --   distinct from — and never derived
                                 --   from — the item's own external bulk.
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
    , ibPercent             ∷ !Float
      -- ^ percentage bonus, fractional like the unit-level modifiers
      --   block (0.1 = +10%). Lands on the modifier's smPercent axis:
      --   effectiveStat = (base + Σdeltas) × (1 + Σpercents). 0 for
      --   purely additive buffs (#392).
    , ibScalesWithCondition ∷ !Bool
      -- ^ when True, the applied bonus = amount × (condition/100). A
      --   100%-condition technogoggles confers the full +1; a 50%
      --   pair confers +0.5. When False the bonus is flat. The same
      --   factor scales the percent component (a worn 50%-condition
      --   "+10%" buff confers +5%).
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
--
--   Since #1716 the exclusion above is ENFORCED, at the authoring
--   boundary and nowhere else: 'Engine.Asset.YamlItems.parseItemYamlFood'
--   rejects a definition unless exactly one of these two is strictly
--   positive and the other exactly zero, and rejects bulk nutrition on a
--   definition with no `container:` block (fill is 0 for a non-container,
--   so it could never be eaten). Every consumer may therefore rely on
--   the exclusion; none of them clamps defensively.
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
    , idTexture     ∷ !TextureHandle
      -- ^ The sprite as SCENE art: what 'World.Render.GroundItemQuads'
      --   draws for a dropped item, following the player's filter.
    , idIconTexture ∷ !TextureHandle
      -- ^ The same sprite uploaded under the UI policy (#2075), for the
      --   inventory / equipment / container panels. A second handle on a
      --   second slot, because a slot's sampler is fixed by the policy
      --   that uploaded it and this art is drawn in both places.
    , idWeight      ∷ !Float            -- ^ empty weight in kg (the
                                        --   mean, when a spec exists)
    , idWeightSpec  ∷ !(Maybe (Float, Float))
                                        -- ^ optional (mean, range) for
                                        --   per-instance weight rolls
                                        --   (truncated normal, like
                                        --   stats). Nothing = every
                                        --   instance weighs idWeight.
    , idBulk        ∷ !Float            -- ^ EXTERNAL bulk in litres
                                        --   (#1233): how much practical
                                        --   packing space one of these
                                        --   consumes inside a container.
                                        --   An abstract authored scalar,
                                        --   NOT geometric volume and not
                                        --   a density input — it folds in
                                        --   casing, awkward dimensions
                                        --   and ordinary packing slack,
                                        --   so a compact 15 kg battery
                                        --   authors less than a lighter,
                                        --   broad steel plate. Always
                                        --   explicitly authored, finite
                                        --   and strictly positive; there
                                        --   is NO default and it is never
                                        --   inferred from weight or
                                        --   material (the YAML loader
                                        --   rejects a definition lacking
                                        --   it).
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
    , idQualityTiers  ∷ ![QualityTier]
      -- ^ Data-driven quality→label thresholds (`quality_tiers:` in
      --   YAML), e.g. 90→"excellent" so a 95%-quality coffee reads
      --   "coffee (excellent)". Empty ⇒ fall back to
      --   'defaultQualityTiers'.
      --
      --   A NON-EMPTY table replaces 'defaultQualityTiers' wholesale —
      --   'qualityTierLabel' never mixes the two — so it has to be
      --   self-sufficient, and since #1739
      --   'Engine.Asset.YamlItems.parseItemYamlQualityTiers' refuses to
      --   load one that is not. Every non-empty value reaching this
      --   field therefore satisfies all four invariants:
      --
      --     * exactly one band has @qtMin ≡ 0@, so the table has its own
      --       floor and no quality in 0..100 falls through it;
      --     * every 'qtMin' is finite and within 0..100 inclusive;
      --     * no two bands share a 'qtMin', so the highest-band-wins
      --       rule below is never decided by author order;
      --     * every 'qtLabel' is non-blank, so no accepted band renders
      --       as an absent tier.
      --
      --   The empty case is unconstrained, because it selects
      --   'defaultQualityTiers', which satisfies the same four.
    , idContainer   ∷ !(Maybe ItemContainer)
    , idDefaultContents ∷ ![ItemContentEntry]
      -- ^ For ITEM-containers (a first-aid kit, a toolbox): the contents a
      --   fresh instance spawns holding. Every entry is materialised into
      --   `iiContents` by "Item.Materialize" — the ONE mint boundary
      --   (#1418) — at creation, so EVERY creation path spawns a kit
      --   stocked, not just unit spawning. Empty for everything that
      --   doesn't hold items. Authored recursively: see 'ItemContentEntry'.
    , idStorage     ∷ !(Maybe ItemStorage)
      -- ^ Optional portable ITEM-storage capacity (#1233): the internal
      --   weight + bulk limits this item offers its contents. Nothing for
      --   everything that isn't portable storage. Completely independent
      --   of 'idContainer' above — neither implies, defaults, or
      --   validates the other (D-12).
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
    , idSourcePath  ∷ !Text
      -- ^ The YAML file this definition was registered FROM (#1232) —
      --   provenance only, never identity: 'idName' is the id, and moving
      --   a definition anywhere within @data/items/@ changes this and
      --   nothing else. Exists so a duplicate-id replacement can name
      --   BOTH sides, which needs the loser's path and not just the
      --   winner's. Not persisted: 'ItemManager' is a live registry
      --   rebuilt from YAML every boot, and 'ItemDef' rides no wire DTO.
    } deriving (Show, Eq)

-- | Per-unit instance. References its def by name; currentFill is for
--   containers, quality is rolled once at spawn (immutable), condition
--   is runtime wear state that degrades with use. Condition is NOT
--   authored per definition (#1421): every freshly made item starts at
--   100, and the one exception is the salvage path @item.spawnGround@,
--   which starts an item below full because it was already lying in the
--   world.
data ItemInstance = ItemInstance
    { iiDefName     ∷ !Text
    , iiCurrentFill ∷ !Float    -- ^ litres held; 0 for non-containers
    , iiQuality     ∷ !Float    -- ^ 0..100; how well-made this instance
                                --   is. Multiplicative on effective
                                --   sharpness / damage / armor value.
    , iiCondition   ∷ !Float    -- ^ 0..100; current wear. Starts at 100
                                --   for every fresh item (ground salvage
                                --   excepted, #1421) and degrades with
                                --   use. 0 = broken.
    , iiWeight      ∷ !Float    -- ^ THIS instance's own EMPTY weight (kg)
                                --   and nothing more — it counts neither
                                --   the fill it holds nor anything nested
                                --   inside it. Rolled at creation from the
                                --   def's weight spec when one is declared
                                --   (raw gems vary per find); equals
                                --   idWeight otherwise. For carried or
                                --   contained mass ask 'itemTotalWeight',
                                --   which is the sole authority — a second
                                --   copy of its formula here is exactly
                                --   what drifted. Field order is
                                --   load-bearing (positional Generic
                                --   Serialize) — appended for save v36.
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
    , iiTemp        ∷ !(Maybe Float)
                                -- ^ Temperature (°C) when it differs from the
                                --   surroundings. Nothing = at ambient — the
                                --   effective temperature is the tile's
                                --   ambient (World.Weather.Ambient), which is
                                --   what almost every item sits at, so the
                                --   cooling tick skips it entirely. Just t =
                                --   tracked: the per-page tick relaxes t
                                --   toward the tile's ambient (Newtonian,
                                --   rate ∝ ΔT / thermal mass — see
                                --   Item.Temperature) and drops back to
                                --   Nothing on arrival. Set hot by cooking /
                                --   smelting outputs (via the temp Lua
                                --   setters). Field order is load-bearing
                                --   (positional Generic Serialize) — appended
                                --   for save v68 (#344).
    , iiBulk        ∷ !(Maybe Float)
                                -- ^ THIS instance's own EXTERNAL bulk
                                --   (litres), snapshotted from 'idBulk' at
                                --   creation — the same materialize-once
                                --   discipline 'iiWeight' has, so editing a
                                --   definition's bulk never retroactively
                                --   changes what an already-created item is
                                --   worth (#1233 requirement 6).
                                --
                                --   @Nothing@ means exactly one thing: this
                                --   instance was materialized BEFORE bulk
                                --   existed, so it genuinely has no
                                --   authored value to recover. Historical
                                --   saves decode that way deliberately —
                                --   never as a fabricated 0 (which would be
                                --   an invalid bulk masquerading as data)
                                --   and never re-derived from the current
                                --   definition, which is precisely the
                                --   retroactive reinterpretation
                                --   requirement 6 forbids. Absence is
                                --   REPRESENTED rather than papered over,
                                --   so no reader can silently fall back to
                                --   a definition that has since been
                                --   edited; PLC-4 (the epic's former
                                --   PLC-3B), the first slice that
                                --   ENFORCES a capacity, decides what an
                                --   absent bulk means to it. Field order is
                                --   load-bearing (positional Generic
                                --   Serialize) — appended for #1233.
    , iiStorage     ∷ !(Maybe ItemStorage)
                                -- ^ THIS instance's own INTERNAL storage
                                --   capacities, snapshotted from 'idStorage'
                                --   at creation. @Nothing@ for an item that
                                --   is not portable storage AND for a
                                --   pre-#1233 instance — the same honest
                                --   absence 'iiBulk' documents (a crate in
                                --   an old save was never stamped, so its
                                --   capacity is unrecoverable rather than
                                --   zero). Field order is load-bearing
                                --   (positional Generic Serialize) —
                                --   appended for #1233.
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
--
--   Each child is REPRESENTED by exactly these fields: definition name,
--   current fill, quality, condition, realized weight, sharpness, and
--   its own recursive contents signature. That is the same identity set
--   @scripts/ui/item_list.lua@'s @stackKey@ uses one level UP, which is
--   the point (#1597): a bandage's quality and realized weight are
--   identity while it sits in a unit's inventory, so they must stay
--   identity when the same bandage sits inside a kit. Two kits whose
--   bandages differ only in quality (or only in rolled weight) are NOT
--   interchangeable and must not collapse onto one representative.
--
--   Two fields are deliberately EXCLUDED, and both exclusions are
--   load-bearing:
--
--   * 'iiInstanceId' — physical identity, never represented state. Two
--     distinct instances holding identical children ARE stack-compatible;
--     keying on the id would split every container from every other one
--     and defeat grouping entirely (#67).
--   * 'iiTemp' — tracked temperature, matching the row-level policy
--     #1268 settled in @item_list.lua@: it cools continuously, so keying
--     on it would split and re-merge a row forever. The group's
--     temperature is presented honestly through @tempSummary@ instead,
--     and a temperature-SENSITIVE action defines its own instance
--     selection rather than inheriting the representative from here —
--     the Drink gesture (#1580) fans out into one submenu entry per
--     member, and any further such action owes the same.
itemContentsSig ∷ ItemInstance → Text
itemContentsSig inst
    | null (iiContents inst) = T.empty
    | otherwise = T.intercalate ";" $ sort
        [ T.intercalate ":"
            [ iiDefName c
            , tshow (iiCurrentFill c)
            , tshow (iiQuality c)
            , tshow (iiCondition c)
            , tshow (iiWeight c)
            , tshow (iiSharpness c)
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

-- | One band of a quality→label mapping (#345). `qtMin` is the
--   inclusive lower bound (0..100) of the band; 'qualityTierLabel'
--   resolves a quality value to the highest band whose bound it clears.
--
--   A band is only ever meaningful as part of a TABLE, and the
--   invariants live there rather than on this pair: see
--   'ItemDef'\'s 'idQualityTiers' for the four an authored table must
--   satisfy, and 'Engine.Asset.YamlItems.parseItemYamlQualityTiers'
--   (#1739) for where they are enforced. In particular a bare
--   @QualityTier 80 \"masterwork\"@ is a perfectly well-typed value that
--   would be REJECTED as a whole table, because it has no 0 floor.
data QualityTier = QualityTier
    { qtMin   ∷ !Float
    , qtLabel ∷ !Text
    } deriving (Show, Eq)

-- | Fallback quality tiers for any item def that doesn't declare its
--   own `quality_tiers:` override. The 0-floor band guarantees a
--   result for every non-negative quality value.
defaultQualityTiers ∷ [QualityTier]
defaultQualityTiers =
    [ QualityTier 90 "excellent"
    , QualityTier 75 "good"
    , QualityTier 50 "average"
    , QualityTier 25 "bad"
    , QualityTier 0  "atrocious"
    ]

-- | Resolve a quality percentage to its named tier: the def's own
--   table when it declares one (idQualityTiers), else
--   'defaultQualityTiers'. Picks the highest-qtMin band the value
--   clears.
--
--   The override REPLACES the default table; the default's 0-floor band
--   is deliberately not supplied as a fallback, so this returns
--   'Nothing' for any quality clearing no band of the table in force.
--   That is not a gap to paper over here — 'Nothing' is what the four
--   reader sites turn into an OMITTED tier field, so a repair at this
--   level would keep a malformed table loadable and merely relabel its
--   symptom. #1739 puts the fix at the authoring boundary instead: an
--   accepted non-empty 'idQualityTiers' carries its own @qtMin ≡ 0@
--   band, so for any def that loaded, every finite quality in 0..100
--   resolves to exactly one non-blank label. (A quality OUTSIDE that
--   domain still has no promise: a negative one clears no band, and
--   'Item.Roll.rollGroundQuality' takes an explicit value verbatim.)
qualityTierLabel ∷ ItemDef → Float → Maybe Text
qualityTierLabel def q = qtLabel ⊚ find (\t → q ≥ qtMin t) sorted
  where
    tiers  = if null (idQualityTiers def) then defaultQualityTiers
                                           else idQualityTiers def
    sorted = sortBy (\a b → compare (qtMin b) (qtMin a)) tiers

-- | Engine-wide registry of all loaded item defs.
newtype ItemManager = ItemManager
    { imDefs ∷ HM.HashMap Text ItemDef
    } deriving (Show, Eq)

emptyItemManager ∷ ItemManager
emptyItemManager = ItemManager HM.empty

lookupItemDef ∷ Text → ItemManager → Maybe ItemDef
lookupItemDef name (ItemManager m) = HM.lookup name m
