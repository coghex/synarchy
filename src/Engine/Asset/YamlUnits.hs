{-# LANGUAGE Strict, DeriveGeneric #-}
module Engine.Asset.YamlUnits
    ( UnitYamlDef(..)
    , UnitYamlAnim(..)
    , UnitYamlStat(..)
    , UnitYamlSkill(..)
    , UnitYamlBodyAttr(..)
    , UnitYamlBody(..)
    , UnitYamlInventoryEntry(..)
    , UnitYamlModifier(..)
    , UnitYamlBodyPart(..)
    , UnitYamlLayer(..)
    , UnitYamlNaturalWeapon(..)
    , UnitYamlStrike(..)
    , UnitYamlNaturalResistance(..)
    , UnitYamlFile(..)
    , UnitYamlAssetDef(..)
    , loadUnitYaml
    , loadUnitYamlOutcome
    , loadUnitYamlAssets
    , unitYamlBodyPartToBodyPart
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson (Parser)
import qualified Data.Text as T
import Data.List (intercalate)
import Engine.Core.Log (LoggerState)
import Engine.Asset.YamlList (loadYamlList, loadYamlListOutcome)
import Unit.Types.Combat (BodyPart(..))

-- | One named animation as loaded from YAML. Per-direction frame paths;
--   directions accept short ("S","SW") or long ("south","south-east").
--
--   `flip: true` declares the animation is bilaterally symmetric, so the
--   author only has to supply the 5 eastern-half directions (S, SE, E,
--   NE, N) and the renderer mirrors SW/W/NW from SE/E/NE at draw time.
--   Default is `false` — author must supply all 8 explicitly; missing
--   directions fall back to the unit's static T-pose, not to a flipped
--   sibling (this is the safe choice for anims that show an asymmetric
--   prop like a weapon in the right hand).
data UnitYamlAnim = UnitYamlAnim
    { uyaFps    ∷ !Float
    , uyaLoop   ∷ !Bool
    , uyaFlip   ∷ !Bool
    , uyaFrames ∷ !(Map.Map Text [Text])
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlAnim where
    parseJSON = withObject "UnitYamlAnim" $ \v → UnitYamlAnim
        ⊚ v .:? "fps"    .!= 8.0
        ⊛ v .:? "loop"   .!= True
        ⊛ v .:? "flip"   .!= False
        ⊛ v .:? "frames" .!= Map.empty

-- | One stat as declared in YAML: a base value and a range. At spawn
--   time the engine rolls a value from a truncated-normal distribution
--   centered on @base@ with sigma = @range@ / 4, clamped to the window
--   [base - range/2, base + range/2].
data UnitYamlStat = UnitYamlStat
    { uysBase  ∷ !Float
    , uysRange ∷ !Float
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlStat where
    parseJSON = withObject "UnitYamlStat" $ \v → UnitYamlStat
        ⊚ v .:  "base"
        ⊛ v .:? "range" .!= 0.0

-- | One body attribute as declared in YAML: a mean value and a range.
--   Mean/range rather than base/range to signal that these are physical
--   attributes (rolled once at spawn, fixed thereafter) rather than
--   stats with modifier/XP semantics. Internally still rolls through
--   the same truncated-normal `rollStat` path.
data UnitYamlBodyAttr = UnitYamlBodyAttr
    { uybaMean  ∷ !Float
    , uybaRange ∷ !Float
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlBodyAttr where
    parseJSON = withObject "UnitYamlBodyAttr" $ \v → UnitYamlBodyAttr
        ⊚ v .:  "mean"
        ⊛ v .:? "range" .!= 0.0

-- | Body composition for a unit type. All three optional; defaults are
--   the human-average values calibrated against `defaultUnitYamlBody`.
data UnitYamlBody = UnitYamlBody
    { uybHeight  ∷ !UnitYamlBodyAttr   -- ^ meters
    , uybBulk    ∷ !UnitYamlBodyAttr   -- ^ unitless multiplier (1 = average)
    , uybBodyfat ∷ !UnitYamlBodyAttr   -- ^ fraction 0..1
    } deriving (Show, Eq, Generic)

defaultUnitYamlBody ∷ UnitYamlBody
defaultUnitYamlBody = UnitYamlBody
    { uybHeight  = UnitYamlBodyAttr 1.8 1.0
    , uybBulk    = UnitYamlBodyAttr 1.0 1.0
    , uybBodyfat = UnitYamlBodyAttr 0.2 0.36
    }

instance FromJSON UnitYamlBody where
    parseJSON = withObject "UnitYamlBody" $ \v → UnitYamlBody
        ⊚ v .:? "height"  .!= uybHeight  defaultUnitYamlBody
        ⊛ v .:? "bulk"    .!= uybBulk    defaultUnitYamlBody
        ⊛ v .:? "bodyfat" .!= uybBodyfat defaultUnitYamlBody

-- | One starting-inventory entry: which item def to give the unit,
--   optionally how much fill it has (for containers), and how many
--   copies to grant. Each copy is a distinct ItemInstance (quality
--   and weight are rolled per instance; condition starts full on every
--   one, #1421), so a count: 5 entry rolls five independent items
--   rather than a stacked one.
--
--   drop_priority feeds the spawn-time capacity check: a unit whose
--   full loadout (inventory + equipment + accessories, fill counted
--   at each container's per-unit fill weight) exceeds its rolled
--   carrying_capacity sheds
--   droppable entries in DESCENDING priority until it fits. 0 (the
--   default) = never shed — armor, weapons, and survival kit always
--   arrive. Acolytes mark the pick (2) and shovel (1).
data UnitYamlInventoryEntry = UnitYamlInventoryEntry
    { uyieItem  ∷ !Text         -- ^ ItemDef name (e.g. "canteen_steel_2l")
    , uyieFill  ∷ !(Maybe Float) -- ^ initial fill in litres; nil = empty
    , uyieCount ∷ !Int           -- ^ number of copies; defaults to 1
    , uyieDropPriority ∷ !Int    -- ^ capacity-shed order; 0 = never
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlInventoryEntry where
    parseJSON = withObject "UnitYamlInventoryEntry" $ \v → UnitYamlInventoryEntry
        ⊚ v .:  "item"
        ⊛ v .:? "fill"
        ⊛ v .:? "count" .!= 1
        ⊛ v .:? "drop_priority" .!= 0

-- | One permanent stat modifier every spawned unit of this type
--   carries from birth — e.g. the technomule's "cybernetic
--   enhancements" +50% on carrying_capacity. @delta@ is additive,
--   @percent@ is a fractional multiplier (0.5 = +50%); both default
--   to 0 so a block can declare either or both. @source@ is the
--   label shown in the stat tooltip.
data UnitYamlModifier = UnitYamlModifier
    { uymStat    ∷ !Text
    , uymDelta   ∷ !Float
    , uymPercent ∷ !Float
    , uymSource  ∷ !Text
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlModifier where
    parseJSON = withObject "UnitYamlModifier" $ \v → UnitYamlModifier
        ⊚ v .:  "stat"
        ⊛ v .:? "delta"   .!= 0.0
        ⊛ v .:? "percent" .!= 0.0
        ⊛ v .:  "source"

-- | One skill as declared in YAML. Like a stat (base + range, rolled
--   at spawn). Skills are continuous floats that grow via a closed-
--   form XP formula — no per-level threshold to declare.
data UnitYamlSkill = UnitYamlSkill
    { uyskBase  ∷ !Float
    , uyskRange ∷ !Float
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlSkill where
    parseJSON = withObject "UnitYamlSkill" $ \v → UnitYamlSkill
        ⊚ v .:  "base"
        ⊛ v .:? "range" .!= 0.0

-- | One body part as declared in YAML. Mirrors the runtime
--   `Unit.Types.BodyPart`; loaded into `udBodyParts` and consumed
--   by Combat.Resolution's body-part picker + reach filter.
-- | One tissue layer of a body part: a substance + its thickness (mm).
data UnitYamlLayer = UnitYamlLayer
    { uylName      ∷ !(Maybe Text)   -- ^ combat-log noun; defaults to material
    , uylMaterial  ∷ !Text
    , uylThickness ∷ !Float
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlLayer where
    parseJSON = withObject "UnitYamlLayer" $ \v → UnitYamlLayer
        ⊚ v .:? "name"
        ⊛ v .:  "material"
        ⊛ v .:  "thickness"

data UnitYamlBodyPart = UnitYamlBodyPart
    { uybpId              ∷ !Text
    , uybpName            ∷ !(Maybe Text)
    , uybpParent          ∷ !(Maybe Text)
    , uybpVital           ∷ !Bool
    , uybpAreaWeight      ∷ !Float
    , uybpTacticalValue   ∷ !Float
    , uybpBleedFactor     ∷ !Float
    , uybpHeightLow       ∷ !Float
    , uybpHeightHigh      ∷ !Float
    , uybpLayers          ∷ ![UnitYamlLayer]   -- outer→inner; [] ⇒ default
    , uybpTargetable      ∷ !Bool              -- macro-part (aimed at) vs subpart
    , uybpDepth           ∷ !Float             -- subpart depth 0..1 (slash swath)
    , uybpAffectsLocomotion ∷ !Bool            -- leg/foot-type part (injurySpeedMult)
    , uybpAffectsBalance    ∷ !Bool            -- torso-type part (injurySpeedMult)
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlBodyPart where
    parseJSON = withObject "UnitYamlBodyPart" $ \v → UnitYamlBodyPart
        ⊚ v .:  "id"
        ⊛ v .:? "name"
        ⊛ v .:? "parent"
        ⊛ v .:? "vital"               .!= False
        -- Subparts ("targetable: false") aren't aimed at, so area_weight
        -- is an allocation weight among siblings; default 1.0. Macro-parts
        -- still require it (the targeting picker uses it).
        ⊛ v .:? "area_weight"         .!= 1.0
        ⊛ v .:? "tactical_value"      .!= 0.5
        -- (max_health_factor removed — durability is derived from the
        -- tissue layers; any leftover key in YAML is ignored.)
        ⊛ v .:? "bleed_factor"        .!= 1.0
        ⊛ v .:? "height_low"          .!= 0.0
        -- Default reach band = ground to "tall" (9 m). A part that omits
        -- heights must stay REACHABLE: Combat.Resolution drops targetable
        -- parts whose height_high < reachLo, so a 0.0 default would silently
        -- make an unspecified part un-hittable. Authoring real heights still
        -- narrows the band; this is just a safe, always-reachable fallback.
        ⊛ v .:? "height_high"         .!= 9.0
        ⊛ v .:? "layers"              .!= []
        ⊛ v .:? "targetable"          .!= True
        ⊛ v .:? "depth"               .!= 0.0
        ⊛ v .:? "affects_locomotion"  .!= False
        ⊛ v .:? "affects_balance"     .!= False

-- | Convert one YAML-declared body part into the runtime `BodyPart` both
--   the Lua unit loader and headless physiology tests consume. The SOLE
--   conversion path — a hand-transcribed part list would silently drift
--   from the shipped data, so anything that needs the real acolyte
--   topology (fall calibration, combat tests) must go through this
--   function rather than re-deriving it.
unitYamlBodyPartToBodyPart ∷ UnitYamlBodyPart → BodyPart
unitYamlBodyPartToBodyPart p = BodyPart
    { bpId              = uybpId p
    , bpName            = maybe (uybpId p) id (uybpName p)
    , bpParent          = uybpParent p
    , bpVital           = uybpVital p
    , bpAreaWeight      = uybpAreaWeight p
    , bpTacticalValue   = uybpTacticalValue p
    , bpBleedFactor     = uybpBleedFactor p
    , bpHeightLow       = uybpHeightLow p
    , bpHeightHigh      = uybpHeightHigh p
    , bpLayers          =
        [ ( maybe (uylMaterial l) id (uylName l)
          , uylMaterial l, uylThickness l )
        | l ← uybpLayers p ]
    , bpTargetable      = uybpTargetable p
    , bpDepth           = uybpDepth p
    , bpAffectsLocomotion = uybpAffectsLocomotion p
    , bpAffectsBalance     = uybpAffectsBalance p
    }

-- | Natural (innate) weapon block — claws/fangs/fists. Optional on
--   the unit YAML. When present, Combat.Resolution falls back to
--   this when no equipped weapon is found.
-- | Per-attack-kind strike block inside a natural_weapon. All fields
--   optional so a creature declares only what it has (a clawless biter
--   leaves out `slash`). `material` names a substance from
--   data/substances/*.yaml.
data UnitYamlStrike = UnitYamlStrike
    { uysEff          ∷ !Float
    , uysMaterial     ∷ !Text
    , uysBladeLength  ∷ !Float   -- cm (stab, slash)
    , uysSharpness    ∷ !Float   -- lower = sharper (stab, slash)
    , uysImpactArea   ∷ !Float   -- mm² (blunt)
    , uysMass         ∷ !Float   -- kg of the striking appendage
    , uysLength       ∷ !Float   -- cm lever length; 0 ⇒ use blade_length
    , uysCenterOfMass ∷ !Float   -- 0..1 from the limb
    , uysName         ∷ !Text    -- display name ("claws"/"fangs"/"paw")
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlStrike where
    parseJSON = withObject "UnitYamlStrike" $ \v → UnitYamlStrike
        ⊚ v .:? "eff"            .!= 0.0
        ⊛ v .:? "material"       .!= "flesh"
        ⊛ v .:? "blade_length"   .!= 0.0
        ⊛ v .:? "sharpness"      .!= 1000.0   -- effectively dull if unspecified
        ⊛ v .:? "impact_area"    .!= 0.0
        ⊛ v .:? "mass"           .!= 0.0
        ⊛ v .:? "length"         .!= 0.0
        ⊛ v .:? "center_of_mass" .!= 0.5
        ⊛ v .:? "name"           .!= ""

-- | A natural weapon that delivers no attack of a given kind.
emptyStrike ∷ UnitYamlStrike
emptyStrike = UnitYamlStrike 0.0 "flesh" 0.0 1000.0 0.0 0.0 0.0 0.5 ""

-- | Default blunt strike (everything can throw a clumsy bludgeon).
defaultBluntStrike ∷ UnitYamlStrike
defaultBluntStrike = UnitYamlStrike 0.5 "bone" 0.0 1000.0 0.0 0.0 0.0 0.5 "fists"

data UnitYamlNaturalWeapon = UnitYamlNaturalWeapon
    { uynwWeaponClass          ∷ !Text
    , uynwEffectiveBladeLength ∷ !Float   -- cm; reach only
    , uynwAttackCooldown       ∷ !Float   -- seconds
    , uynwSlash                ∷ !UnitYamlStrike
    , uynwStab                 ∷ !UnitYamlStrike
    , uynwBlunt                ∷ !UnitYamlStrike
    , uynwComboAttack          ∷ !Bool
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlNaturalWeapon where
    parseJSON = withObject "UnitYamlNaturalWeapon" $ \v →
        UnitYamlNaturalWeapon
        ⊚ v .:  "weapon_class"
        ⊛ v .:? "effective_blade_length" .!= 0.0
        ⊛ v .:? "attack_cooldown"        .!= 2.0
        ⊛ v .:? "slash"                  .!= emptyStrike
        ⊛ v .:? "stab"                   .!= emptyStrike
        ⊛ v .:? "blunt"                  .!= defaultBluntStrike
        ⊛ v .:? "combo_attack"           .!= False

-- | Innate per-kind damage resistance. Defaults to all zeros
--   (humans). Bears declare slash 0.5, stab 0.1, blunt 0.3.
data UnitYamlNaturalResistance = UnitYamlNaturalResistance
    { uynrSlash ∷ !Float
    , uynrStab  ∷ !Float
    , uynrBlunt ∷ !Float
    } deriving (Show, Eq, Generic)

defaultUnitYamlNaturalResistance ∷ UnitYamlNaturalResistance
defaultUnitYamlNaturalResistance =
    UnitYamlNaturalResistance 0.0 0.0 0.0

instance FromJSON UnitYamlNaturalResistance where
    parseJSON = withObject "UnitYamlNaturalResistance" $ \v →
        UnitYamlNaturalResistance
        ⊚ v .:? "slash" .!= 0.0
        ⊛ v .:? "stab"  .!= 0.0
        ⊛ v .:? "blunt" .!= 0.0

-- | Only @name@ and @sprite@ are mandatory; everything else has defaults
data UnitYamlDef = UnitYamlDef
    { uydName              ∷ !Text       -- ^ unique identifier (e.g. "acolyte")
    , uydNamePool          ∷ !(Maybe Text)
      -- ^ optional: id of the name pool this unit type draws personal
      --   names from (resolves to data/names/<id>.yaml). Nothing → the
      --   unit type has no personal names (#264).
    , uydDisplayName       ∷ !(Maybe Text)
      -- ^ optional: human-readable species label ("Brown Bear") for the
      --   UI. Nothing → the prettified def name is used.
    , uydSprite            ∷ !Text       -- ^ path to default sprite texture
    , uydBaseWidth         ∷ !Float      -- ^ ground contact diameter in pixels (0 = point)
    , uydMaxSpeed          ∷ !Float      -- ^ tiles/sec reference top speed at agility 1.0 (default 3.0)
    , uydRunThreshold      ∷ !Float      -- ^ run-anim threshold as a fraction of max_speed (default 0.6)
    , uydDirectionalSprites ∷ !(Map.Map Text Text)
      -- ^ optional: direction key ("S","SW",…) → texture path
    , uydPortrait          ∷ !(Maybe Text)
      -- ^ optional: path to portrait texture for the info panel. When
      --   present it is loaded into `udPortrait` and preferred by the v2
      --   info pane; defs without it fall back to the live frame texture.
    , uydStateAnimations   ∷ !(Map.Map Text Text)
      -- ^ optional: state name → animation name (e.g. "idle" → "idle-standing")
    , uydAnimations        ∷ !(Map.Map Text UnitYamlAnim)
      -- ^ optional: animation library
    , uydEagerStats        ∷ !Bool
      -- ^ if true, all stats are rolled at spawn; otherwise rolled
      --   lazily on first getStat. Defaults to false (lazy).
    , uydStats             ∷ !(Map.Map Text UnitYamlStat)
      -- ^ optional: per-stat base/range schema
    , uydBody              ∷ !UnitYamlBody
      -- ^ optional: physical attributes (height, bulk, bodyfat).
      --   Folded into the stat templates at load time so they roll
      --   through the same path. Requires `eager_stats: true` if you
      --   want derived values (max_hydration, weight) at spawn time.
    , uydSkills            ∷ !(Map.Map Text UnitYamlSkill)
      -- ^ optional: per-skill base/range/xp_per_level schema.
      --   Skills are always eager-rolled at spawn (no lazy mode).
    , uydKnowledge         ∷ !(Map.Map Text UnitYamlSkill)
      -- ^ optional: knowledge the unit spawns KNOWING, base/range like a
      --   skill (reuses UnitYamlSkill). Rolled into uiKnowledge at spawn.
    , uydStartingInventory ∷ ![UnitYamlInventoryEntry]
      -- ^ optional: items every freshly spawned unit of this type
      --   starts with. Looked up against the ItemManager at spawn time;
      --   missing item names log a warning and are skipped.
    , uydEquipmentClass    ∷ !(Maybe Text)
      -- ^ optional: name of the EquipmentClass this unit uses
      --   (e.g. "humanoid"). Nothing → no equipment UI for this unit.
    , uydStartingEquipment ∷ !(Map.Map Text Text)
      -- ^ optional: slot id → item def name. Each item is equipped
      --   into the named slot at spawn time, validating that the
      --   item's kind matches the slot's accepted kind.
    , uydStartingAccessories ∷ ![Text]
      -- ^ optional: list of item def names to be equipped as
      --   accessories (no slot) at spawn time. Order preserved.
    , uydBodyParts          ∷ ![UnitYamlBodyPart]
      -- ^ optional: targetable body parts. Empty list = no combat
      --   targeting (resolver bails). Acolyte ships 12-part humanoid,
      --   bear ships 8-part quadruped.
    , uydNaturalResistance  ∷ !UnitYamlNaturalResistance
      -- ^ optional: innate hide/skin resistance per attack kind.
      --   Defaults to all zeros (humans). Bears: slash 0.5, stab 0.1,
      --   blunt 0.3.
    , uydNaturalWeapon      ∷ !(Maybe UnitYamlNaturalWeapon)
      -- ^ optional: innate weapon (claws/fangs/fists). Used by combat
      --   when no equipped weapon is found. Acolytes omit (rely on
      --   equipment); bears declare an "unarmed" natural weapon.
    , uydModifiers          ∷ ![UnitYamlModifier]
      -- ^ optional: permanent stat modifiers seeded at spawn
      --   (technomule: carrying_capacity +50% "cybernetic
      --   enhancements"). Visible in the stat tooltip like any
      --   other modifier.
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlDef where
    parseJSON = withObject "UnitYamlDef" $ \v → UnitYamlDef
        ⊚ v .:  "name"
        ⊛ v .:? "name_pool"
        ⊛ v .:? "display_name"
        ⊛ v .:  "sprite"
        ⊛ v .:? "base_width"          .!= 0.0
        ⊛ requireMaxSpeed v
        ⊛ v .:? "run_threshold"       .!= 0.6
        ⊛ v .:? "directional_sprites" .!= Map.empty
        ⊛ v .:? "portrait"
        ⊛ v .:? "state_animations"    .!= Map.empty
        ⊛ v .:? "animations"          .!= Map.empty
        ⊛ v .:? "eager_stats"         .!= False
        ⊛ v .:? "stats"               .!= Map.empty
        ⊛ v .:? "body"                .!= defaultUnitYamlBody
        ⊛ v .:? "skills"              .!= Map.empty
        ⊛ v .:? "knowledge"           .!= Map.empty
        ⊛ v .:? "starting_inventory"  .!= []
        ⊛ v .:? "equipment_class"
        ⊛ v .:? "starting_equipment"  .!= Map.empty
        ⊛ v .:? "starting_accessories".!= []
        ⊛ v .:? "body_parts"          .!= []
        ⊛ v .:? "natural_resistance"  .!= defaultUnitYamlNaturalResistance
        ⊛ v .:? "natural_weapon"
        ⊛ v .:? "modifiers"           .!= []

-- | Read a unit def's optional @max_speed@ as a FINITE, STRICTLY
--   POSITIVE number of tiles per second, diagnosing every rejection by
--   the unit's own name (#2290) — the shape
--   @Engine.Asset.YamlLootTables.requireLootWeight@ and
--   @Engine.Asset.YamlFlora.requireRegrowthTime@ already use. Both are
--   module-private, so these are code spans rather than Haddock links
--   (@tools\/haddock_link_audit.py@; widening an export list to make a
--   link resolve is what @docs\/haddock_link_resolution_design.md@ D-2
--   forbids).
--
--   Naming the unit is why this is a named parser rather than a
--   @.:? "max_speed" .!= 3.0@ plus a check: 'loadUnitYaml' supplies the
--   failing FILE path, but an ordinary Aeson field error only reaches
--   for a JSON path like @$.units[3].max_speed@ — an index nobody can
--   map back to a unit without counting entries.
--
--   The domain has to be enforced HERE because @max_speed@ is not a
--   speed the simulation ever clamps. It is the run-gait threshold
--   (@maxSpeed * runThreshold@ in "Unit.Thread.Command.Motion"), so a
--   zero makes every commanded speed \"running\" and a negative one
--   inverts the test; and it is the base every AI speed derives from
--   via @unit.getMaxSpeed@, so a NaN silently poisons every gait
--   'scripts\/movement_speed.lua' computes. None of those fail loudly
--   anywhere downstream.
--
--   An ABSENT key keeps the documented 3.0 default — the field is
--   optional and every shipped def that omits it means \"typical\".
--   Only a value the author actually wrote is judged.
--
--   Absence is decided by a direct 'KM.lookup' rather than by @.:?@,
--   which reports an explicit @null@ as 'Nothing' and would hand
--   @max_speed: null@ the default instead of refusing it. A written
--   @null@ (in any of YAML's spellings — @null@, @~@, or an empty
--   value) is a value the author supplied and is not a speed, so it
--   takes the not-a-number branch like any other wrong type.
--
--   Taking the whole 'Aeson.Value' rather than decoding to 'Float'
--   first is deliberate, for the reason the two sibling parsers give:
--   YAML's @.nan@\/@.inf@ resolve to STRINGS (the yaml package's scalar
--   resolver only recognizes ordinary numeric syntax), so decoding
--   first would surface those as a type error naming neither the unit
--   nor what was wrong. Both checks still run AFTER narrowing to the
--   stored 32-bit 'Float': an ordinary @1.0e+100@ is a valid
--   'Scientific' that becomes 'Infinity' there, and an equally
--   ordinary @1.0e-60@ becomes @0.0@ — the value the runtime actually
--   uses is the only one worth checking.
requireMaxSpeed ∷ Aeson.Object → Aeson.Parser Float
requireMaxSpeed v = do
    unitName ← v .:? "name" .!= ("<unnamed>" ∷ Text)
    case KM.lookup (Key.fromText "max_speed") v of
        Nothing  → pure 3.0
        Just val → case val of
            Aeson.Number s →
                let f = realToFrac s ∷ Float
                in if isNaN f ∨ isInfinite f
                     then bad unitName ("must be finite, got " <> tshow val)
                     else if f ≤ 0
                       then bad unitName
                              ("must be strictly positive, got " <> tshow f)
                       else pure f
            _ → bad unitName
                    ("must be a number of tiles per second, got " <> tshow val)
  where
    bad unitName why = fail ∘ T.unpack $
        "unit '" <> unitName <> "': 'max_speed' " <> why

-- | An ASSET-ONLY unit declaration (#1257): the authoritative inventory
--   entry for a shipped @assets\/textures\/units\/\<name\>\/@ tree that
--   is deliberately NOT a gameplay unit.
--
--   It declares animation frames — for @tools\/pack_atlas.py@'s strict
--   inventory and for the @--preview units\/\<name\>@ browser's playback
--   metadata — and nothing else. It carries no @sprite@, no stats, no
--   body: there is nothing here to register, load a gameplay texture
--   for, list, or spawn, and 'loadUnitYaml' never returns one, so the
--   exclusion is a property of WHICH LIST the entry is in rather than of
--   a field it happens to omit or a decode it happens to fail.
--
--   #1261 (TEX-6) promoted @tiller@, @unknown_unit@ and
--   @white_tailed_deer@ to real @units:@ entries, but the form remains
--   supported for a tree that genuinely owns art without being a unit.
--   Such a declaration is previewable and atlas-validated while staying
--   outside the gameplay registry; both shipped entries and fixtures may
--   exercise that boundary.
data UnitYamlAssetDef = UnitYamlAssetDef
    { uyadName       ∷ !Text
      -- ^ must equal the asset directory name — @Engine.Preview.Unit@
      --   resolves @data\/units\/\<name\>.yaml@ and then selects the def
      --   whose name matches the requested unit.
    , uyadAnimations ∷ !(Map.Map Text UnitYamlAnim)
    } deriving (Show, Eq, Generic)

-- | The COMPLETE key set of an asset-only entry. Aeson ignores keys a
--   parser does not ask for, which would let @sprite:@ ride along in an
--   @asset_units:@ entry and decode cleanly — the entry would then be
--   silently skipped by 'loadUnitYaml' and look like a unit that simply
--   failed to register. #1257 requires a disallowed field to be an
--   ERROR, not something ignored, so the key set is checked explicitly.
--   A whitelist, not a gameplay blacklist: a typo must fail too.
assetOnlyKeys ∷ [Key.Key]
assetOnlyKeys = ["name", "animations"]

instance FromJSON UnitYamlAssetDef where
    parseJSON = withObject "UnitYamlAssetDef" $ \v → do
        let extra = filter (∉ assetOnlyKeys) (KM.keys v)
        unless (null extra) $ fail $
            "asset-only unit declaration has unexpected field(s) "
            <> intercalate ", " (map (show ∘ Key.toString) extra)
            <> "; an `asset_units:` entry declares exactly "
            <> intercalate ", " (map (show ∘ Key.toString) assetOnlyKeys)
        UnitYamlAssetDef
            ⊚ v .: "name"
            ⊛ v .: "animations"

-- | One @data\/units\/*.yaml@ file: gameplay defs under @units:@ and
--   asset-only declarations under @asset_units:@. Either key may be
--   absent, but a file with NEITHER is refused — that is exactly what a
--   mistyped top-level key looks like, and silently decoding it as
--   "zero units" would lose a whole file's worth of definitions without
--   a word.
data UnitYamlFile = UnitYamlFile
    { uyfUnits      ∷ [UnitYamlDef]
    , uyfAssetUnits ∷ [UnitYamlAssetDef]
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlFile where
    parseJSON = withObject "UnitYamlFile" $ \v → do
        gameplay ← v .:? "units"
        assets   ← v .:? "asset_units"
        case (gameplay, assets) of
            (Nothing, Nothing) → fail
                "unit YAML declares neither `units:` nor `asset_units:`"
            _ → pure (UnitYamlFile (fromMaybe [] gameplay)
                                   (fromMaybe [] assets))

-- | 'loadUnitYaml' with the decode OUTCOME kept (#2203):
--   'Nothing' is a parse failure, @Just xs@ a file that decoded
--   (possibly to an empty list). The startup loader needs the two
--   apart; every other caller reads 'loadUnitYaml'.
loadUnitYamlOutcome ∷ LoggerState → FilePath → IO (Maybe [UnitYamlDef])
loadUnitYamlOutcome logger =
    loadYamlListOutcome logger "unit" "unit definitions" uyfUnits

-- | The GAMEPLAY unit definitions in a file. Asset-only declarations are
--   not returned, so nothing downstream registers, textures, lists or
--   spawns them.
loadUnitYaml ∷ LoggerState → FilePath → IO [UnitYamlDef]
loadUnitYaml logger path =
    fromMaybe [] ⊚ loadUnitYamlOutcome logger path

-- | The ASSET-ONLY declarations in a file (#1257). Read by the asset
--   inventory and by tests; deliberately not by unit registration.
loadUnitYamlAssets ∷ LoggerState → FilePath → IO [UnitYamlAssetDef]
loadUnitYamlAssets logger =
    loadYamlList logger "unit" "asset-only unit declarations" uyfAssetUnits
