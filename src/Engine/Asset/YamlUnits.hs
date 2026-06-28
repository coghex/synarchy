{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
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
    , defaultUnitYamlBody
    , defaultUnitYamlNaturalResistance
    , loadUnitYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

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
--   copies to grant. Each copy is a distinct ItemInstance (quality /
--   condition are rolled per instance), so a count: 5 entry rolls
--   five independent items rather than a stacked one.
--
--   drop_priority feeds the spawn-time capacity check: a unit whose
--   full loadout (inventory + equipment + accessories, fill counted
--   at 1 L = 1 kg) exceeds its rolled carrying_capacity sheds
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
      -- ^ optional: path to portrait texture for info panel.
      -- Reserved for future use — parsed and carried but not yet rendered.
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
        ⊛ v .:? "max_speed"           .!= 3.0
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

newtype UnitYamlFile = UnitYamlFile
    { uyfUnits ∷ [UnitYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlFile where
    parseJSON = withObject "UnitYamlFile" $ \v → UnitYamlFile
        ⊚ v .: "units"

loadUnitYaml ∷ LoggerState → FilePath → IO [UnitYamlDef]
loadUnitYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse unit YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right uf → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (uyfUnits uf)))
                <> " unit definitions from " <> T.pack path
            return (uyfUnits uf)
