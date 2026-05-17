{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlUnits
    ( UnitYamlDef(..)
    , UnitYamlAnim(..)
    , UnitYamlStat(..)
    , UnitYamlSkill(..)
    , UnitYamlBodyAttr(..)
    , UnitYamlBody(..)
    , UnitYamlInventoryEntry(..)
    , UnitYamlFile(..)
    , defaultUnitYamlBody
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
data UnitYamlAnim = UnitYamlAnim
    { uyaFps    ∷ !Float
    , uyaLoop   ∷ !Bool
    , uyaFrames ∷ !(Map.Map Text [Text])
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlAnim where
    parseJSON = withObject "UnitYamlAnim" $ \v → UnitYamlAnim
        ⊚ v .:? "fps"    .!= 8.0
        ⊛ v .:? "loop"   .!= True
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

-- | One starting-inventory entry: which item def to give the unit
--   and, optionally, how much fill it has (for containers).
data UnitYamlInventoryEntry = UnitYamlInventoryEntry
    { uyieItem ∷ !Text         -- ^ ItemDef name (e.g. "canteen_steel_2l")
    , uyieFill ∷ !(Maybe Float) -- ^ initial fill in litres; nil = empty
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlInventoryEntry where
    parseJSON = withObject "UnitYamlInventoryEntry" $ \v → UnitYamlInventoryEntry
        ⊚ v .:  "item"
        ⊛ v .:? "fill"

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

-- | Only @name@ and @sprite@ are mandatory; everything else has defaults
data UnitYamlDef = UnitYamlDef
    { uydName              ∷ !Text       -- ^ unique identifier (e.g. "acolyte")
    , uydSprite            ∷ !Text       -- ^ path to default sprite texture
    , uydBaseWidth         ∷ !Float      -- ^ ground contact diameter in pixels (0 = point)
    , uydDirectionalSprites ∷ !(Map.Map Text Text)
      -- ^ optional: direction key ("S","SW",…) → texture path
    , uydPortrait          ∷ !(Maybe Text)
      -- ^ optional: path to portrait texture for info panel.
      -- Reserved for future use — parsed and carried but not yet rendered.
    , uydStateAnimations   ∷ !(Map.Map Text Text)
      -- ^ optional: state name → animation name (e.g. "idle" → "breathing-idle")
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
    , uydStartingInventory ∷ ![UnitYamlInventoryEntry]
      -- ^ optional: items every freshly spawned unit of this type
      --   starts with. Looked up against the ItemManager at spawn time;
      --   missing item names log a warning and are skipped.
    } deriving (Show, Eq, Generic)

instance FromJSON UnitYamlDef where
    parseJSON = withObject "UnitYamlDef" $ \v → UnitYamlDef
        ⊚ v .:  "name"
        ⊛ v .:  "sprite"
        ⊛ v .:? "base_width"          .!= 0.0
        ⊛ v .:? "directional_sprites" .!= Map.empty
        ⊛ v .:? "portrait"
        ⊛ v .:? "state_animations"    .!= Map.empty
        ⊛ v .:? "animations"          .!= Map.empty
        ⊛ v .:? "eager_stats"         .!= False
        ⊛ v .:? "stats"               .!= Map.empty
        ⊛ v .:? "body"                .!= defaultUnitYamlBody
        ⊛ v .:? "skills"              .!= Map.empty
        ⊛ v .:? "starting_inventory"  .!= []

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
