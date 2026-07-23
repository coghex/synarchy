{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | The unit template (`UnitDef`) and its small building-block value
--   types, split out of "Unit.Types" (#575) — re-exported there so the
--   public API is unchanged.
module Unit.Types.Def
    ( Animation(..)
    , StatModifier(..)
    , NamePool(..)
    , UnitDef(..)
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle(..))
import Unit.Direction (Direction(..))
import Unit.Types.Combat (BodyPart(..), NaturalWeapon(..), NaturalResistance(..))

-- | A single animation: per-direction frame sequences.
--
--   Missing directions either fall back to T-pose (`aFlip = False`) or
--   are mirrored from their eastern-half counterpart (`aFlip = True` —
--   SW/W/NW pull from SE/E/NE with horizontally swapped UVs). The flag
--   is set per animation in YAML; default is False so an asset author
--   who forgets the flag and lists only 5 dirs sees obvious T-pose
--   fallbacks rather than silently-mirrored weapon hands.
data Animation = Animation
    { aFps    ∷ !Float
    , aLoop   ∷ !Bool
    , aFlip   ∷ !Bool
    , aFrames ∷ !(Map.Map Direction (V.Vector TextureHandle))
    } deriving (Show, Eq)

-- | One modifier on a stat. Multiple modifiers on the same stat
--   compose (after expiry filtering): deltas sum onto the base, then
--   percents sum into one multiplier —
--   @effective = (base + Σdelta) × (1 + Σpercent)@.
--   The (source, stat) pair is unique within a unit — re-adding the
--   same source on the same stat overwrites the previous entry.
--
--   Field order is load-bearing: Generic Serialize is positional, so
--   new fields go at the END (smPercent appended for save v33).
data StatModifier = StatModifier
    { smDelta  ∷ !Float
      -- ^ added to the base; can be negative (debuff).
    , smSource ∷ !Text
      -- ^ logical owner: "poison-A", "age", "wounded-left-arm", etc.
    , smExpiry ∷ !(Maybe Double)
      -- ^ Game-time seconds (gameTimeRef value) when the modifier
      --   becomes inactive. Nothing = permanent (removed only via
      --   removeModifier). Anchored to gameTimeRef rather than POSIX
      --   so the expiry survives save/load.
    , smPercent ∷ !Float
      -- ^ Fractional multiplier contribution: 0.5 = +50%, -0.25 =
      --   -25%, 0 = purely additive. Applied after all deltas.
    } deriving (Show, Eq, Generic, Serialize)

-- | A pool of names a unit type can draw from at spawn (#264). `npGiven`
--   is required for a unit to be named; `npFamily` is optional — a pool
--   with only given names yields single-token names. An empty pool means
--   the unit type has no personal names and falls back to its species
--   label in the UI. Loaded from data/names/<id>.yaml, referenced from a
--   unit def's `name_pool:` key.
data NamePool = NamePool
    { npGiven  ∷ ![Text]
    , npFamily ∷ ![Text]
    } deriving (Show, Eq, Generic)

-- | A unit definition (loaded from YAML, immutable after init).
--   This is the "template" — one per unit type.
data UnitDef = UnitDef
    { udName       ∷ !Text                            -- ^ e.g. "acolyte"
    , udNamePool   ∷ !(Maybe NamePool)
      -- ^ resolved name pool for this unit type, or Nothing if the def
      --   declares no `name_pool` (animals). When present, spawn draws a
      --   persistent personal name into `uiName` (#264).
    , udDisplayName ∷ !(Maybe Text)
      -- ^ optional human-readable species label ("Brown Bear") for the
      --   UI. Nothing → the prettified def name is used. Surfaced through
      --   unit.getInfo as `displayName`.
    , udTexture    ∷ !TextureHandle                   -- ^ default sprite handle
    , udPortrait   ∷ !(Maybe TextureHandle)
      -- ^ optional authored portrait for the info panel (YAML `portrait:`).
      --   Nothing → the UI falls back to the unit's live animation frame.
    , udDirSprites ∷ !(Map.Map Direction TextureHandle)
      -- ^ directional sprite overrides (may be empty)
    , udBaseWidth  ∷ !Float                           -- ^ ground contact diameter
    , udMaxSpeed   ∷ !Float
      -- ^ Per-species reference movement speed in tiles/sec — the top
      --   speed at agility 1.0. Actual sprint = max_speed × agility
      --   (computed Lua-side), so an exceptionally agile individual
      --   exceeds this. AI candidates derive comfort / ordered / sprint
      --   speeds from it + the unit's stats.
    , udRunThreshold ∷ !Float
      -- ^ Gait threshold as a fraction of udMaxSpeed: the sim renders
      --   the Running animation instead of Walking when the current move
      --   speed exceeds udRunThreshold × udMaxSpeed. Per-species (default
      --   0.6) so a lumbering unit breaks into a run later than a nimble
      --   one. Purely visual — independent of the comfort/sprint regime.
    , udAnimations ∷ !(HM.HashMap Text Animation)
      -- ^ named animation library (may be empty)
    , udStateAnims ∷ !(HM.HashMap Text Text)
      -- ^ state name → animation name (e.g. "idle" → "idle-standing")
    , udEagerStats    ∷ !Bool
      -- ^ if true, roll all stats at spawn; otherwise lazy on first read
    , udStatTemplates ∷ !(HM.HashMap Text (Float, Float))
      -- ^ stat name → (base, range). Empty means no rollable stats.
    , udBodyTemplates ∷ !(HM.HashMap Text (Float, Float))
      -- ^ bulk / bodyfat → (mean, range). Rolled at spawn ONLY,
      --   consumed by seedBodyComposition, then dropped. Kept off
      --   `udStatTemplates` so `unit.getStat(uid, "bulk")` never
      --   lazy-rolls a fresh value disconnected from the body
      --   composition the unit was actually spawned with.
    , udSkillTemplates ∷ !(HM.HashMap Text (Float, Float))
      -- ^ skill name → (base, range). Always eager-rolled at spawn.
      --   Skills are continuous floats; XP is applied via a closed-
      --   form formula (see Unit.Stats.applySkillXP) — there's no
      --   per-level threshold to store.
    , udKnowledgeTemplates ∷ !(HM.HashMap Text (Float, Float))
      -- ^ knowledge name → (base, range), rolled at spawn into uiKnowledge.
      --   Knowledge is like a skill but GATED on being "known": a unit only
      --   trains a knowledge it possesses (acquired from a book/teacher, or
      --   spawned-known via this template). Presence in uiKnowledge = known;
      --   the float is the level. Effective capability = level × the
      --   relevant stat (e.g. intelligence) at the point of use.
    , udStartingInventory ∷ ![(Text, Maybe Float, Int)]
      -- ^ ItemDef name + optional initial fill + drop priority.
      --   Materialised into `uiInventory` at spawn by looking each
      --   entry up in the ItemManager; unknown names are dropped with
      --   a log warning. Drop priority > 0 marks the item sheddable
      --   by the spawn-time capacity check (highest sheds first);
      --   0 = always granted.
    , udEquipmentClass ∷ !(Maybe Text)
      -- ^ Name of the EquipmentClass this unit type uses (e.g.
      --   "humanoid"). Nothing means the unit has no equipment UI
      --   at all (e.g. a wandering animal). Resolved against the
      --   EquipmentClassManager at UI-build time, not at spawn.
    , udStartingEquipment ∷ !(HM.HashMap Text Text)
      -- ^ slot id → item def name. Materialised into `uiEquipment`
      --   at spawn, validating each item's `idKind` against the
      --   slot's `esKind`. Mismatches log a warning and are dropped.
      --   Empty unless the unit type ships pre-equipped (acolytes
      --   start with armor, gauntlets, boots, dagger).
    , udStartingAccessories ∷ ![Text]
      -- ^ Item def names to materialise into `uiAccessories` at
      --   spawn — robes, goggles, etc. Order preserved.
    , udBodyParts ∷ ![BodyPart]
      -- ^ Body parts targetable by combat. Order is preserved
      --   (the YAML's order — typically vital → limbs).
      --   Empty list = no combat targeting yet; resolver bails
      --   cleanly. Acolyte ships 12-part humanoid, bear 8-part
      --   quadruped.
    , udNaturalResistance ∷ !NaturalResistance
      -- ^ Innate hide/skin resistance per attack kind. Defaults to
      --   zero everywhere (defaultNaturalResistance) for units that
      --   don't declare a `natural_resistance:` block in YAML.
    , udNaturalWeapon ∷ !(Maybe NaturalWeapon)
      -- ^ Optional innate weapon (claws/fangs/fists). When present,
      --   combat code uses it whenever no equipped weapon is found.
      --   Acolytes have Nothing here (they're expected to fight with
      --   their equipped dagger); bears declare an "unarmed" natural
      --   weapon.
    , udModifiers ∷ ![(Text, StatModifier)]
      -- ^ Permanent (stat name, modifier) pairs seeded into
      --   `uiModifiers` at spawn — the species' innate buffs
      --   (technomule: carrying_capacity +50% "cybernetic
      --   enhancements"). Show in tooltips like any other modifier.
    } deriving (Show, Eq)
