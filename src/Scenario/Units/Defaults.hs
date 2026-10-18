{-# LANGUAGE Strict #-}
-- | Deterministic fallback values for scenario units (#2710, SCN-10 of
--   epic #2698; design D-5).
--
--   Every value a unit entry OMITS comes from the unit's definition
--   through a private generator seeded by the unit's stable scenario
--   identity and the value's DOMAIN — never by the gameplay stat RNG, an
--   allocated unit or item id, the tags, the unit's position in the
--   file's unit order, or the order things are built in. So the same
--   placed unit with unchanged definitions gets the same values in every
--   process, and stating one value does not move another (each domain
--   has its own generator).
--
--   The seed is FNV-1a (64-bit, 'Scenario.Items.Refs.stableHash64') over
--   the UTF-8 bytes of @"synarchy.scenario.unit/v1" 0x00 reference 0x00
--   domain@, the item adapters' scheme ("Scenario.Items.Build") with its
--   own namespace. The domains are @stat/<name>@, @body/<name>@,
--   @skill/<name>@, @knowledge/<name>@, @name@, and
--   @<collection>/<slot>/<roll>@ for the definition's starting loadout.
--   The encoding is fixed: changing it changes every scenario's omitted
--   values, which is a format version.
--
--   The draws themselves are the definition's own: 'Unit.Stats.rollStat'
--   over each template's base and range, 'Unit.Stats.pickName' over its
--   name pool, and "Item.Materialize" for its starting kit.
module Scenario.Units.Defaults
    ( scenarioUnitSeed
    , scenarioUnitGen
    , rollTemplate
    , rollTemplatesFor
    , rollName
      -- * The definition's starting loadout
    , DefaultLoadout(..)
    , defaultLoadoutSpecs
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import System.Random (StdGen, mkStdGen)
import Equipment.Types (EquipmentClass)
import Item.Materialize (ItemSpec(..), itemSpec, filledItem)
import Item.Types (ItemManager)
import Unit.Thread.Command.Spawn (startingEquipmentCheck)
import Scenario.Items.Refs (stableHash64)
import Unit.Stats (rollStat, pickName)
import Unit.Types (UnitDef(..))

-- | The seed of one domain of one unit.
scenarioUnitSeed ∷ Text → Text → Word64
scenarioUnitSeed ref domain =
    stableHash64 ("synarchy.scenario.unit/v1\0" <> ref <> "\0" <> domain)

scenarioUnitGen ∷ Text → Text → StdGen
scenarioUnitGen ref domain = mkStdGen (fromIntegral (scenarioUnitSeed ref domain))

-- | One template's value for a unit: one 'rollStat' draw from the
--   domain's own generator.
rollTemplate ∷ Text → Text → (Float, Float) → Float
rollTemplate ref domain (base, range) = fst (rollStat base range (scenarioUnitGen ref domain))

-- | Every template of a map under @prefix/<name>@.
rollTemplatesFor ∷ Text → Text → HM.HashMap Text (Float, Float) → HM.HashMap Text Float
rollTemplatesFor ref prefix =
    HM.mapWithKey (\k t → rollTemplate ref (prefix <> "/" <> k) t)

-- | The name drawn from the definition's pool; @""@ for a definition
--   with none (animals stay unnamed, as at spawn).
rollName ∷ Text → UnitDef → Text
rollName ref d = case udNamePool d of
    Nothing → ""
    Just pool → fst (pickName pool (scenarioUnitGen ref "name"))

-- | The construction trees of a definition's starting loadout, for the
--   collections a unit entry omits. Kept apart by collection so an
--   explicit collection replaces exactly its own default (D-12).
data DefaultLoadout = DefaultLoadout
    { dlInventory   ∷ ![ItemSpec]
    , dlEquipment   ∷ ![(Text, ItemSpec)]   -- ^ by sorted slot
    , dlAccessories ∷ ![ItemSpec]
    }

-- | The definition's starting inventory, equipment and accessories as
--   explicit construction trees, every roll private to the unit
--   (@<collection>/<slot>/<roll>@). Starting equipment is judged by the
--   spawn rule itself ('Unit.Thread.Command.Spawn.startingEquipmentCheck'):
--   an entry whose slot the class does not declare, or whose item's kind
--   the slot does not accept, is not part of the kit. Nothing is shed
--   for capacity: that is ordinary spawning's rule, not a scenario
--   default (the trusted review's capacity addition).
defaultLoadoutSpecs ∷ ItemManager → Maybe EquipmentClass → Text → UnitDef → DefaultLoadout
defaultLoadoutSpecs items mClass ref d = DefaultLoadout
    { dlInventory =
        [ (spec ("inventory/" <> tshow i) name) { spOverrides = filledItem mFill }
        | (i, (name, mFill, _prio)) ← zip [0 ∷ Int ..] (udStartingInventory d) ]
    , dlEquipment =
        [ (slot, spec ("equipment/" <> slot) name)
        | Just cls ← [mClass]
        , (slot, name) ← sortOn fst (HM.toList (udStartingEquipment d))
        , Right () ← [startingEquipmentCheck items cls slot name] ]
    , dlAccessories =
        [ spec ("accessories/" <> tshow i) name
        | (i, name) ← zip [0 ∷ Int ..] (udStartingAccessories d) ]
    }
  where
    spec slot name = (itemSpec name)
        { spPath = slot
        , spRolls = Just (\roll → scenarioUnitGen ref (slot <> "/" <> roll)) }
