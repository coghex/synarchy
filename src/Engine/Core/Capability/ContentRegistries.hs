{-# LANGUAGE UnicodeSyntax #-}
-- | The @content-registries@ capability record of the @EngineEnv@
--   capability split (epic #537, issue #890 — E2, the first real
--   consumer migration after #889's E1 convention): the seven
--   YAML-backed content registries
--   'docs/engineenv_capability_inventory.md' SS5's @content-registries@
--   table groups.
--
--   Follows E1's convention verbatim (see
--   "Engine.Core.Capability.Core" for the full statement of it): one
--   record named @\<Name\>Capability@ with fields prefixed by the
--   record's own initials (here @cr@), one total one-way
--   @to\<Name\>Capability@ projection, every field the exact same live
--   'IORef' 'EngineEnv' already carries (never a copy or a
--   reconstruction), and no import of any consumer of this module.
--
--   Unlike a read-only view, this record carries the registries'
--   __write__ path too: the @engine.load*Yaml@ \/ @item.loadYaml@ \/
--   @equipment.loadYaml@ populators go through the same handles the
--   readers do. Those verbs stay publicly callable at any time and keep
--   their existing insert\/replace semantics (see
--   "Engine.Scripting.Lua.API.Register.Engine") — SS5's
--   "populated once at boot" is the normal startup pattern, not an
--   enforced one-shot invariant, so nothing here freezes a registry.
--
--   Like "Engine.Core.Capability.Core", this module imports only the
--   narrow slice of @Engine.Core.State@ it needs (the bare 'EngineEnv'
--   type plus the seven field accessors) rather than @EngineEnv(..)@ or
--   a bare module import, so it is not itself a full-@EngineEnv@-access
--   consumer under @tools/engine_env_capability_audit.py@'s ratchet.
module Engine.Core.Capability.ContentRegistries
  ( ContentRegistriesCapability(..)
  , toContentRegistriesCapability
  ) where

-- NB: no UPrelude import — every name this module mentions is either a
-- registry type imported below or the 'IORef' constructor-less type
-- itself, so importing it would be an unused import under -Werror
-- (unlike "Engine.Core.Capability.Core", whose record has a 'Bool').
import Data.IORef (IORef)
import Item.Types (ItemManager)
import Equipment.Types (EquipmentClassManager)
import Substance.Types (SubstanceManager)
import Infection.Types (InfectionManager)
import Craft.Types (RecipeManager)
import Location.Types (LocationRegistry)
import LootTable.Types (LootTableRegistry)
import Engine.Core.State
  ( EngineEnv
  , itemManagerRef, equipmentClassManagerRef, substanceManagerRef
  , infectionManagerRef, recipeManagerRef, locationDefsRef
  , lootTableRegistryRef
  )

-- | The @content-registries@ capability: item defs, equipment classes,
--   worked-material substances, infection defs, crafting recipes,
--   location defs, and loot tables. See
--   'docs/engineenv_capability_inventory.md' SS5 @content-registries@
--   and SS7.6.
data ContentRegistriesCapability = ContentRegistriesCapability
  { crItemManagerRef           ∷ IORef ItemManager
  , crEquipmentClassManagerRef ∷ IORef EquipmentClassManager
  , crSubstanceManagerRef      ∷ IORef SubstanceManager
  , crInfectionManagerRef      ∷ IORef InfectionManager
  , crRecipeManagerRef         ∷ IORef RecipeManager
  , crLocationDefsRef          ∷ IORef LocationRegistry
  , crLootTableRegistryRef     ∷ IORef LootTableRegistry
  }

-- | Total projection — every field aliases the identical live
--   container 'EngineEnv' already carries; nothing is copied.
toContentRegistriesCapability ∷ EngineEnv → ContentRegistriesCapability
toContentRegistriesCapability env = ContentRegistriesCapability
  { crItemManagerRef           = itemManagerRef env
  , crEquipmentClassManagerRef = equipmentClassManagerRef env
  , crSubstanceManagerRef      = substanceManagerRef env
  , crInfectionManagerRef      = infectionManagerRef env
  , crRecipeManagerRef         = recipeManagerRef env
  , crLocationDefsRef          = locationDefsRef env
  , crLootTableRegistryRef     = lootTableRegistryRef env
  }
