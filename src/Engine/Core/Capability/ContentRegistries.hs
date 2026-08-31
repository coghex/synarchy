-- | The @content-registries@ capability record of the @EngineEnv@
--   capability split (epic #537, issue #890 — E2, the first real
--   consumer migration after #889's E1 convention): the eight
--   YAML-backed content registries
--   'docs/engineenv_capability_inventory.md' SS5's @content-registries@
--   table groups.
--
--   Follows the capability-record convention
--   ('docs/engineenv_capability_inventory.md' SS2.1 is its one
--   authoritative statement, not restated here); this record's own
--   field prefix is @cr@.
--
--   __This is the raw WRITER interface, and since #1896 it is no longer
--   the only one.__ This record carries the registries' __write__ path:
--   the @engine.load*Yaml@ \/ @item.loadYaml@ \/ @equipment.loadYaml@
--   populators mutate through the same handles they read. Those verbs
--   stay publicly callable at any time and keep their existing
--   insert\/replace semantics (see
--   "Engine.Scripting.Lua.API.Register.Engine") — SS5's "populated once
--   at boot" is the normal startup pattern, not an enforced one-shot
--   invariant, so nothing here freezes a registry.
--
--   For FOUR of the eight registries — items, equipment classes,
--   substances and recipes — that write authority is now narrowed to
--   the module that legitimately holds it, and every other consumer
--   takes "Engine.Core.Capability.ContentRegistriesView" instead, where
--   those four arrive as 'Engine.Core.ReadOnlyRef.ReadOnlyRef's (issue
--   #1896, CMA-2 of epic #1890). So the earlier blanket claim that
--   every reader AND writer in this group reaches these fields through
--   THIS record no longer holds: it is true of the other four
--   registries (infection, locations, loot tables, tutorials — outside
--   the pilot), and of the four selected registries' writers alone.
--
--   The modules that keep this record for a SELECTED field are exactly
--   its four raw writers: 'Engine.Scripting.Lua.API.Items.Defs'
--   (@item.loadYaml@, writing through its @registerItemDefs@ helper),
--   'Engine.Scripting.Lua.API.Equipment.Class'
--   (@equipment.loadYaml@), 'Engine.Scripting.Lua.API.Craft.Recipe'
--   (@engine.loadRecipeYaml@) and
--   'Engine.Scripting.Lua.API.Substance' (@substance.loadYaml@). They
--   obtain it the way they always have — through this projection, wired
--   at their @Engine.Scripting.Lua.API.Register.*@ call sites — never
--   through a fresh raw 'EngineEnv' accessor import.
--
--   Both records project independently and totally from 'EngineEnv' and
--   alias the SAME live containers, so a populator's write is visible
--   through every reader's wrapped handle the instant it lands.
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
import Tutorial.Types (TutorialRegistry)
import Engine.Core.State
  ( EngineEnv
  , itemManagerRef, equipmentClassManagerRef, substanceManagerRef
  , infectionManagerRef, recipeManagerRef, locationDefsRef
  , lootTableRegistryRef, tutorialRegistryRef
  )

-- | The @content-registries@ capability: item defs, equipment classes,
--   worked-material substances, infection defs, crafting recipes,
--   location defs, loot tables, and the tutorial definition tree. See
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
  , crTutorialRegistryRef      ∷ IORef TutorialRegistry
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
  , crTutorialRegistryRef      = tutorialRegistryRef env
  }
