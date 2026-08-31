-- | The reader-facing VIEW of the @content-registries@ capability
--   (issue #1896, CMA-2 of the capability mutation-authority epic
--   #1890) — the structural counterpart of
--   "Engine.Core.Capability.ContentRegistries", which stays the raw
--   WRITER interface.
--
--   Both records are projected totally and independently from
--   'EngineEnv' and alias the very same live containers; they differ
--   only in what a holder may DO with the four registries this pilot
--   selects. Here they arrive as 'ReadOnlyRef' handles, so a consumer
--   narrowed to this record cannot mutate them — a compile error, not a
--   review catch — and the restriction survives the handle being passed
--   into a helper or packed into a context record (design decision D-7;
--   'Building.Knowledge.Live.ContainerObserver' is the production
--   pass-on).
--
--   __Which record a module takes is a statement about authority.__ The
--   four raw writers keep "Engine.Core.Capability.ContentRegistries":
--   'Engine.Scripting.Lua.API.Items.Defs' (@item.loadYaml@, writing
--   through @registerItemDefs@), 'Engine.Scripting.Lua.API.Equipment.Class'
--   (@equipment.loadYaml@), 'Engine.Scripting.Lua.API.Craft.Recipe'
--   (@engine.loadRecipeYaml@) and 'Engine.Scripting.Lua.API.Substance'
--   (@substance.loadYaml@). Every other production consumer of those
--   four registries takes THIS record.
--
--   __Why 'crvInfectionManagerRef' is here, raw, and alone.__
--   'Engine.Scripting.Lua.API.Units.Combat' is the one module in the
--   tree that mixes a selected registry with an out-of-scope one: it
--   reads infection defs as well as item defs. Letting it keep the raw
--   'ContentRegistriesCapability' merely to reach infection would hand
--   the raw item handle straight back, defeating the boundary — so the
--   infection handle is supplied here instead, as the ordinary 'IORef'
--   it is. Infection is deliberately NOT inside the structural boundary
--   this pilot draws (see the epic's out-of-scope list); the other three
--   unselected registries appear on neither record's view because no
--   consumer of this one needs them (SS2.1's "no unused capability
--   records ahead of need").
--
--   Follows the capability-record convention
--   (@docs\/engineenv_capability_inventory.md@ SS2.1 is its one
--   authoritative statement, not restated here), including SS2.1's
--   abstract-wrapper extension that this record introduced; the field
--   prefix is @crv@.
--
--   Like "Engine.Core.Capability.ContentRegistries", this module imports
--   only the narrow slice of @Engine.Core.State@ it needs rather than
--   @EngineEnv(..)@ or a bare module import, so it is not itself a
--   full-@EngineEnv@-access consumer under
--   @tools\/engine_env_capability_audit.py@'s ratchet.
module Engine.Core.Capability.ContentRegistriesView
  ( ContentRegistriesViewCapability(..)
  , toContentRegistriesViewCapability
  ) where

-- NB: no UPrelude import — see "Engine.Core.Capability.ContentRegistries".
import Data.IORef (IORef)
import Item.Types (ItemManager)
import Equipment.Types (EquipmentClassManager)
import Substance.Types (SubstanceManager)
import Infection.Types (InfectionManager)
import Craft.Types (RecipeManager)
import Engine.Core.ReadOnlyRef (ReadOnlyRef, toReadOnlyRef)
import Engine.Core.State
  ( EngineEnv
  , itemManagerRef, equipmentClassManagerRef, substanceManagerRef
  , infectionManagerRef, recipeManagerRef
  )

-- | The reader's view of @content-registries@: item defs, equipment
--   classes, worked-material substances and crafting recipes as
--   'ReadOnlyRef's, plus the infection registry as the raw handle it
--   remains (see the module header for why that one is here).
data ContentRegistriesViewCapability = ContentRegistriesViewCapability
  { crvItemManagerRef           ∷ ReadOnlyRef ItemManager
  , crvEquipmentClassManagerRef ∷ ReadOnlyRef EquipmentClassManager
  , crvSubstanceManagerRef      ∷ ReadOnlyRef SubstanceManager
  , crvRecipeManagerRef         ∷ ReadOnlyRef RecipeManager
  , crvInfectionManagerRef      ∷ IORef InfectionManager
  }

-- | Total projection — every field aliases the identical live container
--   'EngineEnv' already carries. Wrapping is not copying: a
--   'ReadOnlyRef' field observes writes made through the raw writer's
--   own handle the instant they happen.
toContentRegistriesViewCapability ∷ EngineEnv → ContentRegistriesViewCapability
toContentRegistriesViewCapability env = ContentRegistriesViewCapability
  { crvItemManagerRef           = toReadOnlyRef (itemManagerRef env)
  , crvEquipmentClassManagerRef = toReadOnlyRef (equipmentClassManagerRef env)
  , crvSubstanceManagerRef      = toReadOnlyRef (substanceManagerRef env)
  , crvRecipeManagerRef         = toReadOnlyRef (recipeManagerRef env)
  , crvInfectionManagerRef      = infectionManagerRef env
  }
