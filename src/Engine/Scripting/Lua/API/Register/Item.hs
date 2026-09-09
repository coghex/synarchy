module Engine.Scripting.Lua.API.Register.Item
  ( registerItemAPI
  ) where

import Engine.Scripting.Lua.CallStats (LuaCallStats)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Blood
import Engine.Scripting.Lua.API.LootTables
import Engine.Scripting.Lua.API.LootProfiles
import Engine.Scripting.Lua.API.Items
import Engine.Scripting.Lua.API.Forage (itemGetFoodFn)
import Engine.Scripting.Lua.API.Items.Knowledge
  ( itemGetContainerKnowledgeFn, itemObserveContainerWeightFn
  , itemObserveContainerContentsFn, itemForgetContainerKnowledgeFn )
import Engine.Core.State (EngineEnv, statRNGRef)
import Engine.Core.Capability.ContentRegistries
  (toContentRegistriesCapability)
import qualified HsLua as Lua

-- | Populate and install the @blood@, @loot@, and @item@ global
--   tables.
registerItemAPI ∷ LuaCallStats → EngineEnv → Lua.LuaE Lua.Exception ()
registerItemAPI callStats env = do
  -- loot.roll and item.listDefs read their catalogues through the
  -- `content-registries` capability (#890); loot.roll additionally needs
  -- the shared stat RNG, which belongs to `units-buildings-combat` and so
  -- is passed as the bare IORef it is. loot.rollFor (#948) is the
  -- seed-stable draw and is registry-only — it takes no RNG handle.
  let regs = toContentRegistriesCapability env
  -- Blood global (#604 + #606) — the world-scoped blood decal model +
  -- debug surface: spawn a decal (reusing a near-matching generated-
  -- texture descriptor or minting + FIFO-evicting a new one), list
  -- current decals / texture descriptors (each now reporting its
  -- generated pixel data — Blood.Texture), query resolved per-decal
  -- render records (Blood.Render — same data World.Render.BloodQuads
  -- turns into world-space quads), and clear both (see Blood.Types +
  -- docs/blood_decals.md).
  Lua.newtable
  registerLuaFunction callStats "blood" "spawn"          (bloodSpawnFn env)
  registerLuaFunction callStats "blood" "getDecal"       (bloodGetDecalFn env)
  registerLuaFunction callStats "blood" "listDecals"     (bloodListDecalsFn env)
  registerLuaFunction callStats "blood" "getTexture"     (bloodGetTextureFn env)
  registerLuaFunction callStats "blood" "listTextures"   (bloodListTexturesFn env)
  registerLuaFunction callStats "blood" "getTextureCap"  (bloodGetTextureCapFn env)
  registerLuaFunction callStats "blood" "getRenderQuads" (bloodGetRenderQuadsFn env)
  registerLuaFunction callStats "blood" "gpuStats"       (bloodGpuStatsFn env)
  registerLuaFunction callStats "blood" "gpuHandles"     (bloodGpuHandlesFn env)
  registerLuaFunction callStats "blood" "clear"          (bloodClearFn env)
  registerLuaFunction callStats "blood" "getTrailState"  (bloodGetTrailStateFn env)
  Lua.setglobal (Lua.Name "blood")

  -- Loot table global — weighted rolls against data/loot_tables/*.yaml
  -- (loaded via engine.loadLootTableYaml). A `loot_table` location
  -- content entry (#90) rolls through `rollFor`, whose result is a pure
  -- function of the world seed + placed-instance id + content-entry
  -- index + roll index (#948); `roll` is the uncontextual shared-RNG
  -- draw kept for ad-hoc console/test callers.
  Lua.newtable
  registerLuaFunction callStats "loot" "roll"    (lootRollFn regs (statRNGRef env))
  registerLuaFunction callStats "loot" "rollFor" (lootRollForFn regs)
  -- Loot PROFILES (#2499) share this namespace by D-20 and are
  -- read-only: `profile` answers one def as a fresh table, `listProfiles`
  -- the sorted ids. A profile is not a table and is not rolled here —
  -- realization is PLC-13's.
  registerLuaFunction callStats "loot" "profile"      (lootProfileFn regs)
  registerLuaFunction callStats "loot" "listProfiles" (lootListProfilesFn regs)
  Lua.setglobal (Lua.Name "loot")

  Lua.newtable
  registerLuaFunction callStats "item" "listDefs"     (itemListDefsFn regs)
  registerLuaFunction callStats "item" "spawnGround"  (itemSpawnGroundFn env)
  registerLuaFunction callStats "item" "listGround"   (itemListGroundFn env)
  registerLuaFunction callStats "item" "removeGround" (itemRemoveGroundFn env)
  registerLuaFunction callStats "item" "groundCount"  (itemGroundCountFn env)
  registerLuaFunction callStats "item" "getGroundTemp" (itemGetGroundTempFn env)
  registerLuaFunction callStats "item" "setGroundTemp" (itemSetGroundTempFn env)
  registerLuaFunction callStats "item" "hitTestAt"    (itemHitTestAtFn env)
  registerLuaFunction callStats "item" "select"       (itemSelectFn env)
  registerLuaFunction callStats "item" "deselect"     (itemDeselectFn env)
  registerLuaFunction callStats "item" "getSelected"  (itemGetSelectedFn env)
  registerLuaFunction callStats "item" "pickupGround" (itemPickupGroundFn env)
  registerLuaFunction callStats "item" "getGroundForUnit" (itemGetGroundForUnitFn env)
  registerLuaFunction callStats "item" "getFood"      (itemGetFoodFn env)
  registerLuaFunction callStats "item" "debugQuads"   (itemDebugQuadsFn env)
  -- PORTABLE container knowledge (#2512) — what the player remembers
  -- about a crate, keyed by its own instance id and carried across
  -- pages and owners with it. The read verb answers the same field
  -- names `building.getContainerKnowledge` does, so one window renders
  -- either; the two observe verbs are what PLC-8's pickup and open will
  -- call, and nothing in the shipped game calls them yet. See
  -- Engine.Scripting.Lua.API.Items.Knowledge.
  registerLuaFunction callStats "item" "getContainerKnowledge"
                                     (itemGetContainerKnowledgeFn env)
  registerLuaFunction callStats "item" "observeContainerWeight"
                                     (itemObserveContainerWeightFn env)
  registerLuaFunction callStats "item" "observeContainerContents"
                                     (itemObserveContainerContentsFn env)
  registerLuaFunction callStats "item" "forgetContainerKnowledge"
                                     (itemForgetContainerKnowledgeFn env)
  Lua.setglobal (Lua.Name "item")
