module Engine.Scripting.Lua.API.Register.Item
  ( registerItemAPI
  ) where

import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Blood
import Engine.Scripting.Lua.API.LootTables
import Engine.Scripting.Lua.API.Items
import Engine.Scripting.Lua.API.Forage (itemGetFoodFn)
import Engine.Core.State (EngineEnv, statRNGRef)
import Engine.Core.Capability.ContentRegistries
  (toContentRegistriesCapability)
import qualified HsLua as Lua

-- | Populate and install the @blood@, @loot@, and @item@ global
--   tables.
registerItemAPI ∷ EngineEnv → Lua.LuaE Lua.Exception ()
registerItemAPI env = do
  -- loot.roll and item.listDefs read their catalogues through the
  -- `content-registries` capability (#890); loot.roll additionally needs
  -- the shared stat RNG, which belongs to `units-buildings-combat` and so
  -- is passed as the bare IORef it is.
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
  registerLuaFunction "spawn"          (bloodSpawnFn env)
  registerLuaFunction "getDecal"       (bloodGetDecalFn env)
  registerLuaFunction "listDecals"     (bloodListDecalsFn env)
  registerLuaFunction "getTexture"     (bloodGetTextureFn env)
  registerLuaFunction "listTextures"   (bloodListTexturesFn env)
  registerLuaFunction "getTextureCap"  (bloodGetTextureCapFn env)
  registerLuaFunction "getRenderQuads" (bloodGetRenderQuadsFn env)
  registerLuaFunction "gpuStats"       (bloodGpuStatsFn env)
  registerLuaFunction "clear"          (bloodClearFn env)
  registerLuaFunction "getTrailState"  (bloodGetTrailStateFn env)
  Lua.setglobal (Lua.Name "blood")

  -- Loot table global — weighted rolls against data/loot_tables/*.yaml
  -- (loaded via engine.loadLootTableYaml). Consumed by a `loot_table`
  -- location content entry (#90).
  Lua.newtable
  registerLuaFunction "roll" (lootRollFn regs (statRNGRef env))
  Lua.setglobal (Lua.Name "loot")

  Lua.newtable
  registerLuaFunction "listDefs"     (itemListDefsFn regs)
  registerLuaFunction "spawnGround"  (itemSpawnGroundFn env)
  registerLuaFunction "listGround"   (itemListGroundFn env)
  registerLuaFunction "removeGround" (itemRemoveGroundFn env)
  registerLuaFunction "groundCount"  (itemGroundCountFn env)
  registerLuaFunction "getGroundTemp" (itemGetGroundTempFn env)
  registerLuaFunction "setGroundTemp" (itemSetGroundTempFn env)
  registerLuaFunction "hitTestAt"    (itemHitTestAtFn env)
  registerLuaFunction "select"       (itemSelectFn env)
  registerLuaFunction "deselect"     (itemDeselectFn env)
  registerLuaFunction "getSelected"  (itemGetSelectedFn env)
  registerLuaFunction "pickupGround" (itemPickupGroundFn env)
  registerLuaFunction "getFood"      (itemGetFoodFn env)
  registerLuaFunction "debugQuads"   (itemDebugQuadsFn env)
  Lua.setglobal (Lua.Name "item")
