module Engine.Scripting.Lua.API.Register.Designation
  ( registerDesignationAPI
  ) where

import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Structure
import Engine.Scripting.Lua.API.Construct
import Engine.Scripting.Lua.API.Chop
import Engine.Scripting.Lua.API.Till
import Engine.Scripting.Lua.API.Plant
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

-- | Populate and install the @structure@, @construction@, @chop@,
--   @till@, and @plant@ global tables — the debug structure builder
--   plus the designation-tool families that mirror its shape (mine
--   designation lives on @world@; each of these drives
--   setAnchor/clearAnchor/designate from its tool, and
--   nearestDesignation/getDesignationAt/cancelDesignation from its
--   AI, except @plant@ which is single-tile and has no anchor).
registerDesignationAPI ∷ EngineEnv → Lua.LuaE Lua.Exception ()
registerDesignationAPI env = do
  -- Structure global — debug builder for walls / floors / ceilings.
  -- structure.place(gx,gy,slot,texHandle,faceHandle[,z]) / clear / clearAll / count.
  Lua.newtable
  registerLuaFunction "place"    (structurePlaceFn env)
  registerLuaFunction "clear"    (structureClearFn env)
  registerLuaFunction "clearAll" (structureClearAllFn env)
  registerLuaFunction "count"    (structureCountFn env)
  registerLuaFunction "loadedCount" (structureLoadedCountFn env)
  registerLuaFunction "unresolvedPaletteIds" (structureUnresolvedPaletteIdsFn env)
  registerLuaFunction "setPaletteHandle" (structureSetPaletteHandleFn env)
  registerLuaFunction "paletteCount" (structurePaletteCountFn env)
  registerLuaFunction "floorZAt" (structureFloorZAtFn env)
  registerLuaFunction "hasAt"    (structureHasAtFn env)
  registerLuaFunction "getAt"    (structureGetAtFn env)
  Lua.setglobal (Lua.Name "structure")

  -- Construction designation tool (#95). Mirrors the mine-designation
  -- API: the tool drives setAnchor/clearAnchor/designate, the build AI
  -- (#96) drives getPendingJobs/nearestDesignation/setJobStatus.
  Lua.newtable
  registerLuaFunction "setAnchor"          (constructSetAnchorFn env)
  registerLuaFunction "clearAnchor"        (constructClearAnchorFn env)
  registerLuaFunction "designate"          (constructDesignateFn env)
  registerLuaFunction "cancelDesignation"  (constructCancelDesignationFn env)
  registerLuaFunction "getPendingJobs"     (constructGetPendingJobsFn env)
  registerLuaFunction "getDesignationAt"   (constructGetDesignationAtFn env)
  registerLuaFunction "getDesignationCount" (constructGetDesignationCountFn env)
  registerLuaFunction "nearestDesignation" (constructNearestDesignationFn env)
  registerLuaFunction "setJobStatus"       (constructSetJobStatusFn env)
  registerLuaFunction "addJobProgress"     (constructAddJobProgressFn env)
  registerLuaFunction "setMaterialsPaid"   (constructSetMaterialsPaidFn env)
  registerLuaFunction "setDesignateTexture" (constructSetDesignateTextureFn env)
  registerLuaFunction "setLineMode"        (constructSetLineModeFn env)
  Lua.setglobal (Lua.Name "construction")

  -- Chop designation tool (#97). Mirrors the construction-designation
  -- API: the tool drives setAnchor/clearAnchor/designate, the chop AI
  -- drives nearestDesignation/getDesignationAt/cancelDesignation
  -- (claims are Lua-side, like dig jobs — no engine job status).
  Lua.newtable
  registerLuaFunction "setAnchor"           (chopSetAnchorFn env)
  registerLuaFunction "clearAnchor"         (chopClearAnchorFn env)
  registerLuaFunction "designate"           (chopDesignateFn env)
  registerLuaFunction "cancelDesignation"   (chopCancelDesignationFn env)
  registerLuaFunction "getDesignationAt"    (chopGetDesignationAtFn env)
  registerLuaFunction "getDesignationCount" (chopGetDesignationCountFn env)
  registerLuaFunction "nearestDesignation"  (chopNearestDesignationFn env)
  registerLuaFunction "setDesignateTexture" (chopSetDesignateTextureFn env)
  Lua.setglobal (Lua.Name "chop")

  -- Till designation tool (#333). Mirrors the chop-designation API:
  -- the tool drives setAnchor/clearAnchor/designate, the till AI drives
  -- nearestDesignation/getDesignationAt/cancelDesignation (claims are
  -- Lua-side, like dig/chop jobs — no engine job status).
  Lua.newtable
  registerLuaFunction "setAnchor"           (tillSetAnchorFn env)
  registerLuaFunction "clearAnchor"         (tillClearAnchorFn env)
  registerLuaFunction "designate"           (tillDesignateFn env)
  registerLuaFunction "cancelDesignation"   (tillCancelDesignationFn env)
  registerLuaFunction "getDesignationAt"    (tillGetDesignationAtFn env)
  registerLuaFunction "getDesignationCount" (tillGetDesignationCountFn env)
  registerLuaFunction "nearestDesignation"  (tillNearestDesignationFn env)
  registerLuaFunction "setDesignateTexture" (tillSetDesignateTextureFn env)
  Lua.setglobal (Lua.Name "till")

  -- Plant designation tool (#335). Single-tile, no anchor: the tool
  -- drives designate, the farm AI (#336) drives nearestDesignation/
  -- getDesignationAt/cancelDesignation (claims are Lua-side, like dig/
  -- chop/till jobs — no engine job status).
  Lua.newtable
  registerLuaFunction "designate"           (plantDesignateFn env)
  registerLuaFunction "cancelDesignation"   (plantCancelDesignationFn env)
  registerLuaFunction "getDesignationAt"    (plantGetDesignationAtFn env)
  registerLuaFunction "getDesignationCount" (plantGetDesignationCountFn env)
  registerLuaFunction "nearestDesignation"  (plantNearestDesignationFn env)
  registerLuaFunction "setDesignateTexture" (plantSetDesignateTextureFn env)
  Lua.setglobal (Lua.Name "plant")
