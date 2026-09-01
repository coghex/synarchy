module Engine.Scripting.Lua.API.Register.Designation
  ( registerDesignationAPI
  ) where

import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.Structure
import Engine.Scripting.Lua.API.StructureArt
import Engine.Scripting.Lua.API.Construct
import Engine.Scripting.Lua.API.Construct.Payment (constructPayMaterialsFn)
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
  registerLuaFunction "stageWatermark" (structureStageWatermarkFn env)
  registerLuaFunction "clear"    (structureClearFn env)
  registerLuaFunction "clearAll" (structureClearAllFn env)
  registerLuaFunction "count"    (structureCountFn env)
  registerLuaFunction "loadedCount" (structureLoadedCountFn env)
  registerLuaFunction "unresolvedPaletteIds" (structureUnresolvedPaletteIdsFn env)
  registerLuaFunction "setPaletteHandle" (structureSetPaletteHandleFn env)
  registerLuaFunction "paletteCount" (structurePaletteCountFn env)
  registerLuaFunction "registerWallFamily" (structureRegisterWallFamilyFn env)
  registerLuaFunction "registerPackArt" (structureRegisterPackArtFn env)
  registerLuaFunction "isPackKindBuildable" (structurePackKindBuildableFn env)
  registerLuaFunction "packBuildCost"      (structurePackBuildCostFn env)
  registerLuaFunction "resolvePieceArt" (structureResolvePieceArtFn env)
  registerLuaFunction "wireShape"    structureWireShapeFn
  registerLuaFunction "wireNeighbors" (structureWireNeighborsFn env)
  registerLuaFunction "floorZAt" (structureFloorZAtFn env)
  registerLuaFunction "hasAt"    (structureHasAtFn env)
  registerLuaFunction "getAt"    (structureGetAtFn env)
  Lua.setglobal (Lua.Name "structure")

  -- Construction designation tool (#95). Mirrors the mine-designation
  -- API: the tool drives setAnchor/clearAnchor/designate, the build AI
  -- (#96) drives getPendingJobs/nearestDesignation/setJobStatus.
  Lua.newtable
  registerLuaFunction "setAnchor"          (constructSetAnchorFn (toWorldSimCapability env))
  registerLuaFunction "clearAnchor"        (constructClearAnchorFn (toWorldSimCapability env))
  registerLuaFunction "designate"          (constructDesignateFn (toWorldSimCapability env))
  registerLuaFunction "cancelDesignation"  (constructCancelDesignationFn (toWorldSimCapability env))
  registerLuaFunction "cancelDesignationForRefund" (constructCancelDesignationForRefundFn (toWorldSimCapability env))
  registerLuaFunction "getPendingJobs"     (constructGetPendingJobsFn (toWorldSimCapability env))
  registerLuaFunction "getDesignationAt"   (constructGetDesignationAtFn (toWorldSimCapability env))
  registerLuaFunction "getDesignationCount" (constructGetDesignationCountFn (toWorldSimCapability env))
  registerLuaFunction "nearestDesignation" (constructNearestDesignationFn (toWorldSimCapability env))
  registerLuaFunction "setJobStatus"       (constructSetJobStatusFn (toWorldSimCapability env))
  registerLuaFunction "addJobProgress"     (constructAddJobProgressFn (toWorldSimCapability env))
  registerLuaFunction "payMaterials"       (constructPayMaterialsFn env)
  registerLuaFunction "beginPlacement"     (constructBeginPlacementFn (toWorldSimCapability env))
  registerLuaFunction "abortPlacement"     (constructAbortPlacementFn (toWorldSimCapability env))
  registerLuaFunction "resolvePlan"        (constructResolvePlanFn env)
  registerLuaFunction "setDesignateTexture" (constructSetDesignateTextureFn (toWorldSimCapability env))
  registerLuaFunction "setLineMode"        (constructSetLineModeFn (toWorldSimCapability env))
  Lua.setglobal (Lua.Name "construction")

  -- Chop designation tool (#97). Mirrors the construction-designation
  -- API: the tool drives setAnchor/clearAnchor/designate, the chop AI
  -- drives nearestDesignation/getDesignationAt/cancelDesignation
  -- (claims are Lua-side, like dig jobs — no engine job status).
  Lua.newtable
  registerLuaFunction "setAnchor"           (chopSetAnchorFn (toWorldSimCapability env))
  registerLuaFunction "clearAnchor"         (chopClearAnchorFn (toWorldSimCapability env))
  registerLuaFunction "designate"           (chopDesignateFn (toWorldSimCapability env))
  registerLuaFunction "cancelDesignation"   (chopCancelDesignationFn (toWorldSimCapability env))
  registerLuaFunction "getDesignationAt"    (chopGetDesignationAtFn (toWorldSimCapability env))
  registerLuaFunction "getDesignationsAt"
      (chopGetDesignationsAtFn (toWorldSimCapability env))
  registerLuaFunction "getDesignationForInstance"
      (chopGetDesignationForInstanceFn (toWorldSimCapability env))
  registerLuaFunction "getDesignationCount" (chopGetDesignationCountFn (toWorldSimCapability env))
  registerLuaFunction "nearestDesignation"  (chopNearestDesignationFn (toWorldSimCapability env))
  registerLuaFunction "setDesignateTexture" (chopSetDesignateTextureFn (toWorldSimCapability env))
  Lua.setglobal (Lua.Name "chop")

  -- Till designation tool (#333). Mirrors the chop-designation API:
  -- the tool drives setAnchor/clearAnchor/designate, the till AI drives
  -- nearestDesignation/getDesignationAt/cancelDesignation (claims are
  -- Lua-side, like dig/chop jobs — no engine job status).
  Lua.newtable
  registerLuaFunction "setAnchor"           (tillSetAnchorFn (toWorldSimCapability env))
  registerLuaFunction "clearAnchor"         (tillClearAnchorFn (toWorldSimCapability env))
  registerLuaFunction "designate"           (tillDesignateFn (toWorldSimCapability env))
  registerLuaFunction "cancelDesignation"   (tillCancelDesignationFn (toWorldSimCapability env))
  registerLuaFunction "getDesignationAt"    (tillGetDesignationAtFn (toWorldSimCapability env))
  registerLuaFunction "getDesignationCount" (tillGetDesignationCountFn (toWorldSimCapability env))
  registerLuaFunction "nearestDesignation"  (tillNearestDesignationFn (toWorldSimCapability env))
  registerLuaFunction "setDesignateTexture" (tillSetDesignateTextureFn (toWorldSimCapability env))
  Lua.setglobal (Lua.Name "till")

  -- Plant designation tool (#335). Single-tile, no anchor: the tool
  -- drives designate, the farm AI (#336) drives nearestDesignation/
  -- getDesignationAt/cancelDesignation (claims are Lua-side, like dig/
  -- chop/till jobs — no engine job status).
  Lua.newtable
  registerLuaFunction "designate"           (plantDesignateFn (toWorldSimCapability env))
  registerLuaFunction "cancelDesignation"   (plantCancelDesignationFn (toWorldSimCapability env))
  registerLuaFunction "getDesignationAt"    (plantGetDesignationAtFn (toWorldSimCapability env))
  registerLuaFunction "getDesignationCount" (plantGetDesignationCountFn (toWorldSimCapability env))
  registerLuaFunction "nearestDesignation"  (plantNearestDesignationFn (toWorldSimCapability env))
  registerLuaFunction "setDesignateTexture" (plantSetDesignateTextureFn (toWorldSimCapability env))
  Lua.setglobal (Lua.Name "plant")
