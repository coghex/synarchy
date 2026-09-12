module Engine.Scripting.Lua.API.Register.Designation
  ( registerDesignationAPI
  ) where

import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Scripting.Lua.CallStats (LuaCallStats)
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
registerDesignationAPI ∷ LuaCallStats → EngineEnv → Lua.LuaE Lua.Exception ()
registerDesignationAPI callStats env = do
  -- Structure global — debug builder for walls / floors / ceilings.
  -- structure.place(gx,gy,slot,texHandle,faceHandle[,z]) / clear / clearAll / count.
  Lua.newtable
  registerLuaFunction callStats "structure" "place"    (structurePlaceFn env)
  registerLuaFunction callStats "structure" "stageWatermark" (structureStageWatermarkFn env)
  registerLuaFunction callStats "structure" "clear"    (structureClearFn env)
  registerLuaFunction callStats "structure" "clearAll" (structureClearAllFn env)
  registerLuaFunction callStats "structure" "count"    (structureCountFn env)
  registerLuaFunction callStats "structure" "loadedCount" (structureLoadedCountFn env)
  registerLuaFunction callStats "structure" "unresolvedPaletteIds" (structureUnresolvedPaletteIdsFn env)
  registerLuaFunction callStats "structure" "setPaletteHandle" (structureSetPaletteHandleFn env)
  registerLuaFunction callStats "structure" "paletteCount" (structurePaletteCountFn env)
  registerLuaFunction callStats "structure" "registerWallFamily" (structureRegisterWallFamilyFn env)
  registerLuaFunction callStats "structure" "registerPackArt" (structureRegisterPackArtFn env)
  registerLuaFunction callStats "structure" "isPackKindBuildable" (structurePackKindBuildableFn env)
  registerLuaFunction callStats "structure" "packBuildCost"      (structurePackBuildCostFn env)
  registerLuaFunction callStats "structure" "isSafeArtPath" structureIsSafeArtPathFn
  registerLuaFunction callStats "structure" "resolvePieceArt" (structureResolvePieceArtFn env)
  registerLuaFunction callStats "structure" "wireShape"    structureWireShapeFn
  registerLuaFunction callStats "structure" "wireNeighbors" (structureWireNeighborsFn env)
  registerLuaFunction callStats "structure" "floorZAt" (structureFloorZAtFn env)
  registerLuaFunction callStats "structure" "hasAt"    (structureHasAtFn env)
  registerLuaFunction callStats "structure" "getAt"    (structureGetAtFn env)
  Lua.setglobal (Lua.Name "structure")

  -- Construction designation tool (#95). Mirrors the mine-designation
  -- API: the tool drives setAnchor/clearAnchor/designate, the build AI
  -- (#96) drives getPendingJobs/nearestDesignation/setJobStatus.
  Lua.newtable
  registerLuaFunction callStats "construction" "setAnchor"          (constructSetAnchorFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "clearAnchor"        (constructClearAnchorFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "designate"          (constructDesignateFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "cancelDesignation"  (constructCancelDesignationFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "cancelDesignationForRefund" (constructCancelDesignationForRefundFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "getPendingJobs"     (constructGetPendingJobsFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "getDesignationAt"   (constructGetDesignationAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "getDesignationCount" (constructGetDesignationCountFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "nearestDesignation" (constructNearestDesignationFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "setJobStatus"       (constructSetJobStatusFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "addJobProgress"     (constructAddJobProgressFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "payMaterials"       (constructPayMaterialsFn env)
  registerLuaFunction callStats "construction" "beginPlacement"     (constructBeginPlacementFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "abortPlacement"     (constructAbortPlacementFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "resolvePlan"        (constructResolvePlanFn env)
  registerLuaFunction callStats "construction" "setLineMode"        (constructSetLineModeFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "setStructureTarget" (constructSetStructureTargetFn (toWorldSimCapability env))
  registerLuaFunction callStats "construction" "clearStructureTarget" (constructClearStructureTargetFn (toWorldSimCapability env))
  Lua.setglobal (Lua.Name "construction")

  -- Chop designation tool (#97, re-shaped by #1856). The tool drives
  -- the four screen-space gesture verbs — designateAt/designateInRect
  -- add, eraseAt/eraseInRect erase — over the shared selection oracle;
  -- designateInstances/eraseInstances are the exact-identity authority
  -- beneath them, for callers that already hold ids. The chop AI drives
  -- nearestFreeDesignation/getDesignationAt/cancelDesignation (claims
  -- are Lua-side, like dig jobs — no engine job status, which is why
  -- the AI's selector is the claim-EXCLUDING query, #2536).
  Lua.newtable
  registerLuaFunction callStats "chop" "designateAt"         (chopDesignateAtFn env)
  registerLuaFunction callStats "chop" "designateInRect"     (chopDesignateInRectFn env)
  registerLuaFunction callStats "chop" "eraseAt"             (chopEraseAtFn env)
  registerLuaFunction callStats "chop" "eraseInRect"         (chopEraseInRectFn env)
  registerLuaFunction callStats "chop" "designateInstances"
      (chopDesignateInstancesFn (toWorldSimCapability env))
  registerLuaFunction callStats "chop" "eraseInstances"
      (chopEraseInstancesFn (toWorldSimCapability env))
  registerLuaFunction callStats "chop" "cancelDesignation"   (chopCancelDesignationFn (toWorldSimCapability env))
  registerLuaFunction callStats "chop" "getDesignationAt"    (chopGetDesignationAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "chop" "getDesignationsAt"
      (chopGetDesignationsAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "chop" "getDesignationForInstance"
      (chopGetDesignationForInstanceFn (toWorldSimCapability env))
  registerLuaFunction callStats "chop" "getDesignationCount" (chopGetDesignationCountFn (toWorldSimCapability env))
  registerLuaFunction callStats "chop" "nearestDesignation"  (chopNearestDesignationFn (toWorldSimCapability env))
  registerLuaFunction callStats "chop" "nearestFreeDesignation"
      (chopNearestFreeDesignationFn (toWorldSimCapability env))
  registerLuaFunction callStats "chop" "setDesignateTexture" (chopSetDesignateTextureFn (toWorldSimCapability env))
  Lua.setglobal (Lua.Name "chop")

  -- Till designation tool (#333). Mirrors the chop-designation API:
  -- the tool drives setAnchor/clearAnchor/designate, the till AI drives
  -- nearestFreeDesignation/getDesignationAt/cancelDesignation (claims
  -- are Lua-side, like dig/chop jobs — no engine job status, which is
  -- why the AI's selector is the claim-EXCLUDING query, #2534).
  Lua.newtable
  registerLuaFunction callStats "till" "setAnchor"           (tillSetAnchorFn (toWorldSimCapability env))
  registerLuaFunction callStats "till" "clearAnchor"         (tillClearAnchorFn (toWorldSimCapability env))
  registerLuaFunction callStats "till" "designate"           (tillDesignateFn (toWorldSimCapability env))
  registerLuaFunction callStats "till" "cancelDesignation"   (tillCancelDesignationFn (toWorldSimCapability env))
  registerLuaFunction callStats "till" "getDesignationAt"    (tillGetDesignationAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "till" "getDesignationCount" (tillGetDesignationCountFn (toWorldSimCapability env))
  registerLuaFunction callStats "till" "nearestDesignation"  (tillNearestDesignationFn (toWorldSimCapability env))
  registerLuaFunction callStats "till" "nearestFreeDesignation" (tillNearestFreeDesignationFn (toWorldSimCapability env))
  registerLuaFunction callStats "till" "setDesignateTexture" (tillSetDesignateTextureFn (toWorldSimCapability env))
  Lua.setglobal (Lua.Name "till")

  -- Plant designation tool (#335). Single-tile, no anchor: the tool
  -- drives designate, the farm AI (#336) drives nearestFreeDesignation/
  -- getDesignationAt/cancelDesignation (claims are Lua-side, like dig/
  -- chop/till jobs — no engine job status, #2534).
  Lua.newtable
  registerLuaFunction callStats "plant" "designate"           (plantDesignateFn (toWorldSimCapability env))
  registerLuaFunction callStats "plant" "cancelDesignation"   (plantCancelDesignationFn (toWorldSimCapability env))
  registerLuaFunction callStats "plant" "getDesignationAt"    (plantGetDesignationAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "plant" "getDesignationCount" (plantGetDesignationCountFn (toWorldSimCapability env))
  registerLuaFunction callStats "plant" "nearestDesignation"  (plantNearestDesignationFn (toWorldSimCapability env))
  registerLuaFunction callStats "plant" "nearestFreeDesignation" (plantNearestFreeDesignationFn (toWorldSimCapability env))
  registerLuaFunction callStats "plant" "setDesignateTexture" (plantSetDesignateTextureFn (toWorldSimCapability env))
  Lua.setglobal (Lua.Name "plant")
