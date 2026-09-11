module Engine.Scripting.Lua.API.Register.World
  ( registerWorldAPI
  ) where

import Engine.Core.Capability.Core (toCoreCapability)
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Scripting.Lua.CallStats (LuaCallStats)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.World
import Engine.Scripting.Lua.API.WorldQuery
import Engine.Scripting.Lua.API.Forage
import Engine.Scripting.Lua.API.Flora
import Engine.Scripting.Lua.API.Plant (worldGetPlantSuitabilityFn)
import Engine.Scripting.Lua.API.Items.Ground
    (worldSpawnLocationSignificantItemFn)
import Engine.Scripting.Lua.Types (LuaBackendState)
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

-- | Populate and install the @world@ and @flora@ global tables.
--
--   Takes 'LuaBackendState' for @world.suggestName@ (#1106) and
--   @world.getEtymology@ (#1104), which share the concept-catalogue and
--   language cache kept there — the same reason
--   'Engine.Scripting.Lua.API.Register.Engine.registerEngineAPI'
--   already takes it.
registerWorldAPI ∷ LuaCallStats → EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception ()
registerWorldAPI callStats env backendState = do
  Lua.newtable
  registerLuaFunction callStats "world" "getGenDefaults" (worldGetGenDefaultsFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setGenConfig" (worldSetGenConfigFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "init" (worldInitFn env)
  registerLuaFunction callStats "world" "checkMapImagePlan" (worldCheckMapImagePlanFn env)
  registerLuaFunction callStats "world" "getIdentity" (worldGetIdentityFn env)
  registerLuaFunction callStats "world" "getLanguageProvenance" (worldGetLanguageProvenanceFn env)
  registerLuaFunction callStats "world" "suggestName" (worldSuggestNameFn backendState)
  registerLuaFunction callStats "world" "generatedNameCharacters" worldGeneratedNameCharactersFn
  registerLuaFunction callStats "world" "initArena" (worldInitArenaFn env)
  registerLuaFunction callStats "world" "initArenaDone" (worldInitArenaDoneFn env)
  registerLuaFunction callStats "world" "openArena" (worldOpenArenaFn env)
  registerLuaFunction callStats "world" "show" (worldShowFn env)
  registerLuaFunction callStats "world" "hide" (worldHideFn env)
  registerLuaFunction callStats "world" "setTexture" (worldSetTextureFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setCamera" (worldSetCameraFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setSunAngle" (worldSetSunAngleFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setTime" (worldSetTimeFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setDate" (worldSetDateFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getDate" (worldGetDateFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getSeed" (worldGetSeedFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setTimeScale" (worldSetTimeScaleFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getTimeScale" (worldGetTimeScaleFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getActiveWorldId" (worldGetActiveWorldIdFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setMapMode" (worldSetMapModeFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setZoomCursorHover" (worldSetZoomCursorHoverFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setZoomCursorSelect" (worldSetZoomCursorSelectFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "clearZoomCursorSelect" (worldClearZoomCursorSelectFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setZoomCursorSelectTexture"
    (worldSetZoomCursorSelectTextureFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setZoomCursorHoverTexture"
    (worldSetZoomCursorHoverTextureFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setWorldCursorSelectTexture"
    (worldSetWorldCursorSelectTextureFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setWorldCursorHoverTexture"
    (worldSetWorldCursorHoverTextureFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setWorldCursorSelectBgTexture"
    (worldSetWorldCursorSelectBgTextureFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setWorldCursorHoverBgTexture"
    (worldSetWorldCursorHoverBgTextureFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setWorldCursorHover" (worldSetWorldCursorHoverFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setWorldCursorSelect" (worldSetWorldCursorSelectFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "selectTile" (worldSelectTileFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getSelectedTile" (worldGetSelectedTileFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "selectChunk" (worldSelectChunkFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "clearWorldCursorSelect" (worldClearWorldCursorSelectFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setToolMode" (worldSetToolModeFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getToolMode" (worldGetToolModeFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setMineAnchor" (worldSetMineAnchorFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "clearMineAnchor" (worldClearMineAnchorFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "designateMine" (worldDesignateMineFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setMineDesignateTexture"
    (worldSetMineDesignateTextureFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getMineDesignationCount"
    (worldGetMineDesignationCountFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "nearestMineDesignation"
    (worldNearestMineDesignationFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getDigInfoAt" (worldGetDigInfoAtFn env)
  registerLuaFunction callStats "world" "getSpoilInfo" (worldGetSpoilInfoFn env)
  registerLuaFunction callStats "world" "getGemInfoAt" (worldGetGemInfoAtFn env)
  registerLuaFunction callStats "world" "debugTileQuads" (worldDebugTileQuadsFn env)
  registerLuaFunction callStats "world" "addTile"       (worldAddTileFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "listMaterials" (worldListMaterialsFn env)
  registerLuaFunction callStats "world" "digTile"
    (worldDigTileFn (toCoreCapability env) (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getMineDesignationAt"
    (worldGetMineDesignationAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getInitProgress" (worldGetInitProgressFn env)
  registerLuaFunction callStats "world" "waitForInit" (worldWaitForInitFn env)
  registerLuaFunction callStats "world" "destroy" (worldDestroyFn env)
  registerLuaFunction callStats "world" "destroyAll" (worldDestroyAllFn env)
  registerLuaFunction callStats "world" "deleteTile" (worldDeleteTileFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setFluidTile" (worldSetFluidTileFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setSlope" (worldSetSlopeFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setVegAt" (worldSetVegFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setCell" (worldSetCellFn (toWorldSimCapability env))

  registerLuaFunction callStats "world" "getTerrainAt" (worldGetTerrainAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getSlopeAt"   (worldGetSlopeAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getVegAt"     (worldGetVegAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getMaterialAt" (worldGetMaterialAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "isPlantable"  (worldIsPlantableFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getFluidAt" (worldGetFluidAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getSurfaceAt" (worldGetSurfaceAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getChunkInfo" (worldGetChunkInfoFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getAreaFluid" (worldGetAreaFluidFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getRivers" (worldGetRiversFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getRiverAt" (worldGetRiverAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getEtymology"
    (worldGetEtymologyFn (toWorldSimCapability env) backendState)
  registerLuaFunction callStats "world" "loadChunksInRegion" (worldLoadChunksInRegionFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "waitForChunks" (worldWaitForChunksFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getHoverTile" (worldGetHoverTileFn env)
  registerLuaFunction callStats "world" "getHoverPos"  (worldGetHoverPosFn env)
  registerLuaFunction callStats "world" "pickTile"     (worldPickTileFn env)
  registerLuaFunction callStats "world" "zoomTileRect" (worldZoomTileRectFn env)
  registerLuaFunction callStats "world" "pickPos"      (worldPickPosFn env)
  registerLuaFunction callStats "world" "pickChunk"    (worldPickChunkFn env)
  registerLuaFunction callStats "world" "localizeTile" (worldLocalizeTileFn env)
  registerLuaFunction callStats "world" "getWrapWidth" (worldGetWrapWidthFn env)
  registerLuaFunction callStats "world" "getClimateAt" (worldGetClimateAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getAmbientAt" (worldGetAmbientAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getSunAngleAt" (worldGetSunAngleAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "listPlacedLocations"
    (worldListPlacedLocationsFn env)
  registerLuaFunction callStats "world" "getLocationInstance"
    (worldGetLocationInstanceFn env)
  registerLuaFunction callStats "world" "getLocationAwareness"
    (worldGetLocationAwarenessFn env)
  registerLuaFunction callStats "world" "hasSpawnedLocationContents"
    (worldHasSpawnedLocationContentsFn env)
  registerLuaFunction callStats "world" "markLocationContentsSpawned"
    (worldMarkLocationContentsSpawnedFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "markLocationContentsSpawnedById"
    (worldMarkLocationContentsSpawnedByIdFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "spawnLocationSignificantItem"
    (worldSpawnLocationSignificantItemFn env)
  registerLuaFunction callStats "world" "registerLocationEncounterOccupants"
    (worldRegisterLocationEncounterOccupantsFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setLocationEncounterOccupantState"
    (worldSetLocationEncounterOccupantStateFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setLocationEncounterEpisodeState"
    (worldSetLocationEncounterEpisodeStateFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "setLocationLifecycle"
    (worldSetLocationLifecycleFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "hasStampedLocation"
    (worldHasStampedLocationFn env)
  registerLuaFunction callStats "world" "markLocationStamped"
    (worldMarkLocationStampedFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getFloraAt" (worldGetFloraAtFn env)
  registerLuaFunction callStats "world" "getFloraGrowthAt" (worldGetFloraGrowthAtFn env)
  registerLuaFunction callStats "world" "harvestFlora" (worldHarvestFloraFn env)
  registerLuaFunction callStats "world" "harvestFloraInstance" (worldHarvestFloraInstanceFn env)
  registerLuaFunction callStats "world" "findHarvestableFlora"
    (worldFindHarvestableFloraFn env)
  registerLuaFunction callStats "world" "plantCropAt" (worldPlantCropAtFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "getCropPlotAt" (worldGetCropPlotAtFn env)
  registerLuaFunction callStats "world" "getPlantSuitability" (worldGetPlantSuitabilityFn (toWorldSimCapability env))
  registerLuaFunction callStats "world" "plantRowCropAt" (worldPlantRowCropAtFn (toWorldSimCapability env))

  Lua.setglobal (Lua.Name "world")

  Lua.newtable
  registerLuaFunction callStats "flora" "register"
    (floraRegisterFn (toCoreCapability env) (toWorldSimCapability env))
  registerLuaFunction callStats "flora" "setLifecycle" (floraSetLifecycleFn (toWorldSimCapability env))
  registerLuaFunction callStats "flora" "addCycleStage" (floraAddCycleStageFn (toWorldSimCapability env))
  registerLuaFunction callStats "flora" "addCycleOverride" (floraAddCycleOverrideFn (toWorldSimCapability env))
  registerLuaFunction callStats "flora" "addPhase" (floraAddPhaseFn (toWorldSimCapability env))
  registerLuaFunction callStats "flora" "registerForWorldGen" (floraRegisterForWorldGenFn (toWorldSimCapability env))
  registerLuaFunction callStats "flora" "exists" (floraExistsFn (toWorldSimCapability env))

  Lua.setglobal (Lua.Name "flora")
