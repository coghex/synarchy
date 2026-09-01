module Engine.Scripting.Lua.API.Register.World
  ( registerWorldAPI
  ) where

import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.World
import Engine.Scripting.Lua.API.WorldQuery
import Engine.Scripting.Lua.API.Forage
import Engine.Scripting.Lua.API.Flora
import Engine.Scripting.Lua.API.Plant (worldGetPlantSuitabilityFn)
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
registerWorldAPI ∷ EngineEnv → LuaBackendState → Lua.LuaE Lua.Exception ()
registerWorldAPI env backendState = do
  Lua.newtable
  registerLuaFunction "getGenDefaults" (worldGetGenDefaultsFn (toWorldSimCapability env))
  registerLuaFunction "setGenConfig" (worldSetGenConfigFn (toWorldSimCapability env))
  registerLuaFunction "init" (worldInitFn env)
  registerLuaFunction "checkMapImagePlan" (worldCheckMapImagePlanFn env)
  registerLuaFunction "getIdentity" (worldGetIdentityFn env)
  registerLuaFunction "getLanguageProvenance" (worldGetLanguageProvenanceFn env)
  registerLuaFunction "suggestName" (worldSuggestNameFn backendState)
  registerLuaFunction "generatedNameCharacters" worldGeneratedNameCharactersFn
  registerLuaFunction "initArena" (worldInitArenaFn env)
  registerLuaFunction "initArenaDone" (worldInitArenaDoneFn env)
  registerLuaFunction "openArena" (worldOpenArenaFn env)
  registerLuaFunction "show" (worldShowFn env)
  registerLuaFunction "hide" (worldHideFn env)
  registerLuaFunction "setTexture" (worldSetTextureFn (toWorldSimCapability env))
  registerLuaFunction "setCamera" (worldSetCameraFn (toWorldSimCapability env))
  registerLuaFunction "setSunAngle" (worldSetSunAngleFn (toWorldSimCapability env))
  registerLuaFunction "setTime" (worldSetTimeFn (toWorldSimCapability env))
  registerLuaFunction "setDate" (worldSetDateFn (toWorldSimCapability env))
  registerLuaFunction "getDate" (worldGetDateFn (toWorldSimCapability env))
  registerLuaFunction "getSeed" (worldGetSeedFn (toWorldSimCapability env))
  registerLuaFunction "setTimeScale" (worldSetTimeScaleFn (toWorldSimCapability env))
  registerLuaFunction "getTimeScale" (worldGetTimeScaleFn (toWorldSimCapability env))
  registerLuaFunction "getActiveWorldId" (worldGetActiveWorldIdFn (toWorldSimCapability env))
  registerLuaFunction "setMapMode" (worldSetMapModeFn (toWorldSimCapability env))
  registerLuaFunction "setZoomCursorHover" (worldSetZoomCursorHoverFn (toWorldSimCapability env))
  registerLuaFunction "setZoomCursorSelect" (worldSetZoomCursorSelectFn (toWorldSimCapability env))
  registerLuaFunction "clearZoomCursorSelect" (worldClearZoomCursorSelectFn (toWorldSimCapability env))
  registerLuaFunction "setZoomCursorSelectTexture"
    (worldSetZoomCursorSelectTextureFn (toWorldSimCapability env))
  registerLuaFunction "setZoomCursorHoverTexture"
    (worldSetZoomCursorHoverTextureFn (toWorldSimCapability env))
  registerLuaFunction "setWorldCursorSelectTexture"
    (worldSetWorldCursorSelectTextureFn (toWorldSimCapability env))
  registerLuaFunction "setWorldCursorHoverTexture"
    (worldSetWorldCursorHoverTextureFn (toWorldSimCapability env))
  registerLuaFunction "setWorldCursorSelectBgTexture"
    (worldSetWorldCursorSelectBgTextureFn (toWorldSimCapability env))
  registerLuaFunction "setWorldCursorHoverBgTexture"
    (worldSetWorldCursorHoverBgTextureFn (toWorldSimCapability env))
  registerLuaFunction "setWorldCursorHover" (worldSetWorldCursorHoverFn (toWorldSimCapability env))
  registerLuaFunction "setWorldCursorSelect" (worldSetWorldCursorSelectFn (toWorldSimCapability env))
  registerLuaFunction "selectTile" (worldSelectTileFn (toWorldSimCapability env))
  registerLuaFunction "getSelectedTile" (worldGetSelectedTileFn (toWorldSimCapability env))
  registerLuaFunction "selectChunk" (worldSelectChunkFn (toWorldSimCapability env))
  registerLuaFunction "clearWorldCursorSelect" (worldClearWorldCursorSelectFn (toWorldSimCapability env))
  registerLuaFunction "setToolMode" (worldSetToolModeFn (toWorldSimCapability env))
  registerLuaFunction "getToolMode" (worldGetToolModeFn (toWorldSimCapability env))
  registerLuaFunction "setMineAnchor" (worldSetMineAnchorFn (toWorldSimCapability env))
  registerLuaFunction "clearMineAnchor" (worldClearMineAnchorFn (toWorldSimCapability env))
  registerLuaFunction "designateMine" (worldDesignateMineFn (toWorldSimCapability env))
  registerLuaFunction "setMineDesignateTexture"
    (worldSetMineDesignateTextureFn (toWorldSimCapability env))
  registerLuaFunction "getMineDesignationCount"
    (worldGetMineDesignationCountFn (toWorldSimCapability env))
  registerLuaFunction "nearestMineDesignation"
    (worldNearestMineDesignationFn (toWorldSimCapability env))
  registerLuaFunction "getDigInfoAt" (worldGetDigInfoAtFn env)
  registerLuaFunction "getSpoilInfo" (worldGetSpoilInfoFn env)
  registerLuaFunction "getGemInfoAt" (worldGetGemInfoAtFn env)
  registerLuaFunction "debugTileQuads" (worldDebugTileQuadsFn env)
  registerLuaFunction "addTile"       (worldAddTileFn (toWorldSimCapability env))
  registerLuaFunction "listMaterials" (worldListMaterialsFn env)
  registerLuaFunction "digTile" (worldDigTileFn (toWorldSimCapability env))
  registerLuaFunction "getMineDesignationAt"
    (worldGetMineDesignationAtFn (toWorldSimCapability env))
  registerLuaFunction "getInitProgress" (worldGetInitProgressFn env)
  registerLuaFunction "waitForInit" (worldWaitForInitFn env)
  registerLuaFunction "destroy" (worldDestroyFn env)
  registerLuaFunction "destroyAll" (worldDestroyAllFn env)
  registerLuaFunction "deleteTile" (worldDeleteTileFn (toWorldSimCapability env))
  registerLuaFunction "setFluidTile" (worldSetFluidTileFn (toWorldSimCapability env))
  registerLuaFunction "setSlope" (worldSetSlopeFn (toWorldSimCapability env))
  registerLuaFunction "setVegAt" (worldSetVegFn (toWorldSimCapability env))
  registerLuaFunction "setCell" (worldSetCellFn (toWorldSimCapability env))

  registerLuaFunction "getTerrainAt" (worldGetTerrainAtFn (toWorldSimCapability env))
  registerLuaFunction "getSlopeAt"   (worldGetSlopeAtFn (toWorldSimCapability env))
  registerLuaFunction "getVegAt"     (worldGetVegAtFn (toWorldSimCapability env))
  registerLuaFunction "isPlantable"  (worldIsPlantableFn (toWorldSimCapability env))
  registerLuaFunction "getFluidAt" (worldGetFluidAtFn (toWorldSimCapability env))
  registerLuaFunction "getSurfaceAt" (worldGetSurfaceAtFn (toWorldSimCapability env))
  registerLuaFunction "getChunkInfo" (worldGetChunkInfoFn (toWorldSimCapability env))
  registerLuaFunction "getAreaFluid" (worldGetAreaFluidFn (toWorldSimCapability env))
  registerLuaFunction "getRivers" (worldGetRiversFn (toWorldSimCapability env))
  registerLuaFunction "getRiverAt" (worldGetRiverAtFn (toWorldSimCapability env))
  registerLuaFunction "getEtymology"
    (worldGetEtymologyFn (toWorldSimCapability env) backendState)
  registerLuaFunction "loadChunksInRegion" (worldLoadChunksInRegionFn (toWorldSimCapability env))
  registerLuaFunction "waitForChunks" (worldWaitForChunksFn (toWorldSimCapability env))
  registerLuaFunction "getHoverTile" (worldGetHoverTileFn env)
  registerLuaFunction "getHoverPos"  (worldGetHoverPosFn env)
  registerLuaFunction "pickTile"     (worldPickTileFn env)
  registerLuaFunction "pickPos"      (worldPickPosFn env)
  registerLuaFunction "pickChunk"    (worldPickChunkFn env)
  registerLuaFunction "localizeTile" (worldLocalizeTileFn env)
  registerLuaFunction "getWrapWidth" (worldGetWrapWidthFn env)
  registerLuaFunction "getClimateAt" (worldGetClimateAtFn (toWorldSimCapability env))
  registerLuaFunction "getAmbientAt" (worldGetAmbientAtFn (toWorldSimCapability env))
  registerLuaFunction "getSunAngleAt" (worldGetSunAngleAtFn (toWorldSimCapability env))
  registerLuaFunction "listPlacedLocations"
    (worldListPlacedLocationsFn env)
  registerLuaFunction "getLocationInstance"
    (worldGetLocationInstanceFn env)
  registerLuaFunction "getLocationAwareness"
    (worldGetLocationAwarenessFn env)
  registerLuaFunction "hasSpawnedLocationContents"
    (worldHasSpawnedLocationContentsFn env)
  registerLuaFunction "markLocationContentsSpawned"
    (worldMarkLocationContentsSpawnedFn (toWorldSimCapability env))
  registerLuaFunction "markLocationContentsSpawnedById"
    (worldMarkLocationContentsSpawnedByIdFn (toWorldSimCapability env))
  registerLuaFunction "registerLocationSignificantSpawn"
    (worldRegisterLocationSignificantSpawnFn (toWorldSimCapability env))
  registerLuaFunction "registerLocationEncounterOccupants"
    (worldRegisterLocationEncounterOccupantsFn (toWorldSimCapability env))
  registerLuaFunction "setLocationEncounterOccupantState"
    (worldSetLocationEncounterOccupantStateFn (toWorldSimCapability env))
  registerLuaFunction "setLocationEncounterEpisodeState"
    (worldSetLocationEncounterEpisodeStateFn (toWorldSimCapability env))
  registerLuaFunction "setLocationLifecycle"
    (worldSetLocationLifecycleFn (toWorldSimCapability env))
  registerLuaFunction "hasStampedLocation"
    (worldHasStampedLocationFn env)
  registerLuaFunction "markLocationStamped"
    (worldMarkLocationStampedFn (toWorldSimCapability env))
  registerLuaFunction "getFloraAt" (worldGetFloraAtFn env)
  registerLuaFunction "getFloraGrowthAt" (worldGetFloraGrowthAtFn env)
  registerLuaFunction "harvestFlora" (worldHarvestFloraFn env)
  registerLuaFunction "harvestFloraInstance" (worldHarvestFloraInstanceFn env)
  registerLuaFunction "findHarvestableFlora"
    (worldFindHarvestableFloraFn env)
  registerLuaFunction "plantCropAt" (worldPlantCropAtFn (toWorldSimCapability env))
  registerLuaFunction "getCropPlotAt" (worldGetCropPlotAtFn env)
  registerLuaFunction "getPlantSuitability" (worldGetPlantSuitabilityFn (toWorldSimCapability env))
  registerLuaFunction "plantRowCropAt" (worldPlantRowCropAtFn (toWorldSimCapability env))

  Lua.setglobal (Lua.Name "world")

  Lua.newtable
  registerLuaFunction "register" (floraRegisterFn (toWorldSimCapability env))
  registerLuaFunction "setLifecycle" (floraSetLifecycleFn (toWorldSimCapability env))
  registerLuaFunction "addCycleStage" (floraAddCycleStageFn (toWorldSimCapability env))
  registerLuaFunction "addCycleOverride" (floraAddCycleOverrideFn (toWorldSimCapability env))
  registerLuaFunction "addPhase" (floraAddPhaseFn (toWorldSimCapability env))
  registerLuaFunction "registerForWorldGen" (floraRegisterForWorldGenFn (toWorldSimCapability env))
  registerLuaFunction "exists" (floraExistsFn (toWorldSimCapability env))

  Lua.setglobal (Lua.Name "flora")
