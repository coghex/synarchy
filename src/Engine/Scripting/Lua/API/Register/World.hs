module Engine.Scripting.Lua.API.Register.World
  ( registerWorldAPI
  ) where

import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.World
import Engine.Scripting.Lua.API.WorldQuery
import Engine.Scripting.Lua.API.Forage
import Engine.Scripting.Lua.API.Flora
import Engine.Scripting.Lua.API.Plant (worldGetPlantSuitabilityFn)
import Engine.Core.State (EngineEnv)
import qualified HsLua as Lua

-- | Populate and install the @world@ and @flora@ global tables.
registerWorldAPI ∷ EngineEnv → Lua.LuaE Lua.Exception ()
registerWorldAPI env = do
  Lua.newtable
  registerLuaFunction "getGenDefaults" (worldGetGenDefaultsFn env)
  registerLuaFunction "setGenConfig" (worldSetGenConfigFn env)
  registerLuaFunction "init" (worldInitFn env)
  registerLuaFunction "getIdentity" (worldGetIdentityFn env)
  registerLuaFunction "initArena" (worldInitArenaFn env)
  registerLuaFunction "initArenaDone" (worldInitArenaDoneFn env)
  registerLuaFunction "openArena" (worldOpenArenaFn env)
  registerLuaFunction "show" (worldShowFn env)
  registerLuaFunction "hide" (worldHideFn env)
  registerLuaFunction "setTexture" (worldSetTextureFn env)
  registerLuaFunction "setCamera" (worldSetCameraFn env)
  registerLuaFunction "setSunAngle" (worldSetSunAngleFn env)
  registerLuaFunction "setTime" (worldSetTimeFn env)
  registerLuaFunction "setDate" (worldSetDateFn env)
  registerLuaFunction "getDate" (worldGetDateFn env)
  registerLuaFunction "getSeed" (worldGetSeedFn env)
  registerLuaFunction "setTimeScale" (worldSetTimeScaleFn env)
  registerLuaFunction "getTimeScale" (worldGetTimeScaleFn env)
  registerLuaFunction "getActiveWorldId" (worldGetActiveWorldIdFn env)
  registerLuaFunction "setMapMode" (worldSetMapModeFn env)
  registerLuaFunction "setZoomCursorHover" (worldSetZoomCursorHoverFn env)
  registerLuaFunction "setZoomCursorSelect" (worldSetZoomCursorSelectFn env)
  registerLuaFunction "clearZoomCursorSelect" (worldClearZoomCursorSelectFn env)
  registerLuaFunction "setZoomCursorSelectTexture"
    (worldSetZoomCursorSelectTextureFn env)
  registerLuaFunction "setZoomCursorHoverTexture"
    (worldSetZoomCursorHoverTextureFn env)
  registerLuaFunction "setWorldCursorSelectTexture"
    (worldSetWorldCursorSelectTextureFn env)
  registerLuaFunction "setWorldCursorHoverTexture"
    (worldSetWorldCursorHoverTextureFn env)
  registerLuaFunction "setWorldCursorSelectBgTexture"
    (worldSetWorldCursorSelectBgTextureFn env)
  registerLuaFunction "setWorldCursorHoverBgTexture"
    (worldSetWorldCursorHoverBgTextureFn env)
  registerLuaFunction "setWorldCursorHover" (worldSetWorldCursorHoverFn env)
  registerLuaFunction "setWorldCursorSelect" (worldSetWorldCursorSelectFn env)
  registerLuaFunction "selectTile" (worldSelectTileFn env)
  registerLuaFunction "getSelectedTile" (worldGetSelectedTileFn env)
  registerLuaFunction "selectChunk" (worldSelectChunkFn env)
  registerLuaFunction "clearWorldCursorSelect" (worldClearWorldCursorSelectFn env)
  registerLuaFunction "setToolMode" (worldSetToolModeFn env)
  registerLuaFunction "getToolMode" (worldGetToolModeFn env)
  registerLuaFunction "setMineAnchor" (worldSetMineAnchorFn env)
  registerLuaFunction "clearMineAnchor" (worldClearMineAnchorFn env)
  registerLuaFunction "designateMine" (worldDesignateMineFn env)
  registerLuaFunction "setMineDesignateTexture"
    (worldSetMineDesignateTextureFn env)
  registerLuaFunction "getMineDesignationCount"
    (worldGetMineDesignationCountFn env)
  registerLuaFunction "nearestMineDesignation"
    (worldNearestMineDesignationFn env)
  registerLuaFunction "getDigInfoAt" (worldGetDigInfoAtFn env)
  registerLuaFunction "getSpoilInfo" (worldGetSpoilInfoFn env)
  registerLuaFunction "getGemInfoAt" (worldGetGemInfoAtFn env)
  registerLuaFunction "debugTileQuads" (worldDebugTileQuadsFn env)
  registerLuaFunction "addTile"       (worldAddTileFn env)
  registerLuaFunction "listMaterials" (worldListMaterialsFn env)
  registerLuaFunction "digTile" (worldDigTileFn env)
  registerLuaFunction "getMineDesignationAt"
    (worldGetMineDesignationAtFn env)
  registerLuaFunction "getInitProgress" (worldGetInitProgressFn env)
  registerLuaFunction "waitForInit" (worldWaitForInitFn env)
  registerLuaFunction "destroy" (worldDestroyFn env)
  registerLuaFunction "destroyAll" (worldDestroyAllFn env)
  registerLuaFunction "deleteTile" (worldDeleteTileFn env)
  registerLuaFunction "setFluidTile" (worldSetFluidTileFn env)
  registerLuaFunction "setSlope" (worldSetSlopeFn env)
  registerLuaFunction "setVegAt" (worldSetVegFn env)
  registerLuaFunction "setCell" (worldSetCellFn env)

  registerLuaFunction "getTerrainAt" (worldGetTerrainAtFn env)
  registerLuaFunction "getSlopeAt"   (worldGetSlopeAtFn env)
  registerLuaFunction "getVegAt"     (worldGetVegAtFn env)
  registerLuaFunction "isPlantable"  (worldIsPlantableFn env)
  registerLuaFunction "getFluidAt" (worldGetFluidAtFn env)
  registerLuaFunction "getSurfaceAt" (worldGetSurfaceAtFn env)
  registerLuaFunction "getChunkInfo" (worldGetChunkInfoFn env)
  registerLuaFunction "getAreaFluid" (worldGetAreaFluidFn env)
  registerLuaFunction "getRivers" (worldGetRiversFn env)
  registerLuaFunction "loadChunksInRegion" (worldLoadChunksInRegionFn env)
  registerLuaFunction "waitForChunks" (worldWaitForChunksFn env)
  registerLuaFunction "getHoverTile" (worldGetHoverTileFn env)
  registerLuaFunction "getHoverPos"  (worldGetHoverPosFn env)
  registerLuaFunction "pickTile"     (worldPickTileFn env)
  registerLuaFunction "pickPos"      (worldPickPosFn env)
  registerLuaFunction "pickChunk"    (worldPickChunkFn env)
  registerLuaFunction "getClimateAt" (worldGetClimateAtFn env)
  registerLuaFunction "getAmbientAt" (worldGetAmbientAtFn env)
  registerLuaFunction "getSunAngleAt" (worldGetSunAngleAtFn env)
  registerLuaFunction "listPlacedLocations" (worldListPlacedLocationsFn env)
  registerLuaFunction "hasSpawnedLocationContents"
    (worldHasSpawnedLocationContentsFn env)
  registerLuaFunction "markLocationContentsSpawned"
    (worldMarkLocationContentsSpawnedFn env)
  registerLuaFunction "hasStampedLocation"
    (worldHasStampedLocationFn env)
  registerLuaFunction "markLocationStamped"
    (worldMarkLocationStampedFn env)
  registerLuaFunction "getFloraAt" (worldGetFloraAtFn env)
  registerLuaFunction "getFloraGrowthAt" (worldGetFloraGrowthAtFn env)
  registerLuaFunction "harvestFlora" (worldHarvestFloraFn env)
  registerLuaFunction "findHarvestableFlora"
    (worldFindHarvestableFloraFn env)
  registerLuaFunction "plantCropAt" (worldPlantCropAtFn env)
  registerLuaFunction "getCropPlotAt" (worldGetCropPlotAtFn env)
  registerLuaFunction "getPlantSuitability" (worldGetPlantSuitabilityFn env)
  registerLuaFunction "plantRowCropAt" (worldPlantRowCropAtFn env)

  Lua.setglobal (Lua.Name "world")

  Lua.newtable
  registerLuaFunction "register" (floraRegisterFn env)
  registerLuaFunction "setLifecycle" (floraSetLifecycleFn env)
  registerLuaFunction "addCycleStage" (floraAddCycleStageFn env)
  registerLuaFunction "addCycleOverride" (floraAddCycleOverrideFn env)
  registerLuaFunction "addPhase" (floraAddPhaseFn env)
  registerLuaFunction "registerForWorldGen" (floraRegisterForWorldGenFn env)
  registerLuaFunction "exists" (floraExistsFn env)

  Lua.setglobal (Lua.Name "flora")
