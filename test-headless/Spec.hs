module Main where

import UPrelude
import Test.Hspec
import qualified Data.List as MeasureList
import qualified System.Environment as MeasureEnv
import qualified System.IO as MeasureIO
import qualified Test.Hspec.Runner as MeasureRunner
import Test.Headless.Harness (withHeadlessEngine, withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import qualified Test.Headless.Harness.WorkerHealth as HarnessWorkerHealth
import qualified Test.Headless.UPrelude as UPreludeSpec
import qualified Test.Headless.Audio.Native as AudioNative
import qualified Test.Headless.Audio.Config as AudioConfig
import qualified Test.Headless.Audio.Catalog as AudioCatalog
import qualified Test.Headless.Audio.Upload as AudioUpload
import qualified Test.Headless.Audio.Transport as AudioTransport
import qualified Test.Headless.Audio.Spatial as AudioSpatial
import qualified Test.Headless.Audio.Runtime as AudioRuntime
import qualified Test.Headless.Audio.Integration as AudioIntegration
import qualified Test.Headless.Audio.Health as AudioHealth
import qualified Test.Headless.Audio.Preview as AudioPreview
import qualified Test.Headless.Audio.PreviewUI as AudioPreviewUI
import qualified Test.Headless.Audio.Thread as AudioThread
import qualified Test.Headless.Audio.Lua as AudioLua
import qualified Test.Headless.Audio.Settings as AudioSettings
import qualified Test.Headless.Capability.Audio as CapabilityAudio
import qualified Test.Headless.WorldGen as WorldGen
import qualified Test.Headless.WorldGen.Geology as Geology
import qualified Test.Headless.WorldGen.Parity as Parity
import qualified Test.Headless.WorldGen.Flatness as Flatness
import qualified Test.Headless.WorldGen.SoilGate as SoilGate
import qualified Test.Headless.WorldGen.SoilShed as SoilShed
import qualified Test.Headless.WorldGen.SoilRedistribution as SoilRedistribution
import qualified Test.Headless.WorldGen.Exposure as Exposure
import qualified Test.Headless.WorldGen.ZoomParity as ZoomParity
import qualified Test.Headless.WorldGen.ZoomArtifact as ZoomArtifact
import qualified Test.Headless.WorldGen.ZoomOceanFill as ZoomOceanFill
import qualified Test.Headless.WorldGen.BorderProbe as BorderProbe
import qualified Test.Headless.WorldGen.WrapSeam as WrapSeam
import qualified Test.Headless.WorldGen.CoastBreach as CoastBreach
import qualified Test.Headless.WorldGen.Breakthrough as Breakthrough
import qualified Test.Headless.WorldGen.ExactRiver as ExactRiver
import qualified Test.Headless.WorldGen.ExactRiverWorld as ExactRiverWorld
import qualified Test.Headless.WorldGen.SharedSpillway as SharedSpillway
import qualified Test.Headless.WorldGen.BedDepth as BedDepth
import qualified Test.Headless.WorldGen.FluidSurfaceFold as FluidSurfaceFold
import qualified Test.Headless.WorldGen.ConfigLoad as WorldGenConfigLoad
import qualified Test.Headless.Unit.Pathing.Cost as PathingCost
import qualified Test.Headless.Unit.Pathing.Hazard as PathingHazard
import qualified Test.Headless.Unit.Pathing.MotionArgs as PathingMotionArgs
import qualified Test.Headless.Unit.Pathing.MoveToApi as PathingMoveToApi
import qualified Test.Headless.Unit.SimPageOwnership as SimPageOwnership
import qualified Test.Headless.Unit.Pathing.AStar as PathingAStar
import qualified Test.Headless.Unit.Pathing.Config as PathingConfig
import qualified Test.Headless.Unit.Render.PickFrame as PickFrame
import qualified Test.Headless.Unit.HitTest as UnitHitTest
import qualified Test.Headless.Unit.Anim as AnimTest
import qualified Test.Headless.Unit.Injury as InjuryTest
import qualified Test.Headless.Unit.InjurySpeed as InjurySpeedTest
import qualified Test.Headless.Unit.Fall as FallTest
import qualified Test.Headless.Unit.StopTransition as StopTransition
import qualified Test.Headless.Unit.Stats as StatsTest
import qualified Test.Headless.Unit.StanceRecovery as StanceRecovery
import qualified Test.Headless.Unit.FrameOrganFailure as FrameOrganFailure
import qualified Test.Headless.Unit.LeanFloorDeath as LeanFloorDeath
import qualified Test.Headless.Unit.SourceDrinkingHydration as SourceDrinkingHydration
import qualified Test.Headless.Unit.ResourceTickCarry as ResourceTickCarry
import qualified Test.Headless.Unit.RegrowthPrecision as RegrowthPrecision
import qualified Test.Headless.Unit.SourceDrinkPose as SourceDrinkPose
import qualified Test.Headless.Unit.MentalWanderFallback as MentalWanderFallback
import qualified Test.Headless.Unit.TerminalDeathPose as TerminalDeathPose
import qualified Test.Headless.Unit.StaminaCommit as StaminaCommit
import qualified Test.Headless.Unit.AddXpApi as UnitAddXpApi
import qualified Test.Headless.Unit.AccessoryUnequip as AccessoryUnequip
import qualified Test.Headless.Unit.SpawnShed as SpawnShedTest
import qualified Test.Headless.Unit.Transfer as UnitTransfer
import qualified Test.Headless.Unit.TransferApi as UnitTransferApi
import qualified Test.Headless.Unit.TransferOrderApi as UnitTransferOrderApi
import qualified Test.Headless.Unit.CargoApi as UnitCargoApi
import qualified Test.Headless.Unit.WoundsApi as UnitWoundsApi
import qualified Test.Headless.Unit.MedicalReach as UnitMedicalReach
import qualified Test.Headless.Unit.MedicalKitInstance as UnitMedicalKitInstance
import qualified Test.Headless.Unit.MedicalWoundIdentity as UnitMedicalWoundIdentity
import qualified Test.Headless.Unit.MedicAntibioticSupply as UnitMedicAntibioticSupply
import qualified Test.Headless.Unit.NightPerception as NightPerception
import qualified Test.Headless.Unit.LineOfSight as LineOfSightTest
import qualified Test.Headless.World.ArenaSeed as ArenaSeed
import qualified Test.Headless.World.TimeLocal as TimeLocal
import qualified Test.Headless.World.Climate as Climate
import qualified Test.Headless.Item.GroundMove as GroundMove
import qualified Test.Headless.Item.GroundPageOwnership as GroundPageOwnership
import qualified Test.Headless.Item.GroundSelection as GroundSelection
import qualified Test.Headless.Lua.FoodHarvestTarget as LuaFoodHarvestTarget
import qualified Test.Headless.Lua.UnitAiPickupPage as LuaUnitAiPickupPage
import qualified Test.Headless.Lua.UnitAiRepairGround as LuaUnitAiRepairGround
import qualified Test.Headless.Item.Temperature as ItemTemp
import qualified Test.Headless.Item.BuffYaml as ItemBuffYaml
import qualified Test.Headless.Item.QualityTier as ItemQualityTier
import qualified Test.Headless.Item.ContentsSignature as ItemContentsSig
import qualified Test.Headless.Item.Condition as ItemCondition
import qualified Test.Headless.Item.SteelHelmet as ItemSteelHelmet
import qualified Test.Headless.Item.RepairFinite as ItemRepairFinite
import qualified Test.Headless.Item.Materialize as ItemMaterialize
import qualified Test.Headless.Item.BulkStorage as ItemBulkStorage
import qualified Test.Headless.Item.Ownership as ItemOwnership
import qualified Test.Headless.Item.FoodNutrition as ItemFoodNutrition
import qualified Test.Headless.Item.Discovery as ItemDiscovery
import qualified Test.Headless.Asset.TextureFallback as TextureFallback
import qualified Test.Headless.Asset.FloraContent as FloraContent
import qualified Test.Headless.Asset.FloraRegrowthSchema as FloraRegrowthSchema
import qualified Test.Headless.Asset.FloraHarvestPolicySchema as FloraHarvestPolicySchema
import qualified Test.Headless.Asset.FloraVocabularySchema as FloraVocabularySchema
import qualified Test.Headless.Asset.InfectionSchema as InfectionSchema
import qualified Test.Headless.Asset.UnitBodyGraph as AssetUnitBodyGraph
import qualified Test.Headless.Asset.UnitInventory as AssetUnitInventory
import qualified Test.Headless.Asset.Types as AssetTypes
import qualified Test.Headless.Asset.YamlList as AssetYamlList
import qualified Test.Headless.Startup.AssetLogging as StartupAssetLogging
import qualified Test.Headless.Startup.Readiness as StartupReadiness
import qualified Test.Headless.Asset.MaterialMoveCost as AssetMaterialMoveCost
import qualified Test.Headless.Preview.Discovery as PreviewDiscovery
import qualified Test.Headless.Unit.Atlas as UnitAtlas
import qualified Test.Headless.Unit.Atlas.Loader as UnitAtlasLoader
import qualified Test.Headless.Preview.UnitAnimation as PreviewUnitAnimation
import qualified Test.Headless.Preview.Building as PreviewBuilding
import qualified Test.Headless.Preview.BuildingMatrix as PreviewBuildingMatrix
import qualified Test.Headless.Preview.BuildingMatrixView as PreviewBuildingMatrixView
import qualified Test.Headless.Preview.Zoom as PreviewZoom
import qualified Test.Headless.Preview.KeyboardNavigation as PreviewKeyboardNavigation
import qualified Test.Headless.Preview.StructurePack as PreviewStructurePack
import qualified Test.Headless.World.Save.Sanitize as SaveSanitize
import qualified Test.Headless.World.Save.Serialize as SaveSerialize
import qualified Test.Headless.World.Save.Envelope as SaveEnvelope
import qualified Test.Headless.World.Save.Components as SaveComponents
import qualified Test.Headless.World.Save.Compat as SaveCompat
import qualified Test.Headless.World.Save.Integrity as SaveIntegrity
import qualified Test.Headless.World.Save.Storage as SaveStorage
import qualified Test.Headless.World.Save.Contract as SaveContract
import qualified Test.Headless.World.Identity as WorldIdentity
import qualified Test.Headless.World.GeneratedIdentity as GeneratedIdentity
import qualified Test.Headless.World.GeneratedLibrary as GeneratedLibrary
import qualified Test.Headless.World.MapImagePlan as MapImagePlan
import qualified Test.Headless.World.MapPyramid as MapPyramid
import qualified Test.Headless.World.PagedMapArtifact as PagedMapArtifact
import qualified Test.Headless.World.MapImageAdmission as MapImageAdmission
import qualified Test.Headless.World.MaterialRegistryMerge as MaterialRegistryMerge
import qualified Test.Headless.World.TransferOrders as WorldTransferOrders
import qualified Test.Headless.World.FluidWritebackStaleness as FluidWritebackStaleness
import qualified Test.Headless.World.Solidification as Solidification
import qualified Test.Headless.World.SolidificationOccupants as SolidificationOccupants
import qualified Test.Headless.World.FluidWritebackIncarnation as FluidWritebackIncarnation
import qualified Test.Headless.World.CursorInfo as CursorInfo
import qualified Test.Headless.World.CursorTextureDispatch as CursorTextureDispatch
import qualified Test.Headless.World.SelectTileZ as SelectTileZ
import qualified Test.Headless.World.SelectChunk as SelectChunk
import qualified Test.Headless.World.ChunkIdentity as ChunkIdentity
import qualified Test.Headless.World.ChunkMemory as ChunkMemory
import qualified Test.Headless.World.ChunkPageBinding as ChunkPageBinding
import qualified Test.Headless.World.ChunkQueueFrame as ChunkQueueFrame
import qualified Test.Headless.World.ActionOutcome as ActionOutcome
import qualified Test.Headless.World.Spoil as Spoil
import qualified Test.Headless.World.RenderedSurface as RenderedSurface
import qualified Test.Headless.World.IslandColumns as IslandColumns
import qualified Test.Headless.World.ChunkCoordinates as ChunkCoordinates
import qualified Test.Headless.Combat.Admission as CombatAdmission
import qualified Test.Headless.Combat.Damage as CombatDamage
import qualified Test.Headless.Combat.MaxStamina as CombatMaxStamina
import qualified Test.Headless.Combat.MentalEffectiveness as CombatMentalEffectiveness
import qualified Test.Headless.Combat.Severing as CombatSevering
import qualified Test.Headless.Combat.Wounds as CombatWounds
import qualified Test.Headless.Magma.Shape as MagmaShape
import qualified Test.Headless.Sim.Admission as SimAdmission
import qualified Test.Headless.Sim.ExactFluid as SimExactFluid
import qualified Test.Headless.Sim.Seam as SimSeam
import qualified Test.Headless.Sim.Conservation as SimConservation
import qualified Test.Headless.Sim.Reaction as SimReaction
import qualified Test.Headless.Input.KeyNames as InputKeyNames
import qualified Test.Headless.Input.Bindings as InputBindings
import qualified Test.Headless.Input.State as InputState
import qualified Test.Headless.Input.Inject as InputInject
import qualified Test.Headless.Input.Followup as InputFollowup
import qualified Test.Headless.Input.InjectOwnership as InputInjectOwnership
import qualified Test.Headless.Lua.DebugQueue as LuaDebugQueue
import qualified Test.Headless.Lua.SceneText as LuaSceneText
import qualified Test.Headless.Lua.RenderQueue as LuaRenderQueue
import qualified Test.Headless.Lua.PreviewGeneration as LuaPreviewGeneration
import qualified Test.Headless.Lua.PauseGate as LuaPauseGate
import qualified Test.Headless.World.PauseSpeed as PauseSpeed
import qualified Test.Headless.World.SessionEpoch as SessionEpoch
import qualified Test.Headless.World.PageIncarnation as PageIncarnation
import qualified Test.Headless.World.TimeScaleDomain as TimeScaleDomain
import qualified Test.Headless.World.GenConfigDomain as GenConfigDomain
import qualified Test.Headless.Equipment.Reconcile as EquipmentReconcile
import qualified Test.Headless.Lua.ScriptState as LuaScriptState
import qualified Test.Headless.Lua.TickInterval as LuaTickInterval
import qualified Test.Headless.Lua.CallStats as LuaCallStats
import qualified Test.Headless.Lua.UiDescriptors as LuaUiDescriptors
import qualified Test.Headless.Lua.SchedulerFairness as LuaSchedulerFairness
import qualified Test.Headless.Graphics.SwapchainResize as GraphicsSwapchainResize
import qualified Test.Headless.Input.LayerA as InputLayerA
import qualified Test.Headless.Input.WheelPolicy as InputWheelPolicy
import qualified Test.Headless.Graphics.VideoConfig as VideoConfig
import qualified Test.Headless.Graphics.VulkanAppIdentity as VulkanAppIdentity
import qualified Test.Headless.Graphics.BindlessFeatures as BindlessFeatures
import qualified Test.Headless.Graphics.InstancePlan as GraphicsInstancePlan
import qualified Test.Headless.Graphics.WindowMode as GraphicsWindowMode
import qualified Test.Headless.Graphics.AmbientLight as AmbientLight
import qualified Test.Headless.Graphics.Screenshot as GraphicsScreenshot
import qualified Test.Headless.Graphics.SwapchainSelection as GraphicsSwapchainSelection
import qualified Test.Headless.Graphics.UniformLayout as GraphicsUniformLayout
import qualified Test.Headless.Graphics.VertexLayout as GraphicsVertexLayout
import qualified Test.Headless.Graphics.WorldVertexCoords as GraphicsWorldVertexCoords
import qualified Test.Headless.Graphics.FontFallback as GraphicsFontFallback
import qualified Test.Headless.Graphics.FontRepertoire as GraphicsFontRepertoire
import qualified Test.Headless.Construct.AttemptIdentity as ConstructAttemptIdentity
import qualified Test.Headless.Construct.Corners as ConstructCorners
import qualified Test.Headless.Construct.Footprint as ConstructFootprint
import qualified Test.Headless.Construct.Plan as ConstructPlan
import qualified Test.Headless.Render.StructureGhost as StructureGhost
import qualified Test.Headless.Render.FrameAssembly as FrameAssembly
import qualified Test.Headless.Construct.PlanInvalidation as ConstructPlanInvalidation
import qualified Test.Headless.Construct.PendingRefusal as ConstructPendingRefusal
import qualified Test.Headless.Craft.Execute as CraftExecute
import qualified Test.Headless.Craft.Bills as CraftBills
import qualified Test.Headless.Craft.OutputIdentity as CraftOutputIdentity
import qualified Test.Headless.Craft.BillPageBinding as CraftBillPageBinding
import qualified Test.Headless.Craft.BillReconcile as CraftBillReconcile
import qualified Test.Headless.Power.Types as PowerTypes
import qualified Test.Headless.Power.Placement as PowerPlacement
import qualified Test.Headless.Power.Demolition as PowerDemolition
import qualified Test.Headless.Power.Network as PowerNetwork
import qualified Test.Headless.Language.Semantic as LanguageSemantic
import qualified Test.Headless.Language.Generated as LanguageGenerated
import qualified Test.Headless.Language.Suggest as LanguageSuggest
import qualified Test.Headless.Language.Etymology as LanguageEtymology
import qualified Test.Headless.Language.EtymologyPageScope
    as LanguageEtymologyPageScope
import qualified Test.Headless.Blood.Types as BloodTypes
import qualified Test.Headless.Blood.Texture as BloodTexture
import qualified Test.Headless.Blood.Impact as BloodImpact
import qualified Test.Headless.Blood.Trail as BloodTrail
import qualified Test.Headless.Blood.Teardown as BloodTeardown
import qualified Test.Headless.Blood.LuaApi as BloodLuaApi
import qualified Test.Headless.UI.CreateWorldControls as CreateWorldControls
import qualified Test.Headless.UI.Tooltip as UITooltip
import qualified Test.Headless.UI.InputOwnership as UIInputOwnership
import qualified Test.Headless.UI.ZoomBandInputGate as UIZoomBandInputGate
import qualified Test.Headless.UI.HudHoverGate as UIHudHoverGate
import qualified Test.Headless.UI.ItemInfoRowSelection as UIItemInfoRowSelection
import qualified Test.Headless.UI.UnitInfoRowSelection as UIUnitInfoRowSelection
import qualified Test.Headless.UI.ElementInputPolicy as UIElementInputPolicy
import qualified Test.Headless.UI.ControlActivation as UIControlActivation
import qualified Test.Headless.UI.HierarchyOwnership as UIHierarchyOwnership
import qualified Test.Headless.UI.FocusNavigation as UIFocusNavigation
import qualified Test.Headless.UI.DropdownCommit as UIDropdownCommit
import qualified Test.Headless.UI.Clipping as UIClipping
import qualified Test.Headless.UI.ListScrollSync as UIListScrollSync
import qualified Test.Headless.UI.InteractiveBounds as UIInteractiveBounds
import qualified Test.Headless.UI.PopupPlacement as UIPopupPlacement
import qualified Test.Headless.Event.PlayerEventProgress as PlayerEventProgress
import qualified Test.Headless.Event.PopupCoordPage as PopupCoordPage
import qualified Test.Headless.UI.PopupQueueTeardown as UIPopupQueueTeardown
import qualified Test.Headless.UI.RandboxContainment as UIRandboxContainment
import qualified Test.Headless.UI.ResponsiveMenus as UIResponsiveMenus
import qualified Test.Headless.UI.ResponsiveGameplay as UIResponsiveGameplay
import qualified Test.Headless.UI.SettingsDefaultsKeybinds
    as UISettingsDefaultsKeybinds
import qualified Test.Headless.UI.SettingsRevert
    as UISettingsRevert
import qualified Test.Headless.UI.TutorialHud as UITutorialHud
import qualified Test.Headless.UI.UnicodeTextEditing as UIUnicodeTextEditing
import qualified Test.Headless.Lua.DragSelectDeferred as LuaDragSelectDeferred
import qualified Test.Headless.Lua.DebugGrab as LuaDebugGrab
import qualified Test.Headless.Lua.ChopGesture as LuaChopGesture
import qualified Test.Headless.Lua.ChopDesignationClaim as LuaChopDesignationClaim
import qualified Test.Headless.Lua.MineDesignationEligibility as LuaMineDesignationEligibility
import qualified Test.Headless.Lua.ChopFellXp as LuaChopFellXp
import qualified Test.Headless.Lua.TextWrapping as LuaTextWrapping
import qualified Test.Headless.Lua.GroupedLogRetention as LuaGroupedLogRetention
import qualified Test.Headless.Lua.TextTruncation as LuaTextTruncation
import qualified Test.Headless.Lua.WidthTruncation as LuaWidthTruncation
import qualified Test.Headless.Lua.ShellInput as LuaShellInput
import qualified Test.Headless.Lua.RandomStream as LuaRandomStream
import qualified Test.Headless.Lua.ConsoleTableKeys as LuaConsoleTableKeys
import qualified Test.Headless.Lua.LogSource as LuaLogSource
import qualified Test.Headless.Lua.InjuryNarration as LuaInjuryNarration
import qualified Test.Headless.UI.Slider as UISlider
import qualified Test.Headless.UI.BarFillColor as UIBarFillColor
import qualified Test.Headless.UI.ClickCorrelation as UIClickCorrelation
import qualified Test.Headless.UI.TransferContextMenu as UITransferContextMenu
import qualified Test.Headless.UI.ItemList as UIItemList
import qualified Test.Headless.UI.ContainerWindowStack as UIContainerWindowStack
import qualified Test.Headless.Load.ReplacementTeardown as LoadReplacementTeardown
import qualified Test.Headless.UI.TransferGestures as UITransferGestures
import qualified Test.Headless.UI.ConsumableGesture as UIConsumableGesture
import qualified Test.Headless.UI.TransferSession as UITransferSession
import qualified Test.Headless.World.Calendar as Calendar
import qualified Test.Headless.World.SubMinuteClock as SubMinuteClock
import qualified Test.Headless.World.FloraGrowth as FloraGrowth
import qualified Test.Headless.River.CalderaHazard as RiverCalderaHazard
import qualified Test.Headless.River.InlandSources as RiverInlandSources
import qualified Test.Headless.World.Render.FrontWallLift as FrontWallLift
import qualified Test.Headless.World.Render.StructureRotation as StructureRotation
import qualified Test.Headless.World.Render.GroundItemSeam as GroundItemSeam
import qualified Test.Headless.World.Render.StructureSeam as StructureSeam
import qualified Test.Headless.World.Render.PickSeam as PickSeam
import qualified Test.Headless.World.Render.QuadSnapshot as QuadSnapshot
import qualified Test.Headless.World.Render.SceneStats as SceneStats
import qualified Test.Headless.World.Render.SolarAttribution as SolarAttribution
import qualified Test.Headless.World.Render.DesignationFaceMap as DesignationFaceMap
import qualified Test.Headless.World.DesignationSeam as DesignationSeam
import qualified Test.Headless.World.DigDomain as DigDomain
import qualified Test.Headless.World.Chop.Selection as ChopSelection
import qualified Test.Headless.World.Chop.Authority as ChopAuthority
import qualified Test.Headless.World.Chop.TagPolicy as ChopTagPolicy
import qualified Test.Headless.World.FloraIdentity as FloraIdentity
import qualified Test.Headless.World.FloraOrder as FloraOrder
import qualified Test.Headless.World.CropPlant as CropPlant
import qualified Test.Headless.World.StructureStage as StructureStage
import qualified Test.Headless.World.StructurePaletteResidue as StructurePaletteResidue
import qualified Test.Headless.Structure.ArtCatalog as StructureArtCatalog
import qualified Test.Headless.Structure.ConstructionFrames as StructureConstructionFrames
import qualified Test.Headless.Structure.ConstructionPacks as StructureConstructionPacks
import qualified Test.Headless.Structure.DestructionFrames as StructureDestructionFrames
import qualified Test.Headless.Structure.DestructionPacks as StructureDestructionPacks
import qualified Test.Headless.World.StructureDestruction as WorldStructureDestruction
import qualified Test.Headless.World.Render.SlopeFacing as SlopeFacing
import qualified Test.Headless.World.Render.FluidLevels as RenderFluidLevels
import qualified Test.Headless.World.Render.SideFace as RenderSideFace
import qualified Test.Headless.World.Render.ZTrackSeam as ZTrackSeam
import qualified Test.Headless.World.Render.SlopeBit as RenderSlopeBit
import qualified Test.Headless.World.Render.FluidLevelMasks as FluidLevelMasks
import qualified Test.Headless.World.Render.ZoomBakeUV as ZoomBakeUV
import qualified Test.Headless.Render.ViewportGuard as ViewportGuard
import qualified Test.Headless.Render.QuadVertices as QuadVertices
import qualified Test.Headless.Graphics.BindlessRebind as BindlessRebind
import qualified Test.Headless.Graphics.TextureSamplerPolicy as TextureSamplerPolicy
import qualified Test.Headless.Graphics.BindlessRelease as BindlessRelease
import qualified Test.Headless.Graphics.BindlessPublish as BindlessPublish
import qualified Test.Headless.Lua.AssetFailure as LuaAssetFailure
import qualified Test.Headless.Lua.MessageStrictness as LuaMessageStrictness
import qualified Test.Headless.Core.ConfigState as ConfigState
import qualified Test.Headless.Core.ConfigWrite as ConfigWrite
import qualified Test.Headless.Core.Queue as CoreQueue
import qualified Test.Headless.Core.LogCategoryEnv as LogCategoryEnv
import qualified Test.Headless.Core.FixtureLogging as FixtureLogging
import qualified Test.Headless.Core.LogMonad as LogMonad
import qualified Test.Headless.Core.LogParity as LogParity
import qualified Test.Headless.Core.LogThresholdEnv as LogThresholdEnv
import qualified Test.Headless.Core.LoopStartup as LoopStartup
import qualified Test.Headless.Core.MonotonicClock as MonotonicClock
import qualified Test.Headless.Core.StepProtocol as StepProtocol
import qualified Test.Headless.Core.ShutdownAtlasRelease as ShutdownAtlasRelease
import qualified Test.Headless.Core.WorkerLifecycle as WorkerLifecycle
import qualified Test.Headless.Core.DebugListener as DebugListener
import qualified Test.Headless.Core.DebugSocket as DebugSocket
import qualified Test.Headless.Core.DebugConsoleStop as DebugConsoleStop
import qualified Test.Headless.App.Cli as AppCli
import qualified Test.Headless.App.ChunkRegion as AppChunkRegion
import qualified Test.Headless.App.DumpSettleWait as DumpSettleWait
import qualified Test.Headless.World.FluidDiagnostics as FluidDiagnostics
import qualified Test.Headless.App.PreviewConfig as PreviewConfig
import qualified Test.Headless.App.ResourceRoot as AppResourceRoot
import qualified Test.Headless.Camera.Finite as CameraFinite
import qualified Test.Headless.Camera.GotoClamp as GotoClamp
import qualified Test.Headless.Camera.GotoLoad as GotoLoad
import qualified Test.Headless.Camera.ZoomScroll as ZoomScroll
import qualified Test.Headless.Scene.BatchMerge as BatchMerge
import qualified Test.Headless.Render.PanMargin as PanMargin
import qualified Test.Headless.Location.Bounds as LocationBounds
import qualified Test.Headless.Building.PageBinding as BuildingPageBinding
import qualified Test.Headless.Building.PortalSpawnBinding as BuildingPortalSpawnBinding
import qualified Test.Headless.Building.FootprintExclusivity
    as BuildingFootprintExclusivity
import qualified Test.Headless.Building.Placement as BuildingPlacement
import qualified Test.Headless.Building.RemoteWarning as BuildingRemoteWarning
import qualified Test.Headless.Building.AssetSchema as BuildingAssetSchema
import qualified Test.Headless.Building.CameraFacing as BuildingCameraFacing
import qualified Test.Headless.Building.DestructionPresentation as BuildingDestruction
import qualified Test.Headless.Building.Ghost as BuildingGhost
import qualified Test.Headless.Building.MachineShopConstruction
    as MachineShopConstruction
import qualified Test.Headless.Building.WorkbenchConstruction
    as WorkbenchConstruction
import qualified Test.Headless.Save.AutosaveGuards as AutosaveGuards
import qualified Test.Headless.Save.AutosaveListing as AutosaveListing
import qualified Test.Headless.Save.AutosaveRotation as AutosaveRotation
import qualified Test.Headless.Save.ListingFailureBoundary as ListingFailureBoundary
import qualified Test.Headless.Save.MenuListingOrder as MenuListingOrder
import qualified Test.Headless.Save.Barrier as SaveBarrier
import qualified Test.Headless.Save.OwnerPark as SaveOwnerPark
import qualified Test.Headless.Load.Status as LoadStatus
import qualified Test.Headless.Load.Terminalize as LoadTerminalize
import qualified Test.Headless.Save.Snapshot as SaveSnapshot
import qualified Test.Headless.Location.Discovery as LocationDiscovery
import qualified Test.Headless.World.LocationDiscovery as WorldLocationDiscovery
import qualified Test.Headless.Building.Knowledge as ContainerKnowledge
import qualified Test.Headless.Item.PortableKnowledge as PortableKnowledge
import qualified Test.Headless.Item.NestedContents as NestedContents
import qualified Test.Headless.Item.PortableWindow as PortableWindow
import qualified Test.Headless.Location.Instance as LocationInstance
import qualified Test.Headless.Location.SignificantContents as LocationSignificantContents
import qualified Test.Headless.Location.ContainerShells as LocationContainerShells
import qualified Test.Headless.Location.Naming as LocationNaming
import qualified Test.Headless.River.Naming as RiverNaming
import qualified Test.Headless.Location.LootDeterminism as LocationLootDeterminism
import qualified Test.Headless.Loot.Profiles as LootProfiles
import qualified Test.Headless.Loot.Realization as LootRealization
import qualified Test.Headless.Location.MapIcons as LocationMapIcons
import qualified Test.Headless.Location.Stamping as LocationStamping
import qualified Test.Headless.Location.StampCommit as LocationStampCommit
import qualified Test.Headless.Tutorial.Definitions as TutorialDefinitions
import qualified Test.Headless.Lua.SaveModules as LuaSaveModules
import qualified Test.Headless.Lua.SharedHelpers as LuaSharedHelpers
import qualified Test.Headless.Lua.SaveBridge as LuaSaveBridge
import qualified Test.Headless.Lua.TutorialProgress as LuaTutorialProgress
import qualified Test.Headless.Lua.TutorialEvaluation as LuaTutorialEvaluation
import qualified Test.Headless.Lua.UnitAiLocations as LuaUnitAiLocations
import qualified Test.Headless.Lua.UnitAiCanteenDrain as LuaUnitAiCanteenDrain
import qualified Test.Headless.Lua.UnitAiHold as LuaUnitAiHold
import qualified Test.Headless.Lua.UnitAiWaterCanteens as LuaUnitAiWaterCanteens
import qualified Test.Headless.Lua.CombatLogRefusal as LuaCombatLogRefusal
import qualified Test.Headless.Lua.UnitAiCombatMove as LuaUnitAiCombatMove
import qualified Test.Headless.Lua.UnitAiEncounter as LuaUnitAiEncounter
import qualified Test.Headless.Lua.UnitAiStall as LuaUnitAiStall
import qualified Test.Headless.Lua.UnitAiHarvest as LuaUnitAiHarvest
import qualified Test.Headless.Lua.UnitAiYieldProximity as LuaUnitAiYieldProximity
import qualified Test.Headless.Lua.UnitAiLogisticsTargets as LuaUnitAiLogisticsTargets
import qualified Test.Headless.Lua.BuilderEligibility as LuaBuilderEligibility
import qualified Test.Headless.Building.ConstructionEligibility as BuildingConstructionEligibility
import qualified Test.Headless.Lua.UnitAiPageTargets as LuaUnitAiPageTargets
import qualified Test.Headless.Lua.UnitAiLoadReset as LuaUnitAiLoadReset
import qualified Test.Headless.Lua.FarmDesignationClaim as LuaFarmDesignationClaim
import qualified Test.Headless.Lua.UnitAiReconcile as LuaUnitAiReconcile
import qualified Test.Headless.Lua.SessionTeardown as LuaSessionTeardown
import qualified Test.Headless.Lua.BuildingSpawnSentinel as LuaBuildingSpawnSentinel
import qualified Test.Headless.Lua.WorkClaimCapacity as LuaWorkClaimCapacity
import qualified Test.Headless.Lua.CraftCycleReplenishment as LuaCraftCycleReplenishment
import qualified Test.Headless.Lua.CraftBillQueuePriority as LuaCraftBillQueue
import qualified Test.Headless.Lua.WorkClockBounds as LuaWorkClockBounds
import qualified Test.Headless.Lua.Faction as LuaFaction
import qualified Test.Headless.Unit.Faction as UnitFaction
import qualified Test.Headless.Unit.StandardSpawn as UnitStandardSpawn
import qualified Test.Headless.Unit.FactionCatalogue as UnitFactionCatalogue
import qualified Test.Headless.Unit.FactionProfile as UnitFactionProfile
import qualified Test.Headless.Capability.Building as CapabilityBuilding
import qualified Test.Headless.Capability.ContentRegistriesView as CapabilityContentRegistriesView
import qualified Test.Headless.Capability.Events as CapabilityEvents
import qualified Test.Headless.Capability.Input as CapabilityInput
import qualified Test.Headless.Capability.Render as CapabilityRender
import qualified Test.Headless.Capability.RenderHandoff as CapabilityRenderHandoff
import qualified Test.Headless.Capability.SaveLoad as CapabilitySaveLoad
import qualified Test.Headless.Capability.Ui as CapabilityUi
import qualified Test.Headless.Capability.UnitCombat as CapabilityUnitCombat
import qualified Test.Headless.Capability.WorldSim as CapabilityWorldSim

main ∷ IO ()
main = do
    (measureConfig0, measureForest) ← MeasureRunner.evalSpec
        MeasureRunner.defaultConfig measuredSpec
    measureConfig ← MeasureRunner.readConfig measureConfig0
        ∘ (["--match", "/@G456/", "--match", "/@G457/", "--match", "/@G463/", "--match", "/@G579/", "--match", "/@G588/", "--match", "/@G589/", "--match", "/@G595/", "--match", "/@G598/"] ⧺) =≪ MeasureEnv.getArgs
    let measureTee mk fc = do
            base ← mk fc
            pure $ \event → do
                () ← base event
                let rendered = show event
                when ("ItemDone " `MeasureList.isPrefixOf` rendered) $
                    MeasureIO.hPutStrLn MeasureIO.stdout
                        ("@@HSPEC-ITEM " ⧺ rendered ⧺ " @@END")
        measureConfig' = measureConfig
            { MeasureRunner.configFormat =
                measureTee ⊚ MeasureRunner.configFormat measureConfig }
    MeasureEnv.withArgs [] (MeasureRunner.runSpecForest measureForest
        measureConfig') ≫= MeasureRunner.evaluateResult

measuredSpec ∷ Spec
measuredSpec = do
    describe "@G456" $ do
        describe "World.ZoomMap.Artifact" ZoomArtifact.spec
    describe "@G457" $ do
        ZoomOceanFill.spec
        -- ONE engine for all worldgen specs. Worlds are memoized by
        -- (seed, size, plateCount) via Test.Headless.Harness.sharedWorld
        -- — generation is the entire cost of this suite, so specs share
        -- worlds instead of regenerating identical ones per module
        -- (was 16 generations / ~185 s; now ~6 / well under a minute).
    describe "@G463" $ do
        aroundAll withHeadlessEngine $ do
            describe "@S464" $ do
                describe "World Generation" WorldGen.spec
            describe "@S465" $ do
                describe "World.SelectTileZ" SelectTileZ.spec
            describe "@S466" $ do
                UITransferContextMenu.spec
            describe "@S467" $ do
                UIItemList.spec
            describe "@S468" $ do
                describe "World.ActionOutcome" ActionOutcome.spec
            describe "@S469" $ do
                ChunkIdentity.spec
            describe "@S470" $ do
                ChunkQueueFrame.spec
            describe "@S471" $ do
                describe "Geology" Geology.spec
            describe "@S472" $ do
                describe "Chunk/Fast Parity" Parity.spec
            describe "@S473" $ do
                describe "Biome Flatness" Flatness.spec
            describe "@S474" $ do
                describe "Column Exposure" Exposure.spec
            describe "@S475" $ do
                describe "Zoom/Detail Parity" ZoomParity.spec
            describe "@S476" $ do
                ZoomArtifact.worldSpec
            describe "@S477" $ do
                MapPyramid.worldSpec
            describe "@S478" $ do
                describe "Border Probe" BorderProbe.spec
            describe "@S479" $ do
                Climate.spec
            describe "@S480" $ do
                describe "Asset.TextureFallback" TextureFallback.spec
                -- Not worldgen -- the Lua half binds the real engine.loadTexture
                -- to this env and reads what it queues, which is the only honest
                -- proof of #2075's caller-declared classification.
            describe "@S484" $ do
                describe "texture sampler policy" TextureSamplerPolicy.spec
                -- Not worldgen — needs the live EngineEnv's queues/refs to
                -- drive the #697 fence relay by hand (harness runs neither
                -- the input nor the Lua thread, so the queues are the test's).
            describe "@S488" $ do
                describe "Input.Followup" InputFollowup.spec
                -- #1927: a split hold's modifier lifetime is a property of the
                -- ownership record the INPUT THREAD keeps between two
                -- independent verb calls, so it can only be asserted as state
                -- against a live env — same technique as Input.Followup above.
                -- The name keeps `--match "Input.Inject"` (the issue's focused
                -- acceptance command) selecting it alongside the pure
                -- sequence-shape group.
            describe "@S496" $ do
                describe "Input.Inject ownership" InputInjectOwnership.spec
                -- Same technique as Input.Followup above: no world dependency
                -- at all, just the live EngineEnv's queues/refs to construct a
                -- real Lua backend and drive processLuaMsg directly.
            describe "@S500" $ do
                describe "Lua.DebugQueue" LuaDebugQueue.spec
                -- Same technique as Lua.DebugQueue above (#1961): the scene
                -- handlers are GPU-free, so the real luaToEngineQueue →
                -- processLuaMessages → Message.Scene route runs headless
                -- against the live env. The spec installs and restores its
                -- own active scene rather than leaking one into this shared
                -- environment.
            describe "@S507" $ do
                describe "Lua.SceneText" LuaSceneText.spec
                -- Same technique (#2192): the sprite half of the scene route —
                -- a spawn through processLuaMessages, the manager's own
                -- rebuild, then the pure frame assembly — against the live env.
            describe "@S511" $ do
                FrameAssembly.envSpec
                -- Same technique as Lua.DebugQueue above: the live EngineEnv's
                -- queues/refs are only there to build a real Lua backend, whose
                -- console boundary is what #1955's key contract lives on.
            describe "@S515" $ do
                describe "debug console table keys" LuaConsoleTableKeys.spec
            describe "@S516" $ do
                LuaUnitAiReconcile.envSpec
            describe "@S517" $ do
                describe "Lua.RenderQueue" LuaRenderQueue.spec
            describe "@S518" $ do
                describe "Lua.PreviewGeneration" LuaPreviewGeneration.spec
            describe "@S519" $ do
                describe "Lua.PauseGate" LuaPauseGate.spec
            describe "@S520" $ do
                describe "Lua.ScriptState" LuaScriptState.spec
            describe "@S521" $ do
                LuaTickInterval.spec
            describe "@S522" $ do
                LuaSchedulerFairness.spec
                -- Same technique as Input.Followup above: F4 (#730) Layer A's
                -- non-click producers live inside Engine.Input.Thread's real
                -- processInputs, driven directly against the live EngineEnv.
            describe "@S526" $ do
                describe "Input.LayerA" InputLayerA.spec
                -- Same technique again (#1693): the framebuffer-resize →
                -- swapchain-recreation request is decided entirely from the
                -- live env's framebufferSizeRef and the main-thread
                -- GraphicsState record, so the whole contract is provable with
                -- no GPU.
            describe "@S532" $ do
                describe "swapchain resize request" GraphicsSwapchainResize.spec
            describe "@S533" $ do
                ExactRiverWorld.spec
            describe "@S534" $ do
                describe "River.InlandSources" RiverInlandSources.spec
                -- Capability-projection aliasing (#891): pure handle-equality
                -- checks against the already-booted env — no worldgen, no
                -- mutation, so it rides the shared engine above.
            describe "@S538" $ do
                describe "Capability.Building projections" CapabilityBuilding.spec
                -- #1896 adds the one property a plain projection test cannot
                -- carry: the `ReadOnlyRef` wrapper ALIASES its handle rather
                -- than snapshotting it, so a write through the raw writer
                -- record is observed through the read-only view.
            describe "@S543" $ do
                describe "ReadOnlyRef and Capability.ContentRegistriesView projections"
                         CapabilityContentRegistriesView.spec
            describe "@S545" $ do
                describe "Capability.Events projections" CapabilityEvents.spec
            describe "@S546" $ do
                CapabilityAudio.spec
            describe "@S547" $ do
                describe "Capability.Input projections" CapabilityInput.spec
            describe "@S548" $ do
                describe "Capability.Render projections" CapabilityRender.spec
            describe "@S549" $ do
                describe "Capability.RenderHandoff projections" CapabilityRenderHandoff.spec
            describe "@S550" $ do
                describe "Capability.SaveLoad projections" CapabilitySaveLoad.spec
            describe "@S551" $ do
                describe "Capability.Ui projections" CapabilityUi.spec
            describe "@S552" $ do
                describe "Capability.UnitCombat projections" CapabilityUnitCombat.spec
            describe "@S553" $ do
                describe "Capability.WorldSim projections" CapabilityWorldSim.spec
                -- Same technique: no world dependency, just the live EngineEnv's
                -- content-registry refs projected through the real capability so
                -- the Lua-facing tutorial surface is exercised end to end (#957).
            describe "@S557" $ do
                TutorialDefinitions.luaSpec
            describe "@S558" $ do
                LuaTutorialEvaluation.luaSpec
                -- Same technique (#1946): the loot-table load-and-register
                -- boundary is entirely the live env's content-registry ref
                -- projected through the real capability, so it rides the
                -- shared engine and borrows/restores that one ref.
            describe "@S563" $ do
                LocationLootDeterminism.luaSpec
                -- Same technique (#2499): the loot-profile load-and-register
                -- boundary and its two read-only queries are entirely the live
                -- env's content-registry refs projected through the real
                -- capabilities, so this rides the shared engine and
                -- borrows/restores the two refs it seeds.
            describe "@S569" $ do
                LootProfiles.luaSpec
                -- Same technique again (#2502), plus one more borrowed ref: the
                -- seed `loot.simulate` measures against is the ACTIVE world
                -- page's, so this seeds the visible-page list with the shared
                -- canonical world and restores it.
            describe "@S574" $ do
                LootRealization.luaSpec
            -- Own engine (not the shared-worlds one above): the #707 save/load
            -- story snapshots and reloads EVERY live page, so an empty world
            -- manager keeps it scoped to its own cheap private w8 pages instead
            -- of re-restoring the shared worlds.
    describe "@G579" $ do
        aroundAll withHeadlessEngine $
            describe "World identity (#707)" WorldIdentity.spec
        -- #2021 (WML-3). The pure half needs no engine. The boundary half
        -- gets its OWN engine for the same reason "World identity" does: it
        -- creates private w8 pages and saves EVERY live page, which the
        -- shared-worlds engine above must not gain. Its saves publish to
        -- fixed slot names under the cwd-relative saves/, so the whole
        -- engine lifetime runs inside a scratch resource root, entered
        -- before boot and left after teardown (#2650).
    describe "@G588" $ do
        GeneratedIdentity.pureSpec
    describe "@G589" $ do
        aroundAll (withIsolatedResourceRoot ∘ withHeadlessEngine)
            GeneratedIdentity.spec
        -- #2020 (WML-2). The pure half needs no engine at all. The
        -- boundary half gets its OWN engine: it creates a private w8 page
        -- and saves it, which the shared-worlds engine above must not gain,
        -- for the same reason "World identity" is isolated.
    describe "@G595" $ do
        MapImagePlan.spec
        -- #2298 (WML-5). Pure and engine-free; the goldens that need a
        -- generated world are MapPyramid.worldSpec, above.
    describe "@G598" $ do
        MapPyramid.spec
        -- #2693 (WML-7). Format and storage only: scratch library roots,
        -- no engine and no generated world.
    describe "@G601" $ do
        describe "paged map artifact" PagedMapArtifact.spec
    describe "@G602" $ do
        aroundAll withHeadlessEngine MapImageAdmission.spec
        -- #2278. Own engine: it registers an out-of-tree material into the
        -- ONE process-global material registry and creates two private w8
        -- pages, neither of which the shared-worlds engine above may gain.
    describe "@G606" $ do
        aroundAll withHeadlessEngine MaterialRegistryMerge.spec
        -- Own engine (#1718): creates an arena page, which the shared-worlds
        -- engine above must not gain. Its describe names "Arena" so the
        -- issue's `--match "Arena"` acceptance command selects it alongside
        -- the pure contract below.
    describe "@G611" $ do
        aroundAll withHeadlessEngine $
            describe "Arena base seeding (#1718)" ArenaSeed.engineSpec
        -- Own engine (#1246): writes a populated transfer-order store into a
        -- live page's WorldState and saves it, which the shared-worlds
        -- engine above must not see. Registered under the SAME describe as
        -- the pure contract gate so `--match "persistence contract"` covers
        -- both halves -- the codec round trip and the live capture/restore.
    describe "@G618" $ do
        aroundAll withHeadlessEngine $
            describe "persistence contract" WorldTransferOrders.spec
        -- Own engine (#2232): generates a private w8 page AND an arena page,
        -- then FLUSHES the sim queue to read what init seeded -- both of
        -- which the shared-worlds engine above must not see. The flush is
        -- only sound because no sim worker drains that queue in a headless
        -- fixture, so it must not share an engine with anything else that
        -- reads it.
    describe "@G626" $ do
        aroundAll withHeadlessEngine $
            describe "sim chunk admission" SimAdmission.spec
        -- Own engine (#1596): both halves EDIT their own private w8 pages
        -- and hand-deliver WorldApplyFluids batches to the live world
        -- thread, which the shared-worlds engine above must not see. The
        -- save half is registered under the SAME "persistence contract"
        -- describe as the transfer-order gate above, and for the same
        -- reason -- it is the live capture/replay half of that contract,
        -- which no pure codec test can reach.
    describe "@G635" $ do
        aroundAll withHeadlessEngine $ do
            describe "@S636" $ do
                FluidWritebackStaleness.spec
            describe "@S637" $ do
                describe "persistence contract" FluidWritebackStaleness.saveSpec
            -- Own engine (#2485): each example generates its own private w8
            -- page, hand-delivers reaction results to the live world thread, and
            -- reads the commit's sim handoff off an UNDRAINED sim queue -- none
            -- of which the shared-worlds engine may see.
    describe "@G642" $ do
        aroundAll withHeadlessEngine Solidification.spec
        -- Own engine (#2490): the occupant half of the same commit. It
        -- spawns units and drains the unit queue BY HAND against the live
        -- world thread, which the shared-worlds engine must not see either.
    describe "@G646" $ do
        aroundAll withHeadlessEngine SolidificationOccupants.spec
        -- Own engine (#2477): DESTROYS and re-creates a page under the same
        -- id, and drives the sim's own command handler and emit step by
        -- hand against the live world thread -- none of which the
        -- shared-worlds engine above may see.
    describe "@G651" $ do
        aroundAll withHeadlessEngine FluidWritebackIncarnation.spec
        -- Own engine (#1858): the only example in the suite that PUBLISHES
        -- a loaded session, which replaces every live page -- so it must
        -- never run inside the shared-worlds engine, and nothing may be
        -- registered after it in this block.
    describe "@G656" $ do
        aroundAll withHeadlessEngine CropPlant.saveSpec
        -- Own engine: #913's failure-report cases queue a WorldSave for a
        -- page that does not exist, and assert on the shared event log --
        -- both of which would be noise (and, for the log, a source of
        -- cross-talk) inside the shared-worlds engine above.
    describe "@G661" $ do
        aroundAll withHeadlessEngine $
            describe "autosave engine guards (#913)" AutosaveGuards.spec
        -- Own engine: the live transfer API (#1085) WRITES real units,
        -- buildings and items into the manager refs to exercise all four
        -- mutation paths, which would corrupt the shared-worlds engine
        -- above (same precedent as World identity / autosave guards).
    describe "@G667" $ do
        aroundAll withHeadlessEngine UnitTransferApi.spec
        -- Own engine (#1733): the live unit.addXP boundary WRITES the
        -- unit manager ref (and seeds a deliberately corrupt stat map),
        -- so it cannot share the worldgen engine above.
    describe "@G671" $ do
        aroundAll withHeadlessEngine UnitAddXpApi.spec
        -- Own engine (#1605): the live unit.moveTo boundary swaps the
        -- engine's logger to capture the warning it emits and drains the
        -- unit command queue, so it cannot share the worldgen engine.
    describe "@G675" $ do
        aroundAll withHeadlessEngine PathingMoveToApi.spec
        -- Own engine for the same reasons (#2290): the motion-argument
        -- domain gate swaps the logger, drains and refills the unit command
        -- queue, and REWRITES the unit manager to install its own def and
        -- instance, so it can share neither the worldgen engine nor
        -- another spec's managers.
    describe "@G681" $ do
        aroundAll withHeadlessEngine PathingMotionArgs.spec
        -- Own engine for the same reason (#1247): the order executor writes
        -- the unit/building manager refs AND installs its own two-page world
        -- manager so each page brings its own live wsTransferOrdersRef.
        -- Its describe begins "Unit transfer Lua API" so that --match reaches
        -- the contract verbs and the order verbs in one gate.
    describe "@G687" $ do
        aroundAll withHeadlessEngine UnitTransferOrderApi.spec
        -- Own engine (#1673): the four LAX cargo verbs WRITE the unit and
        -- building manager refs and install their own two-page world
        -- manager, so like the two specs above they cannot share the
        -- worldgen engine.
    describe "@G692" $ do
        aroundAll withHeadlessEngine UnitCargoApi.spec
        -- Own engine (#1969): the getWounds schema spec WRITES the unit
        -- manager ref and the infection catalogue, so like the specs above
        -- it cannot share the worldgen engine.
    describe "@G696" $ do
        aroundAll withHeadlessEngine UnitWoundsApi.spec
        -- Own engine (#2297): the medical reach spec WRITES the unit
        -- manager ref and installs its own two-page world manager, for the
        -- same reason as the cargo spec above.
    describe "@G700" $ do
        aroundAll withHeadlessEngine UnitMedicalReach.spec
        -- Own engine (#2302): the medical kit-instance spec WRITES the
        -- unit and item manager refs and installs its own world manager,
        -- for the same reason as the reach spec above.
    describe "@G704" $ do
        aroundAll withHeadlessEngine UnitMedicalKitInstance.spec
    describe "@G705" $ do
        aroundAll withHeadlessEngine UnitMedicalWoundIdentity.spec
    describe "@G706" $ do
        aroundAll withHeadlessEngine UnitMedicAntibioticSupply.spec
        -- Own engine (#1205): the live power.placeNode path WRITES the
        -- unit/building manager refs and installs its own two-page world
        -- manager, so it cannot share the worldgen engine above.
    describe "@G710" $ do
        aroundAll withHeadlessEngine PowerPlacement.spec

        -- #1238: the two nested item-container reads the container-window
        -- stack opens a level from, driven through the registered Lua API
        -- against real live refs.
    describe "@G715" $ do
        aroundAll withHeadlessEngine NestedContents.spec
        -- #2527: the portable level of the same stack. Its own engine, for
        -- the reason PortableKnowledge.LuaApi has one — it installs a whole
        -- synthetic page set, an item registry and the real HUD, and it
        -- drives real world ticks to drain the observation queue.
    describe "@G720" $ do
        PortableWindow.spec
        -- Own engine for the same reason (#1206): the demolition gate
        -- installs its own two-page world manager and drives the real
        -- building-command drain, which would disturb the shared engine.
    describe "@G724" $ do
        aroundAll withHeadlessEngine PowerDemolition.spec
        -- Own engine for the same reason (#1680): the craft-bill claimant
        -- sweep installs its own two-page world manager, rewrites the unit
        -- manager, and pins the engine paused. It needs the world worker
        -- RUNNING -- the whole gate is that the REAL tickWorldTime performs
        -- the reconciliation -- and its pages carry no gen params, so that
        -- worker skips them for chunk loading.
    describe "@G731" $ do
        aroundAll withHeadlessEngine CraftBillReconcile.spec
        -- Own engine (#1585): the blood.gpuHandles gate installs its own
        -- single-page world manager and writes that page's blood handle map
        -- plus the engine-wide texture-size cache, which would disturb the
        -- shared worldgen engine above.
    describe "@G736" $ do
        aroundAll withHeadlessEngine BloodLuaApi.spec
        -- Own engine for the same reason: the #1208 ground-ownership gate
        -- installs TWO live pages and rewrites the unit/world manager refs
        -- to put a unit on the non-active one.
    describe "@G740" $ do
        aroundAll withHeadlessEngine GroundPageOwnership.spec
        -- Own engine for the same reason, and against the same two-page
        -- fixture: #1666's pickup-order gate keeps page A active while the
        -- carrier and its target sit on live, non-active page B, and drives
        -- the production scripts/unit_ai_pickup.lua over it.
    describe "@G745" $ do
        aroundAll withHeadlessEngine LuaUnitAiPickupPage.spec
        -- Own engine for the same reason (#2553): the food-harvest identity
        -- gate installs its own single-page world manager carrying two
        -- co-tenant plants on ONE tile, rewrites the flora catalog, item
        -- registry and unit manager per example, and drives the production
        -- scripts/unit_ai_needs.lua and unit_ai_harvest.lua over the
        -- engine's real flora query and harvest verbs.
    describe "@G752" $ do
        aroundAll withHeadlessEngine LuaFoodHarvestTarget.spec
        -- Own engine for the same reason (#1737): the repair AI's ground
        -- rung is judged against two live pages carrying the SAME gid, and
        -- it drives the production scripts/unit_ai_repair.lua +
        -- unit_ai_repair_target.lua over the engine's real ground,
        -- inventory and page APIs.
    describe "@G758" $ do
        aroundAll withHeadlessEngine LuaUnitAiRepairGround.spec
        -- Own engine for the same reason (#1599): the pause-speed gate
        -- installs its own two-page world manager, rewrites wmVisible
        -- mid-example, and drives the real scripts/pause.lua against the
        -- live engine. Its pages carry NO gen params, so the real world
        -- worker skips them -- but the worker has to be RUNNING, because one
        -- example needs the queued world.setTimeScale drained.
    describe "@G765" $ do
        aroundAll withHeadlessEngine PauseSpeed.spec
        -- Own engine, world-thread-FREE (#2280): the time-scale domain gate
        -- proves that a REFUSED world.setTimeScale enqueues nothing, which is
        -- only observable while no world worker is draining worldQueue. It
        -- installs its own single-page manager and finishes each accepted
        -- call by invoking the production command handler directly.
        -- #2415: the scheduler's load cutover needs a world-thread-FREE
        -- engine. A successful cutover ends in commitLoadPublish, which
        -- queues WorldLoadPublish; a running world worker would pick that
        -- up and try to publish a staged session that does not exist.
    describe "@G775" $ do
        aroundAll withHeadlessEngineNoWorld LuaSchedulerFairness.cutoverSpec
    describe "@G776" $ do
        aroundAll withHeadlessEngineNoWorld TimeScaleDomain.spec
        -- #2310: which PAGE bulk chunk work is admitted to, and which page
        -- the wait watches. The defect lives entirely in the window between
        -- a world.show being enqueued and being applied, so this spec needs
        -- an engine with no world worker draining worldQueue: the show then
        -- sits unapplied for as long as an example needs, and each page's
        -- init queue is exactly what a producer left there.
    describe "@G783" $ do
        aroundAll withHeadlessEngineNoWorld ChunkPageBinding.spec
        -- #2288: the world-generation float domain. The pure half -- the
        -- shared leaf tables, the YAML resolution and the save-side repair --
        -- needs no engine at all. The Lua half gets its OWN engine, and is
        -- world-thread-FREE: it rewrites worldGenConfigRef under every
        -- example and never inits a world, so a running worker would only be
        -- a source of interference.
    describe "@G790" $ do
        GenConfigDomain.pureSpec
    describe "@G791" $ do
        aroundAll withHeadlessEngineNoWorld GenConfigDomain.spec
        -- The staging half needs a WORLD-thread-free engine too, but a
        -- different one: it drives World.Load.Stage.stageSession against a
        -- forged one-page save, so it must not gain (or disturb) the shared
        -- worlds engine's pages. Its page is an arena page, so staging
        -- rebuilds flat chunks instead of generating a world.
    describe "@G797" $ do
        aroundAll withHeadlessEngineNoWorld GenConfigDomain.stagingSpec
        -- #2339: the canonical form of a stored world date, at its two
        -- ingresses. Both halves get their OWN world-thread-free engine, for
        -- the same reasons the gen-domain pair above does. The setter half
        -- installs its own single-page manager, reads back the queue a
        -- world.setDate left there, and finishes each call through the
        -- production handler -- a running worker would drain the queue and
        -- rewrite the page's clock underneath it. The staging half drives
        -- World.Load.Stage.stageSession against a forged one-page arena
        -- save, so it must not gain or disturb the shared engine's pages.
    describe "@G807" $ do
        describe "World.Calendar" $ do
            describe "@S808" $ do
                aroundAll withHeadlessEngineNoWorld Calendar.setterSpec
            describe "@S809" $ do
                aroundAll withHeadlessEngineNoWorld Calendar.stagingSpec
            -- #2471: the retained sub-minute remainder, split the same way. Both
            -- halves need their OWN world-thread-free engine: the tick half
            -- installs its own page set and drives worldTickWith directly (a live
            -- world worker would tick those pages on its own clock and race every
            -- assertion), and the staging half drives World.Load.Stage.stageSession
            -- against a forged one-page arena save, so neither may gain or
            -- disturb the shared engine's pages.
    describe "@G817" $ do
        describe "World.SubMinuteClock" $ do
            describe "@S818" $ do
                aroundAll withHeadlessEngineNoWorld SubMinuteClock.tickSpec
            describe "@S819" $ do
                aroundAll withHeadlessEngineNoWorld SubMinuteClock.stagingSpec
            -- #2307: the saved-equipment-slot reconciliation, split the same
            -- way. The pure half needs no engine; the staging half gets its OWN
            -- world-thread-free engine for the same reason the gen-domain one
            -- above does -- it rewrites the item/equipment-class/unit-def
            -- registries and drives stageSession against a forged one-page
            -- arena save, so it must not gain or disturb the shared engine's
            -- pages.
    describe "@G827" $ do
        EquipmentReconcile.pureSpec
    describe "@G828" $ do
        aroundAll withHeadlessEngineNoWorld EquipmentReconcile.stagingSpec
        -- Own engine for the same reason (#1593): the unit-simulation
        -- page-ownership gate installs its own three-page world manager and
        -- rewrites the unit manager to put a unit on each. WORLD-THREAD-FREE
        -- for the same reason the etymology gate below is: its pages are
        -- hand-built emptyWorldStates carrying defaultWorldGenParams, whose
        -- wgpPlates is empty, so a real world worker picking one up for
        -- chunk loading would die in twoNearestPlates.
    describe "@G836" $ do
        aroundAll withHeadlessEngineNoWorld SimPageOwnership.spec
        -- Own engine for the same reason (#1265): the etymology page-scope
        -- gate installs its own two-page world manager, one page inactive,
        -- to drive world.getEtymology across the target/recurrence boundary.
        -- Named so `--match "Language etymology"` reaches it alongside the
        -- pure suite below.
        --
        -- WORLD-THREAD-FREE (#1362): those pages are hand-built
        -- emptyWorldStates and the spec sends no world command, but the
        -- visible one carries defaultWorldGenParams -- whose wgpPlates is
        -- empty -- so a real worker picked it up for chunk loading and
        -- died in twoNearestPlates on the FIRST example, leaving every
        -- later one running against a CleaningUp engine while hspec
        -- reported green. The spec never needed the worker.
    describe "@G850" $ do
        aroundAll withHeadlessEngineNoWorld $
            describe "Language etymology (page scope)"
                LanguageEtymologyPageScope.spec
        -- Own engine (not the shared-worlds one above): needs a real
        -- pixel hit-test against loaded tile data (renderWorldCursorQuads),
        -- so it generates its own cheap private w8 page rather than sharing
        -- or disturbing the worldgen specs' engine/camera state.
    describe "@G857" $ do
        aroundAll withHeadlessEngine SelectChunk.sharedSpec
        -- Own engine for the same reason: it shows a private w8 page and
        -- teleports the camera, so the world worker generates against THAT
        -- page's window. Sharing the worldgen engine would hand every other
        -- spec a different active world and a moved camera.
    describe "@G862" $ do
        aroundAll withHeadlessEngine GotoLoad.spec
    describe "@G863" $ do
        HarnessWorkerHealth.spec
    describe "@G864" $ do
        describe "Wrap Seam" WrapSeam.spec
    describe "@G865" $ do
        describe "Arena base seeding (#1718)" ArenaSeed.pureSpec
    describe "@G866" $ do
        describe "WorldGen.CoastBreach" CoastBreach.spec
    describe "@G867" $ do
        ExactRiver.spec
    describe "@G868" $ do
        describe "WorldGen.Breakthrough" Breakthrough.spec
    describe "@G869" $ do
        describe "shared lake spillways" SharedSpillway.spec
    describe "@G870" $ do
        describe "WorldGen.BedDepth" BedDepth.spec
    describe "@G871" $ do
        describe "WorldGen.FluidSurfaceFold" FluidSurfaceFold.spec
        -- #2286: filesystem + logger only. Deliberately OUTSIDE the shared
        -- worldgen engine above -- the loader never generates a world.
    describe "@G874" $ do
        describe "world-generation config loading" WorldGenConfigLoad.spec
    describe "@G875" $ do
        describe "Asset.Types" AssetTypes.spec
    describe "@G876" $ do
        describe "Asset.FloraContent" FloraContent.spec
    describe "@G877" $ do
        describe "Asset.FloraRegrowthSchema" FloraRegrowthSchema.spec
    describe "@G878" $ do
        describe "Asset.FloraHarvestPolicySchema" FloraHarvestPolicySchema.spec
    describe "@G879" $ do
        describe "Asset.FloraVocabularySchema" FloraVocabularySchema.spec
    describe "@G880" $ do
        describe "Asset.InfectionSchema" InfectionSchema.spec
    describe "@G881" $ do
        describe "Asset.UnitBodyGraph" AssetUnitBodyGraph.spec
    describe "@G882" $ do
        describe "Asset.UnitInventory" AssetUnitInventory.spec
    describe "@G883" $ do
        describe "Asset.YamlList" AssetYamlList.spec
    describe "@G884" $ do
        StartupAssetLogging.spec
    describe "@G885" $ do
        StartupReadiness.spec
    describe "@G886" $ do
        describe "material move_cost validation" AssetMaterialMoveCost.spec
    describe "@G887" $ do
        describe "Preview.Discovery" PreviewDiscovery.spec
    describe "@G888" $ do
        describe "Preview.UnitAnimation" PreviewUnitAnimation.spec
    describe "@G889" $ do
        describe "Preview.Building" PreviewBuilding.spec
    describe "@G890" $ do
        describe "Preview.BuildingMatrix" $ do
          describe "@S891" $ do
              PreviewBuildingMatrix.spec
          describe "@S892" $ do
              describe "the shipped Lua viewer" PreviewBuildingMatrixView.spec
    describe "@G893" $ do
        describe "Preview.Zoom" PreviewZoom.spec
    describe "@G894" $ do
        describe "Preview.StructurePack" PreviewStructurePack.spec
    describe "@G895" $ do
        describe "building asset schema and lifecycle roles"
            BuildingAssetSchema.spec
    describe "@G897" $ do
        BuildingCameraFacing.spec
    describe "@G898" $ do
        BuildingDestruction.spec
    describe "@G899" $ do
        describe "Machine Shop construction animation" MachineShopConstruction.spec
    describe "@G900" $ do
        describe "Preview.KeyboardNavigation" PreviewKeyboardNavigation.spec
    describe "@G901" $ do
        describe "Workbench construction animation" WorkbenchConstruction.spec
    describe "@G902" $ do
        describe "Bindless texture filter rebinding" BindlessRebind.spec
    describe "@G903" $ do
        describe "Bindless texture release" BindlessRelease.spec
    describe "@G904" $ do
        describe "bindless registration failure" $ do
            describe "@S905" $ do
                BindlessPublish.spec
            describe "@S906" $ do
                LuaAssetFailure.spec
    describe "@G907" $ do
        LuaMessageStrictness.spec
    describe "@G908" $ do
        describe "Unit.Pathing.Cost" PathingCost.spec
    describe "@G909" $ do
        PathingHazard.spec
    describe "@G910" $ do
        describe "Unit.Pathing.AStar" PathingAStar.spec
    describe "@G911" $ do
        describe "Unit.Pathing.Config" PathingConfig.spec
    describe "@G912" $ do
        describe "Unit.Render.pickFrame" PickFrame.spec
    describe "@G913" $ do
        UnitHitTest.spec
    describe "@G914" $ do
        UnitAtlas.spec
    describe "@G915" $ do
        aroundAll withHeadlessEngine UnitAtlasLoader.spec

    describe "@G917" $ do
        aroundAll withHeadlessEngine ItemDiscovery.spec
    describe "@G918" $ do
        aroundAll withHeadlessEngineNoWorld ItemCondition.spec
    describe "@G919" $ do
        aroundAll withHeadlessEngineNoWorld ItemSteelHelmet.spec
        -- Own engine (#1772): the craft-identity gate installs its own
        -- single-page world manager and rewrites the item, recipe and unit
        -- manager refs, exactly like the ItemCondition gate above. It needs
        -- no world -- craft.execute reads none.
    describe "@G924" $ do
        aroundAll withHeadlessEngineNoWorld CraftOutputIdentity.spec
        -- Own engine (#2325): the bill page-binding gate installs its own
        -- TWO-page world manager (each page carrying a bill numbered 1) and
        -- rewrites the item, recipe, unit and building manager refs, so it
        -- cannot share the worldgen engine. Both pages are in-memory
        -- emptyWorldStates -- the craft-bill verbs read no terrain.
    describe "@G930" $ do
        aroundAll withHeadlessEngineNoWorld CraftBillPageBinding.spec
        -- Own engine (#1716): the live unit.feed gate WRITES the item and
        -- unit manager refs, so it cannot share the worldgen engine. It
        -- needs no world at all -- unit.feed reads neither.
    describe "@G934" $ do
        aroundAll withHeadlessEngineNoWorld ItemFoodNutrition.feedSpec
    describe "@G935" $ do
        describe "Unit.Anim" AnimTest.spec
    describe "@G936" $ do
        describe "Unit.Injury" InjuryTest.spec
    describe "@G937" $ do
        describe "Unit.InjurySpeed" InjurySpeedTest.spec
    describe "@G938" $ do
        describe "Unit.Fall" FallTest.spec
    describe "@G939" $ do
        describe "Unit.StopTransition" StopTransition.spec
    describe "@G940" $ do
        describe "Unit.Stats" StatsTest.spec
    describe "@G941" $ do
        StanceRecovery.spec
    describe "@G942" $ do
        FrameOrganFailure.spec
    describe "@G943" $ do
        LeanFloorDeath.spec
    describe "@G944" $ do
        SourceDrinkingHydration.spec
    describe "@G945" $ do
        ResourceTickCarry.spec
    describe "@G946" $ do
        RegrowthPrecision.spec
    describe "@G947" $ do
        SourceDrinkPose.spec
    describe "@G948" $ do
        MentalWanderFallback.spec
    describe "@G949" $ do
        TerminalDeathPose.spec
    describe "@G950" $ do
        StaminaCommit.spec
    describe "@G951" $ do
        AccessoryUnequip.spec
    describe "@G952" $ do
        SpawnShedTest.spec
    describe "@G953" $ do
        UnitTransfer.spec
    describe "@G954" $ do
        describe "Unit.NightPerception" NightPerception.spec
    describe "@G955" $ do
        describe "Unit.LineOfSight (multi-world page ownership)" LineOfSightTest.spec
    describe "@G956" $ do
        describe "World.TimeLocal" TimeLocal.spec
    describe "@G957" $ do
        describe "Item.Temperature" ItemTemp.spec
    describe "@G958" $ do
        describe "Item.BuffYaml" ItemBuffYaml.spec
    describe "@G959" $ do
        describe "Item.QualityTier" ItemQualityTier.spec
    describe "@G960" $ do
        describe "Item.ContentsSignature" ItemContentsSig.spec
    describe "@G961" $ do
        describe "Item.BulkStorage" ItemBulkStorage.spec
    describe "@G962" $ do
        describe "Item.Ownership" ItemOwnership.spec
    describe "@G963" $ do
        describe "Item.FoodNutrition" ItemFoodNutrition.spec
    describe "@G964" $ do
        describe "Item.Materialize" ItemMaterialize.spec
    describe "@G965" $ do
        describe "World.Save.Sanitize" SaveSanitize.spec
    describe "@G966" $ do
        describe "World.Save.Serialize" SaveSerialize.spec
    describe "@G967" $ do
        describe "save envelope" SaveEnvelope.spec
    describe "@G968" $ do
        describe "save components" SaveComponents.spec
    describe "@G969" $ do
        describe "save migrations" SaveCompat.spec
    describe "@G970" $ do
        describe "save migrations" SimExactFluid.saveSpec
    describe "@G971" $ do
        describe "persistence reference integrity" SaveIntegrity.spec
    describe "@G972" $ do
        describe "persistence reference integrity" LuaSaveBridge.spec
    describe "@G973" $ do
        describe "atomic save storage" SaveStorage.spec
    describe "@G974" $ do
        describe "generated world library" GeneratedLibrary.spec
    describe "@G975" $ do
        describe "persistence contract" SaveContract.spec
    describe "@G976" $ do
        describe "autosave staging slots (#1413)" AutosaveListing.spec
    describe "@G977" $ do
        describe "autosave rotation durability (#2229)" AutosaveRotation.spec
    describe "@G978" $ do
        ListingFailureBoundary.spec
    describe "@G979" $ do
        MenuListingOrder.spec
    describe "@G980" $ do
        describe "Save.Barrier" SaveBarrier.spec
    describe "@G981" $ do
        describe "Save.OwnerPark" SaveOwnerPark.spec
        -- Own engine per example, world-thread-FREE (#2291): each case
        -- installs its own single-page session and drives the destroy-all
        -- handler plus the real unit tick directly, so a world worker
        -- draining worldQueue beside it would buy nothing.
    describe "@G986" $ do
        SessionEpoch.spec
    describe "@G987" $ do
        PageIncarnation.spec
    describe "@G988" $ do
        describe "Load.Status" LoadStatus.spec
    describe "@G989" $ do
        describe "Load.Terminalize" LoadTerminalize.spec
    describe "@G990" $ do
        describe "Save.Snapshot" SaveSnapshot.spec
    describe "@G991" $ do
        describe "Lua persistence components" LuaSaveModules.spec
    describe "@G992" $ do
        describe "Lua shared helpers" LuaSharedHelpers.spec
    describe "@G993" $ do
        LuaTutorialProgress.spec
    describe "@G994" $ do
        LuaTutorialEvaluation.spec
    describe "@G995" $ do
        LuaUnitAiLocations.spec
    describe "@G996" $ do
        LuaUnitAiCanteenDrain.spec
    describe "@G997" $ do
        LuaUnitAiHold.spec
    describe "@G998" $ do
        LuaUnitAiWaterCanteens.spec
    describe "@G999" $ do
        LuaCombatLogRefusal.spec
    describe "@G1000" $ do
        LuaUnitAiCombatMove.spec
    describe "@G1001" $ do
        LuaUnitAiEncounter.spec
    describe "@G1002" $ do
        LuaUnitAiStall.spec
    describe "@G1003" $ do
        LuaUnitAiHarvest.spec
    describe "@G1004" $ do
        LuaUnitAiYieldProximity.spec
    describe "@G1005" $ do
        LuaUnitAiLogisticsTargets.spec
    describe "@G1006" $ do
        LuaBuilderEligibility.spec
    describe "@G1007" $ do
        BuildingConstructionEligibility.spec
    describe "@G1008" $ do
        LuaUnitAiPageTargets.spec
    describe "@G1009" $ do
        LuaUnitAiLoadReset.spec
        -- #2534: its own headless engine and a synthetic page, so the
        -- production farming selection runs against the REAL till/plant
        -- designation verbs rather than a stub.
    describe "@G1013" $ do
        LuaFarmDesignationClaim.spec
    describe "@G1014" $ do
        LuaUnitAiReconcile.spec
    describe "@G1015" $ do
        LuaSessionTeardown.spec
    describe "@G1016" $ do
        LuaBuildingSpawnSentinel.spec
    describe "@G1017" $ do
        LuaWorkClaimCapacity.spec
    describe "@G1018" $ do
        LuaCraftCycleReplenishment.spec
    describe "@G1019" $ do
        LuaWorkClockBounds.spec
    describe "@G1020" $ do
        LuaFaction.spec
    describe "@G1021" $ do
        describe "World.CursorInfo" CursorInfo.spec
    describe "@G1022" $ do
        CursorTextureDispatch.spec
    describe "@G1023" $ do
        describe "World.SelectChunk" SelectChunk.spec
    describe "@G1024" $ do
        describe "World.Spoil" Spoil.spec
    describe "@G1025" $ do
        describe "World.DigDomain" DigDomain.spec
    describe "@G1026" $ do
        describe "rendered fluid surface rule (#1112)" RenderedSurface.spec
    describe "@G1027" $ do
        describe "dry island-column fluid smoothing (#1131)" IslandColumns.spec
    describe "@G1028" $ do
        describe "shared chunk-coordinate derivation" ChunkCoordinates.spec
    describe "@G1029" $ do
        WorldLocationDiscovery.spec
    describe "@G1030" $ do
        describe "WorldGen.SoilGate" SoilGate.spec
    describe "@G1031" $ do
        describe "WorldGen.SoilShed" SoilShed.spec
    describe "@G1032" $ do
        describe "WorldGen.SoilRedistribution" SoilRedistribution.spec
    describe "@G1033" $ do
        CombatAdmission.spec
    describe "@G1034" $ do
        describe "Combat.Damage" CombatDamage.spec
    describe "@G1035" $ do
        CombatMaxStamina.spec
    describe "@G1036" $ do
        CombatMentalEffectiveness.spec
    describe "@G1037" $ do
        describe "Combat.Severing" CombatSevering.spec
    describe "@G1038" $ do
        describe "Combat.Wounds" CombatWounds.spec
    describe "@G1039" $ do
        describe "World.Magma.Shape" MagmaShape.spec
    describe "@G1040" $ do
        describe "Sim.Fluid.Seam" SimSeam.spec
    describe "@G1041" $ do
        describe "Sim.Fluid.Conservation" SimConservation.spec
    describe "@G1042" $ do
        describe "Sim.Fluid.Exact" SimExactFluid.spec
    describe "@G1043" $ do
        describe "unlike-fluid reaction" SimReaction.spec
    describe "@G1044" $ do
        Solidification.pureSpec
    describe "@G1045" $ do
        SolidificationOccupants.pureSpec
    describe "@G1046" $ do
        describe "Input.KeyNames" InputKeyNames.spec
    describe "@G1047" $ do
        describe "Input.Bindings" InputBindings.spec
    describe "@G1048" $ do
        describe "Input.Inject" InputInject.spec
    describe "@G1049" $ do
        describe "Input.WheelPolicy" InputWheelPolicy.spec
        -- #1153: three GPU-free specs relocated out of the graphical suite,
        -- which automated gates only ever COMPILE. Each already supplies its
        -- own top-level describe except UPrelude, so `--match "UPrelude"`,
        -- `--match "Engine.Core.Queue"` and `--match "Engine.Input.State"`
        -- all still reach them.
    describe "@G1055" $ do
        describe "UPrelude" UPreludeSpec.spec
    describe "@G1056" $ do
        CoreQueue.spec
    describe "@G1057" $ do
        InputState.spec
    describe "@G1058" $ do
        describe "Graphics.VideoConfig" VideoConfig.spec
    describe "@G1059" $ do
        describe "Graphics.VulkanAppIdentity" VulkanAppIdentity.spec
    describe "@G1060" $ do
        BindlessFeatures.spec
    describe "@G1061" $ do
        GraphicsInstancePlan.spec
    describe "@G1062" $ do
        describe "Graphics.WindowMode" GraphicsWindowMode.spec
    describe "@G1063" $ do
        describe "Graphics.computeAmbientLight" AmbientLight.spec
    describe "@G1064" $ do
        describe "Graphics.Screenshot" GraphicsScreenshot.spec
    describe "@G1065" $ do
        GraphicsSwapchainSelection.spec
    describe "@G1066" $ do
        describe "Graphics.UniformLayout" GraphicsUniformLayout.spec
    describe "@G1067" $ do
        describe "Graphics.VertexLayout" GraphicsVertexLayout.spec
    describe "@G1068" $ do
        describe "world vertex coordinates" GraphicsWorldVertexCoords.spec
    describe "@G1069" $ do
        describe "Graphics.FontFallback" GraphicsFontFallback.spec
    describe "@G1070" $ do
        describe "Font SDF atlas repertoire" GraphicsFontRepertoire.spec
    describe "@G1071" $ do
        describe "Construct.Corners" ConstructCorners.spec
    describe "@G1072" $ do
        describe "Construct.Footprint" ConstructFootprint.spec

        -- #1845: the pure half needs nothing; the render half boots its own
        -- headless engine over one synthetic page, like SceneStats above.
    describe "@G1076" $ do
        BuildingGhost.spec
    describe "@G1077" $ do
        ConstructPlan.spec
    describe "@G1078" $ do
        StructureGhost.spec
    describe "@G1079" $ do
        FrameAssembly.spec
    describe "@G1080" $ do
        ConstructPlanInvalidation.spec
    describe "@G1081" $ do
        ConstructAttemptIdentity.spec
    describe "@G1082" $ do
        describe "Construct.PendingRefusal" ConstructPendingRefusal.spec
    describe "@G1083" $ do
        describe "Craft.Execute" CraftExecute.spec
    describe "@G1084" $ do
        ItemRepairFinite.spec
    describe "@G1085" $ do
        describe "Craft.Bills" CraftBills.spec
    describe "@G1086" $ do
        LuaCraftBillQueue.spec
    describe "@G1087" $ do
        describe "Power.Types" PowerTypes.spec
    describe "@G1088" $ do
        describe "Power.Network" PowerNetwork.spec
    describe "@G1089" $ do
        describe "Language.Semantic" LanguageSemantic.spec
    describe "@G1090" $ do
        describe "Language.Generated" LanguageGenerated.spec
    describe "@G1091" $ do
        describe "Language.Suggest" LanguageSuggest.spec
    describe "@G1092" $ do
        describe "Language etymology" LanguageEtymology.spec
    describe "@G1093" $ do
        describe "Blood.Types" BloodTypes.spec
    describe "@G1094" $ do
        describe "Blood.Texture" BloodTexture.spec
    describe "@G1095" $ do
        describe "Blood.Impact" BloodImpact.spec
    describe "@G1096" $ do
        describe "Blood.Trail" BloodTrail.spec
    describe "@G1097" $ do
        describe "Blood.Teardown" BloodTeardown.spec
    describe "@G1098" $ do
        describe "Create World player-facing controls" CreateWorldControls.spec
    describe "@G1099" $ do
        describe "UI.Tooltip" UITooltip.spec
    describe "@G1100" $ do
        describe "UI.InputOwnership" UIInputOwnership.spec
    describe "@G1101" $ do
        describe "zoom-band entity input gate" UIZoomBandInputGate.spec
    describe "@G1102" $ do
        describe "hud hover gameplay-input gate" UIHudHoverGate.spec
    describe "@G1103" $ do
        describe "Unit Info row selection gate" UIUnitInfoRowSelection.spec
    describe "@G1104" $ do
        describe "Item Info row selection gate" UIItemInfoRowSelection.spec
    describe "@G1105" $ do
        describe "ground item selection" GroundSelection.spec
    describe "@G1106" $ do
        describe "Ground item move" GroundMove.spec
    describe "@G1107" $ do
        describe "UI.ElementInputPolicy" UIElementInputPolicy.spec
    describe "@G1108" $ do
        describe "UI.ControlActivation" UIControlActivation.spec
    describe "@G1109" $ do
        describe "UI hierarchy structural ownership" UIHierarchyOwnership.spec
    describe "@G1110" $ do
        describe "UI.FocusNavigation" UIFocusNavigation.spec
    describe "@G1111" $ do
        describe "UI.DropdownCommit" UIDropdownCommit.spec
    describe "@G1112" $ do
        describe "UI.Clipping" UIClipping.spec
    describe "@G1113" $ do
        describe "List scrollbar sync" UIListScrollSync.spec
    describe "@G1114" $ do
        describe "UI.InteractiveBounds" UIInteractiveBounds.spec
    describe "@G1115" $ do
        describe "UI.PopupPlacement" UIPopupPlacement.spec
        -- #1714: its own engine per example (the event store's sequence
        -- counter is process-lifetime, and one case drives the real
        -- load-publish reset), so it registers here rather than under the
        -- shared-worlds aroundAll above.
    describe "@G1120" $ do
        PlayerEventProgress.spec
        -- #1588: its own engine per example (each case installs its own
        -- WorldManager and asserts on the event ring), so it registers
        -- here rather than under the shared-worlds aroundAll above.
    describe "@G1124" $ do
        PopupCoordPage.spec
        -- #1592: its own engine AND Lua VM per example — the pre-bootstrap
        -- popup state it exercises is a once-per-process condition, so a
        -- shared module table would destroy it.
    describe "@G1128" $ do
        UIPopupQueueTeardown.spec
    describe "@G1129" $ do
        describe "UI.RandboxContainment" UIRandboxContainment.spec
    describe "@G1130" $ do
        describe "UI.ResponsiveMenus" UIResponsiveMenus.spec
    describe "@G1131" $ do
        describe "UI.ResponsiveGameplay" UIResponsiveGameplay.spec
    describe "@G1132" $ do
        UISettingsDefaultsKeybinds.spec
    describe "@G1133" $ do
        UISettingsRevert.spec
    describe "@G1134" $ do
        describe "UI.ContainerWindowStack" UIContainerWindowStack.spec
    describe "@G1135" $ do
        LoadReplacementTeardown.spec
    describe "@G1136" $ do
        UITransferGestures.spec
    describe "@G1137" $ do
        UIConsumableGesture.spec
    describe "@G1138" $ do
        UITransferSession.spec
    describe "@G1139" $ do
        describe "Tutorial HUD" UITutorialHud.spec
    describe "@G1140" $ do
        describe "UI.UnicodeTextEditing" UIUnicodeTextEditing.spec
    describe "@G1141" $ do
        LuaDragSelectDeferred.spec
    describe "@G1142" $ do
        LuaDebugGrab.spec
    describe "@G1143" $ do
        describe "Lua.TextWrapping" LuaTextWrapping.spec
    describe "@G1144" $ do
        describe "Lua.GroupedLogRetention" LuaGroupedLogRetention.spec
    describe "@G1145" $ do
        describe "Lua.TextTruncation" LuaTextTruncation.spec
    describe "@G1146" $ do
        describe "Lua.WidthTruncation" LuaWidthTruncation.spec
    describe "@G1147" $ do
        describe "Lua.ShellInput" LuaShellInput.spec
    describe "@G1148" $ do
        describe "Lua log source" LuaLogSource.spec
    describe "@G1149" $ do
        describe "Lua random stream ownership" LuaRandomStream.spec
    describe "@G1150" $ do
        describe "Lua injury narration" LuaInjuryNarration.spec
    describe "@G1151" $ do
        UISlider.spec
    describe "@G1152" $ do
        UIBarFillColor.spec
    describe "@G1153" $ do
        LuaCallStats.spec
    describe "@G1154" $ do
        ChunkMemory.spec
    describe "@G1155" $ do
        LuaUiDescriptors.spec
    describe "@G1156" $ do
        UIClickCorrelation.spec
    describe "@G1157" $ do
        describe "World.Calendar" Calendar.spec
    describe "@G1158" $ do
        SubMinuteClock.spec
    describe "@G1159" $ do
        describe "World.FloraGrowth" FloraGrowth.spec
    describe "@G1160" $ do
        describe "World.FloraOrder" FloraOrder.spec
    describe "@G1161" $ do
        describe "River.CalderaHazard" RiverCalderaHazard.spec
    describe "@G1162" $ do
        describe "World.Render.FrontWallLift" FrontWallLift.spec
    describe "@G1163" $ do
        describe "World.Render.StructureRotation" StructureRotation.spec
    describe "@G1164" $ do
        describe "World.Render.GroundItemSeam" GroundItemSeam.spec
    describe "@G1165" $ do
        describe "World.Render.GroundItemSeam (engine)" GroundItemSeam.engineSpec
    describe "@G1166" $ do
        describe "World.Render.StructureSeam" StructureSeam.spec
    describe "@G1167" $ do
        describe "World.Render.StructureSeam (engine)" StructureSeam.engineSpec
    describe "@G1168" $ do
        describe "World.Render.PickSeam" PickSeam.spec

        -- #1720: its own headless engine (no worker threads), so the live
        -- camera can be rewritten between capture and build the way the
        -- main thread's pan integration does under the world thread.
    describe "@G1173" $ do
        describe "World.Render.QuadSnapshot" QuadSnapshot.spec

        -- #1921: same shape again — its own headless engine and one
        -- synthetic page, driven through the real 'updateWorldTiles'.
    describe "@G1177" $ do
        SceneStats.spec

        -- #1869: same shape as the line above and for the same reason —
        -- its own headless engine, two synthetic pages, no worker threads.
    describe "@G1181" $ do
        SolarAttribution.spec
    describe "@G1182" $ do
        describe "World.Render.DesignationFaceMap" DesignationFaceMap.spec
    describe "@G1183" $ do
        describe "World.DesignationSeam" DesignationSeam.spec
    describe "@G1184" $ do
        ChopSelection.spec
    describe "@G1185" $ do
        ChopAuthority.spec
    describe "@G1186" $ do
        ChopTagPolicy.spec
    describe "@G1187" $ do
        LuaChopGesture.spec
    describe "@G1188" $ do
        LuaChopFellXp.spec
    describe "@G1189" $ do
        LuaChopDesignationClaim.spec
    describe "@G1190" $ do
        LuaMineDesignationEligibility.spec
    describe "@G1191" $ do
        describe "World.DesignationSeam (engine)" DesignationSeam.engineSpec
    describe "@G1192" $ do
        describe "World.DigDomain (engine)" DigDomain.engineSpec
    describe "@G1193" $ do
        FloraIdentity.spec
    describe "@G1194" $ do
        FloraIdentity.engineSpec
    describe "@G1195" $ do
        describe "World.CropPlant" CropPlant.spec
    describe "@G1196" $ do
        describe "World.CropPlant (engine)" CropPlant.engineSpec

        -- #1674: its own headless engine (no worker threads), so the
        -- WorldSetStructure structure.place emits waits to be dequeued and
        -- dispatched by the example rather than by a racing drainer.
    describe "@G1201" $ do
        StructureStage.spec

        -- #1675: the same shape, for the palette residue a REJECTED
        -- structure.place used to leave behind — its own engine so the
        -- "nothing was queued" half is an assertion on an undrained queue.
    describe "@G1206" $ do
        StructurePaletteResidue.spec

        -- #1842: the unplaced-piece art catalogue. Its own headless engine
        -- for the same reason as the two above -- the placement it compares
        -- against is read off the undrained WorldSetStructure -- plus the
        -- real scripts/structures.lua and scripts/wire.lua, so parity is
        -- against the builder rather than a table written in the test.
    describe "@G1213" $ do
        StructureArtCatalog.spec
    describe "@G1214" $ do
        StructureConstructionFrames.spec
    describe "@G1215" $ do
        StructureConstructionPacks.spec
    describe "@G1216" $ do
        StructureDestructionFrames.spec
    describe "@G1217" $ do
        StructureDestructionPacks.spec
    describe "@G1218" $ do
        WorldStructureDestruction.spec

        -- #1602: its own headless engine (no worker threads), so a queued
        -- BuildingSpawn / WorldDesignateConstruct stays in its queue and
        -- "nothing was committed" is asserted on the queue itself.
    describe "@G1223" $ do
        BuildingPageBinding.spec
    describe "@G1224" $ do
        BuildingPortalSpawnBinding.spec
        -- #2326: its own headless engine for the same reason — an admitted
        -- spawn must sit in its queue until an example drains it, so
        -- "against one pre-commit snapshot" is a controlled state.
    describe "@G1228" $ do
        BuildingFootprintExclusivity.spec
    describe "@G1229" $ do
        describe "World.Render.ZTrackSeam" ZTrackSeam.spec
    describe "@G1230" $ do
        describe "World.Render.SlopeFacing" SlopeFacing.spec
    describe "@G1231" $ do
        describe "World.Render.FluidLevels" RenderFluidLevels.spec
    describe "@G1232" $ do
        describe "World.Render.SideFace" RenderSideFace.spec
    describe "@G1233" $ do
        describe "World.Slope.slopeBit" RenderSlopeBit.spec
    describe "@G1234" $ do
        describe "World.Slope.FaceMaps" FluidLevelMasks.spec
    describe "@G1235" $ do
        describe "World.Render.Zoom.zoomQuadWorldUVs" ZoomBakeUV.spec
    describe "@G1236" $ do
        describe "Render.ViewportGuard" ViewportGuard.spec
    describe "@G1237" $ do
        describe "Render.QuadVertices" QuadVertices.spec
    describe "@G1238" $ do
        describe "Core.ConfigState" ConfigState.spec
    describe "@G1239" $ do
        describe "Core.ConfigWrite" ConfigWrite.spec
    describe "@G1240" $ do
        FixtureLogging.spec
    describe "@G1241" $ do
        LogCategoryEnv.spec
    describe "@G1242" $ do
        LogMonad.spec
    describe "@G1243" $ do
        LogParity.spec
    describe "@G1244" $ do
        LogThresholdEnv.spec
    describe "@G1245" $ do
        LoopStartup.spec
    describe "@G1246" $ do
        MonotonicClock.spec
    describe "@G1247" $ do
        StepProtocol.spec
    describe "@G1248" $ do
        ShutdownAtlasRelease.spec
    describe "@G1249" $ do
        WorkerLifecycle.spec
    describe "@G1250" $ do
        AudioNative.spec
    describe "@G1251" $ do
        AudioConfig.spec
    describe "@G1252" $ do
        AudioCatalog.spec
    describe "@G1253" $ do
        AudioUpload.spec
    describe "@G1254" $ do
        AudioTransport.spec
    describe "@G1255" $ do
        AudioSpatial.spec
    describe "@G1256" $ do
        AudioRuntime.spec
    describe "@G1257" $ do
        AudioIntegration.spec
    describe "@G1258" $ do
        AudioHealth.spec
    describe "@G1259" $ do
        AudioThread.spec
    describe "@G1260" $ do
        AudioPreview.spec
    describe "@G1261" $ do
        AudioPreviewUI.spec
    describe "@G1262" $ do
        AudioLua.spec
    describe "@G1263" $ do
        AudioSettings.spec
    describe "@G1264" $ do
        DebugListener.spec
    describe "@G1265" $ do
        DebugSocket.spec
    describe "@G1266" $ do
        DebugConsoleStop.spec
    describe "@G1267" $ do
        AppCli.spec
    describe "@G1268" $ do
        AppChunkRegion.spec
    describe "@G1269" $ do
        DumpSettleWait.spec
        -- #2535: exact fluid units/level in the dump, cursor and getAreaFluid.
    describe "@G1271" $ do
        FluidDiagnostics.spec
    describe "@G1272" $ do
        AppResourceRoot.spec
    describe "@G1273" $ do
        describe "App.Preview.Config" PreviewConfig.spec
    describe "@G1274" $ do
        describe "Camera.GotoClamp" GotoClamp.spec
    describe "@G1275" $ do
        describe "Camera.ZoomScroll" ZoomScroll.spec
        -- #2337: non-finite camera coordinates, refused at both boundaries.
        -- The pure half -- the load-time repair and its warning -- needs no
        -- engine. The Lua half rewrites the engine's own camera ref under
        -- every example, and the staging half drives stageSession against a
        -- forged one-page save, so each gets its OWN world-thread-free
        -- engine rather than moving the shared worlds engine's camera or
        -- adding a page to it. Its page is an arena page, so staging
        -- rebuilds flat chunks instead of generating a world.
    describe "@G1284" $ do
        CameraFinite.pureSpec
    describe "@G1285" $ do
        aroundAll withHeadlessEngineNoWorld CameraFinite.spec
    describe "@G1286" $ do
        aroundAll withHeadlessEngineNoWorld CameraFinite.stagingSpec
    describe "@G1287" $ do
        describe "Scene.BatchMerge" BatchMerge.spec
    describe "@G1288" $ do
        describe "Render.PanMargin" PanMargin.spec
    describe "@G1289" $ do
        LocationBounds.spec
    describe "@G1290" $ do
        LocationDiscovery.spec
    describe "@G1291" $ do
        UnitFaction.spec
    describe "@G1292" $ do
        UnitStandardSpawn.spec
    describe "@G1293" $ do
        UnitFactionProfile.spec
    describe "@G1294" $ do
        UnitFactionCatalogue.spec
    describe "@G1295" $ do
        ContainerKnowledge.spec
    describe "@G1296" $ do
        PortableKnowledge.spec
    describe "@G1297" $ do
        LocationInstance.spec
    describe "@G1298" $ do
        LocationSignificantContents.spec
        -- #2505: four layers of the pending-container-shell slice. The pure
        -- and stubbed-VM halves need no engine; the YAML boundary and the
        -- spawn boundary each bring their OWN, because one borrows the live
        -- item/loot-profile/location registries and the other rewrites the
        -- world and item managers to install a one-page fixture per example.
    describe "@G1304" $ do
        LocationContainerShells.pureSpec
    describe "@G1305" $ do
        LocationContainerShells.luaSpec
    describe "@G1306" $ do
        LocationContainerShells.yamlSpec
    describe "@G1307" $ do
        LocationContainerShells.engineSpec
    describe "@G1308" $ do
        LocationNaming.spec
    describe "@G1309" $ do
        RiverNaming.spec
    describe "@G1310" $ do
        LocationLootDeterminism.spec
    describe "@G1311" $ do
        LootProfiles.spec
    describe "@G1312" $ do
        LootRealization.spec
    describe "@G1313" $ do
        LocationMapIcons.spec
    describe "@G1314" $ do
        LocationStamping.spec
    describe "@G1315" $ do
        LocationStampCommit.spec
    describe "@G1316" $ do
        TutorialDefinitions.spec
    describe "@G1317" $ do
        BuildingPlacement.spec
    describe "@G1318" $ do
        BuildingRemoteWarning.spec
