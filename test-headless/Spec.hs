module Main where

import UPrelude
import Test.Hspec
import Test.Headless.Harness (withHeadlessEngine, withHeadlessEngineNoWorld)
import qualified Test.Headless.Harness.WorkerHealth as HarnessWorkerHealth
import qualified Test.Headless.UPrelude as UPreludeSpec
import qualified Test.Headless.WorldGen as WorldGen
import qualified Test.Headless.WorldGen.Geology as Geology
import qualified Test.Headless.WorldGen.Parity as Parity
import qualified Test.Headless.WorldGen.Flatness as Flatness
import qualified Test.Headless.WorldGen.SoilGate as SoilGate
import qualified Test.Headless.WorldGen.SoilShed as SoilShed
import qualified Test.Headless.WorldGen.SoilRedistribution as SoilRedistribution
import qualified Test.Headless.WorldGen.Exposure as Exposure
import qualified Test.Headless.WorldGen.ZoomParity as ZoomParity
import qualified Test.Headless.WorldGen.BorderProbe as BorderProbe
import qualified Test.Headless.WorldGen.WrapSeam as WrapSeam
import qualified Test.Headless.WorldGen.CoastBreach as CoastBreach
import qualified Test.Headless.WorldGen.BedDepth as BedDepth
import qualified Test.Headless.WorldGen.FluidSurfaceFold as FluidSurfaceFold
import qualified Test.Headless.Unit.Pathing.Cost as PathingCost
import qualified Test.Headless.Unit.Pathing.Hazard as PathingHazard
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
import qualified Test.Headless.Unit.AddXpApi as UnitAddXpApi
import qualified Test.Headless.Unit.AccessoryUnequip as AccessoryUnequip
import qualified Test.Headless.Unit.SpawnShed as SpawnShedTest
import qualified Test.Headless.Unit.Transfer as UnitTransfer
import qualified Test.Headless.Unit.TransferApi as UnitTransferApi
import qualified Test.Headless.Unit.TransferOrderApi as UnitTransferOrderApi
import qualified Test.Headless.Unit.CargoApi as UnitCargoApi
import qualified Test.Headless.Unit.NightPerception as NightPerception
import qualified Test.Headless.Unit.LineOfSight as LineOfSightTest
import qualified Test.Headless.World.ArenaSeed as ArenaSeed
import qualified Test.Headless.World.TimeLocal as TimeLocal
import qualified Test.Headless.World.Climate as Climate
import qualified Test.Headless.Item.GroundPageOwnership as GroundPageOwnership
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
import qualified Test.Headless.Item.FoodNutrition as ItemFoodNutrition
import qualified Test.Headless.Item.Discovery as ItemDiscovery
import qualified Test.Headless.Asset.TextureFallback as TextureFallback
import qualified Test.Headless.Asset.FloraContent as FloraContent
import qualified Test.Headless.Asset.FloraRegrowthSchema as FloraRegrowthSchema
import qualified Test.Headless.Asset.UnitInventory as AssetUnitInventory
import qualified Test.Headless.Asset.Types as AssetTypes
import qualified Test.Headless.Asset.YamlList as AssetYamlList
import qualified Test.Headless.Asset.MaterialMoveCost as AssetMaterialMoveCost
import qualified Test.Headless.Preview.Discovery as PreviewDiscovery
import qualified Test.Headless.Unit.Atlas as UnitAtlas
import qualified Test.Headless.Unit.Atlas.Loader as UnitAtlasLoader
import qualified Test.Headless.Preview.UnitAnimation as PreviewUnitAnimation
import qualified Test.Headless.Preview.Building as PreviewBuilding
import qualified Test.Headless.Preview.Zoom as PreviewZoom
import qualified Test.Headless.World.Save.Sanitize as SaveSanitize
import qualified Test.Headless.World.Save.Serialize as SaveSerialize
import qualified Test.Headless.World.Save.Envelope as SaveEnvelope
import qualified Test.Headless.World.Save.Components as SaveComponents
import qualified Test.Headless.World.Save.Compat as SaveCompat
import qualified Test.Headless.World.Save.Integrity as SaveIntegrity
import qualified Test.Headless.World.Save.Storage as SaveStorage
import qualified Test.Headless.World.Save.Contract as SaveContract
import qualified Test.Headless.World.Identity as WorldIdentity
import qualified Test.Headless.World.TransferOrders as WorldTransferOrders
import qualified Test.Headless.World.FluidWritebackStaleness as FluidWritebackStaleness
import qualified Test.Headless.World.CursorInfo as CursorInfo
import qualified Test.Headless.World.CursorTextureDispatch as CursorTextureDispatch
import qualified Test.Headless.World.SelectTileZ as SelectTileZ
import qualified Test.Headless.World.SelectChunk as SelectChunk
import qualified Test.Headless.World.ChunkQueueFrame as ChunkQueueFrame
import qualified Test.Headless.World.ActionOutcome as ActionOutcome
import qualified Test.Headless.World.Spoil as Spoil
import qualified Test.Headless.World.RenderedSurface as RenderedSurface
import qualified Test.Headless.World.IslandColumns as IslandColumns
import qualified Test.Headless.World.ChunkCoordinates as ChunkCoordinates
import qualified Test.Headless.Combat.Damage as CombatDamage
import qualified Test.Headless.Combat.MaxStamina as CombatMaxStamina
import qualified Test.Headless.Combat.MentalEffectiveness as CombatMentalEffectiveness
import qualified Test.Headless.Combat.Severing as CombatSevering
import qualified Test.Headless.Combat.Wounds as CombatWounds
import qualified Test.Headless.Magma.Shape as MagmaShape
import qualified Test.Headless.Sim.Seam as SimSeam
import qualified Test.Headless.Input.KeyNames as InputKeyNames
import qualified Test.Headless.Input.Bindings as InputBindings
import qualified Test.Headless.Input.State as InputState
import qualified Test.Headless.Input.Inject as InputInject
import qualified Test.Headless.Input.Followup as InputFollowup
import qualified Test.Headless.Input.InjectOwnership as InputInjectOwnership
import qualified Test.Headless.Lua.DebugQueue as LuaDebugQueue
import qualified Test.Headless.Lua.RenderQueue as LuaRenderQueue
import qualified Test.Headless.Lua.PreviewGeneration as LuaPreviewGeneration
import qualified Test.Headless.Lua.PauseGate as LuaPauseGate
import qualified Test.Headless.World.PauseSpeed as PauseSpeed
import qualified Test.Headless.Lua.ScriptState as LuaScriptState
import qualified Test.Headless.Lua.TickInterval as LuaTickInterval
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
import qualified Test.Headless.Graphics.UniformLayout as GraphicsUniformLayout
import qualified Test.Headless.Graphics.VertexLayout as GraphicsVertexLayout
import qualified Test.Headless.Graphics.FontFallback as GraphicsFontFallback
import qualified Test.Headless.Graphics.FontRepertoire as GraphicsFontRepertoire
import qualified Test.Headless.Construct.Corners as ConstructCorners
import qualified Test.Headless.Construct.Footprint as ConstructFootprint
import qualified Test.Headless.Construct.PendingRefusal as ConstructPendingRefusal
import qualified Test.Headless.Craft.Execute as CraftExecute
import qualified Test.Headless.Craft.Bills as CraftBills
import qualified Test.Headless.Craft.OutputIdentity as CraftOutputIdentity
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
import qualified Test.Headless.UI.UnitInfoRowSelection as UIUnitInfoRowSelection
import qualified Test.Headless.UI.ElementInputPolicy as UIElementInputPolicy
import qualified Test.Headless.UI.ControlActivation as UIControlActivation
import qualified Test.Headless.UI.HierarchyOwnership as UIHierarchyOwnership
import qualified Test.Headless.UI.FocusNavigation as UIFocusNavigation
import qualified Test.Headless.UI.Clipping as UIClipping
import qualified Test.Headless.UI.InteractiveBounds as UIInteractiveBounds
import qualified Test.Headless.UI.PopupPlacement as UIPopupPlacement
import qualified Test.Headless.Event.PlayerEventProgress as PlayerEventProgress
import qualified Test.Headless.Event.PopupCoordPage as PopupCoordPage
import qualified Test.Headless.UI.PopupQueueTeardown as UIPopupQueueTeardown
import qualified Test.Headless.UI.ResponsiveMenus as UIResponsiveMenus
import qualified Test.Headless.UI.ResponsiveGameplay as UIResponsiveGameplay
import qualified Test.Headless.UI.SettingsDefaultsKeybinds
    as UISettingsDefaultsKeybinds
import qualified Test.Headless.UI.SettingsRevert
    as UISettingsRevert
import qualified Test.Headless.UI.TutorialHud as UITutorialHud
import qualified Test.Headless.UI.UnicodeTextEditing as UIUnicodeTextEditing
import qualified Test.Headless.Lua.DragSelectDeferred as LuaDragSelectDeferred
import qualified Test.Headless.Lua.TextWrapping as LuaTextWrapping
import qualified Test.Headless.Lua.TextTruncation as LuaTextTruncation
import qualified Test.Headless.Lua.WidthTruncation as LuaWidthTruncation
import qualified Test.Headless.Lua.ShellInput as LuaShellInput
import qualified Test.Headless.Lua.RandomStream as LuaRandomStream
import qualified Test.Headless.Lua.ConsoleTableKeys as LuaConsoleTableKeys
import qualified Test.Headless.Lua.InjuryNarration as LuaInjuryNarration
import qualified Test.Headless.UI.Slider as UISlider
import qualified Test.Headless.UI.BarFillColor as UIBarFillColor
import qualified Test.Headless.UI.ClickCorrelation as UIClickCorrelation
import qualified Test.Headless.UI.TransferContextMenu as UITransferContextMenu
import qualified Test.Headless.UI.ItemList as UIItemList
import qualified Test.Headless.UI.ContainerWindowStack as UIContainerWindowStack
import qualified Test.Headless.UI.TransferGestures as UITransferGestures
import qualified Test.Headless.UI.ConsumableGesture as UIConsumableGesture
import qualified Test.Headless.UI.TransferSession as UITransferSession
import qualified Test.Headless.World.Calendar as Calendar
import qualified Test.Headless.World.FloraGrowth as FloraGrowth
import qualified Test.Headless.River.CalderaHazard as RiverCalderaHazard
import qualified Test.Headless.River.InlandSources as RiverInlandSources
import qualified Test.Headless.World.Render.FrontWallLift as FrontWallLift
import qualified Test.Headless.World.Render.StructureRotation as StructureRotation
import qualified Test.Headless.World.Render.GroundItemSeam as GroundItemSeam
import qualified Test.Headless.World.Render.StructureSeam as StructureSeam
import qualified Test.Headless.World.Render.PickSeam as PickSeam
import qualified Test.Headless.World.Render.QuadSnapshot as QuadSnapshot
import qualified Test.Headless.World.Render.SolarAttribution as SolarAttribution
import qualified Test.Headless.World.Render.DesignationFaceMap as DesignationFaceMap
import qualified Test.Headless.World.DesignationSeam as DesignationSeam
import qualified Test.Headless.World.StructureStage as StructureStage
import qualified Test.Headless.World.StructurePaletteResidue as StructurePaletteResidue
import qualified Test.Headless.Structure.ArtCatalog as StructureArtCatalog
import qualified Test.Headless.World.Render.SideFace as RenderSideFace
import qualified Test.Headless.World.Render.ZTrackSeam as ZTrackSeam
import qualified Test.Headless.World.Render.SlopeBit as RenderSlopeBit
import qualified Test.Headless.World.Render.WaterSlope as RenderWaterSlope
import qualified Test.Headless.World.Render.ZoomBakeUV as ZoomBakeUV
import qualified Test.Headless.Render.ViewportGuard as ViewportGuard
import qualified Test.Headless.Render.QuadVertices as QuadVertices
import qualified Test.Headless.Graphics.BindlessRebind as BindlessRebind
import qualified Test.Headless.Graphics.BindlessRelease as BindlessRelease
import qualified Test.Headless.Graphics.BindlessPublish as BindlessPublish
import qualified Test.Headless.Lua.AssetFailure as LuaAssetFailure
import qualified Test.Headless.Core.ConfigState as ConfigState
import qualified Test.Headless.Core.Queue as CoreQueue
import qualified Test.Headless.Core.LogCategoryEnv as LogCategoryEnv
import qualified Test.Headless.Core.LogMonad as LogMonad
import qualified Test.Headless.Core.LogParity as LogParity
import qualified Test.Headless.Core.LogThresholdEnv as LogThresholdEnv
import qualified Test.Headless.Core.LoopStartup as LoopStartup
import qualified Test.Headless.Core.ShutdownAtlasRelease as ShutdownAtlasRelease
import qualified Test.Headless.Core.WorkerLifecycle as WorkerLifecycle
import qualified Test.Headless.Core.DebugListener as DebugListener
import qualified Test.Headless.App.Cli as AppCli
import qualified Test.Headless.App.ChunkRegion as AppChunkRegion
import qualified Test.Headless.App.PreviewConfig as PreviewConfig
import qualified Test.Headless.App.ResourceRoot as AppResourceRoot
import qualified Test.Headless.Camera.GotoClamp as GotoClamp
import qualified Test.Headless.Camera.ZoomScroll as ZoomScroll
import qualified Test.Headless.Scene.BatchMerge as BatchMerge
import qualified Test.Headless.Render.PanMargin as PanMargin
import qualified Test.Headless.Location.Bounds as LocationBounds
import qualified Test.Headless.Building.PageBinding as BuildingPageBinding
import qualified Test.Headless.Building.PortalSpawnBinding as BuildingPortalSpawnBinding
import qualified Test.Headless.Building.Placement as BuildingPlacement
import qualified Test.Headless.Building.RemoteWarning as BuildingRemoteWarning
import qualified Test.Headless.Building.WorkbenchConstruction
    as WorkbenchConstruction
import qualified Test.Headless.Save.AutosaveGuards as AutosaveGuards
import qualified Test.Headless.Save.AutosaveListing as AutosaveListing
import qualified Test.Headless.Save.MenuListingOrder as MenuListingOrder
import qualified Test.Headless.Save.Barrier as SaveBarrier
import qualified Test.Headless.Load.Status as LoadStatus
import qualified Test.Headless.Save.Snapshot as SaveSnapshot
import qualified Test.Headless.Location.Discovery as LocationDiscovery
import qualified Test.Headless.World.LocationDiscovery as WorldLocationDiscovery
import qualified Test.Headless.Building.Knowledge as ContainerKnowledge
import qualified Test.Headless.Item.NestedContents as NestedContents
import qualified Test.Headless.Location.Instance as LocationInstance
import qualified Test.Headless.Location.Naming as LocationNaming
import qualified Test.Headless.River.Naming as RiverNaming
import qualified Test.Headless.Location.LootDeterminism as LocationLootDeterminism
import qualified Test.Headless.Location.MapIcons as LocationMapIcons
import qualified Test.Headless.Location.Stamping as LocationStamping
import qualified Test.Headless.Tutorial.Definitions as TutorialDefinitions
import qualified Test.Headless.Lua.SaveModules as LuaSaveModules
import qualified Test.Headless.Lua.SharedHelpers as LuaSharedHelpers
import qualified Test.Headless.Lua.SaveBridge as LuaSaveBridge
import qualified Test.Headless.Lua.TutorialProgress as LuaTutorialProgress
import qualified Test.Headless.Lua.TutorialEvaluation as LuaTutorialEvaluation
import qualified Test.Headless.Lua.UnitAiLocations as LuaUnitAiLocations
import qualified Test.Headless.Lua.UnitAiHold as LuaUnitAiHold
import qualified Test.Headless.Lua.UnitAiCombatMove as LuaUnitAiCombatMove
import qualified Test.Headless.Lua.UnitAiEncounter as LuaUnitAiEncounter
import qualified Test.Headless.Lua.UnitAiStall as LuaUnitAiStall
import qualified Test.Headless.Lua.UnitAiHarvest as LuaUnitAiHarvest
import qualified Test.Headless.Lua.UnitAiLogisticsTargets as LuaUnitAiLogisticsTargets
import qualified Test.Headless.Lua.UnitAiPageTargets as LuaUnitAiPageTargets
import qualified Test.Headless.Lua.UnitAiLoadReset as LuaUnitAiLoadReset
import qualified Test.Headless.Lua.UnitAiReconcile as LuaUnitAiReconcile
import qualified Test.Headless.Lua.SessionTeardown as LuaSessionTeardown
import qualified Test.Headless.Lua.BuildingSpawnSentinel as LuaBuildingSpawnSentinel
import qualified Test.Headless.Lua.WorkClaimCapacity as LuaWorkClaimCapacity
import qualified Test.Headless.Lua.Faction as LuaFaction
import qualified Test.Headless.Unit.Faction as UnitFaction
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
main = hspec $ do
    -- ONE engine for all worldgen specs. Worlds are memoized by
    -- (seed, size, plateCount) via Test.Headless.Harness.sharedWorld
    -- — generation is the entire cost of this suite, so specs share
    -- worlds instead of regenerating identical ones per module
    -- (was 16 generations / ~185 s; now ~6 / well under a minute).
    aroundAll withHeadlessEngine $ do
        describe "World Generation" WorldGen.spec
        describe "World.SelectTileZ" SelectTileZ.spec
        UITransferContextMenu.spec
        UIItemList.spec
        describe "World.ActionOutcome" ActionOutcome.spec
        ChunkQueueFrame.spec
        describe "Geology" Geology.spec
        describe "Chunk/Fast Parity" Parity.spec
        describe "Biome Flatness" Flatness.spec
        describe "Column Exposure" Exposure.spec
        describe "Zoom/Detail Parity" ZoomParity.spec
        describe "Border Probe" BorderProbe.spec
        Climate.spec
        describe "Asset.TextureFallback" TextureFallback.spec
        -- Not worldgen — needs the live EngineEnv's queues/refs to
        -- drive the #697 fence relay by hand (harness runs neither
        -- the input nor the Lua thread, so the queues are the test's).
        describe "Input.Followup" InputFollowup.spec
        -- #1927: a split hold's modifier lifetime is a property of the
        -- ownership record the INPUT THREAD keeps between two
        -- independent verb calls, so it can only be asserted as state
        -- against a live env — same technique as Input.Followup above.
        -- The name keeps `--match "Input.Inject"` (the issue's focused
        -- acceptance command) selecting it alongside the pure
        -- sequence-shape group.
        describe "Input.Inject ownership" InputInjectOwnership.spec
        -- Same technique as Input.Followup above: no world dependency
        -- at all, just the live EngineEnv's queues/refs to construct a
        -- real Lua backend and drive processLuaMsg directly.
        describe "Lua.DebugQueue" LuaDebugQueue.spec
        -- Same technique as Lua.DebugQueue above: the live EngineEnv's
        -- queues/refs are only there to build a real Lua backend, whose
        -- console boundary is what #1955's key contract lives on.
        describe "debug console table keys" LuaConsoleTableKeys.spec
        LuaUnitAiReconcile.envSpec
        describe "Lua.RenderQueue" LuaRenderQueue.spec
        describe "Lua.PreviewGeneration" LuaPreviewGeneration.spec
        describe "Lua.PauseGate" LuaPauseGate.spec
        describe "Lua.ScriptState" LuaScriptState.spec
        LuaTickInterval.spec
        -- Same technique as Input.Followup above: F4 (#730) Layer A's
        -- non-click producers live inside Engine.Input.Thread's real
        -- processInputs, driven directly against the live EngineEnv.
        describe "Input.LayerA" InputLayerA.spec
        -- Same technique again (#1693): the framebuffer-resize →
        -- swapchain-recreation request is decided entirely from the
        -- live env's framebufferSizeRef and the main-thread
        -- GraphicsState record, so the whole contract is provable with
        -- no GPU.
        describe "swapchain resize request" GraphicsSwapchainResize.spec
        describe "River.InlandSources" RiverInlandSources.spec
        -- Capability-projection aliasing (#891): pure handle-equality
        -- checks against the already-booted env — no worldgen, no
        -- mutation, so it rides the shared engine above.
        describe "Capability.Building projections" CapabilityBuilding.spec
        -- #1896 adds the one property a plain projection test cannot
        -- carry: the `ReadOnlyRef` wrapper ALIASES its handle rather
        -- than snapshotting it, so a write through the raw writer
        -- record is observed through the read-only view.
        describe "ReadOnlyRef and Capability.ContentRegistriesView projections"
                 CapabilityContentRegistriesView.spec
        describe "Capability.Events projections" CapabilityEvents.spec
        describe "Capability.Input projections" CapabilityInput.spec
        describe "Capability.Render projections" CapabilityRender.spec
        describe "Capability.RenderHandoff projections" CapabilityRenderHandoff.spec
        describe "Capability.SaveLoad projections" CapabilitySaveLoad.spec
        describe "Capability.Ui projections" CapabilityUi.spec
        describe "Capability.UnitCombat projections" CapabilityUnitCombat.spec
        describe "Capability.WorldSim projections" CapabilityWorldSim.spec
        -- Same technique: no world dependency, just the live EngineEnv's
        -- content-registry refs projected through the real capability so
        -- the Lua-facing tutorial surface is exercised end to end (#957).
        TutorialDefinitions.luaSpec
        LuaTutorialEvaluation.luaSpec
        -- Same technique (#1946): the loot-table load-and-register
        -- boundary is entirely the live env's content-registry ref
        -- projected through the real capability, so it rides the
        -- shared engine and borrows/restores that one ref.
        LocationLootDeterminism.luaSpec
    -- Own engine (not the shared-worlds one above): the #707 save/load
    -- story snapshots and reloads EVERY live page, so an empty world
    -- manager keeps it scoped to its own cheap private w8 pages instead
    -- of re-restoring the shared worlds.
    aroundAll withHeadlessEngine $
        describe "World identity (#707)" WorldIdentity.spec
    -- Own engine (#1718): creates an arena page, which the shared-worlds
    -- engine above must not gain. Its describe names "Arena" so the
    -- issue's `--match "Arena"` acceptance command selects it alongside
    -- the pure contract below.
    aroundAll withHeadlessEngine $
        describe "Arena base seeding (#1718)" ArenaSeed.engineSpec
    -- Own engine (#1246): writes a populated transfer-order store into a
    -- live page's WorldState and saves it, which the shared-worlds
    -- engine above must not see. Registered under the SAME describe as
    -- the pure contract gate so `--match "persistence contract"` covers
    -- both halves -- the codec round trip and the live capture/restore.
    aroundAll withHeadlessEngine $
        describe "persistence contract" WorldTransferOrders.spec
    -- Own engine (#1596): both halves EDIT their own private w8 pages
    -- and hand-deliver WorldApplyFluids batches to the live world
    -- thread, which the shared-worlds engine above must not see. The
    -- save half is registered under the SAME "persistence contract"
    -- describe as the transfer-order gate above, and for the same
    -- reason -- it is the live capture/replay half of that contract,
    -- which no pure codec test can reach.
    aroundAll withHeadlessEngine $ do
        FluidWritebackStaleness.spec
        describe "persistence contract" FluidWritebackStaleness.saveSpec
    -- Own engine: #913's failure-report cases queue a WorldSave for a
    -- page that does not exist, and assert on the shared event log --
    -- both of which would be noise (and, for the log, a source of
    -- cross-talk) inside the shared-worlds engine above.
    aroundAll withHeadlessEngine $
        describe "autosave engine guards (#913)" AutosaveGuards.spec
    -- Own engine: the live transfer API (#1085) WRITES real units,
    -- buildings and items into the manager refs to exercise all four
    -- mutation paths, which would corrupt the shared-worlds engine
    -- above (same precedent as World identity / autosave guards).
    aroundAll withHeadlessEngine UnitTransferApi.spec
    -- Own engine (#1733): the live unit.addXP boundary WRITES the
    -- unit manager ref (and seeds a deliberately corrupt stat map),
    -- so it cannot share the worldgen engine above.
    aroundAll withHeadlessEngine UnitAddXpApi.spec
    -- Own engine (#1605): the live unit.moveTo boundary swaps the
    -- engine's logger to capture the warning it emits and drains the
    -- unit command queue, so it cannot share the worldgen engine.
    aroundAll withHeadlessEngine PathingMoveToApi.spec
    -- Own engine for the same reason (#1247): the order executor writes
    -- the unit/building manager refs AND installs its own two-page world
    -- manager so each page brings its own live wsTransferOrdersRef.
    -- Its describe begins "Unit transfer Lua API" so that --match reaches
    -- the contract verbs and the order verbs in one gate.
    aroundAll withHeadlessEngine UnitTransferOrderApi.spec
    -- Own engine (#1673): the four LAX cargo verbs WRITE the unit and
    -- building manager refs and install their own two-page world
    -- manager, so like the two specs above they cannot share the
    -- worldgen engine.
    aroundAll withHeadlessEngine UnitCargoApi.spec
    -- Own engine (#1205): the live power.placeNode path WRITES the
    -- unit/building manager refs and installs its own two-page world
    -- manager, so it cannot share the worldgen engine above.
    aroundAll withHeadlessEngine PowerPlacement.spec

    -- #1238: the two nested item-container reads the container-window
    -- stack opens a level from, driven through the registered Lua API
    -- against real live refs.
    aroundAll withHeadlessEngine NestedContents.spec
    -- Own engine for the same reason (#1206): the demolition gate
    -- installs its own two-page world manager and drives the real
    -- building-command drain, which would disturb the shared engine.
    aroundAll withHeadlessEngine PowerDemolition.spec
    -- Own engine for the same reason (#1680): the craft-bill claimant
    -- sweep installs its own two-page world manager, rewrites the unit
    -- manager, and pins the engine paused. It needs the world worker
    -- RUNNING -- the whole gate is that the REAL tickWorldTime performs
    -- the reconciliation -- and its pages carry no gen params, so that
    -- worker skips them for chunk loading.
    aroundAll withHeadlessEngine CraftBillReconcile.spec
    -- Own engine (#1585): the blood.gpuHandles gate installs its own
    -- single-page world manager and writes that page's blood handle map
    -- plus the engine-wide texture-size cache, which would disturb the
    -- shared worldgen engine above.
    aroundAll withHeadlessEngine BloodLuaApi.spec
    -- Own engine for the same reason: the #1208 ground-ownership gate
    -- installs TWO live pages and rewrites the unit/world manager refs
    -- to put a unit on the non-active one.
    aroundAll withHeadlessEngine GroundPageOwnership.spec
    -- Own engine for the same reason, and against the same two-page
    -- fixture: #1666's pickup-order gate keeps page A active while the
    -- carrier and its target sit on live, non-active page B, and drives
    -- the production scripts/unit_ai_pickup.lua over it.
    aroundAll withHeadlessEngine LuaUnitAiPickupPage.spec
    -- Own engine for the same reason (#1737): the repair AI's ground
    -- rung is judged against two live pages carrying the SAME gid, and
    -- it drives the production scripts/unit_ai_repair.lua +
    -- unit_ai_repair_target.lua over the engine's real ground,
    -- inventory and page APIs.
    aroundAll withHeadlessEngine LuaUnitAiRepairGround.spec
    -- Own engine for the same reason (#1599): the pause-speed gate
    -- installs its own two-page world manager, rewrites wmVisible
    -- mid-example, and drives the real scripts/pause.lua against the
    -- live engine. Its pages carry NO gen params, so the real world
    -- worker skips them -- but the worker has to be RUNNING, because one
    -- example needs the queued world.setTimeScale drained.
    aroundAll withHeadlessEngine PauseSpeed.spec
    -- Own engine for the same reason (#1593): the unit-simulation
    -- page-ownership gate installs its own three-page world manager and
    -- rewrites the unit manager to put a unit on each. WORLD-THREAD-FREE
    -- for the same reason the etymology gate below is: its pages are
    -- hand-built emptyWorldStates carrying defaultWorldGenParams, whose
    -- wgpPlates is empty, so a real world worker picking one up for
    -- chunk loading would die in twoNearestPlates.
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
    aroundAll withHeadlessEngineNoWorld $
        describe "Language etymology (page scope)"
            LanguageEtymologyPageScope.spec
    -- Own engine (not the shared-worlds one above): needs a real
    -- pixel hit-test against loaded tile data (renderWorldCursorQuads),
    -- so it generates its own cheap private w8 page rather than sharing
    -- or disturbing the worldgen specs' engine/camera state.
    aroundAll withHeadlessEngine SelectChunk.sharedSpec
    HarnessWorkerHealth.spec
    describe "Wrap Seam" WrapSeam.spec
    describe "Arena base seeding (#1718)" ArenaSeed.pureSpec
    describe "WorldGen.CoastBreach" CoastBreach.spec
    describe "WorldGen.BedDepth" BedDepth.spec
    describe "WorldGen.FluidSurfaceFold" FluidSurfaceFold.spec
    describe "Asset.Types" AssetTypes.spec
    describe "Asset.FloraContent" FloraContent.spec
    describe "Asset.FloraRegrowthSchema" FloraRegrowthSchema.spec
    describe "Asset.UnitInventory" AssetUnitInventory.spec
    describe "Asset.YamlList" AssetYamlList.spec
    describe "material move_cost validation" AssetMaterialMoveCost.spec
    describe "Preview.Discovery" PreviewDiscovery.spec
    describe "Preview.UnitAnimation" PreviewUnitAnimation.spec
    describe "Preview.Building" PreviewBuilding.spec
    describe "Preview.Zoom" PreviewZoom.spec
    describe "Workbench construction animation" WorkbenchConstruction.spec
    describe "Bindless texture filter rebinding" BindlessRebind.spec
    describe "Bindless texture release" BindlessRelease.spec
    describe "bindless registration failure" $ do
        BindlessPublish.spec
        LuaAssetFailure.spec
    describe "Unit.Pathing.Cost" PathingCost.spec
    PathingHazard.spec
    describe "Unit.Pathing.AStar" PathingAStar.spec
    describe "Unit.Pathing.Config" PathingConfig.spec
    describe "Unit.Render.pickFrame" PickFrame.spec
    UnitHitTest.spec
    UnitAtlas.spec
    aroundAll withHeadlessEngine UnitAtlasLoader.spec

    aroundAll withHeadlessEngine ItemDiscovery.spec
    aroundAll withHeadlessEngineNoWorld ItemCondition.spec
    aroundAll withHeadlessEngineNoWorld ItemSteelHelmet.spec
    -- Own engine (#1772): the craft-identity gate installs its own
    -- single-page world manager and rewrites the item, recipe and unit
    -- manager refs, exactly like the ItemCondition gate above. It needs
    -- no world -- craft.execute reads none.
    aroundAll withHeadlessEngineNoWorld CraftOutputIdentity.spec
    -- Own engine (#1716): the live unit.feed gate WRITES the item and
    -- unit manager refs, so it cannot share the worldgen engine. It
    -- needs no world at all -- unit.feed reads neither.
    aroundAll withHeadlessEngineNoWorld ItemFoodNutrition.feedSpec
    describe "Unit.Anim" AnimTest.spec
    describe "Unit.Injury" InjuryTest.spec
    describe "Unit.InjurySpeed" InjurySpeedTest.spec
    describe "Unit.Fall" FallTest.spec
    describe "Unit.StopTransition" StopTransition.spec
    describe "Unit.Stats" StatsTest.spec
    AccessoryUnequip.spec
    SpawnShedTest.spec
    UnitTransfer.spec
    describe "Unit.NightPerception" NightPerception.spec
    describe "Unit.LineOfSight (multi-world page ownership)" LineOfSightTest.spec
    describe "World.TimeLocal" TimeLocal.spec
    describe "Item.Temperature" ItemTemp.spec
    describe "Item.BuffYaml" ItemBuffYaml.spec
    describe "Item.QualityTier" ItemQualityTier.spec
    describe "Item.ContentsSignature" ItemContentsSig.spec
    describe "Item.BulkStorage" ItemBulkStorage.spec
    describe "Item.FoodNutrition" ItemFoodNutrition.spec
    describe "Item.Materialize" ItemMaterialize.spec
    describe "World.Save.Sanitize" SaveSanitize.spec
    describe "World.Save.Serialize" SaveSerialize.spec
    describe "save envelope" SaveEnvelope.spec
    describe "save components" SaveComponents.spec
    describe "save migrations" SaveCompat.spec
    describe "persistence reference integrity" SaveIntegrity.spec
    describe "persistence reference integrity" LuaSaveBridge.spec
    describe "atomic save storage" SaveStorage.spec
    describe "persistence contract" SaveContract.spec
    describe "autosave staging slots (#1413)" AutosaveListing.spec
    MenuListingOrder.spec
    describe "Save.Barrier" SaveBarrier.spec
    describe "Load.Status" LoadStatus.spec
    describe "Save.Snapshot" SaveSnapshot.spec
    describe "Lua persistence components" LuaSaveModules.spec
    describe "Lua shared helpers" LuaSharedHelpers.spec
    LuaTutorialProgress.spec
    LuaTutorialEvaluation.spec
    LuaUnitAiLocations.spec
    LuaUnitAiHold.spec
    LuaUnitAiCombatMove.spec
    LuaUnitAiEncounter.spec
    LuaUnitAiStall.spec
    LuaUnitAiHarvest.spec
    LuaUnitAiLogisticsTargets.spec
    LuaUnitAiPageTargets.spec
    LuaUnitAiLoadReset.spec
    LuaUnitAiReconcile.spec
    LuaSessionTeardown.spec
    LuaBuildingSpawnSentinel.spec
    LuaWorkClaimCapacity.spec
    LuaFaction.spec
    describe "World.CursorInfo" CursorInfo.spec
    CursorTextureDispatch.spec
    describe "World.SelectChunk" SelectChunk.spec
    describe "World.Spoil" Spoil.spec
    describe "rendered fluid surface rule (#1112)" RenderedSurface.spec
    describe "dry island-column fluid smoothing (#1131)" IslandColumns.spec
    describe "shared chunk-coordinate derivation" ChunkCoordinates.spec
    WorldLocationDiscovery.spec
    describe "WorldGen.SoilGate" SoilGate.spec
    describe "WorldGen.SoilShed" SoilShed.spec
    describe "WorldGen.SoilRedistribution" SoilRedistribution.spec
    describe "Combat.Damage" CombatDamage.spec
    CombatMaxStamina.spec
    CombatMentalEffectiveness.spec
    describe "Combat.Severing" CombatSevering.spec
    describe "Combat.Wounds" CombatWounds.spec
    describe "World.Magma.Shape" MagmaShape.spec
    describe "Sim.Fluid.Seam" SimSeam.spec
    describe "Input.KeyNames" InputKeyNames.spec
    describe "Input.Bindings" InputBindings.spec
    describe "Input.Inject" InputInject.spec
    describe "Input.WheelPolicy" InputWheelPolicy.spec
    -- #1153: three GPU-free specs relocated out of the graphical suite,
    -- which automated gates only ever COMPILE. Each already supplies its
    -- own top-level describe except UPrelude, so `--match "UPrelude"`,
    -- `--match "Engine.Core.Queue"` and `--match "Engine.Input.State"`
    -- all still reach them.
    describe "UPrelude" UPreludeSpec.spec
    CoreQueue.spec
    InputState.spec
    describe "Graphics.VideoConfig" VideoConfig.spec
    describe "Graphics.VulkanAppIdentity" VulkanAppIdentity.spec
    BindlessFeatures.spec
    GraphicsInstancePlan.spec
    describe "Graphics.WindowMode" GraphicsWindowMode.spec
    describe "Graphics.computeAmbientLight" AmbientLight.spec
    describe "Graphics.Screenshot" GraphicsScreenshot.spec
    describe "Graphics.UniformLayout" GraphicsUniformLayout.spec
    describe "Graphics.VertexLayout" GraphicsVertexLayout.spec
    describe "Graphics.FontFallback" GraphicsFontFallback.spec
    describe "Font SDF atlas repertoire" GraphicsFontRepertoire.spec
    describe "Construct.Corners" ConstructCorners.spec
    describe "Construct.Footprint" ConstructFootprint.spec
    describe "Construct.PendingRefusal" ConstructPendingRefusal.spec
    describe "Craft.Execute" CraftExecute.spec
    ItemRepairFinite.spec
    describe "Craft.Bills" CraftBills.spec
    describe "Power.Types" PowerTypes.spec
    describe "Power.Network" PowerNetwork.spec
    describe "Language.Semantic" LanguageSemantic.spec
    describe "Language.Generated" LanguageGenerated.spec
    describe "Language.Suggest" LanguageSuggest.spec
    describe "Language etymology" LanguageEtymology.spec
    describe "Blood.Types" BloodTypes.spec
    describe "Blood.Texture" BloodTexture.spec
    describe "Blood.Impact" BloodImpact.spec
    describe "Blood.Trail" BloodTrail.spec
    describe "Blood.Teardown" BloodTeardown.spec
    describe "Create World player-facing controls" CreateWorldControls.spec
    describe "UI.Tooltip" UITooltip.spec
    describe "UI.InputOwnership" UIInputOwnership.spec
    describe "zoom-band entity input gate" UIZoomBandInputGate.spec
    describe "hud hover gameplay-input gate" UIHudHoverGate.spec
    describe "Unit Info row selection gate" UIUnitInfoRowSelection.spec
    describe "UI.ElementInputPolicy" UIElementInputPolicy.spec
    describe "UI.ControlActivation" UIControlActivation.spec
    describe "UI hierarchy structural ownership" UIHierarchyOwnership.spec
    describe "UI.FocusNavigation" UIFocusNavigation.spec
    describe "UI.Clipping" UIClipping.spec
    describe "UI.InteractiveBounds" UIInteractiveBounds.spec
    describe "UI.PopupPlacement" UIPopupPlacement.spec
    -- #1714: its own engine per example (the event store's sequence
    -- counter is process-lifetime, and one case drives the real
    -- load-publish reset), so it registers here rather than under the
    -- shared-worlds aroundAll above.
    PlayerEventProgress.spec
    -- #1588: its own engine per example (each case installs its own
    -- WorldManager and asserts on the event ring), so it registers
    -- here rather than under the shared-worlds aroundAll above.
    PopupCoordPage.spec
    -- #1592: its own engine AND Lua VM per example — the pre-bootstrap
    -- popup state it exercises is a once-per-process condition, so a
    -- shared module table would destroy it.
    UIPopupQueueTeardown.spec
    describe "UI.ResponsiveMenus" UIResponsiveMenus.spec
    describe "UI.ResponsiveGameplay" UIResponsiveGameplay.spec
    UISettingsDefaultsKeybinds.spec
    UISettingsRevert.spec
    describe "UI.ContainerWindowStack" UIContainerWindowStack.spec
    UITransferGestures.spec
    UIConsumableGesture.spec
    UITransferSession.spec
    describe "Tutorial HUD" UITutorialHud.spec
    describe "UI.UnicodeTextEditing" UIUnicodeTextEditing.spec
    LuaDragSelectDeferred.spec
    describe "Lua.TextWrapping" LuaTextWrapping.spec
    describe "Lua.TextTruncation" LuaTextTruncation.spec
    describe "Lua.WidthTruncation" LuaWidthTruncation.spec
    describe "Lua.ShellInput" LuaShellInput.spec
    describe "Lua random stream ownership" LuaRandomStream.spec
    describe "Lua injury narration" LuaInjuryNarration.spec
    UISlider.spec
    UIBarFillColor.spec
    UIClickCorrelation.spec
    describe "World.Calendar" Calendar.spec
    describe "World.FloraGrowth" FloraGrowth.spec
    describe "River.CalderaHazard" RiverCalderaHazard.spec
    describe "World.Render.FrontWallLift" FrontWallLift.spec
    describe "World.Render.StructureRotation" StructureRotation.spec
    describe "World.Render.GroundItemSeam" GroundItemSeam.spec
    describe "World.Render.GroundItemSeam (engine)" GroundItemSeam.engineSpec
    describe "World.Render.StructureSeam" StructureSeam.spec
    describe "World.Render.StructureSeam (engine)" StructureSeam.engineSpec
    describe "World.Render.PickSeam" PickSeam.spec

    -- #1720: its own headless engine (no worker threads), so the live
    -- camera can be rewritten between capture and build the way the
    -- main thread's pan integration does under the world thread.
    describe "World.Render.QuadSnapshot" QuadSnapshot.spec

    -- #1869: same shape as the line above and for the same reason —
    -- its own headless engine, two synthetic pages, no worker threads.
    SolarAttribution.spec
    describe "World.Render.DesignationFaceMap" DesignationFaceMap.spec
    describe "World.DesignationSeam" DesignationSeam.spec
    describe "World.DesignationSeam (engine)" DesignationSeam.engineSpec

    -- #1674: its own headless engine (no worker threads), so the
    -- WorldSetStructure structure.place emits waits to be dequeued and
    -- dispatched by the example rather than by a racing drainer.
    StructureStage.spec

    -- #1675: the same shape, for the palette residue a REJECTED
    -- structure.place used to leave behind — its own engine so the
    -- "nothing was queued" half is an assertion on an undrained queue.
    StructurePaletteResidue.spec

    -- #1842: the unplaced-piece art catalogue. Its own headless engine
    -- for the same reason as the two above -- the placement it compares
    -- against is read off the undrained WorldSetStructure -- plus the
    -- real scripts/structures.lua and scripts/wire.lua, so parity is
    -- against the builder rather than a table written in the test.
    StructureArtCatalog.spec

    -- #1602: its own headless engine (no worker threads), so a queued
    -- BuildingSpawn / WorldDesignateConstruct stays in its queue and
    -- "nothing was committed" is asserted on the queue itself.
    BuildingPageBinding.spec
    BuildingPortalSpawnBinding.spec
    describe "World.Render.ZTrackSeam" ZTrackSeam.spec
    describe "World.Render.SideFace" RenderSideFace.spec
    describe "World.Slope.slopeBit" RenderSlopeBit.spec
    describe "World.Render.WaterSlope" RenderWaterSlope.spec
    describe "World.Render.Zoom.zoomQuadWorldUVs" ZoomBakeUV.spec
    describe "Render.ViewportGuard" ViewportGuard.spec
    describe "Render.QuadVertices" QuadVertices.spec
    describe "Core.ConfigState" ConfigState.spec
    LogCategoryEnv.spec
    LogMonad.spec
    LogParity.spec
    LogThresholdEnv.spec
    LoopStartup.spec
    ShutdownAtlasRelease.spec
    WorkerLifecycle.spec
    DebugListener.spec
    AppCli.spec
    AppChunkRegion.spec
    AppResourceRoot.spec
    describe "App.Preview.Config" PreviewConfig.spec
    describe "Camera.GotoClamp" GotoClamp.spec
    describe "Camera.ZoomScroll" ZoomScroll.spec
    describe "Scene.BatchMerge" BatchMerge.spec
    describe "Render.PanMargin" PanMargin.spec
    LocationBounds.spec
    LocationDiscovery.spec
    UnitFaction.spec
    ContainerKnowledge.spec
    LocationInstance.spec
    LocationNaming.spec
    RiverNaming.spec
    LocationLootDeterminism.spec
    LocationMapIcons.spec
    LocationStamping.spec
    TutorialDefinitions.spec
    BuildingPlacement.spec
    BuildingRemoteWarning.spec
