module Main where

import UPrelude
import Test.Hspec
import Test.Headless.Harness (withHeadlessEngine)
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
import qualified Test.Headless.Unit.Pathing.Cost as PathingCost
import qualified Test.Headless.Unit.Pathing.AStar as PathingAStar
import qualified Test.Headless.Unit.Pathing.Config as PathingConfig
import qualified Test.Headless.Unit.Render.PickFrame as PickFrame
import qualified Test.Headless.Unit.Anim as AnimTest
import qualified Test.Headless.Unit.Injury as InjuryTest
import qualified Test.Headless.Unit.InjurySpeed as InjurySpeedTest
import qualified Test.Headless.Unit.Fall as FallTest
import qualified Test.Headless.Unit.Stats as StatsTest
import qualified Test.Headless.Unit.AccessoryUnequip as AccessoryUnequip
import qualified Test.Headless.Unit.SpawnShed as SpawnShedTest
import qualified Test.Headless.Unit.Transfer as UnitTransfer
import qualified Test.Headless.Unit.TransferApi as UnitTransferApi
import qualified Test.Headless.Unit.TransferOrderApi as UnitTransferOrderApi
import qualified Test.Headless.Unit.NightPerception as NightPerception
import qualified Test.Headless.Unit.LineOfSight as LineOfSightTest
import qualified Test.Headless.World.TimeLocal as TimeLocal
import qualified Test.Headless.World.Climate as Climate
import qualified Test.Headless.Item.GroundPageOwnership as GroundPageOwnership
import qualified Test.Headless.Item.Temperature as ItemTemp
import qualified Test.Headless.Item.BuffYaml as ItemBuffYaml
import qualified Test.Headless.Item.QualityTier as ItemQualityTier
import qualified Test.Headless.Item.BulkStorage as ItemBulkStorage
import qualified Test.Headless.Asset.TextureFallback as TextureFallback
import qualified Test.Headless.Asset.UnitInventory as AssetUnitInventory
import qualified Test.Headless.Asset.Types as AssetTypes
import qualified Test.Headless.Asset.YamlList as AssetYamlList
import qualified Test.Headless.Preview.Discovery as PreviewDiscovery
import qualified Test.Headless.Unit.Atlas as UnitAtlas
import qualified Test.Headless.Unit.Atlas.Loader as UnitAtlasLoader
import qualified Test.Headless.Preview.UnitAnimation as PreviewUnitAnimation
import qualified Test.Headless.Preview.Building as PreviewBuilding
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
import qualified Test.Headless.World.CursorInfo as CursorInfo
import qualified Test.Headless.World.SelectTileZ as SelectTileZ
import qualified Test.Headless.World.SelectChunk as SelectChunk
import qualified Test.Headless.World.ChunkQueueFrame as ChunkQueueFrame
import qualified Test.Headless.World.ActionOutcome as ActionOutcome
import qualified Test.Headless.World.Spoil as Spoil
import qualified Test.Headless.World.RenderedSurface as RenderedSurface
import qualified Test.Headless.World.IslandColumns as IslandColumns
import qualified Test.Headless.Combat.Damage as CombatDamage
import qualified Test.Headless.Combat.MentalEffectiveness as CombatMentalEffectiveness
import qualified Test.Headless.Combat.Severing as CombatSevering
import qualified Test.Headless.Combat.Wounds as CombatWounds
import qualified Test.Headless.Magma.Shape as MagmaShape
import qualified Test.Headless.Sim.Seam as SimSeam
import qualified Test.Headless.Input.KeyNames as InputKeyNames
import qualified Test.Headless.Input.Bindings as InputBindings
import qualified Test.Headless.Input.Inject as InputInject
import qualified Test.Headless.Input.Followup as InputFollowup
import qualified Test.Headless.Lua.DebugQueue as LuaDebugQueue
import qualified Test.Headless.Lua.RenderQueue as LuaRenderQueue
import qualified Test.Headless.Lua.PreviewGeneration as LuaPreviewGeneration
import qualified Test.Headless.Lua.PauseGate as LuaPauseGate
import qualified Test.Headless.Lua.ScriptState as LuaScriptState
import qualified Test.Headless.Input.LayerA as InputLayerA
import qualified Test.Headless.Input.WheelPolicy as InputWheelPolicy
import qualified Test.Headless.Graphics.VideoConfig as VideoConfig
import qualified Test.Headless.Graphics.VulkanAppIdentity as VulkanAppIdentity
import qualified Test.Headless.Graphics.BindlessFeatures as BindlessFeatures
import qualified Test.Headless.Graphics.WindowMode as GraphicsWindowMode
import qualified Test.Headless.Graphics.AmbientLight as AmbientLight
import qualified Test.Headless.Graphics.Screenshot as GraphicsScreenshot
import qualified Test.Headless.Graphics.UniformLayout as GraphicsUniformLayout
import qualified Test.Headless.Graphics.FontFallback as GraphicsFontFallback
import qualified Test.Headless.Graphics.FontRepertoire as GraphicsFontRepertoire
import qualified Test.Headless.Construct.Corners as ConstructCorners
import qualified Test.Headless.Construct.Footprint as ConstructFootprint
import qualified Test.Headless.Craft.Execute as CraftExecute
import qualified Test.Headless.Craft.Bills as CraftBills
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
import qualified Test.Headless.UI.CreateWorldControls as CreateWorldControls
import qualified Test.Headless.UI.Tooltip as UITooltip
import qualified Test.Headless.UI.InputOwnership as UIInputOwnership
import qualified Test.Headless.UI.ElementInputPolicy as UIElementInputPolicy
import qualified Test.Headless.UI.ControlActivation as UIControlActivation
import qualified Test.Headless.UI.FocusNavigation as UIFocusNavigation
import qualified Test.Headless.UI.Clipping as UIClipping
import qualified Test.Headless.UI.InteractiveBounds as UIInteractiveBounds
import qualified Test.Headless.UI.PopupPlacement as UIPopupPlacement
import qualified Test.Headless.UI.ResponsiveMenus as UIResponsiveMenus
import qualified Test.Headless.UI.ResponsiveGameplay as UIResponsiveGameplay
import qualified Test.Headless.UI.TutorialHud as UITutorialHud
import qualified Test.Headless.UI.UnicodeTextEditing as UIUnicodeTextEditing
import qualified Test.Headless.Lua.TextWrapping as LuaTextWrapping
import qualified Test.Headless.Lua.TextTruncation as LuaTextTruncation
import qualified Test.Headless.Lua.WidthTruncation as LuaWidthTruncation
import qualified Test.Headless.Lua.ShellInput as LuaShellInput
import qualified Test.Headless.Lua.RandomStream as LuaRandomStream
import qualified Test.Headless.UI.Slider as UISlider
import qualified Test.Headless.UI.ClickCorrelation as UIClickCorrelation
import qualified Test.Headless.UI.TransferContextMenu as UITransferContextMenu
import qualified Test.Headless.UI.ItemList as UIItemList
import qualified Test.Headless.World.Calendar as Calendar
import qualified Test.Headless.World.FloraGrowth as FloraGrowth
import qualified Test.Headless.River.CalderaHazard as RiverCalderaHazard
import qualified Test.Headless.River.InlandSources as RiverInlandSources
import qualified Test.Headless.World.Render.FrontWallLift as FrontWallLift
import qualified Test.Headless.World.Render.GroundItemSeam as GroundItemSeam
import qualified Test.Headless.World.Render.PickSeam as PickSeam
import qualified Test.Headless.World.DesignationSeam as DesignationSeam
import qualified Test.Headless.World.Render.SideFace as RenderSideFace
import qualified Test.Headless.World.Render.ZTrackSeam as ZTrackSeam
import qualified Test.Headless.World.Render.SlopeBit as RenderSlopeBit
import qualified Test.Headless.World.Render.WaterSlope as RenderWaterSlope
import qualified Test.Headless.World.Render.ZoomBakeUV as ZoomBakeUV
import qualified Test.Headless.Render.ViewportGuard as ViewportGuard
import qualified Test.Headless.Graphics.BindlessRebind as BindlessRebind
import qualified Test.Headless.Graphics.BindlessRelease as BindlessRelease
import qualified Test.Headless.Core.ConfigState as ConfigState
import qualified Test.Headless.Core.LogMonad as LogMonad
import qualified Test.Headless.Core.LogParity as LogParity
import qualified Test.Headless.Core.LoopStartup as LoopStartup
import qualified Test.Headless.Core.DebugListener as DebugListener
import qualified Test.Headless.App.Cli as AppCli
import qualified Test.Headless.Camera.GotoClamp as GotoClamp
import qualified Test.Headless.Camera.ZoomScroll as ZoomScroll
import qualified Test.Headless.Scene.BatchMerge as BatchMerge
import qualified Test.Headless.Render.PanMargin as PanMargin
import qualified Test.Headless.Location.Bounds as LocationBounds
import qualified Test.Headless.Building.Placement as BuildingPlacement
import qualified Test.Headless.Building.RemoteWarning as BuildingRemoteWarning
import qualified Test.Headless.Save.AutosaveGuards as AutosaveGuards
import qualified Test.Headless.Save.Barrier as SaveBarrier
import qualified Test.Headless.Load.Status as LoadStatus
import qualified Test.Headless.Save.Snapshot as SaveSnapshot
import qualified Test.Headless.Location.Discovery as LocationDiscovery
import qualified Test.Headless.World.LocationDiscovery as WorldLocationDiscovery
import qualified Test.Headless.Building.Knowledge as ContainerKnowledge
import qualified Test.Headless.Location.Instance as LocationInstance
import qualified Test.Headless.Location.Naming as LocationNaming
import qualified Test.Headless.River.Naming as RiverNaming
import qualified Test.Headless.Location.LootDeterminism as LocationLootDeterminism
import qualified Test.Headless.Location.MapIcons as LocationMapIcons
import qualified Test.Headless.Tutorial.Definitions as TutorialDefinitions
import qualified Test.Headless.Lua.SaveModules as LuaSaveModules
import qualified Test.Headless.Lua.SaveBridge as LuaSaveBridge
import qualified Test.Headless.Lua.TutorialProgress as LuaTutorialProgress
import qualified Test.Headless.Lua.TutorialEvaluation as LuaTutorialEvaluation
import qualified Test.Headless.Lua.UnitAiLocations as LuaUnitAiLocations
import qualified Test.Headless.Lua.UnitAiStall as LuaUnitAiStall
import qualified Test.Headless.Lua.Faction as LuaFaction
import qualified Test.Headless.Unit.Faction as UnitFaction
import qualified Test.Headless.Capability.Building as CapabilityBuilding
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
        -- Same technique as Input.Followup above: no world dependency
        -- at all, just the live EngineEnv's queues/refs to construct a
        -- real Lua backend and drive processLuaMsg directly.
        describe "Lua.DebugQueue" LuaDebugQueue.spec
        describe "Lua.RenderQueue" LuaRenderQueue.spec
        describe "Lua.PreviewGeneration" LuaPreviewGeneration.spec
        describe "Lua.PauseGate" LuaPauseGate.spec
        describe "Lua.ScriptState" LuaScriptState.spec
        -- Same technique as Input.Followup above: F4 (#730) Layer A's
        -- non-click producers live inside Engine.Input.Thread's real
        -- processInputs, driven directly against the live EngineEnv.
        describe "Input.LayerA" InputLayerA.spec
        describe "River.InlandSources" RiverInlandSources.spec
        -- Capability-projection aliasing (#891): pure handle-equality
        -- checks against the already-booted env — no worldgen, no
        -- mutation, so it rides the shared engine above.
        describe "Capability.Building projections" CapabilityBuilding.spec
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
    -- Own engine (not the shared-worlds one above): the #707 save/load
    -- story snapshots and reloads EVERY live page, so an empty world
    -- manager keeps it scoped to its own cheap private w8 pages instead
    -- of re-restoring the shared worlds.
    aroundAll withHeadlessEngine $
        describe "World identity (#707)" WorldIdentity.spec
    -- Own engine (#1246): writes a populated transfer-order store into a
    -- live page's WorldState and saves it, which the shared-worlds
    -- engine above must not see. Registered under the SAME describe as
    -- the pure contract gate so `--match "persistence contract"` covers
    -- both halves -- the codec round trip and the live capture/restore.
    aroundAll withHeadlessEngine $
        describe "persistence contract" WorldTransferOrders.spec
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
    -- Own engine for the same reason (#1247): the order executor writes
    -- the unit/building manager refs AND installs its own two-page world
    -- manager so each page brings its own live wsTransferOrdersRef.
    -- Its describe begins "Unit transfer Lua API" so that --match reaches
    -- the contract verbs and the order verbs in one gate.
    aroundAll withHeadlessEngine UnitTransferOrderApi.spec
    -- Own engine (#1205): the live power.placeNode path WRITES the
    -- unit/building manager refs and installs its own two-page world
    -- manager, so it cannot share the worldgen engine above.
    aroundAll withHeadlessEngine PowerPlacement.spec
    -- Own engine for the same reason (#1206): the demolition gate
    -- installs its own two-page world manager and drives the real
    -- building-command drain, which would disturb the shared engine.
    aroundAll withHeadlessEngine PowerDemolition.spec
    -- Own engine for the same reason: the #1208 ground-ownership gate
    -- installs TWO live pages and rewrites the unit/world manager refs
    -- to put a unit on the non-active one.
    aroundAll withHeadlessEngine GroundPageOwnership.spec
    -- Own engine for the same reason (#1265): the etymology page-scope
    -- gate installs its own two-page world manager, one page inactive,
    -- to drive world.getEtymology across the target/recurrence boundary.
    -- Named so `--match "Language etymology"` reaches it alongside the
    -- pure suite below.
    aroundAll withHeadlessEngine $
        describe "Language etymology (page scope)"
            LanguageEtymologyPageScope.spec
    -- Own engine (not the shared-worlds one above): needs a real
    -- pixel hit-test against loaded tile data (renderWorldCursorQuads),
    -- so it generates its own cheap private w8 page rather than sharing
    -- or disturbing the worldgen specs' engine/camera state.
    aroundAll withHeadlessEngine SelectChunk.sharedSpec
    describe "Wrap Seam" WrapSeam.spec
    describe "WorldGen.CoastBreach" CoastBreach.spec
    describe "WorldGen.BedDepth" BedDepth.spec
    describe "Asset.Types" AssetTypes.spec
    describe "Asset.UnitInventory" AssetUnitInventory.spec
    describe "Asset.YamlList" AssetYamlList.spec
    describe "Preview.Discovery" PreviewDiscovery.spec
    describe "Preview.UnitAnimation" PreviewUnitAnimation.spec
    describe "Preview.Building" PreviewBuilding.spec
    describe "Bindless texture filter rebinding" BindlessRebind.spec
    describe "Bindless texture release" BindlessRelease.spec
    describe "Unit.Pathing.Cost" PathingCost.spec
    describe "Unit.Pathing.AStar" PathingAStar.spec
    describe "Unit.Pathing.Config" PathingConfig.spec
    describe "Unit.Render.pickFrame" PickFrame.spec
    UnitAtlas.spec
    aroundAll withHeadlessEngine UnitAtlasLoader.spec
    describe "Unit.Anim" AnimTest.spec
    describe "Unit.Injury" InjuryTest.spec
    describe "Unit.InjurySpeed" InjurySpeedTest.spec
    describe "Unit.Fall" FallTest.spec
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
    describe "Item.BulkStorage" ItemBulkStorage.spec
    describe "World.Save.Sanitize" SaveSanitize.spec
    describe "World.Save.Serialize" SaveSerialize.spec
    describe "save envelope" SaveEnvelope.spec
    describe "save components" SaveComponents.spec
    describe "save migrations" SaveCompat.spec
    describe "persistence reference integrity" SaveIntegrity.spec
    describe "persistence reference integrity" LuaSaveBridge.spec
    describe "atomic save storage" SaveStorage.spec
    describe "persistence contract" SaveContract.spec
    describe "Save.Barrier" SaveBarrier.spec
    describe "Load.Status" LoadStatus.spec
    describe "Save.Snapshot" SaveSnapshot.spec
    describe "Lua persistence components" LuaSaveModules.spec
    LuaTutorialProgress.spec
    LuaTutorialEvaluation.spec
    LuaUnitAiLocations.spec
    LuaUnitAiStall.spec
    LuaFaction.spec
    describe "World.CursorInfo" CursorInfo.spec
    describe "World.SelectChunk" SelectChunk.spec
    describe "World.Spoil" Spoil.spec
    describe "rendered fluid surface rule (#1112)" RenderedSurface.spec
    describe "dry island-column fluid smoothing (#1131)" IslandColumns.spec
    WorldLocationDiscovery.spec
    describe "WorldGen.SoilGate" SoilGate.spec
    describe "WorldGen.SoilShed" SoilShed.spec
    describe "WorldGen.SoilRedistribution" SoilRedistribution.spec
    describe "Combat.Damage" CombatDamage.spec
    CombatMentalEffectiveness.spec
    describe "Combat.Severing" CombatSevering.spec
    describe "Combat.Wounds" CombatWounds.spec
    describe "World.Magma.Shape" MagmaShape.spec
    describe "Sim.Fluid.Seam" SimSeam.spec
    describe "Input.KeyNames" InputKeyNames.spec
    describe "Input.Bindings" InputBindings.spec
    describe "Input.Inject" InputInject.spec
    describe "Input.WheelPolicy" InputWheelPolicy.spec
    describe "Graphics.VideoConfig" VideoConfig.spec
    describe "Graphics.VulkanAppIdentity" VulkanAppIdentity.spec
    BindlessFeatures.spec
    describe "Graphics.WindowMode" GraphicsWindowMode.spec
    describe "Graphics.computeAmbientLight" AmbientLight.spec
    describe "Graphics.Screenshot" GraphicsScreenshot.spec
    describe "Graphics.UniformLayout" GraphicsUniformLayout.spec
    describe "Graphics.FontFallback" GraphicsFontFallback.spec
    describe "Font SDF atlas repertoire" GraphicsFontRepertoire.spec
    describe "Construct.Corners" ConstructCorners.spec
    describe "Construct.Footprint" ConstructFootprint.spec
    describe "Craft.Execute" CraftExecute.spec
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
    describe "UI.ElementInputPolicy" UIElementInputPolicy.spec
    describe "UI.ControlActivation" UIControlActivation.spec
    describe "UI.FocusNavigation" UIFocusNavigation.spec
    describe "UI.Clipping" UIClipping.spec
    describe "UI.InteractiveBounds" UIInteractiveBounds.spec
    describe "UI.PopupPlacement" UIPopupPlacement.spec
    describe "UI.ResponsiveMenus" UIResponsiveMenus.spec
    describe "UI.ResponsiveGameplay" UIResponsiveGameplay.spec
    describe "Tutorial HUD" UITutorialHud.spec
    describe "UI.UnicodeTextEditing" UIUnicodeTextEditing.spec
    describe "Lua.TextWrapping" LuaTextWrapping.spec
    describe "Lua.TextTruncation" LuaTextTruncation.spec
    describe "Lua.WidthTruncation" LuaWidthTruncation.spec
    describe "Lua.ShellInput" LuaShellInput.spec
    describe "Lua random stream ownership" LuaRandomStream.spec
    UISlider.spec
    UIClickCorrelation.spec
    describe "World.Calendar" Calendar.spec
    describe "World.FloraGrowth" FloraGrowth.spec
    describe "River.CalderaHazard" RiverCalderaHazard.spec
    describe "World.Render.FrontWallLift" FrontWallLift.spec
    describe "World.Render.GroundItemSeam" GroundItemSeam.spec
    describe "World.Render.GroundItemSeam (engine)" GroundItemSeam.engineSpec
    describe "World.Render.PickSeam" PickSeam.spec
    describe "World.DesignationSeam" DesignationSeam.spec
    describe "World.DesignationSeam (engine)" DesignationSeam.engineSpec
    describe "World.Render.ZTrackSeam" ZTrackSeam.spec
    describe "World.Render.SideFace" RenderSideFace.spec
    describe "World.Slope.slopeBit" RenderSlopeBit.spec
    describe "World.Render.WaterSlope" RenderWaterSlope.spec
    describe "World.Render.Zoom.zoomQuadWorldUVs" ZoomBakeUV.spec
    describe "Render.ViewportGuard" ViewportGuard.spec
    describe "Core.ConfigState" ConfigState.spec
    LogMonad.spec
    LogParity.spec
    LoopStartup.spec
    DebugListener.spec
    AppCli.spec
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
    TutorialDefinitions.spec
    BuildingPlacement.spec
    BuildingRemoteWarning.spec
