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
import qualified Test.Headless.Unit.NightPerception as NightPerception
import qualified Test.Headless.World.TimeLocal as TimeLocal
import qualified Test.Headless.World.Climate as Climate
import qualified Test.Headless.Item.Temperature as ItemTemp
import qualified Test.Headless.Item.BuffYaml as ItemBuffYaml
import qualified Test.Headless.Item.QualityTier as ItemQualityTier
import qualified Test.Headless.Asset.TextureFallback as TextureFallback
import qualified Test.Headless.World.Save.Sanitize as SaveSanitize
import qualified Test.Headless.World.Save.Serialize as SaveSerialize
import qualified Test.Headless.World.Identity as WorldIdentity
import qualified Test.Headless.World.CursorInfo as CursorInfo
import qualified Test.Headless.World.SelectTileZ as SelectTileZ
import qualified Test.Headless.World.SelectChunk as SelectChunk
import qualified Test.Headless.World.ActionOutcome as ActionOutcome
import qualified Test.Headless.World.Spoil as Spoil
import qualified Test.Headless.Combat.Damage as CombatDamage
import qualified Test.Headless.Combat.Severing as CombatSevering
import qualified Test.Headless.Combat.Wounds as CombatWounds
import qualified Test.Headless.Magma.Shape as MagmaShape
import qualified Test.Headless.Sim.Seam as SimSeam
import qualified Test.Headless.Input.KeyNames as InputKeyNames
import qualified Test.Headless.Input.Bindings as InputBindings
import qualified Test.Headless.Input.Inject as InputInject
import qualified Test.Headless.Input.Followup as InputFollowup
import qualified Test.Headless.Input.LayerA as InputLayerA
import qualified Test.Headless.Graphics.VideoConfig as VideoConfig
import qualified Test.Headless.Graphics.AmbientLight as AmbientLight
import qualified Test.Headless.Graphics.Screenshot as GraphicsScreenshot
import qualified Test.Headless.Construct.Corners as ConstructCorners
import qualified Test.Headless.Construct.Footprint as ConstructFootprint
import qualified Test.Headless.Craft.Execute as CraftExecute
import qualified Test.Headless.Craft.Bills as CraftBills
import qualified Test.Headless.Power.Types as PowerTypes
import qualified Test.Headless.Power.Network as PowerNetwork
import qualified Test.Headless.Language.Semantic as LanguageSemantic
import qualified Test.Headless.Language.Generated as LanguageGenerated
import qualified Test.Headless.Blood.Types as BloodTypes
import qualified Test.Headless.Blood.Texture as BloodTexture
import qualified Test.Headless.Blood.Impact as BloodImpact
import qualified Test.Headless.Blood.Teardown as BloodTeardown
import qualified Test.Headless.UI.CreateWorldControls as CreateWorldControls
import qualified Test.Headless.UI.Tooltip as UITooltip
import qualified Test.Headless.UI.InputOwnership as UIInputOwnership
import qualified Test.Headless.UI.ElementInputPolicy as UIElementInputPolicy
import qualified Test.Headless.UI.UnicodeTextEditing as UIUnicodeTextEditing
import qualified Test.Headless.World.Calendar as Calendar
import qualified Test.Headless.World.FloraGrowth as FloraGrowth
import qualified Test.Headless.River.Graph as RiverGraph
import qualified Test.Headless.River.CalderaHazard as RiverCalderaHazard
import qualified Test.Headless.River.InlandSources as RiverInlandSources
import qualified Test.Headless.World.Render.FrontWallLift as FrontWallLift
import qualified Test.Headless.World.Render.SideFace as RenderSideFace
import qualified Test.Headless.World.Render.SlopeBit as RenderSlopeBit
import qualified Test.Headless.World.Render.WaterSlope as RenderWaterSlope
import qualified Test.Headless.World.Render.ZoomBakeUV as ZoomBakeUV
import qualified Test.Headless.Render.ViewportGuard as ViewportGuard
import qualified Test.Headless.Core.ConfigState as ConfigState
import qualified Test.Headless.Camera.GotoClamp as GotoClamp
import qualified Test.Headless.Camera.ZoomScroll as ZoomScroll
import qualified Test.Headless.Scene.BatchMerge as BatchMerge
import qualified Test.Headless.Render.PanMargin as PanMargin
import qualified Test.Headless.Location.Bounds as LocationBounds
import qualified Test.Headless.Save.Barrier as SaveBarrier

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
        describe "World.ActionOutcome" ActionOutcome.spec
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
        -- Same technique as Input.Followup above: F4 (#730) Layer A's
        -- non-click producers live inside Engine.Input.Thread's real
        -- processInputs, driven directly against the live EngineEnv.
        describe "Input.LayerA" InputLayerA.spec
        describe "River.InlandSources" RiverInlandSources.spec
    -- Own engine (not the shared-worlds one above): the #707 save/load
    -- story snapshots and reloads EVERY live page, so an empty world
    -- manager keeps it scoped to its own cheap private w8 pages instead
    -- of re-restoring the shared worlds.
    aroundAll withHeadlessEngine $
        describe "World identity (#707)" WorldIdentity.spec
    -- Own engine (not the shared-worlds one above): needs a real
    -- pixel hit-test against loaded tile data (renderWorldCursorQuads),
    -- so it generates its own cheap private w8 page rather than sharing
    -- or disturbing the worldgen specs' engine/camera state.
    aroundAll withHeadlessEngine SelectChunk.sharedSpec
    describe "Wrap Seam" WrapSeam.spec
    describe "WorldGen.CoastBreach" CoastBreach.spec
    describe "WorldGen.BedDepth" BedDepth.spec
    describe "Unit.Pathing.Cost" PathingCost.spec
    describe "Unit.Pathing.AStar" PathingAStar.spec
    describe "Unit.Pathing.Config" PathingConfig.spec
    describe "Unit.Render.pickFrame" PickFrame.spec
    describe "Unit.Anim" AnimTest.spec
    describe "Unit.Injury" InjuryTest.spec
    describe "Unit.InjurySpeed" InjurySpeedTest.spec
    describe "Unit.Fall" FallTest.spec
    describe "Unit.Stats" StatsTest.spec
    describe "Unit.NightPerception" NightPerception.spec
    describe "World.TimeLocal" TimeLocal.spec
    describe "Item.Temperature" ItemTemp.spec
    describe "Item.BuffYaml" ItemBuffYaml.spec
    describe "Item.QualityTier" ItemQualityTier.spec
    describe "World.Save.Sanitize" SaveSanitize.spec
    describe "World.Save.Serialize" SaveSerialize.spec
    describe "Save.Barrier" SaveBarrier.spec
    describe "World.CursorInfo" CursorInfo.spec
    describe "World.SelectChunk" SelectChunk.spec
    describe "World.Spoil" Spoil.spec
    describe "WorldGen.SoilGate" SoilGate.spec
    describe "WorldGen.SoilShed" SoilShed.spec
    describe "Combat.Damage" CombatDamage.spec
    describe "Combat.Severing" CombatSevering.spec
    describe "Combat.Wounds" CombatWounds.spec
    describe "World.Magma.Shape" MagmaShape.spec
    describe "Sim.Fluid.Seam" SimSeam.spec
    describe "Input.KeyNames" InputKeyNames.spec
    describe "Input.Bindings" InputBindings.spec
    describe "Input.Inject" InputInject.spec
    describe "Graphics.VideoConfig" VideoConfig.spec
    describe "Graphics.computeAmbientLight" AmbientLight.spec
    describe "Graphics.Screenshot" GraphicsScreenshot.spec
    describe "Construct.Corners" ConstructCorners.spec
    describe "Construct.Footprint" ConstructFootprint.spec
    describe "Craft.Execute" CraftExecute.spec
    describe "Craft.Bills" CraftBills.spec
    describe "Power.Types" PowerTypes.spec
    describe "Power.Network" PowerNetwork.spec
    describe "Language.Semantic" LanguageSemantic.spec
    describe "Language.Generated" LanguageGenerated.spec
    describe "Blood.Types" BloodTypes.spec
    describe "Blood.Texture" BloodTexture.spec
    describe "Blood.Impact" BloodImpact.spec
    describe "Blood.Teardown" BloodTeardown.spec
    describe "Create World player-facing controls" CreateWorldControls.spec
    describe "UI.Tooltip" UITooltip.spec
    describe "UI.InputOwnership" UIInputOwnership.spec
    describe "UI.ElementInputPolicy" UIElementInputPolicy.spec
    describe "UI.UnicodeTextEditing" UIUnicodeTextEditing.spec
    describe "World.Calendar" Calendar.spec
    describe "World.FloraGrowth" FloraGrowth.spec
    describe "River.Graph" RiverGraph.spec
    describe "River.CalderaHazard" RiverCalderaHazard.spec
    describe "World.Render.FrontWallLift" FrontWallLift.spec
    describe "World.Render.SideFace" RenderSideFace.spec
    describe "World.Slope.slopeBit" RenderSlopeBit.spec
    describe "World.Render.WaterSlope" RenderWaterSlope.spec
    describe "World.Render.Zoom.zoomQuadWorldUVs" ZoomBakeUV.spec
    describe "Render.ViewportGuard" ViewportGuard.spec
    describe "Core.ConfigState" ConfigState.spec
    describe "Camera.GotoClamp" GotoClamp.spec
    describe "Camera.ZoomScroll" ZoomScroll.spec
    describe "Scene.BatchMerge" BatchMerge.spec
    describe "Render.PanMargin" PanMargin.spec
    LocationBounds.spec
