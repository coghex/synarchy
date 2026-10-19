{-# LANGUAGE Strict #-}
-- | The whole world-content construction (#2711, SCN-11 of epic #2698):
--   terrain, fluid and flora entries in, one 'WorldContent' out — the
--   durable edits, plant state, tag memberships, scenario-to-runtime
--   identity mapping and diagnostics. Pure: it reads its arguments and
--   installs nothing ("Scenario.World.Install" commits it).
--
--   The three families are built in dependency order — terrain first
--   (fluid wetness and a plant's default z depend on the FINAL terrain
--   top of a tile), then fluid, then flora — and their edits are laid
--   into one per-chunk log in that order, so replaying the log rebuilds
--   exactly what was constructed.
module Scenario.World.Build
    ( WorldBuildEnv(..)
    , WorldContent(..)
    , buildWorldContent
    , groupEditsByChunk
    , editTile
    ) where

import UPrelude
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Gameplay.Tags.Types (GameplayTag)
import Scenario.Types
import Scenario.World.Flora
import Scenario.World.Terrain
import Scenario.World.Types (WorldRefs(..))
import World.Arena.Bounds (ArenaBounds)
import World.Edit.Types (WorldEdit(..), WorldEdits, appendEdit, emptyWorldEdits)
import World.Flora.CropPlot (CropPlot)
import World.Flora.Identity (FloraInstanceId)
import World.Flora.Types (FloraCatalog)
import World.Generate.Coordinates (globalToChunk)
import World.Material.Id (MaterialId)

-- | Everything construction reads from the page and the engine.
data WorldBuildEnv = WorldBuildEnv
    { wbeBounds   ∷ !(Maybe ArenaBounds)
    , wbeMaterial ∷ !(Text → Maybe MaterialId)
    , wbeFlora    ∷ !FloraCatalog
    , wbeAbsDay   ∷ !Int
    , wbeCursor   ∷ !Word64
    }

data WorldContent = WorldContent
    { wcEdits          ∷ !WorldEdits
      -- ^ the owned edits, per chunk, in apply order
    , wcCropPlots      ∷ ![((Int, Int), CropPlot)]
    , wcHarvests       ∷ ![(FloraInstanceId, Float)]
    , wcCursor         ∷ !Word64
    , wcTileTags       ∷ ![((Int, Int, Int), Set.Set GameplayTag)]
    , wcOccurrenceTags ∷ ![(FloraInstanceId, (Int, Int), Set.Set GameplayTag)]
    , wcPlotTags       ∷ ![((Int, Int), Set.Set GameplayTag)]
    , wcRefs           ∷ !WorldRefs
    , wcDiagnostics    ∷ ![ScenarioDiagnostic]
    } deriving (Show, Eq)

-- | Build the content of the three families, each entry with its
--   document path.
buildWorldContent ∷ WorldBuildEnv → [(Text, TerrainPatch)] → [(Text, FluidPatch)]
                  → [(Text, FloraEntry)] → WorldContent
buildWorldContent env terrain fluids flora =
    let tb = buildTerrain (TerrainEnv (wbeBounds env) (wbeMaterial env)) terrain
        topOf t = maybe baseTopZ tsSurfaceZ (M.lookup t (tbSpecs tb))
        fb = buildFluids (wbeBounds env) (tbSpecs tb) fluids
        lb = buildFlora FloraEnv
                { feBounds = wbeBounds env, feCatalog = wbeFlora env
                , feAbsDay = wbeAbsDay env, feTerrainTop = topOf
                , feCursor = wbeCursor env }
                flora
        tileTags = M.toAscList $ M.fromListWith Set.union
            [ ((x, y, z), Set.fromList (btpTags p))
            | p ← tbPatches tb, not (null (btpTags p))
            , (x, y, z) ← btpLiveCells p ]
        occTags =
            [ (boInstance o, boTile o, Set.fromList (boTags o))
            | o ← flbOccurrences lb, not (null (boTags o)) ]
        plotTags = M.toAscList $ M.fromListWith Set.union
            [ (bpTile p, Set.fromList (bpTags p))
            | p ← flbPlots lb, not (null (bpTags p)) ]
    in WorldContent
        { wcEdits = groupEditsByChunk (tbEdits tb ⧺ fbEdits fb ⧺ flbEdits lb)
        , wcCropPlots = [ (bpTile p, bpPlot p) | p ← flbPlots lb ]
        , wcHarvests = flbHarvests lb
        , wcCursor = flbCursor lb
        , wcTileTags = tileTags
        , wcOccurrenceTags = occTags
        , wcPlotTags = plotTags
        , wcRefs = WorldRefs
            { wrTerrain = [ (btpId p, btpLiveCells p) | p ← tbPatches tb ]
            , wrFluids = [ (bfpId p, bfpTiles p) | p ← fbPatches fb ]
            , wrOccurrences = [ (boId o, boInstance o) | o ← flbOccurrences lb ]
            , wrPlots = [ (bpId p, bpTile p) | p ← flbPlots lb ] }
        , wcDiagnostics = tbDiagnostics tb ⧺ fbDiagnostics fb ⧺ flbDiagnostics lb
        }

-- | Lay edits into the per-chunk log, preserving their order.
groupEditsByChunk ∷ [WorldEdit] → WorldEdits
groupEditsByChunk = foldl' (\acc e → let (gx, gy) = editTile e
                                         (coord, _) = globalToChunk gx gy
                                     in appendEdit coord e acc)
                           emptyWorldEdits

-- | The tile an edit addresses.
editTile ∷ WorldEdit → (Int, Int)
editTile e = case e of
    WeDeleteTile gx gy              → (gx, gy)
    WeSetFluidTile gx gy _          → (gx, gy)
    WeAddTile gx gy _               → (gx, gy)
    WeSetSlope gx gy _ _            → (gx, gy)
    WeSetCell gx gy _ _             → (gx, gy)
    WeSetStructure gx gy _ _ _ _    → (gx, gy)
    WeClearStructure gx gy _        → (gx, gy)
    WeSetVeg gx gy _ _              → (gx, gy)
    WePlaceFlora gx gy _ _ _        → (gx, gy)
    WeSetFluidSnapshot gx gy _ _    → (gx, gy)
    WeClearFluidSnapshot gx gy      → (gx, gy)
    WePlaceFloraWithId gx gy _ _ _ _ → (gx, gy)
    WePlaceFloraRef gx gy _ _ _ _   → (gx, gy)
    WeSetFloraState gx gy _ _ _ _ _ _ _ → (gx, gy)
