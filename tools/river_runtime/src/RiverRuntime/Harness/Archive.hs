-- | JSON encoding of harness runs and comparisons for the evidence
--   archive (#2719, requirement 8).
--
--   Everything here is a pure function of its input, and every map is
--   written in key order, so encoding the same runs twice yields the same
--   bytes. The archive records the fixture definition itself (and its
--   content hash, computed by the runner over these bytes), the
--   topology and offset of the placement, the adapter identity and
--   configuration, the logical interval, and the operation schedule.
module RiverRuntime.Harness.Archive
    ( fixtureJson
    , placementJson
    , trajectoryJson
    , comparisonJson
    , characterizationJson
    , sha256Hex
    ) where

import UPrelude
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import World.Chunk.Types (ChunkCoord(..))
import Sim.Topology (SimTopology(..))
import RiverRuntime.Harness.Adapter
import RiverRuntime.Harness.Catalog (CharacterizationCase(..))
import RiverRuntime.Harness.Compare
import RiverRuntime.Harness.Fixture
import RiverRuntime.Harness.Placement
import RiverRuntime.Harness.Run

tileJson ∷ Tile → A.Value
tileJson (Tile x y) = A.toJSON [x, y]

cellJson ∷ CellState → A.Value
cellJson (CellState (Elevation e) fluid) = A.object $
    [ "terrain" .= e ] <>
    case fluid of
        Nothing → []
        Just (ty, Quantity q) → [ "fluid" .= tshow ty, "quantity" .= q ]

cellSpecJson ∷ CellSpec → A.Value
cellSpecJson (CellSpec e fl) =
    cellJson (CellState e (fmap (\(FluidSpec ty q) → (ty, q)) fl))

operationJson ∷ Operation → A.Value
operationJson = \case
    SetTerrain t (Elevation e) policy → A.object
        [ "op" .= ("set-terrain" ∷ Text), "tile" .= tileJson t, "terrain" .= e
        , "fluid_policy" .= (case policy of
                                KeepQuantity   → "keep-quantity" ∷ Text
                                DisplaceToSink → "displace-to-sink") ]
    AddFluid t ty (Quantity q) → A.object
        [ "op" .= ("source" ∷ Text), "tile" .= tileJson t
        , "fluid" .= tshow ty, "quantity" .= q ]
    RemoveFluid t (Quantity q) → A.object
        [ "op" .= ("sink" ∷ Text), "tile" .= tileJson t, "quantity" .= q ]

fixtureJson ∷ Fixture → A.Value
fixtureJson fx = A.object
    [ "name"         .= fxName fx
    , "description"  .= fxDescription fx
    , "chunks"       .= [ A.object [ "chunk" .= [cx, cy]
                                   , "residency" .= residency r ]
                        | (LocalChunk cx cy, r) ← fxChunks fx ]
    , "base_terrain" .= unElevation (fxBaseTerrain fx)
    , "cells"        .= [ A.object [ "tile" .= tileJson t, "cell" .= cellSpecJson c ]
                        | (t, c) ← M.toList (fxCells fx) ]
    , "barriers"     .= [ A.object [ "name" .= barrierName b
                                   , "cells" .= map tileJson (barrierCells b)
                                   , "crest" .= unElevation (barrierCrest b) ]
                        | b ← fxBarriers fx ]
    , "schedule"     .= [ A.object [ "at_us" .= unLogicalTime at, "operation" .= operationJson op ]
                        | Scheduled at op ← fxSchedule fx ]
    , "duration_us"  .= unLogicalTime (fxDuration fx)
    , "units"        .= ("eighths of a z-level; quantity is units over the cell's terrain" ∷ Text)
    ]
  where
    residency ResidentActive   = "active" ∷ Text
    residency ResidentInactive = "inactive"

placementJson ∷ Placement → A.Value
placementJson pl = A.object
    [ "name"     .= plName pl
    , "topology" .= case plTopology pl of
        SimFlatTopology     → A.object [ "kind" .= ("flat" ∷ Text) ]
        SimCylindricalU w   → A.object [ "kind" .= ("cylindrical-u" ∷ Text)
                                       , "world_size" .= w ]
    , "chunk_offset" .= [fst (plOffset pl), snd (plOffset pl)]
    ]

adapterJson ∷ AdapterInfo → A.Value
adapterJson ai = A.object
    [ "name"         .= aiName ai
    , "config"       .= M.fromList (aiConfig ai)
    , "intervals_us" .= map unLogicalTime (aiIntervals ai)
    ]

violationJson ∷ Violation → A.Value
violationJson v = A.object
    [ "step" .= vStep v, "kind" .= tshow (vKind v), "detail" .= vDetail v ]

-- | A run: its configuration, every step's totals and operations, every
--   check result, and its final cell state. The final state is written
--   out in full when @full@ is set and as a hash otherwise; the hash is
--   over the same encoding, in the fixture frame, so equal hashes across
--   placements are equal states.
trajectoryJson ∷ Bool → Trajectory → A.Value
trajectoryJson full tr = A.object $
    [ "fixture"     .= fxName fx
    , "placement"   .= placementJson (rcPlacement (trConfig tr))
    , "stored_chunks" .= [ A.object [ "local" .= [lx, ly], "stored" .= [sx, sy] ]
                         | (LocalChunk lx ly, ChunkCoord sx sy) ← storedChunks ]
    , "adapter"     .= adapterJson (trAdapter tr)
    , "interval_us" .= unLogicalTime (rcInterval (trConfig tr))
    , "steps"       .= trSteps tr
    , "face_checks" .= case trFaceChecks tr of
        FaceChecksUnavailable → "unavailable: the adapter reports no face records" ∷ Text
        FaceChecksApplied     → "applied"
    , "violations"  .= map violationJson (trViolations tr)
    , "step_totals_columns" .= (["step", "solved_total", "after_ops_total", "wet_cells"] ∷ [Text])
    , "step_totals" .= [ [ stStep r, totalQuantity (stSolved r)
                         , totalQuantity (stAfterOps r)
                         , M.size (M.filter ((> 0) . cellQuantity) (stAfterOps r)) ]
                       | r ← trRecords tr ]
    , "operations"  .= [ A.object [ "step" .= stStep r, "operation" .= operationJson (aoOp o)
                                  , "accounted_delta" .= aoDelta o ]
                       | r ← trRecords tr, o ← stOps r ]
    , "final_cells_sha256" .= fmap (sha256Hex . A.encode) finalCells
    ]
    <> [ "final_cells" .= finalCells | full ]
  where
    fx = trFixture tr
    storedChunks = case placementMap (rcPlacement (trConfig tr)) fx of
        Right pm → M.toList (pmChunks pm)
        Left _   → []
    -- Every cell that differs from a dry base-terrain wall.
    finalCells = case trRecords tr of
        [] → Nothing
        rs → Just $ A.toJSON
            [ A.object [ "tile" .= tileJson t, "cell" .= cellJson c ]
            | (t, c) ← M.toList (stAfterOps (last rs))
            , c ≢ CellState (fxBaseTerrain fx) Nothing ]

comparisonJson ∷ Text → Text → Comparison → A.Value
comparisonJson nameA nameB cmp = A.object
    [ "run_a" .= nameA
    , "run_b" .= nameB
    , "definitions" .= ("see RiverRuntime.Harness.Compare: nearest-rank p95, "
                        <> "union of wet extents, terrain as zero depth, "
                        <> "Chebyshev Hausdorff boundary distance" ∷ Text)
    , "times_columns" .= ([ "time_us", "step_a", "step_b", "union_wet_cells"
                            , "surface_p95_eighths", "surface_max_eighths"
                            , "wet_dry_disagreement_cells", "wet_boundary_distance" ] ∷ [Text])
    , "times_note" .= ("a both-dry time has null surface columns; a boundary "
                       <> "distance is a tile count, or both-empty / only-in-a / only-in-b" ∷ Text)
    , "times" .= map timeJson (cmpTimes cmp)
    , "milestones" .= map milestoneJson (cmpMilestones cmp)
    ]
  where
    timeJson tc =
        let (n, p, m) = case tcSurface tc of
                SurfaceBothDry → (A.Null, A.Null, A.Null)
                SurfaceError n' p' m' → (A.toJSON n', A.toJSON p', A.toJSON m')
        in A.toJSON [ A.toJSON (unLogicalTime (tcTime tc)), A.toJSON (tcStepA tc)
                    , A.toJSON (tcStepB tc), n, p, m
                    , A.toJSON (tcWetDryCells tc), boundaryJson (tcBoundary tc) ]
    boundaryJson = \case
        BoundaryBothEmpty → A.String "both-empty"
        BoundaryOnlyIn side → A.String ("only-in-" <> sideName side)
        BoundaryTiles d → A.toJSON d
    milestoneJson mc = A.object
        [ "name" .= msName (mcMilestone mc)
        , "tile" .= tileJson (msTile (mcMilestone mc))
        , "kind" .= case msKind (mcMilestone mc) of
            ArrivalAtLeast (Quantity q) → "arrival >= " <> tshow q
            DrainageAtMost (Quantity q) → "drainage <= " <> tshow q
        , "a" .= hitJson (mcA mc), "b" .= hitJson (mcB mc)
        , "delta" .= case mcDelta mc of
            MilestoneNeitherReached → A.String "neither-reached"
            MilestoneOnlyReachedBy side → A.String ("only-reached-by-" <> sideName side)
            MilestoneDelta t s → A.object [ "time_us" .= t, "steps" .= s ]
        ]
    hitJson Nothing = A.String "unreached"
    hitJson (Just h) = A.object
        [ "step" .= mhStep h, "time_us" .= unLogicalTime (mhTime h), "initial" .= mhInitial h ]
    sideName RunA = "a"
    sideName RunB = "b"

-- | One reproduced characterization case, in the archived shape.
characterizationJson ∷ CharacterizationCase → [(Int, Int, Int)] → Maybe Bool → A.Value
characterizationJson cc samples reproduced = A.object
    [ "name" .= ccName cc
    , "source" .= tileJson (ccSource cc), "target" .= tileJson (ccTarget cc)
    , "sourceBed" .= ccSourceBed cc, "targetBed" .= ccTargetBed cc
    , "targetPresent" .= ccTargetPresent cc, "targetActive" .= ccTargetActive cc
    , "samples" .= [ A.object [ "tick" .= k, "sourceUnits" .= s
                              , "targetUnits" .= t, "totalUnits" .= tot ]
                   | (k, (s, t, tot)) ← zip [0 ∷ Int ..] samples ]
    , "matches_archived_baseline" .= reproduced
    ]

-- | Lower-case hex SHA-256 of a byte string.
sha256Hex ∷ BL.ByteString → Text
sha256Hex = T.pack . map (toEnum . fromIntegral) . BL.unpack
          . BB.toLazyByteString . BB.byteStringHex . SHA256.hashlazy
