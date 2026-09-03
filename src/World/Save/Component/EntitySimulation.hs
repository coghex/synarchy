{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | The per-page UNIT SIMULATION state: the @"unit-sim"@ save component
--   (issue #760, save-overhaul B2; extracted from
--   "World.Save.Component.Entities" by #2150).
--
--   - @"unit-sim"@ (required) — per page: per-unit simulation state
--     (position, pose, activity, target, path, gameplay deadlines).
--     Owner: 'Unit.Sim.Types.UnitThreadState'. Depends on @"world-pages"@
--     AND @"units"@ — a sim state must have a matching unit
--     (the orphan check runs at whole-session assembly).
--
--   This is the only entity component with THREE live versions, and its
--   whole version graph is here: the current shape, the frozen v1 and v2
--   shapes, and the two migrations that carry each forward. That
--   independent evolution — a third version and the movement-hazard
--   migration arrived with #1217, while the instance snapshots stayed
--   put — is why the simulation contract owns its own module rather than
--   sharing one with "World.Save.Component.EntitySnapshots".
--
--   Requirement 4 — the on-disk contract is FROZEN, distinct from every
--   mutable runtime record. The live 'UnitSimState' is mirrored by
--   'UnitSimStateDTO' with an explicit field-by-field conversion
--   ('toUnitSimStateDTO'/'fromUnitSimStateDTO'); it is never embedded
--   directly, so a field added, dropped or reordered on the live record
--   changes only that conversion — surfacing as a compile error to
--   reconcile — never as silent byte drift in a shipped payload. Leaf
--   enums ('Pose'/'UnitActivity'/'Direction'/'MoveHazardPolicy') and the
--   durable 'UnitId' newtype are reused as-is: append-only content
--   references with no independent mutable identity (boundary rule leaf
--   clause (a), see "World.Save.Component.Types").
module World.Save.Component.EntitySimulation
    ( unitSimCodec
    , PageSimDTO(..)
    , UnitSimDTO(..)
    , PageSimDTOv1(..)
    , UnitSimDTOv1(..)
    , PageSimDTOv2(..)
    , UnitSimDTOv2(..)
    , migratePageSimDTOv1
    , migrateUnitSimDTOv1
    , migratePageSimDTOv2
    , migrateUnitSimDTOv2
    , UnitSimStateDTO(..)
    , MoveTargetDTO(..)
    , UnitSimStateDTOv1(..)
    , MoveTargetDTOv1(..)
    , toUnitSimStateDTOv1
    , migrateUnitSimStateDTOv1
    , toUnitSimStateDTO
    , fromUnitSimStateDTO
    , applyUnitSim
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Page.Types (WorldPageId)
import Unit.Types (UnitId)
import Unit.Sim.Types
    ( UnitSimState(..), MoveTarget(..), Pose, UnitActivity, Direction )
import Unit.Pathing.Hazard
    ( MoveHazardPolicy, defaultMoveHazardPolicy )
import World.Save.Snapshot (PageSnapshot(..))
import World.Save.Component.Types
import World.Save.Reference (SamePageRef(..))
import World.Save.PageOrder (orderedPages)

-- unit-sim ----------------------------------------------------------

-- | The FROZEN pre-#1217 move target, preserved verbatim for
--   decode-only backward compatibility: identical to the current DTO but
--   for the per-request hazard policy it does not carry. Never edited; a
--   further move-target schema change freezes the CURRENT shape as
--   'MoveTargetDTOv2' rather than touching this one.
data MoveTargetDTOv1 = MoveTargetDTOv1
    { mvt1X     ∷ !Float
    , mvt1Y     ∷ !Float
    , mvt1Speed ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

-- | The FROZEN pre-#1217 unit sim state — the shape the unit-sim
--   component shipped at v1 AND v2, and the one the legacy v90 session
--   ("World.Save.Compat.SessionV90") embeds. Identical to the current
--   DTO but for the move-target shape its 'sim1Target' carries
--   ('MoveTargetDTOv1'). Never edited; a further sim-state schema change
--   freezes the CURRENT shape as 'UnitSimStateDTOv2' rather than
--   touching this one.
data UnitSimStateDTOv1 = UnitSimStateDTOv1
    { sim1RealX            ∷ !Float
    , sim1RealY            ∷ !Float
    , sim1GridZ            ∷ !Int
    , sim1RealZ            ∷ !Float
    , sim1Target           ∷ !(Maybe MoveTargetDTOv1)
    , sim1Pose             ∷ !Pose
    , sim1State            ∷ !UnitActivity
    , sim1Facing           ∷ !Direction
    , sim1LocalPath        ∷ ![(Float, Float)]
    , sim1DrinkUntil       ∷ !(Maybe Double)
    , sim1EatUntil         ∷ !(Maybe Double)
    , sim1PickupUntil      ∷ !(Maybe Double)
    , sim1TransitionUntil  ∷ !(Maybe Double)
    , sim1TransitionStride ∷ !Int
    , sim1PostTransition   ∷ ![Pose]
    , sim1ClimbFromTile    ∷ !(Maybe (Float, Float, Int))
    , sim1ClimbToTile      ∷ !(Maybe (Float, Float, Int))
    , sim1ClimbStartTime   ∷ !(Maybe Double)
    , sim1ClimbSlipAt      ∷ !(Maybe Double)
    , sim1FallFromTile     ∷ !(Maybe (Float, Float, Int))
    , sim1FallToTile       ∷ !(Maybe (Float, Float, Int))
    , sim1PendingClimbXP   ∷ !Float
    , sim1GetUpAt          ∷ !(Maybe Double)
    , sim1PendingFallDrop  ∷ !(Maybe Int)
    , sim1JumpApex         ∷ !(Maybe Float)
    , sim1MoveGrade        ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen shape — the round-trip partner a v1/v2
--   fixture and a migration test are built with.
toUnitSimStateDTOv1 ∷ UnitSimState → UnitSimStateDTOv1
toUnitSimStateDTOv1 s = UnitSimStateDTOv1
    { sim1RealX            = usRealX s
    , sim1RealY            = usRealY s
    , sim1GridZ            = usGridZ s
    , sim1RealZ            = usRealZ s
    , sim1Target           = toMoveTargetDTOv1 <$> usTarget s
    , sim1Pose             = usPose s
    , sim1State            = usState s
    , sim1Facing           = usFacing s
    , sim1LocalPath        = usLocalPath s
    , sim1DrinkUntil       = usDrinkUntil s
    , sim1EatUntil         = usEatUntil s
    , sim1PickupUntil      = usPickupUntil s
    , sim1TransitionUntil  = usTransitionUntil s
    , sim1TransitionStride = usTransitionStride s
    , sim1PostTransition   = usPostTransition s
    , sim1ClimbFromTile    = usClimbFromTile s
    , sim1ClimbToTile      = usClimbToTile s
    , sim1ClimbStartTime   = usClimbStartTime s
    , sim1ClimbSlipAt      = usClimbSlipAt s
    , sim1FallFromTile     = usFallFromTile s
    , sim1FallToTile       = usFallToTile s
    , sim1PendingClimbXP   = usPendingClimbXP s
    , sim1GetUpAt          = usGetUpAt s
    , sim1PendingFallDrop  = usPendingFallDrop s
    , sim1JumpApex         = usJumpApex s
    , sim1MoveGrade        = usMoveGrade s
    }

-- | Migrate a frozen pre-#1217 sim state forward: everything carries
--   across unchanged, and an in-flight move target gains the
--   FALL-PERMITTED policy — the behavior it was saved under, so a unit
--   walking a commanded route across a cliff edge in an old save keeps
--   walking it after the load.
migrateUnitSimStateDTOv1 ∷ UnitSimStateDTOv1 → UnitSimStateDTO
migrateUnitSimStateDTOv1 d = UnitSimStateDTO
    { simRealX            = sim1RealX d
    , simRealY            = sim1RealY d
    , simGridZ            = sim1GridZ d
    , simRealZ            = sim1RealZ d
    , simTarget           = migrateMoveTargetDTOv1 <$> sim1Target d
    , simPose             = sim1Pose d
    , simState            = sim1State d
    , simFacing           = sim1Facing d
    , simLocalPath        = sim1LocalPath d
    , simDrinkUntil       = sim1DrinkUntil d
    , simEatUntil         = sim1EatUntil d
    , simPickupUntil      = sim1PickupUntil d
    , simTransitionUntil  = sim1TransitionUntil d
    , simTransitionStride = sim1TransitionStride d
    , simPostTransition   = sim1PostTransition d
    , simClimbFromTile    = sim1ClimbFromTile d
    , simClimbToTile      = sim1ClimbToTile d
    , simClimbStartTime   = sim1ClimbStartTime d
    , simClimbSlipAt      = sim1ClimbSlipAt d
    , simFallFromTile     = sim1FallFromTile d
    , simFallToTile       = sim1FallToTile d
    , simPendingClimbXP   = sim1PendingClimbXP d
    , simGetUpAt          = sim1GetUpAt d
    , simPendingFallDrop  = sim1PendingFallDrop d
    , simJumpApex         = sim1JumpApex d
    , simMoveGrade        = sim1MoveGrade d
    }

-- | Frozen mirror of 'MoveTarget' (a mutable runtime record). Current
--   shape (unit-sim v3): carries the request's damaging-drop policy
--   (#1217). 'MoveHazardPolicy' is reused as a LEAF — an append-only
--   enum with no independent mutable identity, exactly like 'Pose' and
--   'Direction' beside it.
data MoveTargetDTO = MoveTargetDTO
    { mvtX      ∷ !Float
    , mvtY      ∷ !Float
    , mvtSpeed  ∷ !Float
    , mvtHazard ∷ !MoveHazardPolicy
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of 'UnitSimState' (a mutable runtime record that gains
--   fields as movement/climb/fall features land). Field-for-field with
--   the live record's CURRENT shape; leaf enums are reused (see the
--   module haddock). Conversion is explicit — a change to 'UnitSimState'
--   surfaces here as a type error to reconcile, never as silent byte
--   drift in a shipped v1 save. Current shape (unit-sim v3); the
--   pre-#1217 one is frozen above as 'UnitSimStateDTOv1'.
data UnitSimStateDTO = UnitSimStateDTO
    { simRealX            ∷ !Float
    , simRealY            ∷ !Float
    , simGridZ            ∷ !Int
    , simRealZ            ∷ !Float
    , simTarget           ∷ !(Maybe MoveTargetDTO)
    , simPose             ∷ !Pose
    , simState            ∷ !UnitActivity
    , simFacing           ∷ !Direction
    , simLocalPath        ∷ ![(Float, Float)]
    , simDrinkUntil       ∷ !(Maybe Double)
    , simEatUntil         ∷ !(Maybe Double)
    , simPickupUntil      ∷ !(Maybe Double)
    , simTransitionUntil  ∷ !(Maybe Double)
    , simTransitionStride ∷ !Int
    , simPostTransition   ∷ ![Pose]
    , simClimbFromTile    ∷ !(Maybe (Float, Float, Int))
    , simClimbToTile      ∷ !(Maybe (Float, Float, Int))
    , simClimbStartTime   ∷ !(Maybe Double)
    , simClimbSlipAt      ∷ !(Maybe Double)
    , simFallFromTile     ∷ !(Maybe (Float, Float, Int))
    , simFallToTile       ∷ !(Maybe (Float, Float, Int))
    , simPendingClimbXP   ∷ !Float
    , simGetUpAt          ∷ !(Maybe Double)
    , simPendingFallDrop  ∷ !(Maybe Int)
    , simJumpApex         ∷ !(Maybe Float)
    , simMoveGrade        ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

toMoveTargetDTO ∷ MoveTarget → MoveTargetDTO
toMoveTargetDTO m = MoveTargetDTO
    { mvtX = mtTargetX m, mvtY = mtTargetY m, mvtSpeed = mtSpeed m
    , mvtHazard = mtHazard m }

fromMoveTargetDTO ∷ MoveTargetDTO → MoveTarget
fromMoveTargetDTO d = MoveTarget
    { mtTargetX = mvtX d, mtTargetY = mvtY d, mtSpeed = mvtSpeed d
    , mtHazard = mvtHazard d }

-- | Encoder for the frozen pre-#1217 move target. Drops the hazard
--   policy, which is exactly what the old wire shape held.
toMoveTargetDTOv1 ∷ MoveTarget → MoveTargetDTOv1
toMoveTargetDTOv1 m = MoveTargetDTOv1
    { mvt1X = mtTargetX m, mvt1Y = mtTargetY m, mvt1Speed = mtSpeed m }

-- | A pre-#1217 target had no policy to record, and every route it could
--   have been walking was fall-permitted; that is the only honest
--   default, so it is stated once here rather than guessed per caller.
migrateMoveTargetDTOv1 ∷ MoveTargetDTOv1 → MoveTargetDTO
migrateMoveTargetDTOv1 d = MoveTargetDTO
    { mvtX      = mvt1X d
    , mvtY      = mvt1Y d
    , mvtSpeed  = mvt1Speed d
    , mvtHazard = defaultMoveHazardPolicy
    }

toUnitSimStateDTO ∷ UnitSimState → UnitSimStateDTO
toUnitSimStateDTO s = UnitSimStateDTO
    { simRealX            = usRealX s
    , simRealY            = usRealY s
    , simGridZ            = usGridZ s
    , simRealZ            = usRealZ s
    , simTarget           = toMoveTargetDTO <$> usTarget s
    , simPose             = usPose s
    , simState            = usState s
    , simFacing           = usFacing s
    , simLocalPath        = usLocalPath s
    , simDrinkUntil       = usDrinkUntil s
    , simEatUntil         = usEatUntil s
    , simPickupUntil      = usPickupUntil s
    , simTransitionUntil  = usTransitionUntil s
    , simTransitionStride = usTransitionStride s
    , simPostTransition   = usPostTransition s
    , simClimbFromTile    = usClimbFromTile s
    , simClimbToTile      = usClimbToTile s
    , simClimbStartTime   = usClimbStartTime s
    , simClimbSlipAt      = usClimbSlipAt s
    , simFallFromTile     = usFallFromTile s
    , simFallToTile       = usFallToTile s
    , simPendingClimbXP   = usPendingClimbXP s
    , simGetUpAt          = usGetUpAt s
    , simPendingFallDrop  = usPendingFallDrop s
    , simJumpApex         = usJumpApex s
    , simMoveGrade        = usMoveGrade s
    }

fromUnitSimStateDTO ∷ UnitSimStateDTO → UnitSimState
fromUnitSimStateDTO d = UnitSimState
    { usRealX            = simRealX d
    , usRealY            = simRealY d
    , usGridZ            = simGridZ d
    , usRealZ            = simRealZ d
    , usTarget           = fromMoveTargetDTO <$> simTarget d
    , usPose             = simPose d
    , usState            = simState d
    , usFacing           = simFacing d
    , usLocalPath        = simLocalPath d
    , usDrinkUntil       = simDrinkUntil d
    , usEatUntil         = simEatUntil d
    , usPickupUntil      = simPickupUntil d
    , usTransitionUntil  = simTransitionUntil d
    , usTransitionStride = simTransitionStride d
    , usPostTransition   = simPostTransition d
    , usClimbFromTile    = simClimbFromTile d
    , usClimbToTile      = simClimbToTile d
    , usClimbStartTime   = simClimbStartTime d
    , usClimbSlipAt      = simClimbSlipAt d
    , usFallFromTile     = simFallFromTile d
    , usFallToTile       = simFallToTile d
    , usPendingClimbXP   = simPendingClimbXP d
    , usGetUpAt          = simGetUpAt d
    , usPendingFallDrop  = simPendingFallDrop d
    , usJumpApex         = simJumpApex d
    , usMoveGrade        = simMoveGrade d
    }

-- | Issue #764 (save-overhaul C3): @psSim@'s map KEY is a
--   unit-simulation state's OWNING unit — a durable cross-component
--   reference (this component's own dependency on @"units"@ exists
--   precisely because of it) exactly like a craft bill's station or a
--   power node's host building, just carried as a 'HM.HashMap' key
--   rather than a field value. Typed the same way
--   ("World.Save.Reference"'s 'SamePageRef', which derives 'Hashable'
--   for exactly this use) rather than a bare 'UnitId' — a sim-state
--   entry is always expected on the SAME page as the page slice
--   carrying it (the live 'pgsUnitSimStates' this mirrors is itself
--   page-scoped; "World.Save.Snapshot"'s @OrphanedUnitSimState@ check
--   already enforces the SAME-page relationship this type now
--   documents). Bumped this component to v2; v1 decodes via
--   'migrateUnitSimDTOv1' below. Issue #1217 bumped it again to v3 (the
--   per-request hazard policy on 'MoveTargetDTO'); the v2 shape is
--   frozen as 'PageSimDTOv2'.
data PageSimDTO = PageSimDTO
    { psPageId ∷ !WorldPageId
    , psSim    ∷ !(HM.HashMap (SamePageRef UnitId) UnitSimStateDTO)
    } deriving (Show, Eq, Generic, Serialize)

newtype UnitSimDTO = UnitSimDTO { usdPages ∷ [PageSimDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

-- | The FROZEN v1 shape, preserved verbatim for decode-only backward
--   compatibility — @psSim@ here is keyed by the original bare
--   'UnitId', exactly as it shipped. Never edited; a further schema
--   change adds a v3 type instead (frozen-DTO boundary rule).
data PageSimDTOv1 = PageSimDTOv1
    { ps1PageId ∷ !WorldPageId
    , ps1Sim    ∷ !(HM.HashMap UnitId UnitSimStateDTOv1)
    } deriving (Show, Eq, Generic, Serialize)

newtype UnitSimDTOv1 = UnitSimDTOv1 { usd1Pages ∷ [PageSimDTOv1] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

-- | The FROZEN v2 shape: typed 'SamePageRef' keys (#764) over the
--   pre-#1217 sim state. Preserved verbatim for decode-only backward
--   compatibility; never edited.
data PageSimDTOv2 = PageSimDTOv2
    { ps2PageId ∷ !WorldPageId
    , ps2Sim    ∷ !(HM.HashMap (SamePageRef UnitId) UnitSimStateDTOv1)
    } deriving (Show, Eq, Generic, Serialize)

newtype UnitSimDTOv2 = UnitSimDTOv2 { usd2Pages ∷ [PageSimDTOv2] }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

-- | Translate an unambiguous v1 page slice into the current shape: every
--   v1 sim-state map key has always meant "this unit, on THIS page" (the
--   live 'pgsUnitSimStates' this mirrors is itself page-scoped — see
--   'PageSimDTO' haddock above), so wrapping every key in 'SamePageRef'
--   is total and never ambiguous (requirement 14). The sim states
--   themselves go through the same forward migration v2 uses, so v1 and
--   v2 payloads land on ONE definition of "an old target is
--   fall-permitted".
migratePageSimDTOv1 ∷ PageSimDTOv1 → PageSimDTO
migratePageSimDTOv1 d = PageSimDTO
    { psPageId = ps1PageId d
    , psSim    = HM.map migrateUnitSimStateDTOv1
                     (HM.mapKeys SamePageRef (ps1Sim d))
    }

migrateUnitSimDTOv1 ∷ UnitSimDTOv1 → UnitSimDTO
migrateUnitSimDTOv1 (UnitSimDTOv1 ps) = UnitSimDTO (map migratePageSimDTOv1 ps)

-- | Translate a v2 page slice into the current shape: keys are already
--   typed, so only the sim states move forward (#1217).
migratePageSimDTOv2 ∷ PageSimDTOv2 → PageSimDTO
migratePageSimDTOv2 d = PageSimDTO
    { psPageId = ps2PageId d
    , psSim    = HM.map migrateUnitSimStateDTOv1 (ps2Sim d)
    }

migrateUnitSimDTOv2 ∷ UnitSimDTOv2 → UnitSimDTO
migrateUnitSimDTOv2 (UnitSimDTOv2 ps) = UnitSimDTO (map migratePageSimDTOv2 ps)

-- | Issue #764 (save-overhaul C3): v2 typed the 'SamePageRef' sim-state
--   keys. Issue #1217: the current schema is v3 — a move target now
--   carries the request's damaging-drop policy. v1 decodes through its
--   own frozen 'UnitSimDTOv1', v2 through 'UnitSimDTOv2', and both land
--   on the fall-permitted default their bytes were written under. Issue
--   #1093: previously hand-rolled because the shared helper had no real
--   multi-version dispatch.
unitSimCodec ∷ ComponentCodec UnitSimDTO
unitSimCodec = componentCodec ComponentSpec
    { csComponent     = unitSimComponentId
    , csVersion       = 3
    , csRequired      = True
    , csDeps          = [worldPagesComponentId, unitsComponentId]
    , csEncode        = \snap → UnitSimDTO
        [ PageSimDTO (pgsPageId p)
            (HM.mapKeys SamePageRef (HM.map toUnitSimStateDTO (pgsUnitSimStates p)))
        | p ← orderedPages snap ]
    , csDecode        = id
    , csOlderVersions = [ atVersion 1 migrateUnitSimDTOv1
                        , atVersion 2 migrateUnitSimDTOv2 ]
    , csValidate      = const []
    }

applyUnitSim
    ∷ Word32 → UnitSimDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyUnitSim ver (UnitSimDTO slices) =
    applyPageSlices unitSimComponentId ver psPageId
        (\s p → p { pgsUnitSimStates =
            HM.mapKeys unSamePageRef (HM.map fromUnitSimStateDTO (psSim s)) })
        slices
