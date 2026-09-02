{-# LANGUAGE Strict #-}
-- | The ONE structure-plan resolver (#1844): given a coherent world
--   snapshot and #1842's registered art \/ buildability catalogue, is
--   this candidate tile a structure piece that can actually be built —
--   and if not, in which of four distinguishable ways?
--
--   Everything that decides a structure candidate's fate goes through
--   here: the anchor→hover PREVIEW, the click COMMIT, world-side
--   REVALIDATION after a mutation or a chunk publication, and the
--   worker's own re-checks before claiming, before paying and before
--   placing. Before this they were four separate rules and drifted
--   accordingly — commit admitted what preview never drew, and nothing
--   re-checked a committed designation at all.
--
--   Two aspects of the ordering are load-bearing:
--
--     * CATALOGUE FIRST, TERRAIN SECOND. A known art or build-metadata
--       failure is invalid whatever the terrain is doing, so
--       'PlanUnresolvedTerrain' can only ever mean "a TERRAIN-dependent
--       fact is unavailable" (requirement 6). Answering "unresolved"
--       for a piece whose pack does not exist would let an unloaded
--       chunk hide a permanent failure.
--     * OCCUPANCY INCLUDES READ-YOUR-WRITES STAGING. A placement is
--       visible through 'wsStructureStageRef' before its queued overlay
--       mutation commits, and 'World.Construct.Art.structurePresentAt'
--       is the shared reader that sees both — the same two authorities,
--       in the same order, @structure.hasAt@ consults. A resolver that
--       read only the committed overlay would admit a second designation
--       onto a slot a worker has already filled this tick.
--
--   'PlanOp' is what keeps REVALIDATION from cancelling every
--   designation it checks: preview and commit treat any outstanding
--   designation as a conflict, while revalidation excludes the exact
--   attempt being checked — and only that attempt. A DIFFERENT attempt
--   at the same canonical tile is still a conflict, because the map
--   holds one designation per tile and the other one is the one that is
--   really there.
module World.Construct.Plan
    ( PlanOutcome(..)
    , planOutcomeName
    , PlanResult(..)
    , PlanWorld(..)
    , PlanOp(..)
    , structurePieceSlot
    , structureFinalGridZ
    , structurePieceArtContext
    , resolvePlanPieceArt
    , resolveStructurePlan
    , planSurfaceZAt
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector.Unboxed as VU
import Structure.ArtCatalog
    (StructureArtCatalog, PieceArt, PieceArtContext(..), resolveUnplacedArt
    , packKindBuild, defaultPieceArtContext)
import Structure.Facing (WallEdge(..))
import Structure.Types (StructureSlot(..), StructureStage, slotFromText)
import World.Chunk.Types (LoadedChunk(..), columnIndex)
import Structure.Wire (wireShapeFor)
import World.Construct.Art
    ( structureGridZAt, structurePresentAt, wallCapsAt
    , wireNeighborsWithProposed )
import World.Construct.Attempt (ConstructAttemptId)
import World.Construct.Types
    ( ConstructDesignation(..), ConstructDesignations, StructurePiece(..) )
import World.Generate.Coordinates (canonicalTile, canonicalTileFrame)
import World.Tile.Types (WorldTileData, lookupChunk)

-- | What the resolver says about one candidate tile. Exactly the four
--   outcomes DTV-11 renders from (requirement 5).
--
--   Deliberately NOT serialized: this is a per-frame\/per-check answer
--   derived from live state, never persisted, so it is outside the
--   append-only enum contract.
data PlanOutcome
    = PlanValid
      -- ^ Everything resolves; commit lands it and the worker may act.
    | PlanVisibleInvalid
      -- ^ The piece's ART is known, so it can be DRAWN — in red — but
      --   it cannot be built here. Missing build metadata with otherwise
      --   valid art lands here, and so does every terrain, prerequisite
      --   and occupancy refusal.
    | PlanMissingArt
      -- ^ No art resolves: an unregistered pack, an undeclared kind, an
      --   unparseable kind or edge, or a pack whose textures terminally
      --   failed to load. There is nothing to draw at all.
    | PlanUnresolvedTerrain
      -- ^ A TERRAIN-dependent fact is unavailable because the storing
      --   chunk is not resident. Not a refusal: the candidate is
      --   retained and re-checked when the terrain publishes.
    deriving (Show, Eq)

-- | Stable wire\/log spelling of an outcome.
planOutcomeName ∷ PlanOutcome → Text
planOutcomeName o = case o of
    PlanValid             → "valid"
    PlanVisibleInvalid    → "visible-invalid"
    PlanMissingArt        → "missing-art"
    PlanUnresolvedTerrain → "unresolved-terrain"

-- | The resolver's full answer for one candidate.
data PlanResult = PlanResult
    { prOutcome  ∷ !PlanOutcome
    , prReason   ∷ !Text
      -- ^ Which check decided it, for logs and tests. Stable text, not
      --   a sentence to parse.
    , prSlot     ∷ !(Maybe StructureSlot)
      -- ^ The exact slot this piece would occupy, once the descriptor
      --   parses at all.
    , prSurfaceZ ∷ !(Maybe Int)
      -- ^ The tile's CURRENT surface z, once terrain resolves. This is
      --   the quantity a designation captures as 'cdZ'.
    , prFinalZ   ∷ !(Maybe Int)
      -- ^ The grid z the piece would sit at, once terrain (and, for a
      --   post, its supporting floor) resolves. Never persisted: 'cdZ'
      --   is a SURFACE level, and the progress-slope stamping reads it
      --   as one.
      --
      --   Present on INVALID results too wherever terrain resolved, so
      --   #1846's ghost can draw a refused candidate red at the place it
      --   would have occupied (D-25). Present is not permission: only a
      --   'PlanValid' result may be placed from.
    } deriving (Show, Eq)

-- | The coherent snapshot one resolution runs against. Taken once by
--   the caller so every check in a sweep sees the same world, rather
--   than each check re-reading refs that another thread may move
--   between them.
data PlanWorld = PlanWorld
    { pwWorldSize    ∷ !Int
    , pwTiles        ∷ !WorldTileData
    , pwStage        ∷ !StructureStage
    , pwDesignations ∷ !ConstructDesignations
    , pwCatalog      ∷ !StructureArtCatalog
    , pwProposedWire ∷ !(HS.HashSet (Int, Int))
      -- ^ CANONICAL tiles an uncommitted gesture is proposing to wire and
      --   that this same resolver has already answered 'PlanValid' for
      --   (#1846). Empty for every non-render caller, which is what makes
      --   'wireNeighborsWithProposed' collapse to 'wireNeighborsAt' there.
      --
      --   It reaches ART ONLY, never an outcome: a wire's connection
      --   shape picks which variant of the pack's wire art is drawn, and
      --   registration is all-or-nothing per kind, so no shape can turn
      --   resolvable art into missing art or the reverse. That is what
      --   lets the render pass resolve outcomes once with an empty set
      --   and then re-resolve only the ART against the valid candidates
      --   it found, without the two passes disagreeing.
    }

-- | Which operation is asking, which decides only how outstanding
--   designations are treated (requirement 7).
data PlanOp
    = PlanForPlacement
      -- ^ Preview and commit: EVERY outstanding designation conflicts.
    | PlanForAttempt !ConstructAttemptId
      -- ^ Revalidation and the worker's own re-checks on behalf of one
      --   attempt: that exact attempt is itself, and anything else at
      --   the tile is a conflict.
    | PlanForCommit !ConstructAttemptId
      -- ^ 'PlanForAttempt' for an attempt that is INSIDE its placement
      --   hand-off: the piece in its target slot is the caller's OWN,
      --   staged moments ago, so slot occupancy alone is not a refusal.
      --
      --   Everything else still is. That is the whole difference between
      --   this and skipping the check: the hand-off window is short but
      --   real, and the world thread can drain a terrain, fluid or
      --   catalogue mutation inside it — a site whose surface has
      --   drifted or whose pack has gone must not be completed just
      --   because the worker got there first.
    deriving (Show, Eq)

-- | Which structure slot a descriptor targets, mirroring
--   @scripts\/unit_ai_construct.lua@'s @placeStructurePiece@ derivation
--   (a wall with no recorded edge defaults to @ne@, a post to @n@ — the
--   designation tool has no corner picker yet) so occupancy is checked
--   against the exact slot the worker will eventually place into (#805).
structurePieceSlot ∷ StructurePiece → Maybe StructureSlot
structurePieceSlot (StructurePiece _ kind edge) = case kind of
    "floor"   → slotFromText "floor"
    "ceiling" → slotFromText "ceiling"
    "wire"    → slotFromText "wire"
    "wall"    → slotFromText ("wall_" <> fromMaybe "ne" edge)
    "post"    → slotFromText ("post_" <> fromMaybe "n" edge)
    _         → Nothing

-- | The grid z a placed piece would sit at, given the tile's surface z
--   and the z of any floor already on it.
--
--   Mirrors @scripts\/structures.lua@'s @placeKind@ \/ @placeWall@ and
--   @scripts\/wire.lua@ exactly: floor, wall and wire sit in the air
--   cell ON TOP of the solid terrain (@surface + 1@), a ceiling one
--   above that, and a POST takes its supporting floor's own z — which
--   is why a post with no floor has no final z at all, and is the same
--   fact as its prerequisite failing.
structureFinalGridZ ∷ StructurePiece → Int → Maybe Int → Maybe Int
structureFinalGridZ piece surfaceZ mFloorZ = case spKind piece of
    "floor"   → Just (surfaceZ + 1)
    "wall"    → Just (surfaceZ + 1)
    "wire"    → Just (surfaceZ + 1)
    "ceiling" → Just (surfaceZ + 2)
    "post"    → mFloorZ
    _         → Nothing

-- | A tile's current surface z, or 'Nothing' when its storing chunk is
--   not resident.
--
--   #1175: canonicalised, so a tile named by an anchor-local alias
--   resolves the chunk that actually STORES it. Identity inland.
planSurfaceZAt ∷ Int → WorldTileData → (Int, Int) → Maybe Int
planSurfaceZAt worldSize tileData (gx, gy) =
    let (coord, (lx, ly), _) = canonicalTileFrame worldSize gx gy
    in (\lc → lcSurfaceMap lc VU.! columnIndex lx ly) <$> lookupChunk coord tileData

-- | Resolve one candidate tile.
--
--   @requiredZ@ is the surface level the candidate must sit at: the
--   ANCHOR's surface z for a drag (designations are per-z-level, exactly
--   as mining is), and the designation's own captured 'cdZ' when
--   revalidating. Requirement 4 is why revalidation passes 'cdZ' and
--   never re-derives it: a loaded site whose surface has drifted is
--   INVALID, never silently retargeted upward or downward.
resolveStructurePlan
    ∷ PlanWorld → PlanOp
    → Int              -- ^ required surface z
    → StructurePiece
    → (Int, Int)       -- ^ candidate tile, any u-alias
    → PlanResult
resolveStructurePlan pw op requiredZ piece tile@(gx, gy) =
    case structurePieceSlot piece of
        Nothing → bare PlanMissingArt
            ("unknown structure kind '" <> spKind piece <> "'") Nothing
        Just slot
            -- Catalogue first (requirement 6): a known art or
            -- build-metadata failure outranks anything terrain could
            -- say, including terrain being absent.
            | isNothing mArt →
                bare PlanMissingArt
                    ("no registered art for '" <> spPack piece <> "/"
                       <> spKind piece <> "'") (Just slot)
            | isNothing mBuild →
                -- Decided by the CATALOGUE and not by terrain, which is
                -- the module's ordering rule and stays. The position
                -- fields are still filled in when terrain happens to be
                -- resident, because this is the one refusal whose ART
                -- resolves: #1846's ghost draws such a candidate red,
                -- and it needs the resolver to say where.
                positioned slot PlanVisibleInvalid
                    ("no complete build metadata for '" <> spPack piece
                       <> "/" <> spKind piece <> "'")
            | otherwise → case planSurfaceZAt worldSize tiles tile of
                Nothing →
                    bare PlanUnresolvedTerrain "terrain not resident"
                         (Just slot)
                Just surfaceZ
                    | surfaceZ ≢ requiredZ →
                        -- 'prFinalZ' names where a piece WOULD sit given
                        -- today's surface, for the sole benefit of a
                        -- ghost that has to draw this refusal (D-25:
                        -- every loaded candidate the oracle filters out
                        -- still draws, in red). It is not a retarget:
                        -- the outcome is invalid, and only 'PlanValid'
                        -- results are ever placed from.
                        at slot surfaceZ (finalZ surfaceZ) PlanVisibleInvalid
                            ("surface z " <> tshow surfaceZ
                               <> " differs from the plan's z "
                               <> tshow requiredZ)
                    | spKind piece ≡ "post" ∧ isNothing floorZ →
                        at slot surfaceZ Nothing PlanVisibleInvalid
                            "no floor under the post"
                    | present slot ∧ not committing →
                        at slot surfaceZ (finalZ surfaceZ) PlanVisibleInvalid
                            "the target slot is already occupied"
                    | conflictingDesignation →
                        at slot surfaceZ (finalZ surfaceZ) PlanVisibleInvalid
                            "the tile already carries an outstanding \
                            \construction designation"
                    | otherwise →
                        at slot surfaceZ (finalZ surfaceZ) PlanValid "ok"
  where
    worldSize = pwWorldSize pw
    tiles     = pwTiles pw
    stage     = pwStage pw

    bare outcome reason mSlot = PlanResult
        { prOutcome = outcome, prReason = reason, prSlot = mSlot
        , prSurfaceZ = Nothing, prFinalZ = Nothing }
    at slot surfaceZ mFinal outcome reason = PlanResult
        { prOutcome = outcome, prReason = reason, prSlot = Just slot
        , prSurfaceZ = Just surfaceZ, prFinalZ = mFinal }
    -- A result whose OUTCOME was decided without consulting terrain, but
    -- which still reports where the piece would sit if terrain happens
    -- to be resident. Never changes an outcome — the caller has already
    -- chosen it.
    positioned slot outcome reason = case planSurfaceZAt worldSize tiles tile of
        Nothing       → bare outcome reason (Just slot)
        Just surfaceZ → at slot surfaceZ (finalZ surfaceZ) outcome reason

    present slot = structurePresentAt worldSize tiles stage slot gx gy

    -- The supporting floor's own z, which is also the post's final grid
    -- z. Reads the SAME two authorities @structure.floorZAt@ does, so a
    -- floor placed earlier in this tick counts.
    floorZ = structureGridZAt worldSize tiles stage SFloor gx gy

    finalZ surfaceZ = structureFinalGridZ piece surfaceZ floorZ

    -- The art the piece would be BUILT with, through the one shared
    -- derivation the render pass also calls (#1846). Not a check the
    -- render pass repeats differently: it is the same function, so a
    -- ghost cannot resolve a cap, a wire variant or a sprite the
    -- placer would not.
    mArt   = resolvePlanPieceArt pw piece tile
    -- The registered COST, not the pack's own `buildable` declaration:
    -- what makes a job doable is a cost the engine can actually charge,
    -- and that is the same value 'construction.payMaterials' spends and
    -- a legacy paid designation's receipt is reconstructed from. One
    -- authority, so admission and payment cannot disagree about whether
    -- a kind is buildable.
    mBuild = packKindBuild (pwCatalog pw) (spPack piece) (spKind piece)

    -- The caller is mid-hand-off, so the piece in its target slot is its
    -- own. Only that one check relaxes.
    committing = case op of
        PlanForCommit _ → True
        _               → False

    conflictingDesignation =
        case HM.lookup (canonicalTile worldSize gx gy) (pwDesignations pw) of
            Nothing → False
            Just cd → case op of
                PlanForPlacement   → True
                PlanForAttempt aid → cdAttempt cd ≢ aid
                PlanForCommit aid  → cdAttempt cd ≢ aid

-- | The world CONTEXT one candidate's art resolves through: the cap state
--   a wall's own two end corners give it, and the connection shape a
--   wire's neighbours give it.
--
--   Exported because the render pass needs the SAME derivation the
--   resolver runs (#1846 requirement 4, and the review's correction that
--   the ghost must consume this rather than add a render-only rule). Two
--   copies of "which cap facemap would this wall be built with" is
--   exactly the drift the shared resolver exists to prevent.
--
--   Kinds with no variants ignore both fields, and get
--   'defaultPieceArtContext' — a statement that there is no context to
--   look up, never a stand-in for one that was not.
structurePieceArtContext
    ∷ PlanWorld → StructurePiece → (Int, Int) → PieceArtContext
structurePieceArtContext pw piece (gx, gy) = case spKind piece of
    "wall" → defaultPieceArtContext
        { pacWallCaps = wallCapsAt (pwWorldSize pw) (pwTiles pw) (pwStage pw)
                            (wallEdgeOf (spEdge piece)) gx gy }
    "wire" → defaultPieceArtContext { pacWireShape = wireShapeOf }
    _      → defaultPieceArtContext
  where
    wallEdgeOf mEdge = case mEdge of
        Just "nw" → WallNW
        Just "se" → WallSE
        Just "sw" → WallSW
        _         → WallNE

    wireShapeOf = wireShapeFor
        (wireNeighborsWithProposed (pwWorldSize pw) (pwTiles pw) (pwStage pw)
             (Just (pwDesignations pw)) (pwProposedWire pw) gx gy)

-- | The exact texture and facemap this candidate would be built with, or
--   'Nothing' when the pack, the kind or the art itself does not resolve
--   ('Structure.ArtCatalog.resolveUnplacedArt' — an unregistered pack, an
--   undeclared kind, an unparseable kind or edge, or a pack whose
--   textures terminally failed).
--
--   'Nothing' here IS 'PlanMissingArt': the resolver's own catalogue
--   check calls this, so a ghost that draws exactly what this returns
--   draws nothing precisely when the plan says @missing-art@, with no
--   second rule to keep in step (requirement 7).
resolvePlanPieceArt
    ∷ PlanWorld → StructurePiece → (Int, Int) → Maybe PieceArt
resolvePlanPieceArt pw piece tile =
    resolveUnplacedArt (pwCatalog pw) (spPack piece) (spKind piece)
                       (spEdge piece) (structurePieceArtContext pw piece tile)
