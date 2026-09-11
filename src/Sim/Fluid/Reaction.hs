{-# LANGUAGE Strict #-}
-- | Unlike-fluid contact for the active fluid simulation (#2481, FR-1
--   of epic #2480).
--
--   Every active-sim transfer site used to keep the DESTINATION cell's
--   fluid type whenever the destination was occupied, so lava arriving
--   in water silently became water and water arriving in lava silently
--   became lava. Fluid identity was rewritten by arrival and nothing
--   observed it. The owner's decision (D-1/D-3 in
--   @docs\/fluid_reaction_design.md@) is that unlike contact REACTS:
--   volume is consumed 1:1 from both sides, and a cell whose lava is
--   exhausted that way emits a solidification event describing the
--   stone the contact will become.
--
--   This module owns the whole rule so all five occupied-destination
--   write branches in "Sim.Fluid.Active" — seam @transferCell@, gravity,
--   both lateral branches, and waterfall — route through ONE applier
--   ('applyTransfer') rather than each re-deriving it. Restoring any one
--   of them to the old add-to-destination behavior is therefore a local,
--   independently observable regression.
--
--   The contract, in full:
--
--   * Unlike contact is 'Lava' versus any of 'Ocean', 'Lake', 'River'.
--     Water types among themselves are ONE compatible class.
--   * No occupied contact changes either cell's type. Each live cell
--     keeps its type until its volume reaches zero, at which point it
--     becomes empty ('Nothing') and an ordinary empty destination again.
--   * A contact is resolved from both cells' CURRENT LIVE type and
--     volume, before either side is debited. The consumed amount is the
--     smaller live volume; it is subtracted from both sides and NONE of
--     the requested transfer moves. A reaction can therefore consume
--     more than the planned transfer, which is why every caller re-reads
--     the live source through this applier instead of spending against a
--     frozen snapshot's remaining balance.
--   * A compatible or empty destination takes an ordinary transfer,
--     bounded by the requested amount, the LIVE source volume, and the
--     destination's remaining @'maxBound' ∷ 'Word16'@ capacity. Every
--     undelivered unit stays at the source; no addition or subtraction
--     wraps.
--
--   Events are pure data here, and so is the grouping that turns a
--   delivery of them into coherent 'ReactionResult's. The CONSUMER is
--   the world thread (#2485, 'World.Thread.Command.Reaction'): it is the
--   sole writer of the tiles and the sole minter of live-edit
--   generations, so admitting a result and committing its stone belong
--   there, not here. See 'Sim.State.Types.swsSolidEvents' for where
--   events accumulate and @docs\/engine_contracts.md@ §Fluid reaction
--   for the drain contract.
module Sim.Fluid.Reaction
    ( SolidProduct(..)
    , SolidificationEvent(..)
    , ReactionResult(..)
    , CellSite(..)
    , TransferOutcome(..)
    , unlikeContact
    , solidProductFor
    , applyTransfer
    , dedupeEvents
    , groupReactionResults
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST (ST)
import World.Chunk.Types (ChunkCoord)
import World.Constants (seaLevel)
import World.Fluid.Types (FluidType(..))
import Sim.Fluid.Types (ActiveFluidCell(..), volumeToSurface)

-- | The stone an exhausted lava column will become (D-2/D-5). Resolved
--   at the reaction, from the two contacting cells and the page-wide
--   'seaLevel' constant, so FR-2 never has to reconstruct it from a
--   fluid grid the rest of the tick has already changed.
data SolidProduct
    = SolidObsidian   -- ^ subaerial rapid quench
    | SolidBasalt     -- ^ submerged or subsea solidification
    deriving (Show, Eq)

-- | One lava cell exhausted by unlike contact. Emitted by the tick,
--   consumed by nothing in this slice (FR-2 owns the drain).
data SolidificationEvent = SolidificationEvent
    { sevChunk        ∷ !ChunkCoord
      -- ^ The CANONICAL stored chunk key of the exhausted lava cell —
      --   the key the sim already holds that chunk under, so a wrapped
      --   cylindrical seam names the same coordinate the page stores
      --   (#2044). The lava cell is whichever side of the contact ran
      --   out, which at a seam may be either side.
    , sevIndex        ∷ !Int
      -- ^ Local cell index within that chunk, @ly * chunkSize + lx@.
    , sevWaterChunks  ∷ ![ChunkCoord]
      -- ^ The CANONICAL stored chunk keys of every WATER cell that fed
      --   this coordinate's stone THIS TICK, in contact order and
      --   without repeats. One entry for an ordinary contact — equal to
      --   'sevChunk' when it was in-chunk, different for a seam one,
      --   including across the cylindrical u wrap.
      --
      --   A LIST rather than one key because a coordinate can react more
      --   than once in a tick: exhausted against an in-chunk neighbour,
      --   refilled with lava by a later phase, then exhausted again
      --   across the seam. 'dedupeEvents' keeps ONE stone for that
      --   coordinate, but every chunk that lost fluid to it is still a
      --   participant — FR-2 (#2485) commits the stone and the surviving
      --   water together, so all their live-edit generations have to be
      --   admitted together or half the result lands against a world the
      --   other half no longer describes. Dropping a later contact's
      --   chunk here would leave its consumed-fluid writeback outside
      --   the result's own admission.
    , sevWaterType    ∷ !FluidType
      -- ^ The contacting water side's type ('Ocean', 'Lake' or 'River').
    , sevConsumed     ∷ !Word16
      -- ^ Volume consumed from EACH side by this contact (the smaller
      --   of the two live volumes).
    , sevStoneTop     ∷ !Int
      -- ^ The new stone's top z: the lava column's terrain top plus one.
    , sevWaterSurface ∷ !Int
      -- ^ The contacting water cell's fluid surface AFTER annihilation
      --   ('volumeToSurface' of its terrain and remaining volume).
    , sevProduct      ∷ !SolidProduct
      -- ^ D-5 evaluated at the reaction from the three fields above.
    } deriving (Show, Eq)

-- | Where one side of a contact lives, and how high its terrain is.
--   Enough to name the event's coordinate and to evaluate D-5 without
--   the applier knowing which phase called it.
data CellSite = CellSite
    { csChunk   ∷ !ChunkCoord
    , csIndex   ∷ !Int
    , csTerrain ∷ !Int
    } deriving (Show, Eq)

-- | What one applied request actually did.
data TransferOutcome = TransferOutcome
    { toMoved    ∷ !Int
      -- ^ Units actually moved source → destination. Zero for a
      --   reaction: unlike contact moves none of the request.
    , toConsumed ∷ !Int
      -- ^ Units consumed from EACH side by a reaction. Zero otherwise.
    , toEvent    ∷ !(Maybe SolidificationEvent)
      -- ^ Present only when the reaction exhausted the lava side.
    } deriving (Show, Eq)

noTransfer ∷ TransferOutcome
noTransfer = TransferOutcome 0 0 Nothing

-- | Unlike contact is lava versus water. Water types among themselves
--   are one compatible class, so only the lava-ness of the two sides
--   matters.
unlikeContact ∷ FluidType → FluidType → Bool
unlikeContact a b = isLava a ≢ isLava b
  where isLava Lava = True
        isLava _    = False
{-# INLINE unlikeContact #-}

-- | D-5, evaluated at the moment of annihilation. Basalt when the water
--   side was 'Ocean', OR the water surface left behind stands above the
--   new stone top (the water will equalize onto it), OR the new stone
--   top is at or below 'seaLevel'. Obsidian otherwise.
solidProductFor ∷ FluidType  -- ^ contacting water type
                → Int        -- ^ new stone top
                → Int        -- ^ water surface after annihilation
                → SolidProduct
solidProductFor waterType stoneTop waterSurface
    | waterType ≡ Ocean       = SolidBasalt
    | waterSurface > stoneTop = SolidBasalt
    | stoneTop ≤ seaLevel     = SolidBasalt
    | otherwise               = SolidObsidian

-- | Apply ONE planned transfer request live: the reaction when the two
--   live cells are unlike, an ordinary bounded transfer otherwise.
--
--   Both cells are read from the live grids first, so a request planned
--   against a frozen snapshot is always resolved against what the cells
--   actually hold now. The source and destination may be in the same
--   grid (the in-chunk phases) or in two different ones (the seam), and
--   the applier never assumes which.
applyTransfer ∷ MV.MVector s (Maybe ActiveFluidCell) → CellSite
              → MV.MVector s (Maybe ActiveFluidCell) → CellSite
              → Int
              → ST s TransferOutcome
applyTransfer mSrc srcSite mDst dstSite requested = do
    msrc ← MV.read mSrc (csIndex srcSite)
    case msrc of
        Nothing → pure noTransfer
        Just s
            | afcVolume s ≡ 0 → pure noTransfer
            | otherwise → do
                mdst ← MV.read mDst (csIndex dstSite)
                case mdst of
                    Just d | afcVolume d > 0 →
                        if unlikeContact (afcType s) (afcType d)
                        then annihilate mSrc srcSite s mDst dstSite d
                        else moveInto mSrc srcSite s mDst dstSite (Just d) requested
                    _ → moveInto mSrc srcSite s mDst dstSite Nothing requested

-- | Contact annihilation: consume the smaller live volume from both
--   sides, move none of the request, and emit an event when the side
--   that reached zero was the lava one.
annihilate ∷ MV.MVector s (Maybe ActiveFluidCell) → CellSite → ActiveFluidCell
           → MV.MVector s (Maybe ActiveFluidCell) → CellSite → ActiveFluidCell
           → ST s TransferOutcome
annihilate mSrc srcSite s mDst dstSite d = do
    let srcVol   = afcVolume s
        dstVol   = afcVolume d
        consumed = min srcVol dstVol
        srcVol'  = srcVol - consumed
        dstVol'  = dstVol - consumed
    MV.write mSrc (csIndex srcSite) (settled s srcVol')
    MV.write mDst (csIndex dstSite) (settled d dstVol')
    -- Exactly one side is lava (that is what made the contact unlike),
    -- so at most one event can come out of a contact.
    let ev | afcType s ≡ Lava ∧ srcVol' ≡ 0 =
               Just (eventAt srcSite (afcType d) consumed dstSite dstVol')
           | afcType d ≡ Lava ∧ dstVol' ≡ 0 =
               Just (eventAt dstSite (afcType s) consumed srcSite srcVol')
           | otherwise = Nothing
    pure TransferOutcome { toMoved    = 0
                         , toConsumed = fromIntegral consumed
                         , toEvent    = ev
                         }
  where
    -- A cell whose volume reaches zero IS empty, and an ordinary empty
    -- destination for every later request this tick (requirement 4).
    settled cell vol | vol ≡ 0   = Nothing
                     | otherwise = Just cell { afcVolume = vol }

-- | Build the event for an exhausted lava cell, given the water side it
--   reacted with and that side's remaining volume.
eventAt ∷ CellSite → FluidType → Word16 → CellSite → Word16
        → SolidificationEvent
eventAt lavaSite waterType consumed waterSite waterVol' =
    let stoneTop      = csTerrain lavaSite + 1
        waterSurface  = volumeToSurface (csTerrain waterSite) waterVol'
    in SolidificationEvent
        { sevChunk        = csChunk lavaSite
        , sevIndex        = csIndex lavaSite
        , sevWaterChunks  = [csChunk waterSite]
        , sevWaterType    = waterType
        , sevConsumed     = consumed
        , sevStoneTop     = stoneTop
        , sevWaterSurface = waterSurface
        , sevProduct      = solidProductFor waterType stoneTop waterSurface
        }

-- | An ordinary transfer into an empty or compatible destination,
--   bounded by the request, the LIVE source volume, and the
--   destination's remaining 'Word16' capacity. Whatever does not fit
--   stays at the source, so neither side wraps.
moveInto ∷ MV.MVector s (Maybe ActiveFluidCell) → CellSite → ActiveFluidCell
         → MV.MVector s (Maybe ActiveFluidCell) → CellSite
         → Maybe ActiveFluidCell → Int
         → ST s TransferOutcome
moveInto mSrc srcSite s mDst dstSite mdst requested = do
    let srcVol   = fromIntegral (afcVolume s) ∷ Int
        dstVol   = maybe 0 (fromIntegral . afcVolume) mdst ∷ Int
        capacity = fromIntegral (maxBound ∷ Word16) - dstVol ∷ Int
        actual   = max 0 (min requested (min srcVol capacity))
    if actual ≤ 0
    then pure noTransfer
    else do
        MV.write mSrc (csIndex srcSite)
            (Just s { afcVolume = afcVolume s - fromIntegral actual })
        MV.write mDst (csIndex dstSite) $ Just $ case mdst of
            -- An occupied compatible destination keeps its own type; an
            -- empty one (including a zero-volume leftover) takes the
            -- source's.
            Just d  → d { afcVolume = fromIntegral (dstVol + actual) }
            Nothing → ActiveFluidCell { afcType    = afcType s
                                      , afcVolume  = fromIntegral actual
                                      , afcFlowDir = 0
                                      }
        pure TransferOutcome { toMoved = actual, toConsumed = 0, toEvent = Nothing }

-- | At most ONE event per canonical coordinate per tick (requirement 4),
--   carrying the UNION of every contact that fed it.
--
--   Keeps the first event at each coordinate, in emission order, and
--   therefore its product and its water TYPE: a cell refilled after
--   annihilating keeps the stone it already produced and neither cancels
--   nor duplicates it. A LATER tick may emit another event at that
--   coordinate once new lava has arrived and been exhausted again, which
--   is why this is per-tick and not cumulative.
--
--   What it does NOT drop is the later contacts' participating chunks.
--   A coordinate exhausted against an in-chunk neighbour, refilled by a
--   later phase and exhausted again across the seam has taken fluid from
--   two chunks, and both of their consumed-fluid writebacks ride the same
--   delivery as this one stone. Keeping only the first contact's chunk
--   would leave the second one outside the result's own admission, so an
--   intervening edit there could stale its writeback while the stone
--   committed anyway (#2485).
dedupeEvents ∷ [SolidificationEvent] → [SolidificationEvent]
dedupeEvents events = map merge (foldl' note [] events)
  where
    key e = (sevChunk e, sevIndex e)

    -- (first event at this coordinate, water chunks in contact order)
    note acc e = case break ((≡ key e) . key . fst) acc of
        (before, (kept, waters) : after) →
            before ⧺ (kept, waters ⧺ [ w | w ← sevWaterChunks e
                                         , w `notElem` waters ]) : after
        (before, []) → before ⧺ [(e, sevWaterChunks e)]

    merge (e, waters) = e { sevWaterChunks = waters }

-- | One COHERENT reaction result: every chunk the contacts in it
--   touched, the live-edit generation each of those chunks' half was
--   computed from, and the events to commit as one unit (#2485).
--
--   The world thread admits a result whole or rejects it whole. That is
--   what a two-chunk contact needs — the stone goes in the lava chunk
--   while the surviving water stays in the other, so admitting one side
--   against a generation the other no longer sits at would land half a
--   reaction. It is equally what SIBLING events need: committing one
--   event's 'World.Edit.Types.WeAddTile' advances its chunk's
--   generation, so an event judged afterwards against that advanced
--   number would read as stale purely because its own sibling landed
--   first (requirement 5).
data ReactionResult = ReactionResult
    { rrParticipants ∷ ![(ChunkCoord, Word64)]
      -- ^ Participating chunks in canonical-key order, each with the
      --   'Sim.State.Types.scsEditGen' this result's half for that chunk
      --   was computed from. Every one must still match the page's own
      --   generation for the result to be admitted.
    , rrEvents       ∷ ![SolidificationEvent]
      -- ^ The events, in emission order.
    } deriving (Show, Eq)

-- | Partition a delivery's events into coherent results: two events
--   share a result exactly when they share a participating chunk,
--   transitively.
--
--   Transitively, because admission is per chunk and the commit advances
--   every participating chunk's generation at once. Events A and B
--   sharing chunk X, and B and C sharing chunk Y, all have to be judged
--   from the same pre-commit generations or C would be measured against
--   the number A's commit moved. Genuinely disjoint contacts share no
--   chunk and therefore stay independently eligible: one stale pair does
--   not veto an unrelated fresh one (requirement 4).
--
--   Results come out in first-emission order and each result's events in
--   emission order, so a delivery is reproducible rather than dependent
--   on hash iteration.
groupReactionResults ∷ (ChunkCoord → Word64)
                       -- ^ The generation each chunk's half was computed
                       --   from ('Sim.State.Types.scsEditGen').
                     → [SolidificationEvent]
                     → [ReactionResult]
groupReactionResults genOf events =
    map build (foldl' absorb [] (zip [0 ..] events))
  where
    byIndex = HM.fromList (zip [0 ∷ Int ..] events)

    chunksOf e = HS.fromList (sevChunk e : sevWaterChunks e)

    -- A component is (first event index, its chunks, its event indices).
    absorb comps (i, e) =
        let ks       = chunksOf e
            touching = filter (\(_, keys, _) → overlaps ks keys) comps
            rest     = filter (\(_, keys, _) → not (overlaps ks keys)) comps
            keys'    = foldl' (\acc (_, keys, _) → HS.union acc keys) ks touching
            idxs'    = L.sort (i : concatMap (\(_, _, is) → is) touching)
            first'   = minimum (i : map (\(f, _, _) → f) touching)
        in L.insertBy (\(a, _, _) (b, _, _) → compare a b)
                      (first', keys', idxs') rest

    overlaps ks keys = not (HS.null (HS.intersection ks keys))

    build (_, keys, idxs) = ReactionResult
        { rrParticipants = [ (cc, genOf cc) | cc ← L.sort (HS.toList keys) ]
        , rrEvents       = [ e | i ← idxs, Just e ← [HM.lookup i byIndex] ]
        }
