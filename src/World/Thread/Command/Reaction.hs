{-# LANGUAGE Strict #-}

-- | Committing the lava-water reaction's product as durable stone
--   (#2485, FR-2 of epic #2480).
--
--   #2481 made unlike-fluid contact annihilate volume inside the sim and
--   emit a 'Sim.Fluid.Reaction.SolidificationEvent' wherever lava ran
--   out. Nothing consumed those events. This module is the consumer, and
--   it runs on the WORLD thread because everything the commit needs is
--   owned there: 'World.State.Types.wsTilesRef' (the sole writer),
--   'World.State.Types.wsEditsRef' (the durable log), and
--   'World.State.Types.wsChunkEditGenRef' (the generation mint).
--
--   Durability is why the stone is an EDIT and not a writeback.
--   'World.Thread.Command.applyOneWriteback' replaces a chunk's sim-owned
--   fields in memory and appends nothing, so terrain written only that
--   way would vanish the moment the chunk was evicted or the save
--   reloaded. A @'World.Edit.Types.WeAddTile'@ appended to
--   'World.State.Types.wsEditsRef' replays over regenerated terrain and
--   persists as @wpsEdits@, exactly as a player's own add-tile does.
--
--   Three rules hold this together:
--
--   * __Admission is coherent and comes first.__ A result names every
--     chunk it touched and the generation each of those halves was
--     computed from; all of them must still match, or the whole result
--     is rejected. Half a reaction is never committed, including across
--     the cylindrical seam.
--   * __Generations advance once, at the end.__ The stone edits
--     themselves bump the very generations admission compares against,
--     so a sibling event judged after its neighbour landed would read as
--     stale purely because of it. Every admitted event is applied first
--     and the bump happens after (requirement 5).
--   * __A rejected result takes its own fluid with it.__ Its writebacks
--     are quarantined by the caller and its chunks are re-seeded from the
--     authoritative tiles, so the lava the discarded reaction consumed
--     comes back instead of vanishing with no stone to show for it.
--   * __Admission is decided BEFORE any of it is applied.__ 'admitReaction'
--     resolves every event's material and rehearses every event's edit
--     against a private overlay of the live tiles. Deciding later — while
--     committing — would have let a result whose material or column
--     failed halfway keep the writeback that recorded its annihilation,
--     which is precisely the all-or-nothing rule it is supposed to
--     enforce.
module World.Thread.Command.Reaction
    ( ReactionAdmission(..)
    , admitReaction
    , reactionIsFresh
    , reactionChunks
    , commitReactions
    , convergeRejectedReactions
    , reactionEventTile
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Monad (foldM)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Data.List (nub)
import qualified Data.Text as T
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import Engine.Core.State (EngineEnv, unitQueue)
import Sim.Command.Types (SimCommand(..), ReactionChunkSync(..))
import Control.Applicative ((<|>))
import Sim.Fluid.Reaction (ReactionResult(..), SolidificationEvent(..))
import Unit.Command.Types (UnitCommand(..))
import World.Chunk.Admit (pageIncarnation)
import World.Construct.Revalidate
    (ConstructScope(..), revalidateConstructDesignations)
import World.Edit.Apply (applyEdit)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Flora.Designation (replaceChunkForgettingFlora)
import World.Generate.Coordinates (chunkToGlobal)
import World.Material (MaterialId(..), MaterialRegistry)
import World.Plant.Validate (revalidatePlantDesignations)
import World.Reaction.Stone (stoneMaterialFor)
import World.Thread.Command.Edit.Sync (syncEditToSim)
import World.Thread.Command.Reaction.Zoom (refreshZoomTerrain)
import World.Types

-- | Is every chunk this result touched still at the generation its half
--   was computed from?
--
--   The same equality 'World.Thread.Command.writebackIsFresh' applies to
--   one chunk, over all of them at once: an absent entry reads as
--   generation 0, the baseline a never-edited — or evicted and reloaded
--   — chunk sits at on both sides. Equality, not @>=@, for the same
--   reason it is there: a result claiming a generation this page never
--   issued is no more derived from the current chunks than one claiming
--   an older.
--
--   All-or-nothing, unlike the per-chunk writeback decision, because the
--   two halves of a cross-chunk contact only make sense together. The
--   stone goes in the lava chunk and the surviving water stays in the
--   other; admitting one against a world the other no longer describes
--   is exactly the partial commit requirement 6 forbids.
--
--   NOT sufficient on its own. A generation of 0 also reads fresh for a
--   participant the page no longer HOLDS, because eviction retires the
--   entry — so 'admitReaction' pairs this with a presence check rather
--   than trusting the number alone.
reactionIsFresh ∷ HM.HashMap ChunkCoord Word64 → ReactionResult → Bool
reactionIsFresh gens rr =
    all (\(cc, g) → g ≡ HM.lookupDefault 0 cc gens) (rrParticipants rr)

-- | What the world thread decided about one delivered result.
data ReactionAdmission
    = ReactionAdmitted ![(SolidificationEvent, MaterialId)]
      -- ^ Every event, with its product material already resolved, in
      --   the order they are to be applied. Resolving here rather than
      --   at each edit is what makes the commit unable to fail halfway.
    | ReactionRefused !Text
      -- ^ …and why. The caller quarantines this result's writebacks and
      --   converges its chunks.
    deriving (Show, Eq)

-- | Decide one result, against the tiles and generations as they stand
--   BEFORE anything from this delivery has been applied.
--
--   Four things have to hold, and every one of them is a way a commit
--   could otherwise have landed half a reaction:
--
--   1. every participant is still at the generation its half was
--      computed from ('reactionIsFresh');
--   2. every participant is still LOADED. Eviction retires a chunk's
--      generation entry, so a result computed at generation 0 and
--      delivered after one participant was evicted passes (1) — and then
--      its events, its writebacks and its sync entry are all silently
--      skipped for that chunk, which is a partial commit by omission;
--   3. every event's product material resolves through the registry; and
--      a name the registry does not know is a refusal, not an absent
--      stone, because the lava it would have accounted for is already
--      gone;
--   4. every event's edit actually applies — the chunk is loaded and the
--      column is in range — REHEARSED in order against a private overlay,
--      so a sibling that only becomes applicable (or inapplicable) after
--      an earlier one has grown its column is judged on what it will
--      really meet.
admitReaction ∷ MaterialRegistry → HM.HashMap ChunkCoord Word64
              → WorldTileData → ReactionResult → ReactionAdmission
admitReaction registry gens td rr
    | not (reactionIsFresh gens rr) =
        ReactionRefused "a participant has moved on from the generation \
                        \its half was computed from"
    | (missing : _) ← absentParticipants =
        ReactionRefused ("participant chunk " <> tshow missing
                         <> " is no longer loaded")
    | otherwise = go HM.empty [] (rrEvents rr)
  where
    absentParticipants =
        [ cc | cc ← reactionChunks rr, isNothing (lookupChunk cc td) ]

    go _ acc [] = ReactionAdmitted (reverse acc)
    go overlay acc (ev : rest) =
        case stoneMaterialFor registry (sevProduct ev) of
            Left why → ReactionRefused why
            Right mat →
                case HM.lookup (sevChunk ev) overlay
                         <|> lookupChunk (sevChunk ev) td of
                    Nothing → ReactionRefused
                        ("chunk " <> tshow (sevChunk ev) <> " is not loaded")
                    Just lc
                        | outOfColumnRange lc (sevIndex ev) → ReactionRefused
                            ("the column at " <> tshow (reactionEventTile ev)
                             <> " is out of range")
                        | otherwise →
                            let (gx, gy) = reactionEventTile ev
                                lc' = applyEdit (WeAddTile gx gy mat) lc
                            in go (HM.insert (sevChunk ev) lc' overlay)
                                  ((ev, mat) : acc) rest

-- | The live add-tile handler's own pre-check: replay is silent on an
--   out-of-range column, a live commit is not.
outOfColumnRange ∷ LoadedChunk → Int → Bool
outOfColumnRange lc idx
    | idx < 0 ∨ idx ≥ VU.length (lcTerrainSurfaceMap lc) = True
    | otherwise =
        let oldTopZ = lcTerrainSurfaceMap lc VU.! idx
            col     = lcTiles lc V.! idx
            i       = oldTopZ + 1 - ctStartZ col
        in i < 0 ∨ i > VU.length (ctMats col)

-- | Every chunk a result touched, whether or not it receives stone.
reactionChunks ∷ ReactionResult → [ChunkCoord]
reactionChunks = map fst . rrParticipants

-- | The global tile one event names, from its chunk key and local index.
reactionEventTile ∷ SolidificationEvent → (Int, Int)
reactionEventTile ev =
    chunkToGlobal (sevChunk ev) (sevIndex ev `mod` chunkSize)
                                (sevIndex ev `div` chunkSize)

-- | What one commit has accumulated so far: per chunk, the local cells
--   that became stone, in application order.
type Solidified = HM.HashMap ChunkCoord [Int]

-- | Commit every admitted result: the stone, the durable edits, the one
--   generation advance, the sim handoff, and both live presentations.
--
--   Takes results ALREADY admitted by 'admitReaction', with each event's
--   material resolved, so nothing here can decide to skip an event: the
--   caller has applied this delivery's writebacks on the strength of that
--   decision, and an event dropped now would leave the annihilation
--   recorded with no stone.
--
--   Runs inside 'World.Thread.Command.handleApplyFluidsCommandWith's
--   @try@, so a raise here acknowledges the delivery as a FAILURE before
--   the exception leaves the handler (#2334) — the fast-settle waiter is
--   released either way, and success is only ever reported once every
--   edit below has landed (requirement 8).
commitReactions ∷ EngineEnv → LoggerState → WorldPageId → WorldState
                → [(ReactionResult, [(SolidificationEvent, MaterialId)])]
                → IO ()
commitReactions env logger pageId ws admitted
    | null admitted = pure ()
    | otherwise = do
        solidified ← foldM (commitEvent logger ws) HM.empty
                           (concatMap snd admitted)
        let touched = [ (cc, reverse is) | (cc, is) ← HM.toList solidified ]
        when (not (null touched)) $
            publishCommit env logger pageId ws (map fst admitted) touched

-- | Apply one admitted event. Every event is applied against the chunk
--   as its siblings have left it — the tiles are re-read per event — so
--   two events in one chunk both land.
commitEvent ∷ LoggerState → WorldState → Solidified
            → (SolidificationEvent, MaterialId) → IO Solidified
commitEvent logger ws acc (ev, mat) = do
    td ← readIORef (wsTilesRef ws)
    case lookupChunk (sevChunk ev) td of
        -- Unreachable: 'admitReaction' rehearsed this exact edit against
        -- an overlay of these tiles, and the world thread is the only
        -- writer, so nothing can have removed the chunk in between.
        -- Raising rather than skipping is the point — a skip here is the
        -- partial commit admission exists to prevent, and the delivery's
        -- own writebacks have already landed.
        Nothing → error (T.unpack (unreachable "chunk vanished"))
        Just lc
            | outOfColumnRange lc (sevIndex ev) →
                error (T.unpack (unreachable "column went out of range"))
            | otherwise → do
                -- The same terrain-edit semantics as
                -- 'World.Thread.Command.Edit.Terrain.handleWorldAddTileCommand':
                -- one 'applyEdit', the flora forget that goes with it
                -- (#1854), and the append to the durable log. What is NOT
                -- here is that handler's per-edit 'syncEditToSim' — the
                -- generation advance and the sim handoff are deferred to
                -- 'publishCommit' so a sibling event cannot be staled by
                -- this one (requirement 5).
                let lc' = applyEdit (WeAddTile gx gy mat) lc
                replaceChunkForgettingFlora ws lc lc'
                atomicModifyIORef' (wsEditsRef ws) $ \es →
                    (appendEdit (sevChunk ev) (WeAddTile gx gy mat) es, ())
                logDebug logger CatWorld $
                    "Solidified " <> tshow (gx, gy) <> " to "
                    <> tshow (sevProduct ev) <> " (mat=" <> tshow mat
                    <> ", water=" <> tshow (sevWaterType ev) <> ")"
                pure $ HM.insertWith (⧺) (sevChunk ev) [sevIndex ev] acc
  where
    (gx, gy) = reactionEventTile ev
    unreachable why =
        "solidification commit reached an impossible state at "
        <> tshow (gx, gy) <> ": " <> why
        <> " between admission and application"

-- | Everything that happens ONCE, after every admitted event has been
--   applied: the generation advance, the sim handoff that keeps the
--   reaction's exact active volumes, the designation revalidation, the
--   unit re-ground, and the two live presentations.
publishCommit ∷ EngineEnv → LoggerState → WorldPageId → WorldState
              → [ReactionResult] → [(ChunkCoord, [Int])] → IO ()
publishCommit env logger pageId ws results touched = do
    -- One bump per EDITED chunk, however many of its events landed. A
    -- participant that received no stone keeps the generation it has:
    -- nothing about it changed, and bumping it would fence out its own
    -- in-flight writebacks for no reason.
    let editedChunks = map fst touched
        participants = nub (concatMap reactionChunks results)
    gens ← atomicModifyIORef' (wsChunkEditGenRef ws) $ \g0 →
        let bumped = foldl' (\g cc → HM.insert cc (HM.lookupDefault 0 cc g + 1) g)
                            g0 editedChunks
        in (bumped, bumped)
    td ← readIORef (wsTilesRef ws)
    let solidOf cc = HM.lookupDefault [] cc (HM.fromList touched)
        syncs = [ ReactionChunkSync
                    { rcsCoord      = cc
                    , rcsEditGen    = HM.lookupDefault 0 cc gens
                    , rcsFluid      = lcFluidMap lc
                    , rcsTerrain    = lcTerrainSurfaceMap lc
                    , rcsSolidified = solidOf cc
                    }
                | cc ← participants
                , Just lc ← [lookupChunk cc td] ]
    topo ← pageSimTopology ws
    epoch ← pageIncarnation ws
    Q.writeQueue (wsSimQueue (toWorldSimCapability env))
        (SimReactionCommitted pageId epoch topo syncs)

    -- The detailed tile render rebuilds its quads from the chunk the
    -- edits replaced, so dropping the caches is all it needs.
    bumpQuadCacheGen ws
    writeIORef (wsZoomQuadCacheRef ws) Nothing
    writeIORef (wsBgQuadCacheRef ws)   Nothing
    -- The zoom map does NOT: its renderer samples precomputed pixels.
    refreshZoomTerrain env logger pageId ws editedChunks

    -- #1858 / #1844, scoped to the tiles whose inputs moved, exactly as
    -- the live add-tile handler scopes them.
    let tiles = [ reactionEventTile ev | rr ← results, ev ← rrEvents rr ]
    _ ← revalidatePlantDesignations logger ws
    _ ← revalidateConstructDesignations env logger ws (ConstructKeys tiles)
    -- Units standing on a solidified tile ride up with it.
    forM_ tiles $ \(gx, gy) →
        Q.writeQueue (unitQueue env) (UnitReGround pageId gx gy)
    logDebug logger CatWorld $
        "Committed " <> tshow (length results) <> " reaction result(s), "
        <> tshow (length tiles) <> " stone tile(s)"

-- | Bring the sim back to the authoritative world state for every chunk
--   of a result this page refused (requirement 4).
--
--   A rejected result's own writebacks are quarantined by the caller, so
--   the tiles still hold the PRE-reaction fluid — including the lava the
--   discarded contact consumed. Re-seeding from them is what puts that
--   lava back in the sim's live grid; without it the lava chunk would
--   keep a hole the world never agreed to and no stone to account for
--   it, because only the EDITED chunk is re-seeded by an ordinary edit
--   and a rejected reaction edits nothing.
--
--   'syncEditToSim' is reused verbatim rather than re-implemented: it
--   bumps the chunk's generation before publishing the re-seed, which is
--   also what fences any sim output still in flight from the state this
--   page just refused.
convergeRejectedReactions ∷ EngineEnv → LoggerState → WorldPageId
                          → WorldState → [ReactionResult] → IO ()
convergeRejectedReactions env logger pageId ws rejected
    | null rejected = pure ()
    | otherwise = do
        td ← readIORef (wsTilesRef ws)
        let coords = nub (concatMap reactionChunks rejected)
        forM_ coords $ \cc → case lookupChunk cc td of
            Nothing → logDebug logger CatWorld $
                "Stale reaction convergence skipped: chunk " <> tshow cc
                <> " is not loaded"
            Just lc → syncEditToSim (toWorldSimCapability env) pageId ws lc
        logDebug logger CatWorld $
            "Rejected " <> tshow (length rejected)
            <> " stale reaction result(s); re-seeded "
            <> tshow (length coords) <> " chunk(s) from the live tiles"
