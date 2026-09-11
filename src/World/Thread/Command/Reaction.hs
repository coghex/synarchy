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
module World.Thread.Command.Reaction
    ( reactionIsFresh
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
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.Log
    (logDebug, logError, logWarn, LogCategory(..), LoggerState)
import Engine.Core.State (EngineEnv, unitQueue)
import Sim.Command.Types (SimCommand(..), ReactionChunkSync(..))
import Sim.Fluid.Reaction (ReactionResult(..), SolidificationEvent(..))
import Unit.Command.Types (UnitCommand(..))
import World.Chunk.Admit (pageIncarnation)
import World.Construct.Revalidate
    (ConstructScope(..), revalidateConstructDesignations)
import World.Edit.Apply (applyEdit)
import World.Edit.Types (WorldEdit(..), appendEdit)
import World.Flora.Designation (replaceChunkForgettingFlora)
import World.Generate.Coordinates (chunkToGlobal)
import World.Material (MaterialRegistry)
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
reactionIsFresh ∷ HM.HashMap ChunkCoord Word64 → ReactionResult → Bool
reactionIsFresh gens rr =
    all (\(cc, g) → g ≡ HM.lookupDefault 0 cc gens) (rrParticipants rr)

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
--   Runs inside 'World.Thread.Command.handleApplyFluidsCommandWith's
--   @try@, so a raise here acknowledges the delivery as a FAILURE before
--   the exception leaves the handler (#2334) — the fast-settle waiter is
--   released either way, and success is only ever reported once every
--   edit below has landed (requirement 8).
commitReactions ∷ EngineEnv → LoggerState → WorldPageId → WorldState
                → [ReactionResult] → IO ()
commitReactions env logger pageId ws results
    | null results = pure ()
    | otherwise = do
        registry ← readIORef (wsMaterialRegistryRef (toWorldSimCapability env))
        solidified ← foldM (commitOne logger ws registry) HM.empty results
        let touched = [ (cc, reverse is) | (cc, is) ← HM.toList solidified ]
        when (not (null touched)) $
            publishCommit env logger pageId ws results touched

-- | Apply one result's events. Every event is applied against the chunk
--   as its siblings have left it — the tiles are re-read per event — so
--   two events in one chunk both land.
commitOne ∷ LoggerState → WorldState → MaterialRegistry
          → Solidified → ReactionResult → IO Solidified
commitOne logger ws registry acc rr =
    foldM (commitEvent logger ws registry) acc (rrEvents rr)

commitEvent ∷ LoggerState → WorldState → MaterialRegistry
            → Solidified → SolidificationEvent → IO Solidified
commitEvent logger ws registry acc ev =
    case stoneMaterialFor registry (sevProduct ev) of
        -- Loud, and nothing is committed for this event: a stone the
        -- registry cannot name would leave the consumed lava with no
        -- product at all, which is worse than a visible failure.
        Left why → do
            logError logger CatWorld $
                "Solidification dropped at " <> tshow (gx, gy) <> ": " <> why
            pure acc
        Right mat → do
            td ← readIORef (wsTilesRef ws)
            case lookupChunk (sevChunk ev) td of
                Nothing → skip "chunk not loaded"
                Just lc
                    | outOfColumnRange lc → skip "out of column range"
                    | otherwise → do
                        -- The same terrain-edit semantics as
                        -- 'World.Thread.Command.Edit.Terrain.handleWorldAddTileCommand':
                        -- one 'applyEdit', the flora forget that goes
                        -- with it (#1854), and the append to the durable
                        -- log. What is NOT here is that handler's
                        -- per-edit 'syncEditToSim' — the generation
                        -- advance and the sim handoff are deferred to
                        -- 'publishCommit' so a sibling event cannot be
                        -- staled by this one (requirement 5).
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
    idx = sevIndex ev
    -- Mirrors the live add-tile handler's pre-check: replay is silent on
    -- an out-of-range column, a live commit says so.
    outOfColumnRange lc =
        let oldTopZ = lcTerrainSurfaceMap lc VU.! idx
            col     = lcTiles lc V.! idx
            i       = oldTopZ + 1 - ctStartZ col
        in idx < 0 ∨ idx ≥ VU.length (lcTerrainSurfaceMap lc)
           ∨ i < 0 ∨ i > VU.length (ctMats col)
    skip why = do
        logWarn logger CatWorld $
            "Solidification skipped at " <> tshow (gx, gy) <> ": " <> why
        pure acc

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
    refreshZoomTerrain env logger ws touched

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
