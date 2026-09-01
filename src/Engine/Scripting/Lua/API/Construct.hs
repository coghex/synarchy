{-# LANGUAGE Strict #-}
-- | Lua API for the construction-designation tool (issue #95) — the
--   @construction.*@ namespace. Mirrors the mine-designation API on
--   @world.*@: the tool drives setAnchor / clearAnchor / designate, the
--   build AI (#96) drives getPendingJobs / nearestDesignation /
--   setJobStatus, and the HUD sets the ghost textures.
module Engine.Scripting.Lua.API.Construct
    ( constructSetAnchorFn
    , constructClearAnchorFn
    , constructDesignateFn
    , constructCancelDesignationFn
    , constructGetPendingJobsFn
    , constructGetDesignationAtFn
    , constructCancelDesignationForRefundFn
    , constructGetDesignationCountFn
    , constructNearestDesignationFn
    , constructSetJobStatusFn
    , constructAddJobProgressFn
    , constructBeginPlacementFn
    , constructAbortPlacementFn
    , constructResolvePlanFn
    , constructSetDesignateTextureFn
    , constructSetLineModeFn
    , AttemptArg(..)
    , readAttemptArg
    , requiredAttempt
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef)
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State
    (EngineEnv, activeWorldPageFrom, activeWorldStateFrom)
import World.Construct.Plan
    ( PlanOp(..), PlanResult(..), PlanWorld(..), planOutcomeName
    , resolveStructurePlan )
import Engine.Asset.Handle (TextureHandle(..))
import World.Construct.Attempt (ConstructAttemptId(..))
import Structure.Types (StructureCommitWindow(..), StructureStageToken(..))
import World.Construct.Receipt (receiptEntries)
import World.Types
    (WorldManager(..), WorldState(..), pageWrapWorldSize, selectionMovedSince)
import World.Page.Types (WorldPageId(..))
import World.Chunk.Types (chunkSize)
import World.Generate.Coordinates
    ( globalToChunk, canonicalTile, seamTileDist2, chunkInSeamRegion
    , localizeTileToAnchor )
import World.Command.Types (WorldCommand(..))
import World.Construct.Types
import World.Thread.Command.Cursor.Construct
    ( abortConstructPlacement, beginConstructPlacement
    , popConstructDesignation )

-- | construction.setAnchor(pageId, gx, gy) — first-click anchor.
constructSetAnchorFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructSetAnchorFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tonumber 2
    gyArg     ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetConstructAnchor pageId (round gx) (round gy)
        _ → pure ()
    return 0

-- | construction.clearAnchor(pageId) — cancel the pending rectangle.
constructClearAnchorFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructClearAnchorFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $ WorldClearConstructAnchor pageId
        _ → pure ()
    return 0

-- | construction.designate(pageId, x1, y1, x2, y2, category, a, b, c
--   [, bindGen]) — commits the rectangle for a build target:
--     * category "structure": a=pack, b=kind ("wall"/"floor"/"ceiling"/
--       "post"), c=wall edge ("ne"/"nw"/"se"/"sw"; nil for non-walls)
--     * category "building":  a=building def name (rest ignored)
--   Unknown categories are ignored. A building only marks the anchor.
--
--   Returns whether the designation was ACCEPTED. #1602: @bindGen@
--   (slot 10) is the page-selection generation @world.pickTile@ reported
--   for the click this designation commits. When present it is compared
--   against 'wmSelectionGen' in ONE manager read taken immediately
--   before the command is enqueued; a mismatch — the selection moved, or
--   an effective change is enqueued and not yet applied — enqueues
--   nothing at all and returns false, which is the synchronous answer
--   the build tool turns into its rejected outcome.
--
--   The generation ALSO travels on the command, and the world thread
--   re-checks it before writing anything. That second check is the
--   authoritative one and it is exact rather than best-effort: the world
--   thread is where world.show / world.hide are applied too, so a
--   selection change enqueued before this designation is already applied
--   when the check runs, and one enqueued after is genuinely after the
--   commit. A stale click therefore creates no designation on the
--   captured page NOR on the newly selected one, even if selection moved
--   while the command sat in the queue.
--
--   Omitted (every AI and structure caller) → no check at either point,
--   enqueue as before.
constructDesignateFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructDesignateFn wsc = do
    pageIdArg ← Lua.tostring 1
    x1Arg ← Lua.tonumber 2
    y1Arg ← Lua.tonumber 3
    x2Arg ← Lua.tonumber 4
    y2Arg ← Lua.tonumber 5
    catArg ← Lua.tostring 6
    aArg ← Lua.tostring 7
    bArg ← Lua.tostring 8
    cArg ← Lua.tostring 9
    bindArg ← Lua.tointeger 10
    committed ← case (pageIdArg, x1Arg, y1Arg, x2Arg, y2Arg, catArg) of
        (Just pageIdBS, Just x1, Just y1, Just x2, Just y2, Just catBS) →
            case mkTarget (TE.decodeUtf8Lenient catBS) aArg bArg cArg of
                Nothing → pure False
                Just tgt → Lua.liftIO $ do
                    let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                    stale ← case bindArg of
                        Nothing   → pure False
                        Just want → do
                            wm ← readIORef (wsWorldManagerRef wsc)
                            -- The same predicate building.canPlaceAt
                            -- uses: the selection has moved, or an
                            -- EFFECTIVE change is enqueued and simply
                            -- not applied yet.
                            pure (selectionMovedSince
                                      (fromIntegral want) wm)
                    if stale then pure False else do
                        -- The binding travels WITH the command. The check
                        -- above is the SYNCHRONOUS answer this call owes
                        -- its caller (the build tool records its rejection
                        -- from it and stays armed); the copy carried here
                        -- is what the world thread re-checks at the actual
                        -- commit, where it is exactly serialized against
                        -- world.show / world.hide.
                        Q.writeQueue (wsWorldQueue wsc) $
                            WorldDesignateConstruct pageId
                                (round x1) (round y1) (round x2) (round y2) tgt
                                (fromIntegral <$> bindArg)
                        pure True
        _ → pure False
    Lua.pushboolean committed
    return 1
  where
    mkTarget "structure" (Just packBS) (Just kindBS) edge =
        Just $ CtStructure $ StructurePiece
            (TE.decodeUtf8Lenient packBS) (TE.decodeUtf8Lenient kindBS)
            (TE.decodeUtf8Lenient <$> edge)
    mkTarget "building" (Just defBS) _ _ =
        Just $ CtBuilding (TE.decodeUtf8Lenient defBS)
    mkTarget _ _ _ _ = Nothing

-- | construction.cancelDesignation(gx, gy[, attempt]) — remove the
--   designation at a tile on the active world. Returns nothing
--   (best-effort).
--
--   #1844: @attempt@ is the designation attempt the caller OBSERVED.
--   When present the removal applies only if the stored attempt still
--   matches, so a delayed cancellation from a job that has already gone
--   cannot remove a successor designated at the same tile. Omitting it
--   is the coordinate-only erase — "remove whatever is here" — which is
--   what the player's right-click means and what the AI must never use.
constructCancelDesignationFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructCancelDesignationFn wsc = do
    gxArg ← Lua.tonumber 1
    gyArg ← Lua.tonumber 2
    attArg ← readAttemptArg 3
    -- A SUPPLIED but malformed attempt enqueues nothing: falling
    -- through to the coordinate-only form would let it remove and refund
    -- a successor at the tile.
    case (gxArg, gyArg, cancelAttempt attArg) of
        (Just gx, Just gy, Just mAttempt) → do
            mPage ← Lua.liftIO $ activeWorldPageFrom (wsWorldManagerRef wsc)
            case mPage of
                Just (pageId, _) → Lua.liftIO $
                    Q.writeQueue (wsWorldQueue wsc) $
                        WorldCancelConstruct pageId (round gx) (round gy)
                            mAttempt
                Nothing → pure ()
        _ → pure ()
    return 0

-- | What a caller put in an attempt-id slot.
--
--   THREE answers, not two. The cancellation verbs have a legitimate
--   coordinate-only form — the player's right-click erases whatever is
--   at a tile — so "omitted" has to mean something. But a supplied value
--   that is not an identity must not collapse into it: a malformed or
--   stale attempt reaching the unguarded path would let it remove and
--   refund a SUCCESSOR, which is the exact confusion attempt identity
--   exists to prevent. So it is refused instead.
data AttemptArg
    = AttemptOmitted
      -- ^ Nothing was supplied (or an explicit nil). Only the
      --   cancellation verbs accept this, as the player's erase.
    | AttemptInvalid
      -- ^ Something was supplied and it is not an attempt id. Every
      --   verb refuses it, the cancellations included.
    | AttemptGiven !ConstructAttemptId
    deriving (Show, Eq)

-- | Read an attempt-id slot.
--
--   A present value must be a real, POSITIVE integer: 'Lua.tointeger'
--   alone would coerce a numeric STRING into a number, and an attempt id
--   arriving as a string is a caller bug rather than a value. Ids start
--   at 1, so 0 — the Lua-side "no attempt" sentinel — and every negative
--   are INVALID rather than absent.
readAttemptArg ∷ Lua.StackIndex → Lua.LuaE Lua.Exception AttemptArg
readAttemptArg ix = do
    ty ← Lua.ltype ix
    case ty of
        Lua.TypeNone   → pure AttemptOmitted
        Lua.TypeNil    → pure AttemptOmitted
        Lua.TypeNumber → do
            mN ← Lua.tointeger ix
            pure $ case mN of
                Just n | n > 0 →
                    AttemptGiven (ConstructAttemptId (fromIntegral n))
                _ → AttemptInvalid
        _ → pure AttemptInvalid

-- | The exact attempt a REQUIRED slot names, or 'Nothing' for anything
--   else — where the identity is mandatory an omitted slot is as
--   unusable as a malformed one.
requiredAttempt ∷ AttemptArg → Maybe ConstructAttemptId
requiredAttempt (AttemptGiven a) = Just a
requiredAttempt _                = Nothing

-- | How a CANCELLATION reads one: an omitted slot is the player's
--   coordinate-only erase, a valid one narrows to that exact attempt,
--   and a malformed one is refused outright (the outer 'Nothing').
cancelAttempt ∷ AttemptArg → Maybe (Maybe ConstructAttemptId)
cancelAttempt AttemptOmitted   = Just Nothing
cancelAttempt (AttemptGiven a) = Just (Just a)
cancelAttempt AttemptInvalid   = Nothing

-- | construction.getPendingJobs(cx1, cy1, cx2, cy2) → array of jobs in
--   the chunk region on the active world. Each job:
--     { x, y, z, category, status, progress,
--       lx, ly                -- see below
--       pack, kind, edge      -- structure targets
--       building              -- building targets }
--
--   @x@/@y@ are the CANONICAL stored key — what every other
--   @construction.*@ verb reports and accepts (#1175). @lx@/@ly@ are the
--   SAME tile re-expressed in the u-alias frame local to this region's
--   own centre, which is the scanning worker's neighbourhood: measure
--   distances with those. A job across the seam is physically adjacent
--   yet a whole world away in canonical numbers, so a caller that
--   range-gated on @x@/@y@ would reject every one of them and no worker
--   would ever claim a seam-side job. Identical to @x@/@y@ away from the
--   seam, and in arena / non-wrapping worlds.
--
--   The build AI (#96) reads this to find work. Jobs a worker has
--   claimed ARE included, carrying status "claimed" — the AI filters on
--   status when looking for fresh work (so a second worker still can't
--   re-claim an owned tile) and uses the claimed entries to release
--   stale claims (dead/vanished claimant) back to "pending" on timeout.
constructGetPendingJobsFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructGetPendingJobsFn wsc = do
    cx1Arg ← Lua.tonumber 1
    cy1Arg ← Lua.tonumber 2
    cx2Arg ← Lua.tonumber 3
    cy2Arg ← Lua.tonumber 4
    mWs ← Lua.liftIO $ activeWorldStateFrom (wsWorldManagerRef wsc)
    case (mWs, cx1Arg, cy1Arg, cx2Arg, cy2Arg) of
        (Just ws, Just cx1, Just cy1, Just cx2, Just cy2) → do
            m ← Lua.liftIO $ readIORef (wsConstructDesignationsRef ws)
            worldSize ← Lua.liftIO $ pageWrapWorldSize ws
            -- #1175: the caller's region is a raw box stepped outward
            -- from a worker's own chunk, so at the seam it names ALIASES
            -- of the canonical keys this map holds. Containment counts
            -- those aliases; identity away from the seam. The coords the
            -- jobs report stay canonical, which is what every point verb
            -- below then accepts.
            let inRegion (gx, gy) =
                    let (coord, _) = globalToChunk gx gy
                    in chunkInSeamRegion worldSize
                           (round cx1, round cy1) (round cx2, round cy2) coord
                -- Claimed jobs stay in the list WITH their status (#96):
                -- consumers filter status == "pending" when scanning for
                -- fresh work, and the AI's stale-claim sweep needs to see
                -- claimed entries to release an expired/dead claimant's
                -- job back to pending (acceptance: getPendingJobs shows
                -- "claimed" while in progress, "pending" after release).
                jobs = [ kv | kv@(k, _) ← HM.toList m, inRegion k ]
                -- The region is stepped outward from the worker's own
                -- chunk, so its centre IS that worker's frame.
                centreTile =
                    ( ((round cx1 + round cx2) `div` 2) * chunkSize
                          + chunkSize `div` 2
                    , ((round cy1 + round cy2) `div` 2) * chunkSize
                          + chunkSize `div` 2 )
            Lua.newtable
            forM_ (zip [1 ∷ Int ..] jobs) $ \(i, ((gx, gy), cd)) → do
                pushJobTable gx gy cd
                let (lx, ly) = localizeTileToAnchor worldSize centreTile (gx, gy)
                Lua.pushinteger (fromIntegral lx)
                Lua.setfield (Lua.nth 2) "lx"
                Lua.pushinteger (fromIntegral ly)
                Lua.setfield (Lua.nth 2) "ly"
                Lua.rawseti (Lua.nth 2) (fromIntegral i)
            return 1
        _ → Lua.pushnil >> return 1

-- | construction.getDesignationAt(pageId, gx, gy) → job table | nil.
--   Accepts any u-alias of the tile and reports the CANONICAL stored
--   coords (#1175); identity away from the seam.
constructGetDesignationAtFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructGetDesignationAtFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsConstructDesignationsRef ws)
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    let (gx, gy) = canonicalTile worldSize (round gxN) (round gyN)
                    case HM.lookup (gx, gy) m of
                        Just cd → pushJobTable gx gy cd >> return 1
                        Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | construction.cancelDesignationForRefund(pageId, gx, gy) → job table
--   | nil. Synchronous, ATOMIC pop-and-return (#799):
--   unlike cancelDesignation (fire-and-forget, queued on the world
--   thread), this removes the designation and returns its final state
--   in ONE atomic step, so a caller computing a materials refund from
--   the returned job's 'paid' field never races a second caller over
--   the SAME entry — a rapid double right-click on one designation, or
--   a cancel racing the build AI's own completion removal. (A new
--   designation is no longer such a racer: since #1595 admission
--   refuses a tile that already carries a job instead of replacing it.)
--   See 'World.Thread.Command.Cursor.Construct.popConstructDesignation'.
--
--   #1844: the returned job carries its @receipt@, and only the ONE
--   caller whose delete won gets it — which is what makes a refund
--   happen exactly once. The optional 4th argument narrows the pop to
--   one exact attempt; omitting it is the player's "remove whatever is
--   here" erase.
constructCancelDesignationForRefundFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructCancelDesignationForRefundFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    attArg ← readAttemptArg 4
    case (pageIdArg, gxArg, gyArg, cancelAttempt attArg) of
        (Just pageIdBS, Just gxN, Just gyN, Just mAttempt) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                gxN' = round gxN ∷ Int
                gyN' = round gyN ∷ Int
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    -- #1175: 'popConstructDesignation' canonicalises the
                    -- key itself (shared with the queued cancel), so the
                    -- refund's returned job must report that same frame.
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    let (gx, gy) = canonicalTile worldSize gxN' gyN'
                    mCd ← Lua.liftIO $
                        popConstructDesignation ws (gxN', gyN') mAttempt
                    case mCd of
                        Just cd → pushJobTable gx gy cd >> return 1
                        Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | construction.getDesignationCount(pageId) → n.
constructGetDesignationCountFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructGetDesignationCountFn wsc = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsConstructDesignationsRef ws)
                    Lua.pushinteger (fromIntegral (HM.size m))
                    return 1
                Nothing → Lua.pushinteger 0 >> return 1
        _ → Lua.pushinteger 0 >> return 1

-- | construction.nearestDesignation(pageId, x, y) → gx, gy, dist | nil.
--   Nearest designated tile by Euclidean distance — the build AI's
--   "distance to nearest build job" term. Mirrors nearestMineDesignation.
constructNearestDesignationFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructNearestDesignationFn wsc = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                ux = realToFrac x ∷ Float
                uy = realToFrac y ∷ Float
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsConstructDesignationsRef ws)
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    let dist2 = seamTileDist2 worldSize (ux, uy)
                        best = foldl' (\acc k → case acc of
                                  Nothing → Just (k, dist2 k)
                                  Just (_, d) | dist2 k < d → Just (k, dist2 k)
                                  _ → acc)
                                Nothing (HM.keys m)
                    case best of
                        Just ((gx, gy), d2) → do
                            Lua.pushinteger (fromIntegral gx)
                            Lua.pushinteger (fromIntegral gy)
                            Lua.pushnumber (Lua.Number (realToFrac (sqrt d2)))
                            return 3
                        Nothing → Lua.pushnil >> return 1
                Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | construction.setJobStatus(pageId, gx, gy, status, attempt
--   [, fromToken, toToken]) — build
--   AI marks a job "claimed" / "complete" (complete removes the
--   designation). Unknown status strings are ignored.
--
--   #1844: @attempt@ is REQUIRED and guards the write. A completion for
--   a job that is gone must not delete the successor at its tile, and
--   unlike cancellation there is no honest coordinate-only form of a
--   status transition — it is always some worker reporting on the job it
--   observed. A call without one enqueues nothing at all.
--
--   A COMPLETION may additionally carry the
--   'Structure.Types.StructureCommitWindow' of the placement it is
--   completing — @structure.stageWatermark@ read either side of the
--   placement run. @structure.place@ returning true means STAGED AND
--   QUEUED, not committed, so a completion without that window can
--   delete a paid designation for a placement the world thread went on
--   to decline. Given one, the world thread completes only if nothing in
--   the span was declined, and otherwise cancels the same attempt and
--   refunds its receipt.
constructSetJobStatusFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructSetJobStatusFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    statusArg ← Lua.tostring 4
    attArg ← readAttemptArg 5
    fromArg ← Lua.tointeger 6
    toArg   ← Lua.tointeger 7
    -- #1844: the placement's own commit window, read exactly as
    -- world.markLocationStamped reads its own (#2051). Supplying only
    -- one of the pair, or a pair that is not a forward range, carries NO
    -- window — a half-stated all-or-nothing claim must not silently read
    -- as "nothing to check" when the caller believed it had asked for
    -- the check. An EMPTY range is a real window: a completion that
    -- staged nothing has nothing that can have been declined.
    let mWindow = case (fromArg, toArg) of
            (Just lo, Just hi) | lo ≥ 0, hi ≥ lo →
                Just (StructureCommitWindow
                        (StructureStageToken (fromIntegral lo))
                        (StructureStageToken (fromIntegral hi)))
            _ → Nothing
    -- #1844: no attempt, no command. A status transition is always a
    -- worker reporting on the job it observed, so an attempt-less call
    -- is a caller bug and enqueuing it would let a stale completion
    -- delete a successor at the tile.
    case (pageIdArg, gxArg, gyArg, statusArg, requiredAttempt attArg) of
        (Just pageIdBS, Just gx, Just gy, Just statusBS, Just attempt) →
            case textToConstructStatus (TE.decodeUtf8Lenient statusBS) of
                Just st → Lua.liftIO $ do
                    let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                    Q.writeQueue (wsWorldQueue wsc) $
                        WorldSetConstructStatus pageId (round gx) (round gy)
                            st attempt mWindow
                Nothing → pure ()
        _ → pure ()
    return 0

-- | construction.addJobProgress(pageId, gx, gy, delta) — build AI (#96)
--   pours work into a designation. delta is normalised to the job's
--   total (1.0 = done); the engine clamps the sum to [0, 1]. The AI
--   watches getDesignationAt().progress and finishes the job itself
--   (place piece, then setJobStatus "complete").
constructAddJobProgressFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructAddJobProgressFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    deltaArg ← Lua.tonumber 4
    attArg ← readAttemptArg 5
    -- #1844: no attempt, no command — see setJobStatus above.
    case (pageIdArg, gxArg, gyArg, deltaArg, requiredAttempt attArg) of
        (Just pageIdBS, Just gx, Just gy, Just delta, Just attempt) →
            Lua.liftIO $ do
                let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                Q.writeQueue (wsWorldQueue wsc) $
                    WorldAddConstructProgress pageId (round gx) (round gy)
                        (realToFrac delta) attempt
        _ → pure ()
    return 0

-- | @construction.abortPlacement(pageId, gx, gy, attempt) → job | nil@ —
--   give up a placement hand-off this claimant took but could not use
--   (#1844), returning the removed designation so its receipt can be
--   refunded.
--
--   The mirror of @beginPlacement@, and the only way out of the hand-off
--   other than completing it. Ordinary cancellation is refused while a
--   designation is @placing@ — it would refund a receipt while the
--   claimant's queued placement still lands — but the claimant is the
--   OWNER of that window, and it alone can know that its
--   @structure.place@ staged nothing at all (the target chunk having
--   evicted between the final resolver check and the placement).
--
--   Restricted to a @placing@ designation with a matching attempt, so it
--   is not a second general cancel: a job that never took the hand-off,
--   and a successor at the same tile, are untouched.
constructAbortPlacementFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructAbortPlacementFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    attArg ← readAttemptArg 4
    case (pageIdArg, gxArg, gyArg, requiredAttempt attArg) of
        (Just pageIdBS, Just gxN, Just gyN, Just attempt) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                gxN' = round gxN ∷ Int
                gyN' = round gyN ∷ Int
            mgr ← Lua.liftIO $ readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil ≫ return 1
                Just ws → do
                    worldSize ← Lua.liftIO $ pageWrapWorldSize ws
                    let (gx, gy) = canonicalTile worldSize gxN' gyN'
                    mCd ← Lua.liftIO $
                        abortConstructPlacement ws (gxN', gyN') attempt
                    case mCd of
                        Just cd → pushJobTable gx gy cd ≫ return 1
                        Nothing → Lua.pushnil ≫ return 1
        _ → Lua.pushnil ≫ return 1

-- | @construction.resolvePlan(pageId, gx, gy, attempt) → outcome | nil@
--   — re-run the shared structure-plan resolver for ONE exact attempt
--   (#1844 requirement 10).
--
--   The worker has to ask this before it claims, before it pays and
--   before it places: a designation admitted minutes ago is not evidence
--   it is still buildable, and the world-side invalidator does not run
--   for every possible reason at every possible moment (a catalogue
--   failure sweeps, terrain publication sweeps, but nothing sweeps on
--   the tick the worker happens to arrive). Answering here is what makes
--   the worker's own view the SAME view the resolver has, rather than
--   the three ad-hoc checks it used to make.
--
--   Returns the outcome name — @"valid"@, @"visible-invalid"@,
--   @"missing-art"@ or @"unresolved-terrain"@ — resolved against the
--   designation's OWN captured @cdZ@ and with its own attempt excluded
--   from the occupancy check. nil when the page is gone, when nothing at
--   that tile carries this attempt (the job is gone; the AI already
--   treats that as a release), or when the target is a BUILDING, whose
--   planning is DTV-10's scope and which this resolver does not judge.
constructResolvePlanFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructResolvePlanFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    attArg ← readAttemptArg 4
    let wsc = toWorldSimCapability env
    mOutcome ← case (pageIdArg, gxArg, gyArg, requiredAttempt attArg) of
        (Just pageIdBS, Just gxN, Just gyN, Just attempt) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Nothing → pure Nothing
                Just ws → do
                    worldSize ← pageWrapWorldSize ws
                    let key = canonicalTile worldSize (round gxN) (round gyN)
                    designations ← readIORef (wsConstructDesignationsRef ws)
                    case HM.lookup key designations of
                        Just cd
                          | cdAttempt cd ≡ attempt
                          , CtStructure piece ← cdTarget cd → do
                              tiles ← readIORef (wsTilesRef ws)
                              stage ← readIORef (wsStructureStageRef ws)
                              cat ← readIORef (rhStructureArtCatalogRef
                                                 (toRenderHandoffCapability env))
                              let pw = PlanWorld
                                      { pwWorldSize    = worldSize
                                      , pwTiles        = tiles
                                      , pwStage        = stage
                                      , pwDesignations = designations
                                      , pwCatalog      = cat
                                      }
                              pure ∘ Just ∘ planOutcomeName ∘ prOutcome $
                                  resolveStructurePlan pw
                                      (PlanForAttempt attempt) (cdZ cd)
                                      piece key
                        _ → pure Nothing
        _ → pure Nothing
    case mOutcome of
        Nothing → Lua.pushnil ≫ return 1
        Just o  → Lua.pushstring (TE.encodeUtf8 o) ≫ return 1

-- | @construction.beginPlacement(pageId, gx, gy, attempt) → bool@ — take
--   the final-placement hand-off for ONE exact attempt (#1844
--   requirement 18).
--
--   The worker used to place its piece and then, as a SEPARATE
--   operation, mark the job complete — with the designation still live
--   in between and its own read-your-writes placement already visible.
--   An occupancy-checking invalidator running in that window would see
--   the worker's own success as an external conflict, cancel the job and
--   refund materials that were correctly spent.
--
--   Taking the hand-off moves the designation to @placing@, which
--   revalidation skips entirely. The whole place→complete sequence runs
--   inside one Lua callback, so the window cannot span a tick. A false
--   return means the attempt is gone — cancelled, completed, or replaced
--   — and the caller must place NOTHING.
constructBeginPlacementFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructBeginPlacementFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    attArg ← readAttemptArg 4
    ok ← case (pageIdArg, gxArg, gyArg, requiredAttempt attArg) of
        (Just pageIdBS, Just gx, Just gy, Just attempt) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Just ws → beginConstructPlacement ws (round gx, round gy) attempt
                Nothing → pure False
        _ → pure False
    Lua.pushboolean ok
    return 1

-- | construction.setDesignateTexture(pageId, category, texHandle) — ghost
--   texture for committed designations, keyed by category ("structure" |
--   "building").
constructSetDesignateTextureFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructSetDesignateTextureFn wsc = do
    pageIdArg ← Lua.tostring 1
    catArg ← Lua.tostring 2
    handleArg ← Lua.tointeger 3
    case (pageIdArg, catArg, handleArg) of
        (Just pageIdBS, Just catBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetConstructDesignateTexture pageId
                    (TE.decodeUtf8Lenient catBS) texHandle
        _ → pure ()
    return 0

-- | construction.setLineMode(pageId, enabled) — wire path tool (#359):
--   while enabled, the anchor→hover preview (World/Render/CursorQuads.hs)
--   snaps to a straight 1-wide line along whichever axis has the larger extent
--   from the anchor, instead of the default filled rectangle. The build
--   tool's commit (scripts/build_tool.lua) snaps the SAME way before
--   calling designate, so the committed tiles always match what
--   previewed.
constructSetLineModeFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructSetLineModeFn wsc = do
    pageIdArg ← Lua.tostring 1
    enabledArg ← Lua.toboolean 2
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldSetConstructLineMode pageId enabledArg
        Nothing → pure ()
    return 0

-- | Push one designation as a Lua table onto the stack.
pushJobTable ∷ Int → Int → ConstructDesignation
             → Lua.LuaE Lua.Exception ()
pushJobTable gx gy cd = do
    let ConstructAttemptId aid = cdAttempt cd
    Lua.newtable
    Lua.pushinteger (fromIntegral gx)
    Lua.setfield (Lua.nth 2) "x"
    Lua.pushinteger (fromIntegral gy)
    Lua.setfield (Lua.nth 2) "y"
    Lua.pushinteger (fromIntegral (cdZ cd))
    Lua.setfield (Lua.nth 2) "z"
    Lua.pushstring (TE.encodeUtf8 (constructTargetCategory (cdTarget cd)))
    Lua.setfield (Lua.nth 2) "category"
    Lua.pushstring (TE.encodeUtf8 (constructStatusToText (cdStatus cd)))
    Lua.setfield (Lua.nth 2) "status"
    Lua.pushnumber (Lua.Number (realToFrac (cdProgress cd)))
    Lua.setfield (Lua.nth 2) "progress"
    -- #1844: the attempt identity every lifecycle verb takes back. A
    -- caller that keeps a job table across ticks is holding the exact
    -- attempt it observed, which is the whole point.
    Lua.pushinteger (fromIntegral aid)
    Lua.setfield (Lua.nth 2) "attempt"
    -- Paid state is READ from the payment record and nothing else — the
    -- receipt's presence IS the paid state (#1844 requirement 15), so
    -- this field can never disagree with 'receipt' below.
    Lua.pushboolean (constructDesignationPaid cd)
    Lua.setfield (Lua.nth 2) "paid"
    -- The exact multiset that was removed, in the receipt's own
    -- canonical order, as an ARRAY of { name = , count = } — a refund
    -- reads this and never re-reads pack metadata. Absent when unpaid.
    forM_ (constructDesignationReceipt cd) $ \receipt → do
        Lua.newtable
        forM_ (zip [1 ∷ Int ..] (receiptEntries receipt)) $ \(i, (name, n)) → do
            Lua.newtable
            Lua.pushstring (TE.encodeUtf8 name)
            Lua.setfield (Lua.nth 2) "name"
            Lua.pushinteger (fromIntegral n)
            Lua.setfield (Lua.nth 2) "count"
            Lua.rawseti (Lua.nth 2) (fromIntegral i)
        Lua.setfield (Lua.nth 2) "receipt"
    case cdTarget cd of
        CtStructure (StructurePiece pack kind edge) → do
            Lua.pushstring (TE.encodeUtf8 pack)
            Lua.setfield (Lua.nth 2) "pack"
            Lua.pushstring (TE.encodeUtf8 kind)
            Lua.setfield (Lua.nth 2) "kind"
            case edge of
                Just e → do
                    Lua.pushstring (TE.encodeUtf8 e)
                    Lua.setfield (Lua.nth 2) "edge"
                Nothing → pure ()
        CtBuilding def → do
            Lua.pushstring (TE.encodeUtf8 def)
            Lua.setfield (Lua.nth 2) "building"
