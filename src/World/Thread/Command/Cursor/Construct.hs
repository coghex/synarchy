-- | Construction designation tool (#95). Mirrors the mine designation
--   tool: an anchor→rectangle commit that stores per-tile designations
--   (build target + status + progress) in wsConstructDesignationsRef.
--   The build AI (#96) is the consumer. Split out of
--   "World.Thread.Command.Cursor" (issue #564).
--
--   #1844 moved STRUCTURE planning off the generic rectangle path:
--   candidates come from "World.Construct.Extent" (the same helper the
--   preview uses) and each is admitted only if "World.Construct.Plan"'s
--   resolver says @PlanValid@ at click time, whatever the preview
--   believed. BUILDING commits are deliberately unchanged — anchor-only,
--   @requested = 1@, and #1595's outstanding-designation refusal —
--   because building planning is DTV-10's scope.
--
--   Every lifecycle operation here is ATTEMPT-GUARDED (#1844
--   requirement 11): the caller names the designation attempt it
--   observed, and a mutation applies only when the stored attempt
--   matches. A delayed status change, progress pour, cancellation or
--   completion from a removed attempt is therefore a no-op against a
--   successor at the same canonical tile, rather than a silent
--   corruption of it.
module World.Thread.Command.Cursor.Construct
    ( handleWorldSetConstructAnchorCommand
    , handleWorldClearConstructAnchorCommand
    , handleWorldDesignateConstructCommand
    , handleWorldCancelConstructCommand
    , handleWorldSetConstructStatusCommand
    , handleWorldAddConstructProgressCommand
    , handleWorldSetConstructDesignateTextureCommand
    , handleWorldSetConstructLineModeCommand
    , popConstructDesignation
    , beginConstructPlacement
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, writeIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle)
import Engine.Core.Capability.RenderHandoff
    (RenderHandoffCapability(..), toRenderHandoffCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import World.Types
import World.Generate (globalToChunk)
import World.Generate.Coordinates (canonicalTile)
import World.Construct.Attempt (ConstructAttemptId, takeConstructAttempts)
import World.Construct.Extent (structureDragExtent)
import World.Construct.Plan
    ( PlanOp(..), PlanOutcome(..), PlanResult(..), PlanWorld(..)
    , planOutcomeName, planSurfaceZAt, resolveStructurePlan )
import World.Construct.Revalidate (constructPlanWorld)
import World.Construct.Types ( ConstructTarget(..), ConstructStatus(..)
                             , ConstructDesignation(..)
                             , newConstructDesignation
                             , constructTargetCategory )
import World.Plant.Validate (revalidatePlantDesignations)
import World.Construct.Apply (applyConstructSlopeToChunk)
import Structure.Types (StructureCommitWindow, takeDeclinedInWindow)
import World.Construct.Revalidate
    ( clearConstructDesignationSlope, constructRefundDeps
    , notifyConstructCompleted, notifyConstructInvalidated
    , refundConstructDesignation )
import World.Thread.Command.Cursor.Common
    (recordDesignationOutcome, recordMissingWorldOutcome)

handleWorldSetConstructAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → IO ()
handleWorldSetConstructAnchorCommand env _logger pageId gx gy = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- #1175: canonical anchor, rectangle formed in its frame.
            worldSize ← pageWrapWorldSize worldState
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructAnchor = Just (canonicalTile worldSize gx gy) }, ())
        Nothing → pure ()

handleWorldClearConstructAnchorCommand ∷ EngineEnv → LoggerState → WorldPageId
    → IO ()
handleWorldClearConstructAnchorCommand env _logger pageId = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructAnchor = Nothing }, ())
        Nothing → pure ()

-- | Commit a construction designation.
--
--   STRUCTURE targets enumerate 'structureDragExtent' — the ONE bounded
--   drag helper the anchor→hover preview also uses, in the anchor's own
--   alias frame and capped at 64 cells per axis INCLUDING the anchor —
--   and then re-resolve every candidate through
--   'resolveStructurePlan' at click time. Commit does not trust preview
--   state: the world can have moved since the ghost was drawn, and each
--   candidate is filtered independently, so partial acceptance is
--   unchanged. Only @PlanValid@ candidates land; visible-invalid and
--   missing-art candidates are omitted (requirement 8).
--
--   BUILDING targets mark only the anchor tile at its own surface z, one
--   footprint rather than a grid of them, and are subject to #1595's
--   outstanding-designation refusal and nothing else. Their @requested@
--   accounting stays 1 and their outcome coordinates stay the anchor's:
--   building planning and building invalidation are DTV-10's scope, and
--   this path is deliberately byte-identical to what it was.
--
--   Clears the anchor afterwards.
handleWorldDesignateConstructCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Int → Int → ConstructTarget → Maybe Word64 → IO ()
handleWorldDesignateConstructCommand env logger pageId gx1 gy1 gx2 gy2 tgt
                                     mBindGen = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    -- #1602: the page BINDING is re-checked here, not merely where the
    -- command was enqueued. This is the exact commit point for a
    -- designation, and it is EXACTLY serialized against page selection:
    -- world.show / world.hide are world-thread commands drained from the
    -- same queue, so a selection change enqueued before this designation
    -- has already been applied to the snapshot above, and one enqueued
    -- after is genuinely after the commit. A stale binding writes
    -- nothing at all — not on the captured page, not on the newly
    -- selected one. An unbound designation (every AI caller, and the
    -- two-click structure rectangle) is unaffected.
    let bindingMoved = maybe False (≢ wmSelectionGen mgr) mBindGen
    case (if bindingMoved then Nothing else lookup pageId (wmWorlds mgr)) of
        Nothing | bindingMoved →
            logDebug logger CatWorld $
                "Construct designation dropped: page binding stale on "
                <> unWorldPageId pageId
        Nothing → recordMissingWorldOutcome env "construction.designate"
            pageId gx1 gy1
        Just worldState → do
            worldSize ← pageWrapWorldSize worldState
            tileData ← readIORef (wsTilesRef worldState)
            stage ← readIORef (wsStructureStageRef worldState)
            designations ← readIORef (wsConstructDesignationsRef worldState)
            cat ← readIORef (rhStructureArtCatalogRef
                               (toRenderHandoffCapability env))
            cs ← readIORef (wsCursorRef worldState)
            let pw = PlanWorld
                    { pwWorldSize    = worldSize
                    , pwTiles        = tileData
                    , pwStage        = stage
                    , pwDesignations = designations
                    , pwCatalog      = cat
                    }
                -- The wire path tool's line mode is read from the PAGE's
                -- own cursor state, which is the very value the preview
                -- reads — not a second flag travelling on the command —
                -- so the two cannot disagree about the shape of the drag.
                lineMode = constructLineMode cs
                mAnchorZ = planSurfaceZAt worldSize tileData (gx1, gy1)
                extent = structureDragExtent worldSize lineMode
                             (gx1, gy1) (gx2, gy2)
                -- A building only ever targets its single anchor tile
                -- (never the swept rectangle), so it always "requests"
                -- exactly 1 regardless of the two-click rectangle size.
                requested = case tgt of
                    CtBuilding _  → 1
                    CtStructure _ → length extent
                -- Structure candidates carry the resolver's verdict, so
                -- the outcome reason below can name what actually
                -- refused them rather than guessing.
                resolved = case (tgt, mAnchorZ) of
                    (CtStructure piece, Just anchorZ) →
                        [ (canonicalTile worldSize gx gy, r)
                        | (gx, gy) ← extent
                        , let r = resolveStructurePlan pw PlanForPlacement
                                      anchorZ piece (gx, gy) ]
                    _ → []
                accepted = [ (k, cdZOf r) | (k, r) ← resolved
                                          , prOutcome r ≡ PlanValid ]
                cdZOf r = fromMaybe 0 (prSurfaceZ r)
                candidateZs = case (tgt, mAnchorZ) of
                    -- A building is a single footprint: only the anchor
                    -- tile, at its own surface z.
                    (CtBuilding _, Just anchorZ) →
                        [ (canonicalTile worldSize gx1 gy1, anchorZ) ]
                    (CtBuilding _, Nothing) → []   -- anchor chunk unloaded
                    _ → accepted
            -- Attempt ids are allocated in ONE atomic step, before the
            -- insert, and are never reissued — a candidate the insert
            -- then refuses simply burns its id. See
            -- "World.Construct.Attempt".
            attempts ← atomicModifyIORef' (wsConstructAttemptRef worldState) $
                \next → let (as, next') = takeConstructAttempts
                                              (length candidateZs) next
                        in (next', as)
            let candidates = [ (k, newConstructDesignation z tgt aid)
                             | ((k, z), aid) ← zip candidateZs attempts ]
                -- #1595: the map is keyed by tile coordinate alone, so a
                -- plain 'HM.insert' would REPLACE whatever job the tile
                -- already carries — silently discarding a claimed and
                -- possibly already-paid designation without the refund
                -- the cancel path performs for exactly that state.
                -- Admission therefore treats ANY existing entry as
                -- occupying the tile, whatever its status, progress,
                -- payment or target category. The structure resolver
                -- already refuses those tiles; this is the BUILDING
                -- path's own refusal, and the atomic backstop for both.
                addOne (m, n) (k, v)
                    | HM.member k m = (m, n)
                    | otherwise     = (HM.insert k v m, n + 1)
            -- The test-and-insert runs INSIDE the atomicModifyIORef' that
            -- publishes it: 'popConstructDesignation' and the synchronous
            -- Lua verbs mutate this same ref off the world thread, so a
            -- read-then-insert pair would be exactly the race the atomic
            -- delete exists to close.
            applied ← atomicModifyIORef' (wsConstructDesignationsRef worldState) $
                \m → foldl' addOne (m, 0 ∷ Int) candidates
            atomicModifyIORef' (wsCursorRef worldState) $ \cs' →
                (cs' { constructAnchor = Nothing }, ())
            logDebug logger CatWorld $
                "Construct designation: +" <> tshow applied
                <> " tiles (" <> constructTargetCategory tgt <> ")"
            let ((xLo, yLo), _) = extentBounds extent (gx1, gy1)
            recordDesignationOutcome env "construction.designate"
                (rejectionReason tgt resolved candidates applied)
                xLo yLo requested applied
  where
    -- The swept rectangle's low corner, for the F4 record. Derived from
    -- the extent itself rather than recomputed, so the two cannot drift;
    -- the extent is never empty (the anchor is always in it), and the
    -- anchor is the honest fallback if it somehow were.
    extentBounds [] anchor = (anchor, anchor)
    extentBounds tiles _   =
        ( (minimum (map fst tiles), minimum (map snd tiles))
        , (maximum (map fst tiles), maximum (map snd tiles)) )

    -- Nothing landed: say WHICH check refused, taking the resolver's own
    -- reason when there is one rather than blaming a check that for
    -- these tiles was never reached.
    rejectionReason (CtBuilding _) _ candidates applied
        | applied ≡ 0 ∧ not (null candidates) =
            "tile already carries an outstanding construction designation"
        | otherwise = "anchor tile ineligible or unloaded"
    -- The first candidate the resolver actually refused, which for a
    -- one-tile drag is the only one and for a rectangle is a
    -- representative rather than a summary. A resolved list with nothing
    -- refused cannot reach here: every entry would have been applied.
    rejectionReason (CtStructure _) resolved _ _ =
        case [ r | (_, r) ← resolved, prOutcome r ≢ PlanValid ] of
            (r : _) → planOutcomeName (prOutcome r) <> ": " <> prReason r
            []      → "anchor tile ineligible or unloaded"

-- | Remove the designation at a tile, REFUNDING its receipt.
--
--   The queued cancel is a real cancellation, not a bookkeeping delete:
--   @construction.cancelDesignation@ is a public verb the build AI calls
--   when a job cannot be finished, and popping a PAID designation
--   without spending its receipt would destroy materials that had
--   already left an inventory. The synchronous
--   @cancelDesignationForRefund@ hands its receipt to the Lua caller
--   instead; this path has no caller to hand it to, so it spends it
--   here — and because the atomic pop returns the designation to exactly
--   one caller, the refund still happens exactly once however many
--   cancellations race.
handleWorldCancelConstructCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Maybe ConstructAttemptId → IO ()
handleWorldCancelConstructCommand env _logger pageId gx gy mAttempt = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            mCd ← popConstructDesignation worldState (gx, gy) mAttempt
            forM_ mCd $ \cd → do
                deps ← constructRefundDeps env
                worldSize ← pageWrapWorldSize worldState
                -- #1175: the CANONICAL key throughout — the refund's
                -- position and the broadcast's coordinate alike. The Lua
                -- claim registry is keyed by the coords the AI reads
                -- back, which are canonical, so an alias-named cancel
                -- that broadcast its alias would leave the real claim
                -- standing and block a successor.
                let ckey = canonicalTile worldSize gx gy
                refundConstructDesignation deps worldState ckey cd
                -- …and detach the claimant, exactly as a world-side
                -- invalidation does. Without it this documented
                -- cancellation API leaves the old claim holding the
                -- tile until a decision tick or a timeout, blocking a
                -- successor designated there immediately — which is the
                -- very thing the exact-attempt contract promises.
                notifyConstructInvalidated env worldState ckey cd
        Nothing → pure ()

-- | Atomically remove a construction designation and reset its
--   corner-progress display, returning the removed designation if any
--   was present.
--
--   The atomicModifyIORef' delete is what serializes competing
--   cancellations (a rapid double right-click, a cancel racing the build
--   AI's own completion removal, a world-thread invalidation racing a
--   player click): whichever caller's delete runs first sees @Just@ the
--   removed designation; every other caller sees 'Nothing', since there
--   is nothing left to remove. That is also what makes a receipt refund
--   happen EXACTLY once — only the winner is handed the receipt.
--
--   #1844: @mAttempt@ makes the pop exact. 'Just' removes the
--   designation only when its attempt matches, so a delayed cancellation
--   for an attempt that has already gone cannot remove a SUCCESSOR at
--   the same tile. 'Nothing' is the player's coordinate-only erase —
--   "remove whatever is here", which has no attempt to name until it
--   looks, and is still exact because the pop returns the one attempt it
--   removed.
--
--   A designation inside its placement HAND-OFF ('CsPlacing') is not
--   poppable at all, whatever attempt is named. The claimant has by then
--   staged its piece and queued the world command that commits it;
--   removing the designation here would refund the receipt while that
--   command still lands, leaving the player with both the structure and
--   its materials back. Cancellation simply loses that race — the window
--   is one Lua callback wide, and the completion that follows settles
--   the attempt either way (committing it, or cancelling and refunding
--   it when the placement was declined or its site drifted).
--
--   A hand-off cannot strand the tile: 'World.Construct.Reconcile'
--   demotes a restored 'CsPlacing' to pending, and the AI's own
--   stale-claim sweep releases one whose claimant stopped refreshing.
--
--   #1175: the tile is canonicalised HERE, once, so every caller accepts
--   any u-alias and resolves the one stored key.
popConstructDesignation ∷ WorldState → (Int, Int)
                        → Maybe ConstructAttemptId
                        → IO (Maybe ConstructDesignation)
popConstructDesignation worldState (rawGX, rawGY) mAttempt = do
    worldSize ← pageWrapWorldSize worldState
    let key = canonicalTile worldSize rawGX rawGY
    mCd ← atomicModifyIORef' (wsConstructDesignationsRef worldState) $ \m →
        case HM.lookup key m of
            Just cd | maybe True (≡ cdAttempt cd) mAttempt
                    , cdStatus cd ≢ CsPlacing →
                (HM.delete key m, Just cd)
            _ → (m, Nothing)
    forM_ mCd $ clearConstructDesignationSlope worldState key
    pure mCd

-- | The final-placement hand-off (#1844 requirement 18): mark ONE exact
--   attempt as entering placement.
--
--   The worker used to queue the piece placement and @CsComplete@ as two
--   independent operations, with the designation still live between
--   them — and read-your-writes staging exposes the placed piece the
--   instant it is staged. A naive occupancy invalidator would see the
--   worker's OWN successful placement as an external conflict, cancel
--   the job and refund materials that were correctly spent.
--
--   'CsPlacing' closes that window without moving art resolution off the
--   Lua thread: revalidation skips a designation in this state, and the
--   whole placement→completion sequence runs inside ONE Lua callback, so
--   the window cannot span a tick. Returns whether the transition was
--   taken; 'False' means the attempt is gone (cancelled, completed, or
--   replaced) and the worker must not place anything.
beginConstructPlacement ∷ WorldState → (Int, Int) → ConstructAttemptId
                        → IO Bool
beginConstructPlacement worldState (rawGX, rawGY) attempt = do
    worldSize ← pageWrapWorldSize worldState
    let key = canonicalTile worldSize rawGX rawGY
    atomicModifyIORef' (wsConstructDesignationsRef worldState) $ \m →
        case HM.lookup key m of
            Just cd | cdAttempt cd ≡ attempt ∧ cdStatus cd ≢ CsComplete →
                (HM.insert key (cd { cdStatus = CsPlacing }) m, True)
            _ → (m, False)

-- | Build AI hook (#96): set a designation's status. Complete removes it
--   (and resets the corner-progress display back to flat ground — the
--   placed piece takes over from there).
--
--   #1844: attempt-guarded, and the attempt is REQUIRED. A completion
--   for an attempt that is no longer there removes nothing, so a stale
--   worker cannot delete the successor designation a player just made at
--   the same tile — and there is no attempt-less form that could.
handleWorldSetConstructStatusCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → ConstructStatus → ConstructAttemptId
    → Maybe StructureCommitWindow → IO ()
handleWorldSetConstructStatusCommand env _logger pageId gx gy st attempt
                                     mWindow = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- #1175: a build-AI job coord is a point op like any other.
            worldSize ← pageWrapWorldSize worldState
            let key = canonicalTile worldSize gx gy
            -- #1844: @structure.place@ returning true means STAGED AND
            -- QUEUED, not committed — the world thread declines the
            -- queued placement when the target chunk evicted in between,
            -- retracting the staged entry and recording the token. A
            -- completion that carries the placement's own commit window
            -- is therefore CONDITIONAL: if anything in that span was
            -- declined, nothing was really built, and deleting the
            -- designation would leave a paid attempt with neither a
            -- structure nor its materials. Same protocol
            -- 'handleWorldMarkLocationStampedCommand' uses for the same
            -- hazard (#2051). No window means no claim to check, which
            -- is every non-completion and every caller that placed
            -- nothing.
            declined ← case (st, mWindow) of
                (CsComplete, Just window) →
                    atomicModifyIORef' (wsStructureStageRef worldState)
                                       (takeDeclinedInWindow window)
                _ → pure False
            -- …and the placement's own SITE is re-resolved one last
            -- time. Requirement 18's hand-off state exempts a placing
            -- designation from the occupancy check — the piece in its
            -- slot is the worker's own — but nothing else, and the world
            -- thread can have drained a terrain, fluid or catalogue
            -- mutation between @beginPlacement@ and this command. A site
            -- whose surface has drifted or whose pack has gone must be
            -- cancelled and refunded here rather than completed, or the
            -- structure lands on ground the plan no longer describes.
            -- 'PlanForCommit' is that exemption, and only that one.
            invalid ← case st of
                CsComplete → do
                    designations ← readIORef
                        (wsConstructDesignationsRef worldState)
                    case HM.lookup key designations of
                        Just cd
                          | cdAttempt cd ≡ attempt
                          , CtStructure piece ← cdTarget cd → do
                              pw ← constructPlanWorld env worldState
                              let r = resolveStructurePlan pw
                                          (PlanForCommit attempt)
                                          (cdZ cd) piece key
                              pure (prOutcome r ≢ PlanValid)
                        _ → pure False
                _ → pure False
            let unbuilt = declined ∨ invalid
            mCd ← atomicModifyIORef' (wsConstructDesignationsRef worldState) $
                \m → case HM.lookup key m of
                    Just cd | cdAttempt cd ≡ attempt → case st of
                        -- A STRUCTURE completion must PROVE its placement
                        -- committed. Without a window there is nothing to
                        -- check, and deleting on that would lose a paid
                        -- designation's receipt for a piece that may never
                        -- have landed — so a windowless structure
                        -- completion does nothing at all. A BUILDING never
                        -- goes through 'structure.place' (it stakes via
                        -- 'building.spawn', which reports its own success
                        -- synchronously), so it has no window to give and
                        -- keeps the plain flow.
                        CsComplete
                          | CtStructure _ ← cdTarget cd
                          , isNothing mWindow → (m, Nothing)
                          | otherwise → (HM.delete key m, Just cd)
                        _ → (HM.insert key (cd { cdStatus = st }) m, Nothing)
                    _ → (m, Nothing)
            forM_ mCd $ \cd → do
                clearConstructDesignationSlope worldState key cd
                if unbuilt
                    -- The placement was declined, or its site stopped
                    -- being buildable while the completion was in
                    -- flight: either way nothing was really built, so
                    -- this is a CANCELLATION wearing a completion's
                    -- clothes. The receipt goes back to the ground
                    -- exactly as any other cancellation's would, and the
                    -- claimant is detached.
                    then do
                        deps ← constructRefundDeps env
                        refundConstructDesignation deps worldState key cd
                        notifyConstructInvalidated env worldState key cd
                    -- …and a CONFIRMED completion says so, which is what
                    -- lets the claimant grant its work XP only for a
                    -- piece that really landed (#1844). Structures only:
                    -- a building's own stake path reports synchronously.
                    else when (st ≡ CsComplete) $
                        notifyConstructCompleted env worldState key cd
        Nothing → pure ()

-- | Build AI hook (#96): pour progress into a designation. Deltas are
--   normalised to the job's total work (1.0 = done); the accumulated
--   value is clamped to [0, 1]. Completion is NOT triggered here — the
--   build AI watches the value and places the piece itself, then sends
--   CsComplete. Each application re-stamps the tile's corner-progress
--   display (the mining slope-mask pipeline, 'World.Construct.Apply')
--   so the site visibly works corner-by-corner.
--
--   #1844: attempt-guarded, and the attempt is REQUIRED, so a delayed
--   pour from a removed attempt cannot advance — or visibly stamp
--   progress onto — a successor.
handleWorldAddConstructProgressCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Float → ConstructAttemptId → IO ()
handleWorldAddConstructProgressCommand env logger pageId gx gy delta attempt = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → do
            -- #1175: a build-AI job coord is a point op like any other.
            worldSize ← pageWrapWorldSize worldState
            let key = canonicalTile worldSize gx gy
            mUpd ← atomicModifyIORef' (wsConstructDesignationsRef worldState) $
                \m → case HM.lookup key m of
                    Just cd | cdAttempt cd ≡ attempt →
                        let cd' = cd { cdProgress = max 0.0 (min 1.0
                                          (cdProgress cd + delta)) }
                        in ( HM.insert key cd' m
                           , Just (cdProgress cd, cd') )
                    _ → (m, Nothing)
            forM_ mUpd $ \(prevProgress, cd') →
                withConstructChunk worldState key $
                    applyConstructSlopeToChunk key prevProgress cd'
            -- #1858: 'applyCornerSlopeToChunk' sheds the tile's surface
            -- vegetation the moment any corner has progressed, so a
            -- build site's own progress write is a way a tile stops
            -- being tilled soil with no vegetation or terrain EDIT
            -- anywhere.
            _ ← revalidatePlantDesignations logger worldState
            pure ()
        Nothing → pure ()

-- | Run a chunk transform for the designation tile's loaded chunk and
--   invalidate the render caches — the same writeback the live dig
--   path uses ('handleWorldDigTileCommand'). No-op when the chunk
--   isn't loaded (the load path re-derives the display instead).
--
--   Takes a CANONICAL tile coord (#1175): every caller canonicalises
--   before touching the designation map, and the transform it passes
--   indexes the resolved chunk with the same coord.
withConstructChunk ∷ WorldState → (Int, Int)
                   → (LoadedChunk → LoadedChunk) → IO ()
withConstructChunk worldState (gx, gy) f = do
    let (coord, _) = globalToChunk gx gy
    td ← readIORef (wsTilesRef worldState)
    case lookupChunk coord td of
        Nothing → pure ()
        Just lc → do
            let lc' = f lc
            atomicModifyIORef' (wsTilesRef worldState) $ \w →
                (insertChunk lc' w, ())
            bumpQuadCacheGen worldState
            writeIORef (wsZoomQuadCacheRef worldState) Nothing
            writeIORef (wsBgQuadCacheRef worldState)   Nothing

handleWorldSetConstructDesignateTextureCommand ∷ EngineEnv → LoggerState
    → WorldPageId → Text → TextureHandle → IO ()
handleWorldSetConstructDesignateTextureCommand env _logger pageId cat tid = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                case cat of
                    "building" → (cs { constructBuildingTexture = Just tid }, ())
                    _          → (cs { constructStructTexture = Just tid }, ())
        Nothing → pure ()

-- | Wire path tool (#359): toggle the anchor→hover preview between the
--   default filled rectangle and a straight 1-wide line. The COMMIT
--   reads this same flag (#1844), so the two cannot disagree about the
--   shape of the drag.
handleWorldSetConstructLineModeCommand ∷ EngineEnv → LoggerState
    → WorldPageId → Bool → IO ()
handleWorldSetConstructLineModeCommand env _logger pageId enabled = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { constructLineMode = enabled }, ())
        Nothing → pure ()
