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
    , constructSetMaterialsPaidFn
    , constructSetDesignateTextureFn
    , constructSetLineModeFn
    ) where

import UPrelude
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import qualified Engine.Core.Queue as Q
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..))
import Engine.Core.State (activeWorldPageFrom, activeWorldStateFrom)
import Engine.Asset.Handle (TextureHandle(..))
import World.Types (WorldManager(..), WorldState(..), pageWrapWorldSize)
import World.Page.Types (WorldPageId(..))
import World.Chunk.Types (chunkSize)
import World.Generate.Coordinates
    ( globalToChunk, canonicalTile, seamTileDist2, chunkInSeamRegion
    , localizeTileToAnchor )
import World.Command.Types (WorldCommand(..))
import World.Construct.Types
import World.Thread.Command.Cursor.Construct (popConstructDesignation)

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
--   before the command is enqueued; a mismatch enqueues nothing at all
--   and returns false, which is the synchronous answer the build tool
--   turns into its rejected outcome.
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
                            pure (fromIntegral want ≢ wmSelectionGen wm)
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

-- | construction.cancelDesignation(gx, gy) — remove the designation at a
--   tile on the active world. Returns nothing (best-effort).
constructCancelDesignationFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructCancelDesignationFn wsc = do
    gxArg ← Lua.tonumber 1
    gyArg ← Lua.tonumber 2
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            mPage ← Lua.liftIO $ activeWorldPageFrom (wsWorldManagerRef wsc)
            case mPage of
                Just (pageId, _) → Lua.liftIO $
                    Q.writeQueue (wsWorldQueue wsc) $
                        WorldCancelConstruct pageId (round gx) (round gy)
                Nothing → pure ()
        _ → pure ()
    return 0

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
--   the SAME entry — whether that's a rapid double right-click on one
--   designation, or a genuinely new designation quickly replacing it
--   at the same tile. See 'World.Thread.Command.Cursor.Construct.popConstructDesignation'.
constructCancelDesignationForRefundFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructCancelDesignationForRefundFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
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
                    mCd ← Lua.liftIO $ popConstructDesignation ws (gxN', gyN')
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

-- | construction.setJobStatus(pageId, gx, gy, status) — build AI marks a
--   job "claimed" / "complete" (complete removes the designation). Unknown
--   status strings are ignored.
constructSetJobStatusFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructSetJobStatusFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    statusArg ← Lua.tostring 4
    case (pageIdArg, gxArg, gyArg, statusArg) of
        (Just pageIdBS, Just gx, Just gy, Just statusBS) →
            case textToConstructStatus (TE.decodeUtf8Lenient statusBS) of
                Just st → Lua.liftIO $ do
                    let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                    Q.writeQueue (wsWorldQueue wsc) $
                        WorldSetConstructStatus pageId (round gx) (round gy) st
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
    case (pageIdArg, gxArg, gyArg, deltaArg) of
        (Just pageIdBS, Just gx, Just gy, Just delta) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (wsWorldQueue wsc) $
                WorldAddConstructProgress pageId (round gx) (round gy)
                    (realToFrac delta)
        _ → pure ()
    return 0

-- | construction.setMaterialsPaid(pageId, gx, gy, paid) — build AI (#799)
--   durably marks a structure designation's material cost as taken from a
--   claimant's inventory. The durable counterpart to the AI's in-memory
--   job.consumed: it rides the designation (and so survives claimant
--   death and save/load), so a replacement worker is never charged the
--   same cost twice. Silently ignored if the designation no longer
--   exists.
--
--   SYNCHRONOUS (direct atomicModifyIORef', not queued): a queued
--   write raced construction.cancelDesignationForRefund's
--   synchronous atomic pop — a cancel issued between "materials just
--   consumed" and "the queued paid=true command finally drains" would
--   pop cdMaterialsPaid still False, refunding nothing for a cost the
--   worker's inventory had already lost for good. Lua callbacks
--   (the AI tick that pays, and any UI click that cancels) run one at a
--   time on the single scripting thread, so making this write happen
--   the instant Lua calls it — instead of some later, unspecified world-
--   thread tick — is what actually closes the window; queuing can't.
constructSetMaterialsPaidFn ∷ WorldSimCapability → Lua.LuaE Lua.Exception Lua.NumResults
constructSetMaterialsPaidFn wsc = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    paidArg ← Lua.toboolean 4
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← readIORef (wsWorldManagerRef wsc)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    -- #1175: point mutation, same alias tolerance as the
                    -- read and both cancellation paths.
                    worldSize ← pageWrapWorldSize ws
                    atomicModifyIORef' (wsConstructDesignationsRef ws) $ \m →
                        (HM.adjust (\cd → cd { cdMaterialsPaid = paidArg })
                                   (canonicalTile worldSize (round gx)
                                                            (round gy)) m, ())
                Nothing → pure ()
        _ → pure ()
    return 0

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
    Lua.pushboolean (cdMaterialsPaid cd)
    Lua.setfield (Lua.nth 2) "paid"
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
