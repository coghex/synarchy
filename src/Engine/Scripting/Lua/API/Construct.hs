{-# LANGUAGE Strict, UnicodeSyntax #-}
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
import Engine.Core.State (EngineEnv(..), activeWorldState, activeWorldPage)
import Engine.Asset.Handle (TextureHandle(..))
import World.Types (WorldManager(..), WorldState(..))
import World.Page.Types (WorldPageId(..))
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Coordinates (globalToChunk)
import World.Command.Types (WorldCommand(..))
import World.Construct.Types
import World.Thread.Command.Cursor.Construct (popConstructDesignation)

-- | construction.setAnchor(pageId, gx, gy) — first-click anchor.
constructSetAnchorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructSetAnchorFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg     ← Lua.tonumber 2
    gyArg     ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (worldQueue env) $
                WorldSetConstructAnchor pageId (round gx) (round gy)
        _ → pure ()
    return 0

-- | construction.clearAnchor(pageId) — cancel the pending rectangle.
constructClearAnchorFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructClearAnchorFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (worldQueue env) $ WorldClearConstructAnchor pageId
        _ → pure ()
    return 0

-- | construction.designate(pageId, x1, y1, x2, y2, category, a, b, c)
--   commits the rectangle for a build target:
--     * category "structure": a=pack, b=kind ("wall"/"floor"/"ceiling"/
--       "post"), c=wall edge ("ne"/"nw"/"se"/"sw"; nil for non-walls)
--     * category "building":  a=building def name (rest ignored)
--   Unknown categories are ignored. A building only marks the anchor.
constructDesignateFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructDesignateFn env = do
    pageIdArg ← Lua.tostring 1
    x1Arg ← Lua.tonumber 2
    y1Arg ← Lua.tonumber 3
    x2Arg ← Lua.tonumber 4
    y2Arg ← Lua.tonumber 5
    catArg ← Lua.tostring 6
    aArg ← Lua.tostring 7
    bArg ← Lua.tostring 8
    cArg ← Lua.tostring 9
    case (pageIdArg, x1Arg, y1Arg, x2Arg, y2Arg, catArg) of
        (Just pageIdBS, Just x1, Just y1, Just x2, Just y2, Just catBS) →
            case mkTarget (TE.decodeUtf8Lenient catBS) aArg bArg cArg of
                Nothing → pure ()
                Just tgt → Lua.liftIO $ do
                    let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                    Q.writeQueue (worldQueue env) $
                        WorldDesignateConstruct pageId
                            (round x1) (round y1) (round x2) (round y2) tgt
        _ → pure ()
    return 0
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
constructCancelDesignationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructCancelDesignationFn env = do
    gxArg ← Lua.tonumber 1
    gyArg ← Lua.tonumber 2
    case (gxArg, gyArg) of
        (Just gx, Just gy) → do
            mPage ← Lua.liftIO $ activeWorldPage env
            case mPage of
                Just (pageId, _) → Lua.liftIO $
                    Q.writeQueue (worldQueue env) $
                        WorldCancelConstruct pageId (round gx) (round gy)
                Nothing → pure ()
        _ → pure ()
    return 0

-- | construction.getPendingJobs(cx1, cy1, cx2, cy2) → array of jobs in
--   the chunk region on the active world. Each job:
--     { x, y, z, category, status, progress,
--       pack, kind, edge      -- structure targets
--       building              -- building targets }
--   The build AI (#96) reads this to find work. Jobs a worker has
--   claimed ARE included, carrying status "claimed" — the AI filters on
--   status when looking for fresh work (so a second worker still can't
--   re-claim an owned tile) and uses the claimed entries to release
--   stale claims (dead/vanished claimant) back to "pending" on timeout.
constructGetPendingJobsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructGetPendingJobsFn env = do
    cx1Arg ← Lua.tonumber 1
    cy1Arg ← Lua.tonumber 2
    cx2Arg ← Lua.tonumber 3
    cy2Arg ← Lua.tonumber 4
    mWs ← Lua.liftIO $ activeWorldState env
    case (mWs, cx1Arg, cy1Arg, cx2Arg, cy2Arg) of
        (Just ws, Just cx1, Just cy1, Just cx2, Just cy2) → do
            m ← Lua.liftIO $ readIORef (wsConstructDesignationsRef ws)
            let inRegion (gx, gy) =
                    let (ChunkCoord cx cy, _) = globalToChunk gx gy
                    in cx ≥ round cx1 ∧ cx ≤ round cx2
                     ∧ cy ≥ round cy1 ∧ cy ≤ round cy2
                -- Claimed jobs stay in the list WITH their status (#96):
                -- consumers filter status == "pending" when scanning for
                -- fresh work, and the AI's stale-claim sweep needs to see
                -- claimed entries to release an expired/dead claimant's
                -- job back to pending (acceptance: getPendingJobs shows
                -- "claimed" while in progress, "pending" after release).
                jobs = [ kv | kv@(k, _) ← HM.toList m, inRegion k ]
            Lua.newtable
            forM_ (zip [1 ∷ Int ..] jobs) $ \(i, ((gx, gy), cd)) → do
                pushJobTable gx gy cd
                Lua.rawseti (Lua.nth 2) (fromIntegral i)
            return 1
        _ → Lua.pushnil >> return 1

-- | construction.getDesignationAt(pageId, gx, gy) → job table | nil.
constructGetDesignationAtFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructGetDesignationAtFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                gx = round gxN ∷ Int
                gy = round gyN ∷ Int
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsConstructDesignationsRef ws)
                    case HM.lookup (gx, gy) m of
                        Just cd → pushJobTable gx gy cd >> return 1
                        Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | construction.cancelDesignationForRefund(pageId, gx, gy) → job table
--   | nil. Synchronous, ATOMIC pop-and-return (#799 review round 5):
--   unlike cancelDesignation (fire-and-forget, queued on the world
--   thread), this removes the designation and returns its final state
--   in ONE atomic step, so a caller computing a materials refund from
--   the returned job's 'paid' field never races a second caller over
--   the SAME entry — whether that's a rapid double right-click on one
--   designation, or a genuinely new designation quickly replacing it
--   at the same tile. See 'World.Thread.Command.Cursor.Construct.popConstructDesignation'.
constructCancelDesignationForRefundFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructCancelDesignationForRefundFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gxN, Just gyN) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                gx = round gxN ∷ Int
                gy = round gyN ∷ Int
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Nothing → Lua.pushnil >> return 1
                Just ws → do
                    mCd ← Lua.liftIO $ popConstructDesignation ws (gx, gy)
                    case mCd of
                        Just cd → pushJobTable gx gy cd >> return 1
                        Nothing → Lua.pushnil >> return 1
        _ → Lua.pushnil >> return 1

-- | construction.getDesignationCount(pageId) → n.
constructGetDesignationCountFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructGetDesignationCountFn env = do
    pageIdArg ← Lua.tostring 1
    case pageIdArg of
        Just pageIdBS → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
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
constructNearestDesignationFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructNearestDesignationFn env = do
    pageIdArg ← Lua.tostring 1
    xArg ← Lua.tonumber 2
    yArg ← Lua.tonumber 3
    case (pageIdArg, xArg, yArg) of
        (Just pageIdBS, Just x, Just y) → do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                ux = realToFrac x ∷ Float
                uy = realToFrac y ∷ Float
            mgr ← Lua.liftIO $ readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just ws → do
                    m ← Lua.liftIO $ readIORef (wsConstructDesignationsRef ws)
                    let dist2 (gx, gy) =
                            let dx = fromIntegral gx - ux
                                dy = fromIntegral gy - uy
                            in dx * dx + dy * dy
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
constructSetJobStatusFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructSetJobStatusFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    statusArg ← Lua.tostring 4
    case (pageIdArg, gxArg, gyArg, statusArg) of
        (Just pageIdBS, Just gx, Just gy, Just statusBS) →
            case textToConstructStatus (TE.decodeUtf8Lenient statusBS) of
                Just st → Lua.liftIO $ do
                    let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                    Q.writeQueue (worldQueue env) $
                        WorldSetConstructStatus pageId (round gx) (round gy) st
                Nothing → pure ()
        _ → pure ()
    return 0

-- | construction.addJobProgress(pageId, gx, gy, delta) — build AI (#96)
--   pours work into a designation. delta is normalised to the job's
--   total (1.0 = done); the engine clamps the sum to [0, 1]. The AI
--   watches getDesignationAt().progress and finishes the job itself
--   (place piece, then setJobStatus "complete").
constructAddJobProgressFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructAddJobProgressFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    deltaArg ← Lua.tonumber 4
    case (pageIdArg, gxArg, gyArg, deltaArg) of
        (Just pageIdBS, Just gx, Just gy, Just delta) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (worldQueue env) $
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
--   SYNCHRONOUS (direct atomicModifyIORef', not queued — review round 6):
--   a queued write raced construction.cancelDesignationForRefund's
--   synchronous atomic pop — a cancel issued between "materials just
--   consumed" and "the queued paid=true command finally drains" would
--   pop cdMaterialsPaid still False, refunding nothing for a cost the
--   worker's inventory had already lost for good. Lua callbacks
--   (the AI tick that pays, and any UI click that cancels) run one at a
--   time on the single scripting thread, so making this write happen
--   the instant Lua calls it — instead of some later, unspecified world-
--   thread tick — is what actually closes the window; queuing can't.
constructSetMaterialsPaidFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructSetMaterialsPaidFn env = do
    pageIdArg ← Lua.tostring 1
    gxArg ← Lua.tonumber 2
    gyArg ← Lua.tonumber 3
    paidArg ← Lua.toboolean 4
    case (pageIdArg, gxArg, gyArg) of
        (Just pageIdBS, Just gx, Just gy) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            mgr ← readIORef (worldManagerRef env)
            case lookup pageId (wmWorlds mgr) of
                Just ws →
                    atomicModifyIORef' (wsConstructDesignationsRef ws) $ \m →
                        (HM.adjust (\cd → cd { cdMaterialsPaid = paidArg })
                                   (round gx, round gy) m, ())
                Nothing → pure ()
        _ → pure ()
    return 0

-- | construction.setDesignateTexture(pageId, category, texHandle) — ghost
--   texture for committed designations, keyed by category ("structure" |
--   "building").
constructSetDesignateTextureFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructSetDesignateTextureFn env = do
    pageIdArg ← Lua.tostring 1
    catArg ← Lua.tostring 2
    handleArg ← Lua.tointeger 3
    case (pageIdArg, catArg, handleArg) of
        (Just pageIdBS, Just catBS, Just handle) → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
                texHandle = TextureHandle (fromIntegral handle)
            Q.writeQueue (worldQueue env) $
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
constructSetLineModeFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
constructSetLineModeFn env = do
    pageIdArg ← Lua.tostring 1
    enabledArg ← Lua.toboolean 2
    case pageIdArg of
        Just pageIdBS → Lua.liftIO $ do
            let pageId = WorldPageId (TE.decodeUtf8Lenient pageIdBS)
            Q.writeQueue (worldQueue env) $
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
