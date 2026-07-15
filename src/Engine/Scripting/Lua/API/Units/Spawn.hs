{-# LANGUAGE Strict, UnicodeSyntax #-}
module Engine.Scripting.Lua.API.Units.Spawn
  ( unitSpawnFn
  , unitDestroyFn
  , unitSetPosFn
  , unitMoveToFn
  , unitJumpFn
  , unitStopFn
  , unitCollapseFn
  , unitCrawlFn
  , unitReviveFn
  , unitRecomputeBodyFn
  , unitKillFn
  , unitTransitionToFn
  , unitGetPoseFn
  , unitGetPosFn
  , unitGetFactionFn
  , unitExistsFn
  )
    where

import UPrelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..), activeWorldPage)
import World.Page.Types (WorldPageId(..))
import Engine.Core.Log (LogCategory(..), logWarn)
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Command.Types (UnitCommand(..))
import Unit.Thread.Command (recomputeBodyDerivedStats)
import Unit.Sim.Types (Pose(..))
import World.Types (WorldManager(..))
import Engine.Scripting.Lua.API.Units.Yaml (surfaceZInWorld)


-- | Spawn a unit. If gridZ is omitted, looks up surface elevation.
--   Falls back to Z=0 if chunk isn't loaded. Returns unit ID or -1.
--
--   Signature: unit.spawn(defName, gx, gy, [gz], [factionId], [pageId])
--   factionId is the spawn-time faction tag — "player" for player-
--   controlled, "wildlife" for everything else. Defaults to
--   "wildlife" when omitted. The arg can sit at slot 4 (when gz is
--   omitted) or slot 5 (when both are supplied); both shapes work
--   so callers don't have to pass an explicit nil for gz.
--   pageId (slot 6) optionally pins the spawn to a specific live world
--   page instead of the active one. A building spawning a unit must
--   pass its OWN page here: scoping the caller's per-tick scan to the
--   active page is not enough, because the active page can change (a
--   queued world.show/hide on the world thread) between the scan and
--   this call, which would otherwise route the unit into the wrong
--   world (#196). Omitted → the active world, as before.
unitSpawnFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSpawnFn env = do
    nameArg     ← Lua.tostring 1
    xArg        ← Lua.tonumber 2
    yArg        ← Lua.tonumber 3
    -- Discriminate slot 4 by Lua type, not by coercion. `tointeger`
    -- succeeds on numeric strings (Lua auto-coerces), so a numeric
    -- faction tag like "5" would silently land in the z-slot and
    -- the faction would default to "wildlife". The actual Lua type
    -- tag is set by the caller and isn't subject to coercion.
    slot4Ty     ← Lua.ltype 4
    zArg        ← case slot4Ty of
        Lua.TypeNumber → Lua.tointeger 4
        _              → return Nothing
    factionArg4 ← case slot4Ty of
        Lua.TypeString → Lua.tostring 4
        _              → return Nothing
    factionArg5 ← Lua.tostring 5
    pageArg6    ← Lua.tostring 6

    case nameArg of
        Nothing → do
            Lua.pushnumber (-1)
            return 1
        Just nameBS → do
            let name = TE.decodeUtf8Lenient nameBS
                gx = case xArg of
                         Just (Lua.Number n) → realToFrac n
                         _                   → 0.0
                gy = case yArg of
                         Just (Lua.Number n) → realToFrac n
                         _                   → 0.0
                -- Resolve faction: slot 5 wins if present, else slot 4
                -- (only when it's actually a Lua string), else default.
                factionId = case factionArg5 of
                    Just fbs → TE.decodeUtf8Lenient fbs
                    Nothing → case factionArg4 of
                        Just fbs → TE.decodeUtf8Lenient fbs
                        Nothing  → "wildlife"

            result ← Lua.liftIO $ do
                -- Check def exists
                um ← readIORef (unitManagerRef env)
                -- Resolve the world the unit will belong to. An explicit
                -- pageId (slot 6) pins it to that live page; otherwise it
                -- defaults to the active world (#78). A unit needs a world
                -- to live in, so reject the spawn when the target page
                -- doesn't resolve to a live world.
                mActive ← case pageArg6 of
                    Just pbs → do
                        let pid = WorldPageId (TE.decodeUtf8Lenient pbs)
                        wm ← readIORef (worldManagerRef env)
                        pure $ (\ws → (pid, ws)) <$> lookup pid (wmWorlds wm)
                    Nothing  → activeWorldPage env
                case (HM.lookup name (umDefs um), mActive) of
                    (Nothing, _) → return (-1)
                    (_, Nothing) → do
                        logger ← readIORef (loggerRef env)
                        logWarn logger CatAsset
                            "unit.spawn: no world to spawn into"
                        return (-1)
                    (Just _, Just (pageId, ws)) → do
                        -- Resolve Z from the SAME page the unit is stamped
                        -- into (ws), not whatever world happens to be
                        -- visible — otherwise an explicit pageId could be
                        -- stamped correctly yet take another page's height
                        -- (or 0) when several worlds are live (#196).
                        gz ← case zArg of
                            Just n  → return (fromIntegral n)
                            Nothing → do
                                let gxi = floor gx ∷ Int
                                    gyi = floor gy ∷ Int
                                mSurf ← surfaceZInWorld ws gxi gyi
                                case mSurf of
                                    Just z  → return z
                                    Nothing → do
                                        logger ← readIORef (loggerRef env)
                                        logWarn logger CatAsset $
                                            "unit.spawn: chunk not loaded at ("
                                            <> T.pack (show gxi) <> ", "
                                            <> T.pack (show gyi)
                                            <> "), defaulting Z=0"
                                        return 0

                        -- Allocate ID
                        uid ← atomicModifyIORef' (unitManagerRef env) $ \um' →
                            let (uid', um'') = nextUnitId um'
                            in (um'', uid')

                        -- Enqueue spawn command, stamped with the active
                        -- world so the unit is world-scoped (#78).
                        Q.writeQueue (unitQueue env) $
                            UnitSpawn uid name gx gy gz factionId pageId

                        return (fromIntegral (unUnitId uid) ∷ Int)

            Lua.pushnumber (Lua.Number (fromIntegral result))
            return 1

unitDestroyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDestroyFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitDestroy uid
            Lua.pushboolean True
            return 1

-- | Teleport a unit. If gridZ is omitted, looks up surface elevation.
unitSetPosFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetPosFn env = do
    idArg ← Lua.tointeger 1
    xArg  ← Lua.tonumber 2
    yArg  ← Lua.tonumber 3
    zArg  ← Lua.tointeger 4

    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                gx = case xArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                gy = case yArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                mGz = case zArg of
                         Just z  → Just (fromIntegral z)
                         Nothing → Nothing
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitTeleport uid gx gy mGz
            Lua.pushboolean True
            return 1

-- | Order a unit to walk to a target. Speed defaults to 2.0 tiles/sec.
unitMoveToFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitMoveToFn env = do
    idArg    ← Lua.tointeger 1
    xArg     ← Lua.tonumber 2
    yArg     ← Lua.tonumber 3
    speedArg ← Lua.tonumber 4

    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                tx = case xArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                ty = case yArg of
                         Just (Lua.Number v) → realToFrac v
                         _                   → 0.0
                speed = case speedArg of
                            Just (Lua.Number v) → realToFrac v
                            _                   → 2.0
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitMoveTo uid tx ty speed
            Lua.pushboolean True
            return 1

-- | unit.jump(uid, gx, gy) — order a unit to LEAP to target tile (gx,gy).
--   The unit thread launches a gravity arc if the gap is within reach
--   (jumping skill + agility/strength) and the unit is standing; otherwise
--   it's a no-op. Returns true if the command was enqueued (not whether
--   the leap will be in range — that's decided on the unit thread).
unitJumpFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitJumpFn env = do
    idArg ← Lua.tointeger 1
    xArg  ← Lua.tointeger 2
    yArg  ← Lua.tointeger 3
    case (idArg, xArg, yArg) of
        (Just n, Just tx, Just ty) → do
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitJump (UnitId (fromIntegral n))
                         (fromIntegral tx) (fromIntegral ty)
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

unitStopFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitStopFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitStop uid
            Lua.pushboolean True
            return 1

-- | unit.collapse(id) — transition the unit into the Collapsed state.
--   The state's anim is resolved via udStateAnims ("collapsed" → name);
--   a non-looping anim plays once and pickFrame holds the last frame.
--   Collapsed units ignore subsequent UnitMoveTo commands.
unitCollapseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCollapseFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitCollapse uid
            Lua.pushboolean True
            return 1

-- | unit.crawl(id) — drop a conscious-but-can't-walk unit (legs broken
--   or severed) to a sustained Crawling pose. Unlike collapse, a crawling
--   unit still accepts move commands and crawls slowly toward its goal;
--   any in-flight target is preserved. unit.revive stands it back up.
unitCrawlFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitCrawlFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitCrawl uid
            Lua.pushboolean True
            return 1

-- | unit.revive(id) — transition a Collapsed unit through the
--   Reviving state and back to Idle. The reviving-state anim plays
--   (typically the collapse anim in reverse via uiAnimReverse). No-op
--   if the unit isn't currently Collapsed.
unitReviveFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitReviveFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitRevive uid
            Lua.pushboolean True
            return 1

-- | unit.recomputeBody(uid) — re-derive strength / strength_body /
--   max_hydration / max_hunger / carrying_capacity from the unit's
--   current body_mass / lean_mass / fat_mass. Call this from Lua after
--   directly mutating any body-composition stat (Phase 3 regrowth,
--   Phase 4 catabolism).
--   Returns true if the unit exists, false otherwise. No-op if the
--   unit's stat map is missing body_mass / lean_mass / height.
unitRecomputeBodyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitRecomputeBodyFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            ok ← Lua.liftIO $ atomicModifyIORef' (unitManagerRef env) $ \um →
                case HM.lookup uid (umInstances um) of
                    Nothing → (um, False)
                    Just inst →
                        let inst' = inst { uiStats =
                                recomputeBodyDerivedStats (uiStats inst) }
                        in (um { umInstances = HM.insert uid inst'
                                                 (umInstances um) }, True)
            Lua.pushboolean ok
            return 1

-- | unit.kill(uid) — terminal. Snaps the unit to the Dead pose and
--   clears all in-flight state. Dead units are filtered out of AI,
--   ignore further commands, and never revive. Issued by the Lua
--   survival code when hydration drops below 5 % or stamina hits 0.
unitKillFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitKillFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $ UnitKill uid
            Lua.pushboolean True
            return 1

-- | unit.transitionTo(uid, poseName, stride?) — initiate a pose
--   transition. poseName is one of "standing", "crouching", "crawling",
--   "collapsed". Optional stride defaults to 1; pass 2 (or higher) to
--   skip frames when chaining transitions back-to-back.
--   No-op if the unit is already in that pose or mid-transition.
unitTransitionToFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitTransitionToFn env = do
    idArg     ← Lua.tointeger 1
    mPoseBS   ← Lua.tostring 2
    mStrideArg ← Lua.tointeger 3
    let stride = case mStrideArg of
            Just s | s ≥ 1 → fromIntegral s
            _              → 1
    case (idArg, mPoseBS >>= parsePose . TE.decodeUtf8Lenient) of
        (Just n, Just target) → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (unitQueue env) $
                UnitTransitionTo uid target stride
            Lua.pushboolean True
            return 1
        _ → do
            Lua.pushboolean False
            return 1

parsePose ∷ Text → Maybe Pose
parsePose "standing"  = Just Standing
parsePose "crouching" = Just Crouching
parsePose "crawling"  = Just Crawling
parsePose "collapsed" = Just Collapsed
parsePose "climbing"  = Just Climbing
parsePose "falling"   = Just Falling
parsePose "sleeping"  = Just Sleeping
parsePose _           = Nothing

-- | unit.getPose(uid) — returns the unit's current pose as a string,
--   one of: "standing" / "crouching" / "crawling" / "collapsed" /
--   "dead" / "climbing" / "falling" / "sleeping" (the full
--   `Unit.Anim.poseTag` set). nil if the unit doesn't exist. Reads
--   `uiPose`, mirrored from `usPose` by Unit.Thread.publishToRender
--   every tick.
unitGetPoseFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPoseFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mPose ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiPose <$> HM.lookup uid (umInstances um))
            case mPose of
                Just label → do
                    Lua.pushstring (TE.encodeUtf8 label)
                    return 1
                Nothing → do
                    Lua.pushnil
                    return 1

unitGetPosFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetPosFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushnil
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            um ← Lua.liftIO $ readIORef (unitManagerRef env)
            case HM.lookup uid (umInstances um) of
                Nothing → do
                    Lua.pushnil
                    return 1
                Just inst → do
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridX inst)))
                    Lua.pushnumber (Lua.Number (realToFrac (uiGridY inst)))
                    Lua.pushnumber (Lua.Number (fromIntegral (uiGridZ inst)))
                    return 3

-- | unit.getFaction(uid) → string | nil
--   Returns the spawn-time-assigned faction tag ("player", "wildlife",
--   etc.). Used by the right-click menu's hostile/friendly check.
unitGetFactionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetFactionFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mFac ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (uiFactionId <$> HM.lookup uid (umInstances um))
            case mFac of
                Just f  → Lua.pushstring (TE.encodeUtf8 f) >> return 1
                Nothing → Lua.pushnil >> return 1

-- | unit.exists(uid) → bool
--   True iff the engine still has a UnitInstance for this id. Used by
--   the AI to drop attack/move goals when their target is destroyed.
unitExistsFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitExistsFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushboolean False >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            exists ← Lua.liftIO $ do
                um ← readIORef (unitManagerRef env)
                pure (HM.member uid (umInstances um))
            Lua.pushboolean exists
            return 1
