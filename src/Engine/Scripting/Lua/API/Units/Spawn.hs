{-# LANGUAGE Strict #-}
module Engine.Scripting.Lua.API.Units.Spawn
  ( unitSpawnFn
  , unitDestroyFn
  , unitSetPosFn
  , unitMoveToFn
  , unitSetMoveSpeedFn
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
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified HsLua as Lua
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State
    (EngineEnv, loggerRef, resolveActiveWorld)
import World.Page.Types (WorldPageId(..))
import Engine.Core.Log (LogCategory(..), logWarn)
import qualified Engine.Core.Queue as Q
import Unit.Types
import Unit.Faction
    ( defaultSpawnFaction, fallbackFaction, factionTag, parseFaction )
import Unit.Command.Types (UnitCommand(..))
import Unit.Thread.Command (recomputeBodyDerivedStats)
import Unit.Sim.Types (Pose(..))
import Unit.Pathing.Hazard
    (defaultMoveHazardPolicy, parseMoveHazardPolicy)
import World.Types (WorldManager(..))
import Engine.Scripting.Lua.API.PageBinding
    (bindingStale, pageBindingStaleReason)
import Engine.Scripting.Lua.API.Units.MotionArgs
    (defaultingSpeed, readMotionArg, requiredCoordinate, requiredSpeed)
import Engine.Scripting.Lua.API.Units.Yaml (surfaceZInWorld)


-- | Spawn a unit. If gridZ is omitted, looks up surface elevation.
--   Falls back to Z=0 if chunk isn't loaded. Returns unit ID or -1.
--
--   Signature:
--   unit.spawn(defName, gx, gy, [gz], [factionId], [pageId], [bindGen])
--   factionId is the spawn-time faction tag — one of the canonical
--   'Unit.Faction.factionTag' values ("player", "wildlife", "hostile",
--   "neutral", "debug"). This is the ingress boundary (#912): the tag is
--   parsed to a typed 'Faction' here, and an unrecognized one warns once
--   for this request and resolves to 'Unit.Faction.fallbackFaction'
--   rather than travelling onward as an unvalidated string.
--
--   Omitting it is DELIBERATE, not an oversight: it yields
--   'Unit.Faction.defaultSpawnFaction' ("wildlife"), which is what the
--   world-gen animal spawns want. Every source that means something else
--   already passes its tag explicitly (portal spawns → "player", the
--   debug overlay → "debug", location contents → "hostile").
--
--   The arg can sit at slot 4 (when gz is
--   omitted) or slot 5 (when both are supplied); both shapes work
--   so callers don't have to pass an explicit nil for gz.
--   pageId (slot 6) optionally pins the spawn to a specific live world
--   page instead of the active one. A building spawning a unit must
--   pass its OWN page here: scoping the caller's per-tick scan to the
--   active page is not enough, because the active page can change (a
--   queued world.show/hide on the world thread) between the scan and
--   this call, which would otherwise route the unit into the wrong
--   world (#196). Omitted → the active world, as before.
--
--   @bindGen@ (slot 7, #1686) is the page-selection generation the
--   caller captured when it decided this spawn was eligible —
--   @building.getActiveIds@ reports it alongside the ids it enumerates.
--   #196 fixed the DESTINATION but left the ELIGIBILITY window open: a
--   portal enumerated while its page was active can still reach this
--   call after the page has been hidden, and an explicit pageId is
--   accepted for any live page, hidden or not. When a binding is
--   present it is checked — generation freshness AND the target page
--   still being the active one — inside the SAME manager read that
--   resolves the page, BEFORE any id is allocated or any command is
--   queued, and a stale one answers @(nil, "page binding stale")@.
--
--   That refusal is deliberately a DISTINCT Lua-visible outcome rather
--   than the @-1@ every other rejection here pushes: @-1@ is truthy in
--   Lua, so a caller's @if not uid@ guard cannot see it, and the tick
--   that owns the binding must branch on the refusal BEFORE it hands
--   out items, commands a walk, or consumes a roster entry. Omitting
--   the binding — location content spawning, AI staking, debug spawns,
--   world-gen animals — keeps the @-1@ contract exactly (#1687 owns
--   that convention).
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
    -- Discriminate slot 7 by Lua type for the same reason slot 4 is
    -- discriminated, but with the opposite failure direction in mind: a
    -- binding that fails to parse must NOT quietly degrade to an
    -- UNBOUND spawn, because unbound is precisely the behaviour the
    -- binding exists to stop. Only a real Lua number is a binding;
    -- absent is unbound; anything else is a malformed binding, and
    -- 'boundSpawnStale' refuses it.
    slot7Ty     ← Lua.ltype 7
    bindArg7    ← case slot7Ty of
        Lua.TypeNumber → Lua.tointeger 7
        _              → return Nothing

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
                -- Resolve the RAW faction tag: slot 5 wins if present,
                -- else slot 4 (only when it's actually a Lua string).
                -- Nothing = the caller omitted it → defaultSpawnFaction.
                rawFactionTag = case factionArg5 of
                    Just fbs → Just (TE.decodeUtf8Lenient fbs)
                    Nothing  → TE.decodeUtf8Lenient <$> factionArg4

            result ← Lua.liftIO $ do
                -- Check def exists
                um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
                -- ONE manager read serves the binding check AND the
                -- target resolution (#1686): re-reading for the second
                -- would reopen the very window the binding closes.
                wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
                -- Resolve the world the unit will belong to. An explicit
                -- pageId (slot 6) pins it to that live page; otherwise it
                -- defaults to the active world (#78). A unit needs a world
                -- to live in, so reject the spawn when the target page
                -- doesn't resolve to a live world.
                let mActive = case pageArg6 of
                        Just pbs →
                            let pid = WorldPageId (TE.decodeUtf8Lenient pbs)
                            in (\ws → (pid, ws)) <$> lookup pid (wmWorlds wm)
                        Nothing → resolveActiveWorld wm
                    stale = boundSpawnStale slot7Ty bindArg7 mActive wm
                case (stale, HM.lookup name (umDefs um), mActive) of
                    (True, _, _) → return (Left pageBindingStaleReason)
                    (_, Nothing, _) → return (Right (-1))
                    (_, _, Nothing) → do
                        logger ← readIORef (loggerRef env)
                        logWarn logger CatAsset
                            "unit.spawn: no world to spawn into"
                        return (Right (-1))
                    (_, Just _, Just (pageId, ws)) → do
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
                                            <> tshow gxi <> ", "
                                            <> tshow gyi
                                            <> "), defaulting Z=0"
                                        return 0

                        -- Ingress parse (#912): from here on the faction
                        -- is typed. An unrecognized tag warns ONCE for
                        -- this request and degrades to the inert
                        -- fallback rather than rejecting the spawn.
                        faction ← case rawFactionTag of
                            Nothing  → return defaultSpawnFaction
                            Just tag → case parseFaction tag of
                                Just f  → return f
                                Nothing → do
                                    logger ← readIORef (loggerRef env)
                                    logWarn logger CatAsset $
                                        "unit.spawn: unrecognized faction tag '"
                                        <> tag <> "' — spawning as '"
                                        <> factionTag fallbackFaction <> "'"
                                    return fallbackFaction

                        -- Allocate ID
                        uid ← atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um' →
                            let (uid', um'') = nextUnitId um'
                            in (um'', uid')

                        -- Enqueue spawn command, stamped with the active
                        -- world so the unit is world-scoped (#78).
                        Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) $
                            UnitSpawn uid name gx gy gz faction pageId

                        return (Right (fromIntegral (unUnitId uid) ∷ Int))

            case result of
                -- A refused binding answers (nil, reason) — the shape
                -- building.spawn already uses for the same refusal —
                -- so the caller can branch on it without going near
                -- the truthy -1 sentinel.
                Left reason → do
                    Lua.pushnil
                    Lua.pushstring (TE.encodeUtf8 reason)
                    return 2
                Right n → do
                    Lua.pushnumber (Lua.Number (fromIntegral n))
                    return 1

-- | Is a bound spawn's binding no longer good (#1686)? Both halves are
--   answered from the ONE 'WorldManager' snapshot the caller already
--   took, so nothing can move between them.
--
--   * The GENERATION half ('bindingStale') is what an A→B→A sequence
--     needs: the page id ends up the same, and only the generation can
--     tell the caller its snapshot is a session old. Its projected
--     component also catches a @world.hide@ that is enqueued but not
--     yet applied, so a tick racing an in-flight change is refused
--     rather than optimistically accepted.
--   * The PAGE half is the redundant one that keeps a supplied page id
--     from being accepted and then quietly ignored — an explicit page
--     is otherwise honoured for ANY live page, hidden included.
--
--   The page half deliberately compares against 'resolveActiveWorld'
--   rather than the strict visible-list head that
--   "Engine.Scripting.Lua.API.Buildings.Spawn" uses. That is the same
--   resolution @building.getActiveIds@ performs when it enumerates the
--   tick, fallback included, so "still the active page" means exactly
--   what the snapshot meant by it — a session with nothing visible
--   still ticks the page it always ticked instead of silently going
--   quiet.
--
--   A slot 7 that is present but is not a Lua number is a MALFORMED
--   binding: refused, never degraded to unbound.
boundSpawnStale ∷ Lua.Type → Maybe Lua.Integer
                → Maybe (WorldPageId, ws) → WorldManager → Bool
boundSpawnStale ty bind mTarget wm = case ty of
    Lua.TypeNil  → False
    Lua.TypeNone → False
    _            → case bind of
        Nothing → True
        Just _  →
            bindingStale bind wm
            ∨ fmap fst mTarget ≢ fmap fst (resolveActiveWorld wm)

unitDestroyFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitDestroyFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) $ UnitDestroy uid
            Lua.pushboolean True
            return 1

-- | Warn and return false without enqueueing anything — the refusal
--   shape @unit.moveTo@\'s hazard-token check already had (#1217) and
--   #2290 extended to every numeric argument of every motion verb.
--
--   'CatAsset' matches that sibling refusal deliberately: one
--   @ENGINE_DEBUG@ category shows every reason a motion verb turned a
--   call down, rather than splitting them across two.
refuseMotionArg ∷ EngineEnv → Text → Lua.LuaE Lua.Exception Lua.NumResults
refuseMotionArg env why = do
    logger ← Lua.liftIO $ readIORef (loggerRef env)
    Lua.liftIO $ logWarn logger CatAsset why
    Lua.pushboolean False
    return 1

-- | Teleport a unit. If gridZ is omitted, looks up surface elevation.
--
--   X and Y must be finite numbers (#2290). A missing, non-numeric,
--   NaN or infinite one refuses the call — nothing is queued and the
--   verb returns false — rather than substituting the 0.0 it used to,
--   which turned a dropped argument into a silent teleport to the world
--   origin and a @math.huge@ into one to an unloaded coordinate.
--
--   Z keeps its integer handling: it is optional by design (absent =
--   look the surface up) and 'Lua.tointeger' cannot yield a non-finite
--   'Int'.
unitSetPosFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetPosFn env = do
    idArg ← Lua.tointeger 1
    xArg  ← readMotionArg 2
    yArg  ← readMotionArg 3
    zArg  ← Lua.tointeger 4

    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                mGz = case zArg of
                         Just z  → Just (fromIntegral z)
                         Nothing → Nothing
                checked = (,) <$> requiredCoordinate "unit.setPos" "x" xArg
                              ⊛ requiredCoordinate "unit.setPos" "y" yArg
            case checked of
                Left why → refuseMotionArg env why
                Right (gx, gy) → do
                    Lua.liftIO $ Q.writeQueue
                        (ucUnitQueue (toUnitCombatCapability env)) $
                        UnitTeleport uid gx gy mGz
                    Lua.pushboolean True
                    return 1

-- | Order a unit to walk to a target. Speed defaults to 2.0 tiles/sec.
--
--   Signature: @unit.moveTo(uid, gx, gy, [speed], [hazardPolicy])@
--
--   @gx@\/@gy@ are REQUIRED finite numbers and @speed@, when supplied,
--   is a finite non-negative one (#2290); a violation joins the
--   hazard-token refusal below in returning false, warning, and
--   enqueueing nothing. A NaN in any of the three used to reach the
--   mover, where it makes every step NaN and the arrival test never
--   true — the unit sticks \"moving\" at a position no tile lookup can
--   map, and that position is then persisted verbatim. A negative
--   speed walked the unit away from its own target.
--
--   @hazardPolicy@ (#1217) is the route's damaging-drop policy, stated
--   EXPLICITLY per request: @"allow_falls"@ (the default, and every
--   pre-#1217 caller's behavior) or @"avoid_falls"@, which makes a
--   descent the cost model classifies as a real fall impassable for this
--   request. An unrecognized token is REFUSED — it returns false, warns,
--   and enqueues nothing — rather than silently degrading to
--   @"allow_falls"@, which would turn a typo in an ambient mover into
--   exactly the cliff walk the policy exists to prevent.
--
--   The token arrives as a raw Lua byte string, so it is decoded
--   LENIENTLY (#665's convention, restored by #1605): bytes that are
--   not valid UTF-8 become replacement characters, which parse as an
--   unrecognized token and take the refusal path above. A strict decode
--   here would throw a @UnicodeException@ out of the Lua call instead
--   of producing the warning this code was written to emit.
unitMoveToFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitMoveToFn env = do
    idArg     ← Lua.tointeger 1
    xArg      ← readMotionArg 2
    yArg      ← readMotionArg 3
    speedArg  ← readMotionArg 4
    hazardArg ← Lua.tostring 5

    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
                -- The target must be finite and the speed, when one is
                -- supplied at all, finite and non-negative (#2290). An
                -- OMITTED speed still keeps the documented 2.0 — the
                -- argument has always been optional — but a supplied
                -- one that is not a number is a caller bug and is
                -- refused rather than silently defaulted.
                checked = do
                    tx ← requiredCoordinate "unit.moveTo" "x" xArg
                    ty ← requiredCoordinate "unit.moveTo" "y" yArg
                    speed ← defaultingSpeed "unit.moveTo" "speed" 2.0 speedArg
                    pure (tx, ty, speed)
                mHazard = case hazardArg of
                    Nothing  → Just defaultMoveHazardPolicy
                    Just raw → parseMoveHazardPolicy (TE.decodeUtf8Lenient raw)
            case (checked, mHazard) of
                -- The numeric domains are checked FIRST so a call that
                -- is wrong in both ways names the argument the author
                -- can act on rather than only the token.
                (Left why, _) → refuseMotionArg env why
                (_, Nothing) → do
                    logger ← Lua.liftIO $ readIORef (loggerRef env)
                    Lua.liftIO $ logWarn logger CatAsset $
                        "unit.moveTo: unrecognized hazard policy '"
                        <> maybe "" TE.decodeUtf8Lenient hazardArg
                        <> "' (expected 'allow_falls' or 'avoid_falls')"
                        <> " — move refused"
                    Lua.pushboolean False
                    return 1
                (Right (tx, ty, speed), Just hazard) → do
                    Lua.liftIO $ Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) $
                        UnitMoveTo uid tx ty speed hazard
                    Lua.pushboolean True
                    return 1

-- | unit.setMoveSpeed(uid, speed) — retarget the speed of an ALREADY
--   in-flight move (see UnitSetMoveSpeed) without resetting its
--   destination or computed local path. A no-op (still returns true —
--   the command enqueues regardless) if the unit isn't currently moving.
--
--   @speed@ is REQUIRED and must be finite and non-negative (#2290).
--   The no-op-for-a-still-unit case is unchanged: it is the HANDLER
--   that finds no target and does nothing, which is a different
--   question from whether the argument named a usable speed.
unitSetMoveSpeedFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitSetMoveSpeedFn env = do
    idArg    ← Lua.tointeger 1
    speedArg ← readMotionArg 2

    case idArg of
        Nothing → do
            Lua.pushboolean False
            return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            -- REQUIRED here, unlike @unit.moveTo@'s optional slot: this
            -- verb exists only to change the speed, so an omitted one
            -- names no retarget at all and the 0.0 it used to substitute
            -- silently froze the unit mid-route (#2290).
            case requiredSpeed "unit.setMoveSpeed" "speed" speedArg of
                Left why → refuseMotionArg env why
                Right speed → do
                    Lua.liftIO $ Q.writeQueue
                        (ucUnitQueue (toUnitCombatCapability env)) $
                        UnitSetMoveSpeed uid speed
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
            Lua.liftIO $ Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) $
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
            Lua.liftIO $ Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) $ UnitStop uid
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
            Lua.liftIO $ Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) $ UnitCollapse uid
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
            Lua.liftIO $ Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) $ UnitCrawl uid
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
            Lua.liftIO $ Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) $ UnitRevive uid
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
            ok ← Lua.liftIO $ atomicModifyIORef' (ucUnitManagerRef (toUnitCombatCapability env)) $ \um →
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
            Lua.liftIO $ Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) $ UnitKill uid
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
    case (idArg, mPoseBS ⌦ parsePose . TE.decodeUtf8Lenient) of
        (Just n, Just target) → do
            let uid = UnitId (fromIntegral n)
            Lua.liftIO $ Q.writeQueue (ucUnitQueue (toUnitCombatCapability env)) $
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
                um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
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
            um ← Lua.liftIO $ readIORef (ucUnitManagerRef (toUnitCombatCapability env))
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
--   Returns the unit's canonical faction tag ("player", "wildlife",
--   etc.). Kept as-is for compatibility (#912): scripts still identify a
--   unit's faction with this, but they must NOT compare two of these
--   strings to decide policy — feed the tag to the @faction@ global
--   ('Engine.Scripting.Lua.API.Faction'), which answers ownership,
--   commandability, alliance, and attack permission from the one typed
--   model the engine itself uses.
unitGetFactionFn ∷ EngineEnv → Lua.LuaE Lua.Exception Lua.NumResults
unitGetFactionFn env = do
    idArg ← Lua.tointeger 1
    case idArg of
        Nothing → Lua.pushnil >> return 1
        Just n → do
            let uid = UnitId (fromIntegral n)
            mFac ← Lua.liftIO $ do
                um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
                pure (uiFactionId <$> HM.lookup uid (umInstances um))
            case mFac of
                Just f  → Lua.pushstring (TE.encodeUtf8 (factionTag f))
                              >> return 1
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
                um ← readIORef (ucUnitManagerRef (toUnitCombatCapability env))
                pure (HM.member uid (umInstances um))
            Lua.pushboolean exists
            return 1
