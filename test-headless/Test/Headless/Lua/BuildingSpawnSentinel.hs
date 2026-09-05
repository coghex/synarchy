{-# LANGUAGE ScopedTypeVariables #-}
-- | The "portal spawn rejection" gate (issue #1687):
--   @scripts/building_spawn.lua@'s per-building tick driven in a bare
--   Lua VM against a @unit.spawn@ double, proving that a REJECTED spawn
--   reaches the tick's failure path and that an ACCEPTED one still runs
--   the success path unchanged.
--
--   Why this needs its own gate. @unit.spawn@ reports every synchronous
--   rejection as the Lua NUMBER @-1@
--   (@Engine.Scripting.Lua.API.Units.Spawn@ -- missing name, unknown
--   def, no target world), and @-1@ is truthy in Lua, so the tick's
--   @if not newUid@ guard could never see one. The whole success path
--   then ran against id @-1@: it handed starting items to a unit that
--   does not exist, commanded it to walk, consumed a roster entry, and
--   parked @lastUid = -1@ -- which the component's OWN @checkWrappedRef@
--   rejects, and @building_spawn@ is a REQUIRED save component, so a
--   portal that had only ever failed to spawn refused @engine.saveWorld@
--   for the entire session. Nothing exercised @tickOne@ at all before
--   this module; there was no @unit.spawn@ stub anywhere in
--   @test-headless/@.
--
--   Pattern is "Test.Headless.Lua.SaveModules"'s: one self-contained
--   chunk per 'it' in a fresh interpreter (stdlib + the @engine@,
--   @building@ and @unit@ globals this module reaches), asserting inside
--   Lua via @assert()@, with a non-OK 'Lua.Status' surfaced as an hspec
--   failure carrying the Lua message. @scripts.unit_ai@ is pre-seeded
--   into @package.loaded@ so the fixture never boots the real AI
--   singleton -- and so @commandMove@, which ONLY the success path
--   reaches, is observable. Spying on @unit.addItem@ alone would be
--   vacuous: the shipped portal's @starting_items@ is empty.
--
--   Runs with @cabal test@'s CWD at the repo root, like every other
--   repo-root-relative Lua path here, so @require("scripts.…")@ resolves
--   through Lua's default @package.path@ with no extra setup.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "portal spawn rejection"'@.
module Test.Headless.Lua.BuildingSpawnSentinel (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Run one self-contained Lua chunk in a fresh interpreter. The chunk
--   signals failure through Lua's own @assert()@/@error()@; a non-OK
--   'Lua.Status' becomes an hspec failure carrying the Lua message.
runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run $ do
        Lua.openlibs
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | The shared fixture: a fake game clock, call counters, and the
--   @engine@ / @building@ / @unit@ globals @scripts/building_spawn.lua@
--   reaches, wired so one @BS.update(dt)@ drives exactly one @tickOne@
--   for one @acolyte_portal@.
--
--   @REMAINING@ starts at the engine's own unseeded @-1@ sentinel, so
--   the first tick exercises @ensureState@'s first-sight seeding
--   (@building.setSpawnRemaining(bid, params.count)@) -- the one piece
--   of state creation a rejected spawn is still allowed to perform.
--   @SPAWN_RESULT@ is what the @unit.spawn@ double hands back; each case
--   sets it before ticking.
fixture ∷ [Text]
fixture =
    [ "CLOCK = 0.0"
    , "WARNINGS, INFOS = {}, {}"
    , "CALLS = { spawn = 0, addItem = 0, commandMove = 0, consumeSpawn = 0 }"
    , "SPAWN_ARGS, MOVE_ARGS = {}, {}"
    , "BID, PAGE, REMAINING = 7, 3, -1"
    , "SPAWN_RESULT = -1"
    , "engine = {"
    , "  gameTime = function() return CLOCK end,"
    , "  isPaused = function() return false end,"
    , "  logWarn  = function(m) WARNINGS[#WARNINGS + 1] = tostring(m) end,"
    , "  logInfo  = function(m) INFOS[#INFOS + 1] = tostring(m) end,"
    -- #2174 moved the module's own init/shutdown narration to Debug;
    -- INFOS therefore still holds only the per-spawn outcome lines the
    -- cases below assert on.
    , "  logDebug = function() end,"
    , "}"
    , "building = {"
    , "  getActiveIds = function() return { BID } end,"
    , "  getInfo = function(bid)"
    , "    if bid ~= BID then return nil end"
    , "    return { id = BID, defName = 'acolyte_portal',"
    , "             gridX = 10, gridY = 20, page = PAGE }"
    , "  end,"
    , "  getActivity = function(_) return 'built' end,"
    , "  getSpawnRemaining = function(_) return REMAINING end,"
    , "  setSpawnRemaining = function(_, n) REMAINING = n end,"
    , "  consumeSpawn = function(_)"
    , "    CALLS.consumeSpawn = CALLS.consumeSpawn + 1"
    , "    REMAINING = REMAINING - 1"
    , "    return REMAINING"
    , "  end,"
    , "  getBuildRequired = function(_) return 0 end,"
    , "  areMaterialsSatisfied = function(_) return true end,"
    , "}"
    , "unit = {"
    , "  spawn = function(defName, x, y, z, factionTag, page)"
    , "    CALLS.spawn = CALLS.spawn + 1"
    , "    SPAWN_ARGS[#SPAWN_ARGS + 1] = { defName = defName, x = x, y = y,"
    , "                                    z = z, factionTag = factionTag,"
    , "                                    page = page }"
    , "    return SPAWN_RESULT"
    , "  end,"
    , "  getInfo = function(_) return nil end,"
    , "  addItem = function(_uid, _def, _fill) CALLS.addItem = CALLS.addItem + 1 end,"
    , "}"
    , "-- The walk-out routes through the AI singleton. Pre-seeding it keeps"
    , "-- the real unit_ai out of this VM and makes commandMove -- which"
    , "-- ONLY the success path reaches -- an observable spy."
    , "package.loaded['scripts.unit_ai'] = {"
    , "  commandMove = function(uid, x, y, speed, internal)"
    , "    CALLS.commandMove = CALLS.commandMove + 1"
    , "    MOVE_ARGS[#MOVE_ARGS + 1] = { uid = uid, x = x, y = y,"
    , "                                  speed = speed, internal = internal }"
    , "  end,"
    , "}"
    , "BS = require('scripts.building_spawn')"
    , "BS.init('test')"
    , "SAVE = require('scripts.lib.save_modules')"
    , "-- The portal's own spawn_interval, restated so the throttle-aware"
    , "-- cases below advance the fake clock by a real amount."
    , "INTERVAL = 2.0"
    , "-- One tick at the current CLOCK."
    , "function tick() BS.update(0.016) end"
    , "-- Advance the clock past the inter-spawn cooldown, then tick."
    , "function tickAfterCooldown()"
    , "  CLOCK = CLOCK + INTERVAL + 0.001"
    , "  tick()"
    , "end"
    , "function portalState() return BS.state[BID] end"
    , "-- The component's OWN wire snapshot (adds __owner, wraps lastUid),"
    , "-- which is the shape validate() is defined against -- not the live"
    , "-- state table."
    , "function wireSnapshot() return SAVE.registry.building_spawn.snapshot() end"
    , "function wireErrors()"
    , "  return SAVE.registry.building_spawn.validate(wireSnapshot())"
    , "end"
    , "function countWarnings(pattern)"
    , "  local n = 0"
    , "  for _, w in ipairs(WARNINGS) do"
    , "    if w:find(pattern, 1, true) then n = n + 1 end"
    , "  end"
    , "  return n"
    , "end"
    ]

spec ∷ Spec
spec = describe "portal spawn rejection (#1687)" $ do

    it "puts the tick on its failure path when unit.spawn returns the \
       \-1 sentinel: nothing is created, no roster entry is consumed, \
       \lastUid is left untouched, and spawnFailures advances" $
        runsOk $ lns $ fixture ++
        [ "SPAWN_RESULT = -1"
        , "tick()"
        , "assert(CALLS.spawn == 1, 'the tick must have attempted exactly one spawn')"
        , "-- Requirement 2: the failure path creates nothing and changes"
        , "-- nothing beyond the throttle bookkeeping."
        , "assert(CALLS.addItem == 0,"
        , "  'a rejected spawn must not hand out starting items')"
        , "assert(CALLS.commandMove == 0,"
        , "  'a rejected spawn must not command a walk-out for a unit that '"
        , "  .. 'does not exist (got ' .. CALLS.commandMove .. ' call(s))')"
        , "assert(CALLS.consumeSpawn == 0,"
        , "  'a rejected spawn must not consume a roster entry (got '"
        , "  .. CALLS.consumeSpawn .. ' call(s))')"
        , "assert(REMAINING == 6,"
        , "  'the roster countdown must stay at the seeded count, got '"
        , "  .. tostring(REMAINING))"
        , "local s = portalState()"
        , "assert(s ~= nil, 'first sight must still create the state row')"
        , "assert(s.lastUid == nil,"
        , "  'lastUid must be left as it was, got ' .. tostring(s.lastUid))"
        , "-- Requirement 3: the existing throttle bookkeeping still runs."
        , "assert(s.spawnFailures == 1,"
        , "  'spawnFailures must advance, got ' .. tostring(s.spawnFailures))"
        , "assert(s.lastSpawnedAt == CLOCK,"
        , "  'the retry throttle must still be stamped')"
        , "assert(#WARNINGS == 1, 'exactly one attempt warning must be logged')"
        , "assert(WARNINGS[1]:find('attempt 1', 1, true) ~= nil,"
        , "  'the warning must name the attempt number: ' .. WARNINGS[1])"
        ]

    it "leaves a never-successfully-spawned portal saveable -- its wire \
       \snapshot passes the component's own validate, and snapshotAll \
       \(the real save-side path) accepts it" $
        runsOk $ lns $ fixture ++
        [ "SPAWN_RESULT = -1"
        , "tick()"
        , "tickAfterCooldown()"
        , "tickAfterCooldown()"
        , "assert(CALLS.spawn == 3, 'three attempts must have been made')"
        , "-- Requirement 4: lastUid can never hold a value checkWrappedRef"
        , "-- rejects. A stored -1 wraps to {__ref='unit', id=-1}, which"
        , "-- validate() refuses -- and building_spawn is REQUIRED, so that"
        , "-- refuses the whole save."
        , "local snap = wireSnapshot()"
        , "assert(snap[BID] ~= nil, 'the live portal must be in the snapshot')"
        , "assert(snap[BID].lastUid == nil,"
        , "  'a portal that only ever failed must carry no lastUid on the wire')"
        , "local errs = wireErrors()"
        , "assert(errs == nil,"
        , "  'the wire snapshot must validate cleanly, got: '"
        , "  .. table.concat(errs or {}, '; '))"
        , "local result = SAVE.snapshotAll()"
        , "assert(result.ok,"
        , "  'engine.saveWorld must not be refused by a portal that has only '"
        , "  .. 'ever failed to spawn: ' .. tostring(result.error))"
        ]

    it "keeps treating nil and false as rejections too, so the pre-#1687 \
       \failure handling does not regress" $
        runsOk $ lns $ fixture ++
        [ "SPAWN_RESULT = nil"
        , "tick()"
        , "SPAWN_RESULT = false"
        , "tickAfterCooldown()"
        , "assert(CALLS.spawn == 2, 'both attempts must have reached unit.spawn')"
        , "assert(CALLS.commandMove == 0 and CALLS.consumeSpawn == 0,"
        , "  'neither a nil nor a false return may reach the success path')"
        , "local s = portalState()"
        , "assert(s.lastUid == nil, 'lastUid must be untouched')"
        , "assert(s.spawnFailures == 2,"
        , "  'both rejections must count, got ' .. tostring(s.spawnFailures))"
        , "assert(REMAINING == 6, 'the roster must be untouched')"
        ]

    it "does not drain the roster across repeated failures, throttles \
       \retries to spawn_interval, and applies the existing \
       \escalate-then-suppress warning policy unchanged" $
        runsOk $ lns $ fixture ++
        [ "SPAWN_RESULT = -1"
        , "-- Failure 1 at CLOCK 0."
        , "tick()"
        , "assert(CALLS.spawn == 1 and #WARNINGS == 1)"
        , "-- A sub-interval update must be throttled away entirely: the"
        , "-- spawn_interval cooldown gate at the top of tickOne is what"
        , "-- keeps a failing portal from retrying every unpaused frame."
        , "CLOCK = CLOCK + (INTERVAL / 4)"
        , "tick()"
        , "assert(CALLS.spawn == 1,"
        , "  'an update inside the cooldown must not reach unit.spawn, got '"
        , "  .. CALLS.spawn .. ' attempt(s)')"
        , "assert(#WARNINGS == 1,"
        , "  'a throttled update must not log a warning either')"
        , "assert(portalState().spawnFailures == 1,"
        , "  'a throttled update must not count as a failure')"
        , "-- Failures 2..8, each a real attempt past the cooldown. The"
        , "-- warning count is pinned after every one: attempts 1-5 each log,"
        , "-- attempt 5 adds the one suppression notice, and 6+ log nothing."
        , "local expectedWarnings = { [2] = 2, [3] = 3, [4] = 4, [5] = 6,"
        , "                           [6] = 6, [7] = 6, [8] = 6 }"
        , "for n = 2, 8 do"
        , "  tickAfterCooldown()"
        , "  assert(CALLS.spawn == n,"
        , "    'attempt ' .. n .. ' must have reached unit.spawn, got ' .. CALLS.spawn)"
        , "  assert(portalState().spawnFailures == n,"
        , "    'spawnFailures must be ' .. n .. ', got '"
        , "    .. tostring(portalState().spawnFailures))"
        , "  assert(#WARNINGS == expectedWarnings[n],"
        , "    'after failure ' .. n .. ' the warning count must be '"
        , "    .. expectedWarnings[n] .. ', got ' .. #WARNINGS)"
        , "end"
        , "-- Requirement 8: N consecutive failing cycles leave the roster"
        , "-- countdown exactly where it started."
        , "assert(REMAINING == 6,"
        , "  'eight consecutive failures must leave the roster at 6, got '"
        , "  .. tostring(REMAINING))"
        , "assert(CALLS.consumeSpawn == 0,"
        , "  'no failing cycle may consume a roster entry')"
        , "assert(CALLS.commandMove == 0,"
        , "  'no failing cycle may command a walk-out')"
        , "assert(portalState().lastUid == nil,"
        , "  'lastUid must still be untouched after eight failures')"
        , "-- The warning policy itself, spelled out."
        , "assert(countWarnings('attempt ') == 5,"
        , "  'exactly five attempt warnings must be logged, got '"
        , "  .. countWarnings('attempt '))"
        , "assert(countWarnings('suppressing further warnings') == 1,"
        , "  'exactly one suppression warning must be logged, got '"
        , "  .. countWarnings('suppressing further warnings'))"
        , "assert(#WARNINGS == 6, 'no other warning may be logged')"
        , "-- Still saveable after all of it."
        , "assert(SAVE.snapshotAll().ok,"
        , "  'a persistently failing portal must not refuse the save')"
        ]

    it "runs the success path unchanged for a positive unit id: the \
       \building's OWN page, the internal walk-out, exactly one roster \
       \entry, the stored id, a reset failure counter, and the success log" $
        runsOk $ lns $ fixture ++
        [ "-- One rejection first, so the counter reset is observable."
        , "SPAWN_RESULT = -1"
        , "tick()"
        , "assert(portalState().spawnFailures == 1)"
        , "SPAWN_RESULT = 42"
        , "tickAfterCooldown()"
        , "assert(CALLS.spawn == 2, 'the second tick must have attempted a spawn')"
        , "-- Requirement 5: the explicit owning-page argument (#196)."
        , "local args = SPAWN_ARGS[#SPAWN_ARGS]"
        , "assert(args.page == PAGE,"
        , "  \"unit.spawn must receive the building's OWN page, got \""
        , "  .. tostring(args.page))"
        , "assert(args.factionTag == 'player',"
        , "  'portal-spawned units must be player-factioned')"
        , "assert(args.defName == 'acolyte',"
        , "  'the roster index must pick the first entry, got ' .. tostring(args.defName))"
        , "assert(args.x == 10.5 and args.y == 20.5,"
        , "  'the spawn point must be the anchor plus spawn_offset, got ('"
        , "  .. tostring(args.x) .. ', ' .. tostring(args.y) .. ')')"
        , "-- The internal commandMove (#1216: internal, so the fresh acolyte"
        , "-- does not end up holding the tile the roster picked)."
        , "assert(CALLS.commandMove == 1,"
        , "  'the success path must command exactly one walk-out, got '"
        , "  .. CALLS.commandMove)"
        , "local mv = MOVE_ARGS[#MOVE_ARGS]"
        , "assert(mv.uid == 42, 'the walk-out must target the spawned unit')"
        , "assert(mv.x == 10.5 and mv.y == 22.5,"
        , "  'the walk target must be the anchor plus walk_to_offset, got ('"
        , "  .. tostring(mv.x) .. ', ' .. tostring(mv.y) .. ')')"
        , "assert(mv.speed == nil, 'no explicit speed -- the sustainable regime')"
        , "assert(mv.internal == true, 'the walk-out must be flagged internal')"
        , "-- Exactly one roster entry consumed."
        , "assert(CALLS.consumeSpawn == 1,"
        , "  'exactly one roster entry must be consumed, got ' .. CALLS.consumeSpawn)"
        , "assert(REMAINING == 5, 'the countdown must be 5, got ' .. tostring(REMAINING))"
        , "local s = portalState()"
        , "assert(s.lastUid == 42, 'the positive id must be stored, got '"
        , "  .. tostring(s.lastUid))"
        , "assert(s.lastSpawnX == 10.5 and s.lastSpawnY == 20.5,"
        , "  'the spawn point must be recorded for the cleared-tile gate')"
        , "assert(s.spawnFailures == 0,"
        , "  'a success must reset spawnFailures, got ' .. tostring(s.spawnFailures))"
        , "-- The success log."
        , "local last = INFOS[#INFOS]"
        , "assert(last:find('spawned acolyte id=42', 1, true) ~= nil,"
        , "  'the success log must name the def and the id: ' .. tostring(last))"
        , "assert(last:find('remaining=5', 1, true) ~= nil,"
        , "  'the success log must carry the new countdown: ' .. tostring(last))"
        , "-- A stored positive id is a valid typed reference on the wire."
        , "local snap = wireSnapshot()"
        , "assert(type(snap[BID].lastUid) == 'table'"
        , "  and snap[BID].lastUid.__ref == 'unit'"
        , "  and snap[BID].lastUid.id == 42,"
        , "  'lastUid must wrap to a typed unit reference on the wire')"
        , "assert(wireErrors() == nil, 'the wire snapshot must still validate')"
        , "assert(SAVE.snapshotAll().ok, 'the save must still be accepted')"
        ]
