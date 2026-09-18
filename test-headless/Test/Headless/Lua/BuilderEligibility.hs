{-# LANGUAGE TypeApplications #-}
-- | The "able-bodied builder census" gate (#2641): a dead or collapsed
--   worker beside a construction site pours no progress, inflates no
--   coordination multiplier, and reserves no recruitment slot.
--
--   Why the defect existed. @scripts/unit_ai.lua@'s collapsed\/dead
--   return suspends the tick through @core.suspendOrders@ WITHOUT
--   reselecting an action or firing the outgoing action's exit hook, so
--   @s.currentAction@ stays @"build_nearby"@ and @s.buildTarget@ stays
--   pointed at the site; @unit.getAllIds@ keeps enumerating a killed
--   instance (@src\/Unit\/Thread\/Command\/Pose.hs@ retains it with
--   @uiPose = "dead"@). Both builder counts inspected only that cached
--   AI state, so a corpse read as a working builder — and because
--   @workerRate@ is superlinear (1, 4, 9 for one to three workers), it
--   also multiplied what the LIVING workers beside it earned.
--
--   The fix is a pose test at each census
--   (@scripts\/unit_ai_stall.lua@'s @isIncapacitated@), deliberately
--   NOT a clear of the cached fields — which is what lets requirement 3
--   hold: a worker revived beside its site resumes contributing with no
--   re-selection, and nothing else that reads those two fields changes
--   meaning.
--
--   The contribution cases drive the REAL @scripts\/building_spawn.lua@
--   tick (@buildingSpawn.update@ → @constructionTickOne@ →
--   @unitAi.countAdjacentBuilders@ → @building.addBuildProgress@), with
--   only the engine globals doubled, so the assertions are about
--   committed progress rather than about the census function in
--   isolation. The recruitment cases drive the real
--   @buildNearbyUtility@, the only caller of @countBuildersAt@.
--
--   Same standalone-Lua-VM pattern as
--   "Test.Headless.Lua.BuildingSpawnSentinel" and
--   "Test.Headless.Lua.UnitAiLogisticsTargets": one self-contained
--   chunk per 'it' in a fresh interpreter, asserting inside Lua via
--   @assert()@, with a non-OK 'Lua.Status' surfaced as an hspec failure
--   carrying the Lua message. @scripts.unit_ai@ is seeded as an empty
--   singleton and then extended by the REAL @scripts.unit_ai_core@,
--   exactly as @scripts\/unit_ai.lua@ does it, so the census under test
--   is the shipped one.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "able-bodied builder census"'@.
module Test.Headless.Lua.BuilderEligibility (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run @Lua.Exception $ do
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

-- | The construction fixture: one 1x1 @workshop@ at (11,10) that is
--   @constructing@, wants 100 work, and has every material delivered,
--   plus a @UNITS@ table the cases populate with workers.
--
--   @workshop@ rather than @acolyte_portal@ deliberately: the spawn
--   sequencer's @tickOne@ returns immediately for a def it carries no
--   config for, so @update@ exercises the construction tick and
--   nothing else.
--
--   @PROGRESS@ accumulates every @building.addBuildProgress@ delta, so
--   a case asserts on what was actually committed through the progress
--   API rather than on the census return value. @workerRate@ is
--   @n * n@ for one to three workers, so at @dt = 0.1@ one worker earns
--   0.1 per tick, two earn 0.4, and three earn 0.9 — the superlinear
--   scaling is exactly what an ineligible body must not feed.
fixture ∷ Text
fixture = lns
    [ "BID, PAGE = 1, 'p'"
    , "PROGRESS = 0.0"
    , "ACTIVITY = 'constructing'"
    , "UNITS = {}   -- [uid] = { x, y, pose }"
    , "engine = { gameTime = function() return 0 end,"
    , "           isPaused = function() return false end,"
    , "           setPaused = function() end,"
    , "           logWarn = function() end, logError = function() end,"
    , "           logInfo = function() end, logDebug = function() end,"
    , "           emitEvent = function() end, emitEventAt = function() end,"
    , "           emitEventForUnit = function() end }"
    , "world = { getActiveWorldId = function() return 1 end }"
    , "debug = debug or {}"
    , "debug.recordOutcome = function() end"
    , "building = {"
    , "  getActiveIds = function() return { BID }, 1 end,"
    , "  getActivity = function() return ACTIVITY end,"
    , "  getBuildRequired = function() return 100.0 end,"
    , "  areMaterialsSatisfied = function() return true end,"
    , "  addBuildProgress = function(_, d) PROGRESS = PROGRESS + d"
    , "                                    return PROGRESS end,"
    , "  getInfo = function(bid)"
    , "    if bid ~= BID then return nil end"
    , "    return { id = BID, defName = 'workshop', gridX = 11, gridY = 10,"
    , "             tileW = 1, tileH = 1, page = PAGE } end }"
    , "unit = {"
    , "  exists = function(uid) return UNITS[uid] ~= nil end,"
    , "  getPose = function(uid)"
    , "    local u = UNITS[uid]; return u and u.pose end,"
    , "  getAllIds = function()"
    , "    local ids = {}"
    , "    for uid in pairs(UNITS) do ids[#ids + 1] = uid end"
    , "    table.sort(ids)"
    , "    return ids end,"
    , "  getInfo = function(uid)"
    , "    local u = UNITS[uid]"
    , "    if not u then return nil end"
    , "    return { gridX = u.x, gridY = u.y, page = PAGE } end }"
    -- Exactly how scripts/unit_ai.lua establishes the singleton its
    -- submodules extend: the empty table first, then the real core.
    , "package.loaded['scripts.unit_ai'] = {}"
    , "local core   = require('scripts.unit_ai_core')"
    , "local unitAi = package.loaded['scripts.unit_ai']"
    , "local BS     = require('scripts.building_spawn')"
    -- A worker standing on (10,10) -- Chebyshev 1 from the site -- with
    -- the cached build_nearby action the census reads.
    , "local function worker(uid, pose)"
    , "  UNITS[uid] = { x = 10, y = 10, pose = pose or 'standing' }"
    , "  core.aiState[uid] = { currentAction = 'build_nearby',"
    , "                        buildTarget = BID }"
    , "  return core.aiState[uid]"
    , "end"
    -- The REAL suspension path a knockdown or a death takes: it clears
    -- stall stamps and work clocks and nothing else, so the cached
    -- action and target survive it exactly as they do in game.
    , "local function incapacitate(uid, pose)"
    , "  UNITS[uid].pose = pose"
    , "  core.suspendOrders(uid)"
    , "  local s = core.aiState[uid]"
    , "  assert(s.currentAction == 'build_nearby' and s.buildTarget == BID,"
    , "    'fixture: suspension must PRESERVE the cached action/target -- '"
    , "    .. 'the census rule is what this gate tests, not a field clear')"
    , "end"
    , "local function tick(n)"
    , "  for _ = 1, n do BS.update(0.1) end"
    , "end"
    , "local function near(got, want, what)"
    , "  assert(math.abs(got - want) < 1e-6,"
    , "    (what or 'progress') .. ': expected ' .. tostring(want)"
    , "    .. ', got ' .. tostring(got))"
    , "end"
    ]

-- | The recruitment fixture, reusing
--   "Test.Headless.Lua.UnitAiLogisticsTargets"' stub world: the shipped
--   acolyte tunables, a stubbed @scripts.movement_speed@ (the real one
--   pulls in the whole physiology chain), and one constructing site.
--
--   @POSES@ is what @unit.getPose@ answers from and @RESERVERS@ what
--   @unit.getAllIds@ enumerates, so a case adds a reservation by
--   naming its uid, its pose, and the site it cached.
recruitFixture ∷ Text
recruitFixture = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "package.loaded['scripts.movement_speed'] ="
    , "  { comfort = function() return 1.0 end,"
    , "    ordered = function() return 1.0 end,"
    , "    sprint  = function() return 1.0 end }"
    , "PAGE = 'p'"
    , "POS = { gridX = 0, gridY = 0, page = PAGE }"
    , "BUILDINGS = {}"
    , "RESERVERS, POSES = {}, {}"
    , "engine = { gameTime = function() return 0 end,"
    , "           logWarn = function() end, logError = function() end,"
    , "           logInfo = function() end, logDebug = function() end,"
    , "           emitEvent = function() end, emitEventAt = function() end,"
    , "           emitEventForUnit = function() end }"
    , "world = { getActiveWorldId = function() return 1 end }"
    , "debug = debug or {}"
    , "debug.recordOutcome = function() end"
    , "local function row(bid)"
    , "  for _, b in ipairs(BUILDINGS) do if b.bid == bid then return b end end"
    , "end"
    , "building = {"
    , "  getActiveIds = function()"
    , "    local ids = {}"
    , "    for _, b in ipairs(BUILDINGS) do ids[#ids + 1] = b.bid end"
    , "    return ids end,"
    , "  getActivity = function(bid) local b = row(bid); return b and b.activity end,"
    , "  getBuildRequired = function(bid) local b = row(bid); return b and b.required end,"
    , "  getStorageCapacity = function() return 0 end,"
    , "  getStorageWeight = function() return 0 end,"
    , "  getInfo = function(bid)"
    , "    local b = row(bid)"
    , "    if not b then return nil end"
    , "    return { gridX = b.x, gridY = b.y, tileW = 1, tileH = 1,"
    , "             page = PAGE } end }"
    , "unit = {"
    , "  exists = function() return true end,"
    , "  getPose = function(uid) return POSES[uid] or 'standing' end,"
    , "  getAllIds = function() return RESERVERS end,"
    , "  getInfo = function() return POS end,"
    , "  getInventory = function() return {} end,"
    , "  getCarryingWeight = function() return 0 end,"
    , "  getStat = function(_, k)"
    , "    if k == 'carrying_capacity' then return 100 end"
    , "    return 1.0 end,"
    , "  getSkill = function() return 25.0 end,"
    , "  moveTo = function() end,"
    , "  stop = function() end,"
    , "  depositToCargo = function() return false end }"
    , "local core      = require('scripts.unit_ai_core')"
    , "local logistics = require('scripts.unit_ai_logistics')"
    , "local PARAMS    = require('scripts.unit_ai_tunables').acolyte"
    , "assert(PARAMS and PARAMS.build_saturation_n,"
    , "  'fixture: the shipped acolyte tunables must carry build_saturation_n')"
    , "local SITE = 77"
    , "BUILDINGS = { { bid = SITE, x = 5, y = 0, activity = 'constructing',"
    , "                required = 240.0 } }"
    -- A unit that has cached SITE as its build target but is not the
    -- actor -- the en-route reservation countBuildersAt exists to see.
    , "local function reserve(uid, pose)"
    , "  RESERVERS[#RESERVERS + 1] = uid"
    , "  POSES[uid] = pose or 'standing'"
    , "  core.aiState[uid] = { currentAction = 'build_nearby',"
    , "                        buildTarget = SITE }"
    , "end"
    , "local ACTOR = 1"
    , "local function actorUtility()"
    , "  local s = { currentAction = nil }"
    , "  local u = logistics.buildNearbyUtility(ACTOR, s, PARAMS)"
    , "  assert(s.buildTarget == SITE, 'fixture: the actor must resolve the site')"
    , "  return u"
    , "end"
    ]

spec ∷ Spec
spec = describe "able-bodied builder census" $ do

    describe "contribution (countAdjacentBuilders via the real construction tick)" $ do

        it "stops earning progress when the only worker dies, keeping what it earned" $
            runsOk $ lns
                [ fixture
                , "worker(1)"
                , "tick(10)"
                , "near(PROGRESS, 1.0, 'ten healthy ticks')"
                , "local earned = PROGRESS"
                , "incapacitate(1, 'dead')"
                , "tick(100)"
                , "near(PROGRESS, earned, 'progress after death')"
                ]

        it "stops earning progress when the only worker collapses" $
            runsOk $ lns
                [ fixture
                , "worker(1)"
                , "tick(10)"
                , "local earned = PROGRESS"
                , "near(earned, 1.0, 'ten healthy ticks')"
                , "incapacitate(1, 'collapsed')"
                , "tick(100)"
                , "near(PROGRESS, earned, 'progress after collapse')"
                ]

        it "resumes on revival, charging only the ticks after it stood up" $
            runsOk $ lns
                [ fixture
                , "worker(1)"
                , "incapacitate(1, 'collapsed')"
                , "tick(50)"
                , "near(PROGRESS, 0.0, 'a collapsed worker earns nothing')"
                -- No re-selection: the cached action is still the one
                -- the worker was knocked down holding.
                , "UNITS[1].pose = 'standing'"
                , "local s = core.aiState[1]"
                , "assert(s.currentAction == 'build_nearby' and s.buildTarget == BID,"
                , "  'revival must need no re-selection')"
                , "tick(10)"
                , "near(PROGRESS, 1.0, 'only the ticks after revival')"
                ]

        it "rates a mixed team by its healthy count alone" $
            runsOk $ lns
                [ fixture
                -- Three adjacent bodies, one of them working: the
                -- pre-fix census read 3 and paid R(3) = 9x, nine times
                -- what the one living worker is owed.
                , "worker(1); worker(2); worker(3)"
                , "incapacitate(2, 'dead')"
                , "incapacitate(3, 'collapsed')"
                , "tick(10)"
                , "near(PROGRESS, 1.0, 'one healthy worker of three bodies')"
                -- And the healthy count is still what scales: stand the
                -- other two up and the same ten ticks pay R(3) = 9.
                , "PROGRESS = 0.0"
                , "UNITS[2].pose = 'standing'; UNITS[3].pose = 'standing'"
                , "tick(10)"
                , "near(PROGRESS, 9.0, 'three healthy workers')"
                ]

        it "counts healthy adjacent workers exactly as before" $
            runsOk $ lns
                [ fixture
                , "worker(1)"
                , "tick(10)"
                , "near(PROGRESS, 1.0, 'one worker')"
                , "PROGRESS = 0.0"
                , "worker(2)"
                , "tick(10)"
                , "near(PROGRESS, 4.0, 'two workers')"
                -- The footprint gate is untouched: a healthy worker two
                -- tiles out still contributes nothing.
                , "PROGRESS = 0.0"
                , "worker(3); UNITS[3].x = 8"
                , "tick(10)"
                , "near(PROGRESS, 4.0, 'a non-adjacent worker must not count')"
                ]

    describe "recruitment (countBuildersAt via buildNearbyUtility)" $ do

        it "ignores a dead or collapsed reservation" $
            runsOk $ lns
                [ recruitFixture
                , "local empty = actorUtility()"
                , "assert(empty > 0, 'an unreserved site must score above zero')"
                , "reserve(2, 'dead'); reserve(3, 'collapsed')"
                , "local withBodies = actorUtility()"
                , "assert(math.abs(withBodies - empty) < 1e-9,"
                , "  'incapacitated reservations must not saturate the site: '"
                , "  .. tostring(withBodies) .. ' vs ' .. tostring(empty))"
                ]

        it "still counts a healthy en-route reservation" $
            runsOk $ lns
                [ recruitFixture
                , "local empty = actorUtility()"
                -- Deliberately NOT footprint-gated: this is the
                -- saturation count, and a worker walking in holds a
                -- slot. POS puts every unit at the origin, five tiles
                -- from the site.
                , "reserve(2, 'standing')"
                , "local reserved = actorUtility()"
                , "assert(reserved > 0 and reserved < empty,"
                , "  'a healthy reservation must lower, not erase, the score: '"
                , "  .. tostring(reserved) .. ' vs ' .. tostring(empty))"
                ]

        it "keeps excluding the actor itself" $
            runsOk $ lns
                [ recruitFixture
                , "local empty = actorUtility()"
                -- The actor's own cached reservation is enumerated like
                -- anyone else's; excludeUid is what keeps it out, and
                -- the pose rule must not become the thing doing that.
                , "reserve(ACTOR, 'standing')"
                , "local withSelf = actorUtility()"
                , "assert(math.abs(withSelf - empty) < 1e-9,"
                , "  'the actor must not count itself: ' .. tostring(withSelf)"
                , "  .. ' vs ' .. tostring(empty))"
                ]

        it "turns no one away once a saturating team is incapacitated" $
            runsOk $ lns
                [ recruitFixture
                , "for uid = 2, PARAMS.build_saturation_n + 1 do"
                , "  reserve(uid, 'standing')"
                , "end"
                , "assert(actorUtility() == -math.huge,"
                , "  'a fully reserved site must reject a new worker')"
                , "for uid = 2, PARAMS.build_saturation_n + 1 do"
                , "  POSES[uid] = 'collapsed'"
                , "end"
                , "assert(actorUtility() > 0,"
                , "  'a site whose whole team is down must be open again')"
                ]
