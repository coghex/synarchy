{-# LANGUAGE TypeApplications #-}
-- | The "bounded work clocks" gate (#2332): the craft @working@ phase
--   and the construct @building@ phase charge elapsed time only for one
--   uninterrupted stretch of AI ticking, exactly as auto-harvest's
--   picking clock already did (#1291 \/ #1582).
--
--   Before this, @scripts\/unit_ai_craft.lua@ charged a bare
--   @now - s.lastCraftAt@ and @scripts\/unit_ai_construct.lua@ a bare
--   @now - s.lastConstructAt@, with no bound and no boundary on the two
--   tick-swallowing returns in @scripts\/unit_ai.lua@ (the
--   collapsed\/dead pose and the drinking\/eating\/pickup\/transitioning
--   activity). Both funnel into @unit_ai_stall.suspendOrders@, which
--   cleared only @lastHarvestAt@ — so a crafter or builder knocked down
--   mid-pour kept its phase and its stale stamp, and the first tick
--   after it stood landed the WHOLE collapse as instant progress. The
--   same stamp also spans a gap no path can announce at all: a hidden
--   world page is not enumerated for AI while the session clock keeps
--   running, and only the interval bound covers that.
--
--   What the cases pin, per requirement:
--
--   * The unannounced gap (requirement 1): an interval past
--     @stall.MAX_CHARGED_INTERVAL@ charges ZERO, not the bound — for
--     both clocks — while an interval exactly EQUAL to it is still one
--     uninterrupted stretch and charges in full. That threshold pair is
--     what separates "bounded" from "clamped": a clamp would still
--     credit five seconds of work that never happened.
--   * The announced interruption (requirement 2): repeated
--     @core.suspendOrders@ ticks — the dispatcher's own swallowed-tick
--     call, firing no @onExit@ — charge nothing, over a span kept
--     deliberately UNDER the bound so the backstop above cannot cover
--     for a missing boundary, and long enough that a charged one would
--     finish the job outright.
--   * The craft working flag (requirement 3): the bill goes
--     non-working for the length of the collapse (so its station stops
--     drawing power for it, @Craft.Bills.cbWorking@), the job is demoted
--     to a phase that re-arms the flag, and the re-entry tick itself
--     pours nothing.
--   * The control: an ordinary 0.5 s interval still pours, so none of
--     the above passes by simply never charging anything.
--   * Requirement 5: the job, its claim, its consumed materials and its
--     banked progress all survive every boundary.
--
--   Same standalone-Lua-VM pattern as "Test.Headless.Lua.UnitAiStall"
--   and "Test.Headless.Lua.WorkClaimCapacity": each 'it' runs one
--   self-contained chunk via 'Lua.dostring' in a fresh interpreter,
--   asserting inside Lua via @assert()@, with a non-OK 'Lua.Status'
--   surfaced as an hspec failure carrying the Lua message. No engine and
--   no GPU — the clock, the station, the designation and the unit are
--   all stubs the chunk drives tick by tick.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "bounded work clocks"'@.
module Test.Headless.Lua.WorkClockBounds (spec) where

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

-- | Engine-API stubs both halves share: one page, one worker at the
--   origin, a stubbed game clock, and the @scripts.movement_speed@
--   module replaced at @package.loaded@ (only @comfort@ is reached, and
--   the real one pulls in the whole physiology chain).
--
--   @scripts.unit_roles@ is the REAL module — a role-less state weighs
--   1.0 — because the arbitration multiplier is not what this gate
--   varies.
commonPrelude ∷ Text
commonPrelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "package.loaded['scripts.movement_speed'] ="
    , "  { comfort = function() return 1.0 end,"
    , "    ordered = function() return 1.0 end,"
    , "    sprint  = function() return 1.0 end }"
    , "NOW = 0"
    , "PAGE = 'stub_page'"
    , "POS = { gridX = 0, gridY = 0, page = PAGE }"
    , "WARNS = 0"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function() WARNS = WARNS + 1 end,"
    , "           logError = function() WARNS = WARNS + 1 end,"
    , "           logInfo = function() end, logDebug = function() end,"
    , "           emitEvent = function() end,"
    , "           emitEventAt = function() end,"
    , "           emitEventForUnit = function() end }"
    , "world = { getActiveWorldId = function() return 1 end }"
    , "debug = debug or {}"
    , "debug.recordOutcome = function() end"
    , "unit = {"
    , "  exists = function() return true end,"
    , "  getAllIds = function() return { 1 } end,"
    , "  getInfo = function() return POS end,"
    , "  getInventory = function() return {} end,"
    , "  getCarryingWeight = function() return 0 end,"
    , "  getStat = function(_, k)"
    , "    if k == 'carrying_capacity' then return math.huge end"
    , "    return 1.0 end,"
    -- Skill 25 is the novice base both pour rates scale by
    -- (0.5 + 25/100 = 0.75); the cases derive their expectations from
    -- RATE below rather than hard-coding a number here.
    , "  getSkill = function() return 25.0 end,"
    , "  getKnowledge = function() return true end,"
    , "  getMentalEffectiveness = function() return 1.0 end,"
    , "  addXP = function() end,"
    , "  moveTo = function() end,"
    , "  stop = function() end,"
    , "  dropItemById = function() end,"
    , "  removeItem = function() return true end }"
    , "item = { listDefs = function() return {} end,"
    , "         listGround = function() return {} end,"
    , "         getGroundForUnit = function() return nil, true end,"
    , "         pickupGround = function() return false end }"
    , "local core = require('scripts.unit_ai_core')"
    , "STALL = require('scripts.unit_ai_stall')"
    , "BOUND = STALL.MAX_CHARGED_INTERVAL"
    -- The state table the DISPATCHER itself would pass, so
    -- core.suspendOrders(1) reaches this very case's `S` — the whole
    -- point of the announced-interruption cases.
    , "S = core.ensureState(1)"
    -- scripts/unit_ai.lua's pose/activity short-circuit: it returns
    -- core.suspendOrders(uid) BEFORE arbitration, so no action's onExit
    -- runs and no utility is scored. `seconds` of collapse, sampled at
    -- the ordinary thought cadence, is exactly what the real dispatcher
    -- does over a knockdown.
    , "function collapse(seconds)"
    , "  local left = seconds"
    , "  while left > 1e-9 do"
    , "    local dt = math.min(0.5, left)"
    , "    NOW = NOW + dt"
    , "    core.suspendOrders(1)"
    , "    left = left - dt"
    , "  end"
    , "end"
    -- A gap NO path announced: the clock simply jumps with no sample
    -- and no suspendOrders call anywhere (a save/load boundary, or a
    -- page hidden while a second visible page keeps the session clock
    -- running).
    , "function jump(seconds) NOW = NOW + seconds end"
    ]

-- | The craft half. One built station (id 7) two tiles east of the
--   worker, and one standing bill on it running a recipe with NO inputs
--   and no fuel, so the fetch phase is a formality and every case
--   reaches the @working@ pour in a fixed number of ticks.
--
--   The @craft.*@ stubs behave the way the engine does where this gate
--   depends on it: @setBillWorking@ really writes the @working@ field
--   @getBill@ answers with (that is @Craft.Bills.cbWorking@, the flag
--   @Power.Network.activeCraftConsumersOn@ keys a station's live demand
--   off), and @addBillProgress@ really accumulates and returns the
--   bill's progress. @CYCLES@ counts completions, so "a charged
--   interruption would have finished the job" is observable rather than
--   inferred.
craftPrelude ∷ Text
craftPrelude = lns
    [ commonPrelude
    , "WORK = 100.0"
    , "CYCLES, EXECUTED = 0, 0"
    -- Adjacent to the worker (Chebyshev 1 to the footprint), so
    -- moveBesideBuilding is satisfied on arrival and the fixture never
    -- depends on a stubbed unit.moveTo actually moving anything.
    , "STATION = { gridX = 1, gridY = 0, tileW = 1, tileH = 1, page = PAGE }"
    , "BILL = { id = 1, recipe = 'stub_recipe', station = 7,"
    , "         mode = 'count', progress = 0 }"
    , "CLAIMS = 0"
    , "POWERED = true"
    , "building = {"
    , "  getActiveIds = function() return { 7 } end,"
    , "  getActivity = function() return 'built' end,"
    , "  getStorage = function() return {} end,"
    , "  getInfo = function(bid) if bid == 7 then return STATION end end }"
    , "power = { isStationPoweredForRecipe = function() return POWERED end }"
    -- #2325: the bill lifecycle verbs take the ACTING UNIT first.
    , "craft = {"
    , "  getBills = function() return { BILL } end,"
    , "  getBill = function(_, id) if id == BILL.id then return BILL end end,"
    , "  get = function(rid)"
    , "    if rid ~= 'stub_recipe' then return nil end"
    , "    return { id = 'stub_recipe', work = WORK, skill = 'smithing',"
    , "             inputs = {}, outputs = {} } end,"
    , "  claimBill = function(id, uid)"
    , "    CLAIMS = CLAIMS + 1; BILL.claimant = uid"
    , "    BILL.claimedAt = NOW; return true end,"
    , "  releaseBill = function() BILL.claimant = nil end,"
    , "  setBillWorking = function(_, id, flag)"
    , "    if id ~= BILL.id then return false end"
    , "    BILL.working = flag; return true end,"
    , "  addBillProgress = function(_, id, delta)"
    , "    if id ~= BILL.id then return nil end"
    , "    BILL.progress = (BILL.progress or 0) + delta"
    , "    return BILL.progress end,"
    , "  completeBillCycle = function()"
    , "    CYCLES = CYCLES + 1; BILL.progress = 0; return 0 end,"
    , "  executeAt = function() EXECUTED = EXECUTED + 1; return true, {} end }"
    , "local craftAi = require('scripts.unit_ai_craft')"
    , "PARAMS = { craft_scan_range = 30.0, craft_base_utility = 3.2,"
    , "           craft_lock_utility = 6.0, craft_rate = 1.0,"
    , "           craft_claim_timeout = 30.0, craft_xp_per_craft = 1.5,"
    , "           pickup_arrival_tiles = 1.2, mule_fetch_arrival = 1.5 }"
    -- One arbitration pass over the single action under test: advance
    -- the clock, score, and execute only if it won -- exactly what
    -- scripts/unit_ai.lua does for an idle unit.
    , "function step(dt)"
    , "  NOW = NOW + dt"
    , "  local u = craftAi.craftUtility(1, S, PARAMS)"
    , "  if u > -math.huge then craftAi.craftExecute(1, S, PARAMS) end"
    , "end"
    , "function progress() return BILL.progress or 0 end"
    -- Progress per second of CHARGED pour: craft_rate * (0.5 + skill/100)
    -- * mental effectiveness / job.work. Derived rather than written
    -- out, so a rate or skill change here cannot make a case vacuous.
    , "RATE = PARAMS.craft_rate * (0.5 + 25.0 / 100.0) * 1.0 / WORK"
    -- Drive fetch -> walking -> working. The walking->working transition
    -- stamps the clock and returns, so this leaves the job armed with
    -- zero charged time and zero progress.
    , "function reachWorking()"
    , "  for _ = 1, 6 do"
    , "    step(0.5)"
    , "    if S.craftJob and S.craftJob.phase == 'working' then return end"
    , "  end"
    , "  error('fixture never reached the craft working phase')"
    , "end"
    ]

-- | The construction half. One pending, already-PAID structure
--   designation on the tile east of the worker, so the walking phase
--   pays nothing and the case reaches the @building@ pour directly.
--
--   Both representations requirement 2 names are observed:
--   @S.constructJob.progress@, the local copy the module keeps, and
--   @DESIGN_PROGRESS@, the engine-facing total accumulated through
--   @construction.addJobProgress@. A boundary that reset one and not the
--   other fails here.
--
--   @structure.packBuildCost@ is deliberately ABSENT so
--   @site.packBuildInfo@ takes its documented @engine.loadYaml@ fallback
--   over the stub pack below, and @construction.resolvePlan@ is absent
--   so @site.planOutcome@ degrades to @\"valid\"@ — the bare-Lua path
--   that module already documents.
constructPrelude ∷ Text
constructPrelude = lns
    [ commonPrelude
    , "WORK = 100.0"
    , "PLACED = 0"
    , "DESIGN_PROGRESS = 0"
    -- build_work is where construct's job.work comes from
    -- (unit_ai_construct.lua's `cand.build.build_work or 1.0`), so the
    -- pour rate RATE derives from below is the one the module uses.
    , "PACK = { build = { floor = { materials = {}, build_work = WORK } } }"
    , "engine.loadYaml = function() return PACK end"
    , "JOB = { x = 1, y = 0, lx = 1, ly = 0, status = 'pending',"
    , "        category = 'structure', pack = 'stub_pack', kind = 'floor',"
    , "        edge = 'ne', paid = true, attempt = 1 }"
    , "construction = {"
    , "  getPendingJobs = function() return { JOB } end,"
    , "  setJobStatus = function(_, _, _, st) JOB.status = st end,"
    , "  getDesignationAt = function(_, x, y)"
    , "    if x == JOB.x and y == JOB.y then return JOB end end,"
    , "  addJobProgress = function(_, _, _, delta)"
    , "    DESIGN_PROGRESS = DESIGN_PROGRESS + delta end,"
    , "  setMaterialsPaid = function() end,"
    , "  beginPlacement = function() return true end,"
    , "  abortPlacement = function() return nil end,"
    , "  cancelDesignation = function() end,"
    , "  cancelDesignationForRefund = function() end }"
    , "structure = { floorZAt = function() return 0 end,"
    , "              hasAt = function() return false end }"
    -- The real scripts.structures pulls in the whole build-tool chain;
    -- only the one placement verb this job kind uses is reached.
    , "package.loaded['scripts.structures'] ="
    , "  { floor = function() PLACED = PLACED + 1; return true end }"
    , "building = { spawn = function() return 1 end,"
    , "             getInfo = function() return nil end }"
    , "local constructAi = require('scripts.unit_ai_construct')"
    , "PARAMS = { construct_scan_range = 30.0, construct_scan_chunks = 2,"
    , "           construct_arrival_tiles = 1.5, construct_base_utility = 3.5,"
    , "           construct_lock_utility = 6.0, construct_rate = 1.0,"
    , "           construct_claim_timeout = 30.0, construct_xp_per_piece = 1.0,"
    , "           pickup_arrival_tiles = 1.2, mule_fetch_arrival = 1.5 }"
    , "function step(dt)"
    , "  NOW = NOW + dt"
    , "  local u = constructAi.constructUtility(1, S, PARAMS)"
    , "  if u > -math.huge then constructAi.constructExecute(1, S, PARAMS) end"
    , "end"
    , "function progress() return (S.constructJob and S.constructJob.progress) or 0 end"
    , "RATE = PARAMS.construct_rate * (0.5 + 25.0 / 100.0) / WORK"
    , "function reachBuilding()"
    , "  for _ = 1, 6 do"
    , "    step(0.5)"
    , "    if S.constructJob and S.constructJob.phase == 'building' then return end"
    , "  end"
    , "  error('fixture never reached the construct building phase')"
    , "end"
    ]

-- | @assert@ over floating-point progress, with the observed value in
--   the message: every expectation here is derived from @RATE@, so a
--   bare equality would be both fragile and unreadable when it failed.
near ∷ Text
near = lns
    [ "function near(actual, expected, what)"
    , "  local tol = math.max(1e-9, math.abs(expected) * 1e-6)"
    , "  if math.abs(actual - expected) > tol then"
    , "    error(what .. ': expected ' .. tostring(expected)"
    , "          .. ', got ' .. tostring(actual), 2)"
    , "  end"
    , "end"
    ]

spec ∷ Spec
spec = describe "bounded work clocks" $ do

    describe "the craft working phase charges one uninterrupted stretch" $ do

        it "charges ZERO for an interval past MAX_CHARGED_INTERVAL — a \
           \gap no path announced, which a clamp would still credit" $
            runsOk $ lns
                [ craftPrelude, near
                , "reachWorking()"
                , "step(0.5)"
                , "local partial = progress()"
                , "near(partial, RATE * 0.5, 'the first charged pour')"
                -- Big enough that a charged interval would run the bill
                -- clean past 1.0 and fire the craft, so "unchanged" is
                -- not merely "slightly less".
                , "jump(BOUND + 600)"
                , "step(0)"
                , "near(progress(), partial,"
                , "     'an unannounced gap must charge nothing at all')"
                , "assert(EXECUTED == 0 and CYCLES == 0,"
                , "  'and must not complete a craft cycle')"
                -- Requirement 5: the job and its claim survive.
                , "assert(S.craftJob and S.craftJob.phase == 'working',"
                , "  'the job must survive the gap, still working')"
                , "assert(BILL.claimant == 1, 'and keep its claim')"
                -- The control: the very next ordinary interval pours.
                , "step(0.5)"
                , "near(progress(), partial + RATE * 0.5,"
                , "     'the next ordinary interval must still pour')"
                ]

        it "charges an interval exactly EQUAL to MAX_CHARGED_INTERVAL in \
           \full — the bound is inclusive, so the rule is a threshold \
           \and not an off-by-one clamp" $
            runsOk $ lns
                [ craftPrelude, near
                , "reachWorking()"
                , "step(BOUND)"
                , "near(progress(), RATE * BOUND,"
                , "     'an interval equal to the bound must charge in full')"
                -- ...and one hair past it charges nothing, from the same
                -- armed state. The pair is what pins the comparison.
                , "local partial = progress()"
                , "jump(BOUND + 1e-3)"
                , "step(0)"
                , "near(progress(), partial,"
                , "     'one hair past the bound must charge nothing')"
                ]

        it "charges nothing for a collapse the dispatcher swallows \
           \through core.suspendOrders, which fires no onExit" $
            runsOk $ lns
                [ craftPrelude, near
                , "reachWorking()"
                , "step(0.5)"
                , "local partial = progress()"
                , "assert(partial > 0, 'work must be underway')"
                -- Deliberately UNDER the bound, so the backstop above
                -- cannot cover for a missing suspendOrders boundary,
                -- and long enough that charging it would be plain.
                , "local span = BOUND * 0.8"
                , "assert(span < BOUND, 'the collapse must stay inside the bound')"
                , "collapse(span)"
                , "near(progress(), partial,"
                , "     'a swallowed tick must charge the pour nothing')"
                -- Re-entry: the first tick back re-arms and pours
                -- nothing, and only the tick AFTER it charges.
                , "step(0.5)"
                , "near(progress(), partial,"
                , "     'the re-entry tick must pour nothing either')"
                , "step(0.5)"
                , "near(progress(), partial + RATE * 0.5,"
                , "     'and the next ordinary tick pours again')"
                ]

        it "would have finished the bill outright had the collapse been \
           \charged — the fixture proves the boundary is load-bearing" $
            runsOk $ lns
                [ craftPrelude, near
                , "reachWorking()"
                -- Pour to just under completion, then collapse for
                -- longer than the remainder: a charged span finishes the
                -- craft, an uncharged one leaves it exactly where it was.
                , "step(0.5)"
                , "BILL.progress = 1.0 - RATE * 0.4"
                , "local partial = progress()"
                , "local span = BOUND * 0.8"
                , "assert(span < BOUND, 'the collapse must stay inside the bound')"
                , "assert(RATE * span > 1.0 - partial,"
                , "  'the collapse must be long enough to finish the bill')"
                , "collapse(span)"
                -- The tick back is the re-entry; a charged collapse
                -- would have fired the craft on it.
                , "step(0.5)"
                , "assert(EXECUTED == 0 and CYCLES == 0,"
                , "  'a collapse must never complete a craft by itself')"
                , "near(progress(), partial, 'and must bank no progress')"
                -- ...and one ordinary tick of its own time then does
                -- finish it, so the case is not merely stuck.
                , "step(0.5)"
                , "assert(EXECUTED == 1 and CYCLES == 1,"
                , "  'the resumed pour must finish the bill on its own time')"
                ]

        it "drops the bill's working flag for the length of the \
           \collapse and re-arms it before the pour resumes (#590 — a \
           \collapsed crafter must not keep its station drawing power)" $
            runsOk $ lns
                [ craftPrelude, near
                , "reachWorking()"
                , "assert(BILL.working == true,"
                , "  'the working phase must mark the bill working')"
                , "step(0.5)"
                , "collapse(BOUND * 0.8)"
                , "assert(BILL.working == false,"
                , "  'a collapsed crafter must not keep the bill working')"
                -- Clearing the flag alone would be a one-way trip: only
                -- the walking->working transition calls setBillWorking
                -- (true), so the phase has to be demoted too.
                , "assert(S.craftJob and S.craftJob.phase ~= 'working',"
                , "  'the job must be demoted to a phase that re-arms it')"
                , "step(0.5)"
                , "assert(BILL.working == true,"
                , "  'and re-entry must re-arm the flag')"
                , "assert(S.craftJob.phase == 'working',"
                , "  'back in the working phase')"
                , "local partial = progress()"
                , "step(0.5)"
                , "near(progress(), partial + RATE * 0.5,"
                , "     'the resumed pour advances again')"
                ]

        it "drops the last-sample stamp itself, so the boundary holds \
           \even where the phase machine would not re-stamp it" $
            runsOk $ lns
                [ craftPrelude, near
                , "reachWorking()"
                , "step(0.5)"
                , "assert(S.lastCraftAt ~= nil,"
                , "  'the pour must be keeping a last-sample stamp')"
                , "collapse(0.5)"
                -- The mechanism unit_ai_stall documents, asserted
                -- directly: M.workInterval reads a MISSING stamp as a
                -- zero-length interval, so dropping it is what makes
                -- the swallowed interval cost nothing. The phase
                -- demotion happens to re-stamp on re-entry as well, and
                -- this keeps the two independent -- a phase machine
                -- that changed would not silently unbound the clock.
                , "assert(S.lastCraftAt == nil,"
                , "  'a swallowed tick must drop the craft stamp')"
                ]

    describe "the construct building phase charges one uninterrupted \
             \stretch" $ do

        it "charges ZERO for an interval past MAX_CHARGED_INTERVAL, on \
           \BOTH the local job copy and the engine-facing designation \
           \progress" $
            runsOk $ lns
                [ constructPrelude, near
                , "reachBuilding()"
                , "step(0.5)"
                , "local partial = progress()"
                , "near(partial, RATE * 0.5, 'the first charged pour')"
                , "near(DESIGN_PROGRESS, partial,"
                , "     'the designation must record the same pour')"
                , "jump(BOUND + 600)"
                , "step(0)"
                , "near(progress(), partial,"
                , "     'an unannounced gap must charge the job nothing')"
                , "near(DESIGN_PROGRESS, partial,"
                , "     'nor the designation')"
                , "assert(PLACED == 0, 'and must not place the piece')"
                , "assert(S.constructJob, 'the job must survive the gap')"
                , "assert(S.constructJob.consumed,"
                , "  'with its paid materials still settled')"
                , "step(0.5)"
                , "near(progress(), partial + RATE * 0.5,"
                , "     'the next ordinary interval must still pour')"
                , "near(DESIGN_PROGRESS, progress(),"
                , "     'and the designation tracks it')"
                ]

        it "charges an interval exactly EQUAL to MAX_CHARGED_INTERVAL in \
           \full, and one hair past it nothing" $
            runsOk $ lns
                [ constructPrelude, near
                , "reachBuilding()"
                , "step(BOUND)"
                , "near(progress(), RATE * BOUND,"
                , "     'an interval equal to the bound must charge in full')"
                , "local partial = progress()"
                , "jump(BOUND + 1e-3)"
                , "step(0)"
                , "near(progress(), partial,"
                , "     'one hair past the bound must charge nothing')"
                ]

        it "charges nothing for a collapse the dispatcher swallows \
           \through core.suspendOrders, and would have placed the piece \
           \had it been charged" $
            runsOk $ lns
                [ constructPrelude, near
                , "reachBuilding()"
                , "step(0.5)"
                , "S.constructJob.progress = 1.0 - RATE * 0.4"
                , "local partial = progress()"
                , "local designBefore = DESIGN_PROGRESS"
                , "local span = BOUND * 0.8"
                , "assert(span < BOUND, 'the collapse must stay inside the bound')"
                , "assert(RATE * span > 1.0 - partial,"
                , "  'and be long enough to finish the piece')"
                , "collapse(span)"
                , "near(progress(), partial,"
                , "     'a swallowed tick must charge the pour nothing')"
                , "near(DESIGN_PROGRESS, designBefore,"
                , "     'nor the designation')"
                , "assert(S.constructJob.phase ~= 'building',"
                , "  'the job must be demoted so re-entry re-stamps the clock')"
                -- Re-entry pours nothing; only the tick after it does.
                , "step(0.5)"
                , "near(progress(), partial, 'the re-entry tick pours nothing')"
                , "assert(PLACED == 0,"
                , "  'a collapse must never place the piece by itself')"
                , "assert(S.constructJob.consumed,"
                , "  'and the paid materials stay settled')"
                , "step(0.5)"
                , "assert(PLACED == 1,"
                , "  'the resumed pour then finishes it on its own time')"
                ]

        it "drops the last-sample stamp itself, for the same reason the \
           \craft clock does" $
            runsOk $ lns
                [ constructPrelude, near
                , "reachBuilding()"
                , "step(0.5)"
                , "assert(S.lastConstructAt ~= nil,"
                , "  'the pour must be keeping a last-sample stamp')"
                , "collapse(0.5)"
                , "assert(S.lastConstructAt == nil,"
                , "  'a swallowed tick must drop the construct stamp')"
                ]
