{-# LANGUAGE TypeApplications #-}
-- | The "commanded order stall budget" gate (#1291):
--   @scripts/unit_ai_stall.lua@'s eligible-time accounting, the
--   @maintainTask@ housekeeping built on it, and
--   @scripts/unit_ai_pickup.lua@'s @pickup_timeout@ sharing it — an
--   interruption of any length costs a pending order nothing, an order
--   the unit IS free to pursue and makes no headway on still expires
--   on schedule, and the budget accumulates ACROSS interruptions rather
--   than restarting after one.
--
--   Since #1769 it also covers the REPORT the commanded-move branch
--   files when it gives up: exactly one @unit_warning@ naming the unit
--   and the order, for a PLAYER order only, and silent when the
--   stuck-walk watchdog already reported that same walk.
--
--   Same standalone-Lua-VM pattern as
--   "Test.Headless.Lua.UnitAiLocations": each 'it' runs one
--   self-contained chunk via 'Lua.dostring' in a fresh interpreter,
--   asserting inside Lua via @assert()@/@error()@, with a non-OK
--   'Lua.Status' surfaced as an hspec failure carrying the Lua message.
--   @scripts/unit_ai_stall.lua@ requires nothing at module scope and
--   takes @now@ from its caller, so the whole clock is a stub here;
--   @maintainTask@ additionally reaches @engine.gameTime@/@unit.getInfo@,
--   which the fixture below drives frame by frame.
--
--   The persistence cases run the REAL @lua.unit_ai@ component
--   (@scripts/unit_ai_save.lua@ + @scripts/lib/save_modules.lua@ +
--   @scripts/lib/data_codec.lua@) end to end — snapshot, encode,
--   decode, apply — the same way "Test.Headless.Lua.SaveModules" does,
--   so a partially consumed budget is proven to survive a real round
--   trip and a pre-#1291 payload is proven to decode to the honest
--   default rather than to an order that can never expire.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "commanded order stall budget"'@.
module Test.Headless.Lua.UnitAiStall (spec) where

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

-- | A stubbed clock, a stubbed unit position, and @tick(seconds,
--   action)@ — one AI update at a time, advancing the clock in
--   sub-second steps the way the real update loop does, with
--   @s.currentAction@ naming whichever action arbitration left in
--   control over each interval. @place(x, y)@ moves the unit.
--
--   @stall.MAX_CHARGED_INTERVAL@ separates "the AI kept ticking" from
--   "the AI was not running at all" (a collapse, an engine animation,
--   a load boundary), so the fixture models the second as @skip@: the
--   clock jumps and no sample is taken, exactly like the tickOne
--   short-circuit returning before @maintainTask@.
prelude ∷ Text
prelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "local stall = require('scripts.unit_ai_stall')"
    , "NOW = 0"
    -- Named, because the abandonment report below must identify the
    -- unit in its own text (#1769), not only by the uid it is filed
    -- against.
    , "POS = { gridX = 0, gridY = 0, name = 'Dorian Kessler' }"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function() end, logInfo = function() end,"
    , "           emitEventForUnit = function(cat, msg, uid)"
    , "             EVENTS = EVENTS or {}"
    , "             EVENTS[#EVENTS + 1] ="
    , "               { cat = cat, msg = msg, uid = uid } end }"
    , "unit = { getInfo = function() return POS end }"
    , "local s = { commandedTask = nil, currentAction = nil }"
    , "local STEP = 0.25"
    , "local function place(x, y) POS.gridX, POS.gridY = x, y end"
    , "-- Run `seconds` of AI ticks with `action` in control."
    , "local function tick(seconds, action)"
    , "  local left = seconds"
    , "  while left > 1e-9 do"
    , "    local dt = math.min(STEP, left)"
    , "    NOW = NOW + dt"
    , "    s.currentAction = action"
    , "    stall.maintainTask(1, s)"
    , "    left = left - dt"
    , "  end"
    , "end"
    , "-- Time in which the AI never ran and nothing announced it: the"
    , "-- clock advances, no sample is taken (a save/load boundary, a"
    , "-- unit that stopped being ticked)."
    , "local function skip(seconds) NOW = NOW + seconds end"
    , "-- The same, but through a path that DOES announce itself: one"
    , "-- suspendOrders per swallowed tick, exactly as unit_ai.lua's"
    , "-- collapsed-pose / engine-animation short-circuit and"
    , "-- unit_ai_mental.lua's preempt call it."
    , "local function shortCircuit(seconds)"
    , "  local left = seconds"
    , "  while left > 1e-9 do"
    , "    local dt = math.min(STEP, left)"
    , "    NOW = NOW + dt"
    , "    stall.suspendOrders(s)"
    , "    left = left - dt"
    , "  end"
    , "end"
    ]

-- | The pickup half of the same accounting: @pickupUtility@ reached
--   through the real @scripts/unit_ai_pickup.lua@, whose module scope
--   needs the @unit_ai@ singleton, @scripts.unit_ai_core@ and
--   @scripts.movement_speed@ to resolve. Only the globals those touch
--   at CALL time need stubbing.
pickupPrelude ∷ Text
pickupPrelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "NOW = 0"
    , "POS = { gridX = 0, gridY = 0 }"
    , "GROUND = { { id = 7, defName = 'radio', x = 40, y = 0, weight = 1 } }"
    , "EVENTS = {}"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function() end, logInfo = function() end,"
    , "           emitEventForUnit = function(cat, msg)"
    , "             EVENTS[#EVENTS + 1] = { cat = cat, msg = msg } end }"
    , "unit = { getInfo = function() return POS end,"
    , "         getCarryingWeight = function() return 0 end,"
    , "         getStat = function() return 100 end }"
    , "item = { listGround = function() return GROUND end,"
    , "-- #1666: the pickup order reads the CARRIER'S OWN page. This"
    , "-- fixture has exactly one page, so the owning-page lookup is"
    , "-- the same GROUND table listGround answers from — resolved"
    , "-- (second return) is always true, which is what lets a missing"
    , "-- id still mean 'gone' here."
    , "         getGroundForUnit = function(_, gid)"
    , "           for _, g in ipairs(GROUND) do"
    , "             if g.id == gid then return g, true end end"
    , "           return nil, true end,"
    , "         listDefs = function() return"
    , "           { { name = 'radio', displayName = 'Field Radio',"
    , "               weight = 1 } } end }"
    , "local pickup = require('scripts.unit_ai_pickup')"
    , "local stall = require('scripts.unit_ai_stall')"
    , "local PARAMS = { pickup_timeout = 30, pickup_utility = 7.5 }"
    , "local s = { pickupOrder = { gid = 7, issuedAt = 0 },"
    , "            currentAction = nil }"
    , "local STEP = 1.0   -- pickup scores on the ~1 s thought tick"
    , "local function place(x, y) POS.gridX, POS.gridY = x, y end"
    , "local function tick(seconds, action)"
    , "  local left = seconds"
    , "  while left > 1e-9 and s.pickupOrder do"
    , "    local dt = math.min(STEP, left)"
    , "    NOW = NOW + dt"
    , "    s.currentAction = action"
    , "    pickup.pickupUtility(1, s, PARAMS)"
    , "    left = left - dt"
    , "  end"
    , "end"
    , "local function skip(seconds) NOW = NOW + seconds end"
    , "local function shortCircuit(seconds)"
    , "  local left = seconds"
    , "  while left > 1e-9 do"
    , "    local dt = math.min(STEP, left)"
    , "    NOW = NOW + dt"
    , "    stall.suspendOrders(s)"
    , "    left = left - dt"
    , "  end"
    , "end"
    ]

-- | The commanded-move ACTION itself, reached through the real
--   @scripts/unit_ai_combat.lua@ — the one place a commanded task is
--   handed to @unit.moveTo@, and therefore the one place the stall
--   module's walk attribution (#1769) is recorded from. An
--   explicit-@speed@ task takes @followCommandExecute@'s early-return
--   branch, so @unit.moveTo@ is the only global it reaches at call
--   time. @MOVES@ records what the engine was actually told to walk.
combatPrelude ∷ Text
combatPrelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "NOW = 0"
    , "POS = { gridX = 0, gridY = 0, name = 'Dorian Kessler' }"
    , "MOVES = {}"
    , "EVENTS = {}"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function() end, logInfo = function() end,"
    , "           emitEventForUnit = function(cat, msg, uid)"
    , "             EVENTS[#EVENTS + 1] ="
    , "               { cat = cat, msg = msg, uid = uid } end }"
    , "unit = { getInfo = function() return POS end,"
    , "         moveTo = function(_, x, y)"
    , "           MOVES[#MOVES + 1] = { x = x, y = y } end }"
    , "local combat = require('scripts.unit_ai_combat')"
    , "local stall = require('scripts.unit_ai_stall')"
    , "local s = { commandedTask = nil, currentAction = nil }"
    , "local STEP = 0.25"
    , "local function tick(seconds, action)"
    , "  local left = seconds"
    , "  while left > 1e-9 do"
    , "    local dt = math.min(STEP, left)"
    , "    NOW = NOW + dt"
    , "    s.currentAction = action"
    , "    stall.maintainTask(1, s)"
    , "    left = left - dt"
    , "  end"
    , "end"
    , "local function command(tx)"
    , "  s.commandedTask = { x = tx, y = 0, speed = 1.0,"
    , "                      startedAt = NOW, player = true }"
    , "end"
    ]

-- | The real @lua.unit_ai@ component, registered against a fake
--   @aiState@ the case fills in. @unit.exists@ is the only global its
--   snapshot path reaches for a state table carrying no job/claim
--   fields.
savePrelude ∷ Text
savePrelude = lns
    [ "engine = { logWarn = function() end, logInfo = function() end }"
    , "unit = { exists = function() return true end }"
    , "local unitAiSave = require('scripts.unit_ai_save')"
    , "local saveModules = require('scripts.lib.save_modules')"
    , "local codec = require('scripts.lib.data_codec')"
    , "local aiState = {}"
    , "unitAiSave.register(aiState)"
    , "local spec = saveModules.registry.unit_ai"
    ]

spec ∷ Spec
spec = describe "commanded order stall budget" $ do

    describe "a pending commanded move survives an interruption" $ do
        it "is still pending after an interruption many times longer \
           \than TASK_TIMEOUT_SEC, and resumes closing on its target" $
            runsOk $ lns
                [ prelude
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW }"
                , "tick(5, 'follow_command')"
                , "assert(s.commandedTask, 'the order must survive its own first ticks')"
                -- treat_ally / combat / a dry-canteen refill: another
                -- action owns the unit, for four times the budget.
                , "tick(240, 'treat_ally')"
                , "assert(s.commandedTask,"
                , "  'an interruption longer than the budget must not expire the order')"
                -- ...and the unit resumes, closing on the target.
                , "place(20, 0)"
                , "tick(5, 'follow_command')"
                , "assert(s.commandedTask, 'the resumed order must still be pending')"
                , "assert(s.commandedTask.bestDist and s.commandedTask.bestDist < 21,"
                , "  'the resumed order must record its new closest approach')"
                ]

        it "is still pending after an interruption the AI did not tick \
           \through at all (a collapse, an engine animation, a load)" $
            runsOk $ lns
                [ prelude
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW }"
                , "tick(5, 'follow_command')"
                , "skip(600)"
                , "tick(1, 'follow_command')"
                , "assert(s.commandedTask,"
                , "  'a gap with no AI sample at all must charge nothing')"
                ]

        it "is not charged for a SHORT no-tick interruption either — a \
           \1 s get-up stun is far under MAX_CHARGED_INTERVAL, so only \
           \the boundary the short-circuit records excludes it" $
            runsOk $ lns
                [ prelude
                -- Deliberately staged one tick under the budget, so a
                -- charged 1 s interruption would expire the order and a
                -- correctly excluded one leaves it pending.
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW }"
                , "tick(59, 'follow_command')"
                , "assert(s.commandedTask, 'still inside the budget at 59 s')"
                , "shortCircuit(1)"
                , "tick(1, 'follow_command')"
                , "assert(s.commandedTask,"
                , "  'a 1 s collapse must cost the order nothing')"
                , "tick(3, 'follow_command')"
                , "assert(s.commandedTask == nil,"
                , "  'and the remaining budget still runs out')"
                ]

        it "accumulates eligible time ACROSS an interruption rather than \
           \restarting after it: 40 s charged, a 300 s interruption, then \
           \25 s more expires it" $
            runsOk $ lns
                [ prelude
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW }"
                , "tick(40, 'follow_command')"
                , "assert(s.commandedTask, 'still inside the budget at 40 s')"
                , "tick(300, 'eat_from_inventory')"
                , "assert(s.commandedTask, 'the interruption itself charges nothing')"
                -- 40 already charged + 15 more is still under 60...
                , "tick(15, 'follow_command')"
                , "assert(s.commandedTask,"
                , "  'the pre-interruption charge must not have been refunded')"
                -- ...and the remaining ~5 s of budget then runs out.
                , "tick(10, 'follow_command')"
                , "assert(s.commandedTask == nil,"
                , "  'the order must expire once the REMAINING budget is spent')"
                ]

    describe "an order the unit is free to pursue still expires" $ do
        it "expires within approximately TASK_TIMEOUT_SEC of eligible \
           \time when no closest approach is ever made" $
            runsOk $ lns
                [ prelude
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW }"
                , "tick(59, 'follow_command')"
                , "assert(s.commandedTask, 'must not give up before the budget')"
                , "tick(3, 'follow_command')"
                , "assert(s.commandedTask == nil,"
                , "  'an unreachable target must still give up')"
                , "assert(NOW < 70, 'and give up at the budget, not later')"
                ]

    describe "an abandoned PLAYER order is reported (#1769)" $ do
        it "reports exactly one unit_warning when the order expires \
           \while the unit is still MOVING — the case the stuck-walk \
           \watchdog cannot cover, since displacement keeps its own \
           \mark fresh while net closure never reaches \
           \TASK_PROGRESS_TILES" $
            runsOk $ lns
                [ prelude
                , "EVENTS = {}"
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW,"
                , "                    player = true }"
                -- The dead band, driven directly: every sample moves the
                -- unit far enough that unit_ai.lua's watchdog would
                -- refresh (> 0.1 tiles of displacement), and no sample
                -- ever nets TASK_PROGRESS_TILES of improvement on the
                -- best distance so far, so this budget never resets.
                , "local worst = 0"
                , "for i = 1, 40 do"
                , "  local prev = POS.gridX"
                , "  place((i % 2 == 0) and 0.3 or 0.0, 0)"
                , "  local step = math.abs(POS.gridX - prev)"
                , "  if step > worst then worst = step end"
                , "  local t = s.commandedTask"
                , "  if t and t.bestDist then"
                , "    assert(t.bestDist == 40,"
                , "      'the order never records a closer approach: '"
                , "      .. tostring(t.bestDist))"
                , "  end"
                , "  tick(2, 'follow_command')"
                , "end"
                , "assert(worst > 0.1,"
                , "  'the unit moved enough to keep the watchdog fresh: '"
                , "  .. tostring(worst))"
                , "assert(s.commandedTask == nil, 'and the order expired')"
                -- Ticking on past the expiry is what proves requirement
                -- 3: the cleared order cannot report a second time.
                , "assert(#EVENTS == 1,"
                , "  'exactly one report: ' .. tostring(#EVENTS))"
                , "assert(EVENTS[1].cat == 'unit_warning',"
                , "  'on the player-visible warning surface: '"
                , "  .. tostring(EVENTS[1].cat))"
                , "assert(EVENTS[1].uid == 1,"
                , "  'filed against the unit that lost the order: '"
                , "  .. tostring(EVENTS[1].uid))"
                , "assert(EVENTS[1].msg:find('Dorian Kessler', 1, true),"
                , "  'naming the unit: ' .. tostring(EVENTS[1].msg))"
                , "assert(EVENTS[1].msg:find('move order', 1, true),"
                , "  'and the order it abandoned: ' .. tostring(EVENTS[1].msg))"
                ]

        it "says nothing for an INTERNAL move — building_spawn.lua's \
           \portal walk-out is not an order the player gave, exactly \
           \as on the arrival branch" $
            runsOk $ lns
                [ prelude
                , "EVENTS = {}"
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW }"
                , "tick(75, 'follow_command')"
                , "assert(s.commandedTask == nil, 'the order still expired')"
                , "assert(#EVENTS == 0,"
                , "  'an internal move reports nothing: ' .. tostring(#EVENTS))"
                ]

        it "says nothing for an order that ARRIVES" $
            runsOk $ lns
                [ prelude
                , "EVENTS = {}"
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW,"
                , "                    player = true }"
                , "tick(5, 'follow_command')"
                , "place(39.5, 0)"
                , "tick(0.25, 'follow_command')"
                , "assert(s.commandedTask == nil, 'arrival cleared the task')"
                , "assert(s.holdAnchor, 'and left the hold it always did')"
                , "assert(#EVENTS == 0,"
                , "  'a completed order reports nothing: ' .. tostring(#EVENTS))"
                ]

        it "says nothing for an order the player SUPERSEDES, while a \
           \replacement that independently expires still reports for \
           \itself" $
            runsOk $ lns
                [ prelude
                , "EVENTS = {}"
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW,"
                , "                    player = true }"
                , "tick(50, 'follow_command')"
                , "assert(s.commandedTask, 'the first order is still pending')"
                -- unit_ai_core's commandMove replaces the table outright.
                , "s.commandedTask = { x = 80, y = 0, startedAt = NOW,"
                , "                    player = true }"
                , "tick(50, 'follow_command')"
                , "assert(s.commandedTask,"
                , "  'the replacement runs its own fresh budget')"
                , "assert(#EVENTS == 0,"
                , "  'the superseded order reported nothing: '"
                , "  .. tostring(#EVENTS))"
                , "tick(15, 'follow_command')"
                , "assert(s.commandedTask == nil,"
                , "  'and the replacement expires on its own')"
                , "assert(#EVENTS == 1,"
                , "  'reporting for itself: ' .. tostring(#EVENTS))"
                ]

        it "says nothing when the stuck-walk watchdog already reported \
           \this order's own walk — one event, one report" $
            runsOk $ lns
                [ prelude
                , "EVENTS = {}"
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW,"
                , "                    player = true }"
                -- followCommandExecute hands this task to the engine.
                , "stall.noteWalk(1, s.commandedTask)"
                , "tick(10, 'follow_command')"
                -- unit_ai.lua's watchdog stops the unit, files its own
                -- `Stuck — can't reach destination` warning on this same
                -- surface, and records that it did. This order is the
                -- one the engine was walking.
                , "s.currentAction = 'follow_command'"
                , "stall.noteStuckReport(1, s)"
                , "tick(60, 'follow_command')"
                , "assert(s.commandedTask == nil, 'the order still expires')"
                , "assert(#EVENTS == 0,"
                , "  'and adds nothing to what the watchdog said: '"
                , "  .. tostring(#EVENTS))"
                ]

        it "still reports when the watchdog's stuck walk was ANOTHER \
           \action's, not this order's" $
            runsOk $ lns
                [ prelude
                , "EVENTS = {}"
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW,"
                , "                    player = true }"
                , "stall.noteWalk(1, s.commandedTask)"
                , "tick(10, 'treat_ally')"
                , "s.currentAction = 'treat_ally'"
                , "stall.noteStuckReport(1, s)"
                , "tick(70, 'follow_command')"
                , "assert(s.commandedTask == nil, 'the order expires')"
                , "assert(#EVENTS == 1,"
                , "  'and reports, since the stuck walk was not its own: '"
                , "  .. tostring(#EVENTS))"
                ]

        it "still reports for a REPLACEMENT order the watchdog was \
           \never about: a commandMove issued mid-walk does not re-run \
           \the already-running action, so the engine keeps walking \
           \the predecessor's destination and a watchdog report in \
           \that window belongs to the predecessor" $
            runsOk $ lns
                [ prelude
                , "EVENTS = {}"
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW,"
                , "                    player = true }"
                , "stall.noteWalk(1, s.commandedTask)"
                , "tick(10, 'follow_command')"
                -- The player commands somewhere else. `follow_command`
                -- is already the running action and the unit is not
                -- idle, so unit_ai.lua's switch-or-idle execute gate
                -- does NOT re-run followCommandExecute: no noteWalk,
                -- and the engine is still walking the FIRST
                -- destination. Ticked long past the watchdog's own
                -- deadline, so this is not a one-tick window.
                , "s.commandedTask = { x = 80, y = 0, startedAt = NOW,"
                , "                    player = true }"
                , "tick(20, 'follow_command')"
                -- Only now does the watchdog report that stalled walk.
                , "s.currentAction = 'follow_command'"
                , "stall.noteStuckReport(1, s)"
                , "tick(70, 'follow_command')"
                , "assert(s.commandedTask == nil,"
                , "  'the replacement expires on its own budget')"
                , "assert(#EVENTS == 1,"
                , "  'and reports, since the watchdog never spoke about '"
                , "  .. 'it: ' .. tostring(#EVENTS))"
                ]

        it "does silence the replacement once the engine is actually \
           \walking IT — the binding is to the issued walk, not a \
           \blanket exemption for anything that replaced something" $
            runsOk $ lns
                [ prelude
                , "EVENTS = {}"
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW,"
                , "                    player = true }"
                , "stall.noteWalk(1, s.commandedTask)"
                , "tick(10, 'follow_command')"
                , "s.commandedTask = { x = 80, y = 0, startedAt = NOW,"
                , "                    player = true }"
                -- The watchdog's unit.stop leaves the unit idle, so the
                -- execute gate DOES re-run and the replacement reaches
                -- the engine.
                , "stall.noteWalk(1, s.commandedTask)"
                , "tick(20, 'follow_command')"
                , "s.currentAction = 'follow_command'"
                , "stall.noteStuckReport(1, s)"
                , "tick(70, 'follow_command')"
                , "assert(s.commandedTask == nil, 'the replacement expires')"
                , "assert(#EVENTS == 0,"
                , "  'silently, because the watchdog reported ITS walk: '"
                , "  .. tostring(#EVENTS))"
                ]

        it "makes that distinction by task IDENTITY, not by comparing \
           \timestamps: game time need not advance between the \
           \player's command and the tick that first sees it, so the \
           \replacement can carry the very same startedAt as the \
           \moment the old walk was last making progress" $
            runsOk $ lns
                [ prelude
                , "EVENTS = {}"
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW,"
                , "                    player = true }"
                , "stall.noteWalk(1, s.commandedTask)"
                , "tick(10, 'follow_command')"
                -- The clock does not move: NOW is the last instant the
                -- watchdog would have stamped, AND the replacement's
                -- own issue time. A > / >= test on those two numbers
                -- cannot separate these orders; identity does.
                , "local tie = NOW"
                , "s.commandedTask = { x = 80, y = 0, startedAt = tie,"
                , "                    player = true }"
                , "assert(s.commandedTask.startedAt == tie,"
                , "  'the replacement is issued at the tying instant')"
                , "tick(0.25, 'follow_command')"
                , "s.currentAction = 'follow_command'"
                , "stall.noteStuckReport(1, s)"
                , "tick(70, 'follow_command')"
                , "assert(s.commandedTask == nil, 'the replacement expires')"
                , "assert(#EVENTS == 1,"
                , "  'and still reports for itself: ' .. tostring(#EVENTS))"
                ]

        it "takes its walk attribution from the REAL \
           \followCommandExecute: a task that action handed to the \
           \engine is silenced by a watchdog report, and one it never \
           \did is not" $
            runsOk $ lns
                [ combatPrelude
                -- The action runs, so the engine is walking THIS task.
                , "command(40)"
                , "combat.followCommandExecute(1, s, {})"
                , "assert(#MOVES == 1 and MOVES[1].x == 40,"
                , "  'the engine was given this walk: ' .. tostring(#MOVES))"
                , "tick(10, 'follow_command')"
                , "s.currentAction = 'follow_command'"
                , "stall.noteStuckReport(1, s)"
                , "tick(70, 'follow_command')"
                , "assert(s.commandedTask == nil, 'the order expires')"
                , "assert(#EVENTS == 0,"
                , "  'silently, because the watchdog reported ITS walk: '"
                , "  .. tostring(#EVENTS))"
                -- A replacement the action never re-ran for: the engine
                -- is still walking the previous destination.
                , "command(80)"
                , "tick(20, 'follow_command')"
                , "assert(#MOVES == 1,"
                , "  'the replacement never reached the engine: '"
                , "  .. tostring(#MOVES))"
                , "s.currentAction = 'follow_command'"
                , "stall.noteStuckReport(1, s)"
                , "tick(70, 'follow_command')"
                , "assert(s.commandedTask == nil, 'the replacement expires')"
                , "assert(#EVENTS == 1,"
                , "  'and reports, since that walk was not its own: '"
                , "  .. tostring(#EVENTS))"
                ]

        it "reports a SECOND stall after a genuine closest approach — \
           \the approach that refreshes the budget retires the \
           \watchdog record along with it" $
            runsOk $ lns
                [ prelude
                , "EVENTS = {}"
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW,"
                , "                    player = true }"
                , "stall.noteWalk(1, s.commandedTask)"
                , "tick(10, 'follow_command')"
                , "s.currentAction = 'follow_command'"
                , "stall.noteStuckReport(1, s)"
                , "place(10, 0)   -- 30 tiles closer: a real approach"
                , "tick(10, 'follow_command')"
                , "assert(s.commandedTask.bestDist < 31,"
                , "  'the approach is recorded: '"
                , "  .. tostring(s.commandedTask.bestDist))"
                , "tick(60, 'follow_command')"
                , "assert(s.commandedTask == nil,"
                , "  'the order expires on the new stall')"
                , "assert(#EVENTS == 1,"
                , "  'which is a new event, so it reports: '"
                , "  .. tostring(#EVENTS))"
                ]

    describe "the closest-approach reset is unchanged" $ do
        it "a new closest approach beyond TASK_PROGRESS_TILES starts the \
           \whole budget over" $
            runsOk $ lns
                [ prelude
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW }"
                , "tick(50, 'follow_command')"
                , "place(10, 0)   -- 30 tiles closer"
                , "tick(50, 'follow_command')"
                , "assert(s.commandedTask,"
                , "  'a closest approach must refresh the whole budget')"
                , "tick(15, 'follow_command')"
                , "assert(s.commandedTask == nil,"
                , "  'and the budget then runs from the approach, not from zero')"
                ]

        it "an interruption that happens to carry the unit CLOSER does \
           \not refund the budget already spent — the reset is the \
           \order's own action's to make, and it still fires on the \
           \first eligible sample afterwards" $
            runsOk $ lns
                [ prelude
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW }"
                , "tick(50, 'follow_command')"
                , "local spent = s.commandedTask.stalledFor"
                , "assert(spent > 49 and spent < 51,"
                , "  'about 50 s charged: ' .. tostring(spent))"
                -- treat_ally / a combat chase drags the unit 30 tiles
                -- toward the commanded tile while it owns the unit.
                , "place(10, 0)"
                , "tick(5, 'treat_ally')"
                , "assert(s.commandedTask.stalledFor == spent,"
                , "  'an interrupted approach must not refund the spent budget: '"
                , "  .. tostring(s.commandedTask.stalledFor))"
                , "assert(s.commandedTask.bestDist == 40,"
                , "  'nor record an approach the order did not make')"
                -- Nothing is lost: the order's own action records it.
                , "tick(5, 'follow_command')"
                , "assert(s.commandedTask.bestDist < 31,"
                , "  'the resumed order records the approach: '"
                , "  .. tostring(s.commandedTask.bestDist))"
                , "assert(s.commandedTask.stalledFor < 5,"
                , "  'and its budget starts over there')"
                ]

        it "an improvement smaller than TASK_PROGRESS_TILES does NOT \
           \reset it — path jitter is not progress" $
            runsOk $ lns
                [ prelude
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW }"
                , "tick(50, 'follow_command')"
                , "place(0.3, 0)   -- 0.3 tiles closer, under the 0.5 step"
                , "tick(15, 'follow_command')"
                , "assert(s.commandedTask == nil,"
                , "  'a sub-threshold improvement must not refresh the budget')"
                ]

        it "arrival within TASK_ARRIVAL_TILES clears the task" $
            runsOk $ lns
                [ prelude
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW }"
                , "tick(5, 'follow_command')"
                , "place(39.5, 0)"
                , "tick(0.25, 'follow_command')"
                , "assert(s.commandedTask == nil, 'arrival must clear the task')"
                ]

    describe "the same accounting governs pickupOrder" $ do
        it "survives an interruption longer than pickup_timeout and \
           \still expires on eligible non-progress, reporting the \
           \failure it has always reported" $
            runsOk $ lns
                [ pickupPrelude
                , "tick(5, 'pickup_ground')"
                , "assert(s.pickupOrder, 'the order must survive its own first ticks')"
                , "tick(120, 'eat_from_inventory')"
                , "assert(s.pickupOrder,"
                , "  'an interruption longer than pickup_timeout must not expire it')"
                , "skip(600)"
                , "tick(1, 'pickup_ground')"
                , "assert(s.pickupOrder, 'nor must a gap with no sample at all')"
                , "assert(#EVENTS == 0, 'and neither reports a failure')"
                -- Now spend the remaining eligible budget without ever
                -- getting closer: the pre-#1291 outcome, unchanged.
                , "tick(40, 'pickup_ground')"
                , "assert(s.pickupOrder == nil,"
                , "  'eligible non-progress must still expire the order')"
                , "assert(#EVENTS == 1 and EVENTS[1].cat == 'unit_warning',"
                , "  'the existing player-visible failure report must survive')"
                , "assert(EVENTS[1].msg:find('pick up'),"
                , "  'and must still name the pickup: ' .. tostring(EVENTS[1].msg))"
                ]

        it "is not charged for a SHORT no-tick interruption either — the \
           \engine `pickup` animation itself is one, and it is briefer \
           \than MAX_CHARGED_INTERVAL" $
            runsOk $ lns
                [ pickupPrelude
                , "tick(29, 'pickup_ground')"
                , "assert(s.pickupOrder, 'still inside the budget')"
                , "shortCircuit(2)"
                , "tick(2, 'pickup_ground')"
                , "assert(s.pickupOrder,"
                , "  'a 2 s engine animation must cost the order nothing')"
                , "tick(5, 'pickup_ground')"
                , "assert(s.pickupOrder == nil,"
                , "  'and the remaining budget still runs out')"
                ]

        it "an interruption that carries the carrier closer does not \
           \refund its budget either" $
            runsOk $ lns
                [ pickupPrelude
                , "tick(20, 'pickup_ground')"
                , "local spent = s.pickupOrder.stalledFor"
                , "assert(spent > 18 and spent < 21,"
                , "  'about 19 s charged: ' .. tostring(spent))"
                , "place(20, 0)"
                , "tick(5, 'eat_from_inventory')"
                , "assert(s.pickupOrder.stalledFor == spent,"
                , "  'an interrupted approach must not refund the spent budget')"
                , "assert(s.pickupOrder.bestDist == 40,"
                , "  'nor record an approach the order did not make')"
                , "tick(3, 'pickup_ground')"
                , "assert(s.pickupOrder.bestDist < 21,"
                , "  'the resumed order records the approach')"
                , "assert(s.pickupOrder.stalledFor < 3,"
                , "  'and its budget starts over there')"
                ]

        it "a new closest approach starts its budget over too" $
            runsOk $ lns
                [ pickupPrelude
                , "tick(25, 'pickup_ground')"
                , "assert(s.pickupOrder, 'still inside the budget')"
                , "place(20, 0)   -- 20 tiles closer"
                , "tick(25, 'pickup_ground')"
                , "assert(s.pickupOrder,"
                , "  'a closest approach must refresh the whole budget')"
                , "tick(10, 'pickup_ground')"
                , "assert(s.pickupOrder == nil,"
                , "  'and the budget then runs from the approach')"
                ]

    describe "the accounting rides the existing lua.unit_ai component" $ do
        it "round-trips a partially consumed budget through the real \
           \snapshot -> encode -> decode -> apply path" $
            runsOk $ lns
                [ savePrelude
                , "aiState[1] = { currentAction = 'follow_command',"
                , "  commandedTask = { x = 40, y = 0, startedAt = 0,"
                , "    bestDist = 40, stalledFor = 41.5, stallSeenAt = 120 } }"
                , "local payload = codec.encode(spec.snapshot())"
                , "local decoded = spec.decode(spec.version, codec.decode(payload))"
                , "for k in pairs(aiState) do aiState[k] = nil end"
                , "spec.apply(decoded, nil)"
                , "local t = aiState[1] and aiState[1].commandedTask"
                , "assert(t, 'the restored unit must still carry its order')"
                , "assert(t.stalledFor == 41.5,"
                , "  'the consumed budget must survive the round trip: '"
                , "  .. tostring(t.stalledFor))"
                , "assert(t.stallSeenAt == 120, 'and so must the last sample')"
                , "assert(t.bestDist == 40, 'alongside the closest approach')"
                ]

        it "a save/load window is neither charged as stall time nor \
           \treated as progress: the restored order resumes with its \
           \remaining budget and expires on that" $
            runsOk $ lns
                [ prelude
                -- What apply() put back: 55 of the 60 s already spent,
                -- last sampled at the moment of the save.
                , "NOW = 200"
                , "s.commandedTask = { x = 40, y = 0, startedAt = 0,"
                , "  bestDist = 40, stalledFor = 55, stallSeenAt = 200 }"
                , "tick(3, 'follow_command')"
                , "assert(s.commandedTask,"
                , "  'the remaining budget must still be there')"
                , "tick(4, 'follow_command')"
                , "assert(s.commandedTask == nil,"
                , "  'and only the REMAINING budget, not a fresh one')"
                ]

        it "a pre-#1291 payload (v4, no accounting fields) decodes to \
           \the honest default: the order behaves as it did before, \
           \never as one that can no longer expire" $
            runsOk $ lns
                [ savePrelude
                -- A v4 order: the absolute progressAt origin, no
                -- stalledFor/stallSeenAt pair at all.
                , "local v4 = { [1] = { currentAction = 'follow_command',"
                , "  __owner = { __ref = 'unit', id = 1 },"
                , "  commandedTask = { x = 40, y = 0, startedAt = 0,"
                , "    bestDist = 40, progressAt = 100 } } }"
                , "local decoded = spec.decode(4, v4)"
                , "spec.apply(decoded, nil)"
                , "local t = aiState[1].commandedTask"
                , "assert(t.progressAt == 100, 'the legacy origin decodes untouched')"
                , "assert(t.stalledFor == nil and t.stallSeenAt == nil,"
                , "  'and nothing is invented for it at decode time')"
                ]

        it "seeds that legacy order from its own progressAt on the first \
           \tick, so it expires when the old rule would have" $
            runsOk $ lns
                [ prelude
                -- 45 s of the 60 s budget already elapsed under the old
                -- absolute rule when the save was written.
                , "NOW = 145"
                , "s.commandedTask = { x = 40, y = 0, startedAt = 0,"
                , "  bestDist = 40, progressAt = 100 }"
                , "tick(1, 'follow_command')"
                , "assert(s.commandedTask, 'the legacy order is not yet spent')"
                , "assert(s.commandedTask.progressAt == nil,"
                , "  'and the legacy origin is retired once reconstructed')"
                , "tick(10, 'follow_command')"
                , "assert(s.commandedTask, \"still under the old rule's deadline\")"
                , "tick(6, 'follow_command')"
                , "assert(s.commandedTask == nil,"
                , "  'and it expires there, exactly as it would have before')"
                ]

        it "an order that was never evaluated (no progressAt) starts \
           \from an unspent budget rather than from its issue time" $
            runsOk $ lns
                [ prelude
                -- The one order shape with no origin to reconstruct: a
                -- command issued while the unit was already short-
                -- circuited, so no tick ever stamped one.
                , "NOW = 500"
                , "s.commandedTask = { x = 40, y = 0, startedAt = 0 }"
                , "tick(1, 'follow_command')"
                , "assert(s.commandedTask,"
                , "  'an unevaluated order must not expire on its first tick')"
                ]
