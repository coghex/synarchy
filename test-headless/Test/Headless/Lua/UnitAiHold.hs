-- | The "position hold after a completed move order" gate (#1216,
--   SURV-4): @scripts/unit_ai_hold.lua@'s @hold_position@ candidate,
--   the anchor @scripts/unit_ai_stall.lua@'s @maintainTask@ writes on
--   the ARRIVAL branch alone, and the accepted-player-command rule
--   that clears it.
--
--   Same standalone-Lua-VM pattern as "Test.Headless.Lua.UnitAiStall",
--   whose fixture this extends: each 'it' runs one self-contained
--   chunk via 'Lua.dostring' in a fresh interpreter, asserting inside
--   Lua via @assert()@, with a non-OK 'Lua.Status' surfaced as an
--   hspec failure carrying the Lua message. Nothing here is mocked
--   that the behaviour depends on — the real @maintainTask@, the real
--   @holdUtility@/@holdExecute@, the real @commandMove@ /
--   @commandAttack@ / @commandPickup@ verbs and the real
--   @lua.unit_ai@ save component all run; only the engine globals
--   they reach (clock, unit position, ground items) are stubbed.
--
--   The two numbers that must never drift — @hold_position@'s utility
--   and @follow_command@'s — are asserted EQUAL against
--   @scripts/unit_ai_combat.lua@'s own exported constant, because the
--   whole priority contract is "a hold sits in exactly the band the
--   order that created it sat in": everything that could interrupt
--   that order still can, everything it outranked still loses.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "position hold"'@.
module Test.Headless.Lua.UnitAiHold (spec) where

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

-- | Engine globals every fixture below needs: a stubbed clock, a
--   stubbed unit position, and a record of the movement verbs the AI
--   issued (@MOVES@ / @STOPS@), so "stands still" and "walks back" are
--   asserted on what the action actually did rather than on a position
--   the fixture would have had to move itself.
--
--   @scripts/movement_speed.lua@ is stubbed at @package.loaded@: the
--   pace a return walk picks is not what this gate is about, and the
--   real module reaches four physiology modules' worth of stats that
--   would all have to be faked to answer one number.
engineStubs ∷ Text
engineStubs = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "package.loaded['scripts.movement_speed'] ="
    , "  { comfort = function() return 1.0 end,"
    , "    ordered = function() return 1.15 end,"
    , "    meander = function() return 0.5 end,"
    , "    sprint  = function() return 2.0 end }"
    , "NOW = 0"
    , "POS = { gridX = 0, gridY = 0 }"
    , "MOVES, STOPS, EVENTS = {}, 0, {}"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function() end, logInfo = function() end,"
    , "           emitEventForUnit = function(cat, msg)"
    , "             EVENTS[#EVENTS + 1] = { cat = cat, msg = msg } end }"
    , "unit = { getInfo = function() return POS end,"
    , "         exists = function() return true end,"
    , "         getStat = function() return 100 end,"
    , "         getCarryingWeight = function() return 0 end,"
    , "         moveTo = function(_, x, y, sp)"
    , "           MOVES[#MOVES + 1] = { x = x, y = y, speed = sp } end,"
    , "         stop = function() STOPS = STOPS + 1 end }"
    , "local function place(x, y) POS.gridX, POS.gridY = x, y end"
    ]

-- | The arrival/timeout half: the real @maintainTask@ driven one AI
--   update at a time, exactly as "Test.Headless.Lua.UnitAiStall"
--   drives it, plus the real @hold_position@ candidate scored and
--   executed over the same state table.
--
--   @scoreAndRun@ is one thought tick of @scripts/unit_ai.lua@'s
--   dispatch loop reduced to this one action: score it with
--   @s.currentAction@ still naming the interval that just elapsed
--   (which is what the eligible-time accounting reads), then run it
--   and record the choice — the same order the real @tickOne@ uses.
holdPrelude ∷ Text
holdPrelude = lns
    [ engineStubs
    , "local stall = require('scripts.unit_ai_stall')"
    , "local hold = require('scripts.unit_ai_hold')"
    , "local s = { currentAction = nil }"
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
    , "-- `seconds` of thought ticks with `action` in control over each"
    , "-- interval, scored against the hold candidate: exactly what"
    , "-- tickOne does, reduced to this one action. `action` names the"
    , "-- interval that JUST ELAPSED (which is what the eligible-time"
    , "-- accounting reads), so a caller models a sustained interrupt by"
    , "-- naming it for the whole span and the hold running by naming"
    , "-- 'hold_position'."
    , "local function scoreAndRun(seconds, action)"
    , "  local left = seconds"
    , "  while left > 1e-9 do"
    , "    local dt = math.min(STEP, left)"
    , "    NOW = NOW + dt"
    , "    s.currentAction = action"
    , "    stall.maintainTask(1, s)"
    , "    if hold.holdUtility(1, s) > -math.huge then"
    , "      s.currentAction = 'hold_position'"
    , "      hold.holdExecute(1, s)"
    , "    end"
    , "    left = left - dt"
    , "  end"
    , "end"
    ]

-- | The command verbs, reached through the real
--   @scripts/unit_ai_core.lua@ and @scripts/unit_ai_pickup.lua@ over
--   the shared @aiState@ singleton those modules own.
commandPrelude ∷ Text
commandPrelude = lns
    [ engineStubs
    , "GROUND = { { id = 7, defName = 'radio', x = 3, y = 0, weight = 1 } }"
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
    , "local unitAi = package.loaded['scripts.unit_ai']"
    , "local core = require('scripts.unit_ai_core')"
    , "local hold = require('scripts.unit_ai_hold')"
    , "require('scripts.unit_ai_pickup')"
    , "local aiState = core.aiState"
    , "local function holding(uid) return aiState[uid].holdAnchor ~= nil end"
    , "local function park(uid, x, y)"
    , "  local s = core.ensureState(uid)"
    , "  s.holdAnchor = { x = x, y = y }"
    , "  return s"
    , "end"
    ]

-- | The real @lua.unit_ai@ component, registered against a fake
--   @aiState@ the case fills in — the same shape
--   "Test.Headless.Lua.UnitAiStall" uses for its own round trips.
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
spec = describe "position hold after a completed move order" $ do

    describe "only a COMPLETED PLAYER move order creates a hold" $ do
        it "anchors the unit at the destination the order named" $
            runsOk $ lns
                [ holdPrelude
                , "s.commandedTask = { x = 10, y = 4, startedAt = NOW,"
                , "                    player = true }"
                , "tick(2, 'follow_command')"
                , "assert(s.holdAnchor == nil, 'no hold before arrival')"
                , "place(10.2, 4.1)"
                , "tick(0.25, 'follow_command')"
                , "assert(s.commandedTask == nil, 'arrival clears the task')"
                , "assert(s.holdAnchor, 'and leaves a hold behind')"
                , "assert(s.holdAnchor.x == 10 and s.holdAnchor.y == 4,"
                , "  'anchored at the COMMANDED tile, not where the unit stopped')"
                ]

        it "leaves NO hold when the order stalls out on TASK_TIMEOUT_SEC \
           \(requirement 5: the hold follows completion, not issue)" $
            runsOk $ lns
                [ holdPrelude
                , "s.commandedTask = { x = 40, y = 0, startedAt = NOW,"
                , "                    player = true }"
                , "tick(75, 'follow_command')"
                , "assert(s.commandedTask == nil, 'the order gave up')"
                , "assert(s.holdAnchor == nil,"
                , "  'an abandoned order must not pin the unit where it stood')"
                ]

        it "leaves NO hold for an INTERNAL move — scripts/building_spawn \
           \.lua's portal walk-out must not pin a fresh acolyte" $
            runsOk $ lns
                [ holdPrelude
                , "s.commandedTask = { x = 10, y = 4, startedAt = NOW }"
                , "place(10.2, 4.1)"
                , "tick(0.25, 'follow_command')"
                , "assert(s.commandedTask == nil, 'the walk-out completed')"
                , "assert(s.holdAnchor == nil,"
                , "  'and the spawned unit stays autonomous')"
                ]

        it "leaves NO hold when the unit vanishes mid-order" $
            runsOk $ lns
                [ holdPrelude
                , "s.commandedTask = { x = 10, y = 4, startedAt = NOW,"
                , "                    player = true }"
                , "unit.getInfo = function() return nil end"
                , "tick(0.25, 'follow_command')"
                , "assert(s.commandedTask == nil and s.holdAnchor == nil,"
                , "  'a gone unit is not a completed order')"
                ]

    describe "the hold sits in follow_command's own priority band" $ do
        it "scores EXACTLY unit_ai_combat.lua's FOLLOW_COMMAND_UTILITY, \
           \so every #306 interrupt that outranked the order still \
           \outranks the hold and everything it outranked still loses" $
            runsOk $ lns
                [ engineStubs
                , "require('scripts.unit_ai_core')"
                , "local hold = require('scripts.unit_ai_hold')"
                , "local combat = require('scripts.unit_ai_combat')"
                , "assert(combat.FOLLOW_COMMAND_UTILITY,"
                , "  'follow_command must export its constant to be pinned to')"
                , "assert(hold.HOLD_UTILITY == combat.FOLLOW_COMMAND_UTILITY,"
                , "  'the hold and the order it succeeds must share one number: '"
                , "  .. tostring(hold.HOLD_UTILITY) .. ' vs '"
                , "  .. tostring(combat.FOLLOW_COMMAND_UTILITY))"
                , "local s = { holdAnchor = { x = 0, y = 0 } }"
                , "assert(hold.holdUtility(1, s) == hold.HOLD_UTILITY,"
                , "  'a holding unit scores it')"
                ]

        it "outranks ambient wander at its most attractive — a fully \
           \rested acolyte on the real shipped tunables" $
            runsOk $ lns
                [ engineStubs
                , "package.loaded['scripts.unit_stats'] ="
                , "  { get = function() return 100 end }"
                , "local hold = require('scripts.unit_ai_hold')"
                , "local needs = require('scripts.unit_ai_needs')"
                , "local params = require('scripts.unit_ai_tunables').acolyte"
                , "local s = { currentAction = 'idle', actionStartedAt = 0,"
                , "            holdAnchor = { x = 0, y = 0 } }"
                , "local w = needs.wanderUtility(1, s, params)"
                , "assert(w > 0, 'wander must really be a live candidate: '"
                , "  .. tostring(w))"
                , "assert(hold.holdUtility(1, s) > w,"
                , "  'the hold must suppress it: ' .. tostring(w))"
                ]

        it "yields outright to a LIVE commanded task, so the two never \
           \contend at their shared utility" $
            runsOk $ lns
                [ engineStubs
                , "local hold = require('scripts.unit_ai_hold')"
                , "local s = { holdAnchor = { x = 0, y = 0 },"
                , "            commandedTask = { x = 9, y = 9 } }"
                , "assert(hold.holdUtility(1, s) == -math.huge,"
                , "  'a pending order owns the unit outright')"
                ]

        it "is not a candidate at all for a unit that was never \
           \commanded" $
            runsOk $ lns
                [ engineStubs
                , "local hold = require('scripts.unit_ai_hold')"
                , "assert(hold.holdUtility(1, { }) == -math.huge)"
                ]

    describe "holding, and returning to the anchor after an interrupt" $ do
        it "stands still while it is on the anchor — it stops, and never \
           \issues a move" $
            runsOk $ lns
                [ holdPrelude
                , "s.holdAnchor = { x = 0, y = 0 }"
                , "scoreAndRun(20, 'idle')"
                , "assert(#MOVES == 0, 'a held unit must not walk anywhere')"
                , "assert(STOPS > 0, 'it holds by stopping')"
                , "assert(s.holdAnchor, 'and it is still holding after 20 s')"
                ]

        it "walks back to the anchor after an interrupt displaced it, \
           \then holds there again" $
            runsOk $ lns
                [ holdPrelude
                , "s.holdAnchor = { x = 0, y = 0 }"
                , "scoreAndRun(2, 'idle')"
                -- drink_from_source / refill_canteen carried it off.
                , "place(6, 0)"
                , "s.currentAction = 'drink_from_source'"
                , "scoreAndRun(1, 'drink_from_source')"
                , "assert(#MOVES > 0, 'a displaced hold must walk home')"
                , "local m = MOVES[#MOVES]"
                , "assert(m.x == 0 and m.y == 0,"
                , "  'and walk to the ANCHOR: ' .. tostring(m.x) .. ',' .. tostring(m.y))"
                , "local before = STOPS"
                , "place(0.1, 0.1)"
                , "scoreAndRun(1, 'hold_position')"
                , "assert(STOPS > before, 'and hold again once back')"
                , "assert(s.holdAnchor, 'the hold outlives the round trip')"
                ]

        it "costs the hold nothing to be interrupted for far longer than \
           \TASK_TIMEOUT_SEC — the return budget charges eligible time \
           \only, exactly as a commanded move does" $
            runsOk $ lns
                [ holdPrelude
                , "s.holdAnchor = { x = 0, y = 0 }"
                , "place(6, 0)"
                , "scoreAndRun(300, 'treat_ally')"
                , "assert(s.holdAnchor,"
                , "  'an interruption of any length must not release the hold')"
                ]

        it "releases an UNREACHABLE anchor rather than re-pathing at it \
           \forever" $
            runsOk $ lns
                [ holdPrelude
                , "s.holdAnchor = { x = 0, y = 0 }"
                , "place(6, 0)"
                , "scoreAndRun(59, 'hold_position')"
                , "assert(s.holdAnchor, 'it must not give up before the budget')"
                , "scoreAndRun(5, 'hold_position')"
                , "assert(s.holdAnchor == nil,"
                , "  'a hold it can never reach must expire like an order')"
                ]

    describe "only an ACCEPTED, EXPLICIT player command clears a hold" $ do
        it "a player commandMove clears it; an internal one does not" $
            runsOk $ lns
                [ commandPrelude
                , "park(1, 5, 5)"
                , "unitAi.commandMove(1, 9, 9)"
                , "assert(not holding(1), 'a player move supersedes the hold')"
                , "assert(aiState[1].commandedTask.player,"
                , "  'and is marked as the player intent that can create one')"
                , "park(2, 5, 5)"
                , "unitAi.commandMove(2, 9, 9, nil, true)"
                , "assert(holding(2),"
                , "  'a spawn walk-out must not release a hold the player set')"
                , "assert(aiState[2].commandedTask.player == nil,"
                , "  'nor may it create one on arrival')"
                ]

        it "a committed (player/scripted) commandAttack clears it; the \
           \AI's own emergent engage does not" $
            runsOk $ lns
                [ commandPrelude
                , "park(1, 5, 5)"
                , "unitAi.commandAttack(1, 42, true)"
                , "assert(not holding(1), 'a player attack order supersedes it')"
                , "park(2, 5, 5)"
                , "unitAi.commandAttack(2, 42)"
                , "assert(holding(2),"
                , "  'autonomous engagement is an interrupt, not a new order')"
                ]

        it "an ACCEPTED commandPickup clears it; a refused one leaves it \
           \exactly as it was" $
            runsOk $ lns
                [ commandPrelude
                , "park(1, 5, 5)"
                , "assert(unitAi.commandPickup(1, 7), 'the order is accepted')"
                , "assert(not holding(1), 'an accepted pickup supersedes it')"
                -- Refusal: the carrier is already at its capacity.
                , "park(2, 5, 5)"
                , "unit.getCarryingWeight = function() return 100 end"
                , "assert(unitAi.commandPickup(2, 7) == false,"
                , "  'an over-capacity pickup is refused')"
                , "assert(holding(2),"
                , "  'and a refusal stores no order, so it must not release')"
                , "assert(aiState[2].pickupOrder == nil, 'nor store one')"
                ]

        it "releaseHold clears it with no movement at all, and reports \
           \whether there was anything to clear" $
            runsOk $ lns
                [ commandPrelude
                , "park(1, 5, 5)"
                , "assert(unitAi.getHold(1), 'getHold reports the anchor')"
                , "assert(unitAi.getHold(1).x == 5 and unitAi.getHold(1).y == 5)"
                , "assert(unitAi.releaseHold(1) == true, 'it released a hold')"
                , "assert(not holding(1) and unitAi.getHold(1) == nil)"
                , "assert(unitAi.releaseHold(1) == false,"
                , "  'and reports honestly when there was none')"
                , "assert(#MOVES == 0 and STOPS == 0,"
                , "  'the release verb issues no movement')"
                , "assert(unitAi.releaseHold(99) == false,"
                , "  'an unticked unit is not holding')"
                ]

    describe "the hold rides the lua.unit_ai component" $ do
        it "round-trips a live hold through the real snapshot -> encode \
           \-> decode -> apply path" $
            runsOk $ lns
                [ savePrelude
                -- The hold landed at v6 (#1216); the component has
                -- moved on since (v7, #1737's ground-sourced repair
                -- provenance; v8, #1844's construct-job attempt
                -- identity). What this gate owns is that the CURRENT
                -- version still round-trips a hold, so it pins the
                -- current number rather than the one #1216 shipped.
                , "assert(spec.version == 8, 'the component is at v8: '"
                , "  .. tostring(spec.version))"
                , "aiState[1] = { currentAction = 'hold_position',"
                , "  holdAnchor = { x = 12, y = -3, stalledFor = 4.5,"
                , "                 stallSeenAt = 90, bestDist = 2,"
                , "                 combatWithdrawalCompletedAt = 87.5 } }"
                , "local payload = codec.encode(spec.snapshot())"
                , "local decoded = spec.decode(spec.version, codec.decode(payload))"
                , "for k in pairs(aiState) do aiState[k] = nil end"
                , "spec.apply(decoded, nil)"
                , "local a = aiState[1] and aiState[1].holdAnchor"
                , "assert(a, 'the restored unit must still be holding')"
                , "assert(a.x == 12 and a.y == -3,"
                , "  'at the same anchor: ' .. tostring(a.x) .. ',' .. tostring(a.y))"
                , "assert(a.stalledFor == 4.5 and a.stallSeenAt == 90"
                , "       and a.bestDist == 2"
                , "       and a.combatWithdrawalCompletedAt == 87.5,"
                , "  'with its return accounting intact')"
                ]

        it "accepts every older input version, each decoding as NOT \
           \holding — a hold is never inferred from a payload that \
           \could not record one" $
            runsOk $ lns
                [ savePrelude
                , "for _, v in ipairs({ 1, 2, 3, 4, 5, 6 }) do"
                , "  assert(saveModules.registry.unit_ai.inputVersions,"
                , "    'the component declares its accepted inputs')"
                , "  local old = { [1] = { currentAction = 'wander',"
                , "    __owner = { __ref = 'unit', id = 1 },"
                , "    commandedTask = { x = 40, y = 0, startedAt = 0 } } }"
                , "  if v == 1 then old[1].__owner = nil end"
                , "  local decoded = spec.decode(v, old)"
                , "  for k in pairs(aiState) do aiState[k] = nil end"
                , "  spec.apply(decoded, nil)"
                , "  assert(aiState[1], 'v' .. v .. ' decodes')"
                , "  assert(aiState[1].holdAnchor == nil,"
                , "    'v' .. v .. ' must decode as not holding')"
                , "  assert(aiState[1].commandedTask.player == nil,"
                , "    'and its pending order carries no invented player intent')"
                , "end"
                , "local accepted = {}"
                , "for _, v in ipairs(spec.inputVersions) do accepted[v] = true end"
                , "for v = 1, 7 do"
                , "  assert(accepted[v], 'v' .. v .. ' must stay an accepted input')"
                , "end"
                ]
