{-# LANGUAGE TypeApplications #-}
-- | The "skill-scaled auto-harvest" gate (#1582):
--   @scripts/unit_ai_harvest.lua@'s progress accumulator — the farming
--   skill finally doing what @data\/units\/acolyte.yaml@'s farming
--   entry, the action's own header and #336 have all promised, namely
--   scaling how long an auto-harvest takes by the same
--   @0.5 + farming\/100@ factor @unitAi.till@ and @unitAi.plant@ use.
--
--   What the cases pin, in the order a reviewer would ask for them:
--   picking is no longer instant (@world.harvestFlora@ is not called on
--   the tick the worker arrives), a farming-100 picker finishes while an
--   otherwise identical farming-0 picker is still working, that
--   low-skill picker does finish given enough time, an absent farming
--   skill falls back to the same 25.0 novice base till and plant use,
--   and the accumulator is bound to the target's TILE so partial work on
--   one plant never lands on another. Deleting the @unit.getSkill@ read,
--   the target binding, or the completion threshold fails one of them.
--
--   Everything the action already did is pinned alongside: untagged
--   @world.findHarvestableFlora@\/@world.harvestFlora@ calls, the
--   @roles.weight@ arbitration multiplier (arbitration, NOT the skill —
--   #265\/requirement 5), the @collecting@ phase pulling ground yields
--   one per tick, the raced\/regrowing recovery that clears
--   @s.harvestTarget@, the @unit.pickup@ bend-down animation, and the
--   farming XP grant on completion.
--
--   Same standalone-Lua-VM pattern as "Test.Headless.Lua.UnitAiStall":
--   each 'it' runs one self-contained chunk via 'Lua.dostring' in a
--   fresh interpreter, asserting inside Lua via @assert()@, with a
--   non-OK 'Lua.Status' surfaced as an hspec failure carrying the Lua
--   message. No engine and no GPU — the clock, the flora and the unit
--   are all stubs the chunk drives tick by tick.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "skill-scaled auto-harvest"'@.
module Test.Headless.Lua.UnitAiHarvest (spec) where

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

-- | One ripe plant at (10, 0), one worker whose position and farming
--   skill the case sets, a stubbed game clock, and @tick(seconds)@ —
--   one AI update at a time on the ~0.5 s cadence the real thought tick
--   runs at, scoring the action and then executing it exactly as
--   @scripts\/unit_ai.lua@ does for an idle unit.
--
--   Every engine call the action makes is recorded rather than
--   performed: @CALLS.harvest@ counts @world.harvestFlora@ (with the
--   tag it was passed, so an untagged call stays untagged),
--   @CALLS.find@ the same for the scan, @CALLS.pickup@ the bend-down
--   anim, @CALLS.moveTo@ the walk, and @XP@ the farming grant.
--   @FLORA@ maps a @\"x,y\"@ key to a yield list; deleting an entry is
--   how a case makes a plant vanish, and @world.harvestFlora@ on a
--   missing one returns the empty list the raced\/regrowing path sees.
prelude ∷ Text
prelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "NOW = 0"
    , "POS = { gridX = 0, gridY = 0 }"
    , "SKILL = 50.0"
    , "GROUND = {}"
    , "XP = 0"
    , "CALLS = { find = 0, harvest = 0, pickup = 0, moveTo = 0,"
    , "          setSkill = 0, tags = {} }"
    , "FLORA = { ['10,0'] = { { gid = 1 }, { gid = 2 } } }"
    , "local function key(x, y) return string.format('%d,%d', x, y) end"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function() end, logInfo = function() end }"
    , "unit = {"
    , "  getInfo = function() return POS end,"
    , "  exists = function() return true end,"
    , "  getStat = function() return 1.0 end,"
    , "  getSkill = function(_, name)"
    , "    if name ~= 'farming' then return 0.0 end"
    , "    return SKILL end,"
    , "  setSkill = function(_, _, v) CALLS.setSkill = CALLS.setSkill + 1"
    , "    SKILL = v end,"
    , "  addXP = function(_, _, amount) XP = XP + amount end,"
    , "  pickup = function() CALLS.pickup = CALLS.pickup + 1 end,"
    , "  moveTo = function(_, x, y) CALLS.moveTo = CALLS.moveTo + 1"
    , "    MOVED_TO = { x = x, y = y } end,"
    , "  stop = function() end,"
    , "  setAnimOverride = function() end,"
    , "  clearAnimOverride = function() end }"
    , "item = { pickupGround = function(_, gid)"
    , "           GROUND[#GROUND + 1] = gid; return true end,"
    , "         listDefs = function() return {} end }"
    , "world = {"
    , "  getActiveWorldId = function() return 1 end,"
    , "  findHarvestableFlora = function(ux, uy, range, tag)"
    , "    CALLS.find = CALLS.find + 1"
    , "    CALLS.tags.find = tag"
    , "    local best, bestD = nil, nil"
    , "    for k, _ in pairs(FLORA) do"
    , "      local sx, sy = k:match('(-?%d+),(-?%d+)')"
    , "      local gx, gy = tonumber(sx), tonumber(sy)"
    , "      local d = math.sqrt((gx - ux) ^ 2 + (gy - uy) ^ 2)"
    , "      if d <= range and (not bestD or d < bestD) then"
    , "        best, bestD = { gx = gx, gy = gy, dist = d }, d"
    , "      end"
    , "    end"
    , "    return best end,"
    , "  harvestFlora = function(gx, gy, tag)"
    , "    CALLS.harvest = CALLS.harvest + 1"
    , "    CALLS.tags.harvest = tag"
    , "    local yields = FLORA[key(gx, gy)]"
    , "    FLORA[key(gx, gy)] = nil"
    , "    return yields or {} end }"
    -- The module under test, reached the way the shipped bootstrap
    -- reaches it: through scripts.unit_ai_farm, whose own require is
    -- the one link unit_ai.lua still makes (#1582's split).
    , "require('scripts.unit_ai_farm')"
    -- The walk speed the action hands unit.moveTo is not what this gate
    -- is about, and mv.comfort derives it from the whole physiology
    -- stack (injuries, salts, exhaustion, starvation, encumbrance).
    -- Pin it to a constant on the module table the action already
    -- holds, rather than stubbing five subsystems' worth of globals.
    , "require('scripts.movement_speed').comfort = function() return 1.0 end"
    , "local unitAi = package.loaded['scripts.unit_ai']"
    , "local harvest = unitAi.harvest"
    , "PARAMS = { harvest_scan_range = 24.0, harvest_base_utility = 2.0,"
    , "           harvest_rate = 0.5, harvest_xp_per_harvest = 1.0 }"
    , "S = {}"
    , "local STEP = 0.5"
    , "local function place(x, y) POS.gridX, POS.gridY = x, y end"
    -- One arbitration pass over the single action under test: score it,
    -- then execute it, exactly as unit_ai.lua does for an idle unit
    -- whose highest-scoring action this is.
    , "local function step(dt)"
    , "  NOW = NOW + dt"
    , "  local u = harvest.utility(1, S, PARAMS)"
    , "  if u > -math.huge then harvest.execute(1, S, PARAMS) end"
    , "  return u"
    , "end"
    , "local function tick(seconds)"
    , "  local left = seconds"
    , "  while left > 1e-9 do"
    , "    local dt = math.min(STEP, left)"
    , "    step(dt)"
    , "    left = left - dt"
    , "  end"
    , "end"
    -- Time in which another action owned the unit: arbitration calls
    -- harvest's onExit on the way out and does not execute it again
    -- until it wins back.
    , "local function preempt(seconds)"
    , "  harvest.onExit(1, S, PARAMS)"
    , "  NOW = NOW + seconds"
    , "end"
    -- The collecting phase runs on execute alone: it is bookkeeping
    -- the action finishes AFTER the plant is gone, so utility has
    -- nothing left to score. Arbitration reaches it the same way,
    -- through whatever other ripe flora keeps auto_harvest winning.
    , "local function execOnly() NOW = NOW + STEP"
    , "  harvest.execute(1, S, PARAMS) end"
    , "local function harvested() return CALLS.harvest > 0 end"
    ]

spec ∷ Spec
spec = describe "skill-scaled auto-harvest" $ do

    describe "picking accumulates work instead of completing instantly" $ do
        it "does not call world.harvestFlora on the tick the worker \
           \reaches the plant" $
            runsOk $ lns
                [ prelude
                , "place(9, 0)   -- already adjacent"
                , "tick(0.5)"
                , "assert(not harvested(),"
                , "  'arriving adjacent must not harvest on the same tick')"
                , "assert(S.harvestTarget, 'the target must be held while working')"
                ]

        it "credits nothing for the walk: a worker out of reach \
           \accumulates no progress however long it takes to arrive" $
            runsOk $ lns
                [ prelude
                , "place(0, 0)"
                , "tick(30)"
                , "assert(CALLS.moveTo > 0, 'an out-of-reach target must be walked to')"
                , "assert(not harvested(), 'walking must not harvest')"
                , "assert((S.harvestProgress or 0) == 0,"
                , "  'walking must accumulate no picking progress')"
                ]

        it "completes at farming 50 in about WORK_TOTAL / (rate * 1.0) \
           \seconds, then grants XP and collects the yields" $
            runsOk $ lns
                [ prelude
                , "SKILL = 50.0"
                , "place(9, 0)"
                -- 0.5 progress/s * (0.5 + 50/100) = 0.5/s → 2 s of
                -- charged time, plus the arrival tick that charges none.
                , "tick(1.5)"
                , "assert(not harvested(), 'must still be working at 1.5 s')"
                , "tick(1.5)"
                , "assert(harvested(), 'must have completed by 3 s at farming 50')"
                , "assert(CALLS.pickup == 1, 'the bend-down anim must play once')"
                , "assert(XP == PARAMS.harvest_xp_per_harvest,"
                , "  'completion must grant farming XP exactly once')"
                , "assert(S.harvestTarget == nil,"
                , "  'a completed pick must forget its target')"
                -- The collecting phase then pulls the two ground yields
                -- one per tick and hands control back.
                , "assert(S.harvestPhase == 'collecting', 'yields must be collected')"
                , "execOnly()"
                , "assert(#GROUND == 1, 'exactly one yield comes off the ground per tick')"
                , "execOnly()"
                , "execOnly()"
                , "assert(#GROUND == 2, 'both yields must be picked up')"
                , "assert(S.harvestPhase == nil and S.harvestLoot == nil,"
                , "  'the collecting phase must end once the loot is gone')"
                ]

    describe "the farming skill decides how long the pick takes" $ do
        it "a farming-100 worker completes while an otherwise identical \
           \farming-0 worker is still working, and the low-skill one \
           \completes later" $
            runsOk $ lns
                [ prelude
                -- Two runs of the SAME fixture differing only in SKILL.
                -- 0.5 * (0.5 + 100/100) = 0.75/s → ~1.33 s charged;
                -- 0.5 * (0.5 +   0/100) = 0.25/s → 4 s charged.
                , "local function run(skill, seconds)"
                , "  NOW = 0; SKILL = skill; XP = 0; GROUND = {}"
                , "  CALLS = { find = 0, harvest = 0, pickup = 0, moveTo = 0,"
                , "            setSkill = 0, tags = {} }"
                , "  FLORA = { ['10,0'] = { { gid = 1 } } }"
                , "  S = {}"
                , "  place(9, 0)"
                , "  tick(seconds)"
                , "  return CALLS.harvest > 0"
                , "end"
                , "assert(run(100.0, 2.5),"
                , "  'a farming-100 worker must complete within 2.5 s')"
                , "assert(not run(0.0, 2.5),"
                , "  'a farming-0 worker must NOT be done in the same 2.5 s')"
                , "assert(run(0.0, 6.0),"
                , "  'the farming-0 worker must still complete, given longer')"
                ]

        it "takes its base rate from the named harvest_rate tunable the \
           \shipped acolyte block declares, not from a literal in the \
           \action" $
            runsOk $ lns
                [ prelude
                , "local shipped = require('scripts.unit_ai_tunables')"
                , "local rate = shipped.acolyte and shipped.acolyte.harvest_rate"
                , "assert(type(rate) == 'number' and rate > 0,"
                , "  'the acolyte block must declare a positive harvest_rate')"
                -- And the action really reads it: doubling the tunable
                -- halves the time, at one fixed skill level.
                , "local function timeToPick(r)"
                , "  NOW = 0; SKILL = 50.0"
                , "  CALLS = { find = 0, harvest = 0, pickup = 0, moveTo = 0,"
                , "            setSkill = 0, tags = {} }"
                , "  FLORA = { ['10,0'] = { { gid = 1 } } }"
                , "  S = {}"
                , "  PARAMS.harvest_rate = r"
                , "  place(9, 0)"
                , "  local elapsed = 0"
                , "  while CALLS.harvest == 0 and elapsed < 60 do"
                , "    step(0.5); elapsed = elapsed + 0.5"
                , "  end"
                , "  return elapsed"
                , "end"
                , "local slow = timeToPick(0.25)"
                , "local fast = timeToPick(0.5)"
                , "assert(slow < 60 and fast < slow,"
                , "  'a higher harvest_rate must finish the pick sooner')"
                ]

        it "an absent farming skill picks at the same 25.0 novice base \
           \till and plant fall back to, not at zero and not for free" $
            runsOk $ lns
                [ prelude
                -- unit.getSkill returning nil is the legacy-save shape.
                , "unit.getSkill = function() return nil end"
                , "place(9, 0)"
                -- 0.5 * (0.5 + 25/100) = 0.375/s → 1/0.375 ≈ 2.67 s.
                , "tick(2.0)"
                , "assert(not harvested(),"
                , "  'the novice fallback must not finish as fast as farming 50')"
                , "tick(1.5)"
                , "assert(harvested(),"
                , "  'the novice fallback must still be a real, finite rate')"
                ]

        it "reads the skill directly rather than through the derived \
           \role: roles.weight moves the utility, never the duration" $
            runsOk $ lns
                [ prelude
                -- A Farmer scores auto_harvest higher...
                , "S.role = 'farmer'"
                , "place(9, 0)"
                , "local farmerUtil = harvest.utility(1, S, PARAMS)"
                , "S.role = 'miner'"
                , "local minerUtil = harvest.utility(1, S, PARAMS)"
                , "assert(farmerUtil > minerUtil,"
                , "  'the derived role must still weight arbitration (#265)')"
                -- ...but two workers of equal SKILL and unequal role
                -- take exactly as long as each other.
                , "local function runRole(role)"
                , "  NOW = 0; SKILL = 50.0; CALLS = { find = 0, harvest = 0,"
                , "    pickup = 0, moveTo = 0, setSkill = 0, tags = {} }"
                , "  FLORA = { ['10,0'] = { { gid = 1 } } }"
                , "  S = { role = role }"
                , "  place(9, 0)"
                , "  local elapsed = 0"
                , "  while CALLS.harvest == 0 and elapsed < 30 do"
                , "    step(0.5); elapsed = elapsed + 0.5"
                , "  end"
                , "  return elapsed"
                , "end"
                , "assert(runRole('farmer') == runRole('miner'),"
                , "  'the role must not change how long a pick takes')"
                ]

    describe "progress belongs to one plant" $ do
        it "does not spend partial work on a plant that vanished: a \
           \nearer replacement target restarts the accumulator" $
            runsOk $ lns
                [ prelude
                , "place(9, 0)"
                , "tick(1.5)"
                , "assert(S.harvestProgress > 0, 'work must have accumulated')"
                , "assert(not harvested(), 'and must not be finished yet')"
                -- The plant under the worker is picked by someone else;
                -- another ripe one is adjacent on the far side.
                , "FLORA['10,0'] = nil"
                , "FLORA['8,0'] = { { gid = 3 } }"
                , "step(0.5)"
                , "assert(S.harvestTarget and S.harvestTarget.x == 8,"
                , "  'the scan must retarget the surviving plant')"
                , "assert(S.harvestProgress == 0,"
                , "  'work on the lost plant must not carry to the new one')"
                , "tick(1.0)"
                , "assert(not harvested(),"
                , "  'the new plant must take its own full time, not the remainder')"
                , "tick(1.0)"
                , "assert(harvested(), 'and then complete on its own schedule')"
                ]

        it "drops the accumulator when the last harvestable plant is \
           \gone, so a later one starts from zero" $
            runsOk $ lns
                [ prelude
                , "place(9, 0)"
                , "tick(1.5)"
                , "assert(S.harvestProgress > 0, 'work must have accumulated')"
                , "FLORA['10,0'] = nil"
                , "step(0.5)"
                , "assert(S.harvestTarget == nil, 'nothing left to target')"
                , "assert(S.harvestProgress == nil and S.harvestProgressAt == nil,"
                , "  'the accumulator must be dropped with the target')"
                , "FLORA['10,0'] = { { gid = 4 } }"
                , "tick(1.0)"
                , "assert(not harvested(),"
                , "  'the regrown plant must be picked from zero, not from the old remainder')"
                ]

        it "keeps the work already done across a preemption but charges \
           \the interruption itself nothing" $
            runsOk $ lns
                [ prelude
                , "place(9, 0)"
                , "tick(1.5)"
                , "local partial = S.harvestProgress"
                , "assert(partial > 0, 'work must have accumulated')"
                -- Combat, thirst, a player order: minutes elsewhere.
                , "preempt(600)"
                , "assert(S.harvestProgress == partial,"
                , "  'a preemption must not refund the work already done')"
                , "step(0.5)"
                , "assert(S.harvestProgress == partial,"
                , "  'and the resumed tick must not charge the gap either')"
                , "assert(not harvested(),"
                , "  'the interruption alone must not complete the pick')"
                ]

    describe "everything the action already did is unchanged" $ do
        it "scans and harvests UNTAGGED — the tag argument belongs to \
           \callers with a specific material in mind, like chop" $
            runsOk $ lns
                [ prelude
                , "place(9, 0)"
                , "tick(3.0)"
                , "assert(harvested(), 'the pick must complete')"
                , "assert(CALLS.tags.find == nil,"
                , "  'world.findHarvestableFlora must be called with no tag')"
                , "assert(CALLS.tags.harvest == nil,"
                , "  'world.harvestFlora must be called with no tag')"
                ]

        it "recovers from a raced completion: the plant is gone by the \
           \time the work finishes, so no yields, no XP, no target" $
            runsOk $ lns
                [ prelude
                , "place(9, 0)"
                -- Two plants, so the scan keeps the worker on this one
                -- until the moment its own yields come back empty.
                , "FLORA['10,0'] = {}   -- ripe to the scan, empty on the pick"
                , "tick(3.0)"
                , "assert(harvested(), 'the work must still have completed')"
                , "assert(CALLS.pickup == 0, 'no bend-down anim over nothing')"
                , "assert(XP == 0, 'a raced pick must grant no XP')"
                , "assert(S.harvestPhase == nil, 'and enter no collecting phase')"
                , "assert(S.harvestTarget == nil, 'the target must be forgotten')"
                ]

        it "requiring scripts.unit_ai_farm still attaches all three \
           \farming actions, so #1582's split moved no public shape" $
            runsOk $ lns
                [ prelude
                , "assert(type(unitAi.till) == 'table', 'unitAi.till must attach')"
                , "assert(type(unitAi.plant) == 'table', 'unitAi.plant must attach')"
                , "assert(type(unitAi.harvest) == 'table', 'unitAi.harvest must attach')"
                , "assert(type(unitAi.harvest.utility) == 'function'"
                , "   and type(unitAi.harvest.execute) == 'function'"
                , "   and type(unitAi.harvest.onExit) == 'function',"
                , "  'the auto_harvest action registry entry must resolve')"
                ]
