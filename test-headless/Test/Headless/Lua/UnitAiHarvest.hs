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
--   #1743 adds the case that closes the gap the rest of this file left
--   open: pending collection reaching @execute@ through ORDINARY
--   arbitration, over a fixture holding exactly one ripe plant, with no
--   @execOnly@ anywhere in it. Everything else here drives the
--   collecting phase through @execOnly@ on purpose, which is what let
--   @utility@ score @-math.huge@ for a pending collection — a score
--   arbitration can never select — without a single example failing.
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
    , "ACTIVITY = 'idle'"
    , "GROUND = {}"
    , "XP = 0"
    , "CALLS = { find = 0, harvest = 0, pickup = 0, moveTo = 0,"
    , "          stop = 0, setSkill = 0, tags = {} }"
    -- The capacity model the collecting phase gates on (#2293), and the
    -- ground rows it weighs. TAKEN is what makes "the refused yield is
    -- still on the ground" answerable: a successful pickup REMOVES the
    -- row and adds its live weight to the carried load, exactly as the
    -- engine would, so a row that still resolves afterwards is one that
    -- was genuinely left behind. MISSING is the separate raced case --
    -- a gid that never resolves at all.
    , "CARRIED, CAPACITY, ROW_WEIGHT = 0.0, 1000.0, 1.0"
    , "TAKEN, MISSING = {}, {}"
    , "PICKUP_CALLS = 0"
    , "WARNINGS = {}"
    , "FLORA = { ['10,0'] = { { gid = 1 }, { gid = 2 } } }"
    , "local function key(x, y) return string.format('%d,%d', x, y) end"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function(m) WARNINGS[#WARNINGS + 1] = m end,"
    , "           logInfo = function() end }"
    , "unit = {"
    , "  getInfo = function() return POS end,"
    , "  exists = function() return true end,"
    , "  getCarryingWeight = function() return CARRIED end,"
    , "  getStat = function(_, name)"
    , "    if name == 'carrying_capacity' then return CAPACITY end"
    , "    return 1.0 end,"
    , "  getSkill = function(_, name)"
    , "    if name ~= 'farming' then return 0.0 end"
    , "    return SKILL end,"
    , "  setSkill = function(_, _, v) CALLS.setSkill = CALLS.setSkill + 1"
    , "    SKILL = v end,"
    , "  addXP = function(_, _, amount) XP = XP + amount end,"
    , "  pickup = function() CALLS.pickup = CALLS.pickup + 1 end,"
    , "  moveTo = function(_, x, y) CALLS.moveTo = CALLS.moveTo + 1"
    , "    MOVED_TO = { x = x, y = y } end,"
    , "  getActivity = function() return ACTIVITY end,"
    , "  stop = function() CALLS.stop = CALLS.stop + 1"
    , "    ACTIVITY = 'idle' end,"
    , "  setAnimOverride = function() end,"
    , "  clearAnimOverride = function() end }"
    , "item = {"
    , "  getGroundForUnit = function(_, gid)"
    , "    if TAKEN[gid] or MISSING[gid] then return nil, true end"
    , "    return { id = gid, defName = 'crop', weight = ROW_WEIGHT }, true end,"
    , "  pickupGround = function(_, gid)"
    , "    PICKUP_CALLS = PICKUP_CALLS + 1"
    , "    if TAKEN[gid] or MISSING[gid] then return false end"
    , "    TAKEN[gid] = true"
    , "    CARRIED = CARRIED + ROW_WEIGHT"
    , "    GROUND[#GROUND + 1] = gid; return true end,"
    , "  listDefs = function() return {} end }"
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
    -- One arbitration pass over the single action under test, on
    -- scripts/unit_ai.lua's own re-execute rule: score it, take it as
    -- the winner, and execute on a SWITCH or when the unit is idle —
    -- never on a repeat tick of an action whose unit is still walking.
    , "local function step(dt)"
    , "  NOW = NOW + dt"
    , "  local u = harvest.utility(1, S, PARAMS)"
    , "  if u <= -math.huge then return u end"
    , "  local switching = S.currentAction ~= 'auto_harvest'"
    , "  S.currentAction = 'auto_harvest'"
    , "  if switching or ACTIVITY == 'idle' then"
    , "    harvest.execute(1, S, PARAMS)"
    , "  end"
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
    , "  S.currentAction = 'treat_ally'"
    , "  NOW = NOW + seconds"
    , "end"
    -- Execute in isolation, with no arbitration pass in front of it:
    -- what pins that #1743's utility change moved nothing INSIDE the
    -- collecting branch — one exact gid per call, consumed from the end
    -- of the recorded list, cleared on exhaustion. The arbitration half
    -- of that phase has its own case below, which uses no execOnly at
    -- all; do not "simplify" the two into one.
    , "local function execOnly() NOW = NOW + STEP"
    , "  harvest.execute(1, S, PARAMS) end"
    , "local function harvested() return CALLS.harvest > 0 end"
    ]

-- | The real @lua.unit_ai@ component, registered against a fake
--   @aiState@ the case fills in — the same shape
--   "Test.Headless.Lua.UnitAiStall" uses for its persistence cases.
--   @unit.exists@ is the only global the snapshot path reaches for a
--   state table carrying no job or claim fields.
savePrelude ∷ Text
savePrelude = lns
    [ "engine = { logWarn = function() end, logInfo = function() end }"
    , "unit = { exists = function() return true end }"
    , "local unitAiSave = require('scripts.unit_ai_save')"
    , "local saveModules = require('scripts.lib.save_modules')"
    , "codec = require('scripts.lib.data_codec')"
    , "aiState = {}"
    , "unitAiSave.register(aiState)"
    , "spec = saveModules.registry.unit_ai"
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
                -- Stop the arbitration ticks ON the completion tick.
                -- Since #1743 utility scores a pending collection, so a
                -- further ordinary tick here would collect a yield of
                -- its own and this case would no longer be measuring
                -- the execute branch in isolation.
                , "tick(1.0)"
                , "assert(harvested(), 'must have completed by 2.5 s at farming 50')"
                , "assert(CALLS.pickup == 1, 'the bend-down anim must play once')"
                , "assert(XP == PARAMS.harvest_xp_per_harvest,"
                , "  'completion must grant farming XP exactly once')"
                , "assert(S.harvestTarget == nil,"
                , "  'a completed pick must forget its target')"
                -- The collecting phase then pulls the two ground yields
                -- one per tick and hands control back.
                , "assert(S.harvestPhase == 'collecting', 'yields must be collected')"
                , "assert(#GROUND == 0, 'the completion tick itself collects nothing')"
                , "execOnly()"
                , "assert(#GROUND == 1, 'exactly one yield comes off the ground per tick')"
                , "execOnly()"
                , "execOnly()"
                , "assert(#GROUND == 2, 'both yields must be picked up')"
                , "assert(S.harvestPhase == nil and S.harvestLoot == nil,"
                , "  'the collecting phase must end once the loot is gone')"
                ]

    -- #1743. The gap every other case in this file leaves open:
    -- utility scored -math.huge the moment the last ripe plant was
    -- gone, which is the ONE state a completed harvest guarantees, so
    -- arbitration could never select the action that owns the pending
    -- collection. Everything here runs through a production-shaped
    -- arbitration pass -- idle is a real candidate scoring 0, and the
    -- winner must beat it strictly, exactly as scripts/unit_ai.lua
    -- selects -- and never calls execute directly.
    describe "a completed harvest collects its yields with no second plant" $ do
        it "keeps auto_harvest selectable through ordinary arbitration \
           \until the last yield is off the ground and the phase has \
           \cleared, scanning for nothing and harvesting nothing more" $
            runsOk $ lns
                [ prelude
                -- scripts/unit_ai_needs.lua registers idle at a flat 0,
                -- and scripts/unit_ai.lua seeds bestScore at -math.huge
                -- and selects on `u > bestScore`. Scoring idle FIRST is
                -- what makes this strict: auto_harvest has to come in
                -- ABOVE 0 to win, so a merely finite negative pending
                -- score -- which the prelude's single-action `step`
                -- would happily execute -- loses here.
                , "local IDLE_UTILITY = 0.0"
                , "local function arbitrate()"
                , "  NOW = NOW + STEP"
                , "  local best, bestScore = nil, -math.huge"
                , "  if IDLE_UTILITY > bestScore then"
                , "    best, bestScore = 'idle', IDLE_UTILITY end"
                , "  local u = harvest.utility(1, S, PARAMS)"
                , "  if u > bestScore then best, bestScore = 'auto_harvest', u end"
                , "  local switching = S.currentAction ~= best"
                , "  S.currentAction = best"
                , "  if best == 'auto_harvest' and (switching or ACTIVITY == 'idle') then"
                , "    harvest.execute(1, S, PARAMS)"
                , "  end"
                , "  return best, bestScore"
                , "end"
                -- Exactly ONE ripe plant in the whole fixture, yielding
                -- the prelude's two gids. Nothing below adds another.
                , "assert(FLORA['10,0'] and #FLORA['10,0'] == 2,"
                , "  'the fixture must hold one ripe plant yielding two gids')"
                , "local planted = 0"
                , "for _ in pairs(FLORA) do planted = planted + 1 end"
                , "assert(planted == 1, 'and no second harvestable plant')"
                , "place(9, 0)"
                -- Work that one plant to completion, stopping ON the
                -- completion tick so nothing has been collected yet.
                , "local guard = 0"
                , "while CALLS.harvest == 0 and guard < 60 do"
                , "  arbitrate(); guard = guard + 1"
                , "end"
                , "assert(CALLS.harvest == 1, 'the single plant must be picked')"
                , "assert(next(FLORA) == nil,"
                , "  'and the world must now hold no harvestable plant at all')"
                , "assert(S.harvestPhase == 'collecting' and #S.harvestLoot == 2,"
                , "  'the completion must leave two recorded gids to collect')"
                , "assert(#GROUND == 0, 'none of them collected yet')"
                -- The defect, stated as an assertion: a pending
                -- collection must outscore idle, and must reach that
                -- score without a scan (requirement 5).
                , "local findsAtCompletion = CALLS.find"
                , "assert(harvest.utility(1, S, PARAMS) > IDLE_UTILITY,"
                , "  'pending collection must score above idle, not -math.huge')"
                , "assert(CALLS.find == findsAtCompletion,"
                , "  'and must reach that score with no findHarvestableFlora scan')"
                -- Two one-item ticks, in the exact order the unindexed
                -- table.remove consumes the recorded list: from the END.
                , "local winner = arbitrate()"
                , "assert(winner == 'auto_harvest',"
                , "  'pending collection must win arbitration over idle')"
                , "assert(#GROUND == 1 and GROUND[1] == 2,"
                , "  'the first tick must pick up exactly gid 2')"
                , "winner = arbitrate()"
                , "assert(winner == 'auto_harvest', 'and win again for the second')"
                , "assert(#GROUND == 2 and GROUND[2] == 1,"
                , "  'the second tick must pick up exactly gid 1')"
                , "assert(S.harvestPhase == 'collecting',"
                , "  'the terminal cleanup tick is still owed')"
                -- ...then the terminal cleanup tick, which picks
                -- nothing up. Eligibility is the PHASE, not a non-empty
                -- list, or this state would strand in turn.
                , "winner = arbitrate()"
                , "assert(winner == 'auto_harvest',"
                , "  'the empty-list cleanup tick must still be selectable')"
                , "assert(#GROUND == 2, 'the cleanup tick collects nothing')"
                , "assert(S.harvestPhase == nil and S.harvestLoot == nil,"
                , "  'and clears the phase and its list')"
                -- Nothing was searched for, and nothing else was
                -- harvested, across all three of those ticks.
                , "assert(CALLS.find == findsAtCompletion,"
                , "  'no scan may run while a collection is pending')"
                , "assert(CALLS.harvest == 1,"
                , "  'and no second plant may be harvested to finish it')"
                , "assert(CALLS.pickup == 1,"
                , "  'the bend-down anim belongs to the pick, not to each yield')"
                -- The bypass is confined to the phase: with it cleared
                -- and no plant left, the scan resumes and idle wins.
                , "winner = arbitrate()"
                , "assert(winner == 'idle',"
                , "  'with the loot gone and no plant left, idle wins again')"
                , "assert(CALLS.find == findsAtCompletion + 1,"
                , "  'and only then does the ordinary scan resume')"
                ]

    describe "collection is gated on carrying capacity (#2293)" $ do
        it "a picker that cannot fit the next yield leaves it on the \
           \ground, ends the collection, and warns exactly once" $
            runsOk $ lns
                [ prelude
                -- Room for the FIRST yield and nothing after it: the
                -- 4 kg row fits under the 5 kg cap on an empty picker,
                -- and taking it puts the second one 3 kg over. The
                -- refusal is therefore reached by ordinary collecting,
                -- not by staging a unit that was already full.
                , "CAPACITY, ROW_WEIGHT = 5.0, 4.0"
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 1, 2 }"
                , "execOnly()"
                , "assert(#GROUND == 1 and GROUND[1] == 2,"
                , "  'the first yield must still be collected normally')"
                , "assert(S.harvestPhase == 'collecting',"
                , "  'and the collection must still be pending')"
                , "local callsBefore = PICKUP_CALLS"
                , "execOnly()"
                -- Requirement 1: the pickup does not happen at all.
                , "assert(PICKUP_CALLS == callsBefore,"
                , "  'a refused yield must not reach item.pickupGround')"
                , "assert(#GROUND == 1, 'and must not land in the inventory')"
                -- Requirement 2: the row is untouched and still there.
                , "local left = item.getGroundForUnit(1, 1)"
                , "assert(left and left.id == 1,"
                , "  'the refused yield must remain on the ground')"
                , "assert(left.weight == ROW_WEIGHT,"
                , "  'and must be neither deleted nor partially moved')"
                -- Requirement 3: the phase ends, and says so once.
                , "assert(S.harvestPhase == nil and S.harvestLoot == nil,"
                , "  'a refusal must end the collection phase')"
                , "assert(#WARNINGS == 1,"
                , "  'a refusal must warn exactly once, got ' .. #WARNINGS)"
                , "assert(WARNINGS[1]:find('leaving ground', 1, true),"
                , "  'the warning must carry the leaving ground outcome')"
                , "assert(WARNINGS[1]:find('unit 1', 1, true),"
                , "  'and must name the worker')"
                , "assert(WARNINGS[1]:find('crop', 1, true),"
                , "  'and must name the ground item')"
                -- ...and does not retry it every tick, which is what
                -- ending the phase rather than re-offering the yield
                -- buys.
                , "execOnly()"
                , "assert(PICKUP_CALLS == callsBefore and #WARNINGS == 1,"
                , "  'the refused yield must not be retried the next tick')"
                ]

        it "weighs the LIVE row on the picker's own page, not a static \
           \def weight, and re-reads the load it just took on" $
            runsOk $ lns
                [ prelude
                -- Two identical fixtures differing only in the live row
                -- weight the owning-page lookup reports. Deleting the
                -- getGroundForUnit read, or weighing anything but that
                -- row, makes these two agree.
                , "CAPACITY, ROW_WEIGHT = 10.0, 3.0"
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 1, 2, 3 }"
                , "execOnly(); execOnly(); execOnly()"
                , "assert(#GROUND == 3,"
                , "  'three 3 kg yields must all fit under a 10 kg cap')"
                , "assert(#WARNINGS == 0, 'and must draw no capacity warning')"
                -- The same three gids, the same cap, a heavier live row.
                , "CARRIED, TAKEN, GROUND = 0.0, {}, {}"
                , "PICKUP_CALLS, WARNINGS = 0, {}"
                , "ROW_WEIGHT = 6.0"
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 1, 2, 3 }"
                , "execOnly()"
                , "assert(#GROUND == 1, 'the first 6 kg yield still fits')"
                , "execOnly()"
                -- 6 + 6 > 10: the carried load the FIRST pickup added
                -- is what refuses the second, so the check re-reads
                -- unit.getCarryingWeight rather than gating once.
                , "assert(#GROUND == 1,"
                , "  'the second must be refused against the load just taken')"
                , "assert(S.harvestPhase == nil,"
                , "  'and must end the collection')"
                , "assert(#WARNINGS == 1, 'with one capacity warning')"
                ]

        it "an unresolvable yield ends the collection with no capacity \
           \warning: a raced row is not a refusal" $
            runsOk $ lns
                [ prelude
                -- Nothing about this unit is near its capacity; the gid
                -- simply does not resolve on its page any more.
                , "CAPACITY, ROW_WEIGHT = 1000.0, 1.0"
                , "MISSING[2] = true"
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 1, 2 }"
                , "execOnly()"
                , "assert(#GROUND == 0,"
                , "  'a vanished row must not be picked up')"
                , "assert(PICKUP_CALLS == 0,"
                , "  'and must not reach item.pickupGround')"
                , "assert(S.harvestPhase == nil and S.harvestLoot == nil,"
                , "  'but must still end the collection cleanly')"
                , "assert(#WARNINGS == 0,"
                , "  'and must NOT be reported as a capacity refusal')"
                ]

        it "a pickup that loses its race after the check ends the \
           \collection with no capacity warning either" $
            runsOk $ lns
                [ prelude
                -- The row resolves and weighs in fine; the pickup then
                -- fails anyway. That is the raced commit, not a
                -- capacity decision.
                , "CAPACITY, ROW_WEIGHT = 1000.0, 1.0"
                , "item.pickupGround = function()"
                , "  PICKUP_CALLS = PICKUP_CALLS + 1; return false end"
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 1, 2 }"
                , "execOnly()"
                , "assert(PICKUP_CALLS == 1,"
                , "  'an admitted yield must still be attempted')"
                , "assert(S.harvestPhase == nil and S.harvestLoot == nil,"
                , "  'and a lost race must end the collection')"
                , "assert(#WARNINGS == 0,"
                , "  'a lost race is not a capacity refusal')"
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
                -- The two runs re-use gid 1, so the load model resets
                -- with the rest of the fixture or the second run's
                -- pickup would race against the first run's row.
                , "  CARRIED = 0.0; TAKEN = {}; MISSING = {}"
                , "  PICKUP_CALLS = 0; WARNINGS = {}"
                , "  CALLS = { find = 0, harvest = 0, pickup = 0, moveTo = 0,"
                , "            stop = 0, setSkill = 0, tags = {} }"
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
                , "            stop = 0, setSkill = 0, tags = {} }"
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
                , "    pickup = 0, moveTo = 0, stop = 0, setSkill = 0, tags = {} }"
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

    describe "an interruption is never picking time" $ do
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

        it "charges nothing for the walk a switch lands in the middle \
           \of: dispatch executes a winning action while the unit is \
           \still moving, so picking must stop it first and start no \
           \clock" $
            runsOk $ lns
                [ prelude
                -- Another action owned the unit and had it walking; the
                -- route happens to pass right by a ripe plant, so
                -- auto_harvest wins arbitration mid-stride. This is
                -- unit_ai.lua's `switching` branch: execute fires even
                -- though the unit is NOT idle.
                , "S.currentAction = 'store_materials'"
                , "ACTIVITY = 'walking'"
                , "place(9, 0)   -- already adjacent, but still moving"
                , "step(0.5)"
                , "assert(CALLS.stop == 1,"
                , "  'entering adjacent harvest while moving must stop the unit')"
                , "assert(S.lastHarvestAt == nil,"
                , "  'and must start no picking clock on that tick')"
                , "assert(not harvested(), 'nor harvest anything yet')"
                -- Whatever the walk was still doing takes a while to
                -- settle; none of it is picking.
                , "NOW = NOW + 4.0"
                , "step(0.5)"
                , "assert((S.harvestProgress or 0) == 0,"
                , "  'the travel interval must not be charged as picking')"
                -- From here it is an ordinary stationary pick.
                , "tick(2.0)"
                , "assert(harvested(),"
                , "  'and the pick then completes on its own working time')"
                ]

        it "charges nothing for a collapse or a mid-animation tick, \
           \which scripts/unit_ai.lua swallows through \
           \core.suspendOrders without firing any onExit" $
            runsOk $ lns
                [ prelude
                -- The state table the dispatcher itself would pass, so
                -- core.suspendOrders(uid) reaches this very case's `S`.
                , "local core = require('scripts.unit_ai_core')"
                , "S = core.ensureState(1)"
                , "place(9, 0)"
                , "tick(1.5)"
                , "local partial = S.harvestProgress"
                , "assert(partial > 0 and not harvested(),"
                , "  'work must be underway but unfinished')"
                -- unit_ai.lua's pose/activity short-circuit returns
                -- core.suspendOrders(uid) BEFORE arbitration, so no
                -- action's onExit runs — this is the only boundary the
                -- picking clock gets. Deliberately a SHORT stun, well
                -- inside MAX_CHARGED_INTERVAL so the backstop below
                -- cannot cover for a missing boundary: 2 s of it is
                -- more than the farming-50 pick has left to do, so a
                -- charged one would finish the plant outright.
                , "for _ = 1, 4 do NOW = NOW + 0.5; core.suspendOrders(1) end"
                , "step(0.5)"
                , "assert(S.harvestProgress == partial,"
                , "  'a swallowed tick must charge the accumulator nothing')"
                , "assert(not harvested(),"
                , "  'and a collapse must never complete a pick by itself')"
                -- ...and the pick then finishes on its own remaining time.
                , "tick(1.5)"
                , "assert(harvested(), 'the resumed pick must still complete')"
                ]

        it "charges nothing for a gap NO path announced either: an \
           \interval past MAX_CHARGED_INTERVAL is not one uninterrupted \
           \stretch of picking, so it counts zero rather than the bound" $
            runsOk $ lns
                [ prelude
                , "local stall = require('scripts.unit_ai_stall')"
                , "place(9, 0)"
                , "tick(1.5)"
                , "local partial = S.harvestProgress"
                , "assert(partial > 0 and not harvested(),"
                , "  'work must be underway but unfinished')"
                -- A save/load boundary, or a unit that simply stopped
                -- being ticked: the clock jumps with no sample and no
                -- suspendOrders call anywhere.
                , "NOW = NOW + (stall.MAX_CHARGED_INTERVAL + 600)"
                , "step(0.5)"
                , "assert(S.harvestProgress == partial,"
                , "  'an unannounced gap must charge nothing at all')"
                , "assert(not harvested(),"
                , "  'and must not complete the pick')"
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

        it "leaves the lua.unit_ai payload shape alone: the picking \
           \accumulator is transient, so no save carries it and no \
           \component version moves" $
            runsOk $ lns
                [ savePrelude
                -- A unit caught mid-pick at save time.
                , "aiState[1] = { currentAction = 'auto_harvest',"
                , "  harvestTarget = { x = 10, y = 0 },"
                , "  harvestProgress = 0.75,"
                , "  harvestProgressAt = { x = 10, y = 0 },"
                , "  lastHarvestAt = 120 }"
                -- #1582 moved no version, and this pin is what says
                -- so -- but it pins the component's CURRENT number, not
                -- the one standing when auto-harvest landed, so a LATER
                -- unrelated bump (v7, #1737's ground-repair provenance;
                -- v8, #1844's construct-job attempt identity; v9,
                -- #1845's staked-building reference) updates it here
                -- rather than being mistaken for one this action
                -- caused. The real guarantee is the payload-shape
                -- assertions below.
                , "assert(spec.version == 9,"
                , "  'auto-harvest must contribute no version move; the '"
                , "  .. 'component is at v9, got '"
                , "  .. tostring(spec.version))"
                , "local snap = spec.snapshot()"
                , "local row = snap[1]"
                , "assert(row, 'the unit must still be snapshotted')"
                , "assert(row.harvestProgress == nil"
                , "   and row.harvestProgressAt == nil"
                , "   and row.lastHarvestAt == nil,"
                , "  'the accumulator and its clock must not reach the payload')"
                -- Everything durable about the action is untouched.
                , "assert(row.currentAction == 'auto_harvest',"
                , "  'the action itself still persists')"
                , "assert(row.harvestTarget and row.harvestTarget.x == 10,"
                , "  'and so does the target, exactly as before #1582')"
                -- And a real round trip restores a unit that picks from
                -- zero rather than one carrying a stale clock.
                , "local decoded = spec.decode(spec.version,"
                , "  codec.decode(codec.encode(snap)))"
                , "for k in pairs(aiState) do aiState[k] = nil end"
                , "spec.apply(decoded, nil)"
                , "local restored = aiState[1]"
                , "assert(restored, 'the row must survive the round trip')"
                , "assert(restored.harvestProgress == nil"
                , "   and restored.lastHarvestAt == nil,"
                , "  'a loaded picker starts its plant over, never mid-pick')"
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
