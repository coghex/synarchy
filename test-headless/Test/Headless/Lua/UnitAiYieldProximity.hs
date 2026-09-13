{-# LANGUAGE TypeApplications #-}
-- | The "harvest collection proximity" gate (#2550).
--
--   Auto-harvest and foraging both finish a pick by leaving the yields
--   on the ground as ordinary items and then draining a recorded gid
--   list one item per tick. Neither phase is cleared by a preemption,
--   and @item.pickupGround@ compares no positions of its own
--   (@src\/Engine\/Scripting\/Lua\/API\/Items\/Ground.hs@ moves the
--   resolved instance into the inventory unconditionally), so before
--   #2550 a worker could harvest, leave for a drink, and then pull its
--   old yields in from ten tiles away.
--
--   What the cases pin: a retained yield out of reach is WALKED to
--   rather than collected, the gid stays pending across the approach
--   and across a second interruption, proximity is re-measured on
--   every tick from the live row on the worker's OWN page, adjacency is
--   the floored-tile Chebyshev @<= 1@ the surrounding approach branches
--   already use, and it is measured over the page's cylindrical
--   u-images so a yield across the U seam is near rather than a world
--   away. Alongside them: adjacent collection is unchanged, a missing
--   yield still ends the phase, an UNREACHABLE yield ends it too rather
--   than oscillating forever, auto-harvest's capacity admission runs
--   only once adjacency holds (so a long approach cannot warn once per
--   step) while foraging stays deliberately exempt, and nothing about
--   the arc re-harvests a plant or grants harvesting XP twice.
--
--   Mutation checks this group is written to survive — a reviewer
--   should be able to break the fix and watch it fail:
--
--     * Restore the pickup ahead of the distance test (collect the tail
--       gid unconditionally) and the auto-harvest case "walks to a
--       retained yield..." and the foraging case "walks to a retained
--       forage yield..." both fail.
--     * Reduce the seam handling to raw subtraction (drop the
--       @world.getWrapWidth@ round trip, or the three-image alias set)
--       and "a yield across the U seam is adjacent..." fails while
--       every other case still passes.
--     * Evaluate capacity admission on approach ticks instead of at
--       adjacency and "judges capacity at the moment of truth..."
--       fails on the warning count.
--     * @table.remove@ the gid on an approach tick and "keeps the gid
--       pending..." fails.
--     * Move the approach budget from @tickCollection@ (utility) back
--       into @nextYield@ (execute) and both "ends the collection
--       cleanly when the yield cannot be reached at all" and "charges
--       the budget from the UTILITY tick" fail — the stuck-walk
--       livelock, whose watchdog cycle outlasts
--       @stall.MAX_CHARGED_INTERVAL@ so an execute-side sample charges
--       zero forever.
--     * Leave a vanished row for execute to retire instead of ending it
--       from @tickCollection@ and "ends a forage collection when the
--       retained yield is gone" fails: forage is not selectable on that
--       tick, so nothing else can clean it up.
--     * Ungate the closest-approach reset, or the adjacency clear, from
--       @eligible@ and "is not refunded by an interrupting action..."
--       fails on each.
--
--   Same standalone-Lua-VM pattern as
--   "Test.Headless.Lua.UnitAiHarvest", which owns the neighbouring
--   @skill-scaled auto-harvest@ gate: each 'it' runs one self-contained
--   chunk in a fresh interpreter, requires the PRODUCTION Lua modules
--   unmodified, and stubs every engine verb. The stubs model the
--   registered verbs' contracts rather than standing in for them —
--   @item.getGroundForUnit@ returns a row carrying @x@\/@y@ and the
--   two-value @(entry|nil, pageResolved)@ shape @Ground.hs@ documents,
--   @item.pickupGround@ performs no distance check whatsoever, and
--   @world.getWrapWidth@ answers per page.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "harvest collection proximity"'@.
module Test.Headless.Lua.UnitAiYieldProximity (spec) where

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

-- | The engine surface both actions share, and the only part of this
--   fixture that models the world rather than the AI.
--
--   @GROUND_AT@ is what makes every proximity assertion below
--   answerable: a ground row's @x@\/@y@ are the live page-local
--   position of the exact instance @item.pickupGround@ would move
--   (@pushGroundRow@ builds the row for both @item.listGround@ and
--   @item.getGroundForUnit@), and a stub without them would let a
--   "proximity" test pass against code that reads no coordinates at
--   all.
--
--   @advance()@ is the engine walking the unit between AI ticks: one
--   tile per tick toward the last @unit.moveTo@ destination, going
--   @idle@ on arrival. It is what makes the dispatcher's re-execute
--   rule (@switching or activity == 'idle'@, modelled in each
--   @step@ below) bite — an action that issued a walk does not run
--   again until the walk ends, exactly as @scripts\/unit_ai.lua@
--   dispatches.
worldStubs ∷ Text
worldStubs = lns
    [ "NOW = 0"
    , "POS = { gridX = 9, gridY = 0, page = 'p1' }"
    , "ACTIVITY = 'idle'"
    , "WRAP = 0"
    , "WRAP_PAGES = {}"
    , "GROUND = {}"
    , "GROUND_AT = {}"
    , "TAKEN, MISSING = {}, {}"
    , "PICKUP_CALLS = 0"
    , "PICKED = {}"
    , "WARNINGS = {}"
    , "MOVED_TO = nil"
    , "CARRIED, CAPACITY, ROW_WEIGHT = 0.0, 1000.0, 1.0"
    , "INVENTORY = {}"
    , "XP = 0"
    , "CALLS = { find = 0, harvest = 0, pickup = 0, moveTo = 0,"
    , "          stop = 0, tags = {} }"
    , "FLORA = { ['10,0'] = { { gid = 1 }, { gid = 2 } } }"
    , "WALK_TILES_PER_TICK = 1.0"
    , "local function key(x, y) return string.format('%d,%d', x, y) end"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function(m) WARNINGS[#WARNINGS + 1] = m end,"
    , "           logInfo = function() end }"
    , "unit = {"
    , "  getInfo = function() return POS end,"
    , "  exists = function() return true end,"
    , "  getInventory = function() return INVENTORY end,"
    , "  getCarryingWeight = function() return CARRIED end,"
    , "  getStat = function(_, name)"
    , "    if name == 'carrying_capacity' then return CAPACITY end"
    , "    if name == 'hunger' then return HUNGER end"
    , "    if name == 'max_hunger' then return 100.0 end"
    , "    if name == 'calories' then return CALORIES end"
    , "    if name == 'max_calories' then return 100.0 end"
    , "    return 1.0 end,"
    , "  getSkill = function() return 50.0 end,"
    , "  addXP = function(_, _, amount) XP = XP + amount end,"
    , "  pickup = function() CALLS.pickup = CALLS.pickup + 1 end,"
    , "  feed = function() end,"
    , "  moveTo = function(_, x, y) CALLS.moveTo = CALLS.moveTo + 1"
    , "    MOVED_TO = { x = x, y = y }; ACTIVITY = 'walking' end,"
    , "  getActivity = function() return ACTIVITY end,"
    , "  stop = function() CALLS.stop = CALLS.stop + 1"
    , "    ACTIVITY = 'idle' end,"
    , "  setAnimOverride = function() end,"
    , "  clearAnimOverride = function() end }"
    -- The registered ground verbs' contracts, and nothing more. Note
    -- pickupGround: no coordinate read, no distance test, no weight —
    -- the whole point is that the CALLER has to gate it.
    , "item = {"
    , "  getGroundForUnit = function(_, gid)"
    , "    if TAKEN[gid] or MISSING[gid] then return nil, true end"
    , "    local at = GROUND_AT[gid]"
    , "    if not at then return nil, true end"
    , "    return { id = gid, defName = 'berry', weight = ROW_WEIGHT,"
    , "             x = at.x, y = at.y }, true end,"
    , "  listGround = function()"
    , "    local out = {}"
    , "    for gid in pairs(GROUND_AT) do"
    , "      if not TAKEN[gid] and not MISSING[gid] then"
    , "        out[#out + 1] = { id = gid } end end"
    , "    table.sort(out, function(a, b) return a.id < b.id end)"
    , "    return out end,"
    , "  getFood = function() return { calories = 100.0 } end,"
    , "  pickupGround = function(_, gid)"
    , "    PICKUP_CALLS = PICKUP_CALLS + 1"
    , "    if TAKEN[gid] or MISSING[gid] then return false end"
    , "    TAKEN[gid] = true"
    , "    CARRIED = CARRIED + ROW_WEIGHT"
    , "    GROUND[#GROUND + 1] = gid"
    , "    PICKED[gid] = true"
    , "    INVENTORY[#INVENTORY + 1] ="
    , "      { food = { nutrition = { calories = 100.0 } } }"
    , "    return true end,"
    , "  listDefs = function() return {} end }"
    , "world = {"
    , "  getActiveWorldId = function() return 'p1' end,"
    -- Page-scoped, as world.getWrapWidth is: the page it was ASKED
    -- about is recorded so a case can prove the actor's own page was
    -- the one consulted, never the active or visible one.
    , "  getWrapWidth = function(p)"
    , "    WRAP_PAGES[#WRAP_PAGES + 1] = p"
    , "    return WRAP end,"
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
    -- A picked plant drops its yields ON the harvested tile.
    , "    for _, yi in ipairs(yields or {}) do"
    , "      GROUND_AT[yi.gid] = { x = gx + 0.5, y = gy + 0.5 } end"
    , "    return yields or {} end }"
    , "function place(x, y) POS.gridX, POS.gridY = x, y end"
    , "NO_WALK = false"
    -- scripts/unit_ai.lua's own default (params.stuck_walk_timeout).
    -- It is deliberately LONGER than stall.MAX_CHARGED_INTERVAL, which
    -- is what makes the stuck-walk path the hard case: a sampler that
    -- only runs between walks sees nothing but intervals past the bound
    -- — and an interval past the bound charges ZERO, not the bound.
    , "STUCK_WALK_TIMEOUT = 6.0"
    , "STUCK_FOR = 0.0"
    , "WATCHDOG_STOPS = 0"
    , "function advance()"
    -- An unreachable destination. The unit does NOT bounce back to idle
    -- on the next tick: it stays `walking` at an unpathable waypoint,
    -- and only unit_ai.lua's stuck-walk watchdog returns it to idle,
    -- after STUCK_WALK_TIMEOUT of no position progress. Modelling that
    -- honestly is the whole point — an instant-idle stub would hand the
    -- action an execute tick every 0.5 s and hide the defect.
    , "  if NO_WALK then"
    , "    if ACTIVITY == 'walking' then"
    , "      STUCK_FOR = STUCK_FOR + STEP"
    , "      if STUCK_FOR >= STUCK_WALK_TIMEOUT then"
    , "        STUCK_FOR = 0.0"
    , "        WATCHDOG_STOPS = WATCHDOG_STOPS + 1"
    , "        ACTIVITY = 'idle'"
    , "      end"
    , "    end"
    , "    return"
    , "  end"
    , "  if ACTIVITY ~= 'walking' or not MOVED_TO then return end"
    , "  local dx = MOVED_TO.x - POS.gridX"
    , "  local dy = MOVED_TO.y - POS.gridY"
    , "  local d = math.sqrt(dx * dx + dy * dy)"
    , "  if d <= WALK_TILES_PER_TICK or d == 0 then"
    , "    POS.gridX, POS.gridY = MOVED_TO.x, MOVED_TO.y"
    , "    ACTIVITY = 'idle'"
    , "  else"
    , "    POS.gridX = POS.gridX + dx / d * WALK_TILES_PER_TICK"
    , "    POS.gridY = POS.gridY + dy / d * WALK_TILES_PER_TICK"
    , "  end"
    , "end"
    , "STEP = 0.5"
    ]

-- | Auto-harvest driven the way @scripts\/unit_ai.lua@ drives it.
harvestPrelude ∷ Text
harvestPrelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , worldStubs
    , "require('scripts.unit_ai_farm')"
    , "local mv = require('scripts.movement_speed')"
    , "mv.comfort = function() return 1.0 end"
    , "mv.ordered = function() return 2.0 end"
    , "local unitAi = package.loaded['scripts.unit_ai']"
    , "harvest = unitAi.harvest"
    , "yieldCollect = require('scripts.unit_ai_yield')"
    , "PARAMS = { harvest_scan_range = 24.0, harvest_base_utility = 2.0,"
    , "           harvest_rate = 0.5, harvest_xp_per_harvest = 1.0 }"
    , "S = {}"
    , "function step(dt)"
    , "  NOW = NOW + (dt or STEP)"
    , "  advance()"
    , "  local u = harvest.utility(1, S, PARAMS)"
    , "  if u <= -math.huge then S.currentAction = 'idle'; return u end"
    , "  local switching = S.currentAction ~= 'auto_harvest'"
    , "  S.currentAction = 'auto_harvest'"
    , "  if switching or ACTIVITY == 'idle' then"
    , "    harvest.execute(1, S, PARAMS)"
    , "  end"
    , "  return u"
    , "end"
    -- Another action owned the unit for `seconds`, and left it at
    -- (x, y). Arbitration fires the outgoing action's onExit on the way
    -- out, which is the boundary the approach budget takes.
    , "function preempt(seconds, x, y)"
    , "  harvest.onExit(1, S, PARAMS)"
    , "  S.currentAction = 'treat_ally'"
    , "  NOW = NOW + seconds"
    , "  ACTIVITY = 'idle'"
    , "  MOVED_TO = nil"
    , "  if x then place(x, y) end"
    , "end"
    -- ONE thought tick that another action owns. Arbitration scores
    -- every registered action every tick, so harvest's utility -- and
    -- with it the collection budget it samples -- still runs; what does
    -- not happen is this action winning or executing. s.currentAction
    -- is set BEFORE the scoring pass, exactly as unit_ai.lua leaves it,
    -- so it names the action that owned the interval just elapsed.
    , "function foreignStep(dt, x, y)"
    , "  NOW = NOW + (dt or STEP)"
    , "  if x then place(x, y) end"
    , "  S.currentAction = 'treat_ally'"
    , "  harvest.utility(1, S, PARAMS)"
    , "end"
    -- Work the fixture's single plant through to a pending collection,
    -- adjacent, exactly as an uninterrupted picker would.
    , "function harvestToCollecting()"
    , "  place(9, 0)"
    , "  local guard = 0"
    , "  while S.harvestPhase ~= 'collecting' and guard < 120 do"
    , "    step(); guard = guard + 1"
    , "  end"
    , "  assert(S.harvestPhase == 'collecting',"
    , "    'the fixture must reach a pending collection')"
    , "  assert(CALLS.harvest == 1, 'by picking exactly one plant')"
    , "  assert(#S.harvestLoot == 2, 'leaving two recorded yields')"
    , "  assert(#GROUND == 0, 'none of them collected yet')"
    , "end"
    ]

-- | Foraging driven through the production @unit_ai_needs@ module.
foragePrelude ∷ Text
foragePrelude = lns
    [ worldStubs
    -- Starving, carrying nothing: #94's emergency hunger ladder is
    -- what selects forage at all, and an empty inventory is what keeps
    -- eat_from_inventory from outranking it.
    , "HUNGER, CALORIES = 10.0, 0.0"
    , "needs = require('scripts.unit_ai_needs')"
    , "local mv = require('scripts.movement_speed')"
    , "mv.comfort = function() return 1.0 end"
    , "mv.ordered = function() return 2.0 end"
    , "yieldCollect = require('scripts.unit_ai_yield')"
    , "PARAMS = { forage_max_fraction = 0.5, forage_search_radius = 24,"
    , "           forage_base_weight = 1.0, forage_urgency_scale = 7.4 }"
    , "S = {}"
    , "function forageStep(dt)"
    , "  NOW = NOW + (dt or STEP)"
    , "  advance()"
    , "  local u = needs.forageUtility(1, S, PARAMS)"
    , "  if u <= -math.huge then S.currentAction = 'idle'; return u end"
    , "  local switching = S.currentAction ~= 'forage'"
    , "  S.currentAction = 'forage'"
    , "  if switching or ACTIVITY == 'idle' then"
    , "    needs.forageExecute(1, S, PARAMS)"
    , "  end"
    , "  return u"
    , "end"
    -- Forage registers no onExit (scripts/unit_ai_actions.lua), so a
    -- preemption announces itself to the approach clock through
    -- unit_ai_stall.suspendOrders — the same boundary unit_ai.lua's
    -- collapsed-pose return takes.
    , "function foragePreempt(seconds, x, y)"
    , "  require('scripts.unit_ai_stall').suspendOrders(S, 1)"
    , "  S.currentAction = 'treat_ally'"
    , "  NOW = NOW + seconds"
    , "  ACTIVITY = 'idle'"
    , "  MOVED_TO = nil"
    , "  if x then place(x, y) end"
    , "end"
    , "function forageToCollecting()"
    , "  FLORA = { ['10,0'] = { { gid = 5 } } }"
    , "  place(9, 0)"
    , "  local guard = 0"
    , "  while S.foragePhase ~= 'collecting' and guard < 60 do"
    , "    forageStep(); guard = guard + 1"
    , "  end"
    , "  assert(S.foragePhase == 'collecting',"
    , "    'the forager must reach a pending collection')"
    , "  assert(#S.forageLoot == 1, 'holding its single recorded yield')"
    , "  assert(#GROUND == 0, 'not yet collected')"
    , "end"
    ]

spec ∷ Spec
spec = describe "harvest collection proximity" $ do

    describe "auto-harvest walks back to a retained yield" $ do
        it "walks to a retained yield an interruption left tiles away \
           \instead of collecting it remotely, keeps the gid pending \
           \for the whole approach, and takes exactly one item on the \
           \first adjacent tick" $
            runsOk $ lns
                [ harvestPrelude
                , "harvestToCollecting()"
                , "local harvestsAtCompletion, xpAtCompletion = CALLS.harvest, XP"
                -- Thirst, combat, a player order: the phase and its
                -- loot list survive, and the worker does not.
                , "preempt(30, 20, 0)"
                , "assert(S.harvestPhase == 'collecting' and #S.harvestLoot == 2,"
                , "  'an interruption must not discard the pending collection')"
                -- The defect, stated as an assertion.
                , "local movesBefore = CALLS.moveTo"
                , "step()"
                , "assert(PICKUP_CALLS == 0,"
                , "  'a yield eleven tiles away must not be picked up')"
                , "assert(CALLS.moveTo == movesBefore + 1,"
                , "  'the worker must walk toward it instead')"
                , "assert(MOVED_TO.x == 10.5 and MOVED_TO.y == 0.5,"
                , "  'and must steer to the live row, not to a stale target')"
                , "assert(#S.harvestLoot == 2,"
                , "  'the gid must stay pending while travelling')"
                -- Every approach tick: the row is still lying there and
                -- nothing has been taken.
                , "local ticks = 0"
                , "while PICKUP_CALLS == 0 and ticks < 120 do"
                , "  local pending = S.harvestLoot[#S.harvestLoot]"
                , "  assert(item.getGroundForUnit(1, pending),"
                , "    'the yield must remain on the ground while approaching')"
                , "  assert(#S.harvestLoot == 2,"
                , "    'and must not be consumed by an approach tick')"
                , "  step(); ticks = ticks + 1"
                , "end"
                , "assert(ticks > 1,"
                , "  'eleven tiles must take more than one tick to close')"
                , "assert(PICKUP_CALLS == 1,"
                , "  'exactly one pickup, on the first tick adjacency holds')"
                , "assert(#GROUND == 1 and GROUND[1] == 2,"
                , "  'and it must be the exact retained instance')"
                , "assert(#S.harvestLoot == 1,"
                , "  'only the collected gid leaves the list')"
                -- Requirement 4: resuming a collection re-harvests
                -- nothing and re-earns nothing.
                , "assert(CALLS.harvest == harvestsAtCompletion,"
                , "  'resuming a collection must not re-harvest a plant')"
                , "assert(XP == xpAtCompletion,"
                , "  'nor grant harvesting XP a second time')"
                , "assert(CALLS.pickup == 1,"
                , "  'the bend-down anim belongs to the pick, not to the walk')"
                -- The second yield is underfoot now, so it comes in on
                -- the next tick and the phase then clears.
                , "step()"
                , "assert(#GROUND == 2 and GROUND[2] == 1,"
                , "  'the second yield is collected in place')"
                , "step()"
                , "assert(S.harvestPhase == nil and S.harvestLoot == nil,"
                , "  'and the terminal tick clears the phase')"
                ]

        it "rechecks proximity after a SECOND interruption: an approach \
           \carried further away by another action is re-measured and \
           \re-issued, never completed on the strength of the first \
           \check" $
            runsOk $ lns
                [ harvestPrelude
                , "harvestToCollecting()"
                , "preempt(30, 20, 0)"
                , "step()"
                , "assert(CALLS.moveTo == 1 and PICKUP_CALLS == 0,"
                , "  'the first resume must issue a walk and take nothing')"
                -- Part-way there...
                , "step(); step()"
                , "assert(PICKUP_CALLS == 0, 'still short of the yield')"
                , "local closed = POS.gridX"
                , "assert(closed < 20, 'and genuinely closer than it started')"
                -- ...and preempted again, this time DRAGGED FURTHER OUT.
                , "preempt(45, 30, 0)"
                , "assert(S.harvestPhase == 'collecting' and #S.harvestLoot == 2,"
                , "  'the second interruption must not discard the yield')"
                , "local movesBefore = CALLS.moveTo"
                , "step()"
                , "assert(PICKUP_CALLS == 0,"
                , "  'the earlier proximity check must not authorise a pickup')"
                , "assert(CALLS.moveTo == movesBefore + 1,"
                , "  'a fresh walk must be issued from the new position')"
                , "assert(MOVED_TO.x == 10.5,"
                , "  'toward the same live row')"
                -- ...and it still finishes.
                , "local ticks = 0"
                , "while PICKUP_CALLS == 0 and ticks < 120 do"
                , "  step(); ticks = ticks + 1"
                , "end"
                , "assert(PICKUP_CALLS == 1 and #GROUND == 1,"
                , "  'the twice-interrupted collection must still complete')"
                ]

        it "stops a worker that is still moving when it reaches \
           \adjacency, then picks up exactly once — dispatch can \
           \execute a winning action mid-stride" $
            runsOk $ lns
                [ harvestPrelude
                , "harvestToCollecting()"
                -- Another action owned the unit and had it walking; the
                -- route happens to pass right by the yields, so
                -- auto_harvest wins back mid-stride. unit_ai.lua's
                -- `switching` branch executes even though ACTIVITY is
                -- not idle.
                , "harvest.onExit(1, S, PARAMS)"
                , "S.currentAction = 'store_materials'"
                , "place(10, 1)   -- adjacent to the yields at (10, 0)"
                , "ACTIVITY = 'walking'"
                , "MOVED_TO = { x = 40.5, y = 40.5 }"
                , "local stopsBefore = CALLS.stop"
                , "step()"
                , "assert(CALLS.stop == stopsBefore + 1,"
                , "  'arriving adjacent while moving must stop the unit once')"
                , "assert(PICKUP_CALLS == 1,"
                , "  'and collect exactly one yield on that same tick')"
                , "assert(CALLS.moveTo == 0,"
                , "  'an adjacent yield must never be walked to')"
                ]

    describe "the existing collection behaviour is unchanged" $ do
        it "collects an adjacent yield in place, one item per tick, \
           \with no walk at all" $
            runsOk $ lns
                [ harvestPrelude
                , "harvestToCollecting()"
                , "assert(CALLS.moveTo == 0,"
                , "  'the fixture must have reached the plant without a recorded walk')"
                , "step()"
                , "assert(#GROUND == 1 and GROUND[1] == 2,"
                , "  'exactly one yield comes off the ground per tick')"
                , "step()"
                , "assert(#GROUND == 2 and GROUND[2] == 1,"
                , "  'in the order the recorded list is consumed: from the END')"
                , "assert(S.harvestPhase == 'collecting',"
                , "  'the terminal cleanup tick is still owed')"
                , "step()"
                , "assert(S.harvestPhase == nil and S.harvestLoot == nil,"
                , "  'and clears the phase and its list')"
                , "assert(CALLS.moveTo == 0,"
                , "  'no part of an adjacent collection may issue a walk')"
                ]

        it "a yield that is no longer on the worker's page ends the \
           \collection in that tick and leaves the rest lying there, \
           \whether the worker is adjacent or still approaching" $
            runsOk $ lns
                [ harvestPrelude
                , "harvestToCollecting()"
                -- Adjacent, and the tail gid raced away.
                , "MISSING[2] = true"
                , "step()"
                , "assert(PICKUP_CALLS == 0,"
                , "  'a vanished row must not reach item.pickupGround')"
                , "assert(S.harvestPhase == nil and S.harvestLoot == nil,"
                , "  'and must end the collection cleanly')"
                , "assert(#WARNINGS == 0,"
                , "  'a raced row is not a capacity refusal')"
                , "assert(item.getGroundForUnit(1, 1),"
                , "  'the surviving yield stays on the ground for someone else')"
                -- The same reading, reached from an approach rather
                -- than from adjacency.
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 1 }"
                , "place(20, 0)"
                , "step()"
                , "assert(CALLS.moveTo > 0, 'the worker is approaching')"
                , "MISSING[1] = true"
                , "ACTIVITY = 'idle'"
                , "step()"
                , "assert(PICKUP_CALLS == 0 and S.harvestPhase == nil,"
                , "  'a row that vanishes mid-approach ends the collection too')"
                ]

        it "judges capacity at the moment of truth: a long approach \
           \draws no warning, and the refusal still warns exactly once \
           \on the adjacent tick" $
            runsOk $ lns
                [ harvestPrelude
                -- A yield this worker can never fit, eleven tiles off.
                , "CAPACITY, ROW_WEIGHT = 1.0, 5.0"
                , "GROUND_AT[7] = { x = 10.5, y = 0.5 }"
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 7 }"
                , "place(20, 0)"
                , "step()"
                , "assert(CALLS.moveTo == 1, 'the worker must approach it')"
                , "assert(#WARNINGS == 0,"
                , "  'capacity must not be judged from eleven tiles away')"
                , "local ticks = 0"
                , "while S.harvestPhase and ticks < 120 do"
                , "  assert(#WARNINGS == 0 or PICKUP_CALLS >= 0, 'progress')"
                , "  step(); ticks = ticks + 1"
                , "end"
                , "assert(S.harvestPhase == nil,"
                , "  'the refusal must end the collection')"
                , "assert(#WARNINGS == 1,"
                , "  'and must warn exactly ONCE across the whole approach, got '"
                , "  .. #WARNINGS)"
                , "assert(WARNINGS[1]:find('leaving ground', 1, true),"
                , "  'with the leaving-ground outcome')"
                , "assert(PICKUP_CALLS == 0,"
                , "  'a refused yield must not reach item.pickupGround')"
                , "assert(item.getGroundForUnit(1, 7),"
                , "  'and must be left where it lies')"
                ]

        it "ends the collection cleanly when the yield cannot be \
           \reached at all, instead of re-deciding and re-pathing \
           \forever" $
            runsOk $ lns
                [ harvestPrelude
                -- The walk goes nowhere: a blocked route, an island,
                -- a yield behind a wall. Without a bound this is the
                -- livelock the approach would introduce — a pending
                -- collection scores above idle unconditionally, and
                -- unit_ai.lua's stuck-walk watchdog clears no phase.
                , "NO_WALK = true"
                , "GROUND_AT[7] = { x = 10.5, y = 0.5 }"
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 7 }"
                , "place(30, 0)"
                , "local ticks = 0"
                , "while S.harvestPhase and ticks < 400 do"
                , "  step(); ticks = ticks + 1"
                , "end"
                , "assert(S.harvestPhase == nil and S.harvestLoot == nil,"
                , "  'an unreachable yield must end the collection')"
                , "assert(S.harvestCollect == nil,"
                , "  'and must leave no approach bookkeeping behind')"
                , "assert(PICKUP_CALLS == 0,"
                , "  'without ever collecting it remotely')"
                , "assert(item.getGroundForUnit(1, 7),"
                , "  'the yield stays on the ground for a worker that can reach it')"
                -- A STALL timer, not a total-trip budget: it took real
                -- time, not one tick.
                , "assert(ticks > 10,"
                , "  'and must not give up on the first fruitless tick')"
                -- The scenario really was the stuck-walk one: the unit
                -- spent whole watchdog intervals walking nowhere, so
                -- the budget cannot have been charged from execute.
                , "assert(WATCHDOG_STOPS > 1,"
                , "  'the fixture must have gone through more than one '"
                , "  .. 'stuck-walk watchdog cycle, got '"
                , "  .. tostring(WATCHDOG_STOPS))"
                ]

        it "charges the budget from the UTILITY tick, so a stuck walk \
           \whose watchdog cycle is longer than MAX_CHARGED_INTERVAL \
           \cannot make an unreachable collection immortal" $
            runsOk $ lns
                [ harvestPrelude
                , "local stall = require('scripts.unit_ai_stall')"
                -- The exact numeric relationship that breaks an
                -- execute-side sampler: every interval such a sampler
                -- could ever observe is longer than the bound, and an
                -- interval past the bound charges ZERO rather than
                -- being clamped down to it. Pinned as an assertion so
                -- this case cannot quietly stop being the hard one.
                , "assert(STUCK_WALK_TIMEOUT > stall.MAX_CHARGED_INTERVAL,"
                , "  'the watchdog cycle must outlast MAX_CHARGED_INTERVAL '"
                , "  .. 'for this case to mean anything')"
                , "NO_WALK = true"
                , "GROUND_AT[7] = { x = 10.5, y = 0.5 }"
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 7 }"
                , "place(30, 0)"
                -- Every tick scores the action; only the ones that find
                -- the unit idle execute it. Count both, so the case can
                -- say the budget did NOT come from the execute path.
                , "local ticks, executed = 0, 0"
                , "while S.harvestPhase and ticks < 600 do"
                , "  if ACTIVITY == 'idle' then executed = executed + 1 end"
                , "  step(); ticks = ticks + 1"
                , "end"
                , "assert(S.harvestPhase == nil and S.harvestLoot == nil,"
                , "  'a permanently stuck approach must still end the '"
                , "  .. 'collection; it ran ' .. tostring(ticks) .. ' ticks')"
                , "assert(PICKUP_CALLS == 0,"
                , "  'and must never collect the yield remotely')"
                , "assert(item.getGroundForUnit(1, 7),"
                , "  'the yield stays on the ground')"
                , "assert(WATCHDOG_STOPS > 1,"
                , "  'the unit must have sat through more than one full '"
                , "  .. 'watchdog cycle, got ' .. tostring(WATCHDOG_STOPS))"
                -- The load-bearing assertion. Charging only on the
                -- execute ticks could not reach the 30 s budget in the
                -- number of executes this run produced, because each
                -- gap between them exceeds MAX_CHARGED_INTERVAL and so
                -- charges nothing at all.
                , "assert(executed * stall.MAX_CHARGED_INTERVAL"
                , "       < yieldCollect.COLLECT_TIMEOUT,"
                , "  'the budget must not have been reachable from the '"
                , "  .. 'execute path alone: ' .. tostring(executed)"
                , "  .. ' execute tick(s)')"
                ]

    describe "proximity is measured in the page's own seam frame" $ do
        it "a yield across the U seam is adjacent, and the same fixture \
           \with no wrap period is a walk — so the check really does \
           \consult world.getWrapWidth on the ACTOR's page" $
            runsOk $ lns
                [ harvestPrelude
                -- Period 64 ⇒ alias step 32. (0, 40)'s cylindrical
                -- image along (+u, -v) is (32, 8), which (33, 8) is one
                -- Chebyshev tile from; raw subtraction puts them 32
                -- tiles apart.
                , "WRAP = 64"
                , "GROUND_AT[7] = { x = 0.5, y = 40.5 }"
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 7 }"
                , "place(33, 8)"
                , "step()"
                , "assert(PICKUP_CALLS == 1,"
                , "  'a seam-adjacent yield must be collected in place')"
                , "assert(CALLS.moveTo == 0,"
                , "  'and must not be walked to across the whole world')"
                , "assert(WRAP_PAGES[1] == 'p1',"
                , "  'the period must be read for the ACTOR own page, got '"
                , "  .. tostring(WRAP_PAGES[1]))"
                -- The control: identical geometry, no wrap period.
                , "WRAP = 0"
                , "TAKEN, GROUND, PICKUP_CALLS = {}, {}, 0"
                , "CALLS.moveTo = 0"
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 7 }"
                , "place(33, 8)"
                , "ACTIVITY = 'idle'"
                , "step()"
                , "assert(PICKUP_CALLS == 0,"
                , "  'without a period the same pair is 32 tiles apart')"
                , "assert(CALLS.moveTo == 1,"
                , "  'so the worker must walk instead')"
                ]

        it "the shared primitive measures Chebyshev tiles over the same \
           \three cylindrical images unit_ai_locations searches, and \
           \collapses to the plain comparison for an absent period" $
            runsOk $ lns
                [ harvestPrelude
                , "local locations = require('scripts.unit_ai_locations')"
                , "assert(type(locations.aliasStep) == 'function'"
                , "   and type(locations.wrapPeriodFor) == 'function',"
                , "  'the seam-alias primitives must be exported, not re-derived')"
                , "assert(locations.aliasStep(64) == 32,"
                , "  'the alias step is half the period')"
                , "assert(locations.aliasStep(0) == 0"
                , "   and locations.aliasStep(nil) == 0"
                , "   and locations.aliasStep(-4) == 0,"
                , "  'an absent or non-positive period collapses to zero')"
                -- Chebyshev, not Euclidean: (1, 1) away is ONE tile in
                -- the frame the surrounding approach branches use.
                , "assert(yieldCollect.chebyshev(0, 0, 1, 1, 0) == 1,"
                , "  'a diagonal neighbour is one Chebyshev tile')"
                , "assert(yieldCollect.adjacent(0, 0, 1, 1, 0),"
                , "  'and is therefore adjacent')"
                , "assert(not yieldCollect.adjacent(0, 0, 2, 0, 0),"
                , "  'two tiles is not')"
                -- The seam, with the period passed EXPLICITLY, which is
                -- what keeps the primitive callable from a bare VM.
                , "assert(yieldCollect.chebyshev(33, 8, 0, 40, 0) == 33,"
                , "  'raw subtraction puts the seam pair 33 tiles apart')"
                -- chebyshev takes the ALIAS STEP (half the period);
                -- adjacent takes the FULL period and derives the step,
                -- exactly as nearestKnownLocation does.
                , "assert(yieldCollect.chebyshev(33, 8, 0, 40,"
                , "                              locations.aliasStep(64)) == 1,"
                , "  'and the cylindrical images put them next door')"
                , "assert(yieldCollect.adjacent(33, 8, 0, 40, 64),"
                , "  'so the seam pair is adjacent under the page period')"
                , "assert(not yieldCollect.adjacent(33, 8, 0, 40, nil),"
                , "  'and is not, with no period at all')"
                ]

    describe "foraging shares the gate and keeps its capacity exemption" $ do
        it "walks to a retained forage yield an interruption left tiles \
           \away, steering by the resolved row rather than the target \
           \forageUtility rewrote on the way past" $
            runsOk $ lns
                [ foragePrelude
                , "forageToCollecting()"
                , "local harvestsAtCompletion = CALLS.harvest"
                -- Dragged away, and a nearer piece of ground food
                -- appears where the worker now stands: forageUtility
                -- rewrites s.forageTarget to THAT gid on its next
                -- scoring pass, so a collecting branch steering by the
                -- target would walk to the wrong item entirely.
                , "foragePreempt(30, 20, 0)"
                , "GROUND_AT[9] = { x = 21.5, y = 0.5 }"
                , "assert(S.foragePhase == 'collecting' and #S.forageLoot == 1,"
                , "  'the interruption must not discard the pending yield')"
                , "forageStep()"
                , "assert(S.forageTarget and S.forageTarget.gid == 9,"
                , "  'the scan must indeed have retargeted the nearer decoy')"
                , "assert(PICKUP_CALLS == 0,"
                , "  'a retained yield eleven tiles away must not be picked up')"
                , "assert(MOVED_TO.x == 10.5 and MOVED_TO.y == 0.5,"
                , "  'and the walk must steer to the RETAINED row, not the decoy')"
                , "assert(#S.forageLoot == 1,"
                , "  'the gid must stay pending while travelling')"
                , "local ticks = 0"
                , "while PICKUP_CALLS == 0 and ticks < 120 do"
                , "  assert(item.getGroundForUnit(1, S.forageLoot[#S.forageLoot]),"
                , "    'the yield must remain on the ground while approaching')"
                , "  forageStep(); ticks = ticks + 1"
                , "end"
                , "assert(ticks > 1, 'the approach must take real time')"
                , "assert(PICKUP_CALLS == 1 and GROUND[1] == 5,"
                , "  'exactly the retained instance, exactly once')"
                , "assert(CALLS.harvest == harvestsAtCompletion,"
                , "  'and no plant may be re-harvested to finish it')"
                -- The pre-existing strand, asserted rather than papered
                -- over: a collected forage yield puts food in the pack,
                -- and forageUtility yields to eat_from_inventory the
                -- moment it does. That is why this group drives the
                -- approach BEFORE any pickup and never assumes forage
                -- stays selectable across two collected yields.
                , "assert(needs.forageUtility(1, S, PARAMS) == -math.huge,"
                , "  'forage steps aside for eat_from_inventory once fed')"
                ]

        it "takes an adjacent forage yield whatever the worker is \
           \carrying — the approach adds a distance test, never the \
           \capacity admission auto-harvest applies" $
            runsOk $ lns
                [ foragePrelude
                -- Hopelessly over capacity, which is exactly the state
                -- #2293 decided a starving forager may still eat out of.
                , "CAPACITY, ROW_WEIGHT, CARRIED = 1.0, 50.0, 40.0"
                , "forageToCollecting()"
                , "forageStep()"
                , "assert(PICKUP_CALLS == 1 and GROUND[1] == 5,"
                , "  'foraging must remain exempt from the capacity gate')"
                , "assert(#WARNINGS == 0,"
                , "  'and must draw no capacity warning')"
                ]

        it "ends a forage collection when the retained yield is gone, \
           \and never walks to an adjacent one" $
            runsOk $ lns
                [ foragePrelude
                , "forageToCollecting()"
                , "assert(CALLS.moveTo == 0,"
                , "  'the adjacent pick needed no recorded walk')"
                -- NOTHING else to forage anywhere in range: the plant
                -- this fixture held has been picked, and the yield it
                -- dropped is the row about to vanish. forageUtility
                -- therefore scores -math.huge and forageExecute never
                -- runs -- which is exactly why retiring a vanished row
                -- has to happen on the UTILITY tick. Adding a second
                -- plant here to keep forage selectable would test the
                -- fixture rather than the code.
                , "assert(next(FLORA) == nil,"
                , "  'the fixture must hold no other harvestable plant')"
                , "MISSING[5] = true"
                , "local u = forageStep()"
                , "assert(u == -math.huge,"
                , "  'forage must indeed be unselectable on this tick, so the '"
                , "  .. 'cleanup cannot have come from forageExecute')"
                , "assert(PICKUP_CALLS == 0,"
                , "  'a vanished row must not reach item.pickupGround')"
                , "assert(S.foragePhase == nil and S.forageLoot == nil,"
                , "  'and must end the collection cleanly')"
                , "assert(S.forageCollect == nil,"
                , "  'leaving no approach bookkeeping behind')"
                ]

    describe "the approach budget is eligible time only" $ do
        it "an interruption is not charged against it: a preempted \
           \approach that keeps making progress still completes" $
            runsOk $ lns
                [ harvestPrelude
                , "local stall = require('scripts.unit_ai_stall')"
                , "harvestToCollecting()"
                , "preempt(5, 24, 0)"
                , "step()"
                , "assert(S.harvestCollect, 'the approach clock must exist')"
                -- Minutes elsewhere, announced through onExit, must not
                -- be charged as approach time.
                , "preempt(600)"
                , "assert(S.harvestCollect.stallSeenAt == nil,"
                , "  'onExit must drop the approach clock last-sample stamp')"
                , "step()"
                , "assert(S.harvestPhase == 'collecting',"
                , "  'a ten-minute interruption must not expire the budget')"
                -- ...and a swallowed tick takes the same boundary,
                -- which is the ONLY one foraging gets.
                , "stall.suspendOrders(S, 1)"
                , "assert(S.harvestCollect == nil"
                , "   or S.harvestCollect.stallSeenAt == nil,"
                , "  'suspendOrders must drop it too')"
                , "local ticks = 0"
                , "while PICKUP_CALLS == 0 and ticks < 120 do"
                , "  step(); ticks = ticks + 1"
                , "end"
                , "assert(PICKUP_CALLS == 1,"
                , "  'and the interrupted approach must still finish')"
                ]

        it "is not refunded by an interrupting action that happens to \
           \carry the worker closer: only this collection's own ticks \
           \may record a new closest approach" $
            runsOk $ lns
                [ harvestPrelude
                -- A yield the worker never closes on by itself.
                , "NO_WALK = true"
                , "GROUND_AT[7] = { x = 10.5, y = 0.5 }"
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 7 }"
                , "place(40, 0)"
                -- Spend a real part of the budget on this collection's
                -- own eligible ticks.
                , "for _ = 1, 30 do step() end"
                , "local spent = S.harvestCollect and S.harvestCollect.stalledFor"
                , "assert(spent and spent > 5,"
                , "  'the approach must have spent real budget first, got '"
                , "  .. tostring(spent))"
                , "assert(S.harvestPhase == 'collecting',"
                , "  'and must not have expired yet')"
                -- Now treat_ally owns the worker and walks it a long
                -- way TOWARD the yield. That is a genuine new closest
                -- approach -- and it is not this collection's, so it
                -- must buy the collection nothing.
                , "local closer = 40"
                , "for _ = 1, 12 do closer = closer - 2; foreignStep(nil, closer, 0) end"
                , "assert(closer > 11,"
                , "  'the interruption must still leave the yield out of reach')"
                , "assert(S.harvestCollect,"
                , "  'the approach record must survive the interruption')"
                , "assert(S.harvestCollect.stalledFor >= spent,"
                , "  'an interruption that carried the worker closer must not '"
                , "  .. 'refund a budget this collection had already spent: '"
                , "  .. tostring(spent) .. ' -> '"
                , "  .. tostring(S.harvestCollect.stalledFor))"
                -- ...and the interval itself is not charged either, so
                -- the interruption is neither a refund nor a penalty.
                , "assert(S.harvestCollect.stalledFor == spent,"
                , "  'nor may the interruption be charged as approach time')"
                -- The sharpest version of the same rule: the
                -- interrupting action carries the worker right PAST the
                -- yield and out the other side. Merely being adjacent
                -- on a tick this collection does not own must not
                -- retire the approach record, which would hand the
                -- whole spent budget back just as surely as a reset.
                , "foreignStep(nil, 10, 0)"
                , "assert(S.harvestCollect,"
                , "  'passing adjacent under another action must not retire '"
                , "  .. 'the approach record')"
                , "foreignStep(nil, 40, 0)"
                , "assert(S.harvestCollect"
                , "   and S.harvestCollect.stalledFor == spent,"
                , "  'and must leave the spent budget exactly as it was: '"
                , "  .. tostring(spent) .. ' -> '"
                , "  .. tostring(S.harvestCollect and S.harvestCollect.stalledFor))"
                -- Deliberately returned to where it started before the
                -- collection resumes, so this case measures the
                -- INELIGIBLE ticks alone. What happens when the
                -- interruption leaves the worker genuinely closer is
                -- the separate contract pinned in the next case; the
                -- two must not be conflated.
                -- The collection still gives up on schedule rather than
                -- being kept alive by repeated interruptions.
                , "local ticks = 0"
                , "while S.harvestPhase and ticks < 600 do"
                , "  step(); ticks = ticks + 1"
                , "end"
                , "assert(S.harvestPhase == nil,"
                , "  'the unreachable collection must still expire')"
                , "assert(PICKUP_CALLS == 0, 'and collect nothing remotely')"
                ]

        it "re-baselines and restarts the budget on the first eligible \
           \sample after an interruption that left the worker closer — \
           \the shared stall contract — and cannot be strung out \
           \forever by it, because the closest approach only ratchets \
           \downward" $
            runsOk $ lns
                [ harvestPrelude
                , "local stall = require('scripts.unit_ai_stall')"
                , "NO_WALK = true"
                , "GROUND_AT[7] = { x = 10.5, y = 0.5 }"
                , "S.harvestPhase = 'collecting'"
                , "S.harvestLoot  = { 7 }"
                , "place(40, 0)"
                , "for _ = 1, 30 do step() end"
                , "local spent = S.harvestCollect and S.harvestCollect.stalledFor"
                , "assert(spent and spent > 5,"
                , "  'the approach must have spent real budget first')"
                , "local baseline = S.harvestCollect.bestDist"
                , "assert(baseline and baseline > 25,"
                , "  'and recorded its closest approach at the far distance')"
                -- treat_ally carries the worker a long way toward the
                -- yield and LEAVES it there. This is the case the
                -- previous example deliberately does not cover.
                , "foreignStep(nil, 20, 0)"
                , "assert(S.harvestCollect.stalledFor == spent,"
                , "  'the ineligible tick itself must neither charge nor refund')"
                , "assert(S.harvestCollect.bestDist == baseline,"
                , "  'nor may it record a closest approach of its own')"
                -- Now the collection resumes, still out of reach, and
                -- runs several eligible ticks from the new position.
                -- scripts/unit_ai_stall.lua states the rule this pins:
                -- \"the first eligible sample after the interruption
                -- records it and starts the budget over then\", and
                -- unit_ai_pickup.lua's pickupUtility gates
                -- bestDist/stall.reset in exactly this shape. The
                -- collection budget deliberately does not diverge from
                -- the order budget it shares an accumulator with.
                , "step(); step(); step()"
                , "assert(S.harvestPhase == 'collecting',"
                , "  'the collection is still pending')"
                , "assert(S.harvestCollect.bestDist"
                , "       and S.harvestCollect.bestDist < baseline,"
                , "  'the first eligible sample re-baselines to where the '"
                , "  .. 'worker now actually stands')"
                , "assert(S.harvestCollect.stalledFor < spent,"
                , "  'and the shared contract restarts the budget from there')"
                -- The part that makes that safe, and the reason this is
                -- not a way to keep an unreachable yield alive: a reset
                -- costs a STRICT decrease in bestDist of more than
                -- PROGRESS_TILES, so the closest approach is a ratchet.
                -- No amount of interrupting can produce an unbounded
                -- number of refunds, and the sequence terminates at
                -- adjacency, which is a completed pickup rather than a
                -- stall.
                , "local ratchet = S.harvestCollect.bestDist"
                , "for _ = 1, 20 do step() end"
                , "if S.harvestPhase then"
                , "  assert(S.harvestCollect == nil"
                , "     or S.harvestCollect.bestDist <= ratchet,"
                , "    'the closest approach may never grow')"
                , "end"
                -- Standing still at the new distance, it expires on
                -- schedule: the re-baseline bought one budget, not
                -- immunity.
                , "local ticks = 0"
                , "while S.harvestPhase and ticks < 600 do"
                , "  step(); ticks = ticks + 1"
                , "end"
                , "assert(S.harvestPhase == nil,"
                , "  'the collection must still expire from the new baseline')"
                , "assert(PICKUP_CALLS == 0, 'and collect nothing remotely')"
                -- The bound, stated as arithmetic rather than as a
                -- hope: from any starting distance, each refund costs a
                -- strict PROGRESS_TILES of real closing, so the number
                -- of them a shuttling interrupter can ever buy is
                -- finite.
                , "assert(stall.MAX_CHARGED_INTERVAL > 0"
                , "   and yieldCollect.PROGRESS_TILES > 0,"
                , "  'the ratchet step must be positive for that bound to hold')"
                ]

        it "classifies the approach records transient, so the \
           \lua.unit_ai component stays at v9" $
            runsOk $ lns
                [ "engine = { logWarn = function() end, logInfo = function() end }"
                , "unit = { exists = function() return true end }"
                , "local unitAiSave = require('scripts.unit_ai_save')"
                , "local saveModules = require('scripts.lib.save_modules')"
                , "local codec = require('scripts.lib.data_codec')"
                , "local aiState = {}"
                , "unitAiSave.register(aiState)"
                , "local spec = saveModules.registry.unit_ai"
                , "assert(spec.version == 9,"
                , "  'the retained-yield approach must move no component '"
                , "  .. 'version; got ' .. tostring(spec.version))"
                , "aiState[1] = { currentAction = 'auto_harvest',"
                , "  harvestPhase = 'collecting', harvestLoot = { 1, 2 },"
                , "  harvestCollect = { bestDist = 4.0, stalledFor = 7.5,"
                , "                     stallSeenAt = 120 },"
                , "  foragePhase = 'collecting', forageLoot = { 5 },"
                , "  forageCollect = { bestDist = 9.0, stalledFor = 2.0,"
                , "                    stallSeenAt = 120 } }"
                , "local snap = spec.snapshot()"
                , "local row = snap[1]"
                , "assert(row, 'the unit must still be snapshotted')"
                , "assert(row.harvestCollect == nil and row.forageCollect == nil,"
                , "  'neither approach record may reach the payload')"
                -- The pending collection ITSELF is durable, which is
                -- precisely why its proximity has to be rechecked after
                -- a load.
                , "assert(row.harvestPhase == 'collecting'"
                , "   and row.harvestLoot and #row.harvestLoot == 2,"
                , "  'the pending harvest collection still persists')"
                , "assert(row.foragePhase == 'collecting'"
                , "   and row.forageLoot and #row.forageLoot == 1,"
                , "  'and so does the pending forage collection')"
                , "local decoded = spec.decode(spec.version,"
                , "  codec.decode(codec.encode(snap)))"
                , "for k in pairs(aiState) do aiState[k] = nil end"
                , "spec.apply(decoded, nil)"
                , "local restored = aiState[1]"
                , "assert(restored, 'the row must survive the round trip')"
                , "assert(restored.harvestCollect == nil"
                , "   and restored.forageCollect == nil,"
                , "  'a loaded worker re-establishes its approach from where it stands')"
                , "assert(restored.harvestPhase == 'collecting',"
                , "  'while the collection it still owes survives')"
                ]
