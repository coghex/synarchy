-- | The "multi-canteen water selection" gate (#2546):
--   @scripts/unit_ai_water.lua@'s canteen selection must scan the WHOLE
--   inventory and score off the MOST URGENT (emptiest) refillable
--   canteen, at all three consumers — @refillUtility@, @refillExecute@
--   and @searchUtility@ — rather than off the first row that happens to
--   have headroom.
--
--   The defect this pins: a partially depleted canteen sitting AHEAD of
--   an empty one hid it. With 1.9 L/2 L first, refill and personal
--   search both went ineligible while the empty peer stayed dry; with
--   1.0 L first, refill scored 1.589 and lost to a player move instead
--   of the 7.5 the same inventory produced in the other order. So the
--   two load-bearing properties here are ORDER INDEPENDENCE (requirement
--   2) and the empty canteen's #306 priority surviving a fuller peer in
--   front of it.
--
--   Same standalone-Lua-VM pattern as "Test.Headless.Lua.UnitAiHold":
--   each 'it' runs one self-contained chunk via 'Lua.dostring' in a
--   fresh interpreter, asserting inside Lua via @assert()@, with a
--   non-OK 'Lua.Status' surfaced as an hspec failure carrying the Lua
--   message. The real @scripts.unit_ai_water@, @scripts.unit_ai_core@
--   and @scripts.unit_ai_tunables@ all run; only the engine globals they
--   reach are stubbed. Crucially the canteen SELECTION and the fill
--   MUTATION are not stubbed away — those are the code under test.
--
--   @unit.getInventory@ and @unit.modifyItemFillById@ are held to the
--   engine's own contract so the group cannot go vacuous: rows carry
--   @defName@/@instanceId@/@currentFill@ with @capacity@ present only
--   for container defs (src\/Engine\/Scripting\/Lua\/API\/Units\/Inventory.hs),
--   and the fill mutation resolves by @instanceId@ and clamps to
--   @[0, capacity]@ (…\/Units\/Equipment.hs). A stub that applied the
--   delta to the first same-def row would let the very defect this
--   group exists to catch pass, so the stub itself is mutation-tested
--   below before anything depends on it.
--
--   The "above the player move" bound is asserted against
--   @scripts/unit_ai_combat.lua@'s exported @FOLLOW_COMMAND_UTILITY@,
--   not a literal 7.0, following "Test.Headless.Lua.UnitAiHold" — a
--   retune of that constant must not silently invalidate this gate.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "multi-canteen water selection"'@.
module Test.Headless.Lua.UnitAiWaterCanteens (spec) where

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

-- | The engine globals the water actions reach, held to the real
--   contracts (see the module header). @INV@ is the unit's inventory in
--   stored order, which is what @unit.getInventory@ preserves and
--   therefore what this whole gate is about; @FLUID@ is the tile map
--   @world.getFluidAt@ reads; @PICKUPS@ records the refill animation so
--   "it actually refilled" is asserted on the verb rather than inferred.
--
--   @scripts.movement_speed@ and @scripts.unit_stats@ are stubbed at
--   @package.loaded@: the pace a walk picks and the stat table a
--   thirst ratio reads are not what this gate is about, and the real
--   modules reach several physiology modules' worth of state to answer
--   one number each.
engineStubs ∷ Text
engineStubs = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "package.loaded['scripts.movement_speed'] ="
    , "  { comfort = function() return 1.0 end,"
    , "    ordered = function() return 1.15 end,"
    , "    meander = function() return 0.5 end,"
    , "    sprint  = function() return 2.0 end }"
    , "STATS = { hydration = 100, max_hydration = 100 }"
    , "package.loaded['scripts.unit_stats'] ="
    , "  { get = function(_, k) return STATS[k] end }"
    , "NOW = 0"
    , "POS = { gridX = 0.5, gridY = 0.5 }"
    , "INV, FLUID, MOVES = {}, {}, {}"
    , "PICKUPS = 0"
    , "local function key(x, y) return string.format('%d,%d', x, y) end"
    , "function setFluid(x, y, kind) FLUID[key(x, y)] = kind end"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function() end, logInfo = function() end,"
    , "           emitEventForUnit = function() end }"
    , "world = { getFluidAt = function(x, y) return FLUID[key(x, y)] end }"
    , "unit = {"
    , "  getInfo = function() return POS end,"
    , "  exists = function() return true end,"
    , "  getStat = function(_, k) return STATS[k] end,"
    , "  getCarryingWeight = function() return 0 end,"
    , "  getVisibleTiles = function() return {} end,"
    , "  getInventory = function() return INV end,"
    , "  stop = function() end,"
    , "  pickup = function() PICKUPS = PICKUPS + 1 end,"
    , "  moveTo = function(_, x, y, sp)"
    , "    MOVES[#MOVES + 1] = { x = x, y = y, speed = sp } end,"
    -- unit.modifyItemFillById: resolve by instanceId, clamp to
    -- [0, capacity], return the APPLIED delta, nil for an unknown id.
    , "  modifyItemFillById = function(_, iid, delta)"
    , "    for _, it in ipairs(INV) do"
    , "      if it.instanceId == iid then"
    , "        local cap = it.capacity or 0"
    , "        local before = it.currentFill"
    , "        local after = math.max(0, math.min(cap, before + delta))"
    , "        it.currentFill = after"
    , "        return after - before"
    , "      end"
    , "    end"
    , "    return nil"
    , "  end }"
    ]

-- | The real water module over that fixture, plus inventory builders.
--
--   @canteen@ mints a container row of the def the shipped acolyte
--   tunables actually name, so the selection under test matches on the
--   same string production does. @nonContainer@ mints the row the
--   presence guard exists for: @unit.getInventory@ emits @capacity@
--   only for defs carrying a container block, and computing emptiness
--   before that guard would divide by nil.
--
--   @refill@ scores against a state that KNOWS a source (refill's
--   precondition) and @search@ against one that does not (search's),
--   so each helper exercises its own branch.
waterPrelude ∷ Text
waterPrelude = lns
    [ engineStubs
    , "local core = require('scripts.unit_ai_core')"
    , "WATER = require('scripts.unit_ai_water')"
    , "COMBAT = require('scripts.unit_ai_combat')"
    , "P = require('scripts.unit_ai_tunables').acolyte"
    , "CAP = 2.0"
    , "NEG = -math.huge"
    , "local nextId = 0"
    , "function canteen(fill, cap)"
    , "  nextId = nextId + 1"
    , "  return { defName = P.canteen_def, instanceId = nextId,"
    , "           currentFill = fill, capacity = cap or CAP }"
    , "end"
    , "function nonContainer(name)"
    , "  nextId = nextId + 1"
    , "  return { defName = name or P.canteen_def, instanceId = nextId,"
    , "           currentFill = 0 }"
    , "end"
    , "function knownSource()"
    , "  local s = {}"
    , "  core.addWaterSource(s, 10, 10)"
    , "  return s"
    , "end"
    , "function refill() return WATER.refillUtility(1, knownSource(), P) end"
    , "function search() return WATER.searchUtility(1, {}, P) end"
    , "function goalSearch()"
    , "  return WATER.searchUtility(1, { activeGoal = 'find_water' }, P)"
    , "end"
    -- Eligible = a real number that is neither -inf nor NaN. The NaN
    -- half matters: NaN fails every `<` comparison in Lua, so a NaN
    -- emptiness would walk past the threshold test and reach arbitration
    -- as a NaN utility rather than being rejected.
    , "function eligible(v)"
    , "  return type(v) == 'number' and v == v and v > NEG and v < math.huge"
    , "end"
    -- The two ends of the shipped quadratic ramp, derived from the
    -- tunables rather than transcribed: at empty (emptiness 1) the ramp
    -- peaks at base+scale, and at the threshold exactly it sits at base.
    , "PEAK_REFILL = P.refill_base_weight + P.refill_urgency_scale"
    , "PEAK_SEARCH = P.search_base_weight"
    , "  + (1.0 - P.search_min_emptiness) * P.search_emptiness_weight"
    -- Fill leaving emptiness exactly at the refill threshold.
    , "THRESHOLD_FILL = CAP * (1.0 - P.refill_min_emptiness)"
    ]

spec ∷ Spec
spec = describe "multi-canteen water selection" $ do

    describe "the fixture's own engine stubs" $
        it "resolves a fill mutation by instanceId and clamps it to \
           \[0, capacity], so a selection that targeted the wrong \
           \same-def canteen could not pass unnoticed" $
            runsOk $ lns
                [ waterPrelude
                , "INV = { canteen(1.0), canteen(0.0) }"
                , "local applied ="
                , "  unit.modifyItemFillById(1, INV[2].instanceId, CAP)"
                , "assert(applied == CAP and INV[2].currentFill == CAP,"
                , "  'the addressed instance must take the whole delta: '"
                , "  .. tostring(applied))"
                , "assert(INV[1].currentFill == 1.0,"
                , "  'a same-def peer must never absorb the write')"
                , "local over ="
                , "  unit.modifyItemFillById(1, INV[1].instanceId, 99.0)"
                , "assert(over == 1.0 and INV[1].currentFill == CAP,"
                , "  'an overfill clamps to capacity: ' .. tostring(over))"
                , "local under ="
                , "  unit.modifyItemFillById(1, INV[1].instanceId, -99.0)"
                , "assert(under == -CAP and INV[1].currentFill == 0.0,"
                , "  'an overdrain clamps to zero: ' .. tostring(under))"
                , "assert(unit.modifyItemFillById(1, 9999, 1.0) == nil,"
                , "  'an unknown instance id resolves to nil')"
                ]

    describe "eligibility and urgency are independent of inventory order" $ do
        it "scores a nearly-full canteen plus an empty one identically in \
           \both orders, off the empty one — the partial canteen must not \
           \hide it from either refill or personal search" $
            runsOk $ lns
                [ waterPrelude
                , "INV = { canteen(1.9), canteen(0.0) }"
                , "local rPartialFirst, sPartialFirst = refill(), search()"
                , "INV = { canteen(0.0), canteen(1.9) }"
                , "local rEmptyFirst, sEmptyFirst = refill(), search()"
                , "assert(eligible(rPartialFirst),"
                , "  'refill must be eligible with the partial canteen '"
                , "  .. 'first: ' .. tostring(rPartialFirst))"
                , "assert(eligible(sPartialFirst),"
                , "  'search must be eligible with the partial canteen '"
                , "  .. 'first: ' .. tostring(sPartialFirst))"
                , "assert(rPartialFirst == rEmptyFirst,"
                , "  'reordering changed the refill score: '"
                , "  .. tostring(rPartialFirst) .. ' vs '"
                , "  .. tostring(rEmptyFirst))"
                , "assert(sPartialFirst == sEmptyFirst,"
                , "  'reordering changed the search score: '"
                , "  .. tostring(sPartialFirst) .. ' vs '"
                , "  .. tostring(sEmptyFirst))"
                , "assert(rPartialFirst == PEAK_REFILL,"
                , "  'both orders must score the EMPTY canteen at the '"
                , "  .. 'ramp peak: ' .. tostring(rPartialFirst))"
                , "assert(sPartialFirst == PEAK_SEARCH,"
                , "  'both orders must score search off the empty '"
                , "  .. 'canteen: ' .. tostring(sPartialFirst))"
                ]

        it "keeps the empty canteen's refill urgency above the player \
           \move's own utility with a half-full canteen in front of it, \
           \in either order" $
            runsOk $ lns
                [ waterPrelude
                , "assert(COMBAT.FOLLOW_COMMAND_UTILITY,"
                , "  'follow_command must export its constant to be '"
                , "  .. 'pinned against')"
                , "INV = { canteen(1.0), canteen(0.0) }"
                , "local rHalfFirst = refill()"
                , "INV = { canteen(0.0), canteen(1.0) }"
                , "local rEmptyFirst = refill()"
                , "assert(rHalfFirst == rEmptyFirst,"
                , "  'reordering changed the refill score: '"
                , "  .. tostring(rHalfFirst) .. ' vs '"
                , "  .. tostring(rEmptyFirst))"
                , "assert(rHalfFirst > COMBAT.FOLLOW_COMMAND_UTILITY,"
                , "  'a dry canteen must outrank the player move (#306) '"
                , "  .. 'whatever sits in front of it: '"
                , "  .. tostring(rHalfFirst) .. ' vs '"
                , "  .. tostring(COMBAT.FOLLOW_COMMAND_UTILITY))"
                ]

    describe "refill execution addresses the canteen it scored" $ do
        it "fills the urgent empty instance at a valid bank and leaves \
           \the less-empty peer untouched, with the peer first" $
            runsOk $ lns
                [ waterPrelude
                , "setFluid(10, 10, 'lake')"
                , "POS.gridX, POS.gridY = 9.5, 10.5"
                , "INV = { canteen(1.0), canteen(0.0) }"
                , "WATER.refillExecute(1, knownSource(), P)"
                , "assert(INV[2].currentFill == CAP,"
                , "  'the empty instance must be filled to capacity: '"
                , "  .. tostring(INV[2].currentFill))"
                , "assert(INV[1].currentFill == 1.0,"
                , "  'the less-empty peer must be untouched, not topped '"
                , "  .. 'up in its place: ' .. tostring(INV[1].currentFill))"
                , "assert(PICKUPS == 1,"
                , "  'the refill must actually have happened once: '"
                , "  .. tostring(PICKUPS))"
                ]

        it "fills the urgent empty instance with the peer second, so \
           \execution agrees with the urgency it reported in either \
           \order" $
            runsOk $ lns
                [ waterPrelude
                , "setFluid(10, 10, 'lake')"
                , "POS.gridX, POS.gridY = 9.5, 10.5"
                , "INV = { canteen(0.0), canteen(1.0) }"
                , "WATER.refillExecute(1, knownSource(), P)"
                , "assert(INV[1].currentFill == CAP,"
                , "  'the empty instance must be filled to capacity: '"
                , "  .. tostring(INV[1].currentFill))"
                , "assert(INV[2].currentFill == 1.0,"
                , "  'the less-empty peer must be untouched: '"
                , "  .. tostring(INV[2].currentFill))"
                , "assert(PICKUPS == 1,"
                , "  'the refill must actually have happened once: '"
                , "  .. tostring(PICKUPS))"
                ]

        it "still walks to the bank rather than refilling when the unit \
           \is not yet adjacent, and leaves every canteen alone while \
           \it does" $
            runsOk $ lns
                [ waterPrelude
                , "setFluid(10, 10, 'lake')"
                , "POS.gridX, POS.gridY = 4.5, 4.5"
                , "INV = { canteen(1.0), canteen(0.0) }"
                , "WATER.refillExecute(1, knownSource(), P)"
                , "assert(#MOVES == 1, 'it must walk toward the bank: '"
                , "  .. tostring(#MOVES))"
                , "assert(PICKUPS == 0 and INV[1].currentFill == 1.0"
                , "  and INV[2].currentFill == 0.0,"
                , "  'nothing may be filled from two tiles away')"
                ]

    describe "controls that must not move" $ do
        it "scores a full canteen plus an empty one off the empty one in \
           \both orders, exactly as it always did" $
            runsOk $ lns
                [ waterPrelude
                , "INV = { canteen(CAP), canteen(0.0) }"
                , "local rFullFirst = refill()"
                , "INV = { canteen(0.0), canteen(CAP) }"
                , "local rEmptyFirst = refill()"
                , "assert(rFullFirst == PEAK_REFILL"
                , "  and rEmptyFirst == PEAK_REFILL,"
                , "  'a full peer must be skipped, not scored: '"
                , "  .. tostring(rFullFirst) .. ' / '"
                , "  .. tostring(rEmptyFirst))"
                ]

        it "stays ineligible when every canteen is full, when none is \
           \carried at all, and when the only rows are non-canteens" $
            runsOk $ lns
                [ waterPrelude
                , "INV = { canteen(CAP), canteen(CAP) }"
                , "assert(refill() == NEG and search() == NEG,"
                , "  'all-full must not want water')"
                , "INV = {}"
                , "assert(refill() == NEG and search() == NEG,"
                , "  'an empty inventory must not want water')"
                , "INV = { nonContainer('rock'), nonContainer('knife') }"
                , "assert(refill() == NEG and search() == NEG,"
                , "  'rows of another def must not want water')"
                ]

        it "stays ineligible when every canteen is above its emptiness \
           \threshold, including the emptiest of several" $
            runsOk $ lns
                [ waterPrelude
                , "INV = { canteen(1.9), canteen(1.85), canteen(1.95) }"
                , "assert(refill() == NEG,"
                , "  'the threshold must still gate the emptiest: '"
                , "  .. tostring(refill()))"
                , "assert(search() == NEG,"
                , "  'the search threshold must still gate it: '"
                , "  .. tostring(search()))"
                -- Exactly AT the threshold the ramp's left end is live,
                -- which is the boundary the max-scan must not shift.
                , "INV = { canteen(THRESHOLD_FILL) }"
                , "local atThreshold = refill()"
                , "assert(eligible(atThreshold)"
                , "  and atThreshold == P.refill_base_weight,"
                , "  'at the threshold refill sits at the ramp base: '"
                , "  .. tostring(atThreshold))"
                ]

        it "scores a single canteen exactly as before — a half-full one \
           \below the player move, an empty one above it" $
            runsOk $ lns
                [ waterPrelude
                , "INV = { canteen(1.0) }"
                , "local half = refill()"
                , "assert(eligible(half)"
                , "  and half > P.refill_base_weight"
                , "  and half < COMBAT.FOLLOW_COMMAND_UTILITY,"
                , "  'one half-full canteen must stay under command so a '"
                , "  .. 'topping-off does not interrupt orders: '"
                , "  .. tostring(half))"
                , "INV = { canteen(0.0) }"
                , "local dry = refill()"
                , "assert(dry == PEAK_REFILL"
                , "  and dry > COMBAT.FOLLOW_COMMAND_UTILITY,"
                , "  'one dry canteen must outrank command: '"
                , "  .. tostring(dry))"
                , "INV = { canteen(1.0) }"
                , "assert(search() == P.search_base_weight"
                , "  + (0.5 - P.search_min_emptiness)"
                , "    * P.search_emptiness_weight,"
                , "  'the single-canteen search curve must not move: '"
                , "  .. tostring(search()))"
                ]

        it "never divides by a missing or zero capacity, so a \
           \non-container row cannot turn the scan's urgency into NaN" $
            runsOk $ lns
                [ waterPrelude
                , "INV = { nonContainer(), canteen(0.0, 0.0), canteen(0.0) }"
                , "local r, s = refill(), search()"
                , "assert(r == r and s == s,"
                , "  'a NaN utility escaped the presence guard: '"
                , "  .. tostring(r) .. ' / ' .. tostring(s))"
                , "assert(r == PEAK_REFILL and s == PEAK_SEARCH,"
                , "  'the real empty canteen must still be selected: '"
                , "  .. tostring(r) .. ' / ' .. tostring(s))"
                , "INV = { nonContainer(), canteen(0.0, 0.0) }"
                , "assert(refill() == NEG and search() == NEG,"
                , "  'degenerate rows alone must be ineligible, not NaN: '"
                , "  .. tostring(refill()) .. ' / ' .. tostring(search()))"
                ]

    describe "the goal-driven find_water branch is unchanged" $
        it "searches at its derived urgency whatever the canteens hold, \
           \including none at all and a brimming one" $
            runsOk $ lns
                [ waterPrelude
                , "local function expected(thirst)"
                , "  return P.goal_search_floor"
                , "       + P.goal_search_urgency * thirst"
                , "end"
                , "INV = {}"
                , "local none = goalSearch()"
                , "INV = { canteen(CAP) }"
                , "local full = goalSearch()"
                , "INV = { canteen(0.0), canteen(1.9) }"
                , "local mixed = goalSearch()"
                , "assert(none == expected(0.0) and full == expected(0.0)"
                , "  and mixed == expected(0.0),"
                , "  'a hydrated scout searches at the floor regardless '"
                , "  .. 'of canteen contents: ' .. tostring(none) .. ' / '"
                , "  .. tostring(full) .. ' / ' .. tostring(mixed))"
                , "STATS.hydration = 50"
                , "INV = { canteen(CAP) }"
                , "local thirstyFull = goalSearch()"
                , "INV = {}"
                , "local thirstyNone = goalSearch()"
                , "assert(thirstyFull == expected(0.5)"
                , "  and thirstyNone == expected(0.5),"
                , "  'the derived urgency must track thirst, not the '"
                , "  .. 'canteens: ' .. tostring(thirstyFull) .. ' / '"
                , "  .. tostring(thirstyNone))"
                , "assert(thirstyFull > full,"
                , "  'thirst must raise it above the hydrated floor')"
                ]
