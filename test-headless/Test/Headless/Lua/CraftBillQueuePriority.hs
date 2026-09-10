{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The "craft bill queue priority" gate (#2523): a worker choosing a
--   FRESH craft bill takes the earliest one in that station's displayed
--   queue, not the newest one it happens to scan last.
--
--   Before this, @scripts\/unit_ai_craft.lua@'s @findCraftBill@ replaced
--   its incumbent on @d <= bestD@. Every bill at one station shares that
--   station's building info, so every same-station candidate ties on
--   distance and the LAST row scanned won — and the AI's listing
--   (@craft.getBills()@ with no argument,
--   @Engine.Scripting.Lua.API.Craft.Bill.craftGetBillsFn@) is
--   @sortOn cbId@, so that was always the highest-id eligible bill. The
--   #330 station panel meanwhile renders @craft.getBills(bid)@ →
--   @Craft.Bills.billsForStation@ → @sortOn cbSeq@, so a player who
--   reordered the queue saw no change in what the worker picked, and a
--   newer repeating bill could starve an earlier one indefinitely.
--
--   The ordering key under test is therefore @seq@ — the manual-reorder
--   key @Craft.Bills.reorderBill@ swaps and @pushBill@ already publishes
--   on every bill table — and NOT the bill id and NOT the position
--   @craft.getBills()@ happens to return a row in. Two cases below run
--   the identical fixture with the listing reversed to prove the second
--   half of that.
--
--   Scope, matching the issue's own boundaries:
--
--   * Distance still decides FIRST, so a queue position at a distant
--     station never becomes a global priority.
--   * An equal-distance tie between DIFFERENT stations goes to the
--     lower station id, purely so the answer is deterministic.
--   * FRESH selections only. A worker already holding a claim keeps it
--     across a reorder (@craftUtility@ returns @craft_lock_utility@ with
--     @s.craftJob@ untouched); reordering never preempts.
--
--   Same standalone-Lua-VM pattern as
--   "Test.Headless.Lua.WorkClaimCapacity": one self-contained chunk per
--   'it' via 'Lua.dostring' in a fresh interpreter, asserting inside Lua
--   via @assert()@, with a non-OK 'Lua.Status' surfaced as an hspec
--   failure carrying the Lua message. @scripts.unit_ai_craft@ and
--   @scripts.unit_ai_fetch@ are the REAL modules — @findCraftBill@ is
--   @local@ and only @craftUtility@ \/ @craftExecute@ \/ @craftOnExit@
--   are exported, so every case drives @craftAi.craftUtility@ and reads
--   the choice back off @S.craftCandidate.bill.id@, exactly as the
--   arbitration loop would.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "craft bill queue priority"'@.
module Test.Headless.Lua.CraftBillQueuePriority (spec) where

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

-- | Engine-API stubs plus the queue vocabulary every case shares.
--
--   @BILLS@ is an unordered store, and the two views the production
--   code reads are modelled separately and faithfully:
--
--   * @craft.getBills()@ — the AI's discovery listing — hands back a
--     copy ordered by @LISTING_ORDER@, @'id'@ (what the engine's
--     @sortOn cbId@ produces) by default and @'reverse'@ when a case
--     wants to show the answer does not depend on it.
--   * @stationQueue(bid)@ — the #330 panel's view — orders by @seq@,
--     mirroring @Craft.Bills.billsForStation@.
--
--   @reorder@ mirrors @Craft.Bills.reorderBill@: it swaps @seq@ with the
--   immediate neighbour in the SAME station's displayed queue and leaves
--   @id@ alone, which is what makes the ids in these assertions a real
--   discriminator rather than a relabelling.
--
--   Materials are real: the default recipe consumes one @ingot@ and the
--   fixture lays a stack of them on the actor's tile, so
--   @craftMaterialsAvailable@ runs its whole inventory → ground → mule →
--   cargo ladder on every candidate instead of being short-circuited by
--   an empty demand set.
prelude ∷ Text
prelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "package.loaded['scripts.movement_speed'] ="
    , "  { comfort = function() return 1.0 end,"
    , "    ordered = function() return 1.0 end,"
    , "    sprint  = function() return 1.0 end }"
    , "NOW, WARNS = 0, 0"
    , "PAGE = 'stub_page'"
    , "POS = { gridX = 0, gridY = 0, page = PAGE }"
    , "INV, GROUND = {}, {}"
    , "STATIONS, BILLS, CLAIMS = {}, {}, {}"
    , "LISTING_ORDER = 'id'"
    , "ITEM_DEFS = { { name = 'ingot', weight = 1.0 },"
    , "              { name = 'unobtanium', weight = 1.0 } }"
    -- Two recipes, distinguished only by whether their single input can
    -- be sourced: 'widget' can (ingots are on the ground), 'vapor'
    -- cannot (nothing anywhere holds unobtanium).
    , "RECIPES = {"
    , "  widget = { id = 'widget', work = 10.0,"
    , "             inputs = { { item = 'ingot', count = 1 } } },"
    , "  vapor  = { id = 'vapor', work = 10.0,"
    , "             inputs = { { item = 'unobtanium', count = 1 } } } }"
    , "local function bump() WARNS = WARNS + 1 end"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = bump, logError = bump,"
    , "           logInfo = function() end, logDebug = function() end,"
    , "           emitEvent = function() end,"
    , "           emitEventAt = function() end,"
    , "           emitEventForUnit = function() end }"
    , "world = { getActiveWorldId = function() return 1 end }"
    , "debug = debug or {}"
    , "debug.recordOutcome = function() end"
    -- Unit 1 is the actor. Any other uid exists (so a rival claimant is
    -- live) but never appears in getAllIds, so findTechnomule finds no
    -- mule and the ground stays the only off-inventory source.
    , "unit = {"
    , "  exists = function() return true end,"
    , "  getAllIds = function() return {} end,"
    , "  getInfo = function() return POS end,"
    , "  getInventory = function() return INV end,"
    , "  getCarryingWeight = function() return 0 end,"
    , "  getStat = function() return math.huge end,"
    , "  getSkill = function() return 25.0 end,"
    , "  getKnowledge = function() return true end,"
    , "  getMentalEffectiveness = function() return 1.0 end,"
    , "  moveTo = function() end, stop = function() end,"
    , "  dropItemById = function() end,"
    , "  removeItem = function() return true end }"
    , "item = {"
    , "  listDefs = function() return ITEM_DEFS end,"
    , "  getGroundForUnit = function(_, gid)"
    , "    for _, g in ipairs(GROUND) do"
    , "      if g.id == gid then return g, true end end"
    , "    return nil, true end,"
    , "  listGround = function() return GROUND end,"
    , "  spawnGround = function() end,"
    , "  pickupGround = function() return false end }"
    -- No storage buildings: getActiveIds stays empty, so cargoCountOf
    -- contributes nothing and an unsourceable input really is
    -- unsourceable.
    , "building = {"
    , "  getActiveIds = function() return {} end,"
    , "  getActivity = function(bid)"
    , "    local s = STATIONS[bid]"
    , "    return s and (s.activity or 'built') or nil end,"
    , "  getStorage = function() return {} end,"
    , "  getInfo = function(bid) return STATIONS[bid] end }"
    , "power = { isStationPoweredForRecipe = function() return true end }"
    , "craft = {"
    , "  getBills = function()"
    , "    local rows = {}"
    , "    for _, b in ipairs(BILLS) do rows[#rows + 1] = b end"
    , "    table.sort(rows, function(x, y)"
    , "      if LISTING_ORDER == 'reverse' then return x.id > y.id end"
    , "      return x.id < y.id"
    , "    end)"
    , "    return rows end,"
    , "  getBill = function(_uid, id)"
    , "    for _, b in ipairs(BILLS) do if b.id == id then return b end end end,"
    , "  get = function(rid) return RECIPES[rid] end,"
    , "  claimBill = function(id) CLAIMS[#CLAIMS + 1] = id; return true end,"
    , "  releaseBill = function() end,"
    , "  setBillWorking = function() end,"
    , "  addBillProgress = function() return 0 end,"
    , "  completeBillCycle = function() return 0 end,"
    , "  executeAt = function() return true, {} end }"
    , "-- A Built station of this page at (x, y)."
    , "function station(bid, x, y)"
    , "  STATIONS[bid] = { gridX = x, gridY = y, tileW = 1, tileH = 1,"
    , "                    page = PAGE }"
    , "end"
    , "-- Lay `count` ingots on the actor's own tile."
    , "function ingots(count)"
    , "  for _ = 1, count do"
    , "    GROUND[#GROUND + 1] = { id = #GROUND + 1, defName = 'ingot',"
    , "                            x = POS.gridX, y = POS.gridY, weight = 1.0 }"
    , "  end"
    , "end"
    , "-- Queue a bill: explicit id AND explicit seq, because separating"
    , "-- the two is the whole subject. opts = { recipe, mode, remaining,"
    , "-- paused, claimant, claimedAt }."
    , "function bill(id, seq, bid, opts)"
    , "  opts = opts or {}"
    , "  BILLS[#BILLS + 1] = {"
    , "    id = id, seq = seq, station = bid,"
    , "    recipe = opts.recipe or 'widget',"
    , "    mode = opts.mode or 'fixed',"
    , "    remaining = opts.remaining or 3,"
    , "    progress = 0, working = false,"
    , "    paused = opts.paused or false,"
    , "    claimant = opts.claimant, claimedAt = opts.claimedAt }"
    , "end"
    , "function billById(id)"
    , "  for _, b in ipairs(BILLS) do if b.id == id then return b end end"
    , "end"
    , "-- Craft.Bills.billsForStation: one station's bills by ascending seq."
    , "function stationQueue(bid)"
    , "  local q = {}"
    , "  for _, b in ipairs(BILLS) do"
    , "    if b.station == bid then q[#q + 1] = b end"
    , "  end"
    , "  table.sort(q, function(x, y) return x.seq < y.seq end)"
    , "  return q"
    , "end"
    , "-- The displayed queue as an id list, for assertion messages and"
    , "-- for pinning what the player would actually be looking at."
    , "function queueIds(bid)"
    , "  local out = {}"
    , "  for _, b in ipairs(stationQueue(bid)) do out[#out + 1] = b.id end"
    , "  return table.concat(out, ',')"
    , "end"
    , "-- Craft.Bills.reorderBill: swap `seq` with the immediate neighbour"
    , "-- in the SAME station's queue; `id` never moves."
    , "function reorder(id, dir)"
    , "  local target = billById(id)"
    , "  assert(target, 'no such bill ' .. tostring(id))"
    , "  local q = stationQueue(target.station)"
    , "  local i"
    , "  for k, b in ipairs(q) do if b.id == id then i = k end end"
    , "  local j = (dir == 'up') and (i - 1) or (i + 1)"
    , "  if j < 1 or j > #q then return false end"
    , "  local nb = q[j]"
    , "  target.seq, nb.seq = nb.seq, target.seq"
    , "  return true"
    , "end"
    , "local craftAi = require('scripts.unit_ai_craft')"
    , "PARAMS = { craft_scan_range = 30.0, craft_base_utility = 3.2,"
    , "           craft_lock_utility = 6.0, craft_rate = 1.0,"
    , "           craft_claim_timeout = 30.0, craft_xp_per_craft = 1.5,"
    , "           pickup_arrival_tiles = 1.2, mule_fetch_arrival = 1.5 }"
    , "-- One FRESH selection: a worker holding no craft job scores the"
    , "-- craft action and records its choice in s.craftCandidate. A fresh"
    , "-- state every call is the point -- this is the decision point the"
    , "-- issue is about, and nothing here ever preempts a held job."
    , "function pick()"
    , "  local s = {}"
    , "  assert(s.craftJob == nil, 'pick() must model a FRESH selection')"
    , "  local u = craftAi.craftUtility(1, s, PARAMS)"
    , "  if u == -math.huge then return nil end"
    , "  assert(s.craftCandidate, 'a scored craft action must leave a candidate')"
    , "  return s.craftCandidate.bill.id"
    , "end"
    , "-- Assert one fresh selection, under BOTH listing orders."
    , "--"
    , "-- The queue expectation is checked FIRST, against the listing the"
    , "-- engine really produces (sortOn cbId), so a failure names the"
    , "-- ordering defect rather than a symptom of it. The reversed pass"
    , "-- then pins the second half of the contract: the answer must not"
    , "-- depend on the order craft.getBills happens to return rows in."
    , "function pickStable(expected, why)"
    , "  LISTING_ORDER = 'id'"
    , "  local byId = pick()"
    , "  LISTING_ORDER = 'reverse'"
    , "  local reversed = pick()"
    , "  LISTING_ORDER = 'id'"
    , "  assert(byId == expected, why .. ': expected bill '"
    , "    .. tostring(expected) .. ', got ' .. tostring(byId))"
    , "  assert(reversed == expected, why .. ': with the craft.getBills'"
    , "    .. ' rows reversed, expected bill ' .. tostring(expected)"
    , "    .. ' but got ' .. tostring(reversed)"
    , "    .. ' -- the choice must not depend on listing order')"
    , "  return byId"
    , "end"
    ]

spec ∷ Spec
spec = describe "craft bill queue priority" $ do

    describe "one station, two eligible bills" $ do
        it "takes the earliest bill in the displayed queue, and follows \
           \the queue when it opposes creation order" $ runsOk $ lns
            [ prelude
            , "station(7, 5, 0)"
            , "ingots(20)"
            , "bill(1, 1, 7)"
            , "bill(2, 2, 7)"
            , "assert(queueIds(7) == '1,2', 'fixture queue: ' .. queueIds(7))"
            -- The regression itself: pre-fix the last-scanned (highest
            -- id) row won every same-station distance tie, so this
            -- asserted 2.
            , "pickStable(1, 'the queue 1,2 head')"
            -- Vacuity guard, direction one: a HIGHER id must win when
            -- the queue puts it first, so a fixture cannot pass by
            -- always preferring the lower id.
            , "assert(reorder(2, 'up'), 'bill 2 must move up')"
            , "assert(queueIds(7) == '2,1', 'reordered queue: ' .. queueIds(7))"
            , "pickStable(2, 'the queue 2,1 head')"
            , "assert(WARNS == 0, 'ordering must be silent')"
            ]

        it "follows a reorder in BOTH directions across successive fresh \
           \selections" $ runsOk $ lns
            [ prelude
            , "station(7, 5, 0)"
            , "ingots(20)"
            , "bill(1, 1, 7)"
            , "bill(2, 2, 7)"
            , "bill(3, 3, 7)"
            , "pickStable(1, 'the untouched queue head')"
            -- Down, twice: bill 1 sinks to the bottom and the pick
            -- follows it down the queue rather than tracking id order.
            , "assert(reorder(1, 'down'), 'bill 1 down')"
            , "assert(queueIds(7) == '2,1,3', 'queue: ' .. queueIds(7))"
            , "pickStable(2, 'after moving bill 1 down once')"
            , "assert(reorder(1, 'down'), 'bill 1 down again')"
            , "assert(queueIds(7) == '2,3,1', 'queue: ' .. queueIds(7))"
            , "pickStable(2, 'after moving bill 1 down twice')"
            -- Up, twice: back to the head, and the pick comes back with
            -- it. Vacuity guard, direction two -- selecting the LOWER id
            -- after a reorder is unreachable for the pre-fix scan.
            , "assert(reorder(1, 'up'), 'bill 1 up')"
            , "assert(reorder(1, 'up'), 'bill 1 up again')"
            , "assert(queueIds(7) == '1,2,3', 'queue: ' .. queueIds(7))"
            , "pickStable(1, 'after moving bill 1 back to the head')"
            -- And the far end refuses to move further, exactly as
            -- reorderBill reports False at either end.
            , "assert(reorder(1, 'up') == false, 'the head cannot move up')"
            , "pickStable(1, 'after a refused move off the head')"
            ]

        it "gives an earlier finite bill precedence over a later \
           \repeating bill at a fresh decision point" $ runsOk $ lns
            [ prelude
            , "station(7, 5, 0)"
            , "ingots(20)"
            -- The Background's starvation shape: a newer repeat bill
            -- alongside an earlier fixed-count one. Pre-fix the repeat
            -- bill (higher id, scanned last) won every fresh selection
            -- too, so the fixed bill never ran.
            , "bill(1, 1, 7, { mode = 'fixed', remaining = 2 })"
            , "bill(2, 2, 7, { mode = 'repeat', remaining = -1 })"
            , "pickStable(1, 'the earlier finite bill on a FRESH selection')"
            -- Requirement 5 in the same fixture: this is about fresh
            -- selection only. A worker already chained onto the repeat
            -- bill keeps it -- craftUtility returns the lock utility and
            -- leaves the job alone, whatever the queue says.
            , "local held = { craftJob = { billId = 2, bid = 7,"
            , "                            recipeId = 'widget', work = 0,"
            , "                            phase = 'fetch' } }"
            , "billById(2).claimant = 1"
            , "billById(2).claimedAt = NOW"
            , "local u = craftAi.craftUtility(1, held, PARAMS)"
            , "assert(u == PARAMS.craft_lock_utility,"
            , "  'a held bill must keep its lock utility, got ' .. tostring(u))"
            , "assert(held.craftJob and held.craftJob.billId == 2,"
            , "  'an in-progress cycle must not be preempted by the queue')"
            ]

    describe "an ineligible earlier bill never blocks a later one" $ do
        it "skips a paused bill at the head of the queue" $ runsOk $ lns
            [ prelude
            , "station(7, 5, 0)"
            , "ingots(20)"
            , "bill(1, 1, 7, { paused = true })"
            , "bill(2, 2, 7)"
            , "assert(queueIds(7) == '1,2', 'queue: ' .. queueIds(7))"
            , "pickStable(2, 'past a paused queue head')"
            -- Unpausing hands the lead straight back, so the skip really
            -- was the pause and not something else about bill 1.
            , "billById(1).paused = false"
            , "pickStable(1, 'after unpausing the queue head')"
            ]

        it "skips a bill another worker holds a fresh claim on" $ runsOk $ lns
            [ prelude
            , "station(7, 5, 0)"
            , "ingots(20)"
            , "NOW = 100"
            , "bill(1, 1, 7, { claimant = 9, claimedAt = 100 })"
            , "bill(2, 2, 7)"
            , "pickStable(2, 'past a freshly claimed queue head')"
            -- Past the claim timeout the claim is stale, and the queue
            -- head is eligible again.
            , "NOW = 100 + PARAMS.craft_claim_timeout + 1"
            , "pickStable(1, 'after the head claim goes stale')"
            ]

        it "skips a bill whose inputs cannot be sourced" $ runsOk $ lns
            [ prelude
            , "station(7, 5, 0)"
            , "ingots(20)"
            , "bill(1, 1, 7, { recipe = 'vapor' })"
            , "bill(2, 2, 7)"
            , "pickStable(2, 'past an unsourceable queue head')"
            -- Supplying the input makes the head eligible, which is what
            -- proves the skip came from the material gate.
            , "GROUND[#GROUND + 1] = { id = #GROUND + 1,"
            , "                        defName = 'unobtanium',"
            , "                        x = POS.gridX, y = POS.gridY,"
            , "                        weight = 1.0 }"
            , "pickStable(1, 'once the head input can be sourced')"
            ]

    describe "proximity still decides between stations" $ do
        it "prefers a nearer station's queue head over a farther \
           \station's, whatever the ids say" $ runsOk $ lns
            [ prelude
            , "station(7, 2, 0)"
            , "station(8, 20, 0)"
            , "ingots(20)"
            -- The near station carries the NEWEST bills; the far one
            -- carries the oldest. A global seq/id priority would take
            -- bill 1 across the map.
            , "bill(5, 5, 7)"
            , "bill(6, 6, 7)"
            , "bill(1, 1, 8)"
            , "pickStable(5, 'the near station and its own queue head')"
            -- Queue order inside the near station still applies; the far
            -- station's position in the global listing never does.
            , "assert(reorder(6, 'up'), 'bill 6 up')"
            , "assert(queueIds(7) == '6,5', 'near queue: ' .. queueIds(7))"
            , "pickStable(6, 'the reordered near-station queue head')"
            -- Out of range entirely: the far station is the only
            -- candidate left, so range admission is unchanged.
            , "STATIONS[7] = nil"
            , "pickStable(1, 'the far bill once the near station is gone')"
            ]

        it "breaks an exact equal-distance tie between two stations by \
           \the lower station id, independently of listing order" $ runsOk $ lns
            [ prelude
            -- Centres (3.5, 0.5) and (0.5, 3.5) from the actor at the
            -- origin: the same two squares summed in the other order, so
            -- the distances are bit-identical rather than merely close.
            , "station(8, 3, 0)"
            , "station(9, 0, 3)"
            , "ingots(20)"
            , "bill(10, 1, 8)"
            , "bill(2, 1, 9)"
            , "pickStable(10, 'the lower station id on an exact tie')"
            -- Swapping the two stations' POSITIONS changes nothing: the
            -- rule is the station id, and the distances stay tied.
            , "STATIONS[8], STATIONS[9] = STATIONS[9], STATIONS[8]"
            , "pickStable(10, 'station 8 after the two swap positions')"
            -- Swapping which station each BILL sits at does change the
            -- winner, so the rule is not secretly the bill id and not
            -- scan order.
            , "billById(10).station, billById(2).station = 9, 8"
            , "pickStable(2, 'whichever bill now sits at station 8')"
            ]
