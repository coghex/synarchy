-- | Every AI target the four LAX item verbs can reach is paired with
--   the ACTING unit's own world page (#1673).
--
--   @unit.getAllIds@, @building.getActiveIds@ and @craft.getBills@ each
--   resolve the ACTIVE page independently, at the instant they are
--   called, and @world.show@ \/ @world.hide@ only ENQUEUE a selection
--   change the world thread applies later. So the active page can move
--   between any two of those calls inside one AI update, and a finder
--   that trusts "the actors and the buildings both came from the active
--   page" can hand an actor a candidate in another world. The finders'
--   own comments asserted that pairing; nothing enforced it.
--
--   The engine-side refusal
--   ('Test.Headless.Unit.CargoApi') is the caller-independent half and
--   closes the item-state consequence. This gate is the OTHER half: an
--   off-page candidate must never be cached, claimed, walked toward, or
--   passed to a lax verb in the first place — a walk toward another
--   page's coordinates is a consequence no commit-time refusal can
--   undo.
--
--   Every case is the same shape: one candidate, at the SAME
--   coordinates and in the same state, run twice — once on the actor's
--   page and once on another. Only the page differs, so a case cannot
--   pass because the candidate was unattractive for some other reason.
--
--   Same standalone-Lua-VM pattern as
--   'Test.Headless.Lua.UnitAiLogisticsTargets': one self-contained
--   chunk per 'it' in a fresh interpreter, asserting inside Lua, with
--   the REAL shipped modules and the REAL shipped acolyte tunables.
--   @scripts.movement_speed@ is stubbed (the real one reaches the whole
--   physiology chain to answer one pace) and @scripts.unit_ai@ is the
--   singleton table the submodules attach to; everything the page rule
--   actually runs through — the finders, the ladders, the caches — is
--   production code.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "AI page pairing"'@.
module Test.Headless.Lua.UnitAiPageTargets (spec) where

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

-- | A world model with an explicit page per entity, plus counters for
--   every observable consequence a mis-paired target would cause: a
--   move request, a stop, and each of the four lax item calls.
--
--   @HOME@ is the acting unit's page and @AWAY@ is the other live one.
--   A case places its ONE candidate on whichever it is testing; the
--   candidate is otherwise byte-identical between the two halves.
prelude ∷ Text
prelude = lns
    [ "package.loaded['scripts.unit_ai'] = {}"
    , "package.loaded['scripts.movement_speed'] ="
    , "  { comfort = function() return 1.0 end,"
    , "    ordered = function() return 1.0 end,"
    , "    sprint  = function() return 1.0 end }"
    , "HOME, AWAY = 'page_home', 'page_away'"
    , "NOW = 0"
    , "UNITS = {}       -- [uid] = info row, page included"
    , "BUILDINGS = {}   -- array of building rows, page included"
    , "BILLS = {}"
    , "GROUND = {}"
    , "MOVES, STOPS = 0, 0"
    , "DEPOSITS, WITHDRAWS, TO_UNIT, TO_BUILDING = 0, 0, 0, 0"
    -- #2297: the medic ladder's own surface. WOUNDS is what makes a
    -- unit a patient, KNOW is per-uid so one medic can genuinely
    -- outrank another, and TREAT_RANGE stands in for the engine's
    -- unit.treatmentRange() so a case can move the reach and watch the
    -- shipped arrival check follow it.
    , "WOUNDS, KNOW = {}, {}"
    , "TREAT_RANGE = 1.5"
    , "TREATS, INFECT_TREATS = 0, 0"
    , "faction = { areAllies = function() return true end }"
    , "function unitRow(uid, defName, x, y, pg, inv)"
    , "  UNITS[uid] = { uid = uid, defName = defName, gridX = x, gridY = y,"
    , "                 page = pg, inv = inv or {}, contents = {} }"
    , "  return UNITS[uid]"
    , "end"
    , "-- A BUILT store, or a CONSTRUCTING build site, at (x, 0) on page pg."
    , "function buildingRow(kind, bid, x, pg, extra)"
    , "  local b = { bid = bid, x = x, y = 0, page = pg, storage = {},"
    , "              need = {}, delivered = {} }"
    , "  if kind == 'store' then"
    , "    b.activity, b.required, b.capacity, b.used = 'built', 0, 100, 0"
    , "  else"
    , "    b.activity, b.required, b.capacity, b.used = 'constructing', 240.0, 0, 0"
    , "  end"
    , "  for k, v in pairs(extra or {}) do b[k] = v end"
    , "  BUILDINGS[#BUILDINGS + 1] = b"
    , "  return b"
    , "end"
    , "local function brow(bid)"
    , "  for _, b in ipairs(BUILDINGS) do if b.bid == bid then return b end end"
    , "end"
    , "engine = { gameTime = function() return NOW end,"
    , "           logWarn = function() end, logError = function() end,"
    , "           logInfo = function() end, logDebug = function() end,"
    , "           emitEvent = function() end, emitEventAt = function() end,"
    , "           emitEventForUnit = function() end }"
    , "world = { getActiveWorldId = function() return 1 end,"
    , "          findHarvestableFlora = function() return nil end }"
    , "debug = debug or {}"
    , "debug.recordOutcome = function() end"
    -- A hungry unit: forage only scores below forage_max_fraction of a
    -- full stomach, so the flat 1.0 every other stat gets would gate
    -- the whole action out.
    , "STATS = { carrying_capacity = 100, hunger = 1, max_hunger = 100,"
    , "          calories = 1, max_calories = 100 }"
    , "unit = {"
    , "  exists = function(uid) return UNITS[uid] ~= nil end,"
    , "  getAllIds = function()"
    , "    local ids = {}"
    , "    for uid in pairs(UNITS) do ids[#ids + 1] = uid end"
    , "    table.sort(ids)"
    , "    return ids end,"
    , "  getInfo = function(uid) return UNITS[uid] end,"
    , "  getInventory = function(uid)"
    , "    return UNITS[uid] and UNITS[uid].inv or {} end,"
    , "  getItemContents = function(uid, defName)"
    , "    local u = UNITS[uid]"
    , "    return u and u.contents[defName] or {} end,"
    -- Half-loaded: store_materials' utility is (carried/capacity)^3, so
    -- an empty unit would score 0 and the same-page control could not
    -- tell "cached and scored" from "found nothing".
    , "  getCarryingWeight = function() return 50 end,"
    , "  getStat = function(_, k)"
    , "    if STATS[k] ~= nil then return STATS[k] end"
    , "    return 1.0 end,"
    , "  getSkill = function() return 25.0 end,"
    -- A LEVEL, not a flag: unit_ai_medic multiplies it by intelligence
    -- to rank medics, so a boolean could not model two of them.
    , "  getKnowledge = function(uid) return KNOW[uid] or 50.0 end,"
    , "  getFaction = function() return 'player' end,"
    , "  getWounds = function(uid) return WOUNDS[uid] or {} end,"
    , "  treatBleeding = function()"
    , "    TREATS = TREATS + 1; return { ok = true } end,"
    , "  treatInfection = function()"
    , "    INFECT_TREATS = INFECT_TREATS + 1; return { ok = true } end,"
    , "  treatmentRange = function() return TREAT_RANGE end,"
    , "  getMentalEffectiveness = function() return 1.0 end,"
    , "  getPose = function() return 'standing' end,"
    , "  dropItemById = function() end,"
    , "  addXP = function() end,"
    , "  pickup = function() end,"
    , "  moveTo = function() MOVES = MOVES + 1 end,"
    , "  stop = function() STOPS = STOPS + 1 end,"
    , "  depositToCargo = function() DEPOSITS = DEPOSITS + 1; return false end,"
    , "  withdrawFromCargo = function() WITHDRAWS = WITHDRAWS + 1; return false end,"
    , "  transferItemToUnit = function() TO_UNIT = TO_UNIT + 1; return false end,"
    , "  transferItemToBuilding = function()"
    , "    TO_BUILDING = TO_BUILDING + 1; return false end }"
    , "building = {"
    , "  getActiveIds = function()"
    , "    local ids = {}"
    , "    for _, b in ipairs(BUILDINGS) do ids[#ids + 1] = b.bid end"
    , "    return ids end,"
    , "  getActivity = function(bid) local b = brow(bid); return b and b.activity end,"
    , "  getBuildRequired = function(bid) local b = brow(bid); return b and b.required end,"
    , "  getStorageCapacity = function(bid) local b = brow(bid); return b and b.capacity end,"
    , "  getStorageWeight = function(bid) local b = brow(bid); return b and b.used end,"
    , "  getStorage = function(bid) local b = brow(bid); return b and b.storage or {} end,"
    , "  getMaterialNeed = function(bid) local b = brow(bid); return b and b.need or {} end,"
    , "  getMaterialDelivered = function(bid)"
    , "    local b = brow(bid); return b and b.delivered or {} end,"
    , "  areMaterialsSatisfied = function(bid)"
    , "    local b = brow(bid); return b == nil or next(b.need) == nil end,"
    , "  getInfo = function(bid)"
    , "    local b = brow(bid)"
    , "    if not b then return nil end"
    , "    return { gridX = b.x, gridY = b.y, tileW = 1, tileH = 1,"
    , "             page = b.page } end }"
    , "PICKUPS = 0"
    , "PICKUP_OK = true"
    -- listGround is the ACTIVE page's listing; getGroundForUnit answers
    -- for the page the NAMED UNIT stands on, and returns (nil, true)
    -- when that page genuinely holds no such id. Rows carry a page and
    -- a per-page id, so the same gid can name a different item on each
    -- page — the exact shape #1666's owning-page reader exists for.
    , "GROUND_BY_PAGE = {}"
    , "item = {"
    , "  listGround = function() return GROUND end,"
    , "  getGroundForUnit = function(uid, gid)"
    , "    local u = UNITS[uid]"
    , "    if not u then return nil, false end"
    , "    local rows = GROUND_BY_PAGE[u.page]"
    , "    if not rows then return nil, false end"
    , "    for _, r in ipairs(rows) do"
    , "      if r.id == gid then return r, true end"
    , "    end"
    , "    return nil, true end,"
    , "  pickupGround = function()"
    , "    PICKUPS = PICKUPS + 1; return PICKUP_OK end,"
    , "  getFood = function(defName)"
    , "    if defName == 'berry' then return { calories = 100 } end end,"
    , "  listDefs = function() return { { name = 'plate_steel', weight = 1 } } end }"
    -- #2325: every bill verb an AI job drives is ACTOR-QUALIFIED — the
    -- acting unit comes first (claimBill keeps its original
    -- (billId, uid, timeout) order) and the engine resolves the id on
    -- THAT unit's own page. These stubs record the (verb, uid, id) triple
    -- of every such call so a case can assert the acting uid really
    -- crossed the boundary; a stub with the old arity would silently make
    -- `id` nil and read as "no such bill".
    , "BILL_CALLS = {}"
    , "COMPLETE_REMAINING = 0"
    , "local function billCall(verb, uid, id, extra)"
    , "  BILL_CALLS[#BILL_CALLS + 1] ="
    , "    { verb = verb, uid = uid, id = id, extra = extra }"
    , "end"
    , "craft = {"
    , "  getBills = function() return BILLS end,"
    , "  getBill = function(uid, id)"
    , "    billCall('getBill', uid, id)"
    , "    for _, b in ipairs(BILLS) do if b.id == id then return b end end end,"
    , "  get = function(rid) return { id = rid, work = 0, inputs = {} } end,"
    , "  claimBill = function(id, uid)"
    , "    billCall('claimBill', uid, id); return true end,"
    , "  releaseBill = function(uid, id) billCall('releaseBill', uid, id) end,"
    , "  setBillWorking = function(uid, id, flag)"
    , "    billCall('setBillWorking', uid, id, flag) end,"
    , "  addBillProgress = function(uid, id, delta)"
    , "    billCall('addBillProgress', uid, id, delta); return 1.0 end,"
    , "  completeBillCycle = function(uid, id)"
    , "    billCall('completeBillCycle', uid, id)"
    , "    return COMPLETE_REMAINING end,"
    , "  executeAt = function(uid, _rid, _bid, billId)"
    , "    billCall('executeAt', uid, billId); return true, {} end }"
    -- Every recorded call must name the acting unit AND a real bill id.
    -- Returns the verbs seen, in order, so a case can also show WHICH
    -- paths it actually reached rather than passing on an empty log.
    , "function billCallsBy(wantUid, wantId)"
    , "  local seen = {}"
    , "  for _, c in ipairs(BILL_CALLS) do"
    , "    assert(c.uid == wantUid, c.verb .. ' got uid ' .. tostring(c.uid)"
    , "      .. ', expected the acting unit ' .. tostring(wantUid))"
    , "    assert(c.id == wantId, c.verb .. ' got bill id ' .. tostring(c.id)"
    , "      .. ', expected ' .. tostring(wantId))"
    , "    seen[#seen + 1] = c.verb"
    , "  end"
    , "  return table.concat(seen, ',')"
    , "end"
    , "power = { isStationPoweredForRecipe = function() return true end }"
    , "repair = { get = function(rid)"
    , "             return { id = rid,"
    , "                      inputs = { { item = 'whetstone', count = 1 } } } end,"
    , "           repairAt = function() return true end }"
    , "equipment = { getLoadout = function() return {} end,"
    , "              getAccessories = function() return {} end }"
    -- findStation ranks over the ACTIVE page; STATION is whatever that
    -- listing would hand back, page and all.
    , "STATION = nil"
    , "building.findStation = function()"
    , "  if not STATION then return nil end"
    , "  return STATION.bid, STATION.x, STATION.y end"
    , "local PARAMS = require('scripts.unit_ai_tunables').acolyte"
    , "local page      = require('scripts.unit_ai_page')"
    , "local fetch     = require('scripts.unit_ai_fetch')"
    , "local logistics = require('scripts.unit_ai_logistics')"
    , "local deliver   = require('scripts.unit_ai_deliver')"
    , "local craftAi   = require('scripts.unit_ai_craft')"
    , "local medic     = require('scripts.unit_ai_medic')"
    , "local repairAi  = require('scripts.unit_ai_repair')"
    , "local needs     = require('scripts.unit_ai_needs')"
    , "-- The acting unit: uid 1, on HOME, at the origin."
    , "local ACTOR = unitRow(1, 'acolyte', 0, 0, HOME,"
    , "                      { { defName = 'plate_steel', category = 'Materials' } })"
    , "local function newState() return { currentAction = nil } end"
    -- One bleeding ally at (3, 0) on the given page: seeping above
    -- treat_min_seep and unclotted, which is all needsTreatment reads.
    , "function woundedAlly(uid, pg, x)"
    , "  unitRow(uid, 'acolyte', x or 3, 0, pg)"
    , "  WOUNDS[uid] = { { kind = 'slash', bandage = 1.0, clot = 0 } }"
    , "end"
    , "function clearUnit(uid) UNITS[uid] = nil; WOUNDS[uid] = nil end"
    ]

spec ∷ Spec
spec = pageSpec >> reachSpec

pageSpec ∷ Spec
pageSpec = describe "AI page pairing" $ do

    describe "store_materials" $ do
        it "ignores a cargo on another page, and takes the same one on its own" $
            runsOk $ lns
                [ prelude
                , "buildingRow('store', 9, 5, AWAY)"
                , "local s = newState()"
                , "s.storeTarget = 4242"
                , "local u = logistics.storeMaterialsUtility(1, s, PARAMS)"
                , "assert(u == -math.huge,"
                , "  'an off-page cargo must not score, got ' .. tostring(u))"
                , "assert(s.storeTarget == nil,"
                , "  'an off-page cargo must not be cached, left ' .. tostring(s.storeTarget))"
                , "logistics.storeMaterialsExecute(1, s, PARAMS)"
                , "assert(MOVES == 0 and DEPOSITS == 0,"
                , "  'an off-page cargo must draw neither a walk nor a deposit')"
                , "-- The SAME building, same coordinates, on the actor's page."
                , "BUILDINGS = {}"
                , "buildingRow('store', 9, 5, HOME)"
                , "local t = newState()"
                , "local v = logistics.storeMaterialsUtility(1, t, PARAMS)"
                , "assert(v > 0, 'a same-page cargo must still score, got ' .. tostring(v))"
                , "assert(t.storeTarget == 9,"
                , "  'a same-page cargo must still be cached, got ' .. tostring(t.storeTarget))"
                , "logistics.storeMaterialsExecute(1, t, PARAMS)"
                , "assert(MOVES == 1, 'a same-page cargo must still draw the walk')"
                ]

    describe "build_nearby" $ do
        it "ignores a site on another page, and takes the same one on its own" $
            runsOk $ lns
                [ prelude
                , "buildingRow('site', 77, 5, AWAY)"
                , "local s = newState()"
                , "s.buildTarget = 4242"
                , "local u = logistics.buildNearbyUtility(1, s, PARAMS)"
                , "assert(u == -math.huge,"
                , "  'an off-page site must not score, got ' .. tostring(u))"
                , "assert(s.buildTarget == nil,"
                , "  'an off-page site must not be cached, left ' .. tostring(s.buildTarget))"
                , "logistics.buildNearbyExecute(1, s, PARAMS)"
                , "assert(MOVES == 0 and STOPS == 0,"
                , "  'an off-page site must draw neither a walk nor a stop')"
                , "BUILDINGS = {}"
                , "buildingRow('site', 77, 5, HOME)"
                , "local t = newState()"
                , "local v = logistics.buildNearbyUtility(1, t, PARAMS)"
                , "assert(v > 0, 'a same-page site must still score, got ' .. tostring(v))"
                , "assert(t.buildTarget == 77, 'a same-page site must still be cached')"
                , "logistics.buildNearbyExecute(1, t, PARAMS)"
                , "assert(MOVES == 1, 'a same-page site must still draw the walk')"
                ]

    describe "deliver_to_build_site" $ do
        it "ignores a build site on another page" $
            runsOk $ lns
                [ prelude
                , "buildingRow('site', 55, 5, AWAY, { need = { plate_steel = 1 } })"
                , "local s = newState()"
                , "local u = deliver.deliverUtility(1, s, PARAMS)"
                , "assert(u == -math.huge,"
                , "  'an off-page site must not score, got ' .. tostring(u))"
                , "assert(s.deliveryPendingTarget == nil,"
                , "  'an off-page site must leave no pending target')"
                , "BUILDINGS = {}"
                , "buildingRow('site', 55, 5, HOME, { need = { plate_steel = 1 } })"
                , "local t = newState()"
                , "local v = deliver.deliverUtility(1, t, PARAMS)"
                , "assert(v > 0, 'a same-page site must still score, got ' .. tostring(v))"
                , "assert(t.deliveryPendingTarget"
                , "  and t.deliveryPendingTarget.bid == 55,"
                , "  'a same-page site must still be planned')"
                ]

        it "checks the claim's page BEFORE the sourcing phases run" $
            runsOk $ lns
                [ prelude
                -- The sourcing phases issue their own moveTo /
                -- pickupGround / transferItemToUnit, so a claim whose
                -- page is only checked at the arrival branch has
                -- already walked the unit and moved items by then. The
                -- claim carries BOTH ground and mule work, and both a
                -- reachable ground item and a reachable mule exist on
                -- the actor's page, so a check that ran too late would
                -- demonstrably fire them.
                , "buildingRow('site', 55, 5, AWAY, { need = { plate_steel = 1 } })"
                , "unitRow(2, 'technomule', 1, 0, HOME,"
                , "        { { defName = 'plate_steel', category = 'Materials' } })"
                , "GROUND = { { id = 1, defName = 'plate_steel', x = 0, y = 0,"
                , "             weight = 1 } }"
                , "GROUND_BY_PAGE = { [HOME] = GROUND }"
                , "local s = newState()"
                , "s.deliveryClaim = { bid = 55, materials = { plate_steel = 2 },"
                , "                    fromGround = { plate_steel = 1 },"
                , "                    fromMule = { plate_steel = 1 } }"
                , "deliver.deliverExecute(1, s, PARAMS)"
                , "assert(s.deliveryClaim == nil,"
                , "  'an off-page claim must be released before sourcing')"
                , "assert(PICKUPS == 0,"
                , "  'an off-page claim must never pick a ground item up')"
                , "assert(TO_UNIT == 0,"
                , "  'an off-page claim must never pull from the mule')"
                , "assert(MOVES == 0 and STOPS == 0,"
                , "  'an off-page claim must not steer a walk during sourcing')"
                , "-- Control: the SAME claim on the actor's page does"
                , "-- reach the ground rung, so the assertions above are"
                , "-- about the page and not about an inert fixture."
                , "BUILDINGS = {}"
                , "buildingRow('site', 55, 5, HOME, { need = { plate_steel = 1 } })"
                , "local t = newState()"
                , "t.deliveryClaim = { bid = 55, materials = { plate_steel = 2 },"
                , "                    fromGround = { plate_steel = 1 },"
                , "                    fromMule = { plate_steel = 1 } }"
                , "deliver.deliverExecute(1, t, PARAMS)"
                , "assert(PICKUPS == 1,"
                , "  'a same-page claim must still source from the ground')"
                ]

        it "releases a PERSISTED claim naming a building on another page" $
            runsOk $ lns
                [ prelude
                -- The shape a save written before this check restores,
                -- or a page switch between claim and arrival leaves.
                , "buildingRow('site', 55, 5, AWAY, { need = { plate_steel = 1 } })"
                , "local s = newState()"
                , "s.deliveryClaim = { bid = 55, materials = { plate_steel = 1 } }"
                , "deliver.deliverExecute(1, s, PARAMS)"
                , "assert(s.deliveryClaim == nil,"
                , "  'an off-page claim must be released, left '"
                , "  .. tostring(s.deliveryClaim and s.deliveryClaim.bid))"
                , "assert(MOVES == 0, 'an off-page claim must not steer a walk')"
                , "assert(TO_BUILDING == 0,"
                , "  'an off-page claim must never reach transferItemToBuilding')"
                ]

    describe "the cargo rung of the sourcing ladder" $ do
        it "counts stock on the actor's page only" $
            runsOk $ lns
                [ prelude
                , "local stock = { { defName = 'plate_steel' } }"
                , "buildingRow('store', 20, 5, AWAY, { storage = stock })"
                , "assert(fetch.cargoCountOf('plate_steel', HOME) == 0,"
                , "  'off-page cargo stock must not be counted')"
                , "BUILDINGS = {}"
                , "buildingRow('store', 20, 5, HOME, { storage = stock })"
                , "assert(fetch.cargoCountOf('plate_steel', HOME) == 1,"
                , "  'same-page cargo stock must still be counted')"
                , "assert(fetch.cargoCountOf('plate_steel', nil) == 0,"
                , "  'an unknown actor page must count nothing')"
                ]

        it "never walks to or withdraws from a store on another page" $
            runsOk $ lns
                [ prelude
                , "local stock = { { defName = 'plate_steel' } }"
                , "buildingRow('store', 20, 5, AWAY, { storage = stock })"
                , "local busy = fetch.fetchWantsFromCargo("
                , "  1, { plate_steel = 1 }, ACTOR, PARAMS)"
                , "assert(busy == false, 'no reachable store means the fetch is done')"
                , "assert(MOVES == 0 and WITHDRAWS == 0,"
                , "  'an off-page store must draw neither a walk nor a withdrawal')"
                , "BUILDINGS = {}"
                , "buildingRow('store', 20, 5, HOME, { storage = stock })"
                , "local busy2 = fetch.fetchWantsFromCargo("
                , "  1, { plate_steel = 1 }, ACTOR, PARAMS)"
                , "assert(busy2 == true, 'a distant same-page store must still be walked to')"
                , "assert(MOVES == 1, 'a same-page store must still draw the walk')"
                ]

    describe "craft_job" $ do
        it "ignores a station on another page" $
            runsOk $ lns
                [ prelude
                , "buildingRow('store', 42, 5, AWAY)"
                , "BILLS = { { id = 1, station = 42, recipe = 'r', mode = 'count' } }"
                , "local s = newState()"
                , "local u = craftAi.craftUtility(1, s, PARAMS)"
                , "assert(u == -math.huge,"
                , "  'an off-page station must not score, got ' .. tostring(u))"
                , "assert(s.craftCandidate == nil,"
                , "  'an off-page station must leave no candidate')"
                , "BUILDINGS = {}"
                , "buildingRow('store', 42, 5, HOME)"
                , "local t = newState()"
                , "local v = craftAi.craftUtility(1, t, PARAMS)"
                , "assert(v > 0, 'a same-page station must still score, got ' .. tostring(v))"
                , "assert(t.craftCandidate and t.craftCandidate.bill.id == 1,"
                , "  'a same-page station must still be nominated')"
                ]

        it "checks the station's page BEFORE the fetch phase runs" $
            runsOk $ lns
                [ prelude
                -- Same hazard as the delivery claim: craftExecute's
                -- fetch phase sources from ground, mule and cargo, all
                -- of which move the unit and items. A station checked
                -- only at the walking phase is checked too late.
                , "buildingRow('store', 42, 5, AWAY)"
                , "buildingRow('store', 43, 6, HOME,"
                , "  { storage = { { defName = 'plate_steel' } } })"
                , "unitRow(2, 'technomule', 1, 0, HOME,"
                , "        { { defName = 'plate_steel', category = 'Materials' } })"
                , "GROUND = { { id = 1, defName = 'plate_steel', x = 0, y = 0,"
                , "             weight = 1 } }"
                , "GROUND_BY_PAGE = { [HOME] = GROUND }"
                , "BILLS = { { id = 1, station = 42, recipe = 'r', mode = 'count' } }"
                , "local s = newState()"
                , "s.craftJob = { billId = 1, bid = 42, recipeId = 'r', work = 0,"
                , "               skill = 'smithing',"
                , "               need = { plate_steel = 3 },"
                , "               fromGround = { plate_steel = 1 },"
                , "               fromMule = { plate_steel = 1 },"
                , "               fromCargo = { plate_steel = 1 },"
                , "               phase = 'fetch' }"
                , "craftAi.craftExecute(1, s, PARAMS)"
                , "assert(s.craftJob == nil,"
                , "  'an off-page craft job must be released before sourcing')"
                , "assert(PICKUPS == 0 and TO_UNIT == 0 and WITHDRAWS == 0,"
                , "  'an off-page craft job must move no items while sourcing')"
                , "assert(MOVES == 0 and STOPS == 0,"
                , "  'an off-page craft job must not steer a walk while sourcing')"
                , "-- Control: the same job at a same-page station does"
                , "-- reach the ground rung."
                , "BUILDINGS = {}"
                , "buildingRow('store', 42, 5, HOME)"
                , "local t = newState()"
                , "t.craftJob = { billId = 1, bid = 42, recipeId = 'r', work = 0,"
                , "               skill = 'smithing',"
                , "               need = { plate_steel = 3 },"
                , "               fromGround = { plate_steel = 1 },"
                , "               fromMule = { plate_steel = 1 },"
                , "               fromCargo = { plate_steel = 1 },"
                , "               phase = 'fetch' }"
                , "craftAi.craftExecute(1, t, PARAMS)"
                , "assert(PICKUPS == 1,"
                , "  'a same-page craft job must still source from the ground')"
                ]

        it "passes the ACTING unit to every bill verb it drives" $
            runsOk $ lns
                [ prelude
                -- #2325: bill ids are per-page and every page's allocator
                -- starts at 1, so a retained job.billId names a real bill
                -- on whatever page happens to be visible. Nothing here
                -- switches pages -- the two-page consequence is
                -- Test.Headless.Craft.BillPageBinding's job. What this
                -- case pins is the precondition that gate needs: the AI
                -- actually HANDS the engine an acting unit, at every call
                -- site, including the two release paths that run no page
                -- check of their own.
                , "buildingRow('store', 42, 1, HOME)"   -- adjacent to the actor
                , "BILLS = { { id = 1, station = 42, recipe = 'r',"
                , "            mode = 'count', progress = 0 } }"
                -- (a) walking -> working sets the #590 flag.
                , "local s = newState()"
                , "s.craftJob = { billId = 1, bid = 42, recipeId = 'r', work = 10,"
                , "               skill = 'smithing', need = {}, fromGround = {},"
                , "               fromMule = {}, fromCargo = {}, phase = 'walking' }"
                , "craftAi.craftExecute(1, s, PARAMS)"
                , "assert(s.craftJob.phase == 'working',"
                , "  'an adjacent same-page station must reach the working phase')"
                , "assert(billCallsBy(1, 1) == 'getBill,claimBill,setBillWorking',"
                , "  'walking->working drove ' .. billCallsBy(1, 1))"
                , "assert(BILL_CALLS[#BILL_CALLS].extra == true,"
                , "  'entering the working phase must set the flag TRUE')"
                -- (b) one working tick: pour, craft, complete the cycle.
                , "BILL_CALLS = {}"
                , "NOW = 5"
                , "s.lastCraftAt = 4"
                , "craftAi.craftExecute(1, s, PARAMS)"
                , "assert(billCallsBy(1, 1) =="
                , "  'getBill,claimBill,addBillProgress,executeAt,completeBillCycle',"
                , "  'a working tick drove ' .. billCallsBy(1, 1))"
                -- (c) craftOnExit clears the flag for the SAME unit.
                , "BILL_CALLS = {}"
                , "local x = newState()"
                , "x.craftJob = { billId = 1, bid = 42, recipeId = 'r', work = 10,"
                , "               skill = 'smithing', phase = 'working' }"
                , "craftAi.craftOnExit(1, x, PARAMS)"
                , "assert(billCallsBy(1, 1) == 'setBillWorking',"
                , "  'craftOnExit drove ' .. billCallsBy(1, 1))"
                , "assert(BILL_CALLS[1].extra == false,"
                , "  'leaving the working phase must CLEAR the flag')"
                -- (d) the paused-not-working release, which fires BEFORE
                --     the #1673 station page check and so is the one
                --     mutating path with no page guard of its own.
                , "BILL_CALLS = {}"
                , "BILLS[1].paused = true"
                , "BILLS[1].working = false"
                , "local r = newState()"
                , "r.craftJob = { billId = 1, bid = 42, recipeId = 'r', work = 10,"
                , "               skill = 'smithing', need = {}, fromGround = {},"
                , "               fromMule = {}, fromCargo = {}, phase = 'fetch' }"
                , "craftAi.craftExecute(1, r, PARAMS)"
                , "assert(r.craftJob == nil,"
                , "  'a paused not-yet-working job must be released')"
                , "assert(billCallsBy(1, 1) == 'getBill,releaseBill',"
                , "  'the release path drove ' .. billCallsBy(1, 1))"
                -- (e) craftUtility's vanished-bill drop: the READ whose
                --     answer decides it is actor-qualified too, so a
                --     page-B bill numbered the same can never stand in for
                --     the job this unit is holding.
                , "BILL_CALLS = {}"
                , "BILLS = {}"
                , "local v = newState()"
                , "v.craftJob = { billId = 1, bid = 42, recipeId = 'r', work = 10,"
                , "               skill = 'smithing', phase = 'working' }"
                , "local u = craftAi.craftUtility(1, v, PARAMS)"
                , "assert(v.craftJob == nil, 'a vanished bill must drop the job')"
                , "assert(u == -math.huge, 'and score nothing, got ' .. tostring(u))"
                , "assert(billCallsBy(1, 1) == 'getBill',"
                , "  'craftUtility drove ' .. billCallsBy(1, 1))"
                ]

        it "releases a PERSISTED job naming a station on another page" $
            runsOk $ lns
                [ prelude
                , "buildingRow('store', 42, 5, AWAY)"
                , "BILLS = { { id = 1, station = 42, recipe = 'r', mode = 'count' } }"
                , "local s = newState()"
                , "s.craftJob = { billId = 1, bid = 42, recipeId = 'r', work = 0,"
                , "               skill = 'smithing', need = {}, fromGround = {},"
                , "               fromMule = {}, fromCargo = {}, phase = 'walking' }"
                , "craftAi.craftExecute(1, s, PARAMS)"
                , "assert(s.craftJob == nil,"
                , "  'an off-page craft job must be released')"
                , "assert(MOVES == 0, 'an off-page station must not steer a walk')"
                ]

    describe "the ground rung of the sourcing ladder" $ do
        it "counts and fetches only rows on the actor's own page" $
            runsOk $ lns
                [ prelude
                -- ONE gid, TWO pages, two different items. item.listGround
                -- shows the ACTIVE page's row; item.pickupGround commits on
                -- the CARRIER's page. A count or a walk taken off the
                -- listing alone would promise the actor stock it cannot
                -- reach and then hand that gid to a pickup that moves
                -- something else entirely.
                , "local away = { id = 1, defName = 'plate_steel', x = 3, y = 0,"
                , "               weight = 1 }"
                , "GROUND = { away }                 -- AWAY is 'active' here"
                , "GROUND_BY_PAGE = { [AWAY] = { away } }"
                , "assert(fetch.groundCountOf(1, 0, 0, 'plate_steel', 50) == 0,"
                , "  'an off-page ground row must not be counted')"
                , "local busy = fetch.fetchWantsFromGround("
                , "  1, { plate_steel = 1 }, PARAMS, 50)"
                , "assert(MOVES == 0 and PICKUPS == 0,"
                , "  'an off-page ground row must draw neither a walk nor a pickup')"
                , "assert(busy == false, 'nothing reachable means the fetch is done')"
                , "-- The SAME gid, now naming a row on the actor's own page."
                , "local home = { id = 1, defName = 'plate_steel', x = 3, y = 0,"
                , "               weight = 1 }"
                , "GROUND_BY_PAGE = { [AWAY] = { away }, [HOME] = { home } }"
                , "assert(fetch.groundCountOf(1, 0, 0, 'plate_steel', 50) == 1,"
                , "  'a same-page ground row must still be counted')"
                , "local busy2 = fetch.fetchWantsFromGround("
                , "  1, { plate_steel = 1 }, PARAMS, 50)"
                , "assert(busy2 == true and MOVES == 1,"
                , "  'a distant same-page row must still be walked to')"
                ]

        it "counts nothing for an actor whose page cannot be resolved" $
            runsOk $ lns
                [ prelude
                , "local home = { id = 1, defName = 'plate_steel', x = 1, y = 0,"
                , "               weight = 1 }"
                , "GROUND = { home }"
                , "GROUND_BY_PAGE = { [HOME] = { home } }"
                -- uid 99 is no unit, so getGroundForUnit answers
                -- (nil, false): nothing determined. Fail closed rather
                -- than falling back to the active page.
                , "assert(fetch.groundCountOf(99, 0, 0, 'plate_steel', 50) == 0,"
                , "  'an unresolvable actor must count no ground stock')"
                ]

        it "leaves the until-stock tally on the ACTIVE page, as #795 specifies" $
            runsOk $ lns
                [ prelude
                -- groundStockCountOf answers about the world the player
                -- is looking at (it must equal crafting_panel.lua's
                -- groundStockTally), so it deliberately does NOT join
                -- the per-actor resolution above.
                , "GROUND = { { id = 1, defName = 'widget', x = 9, y = 9 } }"
                , "GROUND_BY_PAGE = {}"
                , "local bill = { mode = 'until', outputItem = 'widget', target = 1 }"
                , "assert(fetch.untilStockSatisfied(bill) == true,"
                , "  'the until-stock tally must still read the active page')"
                ]

    describe "repair_job" $ do
        it "refuses to score a FRESH job against an off-page station" $
            runsOk $ lns
                [ prelude
                -- The fresh-job path carries no bid until it is scored,
                -- so the persisted-target guard cannot see it.
                -- building.findStation ranks over the ACTIVE page, so
                -- the station it names has to be checked here or the
                -- job starts and its fetch phases run first.
                , "local away = buildingRow('store', 42, 5, AWAY)"
                , "STATION = { bid = 42, x = 5, y = 0 }"
                , "UNITS[1].inv = { { defName = 'axe_steel', instanceId = 9,"
                , "                   sharpness = 1, condition = 100 } }"
                , "local s = newState()"
                , "local u = repairAi.utility(1, s, PARAMS)"
                , "assert(u == -math.huge,"
                , "  'an off-page station must not score a repair job, got '"
                , "  .. tostring(u))"
                , "-- The SAME station on the actor's page does score,"
                , "-- and the vetted id is retained for execute."
                , "BUILDINGS = {}"
                , "buildingRow('store', 42, 5, HOME)"
                , "local t = newState()"
                , "local v = repairAi.utility(1, t, PARAMS)"
                , "assert(v > 0,"
                , "  'a same-page station must still score, got ' .. tostring(v))"
                , "assert(t.repairCandidate and t.repairCandidate.bid == 42,"
                , "  'the vetted station must be retained on the candidate, got '"
                , "  .. tostring(t.repairCandidate and t.repairCandidate.bid))"
                ]

        it "checks the station's page BEFORE the fetch phases run" $
            runsOk $ lns
                [ prelude
                -- The third instance of the same hazard: repairJob.bid
                -- is a PERSISTED building reference, and the fetch_item
                -- / fetch_consumable phases issue their own moveTo,
                -- transferItemToUnit and pickupGround before the
                -- walking phase ever looks at the station's page.
                , "buildingRow('store', 42, 5, AWAY)"
                , "unitRow(2, 'technomule', 1, 0, HOME,"
                , "        { { defName = 'whetstone', category = 'Materials' } })"
                , "GROUND = { { id = 1, defName = 'whetstone', x = 0, y = 0,"
                , "             weight = 1 } }"
                , "GROUND_BY_PAGE = { [HOME] = GROUND }"
                , "local s = newState()"
                , "s.repairPhase = 'fetch_consumable'"
                , "s.repairJob = { instanceId = 7, defName = 'axe_steel',"
                , "                axis = 'sharpness', recipeId = 'r',"
                , "                consumable = 'whetstone', consumableCount = 1,"
                , "                bid = 42, itemFetched = false }"
                , "repairAi.execute(1, s, PARAMS)"
                , "assert(s.repairJob == nil,"
                , "  'an off-page repair job must be dropped before sourcing')"
                , "assert(MOVES == 0 and PICKUPS == 0 and TO_UNIT == 0,"
                , "  'an off-page repair job must neither walk nor move items')"
                , "-- Control: the same job at a same-page station is"
                , "-- untouched by the check and advances normally. The"
                , "-- actor already holds the consumable here, so the"
                , "-- phase machine moves straight on rather than"
                , "-- re-testing the ground rung the cases above own."
                , "BUILDINGS = {}"
                , "buildingRow('store', 42, 5, HOME)"
                , "UNITS[1].inv = { { defName = 'whetstone', category = 'Materials' } }"
                , "local t = newState()"
                , "t.repairPhase = 'fetch_consumable'"
                , "t.repairJob = { instanceId = 7, defName = 'axe_steel',"
                , "                axis = 'sharpness', recipeId = 'r',"
                , "                consumable = 'whetstone', consumableCount = 1,"
                , "                bid = 42, itemFetched = false }"
                , "repairAi.execute(1, t, PARAMS)"
                , "assert(t.repairJob ~= nil,"
                , "  'a same-page repair job must survive')"
                , "assert(t.repairPhase == 'walking',"
                , "  'a same-page repair job must advance, got '"
                , "  .. tostring(t.repairPhase))"
                ]

        it "leaves a job that has not resolved a station yet alone" $
            runsOk $ lns
                [ prelude
                -- job.bid stays nil until the walking phase calls
                -- building.findStation, so the hoisted check must not
                -- touch a fresh job just because no station is named.
                , "UNITS[1].inv = { { defName = 'whetstone', category = 'Materials' } }"
                , "local s = newState()"
                , "s.repairPhase = 'fetch_consumable'"
                , "s.repairJob = { instanceId = 7, defName = 'axe_steel',"
                , "                axis = 'sharpness', recipeId = 'r',"
                , "                consumable = 'whetstone', consumableCount = 1,"
                , "                itemFetched = false }"
                , "repairAi.execute(1, s, PARAMS)"
                , "assert(s.repairJob ~= nil,"
                , "  'a station-less repair job must not be dropped')"
                , "assert(s.repairPhase == 'walking',"
                , "  'a station-less repair job must advance, got '"
                , "  .. tostring(s.repairPhase))"
                ]

    describe "foraging for ground food" $ do
        it "never selects, walks to, or picks up a row on another page" $
            runsOk $ lns
                [ prelude
                -- Same one-gid-two-pages shape as the sourcing ladder,
                -- on the OTHER path that reaches item.pickupGround.
                , "local away = { id = 1, defName = 'berry', x = 3, y = 0 }"
                , "GROUND = { away }"
                , "GROUND_BY_PAGE = { [AWAY] = { away } }"
                , "UNITS[1].hunger = 0"
                , "local s = newState()"
                , "local u = needs.forageUtility(1, s, PARAMS)"
                , "assert(u == -math.huge,"
                , "  'off-page ground food must not score, got ' .. tostring(u))"
                , "assert(s.forageTarget == nil,"
                , "  'off-page ground food must leave no target')"
                , "-- The SAME gid naming a row on the actor's own page."
                , "local home = { id = 1, defName = 'berry', x = 3, y = 0 }"
                , "GROUND_BY_PAGE = { [AWAY] = { away }, [HOME] = { home } }"
                , "local t = newState()"
                , "local v = needs.forageUtility(1, t, PARAMS)"
                , "assert(v > -math.huge,"
                , "  'same-page ground food must still score, got ' .. tostring(v))"
                , "assert(t.forageTarget and t.forageTarget.gid == 1,"
                , "  'same-page ground food must be targeted')"
                ]

        it "drops a PERSISTED target whose row is not on the carrier's page" $
            runsOk $ lns
                [ prelude
                -- The shape a save written before this check restores.
                , "local away = { id = 1, defName = 'berry', x = 3, y = 0 }"
                , "GROUND = { away }"
                , "GROUND_BY_PAGE = { [AWAY] = { away } }"
                , "local s = newState()"
                , "s.forageTarget = { kind = 'ground', gid = 1, x = 3, y = 0 }"
                , "needs.forageExecute(1, s, PARAMS)"
                , "assert(s.forageTarget == nil,"
                , "  'an off-page forage target must be dropped')"
                , "assert(MOVES == 0 and PICKUPS == 0,"
                , "  'an off-page forage target must draw neither a walk nor a pickup')"
                ]

        -- #2293: forage is the ONE pickup ladder that deliberately does
        -- not weigh the row, because eat_from_inventory consumes from
        -- the INVENTORY — food a full unit may not pick up is food it
        -- may not eat. Adding a capacity gate to either pickup form
        -- fails these.
        it "an over-capacity hungry unit still acquires ground food" $
            runsOk $ lns
                [ prelude
                -- 500 kg carried against a 100 kg capacity: five times
                -- over, on the rung that is the only way this unit eats.
                , "unit.getCarryingWeight = function() return 500 end"
                , "local home = { id = 1, defName = 'berry', x = 1, y = 0,"
                , "               weight = 40 }"
                , "GROUND = { home }"
                , "GROUND_BY_PAGE = { [HOME] = { home } }"
                , "local s = newState()"
                , "s.forageTarget = { kind = 'ground', gid = 1, x = 1, y = 0 }"
                , "needs.forageExecute(1, s, PARAMS)"
                , "assert(PICKUPS == 1,"
                , "  'a starving unit over capacity must still take the food')"
                , "assert(s.forageTarget == nil,"
                , "  'and the completed target is retired')"
                ]

        it "an over-capacity hungry unit still collects a harvested yield" $
            runsOk $ lns
                [ prelude
                -- The other pickup form: the yields a harvested plant
                -- left on the ground, pulled in one per tick.
                , "unit.getCarryingWeight = function() return 500 end"
                , "local s = newState()"
                , "s.foragePhase = 'collecting'"
                , "s.forageLoot  = { 7, 8 }"
                , "needs.forageExecute(1, s, PARAMS)"
                , "assert(PICKUPS == 1,"
                , "  'the collecting rung must not gate on capacity either')"
                , "assert(s.foragePhase == 'collecting',"
                , "  'and the collection continues while loot remains')"
                , "needs.forageExecute(1, s, PARAMS)"
                , "assert(PICKUPS == 2, 'the second yield is collected too')"
                ]

        it "a failed direct-ground pickup retires the stale target \
           \instead of retrying it" $
            runsOk $ lns
                [ prelude
                -- A raced commit: the row resolves, the pickup loses.
                -- The gid must not be walked back to next tick.
                , "PICKUP_OK = false"
                , "local home = { id = 1, defName = 'berry', x = 1, y = 0 }"
                , "GROUND = { home }"
                , "GROUND_BY_PAGE = { [HOME] = { home } }"
                , "local s = newState()"
                , "s.forageTarget = { kind = 'ground', gid = 1, x = 1, y = 0 }"
                , "needs.forageExecute(1, s, PARAMS)"
                , "assert(PICKUPS == 1, 'the pickup is attempted once')"
                , "assert(s.forageTarget == nil,"
                , "  'a failed pickup must retire the target')"
                , "needs.forageExecute(1, s, PARAMS)"
                , "assert(PICKUPS == 1,"
                , "  'and must not retry the stale gid on the next tick')"
                ]

        it "a failed collecting pickup ends the collection instead of \
           \retrying the stale gid" $
            runsOk $ lns
                [ prelude
                , "PICKUP_OK = false"
                , "local s = newState()"
                , "s.foragePhase = 'collecting'"
                , "s.forageLoot  = { 7, 8 }"
                , "needs.forageExecute(1, s, PARAMS)"
                , "assert(PICKUPS == 1, 'the pickup is attempted once')"
                , "assert(s.foragePhase == nil and s.forageLoot == nil,"
                , "  'a failed collection pickup must end the phase')"
                , "needs.forageExecute(1, s, PARAMS)"
                , "assert(PICKUPS == 1, 'and must not retry the stale gid')"
                ]

    describe "unit-to-unit item targets" $ do
        it "findTechnomule refuses a mule on another page" $
            runsOk $ lns
                [ prelude
                , "unitRow(2, 'technomule', 5, 0, AWAY)"
                , "assert(fetch.findTechnomule(1, 0, 0) == nil,"
                , "  'an off-page mule must not be selected')"
                , "UNITS[2] = nil"
                , "unitRow(2, 'technomule', 5, 0, HOME)"
                , "local m = fetch.findTechnomule(1, 0, 0)"
                , "assert(m and m.uid == 2,"
                , "  'a same-page mule must still be selected')"
                , "-- An actor whose own page cannot be read selects nothing."
                , "assert(fetch.findTechnomule(99, 0, 0) == nil,"
                , "  'an unknown actor must select no mule')"
                ]

        it "findKitHolder refuses a holder on another page" $
            runsOk $ lns
                [ prelude
                , "local function kitHolder(uid, pg)"
                , "  local u = unitRow(uid, 'technomule', 5, 0, pg,"
                , "    { { defName = 'medkit', kind = 'container' } })"
                , "  u.contents['medkit'] = { { defName = 'bandage', count = 3 } }"
                , "end"
                , "kitHolder(2, AWAY)"
                , "assert(medic.findKitHolder(1, 0, 0) == nil,"
                , "  'an off-page kit holder must not be selected')"
                , "UNITS[2] = nil"
                , "kitHolder(2, HOME)"
                , "local h = medic.findKitHolder(1, 0, 0)"
                , "assert(h and h.uid == 2 and h.kit == 'medkit',"
                , "  'a same-page kit holder must still be selected')"
                ]

    describe "treat_ally (#2297)" $ do
        it "ignores a bleeding ally on another page, and takes the same one on its own" $
            runsOk $ lns
                [ prelude
                , "woundedAlly(2, AWAY)"
                , "local s = newState()"
                , "local u = medic.treatAllyUtility(1, s, PARAMS)"
                , "assert(u == -math.huge,"
                , "  'an off-page patient must not score, got ' .. tostring(u))"
                , "assert(s.treatPending == nil,"
                , "  'an off-page patient must leave no pending target')"
                , "-- The SAME ally, same coordinates and same wound, on"
                , "-- the medic's own page."
                , "clearUnit(2)"
                , "woundedAlly(2, HOME)"
                , "local t = newState()"
                , "local v = medic.treatAllyUtility(1, t, PARAMS)"
                , "assert(v > 0,"
                , "  'a same-page patient must still score, got ' .. tostring(v))"
                , "assert(t.treatPending and t.treatPending.uid == 2,"
                , "  'a same-page patient must still be nominated')"
                ]

        -- The nearest-wins scan is the part that has to be page-aware,
        -- not just the ladder around it: a NEARER off-page ally that
        -- still gets picked wins the slot and is then dropped by the
        -- medic ranking, so the on-page ally standing further out is
        -- never treated at all. Only the on-page ally must be seen.
        it "never lets a nearer off-page ally beat the on-page one to the slot" $
            runsOk $ lns
                [ prelude
                , "woundedAlly(2, AWAY, 1)   -- one tile out, wrong world"
                , "woundedAlly(3, HOME, 3)   -- three tiles out, right one"
                , "local s = newState()"
                , "local u = medic.treatAllyUtility(1, s, PARAMS)"
                , "assert(u > 0,"
                , "  'the on-page ally must still be treatable, got ' .. tostring(u))"
                , "assert(s.treatPending and s.treatPending.uid == 3,"
                , "  'the on-page ally must be the one nominated, got '"
                , "  .. tostring(s.treatPending and s.treatPending.uid))"
                ]

        -- The squad rule reads across the whole roster: only the best
        -- AVAILABLE medic takes a patient, and every other one stands
        -- down. A ranked medic on another page therefore does not just
        -- fail to help -- it silences the one that could.
        it "never ranks a medic on another page, and still stands down for an on-page one" $
            runsOk $ lns
                [ prelude
                , "woundedAlly(2, HOME)"
                , "-- A far more capable medic, standing ON the patient."
                , "unitRow(3, 'acolyte', 3, 0, AWAY)"
                , "KNOW[3] = 100.0"
                , "local s = newState()"
                , "local u = medic.treatAllyUtility(1, s, PARAMS)"
                , "assert(u > 0,"
                , "  'an off-page rival must not outrank the on-page medic, got '"
                , "  .. tostring(u))"
                , "assert(s.treatPending and s.treatPending.uid == 2,"
                , "  'the on-page medic must still take the patient')"
                , "-- Control: the SAME rival on the patient's own page"
                , "-- does outrank it, so the case above is about the"
                , "-- page and not about the ranking failing outright."
                , "clearUnit(3)"
                , "unitRow(3, 'acolyte', 3, 0, HOME)"
                , "KNOW[3] = 100.0"
                , "local t = newState()"
                , "local v = medic.treatAllyUtility(1, t, PARAMS)"
                , "assert(v == -math.huge,"
                , "  'a same-page better medic must take the patient, got '"
                , "  .. tostring(v))"
                ]

        -- aiState is global across pages but only the LOADED page is
        -- ticked, so an off-page claimer never reaches treatExecute to
        -- release its claim. Left holding the slot it would pin the
        -- patient forever against a medic standing right beside them --
        -- a suppression no engine-side refusal can undo, because
        -- nothing is ever attempted.
        it "never lets a claim held by a medic on another page suppress an on-page one" $
            runsOk $ lns
                [ prelude
                , "local core = require('scripts.unit_ai_core')"
                , "woundedAlly(2, HOME)"
                , "unitRow(3, 'acolyte', 5, 0, AWAY)"
                , "core.aiState[3] = { treatClaim = { patient = 2 } }"
                , "local s = newState()"
                , "local u = medic.treatAllyUtility(1, s, PARAMS)"
                , "assert(u > 0,"
                , "  'an off-page claimer must not hold the patient, got '"
                , "  .. tostring(u))"
                , "assert(s.treatPending and s.treatPending.uid == 2,"
                , "  'the on-page medic must still take the patient')"
                , "-- Control: the SAME claimer, same claim, on the"
                , "-- patient's own page still reserves it (#306)."
                , "UNITS[3] = nil"
                , "unitRow(3, 'acolyte', 5, 0, HOME)"
                , "local t = newState()"
                , "local v = medic.treatAllyUtility(1, t, PARAMS)"
                , "assert(v == -math.huge,"
                , "  'a same-page claimer must still hold the patient, got '"
                , "  .. tostring(v))"
                ]

        -- A claim OUTLIVES the tick that made it, so the page it was
        -- made on can move underneath it. Releasing it late -- after
        -- the kit fetch, or at the walk -- would already have moved an
        -- item and steered a sprint at another world's coordinates.
        it "releases a stored claim on an off-page patient before any walk, fetch or treat" $
            runsOk $ lns
                [ prelude
                , "woundedAlly(2, AWAY)"
                , "local s = newState()"
                , "s.treatClaim = { patient = 2 }"
                , "medic.treatExecute(1, s, PARAMS)"
                , "assert(s.treatClaim == nil,"
                , "  'an off-page claim must be released')"
                , "assert(MOVES == 0 and STOPS == 0,"
                , "  'an off-page claim must steer neither a walk nor a stop')"
                , "assert(TO_UNIT == 0,"
                , "  'an off-page claim must not fetch a kit')"
                , "assert(TREATS == 0 and INFECT_TREATS == 0,"
                , "  'an off-page claim must not treat')"
                , "clearUnit(2)"
                , "woundedAlly(2, HOME)"
                , "local t = newState()"
                , "t.treatClaim = { patient = 2 }"
                , "medic.treatExecute(1, t, PARAMS)"
                , "assert(t.treatClaim ~= nil,"
                , "  'a same-page claim must be kept')"
                , "assert(MOVES == 1,"
                , "  'a same-page claim must still draw the walk')"
                ]

-- The other half of #2297's "one authority": the shipped arrival
-- check must READ unit.treatmentRange() rather than restate 1.5, so
-- the AI can never walk to a distance the engine then refuses. Moving
-- the stubbed reach either way is the only thing that separates the
-- two.
reachSpec ∷ Spec
reachSpec = describe "AI medic treat reach" $ do
    it "walks or treats by the engine's reach, not by a copied number" $
        runsOk $ lns
            [ prelude
            , "woundedAlly(2, HOME)   -- three tiles from the medic"
            , "TREAT_RANGE = 5.0"
            , "local s = newState()"
            , "s.treatClaim = { patient = 2 }"
            , "medic.treatExecute(1, s, PARAMS)"
            , "assert(MOVES == 0,"
            , "  'a patient inside the engine reach must not be walked to')"
            , "assert(TREATS == 1,"
            , "  'a patient inside the engine reach must be treated')"
            , "TREAT_RANGE = 1.5"
            , "local t = newState()"
            , "t.treatClaim = { patient = 2 }"
            , "medic.treatExecute(1, t, PARAMS)"
            , "assert(MOVES == 1,"
            , "  'a patient beyond the engine reach must be walked to')"
            , "assert(TREATS == 1,"
            , "  'a patient beyond the engine reach must not be treated')"
            ]
